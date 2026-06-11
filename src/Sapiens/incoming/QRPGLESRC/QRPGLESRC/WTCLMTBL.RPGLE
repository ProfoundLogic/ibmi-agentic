     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/title WTCLMTBL  - API - Claim Table
     h/define   PROFOUNDUI
     h nomain
     h DFTNAME(WTCLMTBL)
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:
     h*
     h*  Called from: WPICLMPV
     h*  Calls to: SA001R
     h*
     h*
     h*  Created on 8/04/2014 by c:295 for Project WC2247
     h**********************************************************************
     h* Revisions:
     h*
     h*  03/02/23 C:340 Project: WC2430
     h*                 Add Lump Sum Indicator
     h*                 Add State Drop Down and Radio to Include/Exclude States.
     h*
     h*  12/10/20 C:062 Project: 89WCR
     h*                 Add Exclude from ALAE
     h*
     h*  12/06/19 C:079 Project: 129WVK
     h*                 Add Vocational Rehab Paid Under Comp field
     h*
     h*  08/19/19 C:079 Project: WC2604
     h*                 Add IDC Benefit Type Code
     h*
     h*  08/31/18 C:079 Support: 146/72945
     h*                 Add Order by property on drop down fields
     h*
     h*  05/01/18 C:274 Project: WC1917
     h*                 Added EditAddBP procedure.
     h*
     h*  09/15/14 C:282 Support: 86742
     h*                 Added the Edit to not allow same reserve code
     h*                 to be picked more than once.
     h*
     h**********************************************************************
     fwtclmtbl  cf   e             workstn extfile('WTCLMTBL')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')

     fwtabl     if   e           k disk    rename(pb201:pb200)
      *** Beg Add *** 05/01/18 **************************************************
     f                                     rename(ppa01:ppa0L)

     fwtpap     uf a e             disk    rename(ppa01:ppa0p)
      *** End Add *** 05/01/18 **************************************************

     fwtgtl     if   e           k disk    rename(pgt01:pgt00)

     fssylf     if   e           K disk
     f                                     IGNORE(SSY01)
     f                                     IGNORE(SPC01)
     f                                     IGNORE(SRC01)
     f                                     IGNORE(SBC01)
     f                                     IGNORE(SCS01)

     fwtb2p     uf a e             disk    rename(pb201:pb20p)
      *** Beg Add *** 03/02/23 **************************************************
     FWTB2PST   UF A E           K disk    ReName(PB2S01:PB2S0P)
      *** End Add *** 03/02/23 **************************************************

     d psds          esds                  extname(szq1p)

     d* Add Payment value code
     d AddPayVal       pr

      *** Beg Add *** 05/01/18 **************************************************
     d* Add Part
     d AddPart         pr
      *** End Add *** 05/01/18 **************************************************

     d* Move database record to screen
     d MoveDbSc        pr

      *** Beg Add *** 05/01/18 **************************************************
     d* Move Body Part database record to screen
     d MoveBPDbSc      pr
      *** End Add *** 05/01/18 **************************************************

     d* Validate Add/Edit screen
     d Validate        pr

      *** Beg Add *** 05/01/18 **************************************************
     d* Validate Add/Edit screen
     d ValidateBP      pr
      *** End Add *** 05/01/18 **************************************************

     d* Add/Edit Payment value code
     d ProcessAdd      pr

     d* Retrieve system values
     d rtvsysval       pr                  ExtPgm('SA001R')
     d  ##Wcif                        1
     d  ##Mcif                        1
     d  ##Wcin                        1
     d  ##Ar                          1
     d  ##Ntwk                        1
     d  ##Ur                          1
     d  ##Med                         1
     d  ##Edi                         1
     d  ##Wcdk                        1
     d  ##Rhab                        1
     d  ##Mss                         1
     d  ##Fsch                        1
     d  ##Prvb                        1
     d  ##Npwc                        1
     d  ##Os                          1
     d  ##Arls                        1
     d  ##Ccod                        1
     d  ##Crvw                        1
     d  ##Cchk                        1
     d  ##Prom                        1
     d  ##Mcrv                        1
     d  ##Freq                        1
     d  ##Crac                        1
     d  ##Preo                        1
     d  ##Parn                        1
     d  ##Mgri                        1
     d  ##Kcif                        1
     d  ##Curs                        1
     d  ##Pcib                        1
     d  ##Clt#                        3
     d  ##Ucpf                        1
     d  ##Dcf                         1
     d  ##Deif                        1
     d  ##Aure                        1
     d  ##Cred                        1
     d  ##Idrt                        1
     d  ##Cprv                        1
     d  ##Eft                         1
     d  ##Govr                        1
     d  ##Ncdc                        1
     d  ##Splt                        1
     d  ##Last                        1

     d  ##Wcif         s              1
     d  ##Mcif         s              1
     d  ##Wcin         s              1
     d  ##Ar           s              1
     d  ##Ntwk         s              1
     d  ##Ur           s              1
     d  ##Med          s              1
     d  ##Edi          s              1
     d  ##Wcdk         s              1
     d  ##Rhab         s              1
     d  ##Mss          s              1
     d  ##Fsch         s              1
     d  ##Prvb         s              1
     d  ##Npwc         s              1
     d  ##Os           s              1
     d  ##Arls         s              1
     d  ##Ccod         s              1
     d  ##Crvw         s              1
     d  ##Cchk         s              1
     d  ##Prom         s              1
     d  ##Mcrv         s              1
     d  ##Freq         s              1
     d  ##Crac         s              1
     d  ##Preo         s              1
     d  ##Parn         s              1
     d  ##Mgri         s              1
     d  ##Kcif         s              1
     d  ##Curs         s              1
     d  ##Pcib         s              1
     d  ##Clt#         s              3
     d  ##Ucpf         s              1
     d  ##Dcf          s              1
     d  ##Deif         s              1
     d  ##Aure         s              1
     d  ##Cred         s              1
     d  ##Idrt         s              1
     d  ##Cprv         s              1
     d  ##Eft          s              1
     d  ##Govr         s              1
     d  ##Ncdc         s              1
     d  ##Splt         s              1
     d  ##Last         s              1

     d isValid         s              1n   inz(*off)
     d ##mode          s              1
     d intRRN          s              9  0 inz
     d intCo#          s              3  0
      *** Beg Add *** 03/02/23 **************************************************
     d inState         S              2
     d inSta1          S              2
     d intPos          S              3  0
     d isValidS        S              1n   inz(*off)
     d StrLen          S              3  0
      *** End Add *** 03/02/23 **************************************************
      *** Beg Add *** 09/15/14 **************************************************
     d  scRsvCodeDs    ds
     d  scRsvCode1
     d  scRsvCode2
     d  scRsvCode3
     d  scRsvCode4
     d  scRsvCode5
     d  scRsvCode6
     d  scRsvCode7
     d  scRsvCode8
     d  scRsvCode9
     d  scRsvCod10
     d  scRsvCode                     2    Overlay(scRsvCodeDs) Dim(10)
      *
     d  erRsvCodeDs    ds
     d  erReserve1
     d  erReserve2
     d  erReserve3
     d  erReserve4
     d  erReserve5
     d  erReserve6
     d  erReserve7
     d  erReserve8
     d  erReserve9
     d  erReserv10
     d  erRsvCode                     1    Overlay(erRsvCodeDs) Dim(10)
     d                                     Inz(*Off)
      *
     d  msRsvCodeDs    ds
     d  msReserve1
     d  msReserve2
     d  msReserve3
     d  msReserve4
     d  msReserve5
     d  msReserve6
     d  msReserve7
     d  msReserve8
     d  msReserve9
     d  msReserv10
     d  msRsvCode                    10    Overlay(msRsvCodeDs) Dim(10)
      *
     d ArrRsvCode      s              2    dim(10)
     d i               s              3  0
      *** End Add *** 09/15/14 **************************************************
     d* copy books
     d/copy sPrDate
     d/copy sPrString
     d/copy wPrGroup
     d/copy wPrUsrPrms
     d/copy wTclmtblPr
     d*** Beg Add *** 08/19/19 *********************************************
     d/copy sprModule
     d*** End Add *** 08/19/19 *********************************************

       //===================================================================
       // EditAddPV1   - Edit/Add Payment Value 1 codes
       //===================================================================
     p EditAddPV1      b                   Export
     d EditAddPV1      pi
     d inRRN                          9  0 options(*nopass)

      /Free
       // To get the company number
       intCo# = UserCo#(q1user);

       // Program Mode.
       If %parms() >  0;
          ##mode  = 'C';
          intRRN  = inRRN;
       Else;
          ##mode  = 'A';
          intRRN  = *zeros;
       Endif;

       ProcessAdd();


       intRRN  = *zeros;
       return;
      /End-Free
     p EditAddPV1      e

       //===================================================================
       // ProcessAdd - process addition of Payment Value 1 Code
       //===================================================================
     p ProcessAdd      b
     d ProcessAdd      pi

      /free
       clear RcdAddPayv;

       // Is EDI module and EFT is installed
        ##Wcif = *Blanks;
        ##Mcif = *Blanks;
        ##Wcin = *Blanks;
        ##Ar   = *Blanks;
        ##Ntwk = *Blanks;
        ##Ur   = *Blanks;
        ##Med  = *Blanks;
        ##Edi  = *Blanks;
        ##Wcdk = *Blanks;
        ##Rhab = *Blanks;
        ##Mss  = *Blanks;
        ##Fsch = *Blanks;
        ##Prvb = *Blanks;
        ##Npwc = *Blanks;
        ##Os   = *Blanks;
        ##Arls = *Blanks;
        ##Ccod = *Blanks;
        ##Crvw = *Blanks;
        ##Cchk = *Blanks;
        ##Prom = *Blanks;
        ##Mcrv = *Blanks;
        ##Freq = *Blanks;
        ##Crac = *Blanks;
        ##Preo = *Blanks;
        ##Parn = *Blanks;
        ##Mgri = *Blanks;
        ##Kcif = *Blanks;
        ##Curs = *Blanks;
        ##Pcib = *Blanks;
        ##Clt# = *Blanks;
        ##Ucpf = *Blanks;
        ##Dcf  = *Blanks;
        ##Deif = *Blanks;
        ##Aure = *Blanks;
        ##Cred = *Blanks;
        ##Idrt = *Blanks;
        ##Cprv = *Blanks;
        ##Eft  = *Blanks;
        ##Govr = *Blanks;
        ##Ncdc = *Blanks;
        ##Splt = *Blanks;
        ##Last = *Blanks;

        Rtvsysval(##Wcif:##Mcif:##Wcin:##Ar:##Ntwk:##Ur:##Med:##Edi:
         ##Wcdk:##Rhab:##Mss:##Fsch:##Prvb:##Npwc:##Os:##Arls:
         ##Ccod:##Crvw:##Cchk:##Prom:##Mcrv:##Freq:##Crac:##Preo:
         ##Parn:##Mgri:##Kcif:##Curs:##Pcib:##Clt#:##Ucpf:##Dcf:
         ##Deif:##Aure:##Cred:##Idrt:##Cprv:##Eft:##Govr:##Ncdc:
         ##Splt:##Last);

       // Is EDI module is installed
          Chain 'WC' Swc01;
          If %found and WCfl09 = 'Y';
             indEdi  = *on;
          Else;
             indEdi  = *off;
          Endif;

       // Is EFT module is installed
       If ##Eft = 'Y';
          indEft =*on;
       Else;
          indEft  = *off;
       Endif;

       // Is IDC module installed
       //*** Beg Add *** 08/19/19 ******************************************
       isIDC = isModEnabled('IDC');
       //*** End Add *** 08/19/19 ******************************************

       btnAddRcd = *off;
       btnCanRcd = *off;

       // processing for Edit mode
       If ##mode = 'C' and intRrn <> 0;
          Chain(n) intRrn pb20p;
          If %found;
             indAdpayCd = *on;
             scAdPayVal = B2key;
             MoveDbSc();
          Endif;
       Endif;

       // processing for Add mode
       If ##mode = 'A';
          indStsDis  = *on;
          scPayStat  = 'A';
          scEft      = *blanks;
          scWcStat   = *blanks;
          scCaFraud  = *blanks;
          scWeekDays = *blanks;
          scDesRec   = *blanks;
       Endif;

       //*** Beg Add *** 03/02/23 ******************************************
       // Load State Codes for Include or Exclude the selected states

         Clear BSState;
         Clear scStateCd;
         SetLL (intCo# : scAdPayVal) WTB2PST;
         ReadE (intCo# : scAdPayVal) WTB2PST;
         Dow Not %EOF;
           If scStateCd <> *blank;
              scStateCd = %trim(scStateCd) + ',';
           EndIf;
           scStateCd = %trim(scStateCd) + BSState;
           ReadE (intCo# : scAdPayVal) WTB2PST;
         EndDo;

       //*** End Add *** 03/02/23 ******************************************

       Dou BtnCanRcd = *on or (BtnAddRcd = *on and isValid = *off);
          Exfmt rcdAddPayv;
          isValid = *off;

          erPayCode = *off;
          erAddPayb = *off;
          erDesc    = *off;

          Select;
             When BtnAddRcd = *on;
                Validate() ;
                If isValid = *off;
                   AddPayVal();
                Endif;
             When BtnCanRcd = *on;
                BtnCanRcd = *off;
                Leave;
          Endsl;
       Enddo;
      /end-free
     p ProcessAdd      e
       //===================================================================
       // Validate - Validate Add/Edit screen
       //===================================================================
     p Validate        b
     d Validate        pi

     d ##stat          s              1
     d strKey          s                   Like (scAdPayVal)

      /free

       If ##mode = 'A';
          // Check if Payment Value Code 1 is blank.
          If scAdPayVal = *blanks;
             erAddPayb  = *on;
             isValid = *on;
          Endif;

          // Validate payment value code 1.
          If scAdPayVal <> *blanks;
             strKey = scAdPayVal;
             Chain(n) (intCo#:strKey) pb200;
             If %found;
                erPayCode = *on;
                isValid = *on;
             Endif;
          Endif;
       Endif;

       // Validate description is blank.
       If scAddDesc  = *blanks;
          erDesc     = *on;
          isValid = *on;
       Endif;

       // *** Beg Add *** 09/15/14 ********************************************
       // Clear Reserve Code Array
       Clear ArrRsvCode;
       Reset erRsvCode;
       //  A reserve code cannot be used more than once.

       for i=1 to %Elem(scRsvCode);
          If scRsvCode(i) <> *Blanks;
             If %LookUp(scRsvCode(i):ArrRsvCode) <> *Zero
                And scRsvCode(i) <> *Blanks;
                If erRsvCode(i) <> *On;
                   erRsvCode(i) =  *On;
                   msRsvCode(i) =  'WCR0557';
                Endif;
                isValid=*On;
             EndIf;
          Endif;

          ArrRsvCode(i)=scRsvCode(i);
       EndFor;
       // *** End Add *** 09/15/14 ********************************************

       // *** Beg Add *** 03/02/23 ********************************************
       // Validate Included/Excluded States

       If scStateCd <> *Blanks;
          intPos = %Scan(',':scStateCd);
          If intPos = 01;
             inSta1 = %SubSt(scStateCd:2:2);
          If inSta1 <> *Blanks;
             erStateCd = '1';
             msStateCd = 'WCR4820';
             isValid=*On;
          EndIf;
          EndIf;
       EndIf;

       // *** End Add *** 03/02/23 ********************************************

      /end-free
     p Validate        e
       //===================================================================
       // AddPayVal  - Add/Edit  Payment value  - MoveScDb
       //===================================================================
     p AddPayVal       b
     d AddPayVal       pi

      /free


       Chain intRrn pb20p;

       B2key  = Upper(scAdPayVal);
       B2desc = Upper(scAddDesc);
       B2plCd = Upper(scAddPlccd);
       B2stat = scPayStat;
       B2bnft = scAddBenIn;
       B2minr = scAddMinCD;
       B2pyCd = scAddEdiAd;
       B2ndDt = scAddDays;
       B2rmCd = scAddDarts;
       B2rc01 = scRsvCode1;
       B2rc02 = scRsvCode2;
       B2rc03 = scRsvCode3;
       B2rc04 = scRsvCode4;
       B2rc05 = scRsvCode5;
       B2rc06 = scRsvCode6;
       B2rc07 = scRsvCode7;
       B2rc08 = scRsvCode8;
       B2rc09 = scRsvCode9;
       B2rc10 = scRsvCod10;
       B2flg1 = scCaFraud;
       B2eft  = scEft;
       B2ercv = scAddEdirc;
       B2ccw  = scWcStat;
       B2exwdc= scWeekDays;
       B2rcvr = scDesRec;
       B2user = q1user;
       B2pgm  = q1pgm;
       B2dt   = Dateto7(SystemDate());
       B2time = Timeto6(SystemTime());
       //*** Beg Add *** 08/19/19 *****************************************
       B2iBtC = scIdcBtCd;
       //*** End Add *** 08/19/19 *****************************************
       //*** Beg Add *** 12/06/19 *****************************************
       B2vHpC = scVhPc;
       //*** End Add *** 12/06/19 *****************************************
       //*** Beg Add *** 12/10/20 *****************************************
       B2exALAE = scExcALAE;
       //*** End Add *** 12/06/19 *****************************************
       //*** Beg Add *** 03/02/23 *****************************************
       B2LmpSum   = scLumpSum;
       B2StSpInEx = scDStateSp;
       //*** End Add *** 03/02/23 *****************************************

       If ##mode = 'A';
          B2co#  = userCo#(q1user);
          Write pb20p;
       Else;
          Update pb20p;
       //*** Beg Add *** 03/02/23 *****************************************
       // Save Included/Excluded States

          SetLL (intCo# : scAdPayVal) WTB2PST;
          ReadE (intCo# : scAdPayVal) WTB2PST;
          Dow Not %EOF;
            Delete PB2S0P;
            ReadE (intCo# : scAdPayVal) WTB2PST;
          EndDo;

          intPos = 0;
          StrLen = %Len(%Trim(scStateCd));
          intPos = %Scan(',':scStateCd);
          Dow intPos > 0 and intPos < StrLen;
              If intPos = 03;
                 intPos = 1;
                 inState = %SubSt(scStateCd:intPos:2);
                 ExSr WriteSts;
                 intPos = intPos + 3;
                 inState = %SubSt(scStateCd:intPos:2);
                 ExSr WriteSts;
              Else;
                 inState = %SubSt(scStateCd:intPos+1:2);
                 ExSr WriteSts;
                 intPos = intPos + 2;
              EndIf;
              intPos = %Scan(',':scStateCd:intPos);
          EndDo;
       //*** End Add *** 03/02/23 *****************************************
       Endif;
       Unlock WTB2P;

       //*** Beg Add *** 03/02/23 *****************************************
       BegSr WriteSts;
         BsCO#      = intCo#;
         BsKEY      = Upper(scAdPayVal);
         BsSTSPINEX = scDStateSp;
         BsSTATE    = %Trim(inState);
         BsUSER     = q1user;
         BsPGM      = q1pgm;
         BsTIME     = Timeto6(SystemTime());
         BsDT       = Dateto7(SystemDate());
         Write PB2S0P;
       EndSr;
       //*** End Add *** 03/02/23 *****************************************

      /end-free
     p AddPayVal       e


       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     p MoveDbSc        b
     d MoveDbSc        pi

      /free

       scAdPayVal = B2key;
       scAddDesc  = B2desc;
       scAddPlccd = B2plCd;
       scPayStat  = B2stat;
       scAddBenIn = B2bnft;
       scAddMinCD = B2minr;
       scAddEdiAd = B2pyCd;
       scAddDays  = B2nddt;
       scAddDarts = B2rmCd;
       scRsvCode1 = B2rc01;
       scRsvCode2 = B2rc02;
       scRsvCode3 = B2rc03;
       scRsvCode4 = B2rc04;
       scRsvCode5 = B2rc05;
       scRsvCode6 = B2rc06;
       scRsvCode7 = B2rc07;
       scRsvCode8 = B2rc08;
       scRsvCode9 = B2rc09;
       scRsvCod10 = B2rc10;
       scCaFraud  = B2flg1;
       scEft      = B2eft ;
       scAddEdirc = B2ercv;
       scWcStat   = B2ccw;
       scWeekDays = B2exwdc;
       scDesRec   = B2rcvr;
       //*** Beg Add *** 08/19/19 *****************************************
       scIdcBtCd = B2iBtC;
       //*** End Add *** 08/19/19 *****************************************
       //*** Beg Add *** 12/06/19 *****************************************
       scVhPc = B2vHpC;
       //*** End Add *** 12/06/19 *****************************************
       //*** Beg Add *** 12/10/20 *****************************************
       scExcALAE = B2exALAE;
       //*** End Add *** 12/06/19 *****************************************
       //*** Beg Add *** 03/02/23 *****************************************
       scLumpSum = B2LmpSum;
       scDStateSp = B2StSpInEx;
       //*** End Add *** 03/02/23 *****************************************
      /end-free
     p MoveDbSc        e

      *** Beg Add *** 05/01/18 **************************************************
       //===================================================================
       // EditAddBP   - Edit/Add Body Part
       //===================================================================
     p EditAddBP       b                   Export
     d EditAddBP       pi
     d inRRNBP                        9  0 options(*nopass)

      /Free
       // To get the company number
       intCo# = UserCo#(q1user);

       // Program Mode.
       If %parms() >  0;
          ##mode  = 'C';
          intRRN  = inRRNBP;
       Else;
          ##mode  = 'A';
          intRRN  = *zeros;
       Endif;

       ProcessAddBP();


       intRRN  = *zeros;
       return;
      /End-Free
     p EditAddBP       e

       //===================================================================
       // ProcessAddBP - process addition of Body Part
       //===================================================================
     p ProcessAddBP    b
     d ProcessAddBP    pi

      /free
       clear RcdAddPart;

       // Is EDI module and EFT is installed
        ##Wcif = *Blanks;
        ##Mcif = *Blanks;
        ##Wcin = *Blanks;
        ##Ar   = *Blanks;
        ##Ntwk = *Blanks;
        ##Ur   = *Blanks;
        ##Med  = *Blanks;
        ##Edi  = *Blanks;
        ##Wcdk = *Blanks;
        ##Rhab = *Blanks;
        ##Mss  = *Blanks;
        ##Fsch = *Blanks;
        ##Prvb = *Blanks;
        ##Npwc = *Blanks;
        ##Os   = *Blanks;
        ##Arls = *Blanks;
        ##Ccod = *Blanks;
        ##Crvw = *Blanks;
        ##Cchk = *Blanks;
        ##Prom = *Blanks;
        ##Mcrv = *Blanks;
        ##Freq = *Blanks;
        ##Crac = *Blanks;
        ##Preo = *Blanks;
        ##Parn = *Blanks;
        ##Mgri = *Blanks;
        ##Kcif = *Blanks;
        ##Curs = *Blanks;
        ##Pcib = *Blanks;
        ##Clt# = *Blanks;
        ##Ucpf = *Blanks;
        ##Dcf  = *Blanks;
        ##Deif = *Blanks;
        ##Aure = *Blanks;
        ##Cred = *Blanks;
        ##Idrt = *Blanks;
        ##Cprv = *Blanks;
        ##Eft  = *Blanks;
        ##Govr = *Blanks;
        ##Ncdc = *Blanks;
        ##Splt = *Blanks;
        ##Last = *Blanks;

        Rtvsysval(##Wcif:##Mcif:##Wcin:##Ar:##Ntwk:##Ur:##Med:##Edi:
         ##Wcdk:##Rhab:##Mss:##Fsch:##Prvb:##Npwc:##Os:##Arls:
         ##Ccod:##Crvw:##Cchk:##Prom:##Mcrv:##Freq:##Crac:##Preo:
         ##Parn:##Mgri:##Kcif:##Curs:##Pcib:##Clt#:##Ucpf:##Dcf:
         ##Deif:##Aure:##Cred:##Idrt:##Cprv:##Eft:##Govr:##Ncdc:
         ##Splt:##Last);

       // Is EFT module is installed
       If ##Eft = 'Y';
          indEft =*on;
       Else;
          indEft  = *off;
       Endif;

       btnAddRcd = *off;
       btnCanRcd = *off;

       // processing for Edit mode
       If ##mode = 'C' and intRrn <> 0;
          Chain(n) intRrn ppa0p;
          If %found;
             indAdprtd = *on;
             scAdPart = PAkey;
             MoveBPDbSc();
          Endif;
       Endif;

       // processing for Add mode
       If ##mode = 'A';
          indStsDis  = *on;
          scPartStat  = 'A';
       Endif;

       Dou BtnCanRcd = *on or (BtnAddRcd = *on and isValid = *off);
          Exfmt rcdAddPart;
          isValid = *off;

          erPayCode = *off;
          erAddPayb = *off;
          erDesc    = *off;

          Select;
             When BtnAddRcd = *on;
                ValidateBP() ;
                If isValid = *off;
                   AddPart();
                Endif;
             When BtnCanRcd = *on;
                BtnCanRcd = *off;
                Leave;
          Endsl;
       Enddo;
      /end-free
     p ProcessAddBP    e


       //===================================================================
       // AddPart    - Add/Edit  Part  - MoveScDb    XXX
       //===================================================================
     p AddPart         b
     d AddPart         pi

      /free
       Chain intRrn ppa0p;

       PAkey  = Upper(scAdPart);
       PAdesc = Upper(scAddDesc);
       PAstat = scPartStat;
       PAmbr  = scWeekDays;
       PAedip = scAddEDIcd;
       PAuser = q1user;
       PApgm  = q1pgm;
       PAdt   = Dateto7(SystemDate());
       PAtime = Timeto6(SystemTime());

       If ##mode = 'A';
          PAco#  = userCo#(q1user);
          Write ppa0p;
       Else;
          Update ppa0p;
       Endif;
       Unlock WTB2P;
      /end-free
     p AddPart         e

       //===================================================================
       // MoveBPDbSc - Move Body Part database record to screen
       //===================================================================
     p MoveBPDbSc      b
     d MoveBPDbSc      pi

      /free
       scAdPart = PAkey;
       scAddDesc = PAdesc;
       scPartStat = PAstat;
       scAddEDIcd = PAedip;
      /end-free

     p MoveBPDbSc      e

       //===================================================================
       // ValidateBP - Validate Add/Edit screen
       //===================================================================
     p ValidateBP      b
     d ValidateBP      pi

     d ##stat          s              1
     d strKey          s                   Like (PAkey)

      /free

       If ##mode = 'A';
          erPart  = *off;
          // Check if Body Part Code is blank.
          If scAdPart = *blanks;
             erAddPart  = *on;
             isValid = *on;
          Endif;

          // Validate Body Part Code.
          If scAdPart <> *blanks;
             strKey = scAdPart;
             Chain(n) (intCo#:strKey) ppa0l;
             If %found;
                erPart  = *on;
                isValid = *on;
             Endif;
          Endif;
       Endif;

       // Validate description is blank.
       If scAddDesc  = *blanks;
          erDesc     = *on;
          isValid = *on;
       Endif;

      /end-free
     p ValidateBP      e
      *** End Add *** 05/01/18 **************************************************
