     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE WA023R3 - API to get EFT and Pre-note status
     h bnddir('WTBNDDIR')
     H*  CREATED: 06/24/08  PROGRAMMER: C:191 PROJECT: WC1453         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/05/23 C:65  Support: 107397 (Prescient)                    *
     H*                Clear return indicator on eft procedure.       *
     H*                                                               *
     H* 03/31/22 C:136 Support: 101683                                *
     H*                Populate emp# for GetStRptDv in ChkForEFT      *
     H*                Don't look for SRD EFT setup if NSRD has       *
     H*                valid setup.                                   *
     H*                                                               *
     H* 07/28/21 C:136 Support: 96220                                 *
     H*                Adjust logic for validating claims EFT         *
     H*                                                               *
     H* 04/06/21 C:136 Project: WC1986                                *
     H*                Add code to accomodate starting policy year    *
     H*                and EFT in Quote                               *
     H*                                                               *
     H* 07/23/15 C:136 Support: 36153                                 *
     H*                Add Bank Code parameter (CKTP)                 *
     H*                                                               *
     H* 05/27/15 C:062 Project: 114MRH                                *
     H*                Added new parameter EFT Flag.                  *
     H*                                                               *
     H* 04/17/12 C:282 Project: WC2044                                *
     H*                Added new parameter EFT Flag.                  *
     H*                                                               *
     H* 04/21/11 C:190 Project: 114WYI                                *
     H*                ACH for Premium Payments via Agent.            *
     H*                                                               *
     H* 12/06/10 C:136 Project: WC1806 - Priss 78                     *
     H*                EFT flag was not being returned in parms if    *
     H*                EFT record doesn't exist                       *
     H*                                                               *
     H* 10/26/09 C:191 Project: WC1806 - Priss                        *
     H*                Check Bank account master record for EFT setup *
     H*                If Immediate Destitation is zero then EFT is   *
     H*                NOT setup. Send EFT status as not allowed.     *
     H*                                                               *
     H* 07/12/10 C:136 Support: WA025 73052 (Not needed)              *
     H*                Use WMEFL3 to get the WMEFP record for 'C'     *
     H*                type claims so that EFT's process correctly    *
     H*                for Additional Addresses                       *
     H*                                                               *
     H* 06/29/09 C:191 Project: WC1806                                *
     H*                Use New LF to validate EFT                     *
     H*                Make generic to use with all types             *
     H*                Use "Bypass Prenote Processing Flag" for       *
     H*                prenote processing                             *
     H*                                                               *
     H*****************************************************************
     FWMEFL7    IF   E           K DISK
     F                                     infds(dbfds1)
     FWSCKL     IF   E           K DISK
       //*** beg add ***** 04/06/21 **************************
       Dcl-F WMEFP Rename(pef01:pef0p);
       Dcl-S chrDftSrD Char(1);
       Dcl-S chrEnv Char(1);
       Dcl-S intEmp# Packed(9:0);
       Dcl-S intFyr Packed(3:0);
       Dcl-S intStRptDiv Packed(5:0);
      /Copy wPrEmplr
       //*** end add ***** 04/06/21 **************************

     D dbfds1          ds
     D  dbrrn1               397    400b 0
     d ##Co#           s              3
     d ##Fnd           s              3
     d ##Appl          s              2
     d ##Srdt          s              8
     d ##Acct          s             20
     d ##Call          s              1
     d ##Hdcy          s              1
     d ##Chkq          s             10
     d ##FTYP          s             10
     d ##Rtcd          s              1
     d ##Efdt          s              7
     d ##Cpyq          s             10
     d ##Cktp          s             10
     d ##Coid          s             20
     d ##Eftc          s              1
      *
     D GetBankAcc      pr            20    ExtProc('GETBANKACC')
     d ##Co#                          3                                         01
     d ##Fnd                          3                                         02
     d ##Appl                         2                                         03
     d ##Srdt                         8                                         04
     d ##Call                         1                                         06
     d ##Hdcy                         1                                         07
     d ##Chkq                        10                                         08
     d ##FTYP                        10                                         09
     d ##Efdt                         7                                         11
     d ##Cpyq                        10                                         12
     d ##Cktp                        10    options(*nopass)                     13
     d ##Eftc                         1    options(*nopass)                     15
     d ##Coid                        20    options(*nopass)                     14
     d ##Rtcd                         1    options(*nopass)                     10
      /copy scopydtpl
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3             1
     C                   PARM                    QQTYPE            1             2
     C                   PARM                    QQFND             3             3
     C                   PARM                    QQKEY            30             4
     C                   PARM                    QQDATE            7             5
     C                   PARM                    QQEFT             1             6
     C                   PARM                    QQPRE             1             7
     C                   PARM                    QQRRN             9             8
     C                   PARM                    QQEOBT            3             9
     C                   PARM                    QQCKTP           10            10
      *** beg add ***** 04/06/21 **************************
     c                   Parm                    qqFyr             3            11
     c                   Parm                    qqEnv             1            12
     c                   Parm                    qqDftSrD          1            13
      *** end add ***** 04/06/21 **************************
     C*
     C*
     C     EFKY          KLIST
     C                   KFLD                    XXTYPE
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXKEY
     C                   KFLD                    XXDATE
     C*
     C     EFKY1         KLIST
     C                   KFLD                    XXTYPE
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXKEY
     C*
     C*
     C     CKKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ##ACCT
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQTYPE        XXTYPE            1
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQKEY         XXKEY            30
     C                   MOVE      QQDATE        XXDATE            7 0
       //*** beg add ***** 04/06/21 ***********************
       If %parms() > 10;
          intFyr = %Int(qqFyr);
       Endif;
       If %parms() > 11;
          chrEnv = qqEnv;
       Endif;
       //*** end add ***** 04/06/21 ***********************
     C                   MOVE      *OFF          EFT               1
     C                   MOVE      *OFF          PRENOT            1
     C                   Z-ADD     *ZERO         XXRRN             9 0
     C                   IF        %PARMS >= 8
     C                   MOVE      XXRRN         QQRRN
     C                   ENDIF
     C*
     C***************
     C*  EFT TRANSACTIONS ALLOWED
     C***************
     C*
       //*** beg chg ***** 04/06/21 ***********************
     C* RETRIEVE EFFECTIVE EFT SETUP RECORD
     C***     EFKY          SETGT     PEF01
     C***     EFKY1         READPE    PEF01                                  60
     C***                   DOW       *IN60 = *OFF
     C**** LOOP THRU FILE TO GET EFFECTIVE RECORD
     C***                   IF        XXDATE >= EFEFDT
     C***                             AND (XXDATE < EFEXDT OR EFEXDT = 0)
     C***                   MOVE      *ON           EFT
     C***                   LEAVE
     C***                   ENDIF                                                  EFFECTIVE RECORD
     C***     EFKY1         READPE    PEF01                                  60
     C***                   ENDDO                                                  *IN60
     C****
     C**** NO VALID EFT RECORD FOUND, RETURN
     C***                   IF        EFT = *OFF
     C***                   MOVE      EFT           QQEFT
     C***                   SETON                                        LR
     C***                   RETURN
     C***                   ENDIF                                                   EFT = *OFF
     C****
     C**** GET BYPASS PRE-NOTE PROCESSING FLAG
     C***                   EVAL      XXBYPN = 'N'
     C****
     C**** RETRIEVE BANK ACCOUNT#
     C***                   SELECT
     C***                   WHEN      EFTYPE = 'A'
     C***                   EVAL      ##APPL = 'AG'
     C***                   WHEN      EFTYPE = 'C' OR EFTYPE = 'D' OR EFTYPE = 'P'
     C***                   EVAL      ##APPL = 'WC'
     C***                   WHEN      EFTYPE = 'E'
     C***                   EVAL      ##APPL = 'WP'
     C***                   ENDSL
       // NO VALID EFT RECORD FOUND, RETURN
       //IF EFT = *OFF;
     C***                   MOVE      EFT           QQEFT
       // *INLR = *ON;
       // RETURN;
       //ENDIF;                                                                 // EFT = *OFF
       EFT = ChkForEFT(xxCo#:xxFnd:xxKey:qqType:intFyr:chrEnv);
       xxByPN = 'N';  // Pre-Note Flag
       Select;
       When EFT = *Off;
          qqEFT = EFT;
          *Inlr = *On;
          Return;
       When EFtype = 'A';
          ##Appl = 'AG';   // Bank Application Type
       When EFtype  = 'C' OR EFtype  = 'D' OR EFtype  = 'P';
          ##Appl = 'WC';
       When EFtype  = 'E';
          ##Appl = 'WP';
       Endsl;
       //*** end chg ***** 04/06/21 ***********************
     C*
     C* Call WA023R to retrieve Bank account#
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    dtmdyy
     C                   MOVEL(P)  SRDTO         ##SRDT
      /free

       ##acct = *blanks;
       ##call = *blanks;
       ##hdcy = *blanks;
       ##chkq = *blanks;
       ##ftyp = *blanks;
       ##rtcd = *blanks;
       ##efdt = *zero;
       ##cpyq = *blanks;
       If %parms >= 10;
          ##cktp = qqcktp;
       Else;
          ##cktp = *blanks;
       Endif;
       ##Coid = *blanks;
       ##Eftc = 'E';

       ##acct= GetBankAcc(##co#: ##fnd: ##appl: ##srdt: ##call:
                ##hdcy: ##chkq: ##ftyp: ##efdt: ##cpyq:
                ##cktp: ##eftc: ##coid: ##rtcd);
      /end-free
     C
     C                   IF        ##RTCD <> '1'
     C*
     C* RETRIEVE BYPASS PRENOT FLAG FROM BANK ACCOUNT MASTER
     C     CKKY          CHAIN     PCK01
     C* If Bank master record not found for EFT not setup
     C                   IF        NOT %FOUND OR CKIDST = 0
     C                   MOVE      *OFF          EFT
     C                   ENDIF                                                  NOT %FOUND
     C                   IF        %FOUND
     C                   MOVE      CKBYPN        XXBYPN            1
     C                   ENDIF                                                  %FOUND
     C*
     C                   ELSE                                                   ##RTCD <> '1'
     C* If WA023R did not find bank account code, set EFT to OFF
     C                   MOVE      *OFF          EFT
     C                   ENDIF                                                  ##RTCD <> '1'
     C
     C*
     C*  EFT TRANSACTIONS HAVE NOT BEEN APPROVED BUT PRENOTE HAS BEEN
     C*  SENT, DO NOT PROCESS AS EFT
     C*
     C     EFT           IFEQ      *ON
     C     XXBYPN        ANDNE     'Y'
     C     EFPDT         ANDNE     *ZERO
     C     EFADT         ANDEQ     *ZERO
     C                   MOVE      *OFF          EFT
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  EFT TRANSACTIONS ALLOWED BUT PRENOTE HAS NOT BEEN SENT
     C*
     C     EFT           IFEQ      *ON
     C     XXBYPN        ANDNE     'Y'
     C     EFPDT         ANDEQ     *ZERO
     C                   MOVE      *ON           PRENOT
     C                   ENDIF                                                  EFT=*ON
      *
      *  BANK INFO NOT SET UP IN W0028R07 OPTION 7 FROM W/W EFT ACCOUNTS
      *
     C     EFT           IFEQ      *ON
     C     EFID#         IFEQ      *BLANK
     C     EFDGIT        OREQ      *BLANK
     C     EFACCT        OREQ      *BLANK
     C                   MOVE      *OFF          EFT
     C                   MOVE      *OFF          PRENOT
     C                   ENDIF
     C                   ENDIF
     C*
       //*** beg chg ***** 04/06/21 **************************
     C***                   MOVE      EFT           QQEFT
     C***                   MOVE      PRENOT        QQPRE
     C***                   EVAL      XXRRN = DBRRN1
     C***                   IF        %PARMS >= 8
     C***                   MOVE      XXRRN         QQRRN
     C***                   ENDIF
     C***                   If        %Parms >= 9
     C***                   eval      qqEobT = EFeobT
     C***                   EndIf
     C***
     C***                   SETON                                        LR
       qqEft = EFT;
       qqPre = preNot;
       xxRRN = dbRRN1;
       // Clear SRD Default flag if EFT isn't active due to prenote/setup issues
       If EFT = *Off;
          chrDftSrd = *Blank;
       EndIf;
       If %Parms >= 8;
          qqRRN = %EditC(xxRRN:'X');
       Endif;
       If %Parms >= 9;
          qqEobt = EFeobT;
       Endif;
       If %Parms > 12;
          qqDftSrD = chrDftSrD;
       Endif;
       *Inlr = *On;
       //==================================================
       Dcl-Proc ChkForEft;
       //==================================================
       Dcl-Pi *N ind;
        inCo# Like(EFco#);
        inFnd Like(EFfnd);
        inKey Like(EFkey);
        inType like(EFtype);
        inFyr Like(EFfyr);
        inEnv Char(1);
       End-Pi;
       Dcl-S flg1stExpRecDft Char(1) Inz('X');
       Dcl-S indEFT Ind;
       Dcl-S intCtr Packed(1:0);
       Dcl-S intStart Packed(1:0);
       Dcl-S isStRptDiv Ind;
       Dcl-S svRRN Packed(9:0);
       //*** begin add ***  04/05/23  **************************************
       Clear indEFT;
       Clear isStRptDiv;
       //*** end add *****  04/05/23  **************************************
       // If 'Employer' type, check current division first.
       //  Then check St Rpt Division if EFT isn't set up
       //  on current division
       If inType = 'E';
          intStart = 1;
         //*** beg add ***** 03/31/22 ********************
          intEmp# = %Int(%Subst(inKey:1:9));
         //*** end add ***** 03/31/22 ********************
          intStRptDiv = GetStRptDv(xxCo#:xxFnd:intEmp#);
          If intStRptDiv = %Int(%Subst(inKey:10:5));
             isStRptDiv = *On;
          Endif;
       Else;
          intStart = 2;
       Endif;
       xxKey = inKey;
       For intCtr = intStart To 2;
          Select;
          When flg1stExpRecDft <> 'X'
           And flg1stExpRecDft <> 'Y';  // NSRD doesn't check SRD EFT Status
             Leave;
          When intCtr = 2 and inType = 'E';
             If intStRptDiv = %Int(%Subst(inKey:10:5));
                Leave;
             Endif;
             flg1stExpRecDft = *Blank;
             isStRptDiv = *On;
             xxKey = %Subst(xxKey:1:9) + %Editc(intStRptDiv:'X');
          Endsl;
          Setgt efKy Pef01;
          Dou %Eof(WMEFL7);
             Readpe efKy1 Pef01;
             If %Eof;
                Leave;
             Endif;
             Select;
             When inEnv = *Blank And EFenvFlg = 'Q';
                Iter;
             When xxDate < EFefdt;
                Iter;
             When EFexdt <> 0 And xxDate >= EFexdt;
                If isStRptDiv = *Off And flg1stExpRecDft = 'X';
                   flg1stExpRecDft = EFDftSrd;  // Only chk St RPt Div if 'Y'
                Endif;
                Iter;
             When inFyr <> 0 And EFSfyr <> 0
              And inFyr < EFSfyr;
            // Keep RRN of current active EFT record
            //  for a later year in case the
            //  older year is still eligible for EFT
            //  and the bank has changed as of the
            //  current date as reflected in the
            //  later year.
                If EFenvFlg <> 'Q';
                   svRRN = dbRRN1;
                Endif;
                Iter;
            //*** beg chg ***** 07/28/21 ******************
            //***When intCtr = 2 And EFdftSrD <> 'Y';
             When intCtr = 2 And EFdftSrD <> 'Y' And inType = 'E';
            //*** end chg ***** 07/28/21 ******************
                Iter;
             Other;
                indEFT = *On;
                Leave;
             Endsl;
          Enddo;
          Select;
          When indEFT = *On;
             If svRRN <> 0;
               // Retrieve data from record with
               //  later starting policy year.
                Chain svRRN pef0p;
             Endif;
             If intCtr = 2;  // St Rpt Div provides EFT
                chrDftSrD = EFDftSrD;
             Endif;
            //*** beg add ***** 03/31/22 *****************
             Leave;
            //*** end add ***** 03/31/22 *****************
          Endsl;
       EndFor;
       Return indEFT;
       End-Proc;
       //*** end chg ***** 04/06/21 *************************
