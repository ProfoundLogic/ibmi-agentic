     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H nomain
     H/TITLE WPIIDC - NCCI Indemnity Data Call
     H DFTNAME(WPIIDC)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H/DEFINE   PROFOUNDUI
      *===============================================================*
      *  Created   : 06/20/19                                         *
      *  Programmer: C:180                                            *
      *  Project   : WC2604 - NCCI Indemnity Data Call                *
      *===============================================================*
      *  Description:  Service program which contains APIs for the NCCI
      *                Indemnity Data Call App.
      *
      * Revisions
      *
      * 11/15/25    C:053  Support 146/120110
      *                    Correct Class & Claim Indicator codes
      *
      * 10/15/25    C:053  Support 140/119861
      *                    Correct State code lookup
      *
      * 06/06/25    C:344  Support 133/118331
      *                    Include maintenance fields when updating IDC file
      *
      * 04/28/25    C:340  Support 118922
      *                    Populate QTEXPST & QTACCSTCD.
      *
      * 01/14/25    C:340  Project WC2995
      *                    Use New fields(splited from QTFILLER) of WMIDCQT
      *
      * 11/16/22    C:053  Support 133/104092
      *                    Correct code usage on percentage
      *
      * 12/15/20    C:180  Support 93297
      *                    Fix Quarterly policy Id to make it match
      *                    NCCI Unit Stat Reporting.
      *
      * 10/14/20    C:079  Support 146/91816
      *                    Add check to see if Transfer needs to be
      *                    executed
      *
      * 09/21/20    C:180  Support 91568
      *                    X-ref Go Claim method of determining pre-inj
      *                    wage code 2 to 1 in NCCI.
      *
      * 09/01/20    C:180  Support 90991
      *                    Fix transaction amount for Indemnity
      *                    Recovery reimbursements.
      *
      * 08/31/20    C:180  Support 91011
      *                    Default Temporary Medical Extinguishment to
      *                    blanks when not applicable.
      *
      * 08/30/20    C:079  Support 90986
      *                    Change how we load CMCRT into Weekly Benefit
      *                    Amount since its 7.2 and field is 9A.
      *
      * 06/11/20    C:180  Support 89284
      *                    Add FormatPolId procedure
      *
      * 05/12/20    C:180  Support 88566
      *                    Use Qtr processing date to get Closing date.
      *
      * 05/11/20    C:180  Fix impairment percent.
      * 04/21/20    C:180  Default Benefit Offset Code to '1'
      *===============================================================*
      *
     FWMIDCTR   O    E             DISK    usropn
     FWMIDCQT   O    E             DISK    usropn rename(PQT01:PQT00)
     FWMIDCTRL3 UF   E           K DISK    usropn rename(PTR01:PTR03) Prefix(x)
     FWMIDCQTL3 UF   E           K DISK    usropn rename(PQT01:PQT03) Prefix(x)
     FWDPAP     IF   E             DISK    usropn
     FWDPAL3    IF   E           K DISK    usropn rename(PPA01:PPA03)
     FWMCMP     IF   E             DISK    usropn rename(PCM01:PCM00)
     FWMCML1    IF   E           K DISK    usropn
     FWMLDL1    IF   E           K DISK    usropn
     FWMCHL     IF   E           K DISK    usropn rename(PCH01:PCH00)
     FWMCDL     IF   E           K DISK    usropn rename(pcd01:pcd00)
     FWTGTL     IF   E           K DISK    usropn
     FWDF2L     IF   E           K DISK    usropn
     FWDELL     IF   E           K DISK    usropn
     FWMIDCQTL  IF   E           K DISK    usropn
     FWMRHL1    IF   E           K DISK    usropn
     FWMBYL     IF   E           K DISK    usropn
     FWMLLL     IF   E           K DISK    usropn
     FWMBNL     IF   E           K DISK    usropn
     FWMFNL     IF   E           K DISK    usropn
     FWMRPL4    IF   E           K DISK    usropn
     FWMAWL3    IF   E           K DISK    usropn
     FWTDDL2    IF   E           K DISK    usropn
     FSTSTL3    IF   E           K DISK    usropn
     FWTABL     IF   E           K DISK    usropn ignore(pat01)
     F                                     ignore(pba01)
     F                                     ignore(pbc01)
     F                                     ignore(pb701)
     F                                     ignore(pca01)
     F                                     ignore(pcc01)
     F                                     ignore(pcn01)
     F                                     ignore(pcs01)
     F                                     ignore(phy01)
     F                                     ignore(pid01)
     F                                     ignore(pis01)
     F                                     ignore(plr01)
     F                                     ignore(pna01)
     F                                     ignore(pnc01)
     F                                     ignore(ppa01)
     F                                     ignore(ppc01)
     F                                     ignore(prc01)
     F                                     ignore(prs01)
     F                                     ignore(psp01)
     F                                     ignore(psy01)
     F                                     ignore(ptr01)
     F                                     ignore(ptt01)
     F                                     ignore(pvs01)
      *** Beg Add *** 01/14/25 *** WC2995 *****************************
     FWMWHL     IF   E           K DISK    usropn
      *** End Add *** 01/14/25 *** WC2995 *****************************
      *** Beg Add *** 04/28/25 *** 118922 *****************************
     FWMRSL     IF   E           K DISK    usropn
      *** End Add *** 04/28/25 *** 118922 *****************************
     D/copy WPRIDC
     D psds          esds                  ExtName(szq1p)
     D TRDS1         E DS                  EXTNAME(WMIDCTR)
     D TRDS3         E DS                  EXTNAME(WMIDCTRL3) prefix(x)
     D QTDS1         E DS                  EXTNAME(WMIDCQT)
     D QTDS3         E DS                  EXTNAME(WMIDCQTL3) prefix(x)
     D                 DS
     D  RHS                    1     90  2 DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2

     D                 DS
     D  RHR                    1     20    DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20

     D KeyST           DS
     D  kkState                             INZ Like(STkey)
     D xxAmt           S              9  0 INZ
     D x2Amt           S              9  2 INZ

      *================================================================
      * GetIdcState - Gets the IDC state code
      *  Parameters - 2a  State code
      *  Returns    - 2a  IDC state code
      *================================================================
     PGetIdcState      B                   Export
     DGetIdcState      PI             2
     D qqCo#                          3  0 Const
     D qqState                        2    Const

     D kkTbId          S              4    INZ('IJST')
     D kkTbKey         S             10    INZ
     D strIdcState     S              2    INZ

      /Free
       If not %Open(WTGTL);
          Open WTGTL;
       Endif;

       kkTbKey = qqState;
       Chain (qqCo#:kkTbId:kkTbKey) PGT01;
       If %found(WTGTL);
          strIdcState =  %subst(GTcf01:1:2);
       Else;
         strIdcState = *blanks;
       Endif;

       If %Open(WTGTL);
          Close WTGTL;
       Endif;

       Return strIdcState;
      /End-Free
     PGetIdcState      E
      *================================================================
      * WriteTxLog  - Write the NCCI IDC transactional log record
      *  Parameters - 9,0 Payment file RRN
      *  Returns    - 1,0 Ending code
      *================================================================
     PWriteTxLog       B                   Export
     DWriteTxLog       PI
     D qqRrn                          9  0 Const
     D kkBR2           S             10A   inz
      *** Begin Delete *** 06/11/20 ***************************
     D*strCaP#         S             25A   inz
     D*intPos          S              5  0 INZ
      *** End   Delete *** 06/11/20 ***************************
     D intToday70      S              7  0 INZ
      /Free
       //Open Files
       OpenIdcFiles();

       //Today's date
       intToday70 = %dec(%date():*cymd);

       //Retrieve Payment and Write IDC transactional record
       Chain qqRrn PPA01;

       If %found(WDPAP) and PAretp = 'C';

          Chain (PAco#:PAfnd:PAfyr:PAcase) PCM01;
          If %found(WMCML1) and IsIdcState(CMco#:CMjrst)= *On and CMLTFL = 'I';
             Clear PTR01;
             //Initialize numeric fields
             TRtrnId    = *BLANKS;
             TRpolId    = *BLANKS;
             TRclmId    = *Blanks;

             TRbenOfCd  = '1';
             TRbenOfAmt = *zeros;
             TRwkBenAmt = *zeros;
             TRtrnAmt   = *zeros;
             TRbchId    = *Zeros;

             //Processing fields
             TRrecTyCd = '01';      //Transactional
             TRtrnCd = '01';
             TRtrnDate = %Char(%date(PAtrdt:*cymd):*iso0);
             TRtrnId = *blanks;

             //Key fields
             //Carrier Code
             Chain (PAco#:PAfnd) PF201;
             If %found(WDF2L);
                TRcarCd = %Editc(F2car#:'X');
             Endif;

             //Policy Number
             Chain (CMco#:CMfnd:CMfyr:CMemp#:CMdiv) PRP01;
             If %found(WMRPL4) and RPcap# <> *blanks;

                //** Begin Add *** 06/11/20 ****************************
                TRpolId = FormatPolId(RPcap#);
                //** End   Add *** 06/11/20 ****************************

                //*** Begin Delete *** 06/11/20 ************************
                //Remove hyphens
                //strCaP# = RPcap#;
                //IntPos = %scan('-':strCaP#);
                //If IntPos > *zero;
                //   Dou IntPos = *zero;
                //      strCaP# = %subst(strCaP#:1:IntPos-1) +
                //                %subst(strCaP#:IntPos+1:25-IntPos);
                //      IntPos = %scan('-':strCaP#);
                //   Enddo;
                //
                //   TRpolId = strCaP#;
                //Endif;
                //*** End   Delete *** 06/11/20 ************************
             Else;
                TRpolId = %Editc(CMfnd:'X') +
                %Editc(CMemp#:'X') + %Editc(CMfyr:'X');
             Endif;

             //Policy Effective Date
             Chain (CMco#:CMfnd:CMEmp#:CMdiv:CMfyr) PEL01;
             If %found(WDELL);
                TRpolEfDt =  %Char(%date(ELpfdt:*cymd):*iso0);
             Endif;

             //Claim Number Identifier
             TRclmId = %Editc(CMcase:'X'); //Like WCSTAT, no fund.

             //Accident Date
             TRacdt = %Char(%date(CMacdt:*cymd):*iso0);

             //Transactional data
             TRjrStCd = GetIdcState(CMco#:CMjrst);

             If TRjrstCd = *blanks;
                kkState = CMjrst;
                chain %KDS(KeySt) STSTL3;
                If %found(STSTL3);
                    TRjrstCd = STnst;
                Else;
                   TRjrstCd = '00';
                Endif;
             Endif;

             TRtrnFrDt =  %Char(%date(PAfrdt:*cymd):*iso0);
             TRtrnToDt =  %Char(%date(PAtodt:*cymd):*iso0);

             //Benefit Type Code
             kkBR2 = PAbr2;
             Chain (PAco#:kkBR2) PB201;
             If %found(WTABL);
                TRbenTyCd  = B2ibtc;
             Endif;

             //*** Begin Change *** 09/01/20 ***********************************
             //If PAtype = 'R' or PAtype ='S' or PAtype = 'V';
             If (PAtype = 'R' and B2ibtc <> '30')  or
                (PAtype ='S' or PAtype = 'V');
             //*** End Change *** 09/01/20 *************************************
                PAamt = PAamt * -1;
             Endif;

             If PAamt >= *Zeros;
                 %Subst(TRtrnAmt:4) = %Editc(PAamt:'X');
             Else;
                 x2Amt = PAamt * -1;
                 %Subst(TRtrnAmt:4) = %Editc(x2Amt:'X');
                 %subst(TRtrnAmt:1:1) = '-';
             Endif;


             //Lump Sum Indicator
             If PAlmps = *blanks;
                TRlmpSmInd = 'N';
             Else;
                TRlmpSmInd = PAlmps;
             Endif;

             //Benefit Offset Code
             Setll (CMco#:CMfnd:CMfyr:CMcase) PBN01;
             If %equal(WMBNL);
                Reade (CMco#:CMfnd:CMfyr:CMcase) PBN01;
                Dow not %eof(WMBNL);

                   If BNstat = 'A';
                      If (PAtrdt >= BNstdt) and  //Within Range
                         (PAtrdt <= BNendt or BNendt = *Zeros);
                         Select;
                         When %subst(BNacod:1:1) = 'S';
                            TRbenOfCd  = '2';
                         When %subst(BNacod:1:1) = *blanks;
                            TRbenOfCd  = '1';
                         Other;
                            TRbenOfCd  = '3';
                         Endsl;

             //Benefit Offset Amount
                         %Subst(TRbenOfAmt:3) = %Editc(BNamt:'X');
                         Leave;
                      Endif;
                   Endif;

                  Reade (CMco#:CMfnd:CMfyr:CMcase) PBN01;
                Enddo;
             Endif;

             //Weekly Benefit Amount
             Setgt (CMco#:CMfnd:CMfyr:CMcase:KKbr2:PAtrdt) PAW01;
             Readpe (CMco#:CMfnd:CMfyr:CMcase:KKbr2) PAW01;
             If not %eof(WMAWL3);
                TRwkBenAmt = %Editc(AWcrt:'X');
             Else;
             //*** Beg Chg *** 08/30/20 ************************************
             //****TRwkBenAmt = %Editc(CMcrt:'X');
                %Subst(TRwkBenAmt:3) = %Editc(CMcrt:'X');
             //*** End Chg *** 08/30/20 ************************************
             Endif;

             TRfiller   = *blanks;

             //Sapiens Key and Maintenance Fields
             TRco#   = PAco#;
             TRfnd   = PAfnd;
             TRfyr   = PAfyr;
             TRcase  = PAcase;
             TRuser  = Q1user;
             TRpgm   = Q1pgm;
             TRtime  = %int(%char(%time():*ISO0));
             TRdt    = %dec(%date():*cymd);
             TRiusr  = Q1user;
             TRipgm  = Q1pgm;
             TRitim  = %int(%char(%time():*ISO0));
             TRidt   = %dec(%date():*cymd);
             Write PTR01;
          Endif;
       Endif;

       //Close Files
       CloseIdcFiles();

      /End-Free
     PWriteTxLog       E
      *================================================================
      * OpenIdcFiles : Open Idc Files
      *  Parameters - None
      *  Returns    - Nothing
      *================================================================
     POpenIdcFiles     B                   Export
     DOpenIdcFiles     PI
      /Free
       If not %Open(WDPAP);
          Open WDPAP;
       Endif;

       If not %Open(WDPAL3);
          Open WDPAL3;
       Endif;

       If not %Open(WMCMP);
          Open WMCMP;
       Endif;

       If not %Open(WMCML1);
          Open WMCML1;
       Endif;

       If not %Open(WMLDL1);
          Open WMLDL1;
       Endif;

       If not %Open(WMRHL1);
          Open WMRHL1;
       Endif;

       If not %Open(WDF2L);
          Open WDF2L;
       Endif;

       If not %Open(WTABL);
          Open WTABL;
       Endif;

       If not %Open(WDELL);
          Open WDELL;
       Endif;

       If not %Open(WMIDCTR);
          Open WMIDCTR;
       Endif;

       If not %Open(WMIDCQT);
          Open WMIDCQT;
       Endif;

       If not %Open(WMCDL);
          Open WMCDL;
       Endif;

       If not %Open(WMCHL);
          Open WMCHL;
       Endif;

       If not %Open(WMBYL);
          Open WMBYL;
       Endif;

       If not %Open(WMLLL);
          Open WMLLL;
       Endif;

       If not %Open(WMBNL);
          Open WMBNL;
       Endif;

       If not %Open(WMFNL);
          Open WMFNL;
       Endif;

       If not %Open(WMRPL4);
          Open WMRPL4;
       Endif;

       If not %Open(WMAWL3);
          Open WMAWL3;
       Endif;

       If not %Open(WTDDL2);
          Open WTDDL2;
       Endif;

       If not %Open(STSTL3);
          Open STSTL3;
       Endif;
      /End-Free
     POpenIdcFiles     E
      *================================================================
      * CloseIdcFiles : Close Idc Files
      *  Parameters - None
      *  Returns    - Nothing
      *================================================================
     PCloseIdcFiles    B                   Export
     DCloseIdcFiles    PI
      /Free

       If %Open(WDPAP);
          Close WDPAP;
       Endif;

       If %Open(WDPAL3);
          Close WDPAL3;
       Endif;

       If %Open(WMCMP);
          Close WMCMP;
       Endif;

       If %Open(WMCML1);
          Close WMCML1;
       Endif;

       If %Open(WMLDL1);
          Close WMLDL1;
       Endif;

       If %Open(WMRHL1);
          Close WMRHL1;
       Endif;

       If %Open(WMIDCTR);
          Close WMIDCTR;
       Endif;

       If %Open(WDF2L);
          Close WDF2L;
       Endif;

       If %Open(WTABL);
          Close WTABL;
       Endif;

       If %Open(WDELL);
          Close WDELL;
       Endif;

       If %Open(WMIDCQT);
          Close WMIDCQT;
       Endif;

       If %Open(WMCDL);
          Close WMCDL;
       Endif;

       If %Open(WMCHL);
          Close WMCHL;
       Endif;

       If %Open(WMBYL);
          Close WMBYL;
       Endif;

       If %Open(WMLLL);
         Close WMLLL;
       Endif;

       If %Open(WMBNL);
          Close WMBNL;
       Endif;

       If %Open(WMFNL);
          Close WMFNL;
       Endif;

       If %Open(WMRPL4);
          Close WMRPL4;
       Endif;

       If %Open(WMAWL3);
          Close WMAWL3;
       Endif;

       If %Open(WMIDCTRL3);
          Close WMIDCTRL3;
       Endif;

       If %Open(WMIDCQTL3);
          Close WMIDCQTL3;
       Endif;

       If %Open(WTDDL2);
          Close WTDDL2;
       Endif;

       If %Open(STSTL3);
          Close STSTL3;
       Endif;
      /End-Free
     PCloseIdcFiles    E
      *================================================================
      * IsIdcState -  Checks if State reports IDC.
      *  Parameters - 2a  State code
      *  Returns    - 2a  IDC state code
      *================================================================
     PIsIdcState       B                   Export
     DIsIdcState       PI             1n
     D qqCo#                          3  0 Const
     D qqState                        2    Const

     D kkTbId          S              4    INZ('IJST')
     D kkTbKey         S             10    INZ
     D IsIdcState      S              1n   INZ(*Off)

      /Free
       If not %Open(WTGTL);
          Open WTGTL;
       Endif;

       kkTbKey = qqState;
       Chain (qqCo#:kkTbId:kkTbKey) PGT01;
       If %found(WTGTL) and %subst(GTcf01:1:2) <> *blanks;
          IsIdcState =  *On;
       Endif;

       If %Open(WTGTL);
          Close WTGTL;
       Endif;

       Return IsIdcState;
      /End-Free
     PIsIdcState       E
      *================================================================
      * WriteQtLog  - Write the NCCI IDC Quaterly log record
      *  Parameters - 9,0 Payment file RRN
      *  Returns    - 1,0 Ending code
      *================================================================
     PWriteQtLog       B                   Export
     DWriteQtLog       PI
     D qqRrnQt                        9  0 Const
     D qqDate                         8    Const
     D strTrnDate      S              4
     D strKey          S             10    Inz
     D intStsCnt       S              5  0 Inz
     D Idx             S              5  0 Inz
     D intQtrEnd       S              7  0 Inz
      *** Begin Add *** 05/12/20 ***************************
     D qqDate70        S              7  0 Inz
      *** End   Add *** 05/12/20 ***************************
     D intIpdTdt       S              9  2 Inz
     D intMpdTdt       S              9  2 Inz
     D intLpdTdt       S              9  2 Inz
     D intAmt          S              9  2 Inz
     D intRcvAmt       S              9  2 Inz
     D intIrsv         S              9  2 Inz
     D intMrsv         S              9  2 Inz
     D intALAErsv      S              9  2 Inz
     D intIincur       S              9  2 Inz
     D intMincur       S              9  2 Inz
     D intIrecov       S              9  2 Inz
     D intMrecov       S              9  2 Inz
     D intALAE         S              9  2 Inz
     D intDedIrec      S              9  2 Inz
     D intDedMrec      S              9  2 Inz
     D intDedALAErec   S              9  2 Inz
     D Date8A          S              8A
     D isLrgD          S               N   inz(*off)
      *** Begin Delete *** 06/11/20 ************************
     D*strCaP#         S             25A   inz
     D*intPos          S              5  0 INZ
      *** End   Delete *** 06/11/20 ************************
     D kkdcde          s                   like(eldcde) Inz
     D kkdcef          s                   like(eldcef) Inz
      /Free
       //Open Files
       OpenIdcFiles();

       //Retrieve Claim Master and Write IDC transactional record
       Chain qqRrnQt PCM00;

       If %found(WMCMP);
          Clear PQT00;
          //Initialize numeric fields
          QTcarCd    = *zeros;
          QTpolEfDt  = *zeros;
          QTclmId    = *zeros;
          QTbthYear  = *zeros;
          QThrDate   = *zeros;
          QTclDate   = *zeros;
          QTreoDate  = *zeros;
          QTmmiDate  = *zeros;
          QTrptInsDt = *zeros;
          QTimpPerC  = *zeros;
          QTdisloEcp = *zeros;
          QTpreDisp  = *zeros;
          QTpartCd  = *zeros;
          QTnatrCd  = *zeros;
          QTcausCd  = *zeros;
          QTlossCD  = *zeros;
          QTsettCD  = *zeros;
          QTtmpDisCd = *Zeros;
          QTindPdTdt = *Zeros;
          QTmedPdTdt = *blanks;
          QTincInAmt = *Zeros;
          QTincMeAmt = *Zeros;
          QTempLgAmt =  *Zeros;
          QTallLosPd =  *Zeros;
          QTprInjAww =  *Zeros;

      *** Beg Add *** 01/14/25 *** WC2995 *****************************
          QTclScd    = *Zeros;
          QTzipInst  = *Zeros;
          QTdependt  = *Zeros;
          QTexpSt    = *Zeros;
          QTindClmCd = *Zeros;
          QTrtWd     = *Zeros;
          QTfiller   = *Blanks;

      *** Beg Chg *** 11/15/25 ********************************
       // QTclScd    = %editc(CMClas:'X');
          QTclscd = %Subst(%editc(CMclas:'X'):2:4);
      *** End Chg *** 11/15/25 ********************************
          QTzipInst  = %editc(CMacZ1:'X') + %editc(CMacZ2:'X');
          QTindClmCd = CMindClmCd;
      *** Beg Add *** 11/15/25 ********************************
          If QTindClmCd = ' ';
             QTindClmCd = '0';
          EndIf;
      *** End Add *** 11/15/25 ********************************

          If CM#Dep > 20;
             QTdependt  = '20';
          Else;
             QTdependt  = %Subst(%editc(CM#Dep:'X'):2:2);
          EndIf;

          If not %Open(WMWHL);
             Open WMWHL;
          Endif;

          SetGt (CMCo#:CMFnd:CMFyr:CMCase:9999999:'R') WMWHL;
          Readpe (CMCo#:CMFnd:CMFyr:CMCase) WMWHL;
          Dow Not %Eof(WMWHL);
              If WHWKST = 'R';
                 QTrtWd = %Char(%Date(%Char(WHdate):*CYMD0):*ISO0);
                 Leave;
              EndIf;
              Readpe (CMCo#:CMFnd:CMFyr:CMCase) WMWHL;
          EndDo;

          If %Open(WMWHL);
             Close WMWHL;
          Endif;
      *** End Add *** 01/14/25 *** WC2995 *****************************

          QTrecTyCd = '02'; //Quaterly
          QTtrnDate = %Subst(qqDate:5:4) + %Subst(qqDate:1:2) +
                     %Subst(qqDate:3:2);

          //Key Elements

          //Carrier Code
          Chain (CMco#:CMfnd) PF201;
          If %found(WDF2L);
             QTcarCd = %Editc(F2car#:'X');
          Endif;

          //Policy Number
          Chain (CMco#:CMfnd:CMfyr:CMemp#:CMdiv) PRP01;
          If %found(WMRPL4) and RPcap# <> *blanks;
             //** Begin Add *** 06/11/20 ****************************
             QTpolId = FormatPolId(RPcap#);
             //** End   Add *** 06/11/20 ****************************

             //*** Begin Delete *** 06/11/20 **************************
             //Remove hyphens
             //strCaP# = RPcap#;
             //IntPos = %scan('-':strCaP#);
             //If IntPos > *zero;
             //   Dou IntPos = *zero;
             //      strCaP# = %subst(strCaP#:1:IntPos-1) +
             //                %subst(strCaP#:IntPos+1:25-IntPos);
             //      IntPos = %scan('-':strCaP#);
             //   Enddo;
             //
             //   QTpolId = strCaP#;
             //Endif;
             //*** End   Delete *** 06/11/20 **************************
          Else;
             //*** Begin change *** 12/15/20 *********************//
             //* QTpolId = %Editc(CMfnd:'X') +
             //*    %Editc(CMemp#:'X') + %Editc(CMdiv:'X');
             QTpolId = %Editc(CMfnd:'X') +
                       %Editc(CMemp#:'X') + %Editc(CMfyr:'X');
             //*** End   change *** 12/15/20 *********************//
          Endif;

          //Policy Effective Date
          Chain (CMco#:CMfnd:CMEmp#:CMdiv:CMfyr) PEL01;
          If %found(WDELL);
             QTpolEfDt =  %Char(%date(ELpfdt:*cymd):*iso0);
          Endif;

          //Claim Number
          QTclmId =  %Editc(CMcase:'X');

          //Accident Date
          QTacDt =  %Char(%date(CMacDt:*cymd):*iso0);

          //Quaterly Data Elements
          QTjrStCd = GetIdcState(CMco#:CMjrst);

          If QTjrstCd = *blanks;
             kkState = CMjrst;
             chain %KDS(KeySt) STSTL3;
             If %found(STSTL3);
                 QTjrstCd = STnst;
             Else;
                QTjrstCd = '00';
             Endif;
          Endif;

          If QTjrstCd = *blanks;
             QTjrstCd = '00';
          Endif;

      *** Beg Add *** 01/14/25 *** WC2995 *****************************
          QTexpSt = QTjrstCd;
      *** Beg Add *** 04/28/25 *** 118922 *****************************

          If Not %Open(WMRSL);
             Open WMRSL;
          Endif;

          Chain (CMco#:CMfnd:CMEmp#:CMdiv:CMfyr) PEL01;
          If %Found(WDELL);
             Chain (CMco#:ELRtCd) PRS01;
               If %Found(WMRSL);
               //*** Beg Chg *** 10/15/25 ********************************
               //*** QTexpSt = RSJrSt;
                  QTexpSt = GetIdcState(RSco#:RSJrSt);
               //*** End Chg *** 10/15/25 ********************************
               EndIf;
          Endif;

          If %Open(WMRSL);
             Close WMRSL;
          Endif;
      *** End Add *** 04/28/25 *** 118922 *****************************
      *** End Add *** 01/14/25 *** WC2995 *****************************

          Chain CMss# PCD00;
          If %Found(WMCDL);
             Select;
             When CDsex = 'M';
                QTclmGend = '1';
             When CDsex = 'F';
                QTclmGend = '2';
             Other;
                QTclmGend = '0';
             Endsl;

             If CDdtOb > *zeros;
                QTbthYear =  %Char(%date(CDdtOb:*cymd):*iso0);
             Endif;
          Endif;

          If CMhiDt > *zeros;
             QThrDate =  %Char(%date(CMhiDt:*cymd):*iso0);
          Endif;

          If CMeSts = *blanks;
             QTempStCd = *blanks;
          Else;
          // Use cross reference flag for Employment Status if populated
             If not %Open(WTGTL);
                Open WTGTL;
             Endif;

             Chain (CMco#:'EMST':CMeSts) pgt01;
             If %Found(wtgtl) and GTfg04 <> ' ';
                QTempStCd = GTfg04;
             Else;
                Select;
                When CMests = 'FT';                              //Full time
                   QTempStCd = '1';
                When CMests = 'PT';                              //Part time
                   QTempStCd = '2';
                When CMests = 'SL';                              //Seasonal
                   QTempStCd = '8';
                When CMests = 'VO';                              //Volunteer
                   QTempStCd = '9';
                Other;                                           //Other
                   QTempStCd = 'X';
                Endsl;
             Endif;

             If %Open(WTGTL);
                Close WTGTL;
             Endif;
          Endif;

          //Get closed and re-open dates

          //*** Begin Add *** 05/12/20 ********************************
          qqDate70 = %dec(%char(%date(qqDate:*usa0):*cymd0):7:0);
          //*** End   Add *** 05/12/20 ********************************

          Setll (CMco#:CMfnd:CMfyr:CMcase) PCH00;
          Reade (CMco#:CMfnd:CMfyr:CMcase) PCH00;
          intStsCnt = *Zeros;

          //*** Begin Change *** 05/12/20 *****************************
          //Dow not %eof(WMCHL);
          Dow not %eof(WMCHL) and CHefdt <= qqDate70;
          //*** End Change *** 05/12/20 *******************************
             Date8a = *Zeros;
             If CHefdt <> *Zeros;
                Date8a = %Char(%date(CHefdt:*cymd):*iso0);
             Endif;

             Select;
             When CHocSt='O' and intStsCnt = *Zeros;
                intStsCnt = 1;
             When CHocSt='O' and intStsCnt = 1;
                QTreoDate = Date8a;
             When CHocst='C';
                QTclDate = Date8a;
             EndSl;

             Reade  (CMco#:CMfnd:CMfyr:CMcase) PCH00;
          Enddo;

          If CMmmi > *zeros;
             QTmmiDate  = %Char(%date(CMmmi:*cymd):*iso0);
          Endif;

          If CMdtrp > *Zeros;
             QTrptInsDt =  %Char(%date(CMrcvD:*cymd):*iso0);
          Endif;

      *** Beg Chg *** 04/28/25 *** 118922 *****************************
      *** If CMidcAcs <> *blanks;
      ***    QTaccStCd  = CMidcAcs;
      *** Else;
      ***    QTaccStCd =  GetIdcState(CMco#:CMjrst);
      *** Endif;

          Select;
          When CMidcAcs <> *Blanks;
          //*** Beg Chg *** 10/15/25 ********************************
          //*** QTaccStCd  = CMidcAcs;
             QTaccStCd =  GetIdcState(CMco#:CMidcAcs);
          //*** End Chg *** 10/15/25 ********************************
          When CMAST <> *Blanks;
          //*** Beg Chg *** 10/15/25 ********************************
          //*** QTaccStCd  = CMaSt;
             QTaccStCd =  GetIdcState(CMco#:CMast);
          //*** End Chg *** 10/15/25 ********************************
          Other;
             QTaccStCd =  GetIdcState(CMco#:CMjrst);
          Endsl;

      *** End Chg *** 04/28/25 *** 118922 *****************************

          If QTaccStCd = *blanks;
             QTaccStCd = '00';
          Endif;

          Chain (CMco#:CMfnd:CMfyr:CMcase) WMLLL;
          If %found(WMLLL);
             If LLplaA > *Zeros or LLplaN > *blanks;
                QTrepInd   = 'Y';
             Else;
                QTrepInd = 'N';
             Endif;
          Else;
             QTrepInd = 'N';
          Endif;

          Select;
         //*** Begin Change 09/21/20 *************************************
         // When CMwMth = '1';
         //   QTmethWWG = CMwMth;
          When CMwMth = '1' or CMwMth = '2';
             QTmethWWG = '1';
         //*** End   Change 09/21/20 *************************************
          When CMwMth = '3';
             QTmethWWG = '2';
          When CMwMth = '4';
             QTmethWWG = '3';
          Other;
             QTmethWWG = '0';
          Endsl;

          Setll (CMco#:CMfnd:CMfyr:CMcase) WMBYL;
          Select;
          When CMipct > *zero;
             QTimpBasCd = '1';
          //*** Begin Add 05/11/20 ***************************
             QTimpPerc  =  %Subst(%Editc(CMipct:'X'):1:3);
          //*** End   Add 05/11/20 ***************************
          When %equal(WMBYL);
             QTimpBasCd = '2';
          //*** Begin Add 11/16/22 ***************************
             Chain (CMco#:CMfnd:CMfyr:CMcase) PBY01;
          //*** End   Add 11/16/22 ***************************
          //*** Begin Add 05/11/20 ***************************
             QTimpPerc  =  %Subst(%Editc(BYpct:'X'):1:3);
          //*** End   Add 05/11/20 ***************************
          Other;
             QTimpBasCd = '0';
          Endsl;

          //*** Begin Delete 05/11/20 ***************************
          //QTimpPerc  =  %Subst(%Editc(CMipct:'X'):1:3);
          //*** End Delete 05/11/20 ***************************

          If isLOEC(CMco#:CMjrst) = *On;
             QTdisLOECp =  %Subst(%Editc(CMloec:'X'):1:3);
          Endif;

          QTpreDisp  =  %Subst(%Editc(CMpedis:'X'):1:3);

          QTpartCd = CMpart;
          QTnatrCd = CMnatr;
          QTcausCd = CMcaus;

          QTlossCD  = %Editc(CMlossCd:'X');      //Act-Loss code

          If %found(WMLLL);
             If LLmtst <> *blanks;
                QTsettCD = LLmtst;               //Type of Settlement - Loss Con
             Endif;
             QTmedExtIn = LLmdex;
          //*** Begin Add 08/31/20 *************************************
          Else;
             QTmedExtIn = *Blank;
          //*** End   Add 08/31/20 *************************************
          Endif;

          //*** Begin Delete *** 08/31/20 *********************************
          // Blanks is OK when not applicable or unknown.
          //If QTmedExtIn = *blanks;
          //   QTmedExtIn = 'N';
          //Endif;
          //*** End   Delete *** 08/31/20 *********************************

          If CMtdbe <> *blanks;
             QTtmpDisCd = CMtdbe;
          Endif;

          Exsr GetPayIncur;
          Eval(H) xxAmt = intIpdTdt;                   //Indemnity Payments
          Select;
          When xxAmt > *Zeros;
             QTindPdTdt = %Editc(xxAmt:'X');
          When xxAmt < *Zeros;
             xxAmt = xxAmt * -1;
             QTindPdTdt = %Editc(xxAmt:'X');
             %subst(QTindPdTdt:1:1) = '-';
          Other;
             QTindPdTdt = *Zeros;
          Endsl;

          Eval(H) xxAmt = intMpdTdt;                  //Medical Payments
          Select;
          When xxAmt > *Zeros;
             QTmedPdTdt = %Editc(xxAmt:'X');
          When xxAmt < *Zeros;
             xxAmt = xxAmt * -1;
             QTmedPdTdt = %Editc(xxAmt:'X');
             %subst(QTmedPdTdt:1:1) = '-';
          Other;
             QTmedPdTdt = *Zeros;
          Endsl;

          Eval(H) xxAmt = intIincur;                  //Indemnity Incurred
          Select;
          When xxAmt > *Zeros;
             QTincINamt = %Editc(xxAmt:'X');
          When xxAmt < *Zeros;
             xxAmt = xxAmt * -1;
             QTincINamt = %Editc(xxAmt:'X');
             %subst(QTincINamt:1:1) = '-';
          Other;
             QTincINamt = *Zeros;
          Endsl;

          Eval(H) xxAmt = intMincur;                  //Medical Incurred
          Select;
          When xxAmt > *Zeros;
             QTincMEamt = %Editc(xxAmt:'X');
          When xxAmt < *Zeros;
             xxAmt = xxAmt * -1;
             QTincMEamt = %Editc(xxAmt:'X');
             %subst(QTincMEamt:1:1) = '-';
          Other;
             QTincMEamt = *Zeros;
          Endsl;


          Eval(H) xxAmt = intLpdTdt;
          QTEMPLGAMT = %Editc(xxAmt:'X');            //Employer Legal Amt Paid
          Eval(H) xxAmt = intALAE;
          QTALLLOSPD = %Editc(xxAmt:'X');            //Alloc Loss Adj Exp Paid

          QTPRINJAWW = %Subst(%Editc(%DECH(CMprww:5:0):'X'):1:5);

         //Sapiens Key Fields
          QTco#   = CMco#;
          QTfnd   = CMfnd;
          QTfyr   = CMfyr;
          QTcase  = CMcase;

          QTpYr = %int(%Subst(qqDate:5:4));

          Select;
          When %Subst(qqDate:1:2) = '03' and %Subst(qqDate:3:2) = '31';
             QTpQtr =  1;
          When %Subst(qqDate:1:2) = '06' and %Subst(qqDate:3:2) = '30';
             QTpQtr =  2;
          When %Subst(qqDate:1:2) = '09' and %Subst(qqDate:3:2) = '30';
             QTpQtr =  3;
          When %Subst(qqDate:1:2) = '12' and %Subst(qqDate:3:2) = '31';
             QTpQtr =  4;
          Endsl;

          QTbchId = *zeros;
       Endif;


       If QTindPdTdt <> *Zeros or QTincInAmt <> *Zeros or
          QTmedPdTdt <> *zeros or QTincMeAmt <> *Zeros;
          QTuser  = Q1user;                       //Sapiens Maintenance Fields
          QTpgm   = Q1pgm;
          QTtime  = %int(%char(%time():*ISO0));
          QTdt    = %dec(%date():*cymd);
          QTiusr  = Q1user;
          QTipgm  = Q1pgm;
          QTitim  = %int(%char(%time():*ISO0));
          QTidt   = %dec(%date():*cymd);
          Write PQT00;
       Endif;
       //Close Files
       CloseIdcFiles();
       //------------------------------------------------------------------
       //  Get Payments, reserves, incurred                               *
       //------------------------------------------------------------------
       Begsr GetPayIncur;
       //Get Fund
       chain (CMco#:CMfnd) WMFNL;
       If %found(WMFNL);
       Else;
          Clear pfn01;
       Endif;

       //Get Qtr end date
       intQtrEnd = %dec(%char(%date(qqDate:*usa0):*cymd0):7:0);

       //Process Payments
       Setll (CMco#:CMfnd:CMfyr:CMcase) PPA03;
       Reade (CMco#:CMfnd:CMfyr:CMcase) PPA03;

       //Initialize summary fields
       intIpdTdt = *Zeros;
       intMpdTdt = *zeros;
       intLpdTdt = *Zeros;
       intALAE   = *Zeros;

       intIrecov = *Zeros;
       intMrecov = *Zeros;

       intDedIrec = *Zeros;
       intDedMrec = *Zeros;

       Dow not %eof(WDPAL3);
          If PAtrdt <= intQtrEnd;         //To date

             //Change sign
             If PAtype = 'V' or PAtype ='S' or PAtype = 'R';
                intAmt = PAamt * -1;
             Else;
                intAmt = PAamt;
             Endif;

             //Accumulate Recoveries;
             If PAtype = 'R';
                Select;
                When PAtrcd = 'MR';
                   intMrecov = intMrecov - intAmt;
                When PAtrcd = 'CR';
                   intIrecov = intIrecov - intAmt;
                Endsl;
             Endif;

             //Calculate Total Paid

             If PAbr2 <> FNmep1 and            //Exclude Ded Recoveries
                PAbr2 <> FNcop1 and
                PAbr2 <> FNexp1 and
                PAbr2 <> FNlep1 or
                PAbr2 = *blanks;

                Select;
                When PAretp = 'C';
                   intIpdTdt = intIpdTdt + intAmt;
                When PAretp = 'M';
                   intMpdTdt = intMpdTdt + intAmt;
                When PAretp = 'E' or PAretp = 'L';
                   strKey = PArecd;
                   Chain (CMco#:strKey) PRT01;
                   If %found(WTABL) and RTCTGY <> '1B';
                      intALAE   = intALAE + intAmt;         //Allocated Loss Adj
                   Else;
                      intIpdTdt = intIpdTdt + intAmt;
                   Endif;
                   If PAretp = 'L';
                      intLpdTdt = intLpdTdt + intAmt;
                   Endif;
                Endsl;

               //Check for Legal Amount  Pending:Should we also check PAretp='L'
               // strKey = PAbr2;
               // Chain (CMco#:strKey) pb201;
               //  If %found and (B2IBTC = '30' or B2IBTC = '31');
               //    intLpdTdt = intLpdTdt + intAmt;
               // Endif;


              //Process Ded Recoveries
             Else;
                Select;
                When PAretp = 'C';
                   intDedIrec = intDedIrec - intAmt;
                When PAretp = 'M';
                   intDedMrec = intDedMrec - intAmt;
                When PAretp = 'E' or PAretp = 'L';
                   strKey = PArecd;
                   Chain (CMco#:strKey) PRT01;
                   If %found(WTABL) and RTCTGY <> '1B';
                      intDedALAErec = intDedALAErec - intAmt;  //Ded ALAE Recove
                   Else;
                      intDedIrec = intDedIrec - intAmt;  //Ded Indemnity Recover
                   Endif;
                Endsl;
             Endif;
          Endif;

          Reade (CMco#:CMfnd:CMfyr:CMcase) PPA03;
       Enddo;

       //Get Reserves
       Setll (CMco#:CMfnd:CMfyr:CMcase) PRH01;
       Reade (CMco#:CMfnd:CMfyr:CMcase) PRH01;

       Dow not %eof(WMRHL1);
          If RHefdt <= intQtrEnd;         //To date
             For idx = 1 to 10;
                strKey = RHR(idx);
                Chain (CMco#:strKey) PRT01;
                If %found;
                   Select;
                   When RTtype = 'C';
                      intIrsv = intIrsv + RHS(idx);
                   When RTtype = 'M';
                      intMrsv = intMrsv + RHS(idx);
                   When RTtype = 'E' or RTtype = 'L';
                      If RTctgy <> '1B';
                         intALAErsv = intALAErsv + RHS(idx);
                      Else;
                         intIrsv = intIrsv + RHS(idx);
                      Endif;
                   Endsl;
                Endif;
             Endfor;
          Endif;

          Reade (CMco#:CMfnd:CMfyr:CMcase) PRH01;
       Enddo;


      //Deductible recoveries
       If CMefdt <=IntQtrEnd;

          isLrgD = *Off;
          Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) WDELL;
          If %found(WDELL);
             strkey = %Editc(ELbscd:'X');
             kkdcde = ELdcde;
             kkdcef = ELdcef;
             Chain (CMco#:strKey) PBS01;
             If %found(WTABL) and BSlpgm ='L';
                Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) WMLDL1;
                If %found(WMLDL1);
                   isLrgD = *On;
                Endif;
             Endif;
          Endif;

          If isLrgD = *Off;
             setgt (CMco#:CMfnd:CMjrst:kkdcde:kkdcef) WTDDL2;
             readp WTDDL2;
          Endif;

          If (not %eof(WTDDL2) and
              DDstat = 'A' and DDtype <> 'G' and isLrgD = *Off) or
             (isLrgD = *On and LDtyp1 <> 'G');
             intIrsv = intIrsv + intDedIrec;
             intMrsv = intMrsv + intDedMrec;
          Endif;

       //Override previously calculated Indemnity Incurred/Reserve/Payments
          Setgt (CMco#:CMfnd:CMfyr:CMcase:intQtrEnd) WMCHL;
          Readpe (CMco#:CMfnd:CMfyr:CMcase) WMCHL;

          If CMcstf <> *Zeros;
             intIpdTdt = CMcstf;
             If not %eof(WMCHL);
                If CHocst = 'C';
                   IntIrsv = CMcstf;
                Endif;
             Endif;
          Endif;

          If cmmstf <> 0;
             intMpdTdt = cmmstf;
             If chocst = 'C';
                intMrsv = cmmstf;
             Endif;
          Endif;


          //CA adjustment
          chain (CMco#:CMfnd:CMfyr:CMcase) WMLLL;
          If not %found(WMLLL);
             clear PLL01;
          Endif;

          If CMjrst = 'CA' and (CMlsrv = '03' or CMlsrv = '05' or
                                CMlsrv = '06' or CMncci = '08' or
                                LLfrau = '01');
             IntIrsv = IntIrsv + IntIrecov;
             IntMrsv = IntMrsv + IntMrecov;
          Endif;

       //Get Incurred
       intIincur = intIrsv;
       intMincur = intMrsv;

       Endif;

       Endsr;

      /End-Free
     PWriteQtLog       E
      *================================================================
      * IsQtLogFnd    - Checks if a Quarterly IDC Log record is found.
      *  Parameters   - Co#, Fund, Fund year, Case, Processing Yr,
      *                 Processing Qtr.
      *  Returns      - '0' if found, '1' if not found.
      *================================================================
     PIsQtLogFnd       B                   Export
     DIsQtLogFnd       PI             1n
     D qqCo#                          3  0 Const
     D qqFnd                          3  0 Const
     D qqFyr                          3  0 Const
     D qqCase                         7  0 Const
     D qqPyr                          4  0 Const
     D qqPqtr                         1  0 Const

     D kkCo#           s              3  0
     D kkFnd           s              3  0
     D kkFyr           s              3  0
     D kkCase          s              7  0
     D kkPyr           s              4  0
     D kkPqtr          s              1  0
     D IsQtLogRecFnd   S              1n   INZ(*Off)

      /Free
       If not %Open(WMIDCQTL);
          Open WMIDCQTL;
       Endif;

       kkCo#  = qqCo#;
       kkFnd  = qqFnd;
       kkFyr  = qqFyr;
       kkCase = qqCase;
       kkPyr  = qqPyr;
       kkPqtr = qqPqtr;

       Chain (kkCo#:kkFnd:kkFyr:kkCase:kkPyr:kkPqtr) PQT01;
       If %found(WMIDCQTL);
          IsQtLogRecFnd = *Off;
       Else;
          IsQtLogRecFnd = *On;
       Endif;

       If %Open(WMIDCQTL);
          Close WMIDCQTL;
       Endif;

       Return IsQtLogRecFnd;
      /End-Free
     PIsQtLogFnd       E
      *================================================================
      * GetPrcQtr     -  Gets the processing Quarter period.
      *  Parameters   -  8, date
      *  Returns      -  Processing Quarter, Month and Day
      *================================================================
     PGetPrcQtr        B                   Export
     DGetPrcQtr        PI             5A
     D qqInDate                       8A   Const
     D PrcQtr          S              5A
      /Free
       Select;
       When %subst(qqInDate:1:4) = '0331';
          PrcQtr = '10101';
       When %subst(qqInDate:1:4) = '0630';
          PrcQtr = '20401';
       When %subst(qqInDate:1:4) = '0930';
          PrcQtr = '30701';
       When %subst(qqInDate:1:4) = '1231';
          PrcQtr = '41001';
       Other;
          PrcQtr = *Zeros;
       Endsl;

       Return PrcQtr;
      /End-Free
     PGetPrcQtr        E
      *================================================================
      * GetQtrFrmDt   -  Gets the processing Quarter Beginning Date
      *  Parameters   -  Quater code 1A, Quarter Year
      *  Returns      -  Processing Quarter End Date.
      *================================================================
     PGetQtrFrmDt      B                   Export
     DGetQtrFrmDt      PI             7  0
     D qqQtrCode                      1A   Const
     D qqQtrYr                        4A   Const

     D xxDate8A        S              8A   inz
     D inQtrCode       S              1A   inz
     D inQtrYr         S              4A   inz
     D outFrmQtrDt     S              7  0 inz
      /Free
       inQtrCode = qqQtrCode;
       inQtrYr = qqQtrYr;
       Select;
       When InQtrCode = '1';
          xxDate8A = '0101' + inQtrYr;
          outFrmQtrDt = %dec(%char(%date(xxDate8A:*usa0):*cymd0):7:0);
       When InQtrCode = '2';
          xxDate8A = '0401' + inQtrYr;
          outFrmQtrDt = %dec(%char(%date(xxDate8A:*usa0):*cymd0):7:0);
       When InQtrCode = '3';
          xxDate8A = '0701' + inQtrYr;
          outFrmQtrDt = %dec(%char(%date(xxDate8A:*usa0):*cymd0):7:0);
       When InQtrCode = '4';
          xxDate8A = '1001' + inQtrYr;
          outFrmQtrDt = %dec(%char(%date(xxDate8A:*usa0):*cymd0):7:0);
       Other;
          outFrmQtrDt = *Zeros;
       Endsl;
       Return outFrmQtrDt;
      /End-Free
     PGetQtrFrmDt      E
      *================================================================
      * GetQtrEndDt   -  Gets the processing Quarter End Date.
      *  Parameters   -  Quater code 1A, Quarter Year
      *  Returns      -  Processing Quarter End Date.
      *================================================================
     PGetQtrEndDt      B                   Export
     DGetQtrEndDt      PI             7  0
     D qqQtrCode                      1A   Const
     D qqQtrYr                        4A   Const

     D xxDate8A        S              8A   inz
     D inQtrCode       S              1A   inz
     D inQtrYr         S              4A   inz
     D outEndQtrDt     S              7  0 inz
      /Free
       inQtrCode = qqQtrCode;
       inQtrYr = qqQtrYr;
       Select;
       When InQtrCode = '1';
          xxDate8A = '0331' + inQtrYr;
          outEndQtrDt = %dec(%char(%date(xxDate8A:*usa0):*cymd0):7:0);
       When InQtrCode = '2';
          xxDate8A = '0630' + inQtrYr;
          outEndQtrDt = %dec(%char(%date(xxDate8A:*usa0):*cymd0):7:0);
       When InQtrCode = '3';
          xxDate8A = '0930' + inQtrYr;
          outEndQtrDt = %dec(%char(%date(xxDate8A:*usa0):*cymd0):7:0);
       When InQtrCode = '4';
          xxDate8A = '1231' + inQtrYr;
          outEndQtrDt = %dec(%char(%date(xxDate8A:*usa0):*cymd0):7:0);
       Other;
          outEndQtrDt = *Zeros;
       Endsl;
       Return outEndQtrDt;
      /End-Free
     PGetQtrEndDt      E
      *================================================================
      *  IsLOEC     -  Gets LOEC flag
      *  Parameters - 2a  State code
      *  Returns    - 3a  LOEC
      *================================================================
     PIsLOEC           B                   Export
     DIsLOEC           PI             1n
     D qqCo#                          3  0 Const
     D qqState                        2    Const

     D kkTbId          S              4    INZ('IJST')
     D kkTbKey         S             10    INZ
     D IsLOEC          S              1n   INZ(*OFF)

      /Free
       If not %Open(WTGTL);
          Open WTGTL;
       Endif;

       kkTbKey = qqState;
       Chain (qqCo#:kkTbId:kkTbKey) PGT01;
       If %found(WTGTL) and GTfg01 = 'L';
          IsLOEC =  *On;
       Else;
          IsLOEC = *Off;
       Endif;

       If %Open(WTGTL);
          Close WTGTL;
       Endif;

       Return IsLOEC;
      /End-Free
     PIsLOEC           E
      *
     PTransfer         B                   Export
      *===============================================================
      *  Transfer   - Process transfer for IDC key changes
      *  Parameters - Old IDC key, New IDC key
      *  Returns    - '1'= Successful transfer, '0'= Failed Transfer
      *===============================================================
     DTransfer         PI             1n
     D qqCo#1                         3  0 Const
     D qqCar1                         5    Const
     D qqPolId1                      18    Const
     D qqPolEfDt1                     8    Const
     D qqClmId1                      12    Const
     D qqAcDt1                        8    Const
     D qqCo#2                         3  0 Const
     D qqCar2                         5    Const
     D qqPolId2                      18    Const
     D qqPolEfDt2                     8    Const
     D qqClmId2                      12    Const
     D qqAcDt2                        8    Const
     D qqFyr2                         3  0 Const

     D isReturnCd      S              1n   INZ(*off)
     D TRNamt          S             12  0 INZ
     D isError         S              1n   INZ(*off)
     D kkSts           S                   LIKE(TRsts)
     D svTRfnd         S                   LIKE(xTRfnd) inz
     D svTRfyr         S                   LIKE(xTRfyr) inz
     D svTRcase        S                   LIKE(xTRcase) inz
     D svQTfnd         S                   LIKE(xQTfnd) inz
     D svQTfyr         S                   LIKE(xQTfyr) inz
     D svQTcase        S                   LIKE(xQTcase) inz
      *
      /Free

       //Open Files
       If not %Open(WMIDCTRL3);
          Open WMIDCTRL3;
       Endif;

       If not %Open(WMIDCQTL3);
          Open WMIDCQTL3;
       Endif;

       If not %Open(WMIDCTR);
          Open WMIDCTR;
       Endif;

       If not %Open(WMIDCQT);
          Open WMIDCQT;
       Endif;

       //Load keys

       //*** Beg Add *** 10/14/20 ******************************************
       kkSts = *blank;
       Setll (qqCo#2:qqCar2:qqPolId2:qqPolEfDt2:qqClmid2:qqAcdt2:kkSts)
       WMIDCTRL3;
       If Not %Equal(WMIDCTRL3);
       //*** End Add *** 10/14/20 ******************************************
          //Transfer Transactional Records
          Exsr TrnfTrKey;

          //Transfer Quaterly Records
          If isError = *off;
             Exsr TrnfQtKey;
          Endif;
       //*** Beg Add *** 10/14/20 ******************************************
       Else;
          isReturnCd = *off;
          isError    = *on;
       Endif;
       //*** End Add *** 10/14/20 ******************************************

       //Close Files
       CloseIdcFiles();
       Return isReturnCd;

       //------------------------------------------------------------------
       //  Transfer Transactional data (Update key)                       *
       //------------------------------------------------------------------
       Begsr TrnfTrKey;
       kkSts = *blank;
       Setll (qqCo#1:qqCar1:qqPolId1:qqPolEfDt1:qqClmid1:qqAcdt1:kkSts)
       WMIDCTRL3;
       If %equal(WMIDCTRL3) ;
          Reade (qqCo#1:qqCar1:qqPolId1:qqPolEfDt1:qqClmid1:qqAcdt1:kkSts)
                 WMIDCTRL3;

          svTRfnd   = xTRfnd;                 //Save Go fields
          svTRfyr   = xTRfyr;
          svTRcase  = xTRcase;


          Dow not %eof(WMIDCTRL3);
             xTRsts = 'R';                    //Mark original rec as replaced.
             //**** Beg Add **** 06/06/25 **********************************
             TRuser  = Q1user;
             TRpgm   = Q1pgm;
             TRtime  = %int(%char(%time():*ISO0));
             TRdt    = %dec(%date():*cymd);
             //**** End Add **** 06/06/25 **********************************
             Update PTR03;

             //Write Offsetting record
             TRds1 = TRds3;
             TRsts = 'O';                      //Offset status
             TRbchid = *zeros;                 //Reset Batch Id
             TRtrnDate = %char(%date():*iso0);
             TRNamt = %int(TRtrnAmt);
             TRNamt = TRNamt * -1;             //Negate amount
             If TRNamt >= *zeros;
                TRtrnAmt = %editc(TRNamt:'X');
             Else;
                TRNamt = TRNamt * -1;
                TRtrnAmt = %editc(TRNamt:'X');
                %subst(TRtrnAmt:1:1) = '-';
             Endif;

             //Populate time stamp GO fields
             TRuser  = Q1user;
             TRpgm   = Q1pgm;
             TRtime  = %int(%char(%time():*ISO0));
             TRdt    = %dec(%date():*cymd);
             TRiusr  = Q1user;
             TRipgm  = Q1pgm;
             TRitim  = %int(%char(%time():*ISO0));
             TRidt   = %dec(%date():*cymd);
             Write PTR01;                      //Write negation

             //Write New Record with New keys
             TRds1 = TRds3;                    //Restore original record
             TRsts = *blanks;                  //Reset status
             TRbchid = *Zeros;                 //Reset Batch Id
             TRtrnDate = %char(%date():*iso0);

             TRco#     = qqCo#2;               //Replace with New Key fields
             TRcarCd   = qqCar2;
             TRpolId   = qqPolId2;
             TRpolEfDt = qqPolEfDt2;
             TRClmId   = qqClmId2;
             TRAcDt    = qqAcDt2;
             TRtrnDate = %char(%date():*iso0);

             //Populate time stamp GO fields
             If qqFyr2 > *Zeros;
                TRfyr    = qqFyr2;
             Endif;
             TRcase  = %int(%subst(qqClmId2:1:7));
             TRuser  = Q1user;
             TRpgm   = Q1pgm;
             TRtime  = %int(%char(%time():*ISO0));
             TRdt    = %dec(%date():*cymd);
             TRiusr  = Q1user;
             TRipgm  = Q1pgm;
             TRitim  = %int(%char(%time():*ISO0));
             TRidt   = %dec(%date():*cymd);
             Write PTR01;                      //Write new record with new keys

             Reade (qqCo#1:qqCar1:qqPolId1:qqPolEfDt1:qqClmid1:qqAcdt1:kkSts)
                    WMIDCTRL3;
          Enddo;
          isReturnCd = *on;
       Else;
          isError = *on;
       Endif;
       Endsr;
       //------------------------------------------------------------------
       //  Transfer Quarterly data (Update key)                           *
       //------------------------------------------------------------------
       Begsr TrnfQtKey;
       kkSts = *blank;
       Setll (qqCo#1:qqCar1:qqPolId1:qqPolEfDt1:qqClmid1:qqAcdt1:kkSts)
             WMIDCQTL3;
       If %equal(WMIDCQTL3);
          Reade (qqCo#1:qqCar1:qqPolId1:qqPolEfDt1:qqClmid1:qqAcdt1:kkSts)
                WMIDCQTL3;

          Dow not %eof(WMIDCQTL3);
             xQTsts = 'R';                    //Mark original rec as replaced.
             //**** Beg Add **** 06/06/25 **********************************
             QTuser  = Q1user;
             QTpgm   = Q1pgm;
             QTtime  = %int(%char(%time():*ISO0));
             QTdt    = %dec(%date():*cymd);
             //**** End Add **** 06/06/25 **********************************
             Update PQT03;

             //Write Deletion
             svQTfnd   = xQTfnd;                 //Save Go fields
             svQTfyr   = xQTfyr;
             svQTcase  = xQTcase;
             Exsr DltQTkey;

             //Write new quaterly record with new keys
             QTds1 = QTds3;                    //Restore original record
             QTsts = *blanks;                  //Reset status
             QTbchid = *Zeros;                 //Reset Batch Id
             QTTRnDate = %char(%date():*iso0);

             QTpYr  = %int(%subst(QTtrnDate:1:4)); //Reset Processing Qtr
             Select;
             When %subst(QTtrnDate:5:2) <='03';
                QTpQtr = 1;
             When %subst(QTtrnDate:5:2) <='06';
                QTpQtr = 2;
             When %subst(QTtrnDate:5:2) <='09';
                QTpQtr = 3;
             When %subst(QTtrnDate:5:2) <='12';
                QTpQtr = 4;
             Other;
                QTpQtr = *Zeros;
             Endsl;


             If QTpYr = xQTpYr and QTpQtr = xQTpQtr; //Skip new rec for Cur Qtr
             Else;
                QTco#     = qqCo#2;               //Replace with New Key fields
                QTcarCd   = qqCar2;
                QTpolId   = qqPolId2;
                QTpolEfDt = qqPolEfDt2;
                QTClmId   = qqClmId2;
                QTAcDt    = qqAcDt2;
                If qqFyr2 > *Zeros;
                   QTfyr    = qqFyr2;
                Endif;
                QTtrnDate = %char(%date():*iso0);

                //Populate time stamp GO fields
                QTuser  = Q1user;
                QTpgm   = Q1pgm;
                QTtime  = %int(%char(%time():*ISO0));
                QTdt    = %dec(%date():*cymd);
                QTiusr  = Q1user;
                QTipgm  = Q1pgm;
                QTitim  = %int(%char(%time():*ISO0));
                QTidt   = %dec(%date():*cymd);
                Write PQT00;                      //Write new record with new ke
             Endif;

             Reade (qqCo#1:qqCar1:qqPolId1:qqPolEfDt1:qqClmid1:qqAcdt1:kkSts)
                   WMIDCQTL3;
          Enddo;
          isReturnCd = *on;

       Else;
          //Write Deletion
             svQTfnd   = svTRfnd;                 //Save Go fields
             svQTfyr   = svTRfyr;
             svQTcase  = svTRcase;
             Exsr DltQTkey;
             isReturnCd = *on;
       Endif;
       Endsr;
       //------------------------------------------------------------------
       //  Delete QT key                                                  *
       //------------------------------------------------------------------
       Begsr DltQtKey;
             Clear QTds1;                     //Clear all IDC fields except key
             QTco#     = qqCo#1;
             QTcarCd   = qqCar1;
             QTpolId   = qqPolId1;
             QTpolEfDt = qqPolEfDt1;
             QTClmId   = qqClmId1;
             QTAcDt    = qqAcDt1;
             QTrecTyCd = '02';                 //Restore rec type
             QTtrnDate = %char(%date():*iso0);

             //Populate GO fields
             QTsts   = 'O';                     //Offset status
             QTbchid = *Zeros;                  //Reset Batch Id
             QTfnd   = svQTfnd;                 //Restore Go fiels
             QTfyr   = svQTfyr;
             QTcase  = svQTcase;

             QTpYr  = %int(%subst(QTtrnDate:1:4)); //Reset Processing Qtr
             Select;
             When %subst(QTtrnDate:5:2) <='03';
                QTpQtr = 1;
             When %subst(QTtrnDate:5:2) <='06';
                QTpQtr = 2;
             When %subst(QTtrnDate:5:2) <='09';
                QTpQtr = 3;
             When %subst(QTtrnDate:5:2) <='12';
                QTpQtr = 4;
             Other;
                QTpQtr = *Zeros;
             Endsl;

             QTuser  = Q1user;
             QTpgm   = Q1pgm;
             QTtime  = %int(%char(%time():*ISO0));
             QTdt    = %dec(%date():*cymd);
             QTiusr  = Q1user;
             QTipgm  = Q1pgm;
             QTitim  = %int(%char(%time():*ISO0));
             QTidt   = %dec(%date():*cymd);
             Write PQT00;                      //Write Deletion
       Endsr;
      /End-Free
     PTransfer         E
      *================================================================
      * FormatPolId - Format Policy Id
      *  Parameters - 25 Unformatted Policy Id
      *  Returns    - 18 Formatted Policy Id
      *================================================================
     PFormatPolId      B                   Export
     DFormatPolId      PI            18a
     D qqPolId                       25    Const
     d intPos          s              5  0
     d IntLen          s              5  0
     d strBchar        s              1
     d strCap#         s             25
     d strPolId        s             18
      /Free
       strCap# = qqPolId;

       //Remove Hyphens
       strBchar = '-';
       Exsr GetPolId;

       //Remove Blanks
       strBchar = ' ';
       Exsr GetPolId;

       strPolId = strCap#;
       Return strPolId;

       //------------------------------------------------------------------
       //  Get Formatted Policy Id                                        *
       //------------------------------------------------------------------
       Begsr GetPolId;
       IntLen = %checkr(' ':strCap#);
       IntPos  = %scan(strBchar:strCap#);

       If (IntPos > *zero and strBchar <> *blank) or
          (IntPos > *zero and strBchar = *blank and IntPos < IntLen);
          Dou IntPos = *zero or IntPos >= IntLen;
             strCap# = %subst(strCap#:1:IntPos-1) +
                      %subst(strCap#:IntPos+1:25-IntPos);
             IntPos = %scan(strBchar:strCap#);
             IntLen = %checkr(' ':strCap#);
          Enddo;

       Endif;
       Endsr;
      /End-free
     PFormatPolId      E
