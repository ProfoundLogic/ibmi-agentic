     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H*
     H/TITLE WTFROIA1 - Generate IA-1 Froi Data
     H*****************************************************************
     H* Program Description:  This program populates the FROI         *
     H*                       workfile for IA-1 FROI.                 *
     H*                                                               *
     H* Called From:  STLETTER                                        *
     H* Calls To:     None                                            *
     H*                                                               *
     H*****************************************************************
     H* Created on 06/12/2018 by c:62 for Project 102WHM
     H*****************************************************************
     H* REVISIONS:
      * 10/02/20 C:180 Support 90013 - Change format for Date Received.
      *
      * 09/28/20 C:180 Support 90013 - Remove typo ** comment
      *                                on Name of Preparer change
      *                                dated 08/12/20.
      *                                Record of change is under
      *                                08/12/20, not 09/28/20.
      *
      * 08/28/20 C:180 Support 90013 - Fix accident county
      *
      * 08/12/20 C:180 Support 90013
      *                - Fix accident state, Business type Description,
      *                  Signature of preparer (Space after comma), and
      *                  Carrier FEIN.
      *
      *  06/09/20 C:298 Project: SY420
      *                Mask SSN
      *                                                               *
      * 04/30/20 C:62  Ticket: 88176(099)
      *                -Load missing fields.
      *                -Load employment type.
     H*
     H*  08/28/19 C:053 Support: 82228
     H*                 Correct field for proper link
     H*****************************************************************
     Fsmcol     if   e           k Disk    Rename(pco01:pco00)
     Fsxfrp     o    e             Disk    Rename(pfr01:pfr0p)
     Fwdell     if   e           k DisK    Rename(pel01:pel00)
     F*** Begin Add *** 04/30/20 *******************************
     Fwdf2l     if   e           k Disk    Rename(pf201:pf200)
     F*** End   Add *** 04/30/20 *******************************
     Fwdf3l     if   e           k Disk    Rename(pf301:pf300)
     Fwmaal1    if   e           k Disk
     Fwmagl     if   e           k Disk    Rename(pag01:pag00)
     Fwmahl     if   e           k Disk    Rename(pah01:pah00)
     Fwmall     if   e           k Disk    Rename(pal01:pal00)
     Fwmcdl     if   e           k Disk    Rename(pcd01:pcd00)
     Fwmcml1    if   e           k Disk
     F*** Begin Add *** 04/30/20 *******************************
     Fwmecl1    if   e           k Disk
     F*** End   Add *** 04/30/20 *******************************
     Fwmeml     if   e           k Disk    Rename(pem01:pem00)
     Fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)
     Fwmnil     if   e           k Disk    Rename(pni01:pni00)
     F*** Begin Add *** 04/30/20 *******************************
     Fwmofl     if   e           k Disk    Rename(pof01:pof00)
     F*** End   Add *** 04/30/20 *******************************
     Fwmrpl     if   e           k Disk    Rename(prp01:prp00)
     Fwmtpl1    if   e           k Disk
     Fwtabl     if   e           k Disk    Include(pca01)
     F                                     Include(pna01)
     F                                     Include(ppa01)
     F                                     Include(pcn01)
     Fwtgtl     if   e           k Disk    Rename(pgt01:pgt00)
     Fxmlprel1  if   e           k Disk    Rename(xp100:pxp01)

     D psds          esds                  ExtName(SZQ1P)
     D*
     d* Retrieve client id
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d   clientID                     3

     d clientID        s              3    Inz(' ')
     d kkPcnt          s              1    Inz('1')
     d jurState        s              2    Inz('A1')
     d chrTbID         s                   Like(GTtbID)
     d chrKey          s                   Like(GTkey)
     d inXML           s                   Like(XPxml)

      //*** Begin Add *** 08/12/20 *******************************
       DCL-S Position packed(5:0);
       DCL-S intIdx packed(5:0) INZ(1);
       DCL-S str1 char(1);
      //*** End   Add *** 08/12/20 *******************************

     D/copy sPrModule
     D/copy sPrXML
     D***  Begin Add  ***  06/09/20  ****************************
     D/copy sPrMask
     D***  End   Add  ***  06/09/20  ****************************
      *
      *
     C*
     C     *Entry        plist
     C                   parm                    inXML
     C*
      /free
       Chain inXML pxp01;
       If %Found(xmlprel1);
          Chain (XPco#:XPfnd:XPfYr:XPcase) pcm01;
          If %Found(wmcml1);
             ExSr PgmStart;
             ExSr WrtSXFRP;
          EndIf;
       EndIf;

       *inLR = *On;
      //-------------------------------------------------------------
      // PgmStart - Program Start
      //--------------------------------------------------------------
         Begsr PgmStart;

            // Get Client ID
            RtvCltID(clientID);

            // Access Company Master
            Chain (CMco#) pco00;

            //*** Begin Add *** 04/30/20 ****************
            // Group Auxiliary
            Chain (CMco#:CMfnd) pf200;
            //*** End   Add *** 04/30/20 ****************

            // Access Group EDI File
            Chain (CMco#:CMfnd:CMjrSt) pf300;

            // Access Group Master
            Chain (CMco#:CMfnd) pfn00;

            //*** Begin Add *** 04/30/20 ****************
            // Access Employer Contacts
            Chain (CMco#:CMfnd:CMemp#:CMdiv:'1') pec01;
            //*** End   Add *** 04/30/20 ****************

            // Access Employer Master and Employer Detail
            Chain (CMco#:CMfnd:CMemp#:CMdiv) pem00;
            Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) pel00;

            // Access Carrier/State Reporting
            Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) prp00;

            // Access Agency Information
            SetGt  (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) pah00;
            ReadPE (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) pah00;
            DoW Not %Eof(wmahl) And AHstat = 'I';
               ReadPE (CMco#:CMfnd:CMemp#:CMdiv:CMfYr) pah00;
               If %Eof(wmahl);
                  AHagt# = 0;
                  AGname = ' ';
               EndIf;
            EndDo;
            // Now Access Agent Assignment File
            If AHagt# <> 0;
               Chain (CMco#:CMfnd:AHagt#) paa01;
               If %Found(wmaal1);
                  Chain (CMco#:AAfein:AAmod) pag00;
               EndIf;
            EndIf;

            // Access Employer Address Location
            Chain (CMco#:CMfnd:CMemp#:CMdiv:CMctg1) pal00;

            // Access Claim Additional Information
            Chain (CMco#:CMfnd:CMfYr:CMcase:jurState) pni00;

            //*** Begin Add *** 04/30/20 ****************
            // Office Information
            Chain (CMco#:CMoff#) pof00;
            //*** End   Add *** 04/30/20 ****************

            // Access Treating Physician Information
            Chain (CMco#:CMfnd:CMfYr:CMcase) ptp01;

            // Access Claim Demographics
            Chain (CMss#) pcd00;

         EndSr;
      //-------------------------------------------------------------
      // WrtSXFRP - Write to Scriptura First Report if Injury File
      //--------------------------------------------------------------
         Begsr WrtSXFRP;

            Clear pfr0p;

            //*** Beg Chg *** 08/28/19 ******
            // FRxml    = inXML;
            FRxml    = XPxmlt;
            //*** End Chg *** 08/28/19 ******
            FReNam   = EMname;
            FRco#    = CMco#;
            FRfnd    = CMfnd;
            FRfName  = FNname;
            FRfYr    = CMfyr;
            FRcase   = CMcase;
            FReMad1  = EMmAd1;
            FReMad2  = EMmAd2;
            FReMcty  = EMmCty;
            FReMst   = EMmSt;
            FReMzip  = %EditC(EMmZip:'X');
            FRePad1  = EMpAd1;
            FRePad2  = EMpAd2;
            FRePcty  = EMpCty;
            FRePst   = EMpSt;
            FRePzip  = %EditC(EMpZip:'X');
            FRjrSt   = CMjrSt;
            FRacl#   = CMacl#;
            FRlPad1  = ALpAd1;
            FRlPad2  = ALpAd2;
            FRlPcty  = ALpCty;
            FRlPst   = ALpSt;
            FRlPzip  = ALpZip;
            FRlMad1  = ALmAd1;
            FRlMad2  = ALmAd2;
            FRlMcty  = ALmCty;
            FRlMst   = ALmSt;
            FRlMzip  = ALmZip;
            FRsic    = CMsic;

            chrTbID  = 'SIC';
            chrKey   = CMsic;
            Chain (CMco#:chrTbID:chrKey) pgt00;
            If %Found(wtgtl);
               FRsicDs  = GTdesc;
            EndIf;

            FReFei   = EMfein;
            FReArea  = EMarea;
            FRePhn#  = EMphn#;
            FRcoNm   = COname;

            Monitor;
               FRpfDt = %dec(%date(ELpfDt:*cymd):*usa);
            On-Error;
               FRpfDt = 0;
            EndMon;

            //*** Begin Add *** 04/30/20 ****************
            Monitor;
               FRhiDt = %dec(%date(CMhiDt:*cymd):*usa);
            On-Error;
               FRhiDt = 0;
            EndMon;
            //*** End   Add *** 04/30/20 ****************

            Monitor;
               FRptDt = %dec(%date(ELptDt:*cymd):*usa);
            On-Error;
               FRptDt = 0;
            EndMon;

            FRcoAd1  = COadr1;
            FRcoAd2  = COadr2;
            FRcoCty  = COcty;
            FRcoSt   = COst;
            FRcoZip  = COzip;

            If FNsFin = 'Y';
               FRsFin   = 'X';
            EndIf;

            If clientID = '025';
               FRcoFe = F3Ifei;
            Else;
            //* Begin Change *** 08/12/20 *************************************
               //FRcoFe = COfein;
               FRcoFe = F2fein;
            //* End   Change *** 08/12/20 *************************************
            EndIf;

            If RPcap# <> ' ';
               FRPol# = RPcap#;
            Else;
               FRPol# = %EditC(CMfnd:'X') + %EditC(CMemp#:'X') +
                        %EditC(CMfYr:'X');
            EndIf;

            FRagNm   = AGname;
            FRcFsn   = CDfsn;
            FRcLsn   = CDlsn;
            FRcMid   = CDmid;

            Monitor;
               FRdtob = %dec(%date(CDdtob:*cymd):*usa);
            On-Error;
               FRdtob = 0;
            EndMon;

//*********  Begin Chg  ***  06/09/20  **********************
//*******   FRss#    = CDss#;
            FRss#    = MaskData('SSN':'CDss#':CDss#);
//*********  End Chg  ***  06/09/20  **********************
            FRstoh   = NI10A2;
            FRsex    = CDsex;
            FRcAd1   = CDadr1;
            FRcAd2   = CDadr2;
            FRcCty   = CDcty;
            FRcSt    = CDst;
            FRcZip   = CDzip;
            FRcArea  = CDarea;
            FRcPhn#  = CDphn#;
            FRmSt    = CDmSts;
            FRocDs   = CMocDs;

            chrTbID  = 'EMST';
            chrKey   = CMeSts;
            Chain (CMco#:chrTbID:chrKey) pgt00;
            If %Found(wtgtl);
               FRestD = %Subst(GTdesc:1:30);
            EndIf;

            FRdPnd   = CM#dep;
            FRclas   = CMclas;
            FRwgp    = NI20A9;
            FRpoi    = CMpoi;
            FRaww    = CMaww;
            FRdays   = CMdays;
            FRfSal   = CMfSal;
            FRbgTm   = NItim2;

            Monitor;
               FRacDt = %dec(%date(CMacdt:*cymd):*usa);
            On-Error;
               FRacDt = 0;
            EndMon;

            FRacTm   = CMacTm;

            Monitor;
               FRdtLw = %dec(%date(CMdtLw:*cymd):*usa);
            On-Error;
               FRdtLw = 0;
            EndMon;

            Monitor;
               FRdtRp = %dec(%date(CMdtRp:*cymd):*usa);
            On-Error;
               FRdtRp = 0;
            EndMon;

            Monitor;
               FRdbDt = %dec(%date(CMdbDt:*cymd):*usa);
            On-Error;
               FRdbDt = 0;
            EndMon;

            FRcont   = EMcont;
            FRnatr   = CMnatr;
            FRpart   = CMpart;
            FRcaus   = CMcaus;

            chrKey = CMnatr;
            Chain (CMco#:chrKey) pna01;
            If %Found(wtabl);
               FRnads   = NAdesc;
               FRnatrDs = NAdesc;
            EndIf;

            chrKey = CMpart;
            Chain (CMco#:chrKey) ppa01;
            If %Found(wtabl);
               FRpads   = PAdesc;
               FRpartDs = PAdesc;
            EndIf;

            chrKey = CMcaus;
            Chain (CMco#:chrKey) pca01;
            If %Found(wtabl);
               FRcads   = CAdesc;
               FRcausDs = CAdesc;
            EndIf;

            FRePrm   = CMeAdr;
            FRaccL1  = NI40A7;
            FRaccL2  = NI40A0;
            FRaCty   = NI30A1;
            FRaSt    = NI10A1;
            FRaZip   = NI9N1;
            FReUse1  = NI60A1;
            FReUse2  = NI60A2;
            FRempA1  = NI60A3;
            FRempA2  = NI60A4;
            FRwPrc1  = NI60A5;
            FRwPrc2  = NI60A6;
            FRhowi1  = NI80A1;
            FRhowi2  = NI80A2;

            FRsafeP  = NI1A1;
            FRsafeU  = NI1A2;

            Monitor;
               FRrtw  = %dec(%date(CMrtw:*cymd):*usa);
            On-Error;
               FRrtw  = 0;
            EndMon;

            Monitor;
               FRdod  = %dec(%date(CMdod:*cymd):*usa);
            On-Error;
               FRdod  = 0;
            EndMon;

            FRinit   = CMiTrt;

            chrTbID  = 'INIT';
            chrKey   = CMiTrt;
            Chain (CMco#:chrTbID:chrKey) pgt00;
            If %Found(wtgtl);
               FRinitDs = GTdesc;
            EndIf;

            FRphyN   = TPphyN;
            FRphyA   = TPphyA;
            FRtCty   = TPphyC;
            FRtSt    = TPphyS;
            FRtZip   = TPphyZ;
            FRhosN   = CMhosN;
            FRhosA   = CMhosA;
            FRhosC   = CMhosC;
            FRhosS   = CMhosS;
            FRhosZ   = CMhosZ;
            FRphyE   = NI40A4;
            FRhosE   = NI40A8;
            FRprPhA  = NI80A3;

            Monitor;
               FRdTan = %dec(%date(NIdt5:*cymd):*usa);
            On-Error;
               //*** Begin Chg *** 04/30/20 *****************************
               //*** FRdod  = 0;
               FRdTan  = 0;
               //*** End   Chg *** 04/30/20 *****************************
            EndMon;

            FRdte    = %dec(%date():*usa);

            //*** Begin Change *** 08/12/20 *****************************
            FRprNm   = NIprNm;
            FRprNM = SpaceAftComma(FRprNM);
            //*** End   Change *** 08/12/20 *****************************

            If NIprPh <> ' ' And NIprPh <> 'nullnull';
               FRprPh   = %Dec(%Trim(NIprPh):10:0);
            EndIf;

            //*** Begin Add *** 04/30/20 *********************************
            FRctg1   = CMctg1;
            FRnaob   = EMnaob;
            FRgt1    = CMgt1;
            FRacnm   = CMacnm;
            FRtparea = TPphyr;
            FRtpphn# = TPphyp;
            FRhrsd   = CMhrsd;
            FRhrsw   = CMhrsd * CMdays;
            FRacd2   = CMacd2;
            FRfarea  = FNph1a;
            FRfphn#  = FNph1;
            FRfpad1  = FNpad1;
            FRfpad2  = FNpad2;
            FRfpcty  = FNpcty;
            FRfpst   = FNpst ;
            FRfpzip  = FNpzip;
            FRfmad1  = FNmad1;
            FRfmad2  = FNmad2;
            FRfmcty  = FNmcty;
            FRfmst   = FNmst ;
            FRfmzip  = FNmzip;
            FRalnam  = FNanam;
            FRalad1  = FNaad1;
            FRalad2  = FNaad2;
            FRalcty  = FNacty;
            FRalst   = FNast ;
            FRalzip  = FNazip;
            FRcara   = COarea;
            FRcph#   = COphn#;
            //* Begin Chg ***   10/02/20 **************************************
            //FRrcvd   = CMrcvd;
            FRrcvd   = %dec(%date(CMrcvd:*cymd):*usa);
            //* End   Chg ***   10/02/20 **************************************
            FRcads   = GetCauseDs(CMcaus);
            FRfdnm   = EMfdnm;
            FRfsdt   = CMfsdt;
            FReagr   = CMeagr;
            FRhosr   = CMhosr;
            FRhosp   = CMhosp;
            FRcrt    = CMcrt ;
            FRtpaara = F2area;
            FRtpaphn = F2sph#;
            FRiaud   = F2iaud;
            FRnaic   = EMnaic;
            //* Begin Add ***   08/12/20 **************************************
            chrTbID  = 'NAIC';
            chrKey   = EMnaic;
            Chain (CMco#:chrTbID:chrKey) pgt00;
            If %Found(wtgtl);
               FRnaicDs  = GTdesc;
            EndIf;
            //* End   Add ***   08/12/20 **************************************
            FRsctc   = F2sctc;
            FRuser   = CMiusr;
            FReemad  = ECemal;
            FRoadr   = OFadr1;
            FRocty   = OFcty ;
            FRost    = OFst  ;
            FRozip   = OFzip ;
            FRonam   = OFname;
            FRoarea  = OFarea;
            FRophn#  = OFphn#;
            FRprww   = CMprww;
            FRpoww   = CMpoww;
            FRsemp   = EMsemp;
            FRcrcd   = F2crcd;
            FRwara   = CDwara;
            FRwph#   = CDwph#;
            FRcfar   = COfara;
            FRcfax   = COfax#;
            FRnab1   = EMnaob;
            FRpst    = CMeadr;
            FRsunm   = CMespv;
            FRsicd   = EMsic ;
            FRaccL1  = CMacdr;
            FRaCty   = CMacty;
            //*** Begin change *** 08/12/20 ****************************************************
            //FRaSt    = CMacst;
            FRast    = CMast;
            //*** End  change *** 08/12/20 ****************************************************
            FRaZip   = CMacz1 + Cmacz2;

            //*** Begin change *** 08/28/20 ************************************************
            //chrKey = CMjrSt + CDcnty;
            chrKey = CMaSt + CMaccy;
            //*** End  change *** 08/28/20 ************************************************
            Chain (CMco#:chrKey) pcn01;
            If %Found(wtabl);
               FRcnty   = CNdesc;
            EndIf;
            //*** End   Add *** 04/30/20 *********************************

            FRiDt =  %dec(%date():*cymd);

            Write pfr0p;

         Endsr;
       //===================================================================
       // GetCauseDs - Get Cause Code Description
       //===================================================================
     P GetCauseDs      B
     D GetCauseDs      PI            40
     D  causeCd                       2    Const

      /Free

         chrKey = causeCd;
         Chain (CMco#:chrKey) pca01;
         If %Found(WTABL);
            Return CAdesc;
         EndIf;

         Return  ' ';

      /End-Free
     P GetCauseDs      E
       //--------------------------------------------------------------*
       // Space after a comma is found                                 *
       //--------------------------------------------------------------*
       DCL-PROC SpaceAftComma;
         DCL-PI *N char(30);
           @strWithComma char(30);
         END-PI;

         DCL-S str30 char(30);

         str30 = @strWithComma;

         Dou Position = *Zero or intIdx > %len(str30);
            Position = %Scan(',':str30:intIdx);

            If Position > *Zero and Position < %len(str30);
                str1 = %subst(str30:Position + 1);
                If str1 > *blank;
                   str30 = %Subst(str30:1:Position-1) + ',' + ' ' +
                           %Subst(str30:Position +1:30-Position);
                Endif;
            Endif;

            intIdx = Position + 1;
         Enddo;

         Return str30;
       END-PROC;
      /End-free
