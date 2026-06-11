     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
     H BNDDIR('STBNDDIR')
     H*====================================================================
     H/TITLE FXMLDATA - Generate work file data for WCMC Scriptura requests.
     H*====================================================================
     H*  Description:
     H*
     H*  Build work file data to be used for Scriptura Document jobs on
     H*  WCMC processes.  This program will accept the XML document name
     H*  as an entry parameter to chain to the XMLPREP file to retrieve
     H*  the job specific information for generating data.
     H*
     H*  Called from:  STLETTER
     H*
     H*  Created:  05/29/15  C:79  Project: SY251
     H*====================================================================
     H*  Revisions:
     H*
     H*  06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*  04/10/18 C:79  Support: 99 70249
     H*                 Load Employer Recipient from Employer file
     H*
     H*  11/16/16 C:65  Project: SY371
     H*                 Check XPData on individual records.
     H*
     H*  08/04/16 C:65  Project: SY371
     H*                 Key the work file data with the template XML filename.
     H*                 Add processing to loop over records linked to a
     H*                 "master" record to generate work file data.
     H*
     H*  11/04/15 C: 079 Project: SY251
     H*                  Z-add Date field into days field
     H*
     H*  08/05/15 C: 079 Project: SY251
     H*                  Add group and company field mapping
     H*
     H*====================================================================
     FXMLPREL1  iF   E           K DISK
     FFMCML1    if   e           k disk    rename(pcm01:pcm00)
     FFMCDL     if   e           k disk    rename(pcd01:pcd00)
     FFMCHL2    if   e           k disk    rename(pch01:pch02)
     FSADAL1    if   e           k disk    rename(pda01:pda00)
     FFMDIL     if   e           k disk    rename(pdi01:pdi00)
     FFMICD10L1 if   e           k disk    rename(pic01:pic00)
     FFMPDL     if   e           k disk    rename(ppd01:ppd00)
     FFMP1L     if   e           k disk    rename(pp101:pp100)
     FFAPPL1    if   e           k disk    rename(ppp01:ppp00)
     FFMFNL     if   e           k disk    rename(pfn01:pfn00)
     FFMEML     if   e           k disk    rename(pem01:pem00)
     FFMSML     if   e           k disk    rename(psm01:psm00)
     FUMPCL9    if   e           k disk    rename(ppc01:ppc00)
     FFTGTL     if   e           k disk    rename(pgt01:pgt00)
     FSMUPL1    if   e           k disk    rename(pup01:pup01)
     FFTABL     if   e           k disk
     FSXDMP     o    e             disk    rename(pdm01:pdm00)
     F*** Beg Add *** 08/05/15 ********************************
     FSMCOL     if   e           k disk    rename(pco01:pco00)
     FFMF1L     if   e           k disk    rename(pf101:pf100)
     F*** End Add *** 08/05/15 ********************************
     D*
     D PSDS          ESDS                  extname(SZQ1P)
     D*
     D xmlFile         s                   like(XPXML)
     d*** begin add ***  08/04/16  ****************************************
     d xmlLink         s             60    inz(' ')
     d xmlFile2        s             60    inz(' ')
     d xxProcess       s              1    inz(' ')
     d*** end add *****  08/04/16  ****************************************
     D*
     D keyCo#          s                   like(CMCO#)
     D keySub#         s                   like(CMSUB#)
     D keyFnd          s                   like(CMFND)
     D keyFyr          s                   like(CMFYR)
     D keyCase         s                   like(CMCASE)
     D keyEmp#         s                   like(CMEMP#)
     D keyDiv          s                   like(CMDIV)
     D keyCert         s                   like(PCCERT)
     D keyExt          s                   like(PCEXT)
     D*
     D xxKey           s                   like(GTKEY)
     D xxTbid          s                   like(GTTBID)
     D xxEffDate       s                   like(CMACDT)
     D curDate         s                   like(CMACDT)
     D outDate         s              7
     D*** Beg Add *** 11/04/15 ***************************************
     D xxpcDt1         s              3
     D*** End Add *** 11/04/15 ***************************************
     D*
     D/copy sPrDate
     D/copy sPrEnv
     D***  Begin Add  ***  06/09/20  ****************************
     D/copy sPrMask
     D***  End   Add  ***  06/09/20  ****************************
     D*
     C*** begin add ***  08/04/16  ****************************************
     C     key1XML       klist
     C                   kfld                    xmlFile2
     C                   kfld                    xmlLink
     C*** end add *****  08/04/16  ****************************************
     C*
     C* Entry list to program.
     C*
     C     *entry        plist
     C                   parm                    QQXML            60
     C*
     C                   Movel(p)  QQXML         xmlFile
      /free

       //* Chain to XML Prep file to retrieve job information.
       Chain xmlFile xp100;
       If not %found;
          *inLR = *on;
          Return;
       Endif;

       //*** begin add ***  08/04/16  **************************************************
       xxProcess = ' ';

       If XPLink = ' ';
          xxProcess = 'Y';
       Else;
          xmlFile2 = ' ';
          xmlLink = XPLink;
          Setll key1XML xp100;
          Reade(n) key1XML xp100;
          If not %eof;
             xxProcess = 'Y';
          Endif;
       Endif;

       Dow xxProcess = 'Y';
       //*** end add *****  08/04/16  **************************************************
       //*** begin add ***  11/16/16  **************************************************
       If XPData = 'Y';
       //*** end add *****  11/16/16  **************************************************

       keyCo#  = XPCo#;
       keyFnd  = XPFnd;
       keySub# = XPSub#;
       keyFyr  = XPFyr;
       keyCase = XPCase;
       keyCert = XPCert;
       keyExt  = XPExt;

       //* Get the claim record data..
         If XPcat = 'MC' or XPcat = 'PC';
          Chain (keyCo#:keySub#:keyFnd:keyFyr:keyCase) pcm00;
         EndIf;

         If not %found;
            *inLR = *on;
            Return;
         Endif;

       //* Get the PreCert record data.
         If XPcat = 'PC';
          Chain (keyCo#:keySub#:keyFnd:keyFyr:keyCase:keyCert:keyExt) ppc00;
         EndIf;

         If not %found;
            *inLR = *on;
            Return;
         Endif;

       //* Get the current date in CYYMMDD  passed in.
         curDate = Dateto7(SystemDate());

       //* Clear record format for workfile and populate key fields.
         Clear pdm00;

       //* Populate fields from key fields passed in.
       //*** begin chg ***  08/04/16  ******************************************
       //*****DMXml  = xmlFile;
         DMXml  = XPXMLT;
       //*** end chg *****  08/04/16  ******************************************
         DMCo#  = keyCo#;
         DMSub# = keySub#;
         DMFnd  = keyFnd;
         DMFyr  = keyFyr;
         DMCase = keyCase;
         DMemp# = keyEmp#;
         DMdiv  = keyDiv;

       //* Populate fields associated with each area of work file based
       //* on the Category of the Letter
       //*** Beg Add *** 08/05/15 ******************************************
       Exsr Company;
       Exsr Group;
       //*** End Add *** 08/05/15 ******************************************

       //* Populate claim data.
         If XPcat = 'MC' or XPcat = 'PC';
            Exsr Claim;
         Endif;

       //* Populate PreCert data if needed
         If XPcat = 'PC';
            Exsr PreCert;
         Endif;

       //* Populate Recipient data
         Exsr Recipient;

       //* Populate environment and test group fields
         DMEnviro = CheckEnv();

         Chain (keyCo#:keySub#:keyFnd) pfn00;

         If %found;
           DMtstGrp = FNtest;
         EndIf;

       //* Maintenance info.
         DMPgm  = Q1PGM;
         DMUser = Q1USER;
         DMTime = Timeto6(SystemTime());
         DMDt   = Dateto7(SystemDate());

       //* Write workfile record.
         Write pdm00;
       //*** begin add ***  11/16/16  **************************************************
       Endif;
       //*** end add *****  11/16/16  **************************************************

       //*** begin add ***  08/04/16  **************************************************
          If XPLink = ' ';
             xxProcess = ' ';
          Else;
             Reade(n) key1XML xp100;
             If not %eof;
                xxProcess = 'Y';
             Endif;
          Endif;

       Enddo;
       //*** end add *****  08/04/16  **************************************************
      /end-free
     C*
     C* Exit program.
     C*
     C                   Eval      *inLR = *on
     C                   Return
     C*** Beg Add *** 08/05/15 ****************************************
     C**************************************************************************************
     C*
     C* Company
     C*
     C**************************************************************************************
     C     Company       Begsr
      /free

       Chain keyCO# pco00;
       If %found;

          DMCoName  = COName;
          DMCoPAdr1 = COAdr1;
          DMCoPAdr2 = COAdr2;
          DMCoPCty  = COCty;
          DMCoPSt   = COSt;
          DMCoPZip  = COZip;
          DMCoFein = COFein;
          DMCoMadr1= COMad1;
          DMCoMadr2= COMad2;
          DMCoMCty = COMCty;
          DMCoMSt  = COMSt;
          DMCoMZip = COMZip;
          DMCoArea = COArea;
          DMCoPhn# = COPhn#;
          DMCoFxar = COFAra;
          DMCoFax# = COFax#;

       Endif;

      /end-free
     C                   Endsr                                                  Company subr
     C**************************************************************************************
     C*
     C* Group
     C*
     C**************************************************************************************
     C     Group         Begsr
      /free

       Chain (keyCo#:keySub#:keyFnd) pfn00;
        If %found;
         Chain (keyCo#:keySub#:keyFnd) pf100;
         If %found;
          DMGpName = FNName;
          DMGpSName= FNShNm;
          DMGpAName= F1ANme;
          DMGpNum  = FNFnd;
          DMGpPAdr1= FNPAd1;
          DMGpPAdr2= FNPAd2;
          DMGpPCty = FNPCty;
          DMGpPSt  = FNPSt;
          DMGpPZip = FNPZip;
          DMGpMAdr1= FNMAd1;
          DMGpMAdr2= FNMAd2;
          DMGpMCty = FNMCty;
          DMGpMSt  = FNMSt;
          DMGpMZip = FNMZip;
          DMGpAAdr1= F1AAd1;
          DMGpAAdr2= F1AAd2;
          DMGpACty = F1ACty;
          DMGpASt  = F1ASt;
          DMGpAZip = F1AZip;
          DMGpArea = FNAra1;
          DMGpPhn# = FNPh1;
          DMGpOArea= FNArea;
          DMGpOPhn#= FNPh2;
          DMGpFxAr = FNFAra;
          DMGpFax# = FNFax;

         Endif;

        Endif;

      /end-free
     C                   Endsr                                                  Group subr
     C*** End Add *** 08/05/15 ****************************************
     C*
     C**************************************************************************************
     C*
     C* GroupRecip
     C*
     C**************************************************************************************
     C     GroupRecip    Begsr
      /free

       Chain (keyCo#:keySub#:keyFnd) pfn00;

       If %found;
          DMrcName   = FNname;
          DMrcAttn   = XPAttn;
          DMrcMAdr1  = FNmAd1;
          DMrcMAdr2  = FNmAd2;
          DMrcMCty   = FNmCty;
          DMrcMSt    = FNmSt;
          DMrcMZip   = FNmZip;
          DMrcPAdr1  = FNpAd1;
          DMrcPAdr2  = FNpAd2;
          DMrcPCty   = FNpCty;
          DMrcPSt    = FNpSt;
          DMrcPZip   = FNpzip;
          DMrcArea   = FNArea;
          DMrcPhn#   = FNph2;
          DMrcTFrA   = 800;
          DMrcTFr    = FNph1;
          DMrcFaxA   = 0;
          DMrcFax    = 0;
          DMrcNOB    = *blanks;
       Endif;

      /end-free
     C                   Endsr                                                  Group subr
     C**************************************************************************************
     C*
     C* Claim
     C*
     C**************************************************************************************
     C     Claim         Begsr
      /free

       DMClaim# = %editc(keySub#:'X') + '-' + %editc(keyFnd:'X') + '-' +
                  %editc(keyFyr:'X') + '-' + %editc(keyCase:'X');

       //* Claim Demographics
       Chain CMSS# pcd00;
       If %found;

          DMCdName = %trim(CDFsn) + ' ' + %trim(CDLsn);
          DMCdLsn  = CDLsn;
          DMCdFsn  = CDFsn;
          DMCdMid  = CDMid;
          DMCdAdr1 = CDAdr1;
          DMCdAdr2 = CDAdr2;
          DMCdCty  = CDCty;
          DMCdSt   = CDSt;
          DMCdZip  = CDZip;
          DMCdOAdr1 = CDAdr1;
          DMCdOAdr2 = CDAdr2;
          DMCdOCty  = CDCty;
          DMCdOAdr3 = CDAdr3;

          xxTbid = 'CTRY';
          xxkey = CDctry;

          Chain (keyCo#:xxTbid:xxKey) pgt00;
          If %found;
             DMCdCtry  = %trim(GTdesc);
          Endif;

//*********  Begin Chg  ***  06/09/20  **********************
//******* DMCdSS#   = CDSS#;
          DMCdSS#= MaskData('SSN':'CDSS#':CDSS#);
//*********  End   Chg  ***  06/09/20  **********************
          DMCdDtoB = CDDtoB;

          DMCdArea = CDArea;
          DMCdPhn# = CDPhn#;
          DMCdMSts = CDMSts;
          DMCdWAra = CDWAra;
          DMCdWPh# = CDWPh#;
          DMCMDEDT = CMDEDT;

       Endif;

       //* Set Claim status
       If CMStat = 'O';
          Setll (keyCo#:keySub#:keyFnd:keyFyr:keyCase:curDate) pch02;
          Reade (keyCo#:keySub#:keyFnd:keyFyr:keyCase) pch02;
          Dow not %eof;
             If CHOcst = 'C';
                CMStat = 'R';
                Leave;
             Endif;
             Reade (keyCo#:keySub#:keyFnd:keyFyr:keyCase) pch02;
          Enddo;
       Endif;

       Select;
       When CMStat = 'O';
          DMCmStat = 'Open';
       When CMStat = 'C';
          DMCmStat = 'Closed';
       When CMStat = 'R';
          DMCmStat = 'Reopened';
       Endsl;

       DMCmAcdt = CMAcdt;
       DMCmDtrp = CMDtrp;
       DMCmAcTm = CMActm;
       DMCmAcds = CMAcds;
       DMCmAcd2 = CMAcd2;
       DMCmDtlw = CMDtlw;
       DMCmLtfl = CMLtfl;
       DMCmMMI  = CMMMI;
       DMCmMMIP = CMMMIP;

       DMcmCFlg = CMcFlg;
       DMcmMCA  = CMmca;
       DMcmDny  = CMdny;
       DMcmCvFl = CMcvFl;
       DMcm1a3  = CM1a3;

       DMcmCCl# = CMcCl#;
       DMcmSCl# = CMsCl#;

       DMcmASt  = CMaSt;

       DMCmOcds = CMOcds;

       //* Employment Status
       xxTbid = 'ESST';
       xxkey = CMeSts;
       Chain (keyCo#:xxTbid:xxKey) pgt00;
       If %found;
          DMCMeSts = %trim(GTdesc);
       Endif;

       DMCmDyWk = CMdyWk;
       DMCmHrDy = CMhrDy;
       DMCmHidt = CMhidt;

       //* Case Management Nurse
       If CMcMNu <> ' ';
          Chain(n) CMcMNu pup01;
          If %found;
             DMCnName = UPUsnm;
             DMCnTitl = UPTitl;
       //*** Beg Add *** 08/05/15 ************************************
             DMcnArea = UPphnA;
             DMcnPhn# = UPPhn#;
       //*** End Add *** 08/05/15 ************************************
             DMCnExt# = UPExt#;
             DMCnMail = UPMail;
          Endif;
       Endif;

       //* Case Management Supervisor
       If CMcMSu <> ' ';
          Chain(n) CMcMSu pup01;
          If %found;
             DMCsName = UPUsnm;
             DMCsTitl = UPTitl;
       //*** Beg Add *** 08/05/15 ************************************
             DMcsArea = UPphnA;
             DMcsPhn# = UPPhn#;
       //*** End Add *** 08/05/15 ************************************
             DMCsExt# = UPExt#;
             DMCsMail = UPMail;
          Endif;
       Endif;

       //* Supervisor
       If CMclSu <> ' ';
          Chain(n) CMclSu pup01;
          If %found;
             DMsupName = UPUsnm;
             DMsupTitl = UPTitl;
       //*** Beg Add *** 08/05/15 ************************************
             DMsupArea = UPphnA;
             DMsupPhn# = UPPhn#;
       //*** End Add *** 08/05/15 ************************************
             DMSupExt# = UPExt#;
             DMSupMail = UPMail;
          Endif;
       Endif;

       //* Claims Adjuster
       If CMclAd <> ' ';
          Chain(n) CMclAd pup01;
          If %found;
             DMadjName = UPUsnm;
             DMadjTitl = UPTitl;
       //*** Beg Add *** 08/05/15 ************************************
             DMadjArea = UPphnA;
             DMadjPhn# = UPPhn#;
       //*** End Add *** 08/05/15 ************************************
             DMadjExt# = UPExt#;
             DMadjMail = UPMail;
          Endif;
       Endif;

       //* Field Adjuster
       If CMclFa <> ' ';
          Chain(n) CMclFa pup01;
          If %found;
             DMfAdName = UPUsnm;
             DMfAdTitl = UPTitl;
       //*** Beg Add *** 08/05/15 ************************************
             DMfadArea = UPphnA;
             DMfadPhn# = UPPhn#;
       //*** End Add *** 08/05/15 ************************************
             DMfAdExt# = UPExt#;
             DMfAdMail = UPMail;
          Endif;
       Endif;

       //* MC Staff Code 3
       If CMstf3 <> ' ';
          Chain(n) CMstf3 pup01;
          If %found;
             DMst3Name = UPUsnm;
             DMst3Titl = UPTitl;
       //*** Beg Add *** 08/05/15 ************************************
             DMst3Area = UPphnA;
             DMst3Phn# = UPPhn#;
       //*** End Add *** 08/05/15 ************************************
             DMst3Ext# = UPExt#;
             DMst3Mail = UPMail;
          Endif;
       Endif;

       DMcmInDt = CMinDt;

       xxTbid = 'TSIT';
       xxkey = CMtSit;
       Chain (keyCo#:xxTbid:xxKey) pgt00;
       If %found;
          DMcmTSit = GTdesc;
       Endif;

       DMcmYrLX = CMyrLX;
       DMcmODis = CMoDis;
       DMcmDbDt = CMdbDt;

       //* Diagnosis Code ICD-9 or ICD-10
       Chain (keyCo#:keySub#:keyFnd:keyFyr:keyCase) pda00;
       Dow not %eof;

          If DAprim = 'Y';
             Select;
             When DAiCDInd <> 'Y';
                Chain (DAiCD9) pdi00;
                   If %found;
                     DMcmPDia = DIds1;
                     DMcmPDiaD = DIdsc1;
                   EndIf;
             When DAiCDInd = 'Y';
                Chain (DAiCD1001) pic00;
                   If %found;
                     DMcmPDia = ICcode;
                     DMcmPDiaD = ICDsc1;
                   EndIf;
             EndSl;
             Leave;
          EndIf;

          Reade (keyCo#:keySub#:keyFnd:keyFyr:keyCase) pda00;

       EndDo;

       DMcmDtRe = CMdtRe;
       DMcmEntD = CMentD;
       DMcmRcvD = CMrcvD;
       DMcmPrec = CMprec;
       DMcmPrex = CMprex;

       //* Claim Providers
          Chain (keyCo#:keySub#:keyFnd:keyFyr:keyCase) ppp00;
          Dow not %eof;
             If PPpCP  = 'Y';

                DMppFEIN = PPfEIN;
                DMppMod  = PPmod;

                Chain (PPfein:PPmod) ppd00;

                If %Found;
                  DMppEmAd   = PDemAd;
                  If PDfsn <> *blanks;
                    DMppName = %trim(PDFsn) + ' ' + %trim(PDname);
                  Else;
                    DMppName = PDname;
                  Endif;

                  DMppAdr1  = PDadr1;
                  DMppAdr2  = PDadr2;
                  DMppCty   = PDcty;
                  DMppSt    = PDst;
                  DMppZip   = PDzip;

                  Chain (PPFein:PPMod) pp100;
                  If %found;
                    DMppN#02  = P1n#02;
                    DMppOAdr1  = P1adr1;
                    DMppOAdr2  = P1adr2;
                    DMppOCty   = P1cty;
                    DMppOAdr3  = P1Adr3;

                    xxTbid = 'CTRY';
                    xxkey = P1ctry;

                    Chain (keyCo#:xxTbid:xxKey) pgt00;
                    If %found;
                      DMppCtry  = %trim(GTdesc);
                    Endif;

                  Endif;

                  DMppArea  = PDarea;
                  DMppPhn#  = PDphn#;
                  DMppFxAr  = PDfxAr;
                  DMppFax#  = PDfax#;

                  DMppFrDt  = PPfrDt;
                  DMppToDt  = PPtoDt;

                  xxkey = PDSpCd;
                  Chain (keyCo#:xxKey) psp01;
                  If %found;
                     DMppSpec = SPdesc;
                  Endif;

                  DMppTSit  = PPtSit;

                EndIf;

                Leave;

             Endif;

             Reade (keyCo#:keySub#:keyFnd:keyFyr:keyCase) ppp00;

          Enddo;

      /end-free
     C                   Endsr
     C**************************************************************************************
     C*
     C* EmployerRecip
     C*
     C**************************************************************************************
     C     EmployerRecip Begsr
      /free

       Chain (keyCo#:keySub#:keyFnd:keyEmp#:keyDiv) pem00;

       If %found;
        //*** Beg Chg *** 04/10/18 ********************************
        //***  DMrcName   = FNname;
        //***  DMrcAttn   = XPAttn;
        //***  DMrcMAdr1  = FNmAd1;
        //***  DMrcMAdr2  = FNmAd2;
        //***  DMrcMCty   = FNmCty;
        //***  DMrcMSt    = FNmSt;
        //***  DMrcMZip   = FNmZip;
        //***  DMrcPAdr1  = FNpAd1;
        //***  DMrcPAdr2  = FNpAd2;
        //***  DMrcPCty   = FNpCty;
        //***  DMrcPSt    = FNpSt;
        //***  DMrcPZip   = FNpzip;
        //***  DMrcArea   = FNArea;
        //***  DMrcPhn#   = FNph2;
        //***  DMrcTFrA   = 800;
        //***  DMrcTFr    = FNph1;
        //***  DMrcFaxA   = 0;
        //***  DMrcFax    = 0;
        //***  DMrcNOB    = *blanks;
          DMrcName   = EMname;
          DMrcAttn   = XPAttn;
          DMrcMAdr1  = EMmAd1;
          DMrcMAdr2  = EMmAd2;
          DMrcMCty   = EMmCty;
          DMrcMSt    = EMmSt;
          DMrcMZip   = EMmZip;
          DMrcPAdr1  = EMpAd1;
          DMrcPAdr2  = EMpAd2;
          DMrcPCty   = EMpCty;
          DMrcPSt    = EMpSt;
          DMrcPZip   = EMpzip;
          DMrcArea   = EMArea;
          DMrcPhn#   = EMphn#;
          DMrcTFrA   = *Zeros;
          DMrcTFr    = *Zeros;
          DMrcFaxA   = EMfAra;
          DMrcFax    = EMfax#;
          DMrcNOB    = *blanks;
        //*** End Chg *** 04/10/18 ********************************
       Endif;

      /end-free
     C                   Endsr                                                  Group subr
     C**************************************************************************************
     C*
     C* PreCert
     C*
     C**************************************************************************************
     C     PreCert       Begsr
      /free

          DMpCStat = PCstat;
          DMpCLiab = PCliab;
          DMpCAdDt = PCadDt;
          DMpCRvDt = PCrvDt;
          DMpCCert = PCcert;
          DMpCExt  = PCext;

       //* Diagnosis Code ICD-9 or ICD-10
          Select;
          When PCICDInd <> 'Y';
             Chain (PCiCD1) pdi00;
             If %found;
               DMpcPDia = DIds1;
               DMpcPDiaD = DIdsc1;
             EndIf;
          When DAiCDInd = 'Y';
             Chain (PCICD1001) pic00;
             If %found;
               DMpcPDia = ICcode;
               DMpcPDiaD = ICDsc1;
             EndIf;
          EndSl;

          If PCintr <> ' ';
             Chain(n) PCintr pup01;
             If %found;
                DMpCIname = UPusNm;
       //*** Beg Add *** 08/05/15 ************************************
                DMpCIArea = UPphnA;
                DMPCIPhn# = UPPhn#;
       //*** End Add *** 08/05/15 ************************************
                DMpCIExt# = UPExt#;
                DMpCMail  = UPMail;
                DMpCTitl  = UPTitl;
             Endif;

          Endif;

          DMpCReqD = PCreqD;
          DMpCCerD = PCcerD;
          DMpCRecD = PCrecD;
          DMpCEntD = PCentD;
          DMpCAdSt = PCadSt;
          DMpCTyAd = PCtyAd;
          DMpCPARf = PCpaRf;

       //* Treating Physician

          Chain (PCtPhy:PCtPhM) ppd00;

          If %Found;
            DMpCTPhy = PDfEIN;
            DMpCTPhM = PDmod;
            DMpCTEmAd   = *blanks;
              If PDfsn <> *blanks;
                DMpcName = %trim(PDFsn) + ' ' + %trim(PDname);
              Else;
                DMpCName = PDname;
              Endif;

              DMpCAdr1  = PDadr1;
              DMpCAdr2  = PDadr2;
              DMpCCty   = PDcty;
              DMpCSt    = PDst;
              DMpCZip   = PDzip;

              Chain (PCtPhy:PCtPhm) pp100;

              If %found;
                DMpcN#02  = P1n#02;
                DMpcOAdr3  = P1Adr3;

                xxTbid = 'CTRY';
                xxkey = P1ctry;

                Chain (keyCo#:xxTbid:xxKey) pgt00;
                If %found;
                  DMpcCtry  = %trim(GTdesc);
                Endif;
              Endif;

              DMpcArea  = PDarea;
              DMpcPhn#  = PDphn#;
              DMpcFxAr  = PDfxAr;
              DMpcFax#  = PDfax#;

              xxkey = PDSpCd;
              Chain (keyCo#:xxKey) psp01;
              If %found;
                 DMppSpec = SPdesc;
              Endif;

            EndIf;
      /end-free

       //*** Beg Chg *** 11/04/15 ***************************************
       //*** DMpCDt1  = PCDt1;
     C                   Move      PcDt1         DMpcdt1
       //*** End Chg *** 11/04/15 ***************************************
      /free

       //* Billing Provider

          Chain (PCfein:PCmod) ppd00;
          If %Found;
            DMpCFein = PDfEIN;
            DMpCMod  = PDmod;
            DMpCBEmAd  = *blanks;
              If PDfsn <> *blanks;
                DMpcBName = %trim(PDFsn) + ' ' + %trim(PDname);
              Else;
                DMpCBName = PDname;
              Endif;

              DMpCBAdr1  = PDadr1;
              DMpCBAdr2  = PDadr2;
              DMpCBCty   = PDcty;
              DMpCBSt    = PDst;
              DMpCBZip   = PDzip;

              Chain (PCFein:PCmod) pp100;

              If %found;
                DMpcBN#02  = P1n#02;
                DMpcBOAd3  = P1Adr3;

                xxTbid = 'CTRY';
                xxkey = P1ctry;

                Chain (keyCo#:xxTbid:xxKey) pgt00;
                If %found;
                  DMpcBCtry  = %trim(GTdesc);
                Endif;

              Endif;

              DMpcBArea  = PDarea;
              DMpcBPhn#  = PDphn#;
              DMpcBFxAr  = PDfxAr;
              DMpcBFax#  = PDfax#;

              xxkey = PDspCd;
              Chain (keyCo#:xxKey) psp01;
              If %found;
                 DMpcBSpec = SPdesc;
              Endif;

            EndIf;

          DMpCTypA = PCtypA;
          DMpCrInj = PCrInj;
          DMpCHmHl = PChmHl;

      /end-free
     C                   Endsr                                                  PreCert subr
     C**************************************************************************************
     C*
     C* ProviderRecip
     C*
     C**************************************************************************************
     C     ProviderRecip Begsr
      /free

       Chain (XPpFein:XPpMod) ppd00;
       If %found;

          DMrcFein  = XPpFein;
          DMrcFein  = XPpMod;

          If PDfsn <> *blanks;
             DMrcName = %trim(PDFsn) + ' ' + %trim(PDname);
          Else;
             DMrcName = PDname;
          Endif;

          DMrcAttn   = XPAttn;
          DMrcMAdr1  = PDadr1;
          DMrcMAdr2  = PDadr2;
          DMrcMCty   = PDcty;
          DMrcMSt    = PDst;
          DMrcMZip   = PDzip;

          Chain (XPpFein:XPpMod) pp100;

             If %found;
               DMrcPAdr1  = P1adr1;
               DMrcPAdr2  = P1adr2;
               DMrcPCty   = P1cty;
               DMrcPSt    = P1st;
               DMrcPZip   = P1zip;
             Endif;

          DMrcArea  = PDarea;
          DMrcPhn#  = PDphn#;
          DMrcTFrA  = 0;
          DMrcTFr   = 0;
          DMrcFaxA  = PDfxAr;
          DMrcFax   = PDfax#;
          DMrcNOB   = *blanks;

       Endif;

      /end-free
     C                   Endsr                                                  Employer subr
     C*
     C**************************************************************************************
     C*
     C* Recipient
     C*
     C**************************************************************************************
     C     Recipient     Begsr
      /free

       //* Load field based on Recipient type passed in
       //* The type will hold C, P, S, G, E, or O for case, provider,
       //* subscriber, group, employer, or other.

       //* Case Recipient

          Select;

            When XPrType = 'C';
              DMrcName  = DMCdName;
              DMrcAttn  = XPAttn;
              DMrcMAdr1  = CDadr1;
              DMrcMAdr2  = CDadr2;
              DMrcMCty   = CDcty;
              DMrcMSt    = CDst;
              DMrcMZip   = CDzip;
              DMrcPAdr1  = CDadr1;
              DMrcPAdr2  = CDadr2;
              DMrcPCty   = CDcty;
              DMrcPSt    = CDst;
              DMrcPZip   = CDzip;
              DMrcArea  = CDarea;
              DMrcPhn#  = CDphn#;
              DMrcTFrA  = 0;
              DMrcTFr   = 0;
              DMrcFaxA  = 0;
              DMrcFax   = 0;
              DMrcNOB   = *blanks;

       //* Provider Recipient

            When XPrType = 'P';
                ExSr ProviderRecip;

       //* Subscriber Recipient

            When XPrType = 'S';

                Exsr SubscriberRec;

       //* Group Recipient

            When XPrType = 'G';
                Exsr GroupRecip;

       //* Employer Recipient

            When XPrType = 'E';
                Exsr EmployerRecip;

       //* Other Recipient

            When XPrType = 'O';
                Exsr OtherRecip;

          EndSl;

      /end-free
     C                   Endsr                                                  Recipient subr
     C**************************************************************************************
     C*
     C* OtherRecip
     C*
     C**************************************************************************************
     C     OtherRecip    Begsr
      /free

          DMrcName   = XPoName;
          DMrcAttn   = XPAttn;
          DMrcMAdr1  = XPoAdr1;
          DMrcMAdr2  = XPoAdr2;
          DMrcMCty   = XPoCity;
          DMrcMSt    = XPoSt;
          DMrcMZip   = XPoZip;
          DMrcPAdr1  = XPoAdr1;
          DMrcPAdr2  = XPoAdr2;
          DMrcPCty   = XPoCity;
          DMrcPSt    = XPoSt;
          DMrcPZip   = XPoZip;
          DMrcArea   = *zeros;
          DMrcPhn#   = *zeros;
          DMrcTFrA   = 0;
          DMrcTFr    = 0;
          DMrcFaxA   = 0;
          DMrcFax    = 0;
          DMrcNOB    = *blanks;

      /end-free
     C                   Endsr                                                  Other subr
     C**************************************************************************************
     C*
     C* SubscriberRec
     C*
     C**************************************************************************************
     C     SubscriberRec Begsr
      /free

       Chain (keyCo#:keySub#) psm00;

       If %found;
          DMrcName   = SMname;
          DMrcAttn   = XPAttn;
          DMrcMAdr1  = SMadr1;
          DMrcMAdr2  = SMadr2;
          DMrcMCty   = SMcty;
          DMrcMSt    = SMst;
          DMrcMZip   = SMzip;
          DMrcPAdr1  = SMadr1;
          DMrcPAdr2  = SMadr2;
          DMrcPCty   = SMcty;
          DMrcPSt    = SMst;
          DMrcPZip   = SMzip;
          DMrcArea   = SMarea;
          DMrcPhn#   = SMphn#;
          DMrcTFrA   = 0;
          DMrcTFr    = 0;
          DMrcFaxA   = 0;
          DMrcFax    = 0;
          DMrcNOB    = *blanks;
       Endif;

      /end-free
     C                   Endsr                                                  Group subr
     C**************************************************************************************
