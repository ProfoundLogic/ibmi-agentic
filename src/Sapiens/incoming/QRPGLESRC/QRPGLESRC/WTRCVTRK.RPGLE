     hcopyright('(c) 2023 StoneRiver, Inc. All rights reserved.')
     h/TITLE WTRCVTRK - Recovery Tracking
     h/DEFINE   PROFOUNDUI
     h Option(*nodebugio: *srcstmt)
     h DFTACTGRP(*No) ACTGRP(*Caller)  BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description:
     h*
     h*  This program is the merge of below 2 Program/process
     h*  W0600R : Recovery Search
     h*  W0100R : Add/Edit Recovery
     h*
     h*  Called from: Pround UI
     h*
    h*  Created   : 04/20/23    C:340  Project: WC2430
     h**********************************************************************
     h* Revisions:
     h*
     h**********************************************************************
     fWTRCVTRK  cf   e             WorkStn ExtFile('WTRCVTRK')
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRecTrac:rrnRecover)
     fWMRRL     UF A E           K Disk    Rename(prr01:prr00)
     f                                     InfDs(dbfds1)
     fWMPDL     IF   E           K Disk    Rename(ppd01:ppd00)
     fWMRRP     IF   E             Disk
     fWMCML1    IF   E           K Disk
     d
     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d
     d Recovery        Pr                  ExtPgm('WTRCVTRK')
     d  pPrds                        40
     d  pMode                         1
     d  pYear                         3  0
     d  pCase                         7  0
     d  pRqst                         5  0
     d  pRtcd                         1
     d  pGrup                         3  0
     d  pClmRRN                       9  0
     d
     d Recovery        Pi
     d  pPrds                        40
     d  pMode                         1
     d  pYear                         3  0
     d  pCase                         7  0
     d  pRqst                         5  0
     d  pRtcd                         1
     d  pGrup                         3  0
     d  pClmRRN                       9  0
     d
     d* Payments
     d Payments        pr                  ExtPgm('WTCLMPMT')
     d  pMode                         1A
     d  pClmRrn                       9  0
     d  pPmtRrn                       9  0
     d  pTaxId                        9  0
     d  pModifier                     3  0
     d  pFromDt                       7  0
     d  pToDt                         7  0
     d  pEOB#                         9  0
     d  pRecon#                       3  0
     d  pSysType                      1
     d  pBillType                     1
     d  pPayment                      9  2
     d  pTransCode                    2
     d  pPayStatCode                  2
     d  pPayValue1                    2
     d  pPayValue2                    2
     d  pRecvDt                       7  0
     d  pReturnCode                   1
     d
     d wCO#            S                   Like(RRCO#)
     d ##RRN           S                   Like(scRecRRN#)
     d  isValid        S               N   Inz(*Off)
     d  pPmtRrn        s              9  0 Inz(*zeros)
     d  pTaxId         s              9  0 Inz(*zeros)
     d  pModifier      s              3  0 Inz(*zeros)
     d  pFromDt        s              7  0 Inz(*zeros)
     d  pToDt          s              7  0 Inz(*zeros)
     d  pEOB#          s              9  0 Inz(*zeros)
     d  pRecon#        s              3  0 Inz(*zeros)
     d  pSysType       s              1    Inz(*blanks)
     d  pBillType      s              1    Inz(*blanks)
     d  pPayment       s              9  2 Inz(*zeros)
     d  pTransCode     s              2    Inz(*blanks)
     d  pPayStatCode   s              2    Inz(*blanks)
     d  pPayValue1     s              2    Inz(*blanks)
     d  pPayValue2     s              2    Inz(*blanks)
     d  pRecvDt        s              7  0 Inz(*zeros)
     d  pReturnCode    s              1    Inz(*blanks)
     d  w@Pos          s              3  0 Inz(*zeros)
     d  w@Req#         s                   Like(RRREQ#)
     d  w@Payee        S             12A
     d**********************************************************************
     d* System Information
     d**********************************************************************
     d psds          esds                  extname(szq1p)
     d
      /copy sPrModule
      /copy sPrDate
      /copy sPrSecure
      /copy sPrCompany
      /copy wPrUsrPrms
      /copy wprPolPop
      /copy sPrGenPop

       //===================================================================
       // Main - Main processing
       //===================================================================

         // Get Company no.
          wCO# = UserCo#(q1user);

         Main();
         *InLr = *On;
         Return;

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free
       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          // Clear Sub-files
          ClrSubfiles();

          // Load Recovery Request Grid
          LoadRecovery();

          // Process Recovery Request
          Exsr ExfmtScreens;

          // Add Recovery Request
          Select;
          When scAddRec = *on;
             ##rrn = 0;
             EditAddRec(##rrn);
          When scCancel = *On;
             Leave;
          Other;
             ProcOther();
          EndSL;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

         TabHeight = 550;
         Exfmt RcdRecTrac;

       EndSr;
       /End-Free
     p Main            e
       //===================================================================
       // Add/Edit Claim/Case;
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
      /Free
        Readc sflRecTrac;
        Dow Not %Eof;
           If scRecEdt = *On;
              scRecEdt = *Off;
              ##rrn = scRecRRN#;
              EditAddRec(##rrn);
           Endif;
           If scRTPost = *On;
              If scPayStat = 'Active';
                 pMode   = 'T' ;
                 ##rrn = scRecRRN#;
                 pPmtRrn = ##rrn ;
                 Payments(pMode: pClmRrn: pPmtRrn: pTaxId: pModifier:
                          pFromDt: pToDt: pEOB#: pRecon#: pSysType:
                          pBillType: pPayment: pTransCode: pPayStatCode:
                          pPayValue1: pPayValue2: pRecvDt:pReturnCode);
              Else;
                 PopOneBtn('Unable to post a recovery on an inactive +
                            tracking record':'Error');
              Endif;
           Endif;
           Readc sflRecTrac;
        EndDo;

       /End-Free
     p ProcOther       e
       //===================================================================
       // Add/Edit Claim/Case;
       //===================================================================
     p EditAddRec      b
     d EditAddRec      pi
     d inRRN                          9  0 options(*nopass)

       If inRRN = 0;
          ExSr SrMvDBScr;
          Dou BtnAddCan = *On or (BtnAddSav = *on and isValid = *On);
             LoadCommon();
             Exfmt RcdRecAdd;
             isValid = *On;
             If BtnAddCan = *On;
                BtnAddCan = *Off;
                Leave;
             Endif;
             Select;
                When scAddRec = *on;
                   If BtnAddSav = *On;
                      Validate() ;
                      If isValid = *On;
                         AddRecReq();
                      Endif;
                   Endif;
                When scCancel = *On;
                   scCancel = *Off;
                   Leave;
             Endsl;
          EndDo;
       Else;
          ExSr SrMvDBScr;
          Dou BtnAddCan = *On;
             LoadCommon();
             Exfmt RcdRecAdd;
             Validate() ;
             If BtnAddSav = *On and isValid = *On;
                AddRecReq();
                Leave;
             Endif;
          EndDo;
       EndIf;

       //===================================================================
       // SrMvDBScr : Move Database Fields to Screen
       //===================================================================
       BegSr SrMvDBScr;

         ErPayee  = *Off;
         ErStat   = *Off;
         ErEAmt   = *Off;
         ErRamt   = *Off;
         MsStat   = ' ';
         MsPayee  = ' ';
         MsEAmt   = ' ';
         MsRAmt   = ' ';
         MsClaimant = ' ';
         ErClaimant = *Off;

         If inRRN <> 0 ;
            Chain inRRN PRr01;
            scPayee = %Dec(%Editc(RRFein:'X')+%Editc(RRMod:'X')
                       :12:0);
            scAttorn = %Dec(%Editc(RRAtty:'X')+%Editc(RRAMod:'X')
                       :12:0);
            If RRFein = 0;
               ScClaimant = 'Y';
            Endif;
            ScReq# = RRReq#;
            ScStat = RRStat;
            SCCom1 = RRCom1;
            SCCom2 = RRCom2;
            SCRcvd = RRRcvd;
            SCRamt = RRRamt;
            SCEamt = RREamt;
            SCDtre = dtMDYY(RRDtre);
            SCDtrq = dtMDYY(RRDtrq);
            SCDtrc = dtMDYY(RRDtrc);
         Else;
            scPayee    = 0;
            ScStat     = 'A';
            scAttorn   = 0;
            ScReq#     = 0;
            ScStat     = ' ';
            SCCom1     = ' ';
            SCCom2     = ' ';
            SCRcvd     = 0;
            SCRamt     = 0;
            SCEamt     = 0;
            SCDtre     = 0;
            SCDtrq     = 0;
            SCDtrc     = 0;
            ScClaimant = ' ';
         EndIf;
       EndSr;
     p EditAddRec      e
       //===================================================================
       // LoadCommon - Load common Screen fields
       //===================================================================
     p LoadCommon      b
     d LoadCommon      pi

       // Load common screen fields

       payeeChoi='PDFEIN || '' '' || PDMOD || '' - '' || ' +
        'CASE WHEN PDDBA1=''Y'' THEN TRIM(PDADR1) || '' - '' || ' +
        'TRIM(PDADR2) ELSE ' +
        'TRIM(PDNAME) || '' - '' || TRIM(PDADR1) || '' '' || ' +
        'TRIM(PDADR2) END ' +
        ' || '' - '' || TRIM(PDCTY) || '', '' || ' +
        'PDST || '' '' || SUBSTR(DIGITS(PDZIP),1,5)';

       AttornChoi = 'PDFEIN || '' '' || PDMOD || '' - '' || ' +
        'CASE WHEN PDDBA1=''Y'' THEN TRIM(PDADR1) || '' - '' || ' +
        'TRIM(PDADR2) ELSE ' +
        'TRIM(PDNAME) || '' - '' || TRIM(PDADR1) || '' '' || ' +
        'TRIM(PDADR2) END ' +
        ' || '' - '' || TRIM(PDCTY) || '', '' || ' +
        'PDST || '' '' || SUBSTR(DIGITS(PDZIP),1,5)';

     p LoadCommon      e
       //===================================================================
       //===================================================================
       // Validate : Validate Recovery Request
       //===================================================================
     p Validate        b
     d Validate        pi
      /Free

      // Clear Error Indicators & Messages

       ErPayee    = *Off;
       ErStat     = *Off;
       ErEAmt     = *Off;
       ErRamt     = *Off;
       ErClaimant = *Off;
       MsStat     = ' ';
       MsPayee    = ' ';
       MsEAmt     = ' ';
       MsRAmt     = ' ';
       MsClaimant = ' ';
       isValid = *On;

       If ScPayee = 0 and ScClaimant = ' ';
         ErPayee    = *On;
         MsPayee    = 'WCR0918';
         ErClaimant = *On;
         MsClaimant = 'WCR0918';
         isValid    = *Off;
       EndIf;

       If ScPayee <> 0 and ScClaimant = 'Y';
         ErClaimant = *On;
         ErPayee    = *On;
         MsClaimant = 'WCR0918';
         MsPayee    = 'WCR0918';
         isValid    = *Off;
       EndIf;

       If ScStat = ' ';
         ErStat = *On;
         MsStat = 'WCR0686';
         isValid = *Off;
       EndIf;

       If ScEamt < 0 ;
         ErEamt = *On;
         MsEamt = 'WCR0547';
         isValid = *Off;
       EndIf;

       If ScRamt < 0 ;
         ErRamt = *On;
         MsRamt = 'WCR0547';
         isValid = *Off;
       EndIf;

      /End-Free
     p Validate        e
       //===================================================================
       // AddRecReq : Add Recovery Request
       //===================================================================
     p AddRecReq       b
     d AddRecReq       pi
      /Free

            Chain (wCO# : pGrup : pYear : pCase : ScReq#) WMRRL;
            If %Found;
               w@Req# = ScReq#;
               ExSr SrMvScrDB;
               Update PRR00;
            Else;
               SetGT  (wCO# : pGrup : pYear : pCase) WMRRL;
               ReadPE (wCO# : pGrup : pYear : pCase) WMRRL;
               If Not %EOF;
                  w@Req# = RrReq# + 1;
               Else;
                  w@Req# = 1;
               EndIf;
               Clear PRR00;
               ExSr SrMvScrDB;
               Write PRR00;
            EndIf;

       //===================================================================
       // SrMvScrDB : Move Screen Field to Database Fields
       //===================================================================
       BegSr SrMvScrDB;

         RRCo#  = wCO#  ;
         RRFnd  = pGrup ;
         RRFyr  = pYear ;
         RRCase = pCase ;
         RRReq# = w@Req#;
         w@Payee = %Editc(scPayee:'X');
         RRFein = %Dec(%Subst(%Char(w@Payee):1:9):9:0);
         RRMod  = %Dec(%Subst(%Char(w@Payee):10:3):3:0);
         If scAttorn <> 0;
            RRAtty = %Dec(%Subst(%Char(scAttorn):1:9):9:0);
            RRAMod = %Dec(%Subst(%Char(scAttorn):10:3):3:0);
         Else;
            RRAtty = 0;
            RRAMod = 0;
         EndIf;
         RRStat = ScStat;
         RRCom1 = SCCom1;
         RRCom2 = SCCom2;
         RRRcvd = SCRcvd;
         RRRamt = SCRamt;
         RREamt = SCEamt;
         RRDtre = DTcyMd(SCDtre);
         RRDtrq = DTcyMd(SCDtrq);
         RRUSER = Q1USER;
         RRPGM  = Q1PGM;
         RRDT   = Dateto7(SystemDate());
         RRTIME = Timeto6(SystemTime());

       EndSr;
      /End-Free
     p AddRecReq       e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       rrnRecover = 0;
       clrRecover = *On;
       dspRecSub  = *Off;
       dspRecCtl  = *Off;
       Write RcdRecTrac;

      /End-Free
     p ClrSubfiles     e
       //===================================================================
       //===================================================================
       // Load Recovery Grid
       //===================================================================
     p LoadRecovery    b
     d LoadRecovery    pi

      /free
       // clear the subfile.
       clear sflRecTrac;
       clrRecover= *Off;
       dspRecSub = *On;
       dspRecCtl = *On;
       clear dbRrn1;

       Setll (wCO# : pGrup : pYear : pCase) WMRRL;
       Reade (wCO# : pGrup : pYear : pCase) WMRRL;
       Dow Not %eof(WMRRL);

          // Request Number
          scReqNum = RrReq#;

          // Payee
          scPayDesp = %EditC(RrFein:'X');

          // Modifier
          scPayMod = RrMod;

          // Payee Name
          If RrFein <> 0 ;
             Chain (RrFein : RrMod) WmPdl;
             If %Found;
                If %Trim(PdFsn) <> ' ';
                   scPayName = %Trim(PdName) +', '+ %Trim(PdFsn);
                Else;
                   scPayName = %Trim(PdName) ;
                EndIf;
                scPayeeTyp = PdType;
             Else;
                scPayName = %EditC(RrFein:'X');
                scPayeeTyp = ' ';
             EndIf;
          Else;
             scPayDesp = ' ';
             Chain (wCO# : pGrup : pYear : pCase) WMCML1;
             If %Found and %Trim(CmLsn) <> ' ' and  %Trim(CmFsn) <> ' ' ;
                If %Trim(CmMid) <> ' ';
                   scPayName = %Trim(CmLsn) +', '+ %Trim(CmFsn) + ', '+
                               %Trim(CmMid) ;
                Else;
                   scPayName = %Trim(CmLsn) +', '+ %Trim(CmFsn) ;
                EndIf;
             EndIf;
          EndIf;

          // Payee Status Code
          scPayCode = %Trim(RrStcd);

          // Payee Status
          If RRStat = 'A';
             scPayStat = 'Active';
          Else;
             scPayStat = 'Inactive';
          Endif;

          // Request Date
          If RRDTRQ = 0 ;
             scReqDate = 0 ;
          Else;
             Monitor;
                scReqDate = dtMDYY(RRDTRQ);
             On-Error;
                scReqDate = 0 ;
             EndMon;
          Endif;

          // Request Amount
          scReqAmt = RrRamt;

          // Recovered Amount
          scRecAmt = RrRcvd;

          scRecRRN# = dbRrn1;
          rrnRecover += 1;
          If rrnRecover= 1;
             scRecRowSl = 'A';
          Else;
             clear scRecRowSl;
          Endif;

          If pMode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;

          Write sflRecTrac;
          Reade (wCO# : pGrup : pYear : pCase) WMRRL;
       Enddo;

       rrnRecover = 1;

      /end-free
     p LoadRecovery    e
       //*************************************************************

