     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title WC2435 - Prompt for Policy Certificate Holders
     h/define CONVERT
     h dftactgrp(*no) actgrp('QILE')
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE WTRPTCERTH - Prompt for Policy Certificate Holders
     h******************************************************************
     h* Revisions                                                      *
     H*
     H* 11/20/24 C:340 PROJECT: WC2988
     H*              Add below Parm to W7510C
     H*              New parms : Policy Effective Date Range
     H*                           > FromDate & Todate
     H*                          Type Of Policy
     H*                           > RNB Flag (Renew/New/Both)
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
     H*                                                                *
     H* 07/16/21  C:65  PROJECT: 102WIO                                *
     H*                 Display iDarts fields on regular output window *
     H*                 as long as spool file output is generating.    *
     H*                 Clear out iDarts fields for printing blank     *
     H*                 certs to keep field values from carrying over  *
     H*                 from printing non-blank certs.  Populate # of  *
     H*                 copies into LDA for blank certificates.        *
     H*                                                                *
     H* 05/23/19  C:300 Support: 79508 (FMIC)                          *
     H*                 COI Grid Still not "Sticking" after sort, etc  *
     H*                 Position to selected row on User Grid reload   *
     H*                                                                *
     H* 02/18/19  C:079 Support: 90/76066                              *
     H*                 Default remark 1 on Add                        *
     H*                                                                *
     H* 10/30/18  C:291 Project: WC2593                                *
     H*                 Fix column sorting on grid to retain same sort *
     H*                 sequence after returning control back from     *
     H*                 Add/Edit screen. Display file changes:         *
     H*                 Add property "persist state" = true on grid.   *
     H*                 Add onclick event on BtnCancel to clearState() *
     H*                 and pui.click("BtnExitButtonAll").             *
     H*                                                                *
     H* 04/11/18  C:291 Project: WC2593                                *
     H*                 Add selectin check box column on the grid.     *
     H*                 Change Visibility to hidden on Change status,  *
     H*                 Print and Rollover link.                       *
     H*                 Added Select Action dropdown to select Change  *
     H*                 Status, Print and Rollover options.            *
     H*                                                                *
     H* 02/28/18  C:062 Ticket: 64530(129)                             *
     H*                 Initialize flags to N
     H*                                                                *
     H* 04/25/17  C:309 Project: WC2435                                *
     H*                 Populate maintenance fields
     H*                                                                *
     H* 01/10/17  C:65  PROJECT: SY371                                 *
     H*                 Implement Scriptura output control window      *
     H*                 when process is flagged to generate Scriptura  *
     H*                 output.  The settings are stored/passed via    *
     H*                 a local data area which is picked up by        *
     H*                 API's and the printing program.                *
     h*                                                               *
     h*****************************************************************

     fWTRPTCERTHcf   e             workstn extfile('WTRPTCERTH')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(SflCerHldr:scSflRRN)
     fWtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     f                                     include(rcdComPnl)
     f*
     f* User Parameter Master File
     f*
     fWMCEP     UF A E             DISK
     f                                     RENAME(PCE01:PFRCDF)
     f                                     infds(dbFdsP)
     fWMCEL6    IF   E           K DISK
     f                                     INFDS(DbFds1)
     f                                     RENAME(PCE01:DBRCDF)
     fWMCEL7    IF   E           K DISK
     f                                     INFDS(dbFds2)
     f                                     RENAME(PCE01:DBRCDF2)
     fWdelp     if   e             disk    Rename(pel01:pel0p)
     fwdell1    IF   E           K disk    Rename(pel01:pel02)
     fWMFNL     IF   E           K DISK
     fWMUPL     IF   E           K DISK
     fWMEXL     IF   E           K DISK
     fWmeml     if   e           k disk    Rename(pem01:pem02)
     FWmahl     IF   E           K DISK    rename(pah01:pah02)
     fststl3    IF   E           K DISK
     fStctl1    IF   E           K DISK
     fWmcel     IF   E           K DISK    Rename(pce01:pce00)
      *  GENERIC TABLE FILE
     FWtgtl     IF   E           K DISK
     f*** Beg Add *** 02/18/19 *********************************************
     fWmrsl     if   e           k disk
     fWtxcl     if   e           k disk
     f*** End Add *** 02/18/19 *********************************************

     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
      *** begin add ***  01/10/17  ************************************
     D/copy sPrXML
     D isUseScript     S              1n   inz
     d processType     c                   const('Certificate of Insurance')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
     D*** begin add ***  07/16/21  **************************************
     d xxBlank         s              1
     D*** end add *****  07/16/21  **************************************
     d
     D SVLDA           DS          1024    DTAARA(*LDA)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d*
     d* Scriptura Prompt Screen procedure
     d ScriptPrompt    pr
      *** end add *****  01/10/17  ************************************

     dpsds           esds                  extname(szq1p)

     ddsFileInfo       ds
     d rrn                   397    400b 0

     ddbFdsp           ds                  likeds(dsFileInfo)
     ddbFds1           ds                  likeds(dsFileInfo)
     ddbFds2           ds                  likeds(dsFileInfo)

     D*DBFDSD          DS
     D* DBRRND               397    400B 0
      *
     d WtCertHldr      pr                  Extpgm('WTRPTCERTH')
     d  inKC                          1
     d  inKL                          1
     d  inCall                        1
     d  inrrn                         9
     d  inrrn2                        9
     d
     d WtCertHldr      pi
     d  inKC                          1
     d  inKL                          1
     d  inCall                        1
     d  inrrn                         9
     d  inrrn2                        9

     d outCltId        s              3
     d intPos          s              2  0
     d strrptl         s             23
     d outsufx         s              3
     d EL_rrn          s              9  0
     d*inRptTool       s              1

     d charMsg         s            100
     d outRrn#         s              9
     d inRrn3          s              9  0
     d*inRrn2          s              9  0
     d empRrn          s              9  0
     d intoolrrn       s              9  0
     d isValid         s               n   Inz(*off)
     d intCo#          s                   Like(ELco#)
     d intFnd          s                   Like(ELfnd)
     d intEmp#         s                   Like(ELemp#)
     d intDiv          s                   Like(ELdiv)
     d intFyr          s                   Like(ELFyr)
     d strOutq         s                   Like(Upoutq)
     d inSelect        s           1024
     d strCmntDsc      s             30    inz(*Blanks)
      *
     dintCo#1          s              3
     dintFnd1          s              3
     dintEmp#1         s              9
     dintDiv1          s              5
     dintCert#1        s              5
     dintCert1         s              5
     dintFyr1          s              3
     dintFyr2          s              3
     dintFrmDt         s              8
     dintToDt          s              8
     d inqte           s              1
     d*inKc            s              1
     d intKl           s              1
     d inErr           s              1
     d inCo#           s              3
     d inFnd           s              3
     d inPyr           s              3
     d inEmp#          s              9
     d inDiv           s              5
     d intAgtNbr       s              5  0
     d intAgtNam       s             60
     d intPrdNbr       s              5  0
     d intPrdNam       s             60
     d intArea         s              3  0
     d intPhone        s              7  0
     d intPlan         s              1
     d intDate1        s              8  0
     d intDate2        s              8
     d intEnv          s              1
     d sysDate         s               d
     d sysDate1        s              7  0
     d sysDate2        s              8
     d intCert#        s              5  0
     d intBlnk         s              1
     d intDate         s              8  0
     d intCopy         s              3
     d intOutq         s             10
     d intRfsl         s              1
     d intRmcd         s              5
     d inKey           s             10
     d inTbid          s              4
     d isMode          s              1a   inz(*blanks)
     d*** Beg Add *** 04/11/18 **************************************
     d intFyr3         s                   Like(ELFyr)
     d intHdnRrn       s              5  0
     d isFirstRollOvr  s               n   Inz(*on)
     d isFirstPrint    s               n   Inz(*on)
     d intCEcer#       s                   Like(CEcer#)
     d*** End Add *** 04/11/18 **************************************
     d*** Beg Add *** 11/20/24 *** WC2988 ***********************************
     d intRNB          s              1
     d*** End Add *** 11/20/24 *** WC2988 ***********************************
      *****************************************************************
      * Prototype Declaration
      *****************************************************************
     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy sCopyPolPr
     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrUsrPrms
     d/copy wPrPolErr
     d/copy WTSearchDs
     d/copy sPrLetter

     d* Add details for update
     d AddDetails      pr

     d* Add details for write
     d AddDetails1     pr

     d* Move database record to screen
     d MoveDbSc        pr

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltId                       3

     d* Process other options
     d ProcOther       pr

     d* Process Addition
     d ProcessAdd      pr

     d* Process Change Status
     d  PrcChgStat     pr

     d* Process Print all Option
     d ProcesPrntAll   pr

     d* Process Rollover All Option
     d*ProcesRollAll   pr

     d* Process Print Blank Certificate
     d ProcesPrntBlk   pr

      * Get Agent/Producer Information
     d GetAgtPrdInfo   pr                  extpgm('WA002R')
     d intco#                         3  0
     d intFnd                         3  0
     d intEmp#                        9  0
     d intDiv                         5  0
     d intFyr                         3  0
     d intAgtNbr                      5  0
     d intAgtNam                     60
     d intPrdNbr                      5  0
     d intPrdNam                     60
     d intArea                        3  0
     d intPhone                       7  0
     d intPlan                        1
     d intDate                        8  0

     d* process Edit
     d processEdt      pr

     d* process Deletion
     d processDlt      pr

     d* process Rollover
     d processRoll     pr

     d* process Print Certificate
     d processPrnt     pr

     d* Clear Subfiles
     d ClrSubFiles     pr

     d SA030R          PR                  extpgm('SA030R')
     d outsufx                        3

      * Intialize the Screen fields
     d LoadInVal       pr

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Set Criteria
     d SetCriteria     pr

      * Clear Error/Message fields
     d ClrErr          pr

      * Clear Old data
     d Clrdata         pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Validate Print certificate window
     d ValidPrnt       pr

      * Print Certificate Report Submit
     d PrntCertSbm     pr                  ExtPgm('W7510C')
     d intCo#1                        3
     d intFnd1                        3
     d intFyr1                        3
     d intEmp#1                       9
     d intDiv1                        5
     d intCert#1                      5
     d intCert1                       5
     d intBlnk                        1
     d intDate2                       8
     d intCopy                        3
     d intOutq                       10
     d intRfsl                        1
     d intRmcd                        5
     d*** Beg Add *** 11/20/24 *** WC2988 ***********************************
     d intFrmDt                       8
     d intToDt                        8
     d intRNB                         1
     d*** End Add *** 11/20/24 *** WC2988 ***********************************

      * Rollover Batch process
     d RollBtchSbm     pr                  ExtPgm('WA008R')
     d intKL                          1

      * Rollover Batch process
     d RolloverSbm     pr                  ExtPgm('W7520C')
     d intCo#1                        3
     d intFnd1                        3
     d intFyr1                        3
     d intEmp#1                       9
     d intDiv1                        5
     d intCert#1                      5
     d intFyr2                        3
     d intFrmDt                       8
     d intToDt                        8

      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free

       //===================================================================
       // PrcChgStat - Process Change Status Link
       //===================================================================
     P PrcChgStat      B
     d PrcChgStat      PI
      /free
       Chain scRrn PFRCDF;
       If CEstat = 'I';
          CEstat = 'A';
       Else;
          CEstat = 'I';
       EndIf;
       Update PFRCDF %fields(CEstat);
       // *** Beg Chg *** 04/11/18 **************************************
       scHdnSflN = ScSflRrn;
       // *** End Chg *** 04/11/18 **************************************
      /End-free
     P PrcChgStat      E

       //===================================================================
       // ProcessEdt - Process Edit Link
       //===================================================================
     P ProcessEdt      B
     d ProcessEdt      PI
      /free
          clear IsCertDisb;
          isMode = 'E';
          IsCertDisb = *on;
          // *** Beg Add *** 04/11/18 **************************************
          visCrtSNxt = *Off;
          leftCrtCan = 745;
          // *** End Add *** 04/11/18 **************************************

          Chain(n) scRrn PFRCDF;
          MoveDbSc();

          Dou BtnAddCan = *On;
             Exfmt rcdCertAdd;
          Select;

          When BtnAddSav = *on;
             ClrErr();
             ValidPnl();
             If isValid = *Off;
                AddDetails();
             Leave;
             Endif;

          When BtnAddCan = *on;
             BtnAddCan = *off;
             Leave;

          Other;
          Endsl;

       Enddo;

      /end-free
     P processEdt      E

       //===================================================================
       // ProcessDlt - Process Deletion
       //===================================================================
     P ProcessDlt      B
     d ProcessDlt      PI
      /free

          Dou BtnAddCan = *On;
             Chain(n) scRrn PFRCDF;
             MoveDbSc();
             // *** Beg Add *** 04/11/18 **************************************
             visCrtSNxt = *Off;
             leftCrtCan = 745;
             scHdnSflN = ScSflRrn;
             // *** End Add *** 04/11/18 **************************************
             IsDelete = *On;
             IsCertDisb = *On;
             Exfmt rcdCertAdd;
          Select;

          When BtnAddDlt = *On;
             Chain scRrn PFRCDF;
             Delete PFRCDF;
             //***Beg Add ***** 04/11/18 ********************
             If ScSflRrn - 1 > 0;
                scHdnSfln = ScSflRrn - 1;
             Else;
                scHdnSfln = 1;
             Endif;
             //***End Add ***** 04/11/18 ********************
             Unlock Wmcep;
             Leave;

          When BtnAddCan = *on;
             BtnAddCan = *off;
             Leave;

          Other;
          Endsl;

          Enddo;

       IsDelete = *Off;
       IsCertDisb = *Off;

      /end-free
     P processDlt      E

       //===================================================================
       // ProcessRoll - Process Rollover
       //===================================================================
     P ProcessRoll     B
     d ProcessRoll     PI
      /free

          // *** Beg Chg *** 04/11/18 **************************************
          //scfyr = scFyr + 1;
          //intFyr2  = %EditC(scFyr:'X');
          intFyr3 = scFyr + 1;
          intFyr2  = %EditC(intFyr3:'X');
          // *** End Chg *** 04/11/18 **************************************
          intCo#1  = %EditC(intCo#:'X');
          intFnd1  = %EditC(intFnd:'X');
          intFyr1  = %EditC(intFyr:'X');
          intEmp#1 = %EditC(intEmp#:'X');
          intDiv1  = %EditC(intDiv:'X');
          intCert#1 = %EditC(intCert#:'X');
          intFrmDt = *Blanks;
          intToDt = *Blanks;
          //intKl = ' ';
          //
            If scCertRoll = *On;
             IsAll     = *Off;
             IsSelected= *On;
             IsPrint = *Off;
             IsBatch = *On;
             scCertRoll = *Off;
          EndIf;

            If BtnRollAll = *On;
             IsAll     = *On;
             IsSelected= *Off;
             IsPrint = *Off;
             IsBatch = *On;
             BtnRollAll = *Off;
          EndIf;

          // Information message;

          // *** Beg Chg *** 04/11/18 **************************************
          // Exfmt RcdInfMsg;
          If isFirstRollOvr = *On;
             isFirstRollOvr = *Off;
             Exfmt RcdInfMsg;
          Endif;
          scHdnSflN = ScSflRrn;
          // *** End Chg *** 04/11/18 **************************************

          // If intKL = '0';
          Dou BtnInfCanl = *On;
          Select;
          When  BtnInfSbmt = *On;
                RolloverSbm(intCo#1:intFnd1:intFyr1:intEmp#1:intDiv1:intCert#1:
                intFyr2:intFrmDt:intToDt);
             Leave;

          When  BtnInfCanl = *On;
             BtnInfCanl = *Off;
             Leave;

          Endsl;
          Enddo;

      /end-free
     P processRoll     E

       //===================================================================
       // ProcessPrnt - Process Print Certificate
       //===================================================================
     P ProcessPrnt     B
     d ProcessPrnt     PI
      /free
       //*** begin add ***  03/08/17  **************************************
         IsPrntAll = *off;
       //*** end add *****  03/08/17  **************************************
       //*** begin add ***  07/16/21  **************************************
         xxBlank = ' ';
       //*** end add *****  07/16/21  **************************************
          // *** Beg Chg *** 04/11/18 **************************************
          scHdnSflN = ScSflRrn;
          // *** End Chg *** 04/11/18 **************************************

          Dou BtnPrntCnl = *On;
             scRsfl = 'N';
             Clear scDrmk;

         // Checks if there is a default remark code in this file
          chain (intCo#:intFnd) Pex01;
          If %found(wmexl) and EXcicd <> *Blanks;
             scRsfl = 'Y';
             scDrmk = EXcicd;
          Endif;
         //  inRsfl = scRsfl;

          sysDate1 = DateTo7(systemDate());
          sysDate2 = %char(DateTo8(systemDate()));
          If EMcad1 <> *Zeros and  EMcad1  <= sysDate1;
          // Record is cancelled, display window showing employer is cancelled
             isEmpCncl = *On;
          Else;
         //Record is NOT cancelled, display window for user to enter a
         //remark code to be used.
             isEmpCncl = *Off;
          Endif;
       //*** begin add ***  03/08/17  **************************************
          visScriptC = *off;
          If isEmpCncl  = *off and visScript = *on;
             visScriptC = *on;
          Endif;
       //*** end add *****  03/08/17  **************************************

      *** Beg Add *** 11/20/24 *** WC2988 ***********************************
          isFirstPrint = *Off;
      *** Beg Add *** 11/20/24 *** WC2988 ***********************************
          // *** Beg Chg *** 04/11/18 **************************************
          //Exfmt RcdCertPrt;
          If isFirstPrint = *On;
             Exfmt RcdCertPrt;                     //Display submit screen
          Else;
             BtnPrntSbm = *on;                     //For all other prints turn Submit Btn On
          Endif;
          // *** End Chg *** 04/11/18 **************************************

          Select;

          When BtnPrntSbm = *on;
             BtnPrntSbm = *Off;
             ClrErr();
             ValidPrnt();
       //*** begin add ***  03/08/17  **************************************
             If isValid = *off;
                ScriptPrompt();
                If xxSubmit <> *on;
                   isValid = *on;
      *** Beg Add *** 11/20/24 *** WC2988 ***********************************
                   BtnPrntCnl= *on;
      *** Beg Add *** 11/20/24 *** WC2988 ***********************************
                Endif;
             Endif;
       //*** end add *****  03/08/17  **************************************
             If isValid = *Off;
                intCo#1  = %EditC(intCo#:'X');
                intFnd1  = %EditC(intFnd:'X');
                intFyr1  = %EditC(intFyr:'X');
                intEmp#1 = %EditC(intEmp#:'X');
                intDiv1  = %EditC(intDiv:'X');
                intCert#1 = %EditC(intCert#:'X');
                intCert1 = *Blanks;
                intDate2 = sysDate2;
                intCopy  = '  0';
                intBlnk  = 'N';
                intRfsl  = scRsfl;
                intRmcd  = scDrmk;
                intOutq  = strOutq;
       //*** Beg Chg *** 11/20/24 *** WC2988 ***********************************
          //*** PrntCertSbm(intCo#1:intFnd1:intFyr1:intEmp#1:intDiv1:intCert#1:
          //***             intCert1:intBlnk:intDate2:intCopy:intOutq:intRfsl:
          //***             intRmcd);

                intFrmDt = *Blanks;
                intToDt  = *Blanks;
                intRNB   = *Blanks;

                PrntCertSbm(intCo#1:intFnd1:intFyr1:intEmp#1:intDiv1:intCert#1:
                            intCert1:intBlnk:intDate2:intCopy:intOutq:intRfsl:
                            intRmcd:intFrmDt:intToDt:intRNB);
       //*** End Chg *** 11/20/24 *** WC2988 ***********************************
                IsPrint = *On;
                IsSelected = *Off;
                IsAll      = *Off;
                IsBatch    = *Off;

                // *** Beg Chg *** 04/11/18 **************************************
                //Exfmt RcdInfmsg;
                If isFirstPrint = *On;
                   isFirstPrint = *Off;
                   Exfmt RcdInfmsg;
                Endif;
                // *** End Chg *** 04/11/18 **************************************

                If BtnInfEntr = *On;
                   BtnPrntCnl= *On;
                Endif;

             Leave;
             Endif;

          When BtnPrntCnl= *on;
             BtnPrntCnl= *off;
             Leave;

          Other;
          Endsl;

       Enddo;

      /end-free
     P processPrnt     E

       //===================================================================
       // ProcesPrntAll : Process Print Certificate for All
       //===================================================================
     P ProcesPrntAll   B
     d ProcesPrntAll   PI
      /free

         isEmpCncl = *Off;
         IsPrntAll = *On;
       //*** begin add ***  07/16/21  **************************************
         xxBlank = ' ';
       //*** end add *****  07/16/21  **************************************

         Dou BtnPrntCnl = *On;
            sysDate1 = DateTo7(systemDate());
            sysDate2 = %char(DateTo8(systemDate()));
            If EMcad1 <> *Zeros and  EMcad1  <= sysDate1;
          // Record is cancelled, display window showing employer is cancelled
               isEmpCncl = *On;
            Else;
         //Record is NOT cancelled, display window for user to enter a
         //remark code to be used.
               isEmpCncl = *Off;
            Endif;
       //*** begin add ***  03/08/17  **************************************
          visScriptC = *off;
          If isEmpCncl  = *off and visScript = *on;
             visScriptC = *on;
          Endif;
       //*** end add *****  03/08/17  **************************************

            Clear scCerf;
            Clear scCerT;
            scRsfl = 'N';
            Clear scDrmk;
         // Checks if there is a default remark code in this file
            chain (intCo#:intFnd) Pex01;
            If %found(wmexl) and EXcicd <> *Blanks;
               scRsfl = 'Y';
               scDrmk = EXcicd;
            Endif;
         //  inRsfl = scRsfl;

            Exfmt RcdCertPrt;

            Select;

            When BtnPrntSbm = *on;
               BtnPrntSbm = *Off;
               ClrErr();
               ValidPrnt();
       //*** begin add ***  03/08/17  **************************************
                If isValid = *off;
                   ScriptPrompt();
                   If xxSubmit <> *on;
                      isValid = *on;
                   Endif;
                Endif;
       //*** end add *****  03/08/17  **************************************
               If isValid = *Off;
                  intCo#1  = %EditC(intCo#:'X');
                  intFnd1  = %EditC(intFnd:'X');
                  intFyr1  = %EditC(intFyr:'X');
                  intEmp#1 = %EditC(intEmp#:'X');
                  intDiv1  = %EditC(intDiv:'X');
                  intCert#1= %EditC(scCerf:'X');
                  intCert1 = %EditC(scCert:'X');
                  intDate2 = sysDate2;
                  intCopy  = '  0';
                  intBlnk  = 'N';
                  intRfsl  = scRsfl;
                  intRmcd  = scDrmk;
                  intOutq  = strOutq;
       //*** Beg Chg *** 11/20/24 *** WC2988 ***********************************
            //*** PrntCertSbm(intCo#1:intFnd1:intFyr1:intEmp#1:intDiv1:
            //***             intCert#1:intCert1:intBlnk:intDate2:intCopy:
            //***             intOutq:intRfsl:intRmcd);

                intFrmDt = *Blanks;
                intToDt  = *Blanks;
                intRNB   = *Blanks;

                PrntCertSbm(intCo#1:intFnd1:intFyr1:intEmp#1:intDiv1:intCert#1:
                            intCert1:intBlnk:intDate2:intCopy:intOutq:intRfsl:
                            intRmcd:intFrmDt:intToDt:intRNB);
       //*** End Chg *** 11/20/24 *** WC2988 ***********************************

                IsPrint = *On;
                IsSelected = *Off;
                IsAll      = *Off;
                IsBatch    = *Off;

                Exfmt RcdInfmsg;

                If BtnInfEntr = *On;
                   BtnPrntCnl= *On;
                Endif;

               Leave;
               Endif;

            When BtnPrntCnl= *on;
               BtnPrntCnl= *off;
               Leave;

            Other;
            Endsl;

         Enddo;

      /end-free
     P ProcesPrntAll   E

       //===================================================================
       // ProcessAdd : Process Add Operation
       //===================================================================
     P ProcessAdd      B
     d ProcessAdd      Pi
      /free
         isMode = 'A';

          // *** Beg Add *** 04/11/18 **************************************
          intHdnRrn = scHdnSflN;
          Clear rcdCertAdd;
          scHdnSflN = intHdnRrn;
          visCrtSNxt = *On;
          leftCrtCan = 635;
          // *** End Add *** 04/11/18 **************************************

         Setgt (intCo#:intFnd:intFyr:intEmp#:intDiv) pce00;
         Readpe (intCo#:intFnd:intFyr:intEmp#:intDiv) pce00;
         If not %eof(wmcel);
            scCertNbr = CEcer# + 1;
         Else;
            scCertNbr = 1;
         Endif;

          Dou BtnAddCan = *On;
             ClrData();
             // *** Beg Add *** 02/18/19 *********************************
             LoadDft();
             // *** End Add *** 02/18/19 *********************************
             Exfmt rcdCertAdd;
          Select;

          When BtnAddSav = *on;
             ClrErr();
             ValidPnl();
             If isValid = *Off;
                AddDetails1();
             Leave;
             Endif;

          //*** Beg Add ***** 04/11/18 ************
          When BtnAddSNxt = *on;
             ClrErr();
             ValidPnl();
             If isValid = *Off;
                AddDetails1();
                ProcessAdd();
             Leave;
             Endif;
          //*** End Add ***** 04/11/18 ************

          When BtnAddCan = *on;
             BtnAddCan = *off;
             Leave;

          Other;
          Endsl;

       Enddo;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcesPrntBlk : Process for Blank certificate
       //===================================================================
     P ProcesPrntBlk   B
     d ProcesPrntBlk   PI
      /free
       //*** begin add ***  03/08/17  **************************************
         IsPrntAll = *off;
       //*** end add *****  03/08/17  **************************************
       //*** begin add ***  07/16/21  **************************************
            scRsfl = 'N';
            Clear scDrmk;
            xxBlank = 'Y';
       //*** end add *****  07/16/21  **************************************

         Dou BtnBlnkCnl = *On;
         scCopy = 1 ;

         Exfmt RcdBlnkCer;

         Select;

         When BtnBlnkSbm = *on;
            BtnBlnkSbm = *Off;
         // Clear error
            ClrErr();
         // Validate the number of copies
            If scCopy <  1  or scCopy >  99 ;
               erNbrCopie  = *On;
               msNbrCopie  = 'WCR0536';
               isvalid = *On;
            Endif;

       //*** begin add ***  03/08/17  **************************************
             If isValid = *off;
                ScriptPrompt();
                If xxSubmit <> *on;
                   isValid = *on;
                Endif;
             Endif;
       //*** end add *****  03/08/17  **************************************
            If isValid = *Off;
               intCo#1  = %EditC(intCo#:'X');
               intFnd1  = %EditC(intFnd:'X');
               intFyr1  = %EditC(intFyr:'X');
               intEmp#1 = %EditC(intEmp#:'X');
               intDiv1  = %EditC(intDiv:'X');
               intCert#1 = %EditC(intCert#:'X');
               intCert1 = *Blanks;
               sysDate1 = EMcad1;
               intDate1 = 19000000 + sysDate1;
               intDate2 = %Editc(intDate1:'X');
               intCopy  = %editc(scCopy:'X');
               intBlnk  = 'Y';
               intRfsl  = scRsfl;
               intRmcd  = scDrmk;
               intOutq  = strOutq;
       //*** Beg Chg *** 11/20/24 *** WC2988 ***********************************
         //*** PrntCertSbm(intCo#1:intFnd1:intFyr1:intEmp#1:intDiv1:intCert#1:
         //***             intCert1:intBlnk:intDate2:intCopy:intOutq:intRfsl:
         //***             intRmcd);

                intFrmDt = *Blanks;
                intToDt  = *Blanks;
                intRNB   = *Blanks;

                PrntCertSbm(intCo#1:intFnd1:intFyr1:intEmp#1:intDiv1:intCert#1:
                            intCert1:intBlnk:intDate2:intCopy:intOutq:intRfsl:
                            intRmcd:intFrmDt:intToDt:intRNB);
       //*** End Chg *** 11/20/24 *** WC2988 ***********************************

               IsPrint = *On;
               IsSelected = *Off;
               IsAll      = *Off;
               IsBatch    = *Off;

               Exfmt RcdInfmsg;

               If BtnInfEntr = *On;
                  BtnInfEntr = *Off;
               Endif;

            Leave;
            Endif;

         When BtnBlnkCnl= *on;
            BtnBlnkCnl= *off;
            Leave;

         Other;
         Endsl;

         Enddo;

      /end-free
     P ProcesPrntBlk   E

       //===================================================================
       // ProcOther - process other options
       //===================================================================

     p ProcOther       b
     d ProcOther       pi
      /free

       // Edit Certificate Holders

       Readc sflCerHldr;

       Dow not %eof;

          //***Beg Add ***** 04/11/18 ********************
          If btnPerAct = *on and scSelAct <> *blank and scSelect = 'Y';
             Select;
             When scSelAct = 'Change Status';
                LnkChgSts = *on;
             When scSelAct = 'Rollover';
                scCertRoll = *on;
             When scSelAct = 'Print';
                scCertPrnt = *on;
             Endsl;
          Endif;
          //***End Add ***** 04/11/18 ********************

          Select;

          // processing for Edit link
          When scCertEdt = *on;
             scCertEdt = *off;
             Clear RcdCertAdd;
             BtnAddCan = *off;
             BtnAddSav = *off;
             //*** Beg Add ***** 04/11/18 ************
             BtnAddSNxt = *off;
             scHdnSflN = ScSflRrn;
             //*** End Add ***** 04/11/18 ************
             ProcessEdt();

          // processing for Delete Link
          When scCertDlt  = *on;
             scCertDlt  = *off;
             ProcessDlt();

          // processing for Rollover Link
          When scCertRoll = *on;
          // scCertRoll = *off;
             intcert# = ScCertNbr;
             ProcessRoll();

          // processing for Print Certificate Link
          When scCertPrnt = *on;
             scCertPrnt = *off;
             intcert# = ScCertNbr;
             ProcessPrnt();

          // processing for Change Status.
          When LnkChgSts = *on;
             LnkChgSts = *off;
             PrcChgStat();
             //***Beg Del ***** 04/11/18 ********************
             //Leave;
             //***End Del ***** 04/11/18 ********************
          Endsl;

          Readc sflCerHldr;
       EndDo;

      /end-free
     p ProcOther       E

       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi

      /Free
       //scFnd  = 0;
       //scName = *Blanks;
       scAgt# = 0;
       scAnam = *Blanks;
       ScEdn  = 0;
       ScFyr  = 0;
       ScPySt = *Blanks;
       ScProd = 0;
       ScPnam = *Blanks;
       IsCertDisb = *off;

      /End-Free
     p LoadInVal       e
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       //*** Beg Chg ***** 10/30/18 *************************************
       //Dou BtnCancel = *On ;
       Dou ExitNow = *On ;
       //*** End Chg ***** 10/30/18 *************************************

          // Clear Sub-files
          ClrSubfiles();

          // Load user subfile grid
          Exsr LoadUserGrid;

          //*** Beg Add ***** 04/11/18 ********
          // Initialize Screen fields
          scSelAct = '--- Select List Action ---';
          isFirstRollOvr = *On;
          isFirstPrint   = *On;
          //*** End Add ***** 04/11/18 ********

          // Display Screen
          Exsr ExfmtScreens;

          Select;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;
             LoadInVal();

          When BtnAdd = *On;
             BtnAdd =*Off;
             ProcessAdd();

          When BtnPrntAll = *On;
             BtnPrntAll =*Off;
             intCert# = *Zeros;
             ProcesPrntAll();

          When BtnRollAll = *On;
          // BtnRollAll =*Off;
             intCert# = *Zeros;
             processRoll();

          When BtnPrntBlk = *On;
             BtnPrntBlk =*Off;
             ProcesPrntBlk();

          When BtnCancel = *on;
             BtnCancel = *Off;
             Leave;

          Other;
             ProcOther();

          EndSl;

         Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 770;
          APnlHeight = TabHeight + 60;
          // Write RcdTabCert;
             Exfmt RcdCertHld;

          // Read  RcdTabCert;
       EndSl;

       Endsr;
       //===================================================================
       // Load Subfile Grid
       //===================================================================
       Begsr LoadUserGrid;

       // Clear the subfile
          clear SflCerHldr;
          scSflClr = *off;
          scSflDsp = *on;
          scSflDspCl = *on;
          clear rrn;

       // Populate the subfile
          Setll (intCo#:intFnd:intFyr:intEmp#:intDiv) Wmcel6;
          Reade (intCo#:intFnd:intFyr:intEmp#:intDiv) Wmcel6;
          DoW not %eof(Wmcel6);

             scName#   = Cename;
             scCertnbr = CeCer#;
             scCertRmk = CeRem1;
             scRolFlg  = CeRlfl;

             Select;
              When CEstat = 'A';
                 scStatus = 'Active';
              When CEstat = 'I';
                 scStatus = 'Inactive';
              Other;
                 scStatus = *blanks;
             EndSl;

             //***Beg Add ***** 04/11/18 ********************
             //Concatenate Address
             scAddr = %trim(CEAdr1);
             If CEAdr2 <> *blank;
                scAddr = %trim(scAddr) + ', ' + %trim(CEAdr2);
             Endif;
             If CECty <> *Blank and CEst <> *Blank;
               scAddr = %trim(scAddr) + ', ' + %trim(CECty) + ', ' +
                %trim(CEst) + '  ' + %subst(%editc(CEZip:'X'):1:5);
               If %subst(%editc(CEZip:'X'):6:4) <> '0000';
                  scAddr = %trim(scAddr) + '-' + %subst(%editc(CEZip:'X'):6:4);
               Endif;
             Else;
               scAddr = %trim(scAddr) + ', ' + %trim(CEprov) + ', ' +
                %trim(CECtry);
             Endif;
             //***End Add ***** 04/11/18 ********************

             Select;
              When inCall = 'C';
                 isLnkVisb = *on;
              When inCall = 'D';
                 isLnkVisb = *off;
              Other;
                 isLnkVisb = *off;
             EndSl;


             scRRN  = dbFds1.rrn;
             scSflRRN = scSflRRN +1;
             //*** Beg Chg ***** 04/11/18 *************
             //If scSflRRN =1;
             //   scSflSel = 'A';
             //Else;
             //   Clear scSflSel;
             //Endif;
             Select;
             When isMode = 'A' and intCEcer# = CEcer#;
                scSflSel = 'A';
                scHdnSflN = scSflRRN;
                Clear intCEcer#;
             Other;
                If scSflRRN = scHdnSflN;
                   scSflSel = 'A';
                Else;
                   Clear scSflSel;
                Endif;
             Endsl;
             //*** End Chg ***** 04/11/18 *************

             Write SflCerHldr;
             Reade (intCo#:intFnd:intFyr:intEmp#:intDiv) Wmcel6;
          Enddo;

          //*** Beg Chg ***** 05/23/19 *************
          //*****  scSflRRN = 1;
          scSflRRN = scHdnSflN;
          //*** Beg Chg ***** 05/23/19 *************

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================

     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

       scSflrrn = 0;
       scSflClr= *On;
       scSflDsp   = *Off;
       scSflDspCl = *Off;
       Write RcdCertHld;

      /End-Free
     p ClrSubfiles     e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       //*** begin add ***  01/10/17  ***********************************
       isUseScript = UseScriptura(processType);
       visScript = isUseScript;
       //*** end add *****  01/10/17  ***********************************
       //*** begin add ***  07/16/21  ***********************************
       If UseSpoolOut(processType);
          visScript = *off;
       Endif;
       //*** end add *****  07/16/21  ***********************************

       // Load Action panel with the Company Name.
       intCo# = userCo#(q1user);
       scCompName = GetCompNam(intCo#);
       scFnd  = userGrp(q1user);
       scname     = GetGrpName(Scfnd);
       //intFnd = scFnd;

       // Intialize the Screen fields
       LoadInVal();

       // When screen is called as pop up
       If q1prms >= 5;
          Eval isPopup  = '1';
          inRRn3 = %Dec(inRrn2:9:0);
       Endif;

       // Get Employer Information
       If inRRn3 <> 0;
          Chain inRRn3 pel0p;
          If %Found(Wdelp);
             intCo#  = ELco#;
             intFnd  = ELfnd;
             intEmp# = ELEmp#;
             intDiv  = ELdiv;
             intFyr  = ELFyr;

             Chain (intCo#:intFnd:intEmp#:intDiv) pem02;
             If %Found(Wmeml);
                scEDN =  EMemp#;
                scEmpName  = EMname;
                scFyr      = ElFyr;
             EndIf;
          scFnd  = ELfnd;
          scname = GetGrpName(Scfnd);
          EndIf;
       Endif;

       // Get Agent/Producer Information
       GetAgtPrdInfo(intco#:intFnd:intEmp#:intDiv:intFyr:intAgtNbr:intAgtNam:
                     intPrdNbr:intPrdNam:intArea:intPhone:intPlan:intDate);
          scAgt# = intAgtNbr;
          scAnam = intAgtNam;
          scProd = intPrdNbr;
          scPnam = intPrdNam;

       // Get Policy status
       scPyst  = GetQuoteStsDesc(intCo#:intFnd:inTemp#:intDiv:intFYR:intEnv);

       // Get the client id

       GetClientID(outCltId);

       // Retrieve Output Queue of the User.
       Chain q1user pup01;
       If %Found(Wmupl);
          strOutq = Upoutq;
       EndIf;

       isBtnVisb = *on;
       If inCall = 'D';
          isBtnVisb = *off;
       EndIf;

       //***Beg Add ***** 04/11/18 ********************
       If inCall = 'C';
         ChoSelAct = '--- Select List Action ---,Change Status,Rollover,Print';
       Else;
         ChoSelAct = '--- Select List Action ---,Print';
       Endif;
       scSelAct = '--- Select List Action ---';
       scHdnSflN = 1;

       //***End Add ***** 04/11/18 ********************

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // SetCriteria
       //===================================================================
     p SetCriteria     b
     d SetCriteria     pi

      /Free

       //GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');

      /End-Free

     p SetCriteria     e

       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi

      /Free

         isValid    = *off;
         msName     = *blanks;
         erName     = *off;
         msAddress  = *blanks;
         erAddress  = *off;
         msState    = *blanks;
         erState    = *off;
         msCity     = *blanks;
         erCity     = *off;
         msZip      = *blanks;
         erZip      = *off;
         msNbrCopie = *blanks;
         erNbrCopie = *off;
         msCertNbr  = *blanks;
         erCertNbr  = *off;
         msCntryCod = *blanks;
         erCntryCod = *off;
         msPrPost   = *blanks;
         erPrPost   = *off;
         msDartRmrk = *blanks;
         erDartRmrk = *off;
         msSflFlg   = *blanks;
         erSflFlg   = *off;
         msCertRg   = *blanks;
         erCertRg   = *off;

      /End-Free

     p ClrErr          e

       //===================================================================
       // ClrData - Clear Old data
       //===================================================================
     p ClrData         b
     d ClrData         pi

      /Free

       //ScCertNbr =  *Zeros;
       ScRevision=  *Zeros;
       scAddname =  *Blanks;
       scAddress =  *Blanks;
       scAddress2=  *Blanks;
       scCity    =  *Blanks;
       scState   =  *Blanks;
       scZip     =  *Zeros;
       scCntryCod = *Blanks;
       scProvnce  = *Blanks;
       Scatention = *Blanks;
       ScRmrkCod1 = *Blanks;
       ScRmrkCod2 = *Blanks;
       ScRmrkCod3 = *Blanks;
       ScRmrkCod4 = *Blanks;
       ScRmrkCod5 = ' ';
       scSpclJob  = ' ';
       ScOfcrExc  = ' ';
       ScThrdPrty = ' ';
       ScRollover = ' ';

      /End-Free

     p ClrData         e

       //*** Beg Add *** 02/18/19 ******************************************
       //===================================================================
       // LoadDft - Load any defaults
       //===================================================================
     p LoadDft         b
     d LoadDft         pi

      /Free

        // Remark Code 1 Default

        Chain (ELco#:ELrtCd) prs01;
        If %Found(wmrsl);
           Setgt (ELco#:ELfnd:RSjrSt:ELpfDt) pxc01;
           Readpe (ELco#:ELfnd:RSjrSt) pxc01;
           If not %Eof(wtxcl) and XCdRem <> *blanks;
              ScRmrkCod1 = XCdRem;
           Endif;
        Endif;

      /End-Free

     p LoadDft         e
       //*** End Add *** 02/18/19 ******************************************
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free

       // Validate Certificate number
         If scCertNbr = *Zeros;
            erCertNbr = *On;
            msCertNbr = 'WCR0503';
            isValid   = *On;
         Endif;

       // Validate that the Certificate number already exists
         If scCertNbr <> *Zeros and isMode = 'A';
            Chain (intCo#:intFnd:intFyr:intEmp#:intDiv:sccertNbr) DBRCDF;
            If %Found(Wmcel6);
               erCertNbr = *On;
               msCertNbr = 'WC30005';
               isValid   = *On;
            Endif;
         Endif;

       // Validation Check for Name Field
         If scAddname = *Blanks;
            ername   = *On;
            msName   = 'WCR0503';
            isValid  = *On;
         Endif;

       // Validation Check for Address
         If scAddress = *Blanks;
            erAddress = *On;
            msAddress = 'WCR0503';
            isValid  = *On;
         Endif;

       // Validation Check for City
         If ScCntryCod = *Blanks or ScCntryCod = 'USA';
            If scCity    = *Blanks;
               erCity    = *On;
               //*** Beg Chg ***** 04/11/18 *****************
               //msCity    = 'WCR0503';
               msCity = 'This field can not be blank.';
               //*** End Chg ***** 04/11/18 *****************
               isValid  = *On;
            Endif;
         Endif;

       // Validation for valid state
         If ScCntryCod = *Blanks or ScCntryCod ='USA';
            inKey = scstate;
            Chain inKey Pst01;
            If not %found(Ststl3);
               erState = *On;
               //*** Beg Chg ***** 04/11/18 *****************
               //msState = 'WCR0504';
               msState = 'The state code was not found in a +
                          valid list of values.';
               //*** End Chg ***** 04/11/18 *****************
               isValid = *On;
            Endif;
         Endif;

       // Validation Check for Zip
         If ScCntryCod = *Blanks or ScCntryCod ='USA';
            If scZip   <=  *Zeros;
               erZip   = *On;
               //*** Beg Chg ***** 04/11/18 *****************
               //msZip = 'WCR0502';
               msZip = 'This field can not be less than, or equal to zero.';
               //*** End Chg ***** 04/11/18 *****************
               isValid  = *On;
            Endif;
         Endif;

       // If Country other than USA has been entered
         If ScCntryCod <> 'USA' and  ScCntryCod <> *Blanks;
         // Country code must be valid
            inTbid = 'CTRY';
            inKey  = ScCntryCod;
            Chain (intCo#:inTbid:inkey) Pgt01;
            If not %found(Wtgtl) or GTStat <> 'A';
               erCntryCod = *On;
               msCntryCod = 'Country Code entered must be valid';
               isValid    = *On;
            Endif;

       // USA City should not be entered
            If scCity <> *Blanks;
               erCity  = *On;
               msCity  = 'City should not be entered for address outside USA.';
               isValid = *On;
            Endif;

       // USA State should not be entered
            If scState <> *Blanks;
               erState = *On;
               msState = 'State field should not be entered for address +
                           outside USA.';
               isValid = *On;
            Endif;

       // USA Zip Code should not be entered
            If scZip <> *Zeros;
               erZip  = *On;
               msZip  = 'Zip code should not be entered for address +
                         outside USA.';
               isValid = *On;
            Endif;

       // Province / Postal Code should be entered.
          //*** Beg Chg ***** 04/11/18 **********************
          //If scProvnce <> *Blanks;
            If scProvnce = *Blanks;
          //*** End Chg ***** 04/11/18 **********************
               erPrPost = *On;
               msPrPost = 'Province and Postal Code need to be entered.';
               isValid = *On;
            Endif;
         Endif;

       //If country code = USA, then "Province/Postal Code" should not
       //be entered.
         If ScCntryCod = 'USA' or ScCntryCod = *Blanks;
            If ScProvnce <> *Blanks;
               erPrPost = *On;
               msPrPost = 'Province/Postal Code should not be entered for +
                            address in USA.';
               isValid   = *On;
            Endif;
         Endif;

      /End-Free
     p ValidPnl        e

       //===================================================================
       // ValidPrnt - Validate Print Certificate Window
       //===================================================================
     p ValidPrnt       b
     d ValidPrnt       pi

      /Free

       // Validate the Darts Remark Flag
          If SCrsfl = 'Y';
             If scDrmk = *Blanks;
                erDartRmrk = *On;
                msDartRmrk = 'WCR0878';
                isValid = *On;
             Endif;
          Endif;

       // Validate the Darts Remark Flag
          If SCrsfl = 'N';
             If scDrmk <> *Blanks;
                erSflFlg = *On;
                MsSflFlg = 'WCR0501';
                isValid = *On;
             Endif;
          Endif;

       // Validate the range
          If ScCerf > ScCerT;
             erCertRg = *On;
             msCertRg = 'WCR2220';
             isValid = *On;
          Endif;

      /End-Free
     p ValidPrnt       e

       //===================================================================
       // AddDetails - Add Details
       //===================================================================
     P AddDetails      B
     d AddDetails      PI
     c
      /free

       Chain scRrn PFRCDF;
       CEcer# = ScCertNbr;
       CErev#  = ScRevision;
       CEname  = scAddname;
       CEadr1  = scAddress;
       CEadr2  = scAddress2;
       CEcty   = scCity;
       CEst    = scState;
       CEzip   = scZip;
       CEctry  = scCntryCod;
       CEprov  = scProvnce;
       CEattn  = Scatention;
       CErem1  = ScRmrkCod1;
       CErem2  = ScRmrkCod2;
       CErem3  = ScRmrkCod3;
       CErem4  = ScRmrkCod4;
       CErem5  = ScRmrkCod5;
       //*** Begin Chg *** 02/28/18 ********************************
       If scOfcrExc = ' ';
          scOfcrExc = 'N';
       EndIf;
       If scThrdPrty = ' ';
          scThrdPrty = 'N';
       EndIf;
       If scRollOver = ' ';
          scRollOver = 'N';
       EndIf;
       If scSpclJob = ' ';
          scSpclJob = 'N';
       EndIf;
       //*** End   Add *** 02/28/18 ********************************
       CEeofl  = ScOfcrExc;
       CE3pty  = ScThrdPrty;
       CErlfl  = ScRollover;
       CEType  = ScSpclJob;
      //***** Beg Add *** 04/25/17 ************************
       CEuser  = q1user      ;
       CEpgm   = q1pgm       ;
       CEtime  = %dec(%char(%time():*iso0):6:0);
       CEdt    = %dec(%char(%date():*cymd0):7:0);
      //***** End Add *** 04/25/17 ************************

       Update PFRCDF;

      /end-free
     P AddDetails      E

       //===================================================================
       // AddDetails1 - Add Details
       //===================================================================
     P AddDetails1     B
     d AddDetails1     PI
     c
      /free

       Clear  PFRCDF;
       CEco#   = intCo#;
       CEfnd   = intFnd;
       CEfyr   = intFyr;
       CEemp#  = intEmp#;
       CEdiv   = intDiv;
       CEcer# = ScCertNbr;
       CErev#  = ScRevision;
       CEname  = scAddname;
       CEadr1  = scAddress;
       CEadr2  = scAddress2;
       CEcty   = scCity;
       CEst    = scState;
       CEzip   = scZip;
       CEctry  = scCntryCod;
       CEprov  = scProvnce;
       CEattn  = Scatention;
       CErem1  = ScRmrkCod1;
       CErem2  = ScRmrkCod2;
       CErem3  = ScRmrkCod3;
       CErem4  = ScRmrkCod4;
       CErem5  = ScRmrkCod5;
       //*** Begin Chg *** 02/28/18 ********************************
       If scOfcrExc = ' ';
          scOfcrExc = 'N';
       EndIf;
       If scThrdPrty = ' ';
          scThrdPrty = 'N';
       EndIf;
       If scRollOver = ' ';
          scRollOver = 'N';
       EndIf;
       If scSpclJob = ' ';
          scSpclJob = 'N';
       EndIf;
       //*** End   Add *** 02/28/18 ********************************
       CEeofl  = ScOfcrExc;
       CE3pty  = ScThrdPrty;
       CErlfl  = ScRollover;
       CEType  = ScSpclJob;
       CEstat  = 'A';
      //***** Beg Add *** 04/25/17 ************************
       CEuser  = q1user      ;
       CEpgm   = q1pgm       ;
       CEtime  = %dec(%char(%time():*iso0):6:0);
       CEdt    = %dec(%char(%date():*cymd0):7:0);
       CEiusr  = q1user      ;
       CEipgm  = q1pgm       ;
       CEitm   = %dec(%char(%time():*iso0):6:0);
       CEidt   = %dec(%char(%date():*cymd0):7:0);
      //***** End Add *** 04/25/17 ************************
       //*** Beg Add ***** 04/11/18 ********************************
       intCEcer# = CECer#;
       scHdnSflN = 0;
       //*** End Add ***** 04/11/18 ********************************

       Write  PFRCDF;
       Unlock Wmcep;

      /end-free
     P AddDetails1     E

       //===================================================================
       // MoveDbSc - Move database fields to screen
       //===================================================================
     P MoveDbSc        B
     d MoveDbSc        PI
     c
      /free

       ScCertNbr  = CEcer#;
       ScRevision = CErev#;
       scAddname  = CEname;
       scAddress  = CEadr1;
       scAddress2 = CEadr2;
       scCity     = CEcty;
       scState    = CEst;
       scZip      = CEzip;
       scCntryCod = CEctry;
       scProvnce  = CEprov;
       Scatention = CEattn;
       ScRmrkCod1 = CErem1;
       ScRmrkCod2 = CErem2;
       ScRmrkCod3 = CErem3;
       ScRmrkCod4 = CErem4;
       ScRmrkCod5 = CErem5;
       scSpclJob  = CEtype;
       ScOfcrExc  = CEeofl;
       ScThrdPrty = CE3pty;
       ScRollover = CErlfl;

      /end-free
     P MoveDbSc        E
       //===================================================================
       //*** begin add ***  03/08/17  **************************************

       //===================================================================
       // ScriptPrompt - Display the Scriptura prompt screen.
       //===================================================================
     P ScriptPrompt    B
     d ScriptPrompt    PI
     c
      /free

       xxSubmit = *on;

       If isUseScript;

          xxProcType = processType;

          If isPrntAll = *on;
             xxMass = 'Y';
          Else;
             xxMass = ' ';
          Endif;

          In *Lock *dtaara;

          Clear svLDA;

          xxDart = 'N';
          xxRMCD = ' ';

          If scDrmk <> ' ';
             xxDart = 'Y';
             xxRMCD = scDrmk;
          Endif;
       //*** begin add ***  07/16/21  **************************************
          If xxBlank = 'Y';
             xx#CopiesA = %editc(scCopy:'X');
          Endif;
       //*** end add *****  07/16/21  **************************************

          Out *dtaara;

          //*** begin chg ***  08/13/21  ***********************************
          //***** xxSubmit = OutputWindow(xxProcType:xxMass);
          If inRrn3 <> 0;
             xxSubmit = OutputWindow(xxProcType:xxMass:' ':' ':inRrn3);
          Else;
             xxSubmit = OutputWindow(xxProcType:xxMass);
          Endif;
          //*** end chg *****  08/13/21  ***********************************

       //*** begin del ***  07/16/21  **************************************
       //*****   In *Lock *dtaara;
       //*****   Out *dtaara;
       //*****   scRsfl = xxDart;
       //*****   scDrmk = xxRMCD;
       //*** end del *****  07/16/21  **************************************

       Endif;

      /end-free
     P ScriptPrompt    E
       //*** end add *****  03/08/17  **************************************
