     H Copyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WPIDIVPOP - Dividend Setup poppups Service Program (Profound)
     H DFTNAME(WPIDIVPOP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H/DEFINE   PROFOUNDUI
     H**********************************************************************
    *===============================================================*
    *  Created   : 05/27/13                                         *
    *  Programmer: C:190                                            *
    *  Project   : 147WAI - OptaComp Dividend Plans Processing      *
    *===============================================================*
      *  Description:  Service program which contains the screen files
      *                and processing for Dividend Plan popups.
      *
      *  The popups included are:
      *    Payroll            (for adding/editing payroll classes)
      *
      *****************************************************************************************
      * REVISIONS:
     *
      *****************************************************************************************
     H nomain
    f*
     fwpidivpop cf   e             workstn extfile('WPIDIVPOP')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflListPol:rrnImpPol)
     f                                     usropn
     fsmcol     if   e           k disk    rename(pco01 : pco0l)
     fwtdvp1    uf a e             disk    rename(pdv01:pdv0P)
     fwtdvl11   if   e           k disk    rename(pdv01:pdv01)
     fwddvp     uf a e             disk    rename(pdv01:pdd0P)
     fwddvl     if   e           k disk    rename(pdv01:pdv0l) prefix(l_)
     f                                     infds(dvfds)

     d dvfds           ds
     d  dbrrn                397    400b 0
     d psds          esds                  extname(SZQ1P)

     d clientID        s              3    inz('000')

     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* load impacted policies due to Plan Id code change.
     d loadImpactPol   pr              n
     d inCO#                          3  0 const
     d inPlanID                      10    const
     d inquoteMode                    1    const

     d/copy sPrDate
     d/copy wPrDivPop
     d/copy sPrGenPop
     d/copy wPrEmplr
     d/copy wPrDivPlan
     d WriteDivPRmk    pr
     d  inCo#                         3  0 const
     d  inFnd                         3  0 const
     d  inEmp#                        9  0 const
     d  inDiv                         5  0 const
     d  inFyr                         3  0 const
     d WriteDarts      pr                  ExtPgm('DA700R')
     d  outKey                       40
     d  outComm                     256
     d  outCDat                       8
     d  outDcId                      12
     d  outPNam                      10
     d  outSecl                       3
     d  outUser                      10
     d  outPgm                       10
     d  outFold                      80
     d  outDiFl                       1
     d  outIPgm                      10
     d  outFl01                       1
     d  outFl02                       1
     d  outRmCd                       5
     d  outSeq#                       5
     d  outSbyl                       1
     d  outCom2                      19
     d  outApId                       2
     d  outIDoc                      12
     d  outIQue                      10
     d  outImgF                       1
     d  outIPth                      10
     d  outPIdx                       5
     d  outQPty                       1
     d  outDcTp                       5
     d  outCsId                       2
     d  outCode                       2

       //===================================================================
       // Dividend Plan Add/Edit Popup
       //===================================================================
     P DivdPlanEdit    B                   export
     D DivdPlanEdit    PI
     D inCO#                          3  0 const
     D inRRN                          9  0 const

     D divError        s               n   inz(*off)
     D divAddMode      s               n   inz(*off)
     D divEditMod      s               n   inz(*off)
     D iOpen           s               n   inz(*off)
     D intDate         s              7  0
     D intDate7        s              7  0
     D intToDate       s              7  0
     D intFromDate     s              7  0
     D dvRRN           s              9  0

      /free

       dvRRN = inRRN;
       Open WPIDIVPOP;
       If not %Open(wtdvp1);
          Open wtdvp1;
          iOpen = *On;
       Endif;

       GetClientID(clientID);

       divEditMod = *off;
       divAddMode = *off;

       If  dvRRN <> 0;
          Chain(n) dvRRN PDV0P;
          divEditMod = *on;
          scNextText = 'Save / Add New';
       Else;
          divAddMode = *on;
          scNextText = 'Save / Add Next';
       Endif;

       If divAddMode;
          DivdTitle = 'Dividend Plan ID Code Add';
       Else;
          DivdTitle = 'Dividend Plan ID Code Edit';
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou divAddMode = *off;

       //* Populate the popup window with selected record.

          If divEditMod = *on;
             scPlnIdCde =  DVplid;
             scPlanDesc =  DVpdsc;
             scPlanType =  DVptyp;
             scDivEffDT =  dtMDYY(DVefdt);
             scDivExpDT =  dtMDYY(DVexdt);
             scAutoRnew =  DVauto;
             scGrpDivd  =  DVgdiv;
          Else;
             Clear scPlnIdCde;
             Clear scPlanDesc;
             Clear scPlanType;
             Clear scDivEffDT;
             Clear scDivExpDT;
             Clear scAutoRnew;
             Clear scGrpDivd;
          Endif;

       //* Clear errors and buttons.
          Exsr ClearErrors;
          Clear divEditSav;
          Clear divEditCnl;
          Clear divEditAdd;

       //* Display popup window for Add or Edit mode.
          Dou divEditSav = *on or divEditCnl = *on or divEditAdd = *on;
             Dou divError = *off or divEditCnl = *on;
                Exfmt rcdDivdEdt;
                Exsr ClearErrors;
                   If divEditCnl = *off;
                      Exsr DividendCheck;
                   Endif;
             Enddo;
          Enddo;

          If divEditSav = *on or divEditAdd = *on;
             Exsr DividendSave;
          Endif;

          If divEditAdd = *on and divAddMode = *off;
             scNextText = 'Save / Add Next';
             divdTitle = 'Dividend Plan ID Code Add';
             divAddMode = *on;
             divEditMod = *off;
          Endif;

          If divEditCnl = *on or divEditSav = *on;
             divAddMode = *off;
          Endif;

       Enddo;

       If %Open(wtdvp1) And iOpen;
         Close wtdvp1;
         iOpen = *Off;
       Endif;

       Close WPIDIVPOP;

      /end-free
       //===================================================================
       // Dividend Error Checking
       //===================================================================
      /free
       Begsr DividendCheck;
       Exsr ClearErrors;
       // Dividend Plan ID should not be blank.
       If divAddMode = *On and scPlnIdCde = ' ';
          divError = *on;
          erDivdID = *on;
          msDivdID = 'WCR0503';
       Endif;

       // Effective date edits.
       // Effective date must be entered.
       If scDivEffDT = 0;
          divError= *on;
          msDivEEFDT = 'WCR0503';
          erDivEEFDT = *on;
       Endif;

       // Effective date is valid.
       intDate = 0;
       If scDivEffDT > 0;
          intDate = DtCYMD(scDivEffDT);
          If intDate = 0;
             divError = *on;
             msDivEEFDT = 'WCR0500';
             erDivEEFDT = *on;
          Endif;
       Else;
          intDate = 0;
       Endif;

       // If Exipration date is valid.
       If scDivExpDT > 0;
          intToDate = DtCYMD(scDivExpDT);
          If intToDate = 0;
             divError = *on;
             msDivExDt  = 'WCR0500';
             erDivExDt  = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only Exipration  date entered.
       If divError= *off and scDivEffDT = 0 and scDivExpDT > 0;
          divError = *on;
          msDivEEFDT = 'WCR2246';
          erDivEEFDT = *on;
       Endif;

       // If Effective date > Exipration date
       If divError = *off and scDivEffDT > 0 and scDivExpDT > 0
        and scDivEffDT > scDivExpDT;
          divError = *on;
          msDivEEFDT = 'WCR1502';
          msDivExDt  = 'WCR1502';
          erDivEEFDT = *on;
          erDivExDt  = *on;
       Endif;

       // Record should be unique(CO#, Plan ID, Effective Date)
       intDate7 = dtCYMD(scDivEffDT);
       Chain (inCO#:scPlnIdCde:intDate7) PDV01;
       If %found and divAddMode = *On;
          divError = *on;
          msDivdID   = 'WC30005';
          msDivEEFDT = 'WC30005';
          erDivdID   = *on;
          erDivEEFDT = *on;
       Endif;

       Endsr;
      /end-free
       //===================================================================
       // Save Dividend Data.
       //===================================================================
      /free
       Begsr DividendSave;

       If divEditMod = *on;
          Chain dvRRN PDV0P;
       Else;
          Clear PDV0P;
          DVco#  = inCO#;
          DVplid = scPlnIdCde;
          DVipgm = Q1PGM;
          DViusr = Q1USER;
          DVitim = Timeto6(SystemTime());
          DVidt  = Dateto7(SystemDate());
       Endif;

       DVefdt = dtCYMD(scDivEffDT);
       DVexdt = dtCYMD(scDivExpDT);
       DVpdsc = scPlanDesc;
       DVauto = scAutoRnew;
       DVgdiv = scGrpDivd;
       DVptyp = scPlanType;
       DVpgm  = Q1PGM;
       DVuser = Q1USER;
       DVtime = Timeto6(SystemTime());
       DVdt   = Dateto7(SystemDate());

       If divEditMod = *on;
          Update PDV0P;
       Else;
          Write PDV0P;
       Endif;

       Endsr;
      /end-free
       //===================================================================
       // Clear all Errors
       //===================================================================
      /free
       Begsr ClearErrors;

          divError = *off;
          erDivdID   = *off;
          msDivdID   = *blanks;
          erDivEEFDT = *off;
          msDivEEFDT = *blanks;
          erDivExDt  = *off;
          erDivExDt  = *blanks;

       Endsr;
      /end-free

     P DivdPlanEdit    E
       //===================================================================
       // Dividend Plan Premium Leval Popup
       //===================================================================
     P DivPremLevel    B                   export
     D DivPremLevel    PI
     D inCO#                          3  0 const
     D inRRN                          9  0 const
     D inPlanID                      10    const
     D inquoteMode                    1    const

     d prmError        s               n   inz(*off)
     d prmAddMode      s               n   inz(*off)
     d prmEditMod      s               n   inz(*off)
     d iOpen           s               n   inz(*off)
     d ddRRN           s              9  0

      /free

       ddRRN = inRRN;
       Open WPIDIVPOP;
       If not %Open(wddvp);
          Open wddvp;
          iOpen = *On;
       Endif;

       GetClientID(clientID);

       prmEditMod = *off;
       prmAddMode = *off;
       If  ddRRN <> 0;
          Chain(n) ddRRN PDD0P;
          prmEditMod = *on;
       Else;
          prmAddMode = *on;
       Endif;

       If prmAddMode;
          PrmLTitle = 'Dividend Plan Premium Levels Add';
       Else;
          PrmLTitle = 'Dividend Plan Premium Levels Edit';
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou prmAddMode = *off;

       //* Populate the popup window with selected record.
          If prmEditMod = *on;
             scMinPrem  =  DVmnlv;
             scMaxPrem  =  DVMxlv;
          Else;
             Clear scMinPrem;
             Clear scMaxPrem;
             scPrmIdCde = inPlanId;
          Endif;

       //* Clear errors and buttons.
          Exsr ClrPremErrors;
          Clear prmEditSav;
          Clear prmEditCnl;

       //* Display popup window for Add or Edit mode.
          Dou prmEditSav = *on or prmEditCnl = *on;
             Dou prmError = *off or prmEditCnl = *on;
                Exfmt rcdDivPrem;
                Exsr ClrPremErrors;
                   If prmEditCnl = *off;
                      Exsr DivPremLCheck;
                   Endif;
             Enddo;
          Enddo;

          If prmEditSav = *on;
             If DVmnlv <> scMinPrem or DVmxlv <> scMaxPrem;
                If loadImpactPol(inCO#:inPlanID:inquoteMode);
                   Dou BtnImpCnl = *On or BtnImpConf = *On;
                       Exfmt rcdImpPol;
                   Enddo;
                   Select;
                   When BtnImpConf = *On;
                        prmEditSav = *on;
                        BtnImpConf = *Off;
                        Exsr DivPremLSave;
                   Other;
                     prmEditSav = *Off;
                     BtnImpCnl  = *Off;
                   Endsl;
                Else;
                   If BtnImpCnl <> *On and BtnImpConf <> *On;
                      Exsr DivPremLSave;
                        prmEditSav = *off;
                   Endif;
                Endif;
             Endif;
          Endif;

          If prmEditCnl = *on or prmEditSav = *on;
             prmAddMode = *off;
          Endif;

       Enddo;

       If %Open(wddvp) And iOpen;
          Close wddvp;
          iOpen = *Off;
       Endif;

       Close WPIDIVPOP;

      /end-free
       //===================================================================
       // Dividend Plan Premium Level Error Checking
       //===================================================================
      /free
       Begsr DivPremLCheck;
          Exsr ClrPremErrors;
          // Minimum Premium must be less than Max Premium
          If scMinPrem  <> 0 and scMinPrem > scMaxPrem;
             prmError = *on;
             erMinPrem = *on;
             msMinPrem = 'Minimum Premium must be less than Maximum Premium';
          Endif;
          // Prem Lvl Ranges cannot overlap
          Setll (inCO#:inPlanId) PDV0l;
          Reade (inCO#:inPlanId) PDV0l;
          Dow not(%eof);
             If scMinPrem >= l_DVmnlv and scMinPrem < l_DVmxlv;
                If prmAddMode or prmEditMod and dbrrn <> inrrn;
                   prmError  = *on;
                   erMinPrem = *on;
                   msMinPrem = 'Premium ranges cannot overlap.';
                Endif;
             Endif;
             If scMinPrem < l_DVmnlv and scMaxPrem > l_DVmnlv;
                If prmAddMode or prmEditMod and dbrrn <> inrrn;
                   prmError  = *on;
                   erMaxPrem = *on;
                   msMaxPrem = 'Premium ranges cannot overlap.';
                Endif;
             Endif;
             If prmError;
                Leave;
             Endif;
          Reade (inCO#:inPlanId) PDV0l;
          Enddo;
       Endsr;
      /end-free
       //===================================================================
       // Save Dividend Data.
       //===================================================================
      /free
       Begsr DivPremLSave;

          If prmEditMod = *on;
             Chain ddRRN PDD0P;
          Else;
             Clear PDD0P;
             DVco#  = inCO#;
             DVplid = inPlanID;
             DVipgm = Q1PGM;
             DViusr = Q1USER;
             DVitim = Timeto6(SystemTime());
             DVidt  = Dateto7(SystemDate());
          Endif;

          DVmnlv = scMinPrem;
          DVmxlv = scMaxPrem;
          DVpgm  = Q1PGM;
          DVuser = Q1USER;
          DVtime = Timeto6(SystemTime());
          DVdt   = Dateto7(SystemDate());

          If prmEditMod = *on;
             If scMinPrem = 0 and scMaxPrem = 0;
                Delete PDD0P;
             Else;
                Update PDD0P;
             Endif;
          Else;
             Write PDD0P;
          Endif;

       Endsr;
      /end-free
       //===================================================================
       // Clear all Errors
       //===================================================================
      /free
       Begsr ClrPremErrors;

          prmError = *off;
          erMinPrem  = *off;
          msMinPrem  = *blanks;
          erMaxPrem  = *off;
          msMaxPrem  = *blanks;

       Endsr;
      /end-free

     P DivPremLevel    E
       //===================================================================
       // Load Impacted policies due to Plan ID code change.
       //===================================================================
     P loadImpactPol   B
     d loadImpactPol   PI              n
     d inCO#                          3  0 const
     d inPlanID                      10    const
     d inquoteMode                    1    const

     d xxCO#           s              3  0
     d xxFND           s              3  0
     d xxEMP#          s              9  0
     d xxDIV           s              5  0
     d xxFYR           s              3  0
     d xxDVPR          s             11  2
     d xxDVCD          s             10
     d outResult       s              1
     d outDivBill      s             11  2
     d outRRN          s              9  0

      /free

       GetClientID(clientID);
       // clear the subfile.
       dspImpCode = *off;
       clrImpPol  = *On;
       Write rcdImpPol;
       clrImpPol  = *Off;
       dspImpCode = *on;
       rrnImpPol  = 0;
      /end-free

     c/exec SQL
     c+   declare selectPolicies cursor
     c+      for select
     c+         distinct ELco#, ELfnd, ELemp#, ELdiv, ELfyr, ELdvpr, Eldvcd
     c+      from wdelp
     c+         where ELco# = :inCO# and ELdvcd = :inPlanID and ELdvpr <> 0
     c+         order by ELco#, ELfnd, ELemp#, ELdiv, ELfyr, ELdvpr, Eldvcd
     c/end-exec
     c/exec sql
     c+                  open selectPolicies
     c/end-exec

      /free
       SqlStt = '00000';
       dow SqlStt = '00000';
      /end-free

     c/exec sql
     c+   fetch next
     c+     from selectPolicies
     c+     into :xxCO#,
     c+          :xxFND,
     c+          :xxEMP#,
     c+          :xxDIV,
     c+          :xxFYR,
     c+          :xxDVPR,
     c+          :xxDVCD
     c/end-exec

      /free
          If SqlStt <> '00000';
             leave;
          Endif;
          outResult = ' ';
          outDivBill = 0;
          If ValidDivPlan(xxCO#:xxFnd:xxEmp#:xxDiv:xxFyr:xxDvcd) = *on;
             clear sflListPol;
             scImpGroup = xxFND;
             scImpEmp#  = xxEMP#;
             scImpDivsn = xxDIV;
             scImpYear  = xxFYR;
             scImpEmpNm = GetEmpName(xxEmp#:xxDiv:outRRN:xxfnd:xxCo#);
             scImpPrem  = xxDVPR;
             WriteDivPRmk(xxCO#:xxFnd:xxEmp#:xxDiv:xxFyr);
             rrnImpPol = rrnImpPol + 1;
             Write sflListPol;
          Endif;
       Enddo;
      /end-free

     c/exec sql
     c+   close selectPolicies
     c/end-exec

      /free
       If rrnImpPol >= 1;
          rrnImpPol  = 1;
          return *On;
       Else;
          return *Off;
       Endif;
      /end-free

     P loadImpactPol   E
      *==============================================================
      * WriteDivPRmk - Write DARTS Remark if division doesn't qualify
      *                for diviend
      *==============================================================
     p WriteDivPRmk    b
     d WriteDivPRmk    pi
     d  inCo#                         3  0 const
     d  inFnd                         3  0 const
     d  inEmp#                        9  0 const
     d  inDiv                         5  0 const
     d  inFyr                         3  0 const
     d  outKey         s             40
     d  outComm        s            256
     d  outCDat        s              8
     d  outDcId        s             12
     d  outPNam        s             10
     d  outSecl        s              3
     d  outUser        s             10
     d  outPgm         s             10
     d  outFold        s             80
     d  outDiFl        s              1
     d  outIPgm        s             10
     d  outFl01        s              1
     d  outFl02        s              1
     d  outRmCd        s              5
     d  outSeq#        s              5
     d  outSbyl        s              1
     d  outCom2        s             19
     d  outApId        s              2
     d  outIDoc        s             12
     d  outIQue        s             10
     d  outImgF        s              1
     d  outIPth        s             10
     d  outPIdx        s              5
     d  outQPty        s              1
     d  outDcTp        s              5
     d  outCsId        s              2
     d  outCode        s              2
     d  strCo#         s              3
     d  strFnd         s              3
     d  strEmp#        s              9
     d  strDiv         s              5
     d  strFyr         s              3

      /free
       Chain (inCo#) pco0l;
       If COprth = *blank;
          Return;
       Else;
          outRmcd = COprth;
       Endif;
       strCo#  = %Editc(inCo#  : 'X');
       strFnd  = %Editc(inFnd  : 'X');
       strEmp# = %Editc(inEmp# : 'X');
       strDiv  = %Editc(inDiv  : 'X');
       strFyr  = %Editc(inFyr  : 'X');
       outKey  = strCo# + strFnd + strEmp# + strDiv;
       outComm = 'Policy year ' + strFyr + ' does not meet ' +
        'criteria for dividend processing.';
       outCDat = %Editc(DateTo8(systemDate()):'X');
       outPNam = 'W0050R';
       outUser = Q1user;
       outPgm  = Q1pgm;
       outIPgm = Q1pgm;
       WriteDARTS (outKey  : outComm : outCDat : outDcId :
                   outPNam : outSecl : outUser : outPgm  :
                   outFold : outDiFl : outIPgm : outFl01 :
                   outFl02 : outRmCd : outSeq# : outSbyl :
                   outCom2 : outApId : outIDoc : outIQue :
                   outImgF : outIPth : outPIdx : outQPty :
                   outDcTp : outCsId : outCode );
      /end-free
     p WriteDivPRmk    e
