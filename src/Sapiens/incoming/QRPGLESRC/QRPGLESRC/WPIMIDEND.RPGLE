     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H/TITLE WPIMIDEND  - Mid-term endorsement procedures
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h nomain
      *===============================================================*
      *  Created   : 05/25/12                                         *
      *  Programmer: C:191                                            *
      *  Project   : WC2116                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *  02/12/25 C:342 Support: 150/118447                           *
      *                 Add edit for "Reason for Cancellation" field  *
      *                 in case of Deletion of Division Endorsement.  *
      *                 DSPF Changes:                                 *
      *                 Added error message for field scDelR in       *
      *                 WPIMIDEND. Removed Required property.         *
      *                                                               *
      *  04/22/22 C:079 Support: 140/97336                            *
      *                 Remove dash from FEIN's on change.            *
      *                                                               *
      *  02/02/21 C:136 Project: WC2757                               *
      *                 Change waiver procedures to use               *
      *                 SR00313 as the calling program so that        *
      *                 W0815R02 works properly.                      *
      *                                                               *
      *  12/02/19 C:180 Support:84703                                 *
      *                 Fix county during mailing address update.     *
      *                                                               *
      *  09/20/18 C:267 Project: WC2584                               *
      *                 Add procedure isPenRec to check existance of  *
      *                 record in WMENP. Used along-side State Specific
      *                 endorsement form.                             *
      *                                                               *
      *  03/05/18 C:136 Project: 150WCT                               *
      *                 Add midterm endorsement for additional        *
      *                 name insured                                  *
      *                                                               *
      *  12/30/15  C:306 Support: 38532                               *
      *                  Stop from changing the mailing  address for  *
      *                  expired Location.                            *
      *                                                               *
      *  10/13/14  C:302 Support: 86753                               *
      *                  Stop from changing the physical address with *
      *                  the mailing address unless specify.          *
      *                                                               *
      *  03/28/14  C:302 Support: 86841                               *
      *                  Corrected leading zeros in zip codes.        *
      *                                                               *
      *  03/04/14  C:290 Support: WA133 84870                         *
      *                  Corrected the mailing and physical address   *
      *                  fields used in Physical/mailing Address      *
      *                  change endorsement.                          *
      *                                                               *
      *  11/15/13  C:283 Support: WA133 84193                         *
      *                  Call the procedure to fetch the County       *
      *                  name with correct key.                       *
      *                                                               *
      *  09/16/13  C:287 Support: WA133 83162                         *
      *                  Correctly format the Mail/Physical Address   *
      *                  change used in endorsement. Also, changed    *
      *                  assignment operator which was incorrectly    *
      *                  used to accumulate a count for SEQ#          *
      *                  Incorrect   - 1216              xxSeq# =+1;  *
      *     The above statement is equal to Eval xxSeq# = 1;          *
      *                 Correct use - 1216              xxSeq# +=1;   *
      *    The above statement is equal to Eval xxSeq# = xxSeq# + 1;  *
      *                                                               *
      *  07/26/13  C:295 Support: DR133 82862                         *
      *                  Pass the correct Darts Remark Header for     *
      *                  Physical Address Change endorsement.         *
      *                                                               *
      *  07/11/13  C:191 Redmine: 3329 (LUBA)                         *
      *                  Misc change endorsement should generate even *
      *                  if before/after is not entered               *

      *  07/01/13  C:283 Support: DR146 82643                         *
      *                  Pass the correct Darts Remark Code for       *
      *                  Physical Address Change endorsement.         *
      *                                                               *
      *  06/17/13  C:291 Redmine: 2970                                *
      *                  Fixed heading from Employee master to Billing*
      *                  Division on Mailing Addresss Change screen.  *
      *                  Turn off btnNext and btnPrv before exceuting *
      *                  Address Change Endorsement screen.           *
      *                                                               *
      *  05/17/13  C:282 Redmine:2495 LUBA                            *
      *                  Introducing the space between State and      *
      *                  Zip Code.                                    *
      *                  Changing the key to read the file wdf2l in   *
      *                  InitEndorse procedure.                       *
      *                                                               *
      *  03/19/13  C:191 Redmine:2463 CSI                             *
      *                  Misc change endorsement should print         *
      *                  description from screen                      *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
     fwpimidend cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflMiscChg:rrnMiscChg)
     f                                     usropn
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     fwmdxl     if   e           k disk    rename(pdx01:pdx00)
     fwmenl3    if a e           k disk    rename(pen01:pen03)
     fwtmyp     if   e             disk    rename(pmy01:pmy0p)
     fwmall     uf   e           k disk    rename(pal01:pal00)
     fwmemp     uf   e             disk    rename(pem01:pem0p)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwmefp     if   e             disk    rename(pef01:pef0p)
     fwmczl     if   e           k disk

      /copy sprDate
      /copy sprString
      /copy wprAgent
      /copy wprBank
      /copy wprEft
      /copy wprEmplr
      /copy wprGroup
      /copy wprLocMst
      /copy wprLocMain
      /copy wprMidEnd
      /copy wprTable
      /copy wprUsrPrms
      /copy wprEndorse

     d svEmp           ds                  likerec(pem0p)
     D flgFirst        s               n
     D xxAF01          s             50    inz
     D xxB401          s             50    inz
     D xxSeq#          s              5  0 inz
     D xxTbid          s              4    inz
     D xxKey           s             10    inz
     D rrnMiscChg      s              9  0

     D PSDS          ESDS                  extname(SZQ1P)

     D SVLDA           DS          1024    DTAARA(*LDA)
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     d  ldOWCg                28     28
     d  ldPeoImport           29     29
     d  ldAddNamEnd           30     30
     d  ldStSpcfFlg           40     40
     d  ldWaiverFrm           41     55
     d  ldFlowName           131    190

     d SrAddr          pr                  extpgm('SRADDR')
     d ##co#                          3    const
     d ##fnd                          3    const
     d ##emp#                         9    const
     d ##div                          5    const
     d ##odr                          1
     d ##nodr                         1
     d ##aodr                         1
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d ##adrl                         3  0
     d ##naml                         3  0
     d ##fedl                         3  0
     d ##peo1                        57
     d ##peo2                        57
     d ##pgfl                         1
     d ##conl                         3  0

     d scBBillAdrds    ds
     d scBillAdr1                    50
     d scBillAdr2                    50
     d scBillAdr3                    50
     d scBillAdr4                    50
     d scBBillAdr                    50    Dim(4)
     d                                     OVERLAY(scBBillAdrds)

     dAddNameChange    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dWrtAddNameRec    pr
     d inEEDT                         8  0

     dEmpNameChange    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dEmpMAddChange    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dEmpPAddChange    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dEmpLegalChange   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     d* Location/Entity Mid-term endorsement
     d ProcLocEntEnd   pr                  extpgm('WTPOLLMTE')
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0
     d inType                         1
     d inDlt                          1

     d* Owner/Offcer Mid-term endorsement
     d ProcOwnOffEnd   pr                  extpgm('WTPOLOMTE')
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0
     d inType                         1
     d inAcx                          1

     d* Revised bill process
     d ProcRevBill     pr                  extpgm('WTPOLRBILL')
     d icELrrn                        9
     d inMode                         1
     d outNext                       10
     d inTabInd                       5
     d inQte                          1

     d* Waiver of Subro Mid-term endorsement
     d ProcSubroEnd    pr                  extpgm('W0815R')
     d inKC                           1
     d inKL                           1
     d inCall                         1
     d in00rrn                        9  0
     d inELrrn                        9  0
     d inPgm                         10
     d inWtyp                         1

     dCertOfInsChngs   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     d* Certificate of Insurance changes
     d ProcCertIns     pr                  extpgm('W0051R22')
     d inKC                           1
     d inKL                           1
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     d* Process several Mid-term endorsement
     d ProcMiscEnd     pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0
     d inEnd                          3     const

     dLocMstMidEnd     pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0
     d inEnd#                         5

     dLocEntAddition   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dLocEntChanges    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dLocEntDeletion   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dOwnOffAddition   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dOwnOffChanges    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dOwnOffDeletion   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dEmpFEINChange    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dEmpMiscChange    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dAddDivision      pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dDeleteDivision   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dRevisedBill      pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dRescindDivision  pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dConExpModChg     pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dSubroAddition    pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dSubroChange      pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dSubroDelete      pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dSR42304B         pr                  extpgm('SR42304B')
     d outCo#                         3
     d outFnd                         3
     d outEmp                         9
     d outDiv                         5
     d outFyr                         3
     d outMode                        1
     d outElRrn                       9  0
     d outWvrType                     1

     dInitEndorse      pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0

     dProcNameChange   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dProcMAddChange   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dProcPAddChange   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dProcLegalChange  pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dProcFeinChange   pr              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     dProcAddDivEnd    pr                  extpgm('WTPOLQTE')
     d icELrrn                        9
     d inMode                         1
     d inNext                        10
     d inTabInd                       5
     d inQte                          1

     dWriteDltDivEnd   pr                  extpgm('WTPOLDLTDV')
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0
     d inDelr                        50

     dWriteResDivEnd   pr                  extpgm('WTPOLRESDV')
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0
     d inResr                        50

     dWriteNameRec     pr              n
     d inEEDT                         8  0
     d inRsn                          3  0 const

     dWriteMaddRec     pr              n
     d inEEDT                         8  0
     d inRsn                          3  0 const

     dWritePaddRec     pr              n
     d inEEDT                         8  0
     d inRsn                          3  0 const

     dValidMiscChg     pr              n
     dValidConExp      pr              n
     dUpdateLocEnt     pr
     D inEEDT                         8  0
     p*=====================================================================
     p* GetProcPtr  - Get relevant procedure pointer
     p*  Parm1   - Name of the procedure
     p*  Returns - pointer to the procedure
     P*=====================================================================
     PGetProcPtr       b                   export
     DGetProcPtr       pi              *   procptr
     D inProcName                    10
      *
     D ptrProc         s               *   procptr inz
      /Free
        Select;
        When inProcName = 'W0051R11';
          ptrProc = %paddr(EmpNameChange);
        When inProcName = 'W0051R12';
          ptrProc = %paddr(EmpMAddChange);
        When inProcName = 'W0051R13';
          ptrProc = %paddr(EmpPAddChange);
        When inProcName = 'W0051R14';
          ptrProc = %paddr(EmpLegalChange);
        When inProcName = 'W0051R15';
          ptrProc = %paddr(LocEntAddition);
        When inProcName = 'W0051R16';
          ptrProc = %paddr(OwnOffAddition);
        When inProcName = 'W0051R17';
          ptrProc = %paddr(EmpMiscChange);
        When inProcName = 'W0051R18';
          ptrProc = %paddr(EmpFEINChange);
        When inProcName = 'W0051R19';
          ptrProc = %paddr(AddDivision);
        When inProcName = 'W0051R20';
          ptrProc = %paddr(DeleteDivision);
        When inProcName = 'W0051R21';
          ptrProc = %paddr(RevisedBill);
        When inProcName = 'W0051R22';
          ptrProc = %paddr(CertOfInsChngs);
        When inProcName = 'W0051R23';
          ptrProc = %paddr(ConExpModChg);
        When inProcName = 'W0051R24';
          ptrProc = %paddr(SubroAddition);
        When inProcName = 'W0051R25';
          ptrProc = %paddr(SubroChange);
        When inProcName = 'W0051R26';
          ptrProc = %paddr(SubroDelete);
        When inProcName = 'W0051R40';
          ptrProc = %paddr(RescindDivision);
        When inProcName = 'W0051R55';
          ptrProc = %paddr(LocEntChanges);
        When inProcName = 'W0051R56';
          ptrProc = %paddr(OwnOffChanges);
        When inProcName = 'W0051R65';
          ptrProc = %paddr(LocEntDeletion);
        When inProcName = 'W0051R66';
          ptrProc = %paddr(OwnOffDeletion);
        When inProcName = 'ADDNAMEINS';
          ptrProc = %paddr(AddNameChange);
        EndSl;

        Return ptrProc;
      /End-Free

     PGetProcPtr       E
     p*=====================================================================
     p* isPenRec - Chk WMENP record existance
     p*  Parms   - CO#, FND, EMP#, DIV, FYR, JOBN, END#
     p*  Returns - found indicator
     P*=====================================================================
     PisPenRec         b                   export
     DisPenRec         pi              n
     D inCO#                               like(ELCO#)
     D inFND                               like(ELFND)
     D inEMP#                              like(ELEMP#)
     D inDIV                               like(ELDIV)
     D inFYR                               like(ELFYR)
     D inJOBN                              like(Q1JOBN)
     D inEND#                              like(ENEND#)
      *
     C     ENKY          KLIST
     C                   KFLD                    inCO#
     C                   KFLD                    inFND
     C                   KFLD                    inEMP#
     C                   KFLD                    inDIV
     C                   KFLD                    inFYR
     C                   KFLD                    inJOBN
     C                   KFLD                    inEND#
     C*
      /Free
        Chain ENKY pen03;
        If %Found(WMENL3);
          Return *On;
        Else;
          Return *Off;
        Endif;
      /End-Free

     PisPenRec         E
     p*=====================================================================
     p* GetPenRec  - Get WMENP record
     p*  Parm1   - Endorsement number
     p*  Returns - found indicator
     P*=====================================================================
     PGetPenRec        b                   export
     DGetPenRec        pi              n
     D inEND#                              like(ENEND#)
      *
     C     ENKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    inEND#
     C*
      /Free
        Chain ENKY pen03;
        If %Found(WMENL3);
          Return *On;
        Else;
          Return *Off;
        Endif;
      /End-Free

     PGetPenRec        E
      *===========================================
      * Process additional named insured endorsement
      *===========================================
     p AddNameChange   b
     d AddNameChange   pi              n
     d inEMrrn                        9  0
     d inELrrn                        9  0
     d inMYrrn                        9  0
     d inEEDT                         8  0

     d xxELrrn         s              9    inz
     d xxEnd#          s              5    inz('00250')
     d xxMode          s              1    inz('M')
     d xxRtcd          s               n
     d xxRtnVal        s             10    inz

      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxELrrn = %Editc(inELrrn:'X');
          LocMaster(xxELrrn:xxMode:xxRtnVal:xxEnd#);
          If xxRtnVal = 'NEXT';
            WrtAddNameRec(inEEDt);
            xxRtcd = *On;
          Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     p AddNameChange   e
      *===========================================
     p*=====================================================================
     p* EmpNameChange - Employer name change endorsement
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Endorsement table RRN
     p*  Returns - *On for success
     P*=====================================================================
     PEmpNameChange    b                   export
     DEmpNameChange    pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxMode          s              1    inz('C')
     D xxRtcd          s               n
     D xxEnd#          s              3    inz('110')
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          If DspBillDiv(inEMrrn:xxEnd#:xxMode);
            If ProcNameChange(inEMrrn:inELrrn:inMYrrn:inEEDT);
              xxRtcd = *On;
            Endif;
          Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     PEmpNameChange    E
     P*=====================================================================
     PEmpMAddChange    b                   export
     DEmpMAddChange    pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxMode          s              1    inz('C')
     D xxRtcd          s               n
     D ExitPop         s               n
     D ExitBill        s               n
     D xxEnd#          s              3    inz('120')
      /Free
        xxRtcd = *Off;
        ExitPop = *Off;
        ExitBill = *Off;
        Open wpiMidEnd;

        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          DoU ExitBill = *On;
            If DspBillDiv(inEMrrn:xxEnd#:xxMode);
              DoU ExitPop = *On;
                isMailing = *On;
                scAddTtl = %Trim(MYdesc) + ' Endorsement';
                  btnNext = *Off;
                  btnPrv  = *Off;
                Exfmt rcdAddress;
                Select;
                When btnNext = *On;
                  If ProcMaddChange(inEMrrn:inELrrn:inMYrrn:inEEDT);
                    xxRtcd = *On;
                  Endif;
                  ExitBill = *On;
                  leave;
                When btnPrv = *On;
                    ExitPop = *On;
                EndSl;
              EndDo;
            Else;
              Leave;
            Endif;
          EndDo;
        Endif;

        Close wpimidend;
        Return xxRtcd;
      /End-Free

     PEmpMAddChange    E
     P*=====================================================================
     PEmpPAddChange    b                   export
     DEmpPAddChange    pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxMode          s              1    inz('C')
     D xxRtcd          s               n
     D ExitPop         s               n
     D ExitBill        s               n
     D xxEnd#          s              3    inz('130')
      /Free
        xxRtcd = *Off;
        ExitPop = *Off;
        ExitBill = *Off;
        Open wpiMidEnd;

        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          DoU ExitBill = *On;
            If DspBillDiv(inEMrrn:xxEnd#:xxMode);
              DoU ExitPop = *On;
                isMailing = *Off;
                scAddTtl = %Trim(MYdesc) + ' Endorsement';
                Exfmt rcdAddress;
                Select;
                When btnNext = *On;
                  If ProcPAddChange(inEMrrn:inELrrn:inMYrrn:inEEDT);
                    xxRtcd = *On;
                  Endif;
                  ExitBill = *On;
                  leave;
                When btnPrv = *On;
                    ExitPop = *On;
                EndSl;
              EndDo;
            Else;
              Leave;
            Endif;
          EndDo;
        Endif;

        Close wpimidend;
        Return xxRtcd;
      /End-Free

     PEmpPAddChange    E
     P*=====================================================================
     PEmpLegalChange   b                   export
     DEmpLegalChange   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxMode          s              1    inz('C')
     D xxRtcd          s               n
     D xxEnd#          s              3    inz('140')
      /Free
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          If DspBillDiv(inEMrrn:xxEnd#:xxMode);
            //Change done, invoke mid-term end process
            xxRtcd = ProcLegalChange(inEMrrn:inELrrn:inMYrrn:inEEDT);
          Endif;
        Endif;

        Return xxRtcd;
      /End-Free

     PEmpLegalChange   E
     p*=====================================================================
     p* LocMstMidEnd - Process mid-term endorsements from Location master
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Endorsement table RRN
     p*  Parm4   - Endorsement eff date
     p*  Parm5   - Endorsement#
     p*  Returns - *On for success
     P*=====================================================================
     PLocMstMidEnd     b                   export
     DLocMstMidEnd     pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
     D inEnd#                         5
      *
     D xxMode          s              1    inz('M')
     D xxType          s              1    inz
     D xxDlt           s              1    inz
     D xxRtnVal        s             10    inz
     D xxRtcd          s               n   inz
     D xxELrrn         s              9    inz
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxType = 'S';
          Select;
          When inEnd# = '00152';
            xxDlt = 'Y';
          Other;
            xxDlt = ' ';
          EndSl;
          ProcLocEntEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxType:xxDlt);
          xxELrrn = %Editc(inELrrn:'X');
          LocMaster(xxELrrn:xxMode:xxRtnVal:inEnd#);
          If xxRtnVal = 'NEXT';
            xxType = 'E';
            ProcLocEntEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxType:xxDlt);
            xxRtcd = *On;
          Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     PLocMstMidEnd     E
     p*=====================================================================
     p* LocEntAddition - Addition of Entity/Location mid-term endorsement
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Endorsement table RRN
     p*  Parm4   - Endorsement eff date
     p*  Returns - *On for success
     P*=====================================================================
     PLocEntAddition   b                   export
     DLocEntAddition   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxEnd#          s              5    inz('00150')
     D xxRtcd          s               n   inz
      /Free
        xxRtcd = *Off;
        If LocMstMidEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxEnd#);
          xxRtcd = *On;
        Endif;
        Return xxRtcd;
      /End-Free

     PLocEntAddition   E
     p*=====================================================================
     p* LocEntChanges - Change of Entity/Location endorsement
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Endorsement table RRN
     p*  Parm4   - Endorsement eff date
     p*  Returns - *On for success
     P*=====================================================================
     PLocEntChanges    b                   export
     DLocEntChanges    pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxEnd#          s              5    inz('00151')
     D xxRtcd          s               n   inz
      /Free
        xxRtcd = *Off;
        If LocMstMidEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxEnd#);
            xxRtcd = *On;
        Endif;
        Return xxRtcd;
      /End-Free

     PLocEntChanges    E
     p*=====================================================================
     p* LocEntDeletion -Change of Entity/Location endorsement
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Endorsement table RRN
     p*  Parm4   - Endorsement eff date
     p*  Returns - *On for success
     P*=====================================================================
     PLocEntDeletion   b                   export
     DLocEntDeletion   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxEnd#          s              5    inz('00152')
     D xxRtcd          s               n   inz
      /Free
        xxRtcd = *Off;
        If LocMstMidEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxEnd#);
            xxRtcd = *On;
        Endif;
        Return xxRtcd;
      /End-Free

     PLocEntDeletion   E
     p*=====================================================================
     p* OwnOffAddition - Addition of Owners/Officers mid-term endorsement
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Endorsement table RRN
     p*  Parm4   - Endorsement eff date
     p*  Returns - *On for success
     P*=====================================================================
     POwnOffAddition   b                   export
     DOwnOffAddition   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
     D xxRtnVal        s             10    inz
     D xxType          s              1    inz
     D xxAcx           s              1    inz
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxType = 'S';
          xxAcx = 'A';
          ProcOwnOffEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxType:xxAcx);
          xxRtnVal = PrcOwnOffGrd(inEMrrn:'160':'C');
          If xxRtnVal = 'SAVE_NEXT';
            xxType = 'E';
            ProcOwnOffEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxType:xxAcx);
              xxRtcd = *On;
          Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     POwnOffAddition   E
     p*=====================================================================
     p* OwnOffChanges - Change of Owners/Officers endorsement
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Endorsement table RRN
     p*  Parm4   - Endorsement eff date
     p*  Returns - *On for success
     P*=====================================================================
     POwnOffChanges    b                   export
     DOwnOffChanges    pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
     D xxRtnVal        s             10    inz
     D xxType          s              1    inz
     D xxAcx           s              1    inz
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxType = 'S';
          xxAcx = 'C';
          ProcOwnOffEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxType:xxAcx);
          xxRtnVal = PrcOwnOffGrd(inEMrrn:'161':'C');
          If xxRtnVal = 'SAVE_NEXT';
            xxType = 'E';
            ProcOwnOffEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxType:xxAcx);
              xxRtcd = *On;
          Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     POwnOffChanges    E
     p*=====================================================================
     p* OwnOffDeletion -Change of Owners/Officers endorsement
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Endorsement table RRN
     p*  Parm4   - Endorsement eff date
     p*  Returns - *On for success
     P*=====================================================================
     POwnOffDeletion   b                   export
     DOwnOffDeletion   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
     D xxRtnVal        s             10    inz
     D xxType          s              1    inz
     D xxAcx           s              1    inz
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxType = 'S';
          xxAcx = 'X';
          ProcOwnOffEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxType:xxAcx);
          xxRtnVal = PrcOwnOffGrd(inEMrrn:'162':'C');
          If xxRtnVal = 'SAVE_NEXT';
            xxType = 'E';
            ProcOwnOffEnd(inEMrrn:inELrrn:inMYrrn:inEEDt:xxType:xxAcx);
              xxRtcd = *On;
          Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     POwnOffDeletion   E
     P*=====================================================================
     PEmpFEINChange    b                   export
     DEmpFEINChange    pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxMode          s              1    inz('C')
     D xxRtcd          s               n   inz
     D xxEnd#          s              3    inz('180')
      /Free
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          If DspBillDiv(inEMrrn:xxEnd#:xxMode);
            //Change done, invoke mid-term end process
            xxRtcd = ProcFeinChange(inEMrrn:inELrrn:inMYrrn:inEEDT);
          Endif;
        Endif;

        Return xxRtcd;
      /End-Free

     PEmpFEINChange    e
     P*=====================================================================
     PEmpMiscChange    b                   export
     DEmpMiscChange    pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
      /Free
        Open wpiMidEnd;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxRtcd = ProcMiscEnd(inEMrrn:inELrrn:inMYrrn:inEEdt:'170');
        Endif;

        Close wpiMidEnd;
        Return xxRtcd;
      /End-Free

     PEmpMiscChange    e
     P*=====================================================================
     PConExpModChg     b                   export
     DConExpModChg     pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
      /Free
        Open wpiMidEnd;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxRtcd = ProcMiscEnd(inEMrrn:inELrrn:inMYrrn:inEEdt:'230');
        Endif;

        Close wpiMidEnd;
        Return xxRtcd;
      /End-Free

     PConExpModChg     e
     P*=====================================================================
     PAddDivision      b                   export
     DAddDivision      pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxMode          s              1    inz('M')
     D xxELrrn         s              9    inz
     D xxRtcd          s               n   inz
     D xxNext          s             10    inz
     D xxTab           s              5    inz('00001')
     D xxQte           s              1    inz
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxELrrn = %EditC(inELrrn:'X');
          ProcAddDivEnd(xxELrrn:xxMode:xxNext:xxTab:xxQte);
          Select;
          When xxNext = 'NEXT';
            xxRtcd = *On;
          When xxNext = 'CANCEL';
            xxRtcd = *Off;
          Endsl;
        Endif;

        Return xxRtcd;
      /End-Free

     PAddDivision      e
     P*=====================================================================
     PDeleteDivision   b                   export
     DDeleteDivision   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
      /Free
        Open wpiMidEnd;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxRtcd = ProcMiscEnd(inEMrrn:inELrrn:inMYrrn:inEEdt:'200');
        Endif;

        Close wpiMidEnd;
        Return xxRtcd;
      /End-Free

     PDeleteDivision   e
     P*=====================================================================
     PRescindDivision  b                   export
     DRescindDivision  pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
      /Free
        Open wpiMidEnd;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxRtcd = ProcMiscEnd(inEMrrn:inELrrn:inMYrrn:inEEdt:'205');
        Endif;

        Close wpiMidEnd;
        Return xxRtcd;
      /End-Free

     PRescindDivision  e
     P*=====================================================================
     PRevisedBill      b                   export
     DRevisedBill      pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
     D xxELrrn         s              9    inz
     D xxMode          s              1    inz
     D xxNext          s             10    inz
     D xxTabInd        s              5    inz
     D xxQte           s              1    inz
      /Free
        Open wpiMidEnd;
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          xxELrrn = %Editc(inELrrn:'X');
          xxMode = 'E';
          xxNext = *Blanks;
          xxTabInd = *Zeros;
          xxQte = *Blanks;
          ProcRevBill(xxELrrn:xxMode:xxNext:xxTabInd:xxQte);
          Select;
          When xxNext = 'NEXT';
            xxRtcd = *On;
          When xxNext = 'CANCEL';
            xxRtcd = *Off;
          EndSl;
        Endif;

        Close wpiMidEnd;
        Return xxRtcd;
      /End-Free

     PRevisedBill      e
     P*=====================================================================
     PCertOfInsChngs   b                   export
     DCertOfInsChngs   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
     D xxKC            s              1    inz('0')
     D xxKL            s              1    inz('0')
     D xxCall          s              1    inz
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
          ProcCertIns(xxKC:xxKL:inEMrrn:inELrrn:inMYrrn:inEEDT);
          If xxKC = *On Or xxKL = *On;
            xxRtcd = *Off;
          Else;
            xxRtcd = *On;
          Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     PCertOfInsChngs   e
     P*=====================================================================
     PSubroAddition    b                   export
     DSubroAddition    pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
     D xxKC            s              1    inz('0')
     D xxKL            s              1    inz('0')
     D xxCall          s              1    inz
     D xx00rrn         s              9  0 inz(0)
     D xxPgm           s             10    inz('W0051R24')
     D xxWtyp          s              1    inz('A')
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
           If WriteWaiverDetail(inELRRN:'A') = *On;
              xxRtCd = *On;
           Else;
              xxRtCd = *Off;
           Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     PSubroAddition    e
     P*=====================================================================
     PSubroChange      b                   export
     DSubroChange      pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
     D xxKC            s              1    inz('0')
     D xxKL            s              1    inz('0')
     D xxCall          s              1    inz
     D xx00rrn         s              9  0 inz(0)
     D xxPgm           s             10    inz('W0051R25')
     D xxWtyp          s              1    inz('C')
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
           If WriteWaiverDetail(inELRRN:'C') = *On;
              xxRtCd = *On;
           Else;
              xxRtCd = *Off;
           Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     PSubroChange      e
     P*=====================================================================
     PSubroDelete      b                   export
     DSubroDelete      pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxRtcd          s               n   inz
     D xxKC            s              1    inz('0')
     D xxKL            s              1    inz('0')
     D xxCall          s              1    inz
     D xx00rrn         s              9  0 inz(0)
     D xxPgm           s             10    inz('W0051R26')
     D xxWtyp          s              1    inz('D')
      /Free
        xxRtcd = *Off;
        If InitEndorse(inEMrrn:inELrrn:inMYrrn);
           If WriteWaiverDetail(inELRRN:'D') = *On;
              xxRtCd = *On;
           Else;
              xxRtCd = *Off;
           Endif;
        Endif;
        Return xxRtcd;
      /End-Free

     PSubroDelete      e
      /Free
       //==================================================
       Dcl-Proc WriteWaiverDetail;
       //==================================================
       Dcl-Pi *N Ind;
        inELrrn Packed(9:0);
        inWvrType Char(1) Const;
       End-Pi;

       Dcl-S chrCo# Char(3);
       Dcl-S chrFnd Char(3);
       Dcl-S chrEmp Char(9);
       Dcl-S chrDiv Char(5);
       Dcl-S chrFyr Char(3);
       Dcl-S outMode Char(1) Inz('3');
       Dcl-S xxKC Char(1) Inz('0');
       Dcl-S xxKL Char(1) Inz('0');
       Dcl-S xxCall Char(1) Inz;
       Dcl-S xxRtCd Ind Inz;
       Dcl-S xx00RRN Packed(9:0) Inz(0);
       Dcl-S xxPrtPgm Char(10) Inz;
       Dcl-S xxAddPgm Char(10) Inz;
       Dcl-S xxJrSt Char(2) Inz;
       Dcl-S xxWvrForm Char(15) Inz;
       Dcl-S xxWvrType Char(1) Inz;

       xxJRSt = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
       If GetWaiverInfo(ELco#:ELfnd:xxJrSt:ELpfdt:xxWvrForm:
        xxPrtPgm:xxAddPgm) = *On;
          In *Lock *Dtaara;
          If xxJrSt <> *Blank;
             ldStSpcfFlg = 'Y';
          Else;
             ldStSpcfFlg = ' ';
          Endif;
          ldWaiverFrm = xxWvrForm;
          Out *Dtaara;
          Select;
          When xxAddPgm = 'SR42304B' And inWvrType <> 'D';
           // Special processing for TX Waiver
             xxCall = *Blank;
             xxWvrType = inWvrType;
             chrCo# = %Editc(ELco#:'X');
             chrFnd = %Editc(ELfnd:'X');
             chrEmp = %Editc(ELemp#:'X');
             chrDiv = %Editc(ELdiv:'X');
             chrFyr = %Editc(ELfyr:'X');
             SR42304B( chrCo#:chrFnd:chrEmp:chrDiv:chrFyr:
              outMode:inELrrn:xxWvrType);
             If xxCall = 'F';
                xxRtcd = *Off;
             Else;
                xxRtcd = *On;
             Endif;
          Other;
           // Call to W0815R
             xxCall = *Blank;
             xxWvrType = inWvrType;
             ProcSubroEnd(xxKC:xxKL:xxCall:xx00rrn:inELrrn:xxPrtPgm
              :xxWvrType);
             If xxKC = *On Or xxKL = *On;
               xxRtcd = *Off;
             Else;
               xxRtcd = *On;
             Endif;
          Endsl;
       Else;
          xxRtcd = *Off;
       Endif;
       Return xxRtCd;
       End-Proc;
      /End-Free
     P*=====================================================================
     PInitEndorse      b
     DInitEndorse      pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
      *                                         14
     D xxRtcd          s               n
      /Free
        xxRtcd = *On;
        Chain(n) inEMrrn pem0p svEmp;
        If not %found(wmemp);
          Return *Off;
        Endif;
        Chain inELrrn pel0p;
        If not %found(wdelp);
          Return *Off;
        Endif;
        Chain inMYrrn pmy0p;
        If not %found(wtmyp);
          Return *Off;
        Endif;

        Chain (SVEMP.EMco#:SVEMP.EMfnd) pf200;
        flgFirst = *On;

        Clear pen03;

        Return *On;
      /End-Free

     PInitEndorse      e
     P*=====================================================================
     PProcNameChange   b
     DProcNameChange   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
      /Free
        Chain(n) inEMrrn pem0p;
        In *Lock *dtaara;
        Out *dtaara;

        If  svEmp.EMname <> EMname Or svEmp.EMnam2 <> EMnam2
         Or svEmp.EMfdnm <> EMfdnm Or svEmp.EMfdn2 <> EMfdn2
         Or svEmp.EMnamp <> EMnamp Or svEmp.EMfdnp <> EMfdnp;
          xxB401 = svEmp.EMname + %subst(svEmp.Emnam2:1:10);
          xxAF01 = EMname + %subst(Emnam2:1:10);
          xxSeq# = 0;
          WriteNameRec(inEEDT:1);
          xxB401 = svEmp.EMfdnm + %subst(svEmp.Emfdn2:1:10);
          xxAF01 = EMfdnm + %subst(Emfdn2:1:10);
          xxSeq# +=1;
          WriteNameRec(inEEDT:2);
        Endif;
        If svEMP.EMfein <> EMfein;
         //*** Beg Chg *** 04/22/22 *************************************** ***
         //***xxB401 = %EditW(svEmp.EMfein:'0 -       ');
         //***xxAF01 = %EditW(EMfein:'0 -       ');
         xxB401 = %Editc(svEmp.EMfein:'X');
         xxAF01 = %Editc(EMfein:'X');
         //*** End Chg *** 04/22/22 *************************************** ***
         xxSeq# = 0;
         WriteNameRec(inEEDT:3);
        Endif;
        If svEMP.EMtpob <> EMtpob;
         xxSeq# = 0;
         xxTbid = 'BUSI';
         xxKey = EMtpob;
         xxAF01 = %Trim(GetGenCodeDs(EMco#:xxTbid:xxKey));
         xxKey = svEmp.EMtpob;
         xxB401 = %Trim(GetGenCodeDs(EMco#:xxTbid:xxKey));
         WriteNameRec(inEEDT:7);
        Endif;
        Return *On;
      /End-Free

     PProcNameChange   e
     P*=====================================================================
     PProcMAddChange   b
     DProcMAddChange   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxKey           s             10    inz
     D Zip5            s              5  0 inz
      /Free

        Chain(n) inEMrrn pem0p;
        In *Lock *dtaara;
        Out *dtaara;

        If  svEmp.EMmad1 <> EMmad1 Or svEmp.EMmad2 <> EMmad2
         Or svEmp.EMmcty <> EMmcty Or svEmp.EMmst  <> EMmst
         Or svEmp.EMmzip <> EMmzip;
          xxSeq# = 0;
          If scChgBoth = 'Y';
            xxB401 = 'Mailing Address';
            xxAF01 = *blanks;
            WriteMaddRec(inEEDT:1);
            xxSeq# +=1;
          Endif;

          xxB401 = svEmp.EMmad1;
          xxAF01 = EMmad1;
          WriteMaddRec(inEEDT:1);
            xxSeq# +=1;

          If svEmp.EMmad2 <> *Blanks Or EMmad2 <> *Blanks;
            xxB401 = svEmp.EMmad2;
            xxAF01 = EMmad2;
            WriteMaddRec(inEEDT:1);
            xxSeq# +=1;
          Endif;

          If EMmcty <> *Blanks Or EMmst <> *Blanks Or
           EMmzip <> *Zeros;
                 xxAF01 = %Trim(EMmcty) + ', ' + EMmst + ' ' +
                 %EditW(EMmzip:'0     -    ');
          Endif;

          If svEmp.EMmcty <> *Blanks Or svEmp.EMmst <> *Blanks Or
           svEmp.EMmzip <> *Zeros;
                 xxB401 = %Trim(svEmp.EMmcty) + ', ' + svEmp.EMmst + ' ' +
                 %EditW(svEmp.EMmzip:'0     -    ');
          Endif;
          WriteMaddRec(inEEDT:1);
            xxSeq# +=1;
        Endif;

        If scChgBoth = 'Y';
          If  svEmp.EMmad1 <> EMmad1 Or svEmp.EMmad2 <> EMmad2
           Or svEmp.EMmcty <> EMmcty Or svEmp.EMmst  <> EMmst
           Or svEmp.EMmzip <> EMmzip;
               If EMmad1 <> *Blanks Or EMmcty <> *Blanks Or EMmst <> *Blanks
                Or EMmzip <> *Zeros;
                 // Update Mailing address to physical
                 Chain inEMrrn pem0p;
                 If %found(wmemp);
                   EMpad1 = EMmad1;
                   EMpad2 = EMmad2;
                   EMpcty = EMmcty;
                   EMpst = EMmst;
                   EMpzip= EMmzip;
                  Zip5 = %int(%subst(%editc(EMmzip:'X'):1:5));
                  Chain (EMco#:EMmst:Zip5) PCZ01;
                  If %found(WMCZL);
                    EMcnty = CZcnty;
                  Endif;
                   Update pem0p;
                 Endif;

                 Chain(n) inEMrrn pem0p;

                 xxB401 = 'Physical Address';
                 xxAF01 = *blanks;
                 WriteMaddRec(inEEDT:3);
                 xxSeq# +=1;

                 xxB401 = svEmp.EMpad1;
                 xxAF01 = EMpad1;
                 WriteMaddRec(inEEDT:3);
                 xxSeq# +=1;

                 If svEmp.EMpad2 <> *Blanks Or EMpad2 <> *Blanks;
                   xxB401 = svEmp.EMpad2;
                   xxAF01 = EMpad2;
                   WriteMaddRec(inEEDT:3);
                   xxSeq# +=1;
                 Endif;

                 If EMpcty <> *Blanks Or EMpst <> *Blanks Or
                  EMpzip <> *Zeros;
                   xxAF01 = %Trim(EMpcty) + ', ' + EMpst + ' ' +
                   %EditW(EMpzip:'0     -    ');
                 Endif;

                 If svEmp.EMpcty <> *Blanks Or svEmp.EMpst <> *Blanks Or
                  svEmp.EMpzip <> *Zeros;
                    xxB401 = %Trim(svEmp.EMpcty) + ', ' + svEmp.EMpst + ' ' +
                    %EditW(svEmp.EMpzip:'0     -    ');
                 Endif;
                 WriteMaddRec(inEEDT:3);
                 xxSeq# +=1;
               Endif;
          Endif;
        EndIf;

        If EMcnty <> svEmp.EMcnty;
          xxKey = EMmst + EMcnty;
          xxAF01 = EMcnty + ' - ' + CountyCdDs(EMco#:xxKey);
          xxKey = svEmp.EMmst + svEmp.EMcnty;
          xxB401 = svEmp.EMcnty + ' - ' + CountyCdDs(svEmp.EMco#:xxKey);
          xxSeq# = 0;
          WriteMaddRec(inEEDT:2);
        Endif;
        UpdateLocEnt(inEEDT);

        Return *On;
      /End-Free

     PProcMAddChange   e
     P*=====================================================================
     PProcPAddChange   b
     DProcPAddChange   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
     D xxKey           s             10    inz
      /Free
        Chain(n) inEMrrn pem0p;
        In *Lock *dtaara;
        Out *dtaara;

        If  svEmp.EMpad1 <> EMpad1 Or svEmp.EMpad2 <> EMpad2
         Or svEmp.EMpcty <> EMpcty Or svEmp.EMpst  <> EMpst
         Or svEmp.EMpzip <> EMpzip;
          xxSeq# = 0;
          If scChgBoth = 'Y';
            xxB401 = 'Physical Address';
            xxAF01 = *blanks;
            WritePaddRec(inEEDT:1);
            xxSeq# +=1;
          Endif;

          xxB401 = svEmp.EMpad1;
          xxAF01 = EMpad1;
          WritePaddRec(inEEDT:1);
            xxSeq# +=1;

          If svEmp.EMpad2 <> *Blanks Or EMpad2 <> *Blanks;
            xxB401 = svEmp.EMpad2;
            xxAF01 = EMpad2;
            WritePaddRec(inEEDT:1);
            xxSeq# +=1;
          Endif;

          If EMpcty <> *Blanks Or EMpst <> *Blanks Or
           EMpzip <> *Zeros;
                   xxAF01 = %Trim(EMpcty) + ', ' + EMpst + ' ' +
                   %EditW(EMpzip:'0     -    ');
          Endif;

          If svEmp.EMpcty <> *Blanks Or svEmp.EMpst <> *Blanks Or
           svEmp.EMpzip <> *Zeros;
                 xxB401 = %Trim(svEmp.EMpcty) + ', ' + svEmp.EMpst + ' ' +
                 %EditW(svEmp.EMpzip:'0     -    ');
          Endif;
          WritePaddRec(inEEDT:1);
          xxSeq# +=1;
        Endif;

        If scChgBoth = 'Y';
          If  svEmp.EMpad1 <> EMpad1 Or svEmp.EMpad2 <> EMpad2
           Or svEmp.EMpcty <> EMpcty Or svEmp.EMpst  <> EMpst
           Or svEmp.EMpzip <> EMpzip;
               If EMpad1 <> *Blanks Or EMpcty <> *Blanks Or EMpst <> *Blanks
                Or EMpzip <> *Zeros;
                 // Update Physical address to mailing
                 Chain inEMrrn pem0p;
                 If %found(wmemp);
                   EMmad1 = EMpad1;
                   EMmad2 = EMpad2;
                   EMmcty = EMpcty;
                   EMmst = EMpst;
                   EMmzip= EMpzip;
                   Update pem0p;
                 Endif;
                 Chain(n) inEMrrn pem0p;
                 xxB401 = 'Mailing Address';
                 xxAF01 = *blanks;
                 WritePaddRec(inEEDT:3);
                 xxSeq# +=1;

                 xxB401 = svEmp.EMmad1;
                 xxAF01 = EMmad1;
                 WritePaddRec(inEEDT:3);
                 xxSeq# +=1;

                 If svEmp.EMmad2 <> *Blanks Or EMmad2 <> *Blanks;
                   xxB401 = svEmp.EMmad2;
                   xxAF01 = EMmad2;
                   WritePaddRec(inEEDT:3);
                   xxSeq# +=1;
                 Endif;

                 If EMmcty <> *Blanks Or EMmst <> *Blanks Or
                  EMmzip <> *Zeros;
                   xxAF01 = EMmcty + ', ' + EMmst +
                   %EditW(EMmzip:'0     -    ');
                   xxAF01 = %Trim(EMmcty) + ', ' + EMmst + ' ' +
                   %EditW(EMmzip:'0     -    ');
                 Endif;

                 If svEmp.EMmcty <> *Blanks Or svEmp.EMmst <> *Blanks Or
                  svEmp.EMmzip <> *Zeros;
                   xxB401 = %Trim(svEmp.EMmcty) + ', ' + svEmp.EMmst + ' ' +
                   %EditW(svEmp.EMmzip:'0     -    ');
                 Endif;
                 WritePaddRec(inEEDT:3);
                 xxSeq# +=1;
               Endif;
          Endif;
        EndIf;

        If EMcnty <> svEmp.EMcnty;
          If EMpst <> *Blanks;
             xxKey = EMpst + EMcnty;
          Else;
             xxKey = EMmst + EMcnty;
          Endif;
          xxAF01 = EMcnty + ' - ' + CountyCdDs(EMco#:xxKey);
          If svEmp.EMpst <> *Blanks;
             xxKey = svEmp.EMpst + svEmp.EMcnty;
          Else;
             xxKey = svEmp.EMmst + svEmp.EMcnty;
          Endif;
          xxB401 = svEmp.EMcnty + ' - ' + CountyCdDs(svEmp.EMco#:xxKey);
          xxSeq# = 0;
          WritePaddRec(inEEDT:2);
        Endif;

        Return *On;
      /End-Free

     PProcPAddChange   e
      *================================================================
      * UpdateEnt/Loc - Entity / Location Address
      *================================================================
     PUpdateLocEnt     b
     DUpdateLocEnt     pi
     D inEEDT                         8  0

      /Free

       if SCupdall = 'Y' And ENreq# > *ZERO;

          Setll (EMco#:EMfnd:EMemp#:EMdiv) WMALL;
          Reade (EMco#:EMfnd:EMemp#:EMdiv) WMALL;

          dow not %EOF;
             if svemp.emmad1 = almad1 and svemp.emmad2 = almad2
              and svemp.emmcty = almcty and svemp.emmst = almst
        //** Only to update those entity/location whose Expiration/Deletion
        //** is Zero.
              and svemp.emmzip = almzip and (ALdlDt = 0
              Or ALdlDt > dtCYMD(inEEDT));
                ALmad1 = EMmad1;
                ALmad2 = EMmad2;
                ALmcty = EMmcty;
                ALmst  = EMmst;
                ALmzip = EMmzip;
                ALdt   = dateto7(systemdate());
                ALtime = timeto6(systemtime());
                ALuser = q1User;
                ALpgm  = q1Pgm;
                Update pal00;

             Endif;

             Reade (EMco#:EMfnd:EMemp#:EMdiv) WMALL;

          Enddo;

       Endif;

       Return;

      /End-Free

     PUpdateLocEnt     e
     P*=====================================================================
     PProcLegalChange  b
     DProcLegalChange  pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
      /Free
        Chain(n) inEMrrn pem0p;
        In *Lock *dtaara;
        Out *dtaara;
        If EMtpob <> svEmp.EMtpob;
          xxTbid = 'BUSI';
          xxKey = EMtpob;
          xxAF01 = %Trim(GetGenCodeDs(EMco#:xxTbid:xxKey));
          xxKey = svEmp.EMtpob;
          xxB401 = %Trim(GetGenCodeDs(EMco#:xxTbid:xxKey));
          xxSeq# = 0;
          WriteNameRec(inEEDT:7);
        Endif;
        Return *On;
      /End-Free

     PProcLegalChange  e
     P*=====================================================================
     PProcFeinChange   b
     DProcFeinChange   pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEDT                         8  0
      *
      /Free
        Chain(n) inEMrrn pem0p;
        In *Lock *dtaara;
        Out *dtaara;
        If EMfein <> svEmp.EMfein;
          //*** Beg Chg *** 04/22/22 ******************************************
          //***xxAF01 = %Editw(EMfein:'0 -       ');
          //***xxB401 = %Editw(svEmp.EMfein:'0 -       ');
          xxAF01 = %Editc(EMfein:'X');
          xxB401 = %Editc(svEmp.EMfein:'X');
          //*** End Chg *** 04/22/22 ******************************************
          xxSeq# = 0;
          WriteNameRec(inEEDT:3);
        Endif;
        Return *On;
      /End-Free

     PProcFeinChange   e
     P*=====================================================================
     pWrtAddNameRec    b
     dWrtAddNameRec    pi
     d inEEDT                         8  0
     d sysDt7          s              7  0
      /Free
       Chain (ELco#:ELfnd) pdx00;
       In svLda;
       ldAddNamEnd = '1';  // W7051C1 picks up this value in LDA
       Out svLda;
       Setgt  (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:Q1jobn:ldEnd#) pen03;
       Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:Q1jobn:ldEnd#) pen03;
       If %Eof;
          ENreq# = 0;
       EndIf;
       ENco# = ELco#;
       ENfnd = ELfnd;
       ENemp# = ELemp#;
       ENdiv = ELdiv;
       ENfyr = ELfyr;
       ENjob = Q1jobn;
       ENend# = LDend#;
       ENrmcd = DXemnc;
       ENblin = 'Additional Named Insured'; // Condition SR89600B
       ENncfl = 'Y';
       ENseq# = 0;
       ENreq# += 1;
       ENeedt = dtCYMD(inEEDT);
       ENfnnm = GetGrpName(ELfnd);
       sysDt7 = DateTo7(SystemDate());
       ENdt = sysDt7;
       ENagt# = GetEmpAgt(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:sysdt7:ENagnm);
       ENcar1 = F2car#;
       ENpfd1 = ELpfdt;
       ENptd1 = ELptdt;
       ENdsc1 = MYdsc1;
       ENdsc2 = MYdsc2;
       ENdsc3 = MYdsc3;
       ENtime = TimeTo6(SystemTime());
       ENuser = Q1user;
       ENpgm = Q1pgm;
       ENstat = 'A';
       ENrusr = Q1user;
       Write pen03;
      /End-Free

     pWrtAddNameRec    e
      *** end add ***** 03/05/18 ****************
     P*=====================================================================
     PWriteNameRec     b
     DWriteNameRec     pi              n
     D inEEDT                         8  0
     D inRsn                          3  0 const

     D sysdt7          s              7  0 inz

     C     ENKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#

     C     flgFirst      IFEQ      *ON
     C                   MOVE      *OFF          flgFirst
     C     ENKY          SETGT     PEN03
     C     ENKY          READPE    PEN03                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   ENDIF

     C                   MOVE      *BLANK        ENNCFL
     C                   MOVE      *BLANK        ENFIFL
     C                   MOVE      *BLANK        ENCNFL
     C                   MOVE      *BLANK        ENATFL
     C                   MOVE      *BLANK        ENC3FL
     C                   MOVE      *BLANK        ENTBFL

     C                   MOVE      ELCO#         ENCO#
     C                   MOVE      ELFND         ENFND
     C                   MOVE      ELEMP#        ENEMP#
     C                   MOVE      ELDIV         ENDIV
     C                   MOVE      ELFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C                   EVAL      ENSEQ# = XXSEQ#

     C                   EVAL      ENEEDT = dtCYMD(inEEDT)

      /Free
       Chain (ELco#:ELfnd) pdx00;
       ENNAM1 = svEMP.EMname;
       ENFDN1 = svEMP.EMfdnm;
       ENENM1 = svEMP.EMname + %subst(svEMP.EMnam2:1:10);
       ENENM2 = %subst(svEMP.EMnam2:11:50);
       ENEFN1 = svEMP.EMfdnm + %subst(svEMP.EMfdn2:1:10);
       ENEFN2 = %subst(svEMP.EMfdn2:11:50);
       ENCON1 = svEMP.EMcont;
       ENATT1 = svEMP.EMattn;
       ENCN31 = svEMP.EMcnt3;
       ENfen1 = svEMP.EMfein;
       ENtob1 = svEMP.EMtpob;

       ENMA11 = EMMAD1;
       ENMA21 = EMMAD2;
       ENMCT1 = EMMCTY;
       ENMST1 = EMMST;
       ENMZP1 = EMMZIP;
       ENPA11 = EMPAD1;
       ENPA21 = EMPAD2;
       ENPCT1 = EMPCTY;
       ENPST1 = EMPST;
       ENPZP1 = EMPZIP;

       ENNAM2 = EMname;
       ENANM1 = EMname + %subst(EMnam2:1:10);
       ENANM2 = %subst(EMnam2:11:50);
       ENFDN2 = EMfdnm;
       ENAFN1 = EMfdnm + %subst(EMfdn2:1:10);
       ENAFN2 = %subst(EMfdn2:11:50);
       ENFEN2 = EMfein;
       ENTOB2 = EMtpob;
      /End-free

     C                   SELECT
     C     inRSN         WHENEQ    1                                            EMNAME
     C                   MOVE      'Y'           ENNCFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     inRSN         WHENEQ    2                                            EMFDNM
     C                   MOVE      'Y'           ENNCFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     inRSN         WHENEQ    3                                            EMFEIN
     C                   MOVE      'Y'           ENFIFL
     C                   MOVEL     DXEMFI        ENRMCD
     C     inRSN         WHENEQ    4                                            EMCONT
     C                   MOVE      'Y'           ENCNFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     inRSN         WHENEQ    5                                            EMATTN
     C                   MOVE      'Y'           ENATFL
     C                   MOVEL     DXEMNC        ENRMCD
     C     inRSN         WHENEQ    6                                            EMCNT3
     C                   MOVE      'Y'           ENC3FL
     C                   MOVEL     DXEMNC        ENRMCD
     C     inRSN         WHENEQ    7                                            EMTPOB
     C                   MOVE      'Y'           ENTBFL
     C                   MOVEL     DXEMLS        ENRMCD
     C     inRSN         WHENEQ    8                                            EMTPOB
     C                   MOVE      'Y'           ENMSFL
     C                   MOVEL     DXMISC        ENRMCD
     C     inRSN         WHENEQ    10                                           EXPMOD
     C                   MOVE      'Y'           ENCMFL
     C                   MOVEL     DXCEXM        ENRMCD
     C                   ENDSL

     C                   Eval      ENfnnm = GetGrpName(EMfnd)
     C                   Eval      sysDt7 = DateTo7(SystemDate())
     C                   Eval      ENagt# = GetEmpAgt(ELco#:ELfnd:ELemp#:ELdiv:
     C                              ELfyr:sysdt7:ENagnm)
     C                   Eval      ENcar1 = F2car#
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   Z-ADD     ELPTDT        ENPTD1

     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3

     C                   MOVEL     XXB401        ENBLIN
     C                   MOVEL     XXAF01        ENALIN

     C                   Eval      ENdt = DateTo7(SystemDate())
     C                   Eval      ENtime = TimeTo6(SystemTime())
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR

     C                   WRITE     PEN03
     C                   Return    *On
     PWriteNameRec     e

     P*=====================================================================
     PWriteMaddRec     b
     DWriteMaddRec     pi              n
     D inEEDT                         8  0
     D inRsn                          3  0 const

     D sysdt7          s              7  0 inz
     D Zip50           s              5  0 inz
     D DESCB           C                   CONST('EMPLOYER MAILING AND-
     D                                      PHYSICAL ADDRESS CH-
     D                                     ANGE')

     C     ENKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#

     C     flgFirst      IFEQ      *ON
     C                   MOVE      *OFF          flgFirst
     C     ENKY          SETGT     PEN03
     C     ENKY          READPE    PEN03                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   ENDIF

     C                   MOVE      *BLANK        ENCNTF
     C                   MOVE      *BLANK        ENMAFL
     C                   MOVE      *BLANK        ENPAFL
     C                   MOVE      *BLANK        ENCBAF

     C                   MOVE      ELCO#         ENCO#
     C                   MOVE      ELFND         ENFND
     C                   MOVE      ELEMP#        ENEMP#
     C                   MOVE      ELDIV         ENDIV
     C                   MOVE      ELFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C                   EVAL      ENSEQ# = XXSEQ#

     C                   EVAL      ENEEDT = dtCYMD(inEEDT)

      /Free

       Chain (ELco#:ELfnd) pdx00;
       ENNAM1 = svEMP.EMname;
       ENFDN1 = svEMP.EMfdnm;
       ENENM1 = svEMP.EMname + %subst(svEMP.EMnam2:1:10);
       ENENM2 = %subst(svEMP.EMnam2:11:50);
       ENEFN1 = svEMP.EMfdnm + %subst(svEMP.EMfdn2:1:10);
       ENEFN2 = %subst(svEMP.EMfdn2:11:50);
       ENCON1 = svEMP.EMcont;
       ENATT1 = svEMP.EMattn;
       ENCN31 = svEMP.EMcnt3;
       ENfen1 = svEMP.EMfein;
       ENtob1 = svEMP.EMtpob;
       ENCNT1 = svEmp.EMcnty;

       ENMA12 = EMMAD1;
       ENMA22 = EMMAD2;
       ENMCT2 = EMMCTY;
       ENMST2 = EMMST;
       ENMZP2 = EMMZIP;
       ENCNT2 = EMcnty;


       ENMA11 = svEMP.EMMAD1;
       ENMA21 = svEMP.EMMAD2;
       ENMCT1 = svEMP.EMMCTY;
       ENMST1 = svEMP.EMMST;
       ENMZP1 = svEMP.EMMZIP;
       ENCNT1 = svEmp.EMcnty;

       If scChgBoth = 'Y';
         ENPA12 = EMMAD1;
         ENPA22 = EMMAD2;
         ENPCT2 = EMMCTY;
         ENPST2 = EMMST;
         ENPZP2 = EMMZIP;

         Zip50 = %int(%subst(%editc(EMmzip:'X'):1:5));
         Chain (EMco#:EMmst:Zip50) PCZ01;
         If %found(WMCZL);
            ENcnt2 = CZcnty;
         Endif;

       Endif;

       ENPA11 = svEMP.EMPAD1;
       ENPA21 = svEMP.EMPAD2;
       ENPCT1 = svEMP.EMPCTY;
       ENPST1 = svEMP.EMPST;
       ENPZP1 = svEMP.EMPZIP;

       ENNAM2 = EMname;
       ENANM1 = EMname + %subst(EMnam2:1:10);
       ENANM2 = %subst(EMnam2:11:50);
       ENFDN2 = EMfdnm;
       ENAFN1 = EMfdnm + %subst(EMfdn2:1:10);
       ENAFN2 = %subst(EMfdn2:11:50);
       ENFEN2 = EMfein;
       ENTOB2 = EMtpob;

      /end-free

     C                   MOVEL     DXEMMA        ENRMCD
     C                   SELECT
     C     inRSN         WHENEQ    1                                            ADDRESS
     C     scChgBoth     IFEQ      'Y'
     C                   MOVE      'Y'           ENCBAF
     C                   ENDIF
     C                   MOVE      'Y'           ENMAFL
     C     inRSN         WHENEQ    2                                            COUNTY
     C                   MOVE      'Y'           ENCNTF
     C     inRSN         WHENEQ    3                                            ADDRESS
     C     scChgBoth     IFEQ      'Y'
     C                   MOVE      'Y'           ENCBAF
     C                   ENDIF
     C                   MOVE      'Y'           ENPAFL
     C                   ENDSL

     C                   Eval      ENfnnm = GetGrpName(EMfnd)
     C                   Eval      sysDt7 = DateTo7(SystemDate())
     C                   Eval      ENagt# = GetEmpAgt(ELco#:ELfnd:ELemp#:ELdiv:
     C                              ELfyr:sysdt7:ENagnm)
     C                   Eval      ENcar1 = F2car#
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   Z-ADD     ELPTDT        ENPTD1

     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3

     C                   MOVEL     XXB401        ENBLIN
     C                   MOVEL     XXAF01        ENALIN

     C                   Eval      ENdt = DateTo7(SystemDate())
     C                   Eval      ENtime = TimeTo6(SystemTime())
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C                   MOVE      'Y'           ENADFL

     C     ENCBAF        IFEQ      'Y'
     C                   MOVEL     DESCB         ENDSC1
     C                   ELSE
     C                   MOVEL     MYDSC1        ENDSC1
     C                   ENDIF                                                  ENCBAF = Y

     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3

     C                   MOVEL     XXB401        ENBLIN
     C                   MOVEL     XXAF01        ENALIN

     C                   MOVE      F2CAR#        ENCAR1
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   MOVE      scChgBoth     ENCBAF
     C                   MOVEL     scUpdAll      ENUMFL
     C                   WRITE     PEN03

     C                   Return    *On
     PWriteMaddRec     e
     P*=====================================================================
     PWritePaddRec     b
     DWritePaddRec     pi              n
     D inEEDT                         8  0
     D inRsn                          3  0 const

     D sysdt7          s              7  0 inz
     D DESCB           C                   CONST('EMPLOYER MAILING AND-
     D                                      PHYSICAL ADDRESS CH-
     D                                     ANGE')

     C     ENKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#

     C     flgFirst      IFEQ      *ON
     C                   MOVE      *OFF          flgFirst
     C     ENKY          SETGT     PEN03
     C     ENKY          READPE    PEN03                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   ENDIF

     C                   MOVE      *BLANK        ENCNTF
     C                   MOVE      *BLANK        ENMAFL
     C                   MOVE      *BLANK        ENPAFL
     C                   MOVE      *BLANK        ENCBAF

     C                   MOVE      ELCO#         ENCO#
     C                   MOVE      ELFND         ENFND
     C                   MOVE      ELEMP#        ENEMP#
     C                   MOVE      ELDIV         ENDIV
     C                   MOVE      ELFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C                   EVAL      ENSEQ# = XXSEQ#

     C                   EVAL      ENEEDT = dtCYMD(inEEDT)

      /Free

       Chain (ELco#:ELfnd) pdx00;
       ENNAM1 = svEMP.EMname;
       ENFDN1 = svEMP.EMfdnm;
       ENENM1 = svEMP.EMname + %subst(svEMP.EMnam2:1:10);
       ENENM2 = %subst(svEMP.EMnam2:11:50);
       ENEFN1 = svEMP.EMfdnm + %subst(svEMP.EMfdn2:1:10);
       ENEFN2 = %subst(svEMP.EMfdn2:11:50);
       ENCON1 = svEMP.EMcont;
       ENATT1 = svEMP.EMattn;
       ENCN31 = svEMP.EMcnt3;
       ENfen1 = svEMP.EMfein;
       ENtob1 = svEMP.EMtpob;
       ENCNT1 = svEmp.EMcnty;

       ENPA12 = EMPAD1;
       ENPA22 = EMPAD2;
       ENPCT2 = EMPCTY;
       ENPST2 = EMPST;
       ENPZP2 = EMPZIP;
       ENCNT2 = EMcnty;

       If scChgBoth = 'Y';
         ENMA12 = EMPAD1;
         ENMA22 = EMPAD2;
         ENMCT2 = EMPCTY;
         ENMST2 = EMPST;
         ENMZP2 = EMPZIP;
       Endif;

       ENMA11 = svEMP.EMMAD1;
       ENMA21 = svEMP.EMMAD2;
       ENMCT1 = svEMP.EMMCTY;
       ENMST1 = svEMP.EMMST;
       ENMZP1 = svEMP.EMMZIP;
       ENCNT1 = svEmp.EMcnty;

       ENPA11 = svEMP.EMPAD1;
       ENPA21 = svEMP.EMPAD2;
       ENPCT1 = svEMP.EMPCTY;
       ENPST1 = svEMP.EMPST;
       ENPZP1 = svEMP.EMPZIP;

       ENNAM2 = EMname;
       ENANM1 = EMname + %subst(EMnam2:1:10);
       ENANM2 = %subst(EMnam2:11:50);
       ENFDN2 = EMfdnm;
       ENAFN1 = EMfdnm + %subst(EMfdn2:1:10);
       ENAFN2 = %subst(EMfdn2:11:50);
       ENFEN2 = EMfein;
       ENTOB2 = EMtpob;

      /End-free

     C                   MOVEL     DXEMPA        ENRMCD
     C                   SELECT
     C     inRSN         WHENEQ    1                                            ADDRESS
     C     scChgBoth     IFEQ      'Y'
     C                   MOVE      'Y'           ENCBAF
     C                   ENDIF
     C                   MOVE      'Y'           ENPAFL
     C     inRSN         WHENEQ    2                                            COUNTY
     C                   MOVE      'Y'           ENCNTF
     C     inRSN         WHENEQ    3                                            ADDRESS
     C     scChgBoth     IFEQ      'Y'
     C                   MOVE      'Y'           ENCBAF
     C                   ENDIF
     C                   MOVE      'Y'           ENMAFL
     C                   ENDSL

     C                   Eval      ENfnnm = GetGrpName(EMfnd)
     C                   Eval      sysDt7 = DateTo7(SystemDate())
     C                   Eval      ENagt# = GetEmpAgt(ELco#:ELfnd:ELemp#:ELdiv:
     C                              ELfyr:sysdt7:ENagnm)
     C                   Eval      ENcar1 = F2car#
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   Z-ADD     ELPTDT        ENPTD1

     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3

     C                   MOVEL     XXB401        ENBLIN
     C                   MOVEL     XXAF01        ENALIN

     C                   Eval      ENdt = DateTo7(SystemDate())
     C                   Eval      ENtime = TimeTo6(SystemTime())
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C                   MOVE      'Y'           ENADFL

     C     ENCBAF        IFEQ      'Y'
     C                   MOVEL     DESCB         ENDSC1
     C                   ELSE
     C                   MOVEL     MYDSC1        ENDSC1
     C                   ENDIF                                                  ENCBAF = Y

     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3

     C                   MOVEL     XXB401        ENBLIN
     C                   MOVEL     XXAF01        ENALIN

     C                   MOVE      F2CAR#        ENCAR1
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   MOVE      scChgBoth     ENCBAF
     C                   MOVEL     scUpdAll      ENUMFL
     C                   WRITE     PEN03

     C                   Return    *On

     PWritePaddRec     e
     p*=====================================================================
     p* ProcMiscEnd - Process Misc change endorsement                  s
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Parm3   - Mid Term Endorsement RRN
     p*  Parm4   - Endorsement eff date
     p*  Parm5   - Endorsement #
     p*  Returns - *On for success
     P*=====================================================================
     PProcMiscEnd      b
     DProcMiscEnd      pi              n
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inMYrrn                        9  0
     D inEEdt                         8  0
     D inEnd                          3    const

     D ExitNow         s               n   inz
     D TotRecs         s              5  0 inz(999)
     D xxRtcd          s               n   inz
     D miscEndRecWrit  s               n   inz
     D
      /Free

        In *Lock *dtaara;
        Out *dtaara;

        GetPolSumData(inEMrrn:inELrrn:inEEdt);
        visMiscChg = *Off;
        visDelDiv = *Off;
        visExpMod = *Off;
        scMscPnl = %Trim(MYdesc) + ' Endorsement';
        Exsr ClearMiscChg;
        Select;
        When inEnd = '170';
          visMiscChg = *On;
          btnMscTop = 655;
          scPHeight = 590;
          //scMscPnl = 'Miscellaneous Employers Endorsement';
          Exsr LoadBlankMiscChg;
        When inEnd = '200';
          visDelDiv = *On;
          btnMscTop = 405;
          scPHeight = 340;
          //scMscPnl = 'Deletion of Division Endorsement';
          lblReason = 'Reason for Cancellation';
        When inEnd = '205';
          visDelDiv = *On;
          btnMscTop = 405;
          scPHeight = 340;
          //scMscPnl = 'Rescission of Division Endorsement';
          lblReason = 'Reason for Rescission';
        When inEnd = '230';
          visExpMod = *On;
          btnMscTop = 405;
          scPHeight = 340;
          //scMscPnl = 'Contingent Experience Mod Endorsement';
        Endsl;
        ExitNow = *Off;
        DoU ExitNow = *On;
          ExFmt rcdMiscChg;
          Select;
          When btnMscCanc = *On;
            ExitNow = *On;
            xxRtcd = *Off;
          When btnMscNext = *On;
            Select;
            When inEnd = '170';
              If ValidMiscChg();
                Exsr WrtMiscPen;
                ExitNow = *On;
                xxRtcd = *On;
              Endif;
            When inEnd = '200';
              //*** Beg Add *** 02/12/25 *******************************************
              If ValidDltDiv();
              //*** End Add *** 02/12/25 *******************************************
              WriteDltDivEnd(inEMrrn:inELrrn:inMYrrn:inEEdt:scDelR);
              ExitNow = *On;
              xxRtcd = *On;
              //*** Beg Add *** 02/12/25 *******************************************
              Endif;
              //*** End Add *** 02/12/25 *******************************************
            When inEnd = '205';
              WriteResDivEnd(inEMrrn:inELrrn:inMYrrn:inEEdt:scDelR);
              ExitNow = *On;
              xxRtcd = *On;
            When inEnd = '230';
              If ValidConExp();
                Exsr WrtConExpPen;
                ExitNow = *On;
                xxRtcd = *On;
              Endif;
            EndSl;
          EndSl;
        EndDo;
        Return xxRtcd;
        //************************************************************
        //* Write Misc endorsment records
        //************************************************************
        BegSr WrtMiscPen;
          miscEndRecWrit = *Off;
          DoU 1=0;
            ReadC SflMiscChg;
            If %Eof;
              Leave;
            Endif;
            If scBfor <> *Blanks Or scAfor <> *Blanks;
              xxB401 = scBfor;
              xxAF01 = scAfor;
              mydsc1 = scdesc1;
              mydsc2 = scdesc2;
              mydsc3 = *blanks;
              WriteNameRec(inEEdt:8);
              miscEndRecWrit = *On;
            Endif;
          EndDo;
          If not miscEndRecWrit And scdesc1 <> *Blanks;
            mydsc1 = scdesc1;
            mydsc2 = scdesc2;
            mydsc3 = *blanks;
            WriteNameRec(inEEdt:8);
          ENDIF;
        EndSr;
        //************************************************************
        //* Clear misc change subfile
        //************************************************************
        BegSr ClearMiscChg;
          clrMiscChg = *On;
          dspMiscChg = *Off;
          write rcdMiscChg;
          clrMiscChg = *Off;
          dspMiscChg = *On;
          scDesc1 = *Blanks;
          scDesc2 = *Blanks;
          scDelR = *Blanks;
          scCexm = *Zeros;
          scCemd = *Zeros;
          erDesc1 = *Off;
          erCexm = *Off;
          erCemd = *Off;
          //*** Beg Add *** 02/12/25 *******************************************
          erDelR = *Off;
          msDelR = *blanks;
          //*** End Add *** 02/12/25 *******************************************
        EndSr;
        //************************************************************
        //* Load blank records to subfile
        //************************************************************
        BegSr LoadBlankMiscChg;
          For rrnMiscChg = 1 to TotRecs;
            scBfor = *Blanks;
            scAfor = *Blanks;
            Write SflMiscChg;
          EndFor;
          curRrnMChg = 1;
        EndSr;
        //************************************************************
        //* Write Contingent Exp Mod records
        //************************************************************
        BegSr WrtConExpPen;
          xxSeq# = 0;
          xxB401 = *blanks;
          xxAF01 = %Editc(scCEXM:'L') + ' EFFECTIVE ' +
           %Editw(scCEMD:'  /  /    ');
          WriteNameRec(inEEdt:10);
        EndSr;
      /End-free

     PProcMiscEnd      e
      ****** Beg Add *** 02/12/25 *******************************************
     p*=====================================================================
     p* ValidDltDiv  - Validate Deletion of a Division endorsement
     P*=====================================================================
     PValidDltDiv      b                   export
     DValidDltDiv      pi              n

     D isError         s               n   inz
      /Free
        If scDelR = *Blanks;
          isError = *On;
          erDelR = *On;
          msDelR = 'WCR0503';
        Else;
          erDelR = *Off;
        Endif;
        Return not isError;
      /End-free

     PValidDltDiv      e
      ****** End Add *** 02/12/25 *******************************************
     p*=====================================================================
     p* ValidMiscChg  - Validate Misc change endorsement
     P*=====================================================================
     PValidMiscChg     b                   export
     DValidMiscChg     pi              n

     D isError         s               n   inz
      /Free
        If scDesc1 = *Blanks;
          isError = *On;
          erDesc1 = *On;
          mgDesc1 = 'WCR0503';
        Else;
          erDesc1 = *Off;
        Endif;
        Return not isError;
      /End-free

     PValidMiscChg     e
     p*=====================================================================
     p* ValidConExp   - Validate Contingent Exp mod
     P*=====================================================================
     PValidConExp      b                   export
     DValidConExp      pi              n

     D isError         s               n   inz
     D xxDate          s              8  0 inz
      /Free
        If scCEXM  <= *Zeros;
          isError = *On;
          erCexm = *On;
          mgCexm = 'WCR0502';
        Else;
          erCexm = *Off;
        Endif;
        xxDate = dtCheck(scCEMD);
        If xxDate = 0;
          isError = *On;
          erCEMD  = *On;
          mgCEMD = 'WCR0500';
        Else;
          scCEMD = xxDate;
        Endif;
        Return not isError;
      /End-free
     PValidConExp      e

     p*=====================================================================
     p* GetPolSumData - Get policy summary data for various endorsements
     p*  Parm1   - Employer Master RRN
     p*  Parm2   - Employer detail RRN
     p*  Returns - *On for success
     P*=====================================================================
     PGetPolSumData    b                   export
     DGetPolSumData    pi
     D inEMrrn                        9  0
     D inELrrn                        9  0
     D inEEdt                         8  0

     d AdL1            s             40    inz
     d AdL2            s             40    inz
     d AdL3            s             40    inz
     d ##co#           s              3
     d ##fnd           s              3
     d ##emp#          s              9
     d ##div           s              5
     d ##odr           s              1
     d ##nodr          s              1
     d ##aodr          s              1
     d ##adrl          s              3  0
     d ##naml          s              3  0
     d ##fedl          s              3  0
     d ##peo1          s             57
     d ##peo2          s             57
     d ##pgfl          s              1
     d ##conl          s              3  0
     d ##rrn           s              9
     d ##date          s              7
     d x               s              1  0
     d y               s              3  0
     d z               s              3  0
     d x1              s              1  0
     d x2              s              1  0
     d xxEftType       s              1    inz('E')
     d xxEftKey        s             30    inz
     d EFrrn           s              9  0 inz
     d preNote         s              1    inz

     d arrMadDs        ds
     d ##emn1                        50
     d ##emn2                        50
     d ##emn3                        50
     d ##emn4                        50
     d ##emn5                        50
     d ##emn6                        50
     d ##emn7                        50
     d arrMad                              OVERLAY(arrMadds)
     d                                     LIKE(##emn1) DIM(10)

      /Free

       Chain(n) inEMrrn pem0p;
       Chain(n) inELrrn pel0p;
       If %found;
          Clear scBillDivN;
          Clear scBiPhyAd1;
          Clear scBiPhyAd2;
          Clear scBiPhyAd3;
          Clear scBiMadAd1;
          Clear scBiMadAd2;
          Clear scBiMadAd3;
          Clear scBillAdr1;
          Clear scBillAdr2;
          Clear scBillAdr3;
          Clear scBillAdr4;
          Clear scBilRtSt;
          Clear scBilIncDt;
          Clear scBilPolSt;
          Clear scBilCanDt;
          Clear scBilRsn;
          Clear scBilUnWrt;
          Clear scBilPolRp;
          Clear scBiStRId#;
          Clear scBilEftDt;
          Clear scBIntTrDt;

          scBillDivN= %Char(EMemp#)+' '+%Char(EMdiv)+' '+EMname+EMnam2;

          // Load Physical Address.
          FormatAddress(EMpad1:EMpad2:EMpcty:EMpst:EMpzip:AdL1:AdL2:AdL3);
          scBiPhyAd1 = ADL1;
          scBiPhyAd2 = ADL2;
          scBiPhyAd3 = ADL3;

          // Load Mailing Address.
          FormatAddress(EMmad1:EMmad2:EMmcty:EMmst:EMmzip:AdL1:AdL2:AdL3);
          scBiMadAd1 = ADL1;
          scBiMadAd2 = ADL2;
          scBiMadAd3 = ADL3;

          // Load Billing Address.
          ##co#  = %editc(EMco#:'X');
          ##fnd  = %editc(EMfnd:'X');
          ##emp# = %editc(EMemp#:'X');
          ##div  = %editc(EMdiv:'X');
          ##odr  = ' ';
          ##nodr = ' ';
          ##aodr = ' ';
          ##emn1 = ' ';
          ##emn2 = ' ';
          ##emn4 = ' ';
          ##emn5 = ' ';
          ##emn6 = ' ';
          ##emn7 = ' ';
          ##adrl = 0;
          ##naml = 0;
          ##fedl = 0;
          ##peo1 = ' ';
          ##peo2 = ' ';
          ##pgfl = ' ';
          ##conl = 0;

          Clear arrMad;
          SrAddr(##co#:##fnd:##emp#:##div:##odr:##nodr:##aodr:
           ##emn1:##emn2:##emn3:##emn4:##emn5:##emn6:##emn7:
           ##adrl:##naml:##fedl:##peo1:##peo2:##pgfl:##conl);

          // Load Mad Array and find the first blank Index.
          x1 = ##adrl;
          If ##conl <> 0;
             x1 = ##conl;
          Endif;
          x2 = 1;
          If x1 > 0;
            Dow x1 < 8;
               If arrMad(x1) <> *blanks;
                  scBBillAdr(x2) = arrMad(x1);
                  x1 = x1 + 1;
                  x2 = x2 + 1;
               Else;
                  x1 = x1 + 1;
               Endif;
            Enddo;
          Endif;

          scBilRtSt = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
          scBilIncDt = dtMDYY(GetEmIncDt(EMco#:EMfnd:EMemp#:EMdiv));
          scBilPolSt = GetQuoteStsDesc(EMco#:EMfnd:EMemp#:EMdiv);
          scBilCanDt= dtMDYY(GetCxDate(EMco#:EMfnd:EMemp#:EMdiv));
          scBilRsn = GetCxRsn(EMco#:EMfnd:EMemp#:EMdiv:
           GetCxDate(EMco#:EMfnd:EMemp#:Emdiv));
          scBilUnWrt = GetUsrName(GetUserId(EMuwtr));
          scBilPolRp = GetUsrName(GetUserId(EMpsrv));
          scBiStRId# = EMsemp;
          xxEftKey = BuildEFTkey(xxEftType:EMemp#:EMdiv:'  ');
          ##date = %Editc(dtCymd(inEEdt):'X');
          If GetEftStatus(##co#:xxEftType:##fnd:xxEftKey:##date:preNote:##rrn);
            Monitor;
              EFrrn = %Int(##rrn);
              Chain(n) EFrrn pef0p;
              dspInTrDt = *On;
              scBIntTrDt = dtMDYY(EFidt);
              scBilEftDt = dtMDYY(EFefdt);
            On-Error;
              EFrrn = 0;
              dspInTrDt = *Off;
            EndMon;
          Else;
            dspInTrDt = *Off;
          Endif;

       Endif;
      /End-free

     PGetPolSumData    e
