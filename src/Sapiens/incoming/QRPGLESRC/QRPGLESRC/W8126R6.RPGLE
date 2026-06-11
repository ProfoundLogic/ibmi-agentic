      *  (C) Copyright Progressive Data Solutions, Inc., 2001, 2004
     H/title W8126R6 : Gross Payments Subprogram
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      ****************************************************************
      *  Program Description:                                        *
      *    This program calculates gross payments                    *
      *                                                              *
      *  Called from: W8126C                                         *
      *                                                              *
      *  Calls to:                                                   *
      *                                                              *
      *  Programmer: C: 90                                           *
      *  Created:    10/17/01  PROJECT: CM970                        *
      ****************************************************************
      *  Revisions                                                   *
      *                                                              *
      *  08/02/06 C:136 SUPPORT: WA129 53972                         *
      *                 ADD PROCESSING FOR THE NEW VOID A/R TYPE     *
      *                                                              *
      *  09/16/04 C:136 Support: WA109 43863                         *
      *                 Taxes on deposit should be reported to       *
      *                 municipal tax field, not the monthly         *
      *                 policy fees field.                           *
      *                                                              *
      *  05/27/04 C:136 Project: 109WDU                              *
      *                 Exclude 4th and 5th assessments from         *
      *                 Monthly Policy Fees calculation.             *
      *                                                              *
      *  04/05/04 C:136 Support: 41487                               *
      *                 Exclude Deductible category types from       *
      *                 Monthly Policy Fees calculation.             *
      *                                                              *
      *  Programmer: C: 142                                          *
      *              12/18/01  PROJECT: CM970                       **
      *              Found that file GRSSP was missing some fields. **
      *              Added them and populated.                      **
      ****************************************************************
      * Gross Payments for Clarendon
     FGrssp     UF A E             Disk
      *
      * A/R Detail Work File
XX   FWxx1l1    IF   E           K Disk
      *
      * Employer Detail File
XX   FWdell     IF   E           K Disk
     *
     * Employer Detail File
     FWdell2    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr2)
     F                                     Prefix(L2:2)
     *
     * Rate Set Code Master File
     FWmrsl     IF   E           K Disk
      *
      * Carrier/State Reporting Master File
XX   FWmrpl4    IF   E           K Disk
      *
      * Group Carrier Information File
XX   FWmgcl     IF   E           K Disk
      *
      * Statistical Reporting/Rating File
XX   F*Wdrrl     IF   E           K Disk
      *
      * Installment Fee Information
     FWmifl     IF   E           K Disk
      *
      /eject
      *
     D @@co#           S              3
     D @@todt          S              8
     D Svco#           S                   Like(X1co#)
     D Svfnd           S                   Like(X1fnd)
     D Svemp           S                   Like(X1emp#)
     D Svfyr           S                   Like(X1fyr)
     D Svst            S                   Like(X1st)
     D Xxco#           S                   Like(elco#)
     D FirstFyr        S                   Like(ElFyr)
     D FirstEfd        S                   Like(Elpfdt)
     D Kyrtcd          S                   Like(Elrtcd)

     D Today           S               D   Datfmt(*USA)
     D Datymd          S               D   Datfmt(*YMD)
     D Datusa          S               D   Datfmt(*USA)
     D Srdti           S              8  0
     D Srdto           S             10
     D Srerr           S              1
     D Srffmt          S              8
     D Srtfmt          S              8
     D X7todt          S                   like(elidt)
     D X8todt          S              8  0

     D Amount          S             11  2
     D Monthp          S             11  2
     D Cmclpr          S             20  2
     D InsFee          S             20  2
     D Expena          S             11  2
     D Latfee          S             11  2
     D Secdep          S             20  2
     D Muntax          S             11  2
     D Monpfc          S             11  2
     D Surty1          S             11  2
     D Surty2          S             11  2
     D Surty3          S             11  2
     D Surch1          S             11  2
     D Surch2          S             11  2
     D Surch3          S             11  2
      *** beg add ***** 05/27/04 ***********************************************
     D Surty4          S             11  2
     D Surty5          S             11  2
     D Surch4          S             11  2
     D Surch5          S             11  2
      *** end add ***** 05/27/04 ***********************************************
     D Zeros           S             20  2
     D Hold20          S             20  2
     D First           S              1n
     D Instl_Fee       S              1n
      *
     D                 ds
     D Xxpol#                        15
     D  Elfnd                         3  0 overlay(xxpol#)
     D  Elemp#                        9  0 overlay(xxpol#:4)
     D  Elfyr                         3  0 overlay(xxpol#:13)

     /eject
      *
     C     Wdell_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    X1fnd
     C                   Kfld                    X1emp#
     C                   Kfld                    X1div
     C                   Kfld                    X1fyr
      *
     C     Wdell2_Key    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    SvFnd
     C                   Kfld                    SvEmp
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Kyrtcd
     *
     C     Wmrpl4_Key    Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Svfnd
     C                   Kfld                    Svfyr
     C                   Kfld                    Svemp
     *
    C*    Wdrrl_Key     Klist
    C*                  Kfld                    X1co#
    C*                  Kfld                    X1Fnd
    C*                  Kfld                    X1St
    C*                  Kfld                    ElPfdt
     *
    C*    Wdrrl_Key1    Klist
    C*                  Kfld                    X1co#
    C*                  Kfld                    X1Fnd
    C*                  Kfld                    X1St
     *
     C     Wmifl_Key     Klist
     C                   Kfld                    X1co#
     C                   Kfld                    X1Fnd
     C                   Kfld                    X1St
     C                   Kfld                    ElPfdt
     *
     C     Wmifl_Key1    Klist
     C                   Kfld                    X1co#
     C                   Kfld                    X1Fnd
     C                   Kfld                    X1St
     *
     C     Wmgcl_Key     Klist
     C                   Kfld                    Xxco#
     C                   Kfld                    Svfnd
      *
     C     Dtcymd        Plist
     C                   Parm                    Srdti
     C                   Parm      *Blanks       Srffmt
     C                   Parm      '*CYMD'       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
     *
     C     *entry        Plist
     C                   Parm                    @@co#
     C                   Parm                    @@todt
     *
     C                   Move      @@co#         Xxco#
     C                   Move      @@todt        X8todt
     C                   eval      Srdti = X8todt
     C                   Call      'SRCVTDAT'    Dtcymd
     C                   Movel     Srdto         X7todt
      *
     C                   Eval      First = *On
      *
     /eject
     C                   Dou       %Eof(Wxx1l1)
     *
     C                   Read      Wxx1l1                                 60
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Eval      Instl_Fee = *Off
      *
     C                   If        First
     C                   Eval      First = *Off
     C                   Eval      Svco# = X1co#
     C                   Eval      Svfnd = X1fnd
     C                   Eval      Svemp = X1emp#
     C                   Eval      Svfyr = X1fyr
     C                   Eval      Svst  = X1st
     C                   Endif
      *
     C                   IF        Svco# <> X1co# or
     C                             Svfnd <> X1fnd or
     C                             Svemp <> X1emp# or
     C                             Svfyr <> X1fyr or
     C                             Svst  <> X1st
      *
     C                   Exsr      Write_Grssp
      *
     C                   Eval      Svco# = X1co#
     C                   Eval      Svfnd = X1fnd
     C                   Eval      Svemp = X1emp#
     C                   Eval      Svfyr = X1fyr
     C                   Eval      Svst  = X1st
     C                   Endif
     *
     C                   Eval      Amount = X1amt
      *
     C                   Clear                   Ssefdt
      *
     C                   Exsr      Eff_Date_St
      *
     C     Firstefd      IFNE      *ZERO
     C     *YMD          MOVE      Firstefd      DATUSA
     C                   MOVE      DATUSA        Ssefdt
     C                   ENDIF
      *
     C     Wdell_Key     Chain     Wdell                              60
      *
     C     Wmifl_Key     Setgt     Wmifl
     C     Wmifl_Key1    Readpe    Wmifl
      *
     C                   If        %Eof
     C                   Move      *Blanks       ififbl
     C                   Clear                   ififee
     C                   Endif
      *
      *   Ignore installment fees
      *
     C                   If        Ififpd = X1Arcd and
     C                             Ififpd <> *Blanks
     C                   Eval      Instl_Fee = *On
     C                   Endif
      *
     C                   Exsr      Add_Amounts
      *
     C                   Clear                   Sseomd
      *
     C                   If        x7todt <> *Zero
     C     *Ymd          Move      x7todt        Datusa
     C                   Move      Datusa        Sseomd
     C                   Endif

      *
     C                   Enddo
      *
     C                   If        Not First
     C                   Exsr      Write_Grssp
     C                   Endif
      *
     C*                  Return
     C                   Eval      *Inlr = *On
      /Eject
      **********************************************************************
      *
      **********************************************************************
      *
     C     Add_Amounts   Begsr
      *
      * Add to Monthly Collected Premium
      *
      *** beg chg ***** 04/05/04 ***********************************************
     C*****************  If        X1arty = 'P' or
     C*****************            X1arty = 'Q' or
     C*****************            X1arty = 'I'
     C                   If        (X1arty = 'P' or
     C                             X1arty = 'Q' or
      *** beg add ***** 08/02/06 ***********************************************
     C                             X1arty = 'T' or
      *** end add ***** 08/02/06 ***********************************************
     C                             X1arty = 'I') and
     C                             x1ctyp <> 'R'
      *** end chg ***** 04/05/04 ***********************************************
     C                   Eval      Monthp = Amount + Monthp
     C                   Endif
      * Reversing sign in R,N,V
      *** beg chg ***** 04/05/04 ***********************************************
     C*****************  If        X1arty = 'R' or
     C*****************            X1arty = 'N' or
     C*****************            X1arty = 'V'
     C                   If        (X1arty = 'R' or
     C                             X1arty = 'N' or
     C                             X1arty = 'V') and
     C                             x1ctyp <> 'R'
      *** end chg ***** 04/05/04 ***********************************************
     C                   Eval      Monthp = Monthp - Amount
     C                   Endif
      *
      * Add to Commissionable Collected Premium
      *
     C                   If        X1arty = 'P' and
     C                             X1ctyp = 'P'
      *** beg add ***** 08/02/06 ***********************************************
     C                             or X1arty = 'T'
      *** end add ***** 08/02/06 ***********************************************
      * per Ginger
     C*                  Add       X1camt        Cmclpr
     C                   Eval      Cmclpr = Cmclpr + Amount
      *
     C                   Endif
      * Reversing sign in R,N,V
     C                   If        (X1arty = 'R' or
     C                              X1arty = 'N' or
     C                              X1arty = 'V') and
     C                             X1ctyp = 'P'
     C                   Eval      Cmclpr = Cmclpr - Amount
      *
     C                   Endif
      *
      * Sub from Monthly Collected Premium
      *
     C                   If        Instl_Fee
     C                   Add       Amount        InsFee
     C                   Endif
      *
      * add to Expense Constant Amount
      *
     C                   If        X1arty = 'P' and
     C                             X1ctyp = 'E'
     C                   Add       Amount        Expena
     C                   Endif
      *
      * add to Late Fee
      *
     C                   If        X1arty = 'Q' and
     C                             X1ctyp = 'L'
     C                   Add       Amount        Latfee
     C                   Endif
      *
      * add to Late Fee
      *
     C     X1arty        Ifeq      'I'
     C                   Add       Amount        Secdep
     C                   Endif
      *
      * add to Municipal Tax Amount 1
      *
      *** beg chg ***** 09/16/04 ***********************************************
     C*****X1arty********Ifeq      'Q'
     C*****X1cty*********Andeq     'T'
     C                   if        X1ARTY = 'Q' and
     c                             (X1CTYP = 'T' or X1CTYP = 'D')
      *** end chg ***** 09/16/04 ***********************************************
     C                   Add       Amount        Muntax
     C                   Endif
      *
      * add to Monthly Policy Fees Collected
      *
     C                   If        X1arty = 'Q'  and
     C                             X1ctyp <> 'L' and
     C                             X1ctyp <> 'T' and
     C                             X1ctyp <> '1' and
     C                             X1ctyp <> '2' and
     C                             X1ctyp <> '3' and
      *** beg add ***** 05/27/04 ***********************************************
     C                             X1ctyp <> '4' and
     C                             X1ctyp <> '5' and
      *** end add ***** 05/27/04 ***********************************************
      *** beg add ***** 04/05/04 ***********************************************
     C                             x1ctyp <> 'R' and
      *** end add ***** 04/05/04 ***********************************************
      *** beg add ***** 09/16/04 ***********************************************
     C                             X1CTYP <> 'D' and
      *** end add ***** 09/16/04 ***********************************************
     C                             not Instl_Fee
     C                   Add       Amount        Monpfc
     C                   Endif
      *
      * add to Surcharges
      *
     C     X1arty        Ifeq      'Q'
     C*    Wdrrl_Key     Chain     Wdrrl
      *
     C                   Select
     C     X1ctyp        Wheneq    '1'
     C                   Add       Amount        Surch1
      *
     C     X1ctyp        Wheneq    '2'
     C                   Add       Amount        Surch2
      *
     C     X1ctyp        Wheneq    '3'
     C                   Add       Amount        Surch3
      *
      *** beg add ***** 05/27/04 ***********************************************
     C     X1ctyp        Wheneq    '4'
     C                   Add       Amount        Surch4
      *
     C     X1ctyp        Wheneq    '5'
     C                   Add       Amount        Surch5
      *
      *** end add ***** 05/27/04 ***********************************************
     C                   Endsl
      *
     C                   Endif
      *
     C                   Endsr
      *
      /eject
     **********************************************************************
     *  Get Earliest Effective Date for State /Year
     **********************************************************************
      *
     C     Eff_Date_St   Begsr
      *
     C                   Clear                   FirstFyr
     C                   Clear                   FirstEfd
      *
     C     Wdell2_Key    Setll     Wdell2
      *
     C                   Dou       %eof(Wdell2)
      *
     C     Wdell2_Key    Reade     Wdell2
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        L2Fyr <> X1fyr
     C                   Iter
     C                   Endif
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
      *
     C                   If        RsJrst = X1st
      *
     C                   If        L2pfdt < FirstEfd or
     C                             FirstEfd = *Zero
     C                   Eval      FirstEfd = L2Pfdt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /Eject
      **********************************************************************
      *
      **********************************************************************
     C     Write_Grssp   Begsr
      *
      * Write Gross Payment record
      *
     C     Wmgcl_Key     Chain     Wmgcl                              99
MJF C                   Movel     Gcpgid        Sspgid
     C     Wmrpl4_Key    Chain     Wmrpl4                             99
     *
     C                   If        *In99 = *off and
     C                             Rpcap# <> *Blanks
     C                   Movel(p)  Rpcap#        Sspol
     C                   Else
     C                   Movel(p)  Xxpol#        Sspol
     C                   Endif
      *
     C                   Eval      Hold20 = Monthp
     C                   Eval      Ssmcpr = %Editc(Hold20:'P')
     C                   Eval      Ssmcpr# = Hold20

     C                   Eval      Hold20 = Expena
     C                   Eval      Ssecam = %Editc(Hold20:'P')
     C                   Eval      Ssecam# = Hold20

     C                   Eval      Hold20 = Latfee
     C                   Eval      Sslfee = %Editc(Hold20:'P')
     C                   Eval      Sslfee# = Hold20
      *
     C                   Eval      Hold20 = Muntax
     C                   Eval      Ssmta1 = %Editc(Hold20:'P')
     C                   Eval      Ssmta1# = Hold20
      *
     C                   Eval      Ssmwpr = %Editc(Zeros:'P')
     C                   Eval      Ssmwpr# = Zeros
     C                   Eval      Ssscam = %Editc(Zeros:'P')
     C                   Eval      Ssscam# = Zeros
     C                   Eval      Ssifee = %Editc(Zeros:'P')
     C                   Eval      Ssifee# = *Zero
     C                   Eval      Ssitfe = %Editc(InsFee:'P')
     C                   Eval      Ssitfe# = InsFee
     C                   Eval      Ssmvrf = %Editc(Zeros:'P')
     C                   Eval      Ssmvrf# = Zeros
     C                   Eval      Ssnsff = %Editc(Zeros:'P')
     C                   Eval      Ssnsff# = Zeros
     C                   Eval      Sssecd = %Editc(Secdep:'P')
     C                   Eval      Sssecd# = Secdep
     C                   Eval      Ssmta2 = %Editc(Zeros:'P')
     C                   Eval      Ssmta2# = Zeros
     C                   Eval      Ssmta3 = %Editc(Zeros:'P')
     C                   Eval      Ssmta3# = Zeros
     C                   Eval      Ssmta4 = %Editc(Zeros:'P')
     C                   Eval      Ssmta4# = Zeros
     C                   Eval      Ssmta5 = %Editc(Zeros:'P')
     C                   Eval      Ssmta5# = Zeros
     C                   Eval      Ssothp = %Editc(Zeros:'P')
     C                   Eval      Ssothp# = Zeros
     C                   Eval      Ssprcv = %Editc(Zeros:'P')
     C                   Eval      Ssprcv# = Zeros
     C                   Eval      Ssccpr = %Editc(Cmclpr:'P')
     C                   Eval      Ssccpr# = cmclpr
     C                   Eval      Sssur4 = %Editc(Zeros:'P')
     C                   Eval      Sssur4# = Zeros
     C                   Eval      Sssur5 = %Editc(Zeros:'P')
     C                   Eval      Sssur5# = Zeros
     C                   Eval      Sssty1 = *Blank
     C                   Eval      Sssty2 = *Blank
     C                   Eval      Sssty3 = *Blank
     C                   Eval      Sssty4 = *Blank
     C                   Eval      Sssty5 = *Blank
      *** beg chg ***** 09/16/04 ***********************************************
     C*****************  Eval      Ssmts1 = *Blank
     C                   eval      SSMTS1 = svst
      *** end chg ***** 09/16/04 ***********************************************
     C                   Eval      Ssmts2 = *Blank
     C                   Eval      Ssmts3 = *Blank
     C                   Eval      Ssmts4 = *Blank
     C                   Eval      Ssmts5 = *Blank
     C                   Eval      Ssmwpr = %Editc(Zeros:'P')
     C                   Eval      Ssmwpr# = Zeros
      *
     C                   Eval      Hold20 = Monpfc
     C                   Eval      Ssmpfc = %Editc(Hold20:'P')
     C                   Eval      Ssmpfc# = Hold20
      *
     C                   Eval      Hold20 = Surch1
     C                   Eval      Sssur1 = %Editc(Hold20:'P')
     C                   Eval      Sssur1# = Hold20
      *
     C                   Eval      Hold20 = Surch2
     C                   Eval      Sssur2 = %Editc(Hold20:'P')
     C                   Eval      Sssur2# = Hold20
      *
     C                   Eval      Hold20 = Surch3
     C                   Eval      Sssur3 = %Editc(Hold20:'P')
     C                   Eval      Sssur3# = Hold20
      *
      *** beg add ***** 05/27/04 ***********************************************
     C                   Eval      Hold20 = Surch4
     C                   Eval      Sssur4 = %Editc(Hold20:'P')
     C                   Eval      Sssur4# = Hold20
      *
     C                   Eval      Hold20 = Surch5
     C                   Eval      Sssur5 = %Editc(Hold20:'P')
     C                   Eval      Sssur5# = Hold20
      *
      *** end add ***** 05/27/04 ***********************************************
     C                   Eval      Ssfyr  = Svfyr
     C                   Eval      Ssfnd  = Svfnd
     C                   Eval      Ssemp# = Svemp
     C                   Eval      Ssrst  = Svst
      *
     C                   Write     Gss01
     C                   Exsr      WrPolm
      *
     C                   Clear                   Gss01
      *
     C                   Clear                   Monthp
     C                   Clear                   InsFee
     C                   Clear                   Amount
     C                   Clear                   Expena
     C                   Clear                   Cmclpr
     C                   Clear                   Latfee
     C                   Clear                   Secdep
     C                   Clear                   Muntax
     C                   Clear                   Monpfc
     C                   Clear                   Surty1
     C                   Clear                   Surty2
     C                   Clear                   Surty3
      *** beg add ***** 05/27/04 ***********************************************
     C                   Clear                   Surty4
     C                   Clear                   Surty5
      *** end add ***** 05/27/04 ***********************************************
     C                   Clear                   Surch1
     C                   Clear                   Surch2
     C                   Clear                   Surch3
      *** beg add ***** 05/27/04 ***********************************************
     C                   Clear                   Surch4
     C                   Clear                   Surch5
      *** end add ***** 05/27/04 ***********************************************
      *** beg add ***** 09/16/04 ***********************************************
     C                   Clear                   SSMTS1
      *** end add ***** 09/16/04 ***********************************************
     C                   Clear                   Hold20
      *
     C                   Endsr
      *==============================================================
      * WRPOLM - Write to Policy Master File
      *==============================================================
     C     WRPOLM        BEGSR
      *
     C                   Movel(P)  SSFND         ##POL
     C                   Movel(P)  SSEMP#        XXTMP            10
     C                   CAT(P)    XXTMP:0       ##POL
     C                   Movel(P)  SSFYR         XXTMP            10
     C                   CAT(P)    XXTMP:0       ##POL
      *
     C                   Call      'W8126R2'
     C                   Parm      @@CO#         ##CO#             3
     C                   Parm      @@todt        ##TODT            8
     C                   Parm                    ##POL            15
     C                   Parm      X1Div         ##Div             5 0
      *
     C                   ENDSR
      *
     **********************************************************************
