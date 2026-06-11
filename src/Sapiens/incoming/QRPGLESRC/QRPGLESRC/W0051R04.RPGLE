    Hcopyright('(C) Copyright Fiserv, Inc., 2006 - 2007')
    H*********************************************************************
    H/TITLE W0051R04 - Employer Policy Year Deletion Check
    H*********************************************************************
    H* Description:                                                      *
    H*  This progrma will determine if employer policy year can be       *
    H*   deleted and returns the indicator for displaying the error      *
    H*   message.                                                        *
    H*                                                                   *
    H* Called from:                                                      *
    H*  W0051R                                                           *
    H*  W0050R04                                                         *
    H* Calls to:                                                         *
    H*                                                                   *
    H*                                                                   *
    H* Created: 08/21/06     C:33    Seq#: WA135/52534                   *
    H*********************************************************************
    H* Revisions:                                                        *
    H*                                                                   *
     H* 07/02/18 C:291 Support : 46769                                    *
     H*                Add an edit to prevent the policy year to be       *
     H*                deleted if it is the only year on the policy.      *
     H*                                                                   *
    H* 09/20/13 C:65  Support: WA099/83338                               *
    H*                Remove CX date check and fix submission history    *
    H*                check.  CX date check is only important if the     *
    H*                transaction has been reported and the submission   *
    H*                history check verifies that.                       *
    H*                                                                   *
    H* 06/13/07 C:136 Support: WA132 57014                               *
    H*                Don't allow delete if policy is changing from      *
    H*                summarized to non-summarized and bills exist in    *
    H*                the state reporting division. The revised bill     *
    H*                process must be run to correct this condition.     *
    H*                                                                   *
     H* 05/11/07 C:136 PROJECT: WA099 52029                               *
     H*                Don't allow delete if s/r billing records exist    *
    H*                in the state reporting division.                   *
    H*                                                                   *
    H* 04/30/07  C:33  Seq#: WA129/45522                                 *
    H*                 Implement check against Emp Master cx date.       *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0051f04  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fssylf     if   e           k disk
     F*** Beg Chg *** 07/02/18 ********************************************
     F***wdell     if   e           k disk    rename(pel01:pel00)
     Fwdell     if   e           k disk    rename(pel01:pel00) prefix(x)
     F*** Beg Chg *** 07/02/18 ********************************************
     Fwmexl     if   e           k disk    rename(pex01:pex00)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     Fwmcml     if   e           k disk    rename(pcm01:pcm00)
     Fwmeal     if   e           k disk    rename(pea01:pea00)
     Fwdeal5    if   e           k disk    rename(pea01:pea05)
     Fwdehl6    if   e           k disk    rename(peh01:peh06)
     Fwmshl13   if   e           k disk    rename(psh01:psh13)
     Fwasrl5    if   e           k disk    rename(psr01:psr05)
     D*** begin add *** 04/13/07 *******************************************
     Fwmeal23   if   e           k disk    rename(pea01:pea23)
     Fwdf1l     if   e           k disk    rename(pf101:pf10l)
     D*** end   add *** 04/13/07 *******************************************

     D*entry
     DW0051R04         pr                  extPgm('W0051R04')
     D @@rrn                          9  0 const
     D @@qte                          1    const
     D @@ind                          2  0
     DW0051R04         pi
     D @@rrn                          9  0 const
     D @@qte                          1    const
     D @@ind                          2  0

     D/copy sPrDate
     D***  Begin Add  ***  04/30/07  **************************************
     D/copy wPrEmplr
     D***  End   Add  ***  04/30/07  **************************************

     DARBatchRcds      pr                  like(*in60)
     DAuthorized       pr                  like(*in60)
     DClaimsExist      pr                  like(*in60)
     DDtlHistExist     pr                  like(*in60)
     D***  Begin Add  ***  04/30/07  **************************************
     DEmpMstrCxd       pr                  like(*in60)
     D***  End   Add  ***  04/30/07  **************************************
     DGetStRptDiv      pr                  like(EMDIV)
     DNCCISubSts4      pr                  like(*in60)
     DPaymentsExist    pr                  like(*in60)
     D outInd                              like(@@Ind)
     DPayrollExist     pr                  like(*in60)
     DStRptBillExist   pr                  like(*in60)
     D*** begin add *** 05/11/07 *******************************************
     DSRBillsInSRDiv   pr                  like(*in60)
     D*** end   add *** 05/11/07 *******************************************
     D*** begin add *** 07/02/18 *******************************************
     DOnlyPolicyYear   pr                  like(*in60)
     D*** endin add *** 07/02/18 *******************************************

     D stRptDiv        s                   like(EMDIV)
     D xxInd           s                   like(@@Ind)

     D***  begin add  ***  09/20/13  **************************************
     D                 ds
     D  sst                    1     30    dim(15)
     D  shst01                 1      2
     D  shst02                 3      4
     D  shst03                 5      6
     D  shst04                 7      8
     D  shst05                 9     10
     D  shst06                11     12
     D  shst07                13     14
     D  shst08                15     16
     D  shst09                17     18
     D  shst10                19     20
     D  shst11                21     22
     D  shst12                23     24
     D  shst13                25     26
     D  shst14                27     28
     D  shst15                29     30
     D                 ds
     D  sss                    1     15    dim(15)
     D  shss01                 1      1
     D  shss02                 2      2
     D  shss03                 3      3
     D  shss04                 4      4
     D  shss05                 5      5
     D  shss06                 6      6
     D  shss07                 7      7
     D  shss08                 8      8
     D  shss09                 9      9
     D  shss10                10     10
     D  shss11                11     11
     D  shss12                12     12
     D  shss13                13     13
     D  shss14                14     14
     D  shss15                15     15
     D                 DS
     D  SSD                    1    105  0
     D                                     DIM(15)
     D  SHDT01                 1      7  0
     D  SHDT02                 8     14  0
     D  SHDT03                15     21  0
     D  SHDT04                22     28  0
     D  SHDT05                29     35  0
     D  SHDT06                36     42  0
     D  SHDT07                43     49  0
     D  SHDT08                50     56  0
     D  SHDT09                57     63  0
     D  SHDT10                64     70  0
     D  SHDT11                71     77  0
     D  SHDT12                78     84  0
     D  SHDT13                85     91  0
     D  SHDT14                92     98  0
     D  SHDT15                99    105  0
     D***  end add ******  09/20/13  **************************************
     D psds          esds                  extname(szq1p)

     C     @@rrn         chain     wdelp
     C                   eval      stRptDiv = GetStRptDiv

     C                   select
     C                   when      not Authorized
     C                   eval      @@ind = 77
     C***  Begin Add  ***  04/30/07  **************************************
     C***  begin del  ***  09/20/13  **************************************
     C**********         when      EmpMstrCxd
     C**********         eval      @@ind = 87
     C***  end del ******  09/20/13  **************************************
     C***  End   Add  ***  04/30/07  **************************************
     C                   when      ClaimsExist and @@qte <> 'Q'
     C                   eval      @@ind = 71
     C                   when      PaymentsExist(xxInd) and @@qte <> 'Q'
     C                   eval      @@ind = xxInd
     C*** begin add *** 05/11/07 *******************************************
     C                   when      SRBillsInSRDiv and @@qte <> 'Q'
     C                   eval      @@ind = 85
     C*** end   add *** 05/11/07 *******************************************
     C                   when      PayrollExist and @@qte <> 'Q'
     C                   eval      @@ind = 86
     C                   when      StRptBillExist and @@qte <> 'Q'
     C                   eval      @@ind = 83
     C                   when      NCCISubSts4 and @@qte <> 'Q'
     C                   eval      @@ind = 80
     C                   when      ARBatchRcds and @@qte <> 'Q'
     C                   eval      @@ind = 81
     C                   when      DtlHistExist
     C                   eval      @@ind = 84
     C*** begin add *** 07/02/18 *******************************************
     C                   when      OnlyPolicyYear and @@qte <> 'Q'
     C                   eval      @@ind = 89
     C*** end   add *** 07/02/18 *******************************************
     C                   endsl

     C                   eval      *inlr = *on





     C*====================================================================
     C* Determine if A/R batch records exist
     C*====================================================================
     PARBatchRcds      b
     DARBatchRcds      pi                  like(*in60)

     C     keyWDEAL5     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR

     C     keyWDEAL5     setll     pea05
     *
     C                   dou       %eof
     C     keyWDEAL5     reade     pea05
     C                   if        not %eof and EASTAT <> 'T'
     C                   return    *on
     C                   endif
     C                   enddo

     C                   return    *off

     PARBatchRcds      e





     C*====================================================================
     C* Determine if user is authorized based on security
     C*====================================================================
     PAuthorized       b
     DAuthorized       pi                  like(*in60)

     C     keyWMEXL      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND

     C     q1user        chain     pup00                              60
     C     keyWMEXL      chain     pex00

B02  C                   if        not %found or UPLVL < EXDLTL
     C                   return    *off
     C                   endif

     C                   return    *on

     PAuthorized       e





     C*====================================================================
     C* Determine if claims exist
     C*====================================================================
     PClaimsExist      b
     DClaimsExist      pi                  like(*in60)

     C     keyWMCML      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR

     C     keyWMCML      chain     pcm00

     C                   return    %found

     PClaimsExist      e





     C*====================================================================
     C* Determine the Detail History exists
     C*====================================================================
     PDtlHistExist     b
     DDtlHistExist     pi                  like(*in60)

     D                 DS
     D histTransDt7                   7  0
     D  histTransDtYM                 5  0 overlay(histTransDt7)
     D                 DS
     D histEffDt7                     7  0
     D  histEffDtYM                   5  0 overlay(histEffDt7)
     D                 DS
     D currentDt7                     7  0
     D  currentDtYM                   5  0 overlay(currentDt7)

     C     keyWDEHL6     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C                   kfld                    currentDt7

     C     key1WDEHL6    klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR

     C                   eval      currentDt7 = DateTo7(SystemDate)

     C     keyWDEHL6     setll     peh06

     C                   dou       %eof

     C     key1WDEHL6    readpe    peh06

     C                   if        not %eof

     C                   eval      histTransDt7 = EHTRDT
     C                   eval      histEffDt7   = EHEFDT

     C                   if        histTransDtYM < currentDtYM and
     C                             histEffDtYM   < currentDtYM
     C                   return    *on
     C                   endif

     C                   endif

     C                   enddo

     C                   return    *off

     PDtlHistExist     e





     P***  Begin Add  ***  04/30/07  **************************************
     C*====================================================================
     C* Determine if the Employer Master Record is already cx'd
     C*====================================================================
     PEmpMstrCxd       b
     DEmpMstrCxd       pi                  like(*in60)

     D cxDate          s                   like(EMCAD1)

     C                   eval      cxDate = GetCxDate(ELCO#:ELFND:ELEMP#:
     C                                                ELDIV)

     C                   if        cxDate <> *zero
     C                   return    *on
     C                   endif

     C                   return    *off

     PEmpMstrCxd       e
     P***  End   Add  ***  04/30/07  **************************************





     C*====================================================================
     C* Get the state reporting division
     C*====================================================================
     PGetStRptDiv      b
     DGetStRptDiv      pi                  like(EMDIV)

     D stRptDiv        s                   like(EMDIV)

     C     keyWMEML      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#

     C                   eval      stRptDiv = ELDIV

     C     keyWMEML      setll     WMEML
     C     keyWMEML      reade     WMEML
     C                   dow       not %eof
     C                   if        EMSTRP = 'Y'
     C                   return    EMDIV
     C                   endif
     C     keyWMEML      reade     WMEML
     C                   enddo

     C                   return    stRptDiv

     PGetStRptDiv      e





     C*====================================================================
     C* Determine the status of the NCCI submissions
     C*====================================================================
     PNCCISubSts4      b
     DNCCISubSts4      pi                  like(*in60)

     C     keyWMSHL13    klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR

     C     keyWMSHL13    setll     psh13
     *
     C                   dou       %eof
     *
     C     keyWMSHL13    reade     psh13
     *
     C***  begin chg  ***  09/20/13  **************************************
     C**********         if        SHSTAT = '4' and not %eof
     C**********         return    *on
     C**********         endif
     C                   If        not %eof
     C
     C     1             Do        15            x                 2 0
     C
     C                   If        sst(x) <> '  ' and
     C                             (sss(x) = '4' or ssd(x) <> 0)
     C                   Return    *on
     C                   Endif
     C
     C                   Enddo
     C
     C                   If        shstat = '4'
     C                   Return    *on
     C                   Endif
     C
     C                   Endif
     C***  end chg ******  09/20/13  **************************************
     *
     C                   enddo
     *
     C                   return    *off

     PNCCISubSts4      e





     C*====================================================================
     C* Determine if payments exist
     C*====================================================================
     PPaymentsExist    b
     DPaymentsExist    pi                  like(*in60)
     D outInd                              like(@@Ind)

     D                 DS
     D transDt7                       7  0
     D  transDtYM                     5  0 overlay(transDt7)
     D                 DS
     D postDt7                        7  0
     D  postDtYM                      5  0 overlay(postDt7)
     D                 DS
     D currentDt7                     7  0
     D  currentDtYM                   5  0 overlay(currentDt7)

     C     keyWMEAL      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR

     C                   eval      currentDt7 = DateTo7(SystemDate)

     C     keyWMEAL      chain     pea00                              60
     *
B04  C                   dow       *in60 = *off
      *
     C                   eval      transDt7 = EATRDT
     C                   eval      postDt7  = EAPODT
     *
B05  C                   select
     *
S01  C                   when      EAARTY = 'P' or
     C                             EAARTY = 'I' or
     C                             EAARTY = 'Q'
     C                   eval      outInd = 72
     C                   return    *on
     *
S02  C                   when      transDtYM > postDtYM    and
     C                             transDtYM < currentDtYM
     C                   eval      outInd = 74
     C                   return    *on
     *
S03  C                   when      transDtYM < currentDtYM and
     C                             postDtYM  < currentDtYM
     C                   eval      outInd = 74
     C                   return    *on
     *
S04  C                   other
     C     keyWMEAL      reade     pea00                                  60
     *
E05  C                   endsl
     *
E04  C                   enddo
     *
     C                   return    *off

     PPaymentsExist    e





     C*====================================================================
     C* Determine if S/R payroll records exist
     C*====================================================================
     PPayrollExist     b
     DPayrollExist     pi                  like(*in60)

     C     keyWASRL5     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR

     *
     C     keyWASRL5     setll     psr05
     *
B04  C                   dou       %eof
     *
     C     keyWASRL5     reade     psr05
     *
     C                   if        not %eof
     *
     C                   eval      scEmp# = SREMP#
     C                   eval      scDiv  = SRDIV
     C                   eval      scFyr  = SRFYR
     C                   eval      scOvr  = 'N'
     *
     C                   exfmt     pyrlWDW
     *
     C                   select
B07  C                   when      SCOVR <> 'Y'
     C                   return    *on
     C                   other
     C                   return    *off
E07  C                   endsl
     *
     C                   endif
     *
     C                   enddo
     *
     C                   return    *off

     PPayrollExist     e





     C*** begin add *** 05/11/07 *******************************************
     c*====================================================================
     c* Determine if there is activity for the state reporting division
     c*====================================================================
     pSRBillsInSRDiv   b
     dSRBillsInSRDiv   pi                  like(*in60)

     c     f1ky          klist
     c                   kfld                    emco#
     c                   kfld                    emfnd

     c     wmeal23KY     klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elemp#
     c                   kfld                    eldiv
     c                   kfld                    elfyr
     c*
     c* Are there any self-reported billings for this division in the state reporting division or
     c* does the state reporting division have any other division's self reported billings in it?
     c*
     c     f1ky          chain     pf10l
     c     wmeal23KY     setll     pea23
     c     wmeal23KY     reade     pea23
     c                   dow       not %eof
     c                   if        EADIV = stRptDiv and
     c                             EARDIV <> EADIV and
     c                             EAARCD = F1SRBB
     c                   return    *on
     c                   endif
     c     wmeal23KY     reade     pea23
     c                   enddo
     c                   return    *off

     pSRBillsInSRDiv   e
     C*** end   add *** 05/11/07 *******************************************
     C*====================================================================
     C* Determine if billings exist for the state reporting division
     C*====================================================================
     PStRptBillExist   b
     DStRptBillExist   pi                  like(*in60)

     C     key1WMEAL     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    stRptDiv
     C                   kfld                    ELFYR

      *** beg chg ***** 06/13/07 *******************************************************
     C*****************  if        ELARSM = 'Y' and ELDIV <> stRptDiv
     C                   if        (ELARSM = 'Y' or ELARSM = '@') and
     C                             ELDIV <> stRptDiv
      *** end chg ***** 06/13/07 *******************************************************
     *
     C     key1WMEAL     chain     pea00                              60
     *
     C                   dow       *IN60 = *OFF
     *
     C                   select
     *
     C                   when      EAARTY = 'H' or
     C                             EAARTY = 'F' or
     C                             EAARTY = 'B' or
     C                             EAARTY = 'A' or
     C                             EAARTY = 'D'
     C                   return    *on
     *
     C                   other
     * Need to read with Key having State Reporting Division
     C     key1WMEAL     reade     pea00                                  60
     *
     C                   endsl
     *
     C                   enddo
     *
     C                   endif
     *
     C                   return    *off

     PStRptBillExist   e

     C*====================================================================
     C* Determine if this is the only policy year on this Division
     C*====================================================================
     POnlyPolicyYear   b
     DOnlyPolicyYear   pi                  like(*in60)
      /Free

           Setll (ELco#:ELfnd:ELemp#:ELdiv) pel00;
           Reade (ELco#:ELfnd:ELemp#:ELdiv) pel00;
           DoW not %eof;
              If xELfyr <> ELfyr;
                 Return *off;
              Endif;
           Reade (ELco#:ELfnd:ELemp#:ELdiv) pel00;
           Enddo;

           Return *on;

      /End-Free
     POnlyPolicyYear   e

