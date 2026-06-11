     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
    H*********************************************************************
    H/TITLE W0050R04 - Employer Deletion Check
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H bnddir('WTBNDDIR')
    H*********************************************************************
    H* Description:                                                      *
    H*  This progrma will determine if employer can be deleted and       *
    H*   returns the indicator for displaying the error message.         *
    H*                                                                   *
    H* Called from:                                                      *
    H*  W0050R                                                           *
    H* Calls to:                                                         *
    H*                                                                   *
    H*                                                                   *
    H* Created: 08/21/06     C:33    Seq#: WA135/52534                   *
    H*********************************************************************
    H* Revisions:                                                        *
   h*                                                                   *
     h*  05/24/23 C:79  Support: 150/107702                               *
     h*                 Add check for Quote Security on Delete Employer   *
   h*                                                                   *
     h*  07/01/21 C:33  Support: 96462                                    *
     h*                 Remove edits for Diaries & S/R Payrolls.          *
    H*                                                                   *
    H* 02/04/13 C:282 Project: WC2115                                    *
    H*                Added the condition in PayrollExist procedure      *
    H*                will not open screen file if called from WTPOLTVAL *
    H*                                                                   *
    H* 11/20/12 C:65  Project: WC2116                                    *
    H*                Do not open screen file if called from WTPOLTVAL.  *
    H*                                                                   *
    H* 09/11/12 C:33  Project: WC2116                                    *
    H*                Use program for all divisions.                     *
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
     H* 09/14/06 C:62  PROJECT: 54729                                     *
     H*          C:33  Ignore darts remarks that are completed.           *
    H*********************************************************************
     Fw0050f04  cf   e             workstn usropn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fssylf     if   e           k disk
     F***  Begin Chg  ***  07/01/21  ********************************
     F***sdcmla    if   e           k disk    rename(pcm01:scm0a) prefix(darts)
     Fsdcmla    if   e           k disk    rename(pcm01:scm0a)
     F***  End   Chg  ***  07/01/21  ********************************
     Fwdell     if   e           k disk    rename(pel01:pel00)
     Fwmexl     if   e           k disk    rename(pex01:pex00)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     Fwmcml     if   e           k disk    rename(pcm01:pcm00)
     Fwmeal     if   e           k disk    rename(pea01:pea00)
     Fwdeal5    if   e           k disk    rename(pea01:pea05)
     Fwmshl13   if   e           k disk    rename(psh01:psh13)
     Fwasrl7    if   e           k disk    rename(psr01:psr07)
     Fwmeal23   if   e           k disk    rename(pea01:pea23)
     Fwdf1l     if   e           k disk    rename(pf101:pf10l)

     D*entry
     DW0050R04         pr                  extPgm('W0050R04')
     D @@rrn                          9  0 const
     D @@qte                          1    const
     D @@ind                          2  0
     D @@allDiv                       1n   const options(*nopass)
     DW0050R04         pi
     D @@rrn                          9  0 const
     D @@qte                          1    const
     D @@ind                          2  0
     D @@allDiv                       1n   const options(*nopass)
     d*** Beg Add *** 05/24/23 *********************************************
     d RtvEnv          pr                  ExtPgm('RTVENV')
     d  strEnv                        4
     d*** End Add *** 05/24/23 *********************************************

     D/copy sPrDate

     DARBatchRcds      pr                  like(*in60)
     DAuthorized       pr                  like(*in60)
     DClaimsExist      pr                  like(*in60)
     DDARTSExists      pr                  like(*in60)
     DGetStRptDiv      pr                  like(EMDIV)
     DMCClmsExist      pr                  like(*in60)
     DNCCISubSts4      pr                  like(*in60)
     DPaymentsExist    pr                  like(*in60)
     D outInd                              like(@@Ind)
     DPayrollExist     pr                  like(*in60)
     DStRptBillExist   pr                  like(*in60)
     DSRBillsInSRDiv   pr                  like(*in60)

     D stRptDiv        s                   like(EMDIV)
     D xxInd           s                   like(@@Ind)
     D allDivisions    s              1n   inz
     d*** Beg Add *** 05/24/23 *********************************************
       dcl-s strEnv Char(4);
     d*** End Add *** 05/24/23 *********************************************

     D***  Begin Add  ***  07/01/21  ********************************
     Ddarts          e ds                  extname(SDCMLA:*input) qualified
     D***  End   Add  ***  07/01/21  ********************************

     D psds          esds                  extname(szq1p)

      /Free
       If %parms >= 4;
          allDivisions = @@allDiv;
       Endif;
      /End-free

      *
     C                   MOVE      *OFF          SCREEN
     C                   MOVEL     'WTPOLTVAL'   ##PGMN           10
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGMN           10
     C                   PARM                    ##PGMX            1
     C     ##PGMX        IFEQ      *OFF
     C                   MOVE      *ON           SCREEN            1
     C                   OPEN      W0050F04
     C                   ENDIF
      *
     C     @@rrn         chain     wmemp
     C                   eval      stRptDiv = GetStRptDiv
     C     @@rrn         chain     wmemp

      /Free
       Select;
       When allDivisions;
          Exsr ProcessAll;
       Other;
          Exsr Process;
       Endsl;
      /End-free

     C     SCREEN        IFEQ      *ON
     C                   CLOSE     W0050F04
     C                   ENDIF
     C                   eval      *inlr = *on
      /Free
       Begsr Process;
      /End-free
     C***  Begin Add  ***  07/01/21  ********************************
       // Add new edits to WPIEMPLR/DeleteEmployer
     C***  End   Add  ***  07/01/21  ********************************
     C                   select
     C                   when      not Authorized
     C                   eval      @@ind = 77
     C                   when      ClaimsExist and @@qte <> 'Q'
     C                   eval      @@ind = 73
     C                   when      PaymentsExist(xxInd) and @@qte <> 'Q'
     C                   eval      @@ind = xxInd
     C                   when      SRBillsInSRDiv and @@qte <> 'Q'
     C                   eval      @@ind = 61
     C***  Begin Del  ***  07/01/21  ********************************
     C**********         when      PayrollExist and @@qte <> 'Q'
     C**********         eval      @@ind = 86
     C***  End   Del  ***  07/01/21  ********************************
     C                   when      StRptBillExist and @@qte <> 'Q'
     C                   eval      @@ind = 69
     C                   when      NCCISubSts4 and @@qte <> 'Q'
     C                   eval      @@ind = 81
     C                   when      ARBatchRcds and @@qte <> 'Q'
     C                   eval      @@ind = 82
     C***  Begin Del  ***  07/01/21  ********************************
     C**********         when      DARTSExists() = *On
     C**********         eval      @@ind = 75
     C***  End   Del  ***  07/01/21  ********************************
     C                   when      MCClmsExist and @@qte <> 'Q'
     C                   eval      @@ind = 70
     C                   endsl
      /Free
       Endsr;

       Begsr ProcessAll;
          @@ind = 0;
          Setll (EMco#:EMfnd:EMemp#) pem00;
          Dou %eof(WMEML) or @@ind <> 0;
             Reade (EMco#:EMfnd:EMemp#) pem00;
             If not %eof(WMEML);
                Exsr Process;
             Endif;
          Enddo;
       Endsr;
      /End-free





     C*====================================================================
     C* Determine if A/R batch records exist
     C*====================================================================
     PARBatchRcds      b
     DARBatchRcds      pi                  like(*in60)

     C     keyWDEAL5     klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV

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
     C                   kfld                    EMCO#
     C                   kfld                    EMFND

     C     q1user        chain     pup00                              60
     C     keyWMEXL      chain     pex00
       //*** Beg Add *** 05/24/23 ******************************************
       Callp RtvEnv(strEnv);
       If strEnv = '*LIV';
       //*** End Add *** 05/24/23 ******************************************

B02  C                   if        not %found or UPLVL < EXDLTL
     C                   return    *off
     C                   endif
       //*** Beg Add *** 05/24/23 ******************************************
       Else;
          If strEnv = '*QTE';
B02          If Not %Found or UPlvl < EXdltLqt;
                return *off;
             Endif;
          Endif;
       Endif;
       //*** End Add *** 05/24/23 ******************************************

     C                   return    *on

     PAuthorized       e





     C*====================================================================
     C* Determine if claims exist
     C*====================================================================
     PClaimsExist      b
     DClaimsExist      pi                  like(*in60)

     C     keyWMCML      klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV

     C     keyWMCML      chain     pcm00

     C                   return    %found

     PClaimsExist      e





     C*====================================================================
     C* Determine if DARTS records exist
     C*====================================================================
     PDARTSExists      b
     DDARTSExists      pi                  like(*in60)

     DxxSyKey          s              2

     D                 ds
     D dartsKey                      40
     D  dartsCo#                      3  0 overlay(dartsKey)
     D  dartsFnd                      3  0 overlay(dartsKey:4)
     D  dartsEmp#                     9  0 overlay(dartsKey:7)
     D  dartsDiv                      5  0 overlay(dartsKey:16)
     D  dartsQte                      1    overlay(dartsKey:40)

     C                   eval      xxSyKey = 'SY'
     *
     C     xxSyKey       chain     ssy01
     *
B04  C                   if        %found and SYSCIF = 'Y'  and screen=*On
     *
     *    ESTABLISH KEY
     *
     C                   clear                   dartsKey
     *
     C                   eval      dartsCo#  = EMCO#
     C                   eval      dartsFnd  = EMFND
     C                   eval      dartsEmp# = EMEMP#
     C                   eval      dartsDiv  = EMDIV
     C*
     C                   if        @@qte = 'Q'
     C                   eval      dartsQte = 'Q'
     C                   endif
     *
     *  CHAIN TO DARTS REMARKS DETAIL FILE
     *
     C     dartsKey      setll     scm0a
     *
B05  C                   dou       %eof
     *
     C***  Begin Chg  ***  07/01/21  ********************************
     C*****dartsKey      reade     scm0a
     **********
     C**********         if        not %eof      and
B06  C**********                   dartsCMDIFL = 'Y'  and
     C**********                   dartsCMSTAT <> 'I'
     C**********                    and dartsCMDCDT = *zero
       Reade dartsKey scm0a darts;
       If not %eof
        and darts.CMdiFl = 'Y' and darts.CMstat <> 'I'
        and darts.CMdCDt = *zeros;
     C***  End   Chg  ***  07/01/21  ********************************
     *
     C                   eval      scEmp# = EMEMP#
     C                   eval      scDiv  = EMDIV
     C                   eval      scOvr  = 'N'
     *
     C                   exfmt     dartsWDW
     *
     C                   select
B07  C                   when      scOvr <> 'Y'
     C                   return    *on
     C                   other
     C                   return    *off
E07  C                   endsl
     *
E06  C                   endif
     *
E05  C                   enddo
     *
E04  C                   endif

     C                   return    *off

     PDARTSExists      e





     C*====================================================================
     C* Get the state reporting division
     C*====================================================================
     PGetStRptDiv      b
     DGetStRptDiv      pi                  like(EMDIV)

     D stRptDiv        s                   like(EMDIV)

     C     keyWMEML      KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#

     C                   eval      stRptDiv = EMDIV

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
     C* Determine if MC Claims exist
     C*====================================================================
     PMCClmsExist      b
     DMCClmsExist      pi                  like(*in60)

     D QQFBIF          s              1
     D QQFL01          s              1
     D QQWCIF          s              1
     D ##OUTS          s              1
     D ##RTCD          s              1
     D #@CO#           s              3
     D #@DIV           s              5
     D #@EMP#          s              9
     D #@FND           s              3
     D #@SUB#          s              5

     C                   call      'SA001R'
     C                   parm                    QQWCIF
     C                   parm                    QQFBIF
     C                   parm                    QQFL01
     *
     C                   if        QQFBIF = 'Y' and QQFL01 = 'Y'
     *
     C                   move      EMCO#         #@Co#
     C                   move      EMFND         #@Fnd
     C                   move      EMEMP#        #@Emp#
     C                   move      EMDIV         #@Div
     *
     C                   call      'FA701R'
     C                   parm      *blanks       ##RtCd
     C                   parm                    #@Co#
     C                   parm      *zeros        #@Sub#
     C                   parm                    #@Fnd
     C                   parm      *blanks       ##OutS
     *
     C                   if        ##RtCd <> 'F'
     C                             and ##OutS = 'N'
     *
     C                   call      'FA207R'
     C                   parm      *blanks       ##RtCd
     C                   parm                    #@Co#
     C                   parm      *zeros        #@Sub#
     C                   parm                    #@Fnd
     C                   parm                    #@Emp#
     C                   parm                    #@Div
     *
     C                   if        ##RtCd = *ON
     C                   return    *on
     C                   endif
     *
     C                   endif
     *
     C                   endif

     C                   return    *off

     PMCClmsExist      e





     C*====================================================================
     C* Determine the status of the NCCI submissions
     C*====================================================================
     PNCCISubSts4      b
     DNCCISubSts4      pi                  like(*in60)

     C     keyWMSHL13    klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   Kfld                    EMDIV

     C     keyWMSHL13    setll     psh13
     *
     C                   dou       %eof
     *
     C     keyWMSHL13    reade     psh13
     *
     C                   if        SHSTAT = '4' and not %eof
     C                   return    *on
     C                   endif
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
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV

     C                   eval      currentDt7 = DateTo7(SystemDate)
     C     keyWMEAL      chain     pea00                              60
B04  C                   dow       *in60 = *off

     C                   eval      transDt7 = EATRDT
     C                   eval      postDt7  = EAPODT
B05  C                   select
S01  C                   when      EAARTY = 'P' or
     C                             EAARTY = 'I' or
     C                             EAARTY = 'Q'
     C                   eval      outInd = 72
     C                   return    *on
S02  C                   when      transDtYM > postDtYM    and
     C                             transDtYM < currentDtYM
     C                   eval      outInd = 74
     C                   return    *on
S03  C                   when      transDtYM < currentDtYM and
     C                             postDtYM  < currentDtYM
     C                   eval      outInd = 74
     C                   return    *on
S04  C                   other
     C     keyWMEAL      reade     pea00                                  60
E05  C                   endsl

E04  C                   enddo
     C                   return    *off

     PPaymentsExist    e
     C*====================================================================
     C* Determine if S/R payroll records exist
     C*====================================================================
     PPayrollExist     b
     DPayrollExist     pi                  like(*in60)

     C     keyWASRL7     klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV

     C     keyWASRL7     setll     psr07
B04  C                   dou       %eof
     C     keyWASRL7     reade     psr07

      ******* beg chg  ***  02/04/13 *********************************
     C*****              if        not %eof
     C                   if        not %eof and screen = *On
      ******* end chg  ***  02/04/13 *********************************
     C                   eval      scEmp# = SREMP#
     C                   eval      scDiv  = SRDIV
     C                   eval      scFyr  = SRFYR
     C                   eval      scOvr  = 'N'
     C                   exfmt     pyrlWDW

     C                   select
B07  C                   when      SCOVR <> 'Y'
     C                   return    *on
     C                   other
     C                   return    *off
E07  C                   endsl

     C                   return    *on
     C                   endif

     C                   enddo
     C                   return    *off

     PPayrollExist     e





     c*====================================================================
     c* Determine if there is activity for the state reporting division
     c*====================================================================
     pSRBillsInSRDiv   b
     dSRBillsInSRDiv   pi                  like(*in60)

     c     f1ky          klist
     c                   kfld                    emco#
     c                   kfld                    emfnd

     c     wmeal23KY     klist
     c                   kfld                    emco#
     c                   kfld                    emfnd
     c                   kfld                    ememp#
     c                   kfld                    emdiv
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





     C*====================================================================
     C* Determine if billings exist for the state reporting division
     C*====================================================================
     PStRptBillExist   b
     DStRptBillExist   pi                  like(*in60)

     C     keyWDELL      klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV

     C     key1WMEAL     klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    stRptDiv
     C                   kfld                    ELFYR

     C     keyWDELL      chain     pel00                              60

     C                   dow       *in60 = *off
     *
     C                   if        (ELARSM = 'Y' or ELARSM = '@') and
     C                             ELDIV <> stRptDiv
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
     C     keyWDELL      reade     pel00                                  60
     *
     C                   enddo
     *
     C                   return    *off

     PStRptBillExist   e
