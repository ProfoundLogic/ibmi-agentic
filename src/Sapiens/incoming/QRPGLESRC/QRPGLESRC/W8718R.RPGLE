     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H*********************************************************************
     H/title W8718R - Mass Transfer - Quote to Live - Build WAQOP
     H*********************************************************************
     H* Program description:                                              *
     H*   This program will populate the Quote Transfer File (WAQOP) with *
     H*   employers from the selection criteria.  These will be processed *
     H*   to the live environment by W8700R.                              *
     H*                                                                   *
     H* CALLED FROM:                                                      *
     H*   W8718C                                                          *
     H*                                                                   *
     H* CALLS TO:                                                         *
     H*                                                                   *
     H* PROGRAMMER: C:33    CREATED: 10/10/03    PROJECT: WC1017          *
     H*********************************************************************
     H* REVISIONS:                                                        *
     H*                                                                   *
     H* 03/27/24  C:340 Project: PHW114663                                *
     H*                 Error Adding Division in Quote                    *
     H*                                                                   *
     H* 12/09/19  C:298 Project: WC2691                                   *
     H*                 Accept new optional parameter for State Rpt Div   *
     H*                 only.                                             *
     H*                                                                   *
     H* 04/01/13  C:282 Project: 147WAK                                   *
     H*                 Replacing W0050R17 by WTPOLQTFR                  .*
     H*                                                                   *
     H* 12/12/06  C:33  Project: WC1299                                   *
     H*                 Load value from WMEXP to WAQOP.                  .*
     H*                                                                   *
     H* 01/13/06  C:33  Project: WC1017   WA136/50847                     *
     H*                 Zero out QOTEMP prior to conditionally loading it.*
     H*********************************************************************
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fwdell     if   e           k disk    rename(pel01:pel00)
      *** Beg Add ***** 04/01/13 ******************************************
     f                                     infds(dbfds)
      *** End Add ***** 04/01/13 ******************************************
     Fwmahl     if   e           k disk    rename(pah01:pah00)
     Fwmexl     if   e           k disk    rename(pex01:pex00)

     Fwaqop     o  a e             disk    rename(pqo01:pqo0p)
     Fwabll     uf a e           k disk    rename(pbl01:pbl00)

      *** Beg Add ***** 04/01/13 ******************************************
     d dbfds           ds
     d  rrnWdell             397    400b 0
      *** End Add ***** 04/01/13 ******************************************

     D*entry
     D W8718R          pr
     D  @@Co#                         3
     D  @@Fnd                         3
     D  @@NRB                         1
     D  @@FrDt                        8
     D  @@ToDt                        8
     D  @@State                       2
     D  @@UnWtr                       3
     D  @@Agt#                        5
     D  @@FrPrem                     11
     D  @@ToPrem                     11
      ***** Beg Add ***** 12/09/19 ***********************************
     D  @@RptDiv                      1
      ***** End Add ***** 12/09/19 ***********************************
     D W8718R          pi
     D  @@Co#                         3
     D  @@Fnd                         3
     D  @@NRB                         1
     D  @@FrDt                        8
     D  @@ToDt                        8
     D  @@State                       2
     D  @@UnWtr                       3
     D  @@Agt#                        5
     D  @@FrPrem                     11
     D  @@ToPrem                     11
      ***** Beg Add ***** 12/09/19 ***********************************
     D  @@RptDiv                      1
      ***** End Add ***** 12/09/19 ***********************************

     D ChkAgt#         pr                  like(*in90)
     D ChkDates        pr                  like(*in90)
     D ChkNRB          pr                  like(*in90)
     D ChkPremium      pr                  like(*in90)
     D ChkState        pr                  like(*in90)
     D ChkUnWtr        pr                  like(*in90)
     D ChkW0050R17     pr                  like(*in90)
     D EmpMeetsCrit    pr                  like(*in90)
     D WriteRecords    pr
     D WriteWAQOP      pr
     D WriteWABLP      pr

     D/copy sPrDate
     D/copy wPrAgent
     D/copy wPrEmplr
     D/copy wPrRateSet

     D PSDS          ESDS                  EXTNAME(SZQ1P)

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D xxCo#                               like(EMCO#)
     D xxFnd                               like(EMFND)
     D xxNRB                          1
     D xxFrDt                         8  0
     D xxToDt                         8  0
     D xxState                             like(EMMST)
     D xxUnWtr                             like(EMUWTR)
     D xxAgt#                              like(AHAGT#)
     D xxFrPrem                            like(ELANPR)
     D xxToPrem                            like(ELANPR)
     D svEmp#                              like(EMEMP#)
     D svDiv                               like(EMDIV)
      ***** Beg Add ***** 12/09/19 ***********************************
     D xxRptDiv                       1
     DisTransferIssue  s               n   inz(*off)
     DisDivisionPass   s               n   inz(*off)
     DdecPrem          s             15  2 inz
      ***** End Add ***** 12/09/19 ***********************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C/EJECT
     C*____________________________________________________________________
     C*
     C     keyWMEXL      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C*
     C     keyWMEML      KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    xxFnd

     C     key1WMEML     KLIST
     C                   KFLD                    xxCo#
     C                   KFLD                    xxFnd
     C                   KFLD                    EMEMP#

     C     keyWDELL      klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
     C*_______________________________________________________________
     C*
     C                   exsr      LoadInit

     C     keyWMEML      setll     wmeml

     C                   dou       %eof(wmeml)

     C     keyWMEML      reade     wmeml

     C                   if        not %eof(wmeml)
     C                              and EmpMeetsCrit
     C                   callp     WriteRecords
     C                   endif

     C                   enddo

     C                   eval      *inlr = *on
     C*_______________________________________________________________
     C*
    C*====================================================================
    C* Load initial values
    C*====================================================================
     C*
     C     LoadInit      begsr
     C*
     C                   move      @@Co#         xxCo#
     C                   move      @@Fnd         xxFnd
     C                   move      @@NRB         xxNRB
     C                   move      @@FrDt        xxFrDt
     C                   move      @@ToDt        xxToDt
     C                   move      @@State       xxState
     C                   move      @@UnWtr       xxUnWtr
     C                   move      @@Agt#        xxAgt#
     C                   move      @@FrPrem      xxFrPrem
     C                   move      @@ToPrem      xxToPrem
      ***** Beg Add ***** 12/09/19 ***********************************
     C                   If        q1prms >= 10
     C                   If        @@RptDiv <> ' '
     C                   Eval      xxRptDiv = @@RptDiv
     C                   Else
     C                   Eval      xxRptDiv ='Y'
     C                   Endif
     C                   Else
     C                   Eval      xxRptDiv ='Y'
     C                   Endif
      ***** End Add ***** 12/09/19 ***********************************
     C*
     C                   clear                   EXRAFL
     C     keyWMEXL      chain     pex00
     C*
     C                   endsr
     C*_______________________________________________________________
     C*





    C*====================================================================
    C* ChkAgt# - Check the employer against the agent selection criteria
    C*====================================================================
     PChkAgt#          b

     D ChkAgt#         pi                  like(*in90)

     C                   select
     C                   when      xxAgt# = *zero
     C                   return    *on
     C                   when      xxAgt# = GetEmpAgt(EMCO#:EMFND:
     C                                                EMEMP#:EMDIV)
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl

     PChkAgt#          e





    C*====================================================================
    C* ChkDates - Check the employer against the date selection criteria
    C*====================================================================
     PChkDates         b

     D ChkDates        pi                  like(*in90)

     C                   select
     C                   when      xxToDt = *zero
     C                   return    *on
     C                   when      not %found(wdell)
     C                   return    *off
     C                   when      ELPFDT >= DtCYMD(xxFrDt)
     C                              and ELPFDT <= DtCYMD(xxToDt)
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl

     PChkDates         e





    C*====================================================================
    C* ChkNRB - Check the employer against the NRB selection criteria
    C*====================================================================
     PChkNRB           b

     D ChkNRB          pi                  like(*in90)

     C                   select
     C                   when      xxNRB = 'B'
     C                   return    *on
     C                   when      xxNRB = 'R' and EMRQFL = 'Y'
     C                   return    *on
     C                   when      xxNRB = 'N' and EMRQFL <> 'Y'
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl

     PChkNRB           e





    C*====================================================================
    C* ChkPremium - Check the emp against the premium selection criteria
    C*====================================================================
     PChkPremium       b

     D ChkPremium      pi                  like(*in90)

     C                   select
     C                   when      xxToPrem = *zero
     C                   return    *on
     C                   when      not %found(wdell)
     C                   return    *off
      ***** Beg Chg ***** 12/09/19 ***********************************
     C*****              when      ELANPR >= xxFrPrem and ELANPR <= xxToPrem
     C                   When      decPrem>= xxFrPrem and decPrem<= xxToPrem
      ***** End Chg ***** 12/09/19 ***********************************
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl

     PChkPremium       e





    C*====================================================================
    C* ChkState - Check the employer against the state selection criteria
    C*====================================================================
     PChkState         b

     D ChkState        pi                  like(*in90)

     C                   select
     C                   when      xxState = *blank
     C                   return    *on
     C                   when      not %found(wdell)
     C                   return    *off
      ***** Beg add ***** 12/09/19 ***********************************
     C                   When      xxRptDiv ='Y' and EMstRp <> 'Y'
     C                   Return    *off
      ***** End add ***** 12/09/19 ***********************************
      ***** Beg Chg ***** 12/09/19 ***********************************
     C*****              when      xxState = RatingSt(ELCO#:
     C*****                                    (GetRateSet(
     C*****                                      GetEmpYrRR(ELFYR:ELEMP#:
     C*****                                        GetStRptDv(ELCO#:ELFND:
     C*****                                                   ELEMP#):
     C*****                                        ELFND:ELCO#))))
     C                   When      xxState = RatingSt(ELCO#:
     C                                         (GetRateSet(
     C                                          GetEmpYrRR(ELFYR:ELEMP#:
     C                                                     ELdiv:ELFND:ELCO#))))
      ***** End Chg ***** 12/09/19 ***********************************
     C
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl

     PChkState         e





    C*====================================================================
    C* ChkUnWtr - Check the emp against the underwriter selection criteria
    C*====================================================================
     PChkUnWtr         b

     D ChkUnWtr        pi                  like(*in90)

     C                   select
     C                   when      xxUnWtr = *blank
     C                   return    *on
     C                   when      xxUnWtr = EMUWTR
     C                   return    *on
     C                   other
     C                   return    *off
     C                   endsl

     PChkUnWtr         e





    C*====================================================================
    C* ChkW0050R17 - Check emp against edits in W0050R17
    C*====================================================================
     PChkW0050R17      b

     D ChkW0050R17     pi                  like(*in90)

      *** Beg Chg ***** 04/01/13 ******************************************
     D***** W0050R17        pr                  extPgm('W0050R17')
     D*****  @@mode                        1    const
     D*****  @@Co#                              const like(EMCO#)
     D*****  @@Fnd                              const like(EMFND)
     D*****  @@Emp#                             const like(EMEMP#)
     D*****  @@Div                              const like(EMDIV)
     D*****  @@Error                       1
     d QuoteTranfer    pr                  extPgm('WTPOLQTFR')
     d  inRRN                         9    const
     d  inMode                        1    const
     d  inNextPgm                    10    const
     d  outError                      1
      *** End Chg ***** 04/01/13 ******************************************

     d  error          s              1
      *** Beg Add ***** 04/01/13 ******************************************
     d  inRRN          s              9
     d  inMode         s              1
     d  inNextPgm      s             10
     d  outError       s              1
      *** End Add ***** 04/01/13 ******************************************

      *** Beg Chg ***** 04/01/13 ******************************************
     C*****              callp     W0050R17('M':EMCO#:EMFND:EMEMP#:EMDIV:
     C*****                                 error)
      /Free
       inRRN = %EditC(rrnWdell:'X');
       inMode= 'M';
       inNextPgm = 'W8718R';
       QuoteTranfer(inRRN:inMode:inNextPgm:outError);
       error = outError;
      /End-Free
      *** End Chg ***** 04/01/13 ******************************************

     C                   select
     C                   when      error = '2'
     C                   return    *off
     C                   when      error = '0'
     C                   return    *off
     C                   other
     C                   return    *on
     C                   endsl

     PChkW0050R17      e





    C*====================================================================
    C* EmpMeetsCrit - Check the employer to meet selection criteria
    C*====================================================================
     PEmpMeetsCrit     b

     D EmpMeetsCrit    pi                  like(*in90)

      ***** Beg Chg ***** 12/09/19 ***********************************
     C*****keyWDELL      chain     wdell

     C*****              select
     C*****              when          ChkAgt#
     C*****                        and ChkDates
     C*****                        and ChkNRB
     C*****                        and ChkPremium
     C*****                        and ChkState
     C*****                        and ChkUnWtr
     C*****                        and chkW0050R17
     C*****              return    *on
     C*****              other
     C*****              return    *off
     C*****              endsl
      /Free
       isTransferIssue = *on;
       isDivisionPass = *off;

       Setll (EMco#:EMfnd:EMemp#) pel00 ;
       Reade (EMco#:EMfnd:EMemp#) pel00 ;
       Dow not %eof(wdell);
          If %found(wdell);
             isTransferIssue = *on;
             isDivisionPass = *off;
             Clear decPrem;
             If ChkAgt# and ChkDates and ChkNRB and
              ChkUnWtr;
                If ChkState;
                   isDivisionPass = *on ;
                   decPrem += ELanPr;
                Endif;
             Endif;
             If ChkPremium and isDivisionPass;
                If chkW0050R17;
                   isTransferIssue = *off;
                Endif;
             Endif;
          Endif;
          Reade (EMco#:EMfnd:EMemp#) pel00 ;
       Enddo;

       Setgt (EMco#:EMfnd:EMemp#) wmeml;

       If not isTransferIssue;
          Return *on ;
       Else ;
         Return *off;
       Endif;

      /end-free
      ***** End Chg ***** 12/09/19 ***********************************

     PEmpMeetsCrit     e


    C*====================================================================
    C* WriteRecords - Write employer records for all Divsions
    C*====================================================================
     PWriteRecords     b

     D WriteRecords    pi

     C     key1WMEML     chain     wmeml

     C                   dow       not %eof(WMEML)

     C     keyWDELL      chain     wdell

     C                   callp     WriteWAQOP
     C                   callp     WriteWABLP

     C     key1WMEML     reade     WMEML

     C                   enddo

     C     key1WMEML     setgt     wmeml
     C                   readp     wmeml

     PWriteRecords     e





    C*====================================================================
    C* WriteWABLP - Write employer to Bill Transaction File
    C*====================================================================
     PWriteWABLP       b

     D WriteWABLP      pi

     C     keyWABLL      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELFYR
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV

     C     keyWMAHL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR

     C     keyWABLL      chain     pbl00

     C                   eval      BLPFDT = ELPFDT
     C*
     C                   select
     C                   when      ELRQFL = 'Y'
     C                   eval      BLRFLG = 'R'
     C                   other
     C                   eval      BLRFLG = 'N'
     C                   endsl

     C                   eval      BLAGT# = *zero

     C     keyWMAHL      setgt     pah00
     C                   dou       %eof or AHSTAT = 'A'
     C     keyWMAHL      readpe    pah00
     C                   enddo                                                  *IN50 == 0

     C                   if        not %eof
     C                   eval      BLAGT# = AHAGT#
     C                   endif                                                  *IN50 == 0

     C                   select
     C                   when      %found(WABLL)
     C                   eval      BLFLG1 = 'Y'
     C                   eval      BLFLG3 = 'Y'
     C                   update    pbl00
     C                   when      not %found(WABLL)
     C                   eval      BLCO#  =  ELCO#
     C                   eval      BLFND  =  ELFND
     C                   eval      BLFYR  =  ELFYR
     C                   eval      BLEMP# =  ELEMP#
     C                   eval      BLDIV  =  ELDIV
     C                   eval      BLFLG1 =  'Y'
     C                   eval      BLFLG2 = *BLANKS
     C                   eval      BLFLG3 =  'Y'
     C                   eval      BLSTAT = *BLANKS
     C                   eval      BLUSER = q1user
     C                   eval      BLPGM  = q1pgm
     C                   eval      BLDT   = DateTo7(SystemDate)
     C                   eval      BLTIME = TimeTo6(SystemTime)
     C                   write     pbl00
     C                   endsl                                                  *IN60 IFEQ

     PWriteWABLP       e





    C*====================================================================
    C* WriteWAQOP - Write employer to Quote Transfer File
    C*====================================================================
     PWriteWAQOP       b

     D WriteWAQOP      pi

     C                   eval      QOCO#  = EMCO#
     C                   eval      QOFND  = EMFND
     C                   eval      QOEMP# = EMEMP#
     C                   eval      QODIV  = EMDIV
     C                   eval      QOUSER = q1user
     C                   eval      QOPGM  = q1pgm
     C                   eval      QODT   = DateTo7(SystemDate)
     C                   eval      QOTIME = TimeTo6(SystemTime)

     C                   eval      QORAFL = EXRAFL

     C                   eval      QOTEMP = *zeros
      ***** Beg Chg ***** 03/27/24 *** PHW114663 *********************
     C*                  if        EMRQFL = 'Y'
     C                   if        EMRqfl = 'Y' Or EMRdfl = 'Y'
      ***** End Chg ***** 03/27/24 *** PHW114663 *********************
     C                   eval      QOTEMP = QOEMP#
     C                   endif

     C                   write     pqo0p

     PWriteWAQOP       e
