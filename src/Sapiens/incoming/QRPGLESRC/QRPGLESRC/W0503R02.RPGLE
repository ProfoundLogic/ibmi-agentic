     H Copyright('(c) 2022 Sapiens. All Rights Reserved.')
      /TITLE WW503R02 - Return Fund Check Request Info
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *================================================================
      * Description:  Work with return fund check requests            *
      *                                                               *
      * Called by: W0503R                                             *
      *                                                               *
      * Calls to:  None                                               *
      *                                                               *
      * Created: 03/12/03  C:142/C:136   Project: WC1033              *
      *================================================================
      * Revisions:                                                    *
      *                                                               *
      *  10/13/22 C:291 Project: 129WXD                               *
      *                 Print Federal Name on Refund checks if it     *
      *                 exist and the checkbox is checked at company  *
      *                 level.                                        *
      *                                                               *
      * 04/11/11 C:180 SUPPORT 76457
      *                Donot allow changing payee to Agent if Agent
      *                is not found.
      *
      *                Reorganized ERRCK1.
      *
      *                Populate agent # if needed when changing
      *                payee to agent.
      *
      *                Populate paid-to address info if needed when
      *                changing payee.
      *
      *
      * 12/26/07 C:183 PROJECT: 81WZB                                 *
      *                Implemented Customer ID and Surplus Dividend   *
      *                logic.                                         *
      *                                                               *
      * 07/18/07 C:136 PROJECT: WC1491                                *
      *                IMPLEMENTATION OF IND CASH AND RETURN DEPOSIT  *
      *                                                               *
      *  03/07/05 C:65  Project: WC1202                               *
      *                 Store extended employer name field for checks.*
      *                                                               *
      *  11/05/04 C:33  Project: WC1033                               *
      *                 Include calculations for Agency Billing.      *
      *================================================================
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0503f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwar4p     uf a e             disk
     F                                     rename(pr401:dbrcdf)
     *
     Fwtabl     if   e           k disk
     F                                     ignore(prt01)
     *
     Fwmfnl     if   e           k disk
     *
     Fwdell     if   e           k disk
     Fwmpfl     if   e           k disk
     Fwmeml     if   e           k disk
     Fwmagl     if   e           k disk
     Fwmaal1    if   e           k disk
     Fwdabl     if   e           k disk    rename(pab01:pab00)
      *** Beg Add ***** 10/13/22 ******************************************
     Fsmcol     if   e           k disk    Rename(pco01:pco00)
      *** End Add ***** 10/13/22 ******************************************
     *================================================================
     *  /COPY required constants                                     *
     *================================================================
     D optnm           C                   'Return Fund Check Request-
     D                                      Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     *================================================================
     * Stand Alone Fields - TOP                                      *
     *================================================================
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@mode2         s              1
     D @@rrn           s              9  0
     D ##lvl           s              3  0
     D ##type          s              1
     D ##outq          s             10
     D fraud           S               D   Datfmt(*Iso)
     D toaud           S               D   Datfmt(*Iso)
     D kkfnd           S              3  0
     D svptfl          S              1
     D xxagt#          s                   like(ABAGT#)
     D x5stdt          s                   like(ABSTDT)
     D zip4            S              4
     D zip5            S              5
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D @SCCUST         S                   LIKE(SCCUST)
     D @EMCUST         S                   LIKE(EMCUST)
     D agencyBill      s               n
     D*
     D/copy sprdate
     D/copy sprstring
     D/copy wpragent
     d/copy scopymt41c
     *
     *================================================================
     *  PLIST definitions                                            *
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##lvl
     C                   parm                    ##type
     C                   parm                    ##outq
      *
     *================================================================
     *  klist definitions                                            *
     *================================================================
      *
     C     aaky          klist
     C                   kfld                    ##co#
     C                   kfld                    r4fnd
     C                   kfld                    scagt#
      *
     C     agky          klist
     C                   kfld                    aaco#
     C                   kfld                    aafein
     C                   kfld                    aamod
     C*
     C     keyWMAGL      klist
     C                   kfld                    R4CO#
     C                   kfld                    R4FEIN
     C                   kfld                    R4MOD
      *
     C     elky          klist
     C                   kfld                    ##co#
     C                   kfld                    r4fnd
     C                   kfld                    r4emp#
     C                   kfld                    r4div
    C                   kfld                    r4fyr
     *
     C     emky          klist
     C                   kfld                    ##co#
     C                   kfld                    r4fnd
     C                   kfld                    r4emp#
     C                   kfld                    r4div
      *
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    r4fnd
      *
     C     pfky          klist
     C                   kfld                    ##co#
     C                   kfld                    r4fnd
     C                   kfld                    scpfin
     C*
     C     keyWDABL      klist
     C                   kfld                    R4CO#
     C                   kfld                    x5stdt
     C                   kfld                    xxagt#
      *
     C     tbky          klist
     C                   kfld                    ##co#
     C                   kfld                    key              10
     *================================================================
     * DLTPRE - Pre-processing before delete occurs                  *
     *================================================================
     C     dltpre        begsr
     C                   endsr
     *================================================================
     * DSPLYR - Processing after displaying information              *
     *================================================================
     C     dsplyr        begsr
     C                   endsr
     ***==============================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)                  *
     *================================================================
     C     errck1        begsr
      *
     C                   IF        SCCUST <> *BLANKS
     C     EMKY          CHAIN     WMEML
     C                   IF        %FOUND(WMEML)
     C                   EVAL      @SCCUST = %XLATE(LO:UP:SCCUST)
     C                   EVAL      @EMCUST = %XLATE(LO:UP:EMCUST)
     C                   IF        @EMCUST <> @SCCUST
     C                   SETON                                        35  90
     C                   ENDIF                                                  @EMCUST <> @SCCUST
     C                   ENDIF                                                  %FOUND(WMEML)
     C                   ENDIF                                                  SCCUST <> *BLANKS

     C                   if           scptfl <> 'A' and
     C                                scptfl <> 'E' and
     C                                scptfl <> 'G' and
     C                                scptfl <> 'P'
     C                                and scacfl <> 'Y'
     C                   seton                                        71  90
     C                   endif

     C                   if           scptfl <> svptfl

     C                   select
     C                   when         scptfl = 'P'

     C     elky          chain     pel01
     C                   eval      scpfin = elpfin
     C                   if        %found

     C     pfky          chain     ppf01
     C                   if        %found
     C                   eval      scpfnm = pfname
     C                   eval      scptnm = pfname
     c                   eval      scptn2 = *blanks
     C                   eval      r4pad1 = pfadr1
     C                   eval      r4pad2 = pfadr2
     C                   eval      r4pcty = pfcty
     C                   eval      r4pst  = pfst
     C                   move      pfzip         r4pzip
     C                   else
     C                   seton                                        72  90
     C                   endif                                                  wmpfl %found
     C                   endif                                                  wdell %found

     C                   when         scptfl = 'E'
     C     emky          chain     pem01
     C                   clear                   scpfnm
     C                   clear                   scpfin
      //*** Beg Add ***** 10/13/22 ***********************************
      /Free
        If COfedrc = 'Y' and EMfdnm <> *Blank;
           R4PtNm = EMfdnm;
           R4PtN2 = EMfdn2;
        Else;
      /End-Free
      //*** End Add ***** 10/13/22 ***********************************
     c                   movel(p)  emname        r4ptnm
     c                   move      emnam2        xxwk10           10
     c                   movel(p)  xxwk10        r4ptn2
     c                   move      emnam2        r4ptn2
      //*** Beg Add ***** 10/13/22 ***********************************
      /Free
        Endif;
      /End-Free
      //*** End Add ***** 10/13/22 ***********************************

     C                   if        emmpfl = 'M'
     C                   eval      r4pad1 = emmad1
     C                   eval      r4pad2 = emmad2
     C                   eval      r4pcty = emmcty
     C                   eval      r4pst  = emmst
     C                   move      emmzip        r4pzip
     C                   else
     C                   eval      r4pad1 = empad1
     C                   eval      r4pad2 = empad2
     C                   eval      r4pcty = empcty
     C                   eval      r4pst  = empst
     C                   move      empzip        r4pzip
     C                   endif
     c                   move      scptfl        svptfl

     C                   when      scptfl = 'A'

     c                   if        scagt# = *ZERO and *IN98 = *Off
     C                   call      'WA002R'
     C                   parm      ##co#         ##co#             3 0
     C                   parm      scfnd         ##fnd             3 0
     C                   parm      ##emp#        ##emp             9 0
     C                   parm                    ##div             5 0
     C                   parm      scfyr         ##fyr             3 0
     C                   parm      *zeros        ##agt             5 0
     C                   parm      *blanks       ##anam           60
     C                   parm      *zeros        ##prod            5 0
     C                   parm      *blanks       ##pnam           60
     C                   parm      *zeros        ##area            3 0
     C                   parm      *zeros        ##phon            7 0
     C                   parm      '*'           ##plan            1
     C                   parm      *zeros        ##date            8 0
     C                   parm      *zeros        ##nwrt            5 0
     C                   parm      *zeros        ##rnrt            5 0
      *
     c                   if        ##plan = 'A'
     C                   seton                                        70  90    *in79
     c                   else
     c                   move      ##agt         scagt#
     c                   endif

     C                   endif
      *
     c                   if        scagt# >  *zero  and *IN90 = *OFF
      *
     C     aaky          chain     paa01

     c                   if        %found
     C     agky          chain     pag01

     C                   if        %found
     C                   eval      r4ptnm = agname
     c                   eval      r4ptn2 = *blanks
     C                   eval      r4pad1 = agmad1
     C                   eval      r4pad2 = *Blanks
     C                   eval      r4pcty = agmcty
     C                   eval      r4pst  = agmst
     C                   move      agmzip        r4pzip
     c                   move      scptfl        svptfl
     c                   else
     c                   seton                                        70  90
     C                   endif

     c                   else
     c                   seton                                        70  90
     C                   endif

     c                   else
     c                   seton                                        70  90
     C                   endif
      *
     C                   when      scptfl = 'G'
     C     fnky          chain     pfn01
     C                   if        %found
     C                   eval      r4ptnm = fnname
     c                   eval      r4ptn2 = *blanks
     C                   eval      r4pad1 = fnmad1
     C                   eval      r4pad2 = fnmad2
     C                   eval      r4pcty = fnmcty
     C                   eval      r4pst  = fnmst
     C                   move      fnmzip        r4pzip
     c                   move      scptfl        svptfl
     C                   endif
     C                   endsl

     C                   MOVE      R4PCTY        SCPCTY
     C                   MOVE      R4PST         SCPST
     C                   MOVE      R4PZIP        SCPZIP
     C                   move      r4pzip        zip4
     C                   movel     r4pzip        zip5
     c                   movel(p)  r4ptnm        scptnm
     c                   movel     r4ptn2        xxwk10           10
     c                   move      xxwk10        scptnm
     c                   move(p)   r4ptn2        scptn2
     C                   eval      scadl1 = r4pad1
     C                   eval      scadl2 = r4pad2
     C                   eval      sccstz = %trim(r4pcty) + ', ' +
     C                                            r4pst   + '  ' +
     C                                            zip5 + '-' + zip4
     c                   move      *on           *in90
     C                   endif                                                  svptfl <> scptfl
     C*
     C                   if        scacfl = 'Y'
     c                             and scbtyp = 'A'
     C                   eval      scstdt = dt6Check(scstdt)

     C                   if        scstdt = *zero
     C                   seton                                        73  90
     C                   endif
     C*
     C                   eval      xxagt# = GetAgent#(R4CO#:R4FEIN:R4MOD)
     C                   eval      x5stdt = dt5CYMD(scstdt)
     C     keyWDABL      chain     pab00

     C                   if        not %found
     C                   seton                                        74  90
     C                   endif
     C*
     C                   endif

     C                   endsr
     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)                  *
     *================================================================
     C     errck2        begsr
     C                   endsr
     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)                  *
     *================================================================
     C     errck3        begsr
     C                   endsr
     C*
    C*====================================================================
    C* MaintFlds - Load maintenance fields
    C*====================================================================
     C     MaintFlds     begsr
     C*
     C                   eval      R4DATE = DateTo7(SystemDate)
     C                   eval      R4TIME = TimeTo6(SystemTime)
     C                   eval      R4USER = q1user
     C                   eval      R4PGM  = q1pgm
     C*
     C                   endsr
     C*
     *================================================================
     * MVDBSC - Move data base fields to screen fields               *
     *================================================================
     C     mvdbsc        begsr
      *
     C                   eval      scfein = R4FEIN
     C                   eval      scmod  = R4MOD
     C                   clear                   AGNAME
     C     keyWMAGL      chain     pag01
     C                   eval      scAgtN = AGNAME
     C                   eval      scarcd = R4ARCD
     C                   eval      scptfl = r4ptfl
     C                   eval      svptfl = r4ptfl
     C                   eval      scpfin = r4pfin
     C     pfky          chain     ppf01
     C                   move      r4pzip        zip4
     C                   movel     r4pzip        zip5
     C                   eval      scpfnm = pfname
     c                   movel(p)  r4ptnm        scptnm
     c                   movel     r4ptn2        xxwk10           10
     c                   move      xxwk10        scptnm
     c                   move(p)   r4ptn2        scptn2
     C                   eval      scadl1 = r4pad1
     C                   eval      scadl2 = r4pad2
     C                   eval      sccstz = %trim(r4pcty) + ', ' +
     C                                            r4pst   + '  ' +
     C                                            zip5 + '-' + zip4
     C                   eval      sccom1 = r4com1
     C                   eval      sccom2 = r4com2
     C                   eval      sccom3 = r4com3
     C                   eval      scacfl = R4ACFL
     C                   eval      scstdt = dt6MDYY(R4STDT)
     C                   EVAL      SCCUST = R4CUST
     C                   EVAL      SCAGT# = R4AGT#
     c                   EVAL      SCPCTY = R4PCTY
     c                   EVAL      SCPST  = R4PST
     c                   EVAL      SCPZIP = R4PZIP
     C                   endsr
     *================================================================
     * MVSCDB - Move screen fields to data base fields               *
     *================================================================
     C     mvscdb        begsr
      *
     C                   eval      r4ptfl = scptfl
     C                   eval      r4pfin = scpfin
     c                   movel(p)  scptnm        r4ptnm
     c                   move      scptnm        xxwk10           10
     c                   movel(p)  xxwk10        r4ptn2
     c                   move      scptn2        r4ptn2
     C                   eval      r4com1 = sccom1
     C                   eval      r4com2 = sccom2
     C                   eval      r4com3 = sccom3
     C                   eval      R4STDT = dt5CYMD(scstdt)
     C                   EVAL      R4CUST = SCCUST
     C                   EVAL      R4AGT# = SCAGT#
     C                   EVAL      R4PAD1 = SCADL1
     C                   EVAL      R4PAD2 = SCADL2
     c                   EVAL      R4PCTY = SCPCTY
     c                   EVAL      R4PST  = SCPST
     c                   EVAL      R4PZIP = SCPZIP
      *
     C                   endsr
     *================================================================
     * OTHERR - Processing for other modes                           *
     *================================================================
     C     otherr        begsr
     C                   endsr
     *================================================================
     * OTHFKS - Processing for other functions keys                  *
     *================================================================
     C     othfks        begsr
     C                   endsr
     *================================================================
     * PGMEND - End of program processing                            *
     *================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
     *================================================================
     * PGMSTR - Start of program processing                          *
     *================================================================
     C     pgmstr        begsr
      *
     C     @@rrn         chain(n)  dbrcdf
     C                   eval      scfnd  = r4fnd
     C                   eval      scchk# = r4chk#
     C                   eval      scamt  = r4amt
     C                   eval      scfyr  = r4fyr
     C*
     C                   eval      agencyBill = R4ACFL = 'Y'
     C                   eval      *in98 = agencyBill
      *
     C     fnky          chain     pfn01
     C                   if        %found
     C                   eval      scfndn = fnname
     C                   endif
      /Free
      //*** Beg Add ***** 10/13/22 ********************************
       Chain (##Co#) pco00;
      //*** End Add ***** 10/13/22 ********************************
      /End-Free
      *
     C     emky          chain     pem01
     C                   if        %found
     C                   call      'WA005R'
     C                   parm      r4emp#        ##emp#            9 0
     C                   parm      r4div         ##div             5 0
     C                   parm      emname        ##emnm           40
     C     scedn         parm                    ##edn            80
     C                   endif
      *
     C                   if        @@mode = 'S'
     C                   exsr      chgsts
     C                   seton                                        lr
     C                   return
     C                   endif
      *

     c                   eval      scbtyp = R4BTYP
     c                   select
     c                   when      scbtyp = 'A'
     c                   eval      scbdsc = 'Account Current'
     c                   when      scbtyp = 'D'
     c                   eval      scbdsc = 'Deposit'
     c                   when      scbtyp = 'I'
     c                   eval      scbdsc = 'Independent Cash Distribution'
     c                   eval      scptfl = R4PTFL
     c                   eval      *in44 = *on
     c                   when      scbtyp = 'P'
     c                   eval      scbdsc = 'Premium'
     c                   endsl
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   EVAL      SCSDFL = R4SDFL
      *
      * Reflect the new changes only if the Batch type <> 'A' and it is payable to Employer
      *
     C                   IF        CLTID = '081' AND SCBTYP = 'I'
     C                             AND R4PTFL = 'E'
     C                   EVAL      *IN31 = *ON
     C                   ENDIF                                                  CLTID EQ '081'
      *
     C                   IF        SCBTYP <> 'A'
     C                   EVAL      *IN97 = *ON
     C                   ENDIF                                                  SCBTYP <> 'A'
      *
      * Disable record editing if surplus dividend flag = 'Y'
      *
     C                   IF        SCSDFL = 'Y'
     C                   EVAL      *IN09 = *ON
     C                   ENDIF                                                  SCSDFL = 'Y'

     C                   endsr
     *================================================================
     * PREOTH - Pre-processing for other modes                       *
     *================================================================
     C     preoth        begsr
     C                   endsr
     *================================================================
     * CHGSTS - UPDATE War4p WITH NEW STATUS FLAG                    *
     *================================================================
     C     chgsts        begsr
      *
     C     @@rrn         chain     dbrcdf
      *
     C                   if        r4selc = 'Y'
     C                   eval      r4selc = 'N'
     C                   else
     C                   eval      r4selc = 'Y'
     C                   endif
      *
     C                   exsr      MaintFlds
     C                   update    dbrcdf
      *
     C                   endsr
     *================================================================
     * UPDATR - Update data base record and after processing         *
     *================================================================
     C     updatr        begsr
      *
     C     @@rrn         chain     dbrcdf
     C                   if        %found
     C                   exsr      mvscdb
     C                   exsr      MaintFlds
     C                   update    dbrcdf
     C                   endif

     C                   endsr
     *================================================================
     * WRITER - Write data base record and after processing          *
     *================================================================
     C     writer        begsr
     C                   endsr
