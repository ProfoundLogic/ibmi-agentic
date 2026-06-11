     *  COPYRIGHT((C) Copyright Fiserv, Inc., 2003 - 2008)
      *****************************************************************
      *  DESCRIPTION:  Work with return premium batches.              *
      *                                                               *
      * CALLED BY: W0505R                                             *
      *                                                               *
      * CALLS TO:  NONE                                               *
      *                                                               *
      * Created: 03/12/03  C:136/ Project WC1033                      *
      *****************************************************************
      * Revisions:                                                    *
      *                                                               *
      *  04/14/08 C:136 Support: WA081 63061                          *
      *               - Populate Batch Type when regenerating         *
      *                 the check request                             *
      *               - Do not edit for A/R code when voiding check   *
      *                 if check is Independent Cash.                 *
      *                                                               *
      *  12/26/07 C:183 Project: 81WZB                                *
      *                 Added Surplus Dividend flag and Customer ID   *
      *                                                               *
      *  07/18/07 C:136 Project: WC1491                               *
      *                 Implement Ind Cash Dist and Return Deposit    *
      *                                                               *
      *  05/24/05  C:33  Project: WC1033                              *
      *                  Include calculations for Agency Billing.     *                            *
      *****************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0505f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwar4p     uf a e             disk
     F                                     rename(pr401:dbrcdf)
     *
     Fwtabl     if   e           k disk    ignore(prt01)
     *
     Fwmfnl     if   e           k disk
     *
     Fwmeml     if   e           k disk
     *
     Fwxrnl     if a e           k disk
     *
     Fwsckl     if a e           k disk
     *
     Fwxrtp     o    e           k disk
     *
     Fwmeap     o    e           k disk
    F***  Begin Add  ***  05/24/05  *************************************
     Fwdabp     o    e             disk    rename(pab01:pab0p)
     Fwdabl     if   e           k disk    rename(pab01:pab00)
     Fwsasl     uf   e           k disk    rename(pas01:pas00)
    F***  End   Add  ***  05/24/05  *************************************
     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           c                   'Return Fund Checks'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##emp           s              9  0
     D ##div           s              5  0
     D ##name          s             40
     D ##edn           s             80
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkkey           s                   like(ttkey)
     D kkbat#          s                   like(rnbat#)
    D***  Begin Add  ***  05/24/05  *************************************
     D xxbill          s                   like(ASBILL)
     D xxpaid          s                   like(ASPAID)
     D sysTime6        s                   like(ASTIME)
     D sysDt7          s                   like(ASDT)
    D***  BEG ADD ******  12/26/07  *************************************
     D agencyBill      s               n
    D***  END ADD ******  12/26/07  *************************************

     D/copy sprString
     D/copy sprDate
     D/copy wprAgent
    D***  End   Add  ***  05/24/05  *************************************
     *
    D***  Begin Chg  ***  05/24/05  *************************************
     C**********/COPY SCOPYMT01C
     C/COPY SCOPYMT41C
    D***  End   Chg  ***  05/24/05  *************************************
     *
     *================================================================
     *  PLIST definitions
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
     *================================================================
     *  KLIST definitions
     *================================================================
     c     emky          klist
     c                   kfld                    r4co#
     c                   kfld                    r4fnd
     c                   kfld                    r4emp#
     c                   kfld                    r4div
      *
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    r4fnd
      *
     C     tbky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkkey
      *
     C     rnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkbat#
      *
     C     ckky          klist
     C                   kfld                    ##co#
     C                   kfld                    r4acct
     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr
     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr
     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr
     C                   endsr
     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr
     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr
     *================================================================
     * genRequest - Regenerate check request subroutine
     *================================================================
     C     genRequest    begsr
      *
      * Get next batch # and write return premium batch control file
      *
     c                   eval      kkbat# = *hival
     c     rnky          setgt     prn01
     c     ##co#         readpe    prn01
     c                   if        %eof
     c                   eval      rnbat# = 1
     c                   else
     c                   add       1             rnbat#
     c                   endif
     c                   eval      rnco#  = ##co#
     c                   eval      rnfgrp = r4fnd
     c                   eval      rntgrp = r4fnd
     c                   clear                   rncom1
     c                   clear                   rncom2
     c                   clear                   rncom3
     c                   eval      rnfaud = r4ckdt
     c                   eval      rntaud = r4ckdt
     c                   clear                   rnmin
     c                   eval      rnbpf = 'Y'
     c                   time                    dsdttm

     c***  Begin Chg  ***  05/24/05  **************************************
     c**********         eval      srdti = dsdate
     c**********         call      'SRCVTDAT'    dtcymd
     c**********         movel     srdto         rnidt
     c                   eval      RNIDT  = DateTo7(SystemDate)
     c***  End   Chg  ***  05/24/05  **************************************
     c                   eval      rndate = rnidt
     c                   eval      rntime = dstime
     c                   eval      rnitim = dstime
     c                   eval      rnpgm  = q1pgm
     c                   eval      rnipgm = q1pgm
     c                   eval      rnuser = q1user
     c                   eval      rniusr = q1user
      *** beg add ***** 04/14/08 ******************************************
     c                   eval      rnptyp = r4ptfl
     c                   eval      rnmtyp = 'M'
     c                   select
     c                   when      r4acfl = 'Y'
     c                   eval      rnbtyp = 'A'
     c                   when      r4btyp <> *blanks
     c                   eval      rnbtyp = r4btyp
     c                   eval      rnptyp = r4ptfl
     c                   other
     c                   eval      rnbtyp = 'P'
     c                   endsl
      *** end add ***** 04/14/08 ******************************************
     c                   write     prn01

      * write to wxrtp (return premium batch detail)
     c                   clear                   prt01
     c                   eval      rtco#  = r4co#
     c                   eval      rtbat# = rnbat#
     c                   eval      rtfnd  = r4fnd
     c                   eval      rtemp# = r4emp#
     c                   eval      rtdiv  = r4div
     c     emky          chain     pem01
     c                   eval      rtempn = emname
     c***  Begin Add  ***  05/24/05  **************************************
     c                   eval      RTAGT# = R4AGT#
     c                   eval      RTAGTN = GetAgtName(R4AGT#)
     c                   eval      RTFEIN = R4FEIN
     c                   eval      RTMOD  = R4MOD
     c***  End   Add  ***  05/24/05  **************************************
     c                   eval      rtfyr  = r4fyr
     c                   eval      rtamt  = r4amt
     c                   eval      rtselc = 'Y'
     c***  Begin Add  ***  05/24/05  **************************************
     c                   eval      RTACFL = R4ACFL
     c***  End   Add  ***  05/24/05  **************************************
     c*** beg add ***** 04/14/08 ******************************************
     c                   eval      RTBTYP = RNBTYP
     c                   eval      RTARCD = R4ARCD
     c*** end add ***** 04/14/08 ******************************************
     c                   eval      rtcom1 = 'Regenerated check request'
     c                   eval      rtptfl = r4ptfl
     c                   eval      rtpfin = r4pfin
     c                   eval      rtidt  = rndate
     c                   eval      rtdate = rndate
     c                   eval      rttime = dstime
     c                   eval      rtitim = dstime
     c                   eval      rtpgm  = q1pgm
     c                   eval      rtipgm = q1pgm
     c                   eval      rtuser = q1user
     c                   eval      rtiusr = q1user
     c                   write     prt01

     C                   endsr
     *================================================================
     * KJDO   - Search routine
     *================================================================
     c     kjdo          begsr

     c                   if        hfld = 'WWARCD'

     c                   eval      qqkey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    qqkey            10

     c                   if        *inkl <> *on and qqkey <> *blanks
     c                   eval      wwarcd = qqkey
     c                   endif

     c                   endif

     C                   endsr
     *================================================================
     * MVDBSC - Move data base to screen
     *================================================================
     C     mvdbsc        begsr
      *
     c                   setoff                                       50
     c     fnky          chain     pfn01
     c     emky          chain     pem01
     c                   call      'WA005R'
     c                   parm      ememp#        ##emp
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c                   parm      *blanks       ##edn
      *
     C                   eval      scfnd   =  r4fnd
     c                   eval      scfndn  =  fnname
     c                   eval      scedn   =  ##edn
     C                   eval      scfyr   =  r4fyr
     C                   eval      scacct  =  r4acct
     C                   eval      scptnm  =  r4ptnm
     C                   eval      scchk#  =  r4chk#
     C                   eval      scamt   =  r4amt
     C                   eval      scptfl  =  r4ptfl
     C                   eval      scvdsp  =  r4vdsp

     C                   if        r4ptfl  = 'P'
     C                   eval      scpfin  =  r4pfin
     C                   seton                                        50
     C                   endif

     C                   eval      sccom1 = r4com1
     C                   eval      sccom2 = r4com2
     C                   eval      sccom3 = r4com3
     C***  BEG ADD ******  12/26/07  **************************************
     C                   EVAL      SCCUST = R4CUST
     C***  END ADD ******  12/26/07  **************************************

     C                   endsr
     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr
     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     C                   seton                                        lr
     C                   endsr
     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr

     C*** beg add ***** 07/18/07 ******************************************
     c     @@rrn         chain     dbrcdf
     c                   eval      scbtyp = R4BTYP
     c                   select
     c                   when      scbtyp = 'A'
     c                   eval      scbdsc = 'Account Current'
     c                   when      scbtyp = 'D'
     c                   eval      scbdsc = 'Deposit'
     c                   when      scbtyp = 'I'
     c                   eval      scbdsc = 'Independent Cash Distribution'
     c                   eval      scptfl = R4PTFL
     c                   when      scbtyp = 'P'
     c                   eval      scbdsc = 'Premium'
     c                   endsl
     C*** end add ***** 07/18/07 ******************************************
     C*** BEG ADD ***** 12/26/07 ******************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   EVAL      AGENCYBILL = R4ACFL = 'Y'
     C                   EVAL      *IN98 = AGENCYBILL
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
     C*** END ADD ***** 12/26/07 ******************************************
     C                   endsr
     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     c     @@rrn         chain     dbrcdf
     c     ckky          chain     pck01
     c                   eval      wwarcd = ckrpvs
     c     @@mode        caseq     'V'           voidStop
     c     @@mode        caseq     'S'           voidStop
     c                   endcs
     C                   endsr

     C***  Begin Add  ***  05/24/05  **************************************
    C*===================================================================
    C* updateWSASP - Update WSASP record
    C*===================================================================
    C*
     C     updateWSASP   begsr
    C*
     C     abky          klist
     C                   kfld                    abco#
     C                   kfld                    abstdt
     C                   kfld                    abagt#
    C*
     C     asky          klist
     C                   kfld                    abco#
     C                   kfld                    abstdt
     C                   kfld                    abagt#
    C*
    C*
     C                   clear                   xxbill
     C                   clear                   xxpaid
    C*
     C     abky          chain     pab00                              60
    C*
     C                   dow       *in60 = *off
    C*
     C                   select
     C                   when      abarty = 'A'
     C                             or abarty = 'B'
     C                             or abarty = 'D'
     C                             or abarty = 'F'
     C                             or abarty = 'H'
     C                             or abarty = 'N'
     C                   eval      xxbill = xxbill + abnet
     C                   when      abarty = 'P'
     C                             or abarty = 'I'
     C                             or abarty = 'Q'
     C                             or abarty = 'T'
     C                   eval      xxpaid = xxpaid + abamt
     C                   when      abarty = 'R'
     C                             or abarty = 'V'
     C                   eval      xxpaid = xxpaid - abamt
     C                   endsl
    C*
     C     abky          reade     pab00                                  60
    C*
     C                   enddo
    C*
     C     asky          chain     pas00                              60
    C*
     C                   if        *in60 = *off
    C*
     C                   eval      asbill = xxbill
     C                   eval      aspaid = xxpaid
     C                   eval      asbal = asbill - aspaid
    C*
     C                   eval      ASPGM  = q1pgm
     C                   eval      ASUSER = q1user
     C                   eval      ASDT   = sysDt7
     C                   eval      ASTIME = sysTime6
    C*
     C                   update    pas00
    C*
     C                   endif
    C*
     C                   endsr
     C***  End   Add  ***  05/24/05  **************************************

     *================================================================
      * Void/Stop Payment Subroutine
     *================================================================
     C     voidStop      begsr

     C***** Beg Add ****** 03/15/13 ***************************************
     C                   eval      wwgenchk = 'N'
     C***** End Add ****** 03/15/13 ***************************************
     c                   dou       *in90 = *off or *inkl = *on
     c                   exfmt     wdw01

     c     *inkj         caseq     *on           kjdo
     c                   endcs

     c                   exsr      wderrck
     C                   enddo

     c                   if        *inkl = *off
      *
      * Update check request record
      *
     c     @@rrn         chain     dbrcdf

     c                   if        @@mode = 'S'
     c                   eval      r4vdsp = 'S'
     c                   else
     c                   eval      r4vdsp = 'V'
     c                   endif

     c                   eval      r4pgm  = q1pgm
     c                   eval      r4user = q1user
     c                   time                    dsdttm
     c***  Begin Chg  ***  05/24/05  **************************************
     c**********         eval      srdti = dsdate
     c**********         call      'SRCVTDAT'    dtcymd
     c**********         movel     srdto         r4date
     c                   eval      R4DATE = DateTo7(SystemDate)
     c***  End   Chg  ***  05/24/05  **************************************
     c                   eval      r4time = dstime
     c                   update    dbrcdf

     C***  Begin Add  ***  05/24/05  **************************************
     C                   select
     C                   when      R4ACFL = 'Y'
     C                   exsr      writeAB
     C                   when      R4ACFL <> 'Y'
     C*** beg add ***** 07/18/07 ******************************************
     C                             and R4BTYP <> 'I'
     C*** end add ***** 07/18/07 ******************************************
     C***  End   Add  ***  05/24/05  **************************************
     c                   exsr      writeAR
     C***  Begin Add  ***  05/24/05  **************************************
     C                   endsl
     C***  End   Add  ***  05/24/05  **************************************

     c                   if        wwgenchk = 'Y'
     c                   exsr      genRequest
     c                   endif

     c                   endif

     c                   seton                                        lr
     c                   return

     C                   endsr
     *================================================================
     * WDWERRCK - Error check for Void/Stop pay window
     *================================================================
     C     wderrck       begsr
      *
      * Validate Generate Request Flag
      *
     c                   if        wwgenchk <> 'Y' and wwgenchk <> 'N'
     c                   seton                                        71  90
     c                   end
      *
      * Validate A/R code
      *
      *** beg add ***** 04/14/08 ***********************************************
     c                   if        R4BTYP <> 'I'
      *** end add ***** 04/14/08 ***********************************************
     c                   eval      kkkey = wwarcd
     c     tbky          chain     ptt01
     c  n90              if        not %found or ttstat = 'I' or tttype
     c                             <> 'T' or ttctyp <> 'P'
     c                   seton                                        72  90
     c                   endif
      *** beg add ***** 04/14/08 ***********************************************
     c                   endif
      *** end add ***** 04/14/08 ***********************************************

     c                   endsr

     C***  Begin Add  ***  05/24/05  **************************************
     *================================================================
     * writeAB - Write Agency Billing for void/stop payment
     *================================================================
     C     writeAB       begsr

     c                   eval      sysDt7   = DateTo7(SystemDate)
     c                   eval      sysTime6 = TimeTo6(SystemTime)

     c                   clear                   pab0p

     c                   eval      ABCO#  = R4CO#
     c                   eval      ABFEIN = R4FEIN
     c                   eval      ABMOD  = R4MOD
     c                   eval      ABAGT# = GetAgent#(R4CO#:R4FEIN:R4MOD)
     c                   eval      ABSTDT = R4STDT
     c                   eval      ABFND  = R4FND
     c                   eval      ABEMP# = R4EMP#
     c                   eval      ABDIV  = R4DIV
     c                   eval      ABFYR  = R4FYR
     c                   eval      ABAMT  = R4AMT * 1
     c                   eval      ABCHK# = R4CHK#
     c*
     c                   eval      ABARCD = wwarcd
     c                   eval      kkkey  = ABARCD
     c     tbky          chain     ptt01
     c                   eval      ABARTY = TTTYPE
     c                   eval      ABDESC = TTDESC
     c                   eval      ABTRDT = sysDt7
     c                   eval      ABPODT = sysDt7
     c*
     c                   eval      ABDT   = sysDt7
     c                   eval      ABTIME = sysTime6
     c                   eval      ABPGM  = q1pgm
     c                   eval      ABUSER = q1user
     c                   eval      ABIDT  = ABDT
     c                   eval      ABITIM = ABTIME
     c                   eval      ABIPGM = ABPGM
     c                   eval      ABIUSR = ABUSER

     c                   write     pab0p

     c                   exsr      updateWSASP

     C                   endsr
     C***  End   Add  ***  05/24/05  **************************************

     *================================================================
     * writeAR - write AR for void/stop payment
     *================================================================
     C     writeAR       begsr

     c                   eval      eaco#  = r4co#
     c                   eval      eafnd  = r4fnd
     c                   eval      eaemp# = r4emp#
     c                   eval      eadiv  = r4div
     c                   eval      eafyr  = r4fyr
     c                   eval      eaamt  = r4amt * 1
     c                   eval      eachk# = r4chk#
     c                   eval      eaarcd = wwarcd
     c                   eval      kkkey  = eaarcd
     c     tbky          chain     ptt01
     c                   eval      eaarty = tttype
     c                   eval      eactyp = ttctyp
     c                   eval      eadesc = ttdesc
     c                   time                    dsdttm
     c***  Begin Chg  ***  05/24/05  **************************************
     c**********         eval      srdti = dsdate
     c**********         call      'SRCVTDAT'    dtcymd
     c**********         movel     srdto         eatrdt
     c                   eval      EATRDT = DateTo7(SystemDate)
     c***  End   Chg  ***  05/24/05  **************************************
     c                   eval      eapodt = eatrdt
     c                   eval      eadt   = eatrdt
     c                   eval      eatime = dstime
     c                   eval      eapgm  = q1pgm
     c                   eval      eauser = q1user
     c                   write     pea01

     C                   endsr
     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C                   endsr
     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C                   endsr
