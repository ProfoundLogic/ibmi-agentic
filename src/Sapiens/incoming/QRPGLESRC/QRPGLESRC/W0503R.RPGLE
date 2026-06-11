     Hcopyright('Fiserv, Inc., 2003-2008')
      *================================================================
      *  DESCRIPTION:  Work with Return Fund Check Requests           *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *  CREATED: 03/12/2003  Matt Dawson                             *
      *================================================================
      *  REVISIONS:                                                   *
      *                                                               *
      *  04/24/24 C:300 Project: WC2676                               *
      *                 This program is no longer in use.             *
      *                 Pass-through this program to new screen.      *
      *                 Add extra entry parm to avoid Pass-through of old screen
      *                                                               *
      *  10/17/18 C:136 Support: 51105                                *
      *                 Fix logic that retrieves bank account info    *
      *                                                               *
      *  09/25/08 C:136 Support: WA109 65336                          *
      *                 Fix display issues in screen file.            *
      *                                                               *
      *  12/26/07 C:183 Project: 81WZB                                *
      *                -Added subset for Surplus Dividend, Employer   *
      *                 # range, customer ID range.                   *
      *                                                               *
      *  07/18/07 C:136 Project: WC1491                               *
      *                 Implement Independent Cash and Return Deposit *
      *                                                               *
      *  04/23/07 C:136 Support: WA109 51758                          *
      *                 Edit check request records to ensure that     *
      *                 bank info is set up prior to calling          *
      *                 the check schedule program W6505R to          *
      *                 prevent bad A/R from being written.           *
      *                                                               *
      *  11/05/04 C:33  Project: WC1033                               *
      *                 Include calculations for Agency Billing.      *
      *================================================================
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0503F    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
     *  Physical file definition (for refreshing subfile record)
     Fwar4p     if   e             disk
     F                                     rename(pr401:pfrcdf)
     *
     *  Primary logical file definition (for displaying items)
     Fwar4l     if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pr401:dbrcdf)
     *
      *** beg add ***** 04/23/07 *******************************************************
     Fwar4l2    if   e           k disk    rename(pr401 : pr40l2) prefix(l2)
      *** Beg Change ***  10/17/18 *****************************************************
     F**wmbil1    if   e           k disk    rename(pbi01 : pbi0l1)
     Fwmbil7    if   e           k disk    rename(pbi01 : pbi0l7)
      *** End Change ***  10/17/18 *****************************************************
     Fwsckl     if   e           k disk    rename(pck01 : pck0l)
      *** end add ***** 04/23/07 *******************************************************
     *
     Fwmeml     if   e           k disk
     Fswcpf     if   e           k disk
     *
     D hd              S             78    dim(4)  ctdata perrcd(1)
     D of              S              1    dim(14) ctdata perrcd(1)
     D ofd             S             35    dim(14) alt(of)
     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           C                   'Return Fund Check Requests'
     D slmsgi          C                   'WC39998'
     D slmsgf          C                   'WMESGG'
     D acdpgm          C                   'W0503R02'
     D comapi          C                   'WA001R'
     D uspapi          C                   'W0006R'
     D lo              C                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              C                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D infomx          C                   1
     D viewmx          C                   1
     D sf1pag          C                   10
     D datusa          S               d   datfmt(*Usa)
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D ##call          s              1
     D @@call          s              1
      *
     D kkco#           s                   like(r4co#)
     D kkfnd           s                   like(r4fnd)
     D kkacct          s                   like(r4acct)
     D kkptnm          s                   like(r4ptnm)
     D kkfyr           s                   like(r4fyr)
      *** beg add **** 04/23/07 ********************************************************
     d bad             s               n
      *** end add **** 04/23/07 ********************************************************
     D*** BEG ADD **** 12/26/07 ********************************************************
     D @R4CUST         S                   LIKE(R4CUST)
     D @SSCUS1         S                   LIKE(R4CUST)
     D @SSCUS2         S                   LIKE(R4CUST)
     D*** END ADD **** 12/26/07 ********************************************************
     D*** Beg Add **** 10/17/18 ********************************************************
     D sysDate7        s                   like(BIefdt)
     D*** End Add **** 10/17/18 ********************************************************
     *================================================================
     *  Data Structure and Arrays
     *================================================================
     D                 ds
     D  view1                  1     72
     D  v1selc                 1      1
     D  v1paye                 4     43
     D  v1payt                48     48
     D  v1camt                53     69
     D***  Begin Add  ***  11/05/04  **************************************
     D  v1acfl                71     71
     D***  End   Add  ***  11/05/04  **************************************

      *** beg add ***** 04/23/07 ******************************************
     d                 ds
     d  acct                   1    120
     d                                     dim(6)
     d   WDACCT1               1     20
     d   WDACCT2              21     40
     d   WDACCT3              41     60
     d   WDACCT4              61     80
     d   WDACCT5              81    100
     d   WDACCT6             101    120
     *================================================================
     *  Prototype definitions
     *================================================================
     d retPremSetUp    pr                  like(bad)
      *** end add ***** 04/23/07 ******************************************
      *
     C/COPY SCOPYWW01C
      *
     *================================================================
     *  PLIST definitions
     *================================================================
    C     *entry        plist
    C                   parm                    @@call
      **** Begin Add **** 04/24/24 ******************************************
    C                   parm                    @@PassThru        1
      **** End   Add **** 04/24/24 ******************************************
     *
     *  Add/change/display program parameter list
     *
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     *================================================================
     *  KLIST definitions
     *================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkptnm
     C                   kfld                    kkfyr
     *================================================================
     * CHKSSM - Set indicator 11 for subset message
     *================================================================
     C     chkssm        begsr
      *
     C                   if        ssptnm <> *blanks
     C                   seton                                        11
     C                   endif
      *
     C                   endsr
     *================================================================
     * ERRCK1 - Additional errors on subfile selection
     *================================================================
     C     errck1        begsr
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C*** beg chg ***** 07/18/07 ******************************************
     C                   select
     C*** end chg ***** 07/18/07 ******************************************
     C                   when      scsel = '20' and sfacfl = 'Y'
     C                                          and sfselc = 'N'
     C                                          and sfstdt = *zero
      *** beg add ***** 07/15/18/07 ***************************************
     C                                          and sfbtyp = 'A'
      *** end add ***** 07/15/18/07 ***************************************
     C                   seton                                        71  90
     C                   endsl
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   endsr
     *================================================================
     * ERRCK2 - Error checking for the subset screen
     *================================================================
     C     errck2        begsr
     C***  BEG ADD ******  12/26/07  **************************************
      *
      * Convert SSCUS1 and SSCUS2 to Upper case
      *
     C                   EVAL      @SSCUS1 = %XLATE(LO:UP:SSCUS1)
     C                   EVAL      @SSCUS2 = %XLATE(LO:UP:SSCUS2)
      *
     C                   SELECT
      *
     C                   WHEN      SSCUS1 <> *BLANKS AND SSCUS2 = *BLANKS
     C                   SETON                                        33  90
      *
     C                   WHEN      SSCUS2 <> *BLANKS AND SSCUS1 = *BLANKS
     C                   SETON                                        32  90
      *
     C                   WHEN      @SSCUS2 < @SSCUS1
     C                   SETON                                        39  90
      *
     C                   WHEN      SSEMP1 <> *ZERO AND SSEMP2 = *ZERO
     C                   SETON                                        35  90
      *
     C                   WHEN      SSEMP2 <> *ZERO AND SSEMP1 = *ZERO
     C                   SETON                                        34  90
      *
     C                   WHEN      SSEMP2 < SSEMP1
     C                   SETON                                        40  90
      *
     C                   WHEN      SSAGT1 <> *ZERO AND SSAGT2 = *ZERO
     C                   SETON                                        37  90
      *
     C                   WHEN      SSAGT2 <> *ZERO AND SSAGT1 = *ZERO
     C                   SETON                                        42  90
      *
     C                   WHEN      SSAGT2 < SSAGT1
     C                   SETON                                        41  90
      *
     C                   WHEN      SSSDFL <> 'Y' AND SSSDFL <> 'N' AND
     C                             SSSDFL <> ' ' AND CLTID = '081'
     C                   SETON                                        38  90
      *
     C                   ENDSL
     C***  END ADD ******  12/26/07  **************************************
     C                   endsr
     *================================================================
     * INZSST - Initialize the subset fields
     *================================================================
     C     inzsst        begsr
     C                   endsr
     *================================================================
     * KEYINZ - Initialize key values for program start
     *================================================================
     C     keyinz        begsr
      *
     C                   eval      kkco#  = ##co#
     C                   eval      kkptnm = *loval
     C                   eval      kkfyr  = *loval
      *
     C                   endsr
     *================================================================
     * KEYSET - Set key values when doing "position to"
     *================================================================
     C     keyset        begsr
      *
     C                   select
     C                   when      *in21
     C                   movel(p)  scpst1        kkptnm
     C                   when      *in22
     C                   movel(p)  scpst2        kkacct
     C                   movel(p)  scpst3        kkptnm
     C                   endsl
      *
     C                   endsr
     *================================================================
     * KEYSFL - Set key values from subfile records for paging
     *================================================================
     C     keysfl        begsr
      *
     C                   eval      kkptnm = sfptnm
     C                   eval      kkfyr  = sffyr
      *
     C                   endsr
     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr
      *
     C                   eval      sfptnm = r4ptnm
     C                   eval      sffyr  = r4fyr
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      sfstdt = R4STDT
     C                   eval      sfacfl = R4ACFL
     C                   eval      sfselc = R4SELC
     C***  End   Add  ***  11/05/04  **************************************
     C*** beg add ***** 07/18/07 ******************************************
     C                   eval      sfbtyp = R4BTYP
     C*** end add ***** 07/18/07 ******************************************
      *
     C                   select
     C                   when      *in21
     C                   eval      v1selc = r4selc
     C                   eval      v1payt = r4ptfl
     C                   eval      v1paye = r4ptnm
     C                   eval      v1camt = %editc(r4amt:'J')
     C***  Begin Add  ***  11/05/04  **************************************
     C*** BEG CHG ***** 07/18/07 ******************************************
     C*****************  eval      v1acfl = R4ACFL
     C                   eval      v1acfl = R4BTYP
     C*** END CHG ***** 07/18/07 ******************************************
     C***  End   Add  ***  11/05/04  **************************************
     C                   eval      scdata = view1
     C                   endsl
      *
     C                   endsr
     *================================================================
     * OTHFKS - Process other function keys
     *================================================================
     C     othfks        begsr
      *
     C     *inkg         caseq     *on           check_sched
     C     *inkv         caseq     *on           print_rpt
     C                   endcs
      *
     C                   endsr
     *================================================================
     * Print report
     *================================================================
     C     print_rpt     begsr
      *
     C                   call      'W0503R21'
      *
     C                   endsr
     *================================================================
     * Check Schedule
     *================================================================
     C     check_sched   begsr
      *
      *** beg add ***** 04/23/07 **************************************
     c                   eval      bad = *on
     c                   if        retPremSetup = bad
     c                   exfmt     SETUPWDW
     c                   else
      *** end add ***** 04/23/07 **************************************
     C*** BEG CHG ***** 12/26/07 **************************************
     C*************      call      'W6505R'
     C*************      parm      'Z'           ##call
     C                   CALL      'W6501C'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C*** END CHG ***** 12/26/07 **************************************
      *** beg add ***** 04/23/07 **************************************
     c                   endif
      *** end add ***** 04/23/07 **************************************
      *
     C                   endsr
     *================================================================
     * OTHOPT - Process other options
     *================================================================
     C     othopt        begsr
      *
     C     scsel         caseq     '20'          opt20
     C                   endcs
      *
     C                   endsr
     *================================================================
     * Option 20 processing
     *================================================================
     C     opt20         begsr
      *
     C                   eval      ##mode = 'S'
     C                   call      'W0503R02'    acdpl
      *
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
      **** Begin Add **** 04/24/24 ******************************************
      * Do not Pass-Through to new screen, if 2nd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 2
     c                   Call      'WTRFCHKREQ'
     c                   parm                    @@rrn             9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 04/24/24 ******************************************
      *
      * Display window if return premium module not installed
      *
     C                   read      swcpf
     C                   if        %eof or WCFL25 <> 'Y'
     C                   exfmt     rpinstwdw
     C                   seton                                        lr
     C                   return
     C                   endif

      *** beg add ***** 07/18/07 **************************************
     c                   if        WCFL36 = 'Y'
     c                   move      *on           *in36
     c                   endif
      *** end add ***** 07/18/07 **************************************
     C*** BEG ADD ***** 12/26/07 ******************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   IF        CLTID = '081'
     C                   EVAL      *IN31 = *ON
     C                   ENDIF                                                  CLTID = '081'
      *
     C*** END ADD ***** 12/26/07 ******************************************
      *
      /Free
       //*** Begin Add *** 10/17/18 ***************************************
       sysDate7 = %Dec(%Date():*Cymd);
       //*** End   Add *** 10/17/18 ***************************************
      /End-Free

     C                   endsr
     *================================================================
     * READ   - Read next record from file
     *================================================================
     C     read          begsr
      *
     C                   if        *in21
     C     ##co#         reade(n)  dbrcdf                                 95
     C                   endif
      *
     C                   endsr
     *================================================================
     * READP  - Read previous record from file
     *================================================================
     C     readp         begsr
      *
     C                   if        *in21
     C     ##co#         readpe(n) dbrcdf                                 95
     C                   endif
      *
     C                   endsr
     *================================================================
     * SETGT  - Set greater than on file
     *================================================================
     C     setgt         Begsr
      *
     C                   if        *in21
     C     key1          setgt     dbrcdf                             96
     C                   endif
      *
     C                   endsr
     *================================================================
     * SETLL  - Set lower limits on file
     *================================================================
     C     setll         begsr
      *
     C                   if        *in21
     C     key1          setll     dbrcdf                             96
     C                   endif
      *
     C                   endsr
     *================================================================
     * SF1UPD - Update subfile record when item is deleted
     *================================================================
     C     sf1upd        begsr
     C                   eval      scdata = '***Deleted***'
     C                   endsr
     *================================================================
     * SLCTRD - Move fields for selected record to parameters
     *================================================================
     C     slctrd        begsr
     C                   endsr
     *================================================================
     * SUBSET - Determine if record should be put in subfile
     *================================================================
     C     subset        begsr
      *
     C                   if        r4chk# <> 0
     C                   eval      meetss = *off
     C                   endif
      *
     C                   if        ssptnm <> *blanks
     C     LO:UP         xlate(p)  r4ptnm        u1pdsc
     C     LO:UP         xlate(p)  ssptnm        u2pdsc
     C                   exsr      string
     C                   endif
      *
      *** beg add ***** 07/18/07 *************************************
     C                   if        ssbtyp <> *blanks and
     c                             ssbtyp <> r4btyp
     c                   eval      meetss = *off
     c                   endif

     C                   if        ssptyp <> *blanks and
     c                             ssptyp <> r4ptfl
     c                   eval      meetss = *off
     c                   endif
      *** end add ***** 07/18/07 *************************************
     C***  BEG ADD  *****  12/26/07  **************************************
      *
     C                   IF        SSAGT1 <> *ZERO AND SSAGT2 <> *ZERO
     C                                           AND
     C                             (R4AGT# < SSAGT1 OR R4AGT# > SSAGT2)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSAGT1 <> *ZERO
      *
     C                   IF        SSEMP1 <> *ZERO AND SSEMP2 <> *ZERO
     C                                           AND
     C                             (R4EMP# < SSEMP1 OR R4EMP# > SSEMP2)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSEMP1 <> *ZERO
      *
     C                   IF        SSCUS1 <> *BLANKS AND SSCUS2 <> *BLANKS
     C
     C                   EVAL      @R4CUST = %XLATE(LO:UP:R4CUST)
     C
     C                   IF        @R4CUST < @SSCUS1 OR @R4CUST > @SSCUS2
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  @R4CUST < SSCUS1
      *
     C                   ENDIF                                                  SSCUS1 <> *BLANKS
      *
     C                   IF        CLTID = '081'
      *
     C                   IF        R4SDFL = *BLANK
     C                   MOVE      'N'           @R4SDFL           1
     C                   ELSE
     C                   MOVE      R4SDFL        @R4SDFL           1
     C                   ENDIF                                                  R4SDFL = *BLANK
      *
     C                   IF        (SSSDFL <> *BLANK AND
     C                             SSSDFL <> @R4SDFL)
     C                   EVAL      MEETSS = *OFF
     C                   ENDIF                                                  SSSDFL <> *BLANK
      *
     C                   ENDIF                                                  CLTID = '081'
     C***  END ADD  *****  12/26/07  **************************************
      *
     C                   endsr
      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
     C     string        begsr

     C                   call      'QCLSCAN'
     C                   parm                    u1pdsc           40
     C                   parm      40            strlen            3 0
     C                   parm      1             start             3 0
     C                   parm                    u2pdsc           40
     C                   parm      40            patlen            3 0
     C                   parm      '0'           trans             1
     C                   parm      '1'           trim              1
     C                   parm      ' '           wild              1
     C                   parm                    scnres            3 0
      *
     C                   if        scnres = 0
     C                   eval      meetss = *off
     C                   endif
      *
     C                   endsr
     p*===========================================================================
     p* retPremSetup = returns *on if A/R codes not set up in Bank Account Master
     p*===========================================================================
     p retPremSetup    b
     d retPremSetup    pi                  like(bad)

     d setUpFlag       s                   like(bad)
     d svfnd           s                   like(L2R4FND) inz(0)
     d svappl          s                   like(BIAPPL)  inz('RP')
     d ctr             s              1  0 inz(0)
      *** beg add ***** 07/18/07 *******************************************************
     d ckRDCodes       s               n   inz(*off)
     d ckRPCodes       s               n   inz(*off)
     d x               s              1  0
      *** end add ***** 07/18/07 *******************************************************

     c     l2ky          klist
     c                   kfld                    ##co#
     c                   kfld                    svfnd

     c     biky          klist
     c                   kfld                    ##co#
     c                   kfld                    svappl
     c                   kfld                    svfnd


     c     ckky          klist
     c                   kfld                    ##co#
     c                   kfld                    BIACCT

     c                   eval      setUpFlag = *off
     c     ##co#         setll     pr40l2
     c     ##co#         reade     pr40l2
     c                   dow       not %eof

     c                   if        L2R4SELC = 'Y'

      *** beg add ***** 07/18/07 *******************************************************
     c                   if        L2R4BTYP = 'P'
     c                   eval      ckRPCodes = *on
     c                   endif

     c                   if        L2R4BTYP = 'D'
     c                   eval      ckRDCodes = *on
     c                   endif
      *** end add ***** 07/18/07 *******************************************************

     c                   if        svfnd <> L2R4FND
     c                   eval      svfnd = L2R4FND
      /Free
        //*** Begin Change *** 10/17/18 *************************************************
        //c     biky          setll     pbi0l1
        //c     biky          reade     pbi0l1
        //c                   dow       not%eof
        Setll (##co#:svappl:svfnd:sysDate7) pbi0l7;
        Reade (##co#:svappl:svfnd) pbi0l7;
        If %Eof(WMBIL7);
           Iter;
        Endif;
       //*** End   Change *** 10/17/18 *************************************************
      /End-Free
     c     ckky          chain     pck0l
      *** beg add ***** 07/18/07 *******************************************************
     c                   if        ckRPCodes = *on
      *** beg add ***** 07/18/07 *******************************************************
      *** Begin Change *** 10/17/18 ****************************************************
     c****                 if        CKSTAT <> 'A' or    //Status not applicable
     c****                           CKRPCD = *blanks or //to error message.
     c                   if        CKRPCD = *blanks or
      *** End Change *** 10/17/18 ****************************************************
     c                             CKRPVS = *blanks
     c                   eval      setUpFlag = *on
     c                   eval      ctr = ctr + 1
      *** beg add ***** 07/18/07 *******************************************************
     c                   if        ctr = 7
     c                   eval      ctr = 6
     c                   endif
     c                   eval      x = 1
     c     CKACCT        lookup    acct(x)                                60
     c                   if        *in60 = *off
      *** end add ***** 07/18/07 *******************************************************
     c                   eval      acct(ctr) = CKACCT
     c                   endif
      *** beg add ***** 07/18/07 *******************************************************
     c                   endif
     c                   endif
      *** end add ***** 07/18/07 *******************************************************

      *** beg add ***** 07/18/07 *******************************************************
     c                   if        *in36  and ckRDCodes = *on
     c                   if        CKRDCD = *blanks or
     c                             CKRDVS = *blanks
     c                   eval      setUpFlag = *on
     c                   eval      ctr = ctr + 1
     c                   if        ctr = 7
     c                   eval      ctr = 6
     c                   endif
     c                   eval      x = 1
     c     CKACCT        lookup    acct(x)                                60
     c                   if        *in60 = *off
     c                   eval      acct(ctr) = CKACCT
     c                   endif
     c                   endif
     c                   endif
     c                   eval      ckRDCodes = *off
     c                   eval      ckRPCodes = *off
      *** end add ***** 07/18/07 *******************************************************
      *** Beg Delete *** 10/17/18 ******************************************************
     c***  biky          reade     pbi0l1
     c***                enddo
      *** End Delete *** 10/17/18 ******************************************************

     c                   else
     c     l2ky          setgt     pr40l2
     c                   endif

     c                   endif

     c     ##co#         reade     pr40l2
     c                   enddo

     c                   return    setUpFlag

     p retPremSetup    e
      *===============================================================
** Column headings
Opt  Ck  Payee Name                                Type        Check Amt  Type
** Description--------------MISN
1022=Change                 YYY
1044=Delete                 YYY
1055=Display                YYY
1                           NNN
1                           NNN
12020=Chg ck release status YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
207F7=Write checks          YYY
209F9=Release all           NNN
212F12=Cancel               NYY
217F17=Subset               YYY
221F21=Print report         YYY
3
