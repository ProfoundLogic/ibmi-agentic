     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H bnddir('STBNDDIR')
     H/TITLE M0021R - Work with Environment Library List
     H**************************************************************************
     H/TITLE - Companies Update/Display
     H**************************************************************************
     H*  DESCRIPTION:
     H*    Update/display detail on one company.
     H*
     H*  CALLED FROM:
     H*    MENU
     H*
     H*  CALLS TO:
     H*
     H*  REWRITE: 10/23/02      C:136     Support: WA099 31785
     H**********************************************************
     H* REVISIONS:
     H*
     H* 04/30/19 C:062 Project: 152WAB
     H*                Apply Optacomp Mods.
     H*
     H* 02/21/17 C:62  Project: 45803(148)
     H*                Correct Output logo and path edits
     H*
     H* 12/06/16 C:65  Project: SY371
     H*                Add path field for logo.
     H*
     H* 09/15/16 C:65  Project: SY371
     H*                Add output logo file name to screen 1 for
     H*                Scriptura or other output purposes.
     H*
     H* 07/25/16 C:136 Project: 114WYQ
     H*                Add Network ID field (based on Relay
     H*                module being installed)
     H*
     H* 02/05/16 C:136 Support: 37550
     H*                Chg label 'ACH batch' to 'EFT batch'
     H*
     H* 10/01/15 C:291 Support: Phw# 37522
     H*                Fixed wording on Company Maint for Claim
     H*                Action plan Remark Code. Recompile program.
     H*
     H* 09/17/15 C:302 PROJECT WC2254
     H*                Add 'Employer Copy control' flags.
     H*
     H* 04/08/15 C:279 Project: WC2328                                *
     H*                Added field 'Exclude reserve code from duplic- *
     H*                ate payment criteria'.                         *
     H*                                                               *
     H* 07/28/14 C:295 Project: 114WMD                                *
     H*                 Store the DART setup for Claim Action plan at *
     H*                 Company Level.                                *
     H*                                                               *
     H* 04/30/14 C:283 Support: WC045 85421                           *
     H*                 Corrected the Drop Down SCCKFE to have the    *
     H*                 correct value as "Check #". Re-Compiled.      *
     H*                                                               *
     H* 02/04/14 C:190 Project: 147WAI(SY336 Priss#49)                *
     H*                 Display "Dividend Plan Default Remark Code"   *
     H*                 for CSI only.                                 *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC
     H*                Change the prompt from 'Print checks in FEIN#
     H*                order' to 'Print Check Order' on 2nd screen.
     H*
     H* 05/27/13 C:190 Project: 147WAI
     H*                Add fields to maintain Dividend Plan DARTS Remark Codes.
     H*
     H* 08/01/11 C:190 Project: 133WLP
     H*                Add a new field for Submit mass Policy
     H*                Page to separate job queue
     H*
     H* 12/06/10 C:257 Project: 25WQY
     H*                Debit ACH Settlement that goes to the bank be
     H*                changed from Group Name to Company name.
     H*
     H* 05/12/10 C:136 WC1806 PRISS
     H*                Fix 5th Assessment search
     H*
     H* 04/19/10 C:235 Project: 25WRG
     H*                Claims General Retrofit
     H*
     H* 12/02/09 C:136 Project: 25WPZ
     H*                Add field for fiscal year start month
     H*
     H* 06/25/09 C:136 Project: WC1806
     H*                Add A/R codes for ACH transactions
     H*
     H* 07/14/09 C:189 Project: 114WOJ
     H*                  Change of Address Tracking/Force new Darts
     H*                  Remark
     H*
     H* 11/26/08 C:220 Project: 81WZY
     H*                Return Funds - Batch Edit Sort User Defined
     H*                Controls.
     H*
     H* 06/20/08 C:189 Project: SY156
     H*                Recompiled the program for display file change
     H*                Corrected the upper case text to lower case as
     H*                its not per the standards.Also corrected the
     H*                formatting of 'SORT(C/E)' field
     H*
    H*
     H* 03/13/08 C:183 Project: WC1634
     H*                Add "Batch Transfer Reports Sort Sequence"
    H*                logic.
    H*
     H* 02/22/08 C:180  SUPPORT 62403 - Add Incoming Watts Area Code.
     H*
    H* 12/08/07 C:33  Project: 132WGK/136WCH   OP144/61405
    H*                Change WMRSL to be User Controlled.  Only open the
    H*                 file if WCMS is installed.
    H*
    H* 10/12/07 C:190 Project: WC1557
    H*                Add prompt for Print SS# on cliam check.
    H*
    H* 04/13/07 C:33  Project: 109WDZ  WA099/55779
    H*                Change WTABL to be User Controlled.  Only open the
    H*                 file if WCMS is installed.
    H*
     H* 04/09/07 C:180 Project: WA099/54234
     H*                Make A/R adj fields on SRC02 available only when
     H*                client number is 109. REF. 109WDZ
     H*
    H* 07/10/06 C:136 Project: 132WGK/136WCH
    H*                Add prompt for CA Pure Rate Code
    H*
    H* 10/20/05 C:53  Project: WC1324
    H*                Add prompt entries for SR batch security
    H*
    H* 06/24/05 C:33  Project: 109WDZ
    H*                Add prompt entries for:
    H*                  - A/R premium adjustment code
    H*                  - A/R deposit adjustment code
    H*                  - A/R fee adjustment code
    H*                  - A/R adjustment threshold
     H*
     H* 06/23/05  C:62  Project: WC1307
     H*                 Add jobq flag processing.
     H*
     H* 11/10/03  C:33  Support: WA129/39885
     H*                 Do not allow duplicate company numbers.
     H*
     H* 04/20/03  C:61  WC577
     H*               - Correct - Mailing state not being saved to file.
     H*               - Do not allow update of company number in
     H*                 CHANGE mode.
    H**************************************************************************
     Fs0001f02  cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
    F*
    F*  Physical file definition (for maintaining record)
     Fsmcop     uf a e             disk
     F                                     rename(pco01:dbrcdf)
     F*
     F* System Control File
     Fssylf     if   e           k disk
     F*** beg add ***** 06/25/09 ******************************************
     f                                     prefix(#)
     F*** end add ***** 06/25/09 ******************************************
     F*
     F* State Table
     Fstabl     if   e           k disk
     F***  Begin Add  ***  06/24/05  **************************************
     F***  Begin Chg  ***  04/13/07  **************************************
     F**********wtabl     if   e           k disk
     Fwtabl     if   e           k disk    usropn
     F***  End   Chg  ***  04/13/07  **************************************
     F*** beg del ***** 06/25/09 ******************************************
     F***********                          ignore(psy01)
     F*** end del ***** 06/25/09 ******************************************
     F                                     ignore(ppc01)
     F                                     ignore(prc01)
     F                                     ignore(pbc01)
     F                                     ignore(pcs01)
     F*** beg add ***** 07/10/06 ******************************************
     F                                     ignore(prs01)
     F*** end add ***** 07/10/06 ******************************************
     F***  End   Add  ***  06/24/05  **************************************
     F*** beg add ***** 07/10/06 ******************************************
     F***  Begin Chg  ***  12/08/07  **************************************
     F**********wmrsl     if   e           k disk
     Fwmrsl     if   e           k disk    usropn
     F***  End   Chg  ***  12/08/07  **************************************
     F*** end add ***** 07/10/06 ******************************************
      ****** Beg   add 07/14/09 ***************************************
     Fstctl1    if   e           k disk
      ****** end   add 07/14/09 ***************************************

     D***  Begin Add  ***  11/10/03  **************************************
     D/copy sPrCompany
     D***  End   Add  ***  11/10/03  **************************************

    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'COMPANIES'
     D comapi          c                   'SA005R'
     D***  Beg   Add  ***  11/26/08  **************************************
     D********   #ofscr          c                   2
     D #ofscr          c                   3
     D***  End   Add  ***  11/26/08  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D***  Begin Add  ***  06/24/05  **************************************
     D ##call          s              1
     D ##kc            s              1
     D ##key           s             10
     D ##kl            s              1
     D***  End   Add  ***  06/24/05  **************************************
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@PRDS          s             40
     D @@CALL          s              1
     D @@KEY           s             10
     D @@RTC           s              1
     D*** beg   add *** 08/01/11 *******************************************
     D@OBJTYPE         S              7A   INZ('*JOBQ')
     D*** end   add *** 08/01/11 *******************************************
     D***  Begin Add  ***  06/24/05  **************************************
     D kkco#           s                   like(TTCO#)
     D kkkey           s                   like(TTKEY)
     D***  End   Add  ***  06/24/05  **************************************
     D***  Begin Add  ***  04/13/07  **************************************
     D WTABLopen       s              1n   inz(*off)
     D***  End   Add  ***  04/13/07  **************************************
     D***  Begin Add  ***  12/08/07  **************************************
     D WMRSLopen       s              1n   inz(*off)
     D***  End   Add  ***  12/08/07  **************************************
     D***  Begin Add  ***  04/09/07  **************************************
     D CLTID           s              3
     D***  End   Add  ***  04/09/07  **************************************
    D*** begin add ***** 12/06/16 ********************************************
     d strSlash        s              1
     d strSlash2       s              1
    D*** end add ******* 12/06/16 ********************************************

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
      *** beg add ***** 05/27/13 ******************************************
     d dartSearch      pr                  extpgm('SC305R')
     d ##call                         1
     d ##kc                           1
     d ##kl                           1
     d ##rmcd                         5
     d ##desc                        40
     d ##rtcd                         1
      *** end add ***** 05/27/13 ******************************************
    D*---------------------------------------------------------------------
    D* Procedure Prototypes
    D*---------------------------------------------------------------------
      *** beg add ***** 07/25/16 ******************************************
      /copy SprModule
      *** end add ***** 07/25/16 ******************************************
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C* Key List for the System Control File
     C*
     C     syky1         klist
     C                   kfld                    kkcntl            2
     C*
     C* Key List for the State File
     C*
     C     stky          klist
     C                   kfld                    kkst             10
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C     keyWTABL      klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
     C***  End   Add  ***  06/24/05  **************************************
     C*** beg add ***** 07/10/06 ******************************************
     C     rsky          klist
     C                   kfld                    scco#
     C                   kfld                    sccart
     C*** beg add ***** 07/10/06 ******************************************
     C*
      ****** Beg   add 07/14/09 ***************************************
     C     ctky          klist
     C                   kfld                    scnmad
      ****** End   add 07/14/09 ***************************************
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C***  Begin Add  ***  11/10/03  **************************************
     C*
     C* Do not allow duplicate company #'s, in ADD mode
     C*
     C                   if        @@mode = 'A' and ValidCo#(scco#)
     C                   seton                                        89  90
     C                   endif
     C***  End   Add  ***  11/10/03  **************************************
     ****** beg add ***** 12/06/10 ********************************************
     C                   if        cltid = '025' and scanam = *blanks
     c                   seton                                        88  90
     C                   endif
     ****** end add ***** 12/06/10 ********************************************
     C*
     C* Company Number Must Be Greater then Zero
     C*
     C     scco#         ifle      *zero
     c                   seton                                        71  90
     C                   endif
     C*
     C* Company Name Cannot be Blank
     C*
     C  n90scname        ifeq      *blanks
     c                   seton                                        72  90
     C                   endif
     C*
     C* Address Line 1 Cannot be Blank
     C*
     C  n90scadr1        ifeq      *blank
     c                   seton                                        73  90
     C                   endif
     C*
     C* City Cannot be Blank
     C*
     C  n90sccty         ifeq      *blank
     c                   seton                                        74  90
     C                   endif
     C*
     C* State Cannot be Blank
     C*
     C  n90scst          ifeq      *blank
     c                   seton                                        75  90
     C                   endif
     C*
     C* Validate state
     C*
     C  n90scst          ifne      *blank
     c                   eval      kkst = scst
     c     stky          chain     pst01
     C*
     c                   if        not %found
     c                   seton                                        78  90
     C                   endif
     C*
     C                   endif
     C*
     C* Zip Code Cannot be Zero
     C*
     C  n90sczip1        ifle      *zero
     c                   seton                                        76  90
     C                   endif
     C*
     C* 1099 Amount must be Greater than Zero
     C*
     C                   if        *in50
     C*
     C  n90sc1099        ifle      *zero
     c                   seton                                        77  90
     C                   endif
     C*
     C                   endif
     C*** begin add *** 06/23/05 *******************************************
     C*
     C* jobq flag must be valid if entered.
     C*
     C                   if        not *in90 and scbflg <> 'Y' and
     C                                           scbflg <> ' '
     C                   seton                                        7990
     C                   endif
     C*** end   add *** 06/23/05 *******************************************
     C*
     C*** beg   add *** 08/01/11 *******************************************
     C* jobq flag must be valid if entered.
     C*
     C                   if        not *in90 and scpflg <> 'Y' and
     C                                           scpflg <> ' '
     C                   seton                                        34  90
     c                   leavesr
     C                   endif

     c                   if        scpflg = 'Y'
     C                   Call      'SA030R'
     C                   Parm      *blanks       suffix            3

     C*Determine Sep JOBQ Name for Data Center and non-data center clients
     C                   If        suffix <> *blanks
     C*
     C* Data Center client Sep JOBQ e.g. PDSJOBO132.
     C                   Eval      @SepJobQ='PDSJOBP' + suffix
     C                   Else
     C* Non-data Center client Sep JOBQ
     C                   Eval      @SepJobQ='QBATCHP'
     C                   EndIf                                                  pflr <> *Blanks

     C                   CALL      'SRCHKOBJ'
     C                   PARM                    @SepJobQ         10
     C                   PARM                    @OBJTYPE
     C                   PARM                    @FLG              1
     C*
     C*If JOBQ Not  found
     C                   if        @Flg='1'
     C                   seton                                        35  90
     c                   leavesr
     C                   endif                                                  @Flg='1'
     C                   endif                                                  scpflg = 'Y'
     C*
     C*** end   add *** 08/01/11 *******************************************
       //*** begin add *** 09/15/16 ************************
      /free
       //*** Begin Chg *** 02/21/17 **************************************
       //***Clear erSLogo;
       //***Clear msSLogo;
       //***
       //***If %scan('.':scSLogo) = 0;
       //***   msSLogo = 'Logo file name must have a file name extension.';
       //***   erSlogo = *on;
       //***   *in90 = *on;
       //***Endif;
       //*** begin add *** 12/06/16 ************************
       //***Clear erSLogoPth;
       //***Clear msSLogoPth;
       //***
       //***  // Image path must be entered.
       //***If scSLogoPth = ' ' and scSLogo <> ' ';
       //***   msSLogoPth = 'Logo image file path must be entered.';
       //***   erSLogoPth = *on;
       //***   *in90 = *on;
       //***Else;
       //***
       //***// Path must begin and end with a '/' character.
       //***   strSlash  = %subst(scSLogoPth:1:1);
       //***   strSlash2 = %subst(scSLogoPth:%len(%trim(scSLogoPth)):1);
       //***
       //***   If strSlash <> '/' or strSlash2 <> '/';
       //***      msSLogoPth = 'Logo image file path must begin and end ' +
       //***       'with a ''/''.';
       //***      erSLogoPth = *on;
       //***      *in90 = *on;
       //***   Endif;
       //***
       //***Endif;
       If isScript = *On And scSlogo <> ' ';
          Clear erSLogo;
          Clear msSLogo;

          If %scan('.':scSLogo) = 0;
             msSLogo = 'Logo file name must have a file name extension.';
             erSlogo = *on;
             *in90 = *on;
          Else;
             Clear erSLogoPth;
             Clear msSLogoPth;

             // Image path must be entered.
             If scSLogoPth = ' ' and scSLogo <> ' ';
                msSLogoPth = 'Logo image file path must be entered.';
                erSLogoPth = *on;
                *in90 = *on;
             Else;

             // Path must begin and end with a '/' character.
                strSlash  = %subst(scSLogoPth:1:1);
                If %len(%trim(scSLogoPth)) <> 0;
                   strSlash2 = %subst(scSLogoPth:%len(%trim(scSLogoPth)):1);

                   If strSlash <> '/' or strSlash2 <> '/';
                      msSLogoPth = 'Logo image file path must begin and end ' +
                       'with a ''/''.';
                      erSLogoPth = *on;
                      *in90 = *on;
                   Endif;
                EndIf;
             Endif;
          EndIf;
       EndIf;
       //*** End   Chg *** 02/21/17 **************************************

       //*** end add ***** 12/06/16 ************************
      /end-free
       //*** end add ***** 09/15/16 ************************
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C*
     c                   if        *in51
     C*
     C     scchk1        ifeq      *zero
     C     scchk2        oreq      *zero
     c                   seton                                        71  90
     C                   endif
     C*
     C*  CLAIM NUMBER MUST NOT BE ZERO
     C*
     C  n90sccase        ifeq      *zero
     c                   seton                                        72  90
     C                   endif
     C*
     C*
     C*  ISO ASSIGNED NUMBER CANNOT BE BLANK
     C*
     C  n90sccoi         ifeq      *blank
     c                   seton                                        73  90
     C                   endif
     C*
     C*  ISO TRANSACTION TYPE MUST BE '1', '2', OR '3'.
     C*
     C                   if        sciso <> '1' and sciso <> '2' and
     c                             sciso <> '3'
     C                   seton                                        74  90
     C                   endif
     C*
     C                   endif
     C***  Begin Add  ***  06/24/05  **************************************
     *
     * If A/R premium adjustment code entered,
     *  A/R adjustment code must be:
     *     'A' - Adjustment Type
     *     'P' - Premium Category Type
     *     Active status
     *
     C  n90              if        scpbcd <> *blanks
     C*
     C                   eval      kkco# = scco#
     C                   eval      kkkey = scpbcd
     C*
     C     keyWTABL      chain     ptt01                              60
     *
     C                   if        *in60 = *on
     C                             or ttstat <> 'A'
     C                             or tttype <> 'A'
     C                             or ttctyp <> 'P'
     C                   seton                                        478290
     C                   endif
     *
     C                   endif
      *** beg add ***** 07/10/06 ***********************************************
      *
      *  CA Pure Rate Code must be a valid CA rate set code.
      *
     c  n90              if        sccart <> *blanks
     c     rsky          chain     prs01
     c                   if        not(%found) or rsjrst <> 'CA'
     c                   seton                                        80  90
     c                   endif
     c                   endif
      *** end add ***** 07/10/06 ***********************************************
     *
     * If A/R deposit adjustment code entered,
     *  A/R adjustment code must be:
     *     'H' - Adjustment Type
     *     'D' - Premium Category Type
     *     Active status
     *
     C  n90              if        scdbcd <> *blanks
     C*
     C                   eval      kkco# = scco#
     C                   eval      kkkey = scdbcd
     C*
     C     keyWTABL      chain     ptt01                              60
     *
     C                   if        *in60 = *on
     C                             or ttstat <> 'A'
     C                             or tttype <> 'H'
     C                             or ttctyp <> 'D'
     C                   seton                                        488290
     C                   endif
     *
     C                   endif
     *
     * If A/R fee adjustment code entered,
     *  A/R adjustment code must be:
     *     'F' - Adjustment Type
     *     'F' - Premium Category Type
     *     Active status
     *
     C  n90              if        scfbcd <> *blanks
     C*
     C                   eval      kkco# = scco#
     C                   eval      kkkey = scfbcd
     C*
     C     keyWTABL      chain     ptt01                              60
     *
     C                   if        *in60 = *on
     C                             or ttstat <> 'A'
     C                             or tttype <> 'F'
     C                             or ttctyp <> 'F'
     C                   seton                                        498290
     C                   endif
     *
     C                   endif
      ****** Beg   add 07/14/09 ***************************************
     *
     * Validate DARTS remark
     *
     C  n90              if        scnmad <> *blanks
     C*
     C     ctky          chain     pct01                              60
     *
     C                   if        *in60 = *on
     C                   seton                                        75  90
     C                   endif
     *
     C                   if        *in71 <> *on
     C                   if        ctstat = 'I'
     C                   seton                                        76  90
     C                   endif
     C                   endif
     *
     *
     C                   endif
      ******* Beg Add ***** 07/28/14 **********************************
      /Free
       If scActDart <> *blanks;
          Chain scActDart  pct01;
          If not %found;
             *in90 = '1';
             indActErrN = *on;
          Endif;

          If CTstat = 'I';
             *in90 = '1';
             indActErrN = *on;
          Endif;
       Endif;
      /End-Free
      ******* End Add ***** 07/28/14 **********************************
      ****** End   add 07/14/09 ***************************************
     *  Threshold Range and A/R Adjustment Code are mutually inclusive
     *
     C  n90              if        (scthrl <> *zero or scthrh <> *zero) and
     C                             (scthrl =  *zero or scthrh =  *zero)
     C                   seton                                        84  90
     C                   endif
     *
     *  Threshold values must be in ascending order, if entered.
     *
     C  n90              if        scthrh <> *zero and scthrh <= scthrl
     C                   seton                                        85  90
     C                   endif
     C*
     C*  At least one code must be entered, if threshold entered
     C*
     C  n90              if        scthrh <> *zero and
     C                             (scpbcd = *blanks and
     C                              scdbcd = *blanks and
     C                              scfbcd = *blanks)
     C                   seton                                        83  90
     C                   seton                                        474849
     C                   endif
     C***  End   Add  ***  06/24/05  **************************************
     C*** Beg Add *** 10/20/05 **************************************

     C*** edits
     C
     C*** End Add *** 10/20/05 **************************************
     C*** Beg Add *** 10/12/07 **************************************
     C*
     C* Print SS# on claim checks must be valid
     C*
     C                   if        not *in90 and scpss# <> 'Y' and
     C                                           scpss# <> ' '
     C                   seton                                        2690
     C                   endif
     C*** End Add *** 10/12/07 **************************************
     C*** BEG DEL *** 11/25/13 *********************************************
     C*** BEG ADD *** 04/19/10 *********************************************
     C*****
     C***** Print checks in FEIN order must be valid
     C*****
     C*****              if        not *in90 and scckfe <> 'Y' and
     C*****                                      scckfe <> ' '
     C*****              seton                                        6290
     C*****              endif
     C*** END ADD *** 04/19/10 *********************************************
     C*** END DEL *** 11/25/13 *********************************************
     C*** BEG ADD *** 03/13/08 ********************************************
     C*
     C* Batch Transfer Reports Sort Sequence
     C*
     C                   IF        NOT *IN90 AND SCARSQ <> 'C' AND
     C                             SCARSQ <> 'E' AND SCARSQ <> ' '
     C                   SETON                                        2890
     C                   ENDIF
     C*** END ADD *** 03/13/08 ********************************************
     C*
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C
     C*** BEG ADD *** 11/26/08 ********************************************
     C*** To check for 'N' 'E' or ' ' entry
     C
     C  n90              If        Scorfbs <> 'N' And Scorfbs <> 'E'
     C                             And Scorfbs <> ' '
      *** BEG CHG ***** 06/25/09 ******************************************
     C*****************  SETON                                        2990
     C                   seton                                        71  90
      *** END CHG ***** 06/25/09 ******************************************
     C                   Endif
     C*** End ADD *** 11/26/08 ********************************************
     C*** beg add ***** 06/25/09 ******************************************
      *
      *  Don't execute the following edits if ACH module is not activivated
      *
     c                   if        *in36 = *off
     c                   leavesr
     c                   endif

     c  n90              if        scsycd <> *blank
      *
      *  Batch system code
      *
     C                   eval      kkco# = scco#
     c                   eval      kkkey = scsycd
     c     keyWTABL      chain     psy01
     c                   if        not %found or systat <> 'A' or
     c                             sytype <> 'Y'
     c                   seton                                        79  90
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        80  90
     c                   endif                                                  scarcd<>*blank
      *
      * Edit the A/R payment codes.
      *
     c  n90              if        scarcd <> *blank
     c                   eval      kkkey = scarcd
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'P' or tttype <> 'P'
     c                   seton                                        762090
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772090
     c                   endif                                                  %found

     c                   else
     c                   seton                                        782090
     c                   endif                                                  scarcd<>*blank

     c  n90              if        sc1aar <> *blank
     c                   eval      kkkey = sc1aar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '1' or tttype <> 'Q'
     c                   seton                                        762190
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772190
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank

     c  n90              if        scecar <> *blank
     c                   eval      kkkey = scecar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'E' or tttype <> 'P'
     c                   seton                                        762290
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772290
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank

     c  n90              if        sc2aar <> *blank
     c                   eval      kkkey = sc2aar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '2' or tttype <> 'Q'
     c                   seton                                        762390
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772390
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank

     c  n90              if        scifar <> *blank
     c                   eval      kkkey = scifar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'F' or tttype <> 'Q'
     c                   seton                                        762490
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772490
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sc3aar <> *blank
     c                   eval      kkkey = sc3aar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '3' or tttype <> 'Q'
     c                   seton                                        762590
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772590
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank

     c  n90              if        scsfar <> *blank
     c                   eval      kkkey = scsfar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'S' or tttype <> 'Q'
     c                   seton                                        762690
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772690
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank

     c  n90              if        sc4aar <> *blank
     c                   eval      kkkey = sc4aar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '4' or tttype <> 'Q'
     c                   seton                                        762790
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772790
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        scmfar <> *blank
     c                   eval      kkkey = scmfar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'M' or tttype <> 'Q'
     c                   seton                                        762890
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772890
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sc5aar <> *blank
     c                   eval      kkkey = sc5aar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '5' or tttype <> 'Q'
     c                   seton                                        762990
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        772990
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank
     C
     c  n90              if        sctxar <> *blank
     c                   eval      kkkey = sctxar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> 'T' or tttype <> 'Q'
     c                   seton                                        763090
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        773090
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank
     c*
     c  n90              if        sc6aar <> *blank
     c                   eval      kkkey = sc6aar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '6' or tttype <> 'Q'
     c                   seton                                        763190
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        773190
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank

     c  n90              if        sc7aar <> *blank
     c                   eval      kkkey = sc7aar
     c     keyWTABL      chain     ptt01
     c                   if        %found and ttstat = 'A'

     c                   if        ttctyp <> '7' or tttype <> 'Q'
     c                   seton                                        763290
     c                   endif                                                  ttctyp

     c                   else
     c                   seton                                        773290
     c                   endif                                                  %found

     c                   else
     c                   endif                                                  scarcd<>*blank
     C*** end add ***** 06/25/09 ******************************************

      *** beg add ***** 12/02/09 ******************************************
      *
      * Ensure valid month for fiscal year start month
      *
     c                   if        scfysm < 0 or
     c                             scfysm > 12
     c                   seton                                        83  90
     c                   endif
      *** end add ***** 12/02/09 ******************************************
      *** beg add ***** 05/27/13 ******************************************
      /free
       // Validate Div Plan Letter Dart.
       If scDivPlnLt <> ' ';
          Chain scDivPlnLt pct01;
          If not %found;
             *in90 = '1';
              msPlnLeter = 'WCR0505';
              erPlnLette = '1';
          Endif;
          If ctstat = 'I';
             *in90 = '1';
              msPlnLeter = 'WCR2347';
              erPlnLette = '1';
          Endif;
       Endif;

       // Validate Prem Thrshld
       If scDivPrmTh <> ' ';
          Chain scDivPrmTh pct01;
          If not %found;
             *in90 = '1';
              msPrmThrsh = 'WCR0505';
              erPrmThrsh = '1';
          Endif;
          If ctstat = 'I';
             *in90 = '1';
              msPrmThrsh = 'WCR2347';
              erPrmThrsh= '1';
          Endif;
       Endif;
      /end-free
      *** end add ***** 05/27/13 ******************************************
     C                   endsr
    C*================================================================
    C* KJDO - Processing for F10
    C*================================================================
     c     kjdo          begsr

     C                   eval      @@key = *blanks

     c                   select

     c***  Begin Add  ***  06/24/05  **************************************
     C                   when      hfld = 'SCDBCD'
     C                             or hfld = 'SCPBCD'
     C                             or hfld = 'SCFBCD'
    C*
     C                   call      'W2161R'
     C                   parm      '2'           ##call
     C     *inkc         parm      *blank        ##kc
     C     *inkl         parm      *blank        ##kl
     C                   parm      *blanks       ##key
    C*
     C                   eval      *in81 = *on
    C*
     C                   if        ##key = *blanks
    C*
     C                   select
     C                   when      hfld = 'SCPBCD'
     C                   eval      *in47 = *on
     C                   when      hfld = 'SCDBCD'
     C                   eval      *in48 = *on
     C                   when      hfld = 'SCFBCD'
     C                   eval      *in49 = *on
     C                   endsl
    C*
     C                   else
    C*
     C                   select
     C                   when      hfld = 'SCPBCD'
     C                   eval      *in48 = *on
     C                   eval      scpbcd = ##key
     C                   when      hfld = 'SCDBCD'
     C                   eval      *in49 = *on
     C                   eval      scdbcd = ##key
     C                   when      hfld = 'SCFBCD'
     C                   eval      scfbcd = ##key
     C                   endsl
     C*
     C                   endif
     c***  End   Add  ***  06/24/05  **************************************
     C*** Beg Add *** 10/20/05 **************************************
     C
     C*** search
     C
     C*** End Add *** 10/20/05 **************************************

     c                   when      hfld = 'SCPFAR'

     C                   call      'C2161R'
     C                   parm                    @@PRDS
     C                   parm                    @@CALL
     C                   parm      *blank        @@KEY
     C                   parm                    @@RTC

     C                   if        @@key =  *blanks
     C                   seton                                        41  90
     c                   else
     c                   eval      scpfar = @@key
     c                   seton                                        42  90
     C                   end

     c                   when      hfld = 'SCPFSC'

     C                   call      'C2001R'
     C                   parm                    @@PRDS
     C                   parm                    @@CALL
     C                   parm      *blank        @@KEY
     C                   parm                    @@RTC

     C                   if        @@key =  *blanks
     C                   seton                                        42  90
     c                   else
     c                   eval      scpfsc = @@key
     c                   seton                                        43  90
     C                   end

     c                   when      hfld = 'SCCNAR'

     C                   call      'C2161R'
     C                   parm                    @@PRDS
     C                   parm                    @@CALL
     C                   parm      *blank        @@KEY
     C                   parm                    @@RTC

     C                   if        @@key =  *blanks
     C                   seton                                        43  90
     c                   else
     c                   eval      sccnar = @@key
     c                   seton                                        44  90
     C                   end

     c                   when      hfld = 'SCTRDL'

     C                   call      'C2041R'
     C                   parm                    @@PRDS
     C                   parm                    @@CALL
     C                   parm      *blank        @@KEY
     C                   parm                    @@RTC

     C                   if        @@key =  *blanks
     C                   seton                                        44  90
     c                   else
     c                   eval      sctrdl = @@key
     c                   seton                                        45  90
     C                   end

     c                   when      hfld = 'SCARRE'

     C                   call      'C2161R'
     C                   parm                    @@PRDS
     C                   parm                    @@CALL
     C                   parm      *blank        @@KEY
     C                   parm                    @@RTC

     C                   if        @@key =  *blanks
     C                   seton                                        45  90
     c                   else
     c                   eval      scarre = @@key
     c                   seton                                        46  90
     C                   end
      *** beg add ***** 07/10/06 ***********************************************
     C*
     C*  RATE SET CODE
     C*
     C                   when      hfld = 'SCCART'
     C*
     C                   call      'W2471R'
     C                   parm      '2'           @@call
     C     *inkc         parm                    @@kc
     C     *inkl         parm                    @@kl
     C                   parm                    @@key
     C*
     C     @@key         ifeq      *blanks
     C                   move      *on           *in52
     C                   else
      *** beg chg ***** 10/12/07 ***********************************************
     C                   IF        CLTID <> '109'
     C                   move      *on           *in25
     c                   else
     C                   move      *on           *in53
     C                   endif
     C****************   move      *on           *in53
      *** end chg ***** 10/12/07 ***********************************************
     C                   movel     @@key         sccart
     C                   endif
      *** end add ***** 07/10/06 ***********************************************
      ****** Beg   add 07/14/09 ***************************************
     C*  DARTS CODE SEARCH
     C*
     C                   when      hfld = 'SCNMAD'
     C*
     C                   CALL      'SC305R'
     C                   parm      '2'           @@call
     C     *inkc         parm                    @@kc
     C     *inkl         parm                    @@kl
     C                   parm      *blanks       ##rmcd            5
     C                   parm      *blanks       ##desc           40
     C                   parm      *blanks       ##rtcd            1
     C*
     C     ##rmcd        ifeq      *blanks
     C                   move      *on           *in70
     C                   else
     C                   move      ##rmcd        scnmad
     C                   move      *on           *in27
     C                   endif
      ****** End   add 07/14/09 ***************************************
      ****** Beg add 07/28/14 *****************************************
      /free
       When hfld = 'SCACTDART';
          ##call = '2';
          ##kc = @@kc;
          ##kl = @@kl;
          ##rmcd = ' ';
          ##desc = ' ';
          ##rtcd = ' ';
          callP dartSearch(##call:##kc:##kl:##rmcd:##desc:##rtcd);
          If ##rmcd = ' ';
             indClmAct = '1';
          Else;
             scActDart = ##rmcd ;
          Endif;
      /End-free
      ****** End add 07/28/14 *****************************************
     c                   endsl

      *** beg add ***** 06/25/09 ***********************************************
     *
     *  Allow following searches if ACH Module is activated
     *
     c   36              select
     *
     *   A/R batch system code search
     *
     c                   when      hfld = 'SCSYCD'

     c                   eval      sckey = *blanks
     c                   call      'W1202R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scsycd = sckey
     c                   seton                                        5020
     c                   else
     c                   seton                                        51
     c                   endif                                                  *inkl <> *on
     *
     *   Premium payment A/R code search
     *
     c                   when      hfld = 'SCARCD'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scarcd = sckey
     c                   seton                                        5021
     c                   else
     c                   seton                                        5020
     c                   endif                                                  *inkl <> *on
     *
     *   1st assessment A/R code search
     *
     c                   when      hfld = 'SC1AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc1aar = sckey
     c                   seton                                        5022
     c                   else
     c                   seton                                        5021
     c                   endif                                                  *inkl <> *on
     *
     *   Expense constant A/R code search
     *
     c                   when      hfld = 'SCECAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scecar = sckey
     c                   seton                                        5023
     c                   else
     c                   seton                                        5022
     c                   endif                                                  *inkl <> *on
     *
     *   2nd assessment A/R code search
     *
     c                   when      hfld = 'SC2AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc2aar = sckey
     c                   seton                                        5024
     c                   else
     c                   seton                                        5023
     c                   endif                                                  *inkl <> *on
     *
     *   Installment fees A/R code search
     *
     c                   when      hfld = 'SCIFAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scifar = sckey
     c                   seton                                        5025
     c                   else
     c                   seton                                        5024
     c                   endif                                                  *inkl <> *on
     *
     *   3rd assessment A/R code search
     *
     c                   when      hfld = 'SC3AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc3aar = sckey
     c                   seton                                        5026
     c                   else
     c                   seton                                        5025
     c                   endif                                                  *inkl <> *on
     *
     *   Safety fees A/R code search
     *
     c                   when      hfld = 'SCSFAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scsfar = sckey
     c                   seton                                        5027
     c                   else
     c                   seton                                        5026
     c                   endif                                                  *inkl <> *on
     *
     *   4th assessment A/R code search
     *
     c                   when      hfld = 'SC4AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc4aar = sckey
     c                   seton                                        5028
     c                   else
     c                   seton                                        5027
     c                   endif                                                  *inkl <> *on
      *** beg add ***** 05/12/10 ***********************************************
     *
     *   5th assessment A/R code search
     *
     c                   when      hfld = 'SC5AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc5aar = sckey
     c                   seton                                        5030
     c                   else
     c                   seton                                        5029
     c                   endif                                                  *inkl <> *on
      *** end add ***** 05/12/10 ***********************************************
     *
     *   Membership fees A/R code search
     *
     c                   when      hfld = 'SCMFAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      scmfar = sckey
     c                   seton                                        5029
     c                   else
     c                   seton                                        5028
     c                   endif                                                  *inkl <> *on
     *
     *   State tax fees A/R code search
     *
     c                   when      hfld = 'SCTXAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sctxar = sckey
     c                   seton                                        5031
     c                   else
     c                   seton                                        5030
     c                   endif                                                  *inkl <> *on
     *
     *   6th assessment A/R code search
     *
     c                   when      hfld = 'SC6AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc6aar = sckey
     c                   seton                                        5032
     c                   else
     c                   seton                                        5031
     c                   endif                                                  *inkl <> *on
     *
     *   7th assessment A/R code search
     *
     c                   when      hfld = 'SC7AAR'

     c                   eval      sckey = *blanks
     c                   call      'W2161R'
     c                   parm      '2'           qqcall            1
     c     *inkc         parm                    qqkc              1
     c     *inkl         parm                    qqkl              1
     c                   parm                    sckey            10

     c                   if        *inkl <> *on and sckey <> *blanks
     c                   eval      sc7aar = sckey
     c                   seton                                        5033
     c                   else
     c                   seton                                        5032
     c                   endif                                                  *inkl <> *on

      ****** Beg   add 05/27/13 ***************************************
      /free
       When hfld = 'SCDIVPLNLT';
          ##call = '2';
          ##kc = @@kc;
          ##kl = @@kl;
          ##rmcd = ' ';
          ##desc = ' ';
          ##rtcd = ' ';
          callP dartSearch(##call:##kc:##kl:##rmcd:##desc:##rtcd);
          If  ##rmcd = ' ';
              PlnLtFoucs = '1';
          Else;
              scDivPlnLt = ##rmcd;
              scPlnLtDes = ##desc;
              PrmThFoucs = '1';
          endif;

       When hfld = 'SCDIVPRMTH';
          ##call = '2';
          ##kc = @@kc;
          ##kl = @@kl;
          ##rmcd = ' ';
          ##desc = ' ';
       ##rtcd = ' ';
       callP dartSearch(##call:##kc:##kl:##rmcd:##desc:##rtcd);
       If  ##rmcd = ' ';
           PrmThFoucs = '1';
       Else;
           scDivPrmTh  = ##rmcd;
           scPrmThDes  = ##desc;
           PrmThFoucs  = '1';
       endif;
      /end-free
      ****** end   add 05/27/13 ***************************************
     c                   endsl
      *** end add ***** 06/25/09 ***********************************************

     c                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scco#  = coco#
     C                   eval      scname = CONAME
     C                   eval      scadr1 = COADR1
     C                   eval      scadr2 = COADR2
     C                   eval      sccty  = COCTY
     C                   eval      scst   = COST
     C                   movel     COZIP         sczip1
     C                   move      COZIP         sczip2
     C                   eval      scshnm = COSHNM
     C                   eval      scmad1 = COMAD1
     C                   eval      scmad2 = COMAD2
     C                   eval      scmcty = COMCTY
     ****** beg chg ***** 04/20/03 ********************************************
     C**********         eval      scmst  = SCMST
     C                   eval      scmst  = comst
     ****** end chg ***** 04/20/03 ********************************************
     C                   move      COMZIP        scmzp2
     C                   movel     COMZIP        scmzp1
     C                   eval      scfein = COFEIN
     C                   eval      sc1099 = CO1099
     C                   eval      scarea = COAREA
     C                   eval      scphn# = COPHN#
     C                   eval      scfara = COFARA
     C                   eval      scfphn = COFAX#
     C                   eval      scph1  = COPH1
     C                   eval      scph2  = COPH2
     C                   eval      scchk1 = COCHK1
     C                   eval      scchk2 = COCHK2
     C                   eval      sccase = CONACS
     C                   eval      sccoi  = COCOI
     C                   eval      sciso  = COISO
     C                   eval      scpfar = COPFAR
     C                   eval      scpfsc = COPFSC
     C                   eval      sccnar = COCNAR
     C                   eval      scarre = COARRE
     C                   eval      sctrdl = COTRDL
     C                   eval      sctcc  = COTCC
     C*** begin add *** 06/23/05 *******************************************
     C                   eval      scbflg = cobflg
     C*** end   add *** 06/23/05 *******************************************
     C*** begin add *** 08/01/11 *******************************************
     C                   eval      scpflg = copflg
     C*** end   add *** 08/01/11 *******************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   eval      scdbcd = CODBCD
     C                   eval      scpbcd = COPBCD
     C                   eval      scfbcd = COFBCD
     C                   eval      scthrl = COTHRL
     C                   eval      scthrh = COTHRH
     C***  End   Add  ***  06/24/05  **************************************
     C*** Beg Add *** 10/20/05 **************************************
     C                   eval      scsrpl = COSRPL
     C                   eval      scsrpt = COSRPT
     C*** End Add *** 10/20/05 **************************************
     C*** beg add ***** 07/10/06 ************************************
     C                   eval      sccart = COCART
     C*** end add ***** 07/10/06 ************************************
     C*** beg add ***** 10/12/07 ************************************
     C                   eval      scpss# = copss#
     C*** end add ***** 10/12/07 ************************************
     C*** beg add ***** 04/19/10 ******************************************
     C                   eval      scckfe = cockfe
     C*** end add ***** 04/19/10 *******************************************
     C*** BEG ADD *** 03/13/08 ********************************************
     C                   EVAL      SCARSQ = COARSQ
     C*** END ADD *** 03/13/08 ********************************************

      ******* BEGIN ADD ********* 02/22/08 ********************************
     C                   eval      scph1A  = COPH1A
      ******* END ADD *********** 02/22/08 ********************************
      ******* Beg ADD *********** 11/26/08 ********************************
     C                   Move      Corfbs        Scorfbs
      ******* End ADD *********** 11/26/08 ********************************
      *** beg add ***** 06/25/09 ******************************************
     c                   eval      scarcd = coarcd
     c                   eval      scsycd = cosycd
     c                   eval      scecar = coecar
     c                   eval      sc1aar = co1aar
     c                   eval      scifar = coifar
     c                   eval      sc2aar = co2aar
     c                   eval      scsfar = cosfar
     c                   eval      sc3aar = co3aar
     c                   eval      scmfar = comfar
     c                   eval      sc4aar = co4aar
     c                   eval      sctxar = cotxar
     c                   eval      sc5aar = co5aar
     c                   eval      sc6aar = co6aar
     c                   eval      sc7aar = co7aar
     c                   eval      scachl = coachl
     c                   eval      scacht = coacht
      *** end add ***** 06/25/09 ******************************************
      ******* Beg add *********** 07/14/09 ********************************
     C                   MoveL     conmad        scnmad
      ******* End add ***********07/14/09 *********************************
      *** beg add ***** 12/02/09 ******************************************
     c                   eval      scfysm = cofysm
      *** end add ***** 12/02/09 ******************************************
     ****** beg add ***** 12/06/10 ********************************************
     C                   eval      scanam = coanam
     ****** end add ***** 12/06/10 ********************************************
      *** beg add ***** 05/27/13 ******************************************
      /free
       scDivPlnLt = COpltr;
       Chain scDivPlnLt pct01;
       If %found;
          scPlnLtDes = ##desc;
       Endif;

       scDivPrmTh = COprth;
       Chain scDivPrmTh pct01;
       If %found;
          scPrmThDes = ##desc;
       Endif;

       //*** beg add ***** 07/25/16 ************************
       scNetID = CONetID;
       //*** end add ***** 07/25/16 ************************
       //*** begin add *** 09/15/16 ************************
       scSLogo = COSLogo;
       //*** end add ***** 09/15/16 ************************
       //*** begin add *** 12/06/16 ************************
       scSLogoPth = COSPath;
       //*** end add ***** 12/06/16 ************************
      /end-free
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr

     C                   eval      COCO#  = scco#
     C                   eval      CONAME = scname
     C                   eval      COSHNM = scshnm
     C                   eval      COADR1 = scadr1
     C                   eval      COADR2 = scadr2
     ****** beg del ***** 04/20/03 ********************************************
     C**********         eval      COMCTY = scmcty
     C**********         eval      COMST  = scmst
     C**********         move      scmzp2        COMZIP
     C**********         movel     scmzp1        COMZIP
     C**********         eval      CONAME = scname
     C**********         eval      COADR1 = scadr1
     C**********         eval      COADR2 = scadr2
     ****** end del ***** 04/20/03 ********************************************
     C                   eval      COCTY  = SCCTY
     C                   eval      COST   = scst
     C                   movel     sczip1        COZIP
     C                   move      sczip2        COZIP
     ****** beg del ***** 04/20/03 ********************************************
     C**********         eval      COSHNM = scshnm
     ****** end del ***** 04/20/03 ********************************************
     C                   eval      COMAD1 = scmad1
     C                   eval      COMAD2 = scmad2
     C                   eval      COMCTY = scmcty
     C                   eval      COMST  = scmst
     C                   move      scmzp2        COMZIP
     C                   movel     scmzp1        COMZIP
     C                   eval      COFEIN = scfein
     C                   eval      CO1099 = sc1099
     C                   eval      COSTAT = *blanks
     C                   eval      COAREA = scarea
     C                   eval      COPHN# = scphn#
     C                   eval      COFARA = scfara
     C                   eval      COFAX# = scfphn
     C                   eval      COPH1  = scph1
     C                   eval      COPH2  = scph2
     C                   eval      COCHK1 = scchk1
     C                   eval      COCHK2 = scchk2
     C                   eval      CONACS = sccase
     C                   eval      COCOI  = sccoi
     C                   eval      COISO  = sciso
     C                   eval      COPFAR = scpfar
     C                   eval      COPFSC = scpfsc
     C                   eval      COCNAR = sccnar
     C                   eval      COARRE = scarre
     C                   eval      COTRDL = sctrdl
     C                   eval      COTCC  = sctcc
     C*** begin add *** 06/23/05 *******************************************
     C                   eval      cobflg = scbflg
     C*** end   add *** 06/23/05 *******************************************
     C*** begin add *** 08/01/11 *******************************************
     C                   eval      copflg = scpflg
     C*** end   add *** 08/01/11 *******************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   eval      CODBCD = scdbcd
     C                   eval      COPBCD = scpbcd
     C                   eval      COFBCD = scfbcd
     C                   eval      COTHRL = scthrl
     C                   eval      COTHRH = scthrh
     C***  End   Add  ***  06/24/05  **************************************
     C*** Beg Add *** 10/20/05 **************************************
     C                   eval      COSRPL = scsrpl
     C                   eval      COSRPT = scsrpt
     C*** End Add *** 10/20/05 **************************************
     C*** beg add ***** 07/10/06 ************************************
     C                   eval      COCART = sccart
     C*** end add ***** 07/10/06 ************************************
     C*** beg add ***** 10/12/07 ************************************
     C                   eval      copss# = scpss#
     C*** end add ***** 10/12/07 ************************************
     C*** beg add ***** 04/19/10 *******************************************
     C                   eval      cockfe = scckfe
     C*** end add ***** 04/19/10 *******************************************
     C*** BEG ADD *** 03/13/08 ********************************************
     C                   EVAL      COARSQ = SCARSQ
     C*** END ADD *** 03/13/08 ********************************************

      ******* BEGIN ADD ********* 02/22/08 ********************************
     C                   eval      COPH1A = scph1A
      ******* END ADD *********** 02/22/08 ********************************
      ******* Beg ADD *********** 11/26/08 ********************************
     C                   Move      Scorfbs       Corfbs
      ******* End ADD *********** 11/26/08 ********************************
      *** beg add ***** 06/25/09 ******************************************
     c                   eval      coarcd = scarcd
     c                   eval      cosycd = scsycd
     c                   eval      coecar = scecar
     c                   eval      co1aar = sc1aar
     c                   eval      coifar = scifar
     c                   eval      co2aar = sc2aar
     c                   eval      cosfar = scsfar
     c                   eval      co3aar = sc3aar
     c                   eval      comfar = scmfar
     c                   eval      co4aar = sc4aar
     c                   eval      cotxar = sctxar
     c                   eval      co5aar = sc5aar
     c                   eval      co6aar = sc6aar
     c                   eval      co7aar = sc7aar
     c                   eval      coachl = scachl
     c                   eval      coacht = scacht
      *** end add ***** 06/25/09 ******************************************
      ******* Beg add *********** 07/14/09 ********************************
     C                   MoveL     scnmad        conmad
      ******* End add ***********07/14/09 *********************************
      *** beg add ***** 12/02/09 ******************************************
     c                   eval      COFYSM = scfysm
      *** end add ***** 12/02/09 ******************************************
     ****** beg add ***** 12/06/10 ********************************************
     C                   eval      coanam = scanam
     ****** end add ***** 12/06/10 ********************************************
      *** beg add ***** 05/27/13 ******************************************
      /free
       COpltr = scDivPlnLt;
       COprth = scDivPrmTh;
       //***** Beg Add ****** 07/28/14 ************************************
       COcapd  = scActDart;
       //***** End Add ****** 07/28/14 ************************************
       //***** Beg Add ****** 04/08/15 ************************************
       COERSD = scERSd;
       //***** End Add ****** 04/08/15 ************************************
       //***** Beg Add ****** 09/17/15 ************************************
       COMaintIDt = scMaintIDt;
       COMaintNew = scMaintNew;
       //***** End Add ****** 09/17/15 ************************************
       //*** beg add ***** 07/25/16 ************************
       CONetID = scNetID;
       //*** end add ***** 07/25/16 ************************
       //*** begin add *** 09/15/16 ************************
       COSLogo = scSLogo;
       //*** end add ***** 09/15/16 ************************
       //*** begin add *** 12/06/16 ************************
       COSPath = scSLogoPth;
       //*** end add ***** 12/06/16 ************************
      /end-free
      *** end add ***** 05/27/13 ******************************************
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr

     C***  Begin Chg  ***  06/24/05  **************************************
     c******inkj         caseq     *on           kjdo
     c     *inkj         caseq     *on           kjdo                     90
     C***  End   Chg  ***  06/24/05  **************************************
     c                   end

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*
     C***  Begin Add  ***  04/13/07  **************************************
     C                   if        WTABLopen = *on
     C                   close     WTABL
     C                   eval      WTABLopen = *off
     C                   endif
     C***  End   Add  ***  04/13/07  **************************************
     C***  Begin Add  ***  12/08/07  **************************************
     C                   if        WMRSLopen = *on
     C                   close     WMRSL
     C                   eval      WMRSLopen = *off
     C                   endif
     C***  End   Add  ***  12/08/07  **************************************
     C*
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C* Set Indicators for Products that are Installed-CHECK SYSTEM
     C* CONTROL FILE
     C*   0 = NON-DISPLAY     1=DISPLAY
     C*
     C                   setoff                                       50  51
     C*
     C                   move      'SY'          kkcntl
     C     syky1         chain     ssy01                              60
     C*
     C                   if        *in60
     C                   seton                                        50
     C*
     C                   else
     C*
      *** beg chg ***** 06/25/09 ******************************************
     C*****sypcif        ifeq      'Y'
     C     #sypcif       ifeq      'Y'
      *** end chg ***** 06/25/09 ******************************************
     C                   seton                                        50
     C                   seton                                        51
     C                   end
     C*
      *** beg chg ***** 06/25/09 ******************************************
     C*****sywcif        ifeq      'Y'
     C     #sywcif       ifeq      'Y'
      *** end chg ***** 06/25/09 ******************************************
     C                   seton                                        50
     C***  Begin Add  ***  04/13/07  **************************************
     C                   eval      WTABLopen = *on
     C                   open      WTABL
     C***  End   Add  ***  04/13/07  **************************************
     C***  Begin Add  ***  12/08/07  **************************************
     C                   eval      WMRSLopen = *on
     C                   open      WMRSL
     C***  End   Add  ***  12/08/07  **************************************
     C                   end
     C*
     C                   end

     *** beg add ***** 07/25/16 ***********************************************
      /free
       isRelayMod = isModEnabled('RelaySMS');
      /end-free
     *** end add ***** 07/25/16 ***********************************************
     C*
     *** beg add ***** 06/25/09 ***********************************************
     C                   move      'WC'          kkcntl
     C     syky1         chain     swc01
     c                   if        #WCFL24 = 'Y'
     c                   move      *on           *in36
     c                   else
     c                   move      *off          *in36
     c                   endif
     *** end add ***** 06/25/09 ***********************************************
     C*
     ****** beg chg ***** 04/20/03 ********************************************
     c     @@mode        comp      'A'                                    40
     c     @@mode        comp      'D'                                    09
     ****** end chg ***** 04/20/03 ********************************************
      ***** BEGIN ADD ******   04/09/07  ***********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID

     C                   IF        CLTID = '109'
     C                   EVAL      *IN55 = *ON
     C                   ENDIF
      ***** END  ADD  ******   04/09/07  ***********************************
     C*** BEGIN ADD ****** 12/06/10 ************************************
     C                   IF        CLTID = '025'
     C                   EVAL      *IN39 = *ON
     C                   ENDIF
     C*** END   ADD ****** 12/06/10 ************************************
      ****** beg add ***** 02/04/14 ************************************
      * If Client CSI display:
      /free
       visbCSIOPT = *Off;
       //*** Begin Chg *** 04/30/19 ************************************
       //*** If cltID = '114';
       If cltID = '114' Or cltID = '152';
       //*** End   Chg *** 04/30/19 ************************************
          visbCSIOPT = *On;
       Endif;
      /end-free
      ****** end add ***** 02/04/14 ************************************
       //*** begin add *** 09/15/16 ************************
      /free
       Clear isScript;

       isScript = isModEnabled('Scriptura-Documents')
        or isModEnabled('Scriptura-Various')
        or isModEnabled('3rd Party XML');

       tipSLogo = 'The output logo file name must have a valid image ' +
        'file name and extension.';
       //*** begin add *** 12/06/16 ************************
       tipSLogoPt = 'The output logo path must contain a valid file path ' +
        'and begin/end with a ''/'' character.';
       //*** end add ***** 12/06/16 ************************

      /end-free
       //*** end add ***** 09/15/16 ************************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   move      Q1USER        COUSER
     C                   move      Q1PGM         COPGM
     C                   time                    DSDTTM
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         CODT
     C                   z-add     dstime        COTIME
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   move      *zero         COBUL1
     C                   move      *zero         COBUL2
     C                   move      *zero         COBUL3
     C                   move      *zero         COBUL4
     C                   move      *zero         COBUL5
     C                   move      Q1USER        COUSER
     C                   move      Q1PGM         COPGM
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         CODT
     C                   z-add     dstime        COTIME
     C                   movel     srdto         COIDT
     C                   move      Q1USER        COIUSR
     C                   write     dbrcdf
     C                   feod      smcop
     C                   endsr
