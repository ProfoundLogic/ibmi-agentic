     H Copyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H bnddir('WTBNDDIR')
    H*********************************************************************
    H/TITLE - Rollover Endorsements
    H*********************************************************************
    H*  DESCRIPTION:  Rolls Waiver of Subrogation and Designated         *
    H*                Excluded Workplace to the new year.                *
    H*                                                                   *
    H*  CALLED FROM: W7810C2                                             *
    H*                                                                   *
    H*  CALLS TO: None                                                   *
    H*                                                                   *
    H*  CREATED: 10/22/08      C:136     PROJECT: 46WFY                  *
    H*********************************************************************
    H* REVISIONS:                                                        *
     H*                                                                   *
     H* 05/10/24 C:079 Ticket: 115133-133                                 *
     H*                Update WVEMST                                      *
     H*                                                                   *
     H* 07/18/22 C:33  Support: 94693                                     *
     H*                Use standardized waiver routine.                   *
      *                                                                   *
      * 02/14/20 C:136 Support: 80310                                     *
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *!!! ALL THE OBJECTS FOR THIS SUPPORT ITEM MUST GO TOGETHER  !!!!!!!*
      *!!! AND FIX80310 MUST BE RUN.                               !!!!!!!*
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *                Add support for state waivers to renew.            *
      *                                                                   *
     H* 11/13/18 C:180 Project 133WQY                                     *
     H*                Roll over endorsements WC420601 and WC990302       *
     H*                                                                   *
     H* 04/23/18 C:136 Support: 64561                                     *
     H*                Populate WVEMST (Endorsement Form State) from      *
     H*                rating state if previous year's value was blank    *
     H*                                                                   *
     H* 11/10/16 C:136 Project: SY371                                     *
     H*                Scriptura Phase 2 - add extended description       *
     H*                                                                   *
     H* 03/07/16 C:136 PhaseWare 39815                                    *
     H*                Write the waiver form record if a waiver detail    *
     H*                record is encountered without an associated waiver *
     H*                form record. Do not roll form or detail record     *
     H*                if Excluded.                                       *
     H*                                                                   *
     H* 01/27/11 C:061 support WA133  75390                               *
     H*                - Condition write of renewal WMMUP record to       *
     H*                  write of endorsement data renewal records.       *
     H*                                                                   *
     H* 04/27/09 C:189 133WGG                                             *
     H*                    Update the Manual Endorsement Rollover process *
     H*                    to include option to roll Alternate Employer   *
     H*                    Endorsement.                                   *
    H*                                                                   *
     H* 03/16/09  C:190  Seq#: 66774                                      *
    H*                  Corrected Waivers auto rolling issue.            *
    H*                                                                   *
    H*********************************************************************
     fwtenl     if   e           k disk
     fwtesl     if   e           k disk    rename(pes01:pes00)
     fwdell4    if   e           k disk    rename(pel01 : pel04) prefix(el4)
     fwdell     if   e           k disk    rename(pel01 : pel00) prefix(el0)
     fwmmul     if   e           k disk    rename(pmu01 : pmu00)
     fwmmup     o    e             disk    rename(pmu01 : pmu0p)
     fwmwvl1    if   e           k disk
     fwmwvp     o    e             disk    rename(pwv01 : pwv0p)
     fwmewl1    if   e           k disk
     fwmewp     o    e             disk    rename(pew01 : pew0p)
     fwdell9    if   e           k disk    rename(pel01 : pel09) prefix(el4)
     fwdael     if   e           k disk
     fwdaep     o    e             disk    rename(pae01 : pae0p)
     fwledl     if   e           k disk
     fwledp     o    e             disk    rename(ped01 : ped0p)
     fwxfmp1    if a e           k disk
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d/Copy sPrSql
     d/copy sprdate
     d***  Begin Add  ***  07/18/22  ****************
     d/copy wPrEndorse
     d***  Begin Add  ***  07/18/22  ****************
     d/copy wpremplr
     d rollDEWs        pr
     d rollDEEs        pr
     d CanRollForm     pr              n
     d  kkform                       10    const


    d*=============================================================================================
    d* Data Structures and Arrays
    d*=============================================================================================
     d psds          esds                  extname(szq1p)
     d wv              s             15    dim(#ofWV)  ctdata perrcd(1)
     d dew             s             15    dim(#ofDEW) ctdata perrcd(1)
     d dee             s             15    dim(#ofDEE) ctdata perrcd(1)

      /Free
       Dcl-Ds wvrArray Qualified Dim(20);
        dsState Char(2);
        dsForm Char(15);
        dsEditDate Packed(7:0);
        dsReplForm Char(15);
        dsReplDate Packed(7:0);
        dsAdopDate Packed(7:0);
       End-Ds;
      /End-Free
    d*=============================================================================================
    d* Stand Alone Fields - TOP
    d*=============================================================================================
     d kkco#           s                   like(el4elco#)
     d kkfnd           s                   like(el4elfnd)
     d kkdiv           s                   like(el4eldiv)
     d kkfrdt          s                   like(el4elptdt)
     d kktodt          s                   like(el4elptdt)
     d kkemp#          s                   like(el4elemp#)
     d xxfyr           s                   like(el4elfyr)
     d #ofWV           c                   4
     d #ofDEW          c                   1
     d #ofDEE          c                   1
     d #ofTXNMC        c                   5
     d newYear         s                   like(el4elfyr)
     d newpfdt         s                   like(el4elptdt)
     d kkform          s                   like(muform)
     d rateState       s              2
      /Free
       Dcl-S chrPrevForm Char(15);
       Dcl-S maxElements Packed(5) Inz(%Elem(wvrArray));
       Dcl-S svCo#  Like(WVco#);
       Dcl-S svFnd  Like(WVFnd);
       Dcl-S #OfRtvRows Like(maxElements);
      /End-Free
      //*** Beg Add 05/10/24 *****************************************
       dcl-s intStRptDiv Like(WVdiv);
      //*** End Add 05/10/24 *****************************************
    d*=============================================================================================
    d* KLISTS
    d*=============================================================================================
     c     enky          klist
     c                   kfld                    muco#
     c                   kfld                    mufnd
     c                   kfld                    muform


     c     el4ky         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     el4ky1        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfrdt

     c     el4ky2        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfrdt
     c                   kfld                    kkemp#

     c     el0ky         klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    el4elemp#
     c                   kfld                    kkdiv

     c     el0ky1        klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    el4elemp#
     c                   kfld                    kkdiv
     c                   kfld                    el4elfyr

     c     el4ky9        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfrdt

     c     el4ky09       klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
    d*=============================================================================================
    d* PLISTS
    d*=============================================================================================
     c     *entry        plist
     c                   parm                    qqco#             3
     c                   parm                    qqfnd             3
     c                   parm                    qqemp#            9
     c                   parm                    qqfrdt            8
     c                   parm                    qqtodt            8
     c                   parm                    qqmwsb            1
     c                   parm                    qqmdew            1
     c                   parm                    qqmdee            1
     c                   parm                    qqtxnmc           1
     c                   parm                    qqnctp            1


    c*=============================================================================================
      *  MAINLINE PROCESSING
    c*=============================================================================================
      /Free
       kkCo# = %Int(qqCo#);
       kkFnd = %Int(qqFnd);

       If qqMwSb = 'Y';
          RtvGroupWaiverForms();
       Endif;

       if qqemp# <> *zeros;
          kkEmp# = %Int(qqEmp#);
       endif;

       kkfrdt = dtCYMD(numDate(qqfrdt));
       kktodt = dtCYMD(numDate(qqtodt));

       Select;
       When qqemp# <> *Zeros;
          Setll (kkCo#:kkFnd:kkFrDt:kkEmp#) pel04;
       Other;
          Setll (kkCo#:kkFnd:kkFrDt) pel04;
       Endsl;

       Dou %Eof(WDELL4);
          Reade (kkCo#:kkFnd) pel04;

          If %Eof;
             Leave;
          Endif;

          Select;
          When el4elptdt > kktodt;
             Leave;
          When qqEmp# <> *Zeros And kkEmp# <> el4ELemp#;
             Leave;
          Endsl;

         //Ensure div has a renewal year
          Setgt (kkCo#:kkFnd:el4elemp#:el4eldiv:el4elfyr) pel00;
          Reade (kkCo#:kkFnd:el4elemp#:el4eldiv) pel00;
          If %Eof Or el0elpfdt = el0elptdt;
             Iter;
          Endif;

         //Ensure st rpt div has a renewal year
          kkdiv = GetStRptDv(el4elco#:el4elfnd:el4elemp#);
          Setgt (kkCo#:kkFnd:el4elemp#:kkdiv:el4elfyr) pel00;
          Reade (kkCo#:kkFnd:el4elemp#:kkdiv) pel00;
          If %Eof Or el0elpfdt = el0elptdt;
             Iter;
          Endif;

          newYear = el0elfyr;
          newpfdt = el0elpfdt;

          If qqmwsb = 'Y';
             RollWvOfSubs();
          Endif;

          If qqmdew = 'Y';
             RollDEWs();
          Endif;

          If qqmdee = 'Y';
             RollDEEs();
          Endif;

          If qqtxnmc = 'Y';
             RollWC420601();
          Endif;

          If qqnctp = 'Y';
             RollWC990302();
          Endif;

       Enddo;

       *Inlr = *On;
       //=============================================
       Dcl-Proc SelectWaiverForm;
       //=============================================
       Dcl-Pi *N Char(15);
        inState Char(2);
       End-Pi;

       Dcl-S ix Packed(2);

       chrPrevForm = *Blanks;
       For ix = 1 to #OfRtvRows;
          If newPfDt < wvrArray(ix).dsEditDate;
             Iter;
          EndIf;
          If wvrArray(ix).dsAdopDate > newPfDt;
             Iter;
          EndIf;
         // Check to see if endorsement is replaced and valid for renewal year
          If wvrArray(ix).dsReplDate <> 0
           And wvrArray(ix).dsReplDate <= newPfDt;
             Chain (kkCo#:kkFnd:wvrArray(ix).dsReplForm) pen01;
             If Eneddt > newPfDt;
                Iter;
             Else;
                chrPrevForm = wvrArray(ix).dsForm;
                wvrArray(ix).dsForm  = wvrArray(ix).dsReplForm;
                wvrArray(ix).dsState = ENst;
             Endif;
          EndIf;
         // Check for state specific forms
          Select;
          When wvrArray(ix).dsState = *Blanks;
          When wvrArray(ix).dsState = inState;
             Return wvrArray(ix).dsForm;
          Other;
             Iter;
          EndSl;
         // Check for excluded states if waiver is not state specific
          Chain (kkCo#:wvrArray(ix).dsForm:inState) pes00;
          Select;
          When Not(%Found);
          When ESaDte > newPfDt;
             Iter;
          When ESexdt <> 0 And ESexdt < newPfDt;
             Iter;
          When ESapex = 'E';
             Iter;
          EndSl;
          Return wvrArray(ix).dsForm;
       EndFor;

       Return *Blanks;
       End-Proc;
      /End-Free
     p*===========================================================================
     p* rollWvOfSubs - Roll Waiver of Subrogation Endorsements
     p*===========================================================================
     p rollWvOfSubs    b
     d rollWvOfSubs    pi
     d kkfyr           s                   like(wvfyr)
     d wav#            s                   like(wvwav#)
     d x               s              3  0
     d #ofElem         s              3  0
     d chrRnwWvForm    s             15

      /Free
       //***  Begin Add  ***  07/18/22  *****************
       RenewWaivers(el4ELco#:el4ELfnd:newYear:el4ELfyr:el4ELemp#
        :%char(el4ELdiv));
       Return;
       //***  End   Add  ***  07/18/22  *****************
       // Skip this division if waiver detail already exists in renewal year
       Chain (el4ELco#:el4ELfnd:el4ELemp#:el4ELdiv:newYear) pwv01;
       If %Found;
          Return;
       Endif;
       // Get renewal year waiver form in case it changed.
       rateState =
        GetRatingState(el4ELco#:el4ELfnd:el4ELemp#:el4ELdiv:newYear);
       chrRnwWvForm = SelectWaiverForm(rateState);
       If chrRnwWvForm = *Blanks;
          Return;
       Endif;
       // Skip if the division waiver is excluded from renewal
       Chain (el4ELco#:el4ELfnd:el4ELemp#:el4ELdiv:
        el4ELfyr:chrRnwWvForm) pmu00;
       If %Found and MUexfl = 'E';
          Return;
       Endif;
       // Skip if the policy page waiver is exluded from renewal
       Chain (el4ELco#:el4ELfnd:el4ELemp#:kkDiv:
        el4ELfyr:chrRnwWvForm) pmu00;
       If %Found and MUexfl = 'E';
          Return;
       Endif;
       // Renew waiver detail
       wav# = 1;
       Setll (el4ELco#:el4ELfnd:el4ELemp#:el4ELdiv:el4ELfyr) pwv01;
       Dou %Eof(WMWVL1);
          Reade (el4ELco#:el4ELfnd:el4ELemp#:el4ELdiv:el4ELfyr) pwv01;
          If %Eof;
             Leave;
          Endif;
         // Write one policy page endorsement for each different waiver form
          kkForm = chrRnwWvForm;
          WrtWMMUP();
         // Write waiver detail for policy page endorsement
          WVfyr  = newYear;
          WVefdt = newPFDt;
          WVend# = 0;
          WVprt# = 0;
          WVwav# = wav#;
          wav#  += 1;
          WVuser = Q1user;
          WViusr = Q1user;
          WVpgm  = Q1pgm;
          WVipgm = Q1pgm;
          WVitm = TimeTo6(SystemTime);
          WVidt  = DateTo7(SystemDate);
          WVdt   = WVidt;
          WVtime = WVitm;
          //*** Beg Chg *** 05/10/24 ***************************************
          //***WVemst = rateState;
          intStRptDiv = GetStRptDv(WVco#:WVfnd:WVemp#);
          WVemst = GetRatingState(WVco#:WVfnd:WVemp#:intstRptDiv:
           WVfyr);
          //*** End Chg *** 05/10/24 ***************************************
          WVenm = chrRnwWvForm;
          Write pwv0p;
       Enddo;
      /End-Free

     p rollWvOfSubs    e
     p*===========================================================================
     p* rollDEWs - Roll Designated Excluded Workplace Endorsements
     p*===========================================================================
     p rollDEWs        b
     d rollDEWs        pi
     d x               s              3  0
     d #ofElem         s              3  0
     d wpl#            s                   like(ewwpl#) inz(99999)
     d kkfyr           s                   like(ewfyr)

     c     muky          klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    el4elemp#
     c                   kfld                    el4eldiv
     c                   kfld                    el4elfyr
     c                   kfld                    kkform

     c     newmuky       klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    el4elemp#
     c                   kfld                    el4eldiv
     c                   kfld                    newYear
     c                   kfld                    kkform

     c     ewky          klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    el4elemp#
     c                   kfld                    el4eldiv
     c                   kfld                    kkfyr

      *
      *  Only roll workplace detail if no records exist in the new year.
      *
     c                   eval      kkfyr = newYear
     c     ewky          chain     pew01
     c                   if        not(%found)
      /free
       If CanRollForm('WC000302') = *off;
          Return;
       Endif;
      /end-free
     c                   eval      wpl# = 1
     c                   eval      kkfyr = el4elfyr
     c     ewky          setll     pew01
     c     ewky          reade     pew01
     c                   dow       not(%eof)
     c                   eval      ewfyr  = newYear
     c                   eval      ewwpl# = wpl#
     c                   eval      ewefdt = newpfdt
     c                   add       1             wpl#
     c                   eval      ewuser = q1user
     c                   eval      ewiusr = q1user
     c                   eval      ewpgm  = q1pgm
     c                   eval      ewipgm = q1pgm
     c                   eval      ewtime  = TimeTo6(SystemTime)
     c                   eval      ewidt   = DateTo7(SystemDate)
     c                   eval      ewdt    = ewidt
     c                   eval      ewitm   = ewtime
     c                   write     pew0p

     c                   eval      kkform = 'WC000302'
     c                   callp     wrtWMMUP
     c     ewky          reade     pew01
     c                   enddo

     c                   else
     c                   eval      kkform = 'WC000302'
     c                   callp     wrtWMMUP

     c                   endif

     p rollDEWs        e

     p*===========================================================================
     p* rollDEEs - Roll Alternate Employer Endorsement Rollover
     p*===========================================================================
     p rollDEEs        b
     d rollDEEs        pi
     d x               s              3  0
     d #ofElem         s              3  0
     d kkfyr           s                   like(aefyr)

     c     muky          klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    el4elemp#
     c                   kfld                    el4eldiv
     c                   kfld                    el4elfyr
     c                   kfld                    kkform

     c     newmuky       klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    el4elemp#
     c                   kfld                    el4eldiv
     c                   kfld                    newYear
     c                   kfld                    kkform

     c     aeky          klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    kkfyr
     c                   kfld                    el4elemp#
     c                   kfld                    el4eldiv
      *
      *  Only roll workplace detail if no records exist in the new year.
      *
     c                   eval      kkfyr = newYear
     c     aeky          chain     pae01
     c                   if        not(%found)
      /free
       If CanRollForm('WC000301A') = *off;
          Return;
       Endif;
      /end-free
     c                   eval      kkfyr = el4elfyr
     c     aeky          setll     pae01
     c     aeky          reade     pae01
     c                   dow       not(%eof)
     c                   eval      aefyr  = newYear
     c                   eval      aeefdt = newpfdt
     c                   eval      aeuser = q1user
     c                   eval      aeiusr = q1user
     c                   eval      aepgm  = q1pgm
     c                   eval      aeipgm = q1pgm
     c                   eval      aetime  = TimeTo6(SystemTime)
     c                   eval      aeidt   = DateTo7(SystemDate)
     c                   eval      aedt    = aeidt
     c                   eval      aeitm   = aetime
     c                   write     pae0p

     c                   eval      kkform = 'WC000301A'
     c                   callp     wrtWMMUP
     c     aeky          reade     pae01
     c                   enddo

     c                   else
     c                   eval      kkform = 'WC000301A'
     c                   callp     wrtWMMUP

     c                   endif

     p rollDEEs        e
     p*===========================================================================
     p* RollWC420601 - Roll TX Notice of Material Changes
     p*===========================================================================
     p RollWC420601    b
     d RollWC420601    pi
     d kkfyr           s                   like(el4elfyr)
      /Free
       kkfyr = newYear;
       Setll (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr) pmu00;
       Reade (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr) pmu00;
       Dow not %eof(wmmul);
          If %subst(MUform:1:8) = 'WC420601' and MUexfl <> 'E';
             kkform = MUform;
             Chain (el4elco#:el4elfnd:el4elemp#:el4eldiv:kkfyr:kkform) wledl;

             If not %found(wledl);
                Setll (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr:kkform)
                       ped01;
                Reade (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr:kkform)
                      ped01;
                Dow not %eof(wledl);
                   FMfyr  = kkfyr;
                   FMuser = Q1user;
                   FMpgm  = Q1pgm;
                   FMdt   = DateTo7(SystemDate);
                   FMtime = TimeTo6(SystemTime);
                   FMiusr = Q1user;
                   FMipgm = Q1pgm;
                   FMidt  = DateTo7(SystemDate);
                   Write Ped0p;
                   Reade (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr:kkform)
                         ped01;
                Enddo;
             Endif;
             WrtWMMUP();
          Endif;
          Reade (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr) pmu00;
       Enddo;

      /End-Free

     p RollWC420601    e
     p*===========================================================================
     p* RollWC990302 - Roll Notice of Cancellation to Third Parties
     p*===========================================================================
     p RollWC990302    b
     d RollWC990302    pi
     d kkfyr           s                   like(el4elfyr)
      /Free
       kkfyr = newYear;
       Setll (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr) pmu00;
       Reade (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr) pmu00;
       Dow not %eof(wmmul);
          If %subst(muform:1:8) = 'WC990302'  and MUexfl <> 'E';
             kkform = muform;
             WrtWMMUP();
          Endif;
          Reade (el4elco#:el4elfnd:el4elemp#:el4eldiv:el4elfyr) pmu00;
       Enddo;

      /End-Free

     p RollWC990302    e

       //=============================================
       Dcl-Proc RtvGroupWaiverForms;
       //=============================================
       // Populate relevant group waiver form info into wvrArray
       Exec Sql
        Declare EN1 Cursor For
        Select ENst, ENform, ENeddt, ENrpfm, ENrpdt, ENadte
         From WTENP
         Where ENco# = :kkCo#
          And ENfnd  = :kkFnd
          And ENtype = 'Waiver'
         Order By ENst Desc
         For Read Only;

       Exec Sql Open EN1;
       Exec Sql
        Fetch Next
        From EN1
        For :maxElements Rows
        Into :wvrArray;
       If Not IsValidFetch(sqlStt);
       Endif;
       #OfRtvRows = SQLER3;
       Exec Sql Close EN1;

       End-Proc;
     p*===========================================================================
     p* wrtWMMUP - Write WMMUP record to renewal year
     p*===========================================================================
     p wrtWMMUP        b
     d wrtWMMUP        pi

      // st rpt div
     c     newmuky       klist
     c                   kfld                    el0elco#
     c                   kfld                    el0elfnd
     c                   kfld                    el0elemp#
     c                   kfld                    kkDiv
     c                   kfld                    newYear
     c                   kfld                    kkform

     c     muky          klist
     c                   kfld                    el4elco#
     c                   kfld                    el4elfnd
     c                   kfld                    el4elemp#
     c                   kfld                    el4eldiv
     c                   kfld                    el4elfyr
     c                   kfld                    kkform


      /FREE
       chain newmuky pmu00;
       if not(%found);
          chain muky pmu00;
          if %found;
             MUdiv  = el0eldiv;
             mufyr  = newYear;
             mueedt = newpfdt;
             muend# = 0;
             muuser = q1user;
             muiusr = q1user;
             mupgm  = q1pgm;
             muipgm = q1pgm;
             mutime  = TimeTo6(SystemTime);
             muidt   = DateTo7(SystemDate);
             mudt    = muidt;
             write pmu0p;
          else;
             clear pmu0p;
             muco#  = el0elco#;
             mufnd  = el0elfnd;
             muemp# = el0elemp#;
             mudiv  = el0eldiv;
             mufyr  = el0elfyr;
             muend# = 0;
             museq# = 0;
             muform = kkform;
             chain enky pen01;
             if %found;
                muenst = enst;
                mudesc = endesc;
                MUdsc2 = ENdsc2;
                mumnau = enmnau;
                muprnt = enprnt;
             endif;
             mueedt = el0elpfdt;
             muexfl = 'I';
             mumnau = 'M';
             mustat = 'A';
             muuser = q1user;
             muiusr = q1user;
             mupgm  = q1pgm;
             muipgm = q1pgm;
             mutime  = TimeTo6(SystemTime);
             muidt   = DateTo7(SystemDate);
             mudt    = muidt;
             write pmu0p;
          endif;
          If chrPrevForm <> *Blanks;
             kkForm = chrPrevForm;
          EndIf;
          Chain (el4ELco#:el4ELfnd:el4ELfyr:el4ELemp#:
           el4ELdiv:kkForm) pfm01;
          If %Found;
             F3form = MUform;
             F3div  = el0eldiv;
             F3fyr  = newYear;
             F3eedt = newPFDt;
             F3end# = 0;
             F3iusr = Q1user;
             F3ipgm = Q1pgm;
             F3idt  = MUdt;
             F3user = Q1user;
             F3pgm  = Q1pgm;
             F3dt   = MUdt;
             F3time = MUtime;
             Write pfm01;
          Endif;
       endif;
      /END-FREE

     p wrtWMMUP        e


      *===========================================================================
      * Returns *on if form is OK to renew
      *===========================================================================
     p CanRollForm     b
     d CanRollForm     pi              n
     d  kkForm                       10    const
      /free
       Chain (el4elco# : el4elfnd : el4elemp# :
              el4eldiv : el4elfyr : kkform) pmu00;
       Select;
       When not(%found);
          Return *on;
       When MUEXFL = 'E';
          Return *off;
       Other;
          Return *on;
       Endsl;
      /end-free
     p CanRollForm     e
** WV (Possible Wavier Form Names)
WC000313
WC040306
WC430305
WC990306
** DEW (Possible DEW Form Names)
WC000302
** DEE (Possible Alternate Employer Endorsement Rollover forms)
WC000301A
