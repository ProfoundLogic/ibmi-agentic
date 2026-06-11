     h copyright('Fiserv ISD, 2005')
     h*/TITLE SRCKES - API - CHECK IF "ALL STATES" ENDORSEMENT APPROPRIATE
     h*                     FOR POLICY COVERAGE
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
      *********************************************************************************
     h* PROGRAM DESCRIPTION:                                                          *
      *                                                                               *
     h* Take Company, Group, Employer, Policy Year, Endorsement Form as               *
     h* input, SRCKES will return "Y" if endorsement form may be attached for states  *
     h* coverage on policy.Otherwise SRCKES will return "N".                          *
     h*                                                                               *
     h* CALLED BY:                                                                    *
      *                                                                               *
     h* CALLS TO: NONE                                                                *
      *                                                                               *
     h* CREATED: 11/01/07                                                             *
      *                                                                               *
     h* 11/01/07 C:184 PROJECT: WC1530                                                *
      *********************************************************************************
      * Revisions:                                                                    *
      *                                                                               *
      * 12/13/19 C:291 Project: WC2693                                                *
      *                Add Date control logic for Endorsement State                   *
      *                Includsion/Exclusion                                           *
      *                                                                               *
     * 08/13/09 C:190 Project 135WNB                                                 *
      *                SRCKES is changed to optionally accept division  as a          *
      *                 parameter.                                                    *
      *                                                                               *
     * 06/05/08 C:197 Project 1530 Priss#6                                           *
     *              - Endorsements - Generic end Approved/Excluded States            *
     *                                                                               *
      *********************************************************************************
      * Endorsement States - Approved
     fWTESL     if   e           k disk
      * Employer Detail File
     fWDELL1    if   e           k disk
      * Rate set code master file
     fWMRSL     if   e           k disk
      *================================================================================
      * Definitions
      *================================================================================
     d @@co#           s              3  0
     d @@fnd           s              3  0
     d @@emp#          s              9  0
     d @@fyr           s              3  0
     d @@form          s             15
     d @@attch         s              1
     d kkco#           s              3  0
     d kkfnd           s              3  0
     d kkemp#          s              9  0
     d kkfyr           s              3  0
     d kkform          s             15
     d Ae_flag         s              1
    *** beg add ******* 08/13/09 ******************************************
     d @@div           s              5  0
     d kkdiv           s              5  0
     d pdsds         esds                  extname(szq1p)
    *** end add ******* 08/13/09 ******************************************
     d********** state_array     s              2    DIM(500)
     d state_array     s              2    DIM(100)
    *** END CHG ******* 06/05/08 ******************************************
     d*** Beg Add ***** 12/13/19 *******************************************
     d sysDat          s              7  0
     d/copy sprdate
     d*** End Add ***** 12/13/19 *******************************************
      *================================================================================
      * *entry
      *================================================================================
     c     *entry        plist
     c                   parm                    @@co#
     c                   parm                    @@fnd
     c                   parm                    @@emp#
     c                   parm                    @@fyr
     c                   parm                    @@form
     c                   parm                    @@attch
    *** beg add ******* 08/13/09 ******************************************
     c                   parm                    @@div
    *** end add ******* 08/13/09 ******************************************
      *================================================================================
      * KLIST's
      *================================================================================
     c     esky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkform
      *
     c     esky1         klist
     c                   kfld                    kkco#
     c*                  kfld                    kkfnd
     c                   kfld                    kkform
     c                   kfld                    rsjrst
      *
     c     elky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkemp#
     c*
    *** beg add ******* 08/13/09 ******************************************
     c     elky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c*
    *** end add ******* 08/13/09 ******************************************
      *
     c     rsky          klist
     c                   kfld                    kkco#
     c                   kfld                    elrtcd
      *
      *================================================================================
      * Main
      *================================================================================
     c                   move      @@co#         kkco#
     c                   move      @@fnd         kkfnd
     c                   move      @@emp#        kkemp#
     c                   move      @@fyr         kkfyr
     c                   move      @@form        kkform
    *** beg add ******* 08/13/09 ******************************************
     c                   if        q1prms >= 7
     c                   movel     @@div         kkdiv
     c                   move      *on           *in71
     c                   else
     c                   move      *off          *in71
     c                   endif                                                  q1prms >= 7
    *** end add ******* 08/13/09 ******************************************
     c                   Eval      @@attch = 'N'
      *
      *** Beg Add ***** 12/13/19 *************************************
     c  n71elky          chain     pel01                              61
     c   71elky1         chain     pel01                              61
      *** End Add ***** 12/13/19 *************************************
    *** BEG ADD ******* 06/05/08 ******************************************
     c* Clear State Array
      *
     c                   clear                   state_array
    *** BEG ADD ******* 06/05/08 ******************************************
      * Chain endorsement states - approved table
      *
     c     esky          chain     pes01                              60
     c*
     c                   if        *in60 = *off
     c*Set flag indicating
     c                   eval      ae_flag = esapex
     c*
     c                   z-add     1             i                 3 0
     c     esky          chain     pes01                              60
     c                   Dow       *in60 = *off
     c*
      //*** Beg Add ***** 12/13/19 *************************************
      /Free
        sysDat = DateTo7(SystemDate);
        If ESexdt = 0;
           ESexdt = *hival;
        Endif;
        If ESaDte <= ELpfDt and ELpfDt <= ESexDt;
      /End-Free
      //*** End Add ***** 12/13/19 *************************************
     c*
     c                   if        state_array(i) = ' '
     c                   eval      state_array(i) = esst
     c                   eval      i=i+1
     c                   endif
      //*** Beg Add ***** 12/13/19 *************************************
      /Free
        Endif;
      /End-Free
      //*** End Add ***** 12/13/19 *************************************
     c     esky          reade     pes01                                  60
     c                   Enddo
     c*
     c                   Eval      @@attch = 'N'
     c*
     c* Read Employer Detail File
     c*
    *** beg chg ******* 08/13/09 ******************************************
     c*
     c  n71elky          setll     pel01
     c  n71elky          reade     pel01                                  61
     c   71elky1         chain     pel01                              61
     c*
     c**** elky          setll     pel01
     c**** elky          reade     pel01                                  61
    *** end chg ******* 08/13/09 ******************************************
     c                   Dow       *in61 = *off
     c*
     c* Chain Rate set code master file
     c*
     c     rsky          chain     prs01                              62
     c                   If        *in62 = *off
     c*
     c* Chain endorsement states - approved table
     c*
     c                   EVAL      I=1
     c     rsjrst        lookup    state_array(i)                         60
     c*
     c                   if        Ae_flag = 'A' and *in60 = *on
     c                   Eval      @@attch = 'Y'
     c                   Leave
     c                   endif
     c*
     c                   if        Ae_flag = 'E' and *in60 = *off
     c                   Eval      @@attch = 'Y'
    *** BEG ADD ******* 06/05/08 ******************************************
     c                   Leave
    *** END ADD ******* 06/05/08 ******************************************
     c                   endif
     c*
     c                   EndIf
     c*
     c* Read next Employer Detail File
     c*
    *** beg chg ******* 08/13/09 ******************************************
     c  n71elky          reade     pel01                                  61
     c   71              move      *on           *in61
     c**** elky          reade     pel01                                  61
    *** end chg ******* 08/13/09 ******************************************
     c                   Enddo
     c*
     c                   else
     c*
     c                   Eval      @@attch = 'Y'
     c*
     c                   endif
      *
     c                   eval      *inlr = *on
     c                   return
