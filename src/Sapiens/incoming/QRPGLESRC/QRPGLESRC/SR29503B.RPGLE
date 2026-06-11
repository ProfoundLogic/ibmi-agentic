     * (C) Copyright Progressive Data Solutions, Inc., 2004
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR29503B - Retrospective Premium Endorsement
     *===============================================================*
     * WC290503B  Retrospective Premium Endorsement                  *
     *                One Year Rating Period                         *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 06/29/04                                          *
     * Programmer: C:61                                              *
     * Project   : 114WHO                                            *
     *===============================================================*
     * REVISIONS:                                                    *
      *                                                               *
     *  12/10/04 C:61  Support: WA114 44643                          *
      *                 Withdrawn by NCCI effective 01/01/05.         *
      *                                                               *
     *  06/24/04 C:65  PROJECT: 114WHO                               *
     *                 Add processing to write endorsement data to   *
     *                 the WXPEP reporting file for PA/DE/NJ states. *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
    
     ** Copybook F specs
     F/COPY SCOPYEN01F
    
     * Billing Schedule
     fwtabl     if   e           k disk    include(pbs01)
    
      ** Employer Payroll
     Fwmepl7    if   e           k disk    include(pep01)
    
      ** Additional Work File
     Fwxfmp3    o    e             disk    usropn
     F*** begin add ***  06/24/04  *****************************************
     Fwxpel     uf   e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     f*** end add ***  06/24/04  *******************************************
    
    
     *=========================================================================
     *  Definition Specs
     *=========================================================================

     ** Copybook D specs
     D/COPY SCOPYEN01D
     D*** begin add ***  06/24/04  *****************************************
     D/COPY WWCPOLDAT4
     D*** end add *****  06/24/04  *****************************************
    
     *=========================================================================
     *  stand alone
     *=========================================================================
     d x1              s              2  0
     d total1          s             15  3
     d total2          s             15  3
     d total3          s             15  3
     d TaxMult         s             15  3
     d sysdat7         s                   like(epefdt)
     d svtax           s                   like(eltax)
     d svtaxf          s                   like(eltaxf)
     C*** begin add ***  06/24/04  *****************************************
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     C*** end add *****  06/24/04  *****************************************
    
    
    
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
    
     ** Form ID
     D qqform          c                   const('WC290503B ')
    
     ** Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
    
    
    
     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
    
    
     *=========================================================================
     *  KLIST definitions
     *=========================================================================

      * Employer Payroll

     c     epky          klist
     c                   kfld                    elco#
     c                   kfld                    elfnd
     c                   kfld                    elfyr
     c                   kfld                    elemp#
     c                   kfld                    eldiv

      * Billing Schedule

     c     bsky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxkey            10
     C*** begin add ***  06/24/04  *****************************************
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0
     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#
     C*** end add *****  06/24/04  *****************************************
    
    
    
     *=========================================================================
     *  Main
     *=========================================================================
    
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
    
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     c     othinf        begsr
    
     ** open work file
     c                   open      wxfmp3
    
     *    initialize
     c                   clear                   pfm03
     c                   movel(p)  xxform        fmform
    
     * get info from first NJ division
     c                   exsr      NJinfo
    
     * get Weighted Average Tax Multiplier
     c                   exsr      WeightAVE
    
     ** Write the output record
     C                   write     pfm01
     C                   write     pfm03
     *
     ** Close the endorsement form workfile
     C                   close     wxfmp3
     C*** begin add ***  06/24/04  *****************************************
      *
      * Update the endorsement reporting file with the proper data.
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   Z-ADD     0             xxunid
     C*
     c                   move      *hival        xxdt
     C     pekey         setgt     ppe02
     C     pekey1        readpe    ppe02                                  60
     C                   If        *in60 = *off
     C                   z-add     peunid        xxunid
     C                   endif
     C                   movel     xxform        x1end#
     c     pekey2        chain     ppe01                              60
      *
      * Copy data into the record.
      *
     C                   If        *in60 = *off
      *
     c                   clear                   E00503
      *
     c                   z-add     fmam02        xxllim
     c                   z-add     fma542        xxlcon
     c                   z-add     fma547        xxtxst
     c                   z-add     fma541        xxtxfe
     c                   z-add     fmam03        xxe100
     c                   z-add     fma544        xxm100
     c                   z-add     fma545        xxx100
     c                   z-add     fma546        xxb100
     c                   z-add     fma543        xxlfst
      *
     c                   z-add     0             XXLFFE
     c                   Z-ADD     0             XXRDF1
     c                   Z-ADD     0             XXRDF2
     c                   Z-ADD     0             XXRDF3
     c                   Z-ADD     0             XXM050
     c                   Z-ADD     0             XXM150
     c                   Z-ADD     0             XXX050
     c                   Z-ADD     0             XXX150
     c                   Z-ADD     0             XXB050
     c                   Z-ADD     0             XXB150
     c                   Z-ADD     0             XXE050
     c                   Z-ADD     0             XXE150
     c                   Z-ADD     0             XXREFD
      *
     c                   move      *blanks       pedata
     c                   movel     E00503        pedata
     c                   movel     q1user        peuser
     c                   movel     q1pgm         pepgm
     c                   z-add     dstime        petime
     C                   Eval      srdti = dsdate
     C                   Call      'SRCVTDAT'    dtcymd
     C                   MoveL     srdto         pedt
      *
     C                   update    ppe01
      *
     C                   endif                                                  *in60 ifeq off
      *
     C*** end add *****  06/24/04  *****************************************
    
     C                   endsr
    
    
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr

     c                   time                    dsdttm

     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         sysdat7

     C                   endsr
    
    
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
    
    
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
    
     C     rsky          chain     prs01                              60
    
     C                   if        *in60 = *off
     C                             and (rsjrst = 'NJ')
      ****** beg add ******* 12/10/04 ***********************************
     C                             and (elpfdt < 1050101)
      ****** end add ******* 12/10/04 ***********************************
    
     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60
    
     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))
     C                   move      *on           crtfrm
     C                   endif
    
     C                   endif
    
     C                   endsr
    
    
     *=========================================================================
     *  NJinfo  - Write data from 1st NJ division policy to work file      d
     *=========================================================================
     c     NJinfo        begsr
     c                   z-add     0             svtax
     c                   z-add     0             svtaxf
    
     c     elky2         Chain     pel02                              60
    
     c                   Dow       *in60 = *off
     c     rsky          Chain     prs01                              60
    
     c                   If        ((*in60 = *off) and (rsjrst = 'NJ'))
     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60
    
     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))
     *  Get 1st NJ division Info

     c                   z-add     eltax         fma547
     c                   z-add     eltax         svtax
     c                   z-add     eltaxf        svtaxf
     c                   z-add     ellcvf        fma542
     c                   z-add     elretn        fmam02
     c                   z-add     elelpf        fma543
     c                   z-add     elstpr        fmam03
     c                   z-add     elmnfc        fma544
     c                   z-add     elmxfc        fma545
     c                   z-add     elbsfc        fma546

     c                   leave
     c                   EndIf
    
     c                   EndIf
    
     c     elky2         ReadE     pel02                                  60
     c                   EndDo
    
     C                   endsr
    
    
    
     *=========================================================================
     *  WeightAVE - Get Weighted Average Tax Multiplier                    d
     *=========================================================================
     c     WeightAVE     begsr
     c                   z-add     0             total1
     c                   z-add     0             total2
     c                   z-add     0             total3
    
     *  Get totals
    
     c     elky2         Chain     pel02                              60
    
     c                   Dow       *in60 = *off
     c     rsky          Chain     prs01                              60
    
     c                   If        ((*in60 = *off) and (rsjrst = 'NJ'))
     c                   movel(p)  elbscd        xxkey
     C     bsky          chain     pbs01                              60
    
     C                   if        *in60 = *off
     C                             and ((bslpgm = 'P') or (bslpgm = 'I'))

      * get payroll totals
     C     epky          chain     pep01                              60
    
     c                   Dow       *in60 = *off

     c                   if        (epefdt <= sysdat7)
     c                   add       epempr        total1

     c                   if        (epspec <> 'F')
     c                   add       epempr        total2
     c                   else
     c                   add       epempr        total3
     c                   EndIf

     c                   EndIf

     c     epky          ReadE     pep01                                  60
     c                   EndDo

     c                   EndIf
    
     c                   EndIf
    
     c     elky2         ReadE     pel02                                  60
     c                   EndDo
    
     *  Get Weighted Average Tax Multiplier
    
     c                   select
    
    c                   when      (total3 = 0)
     c                   eval      TaxMult = svtax
    
    c                   when      (total1 = 0)
     c                   eval      TaxMult = (svtax + svtaxf)/2
    
    c                   other
     c                   eval      TaxMult = ((total2/total1)*svtax) +
    c                                       ((total3/total1)*svtaxf)
     c                   endsl
    
     c                   eval      fma541 = TaxMult
    
     c                   endsr
    
    
