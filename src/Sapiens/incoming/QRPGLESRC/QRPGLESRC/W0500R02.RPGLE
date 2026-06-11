     A*  (C) Copyright Fiserv, Inc., 2003 - 2008
     *================================================================
      *  DESCRIPTION:  Return Premium Batch Information               *
      *                                                               *
      * CALLED BY: W0500R                                             *
      *                                                               *
      * CALLS TO:  NONE                                               *
      *                                                               *
      * CREATED: 03/12/03  C:142 PROJECT WC1033                       *
     *================================================================
      * REVISIONS:                                                    *
      *                                                               *
      * 12/26/07 C:183 Project: 81WZB                                 *
      *                Added processing for new field on W0500F02,    *
      *                'Surplus Dividend Distribution'.               *
      *                                                               *
      * 07/18/07 C:136 Project: WC1491 (Independent Cash)             *
      *                Open up process for Independent Cash and       *
      *                Return Deposit                                 *
     *================================================================
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0500F02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwxrnp     uf a e             disk
     F                                     rename(prn01:dbrcdf)
     Fwxrnl     if   e           k disk
     Fwxrtl     uf   e           k disk
     Fwtabl     if   e           k disk
     F                                     ignore(prt01)
     Fwmfnl     if   e           k disk
      *** beg add ****** 07/18/07 *************************************
     Fswcpf     if   e             disk
      *** end add ****** 07/18/07 *************************************
     *================================================================
     *  /COPY required constants                                     *
     *================================================================
     D optnm           C                   'Return Batch Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     *----------------------------------------------------------------
     * Stand Alone Fields - TOP
     *----------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D ##lvl           s              3  0
     D ##type          s              1
     D ##outq          s             10
     D kkfnd           s              3  0
     *----------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *----------------------------------------------------------------
     C/COPY SCOPYMT01C
     *================================================================
     *  PLIST definitions                                            *
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     *
     *  Company file/kkfnd(group) retrieve program parameter list
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
     C     fnpl          plist
     C                   parm      *blank        qqkc              1
     C                   parm      *blank        qqkl              1
     C                   parm      '2'           qqcall            1
     C                   parm                    qqco#             3
     C                   parm      *blanks       qqfnd             3
     C                   parm      *blanks       qqfyr             3
     C                   parm      1             qqview            1 0
      *================================================================
     *  klist definitions                                            *
      *================================================================
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
      *
     C     rnky          klist
     C                   kfld                    ##co#
      *
     C     rtky          klist
     C                   kfld                    ##co#
     C                   kfld                    rnbat#
      *
     C     tbky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkkey            10
      *================================================================
     * CHGBPF - Change batch processing flag                         *
      *================================================================
     C     chgbpf        begsr

     C     @@rrn         chain     dbrcdf
     C                   eval      RNBPF = 'N'
     C                   time                    dsdttm
     C                   eval      rnuser = q1user
     C                   eval      rnpgm  = q1pgm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         rndate
     C                   eval      rntime = dstime
     C                   update    dbrcdf
     C                   seton                                        lr
     C                   return

     C                   endsr
      *** beg add **** 07/18/07 ***************************************
      *================================================================
     * CRTWDW - Display Create Window to get method and batch type   *
      *================================================================
     c     crtwdw        begsr

     c                   dou       *in90 = *off
     c                   exfmt     wdw01

     c                   select
     c                   when      *in36
     c                   if        wdmtyp <> 'A' and
     c                             wdmtyp <> 'M'
     c                   move      *on           *in71
     c                   move      *on           *in90
     c                   endif

     c                   if        wdmtyp =  'A' and
     c                             wdbtyp <> 'P' and
     c                             wdbtyp <> 'D'
     c                   move      *on           *in72
     c                   move      *on           *in90
     c                   endif

     c                   if        wdmtyp =  'M' and
     c                             wdbtyp <> 'A' and
     c                             wdbtyp <> 'D' and
     c                             wdbtyp <> 'I' and
     c                             wdbtyp <> 'P'
     c                   move      *on           *in73
     c                   move      *on           *in90
     c                   endif

     c                   when      not *in36
     c                   if        wdmtyp <> 'A' and
     c                             wdmtyp <> 'M'
     c                   move      *on           *in71
     c                   move      *on           *in90
     c                   endif

     c                   if        wdmtyp =  'A' and
     c                             wdbtyp <> 'P'
     c                   move      *on           *in72
     c                   move      *on           *in90
     c                   endif

     c                   if        wdmtyp =  'M' and
     c                             wdbtyp <> 'A' and
     c                             wdbtyp <> 'P'
     c                   move      *on           *in73
     c                   move      *on           *in90
     c                   endif

     c                   endsl

     c                   if        *inkl = *on
     c                   seton                                        lr
     c                   return
     c                   endif

     c                   enddo

     c                   eval      scmtyp = wdmtyp
     c                   eval      scbtyp = wdbtyp

     c                   if        scmtyp = 'M'
     c                   eval      scmdsc = 'Manual'
     c                   eval      *in42 = *on
     c                   else
     c                   eval      scmdsc = 'Auto'
     c                   endif

     c                   select
     c                   when      scbtyp = 'A'
     c                   eval      scbdsc = 'Account Current'
     c                   when      scbtyp = 'D'
     c                   eval      scbdsc = 'Deposit'
     c                   when      scbtyp = 'I'
     c                   eval      scbdsc = 'Independent Cash Distribution'
     c                   eval      *in41 = *on
     c                   when      scbtyp = 'P'
     c                   eval      scbdsc = 'Premium'
     c                   endsl

     c                   endsr
      *** end add **** 07/18/07 ***************************************
      *================================================================
     * DLTPRE - Pre-processing before delete occurs                  *
      *================================================================
     C     dltpre        begsr
      *
     C     rtky          chain     prt01
     C                   if        %found
     C                   dow       *in60 = *off
     C                   delete    prt01
     C                   reade     prt01                                  60
     C                   enddo
     C                   endif
      *
     C                   endsr
      *================================================================
     * DSPLYR - Processing after displaying information              *
      *================================================================
     C     dsplyr        begsr
     C                   endsr
      *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)                  *
      *================================================================
     C     errck1        begsr
      *
      *  Validate fund range
      **********
      *** beg del ***** 07/18/07 ***********************************************
     C**********         eval      kkfnd= Scffnd
     C*****fnky*         chain     Wmfnl
      **********
     C**********         if        Not %Found
     C**********         seton                                        71  90
     C**********         endif
     C**********         eval      kkfnd= Sctfnd
     C*****fnky*         chain     Wmfnl
      **********
     C**n90*****         if        Not %Found
     C**********         seton                                        72  90
     C**********         endif
     C**n90*****         if        Sctfnd < Scffnd
     C**********         seton                                        73  90
     C**********         endif
      **********
      ***Validate audit date range
      **********
     C**********         if        not *in90
     C**********         eval      srdti = scfaud
     C**********         call      'SRCVTDAT'    datchk
      **********
     C**********         if        srerr = *on
     C**********         seton                                        74  90
     C**********         else
     C**********         movel     srdto         scfaud
     C**********         endif
      **********
     C**********         endif
      **********
     C**********         if        not *in90
     C**********         eval      srdti = sctaud
     C**********         call      'SRCVTDAT'    datchk
      **********
     C**********         if        srerr = *on
     C**********         seton                                        75  90
     C**********         else
     C**********         movel     srdto         sctaud
     C**********         endif
      **********
     C**********         endif
      **********
     C**********         if        not *in90
      **********
     C**********         eval      srdti = scfaud
     C**********         call      'SRCVTDAT'    dtcymd
     C**********         movel     srdto         xxfaud            7 0
      **********
     C**********         eval      srdti = sctaud
     C**********         call      'SRCVTDAT'    dtcymd
     C**********         movel     srdto         xxtaud            7 0
      **********
     C**********         if        xxtaud < xxfaud
     C**********         seton                                        76  90
     C**********         endif
      **********
     c**********         endif                                                  not *in90
      **********
      **Validate minimum amount
      **********
     C**n90*****         if        scmin <= *zero
     C**********         seton                                        77  90
     C**********         endif
      **********
      **Validate manually create batch flag
      **********
     C**n90******        if        scmbat <> *blank and
     C***********                  scmbat <> 'Y'
     C***********        seton                                        78  90
     C***********        endif
      *** end del ***** 07/18/07 ***********************************************
      *** beg add ***** 07/18/07 ***********************************************
     c  n90              if        scbtyp = 'I'
     c                   if        scptyp <> 'A' and scptyp <> 'E'
     c                   seton                                        80  90
     c                   endif
     c                   endif
      *** end add ***** 07/18/07 ***********************************************
      *
      * Validate exclude contested audit flag
      *
     C  n90              if        sccafl <> *blank and
     C                             sccafl <> 'Y'
     C                   seton                                        79  90
     C                   endif
      *** beg add ***** 07/18/07 ***********************************************
      *
      *  Validate fund range
      *
     c                   if        scmtyp = 'A' or
     c                             scmtyp = 'M' and
     c                             scffnd <> 0

     c  n90              if        sctfnd < scffnd
     c                   seton                                        73  90
     c                   endif

     c                   endif                                                  scmtyp = 'A'
      *
      *  Validate audit date range
      *
     c                   if        scmtyp = 'A' or
     c                             scmtyp = 'M' and
     c                             scfaud <> 0

     c                   if        not *in90
     c                   eval      srdti = scfaud
     c                   call      'SRCVTDAT'    datchk

     c                   if        srerr = *on
     c                   seton                                        74  90
     c                   else
     c                   movel     srdto         scfaud
     c                   endif

     c                   endif

     c                   if        not *in90
     c                   eval      srdti = sctaud
     c                   call      'SRCVTDAT'    datchk

     c                   if        srerr = *on
     c                   seton                                        75  90
     c                   else
     c                   movel     srdto         sctaud
     c                   endif

     c                   endif

     c                   if        not *in90

     c                   eval      srdti = scfaud
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxfaud            7 0

     c                   eval      srdti = sctaud
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxtaud            7 0

     c                   if        xxtaud < xxfaud
     c                   seton                                        76  90
     c                   endif

     c                   endif                                                  not *in90

     c                   endif                                                  scmtyp = 'A'
      *
      * Validate minimum amount
      *
     c                   if        scmtyp = 'A' or
     c                             scmtyp = 'M' and
     c                             scmin  <> 0

     c  n90              if        scmin <= *zero
     c                   seton                                        77  90
     c                   endif

     c                   endif                                                  scmtyp = 'A'
      *** end add ***** 07/18/07 ***********************************************

     C                   endsr
      *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)                  *
      *================================================================
     C     errck2        begsr
     C                   endsr
      *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)                  *
      *================================================================
     C     errck3        begsr
     C                   endsr
     *================================================================
     * GETBAT# = Get next available batch number                     *
     *================================================================
     C     getbat#       begsr

     C                   seton                                        40
     C     rnky          setgt     prn01
     C     rnky          readpe    prn01
      *
     C                   if        not %eof
     C                   eval      scbat# = Rnbat# + 1
     C                   else
     C                   eval      scbat# = 1
     C                   endif
      *
      *** beg add ***** 07/18/07 **************************************
     c                   exsr      crtwdw
      *** end add ***** 07/18/07 **************************************
      *
     C                   endsr
      *================================================================
      * KJDO - Search routine                                         *
      *================================================================
     C     kjdo          begsr
      *
     C                   Move      ##CO#         qqco#
      *
      * FROM Group Search
      *
     C     HFLD          ifeq      'SCFFND'
     C                   call      'W0010R'      FNPL
     C                   if        qqfnd <> *blanks
     C                   move      qqfnd         SCFFND
     C                   seton                                        81  90
     C                   endif
     C                   endif
      *
      * TO Group Search
      *
     C     Hfld          ifeq      'SCTFND'
     C                   Move      ##CO#         qqco#
     C                   Call      'W0010R'      FNPL
     C                   if        qqfnd <> *blanks
     C                   move      qqfnd         SCTFND
     C                   seton                                        82  90
     C                   else
     C                   seton                                        81  90
     C                   endif
     C                   endif
      *
     C                   endsr
      *================================================================
     * MVDBSC - Move data base fields to screen fields               *
      *================================================================
     C     Mvdbsc        begsr

     C                   if        not *in40
     C                   eval      scbat# = RNBAT#
      *** beg add ***** 07/18/07 **************************************
     C                   eval      scmtyp = RNMTYP
     C                   eval      scbtyp = RNBTYP
     C                   eval      scptyp = RNPTYP

     c                   if        scmtyp = 'M'
     c                   eval      scmdsc = 'Manual'
     c                   else
     c                   eval      scmdsc = 'Auto'
     c                   endif

     c                   select
     c                   when      scbtyp = 'A'
     c                   eval      scbdsc = 'Auto'
     c                   when      scbtyp = 'D'
     c                   eval      scbdsc = 'Deposit'
     c                   when      scbtyp = 'I'
     c                   eval      scbdsc = 'Independent Cash Distribution'
     c                   eval      *in41 = *on
     c                   when      scbtyp = 'P'
     c                   eval      scbdsc = 'Premium'
     c                   endsl

      *** end add ***** 07/18/07 **************************************
     C                   endif

     C                   eval      scffnd = RNFGRP
     C                   eval      sctfnd = RNTGRP
      *
     C                   eval      srdti = RNFAUD
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scfaud
      *
     C                   eval      srdti = RNTAUD
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         sctaud
      *
     C                   eval      sccafl = RNCAFL
     C                   eval      scmin  = RNMIN
     C                   eval      sccom1 = RNCOM1
     C                   eval      sccom2 = RNCOM2
     C                   eval      sccom3 = RNCOM3
     C*** BEG ADD ***** 12/26/07 **************************************
     C                   EVAL      SCSDFL = RNSDFL
     C*** END ADD ***** 12/26/07 **************************************
      *
     C                   endsr
      *================================================================
     * MVSCDB - Move screen fields to data base fields               *
      *================================================================
     C     mvscdb        begsr
      *
     C                   eval      Rnbat# = Scbat#
      *** beg add ***** 07/18/07 **************************************
     c                   eval      RNMTYP = scmtyp
     c                   eval      RNBTYP = scbtyp
     c                   eval      RNPTYP = scptyp
      *** end add ***** 07/18/07 **************************************
     C                   eval      RNCO#  = ##co#
     C                   eval      Rnfgrp = Scffnd
     C                   eval      Rntgrp = Sctfnd
      *
     C                   eval      srdti = scfaud
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         RNFAUD
      *
     C                   eval      srdti = sctaud
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         RNTAUD
      *
     C                   eval      rncafl = sccafl
     C                   eval      Rnmin  = Scmin
     C                   eval      Rncom1 = Sccom1
     C                   eval      Rncom2 = Sccom2
     C                   eval      Rncom3 = Sccom3
      *
     C                   endsr
      *================================================================
     * OTHERR - Processing for other modes                           *
      *================================================================
     C     otherr        begsr
     C                   endsr
      *================================================================
     * OTHFKS - Processing for other functions keys                  *
      *================================================================
     C     othfks        begsr
     C     *inkj         caseq     *on           kjdo
     C                   endcs
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
      *** beg add ***** 07/18/07 *************************************
     c                   read      swcpf
     c     WCFL36        comp      'Y'                                    36
      *** end add ***** 07/18/07 *************************************
      *
     C     @@mode        caseq     'A'           getbat#
     C     @@mode        caseq     'N'           chgbpf
     C                   endcs
     C*** BEG ADD ***** 12/26/07 **************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3

     C     @@RRN         CHAIN     DBRCDF
     C                   IF        %FOUND AND CLTID = '081'
     C                             AND RNPTYP = 'E' AND RNBTYP = 'I'
     C                   EVAL      *IN31 = *ON
     C                   ENDIF                                                  %FOUND
     C*** END ADD ***** 12/26/07 **************************************
      *
     C                   endsr
     *================================================================
     * PREOTH - Pre-processing for other modes                       *
     *================================================================
     C     preoth        begsr
     C                   endsr
     *================================================================
     * UPDATR - Update data base record and after processing         *
     *================================================================
     C     updatr        begsr
      *
     C     @@rrn         chain     Dbrcdf
B01  C                   if        %Found
     C                   exsr      mvscdb
     C                   time                    dsdttm
     C                   eval      Rnuser = Q1user
     C                   eval      Rnpgm  = Q1pgm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         RNDATE
     C                   eval      RNTIME = dstime
     C                   update    dbrcdf
E01  C                   endif
      *
     C                   endsr
     *================================================================
     * WRITER - Write data base record and after processing          *
     *================================================================
     C     writer        begsr
      *
     C                   clear                   dbrcdf
      *
     C                   exsr      Mvscdb
      *
      *** beg chg ***** 07/18/07 *************************************
     C***********        if        scmbat = *blank
     C                   if        scmtyp = 'A'
      *** end chg ***** 07/18/07 *************************************
     C                   eval      RNBPF = 'N'
     C                   else
     C                   eval      RNBPF = 'Y'
     C                   endif
      *
     C                   eval      RNIPGM = Q1pgm
     C                   eval      RNPGM  = Q1pgm
     C                   eval      RNIUSR = Q1user
     C                   eval      RNUSER = Q1user
     C                   time                    dsdttm
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         rnidt
     c                   movel     srdto         rndate
     C                   eval      RNITIM = dstime
     C                   eval      RNTIME = dstime
     C                   write     dbrcdf
      *
      *** beg chg ***** 07/18/07 *************************************
     C***********        if        scmbat = *blank
     C                   if        scmtyp = 'A'
      *** end chg ***** 07/18/07 *************************************
     C                   move      ##co#         qqco#
     C                   move      scbat#        qqbat#            9
     C                   move      ##outq        qqoutq           10
     C                   call      'W0500C'
     C                   parm                    qqco#
     C                   parm                    qqbat#
     C                   parm                    qqoutq
     C                   endif
      *
     C                   feod      wxrnp
     C                   move      *on           outflg
      *
     C                   endsr
