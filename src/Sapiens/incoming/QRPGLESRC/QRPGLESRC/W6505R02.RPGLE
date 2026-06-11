      *  (C) Copyright Fiserv Insurance Solutions, Inc., 2003-2007
      *================================================================
      * Description:  Return premium fund schedule update/display    *
      *                                                               *
      * Called by: W6505R                                             *
      *                                                               *
      * Calls to:  None                                               *
      *                                                               *
      * Created: 03/12/03  C:142/C:136  Project: WC1033               *
      *================================================================
      * Revisions:                                                    *
      *
      * 10/17/07 C:136 WC1491 PRISS 6
      *                Change title to Return Fund.
      *
      *================================================================
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw6505f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwac6p     uf a e             disk
     F                                     rename(pc601:dbrcdf)
     Fwac6l3    if   e           k disk
     *
     Fwsckl     if   e           k disk
     *
     Fwmfnl     if   e           k disk
     *
     Fwar4l9    uf   e           k disk
     *================================================================
     *  /COPY required constants                                     *
     *================================================================
      *** beg chg **** 10/07/07 ***************************************
     D*********** optnm           c                   'Return Premium Check Schedule'
     D optnm           c                   'Return Fund Check Schedule'
      *** end chg **** 10/07/07 ***************************************
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
     *----------------------------------------------------------------
     * Stand Alone Fields - TOP
     *----------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D #1mode          s              1
     D #1co#           s              3
     D #1fnd           s              3
     D #1fyr           s              3
     D #1case          s              7
     D #1appl          s              2
     D #1data          s             50
     D #1date          s              8
     D #1acct          s             20
     D #1call          s              1
     D #1hdcy          s              1
     D #1chkq          s             10
     D #1ftyp          s             10
     D #1rtcd          s              1
     D #1efdt          s              7
     D #1cpyq          s             10
     D #1cktp          s             10
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkprdt          s                   like(c6prdt)
     D kkfnd           s                   like(c6fnd)
     D kkacct          s                   like(c6acct)
     D kkcktp          s                   like(c6cktp)
      *** beg add ***** 07/18/07 **************************************
     D kkappl          s                   like(c6appl)
      *** end add ***** 07/18/07 **************************************
     *----------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *----------------------------------------------------------------
     C/copy scopymt01c
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
      *
     C     fnpl          plist
     C                   parm      *blank        #1kc              1
     C                   parm      *blank        #1kl              1
     C                   parm      '2'           #1call            1
     C                   parm                    #1co#             3
     C                   parm      *blanks       #1fnd             3
     C                   parm      *blanks       #1fyr             3
     C                   parm      1             #1view            1 0
     *================================================================
     *  KLIST definitions                                            *
     *================================================================
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
      *
     C     ckky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkacct
      *
     C     c6ky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkprdt
     C                   kfld                    kkfnd
     C                   kfld                    kkcktp
      *** beg add ***** 07/18/07 **************************************
     C                   kfld                    kkappl
      *** eng add ***** 07/18/07 **************************************
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
     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)                  *
     *================================================================
     C     errck1        begsr
      *
      * Edit fund
      *
     C                   if        scfnd <> *zero

     C                   eval      kkfnd = scfnd
     C     fnky          chain     pfn01
     C                   if        not %found
     C                   seton                                        71  90
     C                   else
     C                   movel(p)  fnname        scfndn
     C                   endif

     C                   endif                                                  scfnd <> *blank
      *
      * Edit process date
      *
     C                   eval      srdti = scprdt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scprdt
     C  n90              if        srerr = *on
     C                   seton                                        72  90
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
     *================================================================
     * MVDBSC - Move database to screen                              *
     *================================================================
     C     mvdbsc        begsr

     C                   eval      scfnd = c6fnd
     C                   eval      srdti = c6prdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scprdt
     C                   eval      scacct = c6acct
     C                   eval      sccktp = c6cktp
     C                   eval      scstr# = c6chk#
     C                   eval      scsta0 = c6sta0
     C                   eval      scsta1 = c6sta1

     C                   endsr
     *================================================================
     * MVSCDB - Move screen to database                              *
     *================================================================
     C     mvscdb        begsr
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
     C     *inkj         caseq     *on           kjdo
     C                   endcs
     C                   endsr
     *================================================================
      * KJDO - Search routines                                        *
     *================================================================
     C     kjdo          begsr
      *
      *  Group Search
      *
     C  n90              if        hfld = 'SCFND'
     C                   call      'W0010R'      fnpl

     C                   if        #1fnd <> *blank
     C                   move(p)   #1fnd         scfnd
     C                   eval      kkfnd = scfnd
     C                   seton                                        81  90
     C     fnky          chain     pfn01
     C                   eval      scfndn = fnname
     C                   else
     C                   seton                                        80  90
     C                   endif

     C                   endif                                                  hfld = scfnd

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
     C     @@mode        comp      'A'                                    40
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

     C     @@rrn         chain     dbrcdf
B01  C                   if        %found
     C                   exsr      mvscdb
     C                   eval      c6user = q1user
     C                   eval      c6pgm  = q1pgm
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         c6date
     C                   eval      c6time = dstime
     C                   update    dbrcdf
E01  C                   endif

     C                   endsr
     *================================================================
     * WRITER - Write data base record and after processing          *
     *================================================================
     C     writer        begsr

     C                   eval      srdti = scprdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkprdt

     C                   if        scfnd  = 0
     C     ##co#         setll     pr401
     C     ##co#         reade     pr401
     C                   else
     C                   eval      kkfnd  = scfnd
     C     fnky          setll     pr401
     C     fnky          reade     pr401
     C                   endif

     C                   dow       not %eof


     C                   if        r4chk# = 0 and
     C                             r4stat = *blank
     C                   move      ##co#         #1co#
     C                   move      r4fnd         #1fnd
     C                   move      r4fyr         #1fyr
      *** beg add ***** 07/18/07 ******************************************************
      *** beg add ***** 07/18/07 *************************************
     c                   select
     c                   when      R4BTYP = 'I'
     c                   eval      #1appl = 'IC'
     c                   other
     c                   eval      #1appl = 'RP'
     c                   endsl
      *** end add ***** 07/18/07 *************************************
      *** end add ***** 07/18/07 ******************************************************

     C                   call      'WA023R1'
     C                   parm      '2'           #1mode
     C                   parm                    #1co#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
      *** beg chg ***** 07/18/07 ******************************************************
     C*****************  parm      'RP'          #1appl
     C                   parm                    #1appl
      *** end chg ***** 07/18/07 ******************************************************
     C                   parm                    #1data
     C                   parm      *blank        #1cktp
     C                   parm      *blank        #1rtcd

     C                   if        #1rtcd <> '1'
     C                   eval      r4cktp = #1cktp
     C                   update    pr401

     C                   eval      kkfnd  = r4fnd
     C                   eval      kkcktp = #1cktp
      *** beg add ***** 07/18/07 ******************************************************
     C                   eval      kkappl = #1appl
      *** end add ***** 07/18/07 ******************************************************
     C     c6ky          chain     pc601
     C                   if        not %found
     C                   exsr      wrtSched
     C                   endif

     C                   endif                                                  #1rtcd

     C                   endif                                                  r4chk#

     C                   if        scfnd  = 0
     C     ##co#         reade     pr401                                  60
     C                   else
     C     fnky          reade     pr401                                  60
     C                   endif

     C                   enddo

     C                   endsr
      *================================================================
      *  Write Check Schedule File (WAC6P)                            *
      *================================================================
     C     wrtSched      begsr

     C                   move      ##co#         #1co#
     C                   move      r4fnd         #1fnd
     C                   move      r4fyr         #1fyr
     C                   move(p)   scprdt        #1date
      *** beg add ***** 07/18/07 *************************************
     c                   select
     c                   when      R4BTYP = 'I'
     c                   eval      #1appl = 'IC'
     c                   other
     c                   eval      #1appl = 'RP'
     c                   endsl
      *** end add ***** 07/18/07 *************************************

     C                   call      'WA023R'
     C                   parm                    #1co#
     C                   parm                    #1fnd
      *** beg chg ***** 07/18/07 *************************************
     C*****************  parm      'RP'          #1appl
     C                   parm                    #1appl
      *** end chg ***** 07/18/07 *************************************
     C                   parm                    #1date
     C                   parm      *blanks       #1acct
     C                   parm      'C'           #1call
     C                   parm                    #1hdcy
     C                   parm                    #1chkq
     C                   parm                    #1ftyp
     C                   parm                    #1rtcd
     C                   parm                    #1efdt
     C                   parm                    #1cpyq
     C                   parm      kkcktp        #1cktp

     C                   eval      kkacct = #1acct
     C                   if        kkacct <> *blanks and
     C                             #1hdcy = 'Y'
     C     ckky          chain     pck01
     C                   if        %found
     C                   clear                   pc601
     C                   eval      c6co#  = ##co#
     C                   eval      c6fnd  = r4fnd
     C                   eval      c6prdt = kkprdt
     C                   eval      c6acct = #1acct
     C                   eval      c6cktp = #1cktp
      *** beg add ***** 07/18/07 *************************************
     c                   eval      C6APPL = #1appl
      *** end add ***** 07/18/07 *************************************
     C                   eval      c6chk# = cklchk
     C                   eval      c6ipgm = q1pgm
     C                   eval      c6pgm  = q1pgm
     C                   eval      c6iusr = q1user
     C                   eval      c6user = q1user
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         c6idt
     C                   movel     srdto         c6date
     C                   eval      c6itim = dstime
     C                   eval      c6time = dstime
     C                   write     dbrcdf
     C                   endif                                                  %found

     C                   endif                                                  kkacct <> *blanks

     C                   endsr
