    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2004-2007
    H*********************************************************************
    H/TITLE - Return Fund Check Register Prompt
    H*********************************************************************
    H*  DESCRIPTION: Prompts for Return Fund Check Report                *
    H*                                                                   *
    H*  CALLED FROM: Menu                                                *
    H*                                                                   *
    H*  CALLS TO   : W7514C                                              *
    H*                                                                   *
    H*  CREATED: 02/14/04      C:136     PROJECT: WC1033                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
      *  04/24/24 C:300 Project: WC2676                                   *
      *                 This program is no longer in use.                 *
      *                 Pass-through this program to new screen.          *
      *                 Add extra entry parm to avoid Pass-through of old screen
      *
      *  07/18/07 C:136 Project: WC1491                                   *
      *                 Implement Ind Cash, Return Deposit                *
      *                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw7514f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     infds(fids01)
     F
     Fwmfnl     if   e           k disk
     Fwmupl     if   e           k disk
     Fswcpf     if   e           k disk
    D*
     D/COPY scopypr01d
     D/COPY sprdate
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('Return Fund Check -
     D                                     Register Report')
     D comapi          c                   const('WA001R')
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D kkfnd           s              3  0
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
      **** Begin Add **** 04/24/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 04/24/24 ******************************************
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     arcdpl        plist
     C                   parm      '2'           ##call            1
     C                   parm      *blank        ##kc              1
     C                   parm      *blank        ##kl              1
     C                   parm      *blanks       ##key            10
      *
     C     bankpl        plist
     C                   parm      *blank        ##kc              1
     C                   parm      *blank        ##kl              1
     C                   parm      '2'           ##call            1
     C                   parm      *blanks       #1co#             3
     C                   parm      *blanks       ##acct           20
      *
     C     fnpl          plist
     C                   parm      *blank        ##kc              1
     C                   parm      *blank        ##kl              1
     C                   parm      '2'           ##call            1
     C                   parm                    #1co#             3
     C                   parm      *blanks       #1fnd             3
     C                   parm      *blanks       ##fyr             3
     C                   parm      1             ##view            1 0

     C     sbmpl         plist
     C                   parm                    #1co#
     C                   parm                    ##ffnd            3
     C                   parm                    ##tfnd            3
     C                   parm                    ##fdat            8
     C                   parm                    ##tdat            8
     C                   parm      scacct        ##acct           20
     C                   parm      scttyp        ##ttyp            1
     C                   parm      scsort        ##sort            1
     C                   parm      upoutq        ##outq           10
      *** beg add ***** 07/18/07 **************************************
     C                   parm      scbtyp        ##btyp            1
      *** end add ***** 07/18/07 **************************************
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
      *
     C     upky          klist
     C                   kfld                    q1user
    C*================================================================

      **** Begin Add **** 04/24/24 ******************************************
      * Do not Pass-Through to new screen, if 3rd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 3
     c                   Call      'WTRFCHKREG'
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 04/24/24 ******************************************

     C/COPY SCOPYPR01C
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
     C     DSCAPI        begsr
     C                   call      comapi        compl                  91
     C                   endsr
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     errck1        begsr

      *** beg del ***** 07/18/07 *******************************************************
     C**************     if        scffnd > 0
     C**************     eval      kkfnd = scffnd
     C*****fnky*****     chain     pfn01
     C**************     if        not %found
     C**************     seton                                        71  90
     C**************     endif
     C**************     endif
      **************
     C**n90*********     if        sctfnd > 0
     C**************     eval      kkfnd = sctfnd
     C*****fnky*****     chain     pfn01
     C**************     if        not %found
     C**************     seton                                        72  90
     C**************     endif
     C**************     endif
      *** end del ***** 07/18/07 *******************************************************
      *** beg add ***** 07/18/07 *******************************************************
     c   36              if        scbtyp <> *blank and
     c                             scbtyp <> 'A' and
     c                             scbtyp <> 'D' and
     c                             scbtyp <> 'I' and
     c                             scbtyp <> 'P'
     c                   seton                                        71  90
     c                   endif

     c  n36              if        scbtyp <> *blank and
     c                             scbtyp <> 'A' and
     c                             scbtyp <> 'P'
     c                   seton                                        71  90
     c                   endif
      *** end add ***** 07/18/07 *******************************************************
      *
     C  n90              if        sctfnd < scffnd
     C                   seton                                        73  90
     C                   endif
     C
     C  n90              if        scfdat <> 0
     C                   eval      scfdat = dtcheck(scfdat)
     C                   if        scfdat = 0
     C                   seton                                        74  90
     C                   endif
     C                   endif
      *
     C  n90              if        sctdat <> 0
     C                   eval      sctdat = dtcheck(sctdat)
     C                   if        sctdat = 0
     C                   seton                                        75  90
     C                   endif
     C                   endif
      *
     C  n90              if        sctdat < scfdat
     C                   seton                                        76  90
     C                   endif
      *
     C  n90              if        scttyp <> 'R' and
     C                             scttyp <> 'S' and
     C                             scttyp <> 'V' and
     C                             scttyp <> ' '
     C                   seton                                        77  90
     C                   endif
      *
     C  n90              if        scsort <> 'G' and
     C                             scsort <> 'B'
     C                   seton                                        78  90
     C                   endif
      *
     C                   endsr
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     kjdo          begsr
      *
     C                   select
      *
     C                   when      hfld = 'SCFFND'
     C                   call      'W0010R'      fnpl
     C                   if        #1fnd <> *blanks
     C                   move      #1fnd         scffnd
     C                   seton                                        82  90
     C                   endif
      *
     C                   when      hfld = 'SCTFND'
     C                   call      'W0010R'      fnpl
     C                   if        #1fnd <> *blanks
     C                   move      #1fnd         sctfnd
     C                   seton                                        83  90
     C                   else
     C                   seton                                        82  90
     C                   endif
      *
     C                   when      hfld = 'SCACCT'
     C                   call      'W2422R'      bankpl
     C                   if        ##acct <> *blank
     C                   eval      scacct = ##acct
     C                   seton                                        84  90
     C                   else
     C                   seton                                        83  90
     C                   endif
      *
     C                   endsl
      *
     C                   endsr
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
     C     PGMEND        begsr
     C                   seton                                        lr
     C                   endsr
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
     C     PGMSTR        begsr
     *
      * Display window if return premium module not installed
      *
     C                   read      swcpf
     C                   if        %eof or WCFL25 <> 'Y'
     C                   exfmt     rpinstwdw
     C                   seton                                        lr
     C                   return
     C                   endif

      *** beg add ***** 07/18/07 *******************************************************
     c                   if        WCFL36 = 'Y'
     c                   eval      *in36 = *on
     c                   endif
      *** end add ***** 07/18/07 *******************************************************

     C                   if        %parms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scprds
    C*
     C                   if        %parms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
    C*
     C                   endsr
    C*====================================================================
    C* PREDSP - Pre-processing before displaying (every time)
    C*====================================================================
     C     PREDSP        begsr
     C                   endsr
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
     C     PRSCTL        begsr
      *
     C     upky          chain     pup01
     C                   move      ##co#         #1co#
     C                   move      scffnd        ##ffnd
     C                   move      sctfnd        ##tfnd
     C                   move      scfdat        ##fdat
     C                   move      sctdat        ##tdat
      *
     C                   call      'W7514C'      sbmpl
      *
     C                   endsr
