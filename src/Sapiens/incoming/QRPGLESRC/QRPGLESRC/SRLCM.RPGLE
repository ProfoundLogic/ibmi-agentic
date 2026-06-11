     h Copyright('(c) 2025 Sapiens. All Rights Reserved.')
      /TITLE SRLCM - API - LOSS COST MULTIPLIER CALCULATION
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
      *********************************************************************************
     h* PROGRAM DESCRIPTION:                                                          *
      *                                                                               *
     h* Return loss cost multiplier. Look in payroll class file, loss cost multiplier *
     h* file, and ratings and statistics file in order.  If not found, return a value *
     h* of one. Test this program by passing in 'TEST' in last parm position.         *
     h* QSYSPRT will contain the information that came into the program and the       *
     h* corresponding output.                                                         *
     h*                                                                               *
     h* CALLED BY: BILLING PROGRAMS THAT NEED TO PULL IN THE LCM                      *
      *                                                                               *
     h* CALLS TO: NONE                                                                *
      *                                                                               *
     h* CREATED: 02/28/05                                                             *
      *                                                                               *
     h* 02/28/05 C:136 PROJECT: WC1136                                                *
      *********************************************************************************
      * Revisions:                                                                    *
     H*                                                                               *
     H* 09/04/25 C:65  Project: Phw#119668 (Sunz)                                     *
     H*                Remove CLTID call to help clean up job logs.                   *
      *                                                                               *
      * 03/04/10 C:180 Support 71916
      *                Improve performance by calling SRCHKPGM only when
      *                needed (Client-qualify).
      *
      * 10/29/08 C:136 Support: WA093 65781                                           *
      *                Allow the Experience by Class Report (W4121R) to pull in the   *
      *                LCM instead of passing a value of 10000 back to the calling    *
      *                program for KRMS per Pat Cole.                                 *
      *                                                                               *
      * 10/17/08 C:61  Project: WC1644                                                *
      *                Do not apply LCM to terror rates if exclusion flag             *
      *                (WDRRP/RRTERL) is set to 'Y' value.                            *
      *                                                                               *
      * 08/16/08 C:136 Project: 93WGW, PRISS 33                                       *
      *                KRMS doesn't want the LCM applied to its rates.                *
      *                                                                               *
      *********************************************************************************
      * Loss Cost Multiplier (co/rtcd/efdt)
     fWDLML     if   e           k disk    rename(plm01:plm00)
      * Payroll Master (co/rtcd/clas/efdt)
     fWMPRL4    if   e           k disk    rename(ppr01:ppr04)
      * Rate Set Code (co/rtcd)
     fWMRSL     if   e           k disk    rename(prs01:prs00)
      * Ratings and Statistics (co/fnd/st/efdt)
     fWDRRL     if   e           k disk    rename(prr01:prr00)
     Fqsysprt   o    f  132        printer oflind(*inof) usropn
      *================================================================================
      * Definitions
      *================================================================================
     d @@co#           s              3
     d @@fnd           s              3
     d @@rtcd          s              3
     d @@clas          s              5
     d @@efdt          s              7
     d @@lcm           s              5
     d kkco#           s              3  0
     d kkfnd           s              3  0
     d kkst            s              2
     d kkrtcd          s              3
     d kkclas          s              5  0
     d kkefdt          s              7  0
     d fromFile        s             10
     d testMe          s              5
      *================================================================================
      * *entry
      *================================================================================
     c     *entry        plist
     c                   parm                    @@co#
     c                   parm                    @@fnd
     c                   parm                    @@rtcd
     c                   parm                    @@clas
     c                   parm                    @@efdt
     c                   parm                    @@lcm
      *================================================================================
      * KLIST's
      *================================================================================
     c     prky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkrtcd
     c                   kfld                    kkclas
      *
     c     prky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkrtcd
     c                   kfld                    kkclas
     c                   kfld                    kkefdt
      *
     c     rrky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkst
      *
     c     rrky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkst
     c                   kfld                    kkefdt
      *
     c     lmky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkrtcd
      *
     c     lmky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkrtcd
     c                   kfld                    kkefdt
      *================================================================================
      * Main
      *================================================================================
      *** beg add ***** 08/16/08 ******************************************************
     C*** begin chg ***  09/04/25  ***********************************
     C*****              CALL      'RTVCLTID'
     C*****              PARM                    CLTID             3
     C                   Move      *blanks       CLTID             3
     C*** end chg *****  09/04/25  ***********************************
      *** beg add ***** 10/29/08 ******************************************************
     c***** Begin Change *******  03/04/10 ********************************
     c*****              call      'SRCHKPGM'
     c*****              parm      'W4121R'      @@pgm            10
     c*****              parm                    @@found           1
      *** end add ***** 10/29/08 ******************************************************
     c******             if        cltid = '093'
      *** beg add ***** 10/29/08 ******************************************************
     c******                       and @@found <> *on
      *** end add ***** 10/29/08 ******************************************************
     c******             eval      @@LCM = '10000'
     c******             eval      *inlr = *on
     c******             return
     c******             endif
      *** end add ***** 08/16/08 ******************************************************
     C*** begin chg ***  09/04/25  ***********************************
     C* KRMS no longer client, client ID not set in pgm.
     C*** end chg *****  09/04/25  ***********************************
     c                   if        cltid = '093'
     c                   call      'SRCHKPGM'
     c                   parm      'W4121R'      @@pgm            10
     c                   parm                    @@found           1
      *
     c                   if        @@found <> *on
     c                   eval      @@LCM = '10000'
     c                   eval      *inlr = *on
     c                   return
     c                   endif
     c                   endif
     c***** End Change *********  03/04/10 ********************************
      *
     c                   move      @@co#         kkco#
     c                   move      @@fnd         kkfnd
     c                   move      @@rtcd        kkrtcd
     c                   move      @@clas        kkclas
     c                   move      @@efdt        kkefdt
     c                   move      @@lcm         testMe
     c                   move      *zeros        @@lcm
      *
      * Get rating state, find terror code in WDRRP, then look at Rate Set Code level and then
      * statistical level for LCM.
      *
     c     lmky          chain     prs00
     c                   if        %found
     c                   eval      kkst = rsjrst
     c                   else
     c                   eval      kkst = *blanks
     c                   endif
      *
     c     rrky1         setgt     prr00
     c     rrky          readpe    prr00
     c                   if        not %eof and
     c                             (kkclas = rrterc or
     c                              kkclas = rrdtrc)
      ******* beg add ****** 10/17/08 *********************************
      *
     c                   if        rrterl <> 'Y'
      ******* end add ****** 10/17/08 *********************************
      *
      * Look in rate set code level.
      *
     c     lmky1         setgt     plm00
     c     lmky          readpe    plm00
     c                   dow       not %eof and
     c                             lmstat = 'I'
     c     lmky          readpe    plm00
     c                   enddo
      *
     c                   if        not %eof and
     c                             lmlcm <> 0
      *
     c                   if        lmexdt = 0 or
     c                             kkefdt < lmexdt
     c                   move      lmlcm         @@lcm
      *
     c                   if        testMe = 'TEST '
     c                   eval      fromFile = 'TERRWDLMP'
     c                   open      qsysprt
     c                   except    test
     c                   close     qsysprt
     c                   endif
      *
     c                   eval      *inlr = *on
     c                   return
     c                   endif
      *
     c                   endif
      *
     c                   if        rrlcm <> 0
      *
     c                   if        rrlcex = 0 or
     c                             kkefdt < rrlcex
     c                   move      rrlcm         @@lcm

     c                   if        testMe = 'TEST '
     c                   eval      fromFile = 'TERRWDRRP'
     c                   open      qsysprt
     c                   except    test
     c                   close     qsysprt
     c                   endif
      *
     c                   eval      *inlr = *on
     c                   return
     c                   endif
      *
     c                   endif
      ******* beg add ****** 10/17/08 *********************************
      *
     c                   endif
      ******* end add ****** 10/17/08 *********************************
      *
     c                   move      '10000'       @@lcm
     c                   if        testMe = 'TEST '
     c                   eval      fromFile = 'TERRNOLCM'
     c                   open      qsysprt
     c                   except    test
     c                   close     qsysprt
     c                   endif
      *
     c                   eval      *inlr = *on
     c                   return
     c                   endif
      *
      * Look in Payroll Master.
      *
     c     prky1         setgt     ppr04
     c     prky          readpe    ppr04
     c                   dow       not %eof and
     c                             prstat = 'I'
     c     prky          readpe    ppr04
     c                   enddo

     c                   if        not %eof and
     c                             prlcm <> 0
      *
      *** beg add ***** 08/10/05 *******************************************************
     c                   if        prlcex = 0 or
     c                             kkefdt < prlcex
      *** end add ***** 08/10/05 *******************************************************
     c                   move      prlcm         @@lcm
      *
     c                   if        testMe = 'TEST '
     c                   eval      fromFile = 'WMPRP'
     c                   open      qsysprt
     c                   except    test
     c                   close     qsysprt
     c                   endif
      *
     c                   eval      *inlr = *on
     c                   return
      *** beg add ***** 08/10/05 *******************************************************
     c                   endif
      *** end add ***** 08/10/05 *******************************************************
      *
     c                   endif
      *
      * Look in rate set code level.
      *
     c     lmky1         setgt     plm00
     c     lmky          readpe    plm00
     c                   dow       not %eof and
     c                             lmstat = 'I'
     c     lmky          readpe    plm00
     c                   enddo
      *
     c                   if        not %eof and
     c                             lmlcm <> 0
      *
      *** beg add ***** 08/10/05 *******************************************************
     c                   if        lmexdt = 0 or
     c                             kkefdt < lmexdt
      *** end add ***** 08/10/05 *******************************************************
     c                   move      lmlcm         @@lcm
      *
     c                   if        testMe = 'TEST '
     c                   eval      fromFile = 'WDLMP'
     c                   open      qsysprt
     c                   except    test
     c                   close     qsysprt
     c                   endif
      *
     c                   eval      *inlr = *on
     c                   return
     c                   endif
      *
      *** beg add ***** 08/10/05 *******************************************************
     c                   endif
      *** end add ***** 08/10/05 *******************************************************
      *
      * Get rating state, then look in Ratings and Statistics.
      *
     c     lmky          chain     prs00
     c                   if        %found
     c                   eval      kkst = rsjrst
     c                   else
     c                   eval      kkst = *blanks
     c                   endif
      *
     c     rrky1         setgt     prr00
     c     rrky          readpe    prr00
     c                   if        not %eof and
     c                             rrlcm <> 0
      *
      *** beg add ***** 08/10/05 *******************************************************
     c                   if        rrlcex = 0 or
     c                             kkefdt < rrlcex
      *** end add ***** 08/10/05 *******************************************************
     c                   move      rrlcm         @@lcm
      *
     c                   if        testMe = 'TEST '
     c                   eval      fromFile = 'WDRRP'
     c                   open      qsysprt
     c                   except    test
     c                   close     qsysprt
     c                   endif
      *
     c                   eval      *inlr = *on
     c                   return
     c                   endif
      *
      *** beg add ***** 08/10/05 *******************************************************
     c                   endif
      *** end add ***** 08/10/05 *******************************************************
      *
      * Return a value of one if program execution makes it to this point.
      *
     c                   eval      @@lcm = '10000'
      *
     c                   if        testMe = 'TEST '
     c                   eval      fromFile = 'EndOfPgm'
     c                   open      qsysprt
     c                   except    test
     c                   close     qsysprt
     c                   endif
      *
     c                   eval      *inlr = *on
      *
     oqsysprt   e            test
     o                       @@co#             +  9
     o                       @@fnd             +  2
     o                       @@rtcd            +  2
     o                       @@clas            +  2
     o                       @@efdt            +  2
     o                       @@lcm             +  2
     o                       fromFile          +  2
