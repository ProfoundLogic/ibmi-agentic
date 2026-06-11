    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE F8271R
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H**************************************************************************
    H*  F8271R     : First Health State Reporting - File Download
    H*
    H*  Description: Populates the First Health paid Date File (FAQ1P)
    H*
    H*  Called from: F8271C - First Health State Reporting - Override program
    H*  Calls to   : None
    H*
     H**************************************************************************
     H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
     H**************************************************************************
    H*  Revisions:
    H*
    H*  03/19/07 c:62 project 139MHG
    H*                Change to MCMC
    H*
     H*  09/08/06  C:163 PROJECT: 139MCV
     H*                  Medical Bill Interface w/FirstHealth
     H*                  Remove EAESDT update
     H*
     H**************************************************************************
     Ffaeal17   uf   e           k disk
     f*** begin chg *** 03/19/07 *******************************************
     F**********faq1p     o    e             disk
     Ffaq1p     uf a e             disk
     f*** end   chg *** 03/19/07 *******************************************
     Ffaeol4    if   e           k disk
    F*
    D*
     D/COPY SCOPYWW01I
    D*================================================================
    D*  User defined data structures
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D header          ds            90
     D  hactivtnplan           1      3    inz('008')
     D  hclientident           4      6    inz('GIC')
     D  hicn                   7     20
     D  billhdrrecid          21     21    inz('A')
     D  jurisdiction          22     24    inz('199')
     D  actualbillpd          25     32
     D  actualtotpma          33     43
     D  emppmtprvppi          44     44
     D  billstatuscd          45     45    inz('P')
     D  fieldnotused          46     49    inz('0000')
    D*
     D trailer         ds            90
     D  tactivtnplan           1      3    inz('008')
     D  tclientident           4      6    inz('GIC')
     D  ticn                   7     20    inz('99999999999999')
     D  billtrlrecid          21     21    inz('C')
     D  crttimestamp          22     33
     D  crttimestadt          22     29
     D  crttimestatm          30     33
     D  totlicncount          34     39
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@date          s              8
     D @@dcid          s             12
     D count           s              9  0
     D qqco#           s                   like(eoco#)
     D qqsub#          s                   like(eosub#)
     D qqfnd           s                   like(eofnd)
     D qqfyr           s                   like(eofyr)
     D qqcase          s                   like(eocase)
     D qqeob#          s                   like(eoeob#)
     D date            s              6
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     d*** begin add *** 03/19/07 *******************************************
     D countc          s              6a   inz('000000')
     d*** end   add *** 03/19/07 *******************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@date
     C                   parm                    @@dcid
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     DT1MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DT2MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DT3MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DT4MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     eoky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqsub#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
    C*================================================================
    C*  LIKE defines
    C*================================================================
    C*
    C*
    C* EXECUTE MAIN SUBROUTINE
    C*
     C                   exsr      mainsr
    C*
    C*================================================================
    C* MAINSR - MAIN SUBROUTINE
    C*================================================================
     C     mainsr        begsr
     C*
     C                   EXSR      PGMSTR
    C*
    C* Let's start reading thru these checks
    C*
     c*** begin del *** 03/19/07 *******************************************
     C**********         eval      count = *zeros
     c*** end   del *** 03/19/07 *******************************************
     C                   reset                   trailer
    C*
     C                   read      pea01                                  99
     C                   dow       *in99 = *off
    C*
     C                   eval      qqco#  = eaco#
     C                   eval      qqsub# = easub#
     C                   eval      qqfnd  = eafnd
     C                   eval      qqfyr  = eafyr
     C                   eval      qqcase = eacase
     C                   eval      qqeob# = eaeob#
     C     eoky          chain     peo01
    C*
     C                   if        %found(faeol4)
     c*** begin chg *** 03/19/07 *******************************************
     C**********         if        eontwk = 'FH' and eawcpd <> 0
     c*** begin chg *** 03/19/07 *******************************************
     C**********         if        (eontwk = 'MCMC' or eontwk = '6' or
     C**********                    eontwk = '11' or eontwk = '19') and
     C**********                    ((eawcpd <> 0 and eapaid <> 0) or
     C**********                                      eapaid = 0)
     C                   if        eontwk <> 'FH' and eontwk <> ' ' and
     C                             ((eawcpd <> 0 and eapaid <> 0) or
     C                                                eapaid = 0)
     c*** end   chg *** 03/19/07 *******************************************
     c*** end   chg *** 03/19/07 *******************************************
    C*
     C* DATE OF CARRIER PAYMENT
    C*
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DT2MDY
     C                   MOVEL     SRDTO         PAYDT1            8 0
     C*
     C* USE THE CHECK MAILING DATE TO REPORT DATE PAID IF NOT BLANK
     C*
     c*** begin add *** 03/19/07 *******************************************
     c                   if        eapaid <> 0
     c*** end   add *** 03/19/07 *******************************************
     C                   EXSR      GETMDT
     c*** begin add *** 03/19/07 *******************************************
     c                   endif
     c*** end   add *** 03/19/07 *******************************************
     C*
     C                   exsr      wrtdta
     C*
     c*** begin add *** 03/19/07 *******************************************
     c                   if        eapaid <> 0
     c*** end   add *** 03/19/07 *******************************************
     C                   eval      eaedi  = 'S'
     C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
     C                   movel     q1pgm         eapgm
     c*** begin add *** 03/19/07 *******************************************
     c                   endif
     c*** end   add *** 03/19/07 *******************************************
     C*
     C                   update    pea01
     C*
     C                   endif
     C                   endif
    C*
     C                   read      pea01                                  99
     C                   enddo
    C*
     C                   exsr      pgmend
    C*
     C                   eval      *inlr = *on
    C*
     C                   endsr
    C*================================================================
    C* PGMEND - End of Program Processing
    C*================================================================
     C     pgmend        begsr
    C*
     c*** begin del *** 03/19/07 *******************************************
     C**********         clear                   @@dcid
     c*** end   del *** 03/19/07 *******************************************
     C                   if        count > *zeros
     C                   time                    dsdttm
     c*** begin add *** 03/19/07 *******************************************
     C                   if        @@dcid = *blanks
     c*** end   add *** 03/19/07 *******************************************
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtmdy
     C                   movel     srdto         date
     C     'PD'          cat(p)    date:0        @@dcid
     C                   cat       '.DAT':0      @@dcid
     c*** begin add *** 03/19/07 *******************************************
     C                   endif
     c*** end   add *** 03/19/07 *******************************************
    C*
     C                   movel     dsdate        srdti
     C                   call      'SRCVTDAT'    dt4mdy
     C                   movel     srdto         crttimestadt
     C                   movel     dstime        crttimestatm
    C*
     C                   move      count         totlicncount
    C*
     C                   movel(p)  trailer       q1data
    C*
     C                   write     pq101
    C*
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDTA - Write the Data Record
    C*================================================================
     C     wrtdta        begsr
    C*
     C                   clear                   pq101
     C                   reset                   header
    C*
     C                   movel     easinv        hicn
    C*
     C                   if        #1cmdt = *blanks
     C*** begin add *** 06/15/07 *******************************************
     C                   if        eawcpd <> 0
     C*** end   add *** 06/15/07 *******************************************
     C                   movel     eawcpd        actualbillpd
     C*** begin add *** 06/15/07 *******************************************
     C                   else
     C                   movel     eaepdt        actualbillpd
     C                   endif
     C*** end   add *** 06/15/07 *******************************************
     C                   else
     C                   movel     #1cmdt        srdti
     C                   call      'SRCVTDAT'    dt4mdy
     C                   movel     srdto         actualbillpd
     C                   endif
    C*
     C                   movel(p)  header        q1data
    C*
     C                   write     pq101
    C*
     C                   eval      count = count + 1
    C*
     C                   endsr
     C****************************************************************
     C* GETMDT - Subroutine to retrieve the check mailing date       *
     C****************************************************************
     C*
     C     GETMDT        BEGSR
     C*
     C* USE THE CHECK MAILING DATE IF NOT BLANK
     C*
     C     @@WCIF        IFEQ      'Y'
     C*
     C                   MOVEL     EASUB#        ##SUB#            5
     C                   MOVEL     EAFND         #1FND             3
     C*
     C* DETEMINE IF STANDALONE ACCOUNT
     C*
     C                   MOVE      EACO#         #1CO#             3
     C                   CALL      'FA701R'
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM      ' '           ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C*
     C* GET WCMS FUND NUMBER
     C*
     C                   MOVE      *BLANKS       ##WCF#            3
     C                   CALL      'FA700R'
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    ##WCF#
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C*
     C                   MOVE      PAYDT1        #1TRDT
     C                   CALL      'WA050R'
     C                   PARM                    #1CO#
     C                   PARM      ##WCF#        #1FND
     C                   PARM                    #1TRDT            8
     C                   PARM      *BLANKS       #1CMDT            8
     C*
     C     #1CMDT        IFNE      *BLANKS
     C                   MOVE(p)   #1CMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DT4MDY
     C                   MOVEL     SRDTO         PAYDT1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C****************************************************************
     C* PGMSTR - Retrieve install flags, etc.
     C****************************************************************
     C*
     C     PGMSTR        BEGSR
     C*
     C*  Check to see if WCMS is installed
     C*
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1
     C                   PARM                    @@MCIF            1
     C                   PARM                    @@WCIN            1
     c*** begin add *** 03/19/07 *******************************************
     c*
     c*  Was the outfile written to
     c*
     c                   eval      count = 0
     c     *hival        setgt     pq101
     c                   readp     pq101
     c                   if        not %eof
     c                   eval      countc = %subst(q1data:34:6)
     c                   move      countc        count
     c                   delete    pq101
     c                   endif
     c*** end   add`*** 03/19/07 *******************************************
     C*
     C                   ENDSR
