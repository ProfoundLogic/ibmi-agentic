     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE F8292R
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H**************************************************************************
    H*  F8292R     : Medical Bill Paid Date Program
    H*
    H*  Description: Populates the State Reporting File (FAY1P)
    H*
    H*  Called from: F8292C - Medical Bill Paid Date Override CL program
    H*  Calls to   : None
    H*
     H**************************************************************************
     H*  Created on 01/04/2010 by C:227 for Project 139MNL from client 139
     H**************************************************************************
    H*  Revisions:
     H*
     H*  05/01/17  C:079 Project: MC325
     H*                  Add logic for SA016R
    H*
    H*  11/10/16  C:079 Project 90MHT
    H*                  Add processing for selecting Bills for Novare
    H*
    H*  04/04/13  C:277 Project 133MNC
    H*                  Modified the program for MITCH network for LUBA
    H*
    H*  10/23/13  C:274 Project 133MNC
    H*                  Modified the program to incluse ZERO dollars for MITCH
    H*
    H*  10/22/10  C:227 Project 133MJU
    H*                  Modified the program for RMS network for 133 client
    H*
    H*  02/03/10  C:227 Project 139MNL Priss 11
    H*                  Date for all records in FDY1P
    H*
    H*  01/04/10  c:227 Project 139MNL
    H*                  Med Bill Import - Paid Date File
    H*
    H*  03/19/07  c:62  project 139MHG
    H*                  Change to MCMC
    H*
     H*  09/08/06  C:163 PROJECT: 139MCV
     H*                  Medical Bill Interface w/FirstHealth
     H*                  Remove EAESDT update
     H*
     H**************************************************************************
     Ffaeal17   uf   e           k disk
     Ffay1p     uf a e             disk
     Ffdy1p     uf a e             disk    rename(py101:py102)
     Ffaeol4    if   e           k disk
     Fwdpala    if   e           k disk
     F*** Beg Add *** 11/10/16 *****************************************
     FSMiep     if   e             disk
     F*** End Add *** 11/10/16 *****************************************
    F*
     d*** Beg Add *** 05/01/17 ***********************************
     d rtvSettings     pr                  Extpgm('SA016R')
     d  @@Cchg                        1
     d  @@Cifs                        1
     d  @@Csts                        1
     d  @@Cdny                        1
     d  @@Ccvfl                       1
     d  @@Cgrp                        1
     d  @@Cemp                        1
     d  @@Eifs                        1
     d  @@Eagt                        1
     d  @@Eemp                        1
     d  @@Fifs                        1
     d  @@Ftyp                        1
     d  @@Udup                        1
     d  @@Udrc                        1
     d  @@Urel                        1
     d  @@Upcl                        1
     d  @@Updn                        1
     d  @@Udpn                        1
     d  @@ClDays                      5  0
     d  @@IncZro                      1
     d  @@Crcd                        1
     d  @@CrDth                       1
     d  @@RtCd                        1
     d  @@Cchg         s              1
     d  @@Cifs         s              1
     d  @@Csts         s              1
     d  @@Cdny         s              1
     d  @@Ccvfl        s              1
     d  @@Cgrp         s              1
     d  @@Cemp         s              1
     d  @@Eifs         s              1
     d  @@Eagt         s              1
     d  @@Eemp         s              1
     d  @@Fifs         s              1
     d  @@Ftyp         s              1
     d  @@Udup         s              1
     d  @@Udrc         s              1
     d  @@Urel         s              1
     d  @@Upcl         s              1
     d  @@Updn         s              1
     d  @@Udpn         s              1
     d  @@ClDays       s              5  0
     d  @@IncZro       s              1
     d  @@Crcd         s              1
     d  @@CrDth        s              1
     d  @@RtCd         s              1
     d  isSettings     s              1
     d*** End Add *** 05/01/17 ***********************************
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
     D detail          ds            90
     D  dtrequestdt            1      8
     D  dtreview#              9     23
     D  dtcheck#              24     30
     D  dtcheckdt             31     38
     D  dtpaymentamt          39     49
     D  dtstate               50     51
     D  dtprovtaxid           52     60
    D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@date          s              8
     D @@dcid          s             17
     D*** Beg Add *** 04/04/13 ******************************************
     D @@ntwk          s              5
     D*** End Add *** 04/04/13 ******************************************
     D count           s              9  0
     D qqco#           s                   like(eoco#)
     D qqsub#          s                   like(eosub#)
     D qqfnd           s                   like(eofnd)
     D qqfyr           s                   like(eofyr)
     D qqcase          s                   like(eocase)
     D qqeob#          s                   like(eoeob#)
     D date            s              8
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D countc          s              6a   inz('000000')
     D ##CLT#          s              3
     D*** Beg Add *** 11/10/16 *****************************************
     D isSMiep         s              1    inz('N')
     D*** End Add *** 11/10/16 *****************************************
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
     C*** Beg Add *** 04/04/13 *********************************************
     C                   parm                    @@ntwk
     C*** End Add *** 04/04/13 *********************************************
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
     C     paky          klist
     C                   kfld                    qqco#
     C                   kfld                    qqfnd
     C                   kfld                    qqfyr
     C                   kfld                    qqcase
     C                   kfld                    qqeob#
    C*
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
     C*** BEG CHG *** 10/22/10 ****************************************
     C***********        if        ##clt# = '139'
     C***********        if        eontwk <> 'FH' and eontwk <> ' ' and
     C***********                  ((eawcpd <> 0 and eapaid <> 0) or
     C***********                                     eapaid = 0)
     C
     C                   if        (##clt# = '139' and eontwk <> 'FH' and
     C                              eontwk <> ' ') or
     C                             (##clt# = '133' and eontwk = 'RMS') and
     C                             (eawcpd <> 0 and eapaid <> 0)
     C*** Beg Add *** 04/04/13 *********************************************
     C                             or(##clt# = '133' and eontwk = 'MITCH')
     C*** Beg Chg *** 10/23/13 *********************************************
     C***************************  and (eawcpd <> 0 and eapaid <> 0)
     C*** End Chg *** 10/23/13 *********************************************
     C*** End Add *** 04/04/13 *********************************************
     C*** END CHG *** 10/22/10 ****************************************
     C*** Beg Add *** 11/10/16 *******************************************
     C*** Beg Chg *** 05/01/17 *******************************************
     C***********************       or isSMiep = 'Y' and IEincZro = 'Y'
     C                              or Q1prms > 3 and eoNtwk = @@ntWk
     C*** End Chg *** 05/01/17 *******************************************
     C*** End Add *** 11/10/16 *******************************************
    C*
     C* Check Date
    C*
     C                   z-add     eawcpd        srdti
     C                   call      'SRCVTDAT'    dt2mdy
     C                   movel     srdto         paydt1
     C*
     C* Use the check mailing date to report date paid if not blank
     C*
     c                   if        eapaid <> 0
     C*
     C                   exsr      getmdt
     C*
     C* If no mailing date then use provider check date
     C*
     c                   if        #1cmdt = *blanks
     c                   z-add     eawcpd        paydt1            8 0
     c                   endif
     c                   endif
     c*
     c                   if        eapaid = 0
     c                   z-add     eaepdt        paydt1
     c                   endif
     C*
     C* Get payment request date
     c*
     c     paky          setll     ppa01
     c     paky          reade     ppa01                                  71
     c                   dow       *in71 = *off
     c                   if        eawcpc = pachk#
     c                   z-add     patrdt        srdti
     c                   call      'SRCVTDAT'    dtyymd
     c                   movel     srdto         dtpayrqdt         8
     c                   leave
     c                   endif
     c     paky          reade     ppa01                                  71
     c                   enddo
     C*
     C*** BEG ADD *** 02/03/10 *********************************************
     C                   if        dtpayrqdt = *blanks
     C                   movel     eaepdt        dtpayrqdt
     C                   endif
     C*** END ADD *** 02/03/10 *********************************************
     C*
     C*** Beg Del *** 04/04/13 *********************************************
     C*************      exsr      wrtdta
     C*
     c*********          if        eapaid <> 0
     C*****************  eval      eaedi  = 'S'
     C*****************
     C*****************  time                    dsdttm
     C*****************  eval      srdti = dsdate
     C*****************  call      'SRCVTDAT'    dtcymd
     C*****************  movel     srdto         eadt
     C*****************  movel     q1pgm         eapgm
     c*********          endif
     C*****************
     C*****************  update    pea01
     C*****************
     C*** End Del *** 04/04/13 *********************************************
     C*** Beg Add *** 04/04/13 *********************************************
SelecC                   Select
     C*
     C                   when      q1prms <=3
     C                   Exsr      wrtdta
     C*
     C                   when      q1prms >3 and eontwk = @@ntwk
     C                   Exsr      wrtdta
     C*
     C                   Endsl
     C*** End Del *** 04/04/13 *********************************************
     C*** BEG DEL *** 10/22/10 ****************************************
     C***********        endif
     C*** END DEL *** 10/22/10 ****************************************
     C                   endif                                                  ##clt# = '139'
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
     C********           if        count > *zeros
     C                   time                    dsdttm
     C                   if        @@dcid = *blanks
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dt4mdy
     C                   movel     srdto         date
     C     'PAID_'       cat(p)    date:0        @@dcid
     C                   cat       '.TXT':0      @@dcid
     C                   endif
    C*
     C********           endif
    C*
     C                   endsr
    C*================================================================
    C* WRTDTA - Write the Data Record
    C*================================================================
     C     wrtdta        begsr
    C*
     C                   clear                   py101
     C                   reset                   detail
    C*
     C                   movel     dtpayrqdt     dtrequestdt
     C                   movel     easinv        dtreview#
     C                   movel     eawcpc        dtcheck#
     C                   movel     paydt1        dtcheckdt
     C                   move      eapaid        dtpaymentamt
     C                   movel     eoast         dtstate
     C                   movel     eafein        dtprovtaxid
    C*
    C*
     C                   movel(p)  detail        y1data
    C*
    C*Populating fields for FDY1P file
    C*
     C                   movel     dtpayrqdt     y1rqdt
     C                   movel     easinv        y1rvw#
     C                   movel     eawcpc        y1chk#
     C                   movel     paydt1        y1ckdt
     C                   move      eapaid        y1paid
     C                   movel     eoast         y1st
     C                   movel     eafein        y1txid
    C*
     C                   write     py101
     C                   write     py102
    C*
     C                   eval      count = count + 1
    C*
     C                   clear                   dtpayrqdt
     C*** Beg Add *** 04/04/13 *********************************************
     C                   eval      eaedi  = 'S'
     C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
     C                   movel     q1pgm         eapgm
     C*
     C                   update    pea01
     C*** End Add *** 04/04/13 *********************************************
     C                   endsr
     C****************************************************************
     C* getmdt - Subroutine to retrieve the check mailing date       *
     C****************************************************************
     C*
     C     getmdt        begsr
     C*
     C* USE THE CHECK MAILING DATE IF NOT BLANK
     C*
     C     @@wcif        ifeq      'Y'
     C*
     C                   movel     easub#        ##sub#            5
     C                   movel     eafnd         #1fnd             3
     C*
     C* DETEMINE IF STANDALONE ACCOUNT
     C*
     C                   MOVE      eaco#         #1co#             3
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
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#
     c*
     c*  Was the outfile written to
     c*
     c****               eval      count = 0
     c**** *hival        setgt     py101
     c****               readp     py101
     c****               if        not %eof
     c****               eval      countc = %subst(y1data:34:6)
     c****               move      countc        count
     c****               delete    py101
     c****               endif
     C*
     C*** Beg Add *** 11/10/16 *******************************************
      /Free
       //*** Beg Chg *** 05/01/17 ****************************************
       //**********      Chain 1 SMiep;
       //**********      If %found(SMiep);
            rtvSettings(@@Cchg:@@Cifs:@@Csts:@@Cdny:@@Ccvfl:@@Cgrp:
                         @@Cemp:@@Eifs:@@Eagt:@@Eemp:@@Fifs:@@Ftyp:
                         @@Udup:@@Udrc:@@Urel:@@Upcl:@@Updn:@@Udpn:
                         @@ClDays:@@IncZro:@@Crcd:@@CrDth:@@RtCd);
                         if @@RtCd = '1';
       //*** End Chg *** 05/01/17 ****************************************
                           isSMiep = 'Y';
                         Endif;
      /End-Free
     C*** End Add *** 11/10/16 *******************************************
     C                   ENDSR
