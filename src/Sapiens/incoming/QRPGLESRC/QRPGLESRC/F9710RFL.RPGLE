    H*****************************************************************
    H** (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 2004
    H*****************************************************************
    H*    PROGRAM: F9710RFL
    H*    DESCRIPTION: PROMPT FOR FL EDI EXTRACT CREATE WORKFILE
    H*                 CREATE WORKFILE
    H*
    H*    CALLED FROM:
    H*      MENU
    H*    CALLS TO:
    H*      F9710CFL - SUBMITTER FOR F9711CFL
    H*****************************************************************
    H** CREATED: 04/15/04 C: 79 PROJ: FL046
    H*****************************************************************
    H** REVISIONS:
     H*
     A*  03/03/06  C: 079 Support: 50887
     A*                   Submit F9711CFL for quicker processing
     H*
     A*  01/30/06  C: 079 Support: 51212
     A*                   Compile only for screen change
     H*
     H*  08/10/05  C: 79  Project: FL054
     H*                   Exclude Test Groups unless its a test run
    H*
    H*  06/21/04  C: 62  PROJECT 51MFP
    H*                   APPLY 51MDD MOD
    H*
    H*****************************************************************
    H*  INDICATOR USAGE:                                             *
    H*  SCR01:                                                       *
    H*  25     INVALID TYPE OF SUBMISSION                            *
    H*  26     INVALID VALUE FOR TEST/PRODUCTION INDICATOR           *
    H*  27     INVALID VALUE FOR ORIGINAL SUBMISSION INDICATOR       *
    H*  SCR02:                                                       *
    H*  20     INVALID FROM DATE                                     *
    H*  21     INVALID TO DATE                                       *
    H*  22     DATES MUST BE IN ASCENDING ORDER                      *
    H*  23     INVALID FUND 1                                        *
    H*  24     INVALID FUND 2                                        *
    H*  25     INVALID FUND 3                                        *
    H*  26     INVALID FUND 4                                        *
    H*  27     INVALID FUND 5                                        *
    H*  28     INVALID FUND 6                                        *
    H*  29     INVALID FUND 7                                        *
    H*  30     INVALID FUND 8                                        *
    H*  31     INVALID FUND 9                                        *
    H*  32     INVALID FUND 10                                       *
    H*  33     INVALID FUND 11                                       *
    H*  34     INVALID FUND 12                                       *
    H*  35     INVALID FUND 13                                       *
    H*  36     INVALID FUND 14                                       *
    H*  37     INVALID FUND 15                                       *
    H*  38     INVALID FUND 16                                       *
    H*  39     INVALID FUND 17                                       *
    H*  40     INVALID FUND 18                                       *
    H*  41     INVALID FUND 19                                       *
    H*  42     INVALID FUND 20                                       *
    H*  43     INVALID FUND 21                                       *
    H*  44     INVALID FUND 22                                       *
    H*  45     INVALID FUND 23                                       *
    H*  46     INVALID FUND 24                                       *
    H*  47     INVALID FUND 25                                       *
    H*  48     INVALID FUND 26                                       *
    H*  49     INVALID FUND 27                                       *
    H*  50     INVALID FUND 28                                       *
    H*  51     INVALID FUND 29                                       *
    H*  52     INVALID FUND 30                                       *
    H*  53     INVALID FUND 31                                       *
    H*  54     INVALID FUND 32                                       *
    H*  55     INVALID FUND 33                                       *
    H*  56     INVALID FUND 34                                       *
    H*  57     INVALID FUND 35                                       *
    H*  58     INVALID FUND 36                                       *
    H*  59     INVALID FUND 37                                       *
    H*  60     INVALID FUND 38                                       *
    H*  61     INVALID FUND 39                                       *
    H*  62     INVALID FUND 40                                       *
    H*  63     INVALID FUND 41                                       *
    H*  64     INVALID FUND 42                                       *
    H*  65     INVALID FUND 43                                       *
    H*  66     INVALID FUND 44                                       *
    H*  67     INVALID FUND 45                                       *
    H*  68     INVALID FUND 46                                       *
    H*  69     INVALID FUND 47                                       *
    H*  70     INVALID FUND 48                                       *
    H*  71     INVALID FUND 49                                       *
    H*  72     INVALID FUND 50                                       *
    H*  73     INVALID FUND 51                                       *
    H*  74     INVALID FUND 52                                       *
    H*  75     INVALID FUND 53                                       *
    H*  76     INVALID FUND 54                                       *
    H*  77     INVALID FUND 55                                       *
    H*  78     INVALID FUND 56                                       *
    H*  79     INVALID FUND 57                                       *
    H*  80     INVALID FUND 58                                       *
    H*  81     INVALID FUND 59                                       *
    H*  82     INVALID FUND 60                                       *
    H*  96     INDICATES THIS IS NOT AN ORIGINAL SUBMISSION          *
    H*  97     INVALID FUND - DOES NOT EXIST                         *
    H*  98     INVALID FUND - CONTAINS EMPTY GROUP AUXILIARY INFOR.  *
    H*  99     GENERAL CHAIN                                         *
    H*  SCR03:                                                       *
    H*  71     INVALID VALUE FOR RESETTING COUNTER MUST BE Y OR N    *
    H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff9710ffl  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     infds(fids01)
     Fsmcol     if   e           k disk
     Fsmupl     if   e           k disk
     Ffmfnl     if   e           k disk
     Ffmsml     if   e           k disk
     Ffmtsp     if   e           k disk    usropn
    F*
     Ffdf2l     if   e           k disk
     Fftgtp     if   e             disk    rename(pgt01:pgt02)
     Fftgtl     if   e           k disk
     D                 ds
     D  fnd                    1    180  0
     D                                     dim(60)
     D  scfn01                 1      3  0
     D  scfn02                 4      6  0
     D  scfn03                 7      9  0
     D  scfn04                10     12  0
     D  scfn05                13     15  0
     D  scfn06                16     18  0
     D  scfn07                19     21  0
     D  scfn08                22     24  0
     D  scfn09                25     27  0
     D  scfn10                28     30  0
     D  scfn11                31     33  0
     D  scfn12                34     36  0
     D  scfn13                37     39  0
     D  scfn14                40     42  0
     D  scfn15                43     45  0
     D  scfn16                46     48  0
     D  scfn17                49     51  0
     D  scfn18                52     54  0
     D  scfn19                55     57  0
     D  scfn20                58     60  0
     D  scfn21                61     63  0
     D  scfn22                64     66  0
     D  scfn23                67     69  0
     D  scfn24                70     72  0
     D  scfn25                73     75  0
     D  scfn26                76     78  0
     D  scfn27                79     81  0
     D  scfn28                82     84  0
     D  scfn29                85     87  0
     D  scfn30                88     90  0
     D  scfn31                91     93  0
     D  scfn32                94     96  0
     D  scfn33                97     99  0
     D  scfn34               100    102  0
     D  scfn35               103    105  0
     D  scfn36               106    108  0
     D  scfn37               109    111  0
     D  scfn38               112    114  0
     D  scfn39               115    117  0
     D  scfn40               118    120  0
     D  scfn41               121    123  0
     D  scfn42               124    126  0
     D  scfn43               127    129  0
     D  scfn44               130    132  0
     D  scfn45               133    135  0
     D  scfn46               136    138  0
     D  scfn47               139    141  0
     D  scfn48               142    144  0
     D  scfn49               145    147  0
     D  scfn50               148    150  0
     D  scfn51               151    153  0
     D  scfn52               154    156  0
     D  scfn53               157    159  0
     D  scfn54               160    162  0
     D  scfn55               163    165  0
     D  scfn56               166    168  0
     D  scfn57               169    171  0
     D  scfn58               172    174  0
     D  scfn59               175    177  0
     D  scfn60               178    180  0
     D                 ds
     D  qqfnds                 1    180
     D                                     dim(60)
    D* CURRENT DATE
     D                 ds
     D xxdt                           6  0
     D  xxmm                          2  0 overlay(xxdt)
     D  xxdd                          2  0 overlay(xxdt:3)
     D  xxyy                          2  0 overlay(xxdt:5)
    D*
    D* Year prior to current date's year; used to calc Julian date
     D                 ds
     D zzdt                           6  0
     D  zzmm                          2  0 overlay(zzdt)
     D  zzdd                          2  0 overlay(zzdt:3)
     D  zzyy                          2  0 overlay(zzdt:5)
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D                 ds
     D juldate                        5  0
     D  xxyear                        2  0 overlay(juldate)
     D  xxday                         3  0 overlay(juldate:3)
    D*
     D                 ds
     D xxrecf                        10
     D  xxscr#                        4    overlay(xxrecf:2)
    D*
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D qqcall          s              1
     D qqco#           s              3
     D qqfdt           s              8
     D qqjdt           s              3
     D qqmode          s              1
     D qqoutq          s             10
     D qqprds          s             40
     D qqproc          s              1
     D qqrrcd          s              2
     D qqset           s              1
     D qqosub          s              1
     D qqtpin          s              1
     D qqsub#          s              5
     D qqtdt           s              8
     D qqyr            s              2
     D qqfndstr        s            360
     D qqfndstr1       s            360    inz(*blanks)
     D sepfnd          s              1
     D srdays          s              5  0
     D srdfld          s             80
     D srdlen          s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srfrdc          s              6
     D srterm          s              1
     D srtfmt          s              8
     D srtodc          s              6
     D x               s              2  0
     D xxfdt           s              7  0
     D xxfnd           s                   like(fnfnd)
     D xxtdt           s              7  0
     D xxdat           s              3  0
     D y               s              2  0
     D n               s              2  0
     D i               s              2  0
     D kkco#           s                   like(upco#)
     D kksub#          s                   like(upsub#)
     D kkfnd           s                   like(upfnd)
     D kkuser          s                   like(upuser)
     D kkst            s                   like(f2st)
     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C* Key List for the EDI Transaction Run File
    C*
     C     tskey         klist
     C                   kfld                    kkco#
    C*
    C* Key List for the Company Master File
    C*
     C     cokey         klist
     C                   kfld                    kkco#
    C*
    C* Key List for the User Parameter Master File
    C*
     C     upkey         klist
     C                   kfld                    kkuser
    C*
    C* Key List for the Group Master File
    C*
     C     fnkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
    C*
     C     fnkey1        klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
    C*
    C* USED TO FIND ASSOCIATED FIELDS FOR VERIFYING GROUP AUXILIARY
    C* FIELDS ARE NOT EMPTY.  WILL NOT PROCESS FUNDS WITH EMPTY
    C* AUXILIARY FIELDS.
    C*
    C* Key List for the GROUP STATE INFORMATION FILE
    C*
     C     f2key         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
    C*
    C* Key List for the SUBCRIBER FILE
    C*
     C     smkey         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
    C*
    C* Key List for the Generic Table File
    C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
    C*
    C*  Entry Parameters
    C*
     C     *entry        plist
     C                   parm                    qqprds
     C                   parm                    qqcall
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
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
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C******************************************************************
    C* Begin Mainline
    C******************************************************************
     C                   exsr      pgmstart
     C     strt01        tag
     C                   exsr      Headinfo
    C*
    C* Display Screen Formats
    C*
     C     strt          tag
    C*
     C                   if        q2recf = 'SCR01'
     C                   exfmt     scr01
     C                   endif
    C*
     C                   if        q2recf = 'SCR02'
     C                   exfmt     scr02
     C                   endif
    C*
     C                   if        q2recf = 'SCR03'
     C                   exfmt     scr03
     C                   endif
    C*
    C* Process Function Keys
    C*
     C                   if        *in11 = *on
     C                   exsr      othfks
     C                   endif
     C*
     C* Process screens
    C*
     C     q2recf        caseq     'SCR01'       Scr01p
     C     q2recf        caseq     'SCR02'       Scr02p
     C*
     C                   endcs
    C*
    C* End the Program
    C*
     C     term          tag
    C*
     C                   move      *on           *inlr
    C*
     C*=====================================================================
    C* Subroutine Headinfo
     C*=====================================================================
    C*  GET COMPANY NAME
    C*
     C     Headinfo      begsr
    C*
     C     cokey         chain     pco01                              99
     C                   if        *in99 = *on
     C                   clear                   scconm
     C                   else
     C                   movel(p)  coname        srdfld
     C                   call      'SRCENTER'
     C                   parm                    srdfld
     C                   parm      40            srdlen
     C                   movel     srdfld        scconm
     C                   endif
    C*
    C*  GET SUBCRIBER NAME
    C*
     C                   move      upsub#        scsub#
     C     smkey         chain     psm01                              99
     C                   if        *in99 = *on
     C                   clear                   scsbnm
     C                   else
     C                   movel     smname        scsbnm
     C                   endif
    C*
    C* Get the Requested Record, Load Information
    C*
     C                   move      q1user        scuser
     C                   move      qqprds        scprds
    C*
    C* Load the Record Format Information for the First Time
    C*
     C                   clear                   q2recf
     C                   movel     'SCR01'       q2recf
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* Subroutine kjdo - Search routines
     C*=====================================================================
     C*
     C     kjdo          begsr
     C                   endsr
     C*
     C*=====================================================================
     C* Subroutine moveFunds
     C*=====================================================================
     C*
     C     moveFunds     begsr
     C*
     C                   eval      i = 1
     C                   do        60            n
     C
     C                   if        fnd(n) <> 0
     C                   move      fnd(n)        qqfnds(i)
     C                   eval      i = i + 1
     C                   endif
     C
     C                   enddo
     C*
     C                   If        qqfnds(1) <> '000'
     C                   eval      qqfndstr = qqfnds(1) + ' ' +
     C                             qqfnds(2) + ' ' + qqfnds(3) + ' ' +
     C                             qqfnds(4) + ' ' + qqfnds(5) + ' ' +
     C                             qqfnds(6) + ' ' + qqfnds(7) + ' ' +
     C                             qqfnds(8) + ' ' + qqfnds(9) + ' ' +
     C                             qqfnds(10) + ' ' + qqfnds(11) + ' '+
     C                             qqfnds(12) + ' ' + qqfnds(13) + ' '+
     C                             qqfnds(14) + ' ' + qqfnds(15) + ' '+
     C                             qqfnds(16) + ' ' + qqfnds(17) + ' '+
     C                             qqfnds(18) + ' ' + qqfnds(19) + ' '+
     C                             qqfnds(20) + ' ' + qqfnds(21) + ' '+
     C                             qqfnds(22) + ' ' + qqfnds(23) + ' '+
     C                             qqfnds(24) + ' ' + qqfnds(25) + ' '+
     C                             qqfnds(26) + ' ' + qqfnds(27) + ' '+
     C                             qqfnds(28) + ' ' + qqfnds(29) + ' '+
     C                             qqfnds(30) + ' ' + qqfnds(31) + ' '+
     C                             qqfnds(32) + ' ' + qqfnds(33) + ' '+
     C                             qqfnds(34) + ' ' + qqfnds(35) + ' '+
     C                             qqfnds(36) + ' ' + qqfnds(37) + ' '+
     C                             qqfnds(38) + ' ' + qqfnds(39) + ' '+
     C                             qqfnds(40) + ' ' + qqfnds(41) + ' '+
     C                             qqfnds(42) + ' ' + qqfnds(43) + ' '+
     C                             qqfnds(44) + ' ' + qqfnds(45) + ' '+
     C                             qqfnds(46) + ' ' + qqfnds(47) + ' '+
     C                             qqfnds(48) + ' ' + qqfnds(49) + ' '+
     C                             qqfnds(50) + ' ' + qqfnds(51) + ' '+
     C                             qqfnds(52) + ' ' + qqfnds(53) + ' '+
     C                             qqfnds(54) + ' ' + qqfnds(55) + ' '+
     C                             qqfnds(56) + ' ' + qqfnds(57) + ' '+
     C                             qqfnds(58) + ' ' + qqfnds(59) + ' '+
     C                             qqfnds(60) + ' '
     C                   endif
     C*
     C                   eval      qqfndstr1 = %subst(qqfndstr:1:4*(i-1))
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* Subroutine Othfks
     C*=====================================================================
     C*
     C     Othfks        begsr
    C*
    C* F3 = EXIT
    C*
     C                   if        *inkc = *on
     C                   move      *on           *inlr
     C                   goto      term
     C                   endif
    C*
    C* F10 = SEARCH
    C*
     C                   if        *inkj = *on
     C                   exsr      kjdo
     C                   goto      strt
     C                   endif
    C*
    C* F13 = CHG USER PARMS
    C*
     C                   if        *inkm = *on
     C                   call      'F0006R'
     C     upkey         chain     pup01                              99
     C                   if        *in99 = *on
     C                   clear                   upco#
     C                   endif
     C                   goto      strt01
     C                   endif
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* Subroutine Pgmstart - Initialization section
     C*=====================================================================
     C*
     C     Pgmstart      begsr
    C*
    C*  OPEN FILES
    C*
     C                   open      fmtsp
    C*
    C*  INITIALIZE FIELDS
    C*
     C                   eval      kkuser = q1user
    C*
    C*  GET THE USER PARAMETER FILE
    C*
     C     upkey         chain     pup01                              99
     C                   if        *in99 = *on
     C                   clear                   upco#
     C                   else
     C                   eval      kkco#  = upco#
     C                   eval      kksub# = upsub#
     C                   endif
    C*
     C                   time                    dsdttm
    C*
    C* Get the Julian Date
    C*
     C                   eval      xxdt = dsdate
     C                   eval      zzmm = 12
     C                   eval      zzdd = 31
     C                   if        xxyy <> 0
     C                   eval      zzyy = xxyy - 1
     C                   else
     C                   eval      zzyy = 99
     C                   endif
    C*
    C* Call sub-program SRDDFF to Calc Julian Date
    C*
     C                   move      zzdt          srfrdc
     C                   move      xxdt          srtodc
     C                   eval      srdays = 0
     C                   move      ' '           srerr
     C                   move      ' '           srterm
     C                   call      'SRDDIF'
     C                   parm                    srfrdc
     C                   parm                    srtodc
     C                   parm                    srdays
     C                   parm                    srerr
     C                   parm                    srterm
    C*
     C                   eval      xxdat = srdays
    C*
    C*  LOAD THE CURRENT JULIAN DATE
    C*
     C                   eval      xxday  = srdays
     C                   eval      xxyear = xxyy
     C*
     C                   endsr
     C*=====================================================================
     C* Subroutine Submit
     C*=====================================================================
     C     Submit        begsr
     C*
     C                   move      scfrdt        qqfdt
     C                   move      sctodt        qqtdt
     C                   move      xxdat         qqjdt
     C                   move      xxyear        qqyr
     C                   move      scsub#        qqsub#
     C                   move      upco#         qqco#
    C*
    C*  CLOSE FILES
    C*
     C                   close     fmtsp
     C*
     C*** BEG CHG *** 01/19/06 *********************************************
     C****************** call      'F9711CFL'
     C                   call      'F9710CFL'
     C*** END CHG *** 01/19/06 *********************************************
     C                   parm                    qqco#
     C                   parm                    qqsub#
     C                   parm                    qqfndstr1
     C                   parm                    qqfdt
     C                   parm                    qqtdt
     C                   parm                    qqyr
     C                   parm                    qqjdt
     C                   parm      *blanks       qqoutq
     C                   parm      *blank        qqmode
     C                   parm      scproc        qqproc
     C                   parm      scset         qqset
     C                   parm      scosub        qqosub
     C                   parm      sctpin        qqtpin
     C*
     C                   endsr
    C*
    C*=====================================================================
    C* Subroutine Scr01p
    C*=====================================================================
     C     Scr01p        begsr
    C*
    C* Check Subscriber number for a value
    C*
     C                   if        upsub# = 0
     C                   exfmt     wdwsub#
     C                   goto      term
     C                   endif
    C*
    C* Validate type of submission
    C*
     C                   if        scproc <> 'P'
     C                             and scproc <> 'H'
     C                             and scproc <> 'D'
     C                             and scproc <> 'X'
     C                             and scproc <> 'A'
     C                   move      *on           *in25
     C                   goto      strt
     C                   endif
    C*
    C* Validate Original Submission Indicator
    C*
     C                   if        scosub <> 'Y' and scosub <> 'N'
     C                   seton                                        27
     C                   goto      strt
    C*
     C                   else
    C*
     C                   if        scosub = 'N'
     C                   seton                                        96
    C*
     C                   else
     C                   setoff                                       96
    C*
     C                   endif
    C*
     C                   endif
    C*
    C* Validate Test/Production indicator
    C*
     C                   if        sctpin <> 'T' and sctpin <> 'P'
     C                   seton                                        26
     C                   goto      strt
     C                   endif
    C*
    C*  GET THE LAST RUN DATE FROM FMTSP
    C*
     C                   if        scosub = 'Y'
    C*
     C     tskey         chain(n)  pts01                              99
     C*
     C                   if        *in99 = *off and tsrndt = juldate
     C*
     C     scr03t        tag
     C                   exfmt     scr03
     C*
     C                   if        *inkl = *on
     C                   movel     'SCR01'       q2recf
     C                   goto      strt
     C                   endif
    C*
     C                   if        scset <> ' ' and scset <> 'Y' and
     C                             scset <> 'N'
    C*
     C                   seton                                        71
     C                   goto      scr03t
    C*
     C                   endif                                                  if scset <>
     C*
     C                   else
     C                   eval      scset = 'Y'
     C*
     C                   endif
     C*
     C                   endif
    C*
     C                   clear                   q2recf
     C                   movel     'SCR02'       q2recf
     C                   eval      scfrdt = *zero
     C                   eval      sctodt = *zero
     C                   eval      fnd = *zero
     C                   goto      strt
     C*
     C                   endsr
    C*
    C*=====================================================================
    C* Subroutine Scr02p
    C*=====================================================================
    C*
     C     Scr02p        begsr
    C*
     C                   eval      updt = *zero
    C* Validate the Entered "FROM DATE"
    C*
     C                   eval      srdti = scfrdt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scfrdt
     C                   if        srerr = '1'
     C                   move      *on           *in20
     C                   goto      strt
     C                   endif
    C*
    C* Validate the Entered "TO DATE"
    C*
     C                   eval      srdti = sctodt
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sctodt
     C                   if        srerr = '1'
     C                   move      *on           *in21
     C                   goto      strt
     C                   endif
    C*
    C* Validate the Entered Dates to Verify Ascending Order
    C*
     C                   eval      srdti = scfrdt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxfdt
     C                   eval      srdti = sctodt
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxtdt
     C     xxfdt         cabgt     xxtdt         strt                 22
    C*
     C* Process Resubmissions
    C*
    C                   select
    C*
     C* Process Resubmissions
    C*
    C                   when      scosub = 'N'
    C                   exsr      submit
    C                   goto      term
    C*
    C* Validate the FUND, if entered then submit the program
    C*
    C                   when      scosub = 'Y'
    C*
     C                   exsr      validfnd
     C                   exsr      moveFunds
     C                   exsr      subfnds
     C****************** exsr      upddbr
    C*
    C                   endsl
     C*
     C                   endsr
     C*
     C*=====================================================================
     C* Subroutine Subfnds
     C*=====================================================================
     C     Subfnds       begsr
    C*
    C* Call the CLP to Submit the Program for creating tape workfiles
    C* If user does not enter a fund then submit program
    C* for each valid fund within CO#
    C*
    C*
     C                   if        sepfnd = 'N'
    C*
    C*  READ ALL FUNDS AND WRITE TO SCREEN
    C*
     C                   eval      x = 1
     C     fnkey1        chain     pfn01                              99
     C                   dow       *in99 = *off
     C                             and x <= 60
     C*** BEG ADD *** 08/10/05 *********************************************
     C                   if        fntest = 'Y' and sctpin <> 'T'
     C                   goto      skip
     C                   endif
     C*** END ADD *** 08/10/05 *********************************************
     C                   movel     fnfnd         fnd(x)
     C                   eval      x = x + 1
     C*** BEG ADD *** 08/10/05 *********************************************
     C     skip          tag
     C*** END ADD *** 08/10/05 *********************************************
     C     fnkey1        reade     pfn01                                  99
     C                   enddo
     C                   write     scr02
     C                   endif
    C*
     C                   do        60            x
     C                   eval      y = x + 22
     C                   eval      xxfnd = fnd(x)
     C                   if        xxfnd <> 0
    C*
    C*  CHECK FOR VALID FIELDS IN GROUP AUXILIARY
    C*  CHAINING TO FDF2P VERIFYING FIELDS
    C*
     C                   eval      kkst   = 'FL'
     C                   eval      kkfnd = fnd(x)
    C*
     C     f2key         chain     pf201                              99
     C                   if        *in99 = *off
    C*
     C*** BEGIN ADD *** 06/21/04 **************************************
     C                   if        f2fl01 <> 'Y'
     C*** END   ADD *** 06/21/04 **************************************
     C                   if        f25n01 = *zeros
     C                             or f29n01 = *zeros
     C                             or f29n02 = *zeros
     C                   move      *on           *in(y)
     C                   eval      updt = 1
     C                   endif
     C*** BEGIN ADD *** 06/21/04 **************************************
     C                   endif
     C*** END   ADD *** 06/21/04 **************************************
     C                   else
    C*
    C*  USED IN CASE IT DOES NOT EXIST IN THE FILE
    C*
     C                   move      *on           *in(y)
     C                   eval      updt = 1
     C                   endif
    C*
     C                   endif
     C                   enddo
    C*
    C*   IF ANY FUNDS ARE INCOMPLETE, INDICATOR *IN99 WILL DISPLAY
    C*   ERROR.
    C*
     C                   if        updt = 1
     C                   move      *on           *in98
     C                   goto      strt
     C                   endif
    C*
    C*   Call the CL to build the workfile
    C*
     C                   exsr      submit
    C*
     C                   endsr
     C*
     C*=====================================================================
     C* Upddbr
     C*=====================================================================
     C***  Upddbr        begsr
     C**********
     C*****tskey         chain     pts01                              99
     C**********
     C**********         if        *in99 = *off
     C**********         eval      tsrndt = juldate
     C**********         if        scset = Y
     C**********         eval      tsseq# = 0
     C**********         endif
     C**********         update    pts01
     C**********         else
     C**********         eval      tsco# = upco#
     C**********         eval      tsseq#= 0
     C**********         eval      tsrndt = juldate
     C**********         write     pts01
     C**********         endif
    C**********
     C**********         endsr
     C*
     C*=====================================================================
     C* Subroutine Validfnd
     C*=====================================================================
     C     Validfnd      begsr
     C                   move      'N'           sepfnd
     C                   eval      x = 0
     C                   do        60            x
     C                   eval      y = x + 22
    C*
     C                   if        fnd(x) <> 0
     C                   move      'Y'           sepfnd
    C*
     C                   eval      kkfnd = fnd(x)
     C     fnkey         chain     pfn01                              99
     C                   if        *in99 = *on
     C                   move      *on           *in(y)
     C                   move      *on           *in97
    C*
     C                   goto      strt
     C                   endif
    C*
     C                   endif
    C*
     C                   enddo
     C*
     C                   endsr
