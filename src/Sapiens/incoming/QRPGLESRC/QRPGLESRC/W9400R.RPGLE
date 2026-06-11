     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H* ================================================================= *
    H/title Refresh of Suffixed Test Environment (Prompt Program)
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    H* ================================================================= *
    H*  CALLED FROM:  Menu                                               *
    H*                                                                   *
    H*  Calls To:  W9400C                                                *
    H*                                                                   *
    H*  Created  11/02/07      C:132     PROJECT:  132WJL
    H* ================================================================= *
    H*  Revisions                                                        *
    H*                                                                   *
     H*  05/27/15 C:33  Project: SY364                                    *
    H*                -Allow for the suffix to be changed.               *
    H*                -Account for standard suffix naming conventions.   *
    H*                                                                   *
     H*  01/19/10 C:198 Project: 132WJL changes                           *
    H*                 Changed the title.                                *
    H* ================================================================= *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW9400f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    f*
    f* ================================================================= *
    f*
     fsmcol     if   e           k disk
    f*
      * Group Master file
     fWMFNL     IF   E           k DISK
    f*
      * User Parm File
     fWMUPL     if   e           k disk
    f*
     fssypf     if   e             disk
     f*
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d optnm           c                   const('Test Data Refresh with +
     d                                            Production data')
     d ##co#           s              3  0
     d ##call          s              1
     d ##data          s             80
     d ##leng          s              3  0
     d #fsuffix        s              3    inz
     d #tsuffix        s              3    inz
     d xxgrp           s              3  0 inz
     d xfsuffix        s              3    inz
     d xtsuffix        s              3    inz
     d xxcall          s              1
     d xxfnd           s              3    inz
     d xxprds          s             40
     d cltid           s              3    inz
     d DataCtr         s              1    inz
     d #DataCtr        s              1    inz
     d srdti           s              8  0
     d srdto           s             10
     d srffmt          s              8
     d srtfmt          s              8
     d srerr           s              1
     d srdfld          s             80
     d srdlen          s              3  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     d*
     d fids01        e ds                  extname(szq2p)
     d psds          esds                  extname(szq1p)
     d*
     dtheSuffix        ds             3
     d syfl08                         1
     d syfl09                         1
     d syfl10                         1
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

       //***  Begin Add  ***  05/27/15  ************************************
     DsaveSuffix       s                   like(scSuffix)

    DIsLibExists      pr             1n
    DIsDevelopment    pr             1n

    DChkDev           pr                  extPgm('SACHKPDS')
     D outDev                         1n
    DChkLib           pr                  extPgm('SRCHKOBJ')
     D inObj                         10    const
     D inType                        10    const
     D outNotValid                    1
       //***  End   Add  ***  05/27/15  ************************************
     d*
    c*====================================================================
    c* KEYLIST  - List of Keys for file
    c*====================================================================
      *
      *   COMPANY FILE
     c     coky          klist
     c                   kfld                    upco#
      *
      *   User Profile File
     c     upky          klist
     c                   kfld                    q1user
      *
    c*====================================================================
    c* MAINLINE - Main processing for program
    c*====================================================================
      *
      *  GET THE USER PARAMETER FILE
      *
     c     upky          chain     pup01                              60
      *
     c                   if        (*in60)
     c                   move      *zero         upco#
     c                   endif                                                  *IN60 IFEQ
      *
     c                   move      upfnd         xxfnd
      *
      * ==================== *
      *  Setup Top of Screens
      * ==================== *
      *
     c     coky          chain     pco01                              60
     c                   movel(p)  coname        srdfld
     c                   call      'SRCENTER'
     c                   parm                    srdfld
     c                   parm      40            srdlen
     c                   movel     srdfld        scconm
      *
     c                   time                    dsdttm
     c*
     c                   if        (xxprds = *blanks)
     c                   movel(p)  optnm         xxprds
     c                   endif                                                  XXPRDS=*BLANKS
     c*
     c                   movel(p)  xxprds        ##data
     c*
     c                   call      'SRCENTER'
     c                   parm                    ##data
     c                   parm      40            ##leng
     c*
     c                   movel     ##data        scprds
      *
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     c                   movel(p)  'SCR01'       q2recf
      *
      * ==================================== *
      *   GET THE SUFFIX FOR NEW ENVIRONMENT
      * ==================================== *
      *
     C
     C                   read      ssypf                                  99
     C                   if        *in99 = *off
     C                   movel     theSuffix     xfsuffix
     C                   eval      DataCtr = %subst(syfl11:2:1)
     C                   endif
     C
      *
     c                   if        xfsuffix = *blanks
     c                   eval      #fsuffix = *blanks
     c                   call      'RTVCLTID'
     c                   parm                    cltid
     c                   eval      xtsuffix= cltid
     c                   eval      *in71 = *on
     c                   else
     c                   eval      #fsuffix = xfsuffix
     c                   eval      xtsuffix = xfsuffix
     c                   endif

       //***  Begin Add  ***  05/27/15  ************************************
      /Free
       isNoSuffix = %subst(q1User:1:3) <> 'PDS' and not IsDevelopment;
       scSuffix = %subst(%editc((%dec(xtSuffix:3:0) + 800):'X'):2:3);
      /End-free
       //***  End   Add  ***  05/27/15  ************************************
      *
     c                   movel     '9'           xtsuffix
      *
     c                   EVAL      scsufx = xtsuffix
      *
      * ==================== *
      *   DISPLAY ALL FORMATS
      * ==================== *
      *
     c     STRT          tag
      *
     c                   eval      *inkl = *off
     c*
     c     *inkc         cabeq     *on           term
      *
     c                   if        (q2recf = 'SCR01')
     c                   exfmt     scr01
     c                   endif                                                  Q2RECF IFEQ
      *
      * ========================== *
      *   PROCESS ALL COMMAND KEYS
      * ========================== *
      *
      *   COMMAND 03 = END OF JOB
      *
     c     *inkc         cabeq     *on           term
      *
      *   COMMAND 12 = Redisplay Screen
     c*
     c     *inkl         cabeq     *on           strt                            PC @ AGENT
      *
     c                   eval      *in70 = *Off
       //***  Begin Add  ***  05/27/15  ************************************
      /Free
       Clear msSuffix;
       Clear erSuffix;
       If scSuffix <> saveSuffix and not IsLibExists;
          msSuffix = 'The environment for this suffix does not exist. ' +
           'If you continue, the environment will be created for you.';
          erSuffix = *on;
          saveSuffix = scSuffix;
      /End-free
     c                   goto      strt
      /Free
       Endif;
      /End-free
       //***  End   Add  ***  05/27/15  ************************************
     c                   IF        scyorn <> 'Y'
     c                   eval      *in70 = *ON
     c                   goto      strt
     c                   endif                                                  scyorn <> "Y"
      *
     c                   move(P)   xtsuffix      #tsuffix
       //***  Begin Add  ***  05/27/15  ************************************
      /Free
       #tSuffix = scSuffix;
      /End-free
       //***  End   Add  ***  05/27/15  ************************************
     c                   move(P)   DataCtr       #DataCtr
      *
     c                   call      'W9400C'
     c                   parm                    #fsuffix
     c                   parm                    #tsuffix
     c                   parm                    #DataCtr
      *
    c*====================================================================
    c* TERM - End of program processing
    c*====================================================================
     c     TERM          TAG
     c
     c                   move      *on           *inlr
      *
     c                   return
      *
    c*====================================================================
       //***  Begin Add  ***  05/27/15  ************************************
    PIsDevelopment    b
    DIsDevelopment    pi             1n

     D isDev           s              1n
      /Free
       ChkDev(isDev);
       Return isDev;
      /End-free
    PIsDevelopment    e
    c*====================================================================
    PIsLibExists      b
    DIsLibExists      pi             1n

     D isNotValid      s              1n
      /Free
       ChkLib('PDSFIL'+scSuffix:'*LIB':isNotValid);
       Return not isNotValid;
      /End-free
    PIsLibExists      e
       //***  End   Add  ***  05/27/15  ************************************
