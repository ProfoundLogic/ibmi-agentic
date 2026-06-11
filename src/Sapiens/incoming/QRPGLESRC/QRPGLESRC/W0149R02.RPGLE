     HCopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
     H/TITLE W0149R02 - Maintenance Employers Cross-Reference
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: Menu                                               *
     H*                                                               *
     H* CALLS TO:  W0010R - Work With Groups                          *
     H*            W0050R - Work with Employers                       *
     H*                                                               *
     H* CREATED: 09/26/01                                             *
     H*                                                               *
     H* 09/26/01 C:136 PROJECT WC577                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/04/17 C:180 Project C133WQD                                *
     H*                Add new x-ref renewal flag.                    *
     H*                on Employers Cross Reference entry.            *
     H*                                                               *
     H* 07/16/13 C:283 Support WA099 82566                            *
     H*                Should not allow Direct Circular reference     *
     H*                on Employers Cross Reference entry.            *
     H*                                                               *
     H* 09/03/08 C:062 Project WC1611                                 *
     H*                Update the new/renewal flag when x-ref records *
     H*                are added, deleted or changed                  *
     H*                                                               *
     H* 04/26/02 C:136 Support                                        *
     H*                Allow user to return to W/W w/0 error message  *
     H*                if no changes were made in change mode.        *
     H*                                                               *
     H*                                                               *
     H* 12/12/01 C:136 Support WA093 #30230                           *
     H*                *in 73 is turned on in error, deleted errant   *
     H*                code.                                          *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  71 - Invalid fund (WCR0508)                                  *
     H*  72 - Sccf# cannot be blank (WCR0503)                         *
     H*  73 - Current record is invalid (WCR0505)                     *
     H*  74 - Position cursor to SCCE#                                *
     H*  75 - Scce# cannot be blank (WXR0503)                         *
     H*  76 - Position cursor to SCPF#                                *
     H*  77 - Scpf# has invalid fund (WCR0508)                        *
     H*  78 - Scpf# cannot be blank (WCR0503)                         *
     H*  79 - Fund, emp, div invalid record (WCR0505)                 *
     H*  80 - Position cursor to scce#                                *
     H*  81 - Scce# cannot be blank                                   *
     H*  82 - Position cursor to sccar                                *
     H*  83 - Sccar must be Y or blank                                *
     H*  84 - Current and prior info cannot be alike
     H*  85 - Record already exists
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0149f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwzexp     uf a e             disk
     F                                     rename(pex01:dbrcdf)
     F*  Loss Constant logical (for error check routine)
     Fwzexl     if   e           k disk
      *** beg add **** 04/26/02 ***************************************     ************************
     Fwzexl1    if   e           k disk
     F                                     rename(pex01:pex02)
      *** end add **** 04/26/02 ***************************************     ************************
     F*  Fund file (for getting fund name and for validation)
     Fwmfnl     if   e           k disk    infds(dbfdsf)
     F*  Employers file (for getting employer name and for validation)
     Fwmeml     if   e           k disk
     F*  Employers file (for search routine)
     Fwmemp     if   e             disk    rename(pem01:pem02)
      **** Begin Add 05/04/17 *********************************************
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwmnbl     uf   e           k disk    rename(pnb01:pnb00)
     fwdrtl     if a e           k disk    rename(prt01:prt00)
     fwaqtl7    uf   e           k disk    rename(pqt01:pqt07)
    D*---------------------------------------------------------------------
    D* Prototypes
    D*---------------------------------------------------------------------
     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d                                3
     dRtvEnvLib        pr                  ExtPgm('RTVENVLIB')
     d ##ReqType                     10    const
     d ##Lib                         10    const
     d ##MLib                        10    const
      **** End   Add 05/04/17 *********************************************
    D*---------------------------------------------------------------------
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Employer Cross-Reference'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D dbfdsf          ds
     D   dbrrnf              397    400B 0
     D ##co#           s              3  0
     D ##fnd           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D kkfnd           s                   like(exfnd)
     D kkemp#          s                   like(exemp#)
     D kkdiv           s                   like(exdiv)
     D qqkc            s              1
     D qqkl            s              1
     D qqcall          s              1
     D qqco#           s              3
     D qqfnd           s              3
     D qqfyr           s              3
     D qqview          s              1  0
     D qqrrn           s              9  0
     D qqrrn2          s              9  0
     D qqqte           s              1
      *** beg add **** 04/26/02 ***************************************     ************************
     D svcf#           s                   like(exfnd)
     D svce#           s                   like(exemp#)
     D svced#          s                   like(exdiv)
     D svpf#           s                   like(exfnd)
     D svpe#           s                   like(exemp#)
     D svped#          s                   like(exdiv)
      *** end add **** 04/26/02 ***************************************     ************************
      *** beg add **** 09/03/08 ***************************************     ************************
     D #1co#           s              3  0
     D #1fnd           s              3  0
     D #1emp#          s              9  0
      *** end add **** 09/03/08 ***************************************     ************************
      *** Beg add **** 05/04/17 ***************************************     ************************
     d cltId           s              3a
     d hasOneYear      s               n
     d isQuote         s               n
     d svFyr           s              3  0
     d x               s              1  0
     d ##ReqType       s             10    inz('*CURRENT')
     d ##Lib           s             10
     d ##MLib          s             10
      *** End add **** 05/04/17 ***************************************     ************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     fnky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
     C*
     C     emky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
      *
     C     exky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C*** begin add *** 09/03/08 *******************************************
     C                   eval      sccf# = exfnd
     C                   eval      scce# = exemp#
     C                   eval      scpf# = exofnd
     C                   eval      scpe# = exoemp
     C*** end   add *** 09/03/08 *******************************************
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
      *
      *** beg add **** 04/26/02 ***************************************     ************************
     C                   if        *in40
      *** end add **** 04/26/02 ***************************************     ************************
     C                   if        *in90 = *off and sccf# = 0
     C                   seton                                        7290      sccf# = blanks
     C                   endif
      *
     C                   if        *in90 = *off and sccf# <> 0
     C                   eval      kkfnd = sccf#
     C     fnky          chain     pfn01                              60
     C   60              seton                                        7190      invalid fund
     C                   endif
      *
     C                   if        *in90 = *off and scce# = 0
     C                   seton                                        7590      scce# = blanks
     C                   endif
      *
      *** beg del **** 12/12/01 ********************************************************************
     C****************   if        *in90 = *off and scce# <> 0
     C****************   eval      kkfnd = sccf#
     C****************   eval      kkemp#==*scce#
     C*****emky*******   chain     pem01 = *off                       60
     C***60***********   seton     *in90 = *off                       7390      invalid record
     C****************   endif     *in90 = *off
      *** end del **** 12/12/01 ********************************************************************
      *
     C                   if        *in90 = *off
     C                   eval      kkfnd = sccf#
     C                   eval      kkemp# = scce#
     C                   eval      kkdiv = scced#
     C     emky          chain     pem01                              60
     C   60              seton                                        7390      invalid record
     C                   endif
      *
      *** beg add **** 04/26/02 ***************************************     ************************
     C                   if        *in90 = *off
     C     exky          chain     pex01                              60
     C  N60              seton                                        8590
     C                   endif                                                  *in40
      *** end add **** 04/26/02 ***************************************     ************************
      *
     C                   if        *in90 = *off and scpf# = 0
     C                   seton                                        7890      sccf# = blanks
     C                   endif
      *
     C                   if        *in90 = *off and scpf# <> 0
     C                   eval      kkfnd = scpf#
     C     fnky          chain     pfn01                              60
     C   60              seton                                        7790      invalid fund
     C                   endif
      *
     C                   if        *in90 = *off and scpe# = 0
     C                   seton                                        8190      scce# = blanks
     C                   endif
      *
      *** beg del **** 12/12/01 ********************************************************************
     C****************   if        *in90 = *off and scpe# <> 0
     C****************   eval      kkfnd = scpf#
     C****************   eval      kkemp# = scpe#
     C*****emky*******   chain     pem01                              60
     C***60***********   seton                                        7990      invalid record
     C****************   endif
      *** end del **** 12/12/01 ********************************************************************
     C                   if        *in90 = *off
     C                   eval      kkfnd = scpf#
     C                   eval      kkemp# = scpe#
     C                   eval      kkdiv = scped#
     C     emky          chain     pem01                              60
     C   60              seton                                        7990      invalid record
     c                   endif
      *
      *** beg add **** 04/26/02 ***************************************     ************************
     C                   if        *in90 = *off
     C     exky          chain     pex02                              60
     C  N60              seton                                        8690
     C                   endif                                                  *in40
      *
     C                   endif                                                  *in40
      *** end add **** 04/26/02 ***************************************     ************************
      *
     C                   if        *in90 = *off and sccar <> 'Y'
     C                             and sccar <> *blank
     C                   seton                                        8390
     C                   endif
      *
     C                   if        *in90 = *off and sccf# = scpf# and
     C                             scce# = scpe# and scced# = scped#
     C                   seton                                        8490
     C                   endif
      *
      *** beg chg **** 04/26/02 ******************************************
     C*****************  if        *in90 = *off and sccf# <> 0 and
     C*****************            scce# <> 0
     C*****************  eval      kkfnd = sccf#
     C*****************  eval      kkemp# = scce#
     C*****************  eval      kkdiv = scced#
     C*****exky********  chain     pex01                              60
      *
     C                   if        not(*in90) and not(*in40)
     C                   if        (scpf# <> svpf#)
     C                             or (scpe# <> svpe#)
     C                             or (scped# <> svped#)
     C                   eval      kkfnd = scpf#
     C                   eval      kkemp# = scpe#
     C                   eval      kkdiv = scped#
     C     exky          chain     pex02                              60
      *** end chg **** 04/26/02 ******************************************
     C  N60              seton                                        8690
      *** beg add **** 04/26/02 ******************************************
     C                   endif
      *** end add **** 04/26/02 ******************************************
     C                   endif
      *** beg add **** 07/16/13 ***************************************     ************************
     C                   if        *in90 = *off
     C                   eval      kkfnd = sccf#
     C                   eval      kkemp# = scce#
     C                   eval      kkdiv = scced#
     C     exky          setll     pex02
     C     exky          reade     pex02
     C                   Dow       not %eof
     C                   if        SCPE# = EXEMP# and
     C                             SCPF# = EXFND
     C                   eval      *in90 = *on
     C                   eval      *in87 = *on
     C                   endif
     C     exky          reade     pex02
     C                   enddo
      *
     C                   endif
      *** end add **** 07/16/13 ***************************************     ************************

      *** beg add **** 05/04/17 ****************************
      /Free
       errRnwl = *Off;
       If scRnwl = 'Y' and Not(*In90);
          Setgt  (##co#:scpf#:scpe#:scped#) pel00;
          Readpe (##co#:scpf#:scpe#:scped#) pel00;
          svFyr = ELfyr;
          If ELnwrw <> 'R';
             *In90   = *On;
             errRnwl = *On;
             msgRnwl = 'Prior employer must be renewal business.';
          Endif;
       Endif;
      /End-Free
      *** end add **** 05/04/17 ****************************
      *
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* KJDO - Search routines
    C*================================================================
     C     kjdo          begsr
      *
     C                   if        hfld = 'SCCF#'                               search on fund
     C                   move(p)   ##co#         qqco#
     C                   call      'W0010R'
     C     *inkc         parm                    qqkc
     C     *inkl         parm                    qqkl
     C                   parm      '2'           qqcall
     C                   parm                    qqco#
     C                   parm      *zero         qqfnd
     C                   parm                    qqfyr
     C                   parm      1             qqview
      *
     C                   if        qqfnd <> *zero
     C                   move(p)   qqfnd         sccf#
     C                   eval      kkfnd = sccf#
     C     fnky          chain     pfn01                              60        get fnd name
     C  n60              eval      sccfnm = fnname
     C   60              eval      sccfnm = *blanks
     C                   seton                                        74        position crsr scce#
     C                   endif                                                  qqfnd <> 0
     C                   endif                                                  hfld = 'SCCF#'
      *
     C                   if        hfld = 'SCCE#' or hfld = 'SCCED#'            search on emp or div
     C                   if        sccf# = *zero                                search if fund = 0
     C                   eval      qqqte = *blanks
     C                   call      'W0050R'
     C     *inkc         parm                    qqkc
     C     *inkl         parm                    qqkl
     C                   parm      '2'           qqcall
     C                   parm      0             qqrrn
     C                   parm      0             qqrrn2
     C                   parm                    qqqte
     C                   else                                                   search if fund <> 0
     C                   eval      kkfnd = sccf#
     C     fnky          chain     pfn01
     C                   call      'W0050R'
     C     *inkc         parm                    qqkc
     C     *inkl         parm                    qqkl
     C                   parm      '2'           qqcall
     C                   parm      0             qqrrn
     C                   parm      dbrrnf        qqrrn2
     C                   parm      *blank        qqqte
     C                   endif                                                  sccf# = 0
      *
     C                   if        qqrrn <> 0                                   if rcrd is selected
     C     qqrrn         chain     pem02                              60
     C                   if        not *in60
     C                   eval      sccf# = emfnd                                load screen fields
     C                   eval      scce# = ememp#
     C                   eval      sccenm = emname
     C                   eval      scced# = emdiv
     C                   eval      kkfnd = scpf#
     C     fnky          chain     pfn01                              60        get fund name
     C  n60              eval      sccfnm = fnname
     C   60              eval      sccfnm = *blanks
     C                   seton                                        76        position to scpf#
     C                   endif                                                  not 60
     C                   else
     C                   if        *inkl = *on                                  position to scce#
     C                   seton                                        74        if F12 is selected
     C                   endif
     C                   endif                                                  qqrrn <> 0
     C                   endif                                                  hfld = 'SCCE#' or hf
      *
     C                   if        hfld = 'SCPF#'                               search on prior fund
     C                   move(p)   ##co#         qqco#
     C                   call      'W0010R'
     C     *inkc         parm                    qqkc
     C     *inkl         parm                    qqkl
     C                   parm      '2'           qqcall
     C                   parm                    qqco#
     C                   parm      *zero         qqfnd
     C                   parm                    qqfyr
     C                   parm      1             qqview
      *
     C                   if        qqfnd <> *zero                               if record selected
     C                   move(p)   qqfnd         scpf#
     C                   eval      kkfnd = scpf#
     C     fnky          chain     pfn01                              60        get fnd name
     C  n60              eval      scpfnm = fnname
     C   60              eval      scpfnm = *blanks
     C                   seton                                        80        position to scpe#
     C                   else
     C                   if        *inkl = *on                                  postion to scpf# if
     C                   seton                                        76        F12 is selected
     C                   endif
     C                   endif                                                  qqfnd <> 0
     C                   endif                                                  hfld = 'SCPF#'
      *
     C                   if        hfld = 'SCPE#' or hfld = 'SCPED#'            search on emp or div
     C                   if        scpf# = *zero                                search if fund = 0
     C                   eval      qqqte = *blanks
     C                   call      'W0050R'
     C     *inkc         parm                    qqkc
     C     *inkl         parm                    qqkl
     C                   parm      '2'           qqcall
     C                   parm      0             qqrrn
     C                   parm      0             qqrrn2
     C                   parm                    qqqte
     C                   else
     C                   eval      kkfnd = scpf#                                search if fund <> 0
     C     fnky          chain     pfn01
     C                   call      'W0050R'
     C     *inkc         parm                    qqkc
     C     *inkl         parm                    qqkl
     C                   parm      '2'           qqcall
     C                   parm      0             qqrrn
     C                   parm      dbrrnf        qqrrn2
     C                   parm      *blank        qqqte
     C                   endif                                                  scpf# = 0
      *
     C                   if        qqrrn <> 0                                   if rcrd is selected
     C     qqrrn         chain     pem02                              60
     C                   if        not *in60
     C                   eval      scpf# = emfnd                                load screen fields
     C                   eval      scpe# = ememp#
     C                   eval      scpenm = emname
     C                   eval      scped# = emdiv
     C                   eval      kkfnd = scpf#
     C     fnky          chain     pfn01                              60        get fund name
     C  n60              eval      scpfnm = fnname
     C   60              eval      scpfnm = *blanks
     C                   seton                                        82        position to sccar
     C                   endif                                                  not 60
     C                   else
     C                   if        *inkl = *on                                  postion to scpe# if
     C                   seton                                        80        F12 is selected
     C                   endif
     C                   endif                                                  qqrrn <> 0
     C                   endif                                                  hfld = 'SCPE#' or hf
      *
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      sccf# = exfnd
     C                   eval      scce# = exemp#
     C                   eval      scced# = exdiv
     C                   eval      scpf# = exofnd
     C                   eval      scpe# = exoemp
     C                   eval      scped# = exodiv
     C                   eval      sccar = excar
      *** end add **** 04/26/02 ***************************************     ************************
     C                   eval      svcf# = exfnd
     C                   eval      svce# = exemp#
     C                   eval      svced# = exdiv
     C                   eval      svpf# = exofnd
     C                   eval      svpe# = exoemp
     C                   eval      svped# = exodiv
      *** beg add **** 04/26/02 ***************************************     ************************
     C*
     C*  Get current fund and employer name
     C                   eval      kkfnd = exfnd
     C                   eval      kkemp# = exemp#
      *** beg add **** 04/26/02 ***************************************
     C                   eval      kkdiv = exdiv
      *** end add **** 04/26/02 ***************************************
     C     fnky          chain     pfn01                              60
     C  n60              eval      sccfnm = fnname
     C   60              eval      sccfnm = *blanks
     C     emky          chain     pem01                              60
     C  n60              eval      sccenm = emname
     C   60              eval      sccenm = *blanks
     C*
     C*  Get prior fund and employer name
     C                   eval      kkfnd = exofnd
     C                   eval      kkemp# = exoemp
      *** beg add **** 04/26/02 ***************************************
     C                   eval      kkdiv = exodiv
      *** end add **** 04/26/02 ***************************************
     C     fnky          chain     pfn01                              60
     C  n60              eval      scpfnm = fnname
     C   60              eval      scpfnm = *blanks
     C     emky          chain     pem01                              60
     C  n60              eval      scpenm = emname
     C   60              eval      scpenm = *blanks
     C*
      *** Beg Add *** 05/04/17 ****************************************
      /Free
       SCrnwl = EXrnwl;
       If scRnWl = 'Y';
          DsblRnl = '1';
       Endif;
      /End-Free
      *** End Add *** 05/04/17 ****************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      exco# = ##co#
     C                   eval      exfnd = sccf#
     C                   eval      exemp# = scce#
     C                   eval      exdiv = scced#
     C                   eval      exofnd = scpf#
     C                   eval      exoemp = scpe#
     C                   eval      exodiv = scped#
     C                   eval      excar = sccar
      *** Beg Add *** 05/04/17 ****************************************
      /Free
       EXrnwl = SCrnwl;
      /End-Free
      *** End Add *** 05/04/17 ****************************************

     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
      *
     C                   if        *inkj = *on
     C                   eval      *in90 = *on
     C                   exsr      kjdo
     C                   endif
      *
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C*** begin add *** 09/03/08 *******************************************
     C*
     C* If in delete/update modes lets update the employer new/renew flag
     C*
     c                   if        @@mode='E' or @@mode='C'
     c*
     c* Update the old employer detail
     c*
     c                   call      'WA070R'
     c                   parm      ##co#         #1co#
     c                   parm      scpf#         #1fnd
     c                   parm      scpe#         #1emp#
     c*
     c* Update the current employer detail
     c*
     c                   call      'WA070R'
     c                   parm      ##co#         #1co#
     c                   parm      sccf#         #1fnd
     c                   parm      scce#         #1emp#
     c*
     c* Update the ex-old employer detail during a change
     c*
     c                   if        @@mode='C'
     c                   call      'WA070R'
     c                   parm      ##co#         #1co#
     c                   parm      svpf#         #1fnd
     c                   parm      svpe#         #1emp#
     c                   endif
     c*
     c                   endif
     C*** end   add *** 09/03/08 *******************************************
     C                   move      *on           *inlr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   if        @@mode = 'A'
     C                   seton                                        40            add mode
     C                   endif
      **** Begin Add **** 05/04/17 ************************************
      /Free
       GetClientId(CltId);
       If CltId = '133';
          VisRnl = '1';
          DsblRnl = '0';
          Callp RtvEnvLib(##ReqType:##Lib:##MLib);
          Select;
          When %Scan('QTE' :##Lib) <> 0;
             isQuote = *On;
          When %Scan('PDS' :##Lib) <> 0;
             isQuote = *Off;
          When %Scan('DTAQ':##Lib) <> 0;
             isQuote = *On;
          When %Scan('QAQ' :##Lib) <> 0;
             isQuote = *On;
          Other;
             isQuote = *Off;
          Endsl;
          If isQuote = *On;
             strRnwTip = 'Prior employer must be a renewal quote. This ' +
              'flag cannot be unchecked.';
          Else;
             strRnwTip = 'Prior employer must be in renewal status. This ' +
              'flag cannot be unchecked.';
          Endif;
       Else;
          VisRnl = '0';
          DsblRnl = '1';
       Endif;
      /End-Free
      **** End   Add **** 05/04/17 ************************************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      exuser = q1user
     C                   eval      expgm = q1pgm
     C                   time                    dsdttm
     C                   eval      extime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         exdt
     C                   update    dbrcdf
      *** beg add ***** 05/04/17 ******************************
     c                   Exsr      ChangeStatus
      *** end add ***** 05/04/17 ******************************
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      exuser = q1user
     C                   eval      exiusr = q1user
     C                   eval      expgm = q1pgm
     C                   time                    dsdttm
     C                   eval      extime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         exdt
     C                   movel(p)  srdto         exidt
     C                   write     dbrcdf
     C                   feod      wzexp
      *** beg add ***** 05/04/17 ******************************
     c                   Exsr      ChangeStatus
      *** end add ***** 05/04/17 ******************************
     C*** begin add *** 09/03/08 *******************************************
     C*
     C* If in add mode lets update the employer new/renew flag
     C*
     c*
     c* Update the old employer detail
     c*
     c                   call      'WA070R'
     c                   parm      ##co#         #1co#
     c                   parm      scpf#         #1fnd
     c                   parm      scpe#         #1emp#
     c*
     c* Update the current employer detail
     c*
     c                   call      'WA070R'
     c                   parm      ##co#         #1co#
     c                   parm      sccf#         #1fnd
     c                   parm      scce#         #1emp#
     c*
     C*** end   add *** 09/03/08 *******************************************
     C                   endsr
      *** beg add ***** 05/04/17 ******************************
      *========================================================
      * ChangeStatus - changes policy to renewal status
      *========================================================
      /Free
       Begsr ChangeStatus;

       // Leave if Set as Renewal isn't checked
       If scRnwl = *blank;
           LeaveSr;
       Endif;

       // Count number of policy years
       x = 0;
       hasOneYear = *On;
       Setll (EXco#:EXfnd:EXemp#) pel00;
       Reade (EXco#:EXfnd:EXemp#) pel00;
       Dow Not(%Eof);
          x += 1;
          If x = 2;
             hasOneYear = *Off;
             Leave;
          Endif;
       Reade (EXco#:EXfnd:EXemp#) pel00;
       Enddo;

       // Delete new business record if in quote or if
       //  current employer has only one policy year.
       If isQuote or hasOneYear;
          Delete (EXco#:EXfnd:EXemp#) pnb00;
       Endif;

       // Update Quote Tracking to Renewal Pending if in quote.
       If isQuote;
          Chain  (EXco#:EXfnd:EXemp#:EXdiv:'Q') pqt07;
          If %Found;
             QTtype = 'p';
             Update pqt07;
          Endif;
       Endif;

       // Write renewal tracking if it doesn't exist
       Chain (EXco#:EXfnd:EXemp#:svFyr) prt00;
       If Not(%Found);
          RTco#  = EXco#;
          RTfnd  = Exfnd;
          RTemp# = Exemp#;
          RTfyr  = svfyr;
          RTycdt = EXdt;
          RTuser = Q1User;
          RTiusr = Q1User;
          RTpgm  = Q1pgm;
          RTipgm = q1pgm;
          RTdt   = EXdt;
          RTidt  = EXdt;
          RTtime = EXtime;
          RTitm  = EXtime;
          Write prt00;
       Endif;

       Endsr;
      /End-Free
      *** end add ***** 05/04/17 ******************************
