     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
      /title W0062R02 - Employee Information Maintenance
     H Bnddir('WTBNDDIR')
      *****************************************************************
      *
      * CALLED FROM: W0062R - Work with Employees
      * CALLED TO:   None
      *
      * created 04/12/05  C:61  Project wc1283 135WBW
      *****************************************************************
      * Revised:
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 02/17/10 C:237 SUPP 71497                                     *
     H*                FIX THE GROUP# DISPLAY FOR WORK WITH EMPLOYEES *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
      *
      *
      *****************************************************************

     *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0062f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwmeep     uf a e             disk
     F                                     rename(pee01:dbrcdf)
    
      * employee
     Fwmeel1    if   e           k disk
    
      * employer
     Fwmeml     if   e           k disk
    
      * employer policy year
     Fwdell     if   e           k disk
    
      * employer payroll
     Fwmepp     if   e             disk
    
      * employer cross reference
     Fwzexl     if   e           k disk
     F                                     rename(pex01:pex02)
    
      * Payroll Description Extension
     Fwmpxl1    if   e           k disk
    
      * Rating State
     Fwmrsl     if   e           k disk
    
      * Payroll Classification
     Fwmprl1    if   e           k disk
    
      * Table  (Billing Schedule)
     Fwtabl     if   e           k disk
     F                                     ignore(prs01)
    
     C*** BEG   CHG ***  02/17/10  ************************************
     FWMFNL     IF   E           K DISK
     C*** END   CHG ***  02/17/10  ************************************
    
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    
     dss#              ds
     d ss#1                    1      3
     d ss#2                    4      5
     d ss#3                    6      9

     D efDS            ds
     D  EMEFD1
     D  EMEFD2
     D  EMEFD3
     D  EMEFD4
     D  EMEFD5
     D ef                                  OVERLAY(efDS)
     D                                     LIKE(EMEFD1) DIM(5)

     D cxDS            ds
     D  EMCAD1
     D  EMCAD2
     D  EMCAD3
     D  EMCAD4
     D  EMCAD5
     D cx                                  OVERLAY(cxDS)
     D                                     LIKE(EMCAD1) DIM(5)


    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Employee Information'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1


    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------

     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@qte           s              1
     d ##emp#          s                   like(ememp#)
     d ##div           s                   like(emdiv)
     d ##name          s                   like(emname)
     d ##fyr           s                   like(elfyr)
     d ##agt#          s              5  0
     d ##anam          s             60
     d ##prod          s              5  0
     d ##pnam          s             60
     d x               s              7  0
     d sysdt           s              7  0
     d xxst            s                   like(rsjrst)
     d xxstdt          s                   like(elpfdt)
     d x1div           s                   like(eldiv)
     d x1pfdt          s                   like(elpfdt)
     d x1andt          s                   like(elandt)
     d xspfdt          s                   like(elpfdt)
     d xsfyr           s                   like(elfyr)
     d xsandt          s                   like(elandt)
     d xsdiv           s                   like(eldiv)
     d xxkey           s                   like(bskey)
     d xxss#           s                   like(eess#)

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------


     C/COPY SCOPYMT01C


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
     C                   parm                    @@qte
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     c     agtpl         plist
     c                   PARM                    ##CO#
     c                   PARM      elfnd         ##FND
     c                   PARM      elemp#        ##EMP#
     c                   PARM      eldiv         ##DIV
     c                   PARM      elfyr         ##FYR
     c     scagt#        PARM                    ##AGT#
     c                   PARM                    ##ANAM
     c     scprod        PARM                    ##PROD
     c                   PARM                    ##PNAM
      *


     *================================================================
     *  KLIST definitions
     *================================================================

     c     bsky          klist
     c                   kfld                    epco#
     c                   kfld                    xxkey

     c     eeky          klist
     c                   kfld                    epco#
     c                   kfld                    epfnd
     c                   kfld                    epemp#
     c                   kfld                    epdiv
     c                   kfld                    epfyr
     c                   kfld                    epclas
     c                   kfld                    epspec
     c                   kfld                    xxss#

     c     emky          klist
     c                   kfld                    epco#
     c                   kfld                    epfnd
     c                   kfld                    epemp#
     c                   kfld                    epdiv

     c     emky1         klist
     c                   kfld                    epco#
     c                   kfld                    epfnd
     c                   kfld                    epemp#

     C*** BEG   CHG ***  02/17/10  ************************************
     C     fnky          KLIST
     C                   KFLD                    epco#
     C                   KFLD                    epfnd
     C*** END   CHG ***  02/17/10  ************************************

      *  Employer Policy Year

     c     elky          klist
     c                   kfld                    epco#
     c                   kfld                    epfnd
     c                   kfld                    epemp#
     c                   kfld                    epdiv
     c                   kfld                    epfyr

     c     elky1         klist
     c                   kfld                    epco#
     c                   kfld                    epfnd
     c                   kfld                    epemp#
     c                   kfld                    xsdiv
     c                   kfld                    xsfyr

     c     zexky         klist
     c                   kfld                    epco#
     c                   kfld                    epfnd
     c                   kfld                    epemp#
     c                   kfld                    epdiv

      *  Payroll Description Extension

     c     pxky          klist
     c                   kfld                    epco#
     c                   kfld                    epclas
     c                   kfld                    epext
     c                   kfld                    xxst
     C*** BEGIN ADD ***  06/01/06  ************************************
     C                   KFLD                    EPEFDT

     c     pxky1         klist
     c                   kfld                    epco#
     c                   kfld                    epclas
     c                   kfld                    epext
     c                   kfld                    xxst
     C*** END ADD *****  06/01/06  ************************************

      *  Rating State

     c     rsky          klist
     c                   kfld                    elco#
     c                   kfld                    elrtcd

      *  Payroll Class

     c     prky1         klist
     c                   kfld                    epco#
     c                   kfld                    elrtcd
     c                   kfld                    epclas

     c     prky2         klist
     c                   kfld                    epco#
     c                   kfld                    elrtcd
     c                   kfld                    epclas
     c                   kfld                    xxstdt




     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr


     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr


     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr

      *  SS# must be entered (Add mode only)

      *****  Begin Add  ***  06/09/20  **********************
      /Free
       SCSS# = UnMaskData('SSN':'EESS#':SCSS#);
      /End-Free
      *****  End Add  ***  06/09/20  **********************
     c  n90              if        (@@mode = 'A')
      *****  Begin Chg  ***  06/09/20  **********************
     c*****                        and (scss# = 0)
     c                             and (scss# = *blanks)
      *****  End Chg  ***  06/09/20  **********************
     c                   eval      *in71 = *on
     c                   eval      *in90 = *on
     c                   endif

      *  SS# must be unique (Add mode only), for co#/fnd/emp#/div/clas/spec

     c  n90              if        (@@mode = 'A')
     c                   movel     scss#         xxss#
     c     eeky          chain     pee01

     c                   if        %found
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   endif
      *
     c                   endif

      *  Last Name must be entered

     c  n90              if        (sclsn = *blanks)
     c                   eval      *in72 = *on
     c                   eval      *in90 = *on
     c                   endif

      *  First Name must be entered

     c  n90              if        (scfsn = *blanks)
     c                   eval      *in73 = *on
     c                   eval      *in90 = *on
     c                   endif

      *  Reporting Start Date must be entered and valid

     c  n90              if        (*in90 = *off)

     c                   eval      srdti = scsdt
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scsdt

     c                   if        srerr = *on
     c                   eval      *in74 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif

      *  If entered, Reporting End Date must be valid.

     c  n90              if        (scedt <> 0)

     c                   eval      srdti = scedt
     c                   call      'SRCVTDAT'    datchk
     c                   movel     srdto         scedt

     c                   if        srerr = *on
     c                   eval      *in75 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif

      *  If entered, Reporting End Date must be greater than or equal
      *  Reporting Start Date

     c  n90              if        (scsdt <> 0)
     c                             and (scedt <> 0)

     c                   eval      srdti = scsdt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxsdt             7 0

     c                   eval      srdti = scedt
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxedt             7 0

     c                   if        (xxedt < xxsdt)
     c                   eval      *in76 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif

     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

      *****  Begin Chg  ***  06/09/20  **********************
     c*****              movel     eess#         scss#
      /Free
       SCSS# = MaskData('SSN':'EESS#':EESS#);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     c                   eval      sclsn  = eelsn
     c                   eval      scfsn  = eefsn
     c                   eval      scmid  = eemid
     c                   eval      sctitl = eetitl
     c                   eval      scposn = eeposn
     c                   eval      sceapr = eeeapr
     c                   eval      scsapr = eesapr

     c                   z-add     eesdt         srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scsdt

     c                   z-add     eeedt         srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scedt

     c                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     c                   eval      eeco#  = epco#
     c                   eval      eefnd  = epfnd
     c                   eval      eeemp# = epemp#
     c                   eval      eediv  = epdiv
     c                   eval      eefyr  = epfyr
     c                   eval      eeclas = epclas
     c                   eval      eespec = epspec
     c                   eval      eeext  = epext

      *****  Begin Chg  ***  06/09/20  **********************
     c*****              movel     scss#         eess#
      /Free
       EESS# = UnMaskData('SSN':'EESS#':SCSS#);
      /End-Free
      *****  End   Chg  ***  06/09/20  **********************
     c                   eval      eelsn  = sclsn
     c                   eval      eefsn  = scfsn
     c                   eval      eemid  = scmid
     c                   eval      eetitl = sctitl
     c                   eval      eeposn = scposn
     c                   eval      eeeapr = sceapr
     c                   eval      eesapr = scsapr

     c                   z-add     scsdt         srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         eesdt

     c                   z-add     scedt         srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         eeedt

     C                   endsr


     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr


     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr


     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr
     c                   eval      *inlr = *on
     C                   endsr


     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr

     c                   select

     c                   when      @@mode = 'A'
     c                   eval      *in40 = *on

     c                   when      @@mode = 'D'
     c                   eval      *in09 = *on

     c                   endsl

      *  Load Screen Headings

     c     @@RRN2        chain     pep01                              60
     c     emky          chain     pem01                              60
     C*** BEGIN CHG ***  02/17/10  ************************************
     c***  elky          chain     pel01                              60
     c     emky          chain     pel01                              60
     C*** END CHG *****  02/17/10  ************************************
     c                   eval      scclas = epclas
     c                   eval      scspec = epspec
     c                   eval      scext  = epext
     c                   eval      scfyr  = epfyr
     c                   eval      scfnd  = emfnd
     C*** BEGIN CHG ***  02/17/10  ************************************
     c****               eval      scname = ##fnam
     c     fnky          chain     pfn01                              60
     c                   eval      scname = fnname
     C*** END CHG *****  02/17/10  ************************************

     c                   if        elfaf = 'A'
     c                   eval      scpyst = 'Audited'
     c                   else
     c                   eval      scpyst = 'Estimated'
     c                   endif
      *
      *  Concat Employer/Division/Name
      *
     c                   CALL      'WA005R'
     c                   parm      ememp#        ##emp#
     c                   parm      emdiv         ##div
     c                   parm      emname        ##name
     c                   parm                    ##edn            80
      *
     c                   eval      scedn = ##edn

      *  get Cancel date

     c                   eval      x = 5
      *
     c                   dow       (ef(x) = 0) and (x > 1)
     c                   eval      x = x - 1
     c                   enddo
      *
     c                   eval      *in41 = *off
      *
     c                   if        (cx(x) <> 0)
     c                   eval      srdti = cx(x)
     c                   CALL      'SRCVTDAT'    DTMDYY
     c                   movel     srdto         sccxdt
     c                   eval      *in41 = *on
     c                   endif

     c                   time                    DSDTTM
     c                   z-add     dsdate        srdti
     c                   CALL      'SRCVTDAT'    dtcymd
     c                   movel     srdto         sysdt

      *  Get Agent/Producer Information

     c                   call      'WA002R'      AGTPL
     c                   eval      scanam = ##anam
     c                   eval      scpnam = ##pnam

      *  Get Employer Cross Reference info for header

     c                   clear                   pex02
     c     zexky         chain     pex02                              63
     c                   eval      scofnd = exofnd
     c                   eval      scoemp = exoemp
     c                   eval      scodiv = exodiv

      *  get payroll description

     c                   exsr      get_pay_desc

      *  Only display "Self Reported Payroll", if S/R policy.

     c                   movel(p)  elbscd        xxkey
     c     bsky          chain     pbs01

     c                   if        %found
     c                             and bssefl = 'S'
     c                   eval      *in42 = *on
     c                   else
     c                   eval      *in42 = *off
     c                   endif

     C                   endsr


      *================================================================
      * GET_PAY_DESC - Get Payroll Class Description
      *================================================================
     c     get_pay_desc  begsr

      *  This code reflects logic in W0060R02                               li-

      *  Get state reporting division's eff date or anniversary date, if appli-
      *  cable

     c                   eval      x1div  = eldiv
     C                   eval      x1pfdt = elpfdt
     C     rsky          chain     prs01

     c                   if        %found
     c                   eval      xxst = rsjrst

     c                   if        xxst = 'PA'
     c                   eval      xspfdt = elpfdt
     c                   endif

     c                   eval      x1andt = elandt
     c                   eval      xsfyr  = elfyr
     C     emky1         setll     pem01
     C     emky1         reade     pem01

     C                   dow       not %eof

     c                   if        (emstrp = 'Y')
     c                             or (xxst = 'PA')

     c                   if        (xxst = 'PA')
     c                   eval      xsdiv = emdiv
     c     elky1         chain     pel01

     c                   if        %found
     c     rsky          chain     prs01

     c                   if        %found
     c                             and (rsjrst = 'PA')
     c                             and (elpfdt < xspfdt)
     c                   eval      xspfdt = elpfdt
     c                   endif

     c                   endif

     c                   else

     c                   eval      xsdiv = emdiv
     c     elky1         chain     pel01
     c                   eval      xspfdt = elpfdt
     c                   eval      xsandt = elandt
     c                   leave
     c                   endif

     c                   endif

     c     emky1         reade     pem01
     c                   enddo

     c                   if        (x1div <> xsdiv)
     c                             and (x1andt = 0)

     c                   if        (xsandt <> 0)
     c                   eval      xsandt = xsandt + 10000

     c                   if        (xsandt <= x1pfdt)
     c                   eval      xxstdt = xsandt

     c                   else

     c                   eval      xxstdt = xsandt - 10000
     c                   endif

     c                   endif

     c                   endif

     c                   if        (xxstdt = 0)
     c                             and (xspfdt <= x1pfdt)
     c                   eval      xxstdt = xspfdt
     c                   endif

     c                   if        (x1andt <> 0)
     c                             and (x1andt <= xxstdt)
     c                   eval      xxstdt = x1andt
     c                   endif

     c     elky          chain     pel01                              60

      * get description

     c                   eval      scclds = *blanks

     c                   if        scext <> *blanks
     c                   eval      xxst = *blanks
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*****pxky*****     chain     ppx01
     c**************     if        %found
     C     PXKY          setgt     PPX01
     C     PXKY1         readpe    PPX01                                  60

     c                   if        *in60 = *off
     C*** END CHG *****  06/01/06  ************************************
     c                   eval      scclds = pxdesc

     c                   else

     c     rsky          chain     prs01

     c                   if        %found
     c                   eval      xxst = rsjrst
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*****pxky*****     chain     ppx01
     c**************     if        %found
     C     PXKY          setgt     PPX01
     C     PXKY1         readpe    PPX01                                  60

     c                   if        *in60 = *off
     C*** END CHG *****  06/01/06  ************************************
     c                   eval      scclds = pxdesc
     c                   endif

     c                   endif

     c                   endif

     c                   else

     c     prky2         SETGT     PPR01
     c     prky1         readpe    PPR01

     c                   if        not %eof
     c                   eval      scclds = prdesc
     c                   endif

     c                   endif

     c                   endif

     c                   endsr




     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr


     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   eval      eeuser = q1user
     C                   eval      eepgm = q1pgm
     C                   time                    dsdttm
     C                   eval      eetime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         eedt
     C                   update    dbrcdf
E01  C                   endif
     C                   endsr


     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      eeuser = q1user
     C                   eval      eeiusr = q1user
     C                   eval      eepgm  = q1pgm
     C                   eval      eeipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      eetime = dstime
     C                   eval      eeitm  = dstime
     C                   eval      srdti  = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         eedt
     C                   movel(p)  srdto         eeidt
     C                   write     dbrcdf
     C                   feod      wmeep
     C                   endsr


