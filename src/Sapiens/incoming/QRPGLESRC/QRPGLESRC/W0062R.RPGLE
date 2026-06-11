     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')

      /title W0062R - Work with Employees
      *****************************************************************
      *
      * CALLED FROM: W0060R - Work with Employer Payroll
      * CALLED TO:   W0060R02 - Employee Information maintenance.
      *
      * created 04/12/05  C:61  Project wc1283 135WBW
      *****************************************************************
      * Revised:
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 02/11/10 C:237 SUPP 71497                                     *
     H*                FIX THE GROUP# DISPLAY FOR WORK WITH EMPLOYEES *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
      *
      *
      *****************************************************************

      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0062f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
      *
      *  Physical file definition (for refreshing subfile record)
     Fwmeep     if   e             disk
     F                                     rename(pee01:pfrcdf)
      *
      *  Primary logical file definition (for displaying items)
     Fwmeel1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pee01:dbrcdf)
     Fwmeel2    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pee01:dbrcdf2)
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
     C*** BEG   CHG ***  02/11/10  ************************************
     FWMFNL     IF   E           K DISK
     C*** END   CHG ***  02/11/10  ************************************
    
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(11) ctdata perrcd(1)
     D ofd             s             35    dim(11) alt(of)

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

     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

    
      *================================================================
      *  /COPY required constants
      *================================================================
     D optnm           c                   'Employees'
     D slmsgi          c                   'WC39998'
     D slmsgf          c                   'WMESGG'
     D acdpgm          c                   'W0062R02'
     D comapi          c                   'WA001R'
     D uspapi          c                   'W0006R'
     D infomx          c                   1
     D viewmx          c                   2
     D sf1pag          c                   8
      *---------------------------------------------------------------------
      * Stand Alone Fields - TOP
      *---------------------------------------------------------------------
     D ##conm          s             40
     D ##fnam          s             40
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D kkco#           s                   like(eeco#)
     D kkfnd           s                   like(eefnd)
     D kkemp#          s                   like(eeemp#)
     D kkdiv           s                   like(eediv)
     D kkfyr           s                   like(eefyr)
     D kkclas          s                   like(eeclas)
     D kkspec          s                   like(eespec)
     D kkss#           s                   like(eess#)

     d ##co#           s                   like(elco#)
     d ##fnd           s                   like(elfnd)
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
     d x1sdt           s              8
     d x1edt           s              8
     d xxname          s             25
     d kklsn           s                   like(eelsn)
     d kkfsn           s                   like(eefsn)
     d ##rrn2          s                   like(@@rrn2)
     d ##qte           s                   like(@@qte)


      *---------------------------------------------------------------------
      * Stand Alone Fields - BOTTOM
      *---------------------------------------------------------------------

    
     c/cOPY SCOPYWW01C
    

      *================================================================
      *  PLIST definitions
      *================================================================
     c     *entry        plist
     c                   parm      *INKC         @@KC              1
     c                   parm      *INKL         @@KL              1
     c                   parm                    @@call
     c                   parm                    @@RRN             9 0
      ******                                           WMEEP
     c                   parm                    @@RRN2            9 0
      ******                                           WMEPP
     c                   parm                    @@QTE             1
      *
      *
      *
      *  Add/change/display program parameter list
      *
     c     acdpl         plist
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##mode
      *                                    wdeep
     c                   parm      scdbrr        ##rrn
      *                                    wmepp
     c                   parm      @@rrn2        ##rrn2
     c                   parm      @@qte         ##qte
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam

     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr

     c     dtmdy         plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      '*MDY'        srtfmt
     c                   parm                    srdto
     c                   parm                    srerr

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


      *================================================================
      *  KLIST definitions
      *================================================================

     c     key           klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
     c                   kfld                    kkclas
     c                   kfld                    kkspec

     c     key1          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
     c                   kfld                    kkclas
     c                   kfld                    kkspec
     c                   kfld                    kkss#

     c     key2          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
     c                   kfld                    kkclas
     c                   kfld                    kkspec
     c                   kfld                    kklsn
     c                   kfld                    kkfsn
     c                   kfld                    kkss#

     c     emky          klist
     c                   kfld                    epco#
     c                   kfld                    epfnd
     c                   kfld                    epemp#
     c                   kfld                    epdiv

     c     emky1         klist
     c                   kfld                    epco#
     c                   kfld                    epfnd
     c                   kfld                    epemp#
     C*** BEG   CHG ***  02/11/10  ************************************
     C     fnky          KLIST
     C                   KFLD                    epco#
     C                   KFLD                    epfnd
     C*** END   CHG ***  02/11/10  ************************************

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


      *================================================================
      * cHKSSM - Set indicator 11 for subset message
      *================================================================
     c     chkssm        begsr
     c                   endsr


      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     c     errck1        begsr
     c                   endsr


      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     c     errck2        begsr
     c                   endsr


      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     c     inzsst        begsr
     c                   endsr


      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     c     keyinz        begsr
     c                   eval      kkss# = *loval
     c                   eval      kklsn = *loval
     c                   eval      kkfsn = *loval
     c                   endsr


      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     c     keyset        begsr

     c                   move      scpst1        kkss#
     c                   movel(p)  scpst2        kklsn
     c                   movel(p)  scpst2a       kkfsn

     c                   endsr


      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     c     keysfl        begsr
     c                   eval      kkss# = scss#
     c                   eval      kklsn = sclsn
     c                   eval      kkfsn = scfsn
     c                   endsr


      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     c     mvdbsc        begsr

     c                   eval      scss# = eess#
     c                   eval      sclsn = eelsn
     c                   eval      scfsn = eefsn

     c                   eval      ss#   = eess#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'ss#'         qqname           25
     C     ss#           parm      ss#           qqval           100
     C***  End Add  ***  06/09/20  *******************************

     c                   eval      xxname = %trim(eelsn) + ', ' +
     c                                      %trim(eefsn) + ' ' + eemid

     c                   eval      srdti = eesdt
     c                   call      'SRCVTDAT'    dtmdy
     c                   movel     srdto         xxsdt             6 0

     c                   if        srerr = *off
     c                   eval      x1sdt = %editw(xxsdt:'  /  /  ')
     c                   else
     c                   eval      x1sdt = *blanks
     c                   endif

     c                   eval      srdti = eeedt
     c                   call      'SRCVTDAT'    dtmdy
     c                   movel     srdto         xxedt             6 0

     c                   if        srerr = *off
     c                   eval      x1edt = %editw(xxedt:'  /  /  ')
     c                   else
     c                   eval      x1edt = *blanks
     c                   endif

     c                   select

     c                   when      (*in21 = *on)
     c                   eval      scdata = ss#1 + '-' + ss#2 + '-'  + ss#3
     c                             + ' ' + xxname
     c                             + ' ' + %editc(eeeapr:'2')
     c                             + ' ' + x1sdt
     c                             + '  ' + x1edt


     c                   when      (*in22 = *on)
     c                   eval      scdata = xxname
     c                             + ss#1 + '-' + ss#2 + '-'  + ss#3
     c                             + ' ' + %editc(eeeapr:'2')
     c                             + ' ' + x1sdt
     c                             + '  ' + x1edt


     c                   endsl

     c                   endsr


      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     c     othfks        begsr
     c                   endsr


      *================================================================
      * OTHOPT - Process other options
      *================================================================
     c     othopt        begsr
     c                   endsr


      *================================================================
      * PGMEND - End of program processing
      *================================================================
     c     pgmend        begsr
     c                   eval      *inlr = *on
     c                   endsr


      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     c     pgmstr        begsr

      *  Load Screen Headings

     c     @@RRN2        chain     pep01                              60
     c                   eval      kkco#  = epco#
     c                   eval      kkfnd  = epfnd
     c                   eval      kkemp# = epemp#
     c                   eval      kkdiv  = epdiv
     c                   eval      kkfyr  = epfyr
     c                   eval      kkclas = epclas
     c                   eval      kkspec = epspec
     c     emky          chain     pem01                              60
     C*** BEGIN CHG ***  02/11/10  ************************************
     c**   elky          chain     pel01                              60
     c     emky          chain     pel01                              60
     C*** END CHG *****  02/11/10  ************************************
     c                   eval      scclas = epclas
     c                   eval      scspec = epspec
     c                   eval      scext  = epext
     c                   eval      scfyr  = epfyr
     c                   eval      scfnd  = emfnd
     C*** BEGIN CHG ***  02/11/10  ************************************
     c***                eval      scname = ##fnam
     c     fnky          chain     pfn01                              60
     c                   eval      scname = fnname
     C*** END CHG *****  02/11/10  ************************************

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

     c                   endsr


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


      *================================================================
      * READ   - Read next record from file
      *================================================================
     c     read          begsr

     c                   select

     c                   when      *in21 = *on
     c     key           reade     dbrcdf                                 95

     c                   when      *in22 = *on
     c     key           reade     dbrcdf2                                95

     c                   endsl

     c                   endsr


      *================================================================
      * READP  - Read previous record from file
      *================================================================
     c     readp         begsr

     c                   select

     c                   when      *in21 = *on
     c     key           readpe    dbrcdf                                 95

     c                   when      *in22 = *on
     c     key           readpe    dbrcdf2                                95

     c                   endsl

     c                   endsr


      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     c     setgt         begsr

     c                   select

     c                   when      *in21 = *on
     c     key1          setgt     dbrcdf                             96

     c                   when      *in22 = *on
     c     key2          setgt     dbrcdf2                            96

     c                   endsl

     c                   endsr


      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     c     setll         begsr

     c                   select

     c                   when      *in21 = *on
     c     key1          setll     dbrcdf                             96

     c                   when      *in22 = *on
     c     key2          setll     dbrcdf2                            96

     c                   endsl

     c                   endsr


      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     c     sf1upd        begsr
     c                   eval      scdata = '  *Deleted'
     c                   endsr


      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     c     slctrd        begsr
     c                   eval      @@rrn = scdbrr
     c                   endsr


      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     c     subset        begsr
     c                   endsr


** column headings
Opt SS#         Employee                      Payroll      Date      Date
Opt Employee Name            SS#              Payroll      Date      Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
