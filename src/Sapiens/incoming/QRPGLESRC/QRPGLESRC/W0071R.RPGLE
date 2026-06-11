     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
    h*
      /title Work With CiCenter Internet Claims
      *****************************************************************
     h* PROGRAM DESCRIPTION:
     h*
     h*   Read and display CiCenter Internet Claims.
     h*
     h* Created 04/03/03  C:136  Project WC1084
      *****************************************************************
      * Revisions:
      *
      *  07/20/22  C:333 Project: WC2824
      *                  FROI Intake CS Standards
      *                  - bypassed old FROI processing
      *                    calling : WTCLMFROI (New Program)
      *
      *  02/19/21  C:318 Support: 94631
      *                  Mask SSN
      *
      * 03/25/2013 c:62 Support CI114 81922
      *                 Get the current date instead of job date.
      *
      *****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     fw0071f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     f                                     sfile(scr01s:sfrrn1)
     f                                     sfile(scr02s:sfrrn2)
     f                                     infds(fids01)
    f*
    f*  Physical file definition (for refreshing subfile record)
     fwhfrp     if   e             disk
     f                                     rename(pfr01:pfrcdf)
    f*
    f*  Primary logical file definition (for displaying items in view 1)
     fwhfrl     if   e           k disk
     f                                     infds(dbfds)
     f                                     rename(pfr01:dbrcdf)
    f*
    f*  Second logical file definition (for displaying items in view 2)
     fwhfrl1    if   e           k disk
     f                                     infds(dbfds2)
     f                                     rename(pfr01:dbrcdf1)
    f*  Claims file
     fwmcmlb    if   e           k disk

    f*  Employer file
     fwmeml     if   e           k disk

    f*  Employer detail file
     fwdell2    if   e           k disk
    d*
     d hd              s             78    dim(2) ctdata perrcd(1)
     d of              s              1    dim(14) ctdata perrcd(1)
     d ofd             s             35    dim(14) alt(of)
    d*================================================================
    d*  /COPY required constants
    d*================================================================
     d optnm           c                   'CiCenter Internet Claims'
     d slmsgi          c                   'WC39998'
     d slmsgf          c                   'WMESGG'
     d acdpgm          c                   'W0071R02'
     d comapi          c                   'WA001R'
     d uspapi          c                   'W0006R'
     d infomx          c                   1
     d viewmx          c                   2
     d sf1pag          c                   10
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d ##co#           s              3  0
     d ##conm          s             40
     d ##fnam          s             40
     d ##fnd           s              3  0
     d ##kc            s              1
     d ##kl            s              1
     d ##mode          s              1
     d ##cont          s              1
     d ##rrn           s              9  0
     d ##rrn1          s              9  0
     d ##rrn2          s              9
     d @@call          s              1
      //*** Beg ADD *** 07/20/22 **********************************
     d ##call          s              1
      //*** End ADD *** 07/20/22 **********************************
     d kkco            s                   like(frco)
     d kkrcvd          s                   like(frrcvd)
     d kklsn           s                   like(frlsn)
     d kkfsn           s                   like(frfsn)
     d kkss            s                   like(frss)
     d kkacdt          s                   like(fracdt)
     d kkfnd           s                   like(cmfnd)
     d kkemp           s                   like(cmemp#)
     d kkdiv           s                   like(cmdiv)
     d kkefdt          s                   like(cmacdt)
     d svss            s                   like(frss)
     d svemp           s                   like(cmemp#)
     d svacdt          s                   like(fracdt)
     d x1acdt          s                   like(cmacdt)
     d xxss#           s             11
     d mdyACDT         s               d   datfmt(*mdy)
     d xxacdt          s              8
     d mdyRCVD         s               d   datfmt(*mdy)
     d xxrcvd          s              8
     d isoDate         s               d   datfmt(*iso)
     d mdyDate         s               d   datfmt(*mdy)
     d isoCharDt       s              8
     d Up              c                   'ABCDEFGHIJKLMNOPQRS-
     d                                     TUVWXYZ'
     d Lo              c                   'abcdefghijklmnopqrs-
     d                                     tuvwxyz'
     d u1pdsc          s                   like(FRLSN)
     d u2pdsc          s                   like(FRLSN)
     d strlen          s              3  0
     d start           s              3  0
     d patlen          s              3  0
     d trans           s              1
     d trim            s              1
     d wild            s              1
     d scnres          s              3  0
     d x               s              1  0
     d y               s              1  0
     d acc_date        s               d   datfmt(*usa)
     d sys_date        s               d   datfmt(*usa)
    d*---------------------------------------------------------------------
    d* Data Structres
    d*---------------------------------------------------------------------
     dview1            ds
     d v1rcvd                  1      8
     d v1name                 11     35
     d v1ss#                  38     48
    d v1acdt                 54     61
     d v1stat                 66     74
    
     dview2            ds
    d v2name                  1     25
    d v2rcvd                 28     35
    d v2ss#                  38     48
    d v2acdt                 54     61
    d v2stat                 66     74
    
     dss#              ds
     d ss#1                    1      3
     d ss#2                    4      5
     d ss#3                    6      9
    
     d                 ds                  inz
     d  ed                     1     35  0
     d                                     dim(5) ascend
     d  emefd1                 1      7  0
     d  emefd2                 8     14  0
     d  emefd3                15     21  0
     d  emefd4                22     28  0
     d  emefd5                29     35  0

     d                 ds                  inz
     d  cd                     1     35  0
     d                                     dim(5) ascend
     d  emcad1                 1      7  0
     d  emcad2                 8     14  0
     d  emcad3                15     21  0
     d  emcad4                22     28  0
     d  emcad5                29     35  0

      //*** Beg ADD *** 07/20/22 **********************************
      // external program WTCLMFROI
     d WTCLMFROI       pr                  extpgm('WTCLMFROI')
     d                                1
      //*** End ADD *** 07/20/22 **********************************
     c/COPY SCOPYWW01C
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm                    @@call
    c*
    c*  Add/change/display program parameter list
    c*
     c     acdpl         plist
     c     *inkc         parm                    ##kc
     c     *inkl         parm                    ##kl
     c                   parm                    ##mode
     c                   parm      scdbrr        ##rrn
    c*
      *******Beg Chg********11/21/12**********************************
     c**** acdpl1        plist
     c**** *inkc         parm                    ##kc
     c**** *inkl         parm                    ##kl
     c****               parm      'A'           ##mode
     c****               parm                    ##rrn
     c****               parm      'A'           ##cont
     c****               parm      scdbrr        ##rrn1
     c     acdpl1        plist
     c                   parm                    ##rrn2
      *******End Chg********11/21/12**********************************
    c*
    c*  Company file/fund (group) retrieve program parameter list
    c*
     c     compl         plist
     c                   parm                    ##co#
     c                   parm                    ##fnd
     c                   parm                    ##conm
     c                   parm                    ##fnam
    c*================================================================
    c*  KLIST definitions
    c*================================================================
     c     key           klist
     c                   kfld                    kkco

     c     key1          klist
     c                   kfld                    kkco
     c                   kfld                    kkrcvd
     c                   kfld                    kklsn
     c                   kfld                    kkfsn
     c                   kfld                    kkss
     c                   kfld                    kkacdt

     c     key2          klist
     c                   kfld                    kkco
     c                   kfld                    kklsn
     c                   kfld                    kkfsn
     c                   kfld                    kkss
     c                   kfld                    kkacdt
     c                   kfld                    kkrcvd

     c     cmky          klist
     c                   kfld                    kkco
     c                   kfld                    kkss

     c     elky          klist
     c                   kfld                    kkco
     c                   kfld                    kkfnd
     c                   kfld                    kkemp
     c                   kfld                    kkdiv
     c                   kfld                    kkefdt

     c     emky          klist
     c                   kfld                    kkco
     c                   kfld                    kkfnd
     c                   kfld                    kkemp
     c                   kfld                    kkdiv
    c*================================================================
    c* addEdit- Edits invoked before calling W0070R01
    c*================================================================
     c     addEdit       begsr

     c     scdbrr        chain     pfrcdf
      *
      * Check for duplicate claims
      *
     c                   if        svss <> FRSS

     c                   eval      svss = FRSS
     c                   eval      kkco  = FRCO
     c                   move      FRSS          kkss
     c     cmky          chain     pcm01

     c                   if        %found
     c                   seton                                        72  90
     c                   endif

     c                   endif                                                      svss<>frss
      *
      * Validate employer number
      *
     c                   if        not *in90

     c                   eval      svemp = FREMP
     c                   eval      kkfnd = FRFND
     c                   eval      kkemp = FREMP
     c                   eval      kkdiv = FRDIV
     c     emky          chain     pem01

     c                   if        not %found
     c                   seton                                        73  90
     c                   endif

     c                   endif                                                  not *in90
      *
      * Validate social security number (must exist)
      *
     c                   if        FRSS  = *blanks or
     c                             FRSS  = *zeros
     c                   seton                                        74  90
     c                   endif
      *
      * Validate accident date (1) must exist (2) and be less or equal to system date
      *
     c  n90*iso0         test(de)                FRACDT
     c  n90              if        %error
     c                   seton                                        75  90
     c                   else
     c     *iso0         move      FRACDT        acc_date
     c*** Begin Chg *** 03/25/13 *******************************************
     c**** *usa          move      *date         sys_date
     c                   eval      sys_date=%Date()
     c*** End   Chg *** 03/25/13 *******************************************

     c                   if        acc_date > sys_date
     c                   seton                                        76  90
     c                   endif

     c                   endif
     *
     *  Check the employer effective and cancellation dates. Get the most current eff date.
     *
     c  n90              if        svacdt <> FRACDT
     c                   eval      svacdt = FRACDT

     c                   eval      y = 1
     c     *zero         lookup    ed(y)                                  60
     c                   if        *in60 = *off
     c                   eval      y = 5
     c                   else
     c                   eval      y = y - 1
     c                   endif
     *
     *  Compare the accident date to the effective date. If acc date is >= to eff date,
     *  get out of the loop.
     *
     c     *cymd         move      acc_date      x1acdt
     c                   dow       y <> 0
     c                   if        x1acdt >= ed(y)
     c                   leave
     c                   else
     c                   eval      y = y - 1
     c                   endif
     c                   enddo
     *
     *  The accident date can't be greater than the cancel date,  unless overriden by the user.
     *
     c                   if        y = *zero
     c                   eval      y = 1
     c                   seton                                        77  90
     c                   else
     *
     c                   if        cd(y) <= x1acdt
     c                             and cd(y) > *zero
     c                   seton                                        77  90
     c                   endif

     c                   endif                                                  y = *zero
     *
     * Verify policy year coverage (WDELP).
     *
     c                   eval      kkefdt = x1acdt
     c     elky          setgt     pel01
     c     emky          readpe    pel01                                  99
     *
     *  If accident date is not within the policy eff period, dsp window.
     *
     c  n90              if        elpfdt > x1acdt
     c                             or elptdt <= x1acdt
     c                             or elpfdt = *zero
     c                   seton                                        78  90
     c                   endif
    
     c                   endif                                                  not *in90
    
     c                   endsr
    c*================================================================
    c* CHKSSM - Set indicator 11 for subset message
    c*================================================================
     c     chkssm        begsr

     c                   if        sslsn  <> *blank or ssstat <> *blank or
     c                             ssacdt <> *zero  or ssrcvd <> *zero  or
     c                             ssemp  <> *zero  or ssdiv  <> *zero
     c                   seton                                        11
     c                   endif

     c                   endsr
    c*================================================================
    c* ERRCK1 - Additional errors on subfile selection
    c*================================================================
     c     errck1        begsr

     c                   if           scsel = '4'
     c                             or scsel = '6'

     c                   if        scstat <> 'Pending'
     c                   seton                                        71  90
     c                   endif

     c                   endif

     c                   if           scsel = '6'
     c                   exsr      addEdit
     c                   endif

     c                   endsr
    c*================================================================
    c* ERRCK2 - Error checking for the subset screen
    c*================================================================
     c     errck2        begsr
     c                   endsr
    c*================================================================
    c* INZSST - Initialize the subset fields
    c*================================================================
     c     inzsst        begsr
     c                   eval      ssstat = 'P'
     c                   endsr
    c*================================================================
    c* KEYINZ - Initialize key values for program start
    c*================================================================
     c     keyinz        begsr

     c                   eval      kkco  = ##co#
     c                   eval      kklsn = *loval
     c                   eval      kkfsn = *loval
     c                   eval      kkss  = *loval
     c                   eval      kkrcvd = *loval
     c                   eval      kkacdt = *loval

     c                   endsr
    c*================================================================
    c* KEYSET - Set key values when doing "position to"
    c*================================================================
     c     keyset        begsr

     c                   if        *in21 = *on

     c                   eval      kklsn = *blanks
     c     *mdy0         test(de)                scpst1
     c                   if        not %error
     c     *mdy0         move      scpst1        mdyRCVD
     c     *iso0         move      mdyRCVD       kkrcvd
     c                   endif

     c                   endif

     c                   if        *in22 = *on
     c                   eval      kklsn = scpst2
     c                   eval      kkrcvd = *loval
     c                   endif

     c                   endsr
    c*================================================================
    c* KEYSFL - Set key values from subfile records for paging
    c*================================================================
     c     keysfl        begsr

     c                   eval      kkco  = ##co#
     c                   move      scrcvd        kkrcvd
     c                   eval      kklsn = sclsn
     c                   eval      kkfsn = scfsn
     c                   eval      kkss  = scss#
     c                   move      scacdt        kkacdt

     c                   endsr
    c*================================================================
    c* MVDBSC - Move data base fields to screen fields
    c*================================================================
     c     mvdbsc        begsr

     c                   eval      scrcvd = FRRCVD
     c                   eval      scacdt = FRACDT
     c                   eval      scname = %trimr(FRLSN) + ', ' +
     c                                      %trimr(FRFSN)
     c                   eval      sclsn = FRLSN
     c                   eval      scfsn = FRFSN
     c                   eval      scss# = FRSS
     c                   eval      ss#   = scss#
     C***  Begin Add  ***  02/19/21  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'ss#'         qqname           25
     C     ss#           parm      ss#           qqval           100
     C***  End Add  ***  02/19/21  *******************************
     c                   eval      xxss# = ss#1 + '-' + ss#2 + '-'  + ss#3

     c     *iso0         test(de)                FRACDT
     c                   if        not %error
     c     *iso0         move      FRACDT        mdyACDT
     c                   move      mdyACDT       xxacdt
     c                   else
     c                   clear                   xxacdt
     c                   endif

     c     *iso0         test(de)                FRRCVD
     c                   if        not %error
     c     *iso0         move      FRRCVD        mdyRCVD
     c                   move      mdyRCVD       xxrcvd
     c                   else
     c                   clear                   xxrcvd
     c                   endif

     c                   select
     c                   when      FRSTAT = 'P'
     c                   eval      scstat = 'Pending'
     c                   when      FRSTAT = 'T'
     c                   eval      scstat = 'Processed'
     c                   when      FRSTAT = 'D'
     c                   eval      scstat = 'Deleted'
     c                   endsl

     c                   if        *in21 = *on
     c                   eval      v1rcvd = xxrcvd
     c                   eval      v1name = scname
     c                   eval      v1ss#  = xxss#
     c                   eval      v1acdt = xxacdt
     c                   eval      v1stat = scstat
     c                   eval      scdata = view1
     c                   endif

     c                   if        *in22 = *on
     c                   eval      v2name = scname
     c                   eval      v2rcvd = xxrcvd
     c                   eval      v2ss#  = xxss#
     c                   eval      v2acdt = xxacdt
     c                   eval      v2stat = scstat
     c                   eval      scdata = view2
     c                   endif

     c                   endsr
    c*================================================================
    c* OPT6 - Add Claim Function
    c*================================================================
     c     opt6          begsr

      *******Beg Chg********11/21/12**********************************
     c**********         call      'W0070R01'    acdpl1
     c                   eval      ##rrn2 = %editc(scdbrr:'X')
     c                   call      'WTCLMADD'    acdpl1
      *******End Chg********11/21/12**********************************

     c                   endsr
    c*================================================================
    c* OPT11 - Quick Edit Function
    c*================================================================
     c     opt11         begsr

     c                   call      'W0071R11'    acdpl

     c                   endsr
    c*================================================================
    c*================================================================
    c* OTHFKS - Process other function keys
    c*================================================================
     c     othfks        begsr

     c     *inkm         caseq     *on           kmdo                     90
     c                   endcs

     c                   endsr
    c*================================================================
    c* OTHOPT - Process other options
    c*================================================================
     c     othopt        begsr

     c     scsel         caseq     '6'           opt6                     90
     c     scsel         caseq     '11'          opt11                    90
     c                   endcs

     c                   endsr
    c*================================================================
    c* PGMEND - End of program processing
    c*================================================================
     c     pgmend        begsr
     c                   seton                                        lr
     c                   endsr
    c*================================================================
    c* PGMSTR - Start of program processing
    c*================================================================
     c     pgmstr        begsr
      //*** Beg ADD *** 07/20/22 **********************************
      /free
       ##call = @@call;
       WTCLMFROI(##call);

       *inlr = *on;
      /End-free
      //*** End ADD *** 07/20/22 **********************************
     c                   endsr
    c*================================================================
    c* READ   - Read next record from file
    c*================================================================
     c     read          begsr

     c                   if        *in21 = *on
     c                   read      dbrcdf                                 95
     c                   endif

     c                   if        *in22 = *on
     c     key           reade     dbrcdf1                                95
     c                   endif

     c                   endsr
    c*================================================================
    c* READP  - Read previous record from file
    c*================================================================
     c     readp         begsr

     c                   if        *in21 = *on
     c     key           readpe    dbrcdf                                 95
     c                   endif

     c                   if        *in22 = *on
     c     key           readpe    dbrcdf1                                95
     c                   endif

     c                   endsr
    c*================================================================
    c* SETGT  - Set greater than on file
    c*================================================================
     c     setgt         begsr

     c                   if        *in21 = *on
     c     key1          setgt     dbrcdf                             96
     c                   endif

     c                   if        *in22 = *on
     c     key2          setgt     dbrcdf1                            96
     c                   endif

     c                   endsr
    c*================================================================
    c* SETLL  - Set lower limits on file
    c*================================================================
     c     setll         begsr

     c                   if        *in21 = *on
     c     key1          setll     dbrcdf                             96
     c                   endif

     c                   if        *in22 = *on
     c     key2          setll     dbrcdf1                            96
     c                   endif

     c                   endsr
    c*================================================================
    c* SF1UPD - Update subfile record when item is deleted
    c*================================================================
     c     sf1upd        begsr
     c                   endsr
    c*================================================================
    c* SLCTRD - Move fields for selected record to parameters
    c*================================================================
     c     slctrd        begsr
     c                   endsr
     c*================================================================
     c* STRING - Subroutine to search strings
     c*================================================================
     c     string        begsr

     c                   call      'QCLSCAN'
     c                   parm                    u1pdsc
     c                   parm      25            strlen
     c                   parm      1             start
     c                   parm                    u2pdsc
     c                   parm      25            patlen
     c                   parm      '0'           trans
     c                   parm      '1'           trim
     c                   parm      ' '           wild
     c                   parm                    scnres

     c                   if        scnres = 0
     c                   eval      meetss = *off
     c                   endif

     c                   endsr
    c*================================================================
    c* SUBSET - Determine if record should be put in subfile
    c*================================================================
     c     subset        begsr

     c                   if        ssstat <> *blank and
     c                             FRSTAT <> ssstat
     c                   eval      meetss = *off
     c                   endif

     c                   if        ssemp  <> *zero  and
     c                             FREMP  <> ssemp

     c                   if        ssdiv  <> FRDIV
     c                   eval      meetss = *off
     c                   endif

     c                   endif

     c                   if        sslsn <> *blanks
     c     Lo:Up         xlate(p)  FRLSN         u1pdsc
     c     Lo:Up         xlate(p)  sslsn         u2pdsc
     c                   exsr      string
     c                   endif

     c                   if        ssrcvd <> *zero
     c     *usa          test(de)                ssrcvd

     c                   if        not %error
     c     *usa          move      ssrcvd        isoDate
     c     *iso0         move      isoDate       isoCharDt

     c                   else
     c     *mdy          test(de)                ssrcvd

     c                   if        not %error
     c     *mdy          move      ssrcvd        mdyDate
     c     *iso0         move      mdyDate       isoCharDt
     c     *usa          move      mdyDate       ssrcvd
     c                   endif

     c                   endif

     c                   if        FRRCVD <> isoCharDt
     c                   move      *off          meetss
     c                   endif

     c                   endif

     c                   if        ssacdt <> *zero
     c     *usa          test(de)                ssacdt

     c                   if        not %error
     c     *usa          move      ssacdt        isoDate
     c     *iso0         move      isoDate       isoCharDt

     c                   else
     c     *mdy          test(de)                ssacdt

     c                   if        not %error
     c     *mdy          move      ssacdt        mdyDate
     c     *iso0         move      mdyDate       isoCharDt
     c     *usa          move      mdyDate       ssacdt
     c                   endif

     c                   endif

     c                   if        FRACDT <> isoCharDt
     c                   move      *off          meetss
     c                   endif

     c                   endif

     c                   endsr
** Column headings
Opt    Recvd   Claimant name              Social Sec#   Dt of Injury  Stat
Opt  Claimant name                Recvd   Social Sec#   Dt of Injury  Stat
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1066=Add claim              YNN
11111=Quick edit            YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
211F11=Change view          YYY
206F6=Add                   NNN
212F12=Cancel               NNY
213F13=Chg User Parm        YNN
217F17=Subset               YYY
3
