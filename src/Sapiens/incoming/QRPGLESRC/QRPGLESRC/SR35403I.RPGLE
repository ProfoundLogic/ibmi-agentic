      *===================================================================*
      *  (C) Copyright Progressive Data Solutions, Inc., 2001             *
      *===================================================================*
     H/TITLE SR35403I - ADDITIONAL ENTRY PROGRAM FOR WC350403
      *===================================================================*
      *  DESCRIPTION: Collects additional information for form WC350403 . *
      *                                                                   *
      *  CALLED FROM: SR35403                                             *
      *                                                                   *
      *  CALLS TO:    none                                                *
      *                                                                   *
      *===================================================================*
      *  REVSIONS:                                                        *
      *                                                                   *
      *                                                                   *
      *===================================================================*
     H DFTACTGRP(*NO)
     FSR35403f  cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      ** Additional Information File
     FWXFMP1    uf a e           k DISK
      ** Group Master File
     FWMFNL     IF   E           K DISK
      ** Employer Master File
     FWMEML     IF   E           K DISK
     D/COPY SCOPYPR01D
     D optnm           c                   const('Additional information for WC+
     D                                     350403')
     D comapi          c                   const('WA001R')
      *
     D @@call          s              1
     D @@prds          s             40
     D @@co#           s              3
     D @@emp#          s              9
     D @@div           s              5
     D @@fnd           s              3
     D @@fyr           s              3
      *
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
      *
     D xxfrm1          s             15
     D xxcall          s                   like(@@call)
     D xxForm          s             10    inz('WC350403B ')
     D xxco#           s                   like(emco#)
     D xxfnd           s                   like(emfnd)
     D xxemp#          s                   like(ememp#)
     D xxdiv           s                   like(emdiv)
     D xxfyr           s              3  0
     D xxedn           s             80
     C/COPY SCOPYDTPL
      ** Entry parameters
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr
      ** Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      ** Employer Master file
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
      ** Additional Information file
     C     fmky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfrm1
      ** Group Master file
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C
      /COPY SCOPYPR01C

      *====================================================================
      * DSCAPI - Call API for getting descriptions (use 91 on error)
      *====================================================================
     C     DSCAPI        begsr
     C                   call      comapi        compl                  91
     C                   endsr
      *====================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *====================================================================
     C     ERRCK1        begsr
     C                   exsr      othfks
     C                   endsr
      *====================================================================
      * KJDO   - Processing for F10
      *====================================================================
     C     KJDO          begsr
     C                   endsr
      *====================================================================
      * OTHFKS - Processing for other functions keys
      *====================================================================
     C     OTHFKS        begsr
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
     C                   endsr
      *====================================================================
      * PGMEND - End of program processing
      *====================================================================
     C     PGMEND        begsr
     C                   if        *inkc = *on OR
     C                             *inkl = *on
     C                   eval      @@call = 'F'
     C                   endif
     C                   endsr
      *====================================================================
      * PGMSTR - Used for any start of program processing (one-time)
      *====================================================================
     C     PGMSTR        begsr

     C                   if        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif

     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng

     C                   movel     ##data        scprds

     C                   if        q1prms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif

     C                   eval      xxfyr = 0
     C                   move      @@co#         xxco#
     C                   move      @@fnd         xxfnd
     C                   move      @@emp#        xxemp#
     C                   move      @@div         xxdiv
     C                   move      @@fyr         xxfyr

     C                   eval      scfnd = xxfnd
     C                   eval      scfyr = xxfyr
     C                   eval      xxfrm1 = xxform

     C     fnky          chain     pfn01                              60
     C                   eval      scfnnm = fnname

     C     emky          chain     pem01                              60

     C                   call      'WA005R'
     C                   parm                    ememp#
     C                   parm                    emdiv
     C                   parm                    emname
     C                   parm                    xxedn

     C                   eval      scedn = xxedn

     C                   endsr
      *====================================================================
      * PREDSP - Pre-processing before displaying (every time)
      *====================================================================
     C     PREDSP        begsr
      ** Get endorsement additional information
     C     fmky1         chain     pfm01                              60

     C                   if        *in60 = *off
      ** Move the custom fields to the screen
     C                   movel     f3c101        scst
     C                   eval      scpct0 = f3a521
     C                   eval      scpct1 = f3a522
     C                   eval      scpct2 = f3a523

     C                   else

     C                   eval      scst = *blanks
     C                   eval      scpct0 = *zeros
     C                   eval      scpct1 = *zeros
     C                   eval      scpct2 = *zeros

     C                   endif

     C                   endsr
      *====================================================================
      * PRSCTL - Process control after no errors found (Call Submitter)
      *====================================================================
     C     PRSCTL        begsr

     C     fmky1         chain     pfm01                              60

     C                   if        *in60 = *on
     C                   clear                   pfm01
     C                   endif
      ** Move the additional information fields to the file
     C                   eval      f3co# = xxco#
     C                   eval      f3fnd = xxfnd
     C                   eval      f3fyr = xxfyr
     C                   eval      f3emp# = xxemp#
     C                   eval      f3div = xxdiv
     C                   eval      f3form = xxform
     C                   movel     scst          f3c101
     C                   eval      f3a521 = scpct0
     C                   eval      f3a522 = scpct1
     C                   eval      f3a523 = scpct2

     C   60              write     pfm01
     C  N60              update    pfm01

     C                   endsr
