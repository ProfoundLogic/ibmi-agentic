      * (C) Copyright Progressive Data Solutions, Inc., 2004
      *
     H/TITLE SR04305I - ADDITIONAL ENTRY PROGRAM FOR WC040305
      *===================================================================*
      *  DESCRIPTION: Collects additional information for form WC040305 . *
      *                                                                   *
      *  CALLED FROM: SR04305                                             *
      *                                                                   *
      *  CALLS TO:    none                                                *
      *                                                                   *
      *  CREATED: 10/06/04      C:61     PROJECT: 132WAC                  *
      *===================================================================*
      *  REVISIONS:                                                       *
      *                                                                   *
      *                                                                   *
      *===================================================================*


      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR04305f  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

      ** Additional Information File
     FWXFMP1    uf a e           k DISK

      ** Group Master File
     FWMFNL     IF   E           K DISK

      ** Employer Master File
     FWMEML     IF   E           K DISK


      *================================================================
      * /COPY 'D' specs
      *================================================================
    
     D/COPY SCOPYPR01D

    
      *================================================================
      * Stand Alone Fields - TOP
      *================================================================
     D optnm           c                   const('Additional information for WC+
     D                                     040305')
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
     D xxForm          s             10    inz('WC040305  ')
     D xxco#           s                   like(emco#)
     D xxfnd           s                   like(emfnd)
     D xxemp#          s                   like(ememp#)
     D xxdiv           s                   like(emdiv)
     D xxfyr           s              3  0
     D xxedn           s             80
    
     C/COPY SCOPYDTPL
    
    
      *================================================================
      *  PLIST definitions
      *================================================================
      ** Entry parameters
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr
    
      ** Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    
    
      *================================================================
      *  KLIST definitions
      *================================================================

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

    
      ** Group Master file
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
    
    
      *================================================================
      * Main
      *================================================================

     C/COPY SCOPYPR01C
    
    
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
    
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
    
     C                   if        q1prms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
    
     C                   eval      xxfyr = 0
     C                   move      @@co#         xxco#
     C                   move      @@fnd         xxfnd
     C                   move      @@emp#        xxemp#
     C                   move      @@div         xxdiv
     C                   move      @@fyr         xxfyr
      *
     C                   eval      scfnd = xxfnd
     C                   eval      scfyr = xxfyr
     C                   eval      xxfrm1 = xxform
      *
     C     fnky          chain     pfn01                              60
     C                   eval      scfnnm = fnname
      *
     C     emky          chain     pem01                              60
      *
     C                   call      'WA005R'
     C                   parm                    ememp#
     C                   parm                    emdiv
     C                   parm                    emname
     C                   parm                    xxedn
      *
     C                   eval      scedn = xxedn
      *
     C                   endsr


      *====================================================================
      * PREDSP - Pre-processing before displaying (every time)
      *====================================================================
     C     PREDSP        begsr

      ** Get endorsement additional information
     C     fmky1         chain     pfm01                              60

     C                   if        *in60 = *off

      ** Move the custom fields to the screen
     C                   eval      scc401 = f3c401
     C                   eval      scc402 = f3c402
     C                   eval      scc403 = f3c403
     C                   eval      scc404 = f3c404
     C                   eval      scc405 = f3c405
     C                   eval      scc406 = f3c406
     C                   eval      scc407 = f3c407
     C                   eval      scc408 = f3c408
     C                   eval      scc409 = f3c409
     C                   eval      scc410 = f3c410
     C                   eval      scc411 = f3c411
     C                   eval      scc412 = f3c412
     C                   eval      scc413 = f3c413

     C                   else

     C                   clear                   pfm01
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

     C                   eval      f3c401 = scc401
     C                   eval      f3c402 = scc402
     C                   eval      f3c403 = scc403
     C                   eval      f3c404 = scc404
     C                   eval      f3c405 = scc405
     C                   eval      f3c406 = scc406
     C                   eval      f3c407 = scc407
     C                   eval      f3c408 = scc408
     C                   eval      f3c409 = scc409
     C                   eval      f3c410 = scc410
     C                   eval      f3c411 = scc411
     C                   eval      f3c412 = scc412
     C                   eval      f3c413 = scc413

     C   60              write     pfm01
     C  N60              update    pfm01

     C                   endsr


