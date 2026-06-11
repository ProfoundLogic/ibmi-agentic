     *===================================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 2001             *
     *===================================================================*
    H/TITLE SR31305I - ADDITIONAL ENTRY PROGRAM FOR WC310305B
     *===================================================================*
     *  DESCRIPTION: Collects additional information for form WC310305B. *
     *                                                                   *
     *  CALLED FROM: SR31305                                             *
     *                                                                   *
     *  CALLS TO:    none                                                *
     *                                                                   *
     *  CREATED: 05/30/01      C:90     PROJECT: CM027                   *
     *===================================================================*
     *  REVSIONS:                                                        *
     *                                                                   *
     *                                                                   *
     *===================================================================*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR31305f  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      ** Additional Information File
     FWXFMP1    uf a e           k DISK
      ** Group Master File
     FWMFNL     IF   E           K DISK
      ** Employer Master File
     FWMEML     IF   E           K DISK
     *
     *================================================================
     * /COPY 'D' specs
     *================================================================
     *
     D/COPY SCOPYPR01D
     *
     *================================================================
     * Stand Alone Fields - TOP
     *================================================================
     D optnm           c                   const('Additional information for WC+
     D                                     310305B')
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
     D xxForm          s             10    inz('WC310305B ')
     D xxco#           s                   like(emco#)
     D xxfnd           s                   like(emfnd)
     D xxemp#          s                   like(ememp#)
     D xxdiv           s                   like(emdiv)
     D xxfyr           s              3  0
     D xxedn           s             80
    C*
     C/COPY SCOPYDTPL
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
      ** Entry parameters
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@fyr
     *
     ** Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
      *
      ** Employer Master file
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
      *
      ** Additional Information file
     C     fmky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxfyr
     C                   kfld                    xxemp#
     C                   kfld                    xxdiv
     C                   kfld                    xxfrm1
     *
      ** Group Master file
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     *================================================================
     * Main
     *================================================================
     *
     C/COPY SCOPYPR01C
     *
     *====================================================================
     * DSCAPI - Call API for getting descriptions (use 91 on error)
     *====================================================================
     C     DSCAPI        begsr
     *
     C                   call      comapi        compl                  91
     *
     C                   endsr
     *====================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *====================================================================
     C     ERRCK1        begsr
     *
     C                   exsr      othfks
     *
     C                   endsr
     *====================================================================
     * KJDO   - Processing for F10
     *====================================================================
     C     KJDO          begsr
     C                   endsr
     *====================================================================
     * OTHFKS - Processing for other functions keys
     *====================================================================
     C     OTHFKS        begsr
     *
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
     *
     C                   endsr
     *====================================================================
     * PGMEND - End of program processing
     *====================================================================
     C     PGMEND        begsr
      *
     C                   if        *inkc = *on OR
     C                             *inkl = *on
     C                   eval      @@call = 'F'
     C                   endif
      *
     C                   endsr
     *====================================================================
     * PGMSTR - Used for any start of program processing (one-time)
     *====================================================================
     C     PGMSTR        begsr
     *
     C                   if        q1prms >= 1
     C                             and @@prds <> *blanks
     C                   movel(p)  @@prds        ##data
     C                   else
     C                   movel(p)  optnm         ##data
     C                   endif
     *
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     *
     C                   movel     ##data        scprds
     *
     C                   if        q1prms >= 2
     C                   movel     @@call        xxcall
     C                   else
     C                   clear                   xxcall
     C                   endif
     *
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
     *====================================================================
     * PREDSP - Pre-processing before displaying (every time)
     *====================================================================
     C     PREDSP        begsr
      *
      ** Get endorsement additional information
     C     fmky1         chain     pfm01                              60
      *
     C                   if        *in60 = *off
      *
      ** Move the custom fields to the screen
     C                   eval      scnm01 = f3c401
     C                   eval      scnm02 = f3c402
     C                   eval      scnm03 = f3c403
     C                   eval      scnm04 = f3c404
     C                   eval      scnm05 = f3c405
     C                   eval      scnm06 = f3c406
     C                   eval      scnm07 = f3c407
     C                   eval      scnm08 = f3c408
     C                   eval      scnm09 = f3c409
     C                   eval      scnm10 = f3c410
     C                   eval      scttl1 = f3c201
     C                   eval      scttl2 = f3c202
     C                   eval      scttl3 = f3c203
     C                   eval      scttl4 = f3c204
     C                   eval      scttl5 = f3c205
     C                   eval      scttl6 = f3c206
     C                   eval      scttl7 = f3c207
     C                   eval      scttl8 = f3c208
     C                   eval      scttl9 = f3c209
     C                   eval      sctt10 = f3c210
      *
     C                   else
      *
     C                   eval      scnm01 = *blanks
     C                   eval      scnm02 = *blanks
     C                   eval      scnm03 = *blanks
     C                   eval      scnm04 = *blanks
     C                   eval      scnm05 = *blanks
     C                   eval      scnm06 = *blanks
     C                   eval      scnm07 = *blanks
     C                   eval      scnm08 = *blanks
     C                   eval      scnm09 = *blanks
     C                   eval      scnm10 = *blanks
     C                   eval      scttl1 = *blanks
     C                   eval      scttl2 = *blanks
     C                   eval      scttl3 = *blanks
     C                   eval      scttl4 = *blanks
     C                   eval      scttl5 = *blanks
     C                   eval      scttl6 = *blanks
     C                   eval      scttl7 = *blanks
     C                   eval      scttl8 = *blanks
     C                   eval      scttl9 = *blanks
     C                   eval      sctt10 = *blanks
      *
     C                   endif
      *
     C                   endsr
     *====================================================================
     * PRSCTL - Process control after no errors found (Call Submitter)
     *====================================================================
     C     PRSCTL        begsr
      *
     C     fmky1         chain     pfm01                              60
      *
     C                   if        *in60 = *on
     C                   clear                   pfm01
     C                   endif
      *
      ** Move the additional information fields to the file
     C                   eval      f3co# = xxco#
     C                   eval      f3fnd = xxfnd
     C                   eval      f3fyr = xxfyr
     C                   eval      f3emp# = xxemp#
     C                   eval      f3div = xxdiv
     C                   eval      f3form = xxform
     C                   eval      f3c401 = scnm01
     C                   eval      f3c402 = scnm02
     C                   eval      f3c403 = scnm03
     C                   eval      f3c404 = scnm04
     C                   eval      f3c405 = scnm05
     C                   eval      f3c406 = scnm06
     C                   eval      f3c407 = scnm07
     C                   eval      f3c408 = scnm08
     C                   eval      f3c409 = scnm09
     C                   eval      f3c410 = scnm10
     C                   eval      f3c201 = scttl1
     C                   eval      f3c202 = scttl2
     C                   eval      f3c203 = scttl3
     C                   eval      f3c204 = scttl4
     C                   eval      f3c205 = scttl5
     C                   eval      f3c206 = scttl6
     C                   eval      f3c207 = scttl7
     C                   eval      f3c208 = scttl8
     C                   eval      f3c209 = scttl9
     C                   eval      f3c210 = sctt10
     C*
     C   60              write     pfm01
     C  N60              update    pfm01
      *
     C                   endsr
