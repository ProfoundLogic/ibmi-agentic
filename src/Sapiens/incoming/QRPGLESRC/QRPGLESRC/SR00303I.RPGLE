     *  (C) Copyright Progressive Data Solutions, Inc., 2001 - 2005      *
     *
    H/TITLE SR00303I - ADDITIONAL ENTRY PROGRAM FOR WC000303
     *===================================================================*
     *  DESCRIPTION: Collects additional information for form WC000303.  *
     *                                                                   *
     *  CALLED FROM: SR00303                                             *
     *                                                                   *
     *  CALLS TO:    none                                                *
     *                                                                   *
     *  CREATED: 04/05/02      C:90     PROJECT: CM027                   *
     *===================================================================*
     *  REVSIONS:                                                        *
     *                                                                   *
     *  05/24/05 c:61  support wa102 45780                               *
     *                 Change to use form WC000303C.                     *
     *                                                                   *
     *===================================================================*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR00303f  cf   e             workstn
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
     D                                     000303')
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
      ****** beg chg ******** 05/24/05 **********************************
     D*******xxForm          s             10    inz('WC000303B ')
     D xxForm          s             10    inz('WC000303C ')
      ****** end chg ******** 05/24/05 **********************************
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
     C                   eval      scst01 = f3c101
     C                   eval      scst02 = f3c102
     C                   eval      scst03 = f3c103
     C                   eval      scst04 = f3c104
     C                   eval      scst05 = f3c105
     C                   eval      scst06 = f3c106
     C                   eval      scst07 = f3c107
     C                   eval      scst08 = f3c108
     C                   eval      scst09 = f3c109
     C                   eval      scst10 = f3c110
      *
     C                   else
      *
     C                   eval      scst01 = *blanks
     C                   eval      scst02 = *blanks
     C                   eval      scst03 = *blanks
     C                   eval      scst04 = *blanks
     C                   eval      scst05 = *blanks
     C                   eval      scst06 = *blanks
     C                   eval      scst07 = *blanks
     C                   eval      scst08 = *blanks
     C                   eval      scst09 = *blanks
     C                   eval      scst10 = *blanks
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
     C                   eval      f3c101 = scst01
     C                   eval      f3c102 = scst02
     C                   eval      f3c103 = scst03
     C                   eval      f3c104 = scst04
     C                   eval      f3c105 = scst05
     C                   eval      f3c106 = scst06
     C                   eval      f3c107 = scst07
     C                   eval      f3c108 = scst08
     C                   eval      f3c109 = scst09
     C                   eval      f3c110 = scst10
     C*
     C   60              write     pfm01
     C  N60              update    pfm01
      *
     C                   endsr
