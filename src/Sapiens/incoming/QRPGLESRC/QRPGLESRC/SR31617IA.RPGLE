     *===================================================================*
     H* (c) Copyright Fiserv Insurance Solutions, Inc., 2008
     *===================================================================*
    H/TITLE SR31617IA - ADDITIONAL ENTRY PROGRAM FOR WC310617A
     *===================================================================*
     *  DESCRIPTION: Collects additional information for form WC310617 . *
     *                                                                   *
     *  CALLED FROM: SR31617A                                            *
     *                                                                   *
     *  CALLS TO:    none                                                *
     *                                                                   *
     *  CREATED: 12/18/08      C:190     PROJECT: WC1762                 *
     *===================================================================*
     *  REVSIONS:                                                        *
     *                                                                   *
     *                                                                   *
     *===================================================================*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FSR31617f  cf   e             workstn
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
     D                                     310617')
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
     D xxForm          s             10    inz('WC310617A ')
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
     C  n90              exfmt     scr02
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
     C                   eval      scfnam = fnname
      *
     C     emky          chain     pem01                              60
      *
     C                   call      'WA005R'
     C                   parm                    ememp#
     C                   parm                    emdiv
     C                   parm                    emname
     C                   parm                    xxedn
      *
     C                   eval      scenam = xxedn
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
     C                   eval      sccty1 = f3c411
     C                   eval      sccty2 = f3c412

     C                   eval      scemp1 = f3c401
     C                   eval      scemp2 = f3c402
     C                   eval      scemp3 = f3c403
     C                   eval      scemp4 = f3c404
     C                   eval      scemp5 = f3c405
     C                   eval      scemp6 = f3c406
     C                   eval      scemp7 = f3c407
     C                   eval      scemp8 = f3c408
     C                   eval      scemp9 = f3c409
     C                   eval      scem10 = f3c410

     C                   eval      scstc1 = f3c201
     C                   eval      scstc2 = f3c202
     C                   eval      scstc3 = f3c203
     C                   eval      scstc4 = f3c204
     C                   eval      scstc5 = f3c205
     C                   eval      scstc6 = f3c206
     C                   eval      scstc7 = f3c207
     C                   eval      scstc8 = f3c208
     C                   eval      scstc9 = f3c209
     C                   eval      scst10 = f3c210

     C                   eval      sclaw1 = f3c101
     C                   eval      sclaw2 = f3c102
     C                   eval      sclaw3 = f3c103
     C                   eval      sclaw4 = f3c104
     C                   eval      sclaw5 = f3c105
     C                   eval      sclaw6 = f3c106
     C                   eval      sclaw7 = f3c107
     C                   eval      sclaw8 = f3c108
     C                   eval      sclaw9 = f3c109
     C                   eval      sclw10 = f3c110

     C                   eval      scexr1 = f3am01
     C                   eval      scexr2 = f3am02
     C                   eval      scbod1 = f3am03
     C                   eval      scbod2 = f3am04
     C                   eval      scbod3 = f3am05
      *
     C                   else
      *
     C                   eval      sccty1 = *blanks
     C                   eval      sccty2 = *blanks
     C                   eval      scemp1 = *blanks
     C                   eval      scemp2 = *blanks
     C                   eval      scemp3 = *blanks
     C                   eval      scemp4 = *blanks
     C                   eval      scemp5 = *blanks
     C                   eval      scemp6 = *blanks
     C                   eval      scemp7 = *blanks
     C                   eval      scemp8 = *blanks
     C                   eval      scemp9 = *blanks
     C                   eval      scem10 = *blanks

     C                   eval      scstc1 = *blanks
     C                   eval      scstc2 = *blanks
     C                   eval      scstc3 = *blanks
     C                   eval      scstc4 = *blanks
     C                   eval      scstc5 = *blanks
     C                   eval      scstc6 = *blanks
     C                   eval      scstc7 = *blanks
     C                   eval      scstc8 = *blanks
     C                   eval      scstc9 = *blanks
     C                   eval      scst10 = *blanks

     C                   eval      sclaw1 = *blanks
     C                   eval      sclaw2 = *blanks
     C                   eval      sclaw3 = *blanks
     C                   eval      sclaw4 = *blanks
     C                   eval      sclaw5 = *blanks
     C                   eval      sclaw6 = *blanks
     C                   eval      sclaw7 = *blanks
     C                   eval      sclaw8 = *blanks
     C                   eval      sclaw9 = *blanks
     C                   eval      sclw10 = *blanks

     C                   eval      scexr1 = *zeros
     C                   eval      scexr2 = *zeros
     C                   eval      scbod1 = *zeros
     C                   eval      scbod2 = *zeros
     C                   eval      scbod3 = *zeros
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

     C                   eval      f3c411 = sccty1
     C                   eval      f3c412 = sccty2

     C                   eval      f3c401 = scemp1
     C                   eval      f3c402 = scemp2
     C                   eval      f3c403 = scemp3
     C                   eval      f3c404 = scemp4
     C                   eval      f3c405 = scemp5
     C                   eval      f3c406 = scemp6
     C                   eval      f3c407 = scemp7
     C                   eval      f3c408 = scemp8
     C                   eval      f3c409 = scemp9
     C                   eval      f3c410 = scem10

     C                   eval      f3c201 = scstc1
     C                   eval      f3c202 = scstc2
     C                   eval      f3c203 = scstc3
     C                   eval      f3c204 = scstc4
     C                   eval      f3c205 = scstc5
     C                   eval      f3c206 = scstc6
     C                   eval      f3c207 = scstc7
     C                   eval      f3c208 = scstc8
     C                   eval      f3c209 = scstc9
     C                   eval      f3c210 = scst10

     C                   eval      f3c101 = sclaw1
     C                   eval      f3c102 = sclaw2
     C                   eval      f3c103 = sclaw3
     C                   eval      f3c104 = sclaw4
     C                   eval      f3c105 = sclaw5
     C                   eval      f3c106 = sclaw6
     C                   eval      f3c107 = sclaw7
     C                   eval      f3c108 = sclaw8
     C                   eval      f3c109 = sclaw9
     C                   eval      f3c110 = sclw10

     C                   eval      f3am01 = scexr1
     C                   eval      f3am02 = scexr2
     C                   eval      f3am03 = scbod1
     C                   eval      f3am04 = scbod2
     C                   eval      f3am05 = scbod3
     C*
     C   60              write     pfm01
     C  N60              update    pfm01
      *
     C                   endsr
