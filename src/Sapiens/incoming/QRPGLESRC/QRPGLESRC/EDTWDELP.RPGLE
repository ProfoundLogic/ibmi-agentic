     hcopyright('(c) Fiserv Insurance Solutions, Inc., 2005-2008')                                 *
      *=============================================================================================
      /title Edtwdelp - Runs edits over converted file wdelp.                                      *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  CREATED 11/15/05   C:65    PROJECT: 136WBD                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Eliminate the need for library to be passed                                *
      *=============================================================================================
     fwdelp     if   e             disk    rename(pel01 : theFile)
     fwdell     if   e           k disk    rename(pel01 : pel0l)
     fwdf1p     if   e             disk
     fwmemp     if   e             disk
     fwtabl     if   e           k disk
     f                                     ignore(prs01)
     fwmrsl     if   e           k disk
     fwdrrl     if   e           k disk
     fwdscl     if   e           k disk
     fwdecl     if   e           k disk
     fwmdsl3    if   e           k disk
     fwtlml1    if   e           k disk
     fwtddl2    if   e           k disk
     fedterl    uf a e           k disk
     fqsysprt   o    f  180        printer oflind(*inof) usropn
      *=============================================================================================
      * Stand alone fields - required
      *=============================================================================================
     d firstPrt        s               n   inz(*on)
     d firstSummPrt    s               n   inz(*on)
     d prtSummary      s               n   inz(*off)
     d prtError        s               n   inz(*off)
     d companyNm       s             60
     d sysDat7         s                   like(EEDT)
     d sysDate         s              8  0
     d sysTime         s                   like(EETIME)
     d x               s              3  0
     d libName         s                   like(EEFLIB)  inz('PDSFIL    ')
     d rptTitle        s             60    inz('Employer Detail File -
     d                                     WDELP')
     d fileName        s                   like(EEFILE) inz('WDELP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Emp/Div/Yr')
     d errCodes        s            148
     d errDescription  s                   like(EEDATA)
     d field           s                   like(EEFLD)
     d svField         s                   like(EEFLD)
     d svCode          s                   like(EECODE)
     d errField        s                   like(EEFLD)
     d errType         s                   like(EETYPE)
     d XXKEY           s                   like(BSKEY)
     d XXST            s                   like(RSJRST)
     d XXDDCD          s                   like(ELDCDE)
     d errCount        s              7  0
     d recCount        s              9  0
     d errTtlCnt       s              9  0
     d line            s            180
      *=============================================================================================
      * Stand alone fields - unique to program
      *=============================================================================================
     d stopEdit        s               n   inz(*off)
     d rrn             s              9  0
     d firstBlank      s              2  0
      *=============================================================================================
      * Data structures  and arrays - required for copy book
      *=============================================================================================
     d psds          esds                  extname(szq1p)
     d eCde            s              3  0 dim(99) ctdata perrcd(1)
     d eMsg            s             60    dim(99) alt(eCde)
      *=============================================================================================
      * Procedure prototypes
      *=============================================================================================
     d*entry
     d EDTWDELP        pr
     d  @@libName                    10
     d EDTWDELP        pi
     d  @@libName                    10


     c/copy wcpycvedtc
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr
      *
      * Table file key list
      *
     c     tbky          klist
     c                   kfld                    ELCO#
     c                   kfld                    XXKEY
      *
      * Rate set code file key list
      *
     c     rsky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELRTCD
      *
      * Statistical file key list
      *
     c     rrky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    XXST
     c                   kfld                    ELPFDT
      *
     c     rrky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    XXST
      *
      * Discount file key list
      *
     c     dsky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    XXST
     c                   kfld                    ELDISC
     c                   kfld                    ELPFDT
      *
     c     dsky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    XXST
     c                   kfld                    ELDISC
      *
      * Increased limits file key list
      *
     c     lmky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    XXST
     c                   kfld                    XXKEY
     c                   kfld                    ELPFDT
      *
     c     lmky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    XXST
     c                   kfld                    XXKEY
      *
      * Deductible code file key list
      *
     c     ddky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    XXST
     c                   kfld                    XXDDCD
     c                   kfld                    ELPFDT
      *
     c     ddky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    XXST
     c                   kfld                    XXDDCD
      *

     c                   eval      x = 0
     c                   eval      prtError = *off
     c                   eval      stopEdit = *off
     c                   eval      keyField = %editc(ELCO# :'3') + '/' +
     c                                        %trim(%editc(ELFND :'3')) + '/' +
     c                                        %trim(%editc(ELEMP#:'3')) + '/' +
     c                                        %trim(%editc(ELDIV :'3')) + '/' +
     c                                        %trim(%editc(ELFYR :'3'))
     c                   eval      errCodes = *blanks
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(ELCO#) = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 1
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'ELCO#'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Number
      *
     c                   if            ValidGrp(ELFND : rrn : ELCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 2
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'ELFND'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Year
      *
     c                   if        ValidGrpYear(ELFND : ELFYR : rrn :
     c                                          ELCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 3
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'ELFYR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Only process remaining edits if we have a valid CO#/FND/FYR
      *
     c                   if        stopEdit  = *off
      *
     c     rrn           chain     pf101                              60
      *
      * Valid Employer/Division
      *
     c                   if            ValidEMP(ELEMP# : ELDIV : rrn :
     c                                          ELFND : ELCO#) = *off
     c                   eval      x = 4
     c                   eval      errCodes = %editc(x :'X') + ', '
     c                   eval      errField = 'ELEMP#'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        x <> 4
     c     rrn           chain     pem01                              60
     c                   endif
      *
      * Validate policy eff date.
      *
     c                   if        ELPFDT < F1STPO or ELPFDT >= F1ENPO
     c                   eval      x = 5
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELPFDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
     c*
     c                   if        ELPFDT < EMEFD1
     c                   eval      x = 6
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELPFDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
     c*
     c                   if        ELPFDT > ELPTDT
     c                   eval      x = 7
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELPFDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate policy exp date.
      *
     c                   if        ELPTDT < EMEFD1
     c                   eval      x = 8
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELPTDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
     c*
     c                   if        ELPTDT > EMCAD1 and EMCAD1 <> 0 and
     c                             ELFAF = 'A'
     c                   eval      x = 9
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELPTDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate policy anniv date.
      *
     c                   if        ELANDT > ELPFDT
     c                   eval      x = 10
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELANDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate billing schedule code.
      *
     c                   movel(p)  ELBSCD        XXKEY
     c     tbky          chain     pbs01                              60
     c                   if        *in60 = *on
     c                   eval      x = 11
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELBSCD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate rate set code.
      *
     c     rsky          chain     prs01                              60
     c                   if        *in60 = *on
     c                   eval      x = 12
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELRTCD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
     c*
     c                   if        *in60 = *off
     c                   eval      XXST = rsjrst
     c                   else
     c                   eval      XXST = '  '
     c                   endif
      *
     c     rrky          setgt     prr01
     c     rrky1         readpe    prr01                                  60
      *
      * Validate statistical record is set up.
      *
     c                   if        *in60 = *on
     c                   eval      x = 13
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELRTCD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate state tax is set up if tax amount.
      *
     c                   if        ELTXDU <> 0 and RRTXPC = 0
     c                   eval      x = 14
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELTXDU'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for contractors credit.
      *
     c                   if        ELASP1 <> 0 and RRCCCD = 0
     c                   eval      x = 15
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELASP1'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for deviation #1.
      *
     c                   if        ELASP2 <> 0 and RRDV1C = 0
     c                   eval      x = 16
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELASP2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for deviation #2.
      *
     c                   if        ELDEV2 <> 0 and RRDV2C = 0
     c                   eval      x = 17
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELDEV2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate discount code record is set up.
      *
     c                   if        ELDISC <> ' '
      *
     c     dsky          setgt     pds01
     c     dsky1         readpe    pds01                                  60
      *
     c                   if        *in60 = *on
     c                   eval      x = 18
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELDISC'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  ELDISC <> ' '
      *
      * Validate increased limits #1 code record is set up.
      *
     c                   if        ELILCD <> '  '
      *
     c                   movel(p)  ELILCD        XXKEY
     c     lmky          setgt     plm01
     c     lmky1         readpe    plm01                                  60
      *
     c                   if        *in60 = *on
     c                   eval      x = 19
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELILCD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate increased limits #1 expires before incr limits #2.
      *
     c                   if        ELILCD <> '  ' and ELINL2 <> '  '
     c                             and ELILEX > ELI2EF
     c                   eval      x = 20
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELILCD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  ELILCD <> '  '
      *
      * Validate increased limits #2 code record is set up.
      *
     c                   if        ELINL2 <> '  '
      *
     c                   movel(p)  ELINL2        XXKEY
     c     lmky          setgt     plm01
     c     lmky1         readpe    plm01                                  60
      *
     c                   if        *in60 = *on
     c                   eval      x = 21
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELINL2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate increased limits #2 eff before policy exp date.
      *
     c                   if        ELI2EF > ELPTDT
      *
     c                   eval      x = 22
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELINL2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  ELINL2 <> '  '
      *
      * Validate deductible code #1 code record is set up.
      *
     c                   if        ELDCDE <> '   '
      *
     c                   movel(p)  ELDCDE        XXDDCD
     c     ddky          setgt     pdd01
     c     ddky1         readpe    pdd01                                  60
      *
     c                   if        *in60 = *on
     c                   eval      x = 23
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELDCDE'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate deductible code #1 expires before deductible #2.
      *
     c                   if        ELDCDE <> '  ' and ELDCD2 <> '  '
     c                             and ELDCEX > EL2DEF
     c                   eval      x = 24
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELDCDE'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  ELDCDE <> '  '
      *
      * Validate deductible code #2 code record is set up.
      *
     c                   if        ELDCD2 <> '   '
      *
     c                   movel(p)  ELDCD2        XXDDCD
     c     ddky          setgt     pdd01
     c     ddky1         readpe    pdd01                                  60
      *
     c                   if        *in60 = *on
     c                   eval      x = 25
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELDCD2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate deductible code #2 eff before policy exp date.
      *
     c                   if        EL2DEF > ELPTDT
      *
     c                   eval      x = 26
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELDCD2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  ELDCD2 <> '  '
      *
      * Validate stat code has been set up for subj prem credit #1A
      *
     c                   if        ELASPM <> 0 and RR1ACD = 0
     c                   eval      x = 27
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELASPM'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for subj prem credit #1B
      *
     c                   if        ELASPS <> 0 and RR1BCD = 0
     c                   eval      x = 28
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELASPS'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for schedule db/cr.
      *
     c                   if        (ELDBCR > 1 and RRDBCD = 0) or
     c                             (ELDBCR > 0 and ELDBCR < 1 and RRCRCD = 0)
     c                   eval      x = 29
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELDBCR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate safety fee is set up at group level if there is % or amt.
      *
     c                   if        (ELSFPC <> 0 or ELSFMN <> 0) and RRSFAP = ' '
     c                   eval      x = 30
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELSFPC'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate assessment is set up at group level if there is an amount.
      *
     c                   if        EL1AAM <> 0 and (RR1STP = 0 and RR1STF = 0)
     c                   eval      x = 31
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EL1AAM'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        EL2AAM <> 0 and (RR2NDP = 0 and RR2NDF = 0)
     c                   eval      x = 32
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EL2AAM'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        EL3AAM <> 0 and (RR3RDP = 0 and RR3RDF = 0)
     c                   eval      x = 32
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EL3AAM'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        EL4AAM <> 0 and (RR4THP = 0 and RR4THF = 0)
     c                   eval      x = 34
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EL4AAM'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        EL5AAM <> 0 and (RR5THP = 0 and RR5THF = 0)
     c                   eval      x = 35
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EL5AAM'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Waiver stat could should be set up if there are waiver settings
      *
     c                   if        (ELWVPC <> 0 or ELWMIN <> 0 or ELWVFL <> ' ')
     c                             and RRWVAC = 0
     c                   eval      x = 36
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELWVFL'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c     rrky          setgt     psc01
     c     rrky1         readpe    psc01                                  60
      *
      * Validate loss constant record is set up if there is amount.
      *
     c                   if        *in60 = *on and ELLOSS <> 0
     c                   eval      x = 37
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELLOSS'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c     rrky          setgt     pec01
     c     rrky1         readpe    pec01                                  60
      *
      * Validate loss constant record is set up if there is amount.
      *
     c                   if        *in60 = *on and (ELPRTD <> 0 or ELPRTF <> 0)
     c                   eval      x = 38
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELPRTD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Terrorism charge should be set up if there is an amount.
      *
     c                   if        (ELETER <> 0 or ELATER <> 0)
     c                             and RRTERR = 0
     c                   eval      x = 39
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELETER'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Domestic Terror charge should be set up if there is an amount.
      *
     c                   if        (ELEDTR <> 0 or ELADTR <> 0)
     c                             and RRTER2 = 0
     c                   eval      x = 40
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'ELEDTR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      *
     c                   endif                                                  if stopEdit = *off
      *
      * Prt Detail line at the end
      *
     c                   if        prtError = *on
     c                   exsr      srPrint
     c                   endif

     c                   endsr
      *=============================================================================================
      *  srWrtErr
      *=============================================================================================
     c     srWrtErr      begsr
     c                   eval      EEKEY  = keyField
     c                   eval      EEFLD  = errField
     c                   select
     c                   when      %parms >= 1
     c                   eval      EEFLIB = @@libName
     c                   when      fileLib = *blanks
     c                   eval      EEFLIB = libName
     c                   other
     c                   eval      EEFLIB = fileLib
     c                   endsl
     c                   eval      EEFILE = fileName
     c                   eval      EECODE = eCde(x)
     c                   eval      EEDATA = eMsg(x)
     c                   eval      EETYPE = errType
     c************       eval      EESTAT + ???????
     c                   eval      ERIUSR = ELIUSR
     c                   eval      ERIPGM = ELIPGM
     c                   eval      ERIDT  = ELIDT
     c                   eval      ERITIM = ELITM
     c                   eval      ERUSER = ELUSER
     c                   eval      ERPGM  = ELPGM
     c                   eval      ERDT   = ELDT
     c                   eval      ERTIME = ELTIME
     c                   eval      EEUSER = Q1USER
     c                   eval      EEIPGM = Q1PGM
     c                   eval      EEIDT  = sysDat7
     c                   eval      EEITIM = sysTime
     c                   eval      EEUSER = Q1USER
     c                   eval      EEPGM  = Q1PGM
     c                   eval      EEDT   = sysDat7
     c                   eval      EETIME = sysTime
     c                   write     eer01
     c                   endsr

     o/copy wcpycvedto
      *=================================================================================
      * errorCode/errorMessage Table
      *=================================================================================
**   eCde/eMsg  3.0/60
001Invalid company number.
002Invalid group number.
003Invalid group year.
004Invalid Employer/Division.
005Policy eff date is not within group year range.
006Policy eff date is prior to employer eff date.
007Policy eff date is greater than policy exp date.
008Policy exp date is prior to employer eff date.
009Policy exp date is greater than employer cx date.
010Policy anniversary date is greater than policy eff date.
011Billing schedule code is invalid.
012Rate set code is invalid.
013Statistical record has not been set up at group level.
014State tax is present but not set up at group level.
015Contractors credit is present but not set up at group level.
016Deviation #1 is present but not set up at group level.
017Deviation #2 is present but not set up at group level.
018Discount code is invalid.
019Increased limits code #1 is invalid.
020Incr limits #1 exp date greater than IL #2 eff date.
021Increased limits code #2 is invalid.
022Incr limits #2 eff date greater than policy exp date.
023Deductible code #1 is invalid.
024Deductible #1 exp date greater than Deduc #2 eff date.
025Deductible code #2 is invalid.
026Deductible #2 eff date greater than policy exp date.
027Subject prm cr 1A is present but not set up at group level.
028Subject prm cr 1B is present but not set up at group level.
029Schedule DB/CR is present but not set up at group level.
030Safety fee is present but not set up at group level.
0311st Assessment is present but not set up at group level.
0322nd Assessment is present but not set up at group level.
0333rd Assessment is present but not set up at group level.
0344th Assessment is present but not set up at group level.
0355th Assessment is present but not set up at group level.
036Waiver information is present but not set up at group level.
037Loss Constant is present but not set up at group level.
038Expense Constant is present but not set up at group level.
039Terrorism charge is present but not set up at group level.
040Domestic terr chrg is present but not set up at group level.
