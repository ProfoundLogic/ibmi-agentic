     hcopyright('(c) Fiserv Insurance Solutions, Inc., 2005-2008')                                 *
      *=============================================================================================
      /title Edtwdehp - Runs edits over converted file wdehp.                                      *
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
     fwdehp     if   e             disk    rename(peh01 : theFile)
     fwdehl     if   e           k disk    rename(peh01 : peh0l)
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
     d rptTitle        s             60    inz('Employer History File -
     d                                     WDEHP')
     d fileName        s                   like(EEFILE) inz('WDEHP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Emp/Div/Yr/Faf')
     d errCodes        s            148
     d errDescription  s                   like(EEDATA)
     d field           s                   like(EEFLD)
     d svField         s                   like(EEFLD)
     d svCode          s                   like(EECODE)
     d errField        s                   like(EEFLD)
     d errType         s                   like(EETYPE)
     d XXKEY           s                   like(BSKEY)
     d XXST            s                   like(RSJRST)
     d XXDDCD          s                   like(EHDCDE)
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
     d EDTWDEHP        pr
     d  @@libName                    10
     d EDTWDEHP        pi
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
     c                   kfld                    EHCO#
     c                   kfld                    XXKEY
      *
      * Rate set code file key list
      *
     c     rsky          klist
     c                   kfld                    EHCO#
     c                   kfld                    EHRTCD
      *
      * Statistical file key list
      *
     c     rrky          klist
     c                   kfld                    EHCO#
     c                   kfld                    EHFND
     c                   kfld                    XXST
     c                   kfld                    EHPFDT
      *
     c     rrky1         klist
     c                   kfld                    EHCO#
     c                   kfld                    EHFND
     c                   kfld                    XXST
      *
      * Discount file key list
      *
     c     dsky          klist
     c                   kfld                    EHCO#
     c                   kfld                    EHFND
     c                   kfld                    XXST
     c                   kfld                    EHDISC
     c                   kfld                    EHPFDT
      *
     c     dsky1         klist
     c                   kfld                    EHCO#
     c                   kfld                    EHFND
     c                   kfld                    XXST
     c                   kfld                    EHDISC
      *
      * Increased limits file key list
      *
     c     lmky          klist
     c                   kfld                    EHCO#
     c                   kfld                    EHFND
     c                   kfld                    XXST
     c                   kfld                    XXKEY
     c                   kfld                    EHPFDT
      *
     c     lmky1         klist
     c                   kfld                    EHCO#
     c                   kfld                    EHFND
     c                   kfld                    XXST
     c                   kfld                    XXKEY
      *
      * Deductible code file key list
      *
     c     ddky          klist
     c                   kfld                    EHCO#
     c                   kfld                    EHFND
     c                   kfld                    XXST
     c                   kfld                    XXDDCD
     c                   kfld                    EHPFDT
      *
     c     ddky1         klist
     c                   kfld                    EHCO#
     c                   kfld                    EHFND
     c                   kfld                    XXST
     c                   kfld                    XXDDCD
      *

     c                   eval      x = 0
     c                   eval      prtError = *off
     c                   eval      stopEdit = *off
     c                   eval      keyField = %editc(EHCO# :'3') + '/' +
     c                                        %trim(%editc(EHFND :'3')) + '/' +
     c                                        %trim(%editc(EHEMP#:'3')) + '/' +
     c                                        %trim(%editc(EHDIV :'3')) + '/' +
     c                                        %trim(%editc(EHFYR :'3')) + '/' +
     c                                        EHFAF
     c                   eval      errCodes = *blanks
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(EHCO#) = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 1
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'EHCO#'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Number
      *
     c                   if            ValidGrp(EHFND : rrn : EHCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 2
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'EHFND'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Year
      *
     c                   if        ValidGrpYear(EHFND : EHFYR : rrn :
     c                                          EHCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 3
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'EHFYR'
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
     c                   if            ValidEMP(EHEMP# : EHDIV : rrn :
     c                                          EHFND : EHCO#) = *off
     c                   eval      x = 4
     c                   eval      errCodes = %editc(x :'X') + ', '
     c                   eval      errField = 'EHEMP#'
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
     c                   if        EHPFDT < F1STPO or EHPFDT >= F1ENPO
     c                   eval      x = 5
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHPFDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
     c*
     c                   if        EHPFDT < EMEFD1
     c                   eval      x = 6
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHPFDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
     c*
     c                   if        EHPFDT > EHPTDT
     c                   eval      x = 7
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHPFDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate policy exp date.
      *
     c                   if        EHPTDT < EMEFD1
     c                   eval      x = 8
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHPTDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
     c*
     c                   if        EHPTDT > EMCAD1 and EMCAD1 <> 0 and
     c                             EHFAF = 'A'
     c                   eval      x = 9
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHPTDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate policy anniv date.
      *
     c                   if        EHANDT > EHPFDT
     c                   eval      x = 10
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHANDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate billing schedule code.
      *
     c                   movel(p)  EHBSCD        XXKEY
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
     c                   eval      errField = 'EHBSCD'
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
     c                   eval      errField = 'EHRTCD'
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
     c                   eval      errField = 'EHRTCD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for contractors credit.
      *
     c                   if        EHASP1 <> 0 and RRCCCD = 0
     c                   eval      x = 15
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHASP1'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for deviation #1.
      *
     c                   if        EHASP2 <> 0 and RRDV1C = 0
     c                   eval      x = 16
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHASP2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for deviation #2.
      *
     c                   if        EHDEV2 <> 0 and RRDV2C = 0
     c                   eval      x = 17
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHDEV2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate discount code record is set up.
      *
     c                   if        EHDISC <> ' '
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
     c                   eval      errField = 'EHDISC'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  EHDISC <> ' '
      *
      * Validate increased limits #1 code record is set up.
      *
     c                   if        EHILCD <> '  '
      *
     c                   movel(p)  EHILCD        XXKEY
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
     c                   eval      errField = 'EHILCD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate increased limits #1 expires before incr limits #2.
      *
     c                   if        EHILCD <> '  ' and EHI2CD <> '  '
     c                             and EHILEX > EHI2EF
     c                   eval      x = 20
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHILCD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  EHILCD <> '  '
      *
      * Validate increased limits #2 code record is set up.
      *
     c                   if        EHI2CD <> '  '
      *
     c                   movel(p)  EHI2CD        XXKEY
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
     c                   eval      errField = 'EHI2CD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate increased limits #2 eff before policy exp date.
      *
     c                   if        EHI2EF > EHPTDT
      *
     c                   eval      x = 22
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHI2CD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  EHI2CD <> '  '
      *
      * Validate deductible code #1 code record is set up.
      *
     c                   if        EHDCDE <> '   '
      *
     c                   movel(p)  EHDCDE        XXDDCD
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
     c                   eval      errField = 'EHDCDE'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate deductible code #1 expires before deductible #2.
      *
     c                   if        EHDCDE <> '  ' and EHDCD2 <> '  '
     c                             and EHDCEX > EH2DEF
     c                   eval      x = 24
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHDCDE'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  EHDCDE <> '  '
      *
      * Validate deductible code #2 code record is set up.
      *
     c                   if        EHDCD2 <> '   '
      *
     c                   movel(p)  EHDCD2        XXDDCD
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
     c                   eval      errField = 'EHDCD2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate deductible code #2 eff before policy exp date.
      *
     c                   if        EH2DEF > EHPTDT
      *
     c                   eval      x = 26
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHDCD2'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  EHDCD2 <> '  '
      *
      * Validate stat code has been set up for subj prem credit #1A
      *
     c                   if        EHASPM <> 0 and RR1ACD = 0
     c                   eval      x = 27
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHASPM'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for subj prem credit #1B
      *
     c                   if        EHASPS <> 0 and RR1BCD = 0
     c                   eval      x = 28
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHASPS'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate stat code has been set up for schedule db/cr.
      *
     c                   if        (EHDBCR > 1 and RRDBCD = 0) or
     c                             (EHDBCR > 0 and EHDBCR < 1 and RRCRCD = 0)
     c                   eval      x = 29
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHDBCR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Waiver stat could should be set up if there are waiver settings
      *
     c                   if        (EHWVPC <> 0 or EHWMIN <> 0 or EHWVFL <> ' ')
     c                             and RRWVAC = 0
     c                   eval      x = 36
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHWVFL'
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
     c                   if        *in60 = *on and EHWLOS <> 0
     c                   eval      x = 37
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHWLOS'
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
     c                   if        *in60 = *on and (EHPRTD <> 0 or EHPRTF <> 0)
     c                   eval      x = 38
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHPRTD'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Terrorism charge should be set up if there is an amount.
      *
     c                   if        EHWTER <> 0
     c                             and RRTERR = 0
     c                   eval      x = 39
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHWTER'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Domestic Terror charge should be set up if there is an amount.
      *
     c                   if        EHWDTR <> 0
     c                             and RRTER2 = 0
     c                   eval      x = 40
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EHWDTR'
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
     c                   eval      ERIUSR = EHIUSR
     c                   eval      ERIPGM = EHPGM
     c                   eval      ERIDT  = EHIDT
     c                   eval      ERITIM = EHTIME
     c                   eval      ERUSER = EHUSER
     c                   eval      ERPGM  = EHPGM
     c                   eval      ERDT   = EHDT
     c                   eval      ERTIME = EHTIME
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
