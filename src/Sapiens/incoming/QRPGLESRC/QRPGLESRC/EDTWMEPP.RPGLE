     hcopyright('(c) Fiserv Insurance Solutions, Inc., 2005-2008')                                 *
      *=============================================================================================
      /title Edtwmepp - Runs edits over converted file wmepp.                                      *
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
     fwmepp     if   e             disk    rename(pep01 : theFile)
     fwmepl     if   e           k disk    rename(pep01 : pep0l)
     fwdf1p     if   e             disk
     fwmemp     if   e             disk
     fwdelp     if   e             disk
     fwtabl     if   e           k disk
     f                                     ignore(prs01)
     fwmrsl     if   e           k disk
     fwmprl1    if   e           k disk
     fwasrl5    if   e           k disk
     fwmpxl1    if   e           k disk
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
     d rptTitle        s             60    inz('Employer Payroll File -
     d                                     WMEPP')
     d fileName        s                   like(EEFILE) inz('WMEPP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Emp/Div/Yr/Cls/Spc/Efdt')
     d errCodes        s            148
     d errDescription  s                   like(EEDATA)
     d field           s                   like(EEFLD)
     d svField         s                   like(EEFLD)
     d svCode          s                   like(EECODE)
     d errField        s                   like(EEFLD)
     d errType         s                   like(EETYPE)
     d XXKEY           s                   like(BSKEY)
     d XXST            s                   like(RSJRST)
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
     d EDTWMEPP        pr
     d  @@libName                    10
     d EDTWMEPP        pi
     d  @@libName                    10


     c/copy wcpycvedtc
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr
      *
      * Payroll class code key list
      *
     c     prky          klist
     c                   kfld                    EPCO#
     c                   kfld                    ELRTCD
     c                   kfld                    EPCLAS
     c                   kfld                    ELPFDT
      *
     c     prky1         klist
     c                   kfld                    EPCO#
     c                   kfld                    ELRTCD
     c                   kfld                    EPCLAS
      *
      * S/R Payroll class code key list
      *
     c     srky          klist
     c                   kfld                    EPCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR
     c                   kfld                    EPCLAS
     c                   kfld                    EPEFDT
     c                   kfld                    EPSPEC
      *
      * Table file key list
      *
     c     tbky          klist
     c                   kfld                    EPCO#
     c                   kfld                    XXKEY
      *
      * Rate set code file key list
      *
     c     rsky          klist
     c                   kfld                    EPCO#
     c                   kfld                    ELRTCD
      *
      * Extension file key list
      *
     c     pxky          klist
     c                   kfld                    EPCO#
     c                   kfld                    EPCLAS
     c                   kfld                    EPEXT
     c                   kfld                    XXST
      *

     c                   eval      x = 0
     c                   eval      prtError = *off
     c                   eval      stopEdit = *off
     c                   eval      keyField = %editc(EPCO# :'3') + '/' +
     c                                        %trim(%editc(EPFND :'3')) + '/' +
     c                                        %trim(%editc(EPEMP#:'3')) + '/' +
     c                                        %trim(%editc(EPDIV :'3')) + '/' +
     c                                        %trim(%editc(EPFYR :'3')) + '/' +
     c                                        %trim(%editc(EPCLAS:'3')) + '/' +
     c                                        %trim(EPSPEC)      + '/' +
     c                                        %trim(%editc(DtMDYY(EPEFDT):'Y'))
     c                   eval      errCodes = *blanks
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(EPCO#) = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 1
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'EPCO#'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Number
      *
     c                   if            ValidGrp(EPFND : rrn : EPCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 2
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'EPFND'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Year
      *
     c                   if        ValidGrpYear(EPFND : EPFYR : rrn :
     c                                          EPCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 3
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'EPFYR'
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
     c                   if            ValidEMP(EPEMP# : EPDIV : rrn :
     c                                          EPFND : EPCO#) = *off
     c                   eval      x = 4
     c                   eval      errCodes = %editc(x :'X') + ', '
     c                   eval      errField = 'EPEMP#'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        x <> 4
     c     rrn           chain     pem01                              60
     c                   endif
      *
      * Valid Employer POLICY YEAR
      *
     c                   if            ValidEmpYr(EPFYR : EPEMP# : EPDIV :
     c                                          rrn : EPFND : EPCO#) = *off
     c                   eval      x = 5
     c                   eval      errCodes = %editc(x :'X') + ', '
     c                   eval      errField = 'EPFYR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        x <> 5
     c     rrn           chain     pel01                              60
     c                   endif
      *
      * Validate payroll class code.
      *
     c     prky          setgt     ppr01
     c     prky1         readpe    ppr01                                  60
     c                   if        *in60 = *on
     c                   eval      x = 6
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EPCLAS'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate payroll effective date.
      *
     c                   if        EPEFDT < ELPFDT or EPEFDT > ELPTDT
     c                   eval      x = 7
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EPEFDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
     c*
      * Validate billing schedule code if payroll flag is set to 'S'
      *
     c                   movel(p)  ELBSCD        XXKEY
     c     tbky          chain     pbs01                              60
     c                   if        *in60 = *off  and BSSEFL <> 'S'
     c                             and EPSAFL = 'S'
     c                   eval      x = 8
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EPSAFL'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate total s/r payroll matches WASRP if s/r.
      *
     c                   if        *in60 = *off  and BSSEFL = 'S'
      *
     c                   z-add     0             xxtotp           13 2
      *
     c     srky          chain     psr01                              60
     c     *in60         doweq     *off
     c                   add       sramtp        xxtotp
     c     srky          reade     psr01                                  60
     c                   enddo
      *
     c                   if        epsapr <> xxtotp
      *
     c                   eval      x = 9
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EPSAPR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  *in60 ifeq *off
      *
      * Validate the status flag.
      *
     c                   if        EPSTAT <> 'A' and EPSTAT <> 'I'
      *
     c                   eval      x = 10
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EPSTAT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate the extenstion if there.
      *
     c                   if        EPEXT <> '     '
      *
     c     rsky          chain     prs01                              60
     c                   eval      xxst = '  '
     c     pxky          chain     ppx01                              60
      *
     c                   if        *in60 = *on
     c                   eval      xxst = rsjrst
     c     pxky          chain     ppx01                              60
     c                   endif
      *
     c                   if        *in60 = *on
     c                   eval      x = 11
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'EPEXT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   endif                                                  EPEXT ifeq '     '
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
     c                   eval      ERIUSR = EPIUSR
     c                   eval      ERIPGM = EPIPGM
     c                   eval      ERIDT  = EPIDT
     c                   eval      ERITIM = EPITM
     c                   eval      ERUSER = EPUSER
     c                   eval      ERPGM  = EPPGM
     c                   eval      ERDT   = EPDT
     c                   eval      ERTIME = EPTIME
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
005Invalid Employer Policy Year.
006Invalid payroll class code.
007Payroll eff date does not fall within policy period.
008Payroll set to S/R but billing schedule code is not S/R.
009Total payroll in WASRP does not match S/R payroll total.
010Payroll status must be a 'A' or an 'I'
011Payroll extension is invalid.
