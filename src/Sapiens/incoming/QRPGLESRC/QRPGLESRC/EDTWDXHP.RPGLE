     hcopyright('(c) Fiserv Insurance Solutions, Inc., 2005-2008')                                 *
      *=============================================================================================
      /title Edtwdxhp - Runs edits over converted file wdxhp.                                      *
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
     fwdxhp     if   e             disk    rename(pxh01 : theFile)
     fwdxhl     if   e           k disk    rename(pxh01 : pxh0l)
     fwdf1p     if   e             disk
     fwmemp     if   e             disk
     fwdelp     if   e             disk
     fwmrsl     if   e           k disk
     fwmahl     if   e           k disk
     fwtcrl     if   e           k disk
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
     d rptTitle        s             60    inz('Cancellation History File -
     d                                     WDXHP')
     d fileName        s                   like(EEFILE) inz('WDXHP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Emp/Div/Yr/Req#')
     d errCodes        s            148
     d errDescription  s                   like(EEDATA)
     d field           s                   like(EEFLD)
     d svField         s                   like(EEFLD)
     d svCode          s                   like(EECODE)
     d errField        s                   like(EEFLD)
     d errType         s                   like(EETYPE)
     d XXKEY           s                   like(CRKEY)
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
     d EDTWDXHP        pr
     d  @@libName                    10
     d EDTWDXHP        pi
     d  @@libName                    10


     c/copy wcpycvedtc
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr
      *
      * Rate set code file key list
      *
     c     rsky          klist
     c                   kfld                    XHCO#
     c                   kfld                    ELRTCD
      *
      * Agent History file key list
      *
     c     ahky          klist
     c                   kfld                    XHCO#
     c                   kfld                    XHFND
     c                   kfld                    XHEMP#
     c                   kfld                    XHDIV
     c                   kfld                    XHFYR
      *
      * Cancellation Reason file key list
      *
     c     crky          klist
     c                   kfld                    XHCO#
     c                   kfld                    RSJRST
     c                   kfld                    XXKEY
     c                   kfld                    XHCDAT
     c*
     c     crky1         klist
     c                   kfld                    XHCO#
     c                   kfld                    RSJRST
     c                   kfld                    XXKEY

     c                   eval      x = 0
     c                   eval      prtError = *off
     c                   eval      stopEdit = *off
     c                   eval      keyField = %editc(XHCO# :'3') + '/' +
     c                                        %trim(%editc(XHFND :'3')) + '/' +
     c                                        %trim(%editc(XHEMP#:'3')) + '/' +
     c                                        %trim(%editc(XHDIV :'3')) + '/' +
     c                                        %trim(%editc(XHFYR :'3')) + '/' +
     c                                        %trim(%editc(XHREQ#:'3'))
     c                   eval      errCodes = *blanks
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(XHCO#) = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 1
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'XHCO#'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Number
      *
     c                   if            ValidGrp(XHFND : rrn : XHCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 2
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'XHFND'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Year
      *
     c                   if        ValidGrpYear(XHFND : XHFYR : rrn :
     c                                          XHCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 3
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'XHFYR'
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
     c                   if            ValidEMP(XHEMP# : XHDIV : rrn :
     c                                          XHFND : XHCO#) = *off
     c                   eval      x = 4
     c                   eval      errCodes = %editc(x :'X') + ', '
     c                   eval      errField = 'XHEMP#'
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
     c                   if            ValidEmpYr(XHFYR : XHEMP# : XHDIV :
     c                                          rrn : XHFND : XHCO#) = *off
     c                   eval      x = 5
     c                   eval      errCodes = %editc(x :'X') + ', '
     c                   eval      errField = 'XHFYR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        x <> 5
     c     rrn           chain     pel01                              60
     c                   endif
      *
      * Validate cancellation date.
      *
     c                   if        XHCXDT <> EMCAD1
     c                   eval      x = 6
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'XHCXDT'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate agent #.
      *
     c     ahky          chain     pah01                              60
     c                   if        *in60 = *on or AHAGT# <> XHAGT#
     c                   eval      x = 7
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'XHAGT#'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Validate cancellation reason.
      *
     c     rsky          chain     prs01                              60
      *
     c                   movel(p)  xhrsn         xxkey
      *
     c     crky          setgt     pcr01
     c     crky1         readpe    pcr01                                  60
      *
     c     *in60         doweq     *off
     c     crstat        andeq     'I'
     c     crky1         readpe    pcr01                                  60
     c                   enddo
      *
     c                   if        *in60 = *on
     c                   eval      x = 8
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = 'XHRSN'
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
     c                   eval      ERIUSR = XHUSER
     c                   eval      ERIPGM = XHPGM
     c                   eval      ERIDT  = XHDT
     c                   eval      ERITIM = XHTIME
     c                   eval      ERUSER = XHUSER
     c                   eval      ERPGM  = XHPGM
     c                   eval      ERDT   = XHDT
     c                   eval      ERTIME = XHTIME
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
006Cancellation date does not match employer master.
007Agent # does not match agent history.
008Invalid cancellation reason.
