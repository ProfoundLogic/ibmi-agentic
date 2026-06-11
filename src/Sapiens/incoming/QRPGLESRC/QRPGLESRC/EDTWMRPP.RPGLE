     hcopyright('(c) Fiserv Insurance Solutions, Inc., 2005-2008')                                 *
      *=============================================================================================
      /title Edtwmrpp - Runs edits over converted file wmrpp.                                      *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  CREATED 11/15/05   C:65    PROJECT: 136WBD                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Updated for V8R1                                                           *
      *                -Alligned field edits with file layout                                      *
      *                -Eliminate the need for library to be passed                                *
      *=============================================================================================
     fwmrpp     if   e             disk    rename(prp01 : theFile)
     fwmrpl     if   e           k disk    rename(prp01 : prp0l)
     fwdf1p     if   e             disk
     fwmemp     if   e             disk
     fwdelp     if   e             disk
     fwtgtl     if   e           k disk    rename(pgt01 : pgt00)
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
     d                                     WMRPP')
     d fileName        s                   like(EEFILE) inz('WMRPP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Emp/Div/Yr/Req#')
     d errCodes        s            148
     d errDescription  s                   like(EEDATA)
     d field           s                   like(EEFLD)
     d svField         s                   like(EEFLD)
     d svCode          s                   like(EECODE)
     d errField        s                   like(EEFLD)
     d errType         s                   like(EETYPE)
     d errCount        s              7  0
     d recCount        s              9  0
     d errTtlCnt       s              9  0
     d line            s            180
     d kkkey           s                   like(GTKEY)
     d kktbid          s                   like(GTTBID)
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
     d EDTWMRPP        pr
     d  @@libName                    10
     d EDTWMRPP        pi
     d  @@libName                    10


     c/copy wcpycvedtc
      *=============================================================================================
      *  Klists
      *=============================================================================================
     c     gtky          klist
     c                   kfld                    RPCO#
     c                   kfld                    kktbid
     c                   kfld                    kkkey
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr

     c                   eval      x = 0
     c                   eval      prtError = *off
     c                   eval      stopEdit = *off
     c                   eval      keyField = %editc(RPCO# :'3') + '/' +
     c                                        %trim(%editc(RPFND :'3')) + '/' +
     c                                        %trim(%editc(RPEMP#:'3')) + '/' +
     c                                        %trim(%editc(RPDIV :'3')) + '/' +
     c                                        %trim(%editc(RPFYR :'3'))
     c                   eval      errCodes = *blanks
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(RPCO#) = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 1
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'RPCO#'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Number
      *
     c                   if            ValidGrp(RPFND : rrn : RPCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 2
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'RPFND'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
      * Valid Group Year
      *
     c                   if        ValidGrpYear(RPFND : RPFYR : rrn :
     c                                          RPCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 3
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'RPFYR'
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
     c                   if            ValidEMP(RPEMP# : RPDIV : rrn :
     c                                          RPFND : RPCO#) = *off
     c                   eval      x = 4
     c                   eval      errCodes = %editc(x :'X') + ', '
     c                   eval      errField = 'RPEMP#'
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
     c                   if            ValidEmpYr(RPFYR : RPEMP# : RPDIV :
     c                                          rrn : RPFND : RPCO#) = *off
     c                   eval      x = 5
     c                   eval      errCodes = %editc(x :'X') + ', '
     c                   eval      errField = 'RPFYR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
      *
     c                   if        x <> 5
     c     rrn           chain     pel01                              60
     c                   endif
      *
      * Edit type of carrier code
      *
     c                   eval      kktbid = 'CRR'
     c                   eval      kkkey  = RPCARR
     c     gtky          chain     pgt00
     c                   if        not(%found) and
     c                             stopEdit = *off
     c                   eval      stopEdit = *on
     c                   eval      x = 6
     c                   eval      errCodes = %editc(x :'X')
     c                   eval      errField = 'RPCARR'
     c                   eval      errType  =  'T'
     c                   exsr      srWrtErr
     c                   eval      prtError = *on
     c                   endif
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
     c                   eval      ERIUSR = RPIUSR
     c                   eval      ERIPGM = RPIPGM
     c                   eval      ERIDT  = RPIDT
     c                   eval      ERITIM = RPITM
     c                   eval      ERUSER = RPUSER
     c                   eval      ERPGM  = RPPGM
     c                   eval      ERDT   = RPDT
     c                   eval      ERTIME = RPTIME
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
006Invalid Carrier code.
