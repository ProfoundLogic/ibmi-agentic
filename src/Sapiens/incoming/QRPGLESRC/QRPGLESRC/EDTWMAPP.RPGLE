     hcopyright ('(c) Copyright Fiserv Insurance Solutions, Inc., 2008')                           *
      *=============================================================================================
      /title EdtWMAPP - Runs edits over converted file WMAPP.                                      *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  Created 10/29/08   C:33   Project: 114WPD                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      *=============================================================================================
     fwmapp     if   e             disk    rename(pap01 : theFile)
     f                                     prefix(x@:2)
     fststl1    if   e           k disk
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
     d rptTitle        s             60    inz('Edit Agent Producer File -
     d                                     WMAPP')
     d fileName        s                   like(EEFILE) inz('WMAPP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/FEIN/Mod/Prod              ')
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
      *=============================================================================================
      * Stand alone fields - unique to program
      *=============================================================================================
     d stopEdit        s               n   inz(*off)
     d cxFound         s               n   inz(*off)
     d rrn             s              9  0
     d firstBlank      s              2  0
     d xxwrk4          s              4
      *=============================================================================================
      * Data structures  and arrays
      *=============================================================================================
     d psds          esds                  extname(szq1p)
     d#ofErrors        c                   99
     d eCde            s              3  0 dim(#ofErrors) ctdata perrcd(1)
     d eMsg            s             60    dim(#ofErrors) alt(eCde)
      *=============================================================================================
      * Procedure prototypes
      *=============================================================================================
     d*entry
     d EDTWMAPP        pr
     d  @@libName                    10
     d EDTWMAPP        pi
     d  @@libName                    10

     d ProcError       pr
     d   errorNumber                  3  0 Const
     d   fieldName                         Const like(EEFLD)
     d   errorType                         Const like(EETYPE)

     d WrtToFile       pr

     DValidState       pr                  like(*in60)
     D state                               like(X@RST) const

     c/copy wcpycvedtc
      *=============================================================================================
      *  Klists
      *=============================================================================================
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr

     c                   eval      prtError = *off
     c                   eval      keyField = %editc(X@CO# :'3') + '/' +
     c                                        %editc(X@FEIN:'3') + '/' +
     c                                        %trim(%editc(X@MOD:'3')) + '/' +
     c                                        %trim(%editc(X@PROD:'3'))
     c                   eval      errCodes = *blanks
     c                   eval      stopEdit = *off
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(X@CO#) = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(1:'APCO#':'T')
     c                   endif
      *
      * Valid FEIN/Mod
      *
     c                   if        ValidFEIN(X@FEIN:X@MOD:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(2:'APFEIN/MOD':'T')
     c                   endif
      *
      * Last name must exist
      *
     c                   if        X@LSN = *blank
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(3:'APLSN':'M')
     c                   endif
      *
      * Edit license state
      *
     c                   if        ValidState(X@RST) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(4:'APRST':'T')
     c                   endif
      *
      * Prt Detail line at the end
      *
     c                   if        prtError = *on
     c                   exsr      srPrint
     c                   endif

     c                   endsr
     o/copy wcpycvedto
     p*===========================================================================
     p* ProcError - Processes Error Information
     p*===========================================================================
     p ProcError       b                   export
     d ProcError       pi
     d   errorNumber                  3  0 Const
     d   fieldName                         Const like(EEFLD)
     d   errorType                         Const like(EETYPE)

     c                   eval      x = errorNumber
     c     ' '           check     errCodes
     c                   if        %found
     c                   eval      errCodes = %trim(errCodes) +
     c                                        ', ' + %editc(x :'X')
     c                   else
     c                   eval      errCodes = %editc(x :'X')
     c                   endif
     c                   eval      errField = fieldName
     c                   eval      errType  = errorType
     c                   callp     WrtToFile
     c                   eval      prtError = *on

     p ProcError       e
     p*===========================================================================
     p* WrtToFile - Processes Error Information
     p*===========================================================================
     p WrtToFile       b                   export
     d WrtToFile       pi

     c                   eval      EEKEY  = keyField
     c                   eval      EEFLD  = errField
     c                   select
     c                   when      %parms >= 1
     c                   eval      EEFLIB = @@libName
     c                   other
     c                   eval      EEFLIB = fileLib
     c                   endsl
     c                   eval      EEFILE = fileName
     c                   eval      EECODE = eCde(x)
     c                   eval      EEDATA = eMsg(x)
     c                   eval      EETYPE = errType
     c************       eval      EESTAT + ???????
     c                   eval      ERIUSR = X@IUSR
     c                   eval      ERIPGM = X@IPGM
     c                   eval      ERIDT  = X@IDT
     c                   eval      ERITIM = X@ITM
     c                   eval      ERUSER = X@USER
     c                   eval      ERPGM  = X@PGM
     c                   eval      ERDT   = X@DT
     c                   eval      ERTIME = X@TIME
     c                   eval      EEUSER = Q1USER
     c                   eval      EEIPGM = Q1PGM
     c                   eval      EEIDT  = sysDat7
     c                   eval      EEITIM = sysTime
     c                   eval      EEUSER = Q1USER
     c                   eval      EEPGM  = Q1PGM
     c                   eval      EEDT   = sysDat7
     c                   eval      EETIME = sysTime
     c                   write     eer01

     p WrtToFile       e

    P*====================================================================
    P* ValidState - Validate State
    P*====================================================================
    P*
     PValidState       b                   export
    P*
     DValidState       PI                  like(*in60)
     D state                               like(X@RST) const

     d st              s                   like(STKEY)

     c     keySTSTL1     klist
     c                   kfld                    st

     c                   eval      st = state
     c     keySTSTL1     chain     pst01
     c                   return    %found

     PValidState       e

**   eCde/eMsg  3.0/60
001Invalid company number.
002Invalid FEIN/Mod.
003Producer # required field.
004Name not entered.
005Invalid state.
