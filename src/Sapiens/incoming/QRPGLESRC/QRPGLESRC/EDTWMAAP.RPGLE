     hcopyright ('(c) Copyright Fiserv Insurance Solutions, Inc., 2008')                           *
      *=============================================================================================
      /title EdtWMAAP - Runs edits over converted file WMAAP.                                      *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  Created 10/29/08   C:33   Project: 114WPD                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      *=============================================================================================
     fwmaap     if   e             disk    rename(paa01 : theFile)
     f                                     prefix(x@:2)
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
     d rptTitle        s             60    inz('Edit Agent Assignment File -
     d                                     WMAAP')
     d fileName        s                   like(EEFILE) inz('WMAAP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Agt#                   ')
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
     d EDTWMAAP        pr
     d  @@libName                    10
     d EDTWMAAP        pi
     d  @@libName                    10

     d ProcError       pr
     d   errorNumber                  3  0 Const
     d   fieldName                         Const like(EEFLD)
     d   errorType                         Const like(EETYPE)

     d WrtToFile       pr

     d/copy wcpycvedtc
      *=============================================================================================
      *  Klists
      *=============================================================================================
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr

     c                   eval      prtError = *off
     c                   eval      keyField = %editc(X@CO# :'3') + '/' +
     c                                        %editc(X@FND :'3') + '/' +
     c                                        %trim(%editc(X@AGT#:'3'))
     c                   eval      errCodes = *blanks
     c                   eval      stopEdit = *off
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(X@CO#) = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(1:'AACO#':'T')
     c                   endif
      *
      * Valid Group Number
      *
     c                   if        ValidGrp(X@FND : rrn : X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(2:'AAFND':'T')
     c                   endif
      *
      * Valid FEIN/Mod
      *
     c                   if        ValidFEIN(X@FEIN:X@MOD:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(3:'AAFEIN/MOD':'T')
     c                   endif
      *
      * Agent # must exist
      *
     c                   if        X@AGT# = *zero
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(4:'AAAGT#':'M')
     c                   endif
      *
      * Edit Commission flag
      *
     c                   if        X@CMFL <> 'Y' and X@CMFL <> 'N'
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(5:'AACMFL':'U')
     c                   endif
      *
      * Edit Prospect flag
      *
     c                   if        X@PRFL <> 'Y' and X@PRFL <> 'N'
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(6:'AAPRFL':'U')
     c                   endif
      *
      * Edit Mail flag
      *
     c                   if        X@MLFL <> 'Y' and X@MLFL <> 'N'
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(7:'AAMLFL':'U')
     c                   endif
      *
      * Agent name must exist
      *
     c                   if        X@NAME = *blank
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(8:'AANAME':'M')
     c                   endif
      *
      * Prt Detail line at the end
      *
     c                   if        prtError = *on
     c                   exsr      srPrint
     c                   endif

     c                   endsr
     O/copy wcpycvedto
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
     c                   eval      EEFLIB = @@libName
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

**   eCde/eMsg  3.0/60
001Invalid company number.
002Invalid group number.
003Invalid FEIN/Mod.
004Agent # required field.
005Commission flag incorrect.
006Prospect flag incorrect.
007Mail flag incorrect.
008Name not entered.
