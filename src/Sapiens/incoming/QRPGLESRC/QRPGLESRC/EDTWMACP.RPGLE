     hcopyright ('(c) Copyright Fiserv Insurance Solutions, Inc., 2008')                           *
      *=============================================================================================
      /title EdtWMACP - Runs edits over converted file WMACP.                                      *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  Created 10/29/08   C:33   Project: 114WPD                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      *=============================================================================================
     fwmacp     if   e             disk    rename(pac01 : theFile)
     f                                     prefix(x@:2)
     fwtabl     if   e           k disk    include(pcs01)
     fwmapl     if   e           k disk    rename(pap01:pap00)
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
     d rptTitle        s             60    inz('Edit Agent Commission File -
     d                                     WMACP')
     d fileName        s                   like(EEFILE) inz('WMACP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Agt#/Yr                ')
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
     d EDTWMACP        pr
     d  @@libName                    10
     d EDTWMACP        pi
     d  @@libName                    10

     d ProcError       pr
     d   errorNumber                  3  0 Const
     d   fieldName                         Const like(EEFLD)
     d   errorType                         Const like(EETYPE)

     d WrtToFile       pr

     DValidCommCd      pr                  like(*in60)
     D co#                                 like(X@CO#)  const
     D code                                like(X@COCD) const

     DValidCommTp      pr                  like(*in60)
     D type                                like(X@COTP) const

     DValidProd        pr                  like(*in60)
     D co#                                 like(X@CO#)  const
     D fein                                like(X@FEIN) const
     D mod                                 like(X@MOD)  const
     D prod                                like(X@PROD) const

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
     c                                        %editc(X@FND :'3') + '/' +
     c                                        %trim(%editc(X@AGT#:'3')) + '/' +
     c                                        %trim(%editc(X@FYR:'3'))
     c                   eval      errCodes = *blanks
     c                   eval      stopEdit = *off
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(X@CO#) = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(1:'ACCO#':'T')
     c                   endif
      *
      * Valid Group Number
      *
     c                   if        ValidGrp(X@FND : rrn : X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(2:'ACFND':'T')
     c                   endif
      *
      * Valid Agent #
      *
     c                   if        ValidAgt#(X@AGT#:X@FND:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(3:'ACAGT#':'T')
     c                   endif
      *
      * Valid Group year
      *
     c                   if        ValidGrpYear(X@FND:X@FYR:rrn:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(4:'ACFYR':'T')
     c                   endif
      *
      * Valid Employer/Division information
      *
     c                   if        X@EMP# <> *zero
     c                   if        ValidEmp(X@EMP#:X@DIV:rrn:X@FND:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(5:'ACEMP#/DIV':'T')
     c                   endif
     c                   if        ValidEmpYr(X@FYR:X@EMP#:X@DIV:rrn:
     c                                        X@FND:X@CO#)= *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(6:'ACFYR':'T')
     c                   endif
     c                   endif
      *
      * Valid Producer
      *
     c                   if        ValidProd(X@CO#:X@FEIN:X@MOD:X@PROD) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(7:'ACPROD':'T')
     c                   endif
      *
      * Edit Posting date
      *
     c                   if        DtCheck(DtMDYY(X@PODT)) = 0
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(8:'ACPODT':'D')
     c                   endif
      *
      * Edit Transaction date
      *
     c                   if        DtCheck(DtMDYY(X@TRDT)) = 0
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(8:'ACTRDT':'D')
     c                   endif
      *
      * Edit Amount
      *
     c                   if        X@AMT = 0
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(9:'ACAMT':'M')
     c                   endif
      *
      * Edit Commission Code
      *
     c                   if        ValidCommCd(X@CO#:X@COCD) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(10:'ACCOCD':'T')
     c                   endif
      *
      * Edit Commission Type
      *
     c                   if        ValidCommTp(X@COTP) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(11:'ACCOTP':'T')
     c                   endif
      *
      * Edit Commission Description
      *
     c                   if        X@DESC = *blanks
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(12:'ACDESC':'M')
     c                   endif
      *
      * Valid FEIN/Mod
      *
     c                   if        ValidFEIN(X@FEIN:X@MOD:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(13:'ACFEIN/MOD':'T')
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
    P* ValidCommCd - Validate Commission Code
    P*====================================================================
    P*
     PValidCommCd      b                   export
    P*
     DValidCommCd      PI                  like(*in60)
     D co#                                 like(X@CO#)  const
     D code                                like(X@COCD) const

     D key             s                   like(CSKEY)

     C     keyWTCSP      klist
     C                   kfld                    co#
     C                   kfld                    key

     C                   eval      key = code
     C     keyWTCSP      chain     pcs01
     C                   return    %found

     PValidCommCd      e

    P*====================================================================
    P* ValidCommTp - Validate Commission Type
    P*====================================================================
    P*
     PValidCommTp      b                   export
    P*
     DValidCommTp      PI                  like(*in60)
     D type                                like(X@COTP) const

     C                   if        type <> 'A'
     C                              and type <> 'C'
     C                              and type <> 'P'
     C                              and type <> 'V'
     C                   return    *off
     C                   endif

     C                   return    *on

     PValidCommTp      e

    P*====================================================================
    P* ValidProd - Validate Producer
    P*====================================================================
    P*
     PValidProd        b                   export
    P*
     DValidProd        PI                  like(*in60)
     D co#                                 like(X@CO#)  const
     D fein                                like(X@FEIN) const
     D mod                                 like(X@MOD)  const
     D prod                                like(X@PROD) const

     C     keyWMAPP      klist
     C                   kfld                    co#
     C                   kfld                    fein
     C                   kfld                    mod
     C                   kfld                    prod

     C     keyWMAPP      chain     pap00
     C                   return    %found

     PValidProd        e

**   eCde/eMsg  3.0/60
001Invalid company number.
002Invalid group number.
003Invalid agent number.
004Invalid group year.
005Invalid employer/division.
006Invalid employer year.
007Invalid producer.
008Date invalid.
009Amount not entered.
010Invalid Commission code.
011Invalid Commission type.
012Commission description not entered.
013Invalid FEIN/Mod.
