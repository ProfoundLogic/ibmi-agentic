     hcopyright ('(c) Copyright Fiserv Insurance Solutions, Inc., 2008')                           *
      *=============================================================================================
      /title EdtWMAHP - Runs edits over converted file WMAHP.                                      *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  Created 10/29/08   C:33   Project: 114WPD                                                 *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      *=============================================================================================
     fwmahp     if   e             disk    rename(pah01 : theFile)
     f                                     prefix(x@:2)
     fwmarl     if   e           k disk    rename(par01:par00)
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
     d rptTitle        s             60    inz('Edit Agent History File -
     d                                     WMAHP')
     d fileName        s                   like(EEFILE) inz('WMAHP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Emp#/Div/Yr            ')
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
     d EDTWMAHP        pr
     d  @@libName                    10
     d EDTWMAHP        pi
     d  @@libName                    10

     d ProcError       pr
     d   errorNumber                  3  0 Const
     d   fieldName                         Const like(EEFLD)
     d   errorType                         Const like(EETYPE)

     d WrtToFile       pr

     DValidProd        pr                  like(*in60)
     D co#                                 like(X@CO#)  const
     D fnd                                 like(X@FND)  const
     D agt#                                like(X@AGT#) const
     D prod                                like(X@PROD) const

     DValidRate        pr                  like(*in60)
     D co#                                 like(X@CO#)  const
     D fnd                                 like(X@FND)  const
     D rt#                                 like(X@RT#)  const
     D efdt                                like(X@EFDT) const

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
     c                                        %trim(%editc(X@EMP#:'3')) + '/' +
     c                                        %trim(%editc(X@DIV:'3')) + '/' +
     c                                        %trim(%editc(X@FYR:'3'))
     c                   eval      errCodes = *blanks
     c                   eval      stopEdit = *off
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(X@CO#) = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(1:'AHCO#':'T')
     c                   endif
      *
      * Valid Group Number
      *
     c                   if        ValidGrp(X@FND : rrn : X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(2:'AHFND':'T')
     c                   endif
      *
      * Valid Employer/Division information
      *
     c                   if        ValidEmp(X@EMP#:X@DIV:rrn:X@FND:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(3:'AHEMP#/DIV':'T')
     c                   endif
     c                   if        ValidEmpYr(X@FYR:X@EMP#:X@DIV:rrn:
     c                                        X@FND:X@CO#)= *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(4:'AHFYR':'T')
     c                   endif
      *
      * Edit Effective date
      *
     c                   if        DtCheck(DtMDYY(X@EFDT)) = 0
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(5:'AHEFDT':'D')
     c                   endif
      *
      * Valid Agent
      *
     c                   if        ValidAgt#(X@AGT#:X@FND:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(6:'AHAGT#':'T')
     c                   endif
      *
      * Valid Rate
      *
     c                   if        ValidRate(X@CO#:X@FND:X@RT#:X@EFDT) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(7:'AHRT#':'T')
     c                   endif
      *
      * Valid Producer
      *
     c                   if        ValidProd(X@CO#:X@FND:X@AGT#:X@PROD) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(8:'AHPROD':'T')
     c                   endif
      *
      * Secondary
     c                   if        X@SAGT <> *zero
      *  Valid Agent
     c                   if        ValidAgt#(X@SAGT:X@FND:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(6:'AHSAGT':'T')
     c                   endif
      *
      *  Valid Rate
     c                   if        ValidRate(X@CO#:X@FND:X@SRT#:X@EFDT) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(7:'AHSRT#':'T')
     c                   endif
      *
      *  Valid Producer
     c                   if        ValidProd(X@CO#:X@FND:X@SAGT:X@SPRD) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(8:'AHSPRD':'T')
     c                   endif
     c*
     c                   endif
      *
      * Agent of Record
     c                   if        X@AGRC <> *zero
      *  Valid Agent
     c                   if        ValidAgt#(X@AGRC:X@FND:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(6:'AHAGRC':'T')
     c                   endif
      *
      *  Valid Rate
     c                   if        ValidRate(X@CO#:X@FND:X@RTRC:X@EFDT) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(7:'AHRTRC':'T')
     c                   endif
      *
      *  Valid Producer
     c                   if        ValidProd(X@CO#:X@FND:X@AGRC:X@PRRC) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(8:'AHPRRC':'T')
     c                   endif
      *
      *  Secondary
     c                   if        X@2ARC <> *zero
      *   Valid Agent
     c                   if        ValidAgt#(X@2ARC:X@FND:X@CO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(6:'AH2ARC':'T')
     c                   endif
      *
      *   Valid Rate
     c                   if        ValidRate(X@CO#:X@FND:X@2RRC:X@EFDT) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(7:'AH2RRC':'T')
     c                   endif
      *
      *   Valid Producer
     c                   if        ValidProd(X@CO#:X@FND:X@2ARC:X@2PRC) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(8:'AH2PRC':'T')
     c                   endif
     c*
     c                   endif
     c*
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
    P* ValidProd - Validate Producer
    P*====================================================================
    P*
     PValidProd        b                   export
    P*
     DValidProd        PI                  like(*in60)
     D co#                                 like(X@CO#)  const
     D fnd                                 like(X@FND)  const
     D agt#                                like(X@AGT#) const
     D prod                                like(X@PROD) const

     D fein            s                   like(APFEIN) inz
     D mod             s                   like(APMOD)  inz

     C     keyWMAPP      klist
     C                   kfld                    co#
     C                   kfld                    fein
     C                   kfld                    mod
     C                   kfld                    prod

     C                   eval      fein = GetAgtFein(X@CO#:X@FND:X@AGT#:mod)
     C     keyWMAPP      chain     pap00
     C                   return    %found

     PValidProd        e

    P*====================================================================
    P* ValidRate - Validate Rate #
    P*====================================================================
    P*
     PValidRate        b                   export
    P*
     DValidRate        PI                  like(*in60)
     D co#                                 like(X@CO#)  const
     D fnd                                 like(X@FND)  const
     D rt#                                 like(X@RT#)  const
     D efdt                                like(X@EFDT) const

     C     keyWMARL      klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    rt#
     C                   kfld                    efdt

     C     key1WMARL     klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    rt#

     C     keyWMARL      setgt     par00
     C     key1WMARL     readpe    par00
     C                   if        %eof
     C                   return    *off
     C                   endif
     C                   return    *on

     PValidRate        e

**   eCde/eMsg  3.0/60
001Invalid company number.
002Invalid group number.
003Invalid employer/division.
004Invalid employer year.
005Invalid date.
006Invalid agent #.
007Invalid rate.
008Invalid producer.
