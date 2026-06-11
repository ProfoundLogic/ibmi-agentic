     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=============================================================================================
      /title EdtWAQTP - Runs edits over converted file WAQTP.                                      *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  CREATED 11/14/05   C:136   PROJECT: 136WBD                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 05/28/09  C:33  Project: 133WGV                                                            *
      *                 Use a new file for validation.                                             *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Updates for V8R1                                                           *
      *                -Change order of edits to match file layout                                 *
      *                -Eliminate the need for library to be passed                                *
      *=============================================================================================
     fwaqtp     if   e             disk    rename(pqt01 : theFile)
     fwmeml     if a e           k disk    rename(pem01 : pem0l)
     fwmemlq    if   e           k disk    rename(pem01 : pem0q) usropn
     f***  Begin Add  ***  05/28/09  **************************************
     fwtqsl1    if   e           k disk
     f***  End   Add  ***  05/28/09  **************************************
     fedterl    uf a e           k disk
     fqsysprt   o    f  180        printer oflind(*inof)         usropn
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
     d rptTitle        s             60    inz('Edit Agt Quote System File -
     d                                     WAQTP')
     d fileName        s                   like(EEFILE) inz('WAQTP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Emp/Div                ')
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
     d env             s              4
     d qteLib          s             10
      *=============================================================================================
      * Stand alone fields - unique to program
      *=============================================================================================
     d stopEdit        s               n   inz(*off)
     d rrn             s              9  0
     d firstBlank      s              2  0
      *=============================================================================================
      * Data structures  and arrays
      *=============================================================================================
     d psds          esds                  extname(szq1p)

     D #ofTypes        c                   15
     D qteTypes        s              1    dim(#ofTypes) ctdata perrcd(1)
     D  qteDesc        s             25    dim(#ofTypes) alt(QteTypes)

     D #ofErrors       c                   99
     d eCde            s              3  0 dim(#ofErrors) ctdata perrcd(1)
     d eMsg            s             60    dim(#ofErrors) alt(eCde)
      *=============================================================================================
      * Procedure prototypes
      *=============================================================================================
     d*entry
     d EDTWAQTP        pr
     d  @@libName                    10
     d EDTWAQTP        pi
     d  @@libName                    10

     d ProcError       pr
     d   errorNumber                  3  0 Const
     d   fieldName                         Const like(EEFLD)
     d   errorType                         Const like(EETYPE)

     d WrtToFile       pr

     D ValidQteEmp     pr             1
     D  emp#                               like(EMEMP#)                  const
     D  div                                like(EMDIV)  options(*nopass) const
     D  group                              like(EMFND)  options(*nopass) const
     D  co#                                like(EMCO#)  options(*nopass) const

     d ValidQTTYPE     pr             1n

     c/copy wcpycvedtc
      *=============================================================================================
      *  Klists
      *=============================================================================================
     c     emky          klist
     c                   kfld                    QTCO#
     c                   kfld                    QTFND
     c                   kfld                    QTEMP#
     c                   kfld                    QTDIV
      *=============================================================================================
      *  Initialize
      *=============================================================================================
     c     *inzsr        begsr
     c*
     c                   call      'RTVQTELIB'
     c                   parm      '*QTE'        env
     c                   parm                    qteLib
     c*
     C                   callp     runCmd('OVRDBF FILE(WMEMLQ) '
     C                                  + 'TOFILE(' + %trim(qteLib)
     C                                  + '/WMEMLQ) '
     C                                  + 'MBR(*FIRST)':255)
     c*
     c                   open      WMEMLQ
     c*
     c                   endsr
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr

     c                   eval      prtError = *off
     c                   eval      keyField = %editc(QTCO# :'3') + '/' +
     c                                        %editc(QTFND :'3') + '/' +
     c                                        %trim(%editc(QTEMP#:'3')) + '/' +
     c                                        %trim(%editc(QTDIV :'3'))
     c                   eval      errCodes = *blanks
     c                   eval      stopEdit = *off
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(QTCO#) = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(1:'QTCO#':'T')
     c                   endif
      *
      * Valid Group Number
      *
     c                   if            ValidGrp(QTFND : rrn : QTCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(2:'QTFND':'T')
     c                   endif
      *
      * Valid Agent #
      *
     c                   if        ValidAgt#(QTAGT#:QTFND:QTCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(7:'QTAGT#':'T')
     c                   endif
      *
      * Name should be entered
      *
     c                   if        QTNAME = *blanks
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(8:'QTNAME':'M')
     c                   endif
      *
      * Type needs to be entered and valid
      *
     c                   if        ValidQTTYPE = *off
     c                             and stopEdit  = *off
     c                   callp     ProcError(9:'QTTYPE':'T')
     c                   endif
      *
      *  Valid  type change date
      *
     c                   if        DtCheck(DtMDYY(QTWKDT)) = 0
     c                   callp     ProcError(4:'QTWKDT':'D')
     c                   endif
      *
      *  Valid  effective date
      *
     c                   if        DtCheck(DtMDYY(QTEFDT)) = 0
     c                   callp     ProcError(4:'QTEFDT':'D')
     c                   endif
      *
      * Valid Employer/Division
      *
     c                   if            ValidEMP(QTEMP# : QTDIV : rrn :
     c                                          QTFND : QTCO#) = *off
     c                             and ValidQteEMP(QTEMP# : QTDIV :
     c                                             QTFND : QTCO#) = *off
     c                             and stopEdit  = *off
     c                   callp     ProcError(3:'QTEMP#/DIV':'T')
     c                   endif
      *
      *  Valid  week ending date
      *
     c**********         if        DtCheck(DtMDYY(QTNBDT)) = 0
     c                   if        QTSTAT <> 'Q' and
     c                             DtCheck(DtMDYY(QTNBDT)) = 0
     c                   callp     ProcError(4:'QTNBDT':'D')
     c                   endif
      *
      *  The 'W' record dates must equal the inception date of the policy
      *
     c     emky          chain     pem0l
     c                   if            QTTYPE = 'W'
     c                             and QTEFDT <> EMEFD1
     c                             and QTNBDT <> EMEFD1
     c                             and stopEdit = *off
     c                   callp     ProcError(5:'EFDT/NBDT':'R')
     c                   endif
      *
      *  The 'C' record dates must equal the inception date of the policy
      *
     c                   if            QTTYPE = 'C'
     c                             and QTEFDT <> EMCAD1
     c                             and QTNBDT <> EMCAD1
     c                             and stopEdit = *off
     c                   callp     ProcError(6:'EFDT/NBDT':'R')
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

    P*====================================================================
    P* ValidQteEmp - Valid employer/division in Quote
    P*====================================================================
    P*
     PValidQteEmp      b                   export
    P*
     DValidQteEmp      PI             1
     D emp#                                like(EMEMP#)                  const
     D div                                 like(EMDIV)  options(*nopass) const
     D group                               like(EMFND)  options(*nopass) const
     D co#                                 like(EMCO#)  options(*nopass) const

     D division        s                   like(EMDIV)
     D group#          s                   like(EMFND)
     D company         s                   like(EMCO#)

     C     emky          klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    emp#
     C*
     C     emky_1        klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    emp#
     C                   kfld                    division

     C                   eval      company = UserCo#(q1user)
     C                   eval      group#  = UserGrp(q1user)
     C*
     C                   if        %parms >= 3
     C                   eval      group# = group
     C                   endif
     C*
     C                   if        %parms >= 4
     C                   eval      company = co#
     C                   endif
     C*
     C                   if        %parms >= 2
     C                   eval      division = div
     C     emky_1        chain     pem0q                              60
     C                   else
     C     emky          chain     pem0q                              60
     C                   endif
     C*
     C                   return    not *in60
     C*
     PValidQteEmp      e

     p*===========================================================================
     p* ValidQTTYPE - Validate QTTYPE
     p*===========================================================================
     p ValidQTTYPE     b                   export
     d ValidQTTYPE     pi             1n

     d svIn99          s                   like(*in99)

     C***  Begin Add  ***  05/28/09  **************************************
     C     keyWTQSL1     klist
     C                   kfld                    QTCO#
     C                   kfld                    QTTYPE
     C*
     C     keyWTQSL1     chain     pqs01
     C*
     C                   if        not %found
     C***  End   Add  ***  05/28/09  **************************************
     C                   eval      svIn99 = *in99
     C     QTTYPE        lookup    qteTypes                               99
     C                   eval      *in99 = svIn99
     C***  Begin Add  ***  05/28/09  **************************************
     C                   endif
     C***  End   Add  ***  05/28/09  **************************************

     c                   return    %found

     p ValidQTTYPE     e
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
     c                   eval      ERIUSR = EMIUSR
     c                   eval      ERIPGM = EMIPGM
     c                   eval      ERIDT  = EMIDT
     c                   eval      ERITIM = EMITIM
     c                   eval      ERUSER = EMUSER
     c                   eval      ERPGM  = EMPGM
     c                   eval      ERDT   = EMDT
     c                   eval      ERTIME = EMTIME
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

** Quote Types
PPending
QQuoted
WWritten
CCancelled
LLost
DDeclined
RReinstated
BBound
MBound w/Money
TTransfer In
XTransfer Out
FRenewal Pending
GRenewal Quote
USubmitted
SClosed
**   eCde/eMsg  3.0/60
001Invalid company number.
002Invalid group number.
003Invalid Employer/Division.
004The date is invalid or missing.
005Not equal to inception date of policy.
006Not equal to cancellation date of policy.
007Invalid agent number.
008Account name is missing.
009Invalid quote type.
