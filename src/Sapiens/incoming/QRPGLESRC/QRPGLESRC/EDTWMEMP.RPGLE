     hcopyright('(c) Fiserv Insurance Solutions, Inc., 2005-2008')                                 *
      *=============================================================================================
      /title EdtWMEMP - Runs edits over converted file WMEMP.                                      *
     h option(*nodebugio : *srcstmt)                                                               *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  CREATED 11/14/05   C:136   PROJECT: 136WBD                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Updated for V8R1                                                           *
      *                -Alligned field edits with file layout                                      *
      *                -Eliminate the need for library to be passed                                *
      *=============================================================================================
     fwmemp     if   e             disk    rename(pem01 : theFile)
     fststl1    if   e           k disk    rename(pst01 : pst0l1)
     fwmfnl     if   e           k disk    rename(pfn01 : pfn0l)
     fwaqtl7    if   e           k disk    rename(pqt01 : pqt0l7)
     fwdxhl4    if   e           k disk    rename(pxh01 : pxh0l4)
     fwtcnl1    if   e           k disk    rename(pcn01 : pcn0l1)
     fwmexl     if   e           k disk    rename(pex01 : pex0l)
     fwmofl     if   e           k disk    rename(pof01 : pof0l)
     fwtgtl     if   e           k disk    rename(pgt01 : pgt0l)
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
     d rptTitle        s             60    inz('Edit Employer Master File -
     d                                     WMEMP')
     d fileName        s                   like(EEFILE) inz('WMEMP     ')
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
      *=============================================================================================
      * Stand alone fields - unique to program
      *=============================================================================================
     d stopEdit        s               n   inz(*off)
     d cxFound         s               n   inz(*off)
     d rrn             s              9  0
     d firstBlank      s              2  0
     d kkkey           s                   like(STKEY)
     d kktbid          s                   like(GTTBID)
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
     d EDTWMEMP        pr
     d  @@libName                    10
     d EDTWMEMP        pi
     d  @@libName                    10

     d ProcError       pr
     d   errorNumber                  3  0 Const
     d   fieldName                         Const like(EEFLD)
     d   errorType                         Const like(EETYPE)

     d WrtToFile       pr

     c/copy wcpycvedtc
      *=============================================================================================
      *  Klists
      *=============================================================================================
     c     ofky          klist
     c                   kfld                    EMCO#
     c                   kfld                    EMOFF#
     c     gtky          klist
     c                   kfld                    EMCO#
     c                   kfld                    kktbid
     c                   kfld                    kkkey
     c     stky          klist
     c                   kfld                    kkkey
     c     cnky          klist
     c                   kfld                    EMCO#
     c                   kfld                    kkkey
     c     fnky          klist
     c                   kfld                    EMCO#
     c                   kfld                    EMFND
     c     qtky          klist
     c                   kfld                    EMCO#
     c                   kfld                    EMFND
     c                   kfld                    EMEMP#
     c                   kfld                    EMDIV
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr

     c                   eval      prtError = *off
     c                   eval      keyField = %editc(EMCO# :'3') + '/' +
     c                                        %editc(EMFND :'3') + '/' +
     c                                        %trim(%editc(EMEMP#:'3')) + '/' +
     c                                        %trim(%editc(EMDIV :'3'))
     c                   eval      errCodes = *blanks
     c                   eval      stopEdit = *off
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(EMCO#) = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(1:'EMCO#':'T')
     c                   endif
      *
      * Valid Group Number
      *
     c                   if            ValidGrp(EMFND : rrn : EMCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(2:'EMFND':'T')
     c                   endif
      *
      * Valid Employer/Division
      *
     c                   if            ValidEMP(EMEMP# : EMDIV : rrn :
     c                                          EMFND : EMCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(3:'EMEMP#/DIV':'T')
     c                   endif
      *
      *  Employer name is required
      *
     c                   if        EMNAME = *blank
     c                   callp     ProcError(4:'EMNAME':'M')
     c                   endif
      *
      *  Address edits
      *
     c                   if        EMMPFL = 'M'
     c                   if        EMMAD1 = *blank
     c                   callp     ProcError(5:'EMMAD1':'M')
     c                   endif
     c                   if        EMMCTY = *blank
     c                   callp     ProcError(5:'EMMCTY':'M')
     c                   endif
     c                   if        EMMST  = *blank
     c                   callp     ProcError(5:'EMMST':'M')
     c                   else
     c                   eval      kkkey = EMMST
     c     stky          chain     pst0l1
     c                   if        not(%found)
     c                   callp     ProcError(6:'EMMST':'T')
     c                   endif
     c                   endif
     c                   if        EMMZIP = *zeros
     c                   callp     ProcError(5:'EMMZIP':'M')
     c                   endif
     c                   endif
      *
     c                   if        EMMPFL = 'P'
     c                   if        EMPAD1 = *blank
     c                   callp     ProcError(7:'EMPAD1':'M')
     c                   endif
     c                   if        EMPCTY = *blank
     c                   callp     ProcError(7:'EMPCTY':'M')
     c                   endif
     c                   if        EMPST  = *blank
     c                   callp     ProcError(7:'EMPST':'M')
     c                   else
     c                   eval      kkkey = EMPST
     c     stky          chain     pst0l1
     c                   if        not(%found)
     c                   callp     ProcError(8:'EMPST':'T')
     c                   endif
     c                   endif
     c                   if        EMPZIP = *zeros
     c                   callp     ProcError(7:'EMPZIP':'M')
     c                   endif
     c                   endif
      *
      * Inception date edits
      *
     c                   if        DtCheck(DtMDYY(EMEFD1)) = 0
     c                   callp     ProcError(9:'EMEFD1':'D')
     c                   endif
      *
      * Inception must not precede group effective date
      *
     c                   if        stopEdit = *off
     c     fnky          chain     pfn0l
     c                   if        EMEFD1 < FNEFDT
     c                   callp     ProcError(10:'EMEFD1':'D')
     c                   endif
     c                   endif
      *
      *  Cancellation date edits
      *
     c                   if        DtCheck(DtMDYY(EMCAD1)) = 0
     c                   if        EMCAD1 <> 0
     c                   callp     ProcError(11:'EMCAD1':'D')
     c                   endif
     c                   else
     c                   if        stopEdit = *off
     c                   eval      cxFound = *off
     c     qtky          setll     pqt0l7
     c     qtky          reade     pqt0l7
     c                   dow       not(%eof)
     c                   if        QTTYPE = 'C'
     c                   eval      cxFound = *on
     c                   leave
     c                   endif
     c     qtky          reade     pqt0l7
     c                   enddo
     c                   if        cxFound = *off
     c                   callp     ProcError(12:'EMCAD1':'R')
     c                   endif
     c     qtky          chain     pxh0l4
     c                   if        not(%found)
     c                   callp     ProcError(13:'EMCAD1':'R')
     c                   endif
     c                   endif
     c                   endif
      *
      * Watch list flag not valid
      *
     c                   if        EMWALS <> 'Y' and EMWALS <> 'N'
     c                             and stopEdit = *off
     c                   callp     ProcError(21:'EMWALS':'U')
     c                   endif
      *
      * County value not in table
      *
     c     empst         ifne      *blanks
     c                   movel     EMPST         xxwrk4
     c                   else
     c                   movel     EMMST         xxwrk4
     c                   endif
     c                   move      EMCNTY        xxwrk4
     c                   eval      kkkey = xxwrk4
     c     cnky          chain     pcn0l1
     c                   if            not(%found)
     c                             and stopEdit = *off
     c                   callp     ProcError(14:'EMCNTY':'T')
     c                   endif
      *
      * VIP flag not valid
      *
     c                   if        EMVIP <> 'Y' and EMVIP <> 'N'
     c                             and stopEdit = *off
     c                   callp     ProcError(21:'EMVIP':'U')
     c                   endif
      *
      * Region must match default in Admin/DARTS Control if default is provided
      *
     c     fnky          chain     pex0l
     c                   if            EXDRGN <> *zero
     c                             and EXDRGN <> EMRGN#
     c                             and stopEdit = *off
     c                   callp     ProcError(15:'EMRGN#':'T')
     c                   endif
      *
      * If required in Admin/DARTS control file, edit for FEIN.
      *
     c                   if        EXFIFG = 'Y' and EMFEIN = *zero
     c                             and stopEdit = *off
     c                   callp     ProcError(23:'EMFEIN':'M')
     c                   endif
      *
      * Edit type of business code
      *
     c                   eval      kktbid = 'BUSI'
     c                   eval      kkkey  = EMTPOB
     c     gtky          chain     pgt0l
     c                   if        not(%found) and
     c                             stopEdit = *off
     c                   callp     ProcError(16:'EMTPOB':'T')
     c                   endif
      *
      * Edit leasing ID
      *
     c                   eval      kktbid = 'NREL'
     c                   eval      kkkey  = EMLSID
     c     gtky          chain     pgt0l
     c                   if        not(%found) and
     c                             stopEdit = *off
     c                   callp     ProcError(17:'EMLSID':'T')
     c                   endif
      *
      * Edit experience rating flag
      *
     c                   eval      kktbid = 'EXRT'
     c                   eval      kkkey  = EMEXRT
     c     gtky          chain     pgt0l
     c                   if        not(%found) and
     c                             stopEdit = *off
     c                   callp     ProcError(18:'EMEXRT':'T')
     c                   endif
      *
      * If required in Admin/DARTS control file, validate SIC code.
      *
     c                   if        EXSICR = 'Y' and
     c                             stopEdit = *off
     c                   eval      kktbid = 'SIC'
     c                   eval      kkkey  = EMSIC
     c     gtky          chain     pgt0l
     c                   if        not(%found)
     c                   callp     ProcError(19:'EMSIC':'T')
     c                   endif
     c                   endif
      *
      * Validate office number
      *
     c     ofky          chain     pof0l
     c                   if            not(%found)
     c                             and stopEdit = *off
     c                   callp     ProcError(20:'EMOFF#':'R')
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

**   eCde/eMsg  3.0/60
001Invalid company number.
002Invalid group number.
003Invalid Employer/Division.
004Name is required field.
005Mailing address incomplete.
006Mailing state abbreviation is invalid.
007Physical address incomplete.
008Physical state abbreviation is invalid.
009Inception date is invalid.
010Inception date precedes group inception date.
011Cancellation date is invalid.
012Policy is cancelled. WAQTP needs cx record.
013Policy is cancelled. WDXHP needs cx record.
014County value not in table.
015Region not equal to default value in WMEXP
016Type of business code invalid.
017Employee Leasing Id invalid.
018Experience rating flag invalid.
019SIC code not in table.
020Office number is invalid.
021Watch list flag is invalid.
022VIP flag is invalid.
023FEIN not entered.
