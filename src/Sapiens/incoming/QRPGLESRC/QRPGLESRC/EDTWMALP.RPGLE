     hcopyright('(c) Fiserv Insurance Solutions, Inc., 2005-2008')                                 *
      *=============================================================================================
      /title EdtWMALP - Runs edits over converted file WMALP.                                      *
     h option(*nodebugio: *srcstmt)                                                                *
     h debug(*yes)                                                                                 *
      *============================================================================================*
      *                                                                                            *
      *  CREATED 11/14/05   C:136   PROJECT: 136WBD                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD                                                            *
      *                -Eliminate the need for library to be passed                                *
      *                                                                                            *
      * 12/16/05  C:136 Project: 139WAH                                                            *
      *                 Remove edit on location numbers being sequential per LQ                    *
      *=============================================================================================
     fwmalp     if   e             disk    rename(pal01 : theFile)
     fwmall     if   e           k disk    rename(pal01 : pal0l)
     fwmeml     if   e           k disk    rename(pem01 : pem0l)
     fwtgtl     if   e           k disk    rename(pgt01 : pgt0l)
     fwmexl     if   e           k disk    rename(pex01 : pex0l)
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
     d rptTitle        s             60    inz('Edit Additional Locations File -
     d                                     WMALP')
     d fileName        s                   like(EEFILE) inz('WMALP     ')
     d keyField        s             30
     d keyValues       s             30    inz('Co/Grp/Emp/Div/Loc ID         ')
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
     d rrn             s              9  0
     d firstBlank      s              2  0
     d kktbid          s                   like(GTTBID)
     d kkkey           s                   like(GTKEY)
     d xxelid          s             10  0
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
     d EDTWMALP        pr
     d  @@libName                    10
     d EDTWMALP        pi
     d  @@libName                    10

     d ValidSeq        pr             1                                         rtrn *off if bad seq

     d ProcError       pr
     d   errorNumber                  3  0 Const
     d   fieldName                         Const like(EEFLD)
     d   errorType                         Const like(EETYPE)

     d WrtToFile       pr

     c/copy wcpycvedtc
      *=============================================================================================
      *  srEdit
      *=============================================================================================
     c     srEdit        begsr

     c                   move      ALELID        xxelid
     c                   eval      prtError = *off
     c                   eval      keyField = %editc(ALCO# :'3') + '/' +
     c                                        %editc(ALFND :'3') + '/' +
     c                                        %trim(%editc(ALEMP#:'3')) + '/' +
     c                                        %trim(%editc(ALDIV :'3')) + '/' +
     c                                        %trim(%editc(xxelid :'3'))
     c                   eval      errCodes = *blanks
     c                   eval      stopEdit = *off
      *
      * Valid Company Number
      *
     c                   if        ValidCo#(ALCO#) = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(1:'ALCO#':'T')
     c                   endif
      *
      * Valid Group Number
      *
     c                   if            ValidGrp(ALFND : rrn : ALCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(2:'ALFND':'T')
     c                   endif
      *
      * Valid Employer/Division
      *
     c                   if            ValidEMP(ALEMP# : ALDIV : rrn :
     c                                          ALFND : ALCO#) = *off
     c                             and stopEdit  = *off
     c                   eval      stopEdit = *on
     c                   callp     ProcError(3:'ALEMP#/DIV':'T')
     c                   endif
      *
      * Valid Location Type Flag
      *
     c                   if        ALELTP <> 'L' and ALELTP <> 'E'
     c                   callp     ProcError(4:'ALELTP':'U')
     c                   endif
      *
      * Valid Location ID
      *
     c                   if        ALELID = *blanks
     c                   callp     ProcError(5:'ALELID':'M')
     c                   endif

      *** beg del ***** 12/16/05 ***********************************************
     c**************     if        ValidSeq = *off
     c**************     callp     ProcError(6:'ALELID':'I')
     c**************     endif
      *** end del ***** 12/16/05 ***********************************************
      *
      * Effective must be valid and >= WMEMP/EMEFD1
      *
     c     emky          klist
     c                   kfld                    ALCO#
     c                   kfld                    ALFND
     c                   kfld                    ALEMP#
     c                   kfld                    ALDIV

     c     emky          chain     pem0l
     c                   if           (DtCheck(DtMDYY(ALEFDT)) = 0
     c                             or ALEFDT < EMEFD1)
     c                             and stopEdit = *off
     c                   callp     ProcError(7:'ALEFDT':'D')
     c                   endif
      *
      * Employer Name is Required
      *
     c                   if        ALNAME = *blanks
     c                   callp     ProcError(8:'ALNAME':'M')
     c                   endif
      *
      * Valid Mailing or Physical address
      *
     c                   select
     c                   when      ALMAD1 <> *blanks and
     c                             ALMCTY <> *blanks and
     c                             ALMST  <> *blanks and
     c                             ALMZIP <> 0
     c                   when      ALPAD1 <> *blanks and
     c                             ALPCTY <> *blanks and
     c                             ALPST  <> *blanks and
     c                             ALPZIP <> 0
     c                   other
     c                   callp     ProcError(9:'ALxAD1':'M')
     c                   endsl
      *
      * Validate Type of Business field
      *
     c     gtky          klist
     c                   kfld                    ALCO#
     c                   kfld                    kktbid
     c                   kfld                    kkkey

     c                   eval      kktbid = 'BUSI'
     c                   eval      kkkey  = ALTPOB
     c     gtky          chain     pgt0l
     c                   if        not(%found) and
     c                             stopEdit = *off
     c                   callp     ProcError(10:'ALTPOB':'T')
     c                   endif
      *
      * FEIN is required.
      *
     c                   if        ALFEIN = 0
     c                   callp     ProcError(11:'ALFEIN':'M')
     c                   endif
      *
      * FEIN must match EMFEIN if 'L'ocation type of location as opposed to being an 'E'ntity type
      *
     c                   if        ALELTP = 'L' and
     c                             ALFEIN <> EMFEIN and
     c                             stopEdit = *off
     c                   callp     ProcError(12:'ALFEIN':'R')
     c                   endif
      *
      * The report to NCCI flag must be 'Y' or 'N'
      *
     c                   if        ALNCCI <> 'Y' and
     c                             ALNCCI <> 'N'
     c                   callp     ProcError(13:'ALNCCI':'U')
     c                   endif
      *
      * If required in Admin/DARTS control file, validate SIC code.
      *
     c     exky          klist
     c                   kfld                    ALCO#
     c                   kfld                    ALFND

     c     exky          chain     pex0l
     c                   if            %found
     c                             and EXSICR = 'Y'
     c                             and stopEdit = *off
     c                   eval      kktbid = 'SIC'
     c                   eval      kkkey = ALSIC
     c     gtky          chain     pgt0l
     c                   if        not(%found)
     c                   callp     ProcError(14:'ALSIC':'U')
     c                   endif
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
     p* ValidSeq = returns *on if sequence is valid.
     p*===========================================================================
     p ValidSeq        b                   export
     d ValidSeq        pi             1

     d   count         s              7  0 inz(0)
     d   total         s              7  0 inz(0)
     d   goodSeq       s               n   inz(*off)
     d   currFld       s              7  0 inz(0)
     d   saveFld       s              7  0 inz(0)
     d   kkelid        s                   like(ALELID)
     d   svelid        s                   like(ALELID)

     c     alky1         klist
     c                   kfld                    ALCO#
     c                   kfld                    ALFND
     c                   kfld                    ALEMP#
     c                   kfld                    ALDIV
     c                   kfld                    kkelid

     c     alky          klist
     c                   kfld                    ALCO#
     c                   kfld                    ALFND
     c                   kfld                    ALEMP#
     c                   kfld                    ALDIV

     c                   eval      svelid  = ALELID
     c                   eval      kkelid  = *loval
     c                   eval      count   = 0
     c                   eval      total   = 0
     c                   eval      currFld = 0
     c                   eval      saveFld = 0
     c     alky1         setll     pal0l
     c     alky          reade     pal0l
     c                   dow       not(%eof)
     c                   eval      count = count + 1
     c                   move(p)   ALELID        currFld
     c                   if        currFld > saveFld
     c                   eval      saveFld = currFld
     c                   endif
     c     alky          reade     pal0l
     c                   enddo
     c                   eval      kkelid = svelid
     c     alky1         chain     pal0l

     c                   if        count = saveFld
     c                   eval      goodSeq = *on
     c                   endif

     c                   return    goodSeq

     p ValidSeq        e
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
      *=================================================================================
      * errorCode/errorMessage Table
      *=================================================================================
**   eCde/eMsg  3.0/60
001Invalid company number.
002Invalid group number.
003Invalid Employer/Division.
004Location type flag must be 'L' or 'E'.
005Location ID is required.
006Location ID  must be sequential.
007Effective date must be >= to inception date of policy.
008Employer name is required.
009Physical or mailing address is required or incomplete.
010The type of business field is invalid.
011The FEIN is required.
012The FEIN must match WMEMP/EMFEIN.
013The NCCI flag must be 'Y' or 'N'.
014The SIC value is not valid.
