      * (c) Copyright Fiserv Insurance Solutions, Inc., 2005-2008                                  *
      *=============================================================================================
      *  CREATED 11/11/05   C:136   PROJECT: 136WBD                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 10/29/08  C:33  Project: 114WPD
      *                -Add retrieve object library
      *=============================================================================================
      * Procedure prototypes (copy BLDOPT source for WCVEDTSHEL                                    *
      *=============================================================================================
     d fileType        s              7    inz('*FILE')
     d fileLib         s             10    inz
     d dirPDF          s           1024    inz
     d sysTimeUSA      s               t   timFmt(*HMS)
     d sysDateUSA      s               d   datFmt(*USA)
     d/copy sprRunCmd
     d/copy sprcompany
     d/copy sprdate
     d/copy sprstring
     d/copy wprusrprms
     d/copy wprgroup
     d/copy wpremplr
     d/copy wpragent
     d/copy wprproduce
     d/copy wprAR
     d/copy wprrateset
     d/copy wprregion
     d/copy wprchapter
     d/copy wproffice
     d/copy wprpremfin

     c     errKey        klist
     c                   kfld                    fileName

     c                   call      'RTVOBJLIB'
     c                   parm                    fileName
     c                   parm      '*FILE'       fileType
     c                   parm                    fileLib

     C                   callp     runCmd('OVRPRTF FILE(QSYSPRT) '
     C                                  + 'PAGESIZE(66 180) '
     C                                  + 'LPI(6) CPI(15)'
     C                                  :255)
     C                   open      qSysPrt

     c     errKey        setll     eer01
     c     errKey        reade     eer01
     c                   dow       not(%eof)
     c                   delete    eer01
     c     errKey        reade     eer01
     c                   enddo

     c                   eval      companyNm = GetCompNam(UserCo#(Q1USER))
     c                   eval      companyNm = centerFld(companyNm)
     c** **              eval      rptTitle  = centerFld(rptTitle)
     c                   eval      rptTitle  = centerFld(%trim(rptTitle)
     c                              + ' - ' + %trim(fileLib):%size(rptTitle))
     c                   eval      sysDateUSA = SystemDate
     c                   eval      sysDate = DateTo8(sysDateUSA)
     c                   eval      sysDat7 = DateTo7(sysDateUSA)
     c                   eval      sysTimeUSA = SystemTime
     c                   eval      sysTime = TimeTo6(sysTimeUSA)
     c                   move      *all'_'       line

     c                   eval      recCount = 0
     c                   read      theFile
     c                   dow       not(%eof)
     c                   add       1             recCount
     c                   exsr      srEdit
     c                   read      theFile
     c                   enddo

     c                   if        firstPrt   = *on
     c                   eval      *inof = *on
     c                   eval      prtSummary = *on
     c                   endif

     c                   if        prtSummary = *on
     c                   exsr      srSummary
     c                   endif

     C                   close     qSysPrt

     C                   exsr      CvtToPDF

     c                   seton                                        lr
      *=============================================================================================
      *  srPrint
      *=============================================================================================
     c     srPrint       begsr

     c                   if        firstPrt   = *on
     c                   eval      *inof      = *on
     c                   eval      firstPrt   = *off
     c                   eval      prtSummary = *on
     c                   endif

     c                   if        *inof
     c                   except    head
     c                   except    detHeader
     c                   eval      *inof = *off
     c                   endif

     c                   except    detail
     c                   endsr
      *=============================================================================================
      *  srSummary
      *=============================================================================================
     c     srSummary     begsr

     c     errKey        setll     eer01
     c     errKey        reade     eer01
     c                   eval      errCount  = 0
     c                   eval      errTtlCnt = 0
     c                   eval      svField = EEFLD
     c                   eval      svCode  = EECODE
     c                   dow       not(%eof)

     c                   if        firstSummPrt = *on
     c                   eval      *inof        = *on
     c                   eval      firstSummPrt = *off
     c                   endif

     c                   if        *inof
     c                   except    head
     c                   except    errHead
     c                   eval      *inof = *off
     c                   endif

     c                   if           svField <> EEFLD
     c                             or svCode  <> EECODE
     c                   eval      errDescription = eMsg(svCode)
     c                   except    errDetail
     c                   eval      svField = EEFLD
     c                   eval      svCode  = EECODE
     c                   eval      errCount  = 0
     c                   endif

     c                   add       1             errCount
     c                   add       1             errTtlCnt
     c     errKey        reade     eer01
     c                   enddo

     c                   if        *inof
     c                   except    head
     c                   except    errHead
     c                   eval      *inof = *off
     c                   endif

     c                   if        errCount <> 0
     c                   eval      errDescription = eMsg(svCode)
     c                   except    errDetail
     c                   endif

     c                   if        *inof
     c                   except    head
     c                   except    errHead
     c                   eval      *inof = *off
     c                   endif

     c                   except    errTotal

     c                   endsr
      *=============================================================================================
      *  Convert to PDF
      *=============================================================================================
     c     CvtToPDF      begsr

     C     *dtaara       Define    *lda          dirPDF
     C                   in        *dtaara

     C                   if        dirPDF <> *blanks
     C                   callp(e)  runCmd('SPLAMATIC/SPLCONVERT '
     C                                  + 'FILE(QSYSPRT) '
     C                                  + 'TOOBJ(''/' + %trim(dirPDF)
     C                                  + '/' + %trim(q1PGM) + '.pdf'') '
     C                                  + 'TRANSFORM(*PDF811)'
     C                                  :255)
     C                   endif

     c                   endsr
