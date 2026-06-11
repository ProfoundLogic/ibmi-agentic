     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     *=====================================================================
     H/TITLE IS810R - Queued Import (loose TIFF's)
     *=====================================================================
     * Description:  This program imports the TIFF images in a folder
     *   into the Infinity Imaging system.
     *
     *=====================================================================
     * Revisions:
     *
     * 09/17/13  C:33  Project: SY195
     *                 Account for IFS processing.
     *
     * 05/30/08 c:62 Project DR257
     *               Add Rollover processing
     *
     *=====================================================================
     fsafl1l    if   e           k disk

     *=====================================================================
     * Procedure Prototypes
     *=====================================================================
      /copy IS810RINC

     *=====================================================================
     * Constant definitions
     *=====================================================================
     ** Infinity Image API's
     d $GetDocTyp      c                   const('IS822R')
     d $GetDocIdx      c                   const('IS823R')
     d $GetBchSts      c                   const('IS820R')
     d $UpdBchSts      c                   const('IS820R')
     d $UpdDocInf      c                   const('IS824R')

     ** Program control
     d TRUE            c                   const('1')
     d FALSE           c                   const('0')
     d ERROR           c                   const(-1)
     d TIFF            c                   const('TIF')
     d QDLS            c                   const('QDLS')
     d PCFILE          c                   const('PCFILE')

     *=====================================================================
     * Data Structure, Table & Array definitions
     *=====================================================================
     ** Program information
     d psds          esds                  extname(szq1p)
     d docarrx         ds            12
     d docarr                         1    dim(12)

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6    overlay(dsdttm)
     D  dsdate                        8    overlay(dsdttm:7)

     *=====================================================================
     * Standalone field definitions
     *=====================================================================

     * Work Station information
     d @WrkStnID       s              2a                                        Work Station ID
     d @DocType        s              8a                                        Image drive
     d @RtnCode        s              1a                                        Return code

     * DARTS information
     d DARTSeq#        s              5a                                        DARTS Sequence #
     d DARTKey         s             40a                                        DARTS Key (index)
     d rmkCode         s              5a                                        DARTS Remark Code
     d secLvl          s              3a                                        Security level

     * Document information
     d docType         s              8a                                        Document Type
     d docSeq#         s              6a                                        Document Type
     d docSeq2         s              6  0                                      Document Type
     d docDesc         s             40a                                        Document description
     d fileSystem      s             10a                                        File system
     d suffix          s              3a                                        Document suffix
     d delim           s              3a                                        Document delimiter
     d newDocId        s             12a                                        Built document ID
     d bltDocId        s             12a                                        Built document ID
     d imgFolder       s              8a                                        Image folder
     d imgSubFlr       s             12a                                        Image subfolder
     d indexQue        s             10a                                        Index queue
     d indexPgm        s             10a                                        Index program
     d #Bytes          s              5a                                        # bytes in image
     d importPgm       s             10a
     d importDir       s             12a

     * Batch information
     d #DocsInBch      s              5a                                        # docs in batch
     d batchDesc       s             40a                                        Batch description
     d batchSts        s              2a                                        Batch Status
     d pBatchSts       s              2a                                        Batch Status
     d svBchSts        s              2a                                        Batch Status
     d batchSeq#       s              4a                                        Batch sequence #
     d docBchSeq#      s              4a                                        Batch sequence #
     d bltBatchID      s             12a                                        Built batch ID
     d bchErrDesc      s             52a                                        Batch error desc.
     d est#docs        s              5a                                        # docs (estimated)
     d act#docs        s              5a                                        # docs (actual)
     d estNumDoc       s              5  0
     d actNumDoc       s              5  0
     d est#pages       s              5a                                        # pages (estimated)
     d act#pages       s              5a                                        # pages (actual)
     d estNumPag       s              5  0
     d actNumPag       s              5  0
     d #pagPerDoc      s              3a                                        # pages per document
     d #pagThisDc      s              4a                                        # pages per document
     d numPagDoc       s              3  0
     d rtnBchID        s              4  0                                      Returned batch ID
     d docSepFlag      s              1a                                        Using doc separators
     d scannedBch      s             12a                                        Scanned document

     * 'Scan' information
     d scanRep         s             10a                                        Scan rep
     d scanDate8a      s              8a                                        Date scanned
     d scanTime6a      s              6a                                        Time scanned

     * Program control
     d running         s              1a                                        Program state
     d newPath         s             21a                                        Generic return code
     d rtnCode         s              1a                                        Generic return code
     d isTiff          s              1a
     d firstTime       s              1a
     d count           s              2  0
     d newDocFlr       s             80a
     d alphSeq#        s              5a
     d maxDocs         s              5  0
     d oldDir          s             80a
     d madeBatch       s              1a

     d loval           s             12a    inz(*loval)

     *=====================================================================
     * KList definitions
     *=====================================================================
     c     docKey        klist
     c                   kfld                    loval

     *=====================================================================
     * PList definitions
     *=====================================================================
     * Entry parms
     c     *entry        plist
     c                   parm                    @WrkStnID
     c                   parm                    @DocType
     c                   parm                    @RtnCode

     ** Infinity Image API parms
     c     pGetDocTyp    PList
     c                   parm                    @DocType
     c                   parm                    rmkCode
     c                   parm                    fileSystem
     c                   parm                    delim
     c                   parm                    docDesc
     c                   parm                    indexQue
     c                   parm                    #docsInBch
     c                   parm                    secLvl
     c                   parm                    indexPgm
     c                   parm                    batchSts
     c                   parm                    importPgm
     c                   parm                    importDir
     c                   parm      *blanks       rtnCode

     c     pGetDocIdx    PList
     c                   parm                    @WrkStnID
     c                   parm                    docSeq#
     c                   parm                    suffix
     c                   parm                    batchSeq#
     c                   parm                    bltDocID
     c                   parm                    bltBatchID
     c                   parm                    imgFolder
     c                   parm                    imgSubFlr
     c                   parm                    newPath
     c                   parm      *blanks       rtnCode

     c     pUpdBchSts    PList
     c                   parm                    bltBatchID
     c                   parm                    pBatchSts
     c                   parm                    @docType
     c                   parm      q1user        scanRep
     c                   parm                    scanDate8a
     c                   parm                    scanTime6a
     c                   parm                    est#docs
     c                   parm                    act#docs
     c                   parm                    est#pages
     c                   parm                    act#pages
     c                   parm                    #pagPerDoc
     c                   parm      *blank        docSepFlag
     c                   parm                    q1pgm
     c                   parm                    bchErrDesc
     c                   parm      docDesc       batchDesc
     c                   parm      *blank        rtnCode

     c     pUpdDocInf    PList
     c                   parm                    @wrkStnID
     c                   parm                    docSeq#
     c                   parm                    docBchSeq#
     c                   parm                    imgFolder
     c                   parm                    imgSubFlr
     c                   parm                    act#docs
     c                   parm                    @docType
     c                   parm                    newDocId
     c                   parm                    #bytes
     c                   parm                    scanDate8a
     c                   parm                    scanTime6a
     c                   parm                    bltBatchId
     c                   parm                    #pagThisDc
     c                   parm      *blanks       DARTKey
     c                   parm      '00000'       DARTSeq#
     c                   parm                    rtnCode

     *=====================================================================
     * Main
     *=====================================================================

     c                   exsr      $GetNumDoc

     c     *loval        setll     docdtl
     c                   read      docdtl                                 60

     ** Do while entries exist
     c                   dow       NOT *in60
     c                             AND actNumDoc < (maxDocs)

     c                   eval      isTiff = FALSE
     c***  Begin Chg  ***  09/17/13  **************************************
     c**********         if        doctyp = PCFILE
      /Free
       // docTyp comes from SAFL1L
       //  If processing QDLS, = 'PCFILE'. If processing IFS, = '*STMF'
       if docTyp = PCFILE or docTyp = '*STMF';
      /End-free
     c***  End   Chg  ***  09/17/13  **************************************

     ** Check to see if this has a .tif extension
     c                   movel     docnam        docarrx
     c                   do        9             count

     c                   if        docarr(count) = '.'
     c                   if        %subst(docnam:count+1:3) = TIFF
     c                   eval      isTiff = TRUE
     c                   endif
     c                   endif

     c                   enddo

     c                   if        isTiff = TRUE
     C                   time                    dsdttm
     c                   move      dsdate        scanDate8a
     c                   move      dstime        scanTime6a

     c                   if        firstTime = TRUE
     c                   eval      firstTime = FALSE
     c                   eval      madeBatch = TRUE
     c                   eval      pBatchSts = 'SC'
     c                   callb     $UpdBchSts    pUpdBchSts
     c                   endif
    
     ** Copy the doc to a built name/folder
     c                   callp     $CpyDlo(docarrx:oldDir:newDocId:newDocFlr:
     c                                     rtnCode)
     *** 2/22/07 *** add ***************************************************************
     ***   If return code is not blank, an error occured when processing the copy, therefore
     ***    don't create a record. bypass this request.
     ***
     c     rtnCode       ifeq      *blanks
     *** 2/22/07 *** END add ***************************************************************


     ** Update the document info record
     c                   callb     $UpdDocInf    pUpdDocInf
     c*** begin add *** 05/30/08 *******************************************
     c                   if        rtnCode='S'
     c                   eval      @RtnCode='S'
     c                   seton                                        lr
     c                   endif
     c*** end   add *** 05/30/08 *******************************************
     ** Get Doc index info
     c                   callb     $GetDocIdx    pGetDocIdx
     c*** begin add *** 05/30/08 *******************************************
     c                   if        rtnCode='S'
     c                   eval      @RtnCode='S'
     c                   endif
     c*** end   add *** 05/30/08 *******************************************
     c                   eval      newDocId = bltDocId + '.' + TIFF
     c                   eval      newDocFlr = %trim(imgFolder) + '/' +
     c                               %trim(imgSubFlr)

     ** Delete the original document
     c                   callp     $DltDlo(docnam:oldDir:rtnCode)

     ** Increment total number of docs processed
     c                   add       1             actNumDoc

     ** Increment total number of pages
     c                   add       numPagDoc     actNumPag

     *** 2/22/07 ***  add ***************************************************************
     C                   endif                                                   end rtncod =blanks
     *** 2/22/07 ***  END add ***************************************************************

     c                   endif
     c                   endif

     c                   read      docdtl                                 60
     c                   enddo

     ** If a batch was made, do stuff
     c                   if        madeBatch = TRUE

     c                   move      actNumDoc     act#docs
     c                   move      actNumPag     act#pages

     ** Update the batch status record
     C                   time                    dsdttm
     c                   move      dsdate        scanDate8a
     c                   move      dstime        scanTime6a
     c                   eval      pBatchSts = svBchSts
     c                   callb     $UpdBchSts    pUpdBchSts

     ** Increment the index info
     c                   callp     $IncDocIdx(@WrkStnId:'0':'1':@RtnCode)

     c                   endif

     ** End the program
     c                   eval      *inlr = *on
     *=====================================================================




     *=====================================================================
     * *inzsr - Initialization subroutine
     *=====================================================================
     c     *inzsr        begsr

     ** Get Doc type info
     c                   callb     $GetDocTyp    pGetDocTyp
     c                   move      delim         numPagDoc
     c                   move      numPagDoc     #pagPerDoc
     c                   move      numPagDoc     #pagThisDc
     c                   move      #docsInBch    maxDocs
     c                   if        maxDocs = 0
     c                   eval      maxDocs = *hival
     c                   sub       1             maxDocs
     c                   endif
     c                   eval      oldDir = importDir
     c                   eval      svBchSts = batchSts


     ** Get Doc index info
     c                   callb     $GetDocIdx    pGetDocIdx
     c*** begin add *** 05/30/08 *******************************************
     c                   if        rtnCode='S'
     c                   eval      @RtnCode='S'
     c                   endif
     c*** end   add *** 05/30/08 *******************************************
     c                   eval      newDocId = bltDocId + '.' + TIFF
     c                   eval      newDocFlr = %trim(imgFolder) + '/' +
     c                               %trim(imgSubFlr)
     c                   eval      docBchSeq# = batchSeq#

     c                   eval      firstTime = TRUE
     c                   eval      isTiff = FALSE
     c                   eval      madeBatch = FALSE

     c     endinzsr      endsr
     *=====================================================================




     *=====================================================================
     * $SetAttr - Set the attribute tables
     *=====================================================================
     c     $GetNumDoc    begsr

     c                   read      docdtl                                 60

     ** Do while entries exist
     c                   dow       NOT *in60
     c                             AND estNumDoc < maxDocs

     c                   eval      isTiff = FALSE
     c                   if        doctyp = PCFILE

     ** Check to see if this has a .tif extension
     c                   movel     docnam        docarrx
     c                   do        9             count

     c                   if        docarr(count) = '.'
     c                   if        %subst(docnam:count+1:3) = TIFF
     c                   eval      isTiff = TRUE
     c                   endif
     c                   endif

     c                   enddo

     c                   if        isTiff = TRUE
     ** Increment total number of docs processed
     c                   add       1             estNumDoc

     ** Increment total number of pages
     c                   add       numPagDoc     estNumPag

     c                   endif
     c                   endif

     c                   read      docdtl                                 60
     c                   enddo

     c                   move      estNumDoc     est#docs
     c                   move      estNumPag     est#pages

     c     endGetNumD    endsr
     *=====================================================================
