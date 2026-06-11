     *=====================================================================
     *  (c) Copyright Progressive Data Solutions, Inc., 2002
     *=====================================================================
    h/TITLE IS828R - CrtDocName
     *=====================================================================
     *
     * Description:
     *  Create document name and folder/sub-folder for documents imported
     *  into Darts via Visual Darts.
     *
     * Created   : 02/20/02
     * Programmer: C:128
     * Project   : 81WSZ
     *
     *=====================================================================
     * Revisions:
     *
     * 07/29/03 c:62 Project DRMNT
     *               Unlock the record
     *
     *=====================================================================
     * Document Index Control File
     Fimdil     uf   e           k disk
    
     * Folder Balance File
     Fimfbl     uf   e           k disk
    
     *=====================================================================
     * Procedure Prototypes
     *=====================================================================
      /copy IS820RPR
      /copy cvt2rPR

     *=====================================================================
     * Table, Data Structure & Array Definitions
     *=====================================================================

     ** Program information
     D psds          esds                  extname(szq1p)

     *=====================================================================
     * Stand Alone Field Definitions
     *=====================================================================
     D @BatchId        s             12a
     D @BatchSeq       s              4a
     D @Folder         s              8a
     D @SubFldr        s             12a
     D @DocId          s             12a
     D @RtnCode        s              1a
     D @PRE            s             13a
     D @DocSeq#        s              6a
     D @Suffix         s              3a
     D @NewPath        s             21a
     D @WrkStnId       s              2a


     *=====================================================================
     * PList Definitions
     *=====================================================================
     ** ENTRY PARAMETERS
     C     *entry        plist
     C                   parm                    @WrkStnId
     C                   parm                    @DocSeq#
     C                   parm                    @Suffix
     C                   parm                    @BatchSeq
     C                   parm                    @DocId
     C                   parm                    @BatchId
     C                   parm                    @Folder
     C                   parm                    @SubFldr
     C                   parm                    @NewPath
     C                   parm                    @RtnCode
     C                   parm                    @PRE


     *=====================================================================
     * Main
     *=====================================================================
     c                   callp     GetDocIdx(@WrkStnId : @DocSeq# : @Suffix :
     c                                       @BatchSeq : @DocId : @BatchId :
     c                                       @Folder : @SubFldr : @NewPath :
     c                                       @RtnCode : @PRE )

     ** End main
     c                   eval      *inlr = *ON
     *=====================================================================




     *=====================================================================
     * GetDocIdx - Get the document index information
     *=====================================================================
     p GetDocIdx       B                   export

     *** Procedure Interface ***
     d GetDocIdx       PI
     d  @WrkStnId                     2a
     d  @DocSeq#                      6a
     d  @Suffix                       3a
     d  @BatchSeq                     4a
     d  @DocId                       12a
     d  @BatchId                     12a
     d  @Folder                       8a
     d  @SubFldr                     12a
     d  @NewPath                     21a
     d  @RtnCode                      1a
     d  @PRE                         13a

     *** Locals ***
     d xDate           s               d
     d xTime           s               t
     d charDate        s              6a
     d numDate         s              8  0
     d kLastDate       s              6  0
     d kLastTime       s              6  0
     d xPrefix         s              9a
     d xSuffix         s              3a
     d #Suffix         s              3  0
     d errorCode       s              1a

     *** KLists ***
     * Document Index Control file
     c     diky1         klist
     c                   kfld                    @WrkStnId

     * Folder Balance file
     c     fbky1         klist
     c                   kfld                    kLastDate
     c                   kfld                    kLastTime

     * Procedure Definition *
     c                   exsr      srinit

     c     diky1         chain     pdi01                              60

     c                   if        NOT *in60
     * Update Document Index File *
     c                   Eval      diseq# = diseq# + 1
     c                   move      diseq#        @DocSeq#
     C                   movel     disufx        @Suffix
     c                   move      dibchs        @BatchSeq
     c
     c                   eval      @DocId = diwsid + @DocSeq# + '.' + @Suffix
     c                   eval      @BatchId = diwsid + charDate + @BatchSeq
    
     c                   movel     q1user        dilusr
     c                   movel     q1pgm         dilpgm
     c     *hms          move      xTime         diltim
     c     *ymd          move      xDate         dildt
    
     c                   update    pdi01

     c                   else
     * ERROR - Document Index record not found.
     c                   clear                   @DocSeq#
     c                   clear                   @Suffix
     c                   clear                   @BatchSeq
     c                   clear                   @DocId
     c                   clear                   @BatchId
     c                   eval      @RtnCode = '1'

     c                   endif

     * Folder Balance Routine (only do if no errors above)
     c                   If        @RtnCode = *blanks

     c     fbky1         setll     pfb01
     c                   read      pfb01                                  60

     c                   if        NOT *in60
    
     * Increment number of docs *
     c                   eval      fbc#ds = fbc#ds + 1
    
     c                   if        fbc#ds >= fbm#ds
     c                   exsr      newsfl
    
     c                   if        errorCode <> *blanks
     c                   move      '3'           errorCode
     c                   endif
    
     c                   endif
    
     c                   movel     fbcif         @Folder
     c                   movel     fbcisf        @SubFldr
     c                   eval      @NewPath = fbcif + '/' + fbcisf

     c     *ymd          move      xDate         fbldt
     c     *hms          move      xTime         fbltim

     c                   if        errorCode = *blanks
     c                   update    pfb01
     c*** Begin Add *** 07/29/03 *******************************************
     c                   else
     c                   unlock    imfbl
     c*** End   Add *** 07/29/03 *******************************************
     c                   endif

     c                   else
     * ERROR - Folder Balance record not found.
     c                   clear                   @Folder
     c                   clear                   @SubFldr
     c                   eval      @RtnCode = '2'
     c                   endif

     c                   EndIf

     *=====================================================================
     * srinit - Initalization subroutine
     *=====================================================================
     c     srinit        begsr

     c                   clear                   @RtnCode

     ** Intialize date and time fields
     c                   callp     GetCurDate(xDate:xTime)
     c     *ymd          move      xDate         numDate
     c                   move      numDate       charDate

     c                   eval      kLastDate = 0
     c                   eval      kLastTime = 0

     c                   endsr

     *=====================================================================
     * newsfl - Subroutine to create a new subfolder
     *=====================================================================
     C     newsfl        begsr
    
     C                   move      fbcisf        #Suffix
     C                   eval      #Suffix = #Suffix + 1
     C                   movel     fbcisf        xPrefix
     C                   move      #Suffix       xSuffix
     c                   eval      fbcisf = %trim(xPrefix) + %trim(xSuffix)
    
     ** Call CL pgm to create new subfolder
     C                   call      'SCCV5C'
     C                   parm                    fbcif
     C                   parm                    fbcisf
     C                   parm                    errorCode
     C                   PARM                    @PRE
    
     C                   eval      fbc#ds = 0
    
     C     endnewsfl     endsr

     p GetDocIdx       E




