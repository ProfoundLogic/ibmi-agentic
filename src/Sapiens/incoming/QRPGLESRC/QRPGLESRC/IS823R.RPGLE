     *=====================================================================
     *  (c) Copyright Progressive Data Solutions, Inc., 2000
     *=====================================================================
    H/TITLE IS823R - GetDocIdx
     *=====================================================================
     *
     * Description:
     *  Get the Document Index and Folder Balance records and return the
     *  info to the calling program.
     *
     * Created   : 07/03/00
     * Programmer: C:113
     * Project   : DR113
     *
     *=====================================================================
     * Revisions:
     *
      * 03/08/18 C:79 Project 114MJB
      *               Use prefix of incoming document not just .TIF.        ve code.
     *
      * 08/25/09 c:02 Project SEQ66811A
      *               Fix. Seq 59701 (below) caused issue with Import.  remove code.
      *                   Di file never updated with new SEQ# so this causes problems in other pgms
      *                   for import, just going to remove the reset to '0000' for now.
      *
     *
     * 05/30/08 c:62 Project DR257
     *               Add Rollover Processing
     *
     * 08/21/07 c:62 Project 59701
     *               Reset the seq# if not the same day
     *
     *=====================================================================
     * Document Index Control File
     Fimdil     if   e           k disk
    
     * Folder Balance File
     Fimfbl     uf   e           k disk
    
     *=====================================================================
     * Procedure Prototypes
     *=====================================================================
      /copy IS820RPR
      /copy IS823RPR
     d*** Beg Add *** 03/08/18 *******************************************
     d/copy sPrModule
     d*** End Add *** 03/08/18 *******************************************

     *=====================================================================
     * Table, Data Structure & Array Definitions
     *=====================================================================


     *=====================================================================
     * Stand Alone Field Definitions
     *=====================================================================
     D @BatchId        s             12a
     D @BatchSeq       s              4a
     D @Folder         s              8a
     D @SubFldr        s             12a
     D @DocId          s             12a
     D @RtnCode        s              1a
     D @DocSeq#        s              6a
     D @DocSeq2        s              6  0
     D @Suffix         s              3a
     D @NewPath        s             21a
     D @WrkStnId       s              2a
     D*** begin add *** 08/21/07 *******************************************
     D xxdt            s                   like(dildt)
     D*** end   add *** 08/21/07 *******************************************


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


     *=====================================================================
     * Main
     *=====================================================================
     c                   callp     GetDocIdx(@WrkStnId : @DocSeq# : @Suffix :
     c                                       @BatchSeq : @DocId : @BatchId :
     c                                       @Folder : @SubFldr : @NewPath :
     c                                       @RtnCode )

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

     *** Locals ***
     d xDate           s               d
     d xTime           s               t
     d charDate        s              6a
     d numDate         s              8  0
     d kLastDate       s              6  0
     d kLastTime       s              6  0

     *** KLists ***
     * Document Index Control file
     c     diky1         klist
     c                   kfld                    @WrkStnId

     * Folder Balance file
     c     fbky1         klist
     c                   kfld                    kLastDate
     c                   kfld                    kLastTime

     *** Procedure Definition ***
     c                   exsr      srinit

     c     diky1         chain     pdi01                              60

     c                   if        NOT *in60
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   if        diseq#=-1
     * ERROR - Rollover
     c                   clear                   @DocSeq#
     c                   clear                   @Suffix
     c                   clear                   @BatchSeq
     c                   clear                   @DocId
     c                   clear                   @BatchId
     c                   eval      @RtnCode = 'S'
     C                   else
     C*** END   ADD *** 05/30/08 **************************************
     c                   move      diseq#        @DocSeq#
     c*** Beg Add *** 03/08/18 *******************************************
     c                   If        @suffix = *blanks or
     c                             not(isModEnabled('Use Image Extension'))
     c*** End Add *** 03/08/18 *******************************************
     c                   movel     disufx        @Suffix
     c*** Beg Add *** 03/08/18 *******************************************
     c                   Endif
     c*** End Add *** 03/08/18 *******************************************
     C*** begin add *** 08/21/07 *******************************************
     c                   callp     GetCurDate(xDate:xTime)
     c     *ymd          move      xDate         xxdt
     c*
     c                   if        dildt = xxdt
     C*** end   add *** 08/21/07 *******************************************
     c                   move      dibchs        @BatchSeq
     C*** begin add *** 08/21/07 *******************************************
     c                   else
     C*** begin CHG *** 08/25/09 *******************************************
     *********8/25/09 ** move      '0000'        @BatchSeq
     c                   move      dibchs        @BatchSeq
     C*** end   CHG *** 08/25/09 *******************************************
     c                   endif
     C*** end   add *** 08/21/07 *******************************************
     c
     c                   eval      @DocId = diwsid + @DocSeq# + '.' + @Suffix
     c                   eval      @BatchId = diwsid + charDate + @BatchSeq
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   endif
     C*** END   ADD *** 05/30/08 **************************************

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

     c                   movel     fbcif         @Folder
     c                   movel     fbcisf        @SubFldr
     c                   eval      @NewPath = fbcif + '/' + fbcisf

     c                   callp     GetCurDate(xDate:xTime)
     c     *ymd          move      xDate         fbldt
     c     *hms          move      xTime         fbltim

     c                   update    pfb01

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

     p GetDocIdx       E




