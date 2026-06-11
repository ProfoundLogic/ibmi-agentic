     *=====================================================================
     *  (C) Copyright Progressive Data Solutions, Inc., 2000
     *=====================================================================
    H/TITLE IS800R - Main Import Engine
     *=====================================================================
     *
     *
     *=====================================================================
     *  REVISIONS:
     *
     *  08/25/09 c:02 Project SEQ66811
     *                Fixes: 1) Change delay to occur once for 300 sec. not 300 times for 1 sec
     *
     *  05/30/08 c:62 Project DR257
     *                Add rollover error processing
     *
     *=====================================================================
     fimdtl3    if   e           k disk

     *=====================================================================
     * Constant definitions
     *=====================================================================
     d TRUE            c                   const('1')
     d FALSE           c                   const('0')
     d YES             c                   const('Y')
     d NONIDXPGM       c                   const('IS810R')
     d IDXPGM          c                   const('IS811R')
     d ERRORPGM        c                   const('IS805C')
     d GetWsId         c                   const('IS821R')
     d DlyApi          c                   const('IA100C')

     *=====================================================================
     * Procedure Prototypes
     *=====================================================================
      /copy IS800RINC


     *=====================================================================
     * Data Structure, Table & Array definitions
     *=====================================================================
     ** Program information
     D psds          esds                  extname(szq1p)

     *=====================================================================
     * Standalone Field definitions
     *=====================================================================
     * Program control
     d running         s              1a                                        Program state
     d rtnCode         s              1a                                        Generic return code
     d quit            s              1a   inz(FALSE)                           Quit request
     d tempCount       s              3  0 inz(0)
     d delaySecs       s              3  0 inz(300)
     d delayOne        s              3  0 inz(1)
     d i               s              3  0 inz(0)

     d @cod            s              1a
     d @dtaq           s             10a
     d @dtaqLib        s             10a
     d*** begin add *** 05/30/08 *******************************************
     d @rtnc           s              1a
     d*** end   add *** 05/30/08 *******************************************

     d imgDrive        s              1a                                        Image drive
     d wrkStnId        s              2a                                        Workstation ID
     d jobName         s             10a                                        Server job name
     d errPgm          s             10a                                        Program with error
     d pFolder         s             80a

     *=====================================================================
     * KList definitions
     *=====================================================================

     *=====================================================================
     * PList definitions
     *=====================================================================
     c     *entry        plist
     c                   parm                    @cod
     c                   parm                    @dtaq
     c                   parm                    @dtaqLib
     c*** begin add *** 05/30/08 *******************************************
     c                   parm                    @rtnc
     c*** end   add *** 05/30/08 *******************************************

     c     NonIdxPL      plist
     c                   parm                    wrkStnID
     c                   parm                    dtdoct
     c                   parm                    rtnCode

     c     IdxPl         plist
     c                   parm                    wrkStnID
     c                   parm                    dtdoct
     c                   parm                    rtnCode

     c     GetWsIdPl     plist
     c                   parm                    jobName
     c                   parm                    wrkStnId
     c                   parm                    imgDrive
     c                   parm                    rtnCode

     *=====================================================================
     * Main
     *=====================================================================

     c                   read      pdt01                                  60
     * Main loop; runs until a '*QUIT' request is made
     c                   dow       running = TRUE

     ** TODO: processing

     * Do the non-indexed import process ('loose' TIFF's)
     c                   if        dtdid  <> *blanks
     c                   eval      pFolder = dtdid
     c*** copy from IFS to QDLS if there
     C                   Eval      rtnCode = ' '
     c                   callp     $CpyDloEnt(pFolder:dtdoct:wrkStnId:rtnCode:
     c                                        @dtaq:@dtaqLib)
     c*
     c                   callp     $GetDloEnt(pFolder:dtdoct:wrkStnId:rtnCode:
     c                                        @dtaq:@dtaqLib)
     c*** begin add *** 05/30/08 *******************************************
     c                   if        rtnCode='S'
     c                   eval      @rtnc='S'
     c                   return
     c                   endif
     c*** end   add *** 05/30/08 *******************************************
     c                   if        rtnCode <> *blanks
     c                   eval      errPgm = 'IS810R'
     c                   callp     errHandler(rtnCode:errPgm)
     c                   endif

     c                   endif

     C                   Eval      rtnCode = ' '
     * Do the indexed import process (Ascent data)
     c                   if        dtimpg <> *blanks
     c                   call      dtimpg        IdxPl
     c*** begin add *** 05/30/08 *******************************************
     c                   if        rtnCode='S'
     c                   eval      @rtnc='S'
     c                   return
     c                   endif
     c*** end   add *** 05/30/08 *******************************************

     c                   if        rtnCode <> *blanks
     c                   eval      errPgm = dtimpg
     c                   callp     errHandler(rtnCode:errPgm)
     c                   endif

     c                   endif
     C                   Eval      rtnCode = ' '

     * Get the next document type record
     c                   read      pdt01                                  60

     * If we reach the end of the file, set back to beginning and start over
     c                   if        *in60

     c                   if        @cod = TRUE
     c                   eval      quit = TRUE
     c                   else

     * Wait for X seconds
     c                   eval      i = 0
     c** 8/25/09******   dow       i < delaySecs
     c                   dow       i = 0                                        chg to occur 1 time
     c                   callb     DlyApi
     C                   parm                    delaySecs
     *** change 8/25/09 to use 300 sec for delay parm ***
     c********8/25/09 ** parm                    delayOne

     ** Read dataq for quit message
     c                   callp     $CheckQuit(@dtaq:@dtaqLib:quit)

     c                   if        quit = TRUE
     c                   eval      running = FALSE
     c                   leave
     c                   endif
     c                   eval      i = i + 1

     c                   enddo

     c     *loval        setll     pdt01
     c                   read      pdt01                                  60
     c                   endif

     c                   endif

     ** Read dataq for quit message
     c                   if        @cod = FALSE
     c                   callp     $CheckQuit(@dtaq:@dtaqLib:quit)
     c                   endif

     c                   if        quit = TRUE
     c                   eval      running = FALSE
     c                   endif

     c                   enddo

     ** End program
     c                   eval      *inlr = *on

     *=====================================================================
     * *inzsr
     *=====================================================================
     c     *inzsr        begsr

     * Get the Workstation ID
     c                   eval      jobName = q1job
     c                   callb     GetWsId       GetWsIdPl

     * Start running
     c                   eval      running = TRUE

     * Set to beginning of logical file
     c     *loval        setll     pdt01

     c     endinzsr      endsr




     *=====================================================================
     * ErrHandler - Handle an error messages
     *=====================================================================
     p ErrHandler      B

     *** Procedure Interface ***
     d ErrHandler      PI
     d  errCode                       1a   const
     d  errPgm                       10a   const

     *** Locals ***
     d errorCode       s              8a
     d errorDesc       s             80a
     d errProgram      s             10a

     *** Procedure Definition ***
     c                   eval      errorCode = errCode
     c                   eval      errProgram = errPgm
     c                   eval      errorDesc = 'DocType: ' + dtdid
     c                   callb     ERRORPGM
     c                   parm                    errProgram
     c                   parm                    errorCode
     c                   parm                    errorDesc

     c                   return

     p ErrHandler      E
