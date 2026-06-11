     *=====================================================================
     *  (C) Copyright Progressive Data Solutions, Inc., 2000
     *=====================================================================
    H/TITLE IS820R   Update Batch Master Record
     *=====================================================================
     *
     * Description: Updates or writes the Batch Master Record based on
     *   what status is passed in.
     *
     * Created   : 07/03/00
     * Programmer: C:113
     * Project   : DR113
     *
     *=====================================================================
     *  REVISIONS:
     *
     *=====================================================================

     * Batch Administration Master File
     Fimbml     uf a e           k disk

     * Batch Status Table File
     Fitbsl     if   e           k disk

     * Batch Notes File
     Fimbnl     if a e           k disk

     * Document Object File
     Fimdol5    if   e           k disk


     *=====================================================================
     * Prototypes
     *=====================================================================
      /copy IS820RPR


     *=====================================================================
     * Data Structure, Table & Array Definitions
     *=====================================================================
     ** Array of message line starts
     D fc              s             24    dim(5) ctdata perrcd(1)

     ** Date & Time structure
     D                 ds
     D dsDateTime                    14  0
     D  dsTime                        6  0 overlay(dsDateTime)
     D  dsDate                        8  0 overlay(dsDateTime)

     ** Program Information structure
     D psds          esds                  extname(szq1p)


     *=====================================================================
     * Stand Alone Fields
     *=====================================================================
     ** Batch Status Constants
      /copy ISBCHSTS

     d UpdDocRcds      c                   const('IS820R1')
     d ERROR           c                   const('1')

     D @NumPagDoc      s              3a
     D @ActNumDoc      s              5a
     D @ActNumPag      s              5a
     D @BatchId        s             12a
     D @BatchDesc      s             40a
     D @BatchErr       s             52a
     D @BatchSts       s              2a
     D @ChangePgm      s             10a
     D @DocType        s              8a
     D @DocSepFl       s              1a
     D @EstNumDoc      s              5a
     D @EstNumPag      s              5a
     D @RtnCode        s              1a
     D @ScanDate       s              8a
     D @ScanRep        s             10a
     D @ScanTime       s              6a
     D saveBchSts      s              2a
     D sequence#       s              5  0
     D pgmStat         s              1a
     d xDate           s               d
     d xTime           s               t
     d dateChar        s             10a
     d timeChar        s              8a
     D #ScanDate       s              8  0

     *=====================================================================
     * KFld Definitions
     *=====================================================================
     ** Batch Administration
     C     bmky1         klist
     C                   kfld                    @BatchId

     ** Batch Status Table
     C     bsky1         klist
     C                   kfld                    @BatchSts

     ** Batch Notes
     C     bnky1         klist
     C                   kfld                    @BatchId
     C                   kfld                    sequence#

     C     bnky2         klist
     C                   kfld                    @BatchId

     ** Document Object
     C     doky1         klist
     C                   kfld                    @BatchId

     *=====================================================================
     * PList Definitions
     *=====================================================================
     ** Entry parameters
     C     *entry        plist
     C                   parm                    @BatchId
     C                   parm                    @BatchSts
     C                   parm                    @DocType
     C                   parm                    @ScanRep
     C                   parm                    @ScanDate
     C                   parm                    @ScanTime
     C                   parm                    @EstNumDoc
     C                   parm                    @ActNumDoc
     C                   parm                    @EstNumPag
     C                   parm                    @ActNumPag
     C                   parm                    @NumPagDoc
     C                   parm                    @DocSepFl
     C                   parm                    @ChangePgm
     C                   parm                    @BatchErr
     C                   parm                    @BatchDesc
     C                   parm                    @RtnCode

     c     pUpdDocRec    plist
     C                   parm                    @BatchId
     C                   parm                    @BatchSts
     C                   parm                    saveBchSts
     C                   parm                    @ChangePgm
     C                   parm      *blank        @RtnCode

     *================================================================
     * Main
     *================================================================
     C                   clear                   @RtnCode
     c                   move      @ScanDate     #ScanDate

     ** Create the Batch Admin record if it doesn't exist.  Otherwise
     *  change the status to the value passed in.  Depending on this
     *  status, we will perform other functions.

     C     bmky1         chain     pbm01                              69
     C                   if        *in69 = *off
     C                   movel     bmbsts        saveBchSts
     C                   else
     C                   clear                   saveBchSts
     C                   endif

     C     @BatchSts     caseq     BS_SCAN       $Scan
     C     @BatchSts     caseq     BS_RELEASE    $Release
     C     @BatchSts     caseq     BS_HOLD       $Hold
     C     @BatchSts     caseq     BS_QUALCTL    $QualCtl
     C     @BatchSts     caseq     BS_COMP       $Complete
     C     @BatchSts     caseq     BS_INDEX      $Index
     C     @BatchSts     caseq     BS_REJECT     $Reject
     C     @BatchSts     caseq     BS_RESCAN     $Rescan
     C     @BatchSts     caseq     BS_ERROR      $Error
     C     @BatchSts     caseq     BS_DELETE     $Delete
     C                   cas                     $ErrStat
     C                   endcs

     ** If an error occurs during one of the above functions, set the return code
     **  and don't do any more processing.
     C                   if        pgmStat = ERROR
     C                   eval      @RtnCode = ERROR
     C                   else

     C                   clear                   bmstat
     C                   movel     @ChangePgm    bmlpgm
     C                   movel     q1user        bmlusr
     c                   callp     GetCurDate(xDate:xTime)
     C     *hms          move      xTime         bmltim
     C     *ymd          move      xDate         bmldt

     ** Create the Batch Notes
     C                   exsr      $BchNotes

     ** If the Batch Administration record doesn't exist, write a new one.
     C                   if        *in69

     C                   movel     @ChangePgm    bmipgm
     C                   movel     q1user        bmiusr
     c                   callp     GetCurDate(xDate:xTime)
     C     *hms          move      xTime         bmitim
     C     *ymd          move      xDate         bmidt

     C                   write     pbm01

     C                   else
     **  Otherwise, update the existing record.
     C                   update    pbm01
     C                   endif

     ** Call the program to update the individual Document Object records.  These
     **  records are only updated when they are not indexed.
     C                   call      UpdDocRcds    pUpdDocRec

     c                   endif

     ** End main
     C                   move      *on           *inlr
     *===================================================================




     *================================================================
     * *inzsr
     *================================================================
     c     *inzsr        begsr

     c                   clear                   @RtnCode
     c                   move      @ScanDate     #ScanDate

     c                   endsr




     *===================================================================
     * $Scan- UPDATE RECORD TO SCAN STATUS
     *===================================================================
     C     $Scan         begsr

     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi
     C                   movel     @DocType      bmdoct
     C                   movel     @ScanRep      bmsrep
     c                   eval      xDate = CvtToDate(#ScanDate)
     c     *iso          move      xDate         bmsdt
     C                   move      @ScanTime     bmstm
     C                   move      @EstNumDoc    bme#dc
     C                   move      @EstNumPag    bme#pg
     C                   move      @NumPagDoc    bm#pgp
     C                   movel     @DocSepFl     bmdsep
     C                   movel     @BatchDesc    bmbdsc


     C     endsc         endsr




     *===================================================================
     * $Release - UPDATE RECORD TO RELEASE STATUS
     *===================================================================
     C     $Release      begsr

     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi

     C                   move      @ActNumDoc    bma#dc
     C                   move      @ActNumPag    bma#pg

     ** Update the estimates if not present
     c                   if        bme#dc = *zeros
     c                   move      @EstNumDoc    bme#dc
     c                   endif

     c                   if        bme#pg = *zeros
     c                   move      @EstNumPag    bme#pg
     c                   endif

     C     endrl         endsr




     *===================================================================
     * $Complete - UPDATE RECORD TO COMPLETE STATUS
     *===================================================================
     C     $Complete     begsr

     ** Update the status & actual docs and pages
     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi
     C                   move      @ActNumDoc    bma#dc
     C                   move      @ActNumPag    bma#pg

     ** Update the estimates if not present
     c                   if        bme#dc = *zeros
     c                   move      @EstNumDoc    bme#dc
     c                   endif

     c                   if        bme#pg = *zeros
     c                   move      @EstNumPag    bme#pg
     c                   endif

     C     endcm         endsr




     *===================================================================
     * $Index - UPDATE RECORD TO INDEXING STATUS
     *===================================================================
     C     $Index        begsr

     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi
     C                   move      @ActNumDoc    bma#dc
     C                   move      @ActNumPag    bma#pg

     ** Update the estimates if not present
     c                   if        bme#dc = *zeros
     c                   move      @EstNumDoc    bme#dc
     c                   endif

     c                   if        bme#pg = *zeros
     c                   move      @EstNumPag    bme#pg
     c                   endif

     C     endix         endsr




     *===================================================================
     * $Hold- UPDATE RECORD TO HOLD STATUS
     *===================================================================
     C     $Hold         begsr

     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi

     * UPDATE THE ACTUAL VALUES IF NOT ALREADY THERE!!!
     c                   if        bme#dc = *zeros
     c                   move      @EstNumDoc    bme#dc
     c                   endif

     C                   if        bma#dc = *zeros
     C                   move      @ActNumDoc    bma#dc
     C                   endif

     c                   if        bme#pg = *zeros
     c                   move      @EstNumPag    bme#pg
     c                   endif

     C                   if        bma#pg = *zeros
     C                   move      @ActNumPag    bma#pg
     C                   endif

     C     endhd         endsr




     *===================================================================
     * $Reject - UPDATE RECORD TO REJECT STATUS
     *===================================================================
     C     $Reject       begsr

     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi

     ** Update the estimates if not present
     c                   if        bme#dc = *zeros
     c                   move      @EstNumDoc    bme#dc
     c                   endif

     c                   if        bme#pg = *zeros
     c                   move      @EstNumPag    bme#pg
     c                   endif

     C     endrj         endsr




     *===================================================================
     * $Rescan - UPDATE RECORD TO RESCAN STATUS
     *===================================================================
     C     $Rescan       begsr

     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi

     ** Update the estimates if not present
     c                   if        bme#dc = *zeros
     c                   move      @EstNumDoc    bme#dc
     c                   endif

     c                   if        bme#pg = *zeros
     c                   move      @EstNumPag    bme#pg
     c                   endif

     C     endrs         endsr




     *===================================================================
     * $Error - UPDATE RECORD TO ERROR STATUS
     *===================================================================
     C     $Error        begsr

     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi

     C     ender         endsr




     *===================================================================
     * $QualCtl - UPDATE RECORD TO QUALITY CONTROL STATUS
     *===================================================================
     C     $QualCtl      begsr

     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi

     C                   move      @ActNumDoc    bma#dc
     C                   move      @ActNumPag    bma#pg

     ** Update the estimates if not present
     c                   if        bme#dc = *zeros
     c                   move      @EstNumDoc    bme#dc
     c                   endif

     c                   if        bme#pg = *zeros
     c                   move      @EstNumPag    bme#pg
     c                   endif

     C     endqc         endsr




     *===================================================================
     * $Delete - UPDATE RECORD TO DELETED STATUS
     *===================================================================
     C     $Delete       begsr

     C     doky1         chain     pdo01                              62

     C                   if        *in62 = *on
     C                   movel     @BatchSts     bmbsts
     C                   movel     @BatchId      bmbchi
     C                   else
     C                   eval      pgmStat = ERROR
     C                   endif

     C     enddl         endsr




     *===================================================================
     * $ErrStat- ERRROR - STATUS NOT VALID
     *===================================================================
     C     $ErrStat      begsr

     C                   eval      pgmStat = ERROR

     C     enderr        endsr




     *===================================================================
     * $BchNotes - Create the batch notes
     *===================================================================
     C     $BchNotes     begsr

     C                   eval      sequence# = *hival
     C     bnky1         setgt     pbn01
     C     bnky2         readpe    pbn01                                  60
     C                   if        *in60 = *off
     C                   eval      sequence# = 1 + bnasq#
     C                   else
     C                   eval      sequence# = 1
     C                   endif

     C                   movel     @BatchId      bnbchi
     C                   clear                   bncm75
     C                   move      sequence#     bnasq#

     c                   callp     GetCurDate(xDate:xTime)
     c     *iso          move      xDate         bncdat
     c     *iso          move      xDate         bntrdt
     c     *hms          move      xTime         bntrtm

     C                   movel     @ChangePgm    bnpnam

     ** SECURITY VALUES NOT CURRENTLY USED
     C                   clear                   bnsecl
     C                   clear                   bnsbyl

     ** PROTECT THIS RCD, AS IT IS A HISTORY RECORD - "H" - HISTORY
     C                   movel     'H'           bnprfl

     C                   clear                   bnstat
     C                   movel     q1user        bniusr
     C                   movel     q1pgm         bnipgm

     c     *ymd          move      xDate         bnidt
     c     *hms          move      xTime         bnitim

     C                   clear                   bnlusr
     C                   clear                   bnlpgm
     C                   clear                   bnltim
     C                   clear                   bnldt

     ** FORMAT THE COMMENT
     ** Line one - Batch History:
     c                   eval      bncm75 = fc(1)
     C                   write     pbn01

     ** Line two - Status:
     C                   eval      sequence# = sequence# + 1

     C                   clear                   bncm75
     C                   move      sequence#     bnasq#

     C     bsky1         chain     pbs01                              61
     c                   eval      bncm75 = fc(2) + @BatchSts + ' - ' + bsldsc
     C                   write     pbn01

     ** Line three - Date/Time:
     C                   eval      sequence# = sequence# + 1

     C                   clear                   bncm75
     C                   move      sequence#     bnasq#

     c                   callp     GetCurDate(xDate:xTime)
     c     *usa          move      xDate         dateChar
     c     *usa          move      xTime         timeChar
     c                   eval      bncm75 = fc(3) + dateChar + '     ' +
     c                             timeChar

     C                   write     pbn01

     ** Line four - User:
     C                   eval      sequence# = sequence# + 1

     C                   clear                   bncm75
     C                   move      sequence#     bnasq#

     c                   eval      bncm75 = fc(4) + q1user

     C                   write     pbn01

     ** Line five - Error:
     C                   if        @BatchErr <> *blanks

     C                   eval      sequence# = sequence# + 1

     C                   clear                   bncm75
     C                   move      sequence#     bnasq#
     c                   eval      bncm75 = fc(5) + @BatchErr
     C                   write     pbn01

     C                   endif

     C     endBchNot     endsr




     *===================================================================
     * Table Entries
     *===================================================================
**   RMRK ENTRIES
Batch History:
            Status   :
            Date/Time:
            User     :
            Error    :
