     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *============================================================================================*
      /title W4901RA - API for CEU
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
      *============================================================================================*
      *  This program accepts the policy information, the current 'Q' value in W4901R, the state
      *  sort value, the policy from date, and returns the month/s in which a self-reporting bill
      *  resides that has been 'changed' by the 'Q' transaction.  The 'Q' transaction is a revised
      *  bill that is posted into the future.  When it is picked up by the CEU, it might be inserted
      *  prior to an S/R bill that was picked up by a previous report.  Consequently, the S/R bill
      *  is adjusted to the inserted 'Q' transaction, but when it is adjusted by the 'Q'
      *  transaction, the MTD number does not reflect this change. The purpose of the API is to
      *  find the months  that are affected by the 'Q' transaction.  The CEU will then
      *  reverse the affect of the 'Q' transaction and apply that difference back to the 'Q'
      *  transaction. This difference will  be passed onto the MTD number. This will
      *  enable the summation of the MTD numbers to equal the YTD numbers on the current year.
      *                                                                                            *
      *  CALLED FROM: W4901R                                                                       *
      *                                                                                            *
      *  CREATED 02/05/07   C:136   SUPPORT: 56879
      *============================================================================================*
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 02/01/12 C:136 Support: WA090 79130                                                        *
      *                Exclude S/R Billings outside of policy date range                           *
      *                                                                                            *
      *============================================================================================*
      *** CEU Workfile - co#/fnd/emp#/div/yr/trdt/idt/itim
     fwxeul     if   e           k disk    rename(peu01 : peu0l)
      *** CEU Workfile - co#/fnd/st/emp#/div/yr/trdt/idt/itim
     fwxeul2    if   e           k disk    rename(peu01 : peu0l2)
      *=============================================================================================
    d* Stand Alone Fields - TOP
    d*=============================================================================================
     d   kkco#         s                   like(EUCO#)
     d   kkfnd         s                   like(EUFND)
     d   kkemp#        s                   like(EUEMP#)
     d   kkdiv         s                   like(EUDIV)
     d   kkfyr         s                   like(EUFYR)
     d   kkstate       s                   like(EUST)
     d   qValue        s              6  0
     d   stateSort     s              1
     d   svTrDt        s              7  0
     d   svPoDt        s              7  0
     d   mthDiff       s              2  0
      *** beg add ***** 02/01/12  **********************************************
     d   polToDt       s              7  0
      *** end add ***** 02/01/12  **********************************************
    d*=============================================================================================
    d* Data Structures and Arrays
    d*=============================================================================================
     d   mthArray      s              1    dim(13)
     d                 ds
     d   polFrDt                      7  0
     d     polFrYR             1      3  0
     d     polFrMth            4      5  0
     d                 ds
     d   poDt                         7  0
     d     poDtYr              1      3  0
     d     poDtMth             4      5  0
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d*entry
     d W4901RA         pr
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
     d  @@stateSort                   1
     d  @@state                       2
     d  @@pfdt                        7
     d  @@qvalue                      6
     d  @@mtharray                   13
     d W4901RA         pi
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
     d  @@stateSort                   1
     d  @@state                       2
     d  @@pfdt                        7
     d  @@qvalue                      6
     d  @@mtharray                   13
    c*=============================================================================================
      *  KLISTS
    c*=============================================================================================
     c     euky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c     euky2         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkstate
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
    c*=============================================================================================
      *  MAINLINE PROCESSING
    c*=============================================================================================
     c                   move      @@co#         kkco#
     c                   move      @@fnd         kkfnd
     c                   move      @@emp#        kkemp#
     c                   move      @@div         kkdiv
     c                   move      @@fyr         kkfyr
     c                   move      @@stateSort   stateSort
     c                   move      @@state       kkstate
     c                   move      @@pfdt        polFrDt
     c                   move      @@qValue      qValue
      *** beg add ***** 02/01/12  **********************************************
     c                   eval      polToDt = polFrDt + 10000
      *** end add ***** 02/01/12  **********************************************
      *
      *  Position file pointer to record currently being processed by W4901R and save off the
      *  transaction date and the post date
      *
     c                   if        stateSort = 'Y'
     c     euky2         setll     peu0l2
     c                   else
     c     euky          setll     peu0l
     c                   endif

     c                   do        qValue
     c                   if        stateSort = 'Y'
     c     euky2         reade     peu0l2
     c                   else
     c     euky          reade     peu0l
     c                   endif
     c                   enddo

     c                   eval      svTrDt = EUTRDT
     c                   eval      svPoDt = EUPODT

     c                   if        stateSort = 'Y'
     c     euky2         reade     peu0l2
     c                   else
     c     euky          reade     peu0l
     c                   endif
     c                   dow       not(%eof)

     c     EUSORT        ifeq      '1'
     c     EUTRDT        andge     svTrDt
     c     EUTRDT        andle     svPoDt

      *** beg add ***** 02/01/12  **********************************************
     c                   if        EUPODT <  polFrDt  or
     c                             EUPODT >= polToDt
     c                   if        stateSort = 'Y'
     c     euky2         reade     peu0l2
     c                   else
     c     euky          reade     peu0l
     c                   endif
     c                   iter
     c                   endif
      *** end add ***** 02/01/12  **********************************************

     c                   eval      poDt = EUPODT
     c                   eval      mthDiff = poDtMth - PolFrMth
     c                   if        mthDiff < 0
     c                   add       13            mthDiff
     c                   else
     c                   if        polFrYr = poDtYr
     c                   add       1             mthDiff
     c                   else
     c                   add       13            mthDiff
     c                   endif
     c                   endif

     c                   eval      mthArray(mthDiff) = 'Y'
     c                   endif

     c                   if        stateSort = 'Y'
     c     euky2         reade     peu0l2
     c                   else
     c     euky          reade     peu0l
     c                   endif
     c                   enddo

     c                   movea     mthArray      @@mthArray
     c                   seton                                        lr
