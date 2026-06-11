      *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2005
      *========================================================================
    H/TITLE SC983R1 - all Remark Print gather document list for cover page
      *========================================================================
      * Description: This program generates a list of documents for a given
      *              key that will be printed on the cover page.
      *
      * Created    : 04/03/00
      * Programmer : C:113
      * Project    : 87MIW
      *========================================================================
     * Revisions:
     *
     * 07/12/05 C:062 Proj: DR193
     *                Add output type processing.                          d
     *
     * 04/07/03 C:128 Proj: 129WAO
     *                Modify to include all PC documents, attached objects d
     *                as well as "imported" documents.
     *
     * 12/11/02 C:87  Proj: 114IEC
     *                The array storing the document ids (docids) needed to
     *                be cleared after writing to the data queue when
     *                MAX_DOCS is reached during processing.
     *
     * ----------------------------------------------------------------------
     *  NOTE: Please add comments to the top of revisions.
     * ----------------------------------------------------------------------
      *========================================================================
      *========================================================================
      * File Definitions
      *========================================================================
     Fsdcml     if   e           k disk
     F*** Begin Change 04/07/03 ********************************************
     Fsmmdl1    if   e           k disk
     F*** End  Change 04/07/03 ********************************************

     *=========================================================================
      * Prototypes
     *=========================================================================
      *------------------------------------------------------------------------
      * WriteEntry - Write an entry to the dataq.
      *------------------------------------------------------------------------
     D WriteEntry      pr                  extpgm('SC984C1')
     D  queue                        10a
     D  library                      10a
     D  verb                         10a
     D  line1                        78a
     D  line2                        78a
     D  line3                        78a
     d*** begin add *** 07/12/05 *******************************************
     D  output_type                   1a
     d*** end   add *** 07/12/05 *******************************************
     D  line4                        41a   options(*nopass)


     *=========================================================================
      * Variable Definitions
     *=========================================================================
      * Entry parms
     D eKey            s             40a
     D eQue            s             10a
     D eLib            s             10a
     d*** begin add *** 07/12/05 *******************************************
     D efrdt           s              7a
     D etodt           s              7a
     D eoutt           s              1a
     d*** end   add *** 07/12/05 *******************************************

      * Working variables
     D numDocs         s              3  0 inz(0)
     D pVerb           s             10a   inz('*DOCIDS   ')
     d*** begin add *** 07/12/05 *******************************************
     D y               s              3s 0
     D z               s              3s 0
     D cfrdt           s              7s 0
     D ctodt           s              7s 0
     d*** end   add *** 07/12/05 *******************************************

      * Array of document ID's
     D docids          s             12a   dim(22)

      * Breakdown of docID's for passing
     D                 ds
     D c_docs                       275a
     D  line1                        78a   overlay(c_docs)
     D  line2                        78a   overlay(c_docs:79)
     D  line3                        78a   overlay(c_docs:157)
     D  line4                        41a   overlay(c_docs:235)

      * Constants
     D IMAGE           c                   const('***IMAGE***')
     D MAX_DOCS        c                   21
     D MORE            c                   const('*MORE')
    
     *=========================================================================
      * KList Definitions
     *=========================================================================
     C     cmKey1        klist
     C                   kfld                    eKey


     *=========================================================================
      * PLIST Definitions
     *=========================================================================
     C     *entry        plist
     C                   parm                    eKey
     C                   parm                    eQue
     C                   parm                    eLib
     c*** begin add *** 07/12/05 *******************************************
     C                   parm                    efrdt
     C                   parm                    etodt
     C                   parm                    eoutt
     c*** end   add *** 07/12/05 *******************************************

     *=========================================================================
      * Main
     *=========================================================================
     C                   eval      numDocs = 0
     c*** begin add *** 07/12/05 *******************************************
     C                   move      efrdt         cfrdt
     C                   move      etodt         ctodt
     c*** end   add *** 07/12/05 *******************************************

     C     cmkey1        chain     pcm01                              60

     C                   dow       NOT *in60
     C*** Begin Change 04/07/03 ********************************************
     c*** begin add *** 07/12/05 *******************************************
     C                   if        cmcdat >= cfrdt or cfrdt = 0
     C                   if        cmcdat <= ctodt or ctodt = 0
     c*** end   add *** 07/12/05 *******************************************

     C                   if        cmdcid <> *blanks
     C                             AND (cmimgf = 'Y'
     C                             OR cmimgf = 'F'
     C                             OR cmimgf = 'I')
     C                   exsr      ImgDoc
     C                   endif

     C                   exsr      MaxDoc

     C                   exsr      OthDoc
     c*** begin add *** 07/12/05 *******************************************
     c                   endif
     c                   endif
     c*** end   add *** 07/12/05 *******************************************

     C     cmkey1        reade     pcm01                                  60
     C                   enddo
     C*** End   Change 04/07/03 ********************************************

     C                   if        numDocs > 0
     C                   clear                   c_docs
     C                   movea     docids        c_docs

     c*** begin chg *** 07/12/05 *******************************************
     C**********         callp     WriteEntry(eQue:eLib:pVerb:
     C**********                              line1:line2:line3:line4)
     C                   callp     WriteEntry(eQue:eLib:pVerb:
     C                                        line1:line2:line3:eoutt:line4)
     c*** end   chg *** 07/12/05 *******************************************
     C                   endif

     C                   move      *on           *inlr
      *=====================================================================
      * ImgDoc - If this is a Image document. Add to document array.
      *=====================================================================
     C     ImgDoc        begsr

     C                   if        cmdcid = IMAGE
     C                   eval      numDocs = numDocs + 1
     C                   eval      docids(numDocs) = cmidoc
     C                   else
     C                   eval      numDocs = numDocs + 1
     C                   eval      docids(numDocs) = cmdcid
     C                   endif

     C                   endsr
      *=====================================================================
      * OthDoc - If this record has multiple objects attached, add them to
      *          the array.
      *=====================================================================
     C     OthDoc        begsr

      * Get all attached documents

     C                   if        cmimgf <> 'Y'
     C                             AND (cmimgf <> 'F'
     C                             AND cmimgf <> 'I')

     C                   if        cmqpty = 'Y'
     C     cmkey1        chain     pmd01                              61

     C                   dow       NOT *in61
     C                   eval      numDocs = numDocs + 1
     C                   eval      docids(numDocs) = mdoid
     C                   exsr      MaxDoc
     C     cmkey1        reade     pmd01                                  61
     C                   enddo

     C                   endif

     C                   if        cmimgf = 'O'
     C                             OR (cmimgf = 'W')
     c*** begin add *** 07/12/05 *******************************************
     C                   if        cmdcid <> ' '
     c*** end   add *** 07/12/05 *******************************************
     C                   eval      numDocs = numDocs + 1
     C                   eval      docids(numDocs) = cmdcid
     C                   exsr      MaxDoc
     c*** begin add *** 07/12/05 *******************************************
     c                   endif
     c*** end   add *** 07/12/05 *******************************************

     C                   endif

     C                   endif

     C                   endsr
      *=====================================================================
      * MaxDoc - Checks to see if array is full. If so write to dataq and
      *          clear array for next set of documents
      *=====================================================================
     C     MaxDoc        begsr

     C                   if        numDocs = MAX_DOCS
     C                   eval      docids(numDocs + 1) = MORE

     C                   clear                   c_docs
     C                   movea     docids        c_docs
     c*** begin chg *** 07/12/05 *******************************************
     C**********         callp     WriteEntry(eQue:eLib:pVerb:
     C**********                              line1:line2:line3:line4)
     C                   callp     WriteEntry(eQue:eLib:pVerb:
     C                                        line1:line2:line3:eoutt:line4)
     c*** end   chg *** 07/12/05 *******************************************
     C                   eval      numDocs = 0
     C***  begin add  ***  12/11/02  ***************************************
     C                   movea     *blanks       docids
     C***  end   add  ***  12/11/02  ***************************************

     C                   endif

     C                   endsr
