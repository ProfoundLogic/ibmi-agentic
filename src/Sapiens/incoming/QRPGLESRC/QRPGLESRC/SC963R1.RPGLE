      *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2005
      *========================================================================
    H/TITLE SC963R1 - all Remark Print gather document list for cover page
      *========================================================================
      * Description: This program generates a list of documents for a given
      *              key that will be printed on the cover page.
      *
      *========================================================================
      * Created on 07/10/2003 by c:62 for Project 129WAO
      *========================================================================
     * Revisions:
     *
     * 03/14/05 c:62 project 129IJS
      *               Change the query selection of remark code and
      *               summary code to an OR
     *
     * 01/31/05 c:62 project DR129 45568
     *               Process Additional Documents if attached to an image
     *
      *========================================================================
     Fsdcml     if   e           k disk
     Fsmmdl1    if   e           k disk

     *=========================================================================
      * Prototypes
     *=========================================================================
      *------------------------------------------------------------------------
      * WriteEntry - Write an entry to the dataq.
      *------------------------------------------------------------------------
     D WriteEntry      pr                  extpgm('SC964C1')
     D  queue                        10a
     D  library                      10a
     D  verb                         10a
     D  line1                        78a
     D  line2                        78a
     D  line3                        78a
     D  output_type                   1a
     D  line4                        41a   options(*nopass)


     *=========================================================================
      * Variable Definitions
     *=========================================================================
      * Entry parms
     D eKey            s             40a
     D eQue            s             10a
     D eLib            s             10a
     D efdat           s              7a
     D etdat           s              7a
     D eotyp           s              1a
     D esmcds          s             56a
     D ermcds          s            210a

      * Working variables
     D numDocs         s              3  0 inz(0)
     D pVerb           s             10a   inz('*DOCIDS   ')
     D y               s              3s 0
     D z               s              3s 0
     D cfdat           s              7s 0
     D ctdat           s              7s 0

      * Array of document ID's
     D docids          s             12a   dim(22)

     D                 ds
     DRmkArr                          5    dim(42)
     Dxxrm01                   1      5
     Dxxrm02                   6     10
     Dxxrm03                  11     15
     Dxxrm04                  16     20
     Dxxrm05                  21     25
     Dxxrm06                  26     30
     Dxxrm07                  31     35
     Dxxrm08                  36     40
     Dxxrm09                  41     45
     Dxxrm10                  46     50
     Dxxrm11                  51     55
     Dxxrm12                  56     60
     Dxxrm13                  61     65
     Dxxrm14                  66     70
     Dxxrm15                  71     75
     Dxxrm16                  76     80
     Dxxrm17                  81     85
     Dxxrm18                  86     90
     Dxxrm19                  91     95
     Dxxrm20                  96    100
     Dxxrm21                 101    105
     Dxxrm22                 106    110
     Dxxrm23                 111    115
     Dxxrm24                 116    120
     Dxxrm25                 121    125
     Dxxrm26                 126    130
     Dxxrm27                 131    135
     Dxxrm28                 136    140
     Dxxrm29                 141    145
     Dxxrm30                 146    150
     Dxxrm31                 151    155
     Dxxrm32                 156    160
     Dxxrm33                 161    165
     Dxxrm34                 166    170
     Dxxrm35                 171    175
     Dxxrm36                 176    180
     Dxxrm37                 181    185
     Dxxrm38                 186    190
     Dxxrm39                 191    195
     Dxxrm40                 196    200
     Dxxrm41                 201    205
     Dxxrm42                 206    210

     D                 ds
     DSumArr                          4    dim(14)
     Dxxsc01                   1      4
     Dxxsc02                   5      8
     Dxxsc03                   9     12
     Dxxsc04                  13     16
     Dxxsc05                  17     20
     Dxxsc06                  21     24
     Dxxsc07                  25     28
     Dxxsc08                  29     32
     Dxxsc09                  33     36
     Dxxsc10                  37     40
     Dxxsc11                  41     44
     Dxxsc12                  45     48
     Dxxsc13                  49     52
     Dxxsc14                  53     56

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

     C     cmKey2        klist
     C                   kfld                    cmkey
     C                   kfld                    cmseq#

     *=========================================================================
      * PLIST Definitions
     *=========================================================================
     C     *entry        plist
     C                   parm                    eKey
     C                   parm                    eQue
     C                   parm                    eLib
     C                   parm                    efdat
     C                   parm                    etdat
     C                   parm                    eotyp
     C                   parm                    esmcds
     C                   parm                    ermcds

     *=========================================================================
      * Main
     *=========================================================================
     C                   eval      numDocs = 0
     C                   move      efdat         cfdat
     C                   move      etdat         ctdat
     C                   movea     esmcds        sumarr
     C                   movea     ermcds        rmkarr

     C     cmkey1        chain     pcm01                              60

     C                   dow       NOT *in60

     C                   if        cmcdat >= cfdat or cfdat = 0
     C                   if        cmcdat <= ctdat or ctdat = 0

     C                   setoff                                       1617

     C*** begin chg *** 03/14/05 *******************************************
     C**********         if        esmcds = *blanks
     C**********         seton                                        16
     C**********         else
     C                   if        esmcds <> *blanks
     C*** end   chg *** 03/14/05 *******************************************
     C                   if        cmsmcd <> *blanks
     C                   eval      y=1
     C     cmsmcd        lookup    sumarr(y)                              16
     C                   endif
     C                   endif

     C*** begin chg *** 03/14/05 *******************************************
     C**********         if        ermcds = *blanks
     C**********         seton                                        17
     C**********         else
     C                   if        ermcds <> *blanks
     C*** end   chg *** 03/14/05 *******************************************
     C                   if        cmrmcd <> *blanks
     C                   eval      z=1
     C     cmrmcd        lookup    rmkarr(z)                              17
     C                   endif
     C                   endif
     C*** begin add *** 03/14/05 *******************************************
     C                   if        ermcds = *blanks and esmcds = *blanks
     C                   seton                                        16
     C                   endif
     C*** end   add *** 03/14/05 *******************************************

     C*** begin chg *** 03/14/05 *******************************************
     C**********         if        *in16 = *on and *in17 = *on
     C                   if        *in16 or *in17
     C*** end   chg *** 03/14/05 *******************************************

     C                   if        cmdcid <> *blanks
     C                             AND (cmimgf = 'Y'
     C                             OR cmimgf = 'F'
     C                             OR cmimgf = 'I')
     C                   exsr      ImgDoc
     C                   endif

     C                   exsr      MaxDoc

     C                   exsr      OthDoc

     C                   endif
     C                   endif
     C                   endif

     C     cmkey1        reade     pcm01                                  60
     C                   enddo

     C                   if        numDocs > 0
     C                   clear                   c_docs
     C                   movea     docids        c_docs

     C                   callp     WriteEntry(eQue:eLib:pVerb:
     C                                        line1:line2:line3:eotyp:line4)
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

     C*** begin chg *** 01/31/05 *******************************************
     C**********         if        cmimgf <> 'Y'
     C**********                   AND (cmimgf <> 'F'
     C**********                   AND cmimgf <> 'I')
     C                   if        (cmimgf <> 'Y'
     C                              and cmimgf <> 'F'
     C                              and cmimgf <> 'I') or
     C                             (cmqpty = 'Y')
     C*** end   chg *** 01/31/05 *******************************************

     C                   if        cmqpty = 'Y'
     C     cmkey2        chain     pmd01                              61

     C                   dow       NOT *in61
     C                   eval      numDocs = numDocs + 1
     C                   eval      docids(numDocs) = mdoid
     C                   exsr      MaxDoc
     C     cmkey2        reade     pmd01                                  61
     C                   enddo

     C                   endif

     C                   if        cmimgf = 'O'
     C                             OR (cmimgf = 'W')
     C                   if        cmdcid <> ' '
     C                   eval      numDocs = numDocs + 1
     C                   eval      docids(numDocs) = cmdcid
     C                   exsr      MaxDoc
     C                   endif

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
     C                   callp     WriteEntry(eQue:eLib:pVerb:
     C                                        line1:line2:line3:eotyp:line4)
     C                   eval      numDocs = 0
     C                   movea     *blanks       docids

     C                   endif

     C                   endsr
