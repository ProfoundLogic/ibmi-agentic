     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2003
     *========================================================================
     * SC963R3PR - Prototypes
     *========================================================================
     * Created on 06/30/2003 by c:62 for Project 129WAO
     *========================================================================
     * Revisions:
     *
     * 08/01/2014 c:62 Support IM045 86020
     *                 Expand the document id to handle additional objects (SMMDP)
     *
     *========================================================================
      *------------------------------------------------------------------------
      * PrintDoc - Write a request to the dataq for an image document.
      *------------------------------------------------------------------------
     D PrintDoc        PR                  extpgm('SC964C')
     D*** Begin Chg *** 08/01/14 ****************************
     D**docID***                     12a
     D  docID                       100a
     D*** End   Chg *** 08/01/14 ****************************
     D  folder                       80a
     D  key                          40a
     D  sequence                      5a
     D  optical                       1a
     D  image_fax                     1a
     D  qid                           2a
     D  rtnCode                       1a
     D  imagprt                       1a
     D  output_type                   1a
     D  path                        100a
     D  docSuffix                    24a
     D  printFlag                     1a

      *------------------------------------------------------------------------
      * CaseKeyAPI - MC api to get a darts key for a case.
      *------------------------------------------------------------------------
     D EmpKeyAPI       pr                  extpgm('SA719R')
     D  rtncode                       1a
     D  company                       3a
     D  mc_group                      3a
     D  employer                      9a
     D  division                      5a
     D  key                          40a

      *------------------------------------------------------------------------
      * InfoAPI - API to get info for cover pages.
      *------------------------------------------------------------------------
     D InfoAPI         pr                  extpgm('SA702R3')
     D  rtncode                       1a
     D  company                       3a
     D  mc_group                      3a
     D  Employer                      9a
     D  Division                      5a
     D  line1                        78a
     D  line2                        78a
     D  line3                        78a

      *------------------------------------------------------------------------
      * PrintCover - Print the cover page.
      *------------------------------------------------------------------------
     D PrintCover      pr                  extpgm('SC964C1')
     D  queue                        10a
     D  library                      10a
     D  verb                         10a
     D  line1                        78a
     D  line2                        78a
     D  line3                        78a
     D  output_type                   1a
     D  line4                        41a   options(*nopass)

      *------------------------------------------------------------------------
      * LoadBal - Process all the darts remarks for image documents
      *------------------------------------------------------------------------
     D LoadBal         pr
     D  currentID                     2a

      *------------------------------------------------------------------------
      * DocidList - Create a list of documents for a key
      *------------------------------------------------------------------------
     D DocidList       pr                  extpgm('SC963R1')
     D  key                          40a
     D  queue                        10a
     D  library                      10a
     D  from_date                     7a
     D  to_date                       7a
     D  output_type                   1a
     D  summcodesi                   56a
     D  rmrkcodesi                  210a

