     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
     * SC983RPR - Prototypes
     *========================================================================
     * REVISIONS:
     *
     * 07/12/05 c:62 Project Dr193
     *               Add output type processing to prototypes
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================

      *------------------------------------------------------------------------
      * PrintDoc - Write a request to the dataq for an image document.
      *------------------------------------------------------------------------
     D PrintDoc        PR                  extpgm('SC984C')
     D  docID                        12a
     D  folder                       80a
     D  key                          40a
     D  sequence                      5a
     D  optical                       1a
     D  image_fax                     1a
     D  qid                           2a
     D  rtnCode                       1a
     D  imagprt                       1a
     D*** begin add *** 07/12/05 *******************************************
     D  output_type                   1a
     D  path                        100a
     D  docpath                      11a
     D*** end   add *** 07/12/05 *******************************************

      *------------------------------------------------------------------------
      * GetSubAPI - MC API to get a group's corresponding subscriber number.
      *------------------------------------------------------------------------
     D GetSubAPI       pr                  extpgm('FA700R')
     D  return_code                   1a
     D  company                       3a
     D  wc_group                      3a
     D  subscriber                    5a
     D  group                         3a

      *------------------------------------------------------------------------
      * CaseKeyAPI - MC api to get a darts key for a case.
      *------------------------------------------------------------------------
     D CaseKeyAPI      pr                  extpgm('SA703R')
     D  rtncode                       1a
     D  company                       3a
     D  mc_group                      3a
     D  pol_year                      3a
     D  case                          7a
     D  key                          40a

      *------------------------------------------------------------------------
      * InfoAPI - API to get info for cover pages.
      *------------------------------------------------------------------------
     D InfoAPI         pr                  extpgm('SA702RR')
     D  rtncode                       1a
     D  company                       3a
     D**subscriber                    5a
     D  mc_group                      3a
     D  pol_year                      3a
     D  case                          7a
     D  line1                        78a
     D  line2                        78a
     D  line3                        78a

      *------------------------------------------------------------------------
      * PrintCover - Print the cover page.
      *------------------------------------------------------------------------
     D PrintCover      pr                  extpgm('SC984C1')
     D  queue                        10a
     D  library                      10a
     D  verb                         10a
     D  line1                        78a
     D  line2                        78a
     D  line3                        78a
     D*** begin add *** 07/12/05 *******************************************
     D  output_type                   1a
     D*** end   add *** 07/12/05 *******************************************
     D  line4                        41a   options(*nopass)

      *------------------------------------------------------------------------
      * ProcDarts - Process all the darts remarks for image documents
      *------------------------------------------------------------------------
     D*********ProcDarts       pr

      *------------------------------------------------------------------------
      * LoadBal - Process all the darts remarks for image documents
      *------------------------------------------------------------------------
     D LoadBal         pr
     D  currentID                     2a

      *------------------------------------------------------------------------
      * DocidList - Create a list of documents for a key
      *------------------------------------------------------------------------
     D DocidList       pr                  extpgm('SC983R1')
     D  key                          40a
     D  queue                        10a
     D  library                      10a
     D*** begin add *** 07/12/05 *******************************************
     D  from_date                     7a
     D  to_date                       7a
     D  output_type                   1a
     D*** end   add *** 07/12/05 *******************************************

