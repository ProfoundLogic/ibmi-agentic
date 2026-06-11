     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2005
     *========================================================================
     * SC981RPR - Prototypes
     *========================================================================
     * REVISIONS:
      *
      * 07/11/05 C:62  Proj: dr193
      *                Added 'Output type' to entry parms.
     *
     * 11/27/02 c:87  Project: 114IEC
      *                Added parm for call to ImagePrint
      *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================

      *========================================================================
      * Procedure Prototypes
      *========================================================================

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
      * MCreports - MC program to generate reports.
      *------------------------------------------------------------------------
     D MCreports       pr                  extpgm('F0679R')
     D  company                       3  0
     D  wc_group                      3  0
     D  poliycYear                    3  0
     D  case                          7  0

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
      * EmpKeyAPI - MC api to get a darts key for an employer.
      *------------------------------------------------------------------------
     D EmpKeyAPI       pr                  extpgm('SA719R')
     D  rtncode                       1a
     D  company                       3a
     D  mc_group                      3a
     D  employer                      9a
     D  division                      5a
     D  key                          40a

      *------------------------------------------------------------------------
      * DartPrint - Print a darts remark for a key.
      *------------------------------------------------------------------------
     D DartPrint       pr                  extpgm('SC718R')
     D  key                          40a
     D  queue                        10a
     D  mode                          1a
     d*** begin add *** 07/11/05 *******************************************
     D  outt                          1a
     D  frdt                          7a
     D  frdt                          7a
     d*** end   add *** 07/11/05 *******************************************

      *------------------------------------------------------------------------
      * CoverPrint - Print a cover page.
      *------------------------------------------------------------------------
     D CoverPrint      pr                  extpgm('SC981R1')
     D  type                          1a
     D  company                       3a
     ***subscriber                    5a
     D  mc_group                      3a
     D  pol_year                      3a
     D  case                          7a
     D  employer                      9a
     d*** begin add *** 07/11/05 *******************************************
     D  outt                          1a
     D  frdt                          7a
     D  todt                          7a
     d*** end   add *** 07/11/05 *******************************************

      *------------------------------------------------------------------------
      * DocPrint - Print the docs attached to darts remarks
      *------------------------------------------------------------------------
     D DocPrint        pr                  extpgm('SC982R')
     D  key                          40a
     D  outq                         10a
     D  return_code                   1a
     d*** begin add *** 07/11/05 *******************************************
     D  outt                          1a
     D  frdt                          7a
     D  todt                          7a
     d*** end   add *** 07/11/05 *******************************************

      *------------------------------------------------------------------------
      * ImagePrint - Print the image docs attached to darts remarks
      *------------------------------------------------------------------------
     D ImagePrint      pr                  extpgm('SC983C')
     D  company                       3a
     D  group                         3a
     D  employer                      9a
     D  year                          3a
     D  case                          7a
     D***  begin add  ***  11/27/02  ***************************************
     D  psid                          1a
     D***  end   add  ***  11/27/02  ***************************************
     d*** begin add *** 07/11/05 *******************************************
     D  outt                          1a
     D  frdt                          7a
     D  todt                          7a
     d*** end   add *** 07/11/05 *******************************************

