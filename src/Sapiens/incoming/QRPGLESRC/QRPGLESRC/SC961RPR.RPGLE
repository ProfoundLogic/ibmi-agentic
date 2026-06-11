    H*========================================================================
    H* (C) Copyright Progressive Data Solutions, Inc., 2003
    H*========================================================================
    H* SC961RPR - Prototypes
    H*========================================================================
     H* Created on 06/23/2003 by c:62 for Project 129WAO
     H*========================================================================
     H* REVISIONS:
     H*
     H*========================================================================

      *========================================================================
      * Procedure Prototypes
      *========================================================================

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
     D DartPrint       pr                  extpgm('SC708R')
     D  key                          40a
     D  queue                        10a
     D  mode                          1a
     D  from_date                     7a
     D  to_date                       7a
     D  output_type                   1a
     D  query1                      478a

      *------------------------------------------------------------------------
      * CoverPrint - Print a cover page.
      *------------------------------------------------------------------------
     D CoverPrint      pr                  extpgm('SC961R1')
     D  type                          1a
     D  company                       3a
     D  mc_group                      3a
     D  pol_year                      3a
     D  case                          7a
     D  employer                      9a
     D  from_date                     7a
     D  to_date                       7a
     D  output_type                   1a
     D  queue                        10a

      *------------------------------------------------------------------------
      * DocPrint - Print the docs attached to darts remarks
      *------------------------------------------------------------------------
     D DocPrint        pr                  extpgm('SC962C2')
     D  key                          40a
     D  outq                         10a
     D  return_code                   1a
     D  from_date                     7a
     D  to_date                       7a
     D  output_type                   1a
     D  summcodes                    56a
     D  rmrkcodes                   210a

      *------------------------------------------------------------------------
      * ImagePrint - Print the image docs attached to darts remarks
      *------------------------------------------------------------------------
     D ImagePrint      pr                  extpgm('SC963C')
     D  company                       3a
     D  group                         3a
     D  employer                      9a
     D  year                          3a
     D  case                          7a
     D  psid                          1a
     D  from_date                     7a
     D  to_date                       7a
     D  output_type                   1a
     D  summcodesi                   57a
     D  rmrkcodesi                  211a
