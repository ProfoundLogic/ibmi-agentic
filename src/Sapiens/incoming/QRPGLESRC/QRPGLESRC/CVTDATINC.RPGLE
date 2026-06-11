     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * XX/XX/XXXX C:XXX PROJECT: XXXXX                                        *
     *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     *========================================================================*
     * CvtDate                                                                *
     *========================================================================*
      * Description: Converts date formats.                                    *
      *                                                                        *
     * Parameters : 8.0 - date to convert                                     *
      *              8a - date from format                                     *
      *              8a - date to format                                       *
      *                                                                        *
      * Returns    :10a - Converted date, or blank for error                   *
     *========================================================================*
      ** Procedure Prototype
     D CvtDate         PR            10a
     D  date_in                       8  0 Const
     D  from_format                   8a   Const
     D  to_format                     8a   Const

      ** Date check/conversion fields
     D dtCYMD          C                   Const('*CYMD   ')
     D dtMDYY          C                   Const('*MDYY   ')
     D dtMDY           C                   Const('*MDY    ')
     D dtYMD           C                   CONST('*YMD    ')
     D dtYYMD          C                   CONST('*YYMD   ')
