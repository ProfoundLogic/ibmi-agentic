     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
      * PSTRINC - Includes for PSTRHANDLE module                               *
     *========================================================================*
     * REVISIONS:                                                             *
      *                                                                        *
     *========================================================================*
     *========================================================================*
     * Field Definitions                                                      *
     *========================================================================*
      *  Date check/conversion fields
     D dt_CYMD         C                   Const('*CYMD   ')
     D dt_MDYY         C                   Const('*MDYY   ')
     D dt_MDY          C                   Const('*MDY    ')
     D dt_YMD          C                   CONST('*YMD    ')
     D dt_YYMD         C                   CONST('*YYMD   ')
     D BLNKS           C                   CONST(*blanks)
     D date_in         S             10a

     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*

     *------------------------------------------------------------------------*
     * CvtDate                                                                *
     *------------------------------------------------------------------------*
      * Description: Converts date formats.                                    *
      *                                                                        *
     * Parameters : 8.0 - date to convert                                     *
      *               8a - date from format                                    *
      *               8a - date to format                                      *
      *                                                                        *
      * Returns    : 10a - Converted date, or blank for error                  *
     *------------------------------------------------------------------------*
     D CvtDate         PR            10a
     D  date_in                       8  0 Const
     D  from_format                   8a   Const
     D  to_format                     8a   Const


     *------------------------------------------------------------------------*
     * Center                                                                 *
     *------------------------------------------------------------------------*
      * Description: Center text in field.                                     *
      *                                                                        *
     * Parameters : 1024a - string to center                                  *
      *                                                                        *
      * Returns    : 1024a - centered string                                   *
     *------------------------------------------------------------------------*
     D Center          PR          1024a   opdesc
     D  @@string                   1024a   CONST

