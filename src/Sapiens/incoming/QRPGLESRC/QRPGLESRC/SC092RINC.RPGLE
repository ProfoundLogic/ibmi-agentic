     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
     * SC092RINC - Includes for SC092R/SC092R02                               *
     *                                                                        *
     * CREATED   : 01/25/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
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
     * Procedure Prototypes                                                   *
     *========================================================================*
     **************************************************************************
     * CheckObj - External program prototype.                                 *
     **************************************************************************
      * Description: Check for the presence of an object.  Error return is     *
      *              blank for success.                                        *
      *                                                                        *
     * Parameters : 10a - object to check                                     *
      *               7a - type of object                                      *
      *               1a - error return                                        *
      *                                                                        *
      * Returns    : none                                                      *
     **************************************************************************
      ** Procedure Prototype
     D CheckObj        PR                  ExtPgm('SRCHKOBJ')
     D  object                       10a
     D  type                          7a
     D  error                         1a

     **************************************************************************
     * ValProgram                                                             *
     **************************************************************************
      * Description: Validate a given program name.  Returns *on for error,    *
      *              *off for success.                                         *
      *                                                                        *
     * Parameters : 10a - object to check                                     *
      *                                                                        *
      * Returns    :  1a - Error                                               *
     **************************************************************************
      ** Procedure Prototype
     D ValProgram      PR             1a
     D  object                       10a

     *========================================================================*
     * Include Fields                                                         *
     *========================================================================*
     D PgmType         S              7a   Inz('*PGM   ')
     D xxError         S              1a   Inz(*blank)
     D Indexes         C                   Const('Indexes')
     D Types           C                   Const('Key Types')
     D yes             C                   Const('Y')
     D no              C                   Const('N')
