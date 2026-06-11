     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE IDRTMR2-Returns Name Listings for search/Table Returns
     *========================================================================*
      *

     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of  Master Files starting at the incoming           .  *
     *  name.  If rtncode pass in is "A" (1st time) it returns the first      *
     *  5 including the Originator.  If "B" (subsequent calls) then the inc   *
     *  incoming name is skipped, and the next 5 are returned.                *
      *
     * CALLED BY : Vdarts - Visual Darts                                      *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 01/18/2000                                                 *
     * PROGRAMMER: C:02                                                       *
     * PROJECT   : DR086                                                      *
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
     * Indicator Usage                                                        *
     *                                                                        *
     *  60 - General Chain indicator.                                         *
     *  61 - General Chain indicator.                                         *
     *                                                                        *
     *======================================================================= *
      **
     F*         IF   E           K DISK
     *========================================================================*
     * Constant Definitions
     *========================================================================*

      * Note that the CONST keyword is not required.
     D UP              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

      * Note that the literal may be continued on the next line
     D LO              C                   'abcdefghijklmn-
     D                                     opqrstuvwxyz'

     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @@KTYP          S              2a
     D @@NmKy          S             40a
     D @@Keys          S            200a
     D @@Nams          S            200a
     D @@DT1s          S             40a
     D @@DT2s          S             40a
     D @@SS#           S             45a
     D @@Misc          S            200a
     D @@RtnCode       S              1a

     D cPnam           S              6a
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
      ** Call Parm List
     C     ParmL         PList
     C                   Parm                    @@NmKy
     C                   Parm                    @@Keys
     C                   Parm                    @@Nams
     C                   Parm                    @@DT1s
     C                   Parm                    @@DT2s
     C                   Parm                    @@SS#
     C                   Parm                    @@Misc
     C                   Parm                    @@RtnCode

     C     *entry        PList
     C                   Parm                    @@NmKy
     C                   Parm                    @@Keys
     C                   Parm                    @@Nams
     C                   Parm                    @@DT1s
     C                   Parm                    @@DT2s
     C                   Parm                    @@SS#
     C                   Parm                    @@Misc
     C                   Parm                    @@RtnCode
     C                   Parm                    @@KTYP
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C*
     *========================================================================*
     * Main
     *========================================================================*
      *
      ** rtncode in is A(1sttime NAME) or B(othertimes NAME)
      ** OR ** C(1sttime KEY) or D(othertimes KEY)
      *
     C     LO:UP         XLATE     @@ktyp        @@ktyp                 90
     c****
     C                   if        @@rtncode = 'A' or @@rtncode = 'B'
     C                   eval      cPnam  = 'S' + @@ktyp + '02R'
     c                   else
     C                   eval      cPnam  = 'S' + @@ktyp + '03R'
     c                   endif

     C                   Select
     c                   when      @@rtncode = 'C'
     c                   eval      @@rtncode = 'A'
     c                   when      @@rtncode = 'D'
     c                   eval      @@rtncode = 'B'
     c                   ENDSL

     c                   CALL      cpnam         parmL                  99
      * if error ??? what ??? return an error code


     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   ENDSR                                                  *inzsr

