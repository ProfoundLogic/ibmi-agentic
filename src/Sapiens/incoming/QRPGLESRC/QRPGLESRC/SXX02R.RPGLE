     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE S??02R -
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of             Master File starting at the incoming .  *
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
     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*
     DKeys             S             40a   DIM(5)
     DNams             S             40a   DIM(5)
     DDT1s             S              8a   DIM(5)
     DDT2s             S              8a   DIM(5)
     DSS#              S              9a   DIM(5)
     DMisc             S             40a   DIM(5)

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @@NmKy          S             40a
     D @@Keys          S            200a
     D @@Nams          S            200a
     D @@DT1s          S             40a
     D @@DT2s          S             40a
     D @@SS#           S             45a
     D @@Misc          S            200a
     D @@RtnCode       S              1a

     D count           S              2s 0
     D SavRtn          S              1a



     D*xxName          S                   Like(??NAME)

     D                 DS
     D xxKey                         40a
     D xxCompany                      3a   Overlay(xxKey:1)
     D xxGroup                        3a   Overlay(xxKey:4)
     D xxEmployer                     9a   Overlay(xxKey:7)
     D xxDivision                     5a   Overlay(xxKey:16)
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @@NmKy
     C                   Parm                    @@Keys
     C                   Parm                    @@Nams
     C                   Parm                    @@DT1s
     C                   Parm                    @@DT2s
     C                   Parm                    @@SS#
     C                   Parm                    @@Misc
     C                   Parm                    @@RtnCode
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     C*
     *========================================================================*
     * Main
     *========================================================================*



     C                   Eval      *inLR = *on
     *========================================================================*
     * *inzsr - Initialization subroutine
     *========================================================================*
     C     *inzsr        BEGSR

     C                   Eval      @@rtncode = *blanks

     C                   Eval      @@keys = *blanks
     C                   Eval      @@Nams = *blanks
     C                   Eval      @@Dt1s = *blanks
     C                   Eval      @@Dt2s = *blanks
     C                   Eval      @@SS#  = *blanks
     C                   Eval      @@Misc  = *blanks

     C                   ENDSR                                                  *inzsr

