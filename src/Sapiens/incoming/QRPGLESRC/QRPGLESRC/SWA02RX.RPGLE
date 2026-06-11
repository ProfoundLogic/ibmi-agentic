     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE SWG02Rx - WC Agent    Name Search/Return  Headings
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns a list of Agent     Headings                               .  *
     *         FOr Agent Name Search                                          *
      *
     * CALLED BY : WebApps                                                    *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   :                                                            *
     * PROGRAMMER: C:02                                                       *
     * PROJECT   :                                                            *
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
     *========================================================================*
     * Constant Definitions
     *========================================================================*
     *========================================================================*
     * Array, Table & Data Structure Definitions
     *========================================================================*

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D @@Keys          S             15a
     D @@DT1s          S             15a
     D @@DT2s          S             15a
     D @@SS#           S             15a
     D @@Misc          S             15a
     D @@RtnCode       S              1a



     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    @@HD1            60
     C                   Parm                    @@HD2            60
     C                   Parm                    @@Keys
     C                   Parm                    @@DT1s
     C                   Parm                    @@DT2s
     C                   Parm                    @@SS#
     C                   Parm                    @@Misc
     C                   Parm                    @@RtnCode
     *========================================================================*
     * KLIST definitions
     *========================================================================*
     *========================================================================*
     * Main
     *========================================================================*



     C                   eval      @@HD1 = ('WC AGENT LINK')
     C                   eval      @@HD2 = ('Select Agent to view +
     C                             DARTS Documents')
     C                   Eval      @@Keys = ('Agent #')
     C                   eval      @@DT1s = *blanks
     C                   eval      @@Dt2s = *blanks
     C                   eval      @@SS#  = *blanks
     C                   eval      @@Misc = *blanks
     C                   eval      @@RtnCode = *blanks


     C                   return
     C                   Eval      *inLR = *on
