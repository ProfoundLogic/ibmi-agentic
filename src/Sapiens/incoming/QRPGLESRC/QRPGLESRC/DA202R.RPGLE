     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
    H/TITLE DA202R - DPIDATE Date API Procedures
     *=====================================================================
     *
     * Created   : 07/03/00
     * Programmer: C:113
     * Project   : DR113
     *
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================
     * REVISIONS:
     *
     * XX/XX/XXXX C:XXX PROJECT: XXXXX
     *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     H nomain

     *========================================================================
      * Procedure Prototypes
     *========================================================================
      /copy DA202RPR


     *=====================================================================
     * GetCurDate - Gets the current date and time
     *=====================================================================
     p GetCurDate      B                   export

     *** Procedure Interface ***
     d GetCurDate      PI
     d  dDate                          d
     d  tTime                          t   options(*nopass)

     *** Locals ***
     d                 ds
     d dsdttm                        14  0
     d  dsTime                        6  0  overlay(dsdttm)
     d  dsDate                        8  0  overlay(dsdttm:7)

     *** Procedure Definition ***
     c                   time                    dsdttm
     c     *usa          move      dsDate        dDate
     c                   if        %parms > 1
     c     *hms          move      dsTime        tTime
     c                   endif
     c                   return

     p GetCurDate      E
