     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
      * DPRVALID - Validation procedure prototypes
      *
      * Created   : 03/31/00
      * Programmer: C:113
      * Project   : DARTS
      *
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
     * NOTES:
     *
     *
     *========================================================================

      *------------------------------------------------------------------------
      * ValidUser - Validates a given user
      *------------------------------------------------------------------------
      * Returns:     1.0  1 for valid; 0 for invalid
      *
      * Parameters:  10a  User to validate
      *              9.0  RRN of user record
      *------------------------------------------------------------------------
      ** Procedure Prototype
     D ValidUser       PR             1  0
     D  user                         10a   Const
     D  rrn                           9  0

      *------------------------------------------------------------------------
      * ValidWCemp - Validates a workers' comp employer
      *------------------------------------------------------------------------
      * Returns:     1.0  1 for valid; 0 for invalid
      *
      * Parameters:  3.0  company#
      *              3.0  group
      *              9.0  employer#
      *              5.0  division
      *              9.0  rrn
      *
      *------------------------------------------------------------------------
      ** Procedure Prototype
     D ValidWCemp      PR             1  0
     D  company                       3  0 Const
     D  group                         3  0 Const
     D  employer                      9  0 Const
     D  division                      5  0 options(*nopass)
     D  rrn                           9  0 options(*nopass)

      *------------------------------------------------------------------------
      * ValidRmkCd
      *------------------------------------------------------------------------
      * Description: Validates a DARTS remark code and retrieves description
      *
      * Parameter 1:  5a   Remark code to validate
      *              20a   Returned description
      *
      * Returns:     1a   *on for valid; *off for invalid
      *------------------------------------------------------------------------
      ** Procedure Prototype
     D ValidRmkCd      PR             1a
     D  remarkcode                    5a   Const
     D  description                  40a

