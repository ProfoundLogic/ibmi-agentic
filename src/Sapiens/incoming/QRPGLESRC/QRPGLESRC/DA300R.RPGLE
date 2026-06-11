     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
      * DPIVALID - Validation procedures
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
     H nomain

     Fsmupl     if   e           k disk
     Fwmeml     if   e           k disk
     Fwdell     if   e           k disk

      /copy DPRVALID

      *------------------------------------------------------------------------
      * ValidUser
      *------------------------------------------------------------------------
      * Returns:  1.0     1 for valid; 0 for invalid
      *
      * Parameters:  10a  User to validate
      *               9.0 RRN of user record
      *------------------------------------------------------------------------
     P ValidUser       B                   export

     D ValidUser       PI             1  0
     D  user                         10    Const
     D  rrn                           9  0

     C     user          chain     pup01                              60
     C                   if        *in60 = *off
     C                   return    1
     C                   else
     C                   return    0
     C                   endif

     P ValidUser       E

      *------------------------------------------------------------------------
      * ValidWCemp
      *------------------------------------------------------------------------
      * Returns:  1.0     1 for valid; 0 for invalid
      *
      * Parameters:  3.0  company#
      *              3.0  group
      *              9.0  employer#
      *              5.0  division
      *              9.0  rrn
      *------------------------------------------------------------------------
     P ValidWCemp      B                   export

      *** Procedure Interface ***
     D ValidWCemp      PI             1  0
     D  company                       3  0 Const
     D  group                         3  0 Const
     D  employer                      9  0 Const
     D  division                      5  0 options(*nopass)
     D  rrn                           9  0 options(*nopass)

      *** Locals ***
     C     emkey         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    employer

     C     elkey         klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    employer
     C                   kfld                    division

      *** Procedure Definition ***
     C     emkey         chain     pem01                              60
     C                   if        *in60
     C                   return    0
     C                   else

     C                   if        %parms > 3
     C     elkey         chain     pel01                              60

     C                   if        *in60
     C                   return    0
     C                   else
     C                   return    1
     C                   endif

     C                   else
     C                   return    1
     C                   endif
     C                   endif

     P ValidWCemp      E
