     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
      * DA301R - Remark code validation
      * Service Program: DAVALID
      *
      * Created   : 04/20/00
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

     *========================================================================
      * File Definitions
     *========================================================================
     Fstctl1    if   e           k disk    usropn
    
    
     *========================================================================
      * Prototypes
     *========================================================================
      /copy DPRVALID
    
      *------------------------------------------------------------------------
      * ValidRmkCd - Validate a remark code
      *------------------------------------------------------------------------
     P ValidRmkCd      B                   export

      *** Procedure Interface ***
     D ValidRmkCd      PI             1a
     D  eRmkCode                      5a   const
     D  eRmkDesc                     40a
    
      *** Locals ***
      * Key list for Remark Code Table File
     C     ctkey         klist
     C                   kfld                    eRmkCode
    
      *** Procedure Definition ***
      * Open needed files
     C                   open      stctl1

      * Validate code and get description
     C     ctkey         chain     pct01                              99

     C                   if        NOT *in99
     C                             and ctstat = 'A'
     C                   movel(p)  ctdesc        eRmkDesc

      * Close files and return success
     C                   close     stctl1
     C                   return    *on

     C                   else
     C                   eval      eRmkDesc = *blanks

      * Close files and return error
     C                   close     stctl1
     C                   return    *off

     C                   endif
    
     P ValidRmkCd      E
