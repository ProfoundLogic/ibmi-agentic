     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
     * DA501R - Retrieve Error Codes
     * Service Program: DAERROR
     *
     * Created   : 11/20/00
     * Programmer: C:113
     * Project   : DR:113
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
     * NOTES:
     *
     *
     *========================================================================
     H nomain

     *========================================================================
     * File Definitions
     *========================================================================
     Fimerp     if   e           k disk    usropn
    
    
     *========================================================================
     * Prototypes
     *========================================================================
      /copy DA501RPR
    
     *========================================================================
     * GetErrCode   Get an error code
     *========================================================================
     P GetErrCode      B                   export

     *** Procedure Interface ***
     D GetErrCode      PI            80a
     D  eErrCode                      8a   const
     D  eCategory                    10a   options(*NOPASS)
     D  eSubCat                      10a   options(*NOPASS)
    
     *** Locals ***
     * Error Code message
     D     rtnMsg      S             80a

     * Key list for Error Code File
     c     erkey         klist
     c                   kfld                    eErrCode
    
     *** Procedure Definition ***
     * Open needed files
     c                   open      imerp

     * Validate code and get description
     c     erkey         chain     per01                              99

     c                   if        NOT *in99
     c                   movel(p)  ermsg         rtnMsg

     c                   select

     c                   when      %parms = 2
     c                   eval      eCategory = ercat

     c                   when      %parms = 3
     c                   eval      eCategory = ercat
     c                   eval      eSubCat = erscat

     c                   endsl

     c                   else
     c                   eval      rtnMsg = *blanks
     c                   endif

     * Close files and return success
     c                   close     imerp
     c                   return    rtnMsg

     P GetErrCode      E
