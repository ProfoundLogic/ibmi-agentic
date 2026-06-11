     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
     *
     * DA400R - API to see if an application is installed
     *
     * Created   : 07/25/00
     * Programmer: C:113
     * Project   : DARTS
     *
     *========================================================================
     * REVISIONS:
     *             04/26/2002 - CHANGED CONSTANT "CONTROL" TO BE 'SY'
      *             C:002        INSTEAD OF '01' TO "GET" THE SYSCTL RCD
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================

     *========================================================================
     * File Definitions
     *========================================================================
     Fssylf     if   e           k disk
    

     *========================================================================
     * Prototypes
     *========================================================================
      /copy DA400RINC

     *========================================================================
     * Stand-alone field definitions
     *========================================================================
     D  eAppToGet      s              2a                                        Application
     D  eStatus        s              1a                                        Install status

     *========================================================================
     * PList definitions
     *========================================================================
     c     *entry        plist
     c                   parm                    eAppToGet
     c                   parm                    eStatus

     *========================================================================
     * Main
     *========================================================================
     c                   callp     GetInstApp(eAppToGet:eStatus)
     c                   return
     *========================================================================




     *------------------------------------------------------------------------
     * GetInstApp - Checks to see if an app is installed.  Sends an error
     *              back if the app is not defined.
     *------------------------------------------------------------------------
     P GetInstApp      B                   export

     *** Procedure Interface ***
     D GetInstApp      PI
     D  eAppToGet                     2a   const                                Application
     D  eStatus                       1a                                        Install status

     *** Procedure Definition ***
     c     CONTROL       chain     ssy01                              60

     c                   select

     c                   when      eAppToGet = WCMS
     c                   if        sywcif = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = PCMS
     c                   if        SYPCIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = FSMS_BASE
     c                   if        syFBIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = FSMS_INTER
     c                   if        SYFIIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = RCMS
     c                   if        SYRCIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = DARTS
     c                   if        SYSCIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = VDARTS
     c                   if        SYFL01 = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = BAR_CODE
     c                   if        SYBCIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = CL_SCAN
     c                   if        SYCSIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = OFFICE
     c                   if        SYOFIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = IMAGE
     c                   if        SYIMIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   when      eAppToGet = FAX
     c                   if        SYFXIF = 'Y'
     c                   eval      eStatus = INSTALLED
     c                   else
     c                   eval      eStatus = NOT_INST
     c                   endif
     C                   return

     c                   endsl

     c                   eval      eStatus = ERROR

     C                   return

     P GetInstApp      E

