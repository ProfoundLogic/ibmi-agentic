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
     *
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================
     d BAR_CODE        c                   const('BC')
     d CL_SCAN         c                   const('CS')
     d DARTS           c                   const('DA')
     d ERROR           c                   const('E')
     d FAX             c                   const('FX')
     d FSMS_BASE       c                   const('FS')
     d FSMS_INTER      c                   const('FI')
     d IMAGE           c                   const('IM')
     d INSTALLED       c                   const('1')
     d NOT_INST        c                   const('0')
     d OFFICE          c                   const('OV')
     d PCMS            c                   const('PC')
     d RCMS            c                   const('RC')
     d VDARTS          c                   const('VD')
     d WCMS            c                   const('WC')

     d CONTROL         s              2a   inz('SY')

     *------------------------------------------------------------------------
     * GetInstApp - Checks to see if an app is installed.  Sends an error
     *              back if the app is not defined.
     *------------------------------------------------------------------------
     d GetInstApp      PR
     D  eAppToGet                     2a   const                                Application
     D  eStatus                       1a                                        Install status

