     *=====================================================================
     *  (C) Copyright Progressive Data Solutions, Inc., 2000
     *=====================================================================
    H/TITLE IS814R   GetWsInf
     *=====================================================================
     *
     * Retrieves the WS Record for this scan station.
     *
     * Created   : 07/03/00
     * Programmer: C:113
     * Project   : DR113
     *
     *=====================================================================
     *  REVISIONS:
     *
     *=====================================================================
     * Work station ID file
     FIMWSL2    IF   E           K Disk

     *=====================================================================
     * Data Structure & Array Definitions
     *=====================================================================

     *=====================================================================
     * Stand Alone Field Definitions
     *=====================================================================
     D eJobName        s             10
     D eRtnCode        s              1
     D eImgDrive       s              1
     D eWrkStn         s              2
    
     *=====================================================================
     * KList Definitions
     *=====================================================================
     C     wsKey         klist
     C                   kfld                    eJobName

     *=====================================================================
     * PList Definitions
     *=====================================================================
     ** ENTRY PARAMETERS
     C     *entry        plist
     C                   parm                    eJobName
     C                   parm                    eWrkStn
     C                   parm                    eImgDrive
     C                   parm                    eRtnCode

     *=====================================================================
     * Main
     *=====================================================================

     C     wsKey         chain     pws01                              60

     C                   if        *in60 = *off
     C                   movel     wsidrv        eImgDrive
     C                   movel     wswsid        eWrkStn
     C                   clear                   eRtnCode

     C                   else
     * ERROR - This Workstation not configured for Imaging
     C                   clear                   eImgDrive
     C                   clear                   eWrkStn
     C                   eval      eRtnCode = '1'

     C                   endif
    
     C                   return
