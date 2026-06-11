     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
    *===============================================================*
    *  Created   : 03/15/13                                         *
    *  Programmer: C:190                                            *
    *  Project   : 147WAZ                                           *
    *===============================================================*
    H*********************************************************************
    H*  Revisions:                                                       *
     H*
     H* 11/01/16  C:33  Project: WC2405
     H*                 Add procedure GetSysGenDesc to get description
     H*                  from generic table
    H*                                                                   *
    H*  04/12/13 C:33  Project: WC2115  #2636                            *
    H*                 Add CMSInstalled, EDIInstalled & EFTInstalled     *
    H*                                                                   *
    H*  04/05/13 C:279 Redmine: 2577                                     *
    H*                 Added procedure to check if Claims Only           *
    H*********************************************************************
     Fstgtl     if   e           k Disk
     F******** Beg Add ******* 04/05/13 *************************************
     F***  Begin Chg  ***  04/12/13  *************************************
     F**********ssylf     if   e           k disk
     F**********                           ignore(ssy01)
     F**********                           ignore(spc01)
     F**********                           ignore(sfb01)
     F**********                           ignore(src01)
     F**********                           ignore(sdr01)
     F**********                           ignore(sbc01)
     F**********                           ignore(scs01)
     F**********                           ignore(sgl01)
     F**********                           ignore(sp201)
     Fssylf     if   e           k disk    usropn
     F***  End   Chg  ***  04/12/13  *************************************
     F******** End Add ******* 04/05/13 *************************************
     D*
     D/copy sprtable
     D*
     P***  Begin Add  ***  04/12/13  *************************************
     P*====================================================================
     P* Determine if CMS Module is installed
     P*====================================================================
     PCMSInstalled     b                   export
     DCMSInstalled     pi              n

      /free
        If RtvSysCtrlRcd('SY') and %subst(SYfl12:2:1)='Y';
           Return *on;
        Endif;

        Return *off;
      /End-free

     PCMSInstalled     e
     P*
     P*====================================================================
     P* Determine if EDI Module is installed
     P*====================================================================
     PEDIInstalled     b                   export
     DEDIInstalled     pi              n

      /free
        If RtvSysCtrlRcd('WC') and WCfl09='Y';
           Return *on;
        Endif;

        Return *off;
      /End-free

     PEDIInstalled     e
     P*
     P*====================================================================
     P* Determine if EFT Module is installed
     P*====================================================================
     PEFTInstalled     b                   export
     DEFTInstalled     pi              n

      /free
        If RtvSysCtrlRcd('WC') and WCfl35='Y';
           Return *on;
        Endif;

        Return *off;
      /End-free

     PEFTInstalled     e

       //***  Begin Add  ***  11/01/16  **********************************
    P*====================================================================
    P* GetSysGenDesc- Get generic code description
    P*  Returns  - 40A descriptio
    P*   co#       - accepts the company # (required)
    P*   tbid      - accepts the table id (required)
    P*   key/code  - key or code value (required)
    P*====================================================================
     PGetSysGenDesc    b                   export
     DGetSysGenDesc    pi            40
     D co#                            3  0 const
     D tbid                           4    const
     D key                           10    const

      /free
       Clear GTdesc;
       Chain (co#:tbid:key) pgt01;
       Return GTdesc;
      /end-free
     PGetSysGenDesc    e
       //***  End   Add  ***  11/01/16  **********************************
     P*
     P*====================================================================
     P* Retrieve system control file record
     P*====================================================================
     PRtvSysCtrlRcd    b
     DRtvSysCtrlRcd    pi              n
     D inKey                          2    const

      /free
        Open ssylf;

        Select;
        When inKey = 'BC';
           Chain inKey SBC01;
        When inKey = 'CS';
           Chain inKey SCS01;
        When inKey = 'DR';
           Chain inKey SDR01;
        When inKey = 'FB';
           Chain inKey SFB01;
        When inKey = 'GL';
           Chain inKey SGL01;
        When inKey = 'PC';
           Chain inKey SPC01;
        When inKey = 'P2';
           Chain inKey SP201;
        When inKey = 'RC';
           Chain inKey SRC01;
        When inKey = 'SY';
           Chain inKey SSY01;
        When inKey = 'WC';
           Chain inKey SWC01;
        Endsl;

        Close ssylf;

        Return %found;
      /End-free

     PRtvSysCtrlRcd    e
     P***  End   Add  ***  04/12/13  *************************************
     P*
     P*====================================================================
     P* Determine if Valid Key is found
     P*====================================================================
     PValidSysGenKey   b                   export
     DValidSysGenKey   pi             1n
     D inco#                          3  0 const
     D inTableId                      4    const
     D inKey                         10    const

     D isFound         s              1n

      /free
        Chain(N) (inco#:inTableId:inkey) stgtl;
        Select;
        When %Found;
           isFound = *On;
        Other;
           isFound = *Off;
        Endsl;
        Return isFound;
      /End-free
     PValidSysGenKey   e

     P*====================================================================
     P* Determine if Valid Key is found
     P*====================================================================
     PSearchSysGenKey  b                   export
     DSearchSysGenKey  pi             9  0
     D inco#                          3  0 const
     D inTableId                      4    const

     DS2380R           PR                  extpgm('S2380R')
     D ##call                         1    const
     D ##kc                           1    const
     D ##kl                           1    const
     D ##rrn                          9  0
     D ##tbid                         4    const

     D outRrn          s              9  0

      /free
        Callp S2380R('2':'0':'0':outRrn:inTableId);
        Return outRrn;
      /End-free

     PSearchSysGenKey  e
     P******** Beg Add ******* 04/05/13 *************************************
     P*====================================================================
     P* Determine if Claims Only
     P*====================================================================
     PClaimsOnly       b                   export
     DClaimsOnly       pi              n

     D xxKey           s              2    Inz('WC')

      /free
        //***  Begin Chg  ***  04/12/13  **********************************
        //Chain xxKey SWC01;
        //If WCfl08 = 'C';
        If RtvSysCtrlRcd('WC') and WCfl08='C';
        //***  End   Chg  ***  04/12/13  **********************************
           Return *on;
        Endif;
        Return *off;
      /End-free

     PClaimsOnly       e
     P******** End Add ******* 04/05/13 *************************************
