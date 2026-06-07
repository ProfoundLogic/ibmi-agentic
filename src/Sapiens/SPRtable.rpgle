     D*=====================================================================
     D* System Table procedure prototypes
     D*=====================================================================
     P*====================================================================
     P* Determine if key is Valid.
     P*   co#     - accepts the Company # to search over (required)
     P*   tableId - accepts the TableId to search over   (required)
     P*   Key     - accepts the Key to search over   (required)
     P*====================================================================
     DValidSysGenKey   pr             1n
     D inCo#                          3  0 const
     D inTableId                      4    const
     D inKey                         10    const

     P*====================================================================
     P* Search System Key.
     P*   co#     - accepts the Company # to search over (required)
     P*   tableId - accepts the TableId to search over   (required)
     P*   Returns RRN
     P*====================================================================
     DSearchSysGenKey  pr             9  0
     D inCo#                          3  0 const
     D inTableId                      4    const

     P*====================================================================
     P* Validate if Claims Only
     P*   Returns *on - If Claims only
     P*====================================================================
     DClaimsOnly       pr              n

     P*====================================================================
     P* Validate if CMS Module is installed
     P*   Returns *on - If installed
     P*====================================================================
     DCMSInstalled     pr              n

     P*====================================================================
     P* Validate if EDI Module is installed
     P*   Returns *on - If installed
     P*====================================================================
     DEDIInstalled     pr              n

     P*====================================================================
     P* Validate if EFT Module is installed
     P*   Returns *on - If installed
     P*====================================================================
     DEFTInstalled     pr              n

     P*====================================================================
     P* GetSysGenDesc - Get generic code description
     P*  Returns  - 40A descriptio
     P*   co#       - accepts the company # (required)
     P*   tbid      - accepts the table id (required)
     P*   key/code  - key or code value (required)
     P*====================================================================
     DGetSysGenDesc    pr            40
     D co#                            3  0 const
     D tbid                           4    const
     D key                           10    const

     P*====================================================================
     P* Retrieve system control file record
     DRtvSysCtrlRcd    pr              n
     D inKey                          2    const
