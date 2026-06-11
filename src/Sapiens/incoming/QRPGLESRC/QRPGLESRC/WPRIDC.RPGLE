     D*=====================================================================
     D* NCCI Indemnity Data Call prototypes
     D*=====================================================================
     P*=====================================================================
     P* GetIdcState   - Gets Idc state code
     P*=====================================================================
     P*  Parameters:   3, 0 Company #, 2 State code
     P*  Returns:      2 IDC state code
     P*
     DGetIdcState      PR             2
     D qqCo#                          3  0 Const
     D qqState                        2    Const
      *
     P*=====================================================================
     P* WriteTxLog    - Write NCCI IDC Transactional Log Record
     P*=====================================================================
     P*  Parameters:   RRN
     P*
     DWriteTxLog       PR
     D qqrrn                          9  0 Const
      *
     P*=====================================================================
     P* OpenIdcFiles - Opens Indemnity Data Call Files
     P*=====================================================================
     DOpenIdcFiles     PR
      *
     P*=====================================================================
     P* CloseIdcFiles - Closes Indemnity Data Call Files
     P*=====================================================================
     DCloseIdcFiles    PR
      *
     P*=====================================================================
     P* IsIdcState  - Checks if state reports IDC
     P*  Parameters:   3, 0 Company #, 2 State code
     P*  Returns:      '1' if IDC state
     P*=====================================================================
     DIsIdcState       PR             1n
     D qqCo#                          3  0 Const
     D qqState                        2    Const
      *
     P*=====================================================================
     P* WriteQtLog    - Write NCCI IDC Quaterly Log Record
     P*=====================================================================
     P*  Parameters:   RRN
     P*  Returns:      Ending Code
     P*
     DWriteQtLog       PR
     D qqrrnQt                        9  0 Const
     D qqDate                         8A   Const
      *
      *================================================================
      * IsQtLogFnd    -  Checks if a Quarterly IDC Log record is found.
      *  Parameters   -  3,0 Fund; 3,0 Fund year; 7,0 Case;
      *                  4,0 Processing Yr; 1,0 Processing Qtr.
      *  Returns      - '0' if found, '1' if not found.
      *================================================================
     DIsQtLogFnd       PR             1n
     D qqCo#                          3  0 Const
     D qqFnd                          3  0 Const
     D qqFyr                          3  0 Const
     D qqCase                         7  0 Const
     D qqPyr                          4  0 Const
     D qqPqtr                         1  0 Const
      *================================================================
      * GetPrcQtr     -  Gets the processing Quarter period.
      *  Parameters   -  8, date
      *  Returns      -  Processing Quarter, Month and day.
      *================================================================
     DGetPrcQtr        PR             5A
     D qqInDate                       8A   Const
      *================================================================
      * GetQtrFrmDt   -  Gets the processing Quarter Beginning Date.
      *  Parameters   -  Quater code 1A, Quarter Year
      *  Returns      -  Processing Quarter Beginning Date.
      *================================================================
     DGetQtrFrmDt      PR             7  0
     D qqQtrCode                      1A   Const
     D qqQtrYr                        4A   Const
      *================================================================
      * GetQtrEndDt   -  Gets the processing Quarter End Date.
      *  Parameters   -  Quater code 1A, Quarter Year
      *  Returns      -  Processing Quarter End Date.
      *================================================================
     DGetQtrEndDt      PR             7  0
     D qqQtrCode                      1A   Const
     D qqQtrYr                        4A   Const
      *===============================================================
      *  IsLOEC     -  Gets LOEC flag
      *  Parameters - 2a  State code
      *  Returns    - 3a  LOEC
      *===============================================================
     DIsLOEC           PR             1n
     D qqCo#                          3  0 Const
     D qqState                        2    Const
      *===============================================================
      *  Transfer   - Process transfer for IDC key changes
      *  Parameters - Old key, New Key
      *  Returns    - '1'= Successful transfer, '0'= Failed Transfer
      *===============================================================
     DTransfer         PR             1n
     D qqCo#1                         3  0 Const
     D qqCar1                         5    Const
     D qqPolId1                      18    Const
     D qqPolEfDt1                     8    Const
     D qqClmId1                      12    Const
     D qqAcDt1                        8    Const
     D qqCo#2                         3  0 Const
     D qqCar2                         5    Const
     D qqPolId2                      18    Const
     D qqPolEfDt2                     8    Const
     D qqClmId2                      12    Const
     D qqAcDt2                        8    Const
     D qqFyr2                         3  0 Const
      *===============================================================
      * FormatPolId - Format Policy Id
      *  Parameters - 25 Unformatted Policy Id
      *  Returns    - 18 Formatted Policy Id
      *===============================================================
     DFormatPolId      PR            18
     D qqPolId                       25    Const
