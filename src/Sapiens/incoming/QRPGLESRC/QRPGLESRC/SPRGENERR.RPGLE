      *********************************************************************
      * REVISIONS:
     *
      * 07/21/16 C:65  Project: SY371
      *                Add Valid_Date, Valid_Document, Valid_Path procedures.
     *                Valid_Date is a copy of the procedure that exists in
     *                WPIPOLERR.  Valid_Document and Valid_Path procedures
     *                are used to validate scriptura document names and
     *                directory paths.  Add CheckEmailList procedure to
     *                verify a list of emails is valid.
     *
      * 10/11/14 C:279 Support: OP136 86962
      *                Added E-Mail addrees format valadation procedure.
     *
      *********************************************************************
     D*=====================================================================
     D* Generic error procedures
      *=====================================================================
      * Check_Phone# - Check to verify If Phone # has length 10.
      *  Parameter 1 - Phone #
      *  Parameter 2 - Type (Phone = 'P', Cell = 'C', Fax = 'F')
      *  Parameter 3 - messageID
      *=====================================================================
     d Check_Phone#    pr              n
     d   inPhone#                    10  0 Const
     d   inType                       1
     d   inMessage                  150

      ********* beg add ********** 10/11/14 ********************************
      *=====================================================================
      * Check_Email  - Check to verify If Email address is valid
      *  Parameter 1 - Email
      *  Parameter 2 - messageText
      *=====================================================================
     d Check_Email     pr              n
     d   inEmail                     60
     d   inMessage                  200
      ********* end add ********** 10/11/14 ********************************
     d*** begin add ***  07/21/16  *****************************************
    P*====================================================================
    P* CheckEmailList - Verify if string is valid list of one or more emails.
    P*
    P*   string    - passed in for validation                  (required)
    P*   message   - returns error message                     (required)
    P*
    P*====================================================================
    P*
     DCheckEmailList   PR              n
     D string                     32766    options(*varsize) const
     D message                      200
    D*
    D*
    P*====================================================================
    P* Valid_Date - Verify if date passed in is valid and return.
    P*
    P*   date      - passed in for validation                  (required)
    P*   messageID - returns error message ID                  (required)
    P*
    P*====================================================================
    P*
     DValid_Date       PR             1
     D date                           8  0
     D messageID                    100
    D*
    D*
    P*====================================================================
    P* Valid_Document - Verify if Scriptura document name passed in is valid and return.
    P*
    P*   co#       - passed in for validation                  (required)
    P*   type      - passed in for validation                  (required)
    P*   document  - passed in for validation                  (required)
    P*   message   - returns error message                     (required)
    P*
    P*====================================================================
    P*
     DValid_Document   PR             1
     D co#                            3  0 const
     D type                           1    const
     D document                      60    const
     D message                      100
    D*
    P*====================================================================
    P* Valid_Path - Verify if document path passed in is valid and return.
    P*
    P*   path      - passed in for validation                  (required)
    P*   message   - returns error message                     (required)
    P*
    P*====================================================================
    P*
     DValid_Path       PR             1
     D path                         250    const
     D message                      100
    D*
     d*** end add *****  07/21/16  *****************************************
