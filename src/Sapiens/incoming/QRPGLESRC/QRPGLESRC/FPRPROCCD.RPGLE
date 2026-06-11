     h*=====================================================================
     h* Managed Care procedure prototypes
     h*=====================================================================
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************

     D* prototypes
    D*
    P*====================================================================
    P* GetProcCdDes - Get procedure code description
    P*
    P*  GetProcCdDes - returns description for procedure code
    P*   billtype - accepts the type of bill
    P*   proccode - accepts the procedure code to be searched
    P*   adjstate - accepts the adjudication state for the bill
    P*   servdate - accepts the service date for the bill/code
    P*   type     - accepts the service type for the bill/code
    P*   Med#     - accepts the Medicare Number for the bill/code
    P*   desc     - returns the description for the procedure code
    P*====================================================================
    P*
     DGetProcCdDes     PR            25
     D billtype                       1    const
     d*** Beg Chg *** 02/16/18 ********************************************
     D*** proccode                      12    const
     D proccode                      25    const
     d*** End Chg *** 02/16/18 ********************************************
     D adjstate                       2    const
     D servdate                       7S 0 const
     d*** Beg Add *** 02/16/18 ********************************************
     D type                           1    const
     D Med#                          10    const
     d*** End Add *** 02/16/18 ********************************************
     D desc                          25
    P*
    P*====================================================================
