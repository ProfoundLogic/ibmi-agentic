     D*=====================================================================
     D* Work-withs processing procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* readCheck - displays a panel asking user how to continue when few
     P*             records have met the subset criteria
     P*  Parameter 1 - 7 numeric (number of reads)
     P*  Parameter 2 - 3 numeric (number of selects)
     P*  Returns - character field ('1' to '5' that user entered)
     P*=====================================================================
     P* Revisions:
     P* 07/29/16 C: 079 Support: 42622/099
     P*                 Remove OVRFILE
     P*=====================================================================
     DreadCheck        PR             1
     D numOfReads                     7  0 const
     D numOfSels                      3  0 const
     D
     P*=====================================================================
     P*  ovrFile - override file to use SHARE(*YES)
     P*  Parameter 1 - 10 character (file name)
     P*  Returns - nothing
     P*=====================================================================
     D*** Beg Del *** 07/29/16 ****************************************
     D********ovrFile          PR
     D******** fileName                      10    Const
     D*** End Del *** 07/29/16 ****************************************
     P*=====================================================================
     P*  opnQryFile - execute OPNQRYF command
     P*  Parameter 1 - 10 character (file name)
     P*  Returns - nothing
     P*=====================================================================
     DopnQryFile       PR
     D fileName                      10    Const
     D qrySelect                    512    Const
     D
     P*=====================================================================
     P*  closeFile - close file ID opened by OPNQRYF
     P*  Parameter 1 - 10 character (file name)
     P*  Returns - nothing
     P*=====================================================================
     DcloseFile        PR
     D fileName                      10    Const
     D
