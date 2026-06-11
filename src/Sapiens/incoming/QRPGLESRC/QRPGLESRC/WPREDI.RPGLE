     D*=====================================================================
     D* EDI processing procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* IsEdi - Checks to see if EDI claim
    P*
    P*  IsEdi      - is this an EDI claim?
    P*   claimRrn  - Claim RRN                                   (required)
    P*====================================================================
     DIsEdi            PR              n
     D claimRrn                       9  0 const
    P*====================================================================
    P* ExitEdi148 - Checks to see if we should exit from 148 processing
    P*
    P*  ExitEdi148 - Exit Edi
    P*   claimRrn  - Claim RRN                                   (required)
    P*   editReason- Reason to Exit
    P*====================================================================
     DExitEdi148       PR              n
     D claimRrn                       9  0 const
     D exitReason                    80
    P*====================================================================
    P* ExitEdiA49 - Checks to see if we should exit from A49 processing
    P*
    P*  ExitEdiA49 - Exit Edi
    P*   claimRrn  - Claim RRN                                   (required)
    P*   editReason- Reason to Exit
    P*====================================================================
     DExitEdiA49       PR              n
     D claimRrn                       9  0 const
     D exitReason                    80
    P*====================================================================
    P* ShowEdi148 - Checks to see if 148 Change screen should display
    P*
    P*  ExitEdi148 - Display EDI
    P*   claimRrn  - Claim RRN                                   (required)
    P*====================================================================
     DShowEdi148       PR              n
     D claimRrn                       9  0 const
    P*====================================================================
    P* EdiScrCtrl - Return field screen control information for EDI fields
    P*====================================================================
     DEdiScrCtrl       PR
     d   inCompany#                   3  0
     d   inJurState                   2
     d   inTranID                     3
     d   inFormatId                   3
     d   outD433                      1
     d   outD440                      1
     d   outD441                      1
     d   outD441D                     7  0
     d   outD442                      1
     d   outRetCode                   1
