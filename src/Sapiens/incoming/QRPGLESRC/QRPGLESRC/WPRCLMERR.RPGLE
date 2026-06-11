     h**********************************************************************
     h* Revisions:
     h*
     h*  04/10/24 C:079 Ticket: 146/115349
     h*                 Add claimdtl pointer to ErrorCheck3
     h*
     h*  09/06/22 C:079 Project: WC2823
     h*                 FROI field updates
     h*
     h*  12/01/14 C:279 Project: 114WLF
     h*                 Added procedure EdtRsvChgs.
     h*
     h*  06/10/15 C:136 Project: 114WLF
     h*                 Refine claim edits so that they are index
     h*                 specific when changing reserves
     h*
     h**********************************************************************

     d* Proto-types

     d* Error Check - Claim Information
     d ErrorCheck0     pr              n
     d claimRrnIn                     9  0
     d claimPtrIn                      *

     d* Error Check - Management Information
     d ErrorCheck1     pr              n
     d claimRrnIn                     9  0
     D mgmtPtrIn                       *
     D mode                           1

     d* Error Check - Medical Information
     d ErrorCheck2     pr              n
     d claimRrnIn                     9  0
     D medicalPtrIn                    *

     d* Error Check - Benefit Information
     d ErrorCheck3     pr              n
     d claimRrnIn                     9  0
     D benefitPtrIn                    *
     D*** Beg Add *** 04/10/24 *********************************************
     D claimPtrIn                      *
     D*** End Add *** 04/10/24 *********************************************

     d* Error Check - Legal Information
     d ErrorCheck4     pr              n
     d claimRrnIn                     9  0
     D legalPtrIn                      *

     d* Error Check - State Information
     d ErrorCheck5     pr              n
     d claimRrnIn                     9  0
     d*** Beg Add *** 09/06/22 ********************************************
     D statePtrInA1                    *
     d*** End Add *** 09/06/22 ********************************************
     D statePtrIn                      *
     D stateIn                        2

     d* Error Check - Quick diary
     d ErrorCheck6     pr              n
     d claimRrnIn                     9  0
     D diaryPtrIn                      *

      ******** beg add ********** 12/01/14 ************************************
     d EdtRsvChgs      pr
     d inRrnWMCMP                     9  0
     d inArrRsvCod                    2    dim(10)
     d inArrRsvChg                    9s 2 dim(10)
      *** beg chg ***** 06/10/15 *********************
     d*inRrnRtnErr*****               1    dim(25)
     d inRrnRtnErr                    1    dim(45)
      *** end chg ***** 06/10/15 *********************
      ******** end add ********** 12/01/14 ************************************
