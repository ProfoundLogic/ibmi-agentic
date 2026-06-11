     h**********************************************************************
     h* Revisions:
     h*
     h*  12/06/22 C:291 Project: C152WBZ
     h*                 Add EditAddSettlement
     h*
     h*  10/30/19 C:212 Project: WC2706
     h*                 Add parm for Delete link in Body part grid
     h*
     h*  04/20/15 C:300 Project: WC2043
     h*                 Display Prior ICD code Information Window
     h**********************************************************************

     d* Proto-types

     d* Edit/Add AWW History
     d EditAddAww      pr              n
     d claimRRN                       9  0
     d awwRRN                         9  0
     d addAww                          n
     d edtAww                          n

     d* Edit/Add Benefits Information
     d EditAddBenefit  pr              n
     d claimRRN                       9  0
     d benefitRRN                     9  0
     d addBenefit                      n
     d edtBenefit                      n
     d delBenefit                      n

     d* Edit/Add Diagnosis codes
     d EditAddDiag     pr              n
     d claimRRN                       9  0
     d diagRRN                        9  0
     d addDiag                         n
     d edtDiag                         n
     d*** Beg Add *** 06/03/13 ************************************************
     d deleteDiag                      n
     d*** End Add *** 06/03/13 ************************************************
     d*** Beg Add *** 08/30/13 ************************************************
     d cmsSub                          n
     d*** End Add *** 08/30/13 ************************************************

     d* Edit/Add Earnings Information
     d EditAddEarning  pr              n
     d claimRRN                       9  0
     d earningRRN                     9  0
     d addEarning                      n
     d edtEarning                      n
     d delEarning                      n

     d* Edit/Add Multiple Settlement
     d EditAddMult     pr              n
     d claimRRN                       9  0
     d multRRN                        9  0
     d addMult                         n
     d edtMult                         n

     d* Edit/Add Employer Paid Information
     d EditAddPaid     pr              n
     d claimRRN                       9  0
     d paidRRN                        9  0
     d addPaid                         n
     d edtPaid                         n
     d delPaid                         n

     d* Edit/Add Body Part codes
     d EditAddPart     pr              n
     d claimRRN                       9  0
     d partRRN                        9  0
     d addPart                         n
     d edtPart                         n
     d***** Beg Add *** 10/30/19 ********************************
     d delPart                         n
     d***** End Add *** 10/30/19 ********************************

     d* Edit/Add Treating Physicians
     d EditAddPhys     pr              n
     d claimRRN                       9  0
     d physRRN                        9  0
     d addPhys                         n
     d edtPhys                         n
     d delPhys                         n

     d* Edit/Add Trial Information
     d EditAddTrial    pr              n
     d claimRRN                       9  0
     d trialRRN                       9  0
     d addTrial                        n
     d edtTrial                        n
     d*** Beg Add *** 03/14/14 ************************************************
     d delTrial                        n
     d*** End Add *** 03/14/14 ************************************************

     d* Edit/Add Witness Information
     d EditAddWitness  pr              n
     d claimRRN                       9  0
     d witnessRRN                     9  0
     d addWitness                      n
     d edtWitness                      n
     d delWitness                      n

     d* Edit/Add Work Information
     d EditAddWork     pr              n
     d claimRRN                       9  0
     d workRRN                        9  0
     d addWork                         n
     d edtWork                         n
     d delWork                         n

     d*** Beg Add *** 10/25/12 ************************************************
     d* Edit/Add Work Information for CSI
     d EditAddWrk      pr              n
     d claimRRN                       9  0
     d workRRN                        9  0
     d addWork                         n
     d edtWork                         n
     d delWork                         n
     d*** End Add *** 10/25/12 ************************************************

     d* Error Check AWW History
     d ErrorAww        pr              n
     d edtWork                         n

     d* Error Check Benefits Information
     d ErrorBenefit    pr              n

     d* Error Check Diagnosis Code
     d ErrorDiag       pr              n

     d* Error Check Earnings Information
     d ErrorEarning    pr              n

     d* Error Multiple Settlement
     d ErrorMult       pr              n
     d addMult                         n

     d* Error Check Employer Paid information
     d ErrorPaid       pr              n

     d* Error Check Body Part
     d ErrorPart       pr              n

     d* Error Check Treating Physician
     d ErrorPhys       pr              n

     d* Error Trial Information
     d ErrorTrial      pr              n
     d addTrial                        n

     d* Error Witness Information
     d ErrorWitness    pr              n

     d* Error Work Information
     d ErrorWork       pr              n

     d*** Beg Add *** 10/25/12 ************************************************
     d ErrorWrk        pr              n
     d addWork                         n
     d edtWork                         n
     d*** End Add *** 10/25/12 ************************************************

     d* Update/Add AWW History
     d UpdAddAww       pr
     d claimRRN                       9  0
     d awwRRN                         9  0
     d addAww                          n
     d edtAww                          n

     d* Update/Add Benefit Information
     d UpdAddBenefit   pr
     d benefitRRN                     9  0
     d addBenefit                      n
     d edtBenefit                      n
     d delBenefit                      n

     d* Update/Add Diagnosis Code
     d UpdAddDiag      pr
     d diagRRN                        9  0
     d addDiag                         n
     d edtDiag                         n
     d*** Beg Add *** 06/03/13 ************************************************
     d deleteDiag                      n
     d*** End Add *** 06/03/13 ************************************************

     d* Update/Add Earnings Code
     d UpdAddEarning   pr
     d earningRRN                     9  0
     d addEarning                      n
     d edtEarning                      n
     d delEarning                      n

     d* Update/Add Multiple Settlement
     d UpdAddMult      pr
     d multRRN                        9  0
     d addMult                         n
     d edtMult                         n

     d* Update/Add Employer Paid Information
     d UpdAddPaid      pr
     d paidRRN                        9  0
     d addPaid                         n
     d edtPaid                         n
     d delPaid                         n

     d* Update/Add Body Part
     d UpdAddPart      pr
     d partRRN                        9  0
     d addPart                         n
     d edtPart                         n
     d***** Beg Add *** 10/30/19 ********************************
     d delPart                         n
     d***** End Add *** 10/30/19 ********************************

     d* Update/Add Treating Physician
     d UpdAddPhys      pr
     d physRRN                        9  0
     d addPhys                         n
     d edtPhys                         n
     d delPhys                         n

     d* Update/Add Trial Information
     d UpdAddTrial     pr
     d trialRRN                       9  0
     d addTrial                        n
     d edtTrial                        n
     d*** Beg Add *** 03/14/14 ************************************************
     d delTrial                        n
     d*** End Add *** 03/14/14 ************************************************

     d* Update/Add Witness Information
     d UpdAddWitness   pr
     d witnessRRN                     9  0
     d addWitness                      n
     d edtWitness                      n
     d delWitness                      n

     d* Update/Add Work Information
     d UpdAddWork      pr
     d workRRN                        9  0
     d addWork                         n
     d edtWork                         n
     d delWork                         n

     d**** Beg Add *** 10/25/12 ******************************************
     d* Update/Add Work Information for CSI
     d UpdAddWrk       pr
     d workRRN                        9  0
     d addWork                         n
     d edtWork                         n
     d delWork                         n
     d**** End Add *** 10/25/12 ******************************************

     d* SelChgRes - Change Reserves
     d SelChgRes       pr
     d claimRrn                       9  0
     d status                         1

     d* Claim Updated Window
     d ChkClmUpdated   pr              n
     d claimRRN                       9  0
     d claimDate                      7  0
     d claimTime                      6  0

      ****** beg add ****** 04/20/15 *******************************   ********
       //===================================================================
       // ShowICDWdw - Show Prior ICD Code Information Window
       //===================================================================
     D ShowICDWdw      pr              n
     d claimRRN                       9  0
     d @@StsICD                       1
      ****** end add ****** 04/20/15 *******************************   ********

      *** Beg Add ***** 12/06/22 ***********************************
     d* Edit/Add Settlement Information
     d EditAddSettlement...
     d                 pr              n
     d claimRRN                       9  0
     d settleRRN                      9  0
     d addSettle                       n

     d* Error Settlement Information
     d ErrorSettlement...
     d                 pr              n
     d addSettle                       n
      *** End Add ***** 12/06/22 ***********************************
