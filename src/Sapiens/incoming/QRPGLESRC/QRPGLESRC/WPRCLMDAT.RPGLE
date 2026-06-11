
     d* Proto-types

     d* Load Claim Info Screen
     d LoadClmInfo     pr
     D claimRrnIn                     9  0
     D claimPtrIn                      *
     D claimFroiIn                    9  0 options(*nopass)

     d* Load Management Info Screen
     d LoadMgmtInfo    pr
     D claimRrnIn                     9  0
     D mgmtPtrIn                       *

     d* Load Medical Info Screen
     d LoadMedInfo     pr
     D claimRrnIn                     9  0
     D medicalPtrIn                    *
     D claimFroiIn                    9  0 options(*nopass)

     d* Load Benefit Info Screen
     d LoadBenInfo     pr
     D claimRrnIn                     9  0
     D benefitPtrIn                    *
     D claimFroiIn                    9  0 options(*nopass)

     d* Load Legal Info Screen
     d LoadLegInfo     pr
     D claimRrnIn                     9  0
     D legalPtrIn                      *

     d* Load State Info Screen
     d LoadStateInfo   pr
     D claimRrnIn                     9  0
     D statePtrIn                      *
     D stateIn                        2

     d* Load Quick Diary Screen
     d LoadQuickDiary  pr
     D claimRrnIn                     9  0
     D diaryPtrIn                      *

     d* Update Claim Info
     d UpdClmInfo      pr
     D claimRrnIn                     9  0
     D claimPtrIn                      *
     D claimFroiIn                    9  0 options(*nopass)

     d* Update Management Info
     d UpdMgmtInfo     pr
     D claimRrnIn                     9  0
     D mgmtPtrIn                       *

     d* Update Medical Info
     d UpdMedInfo      pr
     D claimRrnIn                     9  0
     D medicalPtrIn                    *

     d* Update Benefit Info
     d UpdBenInfo      pr
     D claimRrnIn                     9  0
     D benefitPtrIn                    *

     d* Update Legal Info
     d UpdLegInfo      pr
     D claimRrnIn                     9  0
     D legalPtrIn                      *

     d* Update State Info
     d UpdStateInfo    pr
     D claimRrnIn                     9  0
     D statePtrIn                      *
     D statePtr                       2

     d* Update Quick Diary Info
     d UpdQuickDiary   pr
     D claimRrnIn                     9  0
     D diaryPtrIn                      *

     d* Writes Claim Record
     D WriteClaim      pr             9  0
     D company#                       3  0 const
     D group#                         3  0 const
     D year                           3  0 const
     D employer#                      9  0 const
     D division                       5  0 const
     D location                      10    const
     D tpa#                           5    const
     D socialSec#                     9    const
     D accidentDate                   7  0 const
     D jurState                       2    const
     D sicCode                        6    const

     D* Populate Top Five Payee Graph Data
     D PopTopFive      pr
     D  claimRrnIn                    9  0

     D* Delete Top Five Payee Graph Data
     D DelTopFive      pr
     D  jobName                      10
     D  jobUser                      10
     D  jobNumber                     6s 0

     d* Checks for Devereux Group
     D ChkDevGroup     pr              n
     D company#                       3  0 const
     D group#                         3  0 const

     d* Write Adjuster Diary
     D WriteAdjRmk     pr
