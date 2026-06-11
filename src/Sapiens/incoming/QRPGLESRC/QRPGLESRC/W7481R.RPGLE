     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
    H*
     H/TITLE W7481R  - Medicare 2nd Pay Extract Program
    H**************************************************************************************
    H*  Description: Medicare 2nd pay extract process - Generate data file based on       *
    H*               claims to be reported to Medicare for 2nd Payer.  This program       *
    H*               will read all claims which are flagged to report to Medicare.  The   *
    H*               process will then determine if data needs to be reported as an Add,  *
    H*               Update, or Delete.  Once the data is generated a log record will be  *
    H*               created and a flat file generated.  When the process is complete a   *
    H*               copy of the flat file will be placed into WXE2PSAV and WXTIPSAV.     *
    H*                                                                                    *
    H*  Called from: W7481C (Override CL)                                                 *
    H*                                                                                    *
    H*  Calls to: W7482C                                                                  *
    H*                                                                                    *
    H*  Created: C:65   Project: WC1819     Date:  06/12/09                               *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*
     H*  03/06/25   C:333  Project: WC2998
     H*                    Changes to CMS Settlement Reporting.
     H*
     H*  12/18/19   C:79   Support: 86042
     H*                    Change condition on DBA name to work properly
     H*                    for both CSI and S1 Medical
     H*
     H*  09/16/19   C:53   Support: 83091
     H*                    Add 152 processing from 114
     H*                    Add 152 processing from 'OPTACOMP'
     H*                    Correct legal information
     H*
     H*  02/02/18   C:53   Project: WC2452
     H*                    Report HICN to MBI change.
     H*
     H*  10/21/15   C:180  Support 31380
     H*                    Fix checking of SP31 error.
     H*
     H*  04/27/15   C:53   Project: WC2261
     H*                    Accomodate ICD-10 codes.
     H*
     H*  09/15/14   C:180  Support WC133 85685
     H*                    Do not report HICN changes
     H*
     H*  12/10/13   C:180  Support WC114 84236
     H*                    Report default pol number when carrier pol
     H*                    is not found.  This may be a temporary change
     H*                    that could be removed once data integrity issue
     H*                    with file WMRPP is resolved.
     H*
     H*                                                                                    *
     H*  08/19/13   C:53   Project: 147WBF                                                 *
     H*                    Move alt # to the 2nd assignment MDDCN#                         *
     H*                                                                                    *
     H*  06/19/13   C:282  Project: 114WFC Priss # 22                                      *
     H*                    Under UDS-M process read the cliams on the basis                *
     H*                    of NAIC.
     H*                                                                                    *
     H*  05/22/13   C:53   Project: 114WFC Priss # 22                                      *
     H*                    Adjust layout                                                   *
     H*                                                                                    *
     H*  04/08/13   C:190  Project: 147WBF                                                 *
     H*                    Added a condition that will populate MDCLM# with CMPDS#         *
     H*                    if FNGRPASC = 'OPTACOMP' and CMPDS# <> *blanks.                 *
     H*                                                                                    *
     H*  02/11/13   C:282  Project: 114WFC                                                 *
     H*                    UDS-M Records                                                   *
     H*                                                                                    *
     H*  11/21/12   C:65   Project: WC2116                                                 *
     H*                    Populate name and federal name field with no data manipulation  *
     H*                    using movel/move instead of concatenation.                      *
     H*                                                                                    *
     H*  11/14/11   C:189  SUPPORT: 78263/WC1879-priss 50                                  *
     H*                    Recompiled so that program gets the updated image of file
     H*                    MED2NGCD in which FAULT INSURANCE LIMIT is numeric              *
     H*                                                                                    *
     H*  10/05/11 C:061 WA025 77443                                                        *
     H*                - replace '-' with *blank in office name.                           *
     H*                - Initialize OFA array to blanks.                                   *
     H*                - Convert office to upper case for compare in OFA.                  *
     H*                - Write Wxe2p detail records with Group Fein.                       *
     H*                  (not Employer Fein).                                              *
     H*                                                                                    *
     H*  08/26/11   C:53   Project: WC2029                                                 *
     H*                    Resend again if disp 50 is received in last claims response.    *
     H*                                                                                    *
     H*  08/29/11   C:062  Support: 77663                                                  *
     H*                    Populate RRE# and FEIN with the OFF#.            isp SP is      *
     H*                                                                                    *
/* 04H*  04/12/11  C: 58  Project: WC1975                                                  *
/*   H*                     Add adjustor staff code to extract file.                       *
     H*                                                                                    *
     H*  04/13/11   C:198  Support: 76484/76499                                            *
     H*                    -incorporate SP functionality - resend again if disp SP is      *
     H*                    received in last claims response.                               *
     H*                                                                                    *
     H*  03/28/11   C:198  Project: WC1879 Priss 142                                       *
     H*                    -send only WC ICD9 codes                                        *
     H*                                                                                    *
     H*  03/17/11   C:198  Project: WC1879 Priss 161                                       *
     H*                    -Made Employer DBA name to have value only when Self Insured    *
     H*                     Type = 'O' only for CSI, since they particularly asked for it. *
     H*                     Other PID partners are not having any problems due to this.    *
     H*                                                                                    *
     H*  02/17/11   C:198  Project: WC1879 Priss 143                                       *
     H*                    -Send firm name first, if non blank otherwise send              *
     H*                     Rep first and last name                                        *
     H*                                                                                    *
     H*  01/21/11   C:198  Project: WC1879 Priss 142                                       *
     H*                    -Send only CMS flag as 'I' ICD9 code to Medicare                *
     H*                                                                                    *
     H*  01/19/11   C:53   Project: WC1963 Support 75560                                   *
     H*                    Use HIC# in the WMCDP per WC1963 changes                        *
     H*                                                                                    *
     H*  11/18/10   C:53   Project: WC1879 Priss 79                                        *
     H*                    New requirments for DN58-62 to be blanks.                       *
     H*                                                                                    *
     H*  11/16/10   C:53   Project: WC1879 Priss 89                                        *
     H*                    Qualify self insured type (DN65)                                *
     H*                                                                                    *
     H*  10/12/10   C:198  Project: WC1879 Priss 67 & 71                                   *
     H*                    Made gender a required field                                    *
     H*                    Fixed resubmission processing.                                  *
     H*                                                                                    *
    H*  10/05/10  C:251  Project: WC1944                                                  *
     H*                   If 'SP31' is encountered in previous extract,then issue a flag   *
     H*                   to send claim as an update                                       *
     H*                                                                                    *
    H*  08/16/10  C:198  Project: WC1879 Priss 50                                         *
     H*                   Re-compiled.                                                     *
     H*                                                                                    *
    H*  05/18/10  C:198  Project: WC1879                                                  *
     H*                   CMS Medicare Extract User Guide 3.0 (Feb 2010) Updates           *
     H*                                                                                    *
    H*  04/16/10  C:198  Project: WC1819 Phase 2 Priss 100                                *
     H*                   Claim input file and TIN file should have same TINs              *
     H*                                                                                    *
    H*  02/15/10  C:198  Project: WC1819 Phase 2 Priss 83                                 *
     H*                   Claim input file and TIN file has date in wrong format. It       *
     H*                   should be in CCYYMMDD.                                           *
     H*                                                                                    *
    H*  02/11/10  C:198  Project: WC1819 Phase 2                                          *
     H*                   Claim input file should not have a delete record for a query log *
     H*                   record in WDD1P.                                                 *
     H*                                                                                    *
    H*  11/19/09  C:62   Project: WC1819                                                  *
     H*                   Add accident description to update process.                      *
     H*                                                                                    *
    H*  10/30/09  C:65   Project: WC1819                                                  *
     H*                   Replace the additional claimants file WDACP with the new file    *
    H*                   WDCAP.                                                           *
     H*                                                                                    *
     H*  10/26/09 C:198 Project: WC1819 Phase 4                                            *
     H*                 When it is determined that a record needs to be written            *
     H*                 to the extract  file(s) then write the log record to the log       *
     H*                 record to the log file in QTEMP/WXMDL.                             *
     H*                                                                                    *
    H*  09/25/09  C:65   Project: WC1819                                                  *
     H*                   Add processing to populate 'FC' into the state field for out of  *
    H*                   country addresses.  Second, add processing to populate the       *
     H*                   entity/organization name into the additional claimant name       *
    H*                   fields.                                                          *
    H*                                                                                    *
    H**************************************************************************************
     Fwxgsl     if   e           k disk
     Fwxmdl     o  a e           k disk    Rename(pd101 : pmd01) Prefix(md:2)
     Fwdd1l     if a e           k disk
     Fwmd1l     if a e           k disk    rename(pd101 : pd201) prefix(d2:2)
     Fsadal5    if a e           k disk
     Fsadal3    if a e           k disk    rename(pda01 : pda02)
     Fwmfnl     if   e           k disk
     Fwdf2l     if   e           k disk
     Fwmeml     if   e           k disk
     Fwmrpl     if   e           k disk
     Fwmcml     if   e           k disk
     Fwmpdl     if   e           k disk
     Fwdcal     if   e           k disk
     Fwdmsl     if   e           k disk
     Fwmcdl     if   e           k disk    Rename(pcd01 : pc201) Prefix(c2:2)
     Fwmlll     if   e           k disk
     Fwmofl     if   e           k disk
     Fwde5l     if   e           k disk
     Fwxe2p     o  a e           k disk
     Fwxtip     o  a e           k disk
      *** Beg Add *** 02/11/13 **************************************************
     Fwxump     o  a e           k disk
     Fwmncl     if   e           k disk
      *** End Add *** 02/11/13 **************************************************
      *** Beg Add *** 06/19/13 **************************************************
     Fwmeml21   if   e           k disk    rename(pem01:pem02) prefix(e2:2)
      *** End Add *** 06/19/13 **************************************************

     d lowerCase       c                   'abcdefghijklmnopqrstuvwxyz'
     d upperCase       c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

     D psds          esds                  extname(szq1p)
     d pch           e ds                  extname(med2ngch)
     d pcd           e ds                  extname(med2ngcd)
     d pce           e ds                  extname(med2ngce)
     d pct           e ds                  extname(med2ngct)
     d pth           e ds                  extname(med2ngth)
     d ptd           e ds                  extname(med2ngtd)
     d ptt           e ds                  extname(med2ngtt)
     **
     D ofa             s             23    dim(500)
     **
     D                 ds
     D  I9A                    1     95    dim(19)
     D  XXI9A                  1     95
     D  MDI901                 1      5
     D  MDI902                 6     10
     D  MDI903                11     15
     D  MDI904                16     20
     D  MDI905                21     25
     D  MDI906                26     30
     D  MDI907                31     35
     D  MDI908                36     40
     D  MDI909                41     45
     D  MDI910                46     50
     D  MDI911                51     55
     D  MDI912                56     60
     D  MDI913                61     65
     D  MDI914                66     70
     D  MDI915                71     75
     D  MDI916                76     80
     D  MDI917                81     85
     D  MDI918                86     90
     D  MDI919                91     95
     **
     D                 ds
     D  I9O                    1     95    dim(19)
     D  XXI9O                  1     95
     D  D1I901                 1      5
     D  D1I902                 6     10
     D  D1I903                11     15
     D  D1I904                16     20
     D  D1I905                21     25
     D  D1I906                26     30
     D  D1I907                31     35
     D  D1I908                36     40
     D  D1I909                41     45
     D  D1I910                46     50
     D  D1I911                51     55
     D  D1I912                56     60
     D  D1I913                61     65
     D  D1I914                66     70
     D  D1I915                71     75
     D  D1I916                76     80
     D  D1I917                81     85
     D  D1I918                86     90
     D  D1I919                91     95
     D*** beg add *** 04/27/15 *****************************
     D                 ds
     D  I1O                    1    133    dim(19)
     D  XXI1O                  1    133
     D  D1I101                 1      7
     D  D1I102                 8     14
     D  D1I103                15     21
     D  D1I104                22     28
     D  D1I105                29     35
     D  D1I106                36     42
     D  D1I107                43     49
     D  D1I108                50     56
     D  D1I109                57     63
     D  D1I110                64     70
     D  D1I111                71     77
     D  D1I112                78     84
     D  D1I113                85     91
     D  D1I114                92     98
     D  D1I115                99    105
     D  D1I116               106    112
     D  D1I117               113    119
     D  D1I118               120    126
     D  D1I119               127    133
     D                 ds
     D  I1A                    1    133    dim(19)
     D  XXI1A                  1    133
     D  MDI101                 1      7
     D  MDI102                 8     14
     D  MDI103                15     21
     D  MDI104                22     28
     D  MDI105                29     35
     D  MDI106                36     42
     D  MDI107                43     49
     D  MDI108                50     56
     D  MDI109                57     63
     D  MDI110                64     70
     D  MDI111                71     77
     D  MDI112                78     84
     D  MDI113                85     91
     D  MDI114                92     98
     D  MDI115                99    105
     D  MDI116               106    112
     D  MDI117               113    119
     D  MDI118               120    126
     D  MDI119               127    133
     D*** end add *** 04/27/15 *****************************
     **
     D                 ds
     D  XXCA                   1    543
     d  xxc1rl                 1      1
     d  xxc1ss                 2     10  0
     d  xxc1ln                11     50
     d  xxc1fn                51     80
     d  xxc1mi                81     81
     d  xxc1a1                82    131
     d  xxc1a2               132    181
     d  xxc1cy               182    211
     d  xxc1st               212    213
     d  xxc1zp               214    218  0
     d  xxc1z4               219    222  0
     d  xxc1ph               223    232  0
     d  xxc1pe               233    237
     d  xxc1ri               238    238
     d  xxc1rn               239    278
     d  xxc1rf               279    308
     d  xxc1rm               309    378
     d  xxc1rt               379    387  0
     d  xxc1r1               388    437
     d  xxc1r2               438    487
     d  xxc1rc               488    517
     d  xxc1rs               518    519
     d  xxc1rz               520    524  0
     d  xxc1r4               525    528  0
     d  xxc1rp               529    538  0
     d  xxc1re               539    543
     **
     D                 ds
     D  XXC1A                  1    543
     d  mdc1rl                 1      1
     d  mdc1ss                 2     10  0
     d  mdc1ln                11     50
     d  mdc1fn                51     80
     d  mdc1mi                81     81
     d  mdc1a1                82    131
     d  mdc1a2               132    181
     d  mdc1cy               182    211
     d  mdc1st               212    213
     d  mdc1zp               214    218  0
     d  mdc1z4               219    222  0
     d  mdc1ph               223    232  0
     d  mdc1pe               233    237
     d  mdc1ri               238    238
     d  mdc1rn               239    278
     d  mdc1rf               279    308
     d  mdc1rm               309    378
     d  mdc1rt               379    387  0
     d  mdc1r1               388    437
     d  mdc1r2               438    487
     d  mdc1rc               488    517
     d  mdc1rs               518    519
     d  mdc1rz               520    524  0
     d  mdc1r4               525    528  0
     d  mdc1rp               529    538  0
     d  mdc1re               539    543
     **
     D                 ds
     D  XXC2A                  1    543
     d  mdc2rl                 1      1
     d  mdc2ss                 2     10  0
     d  mdc2ln                11     50
     d  mdc2fn                51     80
     d  mdc2mi                81     81
     d  mdc2a1                82    131
     d  mdc2a2               132    181
     d  mdc2cy               182    211
     d  mdc2st               212    213
     d  mdc2zp               214    218  0
     d  mdc2z4               219    222  0
     d  mdc2ph               223    232  0
     d  mdc2pe               233    237
     d  mdc2ri               238    238
     d  mdc2rn               239    278
     d  mdc2rf               279    308
     d  mdc2rm               309    378
     d  mdc2rt               379    387  0
     d  mdc2r1               388    437
     d  mdc2r2               438    487
     d  mdc2rc               488    517
     d  mdc2rs               518    519
     d  mdc2rz               520    524  0
     d  mdc2r4               525    528  0
     d  mdc2rp               529    538  0
     d  mdc2re               539    543
     **
     D                 ds
     D  XXC3A                  1    543
     d  mdc3rl                 1      1
     d  mdc3ss                 2     10  0
     d  mdc3ln                11     50
     d  mdc3fn                51     80
     d  mdc3mi                81     81
     d  mdc3a1                82    131
     d  mdc3a2               132    181
     d  mdc3cy               182    211
     d  mdc3st               212    213
     d  mdc3zp               214    218  0
     d  mdc3z4               219    222  0
     d  mdc3ph               223    232  0
     d  mdc3pe               233    237
     d  mdc3ri               238    238
     d  mdc3rn               239    278
     d  mdc3rf               279    308
     d  mdc3rm               309    378
     d  mdc3rt               379    387  0
     d  mdc3r1               388    437
     d  mdc3r2               438    487
     d  mdc3rc               488    517
     d  mdc3rs               518    519
     d  mdc3rz               520    524  0
     d  mdc3r4               525    528  0
     d  mdc3rp               529    538  0
     d  mdc3re               539    543
     **
     D                 ds
     D  XXC4A                  1    543
     d  mdc4rl                 1      1
     d  mdc4ss                 2     10  0
     d  mdc4ln                11     50
     d  mdc4fn                51     80
     d  mdc4mi                81     81
     d  mdc4a1                82    131
     d  mdc4a2               132    181
     d  mdc4cy               182    211
     d  mdc4st               212    213
     d  mdc4zp               214    218  0
     d  mdc4z4               219    222  0
     d  mdc4ph               223    232  0
     d  mdc4pe               233    237
     d  mdc4ri               238    238
     d  mdc4rn               239    278
     d  mdc4rf               279    308
     d  mdc4rm               309    378
     d  mdc4rt               379    387  0
     d  mdc4r1               388    437
     d  mdc4r2               438    487
     d  mdc4rc               488    517
     d  mdc4rs               518    519
     d  mdc4rz               520    524  0
     d  mdc4r4               525    528  0
     d  mdc4rp               529    538  0
     d  mdc4re               539    543
     **
     D                 ds
     D  XXC1O                  1    543
     d  d1c1rl                 1      1
     d  d1c1ss                 2     10  0
     d  d1c1ln                11     50
     d  d1c1fn                51     80
     d  d1c1mi                81     81
     d  d1c1a1                82    131
     d  d1c1a2               132    181
     d  d1c1cy               182    211
     d  d1c1st               212    213
     d  d1c1zp               214    218  0
     d  d1c1z4               219    222  0
     d  d1c1ph               223    232  0
     d  d1c1pe               233    237
     d  d1c1ri               238    238
     d  d1c1rn               239    278
     d  d1c1rf               279    308
     d  d1c1rm               309    378
     d  d1c1rt               379    387  0
     d  d1c1r1               388    437
     d  d1c1r2               438    487
     d  d1c1rc               488    517
     d  d1c1rs               518    519
     d  d1c1rz               520    524  0
     d  d1c1r4               525    528  0
     d  d1c1rp               529    538  0
     d  d1c1re               539    543
     D                 ds
     D  XXC2O                  1    543
     d  d1c2rl                 1      1
     d  d1c2ss                 2     10  0
     d  d1c2ln                11     50
     d  d1c2fn                51     80
     d  d1c2mi                81     81
     d  d1c2a1                82    131
     d  d1c2a2               132    181
     d  d1c2cy               182    211
     d  d1c2st               212    213
     d  d1c2zp               214    218  0
     d  d1c2z4               219    222  0
     d  d1c2ph               223    232  0
     d  d1c2pe               233    237
     d  d1c2ri               238    238
     d  d1c2rn               239    278
     d  d1c2rf               279    308
     d  d1c2rm               309    378
     d  d1c2rt               379    387  0
     d  d1c2r1               388    437
     d  d1c2r2               438    487
     d  d1c2rc               488    517
     d  d1c2rs               518    519
     d  d1c2rz               520    524  0
     d  d1c2r4               525    528  0
     d  d1c2rp               529    538  0
     d  d1c2re               539    543
     **
     D                 ds
     D  XXC3O                  1    543
     d  d1c3rl                 1      1
     d  d1c3ss                 2     10  0
     d  d1c3ln                11     50
     d  d1c3fn                51     80
     d  d1c3mi                81     81
     d  d1c3a1                82    131
     d  d1c3a2               132    181
     d  d1c3cy               182    211
     d  d1c3st               212    213
     d  d1c3zp               214    218  0
     d  d1c3z4               219    222  0
     d  d1c3ph               223    232  0
     d  d1c3pe               233    237
     d  d1c3ri               238    238
     d  d1c3rn               239    278
     d  d1c3rf               279    308
     d  d1c3rm               309    378
     d  d1c3rt               379    387  0
     d  d1c3r1               388    437
     d  d1c3r2               438    487
     d  d1c3rc               488    517
     d  d1c3rs               518    519
     d  d1c3rz               520    524  0
     d  d1c3r4               525    528  0
     d  d1c3rp               529    538  0
     d  d1c3re               539    543
     d*
     D                 ds
     D  XXC4O                  1    543
     d  d1c4rl                 1      1
     d  d1c4ss                 2     10  0
     d  d1c4ln                11     50
     d  d1c4fn                51     80
     d  d1c4mi                81     81
     d  d1c4a1                82    131
     d  d1c4a2               132    181
     d  d1c4cy               182    211
     d  d1c4st               212    213
     d  d1c4zp               214    218  0
     d  d1c4z4               219    222  0
     d  d1c4ph               223    232  0
     d  d1c4pe               233    237
     d  d1c4ri               238    238
     d  d1c4rn               239    278
     d  d1c4rf               279    308
     d  d1c4rm               309    378
     d  d1c4rt               379    387  0
     d  d1c4r1               388    437
     d  d1c4r2               438    487
     d  d1c4rc               488    517
     d  d1c4rs               518    519
     d  d1c4rz               520    524  0
     d  d1c4r4               525    528  0
     d  d1c4rp               529    538  0
     d  d1c4re               539    543
     **
     D                 ds
     D  XXP1A                  1     25
     d  mdt1sd                 1      7S 0
     d  mdt1sa                 8     18S 2
     d  mdt1fd                19     25S 0
     D                 ds
     D  XXP2A                  1     25
     d  mdt2sd                 1      7S 0
     d  mdt2sa                 8     18S 2
     d  mdt2fd                19     25S 0
     D                 ds
     D  XXP3A                  1     25
     d  mdt3sd                 1      7S 0
     d  mdt3sa                 8     18S 2
     d  mdt3fd                19     25S 0
     D                 ds
     D  XXP4A                  1     25
     d  mdt4sd                 1      7S 0
     d  mdt4sa                 8     18S 2
     d  mdt4fd                19     25S 0
     D                 ds
     D  XXP5A                  1     25
     d  mdt5sd                 1      7S 0
     d  mdt5sa                 8     18S 2
     d  mdt5fd                19     25S 0
     **
     D                 ds
     D  XXP1O                  1     25
     d  d1t1sd                 1      7S 0
     d  d1t1sa                 8     18S 2
     d  d1t1fd                19     25S 0
     D                 ds
     D  XXP2O                  1     25
     d  d1t2sd                 1      7S 0
     d  d1t2sa                 8     18S 2
     d  d1t2fd                19     25S 0
     D                 ds
     D  XXP3O                  1     25
     d  d1t3sd                 1      7S 0
     d  d1t3sa                 8     18S 2
     d  d1t3fd                19     25S 0
     D                 ds
     D  XXP4O                  1     25
     d  d1t4sd                 1      7S 0
     d  d1t4sa                 8     18S 2
     d  d1t4fd                19     25S 0
     D                 ds
     D  XXP5O                  1     25
     d  d1t5sd                 1      7S 0
     d  d1t5sa                 8     18S 2
     d  d1t5fd                19     25S 0
     **
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
      *** Beg Add *** 02/11/13 **************************************************
     d                 ds
     d  UDSM                   1     71
     d  charRcdTyp             1      1
     d  charICNaicNo           2      6
     d  charICClmNo            7     26
     d  charRcvClmNo          27     46
     d  charfndClmNo          47     66
     d  charClaimNo           67     71
      *** End Add *** 02/11/13 **************************************************
     **
     D xxco#           s                   like(cmco#)
     D xxfnd           s                   like(cmfnd)
     D svfnd           s                   like(cmfnd)
     D xxprim          s                   like(daprim)
     D x1date          s                   like(d1exdt)
     D xxfein          s                   like(pdfein)
     D xxmod           s                   like(pdmod)
     D xxoff#          s                   like(emoff#)
     D xxdate          s              8  0
     D xxcnt           s              7  0
     D of#             s              3  0
     D FlSP31          s              1
     D Tmexdt          s                   like(E5EXDT)
     D rreFeinOff      s             23
     D yyrre#          s              9
     D yyfein          s              9
      *** Beg Add *** 02/11/13 **************************************
     d charNaic        s              5
     d flgUDSM         s              1
      *** End Add *** 02/11/13 **************************************
      *** Beg Add *** 06/19/13 **************************************
     d intNaic         s              9  0
     d kknaic          s                   like(emrsk#)
      *** End Add *** 06/19/13 **************************************
     d*** Begin add 02/02/18 *************************************************************
     d xxhic9          s              9
     D mbichg          s              1
     d*** End   add 02/02/18 *************************************************************
     **
     D                 ds
     D xxdcn#                        15
     D  mdfnd                         3  0 overlay(xxdcn#)
     D  mdfyr                         3  0 overlay(xxdcn#:4)
     D  mdcase                        7  0 overlay(xxdcn#:7)
     D                 ds
     D xxcap#                        15
     D  cmfnd                         3  0 overlay(xxcap#)
     D  cmemp#                        9  0 overlay(xxcap#:4)
     D  cmfyr                         3  0 overlay(xxcap#:13)
     **
     **  KEY LIST FOR Group Select File
     **
     C     GSKY          KLIST
     C                   KFLD                    xxco#
     **
     **  KEY LIST FOR Group Master File
     **
     C     FNKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     **
     **  KEY LIST FOR Group Auxiliary File
     **
     C     F2KY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     **
     **  KEY LIST FOR Claim File
     **
     C     CMKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     c*
      *** Beg Add *** 06/19/13 *********************************************
     c     keyWMCML      Klist
     c                   Kfld                    xxco#
     c                   Kfld                    xxfnd
     c                   Kfld                    E2EMP#
     c                   Kfld                    E2DIV
      *** End Add *** 06/19/13 *********************************************
     **  KEY LIST FOR Claim Demographics File
     **
     C     CDKY          KLIST
     C                   KFLD                    cmss#
     **
     **  KEY LIST FOR ICD 9 Claim File
     **
     C     DAKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    cmfyr
     C                   KFLD                    cmcase
     C                   KFLD                    xxprim
     **
     C     DAKY1         KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    cmfyr
     C                   KFLD                    cmcase
     **
     **  KEY LIST FOR Claim Legal File
     **
     C     LLKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    cmfyr
     C                   KFLD                    cmcase
     **
     **  KEY LIST FOR Employer Master File
     **
     C     EMKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    cmemp#
     C                   KFLD                    cmdiv
     **
      *** Beg Add *** 06/19/13 *********************************************
      *  Key List for WMEMP
     c     keyWMEML21    Klist
     c                   Kfld                    xxco#
     c                   Kfld                    kknaic
      *** End Add *** 06/19/13 *********************************************
     **  KEY LIST FOR Policy Carrier # File
     **
     C     RPKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    cmemp#
     C                   KFLD                    cmdiv
     C                   KFLD                    cmfyr
     **
     **  KEY LIST FOR Extract Log File
     **
     C     D1KY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    cmfyr
     C                   KFLD                    cmcase
     **
     **  KEY LIST FOR Payee Demographics File
     **
     C     PDKY          KLIST
     C                   KFLD                    xxfein
     C                   KFLD                    xxmod
     **
     **  KEY LIST FOR Additional Claimant Address File
     **
     C     ACKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    cmfyr
     C                   KFLD                    cmcase
     **
     **  KEY LIST FOR Multiple Settlements File
     **
     C     MSKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxfnd
     C                   KFLD                    cmfyr
     C                   KFLD                    cmcase
    C*
    C*  Key list for OFFICE MASTER File
    C*
     C     ofky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxoff#
    C*
    C*  Key list for Extract log master file
    C*
     C     d2ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    x1date
     C*
     C*
     C*  Key List for WDE5P - details of error code generated from extract
     C*
     C     e5ky          KLIST
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
     C     e5ky1         KLIST
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C                   kfld                    Tmexdt
      *** Beg Add *** 02/11/13 *********************************************
      *  Key List for WMNCL
     c     keyWMNCL      Klist
     c                   Kfld                    CMpds#
      *** End Add *** 02/11/13 *********************************************
    C*
     C* Date call structures
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     c*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     c* Entry key list
     c*
     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@call            1
     C                   parm                    @@que            10
     C                   parm                    @@srt             1
      *** Beg Add *** 02/11/13 **********************************************
     c                   parm                    @@Naic            5
      /Free
       flgUDSM  = ' ';
       If %Parms > 4;
          charNaic = @@Naic;
          flgUDSM  = 'Y';
       EndIf;
      /End-Free
      *
     c                   call      'RTVCLTID'
     c                   parm                    xxclt#            3
      *** End Add *** 02/11/13 **********************************************
     C*
     c                   clear                   ofa
     c                   move      @@co#         xxco#
     C*
     C                   time                    dsdttm
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     c                   move      *blank        xxfrst            1
     c                   z-add     0             xxcnt
     c                   z-add     0             of#
     c                   z-add     0             svfnd
     C*
     C* Begin reading group select file to process all selected groups.
     C*
     c     gsky          chain     pgs01                              60
     c                   dow       *in60 = *off
     C*
     C                   if        gsiefl = 'I'
     c                   move      gsfnd         xxfnd
     c*
     c* Write header record if not done.
     c*
     c                   if        xxfrst = *blank
     C                   if        @@call <> 'E'
     c                   exsr      wrthdr
     C                   endif                                                  @@call <> 'E'
     c                   move      'Y'           xxfrst
     c                   z-add     xxfnd         svfnd
     c                   endif
     C*
     C* Set group # to zero for backup member name for multiple group files.
     C*
     c                   if        gsfnd <> svfnd
     c                   z-add     0             svfnd
     c                   endif
     C*
     c     fnky          chain     pfn01                              60
     C*
     C* Read through claims master file to identify Medicare 2nd Pay claims.
     C*
      *** Beg Add *** 06/19/13 **************************************************
     c                   If        flgUDSM = 'Y' and xxClt# = '114'
     c                   move      charNaic      intNaic
     c                   movel     intNaic       kknaic
     c     keyWMEML21    chain     pem02
     c                   If        %Found(wmeml21)
     c     keyWMCML      chain     pcm01                              60
     c                   EndIf
     c                   Else
      *** End Add *** 06/19/13 **************************************************
     c     cmky          chain     pcm01                              60
      *** Beg Add *** 06/19/13 **************************************************
     c                   EndIf
      *** End Add *** 06/19/13 **************************************************
     c                   dow       *in60 = *off
     c*
     c     d1ky          setgt     pd101
     c     d1ky          readpe    pd101                                  40
     c*
     c                   dow       *in40 = *off
     c                   if        d1type = 'Q' or d1rsbm = 'Y'
     c     d1ky          readpe    pd101                                  40
     c                   else
     c                   leave
     c                   endif
     c                   enddo
     c                   select
     c                   when      cmmbs <> 'Y' and *in40 = *off                Claim no longer
     c                             and d1type <> '1'                            Medicare
     c*
     c* Process a Delete transaction reporting old information.
     C                   if        @@call <> 'E'
     c*
     c                   exsr      do_del
     c*
     C                   endif                                                  @@call <> 'E'
     c                   when      cmmbs = 'Y'                                  Medicare claim
     c*
     c     emky          chain     pem01                              60
     c     cdky          chain     pc201                              60
     c*
     c* Populate temporary log file with claim information.
     c*
     c                   clear                   pmd01
     c*
     c                   move      cmco#         mdco#
     c                   move      cmfnd         mdfnd
     c                   move      cmfyr         mdfyr
     c                   move      cmcase        mdcase
     c                   move      xxdcn#        mddcn#
     c                   move      x1date        mdexdt
     c                   move      c2hicn        mdhic#
     c                   move      cmss#         mdss#
     c                   movel(p)  cmlsn         mdlsn
     c                   movel(p)  cmfsn         mdfsn
     c                   movel(p)  cmmid         mdmid
     c                   move      fnsfin        mdslfi
     C                   if        fnsfin = 'Y'
     c                   move      'O'           mdslft
     c                   else
     c                   movel     *blanks       mdslft
     c                   endif
     c****  DBA name is to be conditional only in case of CSI.
      *** Beg Del *** 02/11/13 **************************************************
     c*****              call      'RTVCLTID'
     c*****              parm                    xxclt#            3
      *** End Del *** 02/11/13 **************************************************
     c                   if        mdslft = 'O'
     c                   movel(p)  emname        mdname
     c                   cat       emnam2:1      mdname
     c                   movel(p)  emname        xxn100          100
     c                   move      emnam2        xxn100
     c                   movel(p)  xxn100        mdname
     c                   endif                                                  mdslft = 'O'
     c*** Beg Chg *** 09/16/19 *************************
     c*****************  if        xxclt# <> '114'
     c*** Beg Chg *** 12/18/19 *************************
     c*****************  if        xxclt# <> '114' or xxclt# <> '152'
     c                   if        xxclt# <> '114' and xxclt# <> '152'
     c*** End Chg *** 12/18/19 *************************
     c*** End Chg *** 09/16/19 *************************
     c                   movel(p)  emname        mdname
     c                   cat       emnam2:1      mdname
     c                   movel(p)  emname        xxn100          100
     c                   move      emnam2        xxn100
     c                   movel(p)  xxn100        mdname
     c                   endif                                                  xxclt# <> '114'
     c                   movel(p)  emfdnm        mdfdnm
     c                   cat       emfdn2:1      mdfdnm
     c                   movel(p)  emfdnm        xxf100          100
     c                   move      emfdn2        xxf100
     c                   movel(p)  xxf100        mdfdnm
     c                   move      'E'           mdpity
     c     f2ky          chain(n)  pf201                              60
     c  N60              move      f2fein        mdfein
     c                   movel(p)  xxdcn#        mdclm#
      *****beg add ****** 04/08/13 ************************************
      /free
       //*** Beg Chg *** 09/16/19 ***************************************   ****
       //***If FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
       If xxclt# = '152' and CMpds# <> *blanks;
       //*** End Chg *** 09/16/19 ***************************************   ****
          MDclm# = CMpds#;
       //*** Beg Add *** 08/19/13 ***************************************   ****
          MDdcn# = CMpds#;
       //*** End Add *** 08/19/13 ***************************************   ****
       Endif;
      /end-free
      *****end add ****** 04/08/13 ************************************
     c                   move      emoff#        tmoff#            9 0
     c                   if        emoff# <>  ' '
     c                   eval      mdoff# = %editc(tmoff#:'X')
     c                   else
     c                   movel     *blanks       mdoff#
     c                   endif
     c*
     c* Store RRE#, FEIN and OFF#
     c*

     c                   eval      emoff# = %xlate(lowercase:uppercase:
     c                                      emoff#)

     c                   eval      rreFeinOff=%editc(f2rre#:'X') +
     c                             %editc(f2fein:'X') + emoff#
     c*
     c* Store office # in array for TIN file.
     c*
     c                   z-add     1             x                 3 0
     c     rreFeinOff    lookup    ofa(x)                                 60
     c                   if        *in60 = *off
     c                   add       1             of#
     c                   move      rreFeinOff    ofa(of#)
     c                   endif
     c*
     c* Carrier Policy #
     c*
     c     rpky          chain     prp01                              60
     C**** BEGIN CHANGE **** 12/10/13 **************************************
     c****               if        *in60 = *off
     c                   if        *in60 = *off and rpcap# > *blanks
     C**** END   CHANGE **** 12/10/13 **************************************
     c                   movel     rpcap#        mdcap#
     c                   else
     c                   movel     xxcap#        mdcap#
     c                   endif
     c
     c                   select
     c                   when      c2sex = 'M'
     c                   move      '1'           mdsex
     c                   when      c2sex = 'F'
     c                   move      '2'           mdsex
     c                   other
     C                   if        c2sex <> ' '
     c                   move      '0'           mdsex
     C                   endif
     c                   endsl
     c*
     c                   move      c2dtob        mddtob
     c                   move      cmcmdi        mdcmdi
     c                   move      cmacdt        mdacdt
     c                   move      cmjrst        mdjrst
     c                   if        c2adr3 <> *blanks and c2ctry <> *blanks
     c                   move      'FC'          mdjrst
     c                   endif
     c                   move      cmorm         mdormi
     c                   move      cmormd        mdormd
     c*** beg add *** 04/27/15 *****************************
     c                   if        cmain1 <> *blank
     c                   movel     cmain1        mdain1
     c                   else                                                            dasub# = 0
     c*** end add *** 04/27/15 *****************************
     c                   movel     cmainj        mdainj
     c*** beg add *** 04/27/15 *****************************
     c                   endif                                                           dasub# = 0
     c*** end add *** 04/27/15 *****************************
     c                   if        @@srt <> *blank
     c                   move      cmclex        mdclex
     c                   endif                                                           dasub# = 0
     c*
     c* ICD 9 Infomration
     c*
     c*
     c*
     c                   z-add     1             i                 2 0
     c*
     c     daky1         chain     pda02                              60
     c                   dow       *in60 = *off and i < 20
     c                   if        dasub# = 0
     c                   move      daicd9        i9a(i)
     c*** beg add *** 04/27/15 *****************************
     c                   moveL(p)  daicd1001     i1a(i)
     c*** end add *** 04/27/15 *****************************
     c                   add       1             i
     c                   endif                                                           dasub# = 0
     c     daky1         reade     pda02                                  60
     c                   enddo
     c*
     c* Legal information
     c*
     c     llky          chain     pll01                              60
     c*
     c                   if        *in60 = *off
      //*** Beg Add ***** 03/06/25 *************************
        If LLlump = '' and LLstruct = '';
           MDlumstr = '';
        Else;
           If LLlump = 'Y';
              MDlumstr = 'L';
           Elseif LLstruct = 'Y';
              MDlumstr = 'S';
           Endif;
        Endif;

        MDMSAAMT = LLMSAAMT;
        MDMSAPRD = LLMSAPRD;
        MDINDEPAMT = LLINDEPAMT;
        MDADEPAMT = LLADEPAMT;
        MDCASECNT = LLCASCNT#;
        MDPRADEIN = LLPRADEIN;
      //*** End Add ***** 03/06/25 *************************
     c                   movel(p)  llclri        mdclri
     c                   z-add     llstdt        mdt1sd
     c                   z-add     llsamt        mdt1am
     c                   z-add     llfdly        mdt1fd
     c*
     c                   if        mdclri <> *blank
     c                   move      llplaa        xxfein
     c                   move      llplam        xxmod
     c*
     c* Representative Firm Info.
     c*
     c     pdky          chain     ppd01                              60
     c                   if        *in60 = *off
     c                   movel(p)  llrfnm        mdrfnm
     c*
     c                   if        mdrfnm = ' '
     c                   movel(p)  pdname        mdrpln
     c                   movel(p)  pdfsn         mdrpfn
     c                   endif                                                          mdrfnm = ' '
     c                   movel(p)  llplaa        mdplaa
     c                   if        llpad1 <> *blanks
     c                   movel(p)  llpad1        mdrma1
     c                   movel(p)  llpad2        mdrma2
     c                   movel(p)  llpcty        mdrcty
     c                   movel(p)  llpst         mdrst
     c                   if        llpst = *blanks
     c                   move      'FC'          mdrst
     c                   endif
     c                   movel     llpzip        mdrzip
     c                   move      llpzip        mdrzp4
     c                   movel     llplar        mdrph#
     c                   move      llplap        mdrph#
     c                   else
     c                   movel(p)  pdadr1        mdrma1
     c                   movel(p)  pdadr2        mdrma2
     c                   movel(p)  pdcty         mdrcty
     c                   movel(p)  pdst          mdrst
     c                   if        pdctry <> *blanks and pdadr3 <> *blanks
     c                   move      'FC'          mdrst
     c                   endif
     c                   movel     pdzip         mdrzip
     c                   move      pdzip         mdrzp4
     c                   movel     pdarea        mdrph#
     c                   move      pdphn#        mdrph#
     c                   endif                                                  if llpad1 <> *blanks
     c*** Beg Add *** 09/16/19 ********************************
     c*** no hit on PPD01
     c                   else
     c                   movel(p)  llrfnm        mdrfnm
     c                   movel(p)  llplaa        mdplaa
     c                   if        llpad1 <> *blanks
     c                   movel(p)  llpad1        mdrma1
     c                   movel(p)  llpad2        mdrma2
     c                   movel(p)  llpcty        mdrcty
     c                   movel(p)  llpst         mdrst
     c                   if        llpst = *blanks
     c                   move      'FC'          mdrst
     c                   endif
     c                   movel     llpzip        mdrzip
     c                   move      llpzip        mdrzp4
     c                   movel     llplar        mdrph#
     c                   move      llplap        mdrph#
     c                   endif
     c*** End Add *** 09/16/19 ********************************
     c                   endif                                                  if *in60 = *off
     c*
     c                   endif                                                  if mdclri <> *blank
     c*
     c                   endif                                                  if *in60 = *off
     c*
     c* Additional Claimant info.
     c*
     c                   z-add     1             i
     c     acky          chain     pca01                              60
     c                   dow       *in60 = *off and i < 5
     c*
     c*
     c*
     c                   if        caclrl <> *blank
     c*
     c                   clear                   xxca
     c                   move      caclrl        xxc1rl
     c                   move      cassti        xxc1ss
     c
     c                   if        caclrl = 'X' or caclrl = 'Y' or caclrl = 'Z'
     c                   movel(p)  caname        xxc1ln
     c                   else
     c                   movel(p)  calsn         xxc1ln
     c                   movel(p)  cafsn         xxc1fn
     c                   movel(p)  camid         xxc1mi
     c                   endif
     c
     c                   movel(p)  caadr1        xxc1a1
     c                   movel(p)  caadr2        xxc1a2
     c                   movel(p)  cacty         xxc1cy
     c                   movel(p)  cast          xxc1st
     c*
     c                   if        cactry <> *blanks and caadr3 <> *blanks
     c                   move      'FC'          xxc1st
     c                   endif
     c*
     c                   z-add     0             xxc1ph
     c                   movel     cazip         xxc1zp
     c                   move      cazip         xxc1z4
     c                   movel     caarea        xxc1ph
     c                   move      caphn#        xxc1ph
     c                   move      *blank        xxc1pe
     c                   movel(p)  caclri        xxc1ri
     c                   movel(p)  cacrln        xxc1rn
     c                   movel(p)  cacrfn        xxc1rf
     c                   movel(p)  cacrfm        xxc1rm
     c                   movel(p)  cacrti        xxc1rt
     c                   movel(p)  cacra1        xxc1r1
     c                   movel(p)  cacra2        xxc1r2
     c                   movel(p)  cacrct        xxc1rc
     c                   movel(p)  cacrst        xxc1rs
     c*
     c                   if        carcry <> *blanks and cacra3 <> *blanks
     c                   movel     'FC'          xxc1rs
     c                   endif
     c*
     c                   z-add     0             xxc1rp
     c                   movel     cacrzp        xxc1rz
     c                   move      cacrzp        xxc1r4
     c                   movel     cacrac        xxc1rp
     c                   move      cacrph        xxc1rp
     c                   movel(p)  cacrex        xxc1re
     c*
     c                   select
     c                   when      i = 1
     c                   move      xxca          xxc1a
     c                   when      i = 2
     c                   move      xxca          xxc2a
     c                   when      i = 3
     c                   move      xxca          xxc3a
     c                   when      i = 4
     c                   move      xxca          xxc4a
     c                   endsl
     c*
     c                   add       1             i
     c                   endif                                                  if acclrl <> *blank
     c*
     c     acky          reade     pca01                                  60
     c                   enddo
     c*
     c* Additional Settlements info.
     c*
     c                   z-add     1             i
     c     msky          chain     pms01                              60
     c                   dow       *in60 = *off and i < 5
     c*
     c                   select
     c                   when      i = 1
     c                   move      mstpod        mdt2sd
     c                   z-add     mstpoa        mdt2am
     c                   move      msdlyd        mdt2fd
     c                   when      i = 2
     c                   move      mstpod        mdt3sd
     c                   z-add     mstpoa        mdt3am
     c                   move      msdlyd        mdt3fd
     c                   when      i = 3
     c                   move      mstpod        mdt4sd
     c                   z-add     mstpoa        mdt4am
     c                   move      msdlyd        mdt4fd
     c                   when      i = 4
     c                   move      mstpod        mdt5sd
     c                   z-add     mstpoa        mdt5am
     c                   move      msdlyd        mdt5fd
     c                   endsl
     c*
     c                   add       1             i
     c*
     c     msky          reade     pms01                                  60
     c                   enddo
     c*
     c* Write temporary log record.
     c*
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       If flgUDSM <> 'Y';
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     c                   write     pmd01
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       EndIf;
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C                   if        @@call <> 'E'
     c*
     c* check for claims from previous extract
     c                   exsr      prvSp31
     c*
     c*** Begin add 02/02/18 *************************************************************
     c                   eval      mbichg = ' '
     c                   if        d1hic# <> mdhic#                             HIC# change
     c                   movel(p)  mdhic#        xxhic9
     c                   testn                   xxhic9               99
     c*** not numberic = change from hicn to mbi
     c                   if        *in99 = *off
     c                   eval      mbichg = 'Y'
     c                   endif
     c                   endif
     c*** End   add 02/02/18 ************************************************************
     c*
     c                   select
     c*
     c* No prior log record.
     c                   when      *in40 = *on
     c                             or (d1code = 'SP' and d1type = '0')
     c                             or (d1code = '50' and d1type = '0')
     c*
     c* Process an Add transaction
     c*
     c                   exsr      do_add
     c*
     c* Key field information change.
     c                   when      d1ss# <> mdss#                               SS# changes
     c*** Begin delete 09/15/14*************************************************************
     c***                          or d1hic# <> mdhic#                          HIC# changes
     c*** End   delete 09/15/14 ************************************************************
     c                             or d1cmdi <> mdcmdi                          CMS Date chgs
     c                             or d1ormi <> mdormi                          ORM Ind chgs
     c*
     c* Process a Delete of old transaction and then an Add of New transaction
     c*
     c                   exsr      do_del
     c                   exsr      do_add
     c*
     c* Claim data has change.
     c                   when      d1ormi = 'Y' and d1ormd = 0 and mdormd <> 0  ORM Term Date Added
     c                             or XXI9O <> XXI9A                            ICD9 Codes change
     c*** beg add *** 04/27/15 *****************************
     c                             or XXI1O <> XXI1A                            ICD9 Codes change
     c*** end add *** 04/27/15 *****************************
      *** Beg Del *** 03/06/25 *****************************
     c***                          or d1dinj <> mddinj                          Desc of Inj chgs
      *** End Del *** 03/06/25 *****************************
     c                             or d1fein <> mdfein                          TIN changes
      *** Beg Del *** 03/06/25 *****************************
     c***                          or d1dinj <> mddinj                          Accident Description
      *** End Del *** 03/06/25 *****************************
     c                             or XXC1O <> XXC1A                            Claimant 1 info chgs
     c                             or XXC2O <> XXC2A                            Claimant 1 info chgs
     c                             or XXC3O <> XXC3A                            Claimant 1 info chgs
     c                             or XXC4O <> XXC4A                            Claimant 1 info chgs
     c                             or XXP1O <> XXP1A                            Claimant 1 info chgs
     c                             or XXP2O <> XXP2A                            Claimant 1 info chgs
     c                             or XXP3O <> XXP3A                            Claimant 1 info chgs
     c                             or XXP4O <> XXP4A                            Claimant 1 info chgs
     c                             or XXP5O <> XXP5A                            Claimant 1 info chgs
     C                             or FlSP31 = 'Y'
     c                             or (d1code = 'SP' and d1type ='2')
     c                             or (d1code = '50' and d1type ='2')
     c*** Begin add 02/02/18 *************************************************************
     c                             or mbichg = 'Y'                              HIC# change
     c*** End   add 02/02/18 ************************************************************
     c*
     c* Process an Update transaction
     c*
     c                   exsr      do_upd
     c*
     c                   endsl
     c*
     C                   endif                                                  @@call <> 'E'
     c                   endsl
     c*
      *** Beg Add *** 06/19/13 **************************************************
     c                   If        flgUDSM = 'Y' and xxClt# = '114'
     c     keyWMCML      reade     pcm01                                  60
     c                   Else
      *** End Add *** 06/19/13 **************************************************
     c     cmky          reade     pcm01                                  60
      *** Beg Add *** 06/19/13 **************************************************
     c                   EndIf
      *** End Add *** 06/19/13 **************************************************
     c                   enddo                                                  dow *in60 = *off
     C*
     c*
     C                   endif                                                  gsiefl = 'I'
     c     gsky          reade     pgs01                                  60
     c                   enddo                                                  dow *in60 = *off
     C*
     C                   if        @@call <> 'E'
     c                   exsr      wrttrl
     C*
     C* Write records for the TIN extract file.
     C*
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       If flgUDSM <> 'Y';
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     c                   exsr      wrttin
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       EndIf;
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C*
     C                   endif                                                  @@call <> 'E'
     C* Copy data file to backup as a member in WXE2PSAV and WXTIPSAV.
     C*
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       If flgUDSM <> 'Y';
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C                   if        @@call = 'E'
     C                   call      'W7481R1'
     C                   parm                    @@srt             1
     C                   else
     c                   move      svfnd         @@fnd
     c                   call      'W7482C'
     c                   parm                    @@fnd             3
     C                   endif                                                  @@call = 'E'
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       EndIf;
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C*
     C                   eval      *inlr=*on
     C*
     c*******************************************************************************************
     C*
     C*  Process an Add transaction
     C*
     c*******************************************************************************************
     c     do_add        begsr
     C*
     c                   exsr      cpyrec
     C*
     C                   MOVE      '0'           D1TYPE
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       If flgUDSM <> 'Y';
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C                   WRITE     PD101
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       EndIf;
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C*
     C* Write detail record into extract file.
     C*
     c                   exsr      wrtdet
     C*
     c                   endsr                                                  do_add subroutine
     C*
     c*******************************************************************************************
     C*
     C*  Process an Update transaction
     C*
     c*******************************************************************************************
     c     do_upd        begsr
     C*
     c                   exsr      cpyrec
     C*
     C                   MOVE      '2'           D1TYPE
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       If flgUDSM <> 'Y';
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C                   WRITE     PD101
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       EndIf;
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C*
     C* Write detail record into extract file.
     C*
     c                   exsr      wrtdet
     C*
     c                   endsr                                                  do_upd subroutine
     C*
     c*******************************************************************************************
     C*
     C*  Process a Delete transaction
     C*
     c*******************************************************************************************
     c     do_del        begsr
     C*
     C* Set delete action type and extract date.
     C*
     c                   move      '1'           d1type
     c                   move      x1date        d1exdt
     C*
     C* Clear response date, disposition code, and applied fields.
     C*
     c                   clear                   d1rsdt
     c                   clear                   d1code
     c                   clear                   d2hic#
     c                   clear                   d2ss#
     c                   clear                   d2lsn
     c                   clear                   d2fsn
     c                   clear                   d2mid
     c                   clear                   d2sex
     c                   clear                   d2dtob
     c                   clear                   d2mefd
     c                   clear                   d2mexd
     c                   clear                   d2pity
     C*
     C* Write new log record for transaction.
     C*
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       If flgUDSM <> 'Y';
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     c                   write     pd101
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       EndIf;
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     C*
     C* Write detail record into extract file.
     C*
     c                   exsr      wrtdet
     C*
     c                   endsr                                                  do_del subroutine
     C*
     c*******************************************************************************************
     C*
     C*  Write header record for claims file.
     C*
     c*******************************************************************************************
     c     wrthdr        begsr
     C*
     c                   clear                   pch
     C*
     c                   move      'NGCH'        chrid
     c                   move      'NGHPCLM'     chrft
     c                   move      xxdate        chsdt
     c     f2ky          chain     pf201                              60
     c                   move      f2rre#        chrre
     C*
     c                   clear                   edata
      *** Beg Add *** 02/11/13 *************************************************
      /Free
       Clear Udata;
       If xxClt# = '114' And flgUDSM = 'Y';
          Udata = PCH;
          Write Upay;
       Else;
      /End-Free
      *** End Add *** 02/11/13 *************************************************
     c                   movel     pch           edata
     c                   write     epay
      *** Beg Add *** 02/11/13 *************************************************
      /Free
       EndIf;
      /End-Free
      *** End Add *** 02/11/13 *************************************************
     C*
     c                   endsr                                                  wrthdr subroutine
     c*******************************************************************************************
     C*
     C*  Write detail record for claims file.
     C*
     c*******************************************************************************************
     c     wrtdet        begsr
     C*
     c     d2ky          chain     pd201                              60
     c                   if        *in60 = *on
     c                   clear                   pd201
     c                   move      xxco#         d2co#
     c                   move      xxfnd         d2fnd
     c                   move      x1date        d2exdt
     c                   move      xxco#         d2co#
     c                   move      q1user        d2user
     c                   move      q1user        d2iusr
     c                   move      q1pgm         d2pgm
     c                   move      q1pgm         d2ipgm
     c                   move      x1date        d2dt
     c                   move      x1date        d2idt
     c                   time                    dsdttm
     c                   move      dstime        d2time
     c                   move      dstime        d2itim
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       If flgUDSM <> 'Y';
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     c                   write     pd201
      ****Beg Add *** 02/11/13 **********************************************
      /Free
       EndIf;
      /End-Free
      ****End Add *** 02/11/13 **********************************************
     c                   endif
     C*
     c                   clear                   pcd
     C*
     c                   move      'NGCD'        cdrid
     C*
     c                   move      d1dcn#        cddcn#
     c                   move      d1type        cdtype
     C*
     c                   move      d1hic#        cdhic#
     c                   move      d1ss#         cdss#
     c                   move      d1lsn         cdlsn
     c                   move      d1fsn         cdfsn
     c                   move      d1mid         cdmid
     c                   move      d1sex         cdsex
     C*
     C                   Z-ADD     d1dtob        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cddtob
     C*
     C                   Z-ADD     d1cmdi        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cdcmdi
     C*
     C                   Z-ADD     d1acdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cdacdt
     C*
     c*** beg del *** 04/27/15 *****************************
      ***********
     c***********        move      d1ainj        cdainj
     c***********        move      d1jrst        cdjrst
     c***********        move      d1i901        cdi901
     c***********        move      d1i902        cdi902
     c***********        move      d1i903        cdi903
     c***********        move      d1i904        cdi904
     c***********        move      d1i905        cdi905
     c***********        move      d1i906        cdi906
     c***********        move      d1i907        cdi907
     c***********        move      d1i908        cdi908
     c***********        move      d1i909        cdi909
     c***********        move      d1i910        cdi910
     c***********        move      d1i911        cdi911
     c***********        move      d1i912        cdi912
     c***********        move      d1i913        cdi913
     c***********        move      d1i914        cdi914
     c***********        move      d1i915        cdi915
     c***********        move      d1i916        cdi916
     c***********        move      d1i917        cdi917
     c***********        move      d1i918        cdi918
     c***********        move      d1i919        cdi919
     c*** end del *** 04/27/15 *****************************
     c*** beg add *** 04/27/15 *****************************
     c                   if        d1ain1 <> *blank
     c                   movel(p)  d1ain1        cdainj
     c                   else                                                            dasub# = 0
     c                   movel(p)  d1ainj        cdainj
     c                   endif                                                           dasub# = 0
     c                   move      d1jrst        cdjrst
     c*
     c                   if        xxi1o = *blanks
     c                   move      '9'           cdicdi
     c                   movel(p)  d1i901        cdi901
     c                   movel(p)  d1i902        cdi902
     c                   movel(p)  d1i903        cdi903
     c                   movel(p)  d1i904        cdi904
     c                   movel(p)  d1i905        cdi905
     c                   movel(p)  d1i906        cdi906
     c                   movel(p)  d1i907        cdi907
     c                   movel(p)  d1i908        cdi908
     c                   movel(p)  d1i909        cdi909
     c                   movel(p)  d1i910        cdi910
     c                   movel(p)  d1i911        cdi911
     c                   movel(p)  d1i912        cdi912
     c                   movel(p)  d1i913        cdi913
     c                   movel(p)  d1i914        cdi914
     c                   movel(p)  d1i915        cdi915
     c                   movel(p)  d1i916        cdi916
     c                   movel(p)  d1i917        cdi917
     c                   movel(p)  d1i918        cdi918
     c                   movel(p)  d1i919        cdi919
     c*
     c                   else
     c                   move      '0'           cdicdi
     c                   movel(p)  d1i101        cdi901
     c                   movel(p)  d1i102        cdi902
     c                   movel(p)  d1i103        cdi903
     c                   movel(p)  d1i104        cdi904
     c                   movel(p)  d1i105        cdi905
     c                   movel(p)  d1i106        cdi906
     c                   movel(p)  d1i107        cdi907
     c                   movel(p)  d1i108        cdi908
     c                   movel(p)  d1i109        cdi909
     c                   movel(p)  d1i110        cdi910
     c                   movel(p)  d1i111        cdi911
     c                   movel(p)  d1i112        cdi912
     c                   movel(p)  d1i113        cdi913
     c                   movel(p)  d1i114        cdi914
     c                   movel(p)  d1i115        cdi915
     c                   movel(p)  d1i116        cdi916
     c                   movel(p)  d1i117        cdi917
     c                   movel(p)  d1i118        cdi918
     c                   movel(p)  d1i119        cdi919
     c                   endif
     c*** end add *** 04/27/15 *****************************
      *** Beg Del *** 03/06/25 *****************************
     c***                move      d1dinj        cddinj
      *** End Del *** 03/06/25 *****************************
     c                   move      d1slfi        cdslfi
     c                   move      d1slft        cdslft
     c                   move      d1plnm        cdplnm
     c                   move      d1pfnm        cdpfnm
     c                   move      d1name        cdname
     c                   move      d1fdnm        cdfdnm
     c                   move      d1pity        cdpity
     c     f2ky          chain(n)  pf201                              60
     c  n60              move      f2fein        cdfein
     c                   move      d1off#        cdoff#
     c                   move      d1cap#        cdcap#
     c                   move      d1clm#        cdclm#
     c                   move      d1pcdn        cdpcdn
     c                   move      d1pcln        cdpcln
     c                   move      d1pcfn        cdpcfn
     c                   move      d1pcph        cdpcph
     c                   move      d1pcpe        cdpcpe
     c                   move      d1nfil        cdnfil
     C*
     C                   Z-ADD     d1nfed        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cdnfed
     C*
     c                   move      d1clri        cdclri
     c                   move      d1rpln        cdrpln
     c                   move      d1rpfn        cdrpfn
     c                   move      d1rfnm        cdrfnm
     c                   move      d1plaa        cdplaa
     c                   move      d1rma1        cdrma1
     c                   move      d1rma2        cdrma2
     c                   move      d1rcty        cdrcty
     c                   move      d1rst         cdrst
     c                   move      d1rzip        cdrzip
     c                   move      d1rzp4        cdrzp4
     c                   move      d1rph#        cdrph#
     c                   move      d1rpec        cdrpec
     c                   move      d1ormi        cdormi
     C*
     C                   Z-ADD     d1ormd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cdormd
     C*
     C                   Z-ADD     d1t1sd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cdt1sd
     C*
     c                   move      d1t1am        cdt1am
     C*
     C                   Z-ADD     d1t1fd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cdt1fd
     C*
     c                   move      d1c1rl        cdc1rl
     c                   move      d1c1ss        cdc1ss
     c                   move      d1c1ln        cdc1ln
     c                   move      d1c1fn        cdc1fn
     c                   move      d1c1mi        cdc1mi
     c                   move      d1c1a1        cdc1a1
     c                   move      d1c1a2        cdc1a2
     c                   move      d1c1cy        cdc1cy
     c                   move      d1c1st        cdc1st
     c                   move      d1c1zp        cdc1zp
     c                   move      d1c1z4        cdc1z4
     c                   move      d1c1ph        cdc1ph
     c                   move      d1c1pe        cdc1pe
     c                   move      d1c1ri        cdc1ri
     c                   move      d1c1rn        cdc1rn
     c                   move      d1c1rf        cdc1rf
     c                   move      d1c1rm        cdc1rm
     c                   move      d1c1rt        cdc1rt
     c                   move      d1c1r1        cdc1r1
     c                   move      d1c1r2        cdc1r2
     c                   move      d1c1rc        cdc1rc
     c                   move      d1c1rs        cdc1rs
     c                   move      d1c1rz        cdc1rz
     c                   move      d1c1r4        cdc1r4
     c                   move      d1c1rp        cdc1rp
     c                   move      d1c1re        cdc1re
      *** Beg Add ***** 03/06/25 ***********************************************
        CDMSAAMT = D1MSAAMT;
        CDMSAPRD = D1MSAPRD;
        CDLUMSTR = D1LUMSTR;
        CDINDEPAMT = D1INDEPAMT;
        CDADEPAMT = D1ADEPAMT;
        CDCASECNT = D1CASECNT;
        CDPRADEIN = D1PRADEIN;
      *** End Add ***** 03/06/25 ***********************************************
     C*
     c                   clear                   edata
      *** Beg Add *** 02/11/13 *************************************************
      /Free
       Clear Udata;
       If xxClt# = '114' And flgUDSM = 'Y';
          EXsr UDSMedicare;
       // Udata = %Trim(UDSM) + %Trim (PCD);
       //*** Beg Chg *** 05/22/13 ***************************************   ****
       // Udata = PCD + UDSM;
          Udata = UDSM + PCD;
       //*** End Chg *** 05/22/13 ***************************************   ****
          Write Upay;
       Else;
      /End-Free
      *** End Add *** 02/11/13 *************************************************
     c                   movel     pcd           edata
     c                   write     epay
      *** Beg Add *** 02/11/13 *************************************************
      /Free
       EndIf;
      /End-Free
      *** End Add *** 02/11/13 *************************************************
     C*
     c                   add       1             xxcnt
     C*
     c                   if        d1c2rl <> *blank
     c                   exsr      wrtaux
     c                   endif
     C*
     c                   endsr                                                  wrtdet subroutine
     c*******************************************************************************************
     C*
     C*  Write auxiliary record for claims file.
     C*
     c*******************************************************************************************
     c     wrtaux        begsr
     C*
     c                   clear                   pce
     C*
     c                   move      'NGCE'        cerid
     C*
     c                   move      d1dcn#        cedcn#
     c                   move      d1hic#        cehic#
     c                   move      d1ss#         cess#
     c                   move      d1lsn         celsn
     c                   move      d1fsn         cefsn
     C*
     c                   move      d1c2rl        cec2rl
     c                   move      d1c2ss        cec2ss
     c                   move      d1c2ln        cec2ln
     c                   move      d1c2fn        cec2fn
     c                   move      d1c2mi        cec2mi
     c                   move      d1c2a1        cec2a1
     c                   move      d1c2a2        cec2a2
     c                   move      d1c2cy        cec2cy
     c                   move      d1c2st        cec2st
     c                   move      d1c2zp        cec2zp
     c                   move      d1c2z4        cec2z4
     c                   move      d1c2ph        cec2ph
     c                   move      d1c2pe        cec2pe
     c                   move      d1c2ri        cec2ri
     c                   move      d1c2rn        cec2rn
     c                   move      d1c2rf        cec2rf
     c                   move      d1c2rm        cec2rm
     c                   move      d1c2rt        cec2rt
     c                   move      d1c2r1        cec2r1
     c                   move      d1c2r2        cec2r2
     c                   move      d1c2rc        cec2rc
     c                   move      d1c2rs        cec2rs
     c                   move      d1c2rz        cec2rz
     c                   move      d1c2r4        cec2r4
     c                   move      d1c2rp        cec2rp
     c                   move      d1c2re        cec2re
     C*
     c                   move      d1c3rl        cec3rl
     c                   move      d1c3ss        cec3ss
     c                   move      d1c3ln        cec3ln
     c                   move      d1c3fn        cec3fn
     c                   move      d1c3mi        cec3mi
     c                   move      d1c3a1        cec3a1
     c                   move      d1c3a2        cec3a2
     c                   move      d1c3cy        cec3cy
     c                   move      d1c3st        cec3st
     c                   move      d1c3zp        cec3zp
     c                   move      d1c3z4        cec3z4
     c                   move      d1c3ph        cec3ph
     c                   move      d1c3pe        cec3pe
     c                   move      d1c3ri        cec3ri
     c                   move      d1c3rn        cec3rn
     c                   move      d1c3rf        cec3rf
     c                   move      d1c3rm        cec3rm
     c                   move      d1c3rt        cec3rt
     c                   move      d1c3r1        cec3r1
     c                   move      d1c3r2        cec3r2
     c                   move      d1c3rc        cec3rc
     c                   move      d1c3rs        cec3rs
     c                   move      d1c3rz        cec3rz
     c                   move      d1c3r4        cec3r4
     c                   move      d1c3rp        cec3rp
     c                   move      d1c3re        cec3re
     C*
     c                   move      d1c4rl        cec4rl
     c                   move      d1c4ss        cec4ss
     c                   move      d1c4ln        cec4ln
     c                   move      d1c4fn        cec4fn
     c                   move      d1c4mi        cec4mi
     c                   move      d1c4a1        cec4a1
     c                   move      d1c4a2        cec4a2
     c                   move      d1c4cy        cec4cy
     c                   move      d1c4st        cec4st
     c                   move      d1c4zp        cec4zp
     c                   move      d1c4z4        cec4z4
     c                   move      d1c4ph        cec4ph
     c                   move      d1c4pe        cec4pe
     c                   move      d1c4ri        cec4ri
     c                   move      d1c4rn        cec4rn
     c                   move      d1c4rf        cec4rf
     c                   move      d1c4rm        cec4rm
     c                   move      d1c4rt        cec4rt
     c                   move      d1c4r1        cec4r1
     c                   move      d1c4r2        cec4r2
     c                   move      d1c4rc        cec4rc
     c                   move      d1c4rs        cec4rs
     c                   move      d1c4rz        cec4rz
     c                   move      d1c4r4        cec4r4
     c                   move      d1c4rp        cec4rp
     c                   move      d1c4re        cec4re
     C*
     C                   Z-ADD     d1t2sd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cet2sd
     C*
     c                   move      d1t2am        cet2am
     C*
     C                   Z-ADD     d1t2fd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cet2fd
     C*
     C                   Z-ADD     d1t3sd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cet3sd
     C*
     c                   move      d1t3am        cet3am
     C*
     C                   Z-ADD     d1t3fd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cet3fd
     C*
     C                   Z-ADD     d1t4sd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cet4sd
     C*
     c                   move      d1t4am        cet4am
     C*
     C                   Z-ADD     d1t4fd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cet4fd
     C*
     C                   Z-ADD     d1t5sd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cet5sd
     C*
     c                   move      d1t5am        cet5am
     C*
     C                   Z-ADD     d1t5fd        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         cet5fd
     C*
     c                   clear                   edata
      *** Beg Add *** 02/11/13 *************************************************
      /Free
       Clear Udata;
       If xxClt# = '114' And flgUDSM = 'Y';
          EXsr UDSMedicare;
       // Udata = %Trim(UDSM) + %Trim (PCE);
       //*** Beg Chg *** 05/22/13 ***************************************   ****
       // Udata = PCE + UDSM;
          Udata = UDSM + PCE;
       //*** End Chg *** 05/22/13 ***************************************   ****
          Write Upay;
       Else;
      /End-Free
      *** End Add *** 02/11/13 *************************************************
     c                   movel     pce           edata
     c                   write     epay
      *** Beg Add *** 02/11/13 *************************************************
      /Free
       EndIf;
      /End-Free
      *** End Add *** 02/11/13 *************************************************
     C*
     c                   add       1             xxcnt
     C*
     c                   endsr                                                  wrtdet subroutine
     c*******************************************************************************************
     C*
     C*  Write trailer record for claims file.
     C*
     c*******************************************************************************************
     c     wrttrl        begsr
     C*
     c                   clear                   pct
     C*
     c                   move      'NGCT'        ctrid
     c                   move      'NGHPCLM'     ctrft
     c                   move      xxdate        ctsdt
     c                   move      xxcnt         ctcnt
     c     f2ky          chain     pf201                              60
     c                   move      f2rre#        ctrre
     C*
     c                   clear                   edata
      *** Beg Add *** 02/11/13 *************************************************
      /Free
       Clear Udata;
       If xxClt# = '114' And flgUDSM = 'Y';
          Udata = PCT;
          Write Upay;
       Else;
      /End-Free
      *** End Add *** 02/11/13 *************************************************
     c                   movel     pct           edata
     c                   write     epay
      *** Beg Add *** 02/11/13 *************************************************
      /Free
       EndIf;
      /End-Free
      *** End Add *** 02/11/13 *************************************************
     C*
     c                   endsr                                                  wrttrl subroutine
     c*******************************************************************************************
     C*
     C*  Write records for the TIN extract file.
     C*
     c*******************************************************************************************
     c     wrttin        begsr
     C*
     c     f2ky          chain     pf201                              60
     C*
     C* Write TIN file header record.
     C*
     c                   clear                   pth
     C*
     c                   move      'NGTH'        thrid
     c                   move      'NGHPTIN'     thrft
     c                   move      xxdate        thsdt
     c                   move      f2rre#        thrre
     C*
     c                   clear                   tdata
     c                   movel     pth           tdata
     c                   write     tpay
     C*
     c                   z-add     0             xxcnt
     C*
     c                   z-add     1             x
     C*
     C* Write TIN Detail record for each office that was reported.
     C*
     c                   dow       ofa(x) <> *blanks
     C*
     c                   move      ofa(x)        rreFeinOff
     c                   move      ofa(x)        xxoff#
     c     ofky          chain     pof01                              60
     C*
     c                   if        *in60 = *off
     C*
     C* Write TIN file detail record.
     C*
     c                   clear                   ptd
     C*
     c                   move      'NGTD'        tdrid
     c                   eval      yyrre#=%subst(rreFeinOff:1:9)
     c                   eval      yyfein=%subst(rreFeinOff:10:9)
     c                   move      yyrre#        tdrre
     c                   move      yyfein        tdfein
     C*
     c                   move      xxoff#        tmoff#
     c                   if        xxoff# <>  ' '
     c                   eval      tdstid = %editc(tmoff#:'X')
     c                   else
     c                   movel     *blanks       tdstid
     c                   endif
     c                   movel     ofname        tdname

      * remove '-' from name

     c                   dow       %scan ('-': tdname) <> 0
     c                   eval      tdname = %replace(' ': tdname:
     c                                      %scan ('-': tdname): 1)
     c                   enddo
     c
     c                   movel     ofadr1        tdadr1
     c                   movel     ofadr2        tdadr2
     c                   movel     ofcty         tdcty
     c                   movel     ofst          tdst
     c                   movel     ofzip         tdzip
     c                   move      ofzip         tdzip4
     C*
     c                   clear                   tdata
     c                   movel     ptd           tdata
     c                   write     tpay
     c                   add       1             xxcnt
     c*
     c                   endif                                                  if *in60 = *off
     c*
     c                   add       1             x
     c                   enddo
     C*
     C* Write TIN file trailer record.
     C*
     c                   clear                   ptt
     C*
     c                   move      'NGTT'        ttrid
     c                   move      'NGHPTIN'     ttrft
     c                   move      xxdate        ttsdt
     c                   move      f2rre#        ttrre
     c                   z-add     xxcnt         ttcnt
     C*
     c                   clear                   tdata
     c                   movel     ptt           tdata
     c                   write     tpay
     C*
     c                   endsr                                                  wrttrl subroutine
     c*******************************************************************************************
     C*
     C*  Copy data from WXMDP over to WDD1P.
     C*
     c*******************************************************************************************
     c     cpyrec        begsr
     C*
     C                   CLEAR                   PD101
     C*
     c                   move      MDCO#         D1CO#
     c                   move      MDFND         D1FND
     c                   move      MDFYR         D1FYR
     c                   move      MDCASE        D1CASE
     c                   move      MDDCN#        D1DCN#
     c                   move      MDEXDT        D1EXDT
     c                   move      MDHIC#        D1HIC#
     c                   move      MDSS#         D1SS#
     c                   move      MDLSN         D1LSN
     c                   move      MDFSN         D1FSN
     c                   move      MDMID         D1MID
     c                   move      MDSEX         D1SEX
     c                   move      MDDTOB        D1DTOB
     c                   move      MDCMDI        D1CMDI
     c                   move      MDACDT        D1ACDT
     c*** beg add *** 04/27/15 *****************************
     c                   move      MDAIN1        D1AIN1
     c                   move      MDAINJ        D1AINJ
     c                   move      MDJRST        D1JRST
     c                   move      MDI901        D1I901
     c                   move      MDI902        D1I902
     c                   move      MDI903        D1I903
     c                   move      MDI904        D1I904
     c                   move      MDI905        D1I905
     c                   move      MDI906        D1I906
     c                   move      MDI907        D1I907
     c                   move      MDI908        D1I908
     c                   move      MDI909        D1I909
     c                   move      MDI910        D1I910
     c                   move      MDI911        D1I911
     c                   move      MDI912        D1I912
     c                   move      MDI913        D1I913
     c                   move      MDI914        D1I914
     c                   move      MDI915        D1I915
     c                   move      MDI916        D1I916
     c                   move      MDI917        D1I917
     c                   move      MDI918        D1I918
     c                   move      MDI919        D1I919
     c*** beg add *** 04/27/15 *****************************
     c                   move      MDI101        D1I101
     c                   move      MDI102        D1I102
     c                   move      MDI103        D1I103
     c                   move      MDI104        D1I104
     c                   move      MDI105        D1I105
     c                   move      MDI106        D1I106
     c                   move      MDI107        D1I107
     c                   move      MDI108        D1I108
     c                   move      MDI109        D1I109
     c                   move      MDI110        D1I110
     c                   move      MDI111        D1I111
     c                   move      MDI112        D1I112
     c                   move      MDI113        D1I113
     c                   move      MDI114        D1I114
     c                   move      MDI115        D1I115
     c                   move      MDI116        D1I116
     c                   move      MDI117        D1I117
     c                   move      MDI118        D1I118
     c                   move      MDI119        D1I119
     c*** beg chg *** 04/27/15 *****************************
     c***********        move      MDDINJ        D1DINJ
      *** Beg Del *** 03/06/25 *****************************
     c***                movel(p)  MDDINJ        D1DINJ
      *** End Del *** 03/06/25 *****************************
     c*** end chg *** 04/27/15 *****************************
     c                   move      MDSLFI        D1SLFI
     c                   move      MDSLFT        D1SLFT
     c                   move      MDPLNM        D1PLNM
     c                   move      MDPFNM        D1PFNM
     c                   move      MDNAME        D1NAME
     c                   move      MDFDNM        D1FDNM
     c                   move      MDPITY        D1PITY
     c                   move      MDFEIN        D1FEIN
     c                   move      MDOFF#        D1OFF#
     c                   move      MDCAP#        D1CAP#
     c                   move      MDCLM#        D1CLM#
     c                   move      MDPCDN        D1PCDN
     c                   move      MDPCLN        D1PCLN
     c                   move      MDPCFN        D1PCFN
     c                   move      MDPCPH        D1PCPH
     c                   move      MDPCPE        D1PCPE
     c                   move      MDNFIL        D1NFIL
     c                   move      MDNFED        D1NFED
     c                   move      MDCLRI        D1CLRI
     c                   move      MDRPLN        D1RPLN
     c                   move      MDRPFN        D1RPFN
     c                   move      MDRFNM        D1RFNM
     c                   move      MDPLAA        D1PLAA
     c                   move      MDRMA1        D1RMA1
     c                   move      MDRMA2        D1RMA2
     c                   move      MDRCTY        D1RCTY
     c                   move      MDRST         D1RST
     c                   move      MDRZIP        D1RZIP
     c                   move      MDRZP4        D1RZP4
     c                   move      MDRPH#        D1RPH#
     c                   move      MDRPEC        D1RPEC
     c                   move      MDORMI        D1ORMI
     c                   move      MDORMD        D1ORMD
     c                   move      MDT1SD        D1T1SD
     c                   move      MDT1AM        D1T1AM
     c                   move      MDT1FD        D1T1FD
     c                   move      MDC1RL        D1C1RL
     c                   move      MDC1SS        D1C1SS
     c                   move      MDC1LN        D1C1LN
     c                   move      MDC1FN        D1C1FN
     c                   move      MDC1MI        D1C1MI
     c                   move      MDC1A1        D1C1A1
     c                   move      MDC1A2        D1C1A2
     c                   move      MDC1CY        D1C1CY
     c                   move      MDC1ST        D1C1ST
     c                   move      MDC1ZP        D1C1ZP
     c                   move      MDC1Z4        D1C1Z4
     c                   move      MDC1PH        D1C1PH
     c                   move      MDC1PE        D1C1PE
     c                   move      MDC1RI        D1C1RI
     c                   move      MDC1RN        D1C1RN
     c                   move      MDC1RF        D1C1RF
     c                   move      MDC1RM        D1C1RM
     c                   move      MDC1RT        D1C1RT
     c                   move      MDC1R1        D1C1R1
     c                   move      MDC1R2        D1C1R2
     c                   move      MDC1RC        D1C1RC
     c                   move      MDC1RS        D1C1RS
     c                   move      MDC1RZ        D1C1RZ
     c                   move      MDC1R4        D1C1R4
     c                   move      MDC1RP        D1C1RP
     c                   move      MDC1RE        D1C1RE
     c                   move      MDC2RL        D1C2RL
     c                   move      MDC2SS        D1C2SS
     c                   move      MDC2LN        D1C2LN
     c                   move      MDC2FN        D1C2FN
     c                   move      MDC2MI        D1C2MI
     c                   move      MDC2A1        D1C2A1
     c                   move      MDC2A2        D1C2A2
     c                   move      MDC2CY        D1C2CY
     c                   move      MDC2ST        D1C2ST
     c                   move      MDC2ZP        D1C2ZP
     c                   move      MDC2Z4        D1C2Z4
     c                   move      MDC2PH        D1C2PH
     c                   move      MDC2PE        D1C2PE
     c                   move      MDC2RI        D1C2RI
     c                   move      MDC2RN        D1C2RN
     c                   move      MDC2RF        D1C2RF
     c                   move      MDC2RM        D1C2RM
     c                   move      MDC2RT        D1C2RT
     c                   move      MDC2R1        D1C2R1
     c                   move      MDC2R2        D1C2R2
     c                   move      MDC2RC        D1C2RC
     c                   move      MDC2RS        D1C2RS
     c                   move      MDC2RZ        D1C2RZ
     c                   move      MDC2R4        D1C2R4
     c                   move      MDC2RP        D1C2RP
     c                   move      MDC2RE        D1C2RE
     c                   move      MDC3RL        D1C3RL
     c                   move      MDC3SS        D1C3SS
     c                   move      MDC3LN        D1C3LN
     c                   move      MDC3FN        D1C3FN
     c                   move      MDC3MI        D1C3MI
     c                   move      MDC3A1        D1C3A1
     c                   move      MDC3A2        D1C3A2
     c                   move      MDC3CY        D1C3CY
     c                   move      MDC3ST        D1C3ST
     c                   move      MDC3ZP        D1C3ZP
     c                   move      MDC3Z4        D1C3Z4
     c                   move      MDC3PH        D1C3PH
     c                   move      MDC3PE        D1C3PE
     c                   move      MDC3RI        D1C3RI
     c                   move      MDC3RN        D1C3RN
     c                   move      MDC3RF        D1C3RF
     c                   move      MDC3RM        D1C3RM
     c                   move      MDC3RT        D1C3RT
     c                   move      MDC3R1        D1C3R1
     c                   move      MDC3R2        D1C3R2
     c                   move      MDC3RC        D1C3RC
     c                   move      MDC3RS        D1C3RS
     c                   move      MDC3RZ        D1C3RZ
     c                   move      MDC3R4        D1C3R4
     c                   move      MDC3RP        D1C3RP
     c                   move      MDC3RE        D1C3RE
     c                   move      MDC4RL        D1C4RL
     c                   move      MDC4SS        D1C4SS
     c                   move      MDC4LN        D1C4LN
     c                   move      MDC4FN        D1C4FN
     c                   move      MDC4MI        D1C4MI
     c                   move      MDC4A1        D1C4A1
     c                   move      MDC4A2        D1C4A2
     c                   move      MDC4CY        D1C4CY
     c                   move      MDC4ST        D1C4ST
     c                   move      MDC4ZP        D1C4ZP
     c                   move      MDC4Z4        D1C4Z4
     c                   move      MDC4PH        D1C4PH
     c                   move      MDC4PE        D1C4PE
     c                   move      MDC4RI        D1C4RI
     c                   move      MDC4RN        D1C4RN
     c                   move      MDC4RF        D1C4RF
     c                   move      MDC4RM        D1C4RM
     c                   move      MDC4RT        D1C4RT
     c                   move      MDC4R1        D1C4R1
     c                   move      MDC4R2        D1C4R2
     c                   move      MDC4RC        D1C4RC
     c                   move      MDC4RS        D1C4RS
     c                   move      MDC4RZ        D1C4RZ
     c                   move      MDC4R4        D1C4R4
     c                   move      MDC4RP        D1C4RP
     c                   move      MDC4RE        D1C4RE
     c                   move      MDT2SD        D1T2SD
     c                   move      MDT2AM        D1T2AM
     c                   move      MDT2FD        D1T2FD
     c                   move      MDT3SD        D1T3SD
     c                   move      MDT3AM        D1T3AM
     c                   move      MDT3FD        D1T3FD
     c                   move      MDT4SD        D1T4SD
     c                   move      MDT4AM        D1T4AM
     c                   move      MDT4FD        D1T4FD
     c                   move      MDT5SD        D1T5SD
     c                   move      MDT5AM        D1T5AM
     c                   move      MDT5FD        D1T5FD
     c                   if        @@srt <> *blank
     c                   move      mdclex        d1clex
     C                   endif
      *** Beg Add ***** 03/06/25 ******************************
        D1MSAAMT = MDMSAAMT;
        D1MSAPRD = MDMSAPRD;
        D1LUMSTR = MDLUMSTR;
        D1INDEPAMT = MDINDEPAMT;
        D1ADEPAMT = MDADEPAMT;
        D1CASECNT = MDCASECNT;
        D1PRADEIN = MDPRADEIN;
      *** End Add ***** 03/06/25 ******************************
     C*
     C                   MOVE      Q1PGM         D1PGM
     C                   MOVE      Q1PGM         D1IPGM
     C                   MOVE      Q1USER        D1USER
     C                   MOVE      Q1USER        D1IUSR
     C                   MOVE      X1DATE        D1DT
     C                   MOVE      X1DATE        D1IDT
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        D1TIME
     C                   MOVE      DSTIME        D1ITIM
     C*
     c                   endsr                                                  cpyrec subroutine
     c*******************************************************************************************
     C*******************************************************************************************
     C*
     C* Check if the error code "SP31" was encountered in the previous response file import
     C*
     C*******************************************************************************************
     C     prvSP31       begsr
     C*
     C                   eval      Tmexdt = *ZEROS
     C                   eval      FlSp31 = 'N'
     C*
     C     e5ky          setgt     PE501
     C     e5ky          readpe    PE501                                  60
     C                   if        *in60 = *off
     C*
     C* store date for previous extract for claim read.
     C                   eval      Tmexdt = E5EXDT
     C*
     C                   dow       *in60 = *off
     C*
      **** Begin change **** 10/21/15 ***********************************************************
     C***                   if        E5CODE = 'SP31'
     C***                   eval      FlSp31 = 'Y'
     C***                   leavesr
     C***                   endif
      /Free
         if E5CODE = 'SP31' and E5STAT <> 'C';
            FlSp31 = 'Y';
            leavesr;
         endif;
      /End-free
      **** End change **** 10/21/15 ***********************************************************
     C*
     C     e5ky1         readpe    PE501                                  60
     C                   enddo
     C                   endif
     C*
     C                   endsr
      *** Beg Add *** 02/11/13 ******************************************************************
      *******************************************************************************************
      * UDSMedicare - Retriving the UDS Medicare values
      *******************************************************************************************
      /Free
       Begsr UDSMedicare;

       UDSM = *Blanks;

       charRcdTyp = 'M';
       charICNaicNo = charNaic;

       Chain keyWMNCL pcc01;
       If %Found(WMNCL);
          charICClmNo  = CCicc#;
          charRcvClmNo = CCrc#;
       Endif;

       charFndClmNo = %Editc(CMco#:'X') + %Editc(CMfnd:'X') +
                      %Editc(CMfyr:'X') + %Editc(CMcase:'X');
       charClaimNo  = '    1';

       EndSr;
      /End-Free
      *** End Add *** 02/11/13 ******************************************************************
