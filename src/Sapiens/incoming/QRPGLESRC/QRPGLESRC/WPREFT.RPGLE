     D*=====================================================================
     D* EFT processing procedure prototypes
     D*=====================================================================
     P*====================================================================
     P* EFTADD - Popup for Add/Edit EFT
     P*====================================================================
     d EFTAdd          PR
     d eMode                          1                                         Mode
     d eType                          1                                         Payee Type
     d eFnd                           3  0                                      Payee Type
     d eRrn                           9  0                                      RRN - EDIT WMEFP
     d eRrn1                          9  0                                      RRN - ADD MODE
     d eEnv                           1    const options(*nopass)               RRN - ADD MODE
     d inPolYr                        3  0 const  options(*nopass)              Policy year.
     P*====================================================================
     P* ReqPreNote - Request Pre-Note
     P*====================================================================
     d ReqPreNote      PR
     d eRrn                           9  0                                      RRN - Read WMEFP
     P*====================================================================
     P* ClrPreNote - Clear Pre-Note
     P*====================================================================
     d ClrPreNote      PR
     d eRrn                           9  0                                      RRN - Read WMEFP
     P*====================================================================
     P* EFTBankInfo - EFT Bank Info
     P*====================================================================
     d EFTBankInfo     PR
     d eRrn                           9  0                                      RRN - Read WMEFP
      //----------------------------------------------------------------------
      // BuildEFTkey - Build EFT key
      //----------------------------------------------------------------------
     D BuildEFTkey     Pr            30
     D  inType                        1    const                                1
     D  inEmp#                        9  0 options(*nopass) const               2
     D  inDiv                         5  0 options(*nopass) const               3
     D  inEATP                        2    options(*nopass) const               4
     D  inFYR                         3  0 options(*nopass) const               5
     D  inCase                        7  0 options(*nopass) const               6
     D  inDSeq                        5  0 options(*nopass) const               7
     D  inFein                        9  0 options(*nopass) const               8
     D  inMod                         3  0 options(*nopass) const               9
     D  inAgt#                        5  0 options(*nopass) const               10
