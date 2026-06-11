     Hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
     H**********************************************************************
     H/TITLE WTPOLCFG - Policy Configuration.
     H DFTNAME(WTPOLCFG)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Created   : 05/14/18                                              *
      *  Programmer: C:291                                                 *
      *  Project   : 150WCR                                                *
      *====================================================================*
      *  Revisions:
      *
      *  04/23/25 C:342 Support: 118918
      *                 Do not clear data of Policy/Quote tab while changing
      *                 other tab information- Without pressing Save button
      *
      *  08/01/24 C:291 Project: WC2976
      *                 Add Security for Audit Reversal process under security
      *                 section of Poicy Configuration.
      *
      *  03/21/24 C:300 Project: WC2433
      *                 New Policy Endorsement Forms Screens - Implement Security
      *                 Control on each item in More Action drop-down
      *
      *  11/01/23 C:300 Project: WC2676
      *                 New Return Funds Batches Screens - Implement Security
      *                 Control on each item in More Action drop-down
      *
      *  04/24/23 C:300 Project: WC2675
      *                 New AR Batch Screens - Implement Security Control on
      *                 each item in More Action drop-down and toolbar option
      *
      *  09/12/22 C:342 Support: 103037
      *                 Hide Schedule rating entry & Schedule rating info
      *                 fields from policy configuration screen.
      *  06/22/22 C:079 Support: 102638
      *                 Load Action Panel Items using service pgm
      *
      *  05/09/22 C:079 Project: 129WWV
      *                 Application Configuration Security
      *
      *  03/24/22 C:340 Project: WC2858
      *                 Add Config/Setup Screen for Quick Policy/Quote
      *
      *  03/14/22 C:079 Project: 150WFR
      *                 Add Grp Short Name to Quote & Policy Changes
      *
      *  05/27/21 C:318 Support:95270
      *                 Changes for inquiry mode to make fields disable
      *                 and links invisible
      *
      *  03/22/21 C:295 Ticket: 95095
      *                 Change import email recipient.
      *
      *  12/29/20 C:062 Ticket: 93616(133)
      *                 Change import edits
      *
      *  10/28/20 C:079 Project: WC2671
      *                 Scriptura Audit Request changes
      *
      *  05/15/20 C:136 Project: 150WEU
      *                 Add Import/Export tab
      *                 Import and Output Folder can be changed by
      *                 changing the lock flags in SDACP.
      *
      *  02/07/20 C:053  Project WC2689
      *                  Location Deletion Claim Edit
      *
      *  03/04/19  C:33  Project SY390
      *                  Soft-code 'Portal'
      *                                                                    *
      *  11/08/2018 c:180 Project 150WDC                                   *
      *                   DSPF changes only. No pgm changes.               *
      *                   Add hover text to "Allow Override of PEO..."     *
      *                                                                    *
      *  08/23/2018 c:62 Project SY390
      *                  Add portal menu option
      *                                                                    *
      *  08/08/2018 c:62 Project 150WCS
      *                  Add new all division fields
      *                                                                    *
      * -------------------------------------------------------------------*
      *  NOTE: Please add comments to the top of Revisions.                *
      * -------------------------------------------------------------------*
      *====================================================================*

     fwtpolcfg  cf   e             workstn extfile('WTPOLCFG')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     fsdacl     uf a e           k disk    rename(pac01:pac00)
     f                                     infds(dbfds)
     f*** Beg Add *** 03/14/22 **********************************************
     fwmpal1    uf a e           k disk
     f*** End Add *** 03/14/22 **********************************************
     f*** Beg Add *** 03/24/22 **********************************************
     fWTPQL     UF A E           K Disk
     f*** End Add *** 03/24/22 **********************************************
     f*** Beg Add *** 05/09/22 *************************************************
     fsmupl     if   e           k disk
     fsdtbl1    if   e           k disk
     f*** End Add *** 05/09/22 *************************************************

      // constant
     d intArrDim       c                   10
     d intFlgDim       c                   10
      //** Beg Chg *** 04/24/23 **********************************************
     d***** intKeyDim       c                    8
      //** Beg Chg *** 11/01/23 **********************************************
     d***** intKeyDim       c                   17
      //** Beg Chg *** 03/21/24 **********************************************
     d***** intKeyDim       c                   19
      //*** Beg Chg ***** 08/01/24 *********************************************
     d***** intKeyDim       c                   23
     d intKeyDim       c                   24
      //*** End Chg ***** 08/01/24 *********************************************
      //** End Chg *** 03/21/24 **********************************************
      //** End Chg *** 11/01/23 **********************************************
      //** End Chg *** 04/24/23 **********************************************

      * system data structure
     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0

      // Database file fields for security type
     d                 ds
     d dsSecTypFl              1     10    dim(intFlgDim)
     d ACstp1                  1      1
     d ACstp2                  2      2
     d ACstp3                  3      3
     d ACstp4                  4      4
     d ACstp5                  5      5
     d ACstp6                  6      6
     d ACstp7                  7      7
     d ACstp8                  8      8
     d ACstp9                  9      9
     d ACstp0                 10     10

      // Database file fields for security level
     d                 ds
     d dsSecLvlFl              1     30  0 dim(intFlgDim)
     d ACslv1                  1      3  0
     d ACslv2                  4      6  0
     d ACslv3                  7      9  0
     d ACslv4                 10     12  0
     d ACslv5                 13     15  0
     d ACslv6                 16     18  0
     d ACslv7                 19     21  0
     d ACslv8                 22     24  0
     d ACslv9                 25     27  0
     d ACslv0                 28     30  0

      // Security Type
     d                 ds
     d dsSecTyp                1     10
     d                                     dim(intArrDim)
     d  scAOvPMTp1             1      1
     d  scAOvPMTp2             2      2
     d  scAOvPMTp3             3      3
     d  scAOvPMTp4             4      4
     d  scAOvPMTp5             5      5
     d  scAOvPMTp6             6      6
     d  scAOvPMTp7             7      7
     d  scAOvPMTp8             8      8
     d  scAOvPMTp9             9      9
     d  scAOvPMTp0            10     10

      // Security Level
     d                 ds
     d dsSecLvl                1     30  0
     d                                     dim(intArrDim)
     d  scAOvPMLv1             1      3  0
     d  scAOvPMLv2             4      6  0
     d  scAOvPMLv3             7      9  0
     d  scAOvPMLv4            10     12  0
     d  scAOvPMLv5            13     15  0
     d  scAOvPMLv6            16     18  0
     d  scAOvPMLv7            19     21  0
     d  scAOvPMLv8            22     24  0
     d  scAOvPMLv9            25     27  0
     d  scAOvPMLv0            28     30  0

      // Security Type Error Indicator
     d                 ds
     d dsSecTypEr              1     10
     d                                     dim(intArrDim)
     d  erAOvPMTp1             1      1n
     d  erAOvPMTp2             2      2n
     d  erAOvPMTp3             3      3n
     d  erAOvPMTp4             4      4n
     d  erAOvPMTp5             5      5n
     d  erAOvPMTp6             6      6n
     d  erAOvPMTp7             7      7n
     d  erAOvPMTp8             8      8n
     d  erAOvPMTp9             9      9n
     d  erAOvPMTp0            10     10n

      // Security Level Error Indicator
     d                 ds
     d dsSecLvlEr              1     10
     d                                     dim(intArrDim)
     d  erAOvPMLv1             1      1n
     d  erAOvPMLv2             2      2n
     d  erAOvPMLv3             3      3n
     d  erAOvPMLv4             4      4n
     d  erAOvPMLv5             5      5n
     d  erAOvPMLv6             6      6n
     d  erAOvPMLv7             7      7n
     d  erAOvPMLv8             8      8n
     d  erAOvPMLv9             9      9n
     d  erAOvPMLv0            10     10n

      // Single Division
      // Single Division Security Type
     d                 ds
     d dsSecTypSD              1     10
     d                                     dim(intArrDim)
     d  scAOvSDTp1             1      1
     d  scAOvSDTp2             2      2
     d  scAOvSDTp3             3      3
     d  scAOvSDTp4             4      4
     d  scAOvSDTp5             5      5
     d  scAOvSDTp6             6      6
     d  scAOvSDTp7             7      7
     d  scAOvSDTp8             8      8
     d  scAOvSDTp9             9      9
     d  scAOvSDTp0            10     10

      // Single Division Security Level
     d                 ds
     d dsSecLvlSD              1     30  0
     d                                     dim(intArrDim)
     d  scAOvSDLv1             1      3  0
     d  scAOvSDLv2             4      6  0
     d  scAOvSDLv3             7      9  0
     d  scAOvSDLv4            10     12  0
     d  scAOvSDLv5            13     15  0
     d  scAOvSDLv6            16     18  0
     d  scAOvSDLv7            19     21  0
     d  scAOvSDLv8            22     24  0
     d  scAOvSDLv9            25     27  0
     d  scAOvSDLv0            28     30  0

      // Single Division Security Type Error Indicator
     d                 ds
     d dsSecTypErSD            1     10
     d                                     dim(intArrDim)
     d  erAOvSDTp1             1      1n
     d  erAOvSDTp2             2      2n
     d  erAOvSDTp3             3      3n
     d  erAOvSDTp4             4      4n
     d  erAOvSDTp5             5      5n
     d  erAOvSDTp6             6      6n
     d  erAOvSDTp7             7      7n
     d  erAOvSDTp8             8      8n
     d  erAOvSDTp9             9      9n
     d  erAOvSDTp0            10     10n

      // Single Division Security Level Error Indicator
     d                 ds
     d dsSecLvlErSD            1     10
     d                                     dim(intArrDim)
     d  erAOvSDLv1             1      1n
     d  erAOvSDLv2             2      2n
     d  erAOvSDLv3             3      3n
     d  erAOvSDLv4             4      4n
     d  erAOvSDLv5             5      5n
     d  erAOvSDLv6             6      6n
     d  erAOvSDLv7             7      7n
     d  erAOvSDLv8             8      8n
     d  erAOvSDLv9             9      9n
     d  erAOvSDLv0            10     10n

      // Division Range
      // Division Range Security Type
     d                 ds
     d dsSecTypDR              1     10
     d                                     dim(intArrDim)
     d  scAOvDRTp1             1      1
     d  scAOvDRTp2             2      2
     d  scAOvDRTp3             3      3
     d  scAOvDRTp4             4      4
     d  scAOvDRTp5             5      5
     d  scAOvDRTp6             6      6
     d  scAOvDRTp7             7      7
     d  scAOvDRTp8             8      8
     d  scAOvDRTp9             9      9
     d  scAOvDRTp0            10     10

      // Division Range Security Level
     d                 ds
     d dsSecLvlDR              1     30  0
     d                                     dim(intArrDim)
     d  scAOvDRLv1             1      3  0
     d  scAOvDRLv2             4      6  0
     d  scAOvDRLv3             7      9  0
     d  scAOvDRLv4            10     12  0
     d  scAOvDRLv5            13     15  0
     d  scAOvDRLv6            16     18  0
     d  scAOvDRLv7            19     21  0
     d  scAOvDRLv8            22     24  0
     d  scAOvDRLv9            25     27  0
     d  scAOvDRLv0            28     30  0

      // Division Range Security Type Error Indicator
     d                 ds
     d dsSecTypErDR            1     10
     d                                     dim(intArrDim)
     d  erAOvDRTp1             1      1n
     d  erAOvDRTp2             2      2n
     d  erAOvDRTp3             3      3n
     d  erAOvDRTp4             4      4n
     d  erAOvDRTp5             5      5n
     d  erAOvDRTp6             6      6n
     d  erAOvDRTp7             7      7n
     d  erAOvDRTp8             8      8n
     d  erAOvDRTp9             9      9n
     d  erAOvDRTp0            10     10n

      // Division Range Security Level Error Indicator
     d                 ds
     d dsSecLvlErDR            1     10
     d                                     dim(intArrDim)
     d  erAOvDRLv1             1      1n
     d  erAOvDRLv2             2      2n
     d  erAOvDRLv3             3      3n
     d  erAOvDRLv4             4      4n
     d  erAOvDRLv5             5      5n
     d  erAOvDRLv6             6      6n
     d  erAOvDRLv7             7      7n
     d  erAOvDRLv8             8      8n
     d  erAOvDRLv9             9      9n
     d  erAOvDRLv0            10     10n

      // Unbilled Division
      // Unbilled Division Security Type
     d                 ds
     d dsSecTypUD              1     10
     d                                     dim(intArrDim)
     d  scAOvUDTp1             1      1
     d  scAOvUDTp2             2      2
     d  scAOvUDTp3             3      3
     d  scAOvUDTp4             4      4
     d  scAOvUDTp5             5      5
     d  scAOvUDTp6             6      6
     d  scAOvUDTp7             7      7
     d  scAOvUDTp8             8      8
     d  scAOvUDTp9             9      9
     d  scAOvUDTp0            10     10

      // Unbilled Division Security Level
     d                 ds
     d dsSecLvlUD              1     30  0
     d                                     dim(intArrDim)
     d  scAOvUDLv1             1      3  0
     d  scAOvUDLv2             4      6  0
     d  scAOvUDLv3             7      9  0
     d  scAOvUDLv4            10     12  0
     d  scAOvUDLv5            13     15  0
     d  scAOvUDLv6            16     18  0
     d  scAOvUDLv7            19     21  0
     d  scAOvUDLv8            22     24  0
     d  scAOvUDLv9            25     27  0
     d  scAOvUDLv0            28     30  0

      // Unbilled Division Security Type Error Indicator
     d                 ds
     d dsSecTypErUD            1     10
     d                                     dim(intArrDim)
     d  erAOvUDTp1             1      1n
     d  erAOvUDTp2             2      2n
     d  erAOvUDTp3             3      3n
     d  erAOvUDTp4             4      4n
     d  erAOvUDTp5             5      5n
     d  erAOvUDTp6             6      6n
     d  erAOvUDTp7             7      7n
     d  erAOvUDTp8             8      8n
     d  erAOvUDTp9             9      9n
     d  erAOvUDTp0            10     10n

      // Unbilled Division Security Level Error Indicator
     d                 ds
     d dsSecLvlErUD            1     10
     d                                     dim(intArrDim)
     d  erAOvUDLv1             1      1n
     d  erAOvUDLv2             2      2n
     d  erAOvUDLv3             3      3n
     d  erAOvUDLv4             4      4n
     d  erAOvUDLv5             5      5n
     d  erAOvUDLv6             6      6n
     d  erAOvUDLv7             7      7n
     d  erAOvUDLv8             8      8n
     d  erAOvUDLv9             9      9n
     d  erAOvUDLv0            10     10n

      // Audit All Division
      // Audit All Division Security Type
     d                 ds
     d dsSecTypAD              1     10
     d                                     dim(intArrDim)
     d  scAOvADTp1             1      1
     d  scAOvADTp2             2      2
     d  scAOvADTp3             3      3
     d  scAOvADTp4             4      4
     d  scAOvADTp5             5      5
     d  scAOvADTp6             6      6
     d  scAOvADTp7             7      7
     d  scAOvADTp8             8      8
     d  scAOvADTp9             9      9
     d  scAOvADTp0            10     10

      // Audit All Division Security Level
     d                 ds
     d dsSecLvlAD              1     30  0
     d                                     dim(intArrDim)
     d  scAOvADLv1             1      3  0
     d  scAOvADLv2             4      6  0
     d  scAOvADLv3             7      9  0
     d  scAOvADLv4            10     12  0
     d  scAOvADLv5            13     15  0
     d  scAOvADLv6            16     18  0
     d  scAOvADLv7            19     21  0
     d  scAOvADLv8            22     24  0
     d  scAOvADLv9            25     27  0
     d  scAOvADLv0            28     30  0

      // Audit All Division Security Type Error Indicator
     d                 ds
     d dsSecTypErAD            1     10
     d                                     dim(intArrDim)
     d  erAOvADTp1             1      1n
     d  erAOvADTp2             2      2n
     d  erAOvADTp3             3      3n
     d  erAOvADTp4             4      4n
     d  erAOvADTp5             5      5n
     d  erAOvADTp6             6      6n
     d  erAOvADTp7             7      7n
     d  erAOvADTp8             8      8n
     d  erAOvADTp9             9      9n
     d  erAOvADTp0            10     10n

      // Audit All Division Security Level Error Indicator
     d                 ds
     d dsSecLvlErAD            1     10
     d                                     dim(intArrDim)
     d  erAOvADLv1             1      1n
     d  erAOvADLv2             2      2n
     d  erAOvADLv3             3      3n
     d  erAOvADLv4             4      4n
     d  erAOvADLv5             5      5n
     d  erAOvADLv6             6      6n
     d  erAOvADLv7             7      7n
     d  erAOvADLv8             8      8n
     d  erAOvADLv9             9      9n
     d  erAOvADLv0            10     10n


      // Location Deletion
      // Location Deletion Security Type
     d                 ds
     d dsSecTypDL              1     10
     d                                     dim(intArrDim)
     d  scAOvDLTp1             1      1
     d  scAOvDLTp2             2      2
     d  scAOvDLTp3             3      3
     d  scAOvDLTp4             4      4
     d  scAOvDLTp5             5      5
     d  scAOvDLTp6             6      6
     d  scAOvDLTp7             7      7
     d  scAOvDLTp8             8      8
     d  scAOvDLTp9             9      9
     d  scAOvDLTp0            10     10

      // Location Deletion Security Level
     d                 ds
     d dsSecLvlDL              1     30  0
     d                                     dim(intArrDim)
     d  scAOvDLLv1             1      3  0
     d  scAOvDLLv2             4      6  0
     d  scAOvDLLv3             7      9  0
     d  scAOvDLLv4            10     12  0
     d  scAOvDLLv5            13     15  0
     d  scAOvDLLv6            16     18  0
     d  scAOvDLLv7            19     21  0
     d  scAOvDLLv8            22     24  0
     d  scAOvDLLv9            25     27  0
     d  scAOvDLLv0            28     30  0

      // Location Deletion Security Type Error Indicator
     d                 ds
     d dsSecTypErDL            1     10
     d                                     dim(intArrDim)
     d  erAOvDLTp1             1      1n
     d  erAOvDLTp2             2      2n
     d  erAOvDLTp3             3      3n
     d  erAOvDLTp4             4      4n
     d  erAOvDLTp5             5      5n
     d  erAOvDLTp6             6      6n
     d  erAOvDLTp7             7      7n
     d  erAOvDLTp8             8      8n
     d  erAOvDLTp9             9      9n
     d  erAOvDLTp0            10     10n

      // Location Deletion Security Level Error Indicator
     d                 ds
     d dsSecLvlErDL            1     10
     d                                     dim(intArrDim)
     d  erAOvDLLv1             1      1n
     d  erAOvDLLv2             2      2n
     d  erAOvDLLv3             3      3n
     d  erAOvDLLv4             4      4n
     d  erAOvDLLv5             5      5n
     d  erAOvDLLv6             6      6n
     d  erAOvDLLv7             7      7n
     d  erAOvDLLv8             8      8n
     d  erAOvDLLv9             9      9n
     d  erAOvDLLv0            10     10n

      // Allow Audit Requests in Inquiry Mode Indicator
      // Allow Audit Requests in Inquiry Security Type
     d                 ds
     d dsSecTypAI              1     10
     d                                     dim(intArrDim)
     d  scAaRITp1              1      1
     d  scAaRITp2              2      2
     d  scAaRITp3              3      3
     d  scAaRITp4              4      4
     d  scAaRITp5              5      5
     d  scAaRITp6              6      6
     d  scAaRITp7              7      7
     d  scAaRITp8              8      8
     d  scAaRITp9              9      9
     d  scAaRITp0             10     10

      // Allow Audit Requests in Inquiry Security Level
     d                 ds
     d dsSecLvlAI              1     30  0
     d                                     dim(intArrDim)
     d  scAaRILv1              1      3  0
     d  scAaRILv2              4      6  0
     d  scAaRILv3              7      9  0
     d  scAaRILv4             10     12  0
     d  scAaRILv5             13     15  0
     d  scAaRILv6             16     18  0
     d  scAaRILv7             19     21  0
     d  scAaRILv8             22     24  0
     d  scAaRILv9             25     27  0
     d  scAaRILv0             28     30  0

      // Allow Audit Requests in Inquiry Security Type Error Indicator
     d                 ds
     d dsSecTypErAI            1     10
     d                                     dim(intArrDim)
     d  erAaRITp1              1      1n
     d  erAaRITp2              2      2n
     d  erAaRITp3              3      3n
     d  erAaRITp4              4      4n
     d  erAaRITp5              5      5n
     d  erAaRITp6              6      6n
     d  erAaRITp7              7      7n
     d  erAaRITp8              8      8n
     d  erAaRITp9              9      9n
     d  erAaRITp0             10     10n

      // Allow Audit Requests in Inquiery Security Level Error Indicator
     d                 ds
     d dsSecLvlErAI            1     10
     d                                     dim(intArrDim)
     d  erAaRILv1              1      1n
     d  erAaRILv2              2      2n
     d  erAaRILv3              3      3n
     d  erAaRILv4              4      4n
     d  erAaRILv5              5      5n
     d  erAaRILv6              6      6n
     d  erAaRILv7              7      7n
     d  erAaRILv8              8      8n
     d  erAaRILv9              9      9n
     d  erAaRILv0             10     10n

      //*** Beg Add ***** 08/01/24 *********************************************
      // Allow Audit Reversal - Security Type
     d                 ds
     d dsSecTypAR              1     10
     d                                     dim(intArrDim)
     d  scAaRvTp1              1      1
     d  scAaRvTp2              2      2
     d  scAaRvTp3              3      3
     d  scAaRvTp4              4      4
     d  scAaRvTp5              5      5
     d  scAaRvTp6              6      6
     d  scAaRvTp7              7      7
     d  scAaRvTp8              8      8
     d  scAaRvTp9              9      9
     d  scAaRvTp0             10     10

      //  Allow Audit Reversal - Security Level
     d                 ds
     d dsSecLvlAR              1     30  0
     d                                     dim(intArrDim)
     d  scAaRvLv1              1      3  0
     d  scAaRvLv2              4      6  0
     d  scAaRvLv3              7      9  0
     d  scAaRvLv4             10     12  0
     d  scAaRvLv5             13     15  0
     d  scAaRvLv6             16     18  0
     d  scAaRvLv7             19     21  0
     d  scAaRvLv8             22     24  0
     d  scAaRvLv9             25     27  0
     d  scAaRvLv0             28     30  0

      //  Allow Audit Reversal - Security Type Error Indicator
     d                 ds
     d dsSecTypErAR            1     10
     d                                     dim(intArrDim)
     d  erAaRvTp1              1      1n
     d  erAaRvTp2              2      2n
     d  erAaRvTp3              3      3n
     d  erAaRvTp4              4      4n
     d  erAaRvTp5              5      5n
     d  erAaRvTp6              6      6n
     d  erAaRvTp7              7      7n
     d  erAaRvTp8              8      8n
     d  erAaRvTp9              9      9n
     d  erAaRvTp0             10     10n

      //  Allow Audit Reversal - Security Level Error Indicator
     d                 ds
     d dsSecLvlErAR            1     10
     d                                     dim(intArrDim)
     d  erAaRvLv1              1      1n
     d  erAaRvLv2              2      2n
     d  erAaRvLv3              3      3n
     d  erAaRvLv4              4      4n
     d  erAaRvLv5              5      5n
     d  erAaRvLv6              6      6n
     d  erAaRvLv7              7      7n
     d  erAaRvLv8              8      8n
     d  erAaRvLv9              9      9n
     d  erAaRvLv0             10     10n
      //*** End Add ***** 08/01/24 *********************************************

      //** Beg Add *** 04/24/23 **********************************************
      // Security Control for A/R Batches

      // Allow Flag for Approval Security Type
     d                 ds
     d dsSecTypFA              1     10
     d                                     dim(intArrDim)
     d  scAFlgATp1             1      1
     d  scAFlgATp2             2      2
     d  scAFlgATp3             3      3
     d  scAFlgATp4             4      4
     d  scAFlgATp5             5      5
     d  scAFlgATp6             6      6
     d  scAFlgATp7             7      7
     d  scAFlgATp8             8      8
     d  scAFlgATp9             9      9
     d  scAFlgATp0            10     10

      // Allow Flag for Approval Security Level
     d                 ds
     d dsSecLvlFA              1     30  0
     d                                     dim(intArrDim)
     d  scAFlgALv1             1      3  0
     d  scAFlgALv2             4      6  0
     d  scAFlgALv3             7      9  0
     d  scAFlgALv4            10     12  0
     d  scAFlgALv5            13     15  0
     d  scAFlgALv6            16     18  0
     d  scAFlgALv7            19     21  0
     d  scAFlgALv8            22     24  0
     d  scAFlgALv9            25     27  0
     d  scAFlgALv0            28     30  0

      // Allow Flag for Approval Security Type Error Indicator
     d                 ds
     d dsSecTypErFA            1     10
     d                                     dim(intArrDim)
     d  erAFlgATp1             1      1n
     d  erAFlgATp2             2      2n
     d  erAFlgATp3             3      3n
     d  erAFlgATp4             4      4n
     d  erAFlgATp5             5      5n
     d  erAFlgATp6             6      6n
     d  erAFlgATp7             7      7n
     d  erAFlgATp8             8      8n
     d  erAFlgATp9             9      9n
     d  erAFlgATp0            10     10n

      // Allow Flag for Approval Security Level Error Indicator
     d                 ds
     d dsSecLvlErFA            1     10
     d                                     dim(intArrDim)
     d  erAFlgALv1             1      1n
     d  erAFlgALv2             2      2n
     d  erAFlgALv3             3      3n
     d  erAFlgALv4             4      4n
     d  erAFlgALv5             5      5n
     d  erAFlgALv6             6      6n
     d  erAFlgALv7             7      7n
     d  erAFlgALv8             8      8n
     d  erAFlgALv9             9      9n
     d  erAFlgALv0            10     10n


      // Allow Flag for Transfer Security Type
     d                 ds
     d dsSecTypFT              1     10
     d                                     dim(intArrDim)
     d  scAFlgTTp1             1      1
     d  scAFlgTTp2             2      2
     d  scAFlgTTp3             3      3
     d  scAFlgTTp4             4      4
     d  scAFlgTTp5             5      5
     d  scAFlgTTp6             6      6
     d  scAFlgTTp7             7      7
     d  scAFlgTTp8             8      8
     d  scAFlgTTp9             9      9
     d  scAFlgTTp0            10     10

      // Allow Flag for Transfer Security Level
     d                 ds
     d dsSecLvlFT              1     30  0
     d                                     dim(intArrDim)
     d  scAFlgTLv1             1      3  0
     d  scAFlgTLv2             4      6  0
     d  scAFlgTLv3             7      9  0
     d  scAFlgTLv4            10     12  0
     d  scAFlgTLv5            13     15  0
     d  scAFlgTLv6            16     18  0
     d  scAFlgTLv7            19     21  0
     d  scAFlgTLv8            22     24  0
     d  scAFlgTLv9            25     27  0
     d  scAFlgTLv0            28     30  0

      // Allow Flag for Transfer Security Type Error Indicator
     d                 ds
     d dsSecTypErFT            1     10
     d                                     dim(intArrDim)
     d  erAFlgTTp1             1      1n
     d  erAFlgTTp2             2      2n
     d  erAFlgTTp3             3      3n
     d  erAFlgTTp4             4      4n
     d  erAFlgTTp5             5      5n
     d  erAFlgTTp6             6      6n
     d  erAFlgTTp7             7      7n
     d  erAFlgTTp8             8      8n
     d  erAFlgTTp9             9      9n
     d  erAFlgTTp0            10     10n

      // Allow Flag for Transfer Security Level Error Indicator
     d                 ds
     d dsSecLvlErFT            1     10
     d                                     dim(intArrDim)
     d  erAFlgTLv1             1      1n
     d  erAFlgTLv2             2      2n
     d  erAFlgTLv3             3      3n
     d  erAFlgTLv4             4      4n
     d  erAFlgTLv5             5      5n
     d  erAFlgTLv6             6      6n
     d  erAFlgTLv7             7      7n
     d  erAFlgTLv8             8      8n
     d  erAFlgTLv9             9      9n
     d  erAFlgTLv0            10     10n


      // Allow Edit Report Security Type
     d                 ds
     d dsSecTypRP              1     10
     d                                     dim(intArrDim)
     d  scAEdtRTp1             1      1
     d  scAEdtRTp2             2      2
     d  scAEdtRTp3             3      3
     d  scAEdtRTp4             4      4
     d  scAEdtRTp5             5      5
     d  scAEdtRTp6             6      6
     d  scAEdtRTp7             7      7
     d  scAEdtRTp8             8      8
     d  scAEdtRTp9             9      9
     d  scAEdtRTp0            10     10

      // Allow Edit Report Security Level
     d                 ds
     d dsSecLvlRP              1     30  0
     d                                     dim(intArrDim)
     d  scAEdtRLv1             1      3  0
     d  scAEdtRLv2             4      6  0
     d  scAEdtRLv3             7      9  0
     d  scAEdtRLv4            10     12  0
     d  scAEdtRLv5            13     15  0
     d  scAEdtRLv6            16     18  0
     d  scAEdtRLv7            19     21  0
     d  scAEdtRLv8            22     24  0
     d  scAEdtRLv9            25     27  0
     d  scAEdtRLv0            28     30  0

      // Allow Edit Report Security Type Error Indicator
     d                 ds
     d dsSecTypErRP            1     10
     d                                     dim(intArrDim)
     d  erAEdtRTp1             1      1n
     d  erAEdtRTp2             2      2n
     d  erAEdtRTp3             3      3n
     d  erAEdtRTp4             4      4n
     d  erAEdtRTp5             5      5n
     d  erAEdtRTp6             6      6n
     d  erAEdtRTp7             7      7n
     d  erAEdtRTp8             8      8n
     d  erAEdtRTp9             9      9n
     d  erAEdtRTp0            10     10n

      // Allow Edit Report Security Level Error Indicator
     d                 ds
     d dsSecLvlErRP            1     10
     d                                     dim(intArrDim)
     d  erAEdtRLv1             1      1n
     d  erAEdtRLv2             2      2n
     d  erAEdtRLv3             3      3n
     d  erAEdtRLv4             4      4n
     d  erAEdtRLv5             5      5n
     d  erAEdtRLv6             6      6n
     d  erAEdtRLv7             7      7n
     d  erAEdtRLv8             8      8n
     d  erAEdtRLv9             9      9n
     d  erAEdtRLv0            10     10n


      // Allow Create EFT File Security Type
     d                 ds
     d dsSecTypCF              1     10
     d                                     dim(intArrDim)
     d  scACrtFTp1             1      1
     d  scACrtFTp2             2      2
     d  scACrtFTp3             3      3
     d  scACrtFTp4             4      4
     d  scACrtFTp5             5      5
     d  scACrtFTp6             6      6
     d  scACrtFTp7             7      7
     d  scACrtFTp8             8      8
     d  scACrtFTp9             9      9
     d  scACrtFTp0            10     10

      // Allow Create EFT File Security Level
     d                 ds
     d dsSecLvlCF              1     30  0
     d                                     dim(intArrDim)
     d  scACrtFLv1             1      3  0
     d  scACrtFLv2             4      6  0
     d  scACrtFLv3             7      9  0
     d  scACrtFLv4            10     12  0
     d  scACrtFLv5            13     15  0
     d  scACrtFLv6            16     18  0
     d  scACrtFLv7            19     21  0
     d  scACrtFLv8            22     24  0
     d  scACrtFLv9            25     27  0
     d  scACrtFLv0            28     30  0

      // Allow Create EFT File Security Type Error Indicator
     d                 ds
     d dsSecTypErCF            1     10
     d                                     dim(intArrDim)
     d  erACrtFTp1             1      1n
     d  erACrtFTp2             2      2n
     d  erACrtFTp3             3      3n
     d  erACrtFTp4             4      4n
     d  erACrtFTp5             5      5n
     d  erACrtFTp6             6      6n
     d  erACrtFTp7             7      7n
     d  erACrtFTp8             8      8n
     d  erACrtFTp9             9      9n
     d  erACrtFTp0            10     10n

      // Allow Create EFT File Security Level Error Indicator
     d                 ds
     d dsSecLvlErCF            1     10
     d                                     dim(intArrDim)
     d  erACrtFLv1             1      1n
     d  erACrtFLv2             2      2n
     d  erACrtFLv3             3      3n
     d  erACrtFLv4             4      4n
     d  erACrtFLv5             5      5n
     d  erACrtFLv6             6      6n
     d  erACrtFLv7             7      7n
     d  erACrtFLv8             8      8n
     d  erACrtFLv9             9      9n
     d  erACrtFLv0            10     10n


      // Allow Mass Update Security Type
     d                 ds
     d dsSecTypMU              1     10
     d                                     dim(intArrDim)
     d  scAMasUTp1             1      1
     d  scAMasUTp2             2      2
     d  scAMasUTp3             3      3
     d  scAMasUTp4             4      4
     d  scAMasUTp5             5      5
     d  scAMasUTp6             6      6
     d  scAMasUTp7             7      7
     d  scAMasUTp8             8      8
     d  scAMasUTp9             9      9
     d  scAMasUTp0            10     10

      // Allow Mass Update Security Level
     d                 ds
     d dsSecLvlMU              1     30  0
     d                                     dim(intArrDim)
     d  scAMasULv1             1      3  0
     d  scAMasULv2             4      6  0
     d  scAMasULv3             7      9  0
     d  scAMasULv4            10     12  0
     d  scAMasULv5            13     15  0
     d  scAMasULv6            16     18  0
     d  scAMasULv7            19     21  0
     d  scAMasULv8            22     24  0
     d  scAMasULv9            25     27  0
     d  scAMasULv0            28     30  0

      // Allow Mass Update Security Type Error Indicator
     d                 ds
     d dsSecTypErMU            1     10
     d                                     dim(intArrDim)
     d  erAMasUTp1             1      1n
     d  erAMasUTp2             2      2n
     d  erAMasUTp3             3      3n
     d  erAMasUTp4             4      4n
     d  erAMasUTp5             5      5n
     d  erAMasUTp6             6      6n
     d  erAMasUTp7             7      7n
     d  erAMasUTp8             8      8n
     d  erAMasUTp9             9      9n
     d  erAMasUTp0            10     10n

      // Allow Mass Update Security Level Error Indicator
     d                 ds
     d dsSecLvlErMU            1     10
     d                                     dim(intArrDim)
     d  erAMasULv1             1      1n
     d  erAMasULv2             2      2n
     d  erAMasULv3             3      3n
     d  erAMasULv4             4      4n
     d  erAMasULv5             5      5n
     d  erAMasULv6             6      6n
     d  erAMasULv7             7      7n
     d  erAMasULv8             8      8n
     d  erAMasULv9             9      9n
     d  erAMasULv0            10     10n


      // Allow Edit Open Batches Security Type
     d                 ds
     d dsSecTypEB              1     10
     d                                     dim(intArrDim)
     d  scAEdtBTp1             1      1
     d  scAEdtBTp2             2      2
     d  scAEdtBTp3             3      3
     d  scAEdtBTp4             4      4
     d  scAEdtBTp5             5      5
     d  scAEdtBTp6             6      6
     d  scAEdtBTp7             7      7
     d  scAEdtBTp8             8      8
     d  scAEdtBTp9             9      9
     d  scAEdtBTp0            10     10

      // Allow Edit Open Batches Security Level
     d                 ds
     d dsSecLvlEB              1     30  0
     d                                     dim(intArrDim)
     d  scAEdtBLv1             1      3  0
     d  scAEdtBLv2             4      6  0
     d  scAEdtBLv3             7      9  0
     d  scAEdtBLv4            10     12  0
     d  scAEdtBLv5            13     15  0
     d  scAEdtBLv6            16     18  0
     d  scAEdtBLv7            19     21  0
     d  scAEdtBLv8            22     24  0
     d  scAEdtBLv9            25     27  0
     d  scAEdtBLv0            28     30  0

      // Allow Edit Open Batches Security Type Error Indicator
     d                 ds
     d dsSecTypErEB            1     10
     d                                     dim(intArrDim)
     d  erAEdtBTp1             1      1n
     d  erAEdtBTp2             2      2n
     d  erAEdtBTp3             3      3n
     d  erAEdtBTp4             4      4n
     d  erAEdtBTp5             5      5n
     d  erAEdtBTp6             6      6n
     d  erAEdtBTp7             7      7n
     d  erAEdtBTp8             8      8n
     d  erAEdtBTp9             9      9n
     d  erAEdtBTp0            10     10n

      // Allow Edit Open Batches Security Level Error Indicator
     d                 ds
     d dsSecLvlErEB            1     10
     d                                     dim(intArrDim)
     d  erAEdtBLv1             1      1n
     d  erAEdtBLv2             2      2n
     d  erAEdtBLv3             3      3n
     d  erAEdtBLv4             4      4n
     d  erAEdtBLv5             5      5n
     d  erAEdtBLv6             6      6n
     d  erAEdtBLv7             7      7n
     d  erAEdtBLv8             8      8n
     d  erAEdtBLv9             9      9n
     d  erAEdtBLv0            10     10n


      // Allow Transfer Batches Security Type
     d                 ds
     d dsSecTypTB              1     10
     d                                     dim(intArrDim)
     d  scATrfBTp1             1      1
     d  scATrfBTp2             2      2
     d  scATrfBTp3             3      3
     d  scATrfBTp4             4      4
     d  scATrfBTp5             5      5
     d  scATrfBTp6             6      6
     d  scATrfBTp7             7      7
     d  scATrfBTp8             8      8
     d  scATrfBTp9             9      9
     d  scATrfBTp0            10     10

      // Allow Transfer Batches Security Level
     d                 ds
     d dsSecLvlTB              1     30  0
     d                                     dim(intArrDim)
     d  scATrfBLv1             1      3  0
     d  scATrfBLv2             4      6  0
     d  scATrfBLv3             7      9  0
     d  scATrfBLv4            10     12  0
     d  scATrfBLv5            13     15  0
     d  scATrfBLv6            16     18  0
     d  scATrfBLv7            19     21  0
     d  scATrfBLv8            22     24  0
     d  scATrfBLv9            25     27  0
     d  scATrfBLv0            28     30  0

      // Allow Transfer Batches Security Type Error Indicator
     d                 ds
     d dsSecTypErTB            1     10
     d                                     dim(intArrDim)
     d  erATrfBTp1             1      1n
     d  erATrfBTp2             2      2n
     d  erATrfBTp3             3      3n
     d  erATrfBTp4             4      4n
     d  erATrfBTp5             5      5n
     d  erATrfBTp6             6      6n
     d  erATrfBTp7             7      7n
     d  erATrfBTp8             8      8n
     d  erATrfBTp9             9      9n
     d  erATrfBTp0            10     10n

      // Allow Transfer Batches Security Level Error Indicator
     d                 ds
     d dsSecLvlErTB            1     10
     d                                     dim(intArrDim)
     d  erATrfBLv1             1      1n
     d  erATrfBLv2             2      2n
     d  erATrfBLv3             3      3n
     d  erATrfBLv4             4      4n
     d  erATrfBLv5             5      5n
     d  erATrfBLv6             6      6n
     d  erATrfBLv7             7      7n
     d  erATrfBLv8             8      8n
     d  erATrfBLv9             9      9n
     d  erATrfBLv0            10     10n


      // Allow Purge Security Type
     d                 ds
     d dsSecTypPU              1     10
     d                                     dim(intArrDim)
     d  scAPurgTp1             1      1
     d  scAPurgTp2             2      2
     d  scAPurgTp3             3      3
     d  scAPurgTp4             4      4
     d  scAPurgTp5             5      5
     d  scAPurgTp6             6      6
     d  scAPurgTp7             7      7
     d  scAPurgTp8             8      8
     d  scAPurgTp9             9      9
     d  scAPurgTp0            10     10

      // Allow Purge Security Level
     d                 ds
     d dsSecLvlPU              1     30  0
     d                                     dim(intArrDim)
     d  scAPurgLv1             1      3  0
     d  scAPurgLv2             4      6  0
     d  scAPurgLv3             7      9  0
     d  scAPurgLv4            10     12  0
     d  scAPurgLv5            13     15  0
     d  scAPurgLv6            16     18  0
     d  scAPurgLv7            19     21  0
     d  scAPurgLv8            22     24  0
     d  scAPurgLv9            25     27  0
     d  scAPurgLv0            28     30  0

      // Allow Purge Security Type Error Indicator
     d                 ds
     d dsSecTypErPU            1     10
     d                                     dim(intArrDim)
     d  erAPurgTp1             1      1n
     d  erAPurgTp2             2      2n
     d  erAPurgTp3             3      3n
     d  erAPurgTp4             4      4n
     d  erAPurgTp5             5      5n
     d  erAPurgTp6             6      6n
     d  erAPurgTp7             7      7n
     d  erAPurgTp8             8      8n
     d  erAPurgTp9             9      9n
     d  erAPurgTp0            10     10n

      // Allow Purge Security Level Error Indicator
     d                 ds
     d dsSecLvlErPU            1     10
     d                                     dim(intArrDim)
     d  erAPurgLv1             1      1n
     d  erAPurgLv2             2      2n
     d  erAPurgLv3             3      3n
     d  erAPurgLv4             4      4n
     d  erAPurgLv5             5      5n
     d  erAPurgLv6             6      6n
     d  erAPurgLv7             7      7n
     d  erAPurgLv8             8      8n
     d  erAPurgLv9             9      9n
     d  erAPurgLv0            10     10n


      // Allow List by Employer Security Type
     d                 ds
     d dsSecTypLE              1     10
     d                                     dim(intArrDim)
     d  scAListTp1             1      1
     d  scAListTp2             2      2
     d  scAListTp3             3      3
     d  scAListTp4             4      4
     d  scAListTp5             5      5
     d  scAListTp6             6      6
     d  scAListTp7             7      7
     d  scAListTp8             8      8
     d  scAListTp9             9      9
     d  scAListTp0            10     10

      // Allow List by Employer Security Level
     d                 ds
     d dsSecLvlLE              1     30  0
     d                                     dim(intArrDim)
     d  scAListLv1             1      3  0
     d  scAListLv2             4      6  0
     d  scAListLv3             7      9  0
     d  scAListLv4            10     12  0
     d  scAListLv5            13     15  0
     d  scAListLv6            16     18  0
     d  scAListLv7            19     21  0
     d  scAListLv8            22     24  0
     d  scAListLv9            25     27  0
     d  scAListLv0            28     30  0

      // Allow List by Employer Security Type Error Indicator
     d                 ds
     d dsSecTypErLE            1     10
     d                                     dim(intArrDim)
     d  erAListTp1             1      1n
     d  erAListTp2             2      2n
     d  erAListTp3             3      3n
     d  erAListTp4             4      4n
     d  erAListTp5             5      5n
     d  erAListTp6             6      6n
     d  erAListTp7             7      7n
     d  erAListTp8             8      8n
     d  erAListTp9             9      9n
     d  erAListTp0            10     10n

      // Allow List by Employer Security Level Error Indicator
     d                 ds
     d dsSecLvlErLE            1     10
     d                                     dim(intArrDim)
     d  erAListLv1             1      1n
     d  erAListLv2             2      2n
     d  erAListLv3             3      3n
     d  erAListLv4             4      4n
     d  erAListLv5             5      5n
     d  erAListLv6             6      6n
     d  erAListLv7             7      7n
     d  erAListLv8             8      8n
     d  erAListLv9             9      9n
     d  erAListLv0            10     10n

      //** End Add *** 04/24/23 **********************************************

      //** Beg Add *** 11/01/23 **********************************************
      // Security Control for Return Funds Batches

      // Allow Batch Edit Report Security Type
     d                 ds
     d dsSecTypBE              1     10
     d                                     dim(intArrDim)
     d  scREdtRTp1             1      1
     d  scREdtRTp2             2      2
     d  scREdtRTp3             3      3
     d  scREdtRTp4             4      4
     d  scREdtRTp5             5      5
     d  scREdtRTp6             6      6
     d  scREdtRTp7             7      7
     d  scREdtRTp8             8      8
     d  scREdtRTp9             9      9
     d  scREdtRTp0            10     10

      // Allow Batch Edit Report Security Level
     d                 ds
     d dsSecLvlBE              1     30  0
     d                                     dim(intArrDim)
     d  scREdtRLv1             1      3  0
     d  scREdtRLv2             4      6  0
     d  scREdtRLv3             7      9  0
     d  scREdtRLv4            10     12  0
     d  scREdtRLv5            13     15  0
     d  scREdtRLv6            16     18  0
     d  scREdtRLv7            19     21  0
     d  scREdtRLv8            22     24  0
     d  scREdtRLv9            25     27  0
     d  scREdtRLv0            28     30  0

      // Allow Batch Edit Report Security Type Error Indicator
     d                 ds
     d dsSecTypErBE            1     10
     d                                     dim(intArrDim)
     d  erREdtRTp1             1      1n
     d  erREdtRTp2             2      2n
     d  erREdtRTp3             3      3n
     d  erREdtRTp4             4      4n
     d  erREdtRTp5             5      5n
     d  erREdtRTp6             6      6n
     d  erREdtRTp7             7      7n
     d  erREdtRTp8             8      8n
     d  erREdtRTp9             9      9n
     d  erREdtRTp0            10     10n

      // Allow Batch Edit Report Security Level Error Indicator
     d                 ds
     d dsSecLvlErBE            1     10
     d                                     dim(intArrDim)
     d  erREdtRLv1             1      1n
     d  erREdtRLv2             2      2n
     d  erREdtRLv3             3      3n
     d  erREdtRLv4             4      4n
     d  erREdtRLv5             5      5n
     d  erREdtRLv6             6      6n
     d  erREdtRLv7             7      7n
     d  erREdtRLv8             8      8n
     d  erREdtRLv9             9      9n
     d  erREdtRLv0            10     10n


      // Allow Generate Check Request Security Type
     d                 ds
     d dsSecTypGC              1     10
     d                                     dim(intArrDim)
     d  scGenCRTp1             1      1
     d  scGenCRTp2             2      2
     d  scGenCRTp3             3      3
     d  scGenCRTp4             4      4
     d  scGenCRTp5             5      5
     d  scGenCRTp6             6      6
     d  scGenCRTp7             7      7
     d  scGenCRTp8             8      8
     d  scGenCRTp9             9      9
     d  scGenCRTp0            10     10

      // Allow Generate Check Request Security Level
     d                 ds
     d dsSecLvlGC              1     30  0
     d                                     dim(intArrDim)
     d  scGenCRLv1             1      3  0
     d  scGenCRLv2             4      6  0
     d  scGenCRLv3             7      9  0
     d  scGenCRLv4            10     12  0
     d  scGenCRLv5            13     15  0
     d  scGenCRLv6            16     18  0
     d  scGenCRLv7            19     21  0
     d  scGenCRLv8            22     24  0
     d  scGenCRLv9            25     27  0
     d  scGenCRLv0            28     30  0

      // Allow Generate Check Request Security Type Error Indicator
     d                 ds
     d dsSecTypErGC            1     10
     d                                     dim(intArrDim)
     d  erGenCRTp1             1      1n
     d  erGenCRTp2             2      2n
     d  erGenCRTp3             3      3n
     d  erGenCRTp4             4      4n
     d  erGenCRTp5             5      5n
     d  erGenCRTp6             6      6n
     d  erGenCRTp7             7      7n
     d  erGenCRTp8             8      8n
     d  erGenCRTp9             9      9n
     d  erGenCRTp0            10     10n

      // Allow Generate Check Request Security Level Error Indicator
     d                 ds
     d dsSecLvlErGC            1     10
     d                                     dim(intArrDim)
     d  erGenCRLv1             1      1n
     d  erGenCRLv2             2      2n
     d  erGenCRLv3             3      3n
     d  erGenCRLv4             4      4n
     d  erGenCRLv5             5      5n
     d  erGenCRLv6             6      6n
     d  erGenCRLv7             7      7n
     d  erGenCRLv8             8      8n
     d  erGenCRLv9             9      9n
     d  erGenCRLv0            10     10n
      //** End Add *** 11/01/23 **********************************************

      //** Beg Add *** 03/21/24 **********************************************
      // Security Control for Policy Endorsement Forms

      // Allow Copy Endorsement Forms Type
     d                 ds
     d dsSecTypCE              1     10
     d                                     dim(intArrDim)
     d  scCpyEFTp1             1      1
     d  scCpyEFTp2             2      2
     d  scCpyEFTp3             3      3
     d  scCpyEFTp4             4      4
     d  scCpyEFTp5             5      5
     d  scCpyEFTp6             6      6
     d  scCpyEFTp7             7      7
     d  scCpyEFTp8             8      8
     d  scCpyEFTp9             9      9
     d  scCpyEFTp0            10     10

      // Allow Copy Endorsement Forms Level
     d                 ds
     d dsSecLvlCE              1     30  0
     d                                     dim(intArrDim)
     d  scCpyEFLv1             1      3  0
     d  scCpyEFLv2             4      6  0
     d  scCpyEFLv3             7      9  0
     d  scCpyEFLv4            10     12  0
     d  scCpyEFLv5            13     15  0
     d  scCpyEFLv6            16     18  0
     d  scCpyEFLv7            19     21  0
     d  scCpyEFLv8            22     24  0
     d  scCpyEFLv9            25     27  0
     d  scCpyEFLv0            28     30  0

      // Allow Copy Endorsement Forms Type Error Indicator
     d                 ds
     d dsSecTypErCE            1     10
     d                                     dim(intArrDim)
     d  erCpyEFTp1             1      1n
     d  erCpyEFTp2             2      2n
     d  erCpyEFTp3             3      3n
     d  erCpyEFTp4             4      4n
     d  erCpyEFTp5             5      5n
     d  erCpyEFTp6             6      6n
     d  erCpyEFTp7             7      7n
     d  erCpyEFTp8             8      8n
     d  erCpyEFTp9             9      9n
     d  erCpyEFTp0            10     10n

      // Allow Copy Endorsement Forms Level Error Indicator
     d                 ds
     d dsSecLvlErCE            1     10
     d                                     dim(intArrDim)
     d  erCpyEFLv1             1      1n
     d  erCpyEFLv2             2      2n
     d  erCpyEFLv3             3      3n
     d  erCpyEFLv4             4      4n
     d  erCpyEFLv5             5      5n
     d  erCpyEFLv6             6      6n
     d  erCpyEFLv7             7      7n
     d  erCpyEFLv8             8      8n
     d  erCpyEFLv9             9      9n
     d  erCpyEFLv0            10     10n


      // Allow Delete Endorsement Forms Type
     d                 ds
     d dsSecTypDE              1     10
     d                                     dim(intArrDim)
     d  scDelEFTp1             1      1
     d  scDelEFTp2             2      2
     d  scDelEFTp3             3      3
     d  scDelEFTp4             4      4
     d  scDelEFTp5             5      5
     d  scDelEFTp6             6      6
     d  scDelEFTp7             7      7
     d  scDelEFTp8             8      8
     d  scDelEFTp9             9      9
     d  scDelEFTp0            10     10

      // Allow Delete Endorsement Forms Level
     d                 ds
     d dsSecLvlDE              1     30  0
     d                                     dim(intArrDim)
     d  scDelEFLv1             1      3  0
     d  scDelEFLv2             4      6  0
     d  scDelEFLv3             7      9  0
     d  scDelEFLv4            10     12  0
     d  scDelEFLv5            13     15  0
     d  scDelEFLv6            16     18  0
     d  scDelEFLv7            19     21  0
     d  scDelEFLv8            22     24  0
     d  scDelEFLv9            25     27  0
     d  scDelEFLv0            28     30  0

      // Allow Delete Endorsement Forms Type Error Indicator
     d                 ds
     d dsSecTypErDE            1     10
     d                                     dim(intArrDim)
     d  erDelEFTp1             1      1n
     d  erDelEFTp2             2      2n
     d  erDelEFTp3             3      3n
     d  erDelEFTp4             4      4n
     d  erDelEFTp5             5      5n
     d  erDelEFTp6             6      6n
     d  erDelEFTp7             7      7n
     d  erDelEFTp8             8      8n
     d  erDelEFTp9             9      9n
     d  erDelEFTp0            10     10n

      // Allow Delete Endorsement Forms Level Error Indicator
     d                 ds
     d dsSecLvlErDE            1     10
     d                                     dim(intArrDim)
     d  erDelEFLv1             1      1n
     d  erDelEFLv2             2      2n
     d  erDelEFLv3             3      3n
     d  erDelEFLv4             4      4n
     d  erDelEFLv5             5      5n
     d  erDelEFLv6             6      6n
     d  erDelEFLv7             7      7n
     d  erDelEFLv8             8      8n
     d  erDelEFLv9             9      9n
     d  erDelEFLv0            10     10n

      // Allow Print Endorsement Forms Type
     d                 ds
     d dsSecTypPE              1     10
     d                                     dim(intArrDim)
     d  scPrtEFTp1             1      1
     d  scPrtEFTp2             2      2
     d  scPrtEFTp3             3      3
     d  scPrtEFTp4             4      4
     d  scPrtEFTp5             5      5
     d  scPrtEFTp6             6      6
     d  scPrtEFTp7             7      7
     d  scPrtEFTp8             8      8
     d  scPrtEFTp9             9      9
     d  scPrtEFTp0            10     10

      // Allow Print Endorsement Forms Level
     d                 ds
     d dsSecLvlPE              1     30  0
     d                                     dim(intArrDim)
     d  scPrtEFLv1             1      3  0
     d  scPrtEFLv2             4      6  0
     d  scPrtEFLv3             7      9  0
     d  scPrtEFLv4            10     12  0
     d  scPrtEFLv5            13     15  0
     d  scPrtEFLv6            16     18  0
     d  scPrtEFLv7            19     21  0
     d  scPrtEFLv8            22     24  0
     d  scPrtEFLv9            25     27  0
     d  scPrtEFLv0            28     30  0

      // Allow Print Endorsement Forms Type Error Indicator
     d                 ds
     d dsSecTypErPE            1     10
     d                                     dim(intArrDim)
     d  erPrtEFTp1             1      1n
     d  erPrtEFTp2             2      2n
     d  erPrtEFTp3             3      3n
     d  erPrtEFTp4             4      4n
     d  erPrtEFTp5             5      5n
     d  erPrtEFTp6             6      6n
     d  erPrtEFTp7             7      7n
     d  erPrtEFTp8             8      8n
     d  erPrtEFTp9             9      9n
     d  erPrtEFTp0            10     10n

      // Allow Print Endorsement Forms Level Error Indicator
     d                 ds
     d dsSecLvlErPE            1     10
     d                                     dim(intArrDim)
     d  erPrtEFLv1             1      1n
     d  erPrtEFLv2             2      2n
     d  erPrtEFLv3             3      3n
     d  erPrtEFLv4             4      4n
     d  erPrtEFLv5             5      5n
     d  erPrtEFLv6             6      6n
     d  erPrtEFLv7             7      7n
     d  erPrtEFLv8             8      8n
     d  erPrtEFLv9             9      9n
     d  erPrtEFLv0            10     10n


      // Allow Change Include/Exclude Endorsement Forms Type
     d                 ds
     d dsSecTypIE              1     10
     d                                     dim(intArrDim)
     d  scCieEFTp1             1      1
     d  scCieEFTp2             2      2
     d  scCieEFTp3             3      3
     d  scCieEFTp4             4      4
     d  scCieEFTp5             5      5
     d  scCieEFTp6             6      6
     d  scCieEFTp7             7      7
     d  scCieEFTp8             8      8
     d  scCieEFTp9             9      9
     d  scCieEFTp0            10     10

      // Allow Change Include/Exclude Endorsement Forms Level
     d                 ds
     d dsSecLvlIE              1     30  0
     d                                     dim(intArrDim)
     d  scCieEFLv1             1      3  0
     d  scCieEFLv2             4      6  0
     d  scCieEFLv3             7      9  0
     d  scCieEFLv4            10     12  0
     d  scCieEFLv5            13     15  0
     d  scCieEFLv6            16     18  0
     d  scCieEFLv7            19     21  0
     d  scCieEFLv8            22     24  0
     d  scCieEFLv9            25     27  0
     d  scCieEFLv0            28     30  0

      // Allow Change Include/Exclude Endorsement Forms Type Error Indicator
     d                 ds
     d dsSecTypErIE            1     10
     d                                     dim(intArrDim)
     d  erCieEFTp1             1      1n
     d  erCieEFTp2             2      2n
     d  erCieEFTp3             3      3n
     d  erCieEFTp4             4      4n
     d  erCieEFTp5             5      5n
     d  erCieEFTp6             6      6n
     d  erCieEFTp7             7      7n
     d  erCieEFTp8             8      8n
     d  erCieEFTp9             9      9n
     d  erCieEFTp0            10     10n

      // Allow Change Include/Exclude Endorsement Forms Level Error Indicator
     d                 ds
     d dsSecLvlErIE            1     10
     d                                     dim(intArrDim)
     d  erCieEFLv1             1      1n
     d  erCieEFLv2             2      2n
     d  erCieEFLv3             3      3n
     d  erCieEFLv4             4      4n
     d  erCieEFLv5             5      5n
     d  erCieEFLv6             6      6n
     d  erCieEFLv7             7      7n
     d  erCieEFLv8             8      8n
     d  erCieEFLv9             9      9n
     d  erCieEFLv0            10     10n
      //** End Add *** 03/21/24 **********************************************

      // List of Keys and RRN

     D sdacpKeys_t     ds                  Qualified
     D  key                                Like(ACapID)
     D  rrn                           9  0

     d chkKeys         ds                  Qualified
     D  sdacpKeys                          LikeDs(sdacpKeys_t) dim(intKeyDim)

     D timeStamp_t     ds                  Qualified
     D  date                               Like(ACdt)
     D  time                               Like(ACtime)

     D chkTime         ds                  Qualified Inz
     D  timeStamp                          LikeDs(timeStamp_t) dim(intKeyDim)

     d*** Beg Add *** 05/09/22 *************************************************
       dcl-s namesArr char(100) dim(20);
     d*** End Add *** 05/09/22 *************************************************

      * work variables
     d intCo#          s              3  0
     d mode            s              1
     d strmode         s              1    inz
     d error           s              1n   inz(*off)
     D flgRcdUpdated   s              1
     d saveDT          s                   like(ACdt)
     d saveTM          s                   like(ACtime)
     d strKey          s             10
     d intRrn          s              9  0
     d intStart        s              3  0
     d intCount        s              3  0
     d process         s               n   inz(*off)
     d i               s              3  0
     d*** Beg Add *** 05/09/22 *************************************************
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);
       dcl-s  index         zoned(3:0);
     d*** End Add *** 05/09/22 *************************************************
     d*** Beg Add *** 06/22/22 ************************************************
     d strMenuChoi     s            500    Varying
     d strMenuValu     s            100    Varying
     d*** End Add *** 06/22/22 ************************************************

      * main Processing
     d Main            pr

      // Program Start
     d ProgramStart    pr
     d processMe                       n

      // Save Configuration setup
     d SaveDetail      pr

      // Save Data
     d SaveData        pr
     d intStart                            like(intStart)

      // Edit Check
     d EditCheck       pr

      // Load Data
     d LoadData        pr
     d intStart                            like(intStart)

     dGetSuffix        pr                  extpgm('SA030R')
     d suffix                         3
     d cltSuffix       s              3
     d cmdString       s            150

     dCheckIfsObj      pr                  extPgm('CHKIFSOBJ')
     d inName                       256    const
     d outFound                       1n

     d/copy sPrDate
     d/copy wPrUsrPrms
     d/copy sPrModule
     d/copy sPrCompany
     d/copy sPrGenPop
     d/copy sPrString
     d/copy sPrRunCmd
     d*** Beg Add *** 05/09/22 ************************************************
     d/copy sPrSecure
     d*** End Add *** 05/09/22 ************************************************
     d*** Beg Add *** 06/22/22 ************************************************
     d/copy sPrActPnl
     d*** End Add *** 06/22/22 ************************************************

      * Entry of Program.
     dwtClmCfgPgm      pr                  extpgm('WTPOLCFG')
     d                                9
     d                                1
     d                               10
     d                                5
     dwtClmCfgPgm      pi
     d@@rrn                           9
     d@@mode                          1
     d@@next                         10
     d@@TabInd                        5

      /free
       // start program processing
       ProgramStart(process);

       // main processing
       If process;
          //*** Beg Add *** 05/09/22 ***************************************
          If index <> 0;
          //*** End Add *** 05/09/22 ***************************************
             Main();
          //*** Beg Add *** 05/09/22 ***************************************
          Else;
             scCompName = getCompNam(intCo#);
             isNoAccess = *on;
             TabNames = 'Security';
             apShowMenu = *on;
             TabHeight  = 560;
             aPnlHeight = 620;
             Write rcdSysTbl;
             Exfmt rcdPanel;
             Read rcdSysTbl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;
          //*** End Add *** 05/09/22 ***************************************
       Endif;

       *InLr = *On;
       Return;
      /end-free
       //*** Beg Add *** 05/09/22 *********************************************
       //===================================================================
       // Load Tab Information
       //===================================================================
     p LoadTabInfo     b
     d LoadTabInfo     pi
       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup01;
       If %Found(SMUPL) and UPlvl = 999 and UPtype = '*';
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       // Determine if user is in the App Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed

       // General tab
       Chain (intCo#:'WTPOLCFG':'Policy Configuration':'General'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          TabNames = 'General';
          index = 1;
          namesArr(index) = 'General';
       Endif;

       // PEO tab
       Chain (intCo#:'WTPOLCFG':'Policy Configuration':'PEO'
        :Q1user) pTB01;
       //*** Beg Chg *** 06/22/22 ******************************************
       //***If %Found(SDTBL1) or isUsrAcc = *on;
       If isModEnabled('PEO Module') and (%Found(SDTBL1) or isUsrAcc = *on);
       //*** End Chg *** 06/22/22 ******************************************
          If index = 0;
             TabNames = 'PEO';
          Else;
             TabNames = %Trim(TabNames) + ', PEO';
          Endif;
          index = index + 1;
          namesArr(index) = 'PEO';
       Endif;

       // Import/Export Tab
       Chain (intCo#:'WTPOLCFG':'Policy Configuration':'Import/Export'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'Import/Export';
          Else;
             TabNames = %Trim(TabNames) + ', Import/Export';
          Endif;
          index = index + 1;
          namesArr(index) = 'Import/Export';
       Endif;

       // Policy/Quote Tab
       Chain (intCo#:'WTPOLCFG':'Policy Configuration':'Policy/Quote'
        :Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          If index = 0;
             TabNames = 'Policy/Quote';
          Else;
             TabNames = %Trim(TabNames) + ', Policy/Quote';
          Endif;
          index = index + 1;
          namesArr(index) = 'Policy/Quote';
       Endif;

     p LoadTabInfo     e
       //*** End Add *** 05/09/22 **********************************************
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free
       //*** Beg Add *** 05/09/22 ******************************************
       isNoAccess = *off;
       //*** End Add *** 05/09/22 ******************************************
       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
       Endif;
       If q1prms >= 2;
          mode = @@mode;
          //*** Beg Add *** 05/09/22 ***************************************
          Select;
          When @@mode = 'D';
             dspMode = *on;
          Endsl;
          //*** End Add *** 05/09/22 ***************************************
       Endif;

       // Tab Index Mode.
       TabIndex = 0;

       // Get Company no.
       intCo# = UserCo#(q1user);

       // Get Company name
       scCompName = getCompNam(intCo#);

       // Panel Heading
       scPnlHdg = 'Policy Configuration';

       //Check for applicable options
       //*** Beg Chg *** 06/22/22 ********************************************
       //***scMenuChoi = 'Application Configuration,Claims Configuration';
       //***scMenuValu = 'WTAPPCFG,WTCLMCFG';

       //***If isModEnabled('Managed Care');
       //***   scMenuChoi = %trim(scMenuChoi) + ',Managed Care Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',MTINTCFG';
       //***Endif;

       //***If isModEnabled('PEO Module');
       //***   scMenuChoi = %trim(scMenuChoi) + ',Policy Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',WTPOLCFG';
       //***Endif;
       // Add Portal Configuration
       //***If isModEnabled('Portal Module');
       //***   scMenuChoi = %trim(scMenuChoi) + ',' +
       //***    GetModuleProperty('Portal Module':'Name') + ' Configuration';
       //***   scMenuValu = %trim(scMenuValu) + ',WICONFIG';
       //***Endif;
       GetActPnlItems(strMenuChoi:strMenuvalu);
       scMenuChoi = strMenuChoi;
       scMenuValu = strMenuValu;
       //*** End Chg *** 06/22/22 ********************************************

       Clear chkKeys;
       chkKeys.sdacpKeys(1).key = 'MCPMastOvr';
       chkKeys.sdacpKeys(2).key = 'BillSingle';
       chkKeys.sdacpKeys(3).key = 'BillDivRng';
       chkKeys.sdacpKeys(4).key = 'BillUnbill';
       chkKeys.sdacpKeys(5).key = 'AuditAllDv';
       chkKeys.sdacpKeys(6).key = 'LocDelOver';
       chkKeys.sdacpKeys(7).key = 'AR_Import_';
       chkKeys.sdacpKeys(8).key = 'AudRqstInq';
       //** Beg Add *** 04/24/23 **********************************************
       chkKeys.sdacpKeys(9).key = 'ARBat_FlgA';                     // Flag for Approval
       chkKeys.sdacpKeys(10).key = 'ARBat_FlgT';                    // Flag for Transfer
       chkKeys.sdacpKeys(11).key = 'ARBat_ERpt';                    // Edit Report
       chkKeys.sdacpKeys(12).key = 'ARBat_CrtF';                    // Create EFT File
       chkKeys.sdacpKeys(13).key = 'ARBat_MUpd';                    // Mass Update
       chkKeys.sdacpKeys(14).key = 'ARBat_EBat';                    // Edit Open Batches
       chkKeys.sdacpKeys(15).key = 'ARBat_TBat';                    // Transfer Batches
       chkKeys.sdacpKeys(16).key = 'ARBat_Purg';                    // Purge
       chkKeys.sdacpKeys(17).key = 'ARBat_List';                    // List by Employers
       //** End Add *** 04/24/23 **********************************************
       //** Beg Add *** 11/01/23 **********************************************
       chkKeys.sdacpKeys(18).key = 'RFBat_BEdt';                    // Batch Edit Report
       chkKeys.sdacpKeys(19).key = 'RFBat_GenC';                    // Generate Check Request
       //** End Add *** 11/01/23 **********************************************
       //** Beg Add *** 03/21/24 **********************************************
       chkKeys.sdacpKeys(20).key = 'EndFrm_Cpy';                    // Copy Endorsement Forms
       chkKeys.sdacpKeys(21).key = 'EndFrm_Del';                    // Delete Endorsement Forms
       chkKeys.sdacpKeys(22).key = 'EndFrm_Prt';                    // Print Endorsement Forms
       chkKeys.sdacpKeys(23).key = 'EndFrm_Cie';                    // Change Include/Exclude
       //** End Add *** 03/21/24 **********************************************
       //** Beg Add *** 08/01/24 **********************************************
       chkKeys.sdacpKeys(24).key = 'AudReverse';                    // Audit Reversal
       //** End Add *** 08/01/24 **********************************************
       GetSuffix(cltSuffix);

      //*** Beg Add *** 03/24/22 **********************************************
      // If Policy/Quote Display Control Record does not exists for Company
      // defaults all checkboxes to 'Y' and add record

       Chain(n) intCo# PPQ01 ;
       If Not %Found(WTPQL);
          PQCO#      = intCo# ;
          PQCLEARS   = 'Y';                      // Clearance Screen
          PQBILLW    = 'Y';                      // Billing Master Window
          PQBILLS    = 'Y';                      // Policy and Billing Window
          PQRATEW    = 'Y';                      // Rating Window
          PQRATES    = 'Y';                      // Rating Screen
          PQLDEDW    = 'Y';                      // Large Deductible Window
          PQLDEDS    = 'Y';                      // Large Deductible Screen
          PQSCHDW    = 'Y';                      // Schedule Rating Window
          PQSCHDS    = 'Y';                      // Schedule Rating Screen
          PQRETRW    = 'Y';                      // Retrospective Rating Window
          PQRETRS    = 'Y';                      // Retrospective Rating Screen
          PQADDW     = 'Y';                      // Additional Info Window
          PQADDS     = 'Y';                      // Additional Info Screen
          PQPAYW     = 'Y';                      // Payroll Add Window
          PQPAYS     = 'Y';                      // Payroll Screen
          PQAGTW     = 'Y';                      // Agency Window
          PQAGTS     = 'Y';                      // Agency Screen
          PQLOCS     = 'Y';                      // Location Screen
          PQOFFS     = 'Y';                      // Officer Screen
          PQIDT      = Dateto7(SystemDate());    // INITIAL DATE
          PQITM      = Timeto6(SystemTime());    // INITIAL TIME
          PQIPGM     = Q1pgm;                    // INITIAL PROGRAM
          PQIUSR     = Q1user;                   // INITIAL USER
          PQDT       = Dateto7(SystemDate());    // DATE
          PQTIME     = Timeto6(SystemTime());    // TIME
          PQPGM      = Q1pgm;                    // PROGRAM
          PQUSER     = Q1user;                   // USER
          Write PPQ01;
       EndIf;
      //*** End Add *** 03/24/22 **********************************************
      //*** Beg Add *** 05/09/22 *********************************************
       LoadTabInfo();
       //*** End Add *** 05/09/22 *********************************************
       processMe = *on;
       Return;

      /end-free
     p ProgramStart    e

       //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     D Main            PI

      /free

       //Load General
       Exsr LoadGeneral;

       ExitNow = *Off;
       error = *Off;

       // Process until exit button is pressed.
       Dow ExitNow = *Off;

          If mode = 'D';
             dsbAppCfg = *on;
             isImpLock = *on;
             isOutLock = *on;
          Endif;
          // Process policy Configuration screen.
          Exsr ExfmtScreens;

          error = *off;
          clear dsSecTypEr;
          clear dsSecLvlEr;
          clear dsSecTypErSD;
          clear dsSecLvlErSD;
          clear dsSecTypErDR;
          clear dsSecLvlErDR;
          clear dsSecTypErUD;
          clear dsSecLvlErUD;
          clear dsSecTypErAD;
          clear dsSecLvlErAD;
          clear dsSecTypErAI;
          clear dsSecLvlErAI;
          //** Beg Add *** 04/24/23 **********************************************
          clear dsSecTypErFA;
          clear dsSecLvlErFA;
          clear dsSecTypErFT;
          clear dsSecLvlErFT;
          clear dsSecTypErRP;
          clear dsSecLvlErRP;
          clear dsSecTypErCF;
          clear dsSecLvlErCF;
          clear dsSecTypErMU;
          clear dsSecLvlErMU;
          clear dsSecTypErEB;
          clear dsSecLvlErEB;
          clear dsSecTypErTB;
          clear dsSecLvlErTB;
          clear dsSecTypErPU;
          clear dsSecLvlErPU;
          clear dsSecTypErLE;
          clear dsSecLvlErLE;
          //** End Add *** 04/24/23 **********************************************
          //** Beg Add *** 11/01/23 **********************************************
          clear dsSecTypErBE;
          clear dsSecLvlErBE;
          clear dsSecTypErGC;
          clear dsSecLvlErGC;
          //** End Add *** 11/01/23 **********************************************
          //** Beg Add *** 03/21/24 **********************************************
          clear dsSecTypErCE;
          clear dsSecLvlErCE;
          clear dsSecTypErDE;
          clear dsSecLvlErDE;
          clear dsSecTypErPE;
          clear dsSecLvlErPE;
          clear dsSecTypErIE;
          clear dsSecLvlErIE;
          //** End Add *** 03/21/24 **********************************************
          //*** Beg Add ***** 08/01/24 *******************************************
          clear dsSecTypErAR;
          clear dsSecLvlErAR;
          //*** End Add ***** 08/01/24 *******************************************
          Clear erMsgArImp;
          Clear erMsgArOut;
          Clear erIndArImp;
          Clear erIndArOut;

          // Perform actions on screen.
          Select;

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh = *Off;
             error = *Off;
             Exsr LoadGeneral;
             //*** Beg Add *** 04/23/25 ******************************************
             Exsr GetScreenContol;
             //*** End Add *** 04/23/25 ******************************************

          When btnSave = *on;
             btnSave = *off;
             error   = *off;

             If mode <> 'D';
                editCheck();
                If error = *off;
                   SaveDetail();
                Endif;
             Endif;
          //*** Beg Add *** 05/09/22 ******************************************
          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'WTPOLCFG';
             inPgmName     = 'Policy Configuration';
             inTabName     =  namesArr(TabIndex + 1);
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;
          //*** End Add *** 05/09/22 ******************************************
          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       aMenHeight = 120;
       Select;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 0;
       When namesArr(TabIndex + 1)= 'General';
       //*** End Chg *** 05/09/22 **********************************************
          //*** Beg Chg *** 04/24/23 **********************************************
          //*** TabHeight = 850;
          //*** Beg Chg *** 11/01/23 **********************************************
          //*** TabHeight = 1450;
          //*** Beg Chg *** 03/21/24 **********************************************
          //*** TabHeight = 1660;
          //*** Beg Chg ***** 08/01/24 ******************************************
          //***TabHeight = 2050;
          TabHeight = 2150;
          //*** End Chg ***** 08/01/24 ******************************************
          //*** End Chg *** 03/21/24 **********************************************
          //*** End Chg *** 11/01/23 **********************************************
          //*** End Chg *** 04/24/23 **********************************************
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr WriteAction;
          Write rcdPanel;
          Exfmt RcdGenPol;
          Exsr ReadAction;
          Read rcdPanel;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 1;
       When namesArr(TabIndex + 1)= 'PEO';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 800;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr WriteAction;
          Write rcdPanel;
          Exfmt RcdPeoPol;
          Exsr ReadAction;
          Read rcdPanel;
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 2;
       When namesArr(TabIndex + 1)= 'Import/Export';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 800;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr WriteAction;
          Write rcdPanel;
          Exfmt RcdImExPol;
          Exsr ReadAction;
          Read rcdPanel;
      //*** Beg Add *** 03/24/22 **********************************************
       //*** Beg Chg *** 05/09/22 **********************************************
       //***When TabIndex = 3;
       When namesArr(TabIndex + 1)= 'Policy/Quote';
       //*** End Chg *** 05/09/22 **********************************************
          TabHeight = 800;
          APnlHeight = TabHeight + 60;
          apFootHght = aMenHeight + 125;
          apShowMenu = *on;
          Exsr WriteAction;
          Write rcdPanel;
          //*** Beg Del *** 04/23/25 **********************************************
          //*** Exsr GetScreenContol;
          //*** End Del *** 04/23/25 **********************************************
          ScClrnScr = 'Y' ;
          ScBillMst = 'Y' ;
          Exfmt RcdPolQteC;
          Exsr ReadAction;
          Read rcdPanel;
      //*** End Add *** 03/24/22 **********************************************
       EndSl;

       Endsr;

      //*** Beg Add *** 03/24/22 **********************************************
      //===================================================================
      // Get Screen Conrols
      //===================================================================
       BegSr GetScreenContol;

      // If Policy/Quote Display Control Record does not exists for Company
      // defaults to 'Y' for all checkboxes

         Chain(n) intCo# PPQ01 ;
         If Not %Found(WTPQL);
            PQCO#      = intCo# ;
            PQCLEARS   = 'Y';                    // Clearance Screen
            PQBILLW    = 'Y';                    // Billing Master Window
            PQBILLS    = 'Y';                    // Policy and Billing Window
            PQRATEW    = 'Y';                    // Rating Window
            PQRATES    = 'Y';                    // Rating Screen
            PQLDEDW    = 'Y';                    // Large Deductible Window
            PQLDEDS    = 'Y';                    // Large Deductible Screen
            PQSCHDW    = 'Y';                    // Schedule Rating Window
            PQSCHDS    = 'Y';                    // Schedule Rating Screen
            PQRETRW    = 'Y';                    // Retrospective Rating Window
            PQRETRS    = 'Y';                    // Retrospective Rating Screen
            PQADDW     = 'Y';                    // Additional Info Window
            PQADDS     = 'Y';                    // Additional Info Screen
            PQPAYW     = 'Y';                    // Payroll Add Window
            PQPAYS     = 'Y';                    // Payroll Screen
            PQAGTW     = 'Y';                    // Agency Window
            PQAGTS     = 'Y';                    // Agency Screen
            PQLOCS     = 'Y';                    // Location Screen
            PQOFFS     = 'Y';                    // Officer Screen
            PQIDT      = Dateto7(SystemDate());  // INITIAL DATE
            PQITM      = Timeto6(SystemTime());  // INITIAL TIME
            PQIPGM     = Q1pgm;                  // INITIAL PROGRAM
            PQIUSR     = Q1user;                 // INITIAL USER
            PQDT       = Dateto7(SystemDate());  // DATE
            PQTIME     = Timeto6(SystemTime());  // TIME
            PQPGM      = Q1pgm;                  // PROGRAM
            PQUSER     = Q1user;                 // USER
            Write PPQ01;
         Else ;
            ScClrnScr  = PQCLEARS ;              // Clearance Screen
            ScBillMst  = PQBILLW  ;              // Billing Master Window
            ScPolBilIn = PQBILLS  ;              // Policy and Billing Window
            ScRatInfEn = PQRATEW  ;              // Rating Window
            ScRatInf   = PQRATES  ;              // Rating Screen
            ScLrgDedEn = PQLDEDW  ;              // Large Deductible Window
            ScLrgDedIn = PQLDEDS  ;              // Large Deductible Screen
       //** Beg Chg *** 09/12/22 **********************************************
            ScSchRatEn = *blanks  ;              // Schedule Rating Window
            ScSchRatIn = *blanks  ;              // Schedule Rating Screen
       //** ScSchRatEn = PQSCHDW  ;              // Schedule Rating Window
       //** ScSchRatIn = PQSCHDS  ;              // Schedule Rating Screen
       //** End Chg *** 09/12/22 **********************************************
            ScRtrRatEn = PQRETRW  ;              // Retrospective Rating Window
            ScRtrRatIn = PQRETRS  ;              // Retrospective Rating Screen
            ScAddInfoE = PQADDW   ;              // Additional Info Window
            ScAddInfo  = PQADDS   ;              // Additional Info Screen
            ScPayRolAd = PQPAYW   ;              // Payroll Add Window
            ScPayRolIn = PQPAYS   ;              // Payroll Screen
            ScAgcyEnt  = PQAGTW   ;              // Agency Window
            ScAgcyInfo = PQAGTS   ;              // Agency Screen
            ScLocInfo  = PQLOCS   ;              // Location Screen
            ScOwnInfo  = PQOFFS   ;              // Officer Screen
         EndIf;

       Endsr;
      //*** End Add *** 03/24/22 **********************************************
       //===================================================================
       // Write action panel;
       //===================================================================
       BegSr WriteAction;
             Write rcdSysTbl;
       Endsr;
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;
             Read rcdSysTbl;
       Endsr;
       //===================================================================
       // Load Gegeral
       //===================================================================
       BegSr LoadGeneral;

       // Application Configuration setup
       dsbAppCfg = *off;
       clear rcdGenPol;
       clear rcdPeoPol;
       clear rcdImExPol;
      //*** Beg Add *** 03/24/22 **********************************************
       clear RcdPolQteC;
      //*** End Add *** 03/24/22 **********************************************

       // Allow Override of PEO Master/MCP Restriction
       For i=1 to intKeyDim;
          strKey = chkKeys.sdacpKeys(i).key;
          Chain(n) strKey pac00;
          If not %found;
             clear *all pac00;
             chkKeys.sdacpKeys(i).rrn = 0;
             intRrn = 0;
          Else;
             chkKeys.sdacpKeys(i).rrn = dbrrn;
          Endif;
          intStart = 0;
          LoadData(intStart);
       EndFor;
       //*** Beg Add *** 03/14/22 **********************************************
       Chain(n) (intCo#) ppa01;
       If %Found(WMPAL1);
          scEntGshNm = PAentGshNm;
       Endif;
       //*** End Add *** 03/14/22 **********************************************
       //*** Beg Add *** 04/23/25 **********************************************
       Exsr GetScreenContol;
       //*** End Add *** 04/23/25 **********************************************

       EndSr;

      /end-free
     P Main            E

       //===================================================================
       // Save Data.
       //===================================================================
     p SaveDetail      b
     d SaveDetail      pi

      /free

       // Allow Override of PEO Master/MCP Restriction
       For i = 1 to intKeyDim;
          strKey = chkKeys.sdacpKeys(i).key;
          Chain strKey pac00;
          intStart = 0;
          SaveData(intStart);
          Exsr srWrtUpd;
       EndFor;
       //*** Beg Add *** 03/14/22 **********************************************
       // Update or Write to the Policy Application Config Master File
       Chain (intCo#) ppa01;
       If %Found(WMPAL1);
          PAentGshNm = scEntGshNm;
          PAuser = Q1user;
          PApgm  = Q1pgm;
          PAtime = Timeto6(SystemTime());
          PAdt   = Dateto7(SystemDate());
          Update ppa01;
       Else;
          PAco#      = intCo#;
          PAentGshNm = scEntGshNm;
          PAiusr = Q1user;
          PAiPgm = Q1pgm;
          PAiTim = Timeto6(SystemTime());
          PAiDt  = Dateto7(SystemDate());
          PAuser = Q1user;
          PApgm  = Q1pgm;
          PAtime = Timeto6(SystemTime());
          PAdt   = Dateto7(SystemDate());
          Write ppa01;
       Endif;
       //*** End Add *** 03/14/22 **********************************************

       //*** Beg Add *** 03/24/22 **********************************************
       // Save Checkboxes values to DB

       Chain intCo# PPQ01 ;
       If %Found(WTPQL);
          PQCLEARS  = ScClrnScr  ;               // Clearance Screen
          PQBILLW   = ScBillMst  ;               // Billing Master Window
          PQBILLS   = ScPolBilIn ;               // Policy and Billing Window
          PQRATEW   = ScRatInfEn ;               // Rating Window
          PQRATES   = ScRatInf   ;               // Rating Screen
          PQLDEDW   = ScLrgDedEn ;               // Large Deductible Window
          PQLDEDS   = ScLrgDedIn ;               // Large Deductible Screen
          PQSCHDW   = ScSchRatEn ;               // Schedule Rating Window
          PQSCHDS   = ScSchRatIn ;               // Schedule Rating Screen
          PQRETRW   = ScRtrRatEn ;               // Retrospective Rating Window
          PQRETRS   = ScRtrRatIn ;               // Retrospective Rating Screen
          PQADDW    = ScAddInfoE ;               // Additional Info Window
          PQADDS    = ScAddInfo  ;               // Additional Info Screen
          PQPAYW    = ScPayRolAd ;               // Payroll Add Window
          PQPAYS    = ScPayRolIn ;               // Payroll Screen
          PQAGTW    = ScAgcyEnt  ;               // Agency Window
          PQAGTS    = ScAgcyInfo ;               // Agency Screen
          PQLOCS    = ScLocInfo  ;               // Location Screen
          PQOFFS    = ScOwnInfo  ;               // Officer Screen
          PQDT      = Dateto7(SystemDate());     // DATE
          PQTIME    = Timeto6(SystemTime());     // TIME
          PQPGM     = Q1pgm;                     // PROGRAM
          PQUSER    = Q1user;                    // USER
          Update PPQ01;                          // Update Record
       EndIf;
       //*** End Add *** 03/24/22 **********************************************

       //===================================================================
       // Write/Update Record
       //===================================================================
       BegSr SrWrtUpd;

       ACpgm  = Q1pgm;
       ACuser = Q1user;
       ACtime = Timeto6(SystemTime());
       ACdt   = Dateto7(SystemDate());
       chkTime.timeStamp(i).date = ACdt;
       chkTime.timeStamp(i).time = ACtime;

       If not %found;
          ACapid = strKey;
          ACipgm = Q1pgm;
          ACiusr = Q1user;
          ACitim = Timeto6(SystemTime());
          ACidt  = Dateto7(SystemDate());
          Write pac00;
          chkKeys.sdacpKeys(i).rrn  = dbrrn;
       Else;
          Update pac00;
       Endif;
       unlock SDACL;

       Endsr;
       //===================================================================

      /end-free

     p SaveDetail      e
       //===================================================================
       // Load Data.
       //===================================================================
     p LoadData        b
     d LoadData        pi
     d intStrCnt                      3  0

      /free

       For intCount = 1 to intFlgDim;
          Select;
          When strKey = 'AR_Import_';
             scArImpEml = ACArImpEml;
             scArImpFld = ACArImpFld;
             scArOutFld = ACArOutFld;
            // Disables scArimpFld and scArOutFld
             isImpLock = AClkImpFld = 'Y';
             isOutLock = AClkOutFld = 'Y';
             Leave;
          When strKey = 'MCPMastOvr';
             dsSecTyp(intStrCnt + intCount) = dsSecTypFl(intCount);
             dsSecLvl(intStrCnt + intCount) = dsSecLvlFl(intCount);
          When strKey = 'BillSingle';
             dsSecTypSD(intStrCnt + intCount) = dsSecTypFl(intCount);
             dsSecLvlSD(intStrCnt + intCount) = dsSecLvlFl(intCount);
          When strKey = 'BillDivRng';
             dsSecTypDR(intStrCnt + intCount) = dsSecTypFl(intCount);
             dsSecLvlDR(intStrCnt + intCount) = dsSecLvlFl(intCount);
          When strKey = 'BillUnbill';
             dsSecTypUD(intStrCnt + intCount) = dsSecTypFl(intCount);
             dsSecLvlUD(intStrCnt + intCount) = dsSecLvlFl(intCount);
          When strKey = 'AuditAllDv';
             dsSecTypAD(intStrCnt + intCount) = dsSecTypFl(intCount);
             dsSecLvlAD(intStrCnt + intCount) = dsSecLvlFl(intCount);
          When strKey = 'LocDelOver';
             dsSecTypDL(intStrCnt + intCount) = dsSecTypFl(intCount);
             dsSecLvlDL(intStrCnt + intCount) = dsSecLvlFl(intCount);
             scLDRmk = ACRmcd;
             scLDUser = ACRusr;
          When strKey = 'AudRqstInq';
             dsSecTypAI(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlAI(intStrCnt + intCount) = dsSecLvlFL(intCount);
          //** Beg Add *** 04/24/23 **********************************************
          When strKey = 'ARBat_FlgA';
             dsSecTypFA(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlFA(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'ARBat_FlgT';
             dsSecTypFT(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlFT(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'ARBat_ERpt';
             dsSecTypRP(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlRP(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'ARBat_CrtF';
             dsSecTypCF(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlCF(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'ARBat_MUpd';
             dsSecTypMU(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlMU(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'ARBat_EBat';
             dsSecTypEB(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlEB(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'ARBat_TBat';
             dsSecTypTB(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlTB(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'ARBat_Purg';
             dsSecTypPU(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlPU(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'ARBat_List';
             dsSecTypLE(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlLE(intStrCnt + intCount) = dsSecLvlFL(intCount);
          //** End Add *** 04/24/23 **********************************************
          //** Beg Add *** 11/01/23 **********************************************
          When strKey = 'RFBat_BEdt';
             dsSecTypBE(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlBE(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'RFBat_GenC';
             dsSecTypGC(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlGC(intStrCnt + intCount) = dsSecLvlFL(intCount);
          //** End Add *** 11/01/23 **********************************************
          //** Beg Add *** 03/21/24 **********************************************
          When strKey = 'EndFrm_Cpy';
             dsSecTypCE(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlCE(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'EndFrm_Del';
             dsSecTypDE(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlDE(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'EndFrm_Prt';
             dsSecTypPE(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlPE(intStrCnt + intCount) = dsSecLvlFL(intCount);
          When strKey = 'EndFrm_Cie';
             dsSecTypIE(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlIE(intStrCnt + intCount) = dsSecLvlFL(intCount);
          //** End Add *** 03/21/24 **********************************************
          //*** Beg Add ***** 08/01/24 **********************************************
          When strKey = 'AudReverse';
             dsSecTypAR(intStrCnt + intCount) = dsSecTypFL(intCount);
             dsSecLvlAR(intStrCnt + intCount) = dsSecLvlFL(intCount);
             scARvRmk = ACRmcd;
          //*** End Add ***** 08/01/24 **********************************************
          EndSl;

       Endfor;

       chkTime.timeStamp(i).date = ACdt;
       chkTime.timeStamp(i).time = ACtime;

      /end-free

     p LoadData        e
       //===================================================================
       // Save Data.
       //===================================================================
     p SaveData        b
     d SaveData        pi
     d intStrCnt                      3  0

     d isUpdate        s              1n   inz

      /free

       For intCount = 1 to intFlgDim;
          //*** Beg Add *** 04/24/23 **********************************************
          // These flags/fields are applicable only for 'AR_Import_' application
          ACLkImpFld = ' ';
          ACLkOutFld = ' ';
          //*** End Add *** 04/24/23 **********************************************
          //*** Beg Add ***** 08/01/24 *******************************************
          Clear ACRmcd;
          //*** End Add ***** 08/01/24 *******************************************
          Select;
          When strKey = 'AR_Import_';
             ACArImpEml = scArImpEml;
             ACArImpFld = scArImpFld;
             ACArOutFld = scArOutFld;
            // Prevents changes without pgmr assist
             ACLkImpFld = 'Y';
             ACLkOutFld = 'Y';
             Return;
          When strKey = 'MCPMastOvr';
             dsSecTypFl(intCount) = Upper(dsSecTyp(intStrCnt + intCount));
             dsSecLvlFl(intCount) = dsSecLvl(intStrCnt + intCount);
          When strKey = 'BillSingle';
             dsSecTypFl(intCount) = Upper(dsSecTypSD(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlSD(intStrCnt + intCount);
          When strKey = 'BillDivRng';
             dsSecTypFl(intCount) = Upper(dsSecTypDR(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlDR(intStrCnt + intCount);
          When strKey = 'BillUnbill';
             dsSecTypFl(intCount) = Upper(dsSecTypUD(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlUD(intStrCnt + intCount);
          When strKey = 'AuditAllDv';
             dsSecTypFl(intCount) = Upper(dsSecTypAD(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlAD(intStrCnt + intCount);
          When strKey = 'LocDelOver';
             dsSecTypFl(intCount) = Upper(dsSecTypDL(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlDL(intStrCnt + intCount);
             ACRmcd = scLDRmk;
             ACRusr = scLDUser;
          When strKey = 'AudRqstInq';
          //** Beg Chg *** 04/24/23 **********************************************
             //*** dsSecTypAI(intCount) = Upper(dsSecTypAI(intStrCnt + intCount  ));
             //*** dsSecLvlAI(intCount) = dsSecLvlAI(intStrCnt + intCount);
             // Correct variable name
             dsSecTypFl(intCount) = Upper(dsSecTypAI(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlAI(intStrCnt + intCount);
          //** End Chg *** 04/24/23 **********************************************
          //** Beg Add *** 04/24/23 **********************************************
          When strKey = 'ARBat_FlgA';
             dsSecTypFl(intCount) = Upper(dsSecTypFA(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlFA(intStrCnt + intCount);
          When strKey = 'ARBat_FlgT';
             dsSecTypFl(intCount) = Upper(dsSecTypFT(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlFT(intStrCnt + intCount);
          When strKey = 'ARBat_ERpt';
             dsSecTypFl(intCount) = Upper(dsSecTypRP(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlRP(intStrCnt + intCount);
          When strKey = 'ARBat_CrtF';
             dsSecTypFl(intCount) = Upper(dsSecTypCF(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlCF(intStrCnt + intCount);
          When strKey = 'ARBat_MUpd';
             dsSecTypFl(intCount) = Upper(dsSecTypMU(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlMU(intStrCnt + intCount);
          When strKey = 'ARBat_EBat';
             dsSecTypFl(intCount) = Upper(dsSecTypEB(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlEB(intStrCnt + intCount);
          When strKey = 'ARBat_TBat';
             dsSecTypFl(intCount) = Upper(dsSecTypTB(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlTB(intStrCnt + intCount);
          When strKey = 'ARBat_Purg';
             dsSecTypFl(intCount) = Upper(dsSecTypPU(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlPU(intStrCnt + intCount);
          When strKey = 'ARBat_List';
             dsSecTypFl(intCount) = Upper(dsSecTypLE(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlLE(intStrCnt + intCount);
          //** End Add *** 04/24/23 **********************************************
          //** Beg Add *** 11/01/23 **********************************************
          When strKey = 'RFBat_BEdt';
             dsSecTypFl(intCount) = Upper(dsSecTypBE(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlBE(intStrCnt + intCount);
          When strKey = 'RFBat_GenC';
             dsSecTypFl(intCount) = Upper(dsSecTypGC(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlGC(intStrCnt + intCount);
          //** End Add *** 11/01/23 **********************************************
          //** Beg Add *** 03/21/24 **********************************************
          When strKey = 'EndFrm_Cpy';
             dsSecTypFl(intCount) = Upper(dsSecTypCE(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlCE(intStrCnt + intCount);
          When strKey = 'EndFrm_Del';
             dsSecTypFl(intCount) = Upper(dsSecTypDE(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlDE(intStrCnt + intCount);
          When strKey = 'EndFrm_Prt';
             dsSecTypFl(intCount) = Upper(dsSecTypPE(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlPE(intStrCnt + intCount);
          When strKey = 'EndFrm_Cie';
             dsSecTypFl(intCount) = Upper(dsSecTypIE(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlIE(intStrCnt + intCount);
          //** End Add *** 03/21/24 **********************************************
          //*** Beg Add ***** 08/01/24 *******************************************
          When strKey = 'AudReverse';
             dsSecTypFl(intCount) = Upper(dsSecTypAR(intStrCnt + intCount  ));
             dsSecLvlFl(intCount) = dsSecLvlAR(intStrCnt + intCount);
             ACRmcd = scARvRmk;
          //*** End Add ***** 08/01/24 *******************************************
          EndSl;
       Endfor;
       // Left adjust all values
       Dou not isUpdate;
          Reset isUpdate;
          For intCount = intFlgDim downto 2;
             If dsSecTypFl(intCount)<>*blank and dsSecTypFl(intCount-1)=*blank;
                dsSecTypFl(intCount-1) = dsSecTypFl(intCount);
                dsSecLvlFl(intCount-1) = dsSecLvlFl(intCount);
                Clear dsSecTypFl(intCount);
                Clear dsSecLvlFl(intCount);
                isUpdate = *on;
             Endif;
          Endfor;
       Enddo;

      /end-free

     p SaveData        e
       //===================================================================
       // Edit Detail Data.
       //===================================================================
     p EditCheck       b
     d EditCheck       pi

      /free
       Dcl-S isFound Ind;

       error = *off;
       // Check to be sure screen has not been updated since entry
       // Check if SDACP updated since screen displayed
       For i = 1 to intKeyDim;
          flgRcdUpdated =
          RcdUpdatedErr('SDACP':chkKeys.sdacpKeys(i).rrn:'ACdt':'ACtime':
             'ACuser':chkTime.timeStamp(i).date:chkTime.timeStamp(i).time);

          Select;
          When flgRcdUpdated = '0'; // No error
          When flgRcdUpdated = '1'; // Error
             error = *on;
             Leave;
          When flgRcdUpdated = '2'; // Error with refresh
          Endsl;
       EndFor;

       If error = *off;

          For intCount = 1 to intArrDim;
             For i = 1 to intKeyDim;

             Select;
             // PEO Edits
             When chkKeys.sdacpKeys(i).key = 'MCPMastOvr';
                If dsSecTyp(intCount) = *blanks and dsSecLvl(intCount) > 0;
                   dsSecTypEr(intCount) = *on;
                   error = *on;
                Endif;

                If dsSecTyp(intCount) <> *blanks and dsSecLvl(intCount) = 0;
                   dsSecLvlEr(intCount) = *on;
                   error = *on;
                Endif;

             // Single Division Edits
             When chkKeys.sdacpKeys(i).key = 'BillSingle';
                If dsSecTypSD(intCount) = ' ' and dsSecLvlSD(intCount) > 0;
                   dsSecTypErSD(intCount) = *on;
                   error = *on;
                Endif;

                If dsSecTypSD(intCount) <> ' ' and dsSecLvlSD(intCount) = 0;
                   dsSecLvlErSD(intCount) = *on;
                   error = *on;
                Endif;

             // Division Range Edits
             When chkKeys.sdacpKeys(i).key = 'BillDivRng';
                If dsSecTypDR(intCount) = ' ' and dsSecLvlDR(intCount) > 0;
                   dsSecTypErDR(intCount) = *on;
                   error = *on;
                Endif;

                If dsSecTypDR(intCount) <> ' ' and dsSecLvlDR(intCount) = 0;
                   dsSecLvlErDR(intCount) = *on;
                   error = *on;
                Endif;

             // Unbilled Division Edits
             When chkKeys.sdacpKeys(i).key = 'BillUnbill';
                If dsSecTypUD(intCount) = ' ' and dsSecLvlUD(intCount) > 0;
                   dsSecTypErUD(intCount) = *on;
                   error = *on;
                Endif;

                If dsSecTypUD(intCount) <> ' ' and dsSecLvlUD(intCount) = 0;
                   dsSecLvlErUD(intCount) = *on;
                   error = *on;
                Endif;

             // Audit All Division Edits
             When chkKeys.sdacpKeys(i).key = 'AuditAllDv';
                If dsSecTypAD(intCount) = ' ' and dsSecLvlAD(intCount) > 0;
                   dsSecTypErAD(intCount) = *on;
                   error = *on;
                Endif;

                If dsSecTypAD(intCount) <> ' ' and dsSecLvlAD(intCount) = 0;
                   dsSecLvlErAD(intCount) = *on;
                   error = *on;
                Endif;
             When strKey = 'LocDelOver';
             // Location Deletion Edits
             When chkKeys.sdacpKeys(i).key = 'LocDelOver';
                If dsSecTypDL(intCount) = ' ' and dsSecLvlDL(intCount) > 0;
                   dsSecTypErDL(intCount) = *on;
                   error = *on;
                Endif;

                If dsSecTypDL(intCount) <> ' ' and dsSecLvlDL(intCount) = 0;
                   dsSecLvlErDL(intCount) = *on;
                   error = *on;
                Endif;
             //*** Beg Add ***** 08/01/24 ************************************
             // Audit Reversal Edits
             When chkKeys.sdacpKeys(i).key = 'AudReverse';
                If dsSecTypAR(intCount) = ' ' and dsSecLvlAR(intCount) > 0;
                   dsSecTypErAR(intCount) = *on;
                   error = *on;
                Endif;

                If dsSecTypAR(intCount) <> ' ' and dsSecLvlAR(intCount) = 0;
                   dsSecLvlErAR(intCount) = *on;
                   error = *on;
                Endif;
             //*** End Add ***** 08/01/24 ************************************

             When chkKeys.sdacpKeys(i).key = 'AR_Import_' And intCount = 1;
               // Input and output folders can't be the same.
                If scArImpFld <> ' ' And scArOutFld <> ' ';
                   If scArImpFld = scArOutFld;
                      error = *On;
                      erIndArImp = *On;
                      erIndArOut = *On;
                      erMsgArImp = 'Import and Output folders can ' +
                       'not be the same.';
                      erMsgArOut = erMsgArImp;
                   EndIf;
                EndIf;
               // Folder names must begin with '/'
               If scArImpFld <> ' ';
                  If %Subst(scArImpFld:1:1) <> '/';
                     error = *On;
                     erIndArImp = *On;
                     erMsgArImp = 'Import folder name must begin with ''/''.';
                  Endif;
               Endif;
               If scArOutFld <> ' ';
                  If %Subst(scArOutFld:1:1) <> '/';
                     error = *On;
                     erIndArOut = *On;
                     erMsgArOut = 'Output folder name must begin with ''/''.';
                  Endif;
               Endif;
               // Folder names must end with '/'
               If scArImpFld <> ' ';
                  If %Subst(scArImpFld:%Len(%Trim(scArImpFld)):1) <> '/';
                     error = *On;
                     erIndArImp = *On;
                     erMsgArImp = 'Import folder name must end with ''/''.';
                  Endif;
               Endif;
               If scArOutFld <> ' ';
                  If %Subst(scArOutFld:%Len(%Trim(scArOutFld)):1) <> '/';
                     error = *On;
                     erIndArOut = *On;
                     erMsgArOut = 'Output folder name must end with ''/''.';
                  Endif;
               Endif;
              // Folder names must both be filled out if one is.
               If scArImpFld <> *Blanks And scArOutFld = *Blanks
                Or scArImpFld = *Blanks And scArOutFld <> *Blanks;
                  error = *On;
                  erIndArOut = *On;
                  erIndArImp = *On;
                  erMsgArImp =
                   'Import and output folder names are mutally inclusive.';
                  erMsgArOut = erMsgArImp;
               Endif;
              // Attempt to create folders.
               If ACArImpFld <> scArImpFld And error = *Off;
                  error = CreateDirectory(scArImpFld);
                  If error = *On;
                     CheckIfsObj(scArImpFld:isFound);
                     If isFound = *Off;
                        erIndArImp = *On;
                        erMsgArImp =
                         'Import folder name could not be created.';
                     Else;
                        isImpLock = *On;
                        error = *Off;
                     Endif;
                  Else;
                     isImpLock = *On;
                  Endif;
               Endif;
               If ACArOutFld <> scArOutFld And error = *Off;
                  error = CreateDirectory(scArOutFld);
                  If error = *On;
                     CheckIfsObj(scArOutFld:isFound);
                     If isFound = *Off;
                        erIndArOut = *On;
                        erMsgArOut =
                         'Output folder name could not be created.';
                     Else;
                        isOutLock = *On;
                        error = *Off;
                     Endif;
                  Else;
                     isOutLock = *On;
                  Endif;
               Endif;

             //*** Beg Add *** 04/24/23 **********************************************
             // Flag for Approval Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_FlgA';
                If dsSecTypFA(intCount) = ' ' and dsSecLvlFA(intCount) > 0;
                   dsSecTypErFA(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypFA(intCount) <> ' ' and dsSecLvlFA(intCount) = 0;
                   dsSecLvlErFA(intCount) = *on;
                   error = *on;
                Endif;

             // Flag for Transfer Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_FlgT';
                If dsSecTypFT(intCount) = ' ' and dsSecLvlFT(intCount) > 0;
                   dsSecTypErFT(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypFT(intCount) <> ' ' and dsSecLvlFT(intCount) = 0;
                   dsSecLvlErFT(intCount) = *on;
                   error = *on;
                Endif;

             // Edit Report Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_ERpt';
                If dsSecTypRP(intCount) = ' ' and dsSecLvlRP(intCount) > 0;
                   dsSecTypErRP(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypRP(intCount) <> ' ' and dsSecLvlRP(intCount) = 0;
                   dsSecLvlErRP(intCount) = *on;
                   error = *on;
                Endif;

             // Create EFT File Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_CrtF';
                If dsSecTypCF(intCount) = ' ' and dsSecLvlCF(intCount) > 0;
                   dsSecTypErCF(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypCF(intCount) <> ' ' and dsSecLvlCF(intCount) = 0;
                   dsSecLvlErCF(intCount) = *on;
                   error = *on;
                Endif;

             // Mass Update Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_MUpd';
                If dsSecTypMU(intCount) = ' ' and dsSecLvlMU(intCount) > 0;
                   dsSecTypErMU(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypMU(intCount) <> ' ' and dsSecLvlMU(intCount) = 0;
                   dsSecLvlErMU(intCount) = *on;
                   error = *on;
                Endif;

             // Edit Open Batches Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_EBat';
                If dsSecTypEB(intCount) = ' ' and dsSecLvlEB(intCount) > 0;
                   dsSecTypErEB(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypEB(intCount) <> ' ' and dsSecLvlEB(intCount) = 0;
                   dsSecLvlErEB(intCount) = *on;
                   error = *on;
                Endif;

             // Transfer Batches Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_TBat';
                If dsSecTypTB(intCount) = ' ' and dsSecLvlTB(intCount) > 0;
                   dsSecTypErTB(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypTB(intCount) <> ' ' and dsSecLvlTB(intCount) = 0;
                   dsSecLvlErTB(intCount) = *on;
                   error = *on;
                Endif;

             // Purge Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_Purg';
                If dsSecTypPU(intCount) = ' ' and dsSecLvlPU(intCount) > 0;
                   dsSecTypErPU(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypPU(intCount) <> ' ' and dsSecLvlPU(intCount) = 0;
                   dsSecLvlErPU(intCount) = *on;
                   error = *on;
                Endif;

             // List by Employers Edits
             When chkKeys.sdacpKeys(i).key = 'ARBat_List';
                If dsSecTypLE(intCount) = ' ' and dsSecLvlLE(intCount) > 0;
                   dsSecTypErLE(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypLE(intCount) <> ' ' and dsSecLvlLE(intCount) = 0;
                   dsSecLvlErLE(intCount) = *on;
                   error = *on;
                Endif;
             //*** End Add *** 04/24/23 **********************************************

             //*** Beg Add *** 11/01/23 **********************************************
             // Batch Edit Report Edits
             When chkKeys.sdacpKeys(i).key = 'RFBat_BEdt';
                If dsSecTypBE(intCount) = ' ' and dsSecLvlBE(intCount) > 0;
                   dsSecTypErBE(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypBE(intCount) <> ' ' and dsSecLvlBE(intCount) = 0;
                   dsSecLvlErBE(intCount) = *on;
                   error = *on;
                Endif;

             // Generate Check Request Edits
             When chkKeys.sdacpKeys(i).key = 'RFBat_GenC';
                If dsSecTypGC(intCount) = ' ' and dsSecLvlGC(intCount) > 0;
                   dsSecTypErGC(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypGC(intCount) <> ' ' and dsSecLvlGC(intCount) = 0;
                   dsSecLvlErGC(intCount) = *on;
                   error = *on;
                Endif;
             //*** End Add *** 11/01/23 **********************************************

             //*** Beg Add *** 03/21/24 **********************************************
             // Copy Endorsement Forms Edits
             When chkKeys.sdacpKeys(i).key = 'EndFrm_Cpy';
                If dsSecTypCE(intCount) = ' ' and dsSecLvlCE(intCount) > 0;
                   dsSecTypErCE(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypCE(intCount) <> ' ' and dsSecLvlCE(intCount) = 0;
                   dsSecLvlErCE(intCount) = *on;
                   error = *on;
                Endif;

             // Delete Endorsement Forms Edits
             When chkKeys.sdacpKeys(i).key = 'EndFrm_Del';
                If dsSecTypDE(intCount) = ' ' and dsSecLvlDE(intCount) > 0;
                   dsSecTypErDE(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypDE(intCount) <> ' ' and dsSecLvlDE(intCount) = 0;
                   dsSecLvlErDE(intCount) = *on;
                   error = *on;
                Endif;

             // Print Endorsement Forms Edits
             When chkKeys.sdacpKeys(i).key = 'EndFrm_Prt';
                If dsSecTypPE(intCount) = ' ' and dsSecLvlPE(intCount) > 0;
                   dsSecTypErPE(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypPE(intCount) <> ' ' and dsSecLvlPE(intCount) = 0;
                   dsSecLvlErPE(intCount) = *on;
                   error = *on;
                Endif;

             // Change Include/Exclude Edits
             When chkKeys.sdacpKeys(i).key = 'EndFrm_Cie';
                If dsSecTypIE(intCount) = ' ' and dsSecLvlIE(intCount) > 0;
                   dsSecTypErIE(intCount) = *on;
                   error = *on;
                Endif;
                If dsSecTypIE(intCount) <> ' ' and dsSecLvlIE(intCount) = 0;
                   dsSecLvlErIE(intCount) = *on;
                   error = *on;
                Endif;
             //*** End Add *** 03/21/24 **********************************************
             EndSl;
             EndFor;

          Endfor;

       Endif;

      /end-free

     p EditCheck       e
      /Free
       //===========================================
       Dcl-Proc CreateDirectory;
       //===========================================
        Dcl-Pi *N Ind;
         directoryName Char(100);
        End-Pi;
        cmdString = 'CRTDIR ''' + %Trim(directoryName) + '''';
        Monitor;
           RunCmd(cmdString:%Len(%Trim(cmdString)));
        On-Error;
           Return *On;
        EndMon;
        Return *Off;
       End-Proc;
      /End-Free
