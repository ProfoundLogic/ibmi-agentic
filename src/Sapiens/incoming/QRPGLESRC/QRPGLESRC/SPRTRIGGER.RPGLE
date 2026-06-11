     H*Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**************************************************************************
     H*
     H* Description:  This copybook will be used to process auto diary
     H* triggers
     H*
     H*  Created: 05/22/15 C:279   Project: 114WKA
     H**************************************************************************
     H* Revisions:
      *
      *  09/03/24 C:291 Project: WC2968
      *                 Add IVCNamAddr procedure
      *
      *  12/19/23 C:340 Project: WC2935
      *                 Policy Rule - Failed BIF Notification
      *
      *  05/13/21 C:62  Project: WC2663
      *                 Add Invoice AR files and fields.
      *
      *  12/17/20 C:33  Support: 89324
      *                 Remove prototype for TblNPC
      *
      *  05/22/19 C:33  Support: 79577
      *                 Move more global routines to SPITRIG
      *
      *  04/12/19 C:79  Support: 146/77518
      *                 Add procedure for Nature/Part/Cause
      *
      *  11/20/18 C:79  Support: 146/75289
      *                 Apply changes for trigger name parm
      *
      *  09/06/17 C:33  Project: WC2405
      *                -Account for Inactive triggers
      *
      *  08/15/17 C:062 Project: WC2501
      *                 Add recurring diary processing
      *
      *  04/10/17 C:136 Project: 129WUL
      *                 Add experience mod date and policy
      *                 address change triggers
      *
      *  03/31/17 C:33  Project: WC2405
      *                -Check for trigger existence
      *
      *  12/06/16 C:33  Project: WC2405
      *                -Include generic file changes
      *
      *  11/22/16 C:33  Project: WC2405
      *                -Include agent of record change.
     H*
     H* 07/06/16  C:33  Project: WC2405
     H*                 Add GetRecipientE
     H**************************************************************************
      //***  Begin Add  ***  11/22/16  ************************************
     d ptrWMAHPbfr     s               *   inz
     d ptrWMAHPaft     s               *   inz
      //***  End   Add  ***  11/22/16  ************************************
     d ptrWMCMPbfr     s               *   inz
     d ptrWMCMPaft     s               *   inz
     d ptrWMCDPbfr     s               *   inz
     d ptrWMCDPaft     s               *   inz
     d ptrWDPAPaft     s               *   inz
     d ptrWMACPaft     s               *   inz
     d ptrWDEHPaft     s               *   inz
     d ptrWMSHPaft     s               *   inz
     d ptrWXELPaft     s               *   inz
      *** beg add ***** 04/10/17 ******************
     d ptrWMEMPbfr     s               *   inz
     d ptrWMEMPaft     s               *   inz
      *** end add ***** 04/10/17 ******************
      *** beg add ***** 08/15/17 ******************
     d ptrSDCMPbfr     s               *   inz
     d ptrSDCMPaft     s               *   inz
      *** end add ***** 08/15/17 ******************
      *** Begin Add *** 05/13/21 *******************************************
     d ptrWDEAP_HLDbfr...
     d                 s               *   inz
     d ptrWDEAP_HLDaft...
     d                 s               *   inz
     d ptrWMEAPbfr     s               *   inz
     d ptrWMEAPaft     s               *   inz
      *** End   Add *** 05/13/21 *******************************************

      //***  Begin Add  ***  11/22/16  ************************************
     d dsWMAHP_bfr   e ds                  extname(wmahp)
     d                                     prefix(bfr)
     d                                     based(ptrWMAHPbfr)
     d dsWMAHP_aft   e ds                  extname(wmahp)
     d                                     prefix(aft)
     d                                     based(ptrWMAHPaft)
      //***  End   Add  ***  11/22/16  ************************************
     d dsWMCMP_bfr   e ds                  extname(wmcmp)
     d                                     prefix(bfr)
     d                                     based(ptrWMCMPbfr)
     d dsWMCMP_aft   e ds                  extname(wmcmp)
     d                                     prefix(aft)
     d                                     based(ptrWMCMPaft)
     d dsWMCDP_bfr   e ds                  extname(wmcdp)
     d                                     prefix(bfr)
     d                                     based(ptrWMCDPbfr)
     d dsWMCDP_aft   e ds                  extname(wmcdp)
     d                                     prefix(aft)
     d                                     based(ptrWMCDPaft)
     d dsWDPAP_aft   e ds                  extname(wdpap)
     d                                     prefix(aft)
     d                                     based(ptrWDPAPaft)
     d dsWAMCP_aft   e ds                  extname(wamcp)
     d                                     prefix(aft)
     d                                     based(ptrWAMCPaft)
     d dsWDEHP_aft   e ds                  extname(wdehp)
     d                                     prefix(aft)
     d                                     based(ptrWDEHPaft)
     d dsWMSHP_aft   e ds                  extname(wmshp)
     d                                     prefix(aft)
     d                                     based(ptrWMSHPaft)
     d dsWXELP_aft   e ds                  extname(wxelp)
     d                                     prefix(aft)
     d                                     based(ptrWXELPaft)
      *** beg add ***** 04/10/17 ******************
     d dsWMEMP_bfr   e ds                  extname(wmemp)
     d                                     prefix(bfr)
     d                                     based(ptrWMEMPbfr)
     d dsWMEMP_aft   e ds                  extname(wmemp)
     d                                     prefix(aft)
     d                                     based(ptrWMEMPaft)
      *** end add ***** 04/10/17 ******************

     d*** Begin Add *** 08/15/17 ***********************************
     d* SDCMP before and after data structures
     d dsSDCMP_bfr   e ds                  extname(sdcmp)
     d                                     prefix(pre)
     d                                     based(ptrSDCMPbfr)
     d dsSDCMP_aft   e ds                  extname(sdcmp)
     d                                     prefix(pos)
     d                                     based(ptrSDCMPaft)
     d*** End   Add *** 08/15/17 ***********************************
     d*** Begin Add *** 05/13/21 *******************************************
     d* WDEAP_HLD before and after data structures
     d dsWDEAP_HLD_bfr...
     d               e ds                  extname(wdeap_hld)
     d                                     prefix(bfr)
     d                                     based(ptrWDEAP_HLDbfr)
     d dsWDEAP_HLD_aft...
     d               e ds                  extname(wdeap_hld)
     d                                     prefix(aft)
     d                                     based(ptrWDEAP_HLDaft)
     d* WMEAP before and after data structures
     d dsWMEAP_bfr   e ds                  extname(wmeap)
     d                                     prefix(pre)
     d                                     based(ptrWMEAPbfr)
     d dsWMEAP_aft   e ds                  extname(wmeap)
     d                                     prefix(pos)
     d                                     based(ptrWMEAPaft)
     d
     **** Beg Add *** 12/19/23 *** WC2935 ***************************
     d ptrWIBIFPaft    S               *   Inz
     d
     d dsWIBIFP_aft  E DS                  ExtName(WIBIFP)
     d                                     Prefix(pos)
     d                                     Based(ptrWIBIFPaft)
     **** End Add *** 12/19/23 *** WC2935 ***************************
     d*** End   Add *** 05/13/21 *******************************************
     dBuildFileTrg     pr                  extpgm('STFILETRG')
     d outFile                       10    const
     d outLib                        10    const options(*nopass)
     d outMode                       10    const options(*nopass)

      * Check Module procedure prototypes
       //===================================================================
       // ProcessTrigger - Process Auto Diary Triggers
       //===================================================================
     d ProcessTrigger  pr
     d inTrgName                     10    const
     d inCo#                          3
     d inFnd#                         3
     d inPtrAfter                      *   options(*nopass)
     d inPtrBefore                     *   options(*nopass)

      //***  Begin Add  ***  11/22/16  ************************************
       //===================================================================
       // AgtRcdChg - Agent of Record Change
       //===================================================================
     d AgtRcdChg       pr              n
      //***  End   Add  ***  11/22/16  ************************************

       //===================================================================
       // ClmReten - Percentage of Specific Retention
       //===================================================================
     d ClmReten        pr              n

       //===================================================================
       // Payment2 - Payment
       //===================================================================
     d Payment2        pr              n

       //===================================================================
       // WeeksPd - Compensation Weeks Paid
       //===================================================================
     d WeeksPd         pr              n

      //***  Begin Add  ***  11/22/16  ************************************
       //===================================================================
       // UpdateAOR - Update the AOR across the policy
       //===================================================================
     d UpdateAOR       pr
      //***  End   Add  ***  11/22/16  ************************************

       //===================================================================
       // WriteDARTS - Write Diary Records into Darts file
       //===================================================================
     d WriteDARTS      pr
     d*** Beg Add *** 11/20/18 ********************************************
     d inTrgName                     10
     d*** End Add *** 11/20/18 ********************************************

       //===================================================================
       // CheckMultOccur - Check multiple occurrences
       //===================================================================
     d CheckMultOccur  pr              n
     d*** Beg Add *** 11/20/18 ********************************************
     d inTrgName                     10
     d*** End Add *** 11/20/18 ********************************************

       //===================================================================
       // GetTrgInfo - Get Auto Diary Trigger Info
       //===================================================================
     d GetTrgInfo      pr             1
     d inTrgName                     10
     d inStrKey                      40    options(*nopass)

       //===================================================================
       // GetRecipient
       //===================================================================
     d GetRecipient    pr            10

       //***  Begin Add  ***  07/06/16  ***********************************
       //===================================================================
       // Get external recipient information
       //===================================================================
     d GetRecipientE   pr           100
     d  inType                       10    const
       //***  Begin Add  ***  07/06/16  ***********************************

       //===================================================================
       // ValidUser
       //===================================================================
     d ValidUser       pr            10
     d inStrUser                     10

       //===================================================================
       // GetRoleUser
       //===================================================================
     d GetRoleUser     pr            10
     d inRole                        10

       //===================================================================
       // MeetsTrgPolicy - Validate for policy trigger criteria
       //===================================================================
     d MeetsTrgPolicy  pr              n

       //===================================================================
       // MeetsTrgClm - Validate for claims trigger criteria
       //===================================================================
     d MeetsTrgClm     pr              n

       //===================================================================
       // MeetsTrgTbl - Validate for Table trigger id/key
       //===================================================================
     d MeetsTrgTbl     pr              n
     d inTableId                     25    const
     d inTableKey                    10    const

       //***  Begin Del  12/17/20  *********************
       //===================================================================
       // MeetsTrgTblNPC - Validate for Table trigger id/key NPC
       //===================================================================
     d*MeetsTrgTblNPC  pr              n
     d*inTableId                     25    const
     d*inTableKey                    10    const
       //***  End   Del  12/17/20  *********************

       //===================================================================
       // GetPremium - Get accumulated premium
       //===================================================================
     d GetPremium      pr            13  2
     d inCo##                         3  0
     d inFnd##                        3  0
     d inFyr##                        3  0
     d inEmp##                        9  0
     d inEorA                         1    const
     d inPremLvl                     10

       //===================================================================
       // AudPrem$ - Audited Premium $ Value Change
       //===================================================================
     d AudPrem$        pr              n

       //===================================================================
       // AudPrem - Audited Premium % Change
       //===================================================================
     d AudPrem         pr              n

       //===================================================================
       // AudPremium - Audited Premium
       //===================================================================
     d AudPremium      pr              n
     d inMode                         1    const

       //===================================================================
       // CalcPrem - Mid-term Calculated Premium % Change
       //===================================================================
     d CalcPrem        pr              n

       //===================================================================
       // PolConc - Policy Concentration Information
       //===================================================================
     d PolConc         pr              n

       //===================================================================
       // PolPend - Policy Issuance Pending
       //===================================================================
     d PolPend         pr              n

       //===================================================================
       // NQteSts - New Business Quote Status
       //===================================================================
     d NQteSts         pr              n

       //===================================================================
       // RQteSts - Renewal Business Quote Status
       //===================================================================
     d RQteSts         pr              n

       //===================================================================
       // ProcQteSts - Process for Quote Status
       //===================================================================
     d ProcQteSts      pr              n
     d inType                         1    const

       //===================================================================
       // NewClaim - New Claim Entry
       //===================================================================
     d NewCLaim        pr              n

       //===================================================================
       // ClmChg01 - Claimant Name or Address Change
       //===================================================================
     d ClmChg01        pr              n

       //===================================================================
       // ClmChg02 - Indemnity/Medical/Record Only Status Change
       //===================================================================
     d ClmChg02        pr              n

       //===================================================================
       // ClmChg03 - Claim Indicators
       //===================================================================
     d ClmChg03        pr              n

       //===================================================================
       // ClmChg04 - Claim Status
       //===================================================================
     d ClmChg04        pr              n

       //===================================================================
       // LowRsv - Low Reserves
       //===================================================================
     d LowRsv          pr              n

       //===================================================================
       // LargeRsv - Large Reserve Change
       //===================================================================
     d LargeRsv        pr              n

       //===================================================================
       // LargePmt - Large Payment Change
       //===================================================================
     d LargePmt        pr              n

       //===================================================================
       // Payment - Payment
       //===================================================================
     d Payment         pr              n

       //===================================================================
       // ClmNCCI - NCCI Injury Code
       //===================================================================
     d ClmNCCI         pr              n

       //===================================================================
       // MultiEmp - Multiple Employee Incident
       //===================================================================
     d MultiEmp        pr              n

       //===================================================================
       // ClmNPC - Nature / Part / Cause Code Trigger
       //===================================================================
     d ClmNPC          pr              n

       //===================================================================
       // QteEntry - Quote Entry
       //===================================================================
     d QteEntry        pr              n

       //===================================================================
       // QteRvw - Quote Review
       //===================================================================
     d QteRvw          pr              n

       //===================================================================
       // TotalInc - Total Incurred
       //===================================================================
     d TotalInc        pr              n

       //***  Begin Rnm  ***  05/22/19  *********************************
       //***  Begin Add  ***  03/31/17  *********************************
       //===================================================================
       // Check for trigger existence
       //===================================================================
     dTriggerExistsDeleted...
     d                 pr             1n
     d inCo#                          3  0
     d inName                        10
     d inSeq#                         5  0
       //***  End   Add  ***  03/31/17  *********************************
       //***  End   Rnm  ***  05/22/19  *********************************

       //===================================================================
       // SetStRptDv - Set State Reporting division
       //===================================================================
     d SetStRptDv      pr

       //===================================================================
       // PolPanel - Panel Request to Provider Relations for New Business
       //===================================================================
     d PolPanel        pr              n

       //===================================================================
       // ClsOvrRd - Close overridden file
       //===================================================================
     d ClsOvrRd        pr
     d inFile                        10    const
     d inQry                        255    options(*nopass)

     P* --------------------------------------------------
     P* Build QADSPFFD
     P* --------------------------------------------------
     DBuildFFD         pr
     D inLib                         10
     D inFile                        10

     P* --------------------------------------------------
     P* Check for the existence of QADSPFFD
     P* --------------------------------------------------
     DCheckFFDExists   pr              n

       //***  Begin Rnm  ***  05/22/19  ***********************************
     P* --------------------------------------------------
     P* Get field size
     P* --------------------------------------------------
     DCreateFileTrgDeleted...
     D                 pr             1n
     D inTrgName                    128
       //***  End   Rnm  ***  05/22/19  ***********************************

       //***  Begin Rnm  ***  05/22/19  *********************************
     P***  Begin Add  ***  09/06/17  **************************************
     P* --------------------------------------------------
     P* Drop file triggers
     P* --------------------------------------------------
     DDropFileTrgDeleted...
     D                 pr             1n
     D inTrgName                    128
     P***  End   Add  ***  09/06/17  **************************************
       //***  End   Rnm  ***  05/22/19  *********************************

     P* --------------------------------------------------
     P* Get trigger name
     P* --------------------------------------------------
     DGetTriggerName   pr           128
     D inLib                         10
     D inFile                        10
     D inEvent                        8

      *** beg add ***** 04/10/17 ****************
       //----------------------------------------
       // ExpModDate
       //----------------------------------------
     d ExpModDate      pr              n
       //----------------------------------------
       // PolAddrChg
       //----------------------------------------
     d PolAddrChg      pr              n
      *** end add ***** 04/10/17 ****************
      *** Begin Add *** 05/13/21 *******************************************
      //===================================================
      // Process Quote 3rd Party Payment
      //===================================================
     d Qte3PrtPmt      pr              n
      //===================================================
      // Process 3rd Party Payment Dispute
      //===================================================
     d TPrtPmtDis      pr              n
      *** End   Add *** 05/13/21 *******************************************
      //===================================================
      // Process BIF Failure
      //===================================================
     d BifFailure      pr              n
      //*** Beg Add ***** 09/03/24 *************************
      //===================================================
      // Process IVC Name/Address Failure
      //===================================================
     d IVCNamAddr      pr              n
      //*** End Add ***** 09/03/24 *************************
