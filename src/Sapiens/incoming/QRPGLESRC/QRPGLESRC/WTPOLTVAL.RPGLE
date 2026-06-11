     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL')
     h**********************************************************************
     h*
     h*  Description: Toolbar action validation - perform edits and return error messages.
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: Action Panel buttons via Javascript
     h*  Calls to:
     h*
     h*  Created on 10/17/2012 by C:065 for Project Transform - WC2116
     h**********************************************************************
      * Revisions:
    *                                                               *
      *  08/01/24 C:291 Project: WC2976                               *
      *                 Add Audit Reversal.                           *
      *                                                               *
      *  03/28/23 C:62  Support: 108776(150)                          *
      *                 Fix deletion by extracting the modes correctly*
    *                                                               *
      *  07/01/21 C:33  Support: 96462                                *
      *                 Implement standard employer deletion routine. *
      *
      * 03/16/20 C:180 Project WC2700                                 *
      *                Allow non-renewal cancellation override        *
      *   !!!!!!!!!!!! Do not EBF this code without the rest !!!!!!!!!!!!!!!
      *   !!!!!!!!!!!! of the project. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      *
      * 01/29/20 C:136 Project: WC2187
      *                Check for policy lock before allowing
      *                Add or Delete Division to process.
      *||||||| Include all objects of this project with any EBF ||||||||||||||
      *
      * 01/20/20 C:180 Project: 150WEH                                *
      *                Donot allow Audit with SR payrolls             *
      *                for estimating accounts.                       *

      * 12/16/19 C:180 Project: 150WEH                                *
      *                Add new option Audit with SR payrolls          *
      *                                                               *
      * 12/18/18 C:33  Project: SY406                                 *
      *                Allow for different 'lockout' processes        *
     h*
     h* 08/08/18 C:062 Project: 150WCS
     h*                Remove estimated billing edits.
     h*                Remove audited billing edits.
     h*
     h* 08/01/18 C:267 Support: 64666   (project 150WBU)
     h*               -Check cancellation date prior to execute error message
     h*                for audit bill for SUNZ.
     h*
     H* 06/20/18 C:291 Project: WC2600
     h*                Department Letter edits are now performed in WTPOPUP.
     h*
     h*  10/13/17 C:65  Project: 150WBU
     h*                 For client 150, only allow audit bill to process if
   h*                 entire policy is expired.  All divisions must have
     h*                 audited payroll as well.
     h*
     h* 04/05/17 C:136 Support: 46576
     h*               -Sync up Cert Holders inAction vbg with WTPOPUP's verbiage
     h*               -Remove WCFL03 as a condition for processing Dept Letters
     h*
     h* 04/04/17 C:136 Support: 46414
     h*                WC2436 changed the inAction for 'Renew' to 'Renew Policy'
     h*
     H* 11/04/16 C:180 Support: 43803
     h*                Add security to Dept Letters.
     h*
     h* 03/31/16 C:308 Project: Elevate
     h*          C:33  Genericize the toolbar programming
   h*
   h* 03/17/16 C:136 PhaseWare: 40249
   h*                Correct inData for Delete Employer function so that
   h*                edits work properly when deleting all divisions
   h*
   h* 06/23/15 C:136 Project: WC2182
   h*                Validate Single Employer Renewal
   h*
   h* 01/12/15 C:283 RedMine 4885
   h*                Pass an additional parameter to W0050R04 to
   h*                check if all divisions are requested to be
   h*                deleted.
   h*
   h* 08/08/14 C:290 Support WC099 86393
   h*                Modified the message appearing when a quote
   h*                is currently being worked in FastQuote and
   h*                CompSuite tries to open that for processing.
     h*
     h* 02/06/14 C:279 Project: 129WSB  Priss#: 27
     h*                Add validation to 'Revised' and 'Audit' procees
     h*                for EFT only.
     h*
     h* 11/22/13 C:279 Project: 129WSB
     h*                Add validation to 'Estimated' procees for EFT
     h*                only.
     h*
     h* 10/07/13 C:190 Project: 25WSS
     h*                Add 'Campus Code' to toolbar selections.
     h*
     h* 04/29/13 C:136 Project: SY307
     h*                Add application security to toolbar selections
     h*
     h**********************************************************************
     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)
     FWDELL1    IF   E           K DISK    rename(PEL01:PEL01)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     f                                     infds(dbfds)
     FWMEPL1    IF   E           K DISK    rename(PEP01:PEP01)
     FWDEHL6    IF   E           K DISK    rename(PEH01:PEH06)
     FWDEHL13   IF   E           K DISK    rename(PEH01:PEH13)
     FWMEFL6    IF   E           K DISK    rename(PEF01:PEF06)
     FWMDBL3    IF   E           K DISK    rename(PDB01:PDB03)
     FWMEXL     IF   E           K DISK    rename(PEX01:PEX00)
     FWMRSL     IF   E           K DISK    rename(PRS01:PRS00)
     FWDRRL     IF   E           K DISK    rename(PRR01:PRR00)
     FWMRFL     IF   E           K DISK    rename(RFP01:PRF00)
     FWMGCL     IF   E           K DISK    rename(PGC01:PGC00)
     FWMRPL4    IF   E           K DISK    rename(PRP01:PRP04)
     FWMUPL     IF   E           K DISK    rename(PUP01:PUP00)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      //*** Beg Add ***** 08/01/24 *******************************
     FSDACL     IF   E           K DISK    Rename(PAC01:PAC00)
      //*** End Add ***** 08/01/24 *******************************
      *** beg add ***** 01/29/20 *************************
      /copy sPrLock
      *** beg add ***** 01/29/20 *************************
      /copy sPrDate
     d***  Begin Add  ***  07/01/21  ********************************
      /copy sPrJson
     d***  End   Add  ***  07/01/21  ********************************
      /copy sPrAtrium
      /copy sPrSecure
      /copy wprBank
      /copy wprEft
      /copy wprEmplr
     d ChkAppSecurity  pr
     d  inAction                     20

     dExtractParm      pr                  like(inData)
     d inParm                              like(inData) const
     d inDelimitor                    1                 const
     d inStart                        5  0

     d aCTxRef         s             25    Dim(25) Ctdata Perrcd(1)
     d oFDxRef         s             13    Dim(25) Alt(aCTxRef)

     d dbfds           ds
     d  dbrrn                397    400b 0

      * *ENTRY
     DPolicyToolVal    pr                  ExtPgm('WTPOLTVAL')
     D                                9  0
     D                              512
     D                            32767
     D                                1
      *
     DPolicyToolVal    pi
     D inRRN                          9  0
     D inData                       512
     D outJSON                    32767
     D rtnCode                        1
      *
     D PSDS          ESDS                  extname(SZQ1P)
      *
     D*                DS
     D* data                   1    512
     D* inAction               1     20
     D* inEnv                 21     21
     D* inMode                22    277
     D                 DS
     D dsData                  1    512
     D  dsAction               1     20
     d***  Begin Chg  ***  07/01/21  ********************************
     D**dsEnv***              21     21
     D**dsMode**              22    277
     D*** Begin Chg *** 03/28/23 ************************************
     D**dsEnv***              22     22
     D  dsEnv                 21     21
     D*** End   Chg *** 03/28/23 ************************************
     D  dsMode                23    277
     d***  End   Chg  ***  07/01/21  ********************************

     d***  Begin Add  ***  07/01/21  ********************************
     d node0           s                   Like(yajl_val)
     d i0              s             10i 0 inz
     d key             s             50a   varying
     d val             s                   Like(yajl_val)
     d***  End   Add  ***  07/01/21  ********************************
     D*
     d xxDate          s              7  0
     d message         s            200
     d outCo#          s              3
     d outFnd#         s              3
     d outEftType      s              1    inz('E')
     d outEftKey       s             30    inz
     d outDate         s              7
     d outPreNote      s              1
     d***  Begin Add  ***  12/18/18  ***********************************
     d strApp          s             25    inz
     d***  End   Add  ***  12/18/18  ***********************************
     d ##key#          s             10
     d xxMode          s              1  0
     d intStart        s              5  0 inz(1)
     d inAction        S             20    Inz
     d inEnv           S              1    Inz
     d inMode          S            278    Inz
     d***  Begin Add  ***  07/01/21  ********************************
     D ioJson          s               a   len(1000) varying
     d strErrorMsg     s            500a   varying
     d***  End   Add  ***  07/01/21  ********************************
      *** beg add ***** 01/29/20 *************************
     d strJobInfo      s             28
     d strProcess      s             10
     d strProcess25    s             25
     d strUser         s             50
      *** end add ***** 01/29/20 *************************
      *** Begin Add *** 03/16/20 ****************************************
     d outMode         s              3    Inz('BIL')
      *** End   Add *** 03/16/20 ****************************************
      //*** Beg Add ***** 08/01/24 *******************************
     d strKey          s             10
      //*** End Add ***** 08/01/24 *******************************
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     d***  Begin Add  ***  07/01/21  ********************************
      /include yajl_h
     d***  End   Add  ***  07/01/21  ********************************
     D*
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     keySWCPF      KLIST
     C                   KFLD                    XXSYK             2
      *
      * Table File Key List
      *
     C     keyWTABL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    xxKey            10
     C*
     C*  KEY LIST FOR ADMIN/DARTS FILE
     C*
     C     keyWMEXL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      *
      * Employer Master File Key List
      *
     C     keyWMEML      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      *
     C     key1WMEML     KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      * Employer Detail File Key List
      *
     C     keyWDELL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    xxDiv
     C                   KFLD                    ELFYR
      *
     C     keyWDELL1     KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
      *
      * Employer History File Key List
      *
     C     keyWDEHL6     KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    xxDiv
     C                   KFLD                    ELFYR
      *
     C     keyWDEHL13    KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    xxDATE
      *
     C     key1WDEHL13   KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
      *
      * Employer Payroll File Key List
      *
     C     keyWMEPL1     KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDiv
     C*
     C*  GROUP CARRIER INFORMATION FILE
     C*
     C     keyWMGCL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  CARRIER/STATE REPORTING MASTER FILE
     C*
     C     keyWMRPL4     KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      * ACH Master file
      *
     C     keyWMEFL6     KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
      *
      * Direct Debit File
      *
     C     keyWMDBL3     KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
     C                   KFLD                    xxDATE
      *
     C     key1WMDBL3    KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
      *
      * Rate Set Code File Key List
      *
     C     keyWMRSL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      * Statistical Reporting/Rating File Key List
      *
     C     keyWDRRL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    xxST              2
     C                   KFLD                    ELPFDT
      *
     C     key1WDRRL     KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    xxST
      *
      *  Scheduled Debit/Credit Form
      *
     C     keyWMRFL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     c*
     c* Retrieve client ID
     c*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C                   MOVEL     'WC'          XXSYK
     C     keySWCPF      CHAIN     SWC01                              60
      * If Group allows Payroll Vendor or ACH, set flags on.
     C                   MOVE      *OFF          VNDRFL            1
     C                   MOVE      *OFF          ACHFLG            1
     C     WCFL23        IFEQ      'Y'
     C                   MOVE      *ON           VNDRFL
     C                   ENDIF
     C     WCFL24        IFEQ      'Y'
     C                   MOVE      *ON           ACHFLG
     C                   ENDIF
    C*
     C                   MOVEL(P)  'WA073R'      XXPGM            10
     C                   MOVEL     '*PGM'        XXTYP             7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM
     C                   PARM                    XXTYP
     C                   PARM                    XXNACH            1
      /Free

       inAction = ExtractParm(inData:',':intStart);
       inEnv    = ExtractParm(inData:',':intStart);
       inMode   = ExtractParm(inData:',':intStart);
       // Check to see if not comma delimited
       If inAction = *blanks;
          dsData = inData;
          inAction = dsAction;
          inEnv    = dsEnv;
          inMode   = dsMode;
       Endif;

       Chain(n) inRRN PEL0P;

       Chain keyWMEXL PEX00;

       outJSON = ' ';
       rtnCode = ' ';
       message = ' ';

       ChkAppSecurity(inAction);

       Select;

       When inAction = 'Policy Page';
          Exsr PolicyPage;

       When inAction = 'Department Letters';
          Exsr DepLetters;

       When inAction = 'Premium Notice';
            Exsr PrmNotice;

       When inAction = 'Certificate Holders';
            Exsr CertHolders;

       When inAction = 'Revised Bill';
            Exsr Revised;

       When inAction = 'Audit Request';
            Exsr AuditReq;

       When inAction = 'Endorsements';
            Exsr Endorsements;

       When inAction = 'Quote Transfer';
            Exsr QuoteTransfer;

       When inAction = 'Delete Employer';
            Exsr DeleteEmp;

       When inAction = 'Delete Division';
            Exsr DeleteEmp;

       When inAction = 'Campus';
            Exsr CampusCodes;

       When inAction = 'Renew Policy';
            Exsr Renewal;

       //*** Begin Add *** 12/16/19 ********************************************
       When inAction = 'Audit with S/R';
            Exsr AuditWithSRPayrolls;
       //*** End   Add *** 12/16/19 ********************************************

       //*** Beg Add ***** 08/01/24 ********************************************
       When inAction = 'Audit Reversal';
            Exsr AuditReversal;
       //*** End Add ***** 08/01/24 ********************************************


       Endsl;

       BuildJSON(outJSON :'errorMsg' : %trim(message));
       Return;
      /End-Free

     C*===============================================================
     C* PolicyPage - Edits for Policy page action
     C*===============================================================
     C*
     C     PolicyPage    BEGSR
      *
      * Option not available for claims only systems.
      *
     C     WCFL08        IFEQ      'C'
     C     CLTID         ANDNE     '025'
     C                   eval      message = 'Policy Page is not avail'+
     C                                       'able, system is set for claims '+
     C                                       'only.'
     C                   LeaveSR
     C                   ENDIF                                                  WCFL08 IFEQ
     C*
     C                   Z-ADD     ELDIV         xxDiv             5 0
     C*
     C     keyWMEML      Chain     PEM00                              60
     C     *in60         Doweq     *OFF
     C*
     C     EMSTRP        Ifeq      'Y'
     C                   Z-add     EMDiv         xxDiv
     C                   Leave
     C                   Endif
     C*
     C     keyWMEML      Reade     PEM00                                  60
     C                   Enddo
     C*
     C* Exit with error if no divisions flagged as state reporting.
     C*
     C                   If        *in60 = *on
     C                   eval      message = 'No Division is flagged as '+
     C                                       'state reporting.'
     C                   LeaveSR
     C                   Else
     C     keyWDELL      Chain     PEL00                              60
     C                   Endif
     C*
     C                   Z-ADD     0             XXCXDT            7 0
     C                   Z-ADD     1             I1                2 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C     CX(I1)        ANDNE     *ZERO
     C                   Z-ADD     CX(I1)        XXCXDT
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C* Verify a bill has been run through employer history.
     C*
     C                   MOVE      *BLANK        XXBILL            1
     C     keyWDEHL6     CHAIN     PEH06                              60
     C     *IN60         IFEQ      *OFF
     C     EHTRDT        ANDNE     *ZERO
     C                   MOVE      'Y'           XXBILL
     C                   ENDIF
     C*
     C* IF CLIENT 25 AND FINAL AUDIT FLAG IS NOT BLANK THEN CONSIDERED
     C* BILL.
     C*
     C     CLTID         IFEQ      '025'
     C     ELFAF         ANDNE     *BLANKS
     C                   MOVE      'Y'           XXBILL
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        XXFLAT            1
     C     XXCXDT        IFNE      *ZERO
     C     XXCXDT        ANDLE     ELPFDT
     C                   MOVE      'Y'           XXFLAT
     C                   ENDIF
     C*
     C* Exit with no errors if pass all conditions.
     C*
     C     ELPFDT        IFNE      ELPTDT
     C     XXBILL        ANDNE     *BLANK
     C     XXFLAT        ANDEQ     *BLANK
     C*
      * Opt 17 (Policy Page) is not allowed when either the Scheduled
      * DB/CR form is incomplete or it has not printed yet, and if is
      * client 114.
     C*
     C     WCFL53        IFEQ      'Y'
     C*
     C     keyWMRSL      CHAIN     PRS00                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        xxST
      *
     C     keyWDRRL      SETGT     PRR00
     C     key1WDRRL     READPE    PRR00                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     RRDBCR        ANDEQ     'Y'
     C     ELDBCR        ANDNE     *ZEROS
     C     ELDBCR        ANDNE     1
      *
     C     keyWMRFL      CHAIN     PRF00                              60
      *
      * Scheduled DB/CR form info in Employer detail is incomplete
      *
     C     *IN60         IFEQ      *ON
     C                   eval      message = 'The Schedule DB/CR form informat'+
     C                                       'ion in policy detail is incomple'+
     C                                       'te.  The information must be com'+
     C                                       'pleted before a policy page can '+
     C                                       'be released.'
     C                   LeaveSR
     C                   ENDIF                                                  RFKY
      *
      * Scheduled DB/CR form MUST BE PRINTED
      *
     C     *IN60         IFEQ      *OFF
     C     RFPRTF        ANDNE     'Y'
     C                   eval      message = 'The Schedule DB/CR form must be '+
     C                                       'printed prior to requesting a po'+
     C                                       'licy page.'
     C                   LeaveSR
     C                   ENDIF                                                  RFPRTF <> Y
      *
     C     *IN60         IFEQ      *OFF
      *
      * Check for Missing Reasons-DB/CR form info incomplete
      *
     C                   Move      *off          *in17
      *
     C                   SELECT
     C     RFPRCA        WHENGT    *ZEROS
     C     RFPRCA        ANDNE     1
     C     RFPRRS        ANDEQ     *BLANKS
     C                   SETON                                        17
     C     RFCPCA        WHENGT    *ZEROS
     C     RFCPCA        ANDNE     1
     C     RFCPRS        ANDEQ     *BLANKS
     C                   SETON                                        17
     C     RFMFCA        WHENGT    *ZEROS
     C     RFMFCA        ANDNE     1
     C     RFMFRS        ANDEQ     *BLANKS
     C                   SETON                                        17
     C     RFSDCA        WHENGT    *ZEROS
     C     RFSDCA        ANDNE     1
     C     RFSDRS        ANDEQ     *BLANKS
     C                   SETON                                        17
     C     RFEMCA        WHENGT    *ZEROS
     C     RFEMCA        ANDNE     1
     C     RFEMRS        ANDEQ     *BLANKS
     C                   SETON                                        17
     C     RFMCCA        WHENGT    *ZEROS
     C     RFMCCA        ANDNE     1
     C     RFMCRS        ANDEQ     *BLANKS
     C                   SETON                                        17
     C     RFMSCA        WHENGT    *ZEROS
     C     RFMSCA        ANDNE     1
     C     RFMSRS        ANDEQ     *BLANKS
     C                   SETON                                        17
     C                   ENDSL
      *
     C                   If        *in17 = *on
     C                   eval      message = 'The Schedule DB/CR form informat'+
     C                                       'ion in policy detail is incomple'+
     C                                       'te.  The information must be com'+
     C                                       'pleted before a policy page can '+
     C                                       'be released.'
     C                   LeaveSR
     C                   Endif
      *
     C                   ENDIF                                                  60 = OFF
      *
      *  Check sum of WMRFP BD/CR fields = SCBDCR
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     *ZEROS        ZZDBCD            5 2
     C                   Z-ADD     *ZEROS        ZZDBCC            5 2
     C                   Z-ADD     *ZEROS        ZZTOTC            5 4
     C                   Z-ADD     *ZEROS        ZZTOTD            5 4
     C                   Z-ADD     *ZEROS        ZZDBCR            5 4
      *
     C                   ADD       RFPRCA        ZZDBCC
     C                   ADD       RFPRDA        ZZDBCD
     C                   ADD       RFCPCA        ZZDBCC
     C                   ADD       RFCPDA        ZZDBCD
     C                   ADD       RFMFCA        ZZDBCC
     C                   ADD       RFMFDA        ZZDBCD
     C                   ADD       RFSDCA        ZZDBCC
     C                   ADD       RFSDDA        ZZDBCD
     C                   ADD       RFEMCA        ZZDBCC
     C                   ADD       RFEMDA        ZZDBCD
     C                   ADD       RFMCCA        ZZDBCC
     C                   ADD       RFMCDA        ZZDBCD
     C                   ADD       RFMSCA        ZZDBCC
     C                   ADD       RFMSDA        ZZDBCD
      *
     C     ZZDBCC        IFNE      *ZEROS
     C     ZZDBCC        MULT      -1            ZZDBCC
     C     ZZDBCC        DIV       100           ZZTOTC                         Total Credits
     C                   ENDIF
      *
     C     ZZDBCD        IFNE      *ZEROS
     C     ZZDBCD        DIV       100           ZZTOTD                         Total Debits
     C                   ENDIF
      *
     C     ZZTOTC        IFNE      *ZEROS
     C     ZZTOTD        ORNE      *ZEROS
     C     ZZTOTC        ADD       ZZTOTD        ZZDBCR
     C     ZZDBCR        ADD       1.000         ZZDBCR
     C                   ENDIF
      *
     C     ELDBCR        IFNE      ZZDBCR
     C                   eval      message = 'The sum of the DB/CR percentages'+
     C                                       ' in the rating form do not equal'+
     C                                       ' the Schedule DB/CR factor. The '+
     C                                       'sum must match before a policy p'+
     C                                       'age can be released.'
     C                   LeaveSR
     C                   ENDIF                                                  SCDBCR<>ZZDBCR
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  RRDBCR <> "Y"
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  WCFL53
     C*
     C                   ELSE
     C*
     C     XXBILL        IFNE      *BLANK
     C                   eval      message = 'Unable to generate a policy '+
     C                                       'page because this policy is '+
     C                                       'cancelled flat.'
     C                   ELSE
     C                   eval      message = 'Unable to generate a policy '+
     C                                       'page because this policy has '+
     C                                       'not yet been billed.'
     C                   ENDIF                                                  ELFAF IFNE *BLANK
     C*
     C                   LeaveSR
     C*
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C*
     C                   ENDSR                                                  PolicyPage Subr

     C*===============================================================
     C* Revised - Edits for Revised Bill action.
     C*===============================================================
     C     Revised       BEGSR
      *
      *  Do not allow billing if employer is audited.
      *
     C     ELFAF         IFEQ      'A'
     C                   eval      message = 'Revised bill is not allowed '+
     C                                       'once the policy year has '+
     C                                       'been audited.'
     C                   LeaveSR
     C                   ENDIF
     C*
     C                   eval      xxDate = DateTo7(SystemDate)
     C*
     C     key1WMEML     Chain     PEM00
     C*
     C                   Z-ADD     0             xxCXDT            7 0
     C                   Z-ADD     1             I1                2 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C     CX(I1)        ANDNE     *ZERO
     C                   Z-ADD     CX(I1)        xxCXDT
     C                   ENDIF                                                  I1 IFNE 0
      /Free
       //*** Begin Add *** 03/16/20 ************************************
       If xxCxDt <> 0 And
        OvrRenewalCx(outMode:ELCo#:
        ELFnd:ELEmp#:ELdiv:ELfyr:xxCxDt) = *On;
          xxCxDt = *Zeros;
       Endif;
       //*** End   Add *** 03/16/20 ************************************
      /End-Free
      *
      *  Do not allow billing if employer cancelled
      *
     C     xxCXDT        IFNE      *ZERO
     C                   eval      message = 'Revised bill is not allowed '+
     C                                       'on a cancelled policy.'
     C                   LeaveSR
     C                   ENDIF
     C*
     C* ONLY ALLOW BILL TO BE RUN IF A CARRIER POLICY # IS ENTERED
     C* FOR ALL DIVISIONS
     C*
     C                   Exsr      Carrier#
     C*
     C                   If        message <> *blanks
     C                   LeaveSR
     C                   Endif
     C*
      /free
       Exsr ChkEFT;
      /end-free
     C                   ENDSR                                                  Revised Subr

     C*===============================================================
     C* QuoteTransfer - Edits for Quote Transfer action.
     C*===============================================================
     C*
     C     QuoteTransfer BEGSR
      *
      *  Check for Batch process running
      *
     C                   CALL      'WA044R'
     C                   PARM      '*'           #1RTCD            1
      *
     C     #1RTCD        IFEQ      'F'
     C                   eval      message = 'Quote Transfer cannot be selecte'+
     C                                       'd at this time; the batch proces'+
     C                                       's is currently running.'
     C                   LeaveSR
     C                   ENDIF                                                  #1RTCD
      *
      *  IF Employer record locked by FASTQUOTE.
      *
      //***  Begin Chg  ***  12/18/18  ************************************
     C*****EMFQLK        IFEQ      'Y'
      /Free
        If GetFQLockSts(EMco#:EMfnd:EMemp#:EMdiv:inEnv:*off:strApp) = *on;
      /End-free
      //***  End   Chg  ***  12/18/18  ************************************
      *
      *     Do not allow options
      *
      /free
      //***  Begin Chg  ***  12/18/18  ************************************
      //**********       message = 'FastQuote user has the record locked,' +
      //**********                 ' quote transfer is not allowed until up' +
      //**********                 'date completes.';
           message = %trim(strApp) + ' user has the record locked, quote ' +
            'transfer is not allowed until update completes.';
      //***  End   Chg  ***  12/18/18  ************************************
      /end-free
     C                   LeaveSR
      *
     C                   ENDIF                                                  EMFQLK
      *
      *  IF Employer record locked by FASTQUOTE,
      *     Do not allow option
      *
     C     keyWMEML      CHAIN     PEM00                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EMFQLK        IFEQ      'Y'
      /free
                         message = 'FastQuote user has the record locked,' +
                                   ' quote transfer is not allowed until up' +
                                   'date completes.';
      /end-free
     C                   LeaveSR
     C                   ENDIF
      *
     C     keyWMEML      READE     PEM00                                  60
     C                   ENDDO
      *
      * Reset employer master.
      *
     C     key1WMEML     CHAIN     PEM00                              60
      *
     C                   Move      EMCO#         ##CO#
     C                   Move      *blank        xxAuth            1
      *
     C                   CALL      'SA008R'
     C                   PARM                    ##CO#             3 0
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
      *
     C     ##RCDS        IFEQ      *ON
     C*
     C                   CALL      'SA008R'
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'W'           ##APPL
     C                   PARM      *OFF          ##RTCD            1
     C     xxAUTH        PARM                    ##AUTH            1
     C                   PARM      EMFND         ##FND             3 0
     C                   PARM      EMEMP#        ##EMP             9 0
      *
     C     xxAUTH        IFEQ      'I'
     C                   eval      message = 'Quote transfer is not available.'+
     C                                       '  User profile is only authorize'+
     C                                       'd to use non-update actions.'
     C                   LeaveSR
     C                   ENDIF
      *
     C                   ENDIF                                                  ##RTCD=*ON
      *
     C                   ENDSR                                                  QuoteTransfer Subr

     C*===============================================================
     C* DepLetters - Edits for Department Letters action.
     C*===============================================================
     C*
     C     DepLetters    BEGSR

      /Free
      // *** Beg Add ***** 06/20/18 **************************************
      //This Ajax program is not getting called by Javascript for Documents
      //action from Policy Toolbar. Following edit for Inquiry mode is now
      //performed in WTPOPUP with additional conditions applied under project WC2600.
      // *** End Add ***** 06/20/18 **************************************
       If inMode = 'D';
             message='Option not available in Inquiry mode.';
       Endif;
      /End-Free

     C                   ENDSR                                                  DepLetters Subr
     C*===============================================================
     C* PrmNotice - Edits for Premium Notices action.
     C*===============================================================
     C*
     C     PrmNotice     BEGSR
     C*
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         ##CO#             3 0
     C                   PARM      ELFND         ##FND             3 0
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C                   PARM      *ZERO         ##AREA            3 0
     C                   PARM      *ZERO         ##PHON            7 0
     C                   PARM      '*'           ##PLAN            1
     C                   PARM      *ZERO         ##DATE            8 0
      *
      *  Disable opt "32=Premium Notice" if employer is Account Current.
      *
     C     ##PLAN        IFEQ      'A'
     C                   eval      message = 'Premium notices cannot be run fo'+
     C                                       'r Account Current policies.'
     C                   LeaveSR
     C                   ENDIF                                                  ##PLAN IFEQ
      *
     C                   ENDSR                                                  PrmNotice Subr

     C*===============================================================
     C* CertHolders - Edits for Certificate Holders action.
     C*===============================================================
     C*
     C     CertHolders   BEGSR
      *
      * Option not available for claims only systems.
      *
     C     WCFL08        IFEQ      'C'
     C                   eval      message = 'Certificate Holders is not avail'+
     C                                       'able, system is set for claims '+
     C                                       'only.'
     C                   LeaveSR
     C                   ENDIF                                                  WCFL08 IFEQ
      *
     C                   ENDSR                                                  CertHolders Subr

     C*===============================================================
     C* AuditReq - Edits for Audit Request action.
     C*===============================================================
     C*
     C     AuditReq      BEGSR
      *
      * Option not available for claims only systems.
      *
     C     WCFL08        IFEQ      'C'
     C                   eval      message = 'Audit Request is not avail'+
     C                                       'able, system is set for claims '+
     C                                       'only.'
     C                   LeaveSR
     C                   ENDIF                                                  WCFL08 IFEQ

     C                   ENDSR                                                  AuditReq Subr

     C*===============================================================
     C* Endorsements - Edits for Endorsements action.
     C*===============================================================
     C*
     C     Endorsements  BEGSR
      *
      * Option not available for claims only systems.
      *
     C     WCFL08        IFEQ      'C'
     C                   eval      message = 'Endorsements is not avail'+
     C                                       'able, system is set for claims '+
     C                                       'only.'
     C                   LeaveSR
     C                   ENDIF                                                  WCFL08 IFEQ

     C     keyWMEML      Chain     PEM00                              60
     C     *in60         Doweq     *OFF
     C*
     C     EMSTRP        Ifeq      'Y'
     C                   Leave
     C                   Endif
     C*
     C     keyWMEML      Reade     PEM00                                  60
     C                   Enddo
     C*
     C* Exit with error if no divisions flagged as state reporting.
     C*
     C                   If        *in60 = *on
     C                   eval      message = 'No Division is flagged as '+
     C                                       'state reporting.'
     C                   LeaveSR
     C                   Endif
     C*
     C                   ENDSR                                                  Endorsements Subr

     C*===============================================================
     C* DeleteEmp -  Edits for Employer Deletion
     C*===============================================================
     C     DeleteEmp     BEGSR

      *** beg add ***** 01/29/20 *********************************
      /Free
       If GetPolicyLockStatus(ELco#:ELfnd:ELemp#:strJobInfo:
        strProcess:strUser) = *On;
          If strProcess = 'REVBILLING';
             strProcess25 = 'Revised Billing';
             message = 'This operation is available when ' +
              %Trim(strProcess25) + ' Job(' + %Trim(strJobInfo) +
              ') has completed for ' + %Trim(strUser) + '.';
             LeaveSr;
          Endif;
       Endif;
      /End-Free
      *** end add ***** 01/29/20 *********************************

     C     key1WMEML     Chain     PEM00
     C                   If        %found(WMEML)
     C*
      //***  Begin Chg  ***  07/01/21  ******************
      **********
     C**********         Clear                   ##Qte             1
     C**********         If        inEnv = 'Q'
     C**********         Eval      ##Qte = 'Q'
     C**********         Else
     C**********         Eval      ##Qte = 'W'
     C**********         Endif
      **********
      **********
      *free*****
      *//*Move*the 'all division' mode over. It is being sent
      *//**in*the inEnv position when deleting from live
      *If*inEnv*= '0' or inEnv = '1';
      ****inMode = inEnv;
      ****##Mode = inEnv;
      ****inEnv* = *blank;
      *Else;****
      ****##Mode = inMode;
      *Endif;***
      *end-free*
      **********
     c**********         Call      'W0050R04'
     c**********         Parm      dbrrn         ##Rrn             9 0
     c**********         Parm                    ##Qte
     c**********         Parm      *zeros        ##Ind             2 0
     c**********         Parm                    ##Mode            1
     C**********
     C**Exit*with error if error found
     C**********
     C**********         If        ##Ind = 77
     C**********         eval      message = 'You are not authorized to ' +
     C**********                             'delete this employer.'
     C**********         LeaveSR
     C**********         Endif
     C**********
     C**********         If        ##Ind = 73
     C**********         eval      message = 'Employer not eligible for ' +
     C**********                             'deletion, claim records exist.'
     C**********         LeaveSR
     C**********         Endif
     C**********
     C**********         If        ##Ind = 61
     C**********         eval      message = 'S/R Billings for this division ' +
     C**********                    'exist in the State Reporting Division.'
     C**********         LeaveSR
     C**********         Endif
     C**********
     C**********         If        ##Ind = 72
     C**********         eval      message = 'Employer not eligible for ' +
     C**********                    'deletion; payments have been received.'
     C**********         LeaveSR
     C**********         Endif
     C**********
     C**********         If        ##Ind = 74
     C**********         eval      message = 'Employer not eligible for ' +
     C**********                    'deletion; transactions exist for ' +
     C**********                    'previous month.'
     C**********         LeaveSR
     C**********         Endif
      **********
     C**********         If        ##Ind = 69
     C**********         eval      message = 'Division cannot be deleted ' +
     C**********                    'until A/R summary flag is removed ' +
     C**********                    'and a bill run.'
     C**********         LeaveSR
     C**********         Endif
     C**********
     C**********         If        ##Ind = 81
     C**********         eval      message = 'Deletion invalid.  Data for ' +
     C**********                    'employer has already been ' +
     C**********                    'electronically submitted.'
     C**********         LeaveSR
     C**********         Endif
     C**********
     C**********         If        ##Ind = 82
     C**********         eval      message = 'Deletion invalid.  A non-' +
     C**********                    'transferred A/R Batch record exists.'
     C**********         LeaveSR
     C**********         Endif
     C**********
     C**********         If        ##Ind = 70
     C**********         eval      message = 'WCMC cases still exist on ' +
     C**********                    'this Employer.'
     C**********         LeaveSR
     C**********         Endif
      /Free
       ioJson = '{';
       ioJson += BuildJsonNode('rrn':%char(dbrrn)) + ',';
       ioJson += BuildJsonNode('edit':'*ONLY') + ',';
       if inEnv = 'Q';
          ioJson += BuildJsonNode('quote':'Q') + ',';
       Endif;
       If inEnv = '1';
          ioJson += BuildJsonNode('div':'*ALL') + ',';
          inEnv  = *blank;
       Endif;
       ioJson = %scanrpl(',':'}':ioJson:%len(ioJson)-1);
       If not DeleteEmployer(ioJson);
          node0 = yajl_string_load_tree(ioJson:strErrorMsg);
          Dow YAJL_object_loop(node0:i0:key:val) and strErrorMsg = *blank;
             Select;
             When key = 'ind';
             When key = 'msgId';
             When key = 'msg';
                message = YAJL_get_string(val);
             Endsl;
          Enddo;
          yajl_tree_free(node0);
       Endif;
      /End-free
      //***  End   Chg  ***  07/01/21  ******************
     C*
     C                   Endif
     C*
     C                   ENDSR                                                  DeleteEmp Subr

     C*===============================================================
     C* Carrier # check - Carrier # check for running bills.
     C*===============================================================
     C*
     C     Carrier#      BEGSR
     C*
     C*
     C* ONLY ALLOW BILL TO BE RUN IF A CARRIER POLICY # IS ENTERED
     C* FOR ALL DIVISIONS
     C*
     C     keyWMGCL      CHAIN     PGC00                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     GCCRFL        ANDEQ     'Y'
      *
     C     inEnv         IFNE      'Q'
      *
     C                   MOVE      *BLANKS       RPCAP#
      *
     C     keyWDELL1     SETLL     PEL01
     C     keyWDELL1     READE     PEL01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     keyWMRPL4     CHAIN     PRP04                              61
      *
     C     RPCAP#        IFEQ      *BLANKS
     C     *IN61         OREQ      *ON
     C                   eval      message = 'A bill cannot be submitted unles'+
     C                                       's the carrier policy # has been '+
     C                                      'entered for all divisions.'
     C                   Leave
     C                   ENDIF                                                  RPCAP# = BLANKS
      *
     C     keyWDELL1     READE     PEL01                                  60
     C                   ENDDO                                                  IN60
      *
      /free
       Chain(n) inRRN PEL0P;
      /end-free
      *
     C                   ENDIF                                                  inEnv <> Q
      *
     C                   ENDIF                                                  *IN60, GCCRFL
     C*
     C                   ENDSR                                                  Carrier# Subr

     C*===============================================================
     C* Campus Codes - There are no edits.
     C*===============================================================
     C     CampusCodes   Begsr

     C                   Endsr
      /free
       //============================================================
       // ChkEFT - Check EFT setup.
       //============================================================
       Begsr ChkEFT;

       If inEnv <> 'Q';
          outEftKey = %Editc(ELemp#:'X') + %Editc(ELdiv:'X') + '  ';
          outDate = %Editc(DateTo7(SystemDate):'X');
          outCo#  = %editc(ELco#:'X');
          outFnd# = %editc(ELfnd:'X');
          If not GetEftStatus(outCo#:OutEftType:outFnd#:outEftKey:outDate:
           outPreNote);

             If ELbscd <> *zero;
                ##key# = %char(ELbscd);
                Chain (ELco#:##key#) pbs01;
                If %found and BSeftf = 'Y';
                   message = 'Employer EFT setup is required to run ';

                   Select;
                   When inAction = 'Estimated Bill';
                      message = %trim(message) + ' Estimated';
                   When inAction = 'Revised Bill';
                      message = %trim(message) + ' Revised';
                   When inAction = 'Audited Bill';
                      message = %trim(message) + ' Audit';
                   Endsl;
                   message = %trim(message) + ' bill for this bill plan.';

                   leaveSr;
                Endif;
             Endif;
          Endif;
       Endif;

       Endsr;
       //===============================================================
       // Renewal Subroutine
       //===============================================================
       Begsr Renewal;
       message = 'PolTBar';
       ChkRnwStatus(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:message);
       If message <> *blanks;
          LeaveSr;
       Endif;
       message = 'PolTBar';
       ChkCxStatus(ELco#:ELfnd:ELemp#:ELdiv:message);
       If message <> *blanks;
          LeaveSr;
       Endif;
       message = 'PolTBar';
       ChkFutDtl(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:message);
       If message <> *blanks;
          LeaveSr;
       Endif;
       message = 'PolTBar';
       ChkQteEmp(ELco#:ELfnd:ELemp#:ELdiv:message);
       If message <> *blanks;
          LeaveSr;
       Endif;
       message = 'PolTBar';
       ChkPolDates(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:message);
       If message <> *blanks;
          LeaveSr;
       Endif;
       Endsr;
       //*** Beg Add ***** 08/01/24 ********************************************
       //===============================================================
       // AuditReversal subroutine
       //===============================================================
       Begsr AuditReversal;

       //Check the Security status of the User
       If Not CheckSecLevel('AudReverse');
          message = 'You are not authorized to perform an Audit Reversal.';
          LeaveSR;
       Endif;

       //Check the Audit status of the Policy
       If ELfaf <> 'A';
          message = 'This policy is not audited.';
          LeaveSR;
       Endif;

       //Check if the audit reversal darts remark code is populated
       strKey = 'AudReverse';
       Chain strKey pac00;
       If Not %Found(SDACL) or (%Found(SDACL) and ACRMCD = *Blank);
          message = 'No Remark code has been setup for Audit Reversals ' +
           'in Application Configuration.';
       Endif;

       Endsr;
       //*** End Add ***** 08/01/24 ********************************************

       //*** Begin Add *** 12/16/19 ********************************************
       //===============================================================
       // AuditWithSRPayrolls subroutine
       //===============================================================
       Begsr AuditWithSRPayrolls;
       Setll (ELco#:ELfnd:ELfyr:ELemp#) pep01;
       Reade (ELco#:ELfnd:ELfyr:ELemp#) pep01;

       Dow not %eof(WMEPL1);
          If EPsafl = 'A';
             message = 'Payrolls have been previously audited and cannot be';
             message = %Trim(message) + ' updated from self-reported payrolls.';
             leavesr;
          Endif;
          Reade (ELco#:ELfnd:ELfyr:ELemp#) pep01;
       Enddo;
       //*** End   Add *** 12/16/19 ********************************************

       //*** Begin Add *** 01/20/20 ********************************************
       Setll (ELco#:ELfnd:ELfyr:ELemp#) pel01;
       Reade (ELco#:ELfnd:ELfyr:ELemp#) pel01;
       Dow not %eof(WDELL1);
          ##key# = %Editc(ELbscd:'X');
          Chain (ELco#:##key#) pbs01;
          If %found and BSsefl = 'E';
             message = 'Audit Payrolls for estimating divisions cannot be'  ;
             message = %Trim(message) + ' updated from self-reported payrolls.';
             leavesr;
          Endif;
          Reade (ELco#:ELfnd:ELfyr:ELemp#) pel01;
       Enddo;
       //*** End   Add *** 01/20/20 ********************************************

       Endsr;

      /end-free
     C*===============================================================
      *==================================================================
      * ChkAppSecurity - Compare user and option/function security settings
      *                  and return *on if user is authorized access. The
      *                  default table will translate CompSuite actions to
      *                  old Option/Function Security table.
      *==================================================================
     p ChkAppSecurity  b
     d ChkAppSecurity  pi
     d  inAction                     20

     d  outEnv         s              1    inz('L')
     d  outPgmID       s             10
     d  outType        s              1
     d  outNbr         s              2  0
     d  intIdx         s              2  0
     d  inMessage      s            100
     d  isAllowed      s               n   inz(*on)

      /Free

       If inEnv = 'Q';
          outEnv = 'Q';
       Else;
          outEnv = 'L';
       Endif;

       intIdx = %Lookup(inAction : aCTxRef);
       If intIdx <> 0;
          outPgmID = %subst(oFDxRef(intIdx) : 1 : 10);
          outType  = %subst(oFDxRef(intIdx) : 11 : 1);
          outNbr   = %int(%subst(oFDxRef(intIdx) : 12 : 2));
       Endif;
       isAllowed = CheckOFSec(outPgmID : outType : outNbr :
        outEnv : inMessage);
       If isAllowed = *off;
          message = inMessage;
       Endif;

       // If user is not authorized, blank out inAction so that program
       //  skips to BuildJSON statement.
       If IsAllowed = *off;
          inAction = *blanks;
       Endif;
      /End-Free

     p ChkAppSecurity  e
      *==================================================================
      * Extract entry parms from comma delimited
      *==================================================================
     pExtractParm      b
     dExtractParm      pi                  like(inData)
     d inParm                              like(inData) const
     d inDelimitor                    1                 const
     d inStart                        5  0

     d intLen          s              5  0 inz
     d intPos          s              5  0 inz
     d strParm         s                   like(inData) inz

      /Free
       intPos = %Scan(',':inData:intStart);
       If intPos <= *zero;
          Return *blanks;
       Endif;
       intLen = intPos - intStart;
       strParm = %SubSt(inData:inStart:intLen);
       inStart = intPos + 1;

       Return %trim(strParm);
      /End-free

     pExtractParm      e
** aCTxRef ------------->oFDxRef
Copy                     W0050R    103
Delete Division          W0050R    104
Delete Employer          W0050R    104
Add Division             W0050R    106
Quote Transfer           W0050R    117
Mid Term Endorsements    W0050R    155
Cert Holders             W0051R    112
Policy Page              W0051R    117
Department Letters       W0051R    120
Estimated Bill           W0051R    130
Quote Bill               W0051R    130
Revised Bill             W0051R    131
Premium Notice           W0051R    132
Audit Request            W0051R    133
Audited Bill             W0051R    134
Third Party Billing      W0051R    135
Endorsements             W0051R    154
Mid Term Endorsement     W0050R    155
