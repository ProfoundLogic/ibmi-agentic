     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) bnddir('WTBNDDIR')
     H*================================================================
     H/TITLE W0815R02 - WORK WITH WAIVER HOLDER INFORMATION
     H*================================================================
     H* DESCRIPTION:  This program will allow a user to change, view  *
     H*               or add records in the Waiver Holder file.       *
     H*                                                               *
     H* CALLED FROM: W0815R - Work With Waivers                       *
     H* CALLS TO   : None                                             *
     H*                                                               *
     H* CREATED    : 05/12/99                                         *
     H* PROGRAMMER : C:113                                            *
     H* PROJECT    : CM045                                            *
     H*===============================================================*
     H* Revisions:                                                    *
      *                                                               *
      * 07/31/25 C:300 Support: 117577 (AmWins)                       *
      *                Waiver of Subrogation MTE not attaching WC890600B
      *                Update ENWVFL waiver change flag               *
      *                                                               *
      * 07/08/25 C:349 Support: 119336 (Prescient)                    *
      *                Initialize the EF date array.                  *
      *                                                               *
      * 02/24/25 C:65  Project: Phw#118336  (Carewest)                *
      *                Change the writing of WMENP to write only one  *
      *                record with all of the before and after line   *
      *                descriptions wrapped up into one field each.   *
      *                                                               *
      * 05/10/24 C:079 Ticket: 115133-133                             *
      *                Update WVEMST                                  *
      *                                                               *
      * 08/12/22 C:079 Support: 148/93027                             *
      *                Add new Person or Organization field           *
      *                                                               *
      * 02/02/21 C:136 Project: WC2757                                *
      *               -Default verbiage for blanket waivers.          *
      *               -Don't display waiver detail for TX blanket     *
      *                waivers.                                       *
      *                                                               *
      * 02/12/20 C:136 Support: 80310                                 *
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *!!! ALL THE OBJECTS FOR THIS SUPPORT ITEM MUST GO TOGETHER  !!!*
      *!!! AND FIX80310 MUST BE RUN.                               !!!*
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
      *                Always populate the endorsement form name      *
      *                WVenm.                                         *
      *                                                               *
     H* 04/17/18 C:136 Support: PHW64561                              *
     H*               -Ensure div drop down provides rating state for *
     H*                the waiver record                              *
     H*               -Add excluded states logic to edit division     *
     H*                for the endorsement.                           *
     H*                                                               *
     H* 10/24/17 C:267 Project: C150WBP                               *
     H*                Added waiver form WC990313.                    *
     H*                                                               *
     H* 04/03/16 C:302 Support 39253                                  *
     H*                Add a division drop down to select a different *
     H*                division from the employer's state reporting   *
     H*                division.                                      *
     H*                                                               *
     H* 10/19/14 C:302 Support 86786                                  *
     H*                Added waiver form WC420304B.                   *
     H*                                                               *
     H* 10/02/13 C:287 Project WC2194                                 *
     H*                Added waiver state condition for Texas endors. *
     H*                                                               *
     H* 01/25/11 C:190 SUPPORT 74629                                  *
     H*                For Blanket waiver on policy, changed address  *
     H*                fields to be NOT required.                     *
     H*                                                               *
     H* 01/11/11 C:220 PROJECT WC1936                                 *
     H*                1. Increase array size from 10 to 20 elements  *
     H*                   for XXBL and XXAL                           *
     H*                2. Add labels on endorse cover page to indicate*
     H*                   the change                                  *
     H*                3. Change logic to print all the changed fields*
     H*                   in WOS.                                     *
     H*                4. Changed RPG to RPGLE for better numeric for-*
     H*                   matting.                                    *
     H*                                                               *
     H* 07/29/10 C:190 SUPPORT 73696                                  *
     H*                Corrrected:                                    *
     H*                1. Endorsement not printing while deletion.    *
     H*                2. Changed value not retaining on scnd screen  *
     H*                                                               *
     H* 07/06/10 C:220 SUPPORT 73226                                  *
     H*                Correct the Waiver of subro seq# in add mode.  *
     H*                                                               *
     H* 04/09/10 C:61  SUPPORT WA045 72248                            *
     H*              - Do not write duplicate records to WMENP work   *
     H*                file.  Only one set per waiver write/update.   *
     H*                                                               *
     H* 03/29/10 C:61  SUPPORT WA114 71463                            *
     H*                Correct print of Waiver Name                   *
     H*                                                               *
     H* 12/02/09 C:61  PROJECT 132WMP                                 *
     H*              - Delete "CA Waiver Flat Charge" and             *
     H*                "CA Waiver %" prompts from SCR01 and
     H*                associated fields (SCCPCT, SCCFCG).            *
     H*              - If clt132 and 'S' type waiver then check
     H*                WDRRP/RRFLPC and edit user input accordingly.  *
     H*                                                               *
     H* 11/26/09 C:190 SUPPORT 70785                                  *
     H*                The optional second was not showing up         *
     H*                on the summary page.                           *
     H*                                                               *
     H* 09/02/09 C:61  SUPPORT WA133 69689                            *
     H*                Print project information after Waiver Name    *
     H*                and address on ENDORSE cover page.             *
     H*                                                               *
     H* 08/25/09 C:136 SUPPORT: WA133 69630                           *
     H*                CHANGE INDICATOR USED TO CONDITION DISPLAY     *
     H*                OF WAIVER INFO ON SCR02                        *
     H*                                                               *
     H* 05/03/09 C:61  PROJECT 132WKN PRISS #23                       *
     H*                Do not allow percentage input for CA mid-term  *
     H*                waiver if clt132.                              *
     H*                                                               *
     H* 01/29/09 C:190 PROJECT 136WEB                                 *
     H*                Modify the Waiver Information screen to allow  *
     H*                for entry of up to 100 characters for the      *
     H*                Waiver Name.                                   *
     H*                                                               *
     H* 12/02/08 C:197 PROJECT 132WKN PRISS#20                        *
     H*             - WAIVER OF SUBROGATION ENDORSEMENT-              *
     H*                                                               *
     H* 07/23/08 C:197 PROJECT 132WKN                                 *
     H*                Add prompt field to enter CA Flat Charge.      *
     H*                                                               *
     H* 08/28/07 C:61  SUPPORT WA045 59823                            *
     H*                Do not allow "F5=Refresh" in ADD mode.         *
     H*                --- DSPF change only ---                       *
     H*                                                               *
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*               -Remove all processing related to project 45WFB *
     H*                                                              *
     H* 04/02/07 C:61  SUPPORT WA099 54717                            *
     H*                Remove processing for WXW8L.
     H*                                                               *
     H* 01/30/07 C:184 PROJECT WC1525                                 *
     H*                Allow entry of Waiver if called TO ADD/CHG UT  *
     H*                (WC430305).                                    *
     H*
     H* 09/14/06 C:61  SUPPORT WA133 52938
     H*                Change default DARTS remark code.
     H*                                                               *
     H* 07/26/06 C:33  Project: 45WFB                                 *
     H*                Implement 45WBZ for Back-to-Base.              *
     H*
     H* 06/11/06 C:61  PROJECT WC1333
     H*                Allow entry of Waiver Percentage if called
     H*                to ADD/CHG CA Waiver (WC040306)
     H*                                                               *
     H* 04/26/06 C:61  PROJECT 132WFI                                 *
     H*              - Change DARTS code used depending on type       *
     H*                (add/chg/dlt) of mid-term waiver.              *
     H*                                                               *
     H* 04/03/06 C:61  SUPPORT WA109 51864                            *
     H*              - Only update endorsement number if ADD mode.    *
     H*              - On delete of Waiver, do not write waiver #     *
     H*                to right side of ENDORSE form.                 *
     H*              - On delete of Waiver, write waiver information  *
     H*                (city) correctly on left side of form.         *
     H*              - Save endorsement # that last printed/processed *
     H*                Waiver.                                        *
     H*                                                               *
     H* 02/17/06 C:61  PROJECT WC1333                                 *
     H*                Add prompt field to enter CA Waiver Percentage.*
     H*                                                               *
     H* 11/09/05 C:61  PROJECT WC1333                                 *
     H*                Add field "Employer Division State" to Waiver  *
     H*                Detail file.                                   *
     H*                                                               *
     H* 07/15/05 C:136 PROJECT: 133WBS                                *
     H*                Add field to generate Cert Holder Record       *
     H*                (WMCEP) if rating state is LA.                 *
     H*                                                               *
     H* 05/10/05 C:61  SUPPORT 46984                                  *
     H*              - Reduce SCPROJ and SCPRJ2 from 60 to 50         *
     H*                character field length.                        *
     H*              - Print PRJ2 on ENDORSE form output.             *
     H*                                                               *
     H* 03/25/05 C:61  SUPPORT WA033 46131                            *
     H*              - Write endorsement number to Waiver of Subro    *
     H*                file.                                          *
     H*              - Make Waiver # display only.                    *
     H*                                                               *
     H* 03/07/05 C:65 LPROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 08/27/04 C:09  PROJECT: CM045 (SEQ # 43657)                   *
     H*                Correct issues with data printing on the       *
     H*                WC000313 endorsement form on new policies and  *
     H*                midterm endorsements.                          *
     H*                                                               *
     H* 08/11/04 C:61  SUPPORT WA099 43527                            *
     H*                Print waiver if it has been manually entered.  *
     H*                                                               *
     H* 01/18/04 C:09  PROJECT: CM045 (SEQ # 40663)                   *
     H*                Do not reprint waiver endorsement (WC000313)   *
     H*                if it has already been processed by a prior    *
     H*                midterm endorsement.                           *
     H*                Other changes in: W0815R02                     *
     H*                                                               *
     H* 04/27/00 C:53  SUPPORT: WC065/19768                           *
     H*                SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY. *
     H*                                                               *
     H* 3/28/00 C:113  PROJECT: SUPPORT #21492                        *
     H*                Changed to always grab the default waiver info *
     H*                from the statistical reporting/rating entries. *
     H*                                                               *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: Please add comments to the top of Revisions            *
     H* ------------------------------------------------------------- *
     H*===============================================================*
     H* INDICATOR USAGE:                                              *
     H*  Global                                                       *
     H*  09 - Inquiry mode                                            *
     H* N09 - Update mode                                             *
     H*  31 - Midterm mode                                            *
     H*  40 - Add mode                                                *
     H*  90 - General error                                           *
     H*  93 - Screen was updated while user was in it                 *
     H*                                                               *
     H*  SCR01:                                                       *
     H*   Error:                                                      *
     H*   20 - (WCR0502) Waiver number cannot be <= zero              *
     H*   21 - (WC30005) Waiver number previously used                *
     H*   22 - (WCR0503) Name must be entered                         *
     H*   23 - (WCR0503) Address 1 must be entered                    *
     H*   24 - (WCR0503) City must be entered                         *
     H*   25 - (WCR0504) State not valid                              *
     H*   26 - (WCR0502) Zip 1 must be >= zero                        *
     H*   27 - (WCR0590) Charge/waiver & waiver % mutually exclusive  *
     H*   28 - (WCR0503) Waiver effective date must be entered        *
     H*   29 - (WCR0500) Waiver effective date must be valid          *
     H*   32 - (WCR0930) Waiver eff date must be between policy dates *
     H*===============================================================*
     F** Display File
     FW0815F02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F** System Table File
     FSTSTL3    IF   E           K DISK
     F*
     F** Employer Detail File
     FWDELP     IF   E             DISK
      *
      *** Beg Add * 04/03/16 *
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL00)
     F                                     INFDS(ELDS)
      *
      ** Reporting Rating
     FWDRRL     IF   E           K DISK
     F*
     F** Group Auxiliary File
     FWDF2L     IF   E           K DISK
     F*
     F** Endorsement/DARTS Information File
     FWMDXL     IF   E           K DISK
     F*
     F** Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F** Employer Attached Endorsement File
     FWMENL3    IF A E           K DISK
     F*
     F** Group Master File
     FWMFNL     IF   E           K DISK
     F*
     F** Waiver Holder File
     FWMWVL1    IF   E           K DISK
     F                                     RENAME(PWV01:PWV02)
      *** beg add ***** 02/02/21 **************************
     fwmwvl9    if   e           k disk    rename(pwv01:pwv09) prefix(l9)
      *** end add ***** 02/02/21 **************************
     F*
     F** Waiver Holder physical file
     FWMWVP     UF A E             DISK
     F                                     RENAME(PWV01:DBRCDF)
     F                                     INFDS(DBFDS)
     F** Midterm Endorsement Table File
     FWTMYP     IF   E             DISK
      *
      *  RATING STATE
     FWMRSL     IF   E           K DISK
      *  CERT HOLDERS
     FWMCEL4    IF A E           K DISK
     fwtesl     if   e           k disk    rename(pes01:pes00)
      *** beg add ***** 02/02/21 ***********************
     FWXFML12   uf   e           k disk    Rename(pfm01:pfm12)
     FWMMULA    uf   e           k disk    Rename(pmu01:pmu0a)
      *** end add ***** 02/02/21 ***********************
      *
     D XXBL            S             50    DIM(20)                              Before line
     D XXAL            S             50    DIM(20)                              After line
     D XXLB            S             30    DIM(6) CTDATA PERRCD(1)              LABELS
      *
     D*=========================================================================
     D*  Data Structures & constants definitions
     D*=========================================================================
     D*
     D ELDS            DS
     D  ELRRN                397    400B 0
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*  Effective dates
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               Eff. date
      *** Beg Add *** 07/08/25 ******************************************
     D                                     Inz
      *** End Add *** 07/08/25 ******************************************
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D*  Cancellation dates
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Cancel date
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*  Midterm Endorsment data area
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D  LDSEQ#                27     31  0
     d  ldWvrMode             32     32
     d  ldStSpcfFlg           40     40
     d  ldWaiverFrm           41     55
     D                 DS
     D  NAME                   1    100
     D  NAME1                  1     40
     D  NAME2                 41    100
     D                 DS
     D  SNAME                  1    100
     D  SNAM1                  1     50
     D  SNAM2                 51    100
      *
     D                 DS
     D  SBNAME                 1    100
     D  S#NAME                 1     40
     D  S#NAM2                41    100
     D  SBNAM1                 1     50
     D  SBNAM2                51    100
     D*=========================================================================
     D*  /COPY required constants
     D*=========================================================================
     D OPTNM           C                   CONST('Waiver Information')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D I1              S              3  0 INZ(0)
     d IsStApproved    pr              n
     d xxFlPc          s              1a
      /copy wprEmplr
     d*** begin add ***  02/24/25  ***************************************
     d xxBefore        s           1000a   Inz(' ')
     d xxAfter         s           1000a   Inz(' ')
     D CR              c                   CONST(' CRLF')
     d*** end add *****  02/24/25  ***************************************
     d chrAddPgm       s                   Like(WVpgm)
     d chrPrtPgm       s                   Like(WVpgm)
     d curTxAdvPrem    s                   Like(F3a923)
     d curTxBlkWvrDsc  s                   Like(WvBlkPrjDs)
     d curTxPctPrem    s                   Like(F3a521)
     d curTxWvrPrem    s                   Like(F3a921)
     d chrWvBlkPrjDs   s                   Like(WvBlkPrjDs)
     d chrWvrForm      s                   Like(WVenm)
     d chrWvrType      s              1
     d hasOtherRcdsForEnd#...
     d                 s               n
     d hasOtherRcdsForForm...
     d                 s               n
     d isBlkWvrType    s               n
     d isEligForEnd    s               n
     d s#BlkPrjDs      s                   Like(WvBlkPrjDs)
      //*** Beg Add 05/10/24 *****************************************
       dcl-s intStRptDiv Like(WVdiv);
      //*** End Add 05/10/24 *****************************************
      /Copy wPrEndorse

     C/COPY SCOPYMT01C
     C*=========================================================================
     C*  PLIST Definitions
     C*=========================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@WTYP            1
     C                   PARM                    @@CPGM           10
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL2        PLIST
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*=========================================================================
     C*  KLIST Definitions
     C*=========================================================================
      *  Reporting Rating
      *
     C     RRKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    KKST              2
      *
     C     RRKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    KKST              2
     C                   KFLD                    ELPFDT
      *
     C*
     C*  Endors/DARTS Control Definition file
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  Employer Master file
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  Endorsement Request workfile
     C*
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*
     C*  Group Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  Group Auxiliary file
     C*
     C     F2KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C*  System Table file
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  Waiver Information file
     C*
     C     WVKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    SCWAV#
     C                   KFLD                    SCFYR
     C*
     C     WVKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKRTCD            3
     C*
     C*  CERT HOLDERS FILE
     C*
     C     CEKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*=========================================================================
     C*  *LIKE DEFN definitions
     C*=========================================================================
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    WVWAV#        XXWAV#
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    WVSWPC        XXSWPC
     C     *LIKE         DEFINE    WVFCHG        XXFCHG
     C     *LIKE         DEFINE    WVWMIN        XXWMIN
     C     *LIKE         DEFINE    WVWAV#        S#WAV#
     C     *LIKE         DEFINE    WVADR1        S#ADR1
     C     *LIKE         DEFINE    WVADR2        S#ADR2
     C     *LIKE         DEFINE    WVCTY         S#CTY
     C     *LIKE         DEFINE    WVST          S#ST
     C     *LIKE         DEFINE    WVZIP         S#ZIP
     C     *LIKE         DEFINE    WVPROJ        S#PROJ
     C     *LIKE         DEFINE    WVPRJ2        S#PRJ2
     C     *LIKE         DEFINE    WVEPAY        S#EPAY                         Estimated
     C     *LIKE         DEFINE    WVAPAY        S#APAY                         Audited
     C     *LIKE         DEFINE    WVFCHG        S#FCHG                         Charge per Wavier
     C     *LIKE         DEFINE    WVSWPC        S#SWPC                         Specific waiver %
     C     *LIKE         DEFINE    WVWMIN        S#WMIN                         Min Premium
     C     *LIKE         DEFINE    WVAPRE        S#APRE                         Advance Premium
     C     *LIKE         DEFINE    ELPFDT        XXENDT
     C     *LIKE         DEFINE    DSTIME        SVTIME
     C*=========================================================================
     C*  CKCHGS - SUBROUTINE TO CREATE MID-TERM ENDORSEMENT RECORDS
     C*=========================================================================
     C     CKCHGS        BEGSR
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C     XXSCR#        IFEQ      #OFSCR
     C     @@WTYP        OREQ      'D'
     C*
     C     WVWAV#        IFNE      S#WAV#
     C     WVNAME        ORNE      S#NAME
     C     WVNAM2        ORNE      S#NAM2
     C     WVADR1        ORNE      S#ADR1
     C     WVADR2        ORNE      S#ADR2
     C     WVCTY         ORNE      S#CTY
     C     WVST          ORNE      S#ST
     C     WVZIP         ORNE      S#ZIP
      *** beg del ***** 02/02/21 **************************
     C***     WVPROJ        ORNE      S#PROJ
     C***     WVPRJ2        ORNE      S#PRJ2
     C     WVEPAY        ORNE      S#EPAY
     C     WVAPAY        ORNE      S#APAY
     C     WVFCHG        ORNE      S#FCHG
     C     WVSWPC        ORNE      S#SWPC
     C     WVWMIN        ORNE      S#WMIN
     C     WVAPRE        ORNE      S#APRE
     C     WVblkPrjDs    OrNE      s#blkPrjDs
     C*
     C*  Get Endors/DARTS information
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     @@RRN2        CHAIN     WDELP                              60
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      AGTAPI        AGTPL2
     C*
     C*  Setup for next request number
     C*
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C*
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C*
     C                   Z-ADD     LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C*
     C                   MOVE      S#WAV#        ENOFI1                         SUBRO #
     C                   MOVEL     S#NAME        ENOEN1                         NAME
     C                   MOVEL     S#ADR1        ENOM11                         ADR1
     C                   MOVEL     S#ADR2        ENOM21                         ADR2
     C                   MOVEL     S#CTY         ENOMC1                         CTY
     C                   MOVEL     S#ST          ENOMS1                         ST
     C                   MOVE      S#ZIP         ENOMZ1                         ZIP
       Select;
       When s#BlkPrjDs <> *Blanks;
          ENoc11 = %Subst(s#BlkPrjDs:1:40);
          ENoc21 = %Subst(s#BlkPrjDs:41:40);
       Other;
          ENoc11 = s#Proj;
          ENoc21 = s#Prj2;
       Endsl;
     C*
     C                   MOVE      WVWAV#        ENOFI2                         SUBRO #
     C                   MOVEL     WVNAME        ENOEN2                         NAME
     C                   MOVEL     WVADR1        ENOM12                         ADR1
     C                   MOVEL     WVADR2        ENOM22                         ADR2
     C                   MOVEL     WVCTY         ENOMC2                         CTY
     C                   MOVEL     WVST          ENOMS2                         ST
     C                   MOVE      WVZIP         ENOMZ2                         ZIP
       Select;
       When WVblkPrjDs <> *Blanks;
          ENoc12 = %Subst(WVblkPrjDs:1:40);
          ENoc22 = %Subst(WVblkPrjDs:41:40);
       Other;
          ENoc12 = WVproj;
          ENoc22 = WVprj2;
       Endsl;
     C*
     C                   MOVE      *BLANKS       ENNCFL
     C                   MOVE      *BLANKS       ENADDF
     C                   MOVE      *BLANKS       ENDLDF
     C                   MOVE      *BLANKS       ENFIFL
     C                   MOVE      *BLANKS       ENCNFL
     C                   MOVE      *BLANKS       ENATFL
     C                   MOVE      *BLANKS       ENC3FL
     C                   MOVE      *BLANKS       ENTBFL
     C                   MOVE      *BLANKS       ENRIDF
     C                   MOVE      *BLANKS       ENNOBF
     C                   MOVE      *BLANKS       ENNOFL
     C                   MOVE      'S'           ENMSFL
      *** Begin Add *** 07/31/25 ******************************************
      * Waiver change flag
     C                   MOVE      'Y'           ENWVFL
      *** End   Add *** 07/31/25 ******************************************
      *
     C                   SELECT
     C     @@WTYP        WHENEQ    'A'
     C                   MOVEL(P)  DXWVAR        ENRMCD
     C     @@WTYP        WHENEQ    'C'
     C                   MOVEL(P)  DXWVCR        ENRMCD
     C     @@WTYP        WHENEQ    'D'
     C                   MOVEL(P)  DXWVDR        ENRMCD
     C                   OTHER
     C                   MOVEL(P)  DXMISC        ENRMCD
     C                   ENDSL
      *
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C*  Build up BEFORE line
     C*
     C                   MOVE      *BLANKS       XXBL
     c*** begin add ***  02/24/25  ***************************************
     C                   MOVE      *BLANKS       xxBefore
     c*** end add *****  02/24/25  ***************************************
      *
     C     *IN40         IFEQ      *OFF
      *
     C                   Z-ADD     1             I                 2 0
     C                   MOVEL(P)  S#WAV#        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %editc(s#wav#:'X') + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
      *
     C     SBNAM1        IFNE      *BLANKS
     C                   MOVEL(P)  SBNAM1        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(sbname) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
      *
     C     SBNAM2        IFNE      *BLANKS
     C                   MOVEL(P)  SBNAM2        XXBL(I)
     C                   ADD       1             I
     C                   ENDIF
      *
      *
     C     S#ADR1        IFNE      *BLANKS
     C                   MOVEL     S#ADR1        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(s#Adr1) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
      *
     C     S#ADR2        IFNE      *BLANKS
     C                   MOVEL     S#ADR2        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(s#Adr2) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
      *
     C     S#CTY         IFNE      *BLANKS
     C     S#ST          ORNE      *BLANKS
     C     S#ZIP         ORNE      *ZEROS
     C                   MOVEL     S#ZIP         XXZIP5            5
     C                   MOVE      S#ZIP         XXZIP4            4
     C     S#CTY         CAT       ',':0         XXBL(I)
     C                   CAT       S#ST:1        XXBL(I)
     C                   CAT       XXZIP5:1      XXBL(I)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXBL(I)
     C                   CAT       XXZIP4:0      XXBL(I)
     C                   ENDIF
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(xxBL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF

       If %Subst(s#BlkPrjDs:1:50) <> *Blanks;
          xxBL(i) = %Subst(s#BlkPrjDs:1:50);
          i += 1;
       Endif;

       If %Subst(s#BlkPrjDs:51:50) <> *Blanks;
          xxBL(i) = %Subst(s#BlkPrjDs:51:50);
          i += 1;
       Endif;

       If %Subst(s#BlkPrjDs:101:50) <> *Blanks;
          xxBL(i) = %Subst(s#BlkPrjDs:101:50);
          i += 1;
       Endif;

       If %Subst(s#BlkPrjDs:151:50) <> *Blanks;
          xxBL(i) = %Subst(s#BlkPrjDs:151:50);
          i += 1;
       Endif;

       If %Subst(s#BlkPrjDs:201:50) <> *Blanks;
          xxBL(i) = %Subst(s#BlkPrjDs:201:50);
          i += 1;
       Endif;

     c*** begin add ***  02/24/25  ***************************************
                         If s#BlkPrjDs <> ' ';
                         xxBefore = %trim(xxBefore) + %trim(s#BlkPrjDs) + CR;
                         Endif;
     c*** end add *****  02/24/25  ***************************************
     C     S#PROJ        IFNE      *BLANKS
     C                   MOVEL(P)  S#PROJ        XXTMP1           50
     C                   MOVEL     XXTMP1        XXBL(I)
     C                   ADD       1             I
     C                   ENDIF
      *
     C     S#PRJ2        IFNE      *BLANKS
     C                   MOVEL(P)  S#PRJ2        XXTMP1
     C                   MOVEL     XXTMP1        XXBL(I)
     C                   ADD       1             I
     C                   ENDIF
      *
     C*
     C     S#EPAY        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(S#EPAY :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2           50
     C                   MOVEL     XXLB(1)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(xxBL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     S#APAY        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(S#APAY :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(2)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(xxBL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     S#FCHG        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(S#FCHG :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(3)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(xxBL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     S#SWPC        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(S#SWPC :'K')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(4)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(xxBL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     S#WMIN        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(S#WMIN :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(5)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(xxBL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     S#APRE        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(S#APRE :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(6)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXBL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxBefore = %trim(xxBefore) + %trim(xxBL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Build up AFTER line
     C*
     C                   MOVE      *BLANKS       XXAL
     d*** begin add ***  02/24/25  ***************************************
     C                   MOVE      *BLANKS       xxAfter
     d*** end add *****  02/24/25  ***************************************
     C                   Z-ADD     1             I
      *
     C     WVWAV#        IFNE      *ZERO
     C                   MOVEL     WVWAV#        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %editc(s#wav#:'X') + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
      *
     C     WVNAME        IFNE      *BLANKS
     C                   MOVEL     SNAM1         XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(sname) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
      *
     C     WVNAM2        IFNE      *BLANKS
     C                   MOVEL     SNAM2         XXAL(I)
     C                   ADD       1             I
     C                   ENDIF
      *
     C     WVADR1        IFNE      *BLANKS
     C                   MOVEL     WVADR1        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(wvadr1) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
      *
     C     WVADR2        IFNE      *BLANKS
     C                   MOVEL     WVADR2        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(wvadr2) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
      *
     C     WVCTY         IFNE      *BLANKS
     C     WVST          ORNE      *BLANKS
     C     WVZIP         ORNE      *ZEROS
     C                   MOVEL     WVZIP         XXZIP5            5
     C                   MOVE      WVZIP         XXZIP4            4
     C     WVCTY         CAT       ',':0         XXAL(I)
     C                   CAT       WVST:1        XXAL(I)
     C                   CAT       XXZIP5:1      XXAL(I)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXAL(I)
     C                   CAT       XXZIP4:0      XXAL(I)
     C                   ENDIF
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(xxAL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF

       If %Subst(WVblkPrjDs:1:50) <> *Blanks;
          xxAL(i) = %Subst(WVblkPrjDs:1:50);
          i += 1;
       Endif;

       If %Subst(WVblkPrjDs:51:50) <> *Blanks;
          xxAL(i) = %Subst(WVblkPrjDs:51:50);
          i += 1;
       Endif;

       If %Subst(WVblkPrjDs:101:50) <> *Blanks;
          xxAL(i) = %Subst(WVblkPrjDs:101:50);
          i += 1;
       Endif;

       If %Subst(WVblkPrjDs:151:50) <> *Blanks;
          xxAL(i) = %Subst(WVblkPrjDs:151:50);
          i += 1;
       Endif;

       If %Subst(WVblkPrjDs:201:50) <> *Blanks;
          xxAL(i) = %Subst(WVblkPrjDs:201:50);
          i += 1;
       Endif;

     c*** begin add ***  02/24/25  ***************************************
                         If WVBlkprjds <> ' ';
                         xxAfter = %trim(xxAfter) + %trim(WVBlkprjds) + CR;
                         Endif;
     c*** end add *****  02/24/25  ***************************************
     C     WVEPAY        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(WVEPAY :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2           50
     C                   MOVEL     XXLB(1)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(xxAL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     WVAPAY        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(WVAPAY :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(2)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(xxAL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     WVFCHG        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(WVFCHG :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(3)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(xxAL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     WVSWPC        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(WVSWPC :'K')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(4)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(xxAL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     WVWMIN        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(WVWMIN :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(5)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(xxAL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     WVAPRE        IFNE      *ZEROS
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *BLANKS       XXTMP1
     C                   EVAL      XXTMP1  = %EDITC(WVAPRE :'K':'$')
     C*
     C* Add Labels for the changed values
     C     XXTMP1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXTMP2
     C                   MOVEL     XXLB(6)       XXTMP2
     C                   EVAL      XXTMP2= %TRIM(XXTMP2) + ': ' + %TRIM(XXTMP1)
     C                   MOVEL     XXTMP2        XXAL(I)
     c*** begin add ***  02/24/25  ***************************************
                         xxAfter = %trim(xxAfter) + %trim(xxAL(I)) + CR;
     c*** end add *****  02/24/25  ***************************************
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C                   MOVE      F2CAR#        ENCAR1
     C                   Z-ADD     ELPFDT        ENPFD1
     C*
     C*  Write the records
     C*
     c*** begin chg ***  02/24/25  ***************************************
     C*****              Z-ADD     1             I
     C*****ELWVTP        IFEQ      'S'
     C*****              Z-ADD     20            X                 2 0
     C*****              ELSE
     C*****              Z-ADD     9             X
     C*****              ENDIF
     C*
     C*****              DO        X
     C*
     C*****XXBL(I)       IFEQ      *BLANKS
     C*****XXAL(I)       ANDEQ     *BLANKS
     C*****ELWVTP        ANDNE     'S'
     C*****              LEAVE
     C*****              ENDIF
     C*
     C*****              MOVEL     XXBL(I)       ENBLIN
     C*****              MOVEL     XXAL(I)       ENALIN
     C*
     C*****              WRITE     PEN01
     C*
     C*  Bump up necessary numeric fields for next write if required
     C*
     C*****              ADD       1             I
     C*****              ADD       1             ENTIME
     C*****              ADD       1             ENSEQ#
     C*****              ENDDO
     C                   MOVEL     xxBefore      ENBefore
     C                   MOVEL     xxAfter       ENAfter
     C                   WRITE     PEN01
     c*** end chg *****  02/24/25  ***************************************
     C*
     C                   ENDIF                                                  WVWAV# <> S#WAV#
     C*
     C                   ENDIF
     C                   ENDIF                                                  LDENDA <>0,' '
     C*
     C                   ENDSR                                                  CKCHGS
     C*=========================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*=========================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
     C*
     C     WVSTAT        IFEQ      'I'
     C                   MOVE      'A'           WVSTAT
     C                   ELSE
     C                   MOVE      'I'           WVSTAT
     C                   ENDIF                                                  WVSTAT=I
     C*
     C                   MOVEL     Q1USER        WVUSER
     C                   MOVEL     Q1PGM         WVPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WVDT
     C                   MOVEL     DSTIME        WVTIME
       //*** Beg Add *** 05/10/24 ******************************************
       intStRptDiv = GetStRptDv(WVco#:WVfnd:WVemp#);
       WVemst = GetRatingState(WVco#:WVfnd:WVemp#:intstRptDiv:
         WVfyr);
       //*** End Add *** 05/10/24 ******************************************
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
     C*=========================================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*=========================================================================
     C     DLTPRE        BEGSR

       // Delete endorsement form record if needed.
       Exsr DltWMMUP;
     C*
     C*  Save the old database values
     C*
     C                   MOVE      WVWAV#        S#WAV#
     C                   MOVE      WVNAME        S#NAME
     C                   MOVE      WVNAM2        S#NAM2
     C                   MOVE      WVADR1        S#ADR1
     C                   MOVE      WVADR2        S#ADR2
     C                   MOVEL(P)  WVCTY         S#CTY
     C                   MOVE      WVST          S#ST
     C                   MOVE      WVZIP         S#ZIP
       If WVblkPrjDs <> *Blanks;
          s#BlkPrjDs = WVblkPrjDs;
       Else;
          s#BlkPrjDs = WVproj + WVprj2;
       EndIf;
       WVblkPrjDs = *Blanks;
     C                   Z-ADD     WVEPAY        S#EPAY
     C                   Z-ADD     WVAPAY        S#APAY
     C                   Z-ADD     WVFCHG        S#FCHG
     C                   Z-ADD     WVSWPC        S#SWPC
     C                   Z-ADD     WVWMIN        S#WMIN
     C                   Z-ADD     WVAPRE        S#APRE
     C*
     C                   CLEAR                   WVWAV#
     C                   CLEAR                   WVNAME
     C                   CLEAR                   WVNAM2
     C                   CLEAR                   WVADR1
     C                   CLEAR                   WVADR2
     C                   CLEAR                   WVCTY
     C                   CLEAR                   WVST
     C                   CLEAR                   WVZIP
     C                   CLEAR                   WVPROJ
     C                   CLEAR                   WVPRJ2
      // Don't clear fields just saved...
     C                   CLEAR                   WVEPAY
     C                   CLEAR                   WVAPAY
     C                   CLEAR                   WVFCHG
     C                   CLEAR                   WVSWPC
     C                   CLEAR                   WVWMIN
     C                   CLEAR                   WVAPRE
     C*
     C*  Create Mid-Term Endorsement records
     C*
     C                   EXSR      CKCHGS
     C*
     C                   ENDSR                                                  DLTPRE
       //===========================================================
       Begsr DltWMMUP;
       //===========================================================
        // If no other WMWVP records exist for endorsement #,
        //  delete WMMUP records for the endorsement #.
        // If no other WMWVP records exist for the form on the policy,
        //  delete the policy form record (End# = 0).
        Setll (WVco#:WVfnd:WVfyr:WVemp#) pwv09;
        Dou %Eof(WMWVL9);
           Reade (WVco#:WVfnd:WVfyr:WVemp#) pwv09;
           If %Eof;
              Leave;
           EndIf;
           Select;
           When l9WVenm <> ldWaiverFrm;
              Iter;
           When l9WVdiv = WVdiv And l9WVend# = WVend# And l9WVwav# = WVwav#;
              Iter;
           Endsl;
           If l9WVend# = WVend#;
              hasOtherRcdsForEnd# = *On;
           Endif;
           If l9WVend# <> WVend# Or l9WVdiv <> WVdiv;
              hasOtherRcdsForForm = *On;
           Endif;
        Enddo;
        If hasOtherRcdsForEnd# = *Off;
           Setll (WVco#:WVfnd:WVemp#:WVfyr:ldWaiverFrm:WVend#) pmu0a;
           Dou %Eof(WMMULA);
              Reade (WVco#:WVfnd:WVemp#:WVfyr:ldWaiverFrm:WVend#) pmu0a;
              If %Eof;
                 Leave;
              EndIf;
              Delete (WVco#:WVfnd:WVemp#:WVfyr:ldWaiverFrm:WVend#) pmu0a;
           Enddo;
        Endif;
        If hasOtherRcdsForForm = *Off;
           Setll (WVco#:WVfnd:WVemp#:WVfyr:ldWaiverFrm:0) pmu0a;
           Dou %Eof(WMMULA);
              Reade (WVco#:WVfnd:WVemp#:WVfyr:ldWaiverFrm:0) pmu0a;
              If %Eof;
                 Leave;
              EndIf;
              Delete (WVco#:WVfnd:WVemp#:WVfyr:ldWaiverFrm:0) pmu0a;
           Enddo;
        Endif;
       Endsr;
     C*=========================================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*=========================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*=========================================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*=========================================================================
     C     ERRCK1        BEGSR
       // If division selection has changed, edit for eligibility
       //  and set screen up for blanket or specific waiver as applicable.
        xxScr# = 1;
        msgDivDrp = *Blanks;
        erDivDrp = *Off;
        If ELdiv <> scOoDiv#;
           visPrjDesc = *Off;
           visSpcWvr = *Off;
           *In65 = *Off;
           isBlkWvrType = *Off;
           isEligForEnd = *Off;
           Chain(n) (ELco#:ELfnd:ELemp#:SCooDiv#:ELfyr) wdell;
           If %Found;
              @@RRN2 = ELrrn;
           Else;
              erDivDrp = *On;
              msgDivDrp = 'Division not active in policy year ' +
               %Editc(ELfyr:'X') + '.';
              *In90 = *On;
              LeaveSr;
           Endif;
           If *In90 = *Off And GetWaiverInfo(ELco#:ELfnd:kkSt:ELpfdt:
            chrWvrForm:chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:ELfyr:ELemp#:
            ELdiv) = *On;
              isEligForEnd = *On;
              isBlkWvrType = %Subst(chrWvBlkPrjDs:500:1) = 'B';
              %Subst(chrWvBlkPrjDs:500:1) = *Blank;
              If isBlkWvrType = *On;
                 #OfScr = 1;
                 scBlkPrjDs = chrWvBlkPrjDs;
                 visPrjDesc = *On;
                 visSpcWvr = *Off;
                 topBlkWvrs = 295;
              Else;
                 scBlkPrjDs = *Blanks;
                 visPrjDesc = *On;
                 visSpcWvr = *On;
                 topBlkWvrs = 420;
                 #OfScr = 2;
                 *In65 = *On;
              Endif;
              *In90 = *On;
           Else;
              erDivDrp = *On;
              msgDivDrp = chrWvBlkPrjDs;
              *In90 = *On;
              If GetWaiverInfo(ELco#:ELfnd:kkSt:ELpfdt:
               chrWvrForm:chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:
               ELfyr:ELemp#) = *Off;
                 msgDivDrp = chrWvBlkPrjDs;
              Else;
                 msgDivDrp = 'Division not eligible for endorsement form ' +
                  chrWvrForm + '.';
                 chrWvBlkPrjDs = msgDivDrp;
              Endif;
              LeaveSr;
           Endif;
        Endif;
       // Throw error if division is not changed from initial value
       //  and is not eligible for the endorsement form.
        If Not(*In90) And isEligForEnd = *Off;
           erDivDrp = *On;
           msgDivDrp = chrWvBlkPrjDs;
           *In90 = *On;
           LeaveSr;
        EndIf;
     C*
     C*  Default to latest # in ADD mode
     C*
     C     *IN40         IFEQ      *ON
     C                   MOVE      *ON           *IN60
     C     WVKY1         SETGT     PWV02
     C     WVKY1         READPE    PWV02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     WVWAV#        ADD       1             XXWAV#
     C                   ELSE
     C                   Z-ADD     1             XXWAV#
     C                   ENDIF                                                  *IN60=*OFF
     C                   Eval      scwav# = xxwav#

     C* get the employer detail values
     C                   Z-ADD     ELWVPC        XXSWPC
     C                   Z-ADD     ELFCHG        XXFCHG
     C                   MOVE      ELWMIN        XXWMIN
     C*
     C   40              Z-ADD     XXSWPC        SCSWPC
     C   40              Z-ADD     XXFCHG        SCFCHG
     C   40              Z-ADD     XXWMIN        SCWMIN
     C                   ENDIF                                                  @@MODE=A
     C*
     C*  Waiver effective date must be entered
     C*
     C  N90SCWEFD        IFEQ      *ZERO
     C                   SETON                                        28  90
     C                   ENDIF                                                  SCWEFD = *ZERO
     C*
     C*  Waiver effective date must be valid
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCWEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        29  90
     C                   ELSE
     C                   MOVEL     SRDTO         SCWEFD
     C                   ENDIF                                                  SRERR = *ON
     C*
     C                   ENDIF
     C*
     C*  Waiver effective date must be between employer effective dates
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   SETON                                        32  90
     C*
     C                   Z-ADD     SCWEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
     C*
     C     XXENDT        IFGE      ELPFDT
     C     XXENDT        ANDLT     ELPTDT
     C                   SETOFF                                       32  90
     C                   ENDIF                                                  XXENDT GE ELPFDT
     C*
     C                   ENDIF                                                  *IN90 = *OFF

       // Don't edit the rest of the waiver detail fields
       //  for blanket waivers. They aren't used.
       If isBlkWvrType;
          LeaveSr;
       Endif;

       //*** Beg Add *** 08/12/22 *********************************************
       msPerOrg = 'Name/Address and Person or Organization fields are +
        mutually exclusive';
       msPerOrg2 = 'Name/Address or Person/Organization are required';
       If scPerOrg = *blanks;
       //*** End Add *** 08/12/22 *********************************************
      *
     C*  Name cannot be blank
     C*
     C  N90SCCNAM        IFEQ      *BLANK
       //*** Beg Chg *** 08/12/22 *********************************************
     C***********        SETON                                        22  90
       *in90 = *on;
       erPerOrg2 = *on;
       //*** End Chg *** 08/12/22 *********************************************
     C                   ENDIF                                                  SCCNAM=*BLANK
     C*
     C*  Address line cannot be blank
     C*
     C  N90SCADR1        IFEQ      *BLANK
     C     ELWVTP        ANDNE     'B'
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCADR1=*BLANK
     C*
     C*  City cannot be blank
     C*
     C  N90SCCTY         IFEQ      *BLANK
     C     ELWVTP        ANDNE     'B'
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCCTY=*BLANK
     C*
     C*  State must be valid
     C*
     C     ELWVTP        IFNE      'B'
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        25  90
     C                   ENDIF
     C*
     C*  ZIP code cannot be zero
     C*
     C  N90SCZIP1        IFLE      *ZERO
     C     ELWVTP        ANDNE     'B'
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCZIP1<=*ZERO
       //*** Beg Add *** 08/12/22 *********************************************
       Endif;
       //*** End Add *** 08/12/22 *********************************************
       //*** Beg Add *** 08/12/22 *********************************************
       // Name/Address and Person/Org cannot both be entered
       If *in90 = *off;
          msPerOrg = *blanks;
          If scPerOrg <> *blanks and (scCnam <> *blanks or scAdr1 <> *blanks
            or scAdr2 <> *blanks or scCty <> *blanks or scSt <> *blanks or
            scZip1 <> *zeros);
            erPerOrg = *on;
            *in90 = *on;
            msPerOrg = 'Name/Address and Person or Organization fields are +
             mutually exclusive';
          Endif;
       Endif;
       // Name/Address or Person/Org must be entered
       //***If *in90 = *off;
       //***   msPerOrg2 = *blanks;
       //***   If scPerOrg = *blanks and (scCnam = *blanks or scAdr1 = *blanks
       //***     or scCty = *blanks or scSt = *blanks or
       //***     scZip1 <> *zeros);
       //***     erPerOrg2 = *on;
       //***     *in90 = *on;
       //***     msPerOrg2 = 'Name/Address or Person/Organization are required';
       //***   Endif;
       //***Endif;
       //*** End Add *** 08/12/22 *********************************************
     C*
     C                   Z-ADD     1             XXSCR#            1 0
      *
     C*  We have passed all edits, goto the next screen
     C*
     C                   ENDSR                                                  ERRCK1
     C*=========================================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*=========================================================================
     C     ERRCK2        BEGSR
     C*
     C*  Charge per waiver and spcific waiver % are mutually exclusive
     C*
     C  N90SCSWPC        IFNE      *ZERO
     C     SCFCHG        ANDNE     *ZERO
     C                   SETON                                        278090
     C                   ENDIF                                                  SCSWPC != 0
      *
     C     CLTID         IFEQ      '132'
      *
      * Flat charge
      *
     C     XXFLPC        IFEQ      'F'
     C     SCFCHG        ANDEQ     *ZERO
     C                   SETON                                        71  90
     C                   ENDIF
      *
      * Percentage
      *
     C     XXFLPC        IFEQ      '%'
     C     SCSWPC        ANDEQ     *ZERO
     C                   SETON                                        72  90
     C                   ENDIF
      *
     C                   ENDIF                                                  CLTID 132
      *
      *  VALIDATE CERT HOLDER FIELD FOR (Y/ )
      *
     C  N90SCCERT        IFNE      *BLANK
     C     SCCERT        ANDNE     'Y'
     C                   SETON                                        79  90      *IN79
     C                   ENDIF
     C*
     C*  Check to be sure the screen has not been updated since entry
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90WVDT          IFGT      SAVEDT
     C     WVDT          OREQ      SAVEDT
     C     WVTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  WVDT>SAVEDT
     C*
     C                   Z-ADD     2             XXSCR#
     C*
     C                   ENDSR                                                  ERRCK2
     C*=========================================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*=========================================================================
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR                                                  ERRCK3
     C*=========================================================================
     C* KJDO - SUBROUTINE FOR F10
     C*=========================================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
     C                   ENDSR                                                  KJDO
      *
     C*=========================================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*=========================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *OFF          *IN65
     C     ELWVTP        IFEQ      'S'
     C                   MOVE      *ON           *IN65
     C                   MOVE      2             #OFSCR
     C                   ENDIF                                                   ELWVTP = 'S
     C*
     C*  Check for midterm processing to determine waiver date
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   SETON                                        31        Midterm
     C                   ENDIF
     C*
     C                   SELECT
     C     *IN40         WHENEQ    *ON
     C*
     C                   MOVEL     'A'           WVSTAT
     C                   Z-ADD     XXWAV#        WVWAV#
      *
      *   IF ADD MODE AND JRST = 'LA', DISPLAY SCCERT TO FIND OUT IF CERT
      *   HOLDER RECORD NEEDS TO BE WRITTEN
      *
     C                   MOVE      *BLANKS       SCCERT
     C                   MOVE      ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST              2
      *
     C     XXST          IFEQ      'LA'
     C                   MOVE      *ON           *IN62
     C                   ENDIF
     C*
     C     *IN31         IFEQ      *ON
     C                   Z-ADD     LDEEDT        SCWEFD
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWEFD
     C                   ENDIF                                                  Midterm
     C*
     C     @@MODE        WHENEQ    'C'

     c                   If        WVend# <> *Zero
     c                   Eval      *In31 = *On
     c                   Endif
     c                   Eval      srdti = WVefdt
     c                   Call      'SRCVTDAT'    dtmdyy
     c                   Movel     srdto         scwefd
     C*
     C     *IN09         WHENEQ    *ON
     C                   Z-ADD     WVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWEFD
     C                   SETON                                        31
     C*
     C                   ENDSL
     C*
     C                   Z-ADD     WVWAV#        SCWAV#
     C                   MOVEL(P)  WVNAME        NAME1
     C                   MOVEL(P)  WVNAM2        NAME2
     C                   MOVE      NAME          SNAME
     C                   MOVEL(P)  SNAM1         SCCNAM
     C                   MOVEL(P)  SNAM2         SCNAM2
     C                   MOVEL     WVADR1        SCADR1
     C                   MOVEL     WVADR2        SCADR2
     C                   MOVEL     WVCTY         SCCTY
     C                   MOVEL     WVST          SCST
     C                   MOVEL     WVZIP         SCZIP1
     C                   MOVE      WVZIP         SCZIP2
     C                   Z-ADD     WVEPAY        SCEPAY
     C                   Z-ADD     WVAPAY        SCAPAY
     C                   Z-ADD     WVAPRE        SCAPRE
     C*
     C   40              Z-ADD     XXSWPC        SCSWPC
     C   40              Z-ADD     XXFCHG        SCFCHG
     C   40              Z-ADD     XXWMIN        SCWMIN
     C  N40              Z-ADD     WVSWPC        SCSWPC
     C  N40              Z-ADD     WVFCHG        SCFCHG
     C  N40              Z-ADD     WVWMIN        SCWMIN

       // For blanket waivers, default project vbg on add.
       Select;
       When *In40 = *On And isBlkWvrType;
          scBlkPrjDs = chrWvBlkPrjDs;
       When WVproj <> *Blanks;
          scBlkPrjDs = WVproj + WVprj2;
       Other;
          scBlkPrjDs = WVblkPrjDs;
       EndSl;
       //*** Beg Add *** 08/12/22 **********************************************
       scPerOrg = WVperOrg;
       //*** End Add *** 08/12/22 **********************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*=========================================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*=========================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL(P)  SCCNAM        SNAM1
     C                   MOVEL(P)  SCNAM2        SNAM2
     C                   MOVE      SNAME         NAME
     C                   MOVEL(P)  NAME1         WVNAME
     C                   MOVEL(P)  NAME2         WVNAM2
     C                   MOVEL     SCADR1        WVADR1
     C                   MOVEL     SCADR2        WVADR2
     C                   MOVEL     SCCTY         WVCTY
     C                   MOVEL     SCST          WVST
     C                   MOVEL     SCZIP1        WVZIP
     C                   MOVE      SCZIP2        WVZIP
     C                   Z-ADD     SCEPAY        WVEPAY
     C                   Z-ADD     SCAPAY        WVAPAY
     C                   Z-ADD     SCSWPC        WVSWPC
     C                   Z-ADD     SCFCHG        WVFCHG
     C                   Z-ADD     SCWMIN        WVWMIN
     C                   Z-ADD     SCAPRE        WVAPRE
     C*
     C                   Z-ADD     SCWEFD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WVEFDT
     C   40              MOVE      LDEND#        WVEND#
     C                   MOVE      LDEND#        WVPRT#
       WVemst = GetRatingState(WVco#:WVfnd:WVemp#:WVdiv:WVfyr);
       WVenm = ldWaiverFrm;
       WVblkPrjDs = scBlkPrjDs;
       WVproj = *Blanks;
       WVprj2 = *Blanks;
       // Update blanket waiver description in policy detail if changed.
       If WVblkPrjDs <> s#blkPrjDs And ELwvtp = 'B';
          UpdateBlkWvrDft(WVco#:WVfnd:WVemp#:WVdiv:WVfyr:WVenm:WVblkPrjDs);
       Endif;
       If *In40 = *On;
          WVefdt = %Dec(%Date(LDeedt:*Usa):*Cymd);
       Endif;
       //*** Beg Add *** 08/12/22 **********************************************
       WVperOrg = scPerOrg;
       //*** End Add *** 08/12/22 **********************************************
     C                   ENDSR                                                  MVSCDB
     C*=========================================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*=========================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*=========================================================================
     C* OTHFKS - Subroutine for processing other function keys
     C*=========================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
       //*** Beg Add *** 08/12/22 *********************************************
            erPerOrg  = *off;
            erPerOrg2 = *off;
       //*** End Add *** 08/12/22 *********************************************
     C                   ENDSR                                                  OTHFKS
     C*=========================================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*=========================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*=========================================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*=========================================================================
     C     PGMSTR        BEGSR
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
       dsbOODiv# = *off;
       Select;
       When @@WTyp = 'A';  // Midterm Waiver Add
          *In40 = *On;
          dsbOODiv# = *On;
          @@Mode = @@WTyp;
          chrWvrType = @@WTyp;
       When @@WTyp = 'C';  // Midterm Waiver Chg
          dsbOODiv# = *On;
          @@Mode = @@WTyp;
          chrWvrType = @@WTyp;
       When @@WTyp = 'D';  // Midterm Waiver Dlt
          @@Mode = 'E';
          chrWvrType = @@WTyp;
       When ldWvrMode = 'A';  // W/W Emp Endorsements Add
          *In40 = *On;
          @@Mode = ldWvrMode;
          chrWvrType = @@WTyp;
       When ldWvrMode = 'C';  // W/W Emp Endorsements Chg
          @@Mode = *Blank;
          chrWvrType = @@WTyp;
       Endsl;

     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
      /Free
       If Not *In40;
          Chain(n) @@RRN  DBRCDF;
          dsbOODiv# = *on;
          Chain(n) (WVco#:WVfnd:WVemp#:WVDiv:WVfyr) wdell;
          @@RRN2 = ELrrn;
       Endif;
      /End-Free
     C*
     C*  Get Employer Detail record
     C*
     C     @@RRN2        CHAIN     WDELP                              60

      /Free
       visPrjDesc = *Off;
       visSpcWvr = *Off;
       //*** Beg Add *** 08/12/22 *********************************************
       visPerOrg = *Off;
       //*** End Add *** 08/12/22 *********************************************
       kkSt = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
       chrWvrForm = ldWaiverFrm;
       If GetWaiverInfo(ELco#:ELfnd:kkSt:ELpfdt:chrWvrForm:
        chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:ELfyr:ELemp#:ELdiv) = *On;
          isBlkWvrType = %Subst(chrWvBlkPrjDs:500:1) = 'B';
          %Subst(chrWvBlkPrjDs:500:1) = *Blank;
          isEligForEnd = *On;
       Else;
          isEligForEnd = *Off;
       Endif;
       Select;
       When isEligForEnd = *Off;
       When isBlkWvrType = *On;
          visPrjDesc = *On;
          topBlkWvrs = 295;
       When isBlkWvrType = *Off;
          #OfScr = 2;
          visPrjDesc = *On;
          visSpcWvr = *On;
          topBlkWvrs = 420;
          //*** Beg Add *** 08/12/22 *********************************************
          visPerOrg = *On;
          topPerOrg = 505;
          //*** End Add *** 08/12/22 *********************************************
       Endsl;
       Select;
       When isEligForEnd = *Off;
       When Not(isBlkWvrType);
       When %Subst(ldWaiverFrm:1:8) = 'WC420304';
          Chain(n) (ELco#:ELfnd:ELemp#:ELfyr:ldWaiverFrm) pfm12;
          If %Found;
             curTxWvrPrem = f3a921;
             curTxPctPrem = f3a521;
             curTxAdvPrem = f3a923;
             curTxBlkWvrDsc = F3500A1;
             scBlkPrjDs = F3500A1;
          Endif;
          If chrWvrType = 'A';
             Exsr WrtTxBlkWvr;
          Endif;
          If chrWvrType = 'C';
             Exsr ChgTxBlkWvr;
          Endif;
          If chrWvrType = 'D';
             Exsr DltTxBlkWvr;
          Endif;
          Return;
       When chrWvrType = 'D';
          Exsr PreDlt;
          Return;
       Endsl;
      /End-Free
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
     C*
     C*
     C*  Get Employer Master record
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCOODIV#
     C*
     C*  Compress EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C                   MOVEL     XXEDN         SCEDN

      /Free
       Clear divChoice;
       divChoice = 'EMco# = ' + %char(EMco#) + ' AND EMfnd = '
        + %char(EMfnd) + ' AND EMemp# = ' + %char(EMemp#);
      /End-Free

     C*  Get the most recent cancel date, if any
     C*
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
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
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C*  Get Group information
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C     FNEXDT        IFNE      *ZEROS
     C     *IN09         ANDNE     *ON
     C     SYSDT         COMP      FNEXDT                             09  09
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C*  Get Agent/Producer information
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  Default to latest # in ADD mode
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN60
     C*
     C     WVKY1         SETGT     PWV02
     C     WVKY1         READPE    PWV02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     WVWAV#        ADD       1             XXWAV#
     C                   ELSE
     C                   Z-ADD     1             XXWAV#
     C                   ENDIF                                                  *IN60=*OFF
     C* get the employer detail values
     C                   Z-ADD     ELWVPC        XXSWPC
     C                   Z-ADD     ELFCHG        XXFCHG
     C                   MOVE      ELWMIN        XXWMIN
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C*  Save the old database values
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   S#WAV#
     C                   CLEAR                   S#NAME
     C                   CLEAR                   S#NAM2
     C                   CLEAR                   S#ADR1
     C                   CLEAR                   S#ADR2
     C                   CLEAR                   S#CTY
     C                   CLEAR                   S#ST
     C                   CLEAR                   S#ZIP
     C                   CLEAR                   S#EPAY
     C                   CLEAR                   S#APAY
     C                   CLEAR                   S#FCHG
     C                   CLEAR                   S#SWPC
     C                   CLEAR                   S#WMIN
     C                   CLEAR                   S#APRE
       s#blkPrjDs = *Blanks;
     C*
     C                   ELSE
     C*
     C                   MOVE      WVWAV#        S#WAV#
     C                   MOVE      WVNAME        S#NAME
     C                   MOVE      WVNAM2        S#NAM2
     C                   MOVE      WVADR1        S#ADR1
     C                   MOVE      WVADR2        S#ADR2
     C                   MOVE      WVCTY         S#CTY
     C                   MOVE      WVST          S#ST
     C                   MOVE      WVZIP         S#ZIP
     C                   Z-ADD     WVEPAY        S#EPAY
     C                   Z-ADD     WVAPAY        S#APAY
     C                   Z-ADD     WVFCHG        S#FCHG
     C                   Z-ADD     WVSWPC        S#SWPC
     C                   Z-ADD     WVWMIN        S#WMIN
     C                   Z-ADD     WVAPRE        S#APRE
       S#BlkPrjDs = WVblkPrjDs;
     C*
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SVTIME

      /Free
       If *In40;
          SCOODiv# = EMdiv;
       Else;
          SCOODiv# = WVdiv;
       Endif;
      /End-Free
      *
     C*
     C                   ENDSR                                                  PGMSTR
     C*=========================================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*=========================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*=========================================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*=========================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C     WVWAV#        IFNE      S#WAV#
     C     WVNAME        ORNE      S#NAME
     C     WVNAM2        ORNE      S#NAM2
     C     WVADR1        ORNE      S#ADR1
     C     WVADR2        ORNE      S#ADR2
     C     WVCTY         ORNE      S#CTY
     C     WVST          ORNE      S#ST
     C     WVZIP         ORNE      S#ZIP
     C     WVEPAY        ORNE      S#EPAY
     C     WVAPAY        ORNE      S#APAY
     C     WVFCHG        ORNE      S#FCHG
     C     WVSWPC        ORNE      S#SWPC
     C     WVWMIN        ORNE      S#WMIN
     C     WVAPRE        ORNE      S#APRE
     C     WVblkPrjDs    Orne      s#blkPrjDs
     C*
     C                   MOVEL     Q1USER        WVUSER
     C                   MOVEL     Q1PGM         WVPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WVDT
     C                   MOVEL     DSTIME        WVTIME
       //*** Beg Add *** 05/10/24 ******************************************
       intStRptDiv = GetStRptDv(WVco#:WVfnd:WVemp#);
       WVemst = GetRatingState(WVco#:WVfnd:WVemp#:intstRptDiv:
         WVfyr);
       //*** End Add *** 05/10/24 ******************************************
     C*
     C                   UPDATE    DBRCDF
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   ENDIF
      *
     C                   ENDIF                                                  WV changed
     C*
     C                   ELSE
     C*
     C                   MOVEL     Q1USER        WVUSER
     C                   MOVEL     Q1PGM         WVPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WVDT
     C                   MOVEL     DSTIME        WVTIME
     C*
     C                   UPDATE    DBRCDF
      *
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   ENDIF
      *
     C*
     C                   ENDIF                                                  Midterm
     C*
     C*  Create Mid-Term Endorsement records
     C*
     C                   EXSR      CKCHGS
     C*
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*=========================================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*=========================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     ELCO#         WVCO#
     C                   Z-ADD     ELFND         WVFND
     C                   Z-ADD     ELEMP#        WVEMP#
     C                   Z-ADD     SCOODIV#      WVDIV
     C                   Z-ADD     ELFYR         WVFYR
     C                   Z-ADD     SCWAV#        WVWAV#
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        WVIUSR
     C                   MOVEL     Q1USER        WVUSER
     C                   MOVEL     Q1PGM         WVIPGM
     C                   MOVEL     Q1PGM         WVPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     DSTIME        WVITM
     C                   MOVEL     DSTIME        WVTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WVIDT
     C                   MOVEL     SRDTO         WVDT
       //*** Beg Add *** 05/10/24 ******************************************
       intStRptDiv = GetStRptDv(WVco#:WVfnd:WVemp#);
       WVemst = GetRatingState(WVco#:WVfnd:WVemp#:intstRptDiv:
         WVfyr);
       //*** End Add *** 05/10/24 ******************************************
     C*
     C                   WRITE     DBRCDF
     C   40              ADD       1             XXWAV#
      *
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C                   ENDIF
     C                   FEOD      WMWVP
     C*
     C*  Create Mid-Term Endorsement records
     C*
     C                   EXSR      CKCHGS
     C*
     C     SCCERT        IFEQ      'Y'
     C                   EXSR      SRCERT
     C                   ENDIF

       If isBlkWvrType = *On;
          outFlg = *On;
       Endif;

     C                   ENDSR                                                  WRITER
     C*=========================================================================
     C* SRCERT - WRITE CERT HOLDERS RECORD
     C*=========================================================================
     C     SRCERT        BEGSR
      *
     C     CEKY1         SETGT     PCE01
     C     CEKY1         READPE    PCE01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CECER#        ADD       1             XXCER#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXCER#
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   CLEAR                   PCE01
     C                   Z-ADD     WVCO#         CECO#
     C                   Z-ADD     WVFND         CEFND
     C                   Z-ADD     WVFYR         CEFYR
     C                   Z-ADD     WVEMP#        CEEMP#
     C                   Z-ADD     WVDIV         CEDIV
     C                   Z-ADD     XXCER#        CECER#
     C                   MOVEL(P)  WVNAME        CENAME
     C                   MOVEL(P)  WVADR1        CEADR1
     C                   MOVEL(P)  WVADR2        CEADR2
     C                   MOVEL(P)  WVCTY         CECTY
     C                   MOVEL(P)  WVST          CEST
     C                   Z-ADD     WVZIP         CEZIP
     C                   MOVE      'Y'           CERLFL
     C                   MOVEL     'A'           CESTAT
     C                   MOVEL     Q1USER        CEIUSR
     C                   MOVEL     Q1USER        CEUSER
     C                   MOVEL     Q1PGM         CEIPGM
     C                   MOVEL     Q1PGM         CEPGM
     C                   TIME                    DSDTTM
     C                   MOVEL     DSTIME        CEITM
     C                   MOVEL     DSTIME        CETIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CEIDT
     C                   MOVEL     SRDTO         CEDT
     C                   WRITE     PCE01
     C*
     C                   ENDSR                                                  SRCERT
      /Free
       //===================================================
       Begsr WrtTxBlkWvr;
       //===================================================
        scBlkPrjDs = curTxBlkWvrDsc;
        scFChg = curTxWvrPrem;
        scSWPc = curTxPctPrem;
        scAPre = curTxAdvPrem;
        // Fields needed for CkChgs called in Writer
        scOoDiv# = ELdiv;
        xxScr# = 1;
        // Get next waiver #
        Setgt wvkY1 pwv02;
        ReadPe wvky1 pwv02;
        If Not %Eof;
          xxWav# = WVwav# + 1;
        Else;
          xxWav# = 1;
        Endif;
        scWav# = xxWav#;
        Exsr Writer;
       Endsr;
       //===================================================
       Begsr ChgTxBlkWvr;
       //===================================================
        // Save old values
        s#Wav# = WVwav#;
        s#BlkPrjDs = WVblkPrjDs;
        s#FChg = WVfchg;  // TX Prem
        s#SWPc = WVswpc;  // TX Pct
        s#APre = WVapre;  // TX Adv Prem
        // Put current values in screen fields
        scFChg = curTxWvrPrem;
        scSWPc = curTxPctPrem;
        scAPre = curTxAdvPrem;
        scBlkPrjDs = curTxBlkWvrDsc;
        // Prevents eff date from being cleared.
        scWEfD = %Dec(%Date(WVefdt:*Cymd):*Usa);
        // Save off current values
        Exsr UpDatR;
       Endsr;
       //===================================================
       Begsr DltTxBlkWvr;
       //===================================================
        Exsr PreDlt;
        If hasOtherRcdsForEnd# = *Off
         And hasOtherRcdsForForm = *Off;
           Chain (ELco#:ELfnd:ELemp#:ELfyr:ldWaiverFrm) pfm12;
           Delete pfm12;
        Endif;
       Endsr;
**
Estimated
Audited
Charge per Waiver
Specific Waiver %
Minimum premium
Advance premium
