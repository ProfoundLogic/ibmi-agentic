     Hcopyright('(c) 2024 Sapiens, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8070R6 - EDI/NOI DATA ELEMENT ARRAY LOADER
     H*
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*************************************************************************
     H* REVISIONS:                                                            *
     H*                                                                       *
     H* 01/08/25 C:079 Support 133/118287                                     *
     H*                For AL format contact name differently                 *
     H*                                                                       *
     H* 12/11/24 C:079 Support 102/117947                                     *
     H*                When reporting DN(134) AWW compare to state max/min    *
     H*                                                                       *
     H* 05/23/24 C:053 Support 146/115613                                     *
     H*                Add check for zero division entry.                     *
     H*                                                                       *
     H* 03/29/24 C:053 Support 146/115213                                     *
     H*                Add processing for ASO (a version of PEO)              *
     H*                Prescient: ASO is setup as a PEO group with            *
     H*                           Special handling.                           *
     H*                                                                       *
     H* 03/19/24 C:053 Support 146/115213                                     *
     H*                remove 109011                                          *
     H*                                                                       *
     H* 05/25/23 C:053 Support 146/109011                                     *
     H*                Update PEO processing.                                 *
     H*                   Mod for client 139 is obsolete with this change     *
     H*                   so removed.  Also, no longer a client.              *
     H*                Additional changes made as original PEO processing     *
     H*                   had issues.                                         *
     H*                                                                       *
     H* 04/29/22 C:079 Support: 146/101975                                    *
     H*                Additonal locations, check the reportable flag         *
     H*                                                                       *
     H* 01/27/23 C:053 Support 146/105992                                     *
     H*                DN134 correction.                                      *
     H*                                                                       *
     H* 01/26/23 C:079 Support 146/107040                                     *
     H*                Initialize zzStDt                                      *
     H*                                                                       *
     H* 02/14/22 C:079 Project: WC2797                                        *
     H*                IAIABC 3.1.4 new fields                                *
     H*                                                                       *
     H* 12/13/21 C:053 Support 146/97627                                      *
     H*                Load DN(159) from contacts file                        *
     H*                                                                       *
     H* 12/10/21 C:079 Support 146/97776                                      *
     H*                Load DN(18) using Fed Name then DBA if Fed Name blank  *
     H*                                                                       *
     H* 10/04/21 C:079 Support 96234                                          *
     H*                Dont load DN(134) from wage grid                       *
     H*                                                                       *
     H* 11/12/20 C:053 Support 92766                                          *
     H*                Prevent date error with added data check.              *
     H*                                                                       *
     H* 10/12/20 C:180 Support: 981091 Change AWW eff date to match date of   *
     H*                                Primary comp rate if there is Primary  *
     H*                                comp rate. Otherwise, leave as         *
     H*                                previously programmed.                 *
     H*
     H* 05/04/20 C:180 Support 87315                                          *
     H*                Check for IP comp rate                                 *
     H*                                                                       *
     H* 04/03/20 C:062 Project WC2730                                         *
     H*                -Populate DN(438) with collective bargain.             *
     H*                -Populate DN(249) with added accident premises
     H*                                                                       *
     H* 01/07/20 C:053 Ticket 86297                                           *
     H*                Condition CMPIMP on CLT152 (Temporary patch)           *
     H*                                                                       *
     H* 01/06/20 C:062 Ticket 86069(152)                                      *
     H*                Populate Employer Address with Group info              *
     H*                                                                       *
     H* 09/07/19 C:062 Project 152WAU                                         *
     H*                Populate CMPDS#.                                       *
     H*                                                                       *
     H* 02/19/19 C:053 Support PHW77111                                       *
     H*                Do not fill with zeros                                 *
     H*                                                                       *
     H* 01/09/19 C:180 Project: 146WIS                                        *
     H*                Allow x-ref of marital status for SCS client.          *
     H*                                                                       *
     H* 11/21/18 C:180 Support 74902                                          *
     h*                Fix initial RTWQ                                       *
     h*                                                                       *
     H* 05/30/18 C:274 Project: WC2515                                        *
     H*                Add code for EDI 3.1                                   *
     H*                                                                       *
     H* 04/06/17 C:053 Support PHW46022                                       *
     H*                Update claim with calculated status if applicable      *
     H*                                                                       *
     H* 03/16/17 C:306 Support PHW46022                                       *
     H*                Calculate the value and populate the New field EDI     *
     H*                Claim Type codes.                                      *
     H*                                                                       *
     H* 01/31/17 C:306 PROJECT: WC2410                                        *
     H*                Load the DN array with CMINJS,EMSEMP,CMCLSB            *
     H*                 229.  Injury Severity Type code                       *
     H*                 230.  Employer ID Assnd By Jurisdiction               *
     H*                 231.  Manual Class sub Code.                          *
     H*                                                                       *
     H*                                                                       *
     H* 05/29/14 C:180 SUPPORT WC114 85700                                    *
     H*                Fix denial MTC for PA                                  *
     H*                                                                       *
     H* 05/08/14 C:180 SUPPORT: WC126 85390                                   *
     H*                Left justify SSN                                       *
     H*                                                                       *
     H* 02/26/14 C:190 PROJECT: WC2201 PRISS 13                               *
     H*                Add an edit to load xxCont if xxPcNt is blank.         *
     H*                                                                       *
     H* 02/12/14 C:180 SUPPORT PRISS 027 - WC2234 -
     H*                Fix Work Days Schedule Code                            *
     H*                                                                       *
     H* 01/21/14 C:190 PROJECT: WC2201                                        *
     H*                Changes made to select correct employer contact        *
     H*                based on the selection made on the radio buttons.      *
     H*                                                                       *
     H* 12/12/13 C:287 PROJECT: WC2234                                        *
     H*                Added fields CMPSPA, CMWWTC, CMWDSC, and CMESID from   *
     H*                file WMCMP.                                            *
     H*                                                                       *
     H* 10/15/13 C:180  SUPPORT WC114 83566 -
     H*                 Set release flag.
     H*
     H* 09/20/13 C:180  SUPPORT WC114 83566
     H*                 Fix claim type for PA.
     H*
     H* 04/23/13 C:180  SUPPORT WC146 82143
     H*                 - Add population restriction for DN(74) in NC.
     H*
     H* 04/11/13 C:53  Redmine 2707
     H*                Correct Employer Contact
     H*                 (temp fix until project replaces)
     H*
     H* 04/08/13 C:190 PROJECT 147WBF
     H*                Add a condition that will populate DN,15 with CMPDS#
     H*                if FNGRPASC = 'OPTACOMP' and CMPDS# <>  *blanks.
     H*
     H* 10/02/12 C:180 SUPPORT WC146 80563
     H*                Fix claim type for GA.
     H*
     H* 09/25/12 C:180 SUPPORT WC146 80824
     H*                Fix claim status for GA.
     H*
     H* 04/20/12 C:279 SUPPORT WC136  79265
     H*                Modified to add zeros to the right of DN(25)
     H*                for 'CA' state.
     H*
     H* 01/10/12 C:062 SUPPORT WC114 78690
     H*                PASS SETTLEMENT DATE EVEN IF NO SETTLEMENT DATE
     H*                FOR WV.
     H*
     H* 01/18/11 C:180 SUPPORT 72259 - ADDITIONAL CHANGES
     H*                FOR R1, DO NOT REPORT TPA FEIN/NAME WHEN
     H*                INSURER FEIN MATCHES TPA FEIN.
     H*
     H*
     H* 12/27/10 C:180 SUPPORT 75259
     H*                For R1, check also if TPA info is in WDF3P.
     H*
     H* 11/10/10 C:220 PROJECT WC1933 / PRISS#16
     H*                - DELETED below changes per MF.
     H*
     H* 10/29/10 C:220 PROJECT WC1933 / PRISS#16
     H*                - Additional change for DN,43 load.
     H*                Do not load DN,43 if EDI release is 3.
     H*
     H* 09/06/10 C:220 PROJECT WC1933
     H*                Extract/remove the suffix from the Employee
     H*                last name filed (DN, 43).
     H*
     H* 05/03/10 C:190 PROJECT WC1859 - FROI Late Reason code.
     H*
     H* 09/24/09 C:062 PROJECT 139WMO
     H*                POPULATE DN,74 WITH CMETYP
     H*
     H* 09/21/09 C:180 SUPPORT 69773
     H*                INCORRECT FEIN
     H*
     H* 08/17/09 C:189 SUPPORT 68639
     H*
     H* 07/15/09 C:180 PROJECT WC1836 - Populate accident county.
     H*
     H* 06/17/09 C:189 SUPPORT 68638
     H*                All policies with FNPEOF = 'S' only will be treated
     H*                as PEO .
     H*
     H* 06/09/09 C:220 SUPPORT 68601
     H*                GA is not accepting   "became lost time"
     H*
     H* 05/04/09 C:62  PROJECT WC1807
     H*                Populate qualifier segments.
     H*
     H* 04/23/09 C:180 SUPPORT 68102, 68103
     H*                Fix bug that was clearing RTW date.
     H*
     H*
     H* 03/31/09 C:189 SUPPORT 67555
     H*                Do not clear RTW when MTC is CB.
     H*
     H* 03/21/09 C:189 SUPPORT 67378
     H*                   Dont pick the records with dependent
     H*                   relation as '9' and incative
     H*
     H* 03/19/09 C:189 SUPPORT: 67660
     F*                  Added field for the support item to address
     F*                  the issue with accident location
     H*
     H* 02/10/09 C:53  SUPPORT 67056
     H*                NM does not accept the "L" claim type.
     H*
     H*
     H* 02/10/09 C:53  SUPPORT 67056
     H*                NM does not accept the "L" claim type.
     H*
     H* 01/15/09 C:53  SUPPORT 66117
     H*                Add new claim type determination for R3
     H*
     H* 01/14/09 C:53  SUPPORT 66375
     H*                Remove qualified load of DN#144 & 145
     H*
     H* 01/13/09 C:53  SUPPORT 66233
     H*                Changes for Insured type code.
     H*
     H* 12/02/08 C:53  SUPPORT 66185
     H*                Changes for init/cur RTW.
     H*
     H* 11/19/08 C:53  SUPPORT 65746
     H*                Changes for method of settlment from table.
     H*
     H* 11/17/08 C:53  SUPPORT 63706
     H*                Changes for claim admin & insurer info
     H*                Changes for country code
     H*                Changes for PEO processing
     H*                Changes for ALT POSTAL CODE
     H*
     H* 11/05/08 C:53  SUPPORT 65863
     H*                Correct the population of DN#137, 138, 139 & 140
     H*
     H*
     H* 10/30/08 C:180 SUPPORT 65671, 65757
     H*                Populate ID other than SSN (DN0152, 153, 154 &
     H*                156).
     H*
     H*                Populate Reduced Benef Amt Code (DN0202)
     H*
     H* 10/20/08 C:180 SUPPORT 65603
     H*                Populate date of knowledge of disability.
     H*
     H* 10/16/08 C:53  SUPPORT 65595
     H*                Correct Physical restrictions
     H*
     H* 10/14/08 C:180 SUPPORT 65366
     H*                Loss cvg code for rel 3.0
     H*
     H* 10/08/08 C:180 SUPPORT 65408
     H*          Fix population of Insurer type.
     H*
     H* 09/03/08 C:53 Support: 64912                                  *
     H*              -Do not load "B" in XXCLMT (claim type) for "NM" *
     H*                                                               *
     H* 07/30/08 C:180 Project: 135WLG                                *
     H*                ROES Auto triggers.
     H*
     H* 04/24/08 C:183 PROJECT :WC1637                                *
     H*          When loading data elements 10 - 14, Check            *
     H*          for Office Information first and if no               *
     H*          information exists, load the elements from           *
     H*          Company Information.                                 *
     H*                                                               *
     H* 10/19/07 C:191 PROJECT :WC1584                                *
     H*          LOAD THE DATA ELEMENTS FOR R22 FORMAT FOR EDI        *
     H*          RELEASE 3.                                           *
     H*                                                               *
     H* 06/20/07 C:53 114WMF  BACK TO BASE-114WJD, 46976, 46689       *
     H*                                                               *
     H* 03/15/05 C:15 Project: 114WJE                                 *
     H*              -Do not use the location name and address.       *
     H*              -Use CMASCL instead of CMACL#.                   *
     H* 05/12/05 C:53 Support: 46976                                  *
     H*              -Do not load DN,68 for PA                        *
     H* 05/02/05 C:53 Support: 46689                                  *
     H*               Added PA specific change.                       *
     H*                                                               *
     H* 05/01/07 C:185 Project: WC1177                                *
     H*                Load the data elements for R21 Format for      *
     H*                Release 3.                                     *
     H*                                                               *
     H* 03/29/07 C:15 Support: WC129/56998                            *
     H*               Fixed code that loads the employer name and     *
     H*               address.                                        *
     H*                                                               *
     H* 12/11/06 C:15 Support: WC114/56071                            *
     H*               Change pgm to default the # of days worked (64) *
     H*               to 5 if CMDAYS = 0.                             *
     H*                                                               *
     H* 09/21/06 C:53 Support: 54917                                  *
     H*               Qualify the addition of SC to SIC code.         *
     H*                                                               *
     H* 07/05/06 C:15 Project: 71WHK                                  *
     H*               Added code to use COCHNM for DN,9 for clt071.   *
     H*                                                               *
     H* 04/24/06 C:53 Support: 52440                                  *
     H*               Remove EMDBA from processing as it is no longer *
     H*               used in BASE.                                   *
     H*                                                               *
     H* 02/16/06 C:15 Support:                                        *
     H*               Per ROES, the SIC code should include 'SC' in   *
     H*               the last 2 digits when using the SIC code.  If  *
     H*               using the NAISC code, it will fill all 6 digits.*
     H*                                                               *
     H* 02/15/06 C:15 Project: 135WFV                                 *
     H*               Per Jim Stanco, removed this mod.               *
     H*                                                               *
     H* 02/10/06 C:15 Support: WC135/51396                            *
     H*               Per ROES, DN10 - DN14 should be loaded with the *
     H*               insurer info if the TPA flag = 'N'.             *
     H*                                                               *
     H* 01/11/06 C:15 Project: 135WFV                                 *
     H*               Use the COFEIN instead of F2FEIN.  This change  *
     H*               will only impact clt135.                        *
     H*                                                               *
     H* 10/08/04 C:53 Support: WC055/44145                            *
     H*               Load element 8 W/blanks if state = NE.          *
     H*                                                               *
     H* 07/01/03 C:53 Project: WC1134                                 *
     H*               Comments only                                   *
     H*                                                               *
     H* 03/12/03 C:53 Support: WC055/36107                            *
     H*               Load elements 10,11,12,13,04 if TPA NOT = 'Y'   *
     H*               and state is NE.                                *
     H*                                                               *
     H* 07/10/02 C:53 Project: WC468                                  *
     H*               Changed employer work field from 5 to 9         *
     H*                                                               *
     H* 02/12/01 C:15 Support: WC065/26323                            *
     H*               Only load the RTW qualifier if the RTW date is  *
     H*               loaded.                                         *
     H*                                                               *
     H* 12/05/00 C:15 Project: 25WIZ                                  *
     H*               Changed the usage of the Dt of disability and   *
     H*               the RTW date, per state requirements.  1) The   *
     H*               RTW date should be sent as 0 if the claimant is *
     H*               within a period of lost time.  2) The Dt of     *
     H*               disability should always be the first loss dt.  *
     H*                                                               *
     H* 08/29/00 C:15 Support: WC096/23983                            *
     H*               Added code to load the insurer zip code into    *
     H*               DN,14 if the TPA flag = 'N'.                    *
     H*                                                               *
     H* 06/06/00 C:15 Project: WC468                                  *
     H*               Chgd 'Claim type' calculation (DN,74).          *
     H*                                                               *
     H* 04/03/00 C:118 Project: WC854                                 *
     H*                Change code that compares CMIPCT <> 0 to       *
     H*                compare CMIMP=Y.                               *
     H*                                                               *
     H* 02/18/00 C:15 Project: WC976                                  *
     H*               Chgd pgm to load the TPA fields (DN 8-14) only  *
     H*               if F2TPA = 'Y'.                                 *
     H*                                                               *
     H* 12/16/99 C:15 Project: WC468                                  *
     H*               Chgd pgm to retrive the Insurer FEIN and name   *
     H*               (DN 6 & 7) from the Group Aux FEIN and Group    *
     H*               name fields, respectively.                      *
     H*                                                               *
     H* 03/30/99 C:15 Project: (changes per Barb Pepper)              *
     H*              -Load DN,72 based on the RTW qualifier.          *
     H*              -When loading DN,63 for the A49, default to '1'  *
     H*               if the value is not '1' or '4'.                 *
     H*                                                               *
     H* 03/25/99 C:58 Project: 25WIZ                                  *
     H*               Retrieve CMTRW and CMDBDT from WMWHP
     H*                                                               *
     H*  03/23/99 C:15 Project: WC468                                 *
     H*                Added code to load the PTD totals for DN,81    *
     H*                and the Death Dependents for DN,82.            *
     H*                                                               *
     H* 09/28/98 C:15 Project: WC867                                  *
     H*               Changed field which is loaded into 'Agree to    *
     H*               Compensate Code' (DN75).                        *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 11 - Chain indicator on log file.                             *
     H* 12 - Chain indicator on all other data base files.            *
     H* LR - Indicates when program is to be ended with all files     *
     H*      closed.                                                  *
     H*****************************************************************
     H*
     FSMCOL     IF   E           K DISK
     FWMCML1    UF   E           K DISK
     FWACML     IF   E           K DISK
     F                                     RENAME(PCM01:XCM01)
     FWMCHL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMALL1    IF   E           K DISK
     FWDACL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMLLL     IF   E           K DISK
     FWMBYL     IF   E           K DISK
     FWMBNL     IF   E           K DISK
     FWMTHL     IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     RENAME(PPA01:PPA02)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     FWMLGL4    IF   E           K DISK
     FWMIXL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWMWHL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FWDRBL     IF   E           K DISK
     FWMAWL1    IF   E           K DISK
     FWMAWL6    IF   E           K DISK    RENAME(PAW01:PAW06)
     FWDF3L     IF   E           K DISK
     FWMECL1    IF   E           K DISK
     FWMEXL     IF   E           K DISK    rename(PEX01:PEX01)
     F*
     FWMAWL5    IF   E           K DISK    rename(PAW01:PAW05)
     D XXRLSA          S              1A
     D DN              S            150    DIM(999)                             Data elements
     D PCD             S              3    DIM(10)                              Data elements
     D PTD             S              3    DIM(25)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  XCAZIP                 1      9
     D  XCAZP1                 1      5  0
     D  XCAZP2                 6      9  0
     D                 DS
     D  XXCF10                 1     10
     D  XXCF01                 1      2
     D  XXCF03                 3      4
     D                 DS
     D  XXKEY                  1     10
     D  XXACST                 1      2
     D  XXACCY                 3      4
     D                 DS
     D  PAEDIPWK               1      3
     D  PAEDIPWK2              1      2
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     DxxPcNt           s              1
     d*** Beg Add *** 02/14/22 *********************************************
     d cvtDate         pr                  ExtPgm('SRCVTDAT')
     d  inDate                        8  0
     d  inFormat                      8
     d  outFormat                     8
     d  outDate                      10
     d  outError                      1
     d  inDate         s              8  0
     d  inFormat       s              8
     d  outFormat      s              8
     d  outDate        s             10
     d  outError       s              1
     d*** End Add *** 02/14/22 *********************************************
     C*
     C/EJECT
     C*
     C*--------  MAINLINE -----------------------------------
     C*
     C*   1. Retrieve the log record using the relative record # passed
     C*      to the program.
     C*   2. If retrieval was successful, load the DN (Data Element
     C*      Number) array and pass it back to the caller:
     C*
     C*       o  First, retrieve all of the required records from
     C*          the claim data base.
     C*
     C                   EXSR      SRLDB
     C*
     C*       o  Load the DN array with values from the claim data
     C*          base.
     C*
     C                   EXSR      SRLDN
     C*
     C*
     C     IXRLS         IFEQ      '3'
     C*
     C*       o  Load DN array for R21 format
     C                   EXSR      SRR21
     C*       o  Load DN array for R22 format
     C                   EXSR      SRR22
     C*
     C                   ENDIF
     C*
     C*
     C*       o  Return the DN array to the caller.
     C*
     C                   MOVE      '1'           *INLR
     C/EJECT
     C**************************************************************************
     C*
     C*          ROUTINE - *INZSR
     C*
     C*   Initiallization routine.
     C*          This routine ensures the following steps are done:
     C*          1. All work fields, parm lists, key lists, and
     C*             data structures are defined and initialized.
     C*
     C     *INZSR        BEGSR
     C*
     C                   CLEAR                   DN
     C*
     C*   Define work fields
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMACDT        XXEXDT
     C*
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    ALELID        XXELID
     C     *LIKE         DEFINE    LRLRCD        XXLRCD
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTFG03        XXFG03
     C     *LIKE         DEFINE    WHDATE        XXWKDT
     C     *LIKE         DEFINE    *IN60         LFLAG
     C     *LIKE         DEFINE    *IN60         RFLAG
     C     *LIKE         DEFINE    CMDBDT        XXDBDT
     C     *LIKE         DEFINE    EMNAME        INSNAM
     C     *LIKE         DEFINE    EMFEIN        INSFEI
     C     *LIKE         DEFINE    EMMAD1        INMAD1
     C     *LIKE         DEFINE    EMMAD2        INMAD2
     C     *LIKE         DEFINE    EMMCTY        INMCTY
     C     *LIKE         DEFINE    EMMST         INMST
     C     *LIKE         DEFINE    EMMZIP        INMZIP
     C     *LIKE         DEFINE    WHDATE        XXCRTW                         CURENT RTW
     C     *LIKE         DEFINE    PATRDT        ZZSTDT
     C*** Beg Add *** 05/23/24 ******************************************   ****
     C     *LIKE         DEFINE    CMDIV         XXDIVZ
     C*** End Add *** 05/23/24 ******************************************   ****
     C*
     C                   MOVE      *BLANKS       XCACLC           25
     C                   MOVE      *BLANKS       XCPOLN           25
     C                   MOVE      *BLANKS       XCPHN#           10
     C                   MOVE      *BLANKS       XCFAX#           10
     C                   MOVE      *BLANKS       XCFND             3
     C                   MOVE      *BLANKS       XCEMP#            9
     C                   MOVE      *BLANKS       XCCASE            7
     C                   MOVE      *BLANKS       XCFYR             3
     C                   MOVE      *BLANKS       XCDIV             3
     C                   MOVE      *BLANKS       XXDEP             2 0
     C                   MOVE      *BLANKS       XXCLMS            1
     C                   MOVE      *BLANKS       XXCLMT            1
     C                   MOVE      *BLANKS       XXDAT8            8
     C                   MOVE      *ZEROS        P                 5 0
     C                   MOVE      *ZEROS        XXPI              2 0
     C                   MOVE      *ZEROS        XXPA              2 0
     C                   MOVE      *ZEROS        XXBA              2 0
     C                   MOVE      *ZEROS        XXPD              2 0
     C                   MOVE      *ZEROS        XXDP              2 0
     C                   CLEAR                   PCD
     C*
     C*   Define parm lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQEXDT            8
     C                   PARM                    QQCO#             3            Key info
     C                   PARM                    QQFND             3            Key info
     C                   PARM                    QQFYR             3            Key info
     C                   PARM                    QQCASE            7            Key info
     C                   PARM                    QQTSET            3            Req data
     C                   PARM                    QQMTC             2            Req data
     C                   PARM                    QQJST             2            Req data
     C                   PARM                    DN
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      *
     C                   MOVE      QQEXDT        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C                   MOVE      QQCO#         XXCO#                          Key info
     C                   MOVE      QQFND         XXFND                          Key info
     C                   MOVE      QQFYR         XXFYR                          Key info
     C                   MOVE      QQCASE        XXCASE                         Key info
     C                   MOVEL     QQTSET        XXTSET            3            Req data
     C                   MOVEL     *BLANKS       X1TSET            3            Req data
     C                   MOVEL     QQMTC         XXMTC             2            Req data
     C                   MOVEL     QQJST         XXJST             2            Req data
     C*
     C*   Define key lists
     C*
     C*          SMCOL - Company Master
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*          WMCML1 - Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*     WMLLP - Legal Master
     C*
     C     LLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*          WMCHL - Claim Status History
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*          WMFNL - Fund Master
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*          WMEML - Employer Master
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*          WDELL - Employer Detail File
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*          WMALL1 - Additional Entity/Location File
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXELID
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*          WMCDL - Employee Detail File
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*
     C*          WDACL - Aux Compensation Payee File
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*          WDPAL - Payment Detail File
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*          WMBNL - Benefits Adjustment File
     C*
     C     BNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*          WMBYL - Additional Body Parts File
     C*
     C     BYKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*          WTABL - TABLE FILE KEY LIST
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  Key list for the Generic Table file
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*  Key list for the Work History file
     C*
     C     WHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXWKDT
     C*                                                                *
     C*
     C     WHKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*  Key List for EDI Log File
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTSET
     C*
     C*  Key List for EDI CLAIMS CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    X1TSET
     C*
     C     RBKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMJRST
     C                   KFLD                    CMACDT
     C*
     C     RBKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMJRST
     C*
     C     AWKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXTRDT            7 0
     C                   KFLD                    XXTIME            6 0
     C*
     C     AWKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     AJKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     E3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     R3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     OFKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMOFF#
     C* RETREIVE OVERRIDE INFO FOR CLAIM ADMIN & INSURER INFO
     C     F3KY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXJRST            2
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPCNT
     C*** Beg Add *** 05/23/24 ******************************************   ****
     C*
     C     ECKYZ         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIVZ
     C                   KFLD                    XXPCNT
     C*** End Add *** 05/23/24 ******************************************   ****
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    ##CLT             3
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C*
     C*   ROUTINE - SRLDB
     C*
     C     SRLDB         BEGSR
     C*
     C*
     C*   Retrieve Company Master
     C*
     C     COKY          CHAIN     PCO01                              12
     C     *IN12         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   MOVE      *BLANKS       COADR1
     C                   MOVE      *BLANKS       COADR2
     C                   MOVE      *BLANKS       COCTY
     C                   MOVE      *BLANKS       COST
     C                   MOVE      *ZEROS        COZIP
     C                   MOVE      *ZEROS        COFEIN
     C                   ENDIF
     C*
     C*   Retrieve Fund Information
     C*
     C     FNKY          CHAIN     PFN01                              12
     C     *IN12         IFEQ      *ON
     C                   ENDIF
     C*
     C*   Retrieve Fund Information
     C*
     C     FNKY          CHAIN     PF201                              12
     C     *IN12         IFEQ      *ON
     C                   CLEAR                   PF201
     C                   ENDIF
     C*
     C*   Retrieve Release Information
     C     IXKY          CHAIN     PIX01                              60
     c*
     C                   Z-ADD     0             XXDBDT
     C*   EDI 3.1 --> IXRLS = 'A'
      /Free
       If IXRLS = 'A';
          XXRLSA = *ON;
          IXRLS = '3';
       Endif;
      /End-Free
     C*
     C*   Retrieve Claim Master
     C*
     C     CMKY          CHAIN     PCM01                              12
     C     *IN12         IFEQ      *OFF
     C                   MOVE      'Y'           XXUPDC            1
     C                   ENDIF
     C*
     C     *IN12         IFEQ      *ON
     C     CMKY          CHAIN     XCM01                              12
     C                   ENDIF
     C     *IN12         IFEQ      *ON
     C                   CLEAR                   PCM01
     C                   CLEAR                   XCM01
     C*
     C*
     C                   ELSE
     C*
     C     CMPREX        IFNE      'Y'
     C                   MOVE      'N'           CMPREX
     C                   ENDIF
     C     CMFSAL        IFNE      'Y'
     C                   MOVE      'N'           CMFSAL
     C                   ENDIF
     C*
     C*   Check Work history file for current dates
     C                   Z-ADD     *HIVAL        XXWKDT
     C                   MOVE      *OFF          LFLAG
     C                   MOVE      *OFF          RFLAG
     C                   MOVE      *BLANKS       XXFG03
     C                   MOVE      *ZEROS        XXCRTW
     C                   MOVE      *ZEROS        XXDBDT
     C     WHKY          SETGT     PWH01
     C     WHKY2         READPE    PWH01                                  17
     C*
     C*   Use the latest 'Left work' and 'Returned' dates
     C     *IN17         DOWEQ     *OFF
     C*
     C     WHWKST        IFEQ      'L'
     C     LFLAG         ANDEQ     *OFF
     C                   Z-ADD     WHDATE        XXDBDT
     C                   MOVE      *ON           LFLAG
     C                   ENDIF
     C*
     C     WHWKST        IFEQ      'R'
     C     RFLAG         ANDEQ     *OFF
     C     WHWKST        OREQ      'E'
     C     RFLAG         ANDEQ     *OFF
     C                   Z-ADD     WHDATE        XXCRTW
     C                   MOVE      *ON           RFLAG
     C                   MOVEL(P)  'RTWQ'        XXTBID
     C                   MOVEL(P)  WHRTWQ        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTFG03        XXFG03
     C                   ENDIF
     C                   ENDIF
     C*
     C     LFLAG         IFEQ      *ON
     C     RFLAG         ANDEQ     *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     WHKY2         READPE    PWH01                                  17
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*  If the claimant started lossing time again (ie. the dt of disability
     C*    is > the return to work dt), the RTW date sent should be 0, unless
     C*    the MTC code = 'S1'.
     C*
     C     XXDBDT        IFGT      XXCRTW
     C     QQMTC         ANDNE     'S1'
     C     QQMTC         ANDNE     'CB'
     C     XXCRTW        ANDGT     *ZERO
     C                   Z-ADD     0             CMRTW
     C                   MOVE      *BLANKS       CMRTWQ
     C                   ENDIF
     C*
     C*   Retrieve Claimant Demographics
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              12
     C     *IN12         IFEQ      *ON
     C                   CLEAR                   PCD01
     C                   ENDIF
     C*
     C*   Retrieve Claim Legal Data
     C*
     C     LLKY          CHAIN     PLL01                              60
     C     *IN12         IFEQ      *ON
     C                   CLEAR                   PLL01
     C                   ENDIF
     C*
     C*   Retrieve the Employer Master Record
     C*
     C* PEO INSURED INFO
     C*
     C                   MOVE      *BLANKS       INSNAM
     C                   MOVE      *BLANKS       INSFEI
     C*
     C*** beg chg *** 05/25/23 *************************************
     C*****FNPEOF ****** IFEQ      'S'
     C     FNPEOF        IFNE      ' '
     C*
     C*** Beg Add *** 03/29/24 *********************************************
     C*** Set ASO flag
     C     FNASOF        IFEQ      'Y'
     C                   MOVE      'Y'           XXASOF            1
     C                   ELSE
     C                   MOVE      *BLANK        XXASOF
     C                   ENDIF
     C*** End Add *** 03/29/24 *********************************************
     C*
     C*** end chg *** 05/25/23 *************************************
     C*** beg del *** 05/25/23 *************************************
     C*****FNPEOF ****** OREQ      'G'
     C*****##CLT  ****** ANDEQ     '139'
     C*** end del *** 05/25/23 *************************************
     C                   Z-ADD     1             XXEMP#
     C                   Z-ADD     *ZEROS        XXDIV
     C     EMKY          CHAIN     PEM01                              12
     C     *IN12         IFEQ      *OFF
     C                   MOVEL     EMNAME        INSNAM
     C                   MOVE      EMFEIN        INSFEI
     C     EMMAD1        IFNE      *BLANKS
     C                   MOVE      EMMAD1        INMAD1
     C                   MOVE      EMMAD2        INMAD2
     C                   MOVE      EMMCTY        INMCTY
     C                   MOVE      EMMST         INMST
     C                   MOVE      EMMZIP        INMZIP
     C                   ELSE
     C                   MOVE      EMPAD1        INMAD1
     C                   MOVE      EMPAD2        INMAD2
     C                   MOVE      EMPCTY        INMCTY
     C                   MOVE      EMPST         INMST
     C                   MOVE      EMPZIP        INMZIP
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C     EMKY          CHAIN     PEM01                              12
     C*
     C     *IN12         IFEQ      *ON
     C                   MOVE      *ZEROS        EMEMP#
     C                   MOVE      *ZEROS        EMDIV
     C                   MOVE      *ZEROS        EMFEIN
     C                   MOVE      *BLANKS       EMNAME
     C                   MOVE      *BLANKS       EMMAD1
     C                   MOVE      *BLANKS       EMMAD2
     C                   MOVE      *BLANKS       EMMCTY
     C                   MOVE      *BLANKS       EMMST
     C                   MOVE      *ZEROS        EMMZIP
     C                   MOVE      *BLANKS       EMPAD1
     C                   MOVE      *BLANKS       EMPAD2
     C                   MOVE      *BLANKS       EMPCTY
     C                   MOVE      *BLANKS       EMPST
     C                   MOVE      *ZEROS        EMPZIP
     C                   MOVE      *BLANKS       EMSIC
     C                   ENDIF
     C*
     C*   Retrieve the Employer Detail Record
     C*
     C     ELKY          CHAIN     PEL01                              12
     C*
     C     *IN12         IFEQ      *ON
     C                   MOVE      *ZEROS        ELFND
     C                   MOVE      *ZEROS        ELEMP#
     C                   MOVE      *ZEROS        ELFYR
     C                   MOVE      *ZEROS        ELPFDT
     C                   MOVE      *ZEROS        ELPTDT
     C                   ENDIF
     C                   MOVE      *blanks       XXCONT           47
       //*** Beg Add *** 12/13/21 **********************************************
     C                   MOVE      *blanks       XXCBPH           15
       //*** End Add *** 12/13/21 **********************************************
      /Free
       chain (xxCO#:xxFnd) PEX01;
       xxPcNt = EXecnt;
       If xxPcNt <> ' ';
      /End-free
     C     ECKY          CHAIN     PEC01                              12
     C*** Beg Add *** 05/23/24 ******************************************   ****
     C*** No hit. Check for zero division entry.
     C     *IN12         IFEQ      *ON
     C                   Z-ADD     *ZEROS        XXDIVZ
     C     ECKYZ         CHAIN     PEC01                              12
     C                   ENDIF
     C*** End Add *** 05/23/24 ******************************************   ****
     C     *IN12         IFEQ      *OFF
       //*** Beg Chg *** 01/08/25 ******************************************
     C**** XXCONT        CAT       ECLSN:0       XXCONT
     C**** XXCONT        CAT       ',':0         XXCONT
     C**** XXCONT        CAT       ECFSN:1       XXCONT
       Select;
       When CMjrSt = 'AL';
         xxCont = %Trim(ECfsn) + ' ' + %Trim(EClsn);
       Other;
         xxCont = %Trim(EClsn) + ', ' + %Trim(ECfsn);
       Endsl;
       //*** End Chg *** 01/08/25 ******************************************
       //*** Beg Add *** 12/13/21 **********************************************
     C                   MOVEL     ECAREA        XXAREA
     C                   MOVEL     ECPHN#        XXPHN#
     C     XXAREA        CAT       XXPHN#        XXCBPH
       //*** End Add *** 12/13/21 **********************************************
     C                   ENDIF
      /Free
       Endif;
      /End-free
     C*
     C*   Retrieve the Address Location Record
     C*
     C                   MOVE      CMCTG1        XXELID
     C     ALKY          CHAIN     PAL01                              12
     C*
     C     *IN12         IFEQ      *ON
     C                   MOVE      *BLANKS       ALELID
     C                   MOVE      *BLANKS       ALNAME
     C                   MOVE      *BLANKS       ALDBA
     C                   MOVE      *BLANKS       ALFDNM
     C                   MOVE      *BLANKS       ALMAD1
     C                   MOVE      *BLANKS       ALMAD2
     C                   MOVE      *BLANKS       ALMCTY
     C                   MOVE      *BLANKS       ALMST
     C                   MOVE      *ZEROS        ALMZIP
     C                   MOVE      *BLANKS       ALPAD1
     C                   MOVE      *BLANKS       ALPAD2
     C                   MOVE      *BLANKS       ALPCTY
     C                   MOVE      *BLANKS       ALPST
     C                   MOVE      *ZEROS        ALPZIP
     C                   MOVE      *ZEROS        ALFEIN
     C                   ENDIF
     C*
     C*
     C*   Retrieve # of Dependents
     C*
     C     ACKY          CHAIN     PAC01                              12
     C     *IN12         DOWEQ     *OFF
     C     ACREL         IFNE      *BLANK
     C     ACREL         ANDNE     '9'
     C     ACSTAT        ANDNE     'I'
     C     CMDOD         ANDNE     *ZEROS
     C                   ADD       1             XXDP
     C                   ENDIF
     C     ACREL         IFEQ      '4'
     C     ACREL         OREQ      '7'
     C                   ADD       1             XXDEP
     C                   ENDIF
     C     ACKY          READE     PAC01                                  12
     C                   ENDDO
     C*
     C*
     C*   Determine Claim Type
     C                   MOVE      CMLTFL        XXCLMT
     C     CMLTFL        IFEQ      'R'
     C                   MOVE      'N'           XXCLMT
     C                   ENDIF                                                  E LTFL=R
     C*
     C     CMKY          SETGT     PTH01
     C     CMKY          READPE    PTH01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     XXJST         IFNE      'NM'
     C     THFROM        IFNE      *BLANK
     C     THTO          ANDEQ     'M'
     C***                  MOVE 'B'       XXCLMT
     C     XXJST         IFEQ      'GA'
     C                   MOVE      'I'           XXCLMT
     C                   ELSE
     C                   MOVE      'B'           XXCLMT
     C                   ENDIF
     C                   ENDIF                                                  E THTO=M
     C                   ENDIF
     C*
     C     THFROM        IFNE      *BLANK
     C     THTO          ANDEQ     'I'
     C                   MOVE      'L'           XXCLMT
     C* OVERRIDE FOR NM
     C     XXJST         IFEQ      'NM'
     C                   MOVE      'I'           XXCLMT
     C                   ENDIF
     C* OVERRIDE FOR GA
     C     XXJST         IFEQ      'GA'
     C                   MOVE      'I'           XXCLMT
     C                   ENDIF
     C                   ENDIF                                                  E THTO=I

     C     IXRLS         IFEQ      '3'
     C*
     C                   SELECT
     C*
     C     THFROM        WHENEQ    *BLANK
     C     THTO          ANDEQ     'R'
     C                   MOVE      'N'           XXCLMT
     C*
     C     THFROM        WHENEQ    *BLANK
     C     THTO          ANDEQ     'M'
     C                   MOVE      'M'           XXCLMT
     C*
     C     THFROM        WHENEQ    *BLANK
     C     THTO          ANDEQ     'I'
     C                   MOVE      'I'           XXCLMT
     C*
     C     THFROM        WHENEQ    'R'
     C     THTO          ANDEQ     'M'
     C                   MOVE      'M'           XXCLMT
     C*
     C     THFROM        WHENEQ    'R'
     C     THTO          ANDEQ     'I'
     C                   MOVE      'I'           XXCLMT
     C*
     C     THFROM        WHENEQ    'M'
     C     THTO          ANDEQ     'I'
     C                   MOVE      'L'           XXCLMT
     C*
     C* OVERRIDE FOR NC
     C     XXJST         IFEQ      'NC'
     C                   MOVE      'I'           XXCLMT
     C                   ENDIF

     C     THFROM        WHENEQ    'M'
     C     THTO          ANDEQ     'R'
     C                   MOVE      'N'           XXCLMT
     C*
     C     THFROM        WHENEQ    'L'
     C     THTO          ANDEQ     'M'
     C                   MOVE      'B'           XXCLMT
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  E *IN60=0
     C*
      /Free
                         IF XXJST = 'PA' and IXRLS = '3';
                            XXCLMT = CMLTFL;
                         ENDIF;
      /End-free
     C*
     C*   Substitute Nature, Part and Cause EDI codes from tables
     C*
     C*      Nature Code
     C     CMNATR        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       NAEDIN
     C                   ENDIF
     C                   ENDIF
     C*
     C*      Part Code
     C     CMPART        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMPART        XXKEY
     C     TBKY          CHAIN     PPA02                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       PAEDIP
     C                   ENDIF
     C                   ENDIF
     C*
     C*      Cause Code
     C     CMCAUS        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       CAEDIC
     C                   ENDIF
     C                   ENDIF
     C*
     C*          Determine Claim Status
     C*
     C                   MOVE      CMSTAT        XXCLMS
     C     CHKY          SETGT     PCH01
     C     CHKY          READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     CHOCST        IFEQ      'C'
     C     CHKY          READPE    PCH01                                  60
     C     CHKY          READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CMJRST        ANDNE     'GA'
     C                   MOVE      'X'           XXCLMS
     C                   ELSE
     C                   MOVE      'C'           XXCLMS
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXCLMS        IFEQ      'O'
     C     CHKY          READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'R'           XXCLMS
     C                   ELSE
     C                   MOVE      'O'           XXCLMS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   Get the Payment Late Reason Code AND # OF COMP PMTS
     C*
        //*** Beg Add *** 01/26/23 ******************************************
        Clear zzStDt;
        //*** End Add *** 01/26/23 ******************************************
     C                   MOVE      'N'           XXPROC            1
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C* GET THE TRANSACTION DATE WHICH WILL BE USED IN WV SETTLEMENT DT
     C*
     C     PARETP        IFEQ      'C'
     C     XXMTC         ANDEQ     'IP'
     C                   Z-ADD     PATRDT        ZZSTDT
     C                   ENDIF
     C*
     C     PARETP        IFEQ      'C'
     C     XXMTC         ORNE      'IP'
     C     XXPROC        IFEQ      'Y'
     C     XXMTC         ANDEQ     'IP'
     C                   LEAVE
     C                   END
     C                   MOVE      'Y'           XXPROC
     C*
     C*  SUMMARIZE THE PMTS BY PMT CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              62
     C     *IN62         IFEQ      '0'
     C     B2PYCD        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  'PADJ'        XXTBID
     C                   MOVEL(P)  B2PYCD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C     GTFG01        ANDEQ     ' '
     C     XXPA          ANDLT     10
     C*
     C                   Z-ADD     1             X                 3 0
     C     B2PYCD        LOOKUP    PCD(X)                                 61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    PCD(X)                                 61
     C                   MOVEL     B2PYCD        PCD(X)                          PMT CODE
     C                   ADD       1             XXPA
     C                   ENDIF
     C*
     C                   ENDIF                                                   GTFG01 = 1
     C*
     C                   MOVEL(P)  'PADJ'        XXTBID
     C                   MOVEL(P)  B2PYCD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C     GTFG01        ANDEQ     '2'
     C     XXPD          ANDLT     25
     C*
     C                   Z-ADD     1             X
     C     B2PYCD        LOOKUP    PTD(X)                                 61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    PCD(X)                                 61
     C                   MOVEL     B2PYCD        PTD(X)                          PTD CODE
     C                   ADD       1             XXPD
     C                   ENDIF
     C*
     C                   ENDIF                                                   GTFG01 = 2
     C*
     C*
     C                   END                                                       *IN62 = '0'
     C                   END                                                       PARETP = 'C'
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO
     C*
     C*
     C*   Retrieve # of Benefits Adjustments
     C*
     C     BNKY          CHAIN     PBN01                              12
     C     *IN12         DOWEQ     *OFF
     C     BNSTAT        IFEQ      'A'
     C                   ADD       1             XXBA
     C                   ENDIF
     C     XXBA          IFGE      10
     C                   LEAVE
     C                   ENDIF
     C     BNKY          READE     PBN01                                  12
     C                   ENDDO
     C*
     C*   Retrieve # of Additional Body Part Impairments
     C*
     C     CMIMP         IFEQ      'Y'
     C                   Z-ADD     1             XXPI
     C                   ENDIF
     C*
     C     BYKY          CHAIN     PBY01                              12
     C     *IN12         DOWEQ     *OFF
     C     BYSTAT        IFEQ      'A'
     C                   ADD       1             XXPI
     C                   ENDIF
     C     XXPI          IFGE      6
     C                   LEAVE
     C                   ENDIF
     C     BYKY          READE     PBY01                                  12
     C                   ENDDO
     C*
     C* RETREIVE OVERRIDE INFO FOR CLAIM ADMIN & INSURER INFO
     C*
     C*     - First try state table
     C                   MOVE      *OFF          F3RCD             1
     C                   MOVEL     CMJRST        XXJRST
     C     F3KY          CHAIN     PF301                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *ON           F3RCD
     C*
     C                   ELSE
     C                   MOVEL     *BLANKS       XXJRST
     C     F3KY          CHAIN     PF301                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *ON           F3RCD
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C*
     C*   Routine - SRLDN
     C*
     C*             This routine retrieves each data element being
     C*             edited and places the value in the appropriate
     C*             element in array DN.
     C*
     C     SRLDN         BEGSR
     C*
     C*          Load each element in DN with the appropriate data
     C*          base element.
     C*
     C*          - 1. Transaction Set ID
     C*
     C                   MOVEL     XXTSET        DN(1)
     C*
     C*          - 2. Maintenance Type Code
     C*
     C                   MOVEL     XXMTC         DN(2)
     C*
     C*
     C*  If jurisdiction state = 'PA', the MTC code must be
     C*  00, 02 or CO. Correct if an '04' comes in.
     C*
     C*
     C*          - 3. Maintenance Type Code Date
     C*
     C                   Z-ADD     XXEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(3)
     C*
     C*          - 4. Jurisdiction
     C*
     C                   MOVEL     XXJST         DN(4)
     C*
     C*          - 5. Agency Claim #
     C*
     C     ##CLT         IFEQ      '114'
     C                   MOVEL     CMASCL        DN(5)
     C                   ELSE
     C                   MOVEL     CMACL#        DN(5)
     C                   ENDIF
     C*
     C*          - 6. Insurer(Company) FEIN
     C*
     C     F3RCD         IFEQ      *ON
     C     F3IFEI        ANDNE     *ZEROS
     C                   MOVE      F3IFEI        DN(6)
     C                   MOVEL     F3INAM        DN(7)
     C                   ELSE
     C     F2FEIN        IFNE      *ZEROS
     C                   MOVE      F2FEIN        DN(6)
     C*          - 7. Insurer Name
     C                   MOVEL     FNNAME        DN(7)
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Only load the following fields if F2TPA = 'Y'.
     C*
     C     F2TPA         IFEQ      'Y'
     C*
     C*          - 8. Third Party Administrator FEIN
     C*
     C     COFEIN        IFNE      *ZEROS
     C                   MOVE      COFEIN        DN(8)
     C                   ENDIF
     C*
     C*          - 9. Third Party Administrator Name
     C*
     C                   MOVEL     CONAME        DN(9)
     C     ##CLT         IFEQ      '071'
     C                   MOVEL(P)  COSHNM        DN(9)
     C                   ENDIF
     C*
     C                   ELSE
     C     F3RCD         IFEQ      *ON
     C     F3FEIN        ANDNE     *ZEROS
     C     IXRLS         ANDEQ     '1'
     C*
     C*          - 8. Third Party Administrator FEIN
     C                   MOVE      F3FEIN        DN(8)
     C*
     C*          - 9. Third Party Administrator Name
     C                   MOVEL     F3NAME        DN(9)
     C                   ENDIF
     C                   ENDIF
     C     IXRLS         IFEQ      '1'
     C     DN(6)         IFEQ      DN(8)
     C                   MOVE      *BLANKS       DN(8)
     C                   MOVE      *BLANKS       DN(9)
     C                   ENDIF
     C                   ENDIF
     C*
     C     F3RCD         IFEQ      *ON
     C     F3ADR1        ANDNE     *BLANKS
     C*
     C*          - 10. Claim Administrator Address Line 1
     C                   MOVEL     F3ADR1        DN(10)
     C*
     C*          - 11. Claim Administrator Address Line 2
     C                   MOVEL     F3ADR2        DN(11)
     C*
     C*          - 12. Claim Administrator City
     C                   MOVEL     F3CTY         DN(12)
     C*
     C*          - 13. Claim Administrator State
     C                   MOVEL     F3ST          DN(13)
     C*
     C*          - 14. Claim Administrator Postal Code
     C     F3ZIP         IFNE      *ZEROS
     C                   MOVE      F3ZIP         DN(14)
     C                   ENDIF
     C*
     C                   ELSE                                                   F3RCD
     C*
     C*          - 10. Claim Administrator Address Line 1
     C                   MOVEL     FNPAD1        DN(10)
     C*
     C*          - 11. Claim Administrator Address Line 2
     C                   MOVEL     FNPAD2        DN(11)
     C*
     C*          - 12. Claim Administrator City
     C                   MOVEL     FNPCTY        DN(12)
     C*
     C*          - 13. Claim Administrator State
     C                   MOVEL     FNPST         DN(13)
     C*
     C*          - 14. Claim Administrator Postal Code
     C     FNPZIP        IFNE      *ZEROS
     C                   MOVE      FNPZIP        DN(14)
     C                   ENDIF
     C*
     C                   ENDIF                                                  F3RCD
     C*
     C*
     C     FNMZIP        IFNE      *ZEROS
     C     F2TPA         ANDNE     'Y'
     C     ##CLT         ANDEQ     '096'
     C                   MOVE      FNMZIP        DN(14)
     C                   ENDIF
     C*
     C*
     C*          - 15. Claim Administrator Claim Number
     C*
     C                   MOVEL     XXCO#         XCACLC
     C                   MOVE      XXFND         XCFND
     C                   MOVE      XXFYR         XCFYR
     C                   MOVE      XXCASE        XCCASE
     C*
     C     XCACLC        CAT       XCFND:0       XCACLC
     C     XCACLC        CAT       XCFYR:0       XCACLC
     C     XCACLC        CAT       XCCASE:0      XCACLC
     C                   MOVEL     XCACLC        DN(15)
      /free
       If (FNgrpasc = 'OPTACOMP' Or ##Clt = '152') and CMpds# <> *blanks;
          DN(15) = CMpds#;
       Endif;
      /end-free
     C*
     C*          - 16. Employer FEIN
     C*
     C     ALFEIN        IFGT      *ZEROS
     C                   MOVE      ALFEIN        DN(16)
     C                   ELSE
     C     EMFEIN        IFGT      *ZEROS
     C                   MOVE      EMFEIN        DN(16)
     C                   ENDIF
     C                   ENDIF
      *
     C*** beg del *** 05/25/23 *************************************
     c*** The*insured*info*should*not*be*loaded into the Employer info
     c*** Deleting*this*code*per*Prescient.
     C*** beg chg *** 05/25/23 *************************************
     C*****FNPEOF ****** IFEQ      'S'
     C*** beg add *** 03/19/24 *************************************
     C     FNPEOF        IFNE      ' '
     C*** beg add *** 03/29/24 *************************************
     C     XXASOF        ANDEQ     ' '
     C*** end add *** 03/29/24 *************************************
     C     INSNAM        ANDNE     *BLANKS
     C     INSFEI        ANDNE     *ZEROS
     C                   MOVE      INSFEI        DN(16)
     C                   ENDIF
     C*** end add *** 03/19/24 *************************************
     C*** end chg *** 05/25/23 *************************************
     C*** beg del *** 05/25/23 *************************************
     C***  INSNAM        ANDNE     *BLANKS
     C***  INSFEI        ANDNE     *ZEROS
     C***                MOVE      INSFEI        DN(16)
     C***                ENDIF
     C*** end del *** 05/25/23 *************************************
     C*
     C*          - 17. Insured Name
     C*
     C*** beg chg *** 05/25/23 *************************************
     C*****FNPEOF ****** IFEQ      'S'
     C     FNPEOF        IFNE      ' '
     C*** end chg *** 05/25/23 *************************************
     C*** beg add *** 03/29/24 *************************************
     C*** Note: DN17 - treated the same as PEO whether it is ASO or not.
     C*** end add *** 03/29/24 *************************************
     C     INSNAM        ANDNE     *BLANKS
     C                   MOVEL     INSNAM        DN(17)
     C                   ELSE
     C                   MOVEL     EMNAME        DN(17)
     C                   ENDIF
     C*
     C* Use group name if S1 Medical
     C     ##CLT         IFEQ      '152'
     C                   MOVEL     DN(7)         DN(17)
     C                   EndIf
     C*
     C*          - 18-23. Employer Name & Address
     C*
     C     ##CLT         IFEQ      '114'
     C*
     C     EMNAME        IFNE      *BLANKS
     C                   MOVEL     EMNAME        DN(18)
     C                   ENDIF
     C*
     C     EMPAD1        IFEQ      *BLANKS
     C                   MOVEL     EMMAD1        DN(19)
     C                   MOVEL     EMMAD2        DN(20)
     C                   MOVEL     EMMCTY        DN(21)
     C                   MOVEL     EMMST         DN(22)
     C     EMMZIP        IFNE      *ZEROS
     C                   MOVE      EMMZIP        DN(23)
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     EMPAD1        DN(19)
     C                   MOVEL     EMPAD2        DN(20)
     C                   MOVEL     EMPCTY        DN(21)
     C                   MOVEL     EMPST         DN(22)
     C     EMPZIP        IFNE      *ZEROS
     C                   MOVE      EMPZIP        DN(23)
     C                   ENDIF
     C                   ENDIF
     C*
     C*** beg del *** 05/25/23 *************************************
     c*** The*insured*info*should*not*be*loaded*into*the*Employer*info
     c*** Deleting*this*code*per*Prescient.
     C*** beg chg *** 05/25/23 *************************************
     C*****FNPEOF ****** IFEQ      'S'
     C***  FNPEOF        IFNE      ' '
     C*** beg add *** 03/19/24 *************************************
     C     FNPEOF        IFNE      ' '
     C*** beg add *** 03/29/24 *************************************
     C     XXASOF        ANDEQ     ' '
     C*** end add *** 03/29/24 *************************************
     C                   MOVEL     INSNAM        DN(18)
     C                   MOVEL     INMAD1        DN(19)
     C                   MOVEL     INMAD2        DN(20)
     C                   MOVEL     INMCTY        DN(21)
     C                   MOVEL     INMST         DN(22)
     C                   MOVE      INMZIP        DN(23)
     C                   ENDIF
     C*** end add *** 03/19/24 *************************************
     C*** end chg *** 05/25/23 *************************************
     C*** beg del *** 05/25/23 *************************************
     C*****FNPEOF ****** OREQ      'G'
     C*****##CLT  ****** ANDEQ     '139'
     C*** beg del *** 05/25/23 *************************************
     C***                MOVEL     INSNAM        DN(18)
     C***                MOVEL     INMAD1        DN(19)
     C***                MOVEL     INMAD2        DN(20)
     C***                MOVEL     INMCTY        DN(21)
     C***                MOVEL     INMST         DN(22)
     C***                MOVE      INMZIP        DN(23)
     C***                ENDIF
     C*** end del *** 05/25/23 *************************************
     C*** The following ELSE will end CLT114 processing.
     C                   ELSE
       //*** Beg Chg *** 12/10/21 **********************************************
     C***  ALNAME        IFNE      *BLANKS
     C***                MOVEL     ALNAME        DN(18)
     C***                ELSE
     C***  EMNAME        IFNE      *BLANKS
     C***                MOVEL     EMNAME        DN(18)
     C***                ENDIF
     C***                ENDIF
       // Use Additional Location name if present otherwise use Employer Master name.  Federal
       // name takes priority over DBA name.
       //*** Beg Chg *** 04/29/22 **********************************************
       //***If ALfdNm <> *blanks;
       If ALfdNm <> *blanks and (ALncci <> 'N' and ALncci <> ' ');
       //*** End Chg *** 04/29/22 **********************************************
          DN(18) = ALfdNm;
       Else;
          //*** Beg Chg *** 04/29/22 ************************************** ****
          //***If ALname <> *blanks;
          If ALname <> *blanks and (ALncci <> 'N' and ALncci <> ' ');
          //*** End Chg *** 04/29/22 ************************************** ****
             DN(18) = ALname;
          Else;
             If EMfdNm <> *blanks;
                DN(18) = EMfdNm;
             Else;
                DN(18) = EMname;
             Endif;
          Endif;
       Endif;
       //*** End Chg *** 12/10/21 **********************************************
     C*
     C     ALMAD1        IFNE      *BLANKS
     C                   MOVEL     ALMAD1        DN(19)
     C                   MOVEL     ALMAD2        DN(20)
     C                   MOVEL     ALMCTY        DN(21)
     C                   MOVEL     ALMST         DN(22)
     C     ALMZIP        IFNE      *ZEROS
     C                   MOVE      ALMZIP        DN(23)
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     ALPAD1        IFNE      *BLANKS
     C                   MOVEL     ALPAD1        DN(19)
     C                   MOVEL     ALPAD2        DN(20)
     C                   MOVEL     ALPCTY        DN(21)
     C                   MOVEL     ALPST         DN(22)
     C     ALPZIP        IFNE      *ZEROS
     C                   MOVE      ALPZIP        DN(23)
     C                   ENDIF
     C*
     C*
     C                   ELSE
     C*
     C*
     C     EMPAD1        IFEQ      *BLANKS
     C                   MOVEL     EMMAD1        DN(19)
     C                   MOVEL     EMMAD2        DN(20)
     C                   MOVEL     EMMCTY        DN(21)
     C                   MOVEL     EMMST         DN(22)
     C     EMMZIP        IFNE      *ZEROS
     C                   MOVE      EMMZIP        DN(23)
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     EMPAD1        DN(19)
     C                   MOVEL     EMPAD2        DN(20)
     C                   MOVEL     EMPCTY        DN(21)
     C                   MOVEL     EMPST         DN(22)
     C     EMPZIP        IFNE      *ZEROS
     C                   MOVE      EMPZIP        DN(23)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*** beg del *** 05/25/23 *************************************
     c*** The insured info should not be loaded into the Employer info
     c*** Deleting this code per Prescient.
     C*** beg chg *** 05/25/23 *************************************
     C*****FNPEOF ****** IFEQ      'S'
     C***  FNPEOF ****** IFNE      ' '
     C*** end chg *** 05/25/23 *************************************
     C*** beg add *** 03/19/24 *************************************
     C     FNPEOF        IFNE      ' '
     C*** beg add *** 03/29/24 *************************************
     C     XXASOF        ANDEQ     ' '
     C*** end add *** 03/29/24 *************************************
     C                   MOVEL     INSNAM        DN(18)
     C                   MOVEL     INMAD1        DN(19)
     C                   MOVEL     INMAD2        DN(20)
     C                   MOVEL     INMCTY        DN(21)
     C                   MOVEL     INMST         DN(22)
     C                   MOVE      INMZIP        DN(23)
     C                   ENDIF
     C*** end add *** 03/19/24 *************************************
     C*** beg del *** 05/25/23 *************************************
     C*****FNPEOF ****** OREQ      'G'
     C*****##CLT  ****** ANDEQ     '139'
     C*** end del *** 05/25/23 *************************************
     C***                MOVEL     INSNAM        DN(18)
     C***                MOVEL     INMAD1        DN(19)
     C***                MOVEL     INMAD2        DN(20)
     C***                MOVEL     INMCTY        DN(21)
     C***                MOVEL     INMST         DN(22)
     C***                MOVE      INMZIP        DN(23)
     C***                ENDIF
     C*** end del *** 05/25/23 *************************************
     C                   ENDIF
     C*
     C* Use group name and address if S1 Medical
     C     ##CLT         IFEQ      '152'
     C                   MOVEL     DN(7)         DN(18)
     C                   MOVEL     DN(10)        DN(19)
     C                   MOVEL     DN(11)        DN(20)
     C                   MOVEL     DN(12)        DN(21)
     C                   MOVEL     DN(13)        DN(22)
     C                   MOVE      DN(14)        DN(23)
     C                   EndIf
     C*
     C*          - 24. Self Insured Indicator
     C*
     C                   MOVEL     FNSFIN        DN(24)
     C*
     C*          - 25. SIC Code
     C*
     C*
     C** FOR TX THE NAICS CODE WILL BE LOADED INTO THE SIC CODE FIELD
     C** IF FOUND IN THE GENERIC TABLE FILE ON THE BACK END IN PGM
     C** W8071R
     C*
     C*
     C     EMNAIC        IFNE      *BLANKS
     C     XXJST         ANDNE     'AR'
     C     XXJST         ANDNE     'KY'
     C                   MOVEL     EMNAIC        DN(25)
     C                   ELSE
     C*
     C     CMSIC         IFNE      *BLANKS
     C                   MOVEL     CMSIC         DN(25)
     C                   ELSE
     C                   MOVEL     EMSIC         DN(25)
     C                   ENDIF
     C*
     C     DN(25)        IFNE      *BLANKS
     C     XXJST         IFEQ      'AR'
     C     XXJST         OREQ      'FL'
     C     XXJST         OREQ      'KY'
     C     XXJST         OREQ      'MO'
     C     XXJST         OREQ      'TN'
     C     XXJST         OREQ      'CA'
     C                   CAT       'SC':0        DN(25)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C** IF RELEASE 3, OVERRIDE
     C*
     C     IXRLS         IFEQ      '3'
     C                   MOVEL     EMNAIC        DN(25)
     C                   ENDIF
     C     XXJST         IFEQ      'CA'
     C     6             SUBST     DN(25):1      I                 6
     C     ' ':'0'       XLATE     I             I
     C                   MOVEL     I             DN(25)
     C                   ENDIF
     C*
     C*          - 26. Insured Report Number
     C*
     C                   MOVEL     CMECS#        DN(26)
     C*
     C*          - 27. Insured Location Number
     C*
     C     ALNAME        IFNE      *BLANKS
     C                   MOVEL     ALELID        DN(27)
     C                   ELSE
     C     EMEMP#        IFGT      *ZEROS
     C                   MOVEL     EMEMP#        DN(27)
     C                   MOVE      EMDIV         XCDIV
     C     DN(27)        CAT       XCDIV:0       DN(27)
     C                   ENDIF
     C                   ENDIF
     C*
     C*          - 28. Policy Number
     C*
     C     ELFND         IFGT      *ZEROS
     C     ELFYR         ANDGT     *ZEROS
     C     ELEMP#        ANDGT     *ZEROS
     C                   MOVE      *BLANKS       XCPOLN           25
     C                   MOVE      ELFND         XCFND
     C                   MOVE      ELFYR         XCFYR
     C                   MOVE      ELEMP#        XCEMP#
     C                   MOVEL     XCFND         XCPOLN
     C     XCPOLN        CAT       XCEMP#:0      XCPOLN
     C     XCPOLN        CAT       XCFYR:0       XCPOLN
     C                   MOVEL     XCPOLN        DN(28)
     C                   ENDIF
     C*
     C*          - 29. Policy Effective Date
     C*
     C     ELPFDT        IFNE      *ZEROS
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(29)
     C                   ENDIF
     C*
     C*          - 30. Policy Expire Date
     C*
     C     ELPTDT        IFNE      *ZEROS
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(30)
     C                   ENDIF
     C*
     C*          - 31. Date of Injury
     C*
     C     CMACDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(31)
     C                   ENDIF
     C*
     C*          - 32. Time of Injury
     C*
     C     CMACTM        IFNE      *ZEROS
     C                   MOVEL     CMACTM        XXTIM4            4 0
     C                   MOVE      XXTIM4        DN(32)
     C                   ENDIF
     C*
     C*          - 33. Postal Code of Injury
     C*
     C                   MOVE      *BLANKS       XCAZIP
     C     CMACZ1        IFGT      *ZEROS
     C                   Z-ADD     CMACZ1        XCAZP1
     C                   Z-ADD     CMACZ2        XCAZP2
     C                   MOVE      XCAZIP        DN(33)
     C                   ENDIF
     C*
     C*          - 34. Employer Premisis Indicator
     C*
     C                   MOVEL     CMEADR        DN(34)
     C*
     C*          - 35. Nature of Injury Code
     C*
     C                   MOVEL     NAEDIN        DN(35)
     C*
     C*          - 36. Part of Body Injured Code
     C*
     C                   MOVEL     PAEDIP        PAEDIPWK
     C                   MOVEL     PAEDIPWK2     DN(36)
     C*
     C*          - 37. Cause of Injury Code
     C*
     C                   MOVEL     CAEDIC        DN(37)
     C*
     C*          - 38. Accident Description
     C*
     C     CMACDS        CAT       CMACD2        XXACDS          120
     C     XXACDS        CAT       CMACD3        DN(38)
     C*
     C*          - 39. Initial Treatment
     C*
     C                   MOVEL     CMITRT        DN(39)
     C*
     C*          - 40. Date Reported to Employer
     C*
     C     CMDTRP        IFNE      *ZEROS
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(40)
     C                   ENDIF
     C*
     C*          - 41. Date Reported to Claim Administrator
     C*
     C     CMRCVD        IFNE      *ZEROS
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(41)
     C                   ENDIF
     C*
     C*          - 42. Social Security Number
     C*
     C     CMSS#         IFNE      *ZEROS
     C                   MOVE      *BLANKS       DN(42)
     C                   MOVEL     CMSS#         DN(42)
     C                   ENDIF
     C*
     C*          - 43. Employee Last Name
     C*
     C                   MOVEL     CDLSN         DN(43)
     C*
     C*
     C* Scan to search a blank or a comma in DN,43
     C* if found than remove it from DN,43 and validate the
     C* current suffix from WTGTP file to move in DN,255(suffix).
     C*
     C     ' '           SCAN      DN(43)        XPS1              3 0
     C     ','           SCAN      DN(43)        XPS2              3 0
     C*
     C                   SELECT
     C     XPS1          WHENNE    0
     C* EXTRACT THE LAST NAME ONLY
     C     XPS1          SUBST     DN(43):1      XXLSN            30
     C* LOOKING FOR SUFFIX
     C                   ADD       1             XPS1
     C     4             SUBST     DN(43):XPS1   XXSUF             4
     C*
     C                   MOVEL(P)  'CSUF'        XXTBID
     C     LO:UP         XLATE     XXSUF         XXSUF1            4
     C                   MOVEL(P)  XXSUF1        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  XXSUF         DN(255)
     C                   MOVEL(P)  XXLSN         DN(43)
     C                   ENDIF
     C*
     C     XPS2          WHENNE    0
     C* EXTRACT THE LAST NAME ONLY
     C     XPS2          SUBST     DN(43):1      XXLSN            30
     C* LOOKING FOR SUFFIX
     C                   ADD       1             XPS2
     C     4             SUBST     DN(43):XPS2   XXSUF
     C*
     C                   MOVEL(P)  'CSUF'        XXTBID
     C     LO:UP         XLATE     XXSUF         XXSUF1
     C                   MOVEL(P)  XXSUF1        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  XXSUF         DN(255)
     C                   MOVEL(P)  XXLSN         DN(43)
     C                   ENDIF
     C*
     C                   OTHER
     C                   MOVEL(P)  *BLANKS       DN(255)
     C                   ENDSL
     C*
     C*          - 44. Employee First Name
     C*
     C                   MOVEL     CDFSN         DN(44)
     C*
     C*          - 45. Employee Middle Initial
     C*
     C                   MOVEL     CDMID         DN(45)
     C*
     C*          - 46. Employee Address Line 1
     C*
     C                   MOVEL     CDADR1        DN(46)
     C*
     C*          - 47. Employee Address Line 2
     C*
     C                   MOVEL     CDADR2        DN(47)
     C*
     C*          - 48. Employee City
     C*
     C                   MOVEL     CDCTY         DN(48)
     C*
     C*          - 49. Employee State
     C*
     C                   MOVEL     CDST          DN(49)
     C*
     C*          - 50. Employee Postal Code
     C*
     C     CDZIP         IFNE      *ZEROS
     C                   MOVE      CDZIP         DN(50)
     C                   ENDIF
     C*
     C*          - 51. Employee Phone Number
     C*
     C     CDPHN#        IFGT      *ZEROS
     C                   MOVEL     CDAREA        XCPHN#           10
     C                   MOVE      CDPHN#        XCPHN#           10
     C                   MOVE      XCPHN#        DN(51)
     C                   ENDIF
     C*
     C*          - 52. Employee Date of Birth
     C*
     C     CDDTOB        IFGT      *ZEROS
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(52)
     C                   ENDIF
     C*
     C*          - 53. Employee Gender Code
     C*
     C                   MOVEL     CDSEX         DN(53)
     C*
     C*          - 54. Employee Marital Status Code
     C*
     C                   MOVEL     CDMSTS        DN(54)
      /Free
        xxTBID = 'MSTS';
        xxKey = CDmsts;
        chain GTKY PGT01;
        If %found(WTGTL) and GTCF01 <> *blanks;
           xxCF10 = GTcf01;
           DN(54) = xxCF01;
        Endif;
      /End-Free
     C*
     C*          - 55. Employee Number of Dependents
     C*
     C     XXDEP         IFNE      *ZEROS
     C                   MOVE      XXDEP         DN(55)
     C                   ENDIF
     C*
     C*          - 56. Date Disability Began
     C*
     C     CMDBDT        IFNE      *ZEROS
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(56)
     C                   ENDIF
     C*
     C*          - 57. Date of Death
     C*
     C     CMDOD         IFNE      *ZEROS
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(57)
     C                   ENDIF
     C*
     C*          - 58. Employment Status Code
     C*
     C                   MOVEL     CMESTS        DN(58)
     C*
     C*          - 59. Payroll Class Code
     C*
     C                   MOVE      CMCLAS        DN(59)
     C*
     C*          - 60. Occupation Description
     C*
     C                   MOVEL     CMOCDS        DN(60)
     C*
     C*          - 61. Date of Hire
     C*
     C     CMHIDT        IFNE      *ZEROS
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(61)
     C                   ENDIF
     C*
     C*          - 62. Wage
     C*
     C     CMAWW         IFNE      *ZEROS
     C                   MOVE      CMAWW         DN(62)
     C                   ENDIF
      /Free
       If XXMTC = 'IP';
          Setgt (CMco#:CMfnd:CMfyr:CMcase:XXMTC:XXexdt) PAW05;
          Readpe (XXco#:XXfnd:XXfyr:XXcase:XXMTC) PAW05;
           If not %eof(WMAWL5);
                 If AWaww <> *Zeros;
                    %Subst(DN(62):144) = %Subst(%Editc(AWaww:'X'):3);
                 Endif;
           Endif;
       Endif;
      /End-Free
     C*
     C*          - 63. Wage Period
     C*
     C                   MOVEL     CMWGPD        DN(63)
     C     XXTSET        IFEQ      'A49'
     C     XXJST         ANDEQ     'FL'
     C     XXTSET        OREQ      'RAT'
     C     XXJST         ANDEQ     'FL'
     C     CMWGPD        IFNE      '1'
     C     CMWGPD        ANDNE     '4'
     C                   MOVEL     '1'           DN(63)
     C                   ENDIF
     C                   ENDIF
     C*
     C*          - 64. Number of Days Worked
     C*
     C     CMDAYS        IFNE      *ZEROS
     C                   MOVE      CMDAYS        DN(64)
     C                   ELSE
     C     CMDBDT        IFNE      *ZEROS
     C                   MOVE      '5'           DN(64)
     C                   ENDIF
     C                   ENDIF
     C*
     C*          - 65. Date Last Worked
     C*
     C     CMDTLW        IFNE      *ZEROS
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(65)
     C                   ENDIF
     C*
     C*          - 66. Full Wages Paid on DOI
     C*
     C                   MOVEL     CMPOI         DN(66)
     C*
     C*          - 67. Salary Continued Indicator
     C*
     C                   MOVEL     CMFSAL        DN(67)
     C*
     C*          - 68. Date Returned to Work
     C*
     C*
     C     CMRTWQ        IFEQ      '5'
     C     CMRTWQ        OREQ      '6'
     C*
     C     CMRLW         IFNE      *ZEROS
     C                   Z-ADD     CMRLW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(68)
     C                   ENDIF
     C*
     C                   ELSE
     C     CMRTW         IFNE      *ZEROS
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(68)
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*  If jurisdiction state = 'PA', do not report the date
     C*
     C     XXJST         IFEQ      'PA'
     C                   MOVE      *ALL'0'       DN(68)
     C                   ENDIF
     C*
     C*
     C*          - 69. Pre-existing Disability Indicator
     C*
     C                   MOVEL     CMPREX        DN(69)
     C*
     C*          - 70. Date of MMI
     C*
     C     CMMMI         IFNE      *ZEROS
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(70)
     C                   ENDIF
     C*
     C*          - 71. RTW Qualifier
     C*
     C                   MOVEL     CMRTWQ        DN(71)
     C*
     C*          - 72. Date of Release to Work
     C*
     C     XXCRTW        IFNE      *ZEROS
     C                   Z-ADD     XXCRTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(72)
     C                   ENDIF
     C*
     C*          - 73. Claim Status
     C*
     C                   MOVEL     XXCLMS        DN(73)
     C*
     C*          - 74. Claim Type
     C*
     C     CMETYP        IFNE      *BLANKS
     C                   MOVEL     CMETYP        DN(74)
     C     XXUPDC        IFEQ      'Y'
     C                   MOVE      'N'           XXUPDC
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     XXCLMT        DN(74)
     C                   MOVEL     XXCLMT        CMETYP
     C                   ENDIF
     C*
     C*          - 75. Agreement to Compensate Code
     C*
     C                   MOVEL     CMAGCD        DN(75)
     C*
     C*          - 76. Date of Representation
     C*
     C     LLDTAP        IFNE      *ZEROS
     C                   Z-ADD     LLDTAP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(76)
     C                   ENDIF
     C*
     C*          - 77. Late Reason Code
     C*
     C                   MOVEL     CMLTRE        DN(77)
     C*
     C*          - 78. Number of Permanent Impairments
     C*
     C                   MOVE      XXPI          DN(78)
     C*
     C*          - 79. Number of Payment Adjustments
     C*
     C                   MOVE      XXPA          DN(79)
     C*
     C*          - 80. Number of Benefit Adjustments
     C*
     C                   MOVE      XXBA          DN(80)
     C*
     C*          - 81. Number of Paid-to-Date/Reduced Earnings/
     C*                Recoveries.
     C*
     C                   MOVE      *ZEROS        DN(81)
     C                   MOVE      XXPD          DN(81)
     C*
     C*          - 82. Number of Death Dependent/Payee Relationships.
     C*
     C                   MOVE      *ZEROS        DN(82)
     C                   MOVE      XXDP          DN(82)
     C*
     C*          - 83. Permanent Impairment Body Part Code.
     C*
     C*
     C*          - 84. Permanent Impairment Percent.
     C*
     C*
     C*          - 85. Payment/Adjustment Code.
     C*
     C*
     C*          - 86. Payment/Adjustment Amount Paid to Date.
     C*
     C*
     C*          - 87. Payment/Adjustment Weekly Amount.
     C*
     C*
     C*          - 88. Payment/Adjustment Start Date.
     C*
     C*
     C*          - 89. Payment/Adjustment End Date.
     C*
     C*
     C*          - 90. Payment/Adjustment Weeks Paid.
     C*
     C*
     C*          - 91. Payment/Adjustment Days Paid.
     C*
     C*
     C*          - 92. Benefit Adjustment Code.
     C*
     C*
     C*          - 93. Benefit Adjustment Weekly Amount.
     C*
     C*
     C*          - 94. Benefit Adjustment Start Date.
     C*
     C*
     C*          - 95. Paid-to-Date Reduced Earning/Recovery Code.
     C*
     C*
     C*          - 96. Paid-to-Date Reduced Earning/Recovery Amount.
     C*
     C*
     C*          - 97. Dependent/Payee Relationship Code.
     C*
     C                   EVAL      DN(203) = CMPSPA
     C                   EVAL      DN(204) = CMWWTC
     C                   EVAL      DN(205) = CMWDS
      /Free
        If CMWDS = 'NNNNNNN';
            DN(205) = *Blanks;
        Endif;
      /End-Free
     C                   EVAL      DN(206) = CMESID
     C*      229.  Injury Severity Type code
     C*      230.  Employer ID Assnd By Jurisdiction
     C*      231.  Manual Class sub Code.
     C
     C                   EVAL      DN(229) = CMINJS
     C                   EVAL      DN(230) = EMSEMP
     C                   EVAL      DN(231) = CMCLSB
     C*      201.  Anticipated Wage Loss Indicator
     C*      203.  Employer Paid Salary Prior to Acquisition Code
     C*      401.  Jurisdiction Claim Number - Related
     C*      403.  Initial RTW Type Code
     C*      404.  Initial RTW Physical Restrictions Indicator
     C*      405.  Initial RTW With Same Employer Indicator
     C*      406.  Latest RTW Type Code code
     C*      407.  Latest RTW Physical Restrictions Indicator
     C*      408.  Latest RTW With Same Employer Indicator
     C*      416.  Current Date Employer Had Knowledge of Current Date of Disability
     C*      417.  Current Date Claim Administrator Knew of Current Date of Disability
     C*      418.  Suspension Reason Code - Full
     C*      419.  Suspension Reason Code - Partial
     C*      421.  Location code
     C*      422.  Fingers/toes location code
     C*      423.  Acquired Claim Last Known Indemnity Through Date
     C*      433.  Overpayment Amount - Current
     C
     C****             EDI 3.1 --> XXRLSA = '*ON'
     C                   IF        XXRLSA = *ON
     C                   EVAL      DN(201) = CMAWLI
     C                   EVAL      DN(401) = CMJRC#
     C     CMRTWQ        IFEQ      '1'
     C     CMRTWQ        OREQ      '2'
     C                   MOVEL     'A'           DN(403)
     C                   ENDIF
     C     CMRTWQ        IFEQ      '5'
     C     CMRTWQ        OREQ      '6'
     C                   MOVEL     'R'           DN(403)
     C                   ENDIF
      /Free
       If CMrtwq = '1' or CMrtwq = '5';
          DN(404) = 'N';
       Endif;
     C*
      /End-Free
     C     CMRTWQ        IFEQ      '2'
     C     CMRTWQ        OREQ      '6'
     C                   MOVEL     'Y'           DN(404)
     C                   ENDIF
     C*
     C     CMSAME        IFEQ      'Y'
     C                   MOVEL     'Y'           DN(405)
     C                   ELSE
     C                   MOVEL     ' '           DN(405)
     C                   ENDIF                                                  CMSAME='Y'
     C     CMLRTC        IFEQ      '1'
     C     CMLRTC        OREQ      '2'
     C                   MOVEL     'A'           DN(406)
     C                   ENDIF
     C     CMLRTC        IFEQ      '5'
     C     CMLRTC        OREQ      '6'
     C                   MOVEL     'R'           DN(406)
     C                   ENDIF
      *
      *
      /Free
       If CMlrtc= '1' or CMlrtc = '5';
          DN(407) = 'N';
       Endif;

       If CMlrtc= '2' or CMlrtc = '6';
          DN(407) = 'Y';
       Endif;
      /End-Free
     C*
     C
     C     CMLRSE        IFEQ      '1'
     C                   EVAL      DN(408) = 'Y'
     C                   ELSE
     C                   EVAL      DN(408) = ' '
     C                   ENDIF
     C                   Z-ADD     CMCDEK        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(416)
     C                   Z-ADD     CMCDAK        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(417)
     C                   EVAL      DN(418) = CMSRCF
     C                   EVAL      DN(419) = CMSRCP
     C                   EVAL      DN(421) = CMIMLC
     C                   EVAL      DN(422) = CMLOCFT
     C                   Z-ADD     CMALKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(423)
     C*
     C                   MOVEL     CMOPYA        DN(433)
     C*
     C                   Z-ADD     CMPDED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(436)
       //*** Beg Add *** 02/14/22 ***********************************************
       //    151.  Education Level
       //    440.  Withheld Attorney Fees - Current
       //    441.  Administrator Jurisdiction Reporting Date
       //    442.  Net to Zero
       DN(151) = %Editc(CDeduL:'X');
       DN(440) = %Editc(CMwhAf:'X');
       DN(441) = %Editc(CMarDt:'X');
       DN(442) = CMnTZ;
       DN(443) = %Editc(CMopyA:'X');
       //*** End Add *** 02/14/22 **********************************************
     C                   ENDIF
     C     XXUPDC        IFEQ      'Y'
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     C*   Routine - SRR21
     C*
     C*             This routine retrieves each data element being
     C*             edited and places the value in the appropriate
     C*             element in array DN for R21 format of Release 3.
     C*
     C     SRR21         BEGSR
     C*
     C*          Load each element in DN with the appropriate data
     C*          base element.
     C*
     C*   Retrieve EDI Log Data
     C*
     C     LGKY          CHAIN     PLG01                              12
     c*** Beg Add *** 02/14/22 *********************************************
        Dow LGsts <> 'EC' and *in12 = *off;
           Reade lgky plg01;
           If %Eof(WMLGL4);
              *in12 = *on;
           Endif;
        Enddo;
     c*** End Add *** 02/14/22 *********************************************
     C     *IN12         IFEQ      *OFF
     C*
     C*          - 295. Maintenance Type Code
     C*
     C                   MOVEL     LGMTCC        DN(295)
     C*
     C*          - 296. Maintenance Type Correction Code Date
     C*
     C                   Z-ADD     LGMTC2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(296)
     C*
     C                   ENDIF
     C*
     C*          - 186. Jurisdiction Branch Office Code
     C*
     C                   MOVEL     CMJURO        DN(186)
     C*
     C     F3RCD         IFEQ      *ON
     C     F3FEIN        ANDNE     *ZEROS
     C*
     C*          - 187. Claim Administrator FEIN
     C                   MOVEL     F3FEIN        DN(187)
     C*
     C*          - 188. Claim Administrator Name
     C                   MOVEL     F3NAME        DN(188)
     C*
     C                   ELSE
     C*          - 187. Claim Administrator FEIN
     C     F2FEIN        IFNE      *ZEROS
     C                   MOVEL     F2FEIN        DN(187)
     C                   ENDIF
     C*
     C*          - 188. Claim Administrator Name
     C                   MOVEL     FNNAME        DN(188)
     C                   ENDIF
     C*
     C*          - 135. Claim Administrator Information/Attention Line
     C     F3RCD         IFEQ      *ON
     C     F3INFO        ANDNE     *BLANKS
     C                   MOVEL     F3INFO        DN(135)
     C                   ELSE
     C                   MOVEL     IXCNNM        DN(135)
     C                   ENDIF
     C*
     C*          - 136. Claim Administrator Country Code
     C*
     C                   MOVEL     'US'          DN(136)
     C*
     C*          - 270. Employee ID Type Qualifier
     C*
     C                   MOVEL     'S'           DN(270)
     C     CMEITQ        IFNE      *BLANKS
     C                   MOVEL     CMEITQ        DN(270)
     C                   ENDIF
      *
     C                   MOVEL     CMRBAC        DN(202)
     C*
     C*          -255. Employee Last Name Suffix
     C*
     C*
     C*         -150. Employee Authorization to Release Medical Records
     C*
     C                   MOVEL     CMAURL        DN(150)
     C*
     C*         -157. Employee Social Security Number Release Indicator
     C*
     C                   MOVEL     CMSSRL        DN(157)
      *
      *          -152, 153, 154, 156. ID other than SSN
      *
     C                   SELECT
     C     CMEITQ        WHENEQ    'E'
     C                   MOVEL     CMEID         DN(152)
     C     CMEITQ        WHENEQ    'G'
     C                   MOVEL     CMEID         DN(153)
     C     CMEITQ        WHENEQ    'A'
     C                   MOVEL     CMEID         DN(154)
     C     CMEITQ        WHENEQ    'P'
     C                   MOVEL     CMEID         DN(156)
     C                   ENDSL
     C*
     C*          -155. Employee Mailing Country Code
     C*
     C     CDCTRY        IFNE      *BLANKS
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  CDCTRY        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      GTCF01        XXCF10
     C                   MOVEL     XXCF01        DN(155)
     C                   ELSE
     C                   MOVEL     'US'          DN(155)
     C                   ENDIF
     C                   ENDIF
     C*
     C*          -146. Death Result of Injury Code
     C*
     C                   MOVEL     CMIDTH        DN(146)
     C*
     C*          -290. Type of Loss Code
     C*
     C*                    MOVEL          DN,290
     C*
     C*          -185. Insurer Type Code
     C*
     C     FNSFIN        IFEQ      'Y'
     C                   MOVEL     'S'           DN(185)
     C                   ELSE
     C                   MOVEL     'I'           DN(185)
     C                   ENDIF
     C*
     C*          -292. Insolvent Insurer FEIN
     C*
     C*                    MOVEL          DN,292
     C*
     C*          -200. Claim Administrator Alternate Postal Code
     C*
     C     F3RCD         IFEQ      *ON
     C     F3AZP         ANDNE     *ZEROS
     C                   MOVEL     F3AZP         DN(200)
     C                   ELSE
     C                   MOVEL     FNMZIP        DN(200)
     C                   ENDIF
     C*
     C*          -249. Accident Premises Code
     C*
     C     CMaccPrem     IfEQ      ' '
     C     CMEADR        IFEQ      'Y'
     C                   MOVEL     'E'           DN(249)
     C                   ELSE
     C                   MOVEL     'X'           DN(249)
     C                   ENDIF
     C                   Else
     C                   Movel     CMaccPrem     DN(249)
     C                   EndIf
     C*
     C*          -118. Accident Site County/Parish
     C*
      *
     C                   MOVE      *BLANKS       DN(118)
     C     CMAST         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMAST         XXACST
     C                   MOVEL     CMACCY        XXACCY
      *
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL     CNDESC        DN(118)
      *
     C     *IN60         IFEQ      *ON
     C     CMJRST        ANDEQ     'GA'
     C     CMAST         ANDNE     'GA'
     C                   MOVEL     CMACCY        DN(118)
     C                   ENDIF
     C                   ENDIF                                                              KS
     C*
     C*          -119. Accident Site Location Narrative
     C*
     C                   MOVEL     CMACLN        DN(119)
     C*
     C*          -120. Accident Site Organization Name
     C*
     C                   MOVEL     CMACNM        DN(120)
     C*
     C*          -121. Accident Site City
     C*
     C                   MOVEL     CMACTY        DN(121)
     C*
     C*          -122. Accident Site Street
     C*
     C                   MOVEL     CMACDR        DN(122)
     C*
     C*          -123. Accident Site State Code
     C*
     C                   MOVEL     CMAST         DN(123)
     C*
     C*          -280. Accident Site Country Code
     C*
     C                   MOVEL     *BLANKS       DN(280)
     C*
     C*         -281. Date Employer Had Knowledge of Date of Disability
     C*
     C                   Z-ADD     CMDODD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(281)
     C*
     C*          -329. Employer UI Number
     C*
     C                   MOVEL     *BLANKS       DN(329)
     C*
     C*          -164. Employer Physical Country Code
     C*
     C                   MOVEL     *BLANKS       DN(164)
     C     EMPCTR        IFEQ      *BLANKS
     C                   MOVEL     'US'          DN(164)
     C                   ENDIF
     C*
     C*          -159. Employer Contact Business Phone Number
     C*
       //*** Beg Add *** 12/13/21 **********************************************
     C* Use from contact file if exists
     C*
     C     XXCBPH        IFEQ      *BLANKS
       //*** End Add *** 12/13/21 **********************************************
     C                   MOVEL     EMAREA        XXAREA            3
     C                   MOVEL     EMPHN#        XXPHN#            7
     C     XXAREA        CAT       XXPHN#        XXBPH            15
     C*
     C                   MOVEL     XXBPH         DN(159)
       //*** Beg Add *** 12/13/21 **********************************************
     C                   ELSE
     C                   MOVEL     XXCBPH        DN(159)
     C                   ENDIF
       //*** End Add *** 12/13/21 **********************************************
     C*
     C*          -160. Employer Contact Name
     C*
     C                   MOVEL     XXCONT        DN(160)
     C*
     C*          -160. Employer Mailing Information/Attention Line
     C*
     C                   MOVEL     EMATTN        DN(163)
     C*
     C*          -165. Employer Mailing City
     C*
     C                   MOVEL     EMMCTY        DN(165)
     C*
     C*          -166. Employer Mailing Country Code
     C*
     C                   MOVEL     *BLANKS       DN(166)
     C     EMMCTR        IFEQ      *BLANKS
     C                   MOVEL     'US'          DN(166)
     C                   ENDIF
     C*
     C*          -167. Employer Mailing Postal Code
     C*
     C                   MOVEL     EMMZIP        DN(167)
     C*
     C*          -168. Employer Mailing Primary Address
     C*
     C                   MOVEL     EMMAD1        DN(168)
     C*
     C*          -169. Employer Mailing Secondary Address
     C*
     C                   MOVEL     EMMAD2        DN(169)
     C*
     C*          -170. Employer Mailing State Code
     C*
     C                   MOVEL     EMMST         DN(170)
     C*
     C*          -199. Full Denial Effective Date
     C*
     C                   Z-ADD     CMDYEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(199)
     C*
     C*          -273. Employer Paid Salary in Lieu of Compensation
     C*               Indicator
     C*
     C                   MOVEL     CMFSAL        DN(273)
     C*
     C*          -274. Number of Accident/Injury Description Narratives
     C*
     C*                    MOVEL*ZEROS    DN,274
     C*
     C*          -277. Number of Full Denial Reason Codes
     C*
     C*                    MOVEL*ZEROS    DN,277
     C*
     C*          -176. Number of Denial Reason Narratives
     C*
     C*                    MOVEL*ZEROS    DN,276
     C*
     C*          -278. Number of Managed Care Organizations
     C*
     C*                    MOVEL*ZEROS    DN,278
     C*
     C*          -279. Number of Witnesses
     C*
     C*                    MOVEL*ZEROS    DN,279
     C*
     C*          -224. Physical Restrictions Indicator
     C*
     C     CMRTWQ        IFEQ      '2'
     C     CMRTWQ        OREQ      '6'
     C                   MOVEL     'Y'           DN(224)
     C                   ENDIF
     C     CMRTWQ        IFEQ      '1'
     C     CMRTWQ        OREQ      '5'
     C                   MOVEL     'N'           DN(224)
     C                   ENDIF
     C*
     C*          -189. Current RTW Type Code
     C*
     C     XXFG03        IFEQ      'E'
     C     XXFG03        OREQ      'R'
     C                   SELECT
     C     XXFG03        WHENEQ    'R'
     C                   MOVEL     'A'           DN(189)
     C     XXFG03        WHENEQ    'E'
     C                   MOVEL     'R'           DN(189)
     C                   ENDSL
     C                   ELSE
     C     CMRTWQ        IFEQ      '1'
     C     CMRTWQ        OREQ      '2'
     C                   MOVEL     'A'           DN(189)
     C                   ENDIF
     C     CMRTWQ        IFEQ      '5'
     C     CMRTWQ        OREQ      '6'
     C                   MOVEL     'R'           DN(189)
     C                   ENDIF
     C                   ENDIF
     C*
     C*          -228. RTW with same employer indicator
     C*
     C*
     C     CMSAME        IFEQ      'Y'
     C                   MOVEL     'Y'           DN(228)
     C                   ELSE
     C                   MOVEL     ' '           DN(228)
     C                   ENDIF                                                  CMSAME='Y'
     C*
     C*          -184. Insured type code
     C*
     C*
     C     FNSFIN        IFEQ      'Y'
     C                   MOVEL     'S'           DN(184)
     C                   ELSE
     C                   MOVEL     'I'           DN(184)
     C                   ENDIF
     C*
     C*
     C*** beg chg *** 05/25/23 *************************************
     C*****FNPEOF ****** IFEQ      'S'
     C     FNPEOF        IFNE      ' '
     C*** end chg *** 05/25/23 *************************************
     C*** beg add *** 03/29/24 *************************************
     C*** Note: DN314 - treated the same as PEO whether it is ASO or not.
     C*** end add *** 03/29/24 *************************************
     C     INSFEI        ANDNE     *ZEROS
     C                   MOVE      INSFEI        DN(314)
     C                   ELSE
     C                   MOVE      DN(16)        DN(314)
     C                   ENDIF
     C*
     C*          -438. Collective Bargaining Agreement
     C*
     C                   Movel(p)  CMcolBarg     DN(438)
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*   Routine - SRR22
     C*
     C*             This routine retrieves each data element being
     C*             edited and places the value in the appropriate
     C*             element in array DN for R22 format of Release 3.
     C*
     C     SRR22         BEGSR
     C*
     C*          Load each element in DN with the appropriate data
     C*          base element. Ignore those already loaded in R21 routine
     C*
     C*   Retrieve EDI Log Data
     C*
     C     CMDNLT        IFNE      *ZEROS
     C                   Z-ADD     CMDNLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(298)
     C                   ENDIF
     C*
     C                   MOVEL     CMENEX        DN(213)
     C*
     C                   MOVEL     CMETFS        DN(158)
     C*
     C     CMDODD        IFNE      *ZEROS
     C                   Z-ADD     CMDODD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(281)
     C                   ENDIF
     C*
     C                   MOVEL     CMIDTH        DN(146)
     C*
     C     CMIIF         IFEQ      *ZEROS
     C                   MOVEL     *BLANKS       DN(292)
     C                   ELSE
     C                   MOVEL     CMIIF         DN(292)
     C                   ENDIF
     C*
     C                   MOVEL     CMNCPC        DN(212)
     C*
     C     CMSAME        IFEQ      'Y'
     C                   MOVEL     'Y'           DN(228)
     C                   ELSE
     C                   MOVEL     ' '           DN(228)
     C                   ENDIF                                                  CMSAME='Y'
     C*
     C     CMWMTH        IFEQ      '2'
     C                   MOVEL     'Y'           DN(172)
     C                   ELSE
     C                   MOVEL     'N'           DN(172)
     C                   ENDIF                                                  CMWMTH='2'
     C*
     C*
     C     CMCDDB        IFNE      0
     C                   Z-ADD     CMCDDB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(144)
     C                   ENDIF                                                  CMCDDB<>0
     C*
     C     CMCDLW        IFNE      0
     C                   Z-ADD     CMCDLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(145)
     C                   ENDIF                                                  CMCDLW<>0
     C*
     C                   MOVE      CMDFBE        DN(149)
     C*
     C     CMSUEF        IFNE      0
     C                   Z-ADD     CMSUEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(193)
     C                   ENDIF                                                  CMSUEF<>0
     C*
     C     CMDRES        IFNE      0
     C                   Z-ADD     CMDRES        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(196)
     C                   ENDIF                                                  CMDRES<>0
     C*
     C     CMDYEF        IFNE      0
     C                   Z-ADD     CMDYEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(199)
     C                   ENDIF                                                  CMDYEF<>0
     C*
     C     F3RCD         IFEQ      *ON
     C     F3AZP         ANDNE     *ZEROS
     C                   MOVEL     F3AZP         DN(200)
     C                   ELSE
     C                   MOVEL     FNMZIP        DN(200)
     C                   ENDIF
     C*
     C     CMIDLT        IFNE      0
     C                   Z-ADD     CMIDLT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(297)
     C                   ENDIF                                                  CMIDLT<>0
     C*
     C                   MOVEL     CMPDNY        DN(294)
     C*
     C     LLKY          CHAIN     PLL01                              60
     C     *IN60         IFEQ      *OFF
     C     LLSTDT        IFEQ      0
     C     XXJST         ANDEQ     'WV'
     C     XXMTC         ANDEQ     'IP'
     C                   Z-ADD     ZZSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(299)
     C                   ELSE
     C                   Z-ADD     LLSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(299)
     C                   ENDIF
     C*
     C*     - First try state table
     C                   MOVEL(P)  '  MS'        XXTBID
     C                   MOVEL     CMJRST        XXTBID
     C                   MOVEL(P)  LLMTST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      GTCF01        XXCF10
     C                   MOVEL     XXCF01        DN(293)
     C*
     C                   ELSE
     C                   MOVEL(P)  '  MS'        XXTBID
     C                   MOVEL(P)  LLMTST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      GTCF01        XXCF10
     C                   MOVEL     XXCF01        DN(293)
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ELSE
     C                   Z-ADD     ZZSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(299)
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     CMAWW         IFNE      *ZEROS
     C                   MOVE      CMAWW         DN(286)
     C                   ENDIF
       /Free
       If XXMTC = 'IP';
          Setgt (CMco#:CMfnd:CMfyr:CMcase:XXMTC:XXexdt) PAW05;
          Readpe (XXco#:XXfnd:XXfyr:XXcase:XXMTC)PAW05;
           If not %eof(WMAWL5);
              If AWaww <> *Zeros;
                 %Subst(DN(286):144) = %Subst(%Editc(AWaww:'X'):3);
              Endif;
           Endif;
       Endif;
     C*
     C     CMCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPMAIL        DN(138)
     C                   MOVEL     UPUSNM        DN(140)
     C                   ENDIF
     C*
     C     F3RCD         IFEQ      *ON
     C     F3PH#         ANDGT     *BLANKS
     C                   MOVEL     F3PH#         DN(137)
     C                   ELSE
     C     FNPH1         IFGT      *ZEROS
     C                   MOVEL     FNPH1A        XCPHN#           10
     C                   MOVE      FNPH1         XCPHN#
     C                   MOVEL     XCPHN#        DN(137)
     C                   ENDIF
     C                   ENDIF
     C*
     C     F3RCD         IFEQ      *ON
     C     F3FAX         ANDGT     *BLANKS
     C                   MOVEL     F3FAX         DN(139)
     C                   ELSE
     C     FNFAX         IFGT      *ZEROS
     C                   MOVEL     FNFAXA        XCFAX#           10
     C                   MOVE      FNFAX         XCFAX#
     C                   MOVEL     XCFAX#        DN(137)
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*** Beg Del *** 01/27/23 ******************************************   ***
     C**** CMCRT         IFNE      *ZEROS
     C****               Z-ADD     CMCRT         XXCRT            11 2
     C****               MOVE      XXCRT         DN(134)
     C****               ELSE
     C*** End Del *** 01/27/23 ******************************************   ***
     C*
     C     CMAWW         IFNE      *ZEROS
     C                   MOVE      *BLANKS       DN(134)
     C     RBKY          SETGT     PRB01
     C     RBKY1         READPE    PRB01                                  60
     C     *IN60         IFEQ      *OFF
     C     RBCMPR        ANDNE     0
     C     RBCMPR        DIV       100           XXCMPR            7 6
     C     CMAWW         MULT      XXCMPR        XXAWW            11 2
       //*** Beg Add *** 12/11/24 ***************************************
       // Wage cannot be greater than maximum or lower than minimum
       If xxAww > RBpaCp;
          xxAww = RBpaCp;
       Endif;
       If xxAww < RBmnRt;
          xxAww = RBmnRt;
       Endif;
       //*** End Add *** 12/11/24 ***************************************
     C                   MOVE      XXAWW         DN(134)
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  CMAWW<>0
     C*** Beg Del *** 01/27/23 ******************************************   ***
     C****               ENDIF                                                  CMCRT<>0
     C*** End Del *** 01/27/23 ******************************************   ***
      /Free
       //*** Beg Del *** 10/04/21 *********************************************
       //***If XXMTC = 'IP';
       //***   Setgt (CMco#:CMfnd:CMfyr:CMcase:XXMTC:XXexdt) PAW05;
       //***   Readpe (XXco#:XXfnd:XXfyr:XXcase:XXMTC)PAW05;
       //***    If not %eof(WMAWL5);
       //***          Select;
       //***          When AWcrt <> *zeros;
       //***             XXcrt = AWcrt;
       //***             %Subst(DN(134):140) = %Editc(XXcrt:'X');
       //***          When AWaww <> *Zeros;
       //***             XXaww = AWaww;
       //***             %Subst(DN(134):140) = %Editc(XXaww:'X');
       //***          Endsl;
       //***    Endif;
       //***Endif;
       //*** End Del *** 10/04/21 *********************************************
      *
     C*
       //*** Bed Chg *** 02/14/22 **********************************************
     C**************     MOVE      *BLANKS       DN(151)
       //   151.  Education Level
       DN(151) = %Editc(CDedul:'X');
       // - 439. Benefit Change Reason Code
       DN(439) = LGbcRc;
       //*** End Chg *** 02/14/22 **********************************************
     C*** beg chg *** 05/25/23 *************************************
     C*****FNPEOF ****** IFEQ      'S'
     C     FNPEOF        IFNE      ' '
     C*** end chg *** 05/25/23 *************************************
     C*** beg add *** 03/29/24 *************************************
     C*** Note: DN314 - treated the same as PEO whether it is ASO or not.
     C*** end add *** 03/29/24 *************************************
     C     INSFEI        ANDNE     *ZEROS
     C                   MOVE      INSFEI        DN(314)
     C                   ELSE
     C                   MOVE      DN(16)        DN(314)
     C                   ENDIF
     C* WAGE EFFECTIVE DATE

       //Check for Primary Wage Effective Date
         Chain (CMco#:CMfnd:CMfyr:CMcase) WMAWL6;
         If %found(WMAWL6) and AWefdt <> 0;
            DN(256) =   %char(%date(%Editc(AWefdt:'X'):*cymd0):*USA0);
         Else;
     C                   MOVE      *HIVAL        XXTRDT
     C                   MOVE      *HIVAL        XXTIME
     C     AWKY          SETGT     PAW01                              60
     C     AWKY1         READPE    PAW01                                  60
     C     *IN60         IFEQ      *OFF
     C     AWEFDT        IFNE      0
     C                   Z-ADD     AWEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(256)
     C                   ELSE
     C     AWTRDT        IFNE      0
     C                   Z-ADD     AWTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DN(256)
     C                   ENDIF                                                  AWTRDT<>0
     C                   ENDIF                                                  AWEFDT<>0
     C                   ENDIF                                                  *IN60=*OFF
          If XXMTC = 'IP';
             Setgt (CMco#:CMfnd:CMfyr:CMcase:XXMTC:XXexdt) PAW05;
             Readpe (XXco#:XXfnd:XXfyr:XXcase:XXMTC) PAW05;
              If not %eof(WMAWL5);
                    If AWefDt <> *Zeros;
                       DN(256) = %char(%date(%Editc(AWefdt:'X'):*cymd0):*USA0);
                    Endif;
              Endif;
          Endif;
       Endif;
     C*
     C*     - First try state table
     C                   MOVEL(P)  '  LC'        XXTBID
     C                   MOVEL     CMJRST        XXTBID
     C                   MOVEL(P)  CMLSCV        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      GTCF01        XXCF10
     C     IXRLS         IFEQ      '3'
     C                   MOVEL     XXCF03        DN(290)
     C                   ELSE
     C                   MOVEL     XXCF01        DN(290)
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  '  LC'        XXTBID
     C*                    MOVELCMLSCV    XXKEY     P
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      GTCF01        XXCF10
     C     IXRLS         IFEQ      '3'
     C                   MOVEL     XXCF03        DN(290)
     C                   ELSE
     C                   MOVEL     XXCF01        DN(290)
     C                   ENDIF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     ##CLT         IFNE      '152'
     C     CMPIMP        IFEQ      'Y'
     C                   MOVEL     'Y'           DN(223)
     C                   ELSE
     C                   MOVEL     'N'           DN(223)
     C                   ENDIF                                                  CMPIMP='Y'
     C                   ELSE
     C                   MOVE      CMPIMP        DN(223)
     C                   ENDIF                                                  CMPIMP='Y'
     C*
     C                   ENDSR
