     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H bnddir('WTBNDDIR')
     H*
     H/TITLE W0014R02 - STATISTICAL REPORTING/RATING MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE STAT RPT/RATING FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0014R - STAT RPT/RATING  DEFINITION MAINTENANCE           *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/20/97  PROGRAMMER: C:65  PROJECT: WC651          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     h* 04/18/24 C:291 Project: WC2961                                *
     h*                Change the existing checkbox "Apply schedule   *
     h*                DB/CR after Deviation" to a dropdown and       *
     h*                change the Heading to "Schedule DB/CR Applied  *
     h*                To".                                           *
     h*                                                               *
     H* 11/14/23 C:65  Project: Phw#114374 (Vital)                    *
     H*                Add to base checkbox to allow schd db/cr to    *
     H*                be applied after deviation - checkbox now      *
     H*                visible for all clients.                       *
     H*                                                               *
     H* 09/18/23 C:65  Project: 153WAL                                *
     H*                Add stat code to safety fee.                   *
     H*                                                               *
     H* 08/02/21 C:318 Project:WC2820                                 *
     H*                Added visibility condition to (Gradient)       *
     H*                Discount Calculation Logic Fields              *
     H*                                                               *
     H* 11/23/20 C:62  Project: 148WCA                                *
     H*                Add Monopolistic State                         *
     H*                                                               *
     H* 05/23/19 C:62  Project: 150WEA                                *
     H*                Add Pull Territory Differential from State     *
     H*                Reporting Division                             *
     H*                                                               *
     H* 04/22/19 C:65  Project: WC2610                                *
     H*                Rearrange fields on screen three and four for  *
     H*                better organization, and add fields for        *
     H*                discount calculation logic to screen four.     *
     H*                                                               *
     H* 05/02/19 C:136 Project: SY406                                 *
     H*                trpy Rating Default records when adding        *
     H*                new record.                                    *
     H*                                                               *
     H* 07/11/18 C:291 Project: 148WAV                                *
     H*                Add Deviation #1 and #2 default Percentage and *
     H*                allow changes flags.                           *
     H*                                                               *
     H* 05/25/18 C:291 Support: 64413                                 *
     H*                Fix record corruption on Statistical reporting *
     H*                Rating copy.                                   *
     H*                                                               *
     H* 02/01/18 C:136 Project: 148WAH                                *
     H*                Add policy fee fields for AmWins               *
     H*                                                               *
     H* 01/24/18 C:33  Support: 50626                                 *
     H*                Include LD with RR copy/delete                 *
     H*                                                               *
     H* 11/06/17 C:65  Project: WC2565                                *
     H*                Add stat codes and threshold for high values   *
     H*                of merit rating.                               *
     H*                                                               *
     H* 09/07/17 C:65  Project: 150WAR                                *
     H*                Removed % of manual premium (uncommented).     *
     H*                                                               *
     H* 08/30/17 C:267 Project: 150WAN                                *
     H*                Remove visibility indicator for client 150     *
     H*                for Default Leasing Id field - screen 3        *
     H*                                                               *
     H* 08/03/17 C:65  Project: 150WAR                                *
     H*                Add checkbox for including tax in targeted     *
     H*                premium.                                       *
     H*                                                               *
     H* 08/03/17 C:291 Project: 2504                                  *
     H*                Turn Leasing Id Textbox into Dropdown. Screen  *
     H*                changes only.                                  *
     H*                                                               *
     H* 08/01/17 C:062 Project: 150WAN                                *
     H*                Move PEO Policy Type to W0114R02               *
     H*                                                               *
     H* 07/14/17 C:267 Project: 150WAN                                *
     H*                Add 'Default billing code'                     *
     H*                Add 'Percenage manual premium' (client)        *
     H*                Add 'PEO policy type' (M/S) If FNPEOF = 'C'    *
     H*                Add 'Default Leasing ID'                       *
     H*                Added to screen3.                              *
     H*                                                               *
     H* 08/17/16 C:298 Project: WC2070                                *
     H*                Adding Audit Non Compliance Stat Code and      *
     H*                Multiplier.                                    *
     H*                                                               *
     H* 04/09/15 C:242 Project: 132WQS                                *
     H*                CompSuite Retrofit of Custom Code to Base      *
     H*                Screen:Chg Waiver end to dropdown, Contractor  *
     H*                Credit always visible. Relaxed edit for Dev#1. *
     H*                                                               *
     H* 04/21/14 C:295 Redmine: 4111                                  *
     H*                Recompilation-changes are in display file      *
     H*                Correct alignment of fields which are lying on *
     H*                top of description.                            *
     H*                                                               *
     A* 02/13/14 C:062 Support: WA114 84357                           *
     A*                Correct rrn processing                         *
     H*                                                               *
     H* 11/22/13 C:190 Project: 129WSB                                *
     H*                Add field for Payroll Rate Deviation at SCR03. *
     H*                                                               *
     A* 02/08/13 C:279 Project: SY266 Transform                       *
     A*                Initialize check box with value 'N'            *
     A*                                                               *
     H* 09/14/12 C:282 Project: 114WGD                                *
     H*                Replacing the condtion 'Cltid = 114' by        *
     H*                new system flag WCFL53.                        *
     H*                                                               *
     H* 11/02/11 C:65  Project: WC2078                                *
     F*                ENABLE OPTIONAL HEADINGS FOR TERRORISM CHARGES *
     H*                FOR ALL CLIENTS.                               *
     H*                                                               *
     H* 03/16/11 C:190 Project: 132WPI                                *
     F*                Add the new optional heading field. This will  *
     H*                print in the heading column for the contra-
     H*                ctor's credit line item.                       *
     H*                                                               *
     H* 03/11/11 C:190 Project: 132WPN                                *
     H*                Relax the edit requiring a stat code for the   *
     H*                Deviation #1 field in the state of NM.         *
     H*                (Client 132 only).                             *
     H*                                                               *
     H* 06/24/10 C:227 Project: WC1873                                *
     H*                Removed processing related to "Include terr.   *
     H*                charges in assessment calc".                   *
     H*                                                               *
     H* 06/21/10 C:255 Project: 114WVY
     H*                Add handling for new fields added at screen01  *
     H*                MVSCDB and ERRCK1 for split deviations.        *
     H*                                                               *
     H* 04/19/10 C:255 Project: 114WWI
     H*                Add new field on screen 4 to determine if      *
     H*                the Policy page needs to be generated with     *
     H*                Revise billing.                                *
     H*                                                               *
     H* 01/04/10 C:190 Project: 25WRE
     H*                Add new field on screen 4 to define flexible heading
     H*                for "State Employer #".
     H*                This heading will be used in employer master and
     H*                entity/location maintenance for district #.
     H*
     H* 12/02/09 C:61  PROJECT: 132WMP                                *
     H*                Add field SCFLPC for CLT132 Waiver
     H*                (Flat charge / Percentage) edit (WC990306).    *
     H*                                                               *
     H* 11/16/09 C:136 PROJECT: C139WNK                               *
     H*                ADD NEW LEVELS TO SAFETY FEE DEFINITION        *
     H*                                                               *
     H* 03/02/09 C:65  PROJECT: 81WEI                                 *
     H*                ADD OPTIONAL HEADING FIELDS FOR TERRORISM      *
     H*                CHARGES.                                       *
     H*                                                               *
     H* 09/15/08 C:189 PROJECT: WC1644                                *
     H*                Option to exclude terrorism from LCM           *
     H*                                                               *
     H*                                                               *
     H* 08/22/08 C:191 PROJECT 102WEP                                 *
     H*                Add fields for late fee charge on S/R payroll  *
     H*                billing                                        *
     H*                                                               *
     H* 08/21/08 C:65  PROJECT WC1514/SUPP# 64161                     *
     H*                ALTER ERROR MESSAGE FOR SHORT RATE FLAG.       *
     H*                                                               *
     H* 08/21/08 C:61  PROJECT 114WRC                                 *
     H*                Add new fields SCR04: "Max Debit Factor" and   *
     H*                "Max Credit Factor".                           *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*                                                               *
     H* 07/26/07 C:61  PROJECT WC1570                                 *
     H*                Add new entry to SCR03, "Print State Risk ID#".*
     H*                                                               *
     H* 06/29/07 C:132 Project 114WMS                                 *
     H*                 Delete *IN59 from SCR03 So SCDBCR line stays  *
     H*                 on screen. (COMPILE ONLE-NO CODE CHANGES)     *
     H*                                                               *
     H* 05/30/07 C:33  Conflict resolution                            *
     H*                Create SCR04 - Move terrorism charges to SCR04 *
     H*                                                               *
     H* 04/19/07 C:190 Project 136WCM
     H*                 ADD NEW FIELD 'Exclude terrorism charge on -
     H*                 monthly S/R worksheet.
     H*
     H* 04/06/07 C:65  PROJECT: WC1514                                *
     H*                ADD PREMIUM LEVEL FLAG FOR SHORT RATE PENALTY. *
     H*
     H* 04/04/07 C:132 PROJECT: 114WMS                                *
     H*               - ADD prompt for DB/CR Tracking/Allow print     *
     H*                 form (RRDBCR), Move all up by one line.       *
     H*                SURCHARGE                                      *
     H*                                                               *
     H* 11/27/06 C:65  PROJECT SUPP#51114/114WGH                      *
     H*                ADD STAT CODE FOR BALANCE TO INCR LIMITS MIN.  *
     H*                                                               *
     H* 10/02/06 C:33  Project: WC1196                                *
     H*                Business Cat Assignment                        *
     H*                                                               *
     H* 06/20/06 C:168 PROJECT: 132WEW                                *
     H*                MOVED AROUND SOME FIELDS TO THIRD SCREEN AND   *
     H*                ADDED NEW FIELD TO FIRST SCREEN FOR FACULTATIVE*
     H*                SURCHARGE                                      *
     H*                                                               *
     H* 04/12/05 C:132 PROJECT: WC1417                                *
     H*                Change "TRIA" reference to "Foreign Terrorism" *
     H*                                                               *
     H* 08/10/05 C:136 PROJECT: WC1136                                *
     H*                ADDED LCM EXP DT FIELD                         *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109CQ}                                        *
     H*                Add flag to include exp const on deposit tax.  *
     H*                                                               *
     H* 06/24/05 C:132 PROJECT: WC1136                                *
     H*                Added field in search for Loss cost Multiplier *
     H*                                                               *
     H* 03/31/05 C:53  PROJECT: WC1287/135WCB                         *
     H*                Added field for NAICS code requirements        *
     H*                                                               *
     H* 01/07/05 C:61  PROJECT WC1250                                 *
     H*                Add fields for Domestic Terror Code and        *
     H*                Domestic Terror Rate.                          *
     H*                                                               *
     H* 10/20/04 C:132 PROJECT: WC1136                                *
     H*                Add field to display the Loss Cost Multiplier, *
     H*                WMPRP/PRLCM, Do edits in Add/Change mode.      *
     H*                ADD F10 SEARCH ON LOSS COST MULTIPLIER        *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                ADD VOLUNTARY LABOR CHARGE FIELDS.             *
     H*                                                               *
     H* 11/04/03 C:65  PROJECT WC1120                                 *
     H*                ADD NEW FLAG FOR TERRORISM ON ASSESSMENTS.     *
     H*                                                               *
     H* 05/22/03 C:65  PROJECT WC1120                                 *
     H*                ADD NEW FIELD FOR NON-PAYROLL TERRORISM RATE.  *
     H*                                                               *
     H* 03/04/03 C:132 PROJECT WC1119                                 *
     H*                Add field for Unemployment # required (in file *
     H*                WDRRP/L).                                      *
     H*                                                               *
     H* 01/09/03 C:61  PROJECT WC1120                                 *
     H*                Add fields for Terrorism Risk Act (Rate, Code, *
     H*                Effective Date).                               *
     H*                                                               *
     H* 01/13/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                               *
     H* 06/29/02 C:136 PROJECT: WC1080 (KY DEDUCTIBLE BILLING TAX)    *
     H*                ADD FIELD THAT SPECIFIES WHETHER OR NOT TO TAX *
     H*                DEDUCTIBLE BILLINGS.                           *
     H*                                                               *
     H* 11/20/01 C:65  PROJECT: WC1058                                *
     H*                ADD FIELDS FOR OPTIONAL SAFETY FEE HEADING     *
     H*                AND ENDORSEMENT ID FORM#.                      *
     H*                                                               *
     H* 02/21/01 C:65  PROJECT: WC1026                                *
     H*                ADD STAT CODE FIELDS FOR MERIT RATING.         *
     H*                                                               *
     H* 01/02/01 C:65  PROJECT: WC1024                                *
     H*                REMOVE ASSESSMENT INFORMATION FROM SCR03 AND   *
     H*                ADD A CALL TO PROGRAM W0114R02 FOR SCR04.      *
     H*                                                               *
     H* 08/15/00 C:61  SUPPORT 23970                                  *
     H*              - Disallow F5=Refresh in ADD mode.               *
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 03/28/00 C:113 PROJECT: SUPPORT #21942                        *
     H*                Removed edit to allow a percentage or a flat   *
     H*                charge if waiver type is specific per KK.      *
     H*                                                               *
     H* 02/23/00  C:113 PROJECT: SUPPORT #21942                       *
     H*                 -Changed waiver heading on SCR02.             *
     H*                 -Changed error check to allow waiver level to *
     H*                  be blank if waiver type is blank.            *
     H*                 -Corrected display of waiver fields on SCR02  *
     H*                  when in display mode.                        *
     H*                                                               *
     H* 02/09/00  C:33  PROJECT WC919                                 *
     H*                 ADD DEFAULT RATE SET CODE.                    *
     H*                                                               *
     H* 10/25/99  C:65  PROJECT WC908                                 *
     H*                 ADD STANDARD INCR LIMITS FIELDS.              *
     H*                                                               *
     H* 10/19/99  C:65  PROJECT WC951                                 *
     H*                 EXPAND THE TWO PERCENTAGE FIELDS TO 9.6P      *
     H*                                                               *
     H* 10/07/99  C:65  PROJECT WC937                                 *
     H*                 ADD NEW PERCENTAGE FIELDS FOR F-CLASS ASSM.   *
     H*                                                               *
     H* 09/28/99  C:65  PROJECT WC938                                 *
     H*                 ADD FLAG FOR CALCULATION OF STOCK DISCOUNT    *
     H*                 INDEPENDENT OF OTHER STATES.                  *
     H*                                                               *
     H* 09/20/99 C:61  PROJECT WC949                                  *
     H*                Add column for 'Auto renew (N/ )' next to each *
     H*                applicable rating factor.                      *
     H*                                                               *
     H* 08/06/99 C:113 PROJECT: CM045                                 *
     H*                Do not display waiver fields on SCR02 if a     *
     H*                waiver stat code was not entered on SCR01.     *
     H*                                                               *
     H*  05/12/99 C:113 PROJECT: CM045                                *
     H*                 Waiver of Subro rewrite.                      *
     H*                 Added fields for new processing of waiver     *
     H*                 surcharge and aircraft surcharge.             *
     H*                                                               *
     H*  03/18/99 C:65 PROJECT: 48WOY                                 *
     H*                EXPAND STATE TAX FIELD TO 4 DECIMAL PLACES.    *
     H*                                                               *
     H*  07/13/98 C:65 PROJECT: WC846                                 *
     H*                ALLOW STOCK DISC TO BE APPLIED TO MOD MANUAL.  *
     H*                                                               *
     H*  05/29/98 C:65 PROJECT: WC705                                 *
     H*                ADD OPTIONAL HEADING FOR STATE TAX.            *
     H*                                                               *
     H*  04/30/98 C:65 PROJECT: WC705                                 *
     H*                ADD NEW FLAGS FOR ASSESSMENT CALCULATIONS.     *
     H*                                                               *
     H*  02/09/98 C:65 PROJECT: WC705                                 *
     H*                ADD OPTIONAL HEADING FOR STOCK DISCOUNT.       *
     H*                                                               *
     H*  12/18/97 C:65 PROJECT: V3/WC776                              *
     H*                MOVED BOTTOM OF SCREEN 1 TO SCREEN 3.          *
     H*                ADDED FIRST AND SECOND ASSESSMENT TO SCREEN 3. *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS USED:                                              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD                                                     *
     H* N40 - UPDATE/INQUIRY                                          *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  30 - (WCR0504) The state code was not found in list.         *
     H*  31 - (WC30001) Record already exists.                        *
     H*  32 - (WCR0500) Invalid date.                                 *
     H*  34-39,41-43 (WCR0502) - VALUE CANNOT BE <= TO ZERO (CODES).  *
     H*  44 - (WCR0547) VALUE CANNOT BE < ZERO (MAX SURCHARGE).       *
     H*  45-46 (WCR30009/14) APPLICATION OF DISCOUNT FLAGS.           *
     H*  71 - (WC30009) Aircraft level flag.                          *
     H*  72 - (WCR0547) Waiver code < 0.                              *
     H*  73 - (WCR0503) Header can't be blank if code present.        *
     H*  74 - (WC30009) Waiver level flag.                            *
     H*  80 - (WC31317) Value must be 'N' or ' '.                     *
     H*  81 - (WC31317) Value must be 'N' or ' '.                     *
     H*  82 - (WC31317) Value must be 'N' or ' '.                     *
     H*  83 - (WC31317) Value must be 'N' or ' '.                     *
     H*  84 - (WC31317) Value must be 'N' or ' '.                     *
     H*  85 - (WC31317) Value must be 'N' or ' '.                     *
     H*  86 - (WCR0502) Value can not be <= 0.                        *
     H*                                                               *
     H* SCR02:                                                        *
     H*  20 - (WCR0547) VALUE CANNT BE LESS THAN ZERO.                *
     H*  21 - (WCR0547) VALUE CANNT BE LESS THAN ZERO.                *
     H*  22 - (WC30009) APPLICATION OF DISCOUNT FLAG.                 *
     H*  23 - (WCR0547) VALUE CANNT BE LESS THAN ZERO.                *
     H*  24 - (WCR0501) VALUE MUST BE Y OR N.                         *
     H*  25 - (WCR0501) VALUE MUST BE Y OR N.                         *
     H*  26 - (WCR0501) VALUE MUST BE Y OR N.                         *
     H*  27 - (WCR0501) VALUE MUST BE Y OR N.                         *
     H*  28 - (WCR0501) VALUE MUST BE Y OR N.                         *
     H*  75 - (WCR3142) Waiver type must be S, B or blank.            *
     H*  76 - (WCR0547) Waiver % < 0.                                 *
     H*  77 - (WCR0547) Waiver flat charge < 0.                       *
     H*  78 - (WCR0547) Waiver minimum < 0.                           *
     H*  79 - (WCR0590) Waiver % and flat charge mutually exclusive.  *
     H*                                                               *
     H* SCR03:                                                        *
     H*  47-51 DISCOUNT ERRORS                                        *
     H*  61-68 1ST & 2ND ASSESSMENT                                   *
     H*****************************************************************
     FW0014F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWDRRP     UF A E             DISK
     F                                     RENAME(PRR01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWDRRL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FSTSTL3    IF   E           K DISK
     FWMDSL3    IF   E           K DISK
     FWMDSP     IF   E             DISK
     F                                     RENAME(PDS01:PDS02)
     FWMRSL     IF   E           K DISK
     F*
     F*  LOSS COST MULTIPLIER
     FWDLML     IF   E           K DISK
     F*
     F*  Billing Schedule Code
     FWTBSP     IF   E             DISK    RENAME(PBS01:PBS0P)
     Fwmldl3    uf   e           k disk    rename(pld01:pld03)
     F                                     infds(LDfds3)
     fwdf1l1    if   e           k disk    rename(pf101:pf101)
     fwiratdftl uf a e           k disk    rename(pdf01:pdf00)
     fwiratddtl uf a e           k disk    rename(pdt01:pdt00)
     fwtddl3    if   e           k disk    rename(pdd01:pdd03)
     fwtlml3    if   e           k disk    rename(plm01:plm03)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F*
     fssylf     IF   E           K DISK
     f                                     IGNORE(SSY01)
     f                                     IGNORE(SPC01)
     f                                     IGNORE(SFB01)
     f                                     IGNORE(SRC01)
     f                                     IGNORE(SDR01)
     f                                     IGNORE(SBC01)
     f                                     IGNORE(SCS01)
     f                                     IGNORE(SGL01)
     f                                     IGNORE(SP201)
      /COPY SCOPYMT01D
      *
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D LDfds3          DS
     D  LDrrn3               397    400B 0
     D*
     D OPTNM           C                   CONST('Statistical Report-
     D                                     ing/Rating')
     D #OFSCR          C                   CONST(4)
     D COMAPI          C                   CONST('WA001R')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     DsvRRco#          s                   like(RRco#)
     DsvRRefDt         s                   like(RRefDt)
     DsvRRfnd          s                   like(RRfnd)
     DsvRRst           s                   like(RRst)

     DLargeDeduct      pr                  extPgm('W0051R38')
     D inKc                           1
     D inKl                           1
     D outMode                        1    const
     D outLDrrn                       9  0 const
     D outRRrrn                       9  0 const
     D outMode                        1    const
     d/copy wPrGroup
     C*
     C     *LIKE         DEFINE    RRFND         XXFND
     C     *LIKE         DEFINE    RREFDT        XXLCEX
     C     *LIKE         DEFINE    RRCO#         XXCO#
     C     *LIKE         DEFINE    RRST          XXST
     C     *LIKE         DEFINE    RREFDT        XXEFDT
     C     *LIKE         DEFINE    RSRTCD        XXRTCD
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY            10
     C*
     C*  DISCOUNT FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDCDE            1
     C                   KFLD                    XXEFDT
     C*
     C     DSKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDCDE            1
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  STAT RPT/RATING DEFINITION FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  LOSS COST MULTIPLIER
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ##KEY
     C*
     C     LMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ##KEY
     C                   KFLD                    ZZEFDT            7 0
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKKEY            10
      * Key List for file SWCPF
     C     keySSYLF      Klist
     C                   Kfld                    xxSky             2
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C*
     C/COPY SCOPYMT05C
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C*
      /Free
       Dou not %found or %eof;
          Delete (RRco#:RRfnd:0:0:0:RRst:RRefDt) pld03;
       Enddo;
       Dou not %found or %eof;
          Delete (RRco#:RRfnd:RRst:RRefDt) pdf00;
       Enddo;
       Dou not %found or %eof;
          Delete (RRco#:RRfnd:RRst:RRefDt) pdt00;
       Enddo;
      /End-free
     C*
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   EXSR      OTHFKS
     C*
     C*  IF ANY OTHER FUNCTION KEYS HAVE BEEN PROCESSED, DO NOT
     C*  CHECK FOR ERRORS
     C*
     C     *IN92         IFEQ      *OFF
     C*
     C*  VERIFY STATE AND EFFECTIVE DATE IF IN ADD MODE.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  STATE MUST BE VALID.
     C*
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        9030
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
B02  C  N90SRERR         IFEQ      '1'
     C                   SETON                                        9032
E02  C                   ENDIF                                                  SRERR=1
     C*
     C*  RECORD MUST NOT ALREADY EXISTS FOR ST/EFDT
     C*
     C                   MOVE      SCST          XXST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C     RRKY          CHAIN     PRR01                              60
     C  N90*IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  *IN60 EQ 0
     C*
     C                   ENDIF                                                  *IN40 EQ 1
     C*
     C*  ALL STAT CODES MUST BE GREATER THAN 0 IF THEY HAVE HEADER OR
     C*  LEVEL INFORMATION PRESENT.
     C*
     C  N90SCCCCD        IFLT      0
     C                   MOVE      *ON           *IN86
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C     SCDV1H        IFNE      *BLANK
     C     SCDV1L        ORNE      *BLANK
     C  N90SCDV1C        IFLE      0
     C     @@CFLG        ANDNE     '1'
     C     SCDV1C        ORLE      0
     C     @@CFLG        ANDEQ     '1'
     C     SCST          ANDNE     'NM'
     C     SCDV1C        ORLT      0
     C     @@CFLG        ANDEQ     '1'
     C     SCST          ANDEQ     'NM'
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C                   ENDIF                                                  SCDV1H IFNE
     C*
     C  N90SCDV1C        IFNE      *ZERO
     C     SCDV1H        ANDEQ     *BLANKS
     C     SCDV1L        ORNE      *BLANKS
     C     SCDV1H        ANDEQ     *BLANKS
     C     SCST          ANDEQ     'NM'
     C     @@CFLG        ANDEQ     '1'
      *
     C                   MOVE      *ON           *IN52
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDV1C IFNE
     C*
     C* DEVIATION #1 FLAG MUST BE VALID
     C*
     C     SCDV1C        IFNE      *ZERO
     C     SCDV1L        ORNE      *BLANK
     C     SCST          ANDNE     'NM'
     C     @@CFLG        ANDNE     '1'
     C     SCDV1H        ORNE      *BLANKS
     C     SCST          ANDEQ     'NM'
     C     @@CFLG        ANDEQ     '1'
     C  N90SCDV1L        IFNE      'J'
     C     SCDV1L        ANDNE     'E'
     C     SCDV1L        ANDNE     'S'
     C     SCDV1L        ANDNE     'D'
     C     SCDV1L        ANDNE     'M'
     C                   MOVE      *ON           *IN45
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDV1L IFNE
     C                   ENDIF                                                  SCDV1C IFNE
     C*
     C     SCDV2H        IFNE      *BLANK
     C     SCDV2L        ORNE      *BLANK
     C  N90SCDV2C        IFLE      0
     C                   MOVE      *ON           *IN35
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C                   ENDIF                                                  SCDV2H IFNE
     C*
     C  N90SCDV2C        IFNE      *ZERO
     C     SCDV2H        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN53
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDV2C IFNE
     C*ARD Rate Split flag must be (Y/ )
     C  N90SCDV1S        IFNE      *BLANKS
     C     SCDV1S        ANDNE     'Y'
     C                   SETON                                        61
     C                   SETON                                        90
     C                   ENDIF
     C* DEVIATION #2 FLAG MUST BE VALID
     C*
     C     SCDV2C        IFNE      *ZERO
     C     SCDV2L        ORNE      *BLANK
     C  N90SCDV2L        IFNE      'J'
     C     SCDV2L        ANDNE     'E'
     C     SCDV2L        ANDNE     'S'
     C     SCDV2L        ANDNE     'D'
     C     SCDV2L        ANDNE     'M'
     C                   MOVE      *ON           *IN46
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDV2L IFNE
     C                   ENDIF                                                  SCDV2C IFNE
     C*ARD Rate Split flag must be (Y/ )
     C  N90SCDV2S        IFNE      *BLANKS
     C     SCDV2S        ANDNE     'Y'
     C                   MOVE      *ON           *IN62
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C  N90SC1AHD        IFNE      *BLANKS
     C     SC1ACD        ANDLE     0
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SC1ACD        IFNE      *ZERO
     C     SC1AHD        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN54
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SC1ACD IFNE
     C*
     C  N90SC1BHD        IFNE      *BLANKS
     C     SC1BCD        ANDLE     0
     C                   MOVE      *ON           *IN37
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SC1BCD        IFNE      *ZERO
     C     SC1BHD        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN55
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SC1BCD IFNE
     C*
     C  N90SCCRHD        IFNE      *BLANKS
     C     SCCRCD        ANDLE     0
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCCRCD        IFNE      *ZERO
     C     SCCRHD        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN56
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCCRCD IFNE
     C*
     C  N90SCDBHD        IFNE      *BLANKS
     C     SCDBCD        ANDLE     0
     C                   MOVE      *ON           *IN39
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCDBCD        IFNE      *ZERO
     C     SCDBHD        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN57
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDBCD IFNE
     C*
     C  N90SCECCD        IFLE      0
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMPCD        IFLE      0
     C                   MOVE      *ON           *IN42
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCDCCD        IFLT      0
     C                   MOVE      *ON           *IN43
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C* MAX SURCHARGE CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCAMAX        IFLT      *ZERO
     C                   MOVE      *ON           *IN44
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCAMAX IFLT
     C*
     C                   ENDIF                                                  *IN92=*OFF
     C*
     C     SCDCCD        IFNE      *ZERO
     C  N90SCASFL        IFNE      'J'
     C     SCASFL        ANDNE     'E'
     C     SCASFL        ANDNE     'S'
     C     SCASFL        ANDNE     'D'
     C                   MOVE      *ON           *IN71
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDV2L IFNE
     C                   ENDIF                                                  SCDV2C IFNE
     C*
     C  N90SCWVHD        IFNE      *BLANKS
     C     SCWVCD        ANDLE     0
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCWVCD        IFNE      *ZERO
     C     SCWVHD        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN73
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCWVCD IFNE
      *
     C  N90SCVLCH        IFNE      *BLANKS
     C     SCVLCC        ANDLE     0
     C                   MOVE      *ON           *IN78
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCVLCC        IFNE      *ZERO
     C     SCVLCH        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN79
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCWVCD IFNE
     C*
     C  N90SCMRRD        IFLT      0
     C                   MOVE      *ON           *IN75
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMRRN        IFLT      0
     C                   MOVE      *ON           *IN76
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMRRC        IFLT      0
     C                   MOVE      *ON           *IN77
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C* Edit merit rating threshold and 2nd stat code fields.
     C*
     C                   Clear                   msmrrdt
     C                   Clear                   msmrrd2
     C                   Clear                   ermrrdt
     C                   Clear                   ermrrd2
     C                   Clear                   msmrrct
     C                   Clear                   msmrrc2
     C                   Clear                   ermrrct
     C                   Clear                   ermrrc2
     C*
     C* Debit edits
     C*
     C  N90SCMRRDT       IFLE      1
     C     SCMRRDT       ANDNE     0
     C                   Move      *on           ermrrdt
     C                   Eval      msmrrdt = 'Minimum debit for 2nd stat code' +
     C                             ' cannot be less than or equal to one.'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMRRDT       IFEQ      0
     C     SCMRRD2       ANDNE     0
     C                   Move      *on           ermrrdt
     C                   Eval      msmrrdt = 'Minimum must be specified when' +
     C                             ' a 2nd stat code is entered.'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMRRD2       IFLT      0
     C                   Move      *on           ermrrd2
     C                   Eval      msmrrd2 = '2nd stat code for merit debit' +
     C                             ' cannot be less than zero.'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMRRD2       IFEQ      0
     C     SCMRRDT       ANDNE     0
     C                   Move      *on           ermrrd2
     C                   Eval      msmrrd2 = '2nd stat code must be entered' +
     C                             ' when there is a minimum specified.'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C* Credit edits
     C*
     C  N90SCMRRCT       IFGE      1
     C     SCMRRCT       ANDNE     0
     C                   Move      *on           ermrrct
     C                   Eval      msmrrct = 'Minimum credit for 2nd stat cod' +
     C                             'e cannot be greater than or equal to one.'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMRRCT       IFEQ      0
     C     SCMRRC2       ANDNE     0
     C                   Move      *on           ermrrct
     C                   Eval      msmrrct = 'Minimum must be specified when' +
     C                             ' a 2nd stat code is entered.'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMRRC2       IFLT      0
     C                   Move      *on           ermrrc2
     C                   Eval      msmrrc2 = '2nd stat code for merit credit' +
     C                             ' cannot be less than zero.'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C  N90SCMRRC2       IFEQ      0
     C     SCMRRCT       ANDNE     0
     C                   Move      *on           ermrrc2
     C                   Eval      msmrrc2 = '2nd stat code must be entered' +
     C                             ' when there is a minimum specified.'
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
      *  Facultative Reinsurance Charge stat code
      *
     C  N90SCARCC        IFLT      0
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCARCC LT 0
     C*
     C* VALIDATE BALANCE TO INCR LIMITS MIN STAT CODE.
     C*
     C  N90SCILMC        IFLT      0
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  CODE IFLE 0
     C*
     C*  If the Waiver stat code is blank, do not display waiver fields
     C*  on screen 2.
     C*
     C  N90SCWVCD        IFNE      *ZERO
     C                   SETON                                        33
     C                   ELSE
     C                   SETOFF                                       33
     C                   ENDIF
      *
      *  Rating factor renew flags must be 'N' or *BLANK.
      *
     C  N90              SELECT
      *
     C     SCRWCC        WHENNE    'N'
     C     SCRWCC        ANDNE     *BLANK
     C                   SETON                                        9080
      *
     C     SCRWD1        WHENNE    'N'
     C     SCRWD1        ANDNE     *BLANK
     C                   SETON                                        9081
      *
     C     SCRWD2        WHENNE    'N'
     C     SCRWD2        ANDNE     *BLANK
     C                   SETON                                        9082
      *
     C     SCRW1A        WHENNE    'N'
     C     SCRW1A        ANDNE     *BLANK
     C                   SETON                                        9083
      *
     C     SCRW1B        WHENNE    'N'
     C     SCRW1B        ANDNE     *BLANK
     C                   SETON                                        9084
      *
     C     SCRWDC        WHENNE    'N'
     C     SCRWDC        ANDNE     *BLANK
     C                   SETON                                        9085
      *
     C                   ENDSL
      /Free
       If scAnCs <> 0;
          If scAnCm <= 0;
             erMult = *on;
             *in90 = *on;
             msMult =  'WCR3183';
          Endif;
       Endif;
      /End-free
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     1             XXSCR             1 0
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   EXSR      OTHFKS
     C*** begin add ***  09/18/23  ***********************************
     C                   Clear                   msSFCD
     C                   Clear                   erSFCD
     C*** end add *****  09/18/23  ***********************************
     C*
     C*  IF ANY OTHER FUNCTION KEYS HAVE BEEN PROCESSED, DO NOT
     C*  CHECK FOR ERRORS
     C*
     C     *IN92         IFEQ      *OFF
     C*
     C* SAFETY FEE PERC CAN'T BE LESS THAN ZERO.
     C*
     C  N90SCSFPC        IFLT      0
     C                   MOVE      *ON           *IN20
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSFPC IFLT
     C     SCSFPC        IFNE      *ZERO
     C*
     C  N90SCSFAP        IFNE      'J'
     C     SCSFAP        ANDNE     'E'
     C     SCSFAP        ANDNE     'S'
     C     SCSFAP        ANDNE     'D'
     C     SCSFAP        ANDNE     'X'
     C     SCSFAP        ANDNE     'Y'
     C                   MOVE      *ON           *IN22
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSFAP IFNE
     C*
     C  N90SCSFAM        IFLT      0
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSFAM IFLT
     C*
     C  N90SCSPFL        IFNE      'Y'
     C     SCSPFL        ANDNE     'N'
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCSPFL IFNE
     C*
     C  N90SCYRAD        IFNE      'Y'
     C     SCYRAD        ANDNE     'N'
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCYRAD IFNE
     C*
     C  N90SCCXAD        IFNE      'Y'
     C     SCCXAD        ANDNE     'N'
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCCXAD IFNE
     C*
     C                   ENDIF                                                  SCSFPC IFNE
     C*** begin add ***  09/18/23  ***********************************
     C  N90SCSFCD        IFLT      0
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           erSFCD
     C                   Eval      msSFCD = 'Stat code cannot be less ' +
     C                                      'than zero.'
     C                   ENDIF                                                  SCSFCD IFLT
     C*** end add *****  09/18/23  ***********************************
     C*
     C  N90SCTXPC        IFLT      0
     C                   MOVE      *ON           *IN23
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCTXPC IFLT
     C*
     C     SCTXPC        IFNE      *ZERO
     C*
     C  N90SCTDFL        IFNE      'Y'
     C     SCTDFL        ANDNE     'N'
     C                   MOVE      *ON           *IN24
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCTDFL IFNE
     C*
     C  N90SCTEFL        IFNE      'Y'
     C     SCTEFL        ANDNE     ' '
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCTEFL IFNE
     C*
     C  N90SCRDTX        IFNE      'Y'
     C     SCRDTX        ANDNE     'N'
     C                   MOVE      *ON           *IN58
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCRDTX IFNE
     C*
     C  N90SCIDTP        IFNE      'Y'
     C     SCIDTP        ANDNE     'N'
     C                   MOVE      *ON           *IN28
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCIDTP IFNE
     C*
     C                   ENDIF                                                  SCTXPC IFNE
     C*
     C*  Waiver type must be 'B', 'S' or blank.
     C*
     C  N90SCWDTP        IFNE      'S'
     C     SCWDTP        ANDNE     'B'
     C     SCWDTP        ANDNE     ' '
     C                   SETON                                        75  90
     C                   ENDIF                                                  SCWDTP!=B/S/
     C*
     C*  Waiver default percentage cannot be less than zero.
     C*
     C  N90SCWDPC        IFLT      0
     C                   SETON                                        76  90
     C                   ENDIF                                                  SCWDPC < 0
     C*
     C*  Waiver default flat charge cannot be less than zero.
     C*
     C  N90SCWDFC        IFLT      0
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCWDPF < 0
     C*
     C*  Waiver default minimum cannot be less than zero.
     C*
     C  N90SCWDMN        IFLT      0
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCWDMN < 0
     C*
     C*  Flat charge and percentage are mutually exclusive.
     C*
     C  N90SCWDPC        IFNE      *ZERO
     C     SCWDFC        ANDNE     *ZERO
     C                   SETON                                        79  90
     C                   ENDIF                                                  SCWDPC != 0
     C*
     C*  If the waiver type is blanket 'B', percentage and flat charge
     C*  cannot both be blank.
     C*
     C  N90SCWDTP        IFEQ      'B'
     C     SCWDPC        ANDEQ     0
     C     SCWDFC        ANDEQ     0
     C                   SETON                                        32  90
     C                   ENDIF                                                  SCWVTP = B
     C*
     C*  Waiver premium level flag must be valid
     C*
     C     SCWDTP        IFNE      *BLANKS
     C  N90SCWVFL        IFNE      'J'
     C     SCWVFL        ANDNE     'E'
     C     SCWVFL        ANDNE     'S'
     C     SCWVFL        ANDNE     'D'
     C     SCWVFL        ANDNE     'M'
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCWVFL IFNE
     C                   ENDIF                                                  SCWVCD IFNE
     C*
     C                   ENDIF                                                  *IN92=*OFF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     2             XXSCR             1 0
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
      /Free
       // SetOff error messages
       erDftBilCd = *off;
       erDftLsID  = *off;
       msDftBilCd = *blanks;
       msDftLsID  = *blanks;
      /End-Free
     C*
     C*  CHECK IF OTHER FUNCTION KEYS HAVE BEEN PRESSED
     C*
     C                   EXSR      OTHFKS
     C*
     C*  IF ANY OTHER FUNCTION KEYS HAVE BEEN PROCESSED, DO NOT
     C*  CHECK FOR ERRORS
     C*
     C     *IN92         IFEQ      *OFF

     C*
     C*  Check Default Billing Schedule Code (ie..payment plan)
     C*       is valid if entered
      /Free
       If *in90 = *off;
          If SCBSCD <> 0;
             kkKey = %EDITC(scBsCd:'X');
             Chain (xxCo#:kkKey) PBS01;
             If Not %found or BSstat <> 'A';
                erDftBilCd = *on;
                *in90 = *on;
                msDftBilCd = 'WCR1401';
             Endif;
          Endif;
       Endif;

      /End-free
     C*
     C* DEDUCTIBLE DISCOUNT FLAG MUST BE VALID.
     C*
     C  N90SCRDPR        IFNE      'M'
     C     SCRDPR        ANDNE     'S'
     C                   MOVE      *ON           *IN47
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCRDPR IFNE
     C*
     C                   ENDIF                                                  *IN92=*OFF
     C     SCURFL        IFNE      'Y'
     C     SCURFL        ANDNE     ' '
     C                   SETON                                        90  87
     C                   ENDIF
     C     SCNAIC        IFNE      'Y'
     C     SCNAIC        ANDNE     ' '
     C                   SETON                                        90  88
     C                   ENDIF
      /free
       If scPyrlRDev <> 0 and (scPyrlRDev > 100 or scPyrlRDev < 0);
          msPyrlRDev  = 'WCR0795';
          *in90 = *On;
          erPyrlRDev  = *On;
       Endif;
      /end-free
      *
      * SHORT RATE PENALTY PREMIUM LEVEL FLAG MUST BE VALID.
      *
     C     SCSRAF        IFNE      'M'
     C     SCSRAF        ANDNE     'J'
     C     SCSRAF        ANDNE     'S'
     C                   SETON                                        90  79
     C                   ENDIF
      *
     C  N90SCBAFL        IFNE      *BLANKS
     C     SCBAFL        ANDNE     'G'
     C     SCBAFL        ANDNE     'S'
     C     SCBAFL        ANDNE     'N'
     C                   SETON                                        82  90
     C                   ENDIF
      *
      * Print State Risk ID (Y/ )
      *
     C  N90SCRIFL        IFNE      *BLANKS
     C     SCRIFL        ANDNE     'Y'
     C                   SETON                                        74  90
     C                   ENDIF
     C* Validate Charge fee for late S/R Payroll billing
     C  N90SCLATE        IFNE      'Y'
     C     SCLATE        ANDNE     ' '
     C                   SETON                                        90  97
     C                   SETOFF                                       9596
     C                   ENDIF                                                  SCLATE IFNE
     C*
     C* *Validate A/R code and fee when SCLATE is 'Y'
     C  N90SCLATE        IFEQ      'Y'
     C* A/R code should be entered and should be valid
     C     SCLTCD        IFEQ      *BLANKS
     C                   SETON                                        90  96
     C                   SETOFF                                       9597
     C                   ENDIF                                                  SCLTCD = ' '
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     SCLTCD        KKKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     TTSTAT        ORNE      'A'
     C                   SETON                                        909596
     C                   SETON                                        97
     C                   ENDIF                                                  *IN60 = *ON
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Fee amount should be greater than zero
     C  N90SCLTAM        IFLE      *ZERO
     C                   SETON                                        90  95
     C                   SETOFF                                       9697
     C                   ENDIF                                                  SCLTAM LE 0
     C                   ELSE
     C* *Validate A/R code and fee when SCLATE is ' '
     C* A/R code should be blank
     C     SCLTCD        IFNE      *BLANKS
     C                   SETON                                        909597
     C                   SETOFF                                       96
     C                   ENDIF                                                  SCLTAM LE 0
     C* Fee amount should be zero
     C  N90SCLTAM        IFNE      *ZERO
     C                   SETON                                        909596
     C                   SETOFF                                       97
     C                   ENDIF                                                  SCLTAM LE 0
     C                   ENDIF                                                  SCLATE EQ 'Y'
      *
      * Checks for Loss Cost Multiplier
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLCM         ANDLT     *ZEROS
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN76
     C                   ENDIF
     C  N90SCLCEX        IFNE      0
     C                   Z-ADD     SCLCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLCEX
     C     SRERR         IFEQ      '1'
     C                   SETON                                        80  90
     C                   ENDIF
     C                   ENDIF
      *
     C  N90SCLCEX        IFNE      0
     C                   Z-ADD     SCLCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLCEX
     C     XXLCEX        IFLT      XXEFDT
     C                   SETON                                        81  90
     C                   ENDIF
     C                   ENDIF
     C*
     C*  BODILY INJURY BY ACCIDENT MUST BE >= ZERO
     C*
     C  N90SCACC         IFLT      *ZEROS
     C                   SETON                                        27  90
     C                   ENDIF
     C*
     C*  BODILY INJURY BY DISEASE MUST BE >= ZERO
     C*
     C  N90SCDIS         IFLT      *ZEROS
     C                   SETON                                        28  90
     C                   ENDIF
     C*
     C*  BODILY INJURY BY DISEASE AGGR MUST BE >= ZERO
     C*
     C  N90SCAGG         IFLT      *ZEROS
     C                   SETON                                        29  90
     C                   ENDIF
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     3             XXSCR             1 0
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*===============================================================
     C* ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04
     C*===============================================================
     C     ERRCK4        BEGSR
     C*
     C*  Terror CLASS CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCTERC        IFLT      *ZERO
     C                   SETON                                        90  72
     C                   ENDIF                                                  SCTERC
     C*
     C*  Terror RATE CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCTERR        IFLT      *ZERO
     C                   SETON                                        90  73
     C                   ENDIF                                                  SCTERC
     C*
     C*  NON-PAYROLL Terror RATE CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCTER2        IFLT      *ZERO
     C                   SETON                                        90  74
     C                   ENDIF                                                  SCTER2
     C*
     C* Domestic Terror CLASS CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCDTRC        IFLT      *ZERO
     C                   SETON                                        90  77
     C                   ENDIF                                                  SCDTRC
     C*
     C* Domestic Terror RATE CANNOT BE LESS THAN ZERO.
     C*
     C  N90SCDTRR        IFLT      *ZERO
     C                   SETON                                        90  78
     C                   ENDIF                                                  SCDTRR
     C*
     C*  INCLUDE TERRORISM CHARGE ON ASSMNT CALCS FLAG MUST BE VALID
     C  N90SCPTFL        IFNE      'Y'
     C     SCPTFL        ANDNE     ' '
     C                   SETON                                        90  94
     C                   ENDIF                                                  SCPTFL IFNE
     C*
     C                   SETOFF                                       42
     C*  INCLUDE TERRORISM CHARGE ON ASSMNT CALCS FLAG MUST BE VALID
     C  N90SCTERL        IFNE      'Y'
     C     SCTERL        ANDNE     ' '
     C                   SETON                                        90
     C                   SETON                                        42
     C                   ENDIF                                                  SCTEFL IFNE
      *
      *  DEBIT factor entered must not be less than 1.0
      *
     C  N90SCDMAX        IFNE      *ZERO
     C     SCDMAX        ANDLT     1.0
     C                   SETON                                        90  79
     C                   ENDIF                                                  SCDMAX
     C*
     C                   SETOFF                                       43
     C*  INCLUDE TERRORISM CHARGE ON ASSMNT CALCS FLAG MUST BE VALID
     C  N90SCPPFL        IFNE      'Y'
     C     SCPPFL        ANDNE     ' '
     C                   SETON                                        90
     C                   SETON                                        43
     C                   ENDIF                                                  SCPPFL IFNE
      *
      *  Provide for scheduled DB/CR Tracking/Allow form print
     C  N90SCDBCR        IFNE      'Y'
     C     SCDBCR        ANDNE     ' '
     C                   SETON                                        90  68
     C                   ENDIF                                                  SCTERA IFNE
     C*
     C*** begin del ***  11/14/23  ********************************************
     C* no edit required anymore as its a checkbox and in96 seems to be used
     C* for other purposes, thus removing this coding entirely.
      *  Provide for scheduled DB/CR Tracking/Allow form print
     C**** *IN96         IFEQ      *ON
     C**N90SCDCAP        IFNE      'Y'
     C**** SCDCAP        ANDNE     ' '
     C**** *IN96         ANDEQ     *ON
     C****               SETON                                        90  95
     C****               ENDIF                                                  SCDCAP  FNE
     C****               ENDIF                                                  *IN96
     C*** end del *****  11/14/23  ********************************************
     C*
     C* STOCK DISCOUNT FLAG MUST BE VALID.
     C*
     C  N90SCDSPR        IFNE      'S'
     C     SCDSPR        ANDNE     'D'
     C     SCDSPR        ANDNE     'M'
     C                   MOVE      *ON           *IN51
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCDSPR IFNE
     C*
     C*  PREMIUM DISCOUNT & DISCOUNT TYPE ARE MUTUALLY EXCLUSIVE
     C*
     C  N90SCDSPC        IFNE      *ZERO
     C     SCDISC        ANDNE     *BLANK
     C                   SETON                                        48
     C                   SETON                                        90
     C                   ENDIF                                                  SCDSPC<>*ZERO
     C*
     C*  DISCOUNT MUST BE VALID
     C*
     C  N90SCDISC        IFNE      *BLANK
     C                   MOVEL     SCDISC        XXDCDE
     C     DSKY          SETGT     PDS01
     C     DSKY2         READPE    PDS01                                  49
     C     *IN49         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF                                                  *IN49 IFEQ
     C                   ENDIF                                                  SCDISC<>*BLANK
     C*
     C  N90SCDSIN        IFNE      'Y'
     C     SCDSIN        ANDNE     *BLANK
     C                   SETON                                        69
     C                   SETON                                        90
     C                   ENDIF                                                  SCDSIN IFNE
     C*
     C                   Clear                   erDCL
     C                   Clear                   msDCL
     C                   Clear                   erMinFD
     C                   Clear                   msMinFD
     C                   Clear                   erMinSD
     C                   Clear                   msMinSD
     C                   Clear                   erMinSC
     C                   Clear                   msMinSC
     C     SCDCL         IFNE      'N'
     C     SCDCL         ANDNE     'F'
     C     SCDCL         ANDNE     'S'
     C                   Eval      erDCL = *on
     C                   Eval      msDCL = 'Value must be N,F, or S.'
     C                   SETON                                        90
     C                   Endif
     C*
     C     SCMinFD       IFNE      *zero
     C     SCDCL         ANDEQ     'N'
     C                   Eval      erMinFD = *on
     C                   Eval      msMinFD = 'Value must be zero if no' +
     C                              ' discount logic selected.'
     C                   SETON                                        90
     C                   Endif
     C*
     C     SCMinSD       IFNE      *zero
     C     SCDCL         ANDEQ     'N'
     C                   Eval      erMinSD = *on
     C                   Eval      msMinSD = 'Value must be zero if no' +
     C                              ' discount logic selected.'
     C                   SETON                                        90
     C                   Endif
     C*
     C     SCMinSC       IFNE      *zero
     C     SCDCL         ANDEQ     'N'
     C                   Eval      erMinSC = *on
     C                   Eval      msMinSC = 'Value must be zero if no' +
     C                              ' discount logic selected.'
     C                   SETON                                        90
     C                   Endif
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     4             XXSCR             1 0
     C*
     C                   ENDSR                                                  ERRCK4
     C*
     C*===============================================================
     C* ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR05
     C*===============================================================
     C     ERRCK5        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     5             XXSCR             1 0
     C*
     C                   ENDSR                                                  ERRCK5
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
      ** Set off field focus indicator
     C                   EVAL      indDfLsID = *OFF
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCDISC'
     C*
     C                   MOVE      *BLANK        QQCODE            1
     C                   Z-ADD     0             QQYR              2 0
     C                   Z-ADD     *ZERO         QQLMT            11 2
     C                   MOVE      *BLANK        QQEQTP            1
     C                   Z-ADD     DBRRN3        ##RRN
     C                   CALL      'W0150R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    SCST
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     PDS02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DSCODE        SCDISC
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  ##RRN IFNE
     C*
     C                   MOVE      *ON           *IN50
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCDRAT'
     C*
     C                   CALL      'W2471R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##KEY             3
     C*
     C     *INKL         IFEQ      *ON
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN72
     C                   ELSE
     C                   MOVE      *ON           *IN73
     C                   MOVEL(P)  ##KEY         SCDRAT
     C                   ENDIF                                                  *INKL=*ON
      *
     C     HFLD          WHENEQ    'SCBSCD'
     C                   CALL      'W2155R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
      /Free
       If *INKL = *On;
          *in73  = *on;  //reposition back to field;
       Else;
          Chain ##RRN PBS0P;
          If not (%found);
             *in73  = *on;  //reposition back to field;
          Else;
             scBSCD = %dec(BSKEY:3:0);
                indDfLsID = *on; //set focus ind on Default Lease Id
          EndIf;
       EndIf;

      /End-Free

     C     HFLD          WHENEQ    'SCLTCD'
     C                   MOVE      *BLANKS       SCKEY            10
      *
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C     *INKL         IFNE      *ON
     C     SCKEY         ANDNE     *BLANKS
     C                   MOVEL     SCKEY         SCLTCD
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN95
     C                   MOVE      *ON           *IN96
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C                   ENDSL
     C*
     C     HFLD          IFEQ      'SCLCM'
     C                   CALL      'W2472R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *OFF          ##KC              1
     C                   PARM      *OFF          ##KL              1
     C                   PARM      SCST          ##ST              2
     C                   PARM      *BLANKS       ##KEY             3
     C*
     C     ##KEY         IFNE      *BLANKS
     C                   MOVE      *HIVAL        ZZEFDT
     C     LMKY2         SETGT     PLM01
     C     LMKY          READPE    PLM01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     LMLCM         SCLCM
     C                   SETON                                            90
     C                   ENDIF
     C                   ENDIF                                                  @@KEY <> BLANKS
     C                   ENDIF                                                  HFLD=SCLCM
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
      *
     C                   MOVE      RRURFL        SCURFL
     C                   MOVE      RRNAIC        SCNAIC
      /free
       scPyrlRDev  = RRrdev;
      /end-free
     C                   MOVEL     RRBAFL        SCBAFL
     C                   MOVEL(P)  RRRIFL        SCRIFL
     C*
     C                   IF        RRRDTX = ' '
     C                   EVAL      SCRDTX = 'N'
     C                   ELSE
     C                   MOVE      RRRDTX        SCRDTX
     C                   ENDIF
     C                   MOVE      RRST          SCST
     C                   Z-ADD     RREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     RRCCCD        SCCCCD
     C                   Z-ADD     RRDV1C        SCDV1C
     C                   Z-ADD     RRDV2C        SCDV2C
     C                   MOVE      RRSFHD        SCSFHD
     C*** begin add ***  09/18/23  ***********************************
     C                   Z-ADD     RRSFCD        SCSFCD
     C*** end add *****  09/18/23  ***********************************
     C                   MOVE      RRENID        SCENID
     C                   Z-ADD     RR1ACD        SC1ACD
     C                   Z-ADD     RR1BCD        SC1BCD
     C                   Z-ADD     RRCRCD        SCCRCD
     C                   Z-ADD     RRDBCD        SCDBCD
     C                   Z-ADD     RRECCD        SCECCD
     C                   Z-ADD     RRMPCD        SCMPCD
     C                   Z-ADD     RRDCCD        SCDCCD
     C                   Z-ADD     RRAMAX        SCAMAX
     C                   MOVE      RRDV1L        SCDV1L
     C                   MOVE      RRDV2L        SCDV2L
     C                   MOVE      RRRDPR        SCRDPR
     C                   MOVE      RRDRAT        SCDRAT
      /free
       scBSCD     = RRBSCD;
       scDftLsID  = RRLSID;
       scDv1Dft   = RRDV1DFT;
       scDv2Dft   = RRDV2DFT;
       scDv1Chg   = RRDV1CHG;
       scDv2Chg   = RRDV2CHG;
       scDv1DfRnw = RRDV1DFRNW;
       If RRDV1DFRNW = ' ';
          scDv1DfRnw = 'N';
       Endif;
       scDv2DfRnw = RRDV2DFRNW;
       If RRDV2DFRNW = ' ';
          scDv2DfRnw = 'N';
       Endif;
      /end-free
     C                   MOVE      RRDSPR        SCDSPR
     C                   MOVEL     RRDV1H        SCDV1H
     C                   MOVEL     RRDV2H        SCDV2H
     C                   MOVEL     RR1AHD        SC1AHD
     C                   MOVEL     RR1BHD        SC1BHD
     C                   MOVEL     RRCRHD        SCCRHD
     C                   MOVEL     RRDBHD        SCDBHD
     C                   MOVE      RRDISC        SCDISC
     C                   Z-ADD     RRDSPC        SCDSPC
     C                   Z-ADD     RRSFPC        SCSFPC
     C                   Z-ADD     RRSFAM        SCSFAM

     C                   IF        RRYRAD = ' '
     C                   EVAL      SCYRAD = 'N'
     C                   ELSE
     C                   MOVE      RRYRAD        SCYRAD
     C                   ENDIF

     C                   IF        RRCXAD = ' '
     C                   EVAL      SCCXAD = 'N'
     C                   ELSE
     C                   MOVE      RRCXAD        SCCXAD
     C                   ENDIF
     C                   MOVEL(P)  RRTXHD        SCTXHD
     C                   Z-ADD     RRTXPC        SCTXPC
     C                   MOVE      RRSFAP        SCSFAP
     C                   IF        RRTDFL = ' '
     C                   EVAL      SCTDFL = 'N'
     C                   ELSE
     C                   MOVE      RRTDFL        SCTDFL
     C                   ENDIF
     C                   MOVE      RRTEFL        SCTEFL
     C                   IF        RRSPFL = ' '
     C                   EVAL      SCSPFL = 'N'
     C                   ELSE
     C                   MOVE      RRSPFL        SCSPFL
     C                   ENDIF
     C                   MOVEL(P)  RRDSHD        SCDSHD
     C*
     C                   IF        RRIDTP = ' '
     C                   EVAL      SCIDTP = 'N'
     C                   ELSE
     C                   MOVE      RRIDTP        SCIDTP
     C                   ENDIF
     C*
     C                   MOVE      RRASFL        SCASFL
     C                   MOVE      RRWVAC        SCWVCD
     C                   MOVE      RRMRRD        SCMRRD
     C                   MOVE      RRMRRN        SCMRRN
     C                   MOVE      RRMRRC        SCMRRC
     C                   Z-ADD     RRARCC        SCARCC
     C                   MOVEL     RRWVHD        SCWVHD
     C                   MOVE      RRWDTP        SCWDTP
     C                   Z-ADD     RRWDPC        SCWDPC
     C                   Z-ADD     RRWDFC        SCWDFC
     C                   Z-ADD     RRWDMN        SCWDMN
     C                   MOVE      RRWVFL        SCWVFL
     C                   MOVE      RRRWCC        SCRWCC
     C                   MOVE      RRRWD1        SCRWD1
     C                   MOVE      RRRWD2        SCRWD2
     C                   MOVE      RRRW1A        SCRW1A
     C                   MOVE      RRRW1B        SCRW1B
     C                   MOVE      RRRWDC        SCRWDC
     C                   MOVE      RRDSIN        SCDSIN
     C                   MOVEL     RREMLB        SCEMLB
     C                   Z-ADD     RRACC         SCACC
     C                   Z-ADD     RRDIS         SCDIS
     C                   MOVEL     RRPULLSRD     SCPULLSRD
     C                   MOVEL     RRmono        scMonoSt
     C                   Z-ADD     RRAGG         SCAGG
     C                   MOVEL     RRDCAP        SCDCAP
     C** Set the indicator to display waiver fields when in display mode.
     C     *IN09         IFEQ      *ON
     C*
     C     SCWVCD        IFNE      *ZEROS
     C                   SETON                                        33
     C                   ELSE
     C                   SETOFF                                       33
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C                   Z-ADD     RRTERR        SCTERR
     C                   Z-ADD     RRTERC        SCTERC
     C                   Z-ADD     RRTER2        SCTER2
     C                   MOVE      RRTERH        SCTERH
     C                   MOVE      RRDTRH        SCDTRH
      *
     C                   Z-ADD     RRDTRR        SCDTRR
     C                   Z-ADD     RRDTRC        SCDTRC
     C                   MOVE      RRVLCC        SCVLCC
     C                   MOVEL     RRVLCH        SCVLCH
      *
     C                   MOVE      RRPTFL        SCPTFL
     C                   MOVE      RRTERL        SCTERL
     C                   Z-ADD     RRDMAX        SCDMAX
     C                   Z-ADD     RRCMAX        SCCMAX
     C                   Z-ADD     RRLCM         SCLCM
      *
     C                   Z-ADD     RRLCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLCEX
     C                   MOVE      RRILMC        SCILMC
     C                   MOVE      RRSRAF        SCSRAF
     C                   MOVE      RRDBCR        SCDBCR
     C                   MOVE      RRLATE        SCLATE
     C                   MOVE      RRLTCD        SCLTCD
     C                   Z-ADD     RRLTAM        SCLTAM
     C                   MOVE      RRFLPC        SCFLPC
     C   12
     CAN 40              MOVE      'F'           SCFLPC
     C                   MOVEL     RRSEMH        SCSEMH
     C                   MOVE      RRPPFL        SCPPFL
     C*
     C                   MOVE      RRDV1S        SCDV1S
     C                   MOVE      RRDV2S        SCDV2S
     C                   MOVEL     RRCCHD        SCCCHD
     C                   Move      RRTAXM        SCTAXM
     C                   Move      RRMRRD2       SCMRRD2
     C                   Move      RRMRRDT       SCMRRDT
     C                   Move      RRMRRC2       SCMRRC2
     C                   Move      RRMRRCT       SCMRRCT
     C                   Move      RRDCL         SCDCL
     C                   If        SCDCL = ' '
     C                   Move      'N'           SCDCL
     C                   Endif
     C                   Z-add     RRMinFD       SCMinFD
     C                   Z-add     RRMinSD       SCMinSD
     C                   Z-add     RRMinSC       SCMinSC
      /Free
       scAnCs = RRancs;
       scAnCm = RRancm;
       scPfAm = RRPfAm;
       scPfCd = RRPfCd;
       scPfHd = RRPfHd;
       isGradient = *off;
       If GetGrScoringVnd(RRCO#:RRFND) ='Gradient';
          isGradient = *on;
       Endif;
      /End-Free
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
      *
     C                   MOVEL     SCDCAP        RRDCAP
     C                   MOVE      SCURFL        RRURFL
     C                   MOVE      SCNAIC        RRNAIC
      /free
       RRrdev = scPyrlRDev;
      /end-free
     C                   MOVEL     SCBAFL        RRBAFL
     C                   MOVEL(P)  SCRIFL        RRRIFL
     C*
     C                   MOVE      SCRDTX        RRRDTX
     C                   Z-ADD     XXCO#         RRCO#
     C                   Z-ADD     XXFND         RRFND
     C                   MOVE      SCST          RRST
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RREFDT
     C                   Z-ADD     SCCCCD        RRCCCD
     C                   Z-ADD     SCDV1C        RRDV1C
     C                   Z-ADD     SCDV2C        RRDV2C
     C                   MOVE      SCSFHD        RRSFHD
     C*** begin add ***  09/18/23  ***********************************
     C                   Z-ADD     SCSFCD        RRSFCD
     C*** end add *****  09/18/23  ***********************************
     C                   MOVE      SCENID        RRENID
     C                   Z-ADD     SC1ACD        RR1ACD
     C                   Z-ADD     SC1BCD        RR1BCD
     C                   Z-ADD     SCCRCD        RRCRCD
     C                   Z-ADD     SCDBCD        RRDBCD
     C                   Z-ADD     SCECCD        RRECCD
     C                   Z-ADD     SCMPCD        RRMPCD
     C                   Z-ADD     SCDCCD        RRDCCD
     C                   Z-ADD     SCAMAX        RRAMAX
     C                   MOVE      SCDV1L        RRDV1L
     C                   MOVE      SCDV2L        RRDV2L
     C                   MOVE      SCRDPR        RRRDPR
     C                   MOVE      SCDRAT        RRDRAT
      /free
       RRBSCD    =   scBSCD;
       RRLSID    =   scDftLsID;
       RRDV1DFT   = scDv1Dft;
       RRDV2DFT   = scDv2Dft;
       RRDV1CHG   = scDv1Chg;
       RRDV2CHG   = scDv2Chg;
       RRDV1DFRNW = scDv1DfRNW;
       RRDV2DFRNW = scDv2DfRNW;
      /end-free
     C                   MOVE      SCDSPR        RRDSPR
     C                   MOVE      *BLANKS       RRDV1H
     C                   MOVE      *BLANKS       RRDV2H
     C                   MOVE      *BLANKS       RR1AHD
     C                   MOVE      *BLANKS       RR1BHD
     C                   MOVE      *BLANKS       RRCRHD
     C                   MOVE      *BLANKS       RRDBHD
     C                   MOVEL     SCDV1H        RRDV1H
     C                   MOVEL     SCDV2H        RRDV2H
     C                   MOVEL     SC1AHD        RR1AHD
     C                   MOVEL     SC1BHD        RR1BHD
     C                   MOVEL     SCCRHD        RRCRHD
     C                   MOVEL     SCDBHD        RRDBHD
     C                   MOVE      SCDISC        RRDISC
     C                   Z-ADD     SCDSPC        RRDSPC
     C                   Z-ADD     SCSFPC        RRSFPC
     C                   Z-ADD     SCSFAM        RRSFAM
     C                   MOVE      SCYRAD        RRYRAD
     C                   MOVE      SCCXAD        RRCXAD
     C                   Z-ADD     SCTXPC        RRTXPC
     C                   MOVEL(P)  SCTXHD        RRTXHD
     C                   MOVE      SCSFAP        RRSFAP
     C                   MOVE      SCTDFL        RRTDFL
     C                   MOVE      SCTEFL        RRTEFL
     C                   MOVE      SCSPFL        RRSPFL
     C                   MOVEL(P)  SCDSHD        RRDSHD
     C*
     C                   MOVE      SCIDTP        RRIDTP
     C                   MOVE      SCASFL        RRASFL
     C                   MOVE      SCWVCD        RRWVAC
     C                   MOVE      SCMRRD        RRMRRD
     C                   MOVE      SCMRRN        RRMRRN
     C                   MOVE      SCMRRC        RRMRRC
     C                   Z-ADD     SCARCC        RRARCC
     C                   MOVE      SCWVFL        RRWVFL
     C                   MOVEL     SCWVHD        RRWVHD
     C                   MOVE      SCWDTP        RRWDTP
     C                   Z-ADD     SCWDPC        RRWDPC
     C                   Z-ADD     SCWDFC        RRWDFC
     C                   Z-ADD     SCWDMN        RRWDMN
     C                   MOVE      SCRWCC        RRRWCC
     C                   MOVE      SCRWD1        RRRWD1
     C                   MOVE      SCRWD2        RRRWD2
     C                   MOVE      SCRW1A        RRRW1A
     C                   MOVE      SCRW1B        RRRW1B
     C                   MOVE      SCRWDC        RRRWDC
     C                   MOVE      SCDSIN        RRDSIN
     C                   MOVEL     SCEMLB        RREMLB
     C                   Z-ADD     SCACC         RRACC
     C                   Z-ADD     SCDIS         RRDIS
     C                   MOVEL     SCPULLSRD     RRPULLSRD
     C                   MOVEL     scMonoSt      RRmono
     C                   Z-ADD     SCAGG         RRAGG
     C                   Z-ADD     SCTERR        RRTERR
     C                   Z-ADD     SCTERC        RRTERC
     C                   Z-ADD     SCTER2        RRTER2
     C                   MOVE      SCTERH        RRTERH
     C                   MOVE      SCDTRH        RRDTRH
      *
     C                   Z-ADD     SCDTRR        RRDTRR
     C                   Z-ADD     SCDTRC        RRDTRC
     C                   MOVE      SCVLCC        RRVLCC
     C                   MOVEL     SCVLCH        RRVLCH
      *
     C                   MOVE      SCPTFL        RRPTFL
     C                   MOVE      SCTERL        RRTERL
     C                   Z-ADD     SCDMAX        RRDMAX
     C                   Z-ADD     SCCMAX        RRCMAX
      *
     C                   Z-ADD     SCLCM         RRLCM
     C                   Z-ADD     SCLCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRLCEX
     C                   MOVE      SCILMC        RRILMC
     C                   MOVE      SCSRAF        RRSRAF
     C                   MOVE      SCDBCR        RRDBCR
     C                   MOVE      SCLATE        RRLATE
     C                   MOVE      SCLTCD        RRLTCD
     C                   Z-ADD     SCLTAM        RRLTAM
     C                   MOVE      SCFLPC        RRFLPC
     C                   MOVEL     SCSEMH        RRSEMH
     C                   MOVE      SCPPFL        RRPPFL
     C                   MOVE      SCDV1S        RRDV1S
     C                   MOVE      SCDV2S        RRDV2S
     C                   MOVEL     SCCCHD        RRCCHD
     C                   Move      SCTAXM        RRTAXM
     C                   Move      SCMRRD2       RRMRRD2
     C                   Move      SCMRRDT       RRMRRDT
     C                   Move      SCMRRC2       RRMRRC2
     C                   Move      SCMRRCT       RRMRRCT
     C                   Move      SCDCL         RRDCL
     C                   If        SCDCL = ' '
     C                   Move      'N'           RRDCL
     C                   Endif
     C                   Z-add     SCMinFD       RRMinFD
     C                   Z-add     SCMinSD       RRMinSD
     C                   Z-add     SCMinSC       RRMinSC
      /Free
       RRancs = scAnCs;
       RRancm = scAnCm;
       RRPfAm = scPfAm;
       RRPfCd = scPfCd;
       RRPfHd = scPfHd;
      /End-Free
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
     C*
     C*
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C     *IN09         IFEQ      *ON
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C                   CALL      'W0114R02'                           99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      DBRRN         @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   MOVE      *OFF          *IN92
     C                   MOVE      *OFF          *IN40
     C                   MOVE      *OFF          *IN59
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
      /free
       xxSky = 'WC';
       Chain keySSYLF swc01;
       If %Found(ssylf);
          If WCFL53 = 'Y';
             *In59 = *On;
          EndIf;
       EndIf;
       dsp148 = *Off;
       If cltID = '148';
          dsp148 = *On;
       EndIf;
      /end-free
      *
     C     CLTID         COMP      '132'                                  12
      *
     C*
     C                   MOVE      *OFF          *IN89
     C     CLTID         IFEQ      '109'
     C                   MOVE      *ON           *IN89
     C                   ENDIF
      /free
       If cltID = '129';
          visb129 = *On;
       Else;
          visb129 = *Off;
       Endif;
       If cltID = '150';
          vis150 = *On;
       Else;
          vis150 = *Off;
       Endif;
       If cltID = '148';
          isAmWins = *On;
       Else;
          isAmWins = *Off;
       Endif;
      /end-free

      *
      *   Default Leasing Id - selection criteria
      *
      /free
       scLsIDSel = 'GTCO# =  ' + %char(##CO#) +
        ' AND GTTBID = ''NREL'' AND GTSTAT = ''A''';
      /end-free

     C                   MOVE      *OFF          *IN91
     C                   MOVE      '1'           @@CFLG
     C     CLTID         IFEQ      '132'
     C                   MOVE      *ON           *IN91
     C                   MOVE      '1'           @@CFLG            1
     C                   ENDIF
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'P'
     C                   MOVE      *ON           *IN40
     C                   ENDIF                                                  @@MODE EQ A
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   Z-ADD     @@CO#         XXCO#
     C                   Z-ADD     @@FND         XXFND
     C                   Z-ADD     @@FND         SCFND
     C                   MOVE      RRST          XXST
     C                   Z-ADD     RREFDT        XXEFDT
      /Free
       svRRco# = RRco#;
       svRRfnd = RRfnd;
       svRRst = RRst;
       svRRefDt = RRefDt;
      /End-free
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCFNAM
     C                   ENDIF                                                  *IN60 EQ 0

     C*
     C                   EXSR      MVDBSC
     C*
     C                   Z-ADD     0             XXSCR             1 0
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        RRUSER
     C                   MOVEL     Q1PGM         RRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDT
     C                   MOVEL     DSTIME        RRTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C     XXSCR         IFEQ      4
     C*
     C                   CALL      'W0114R02'                           99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C*
     C                   MOVE      *INKL         *IN90
     C     *INKC         IFEQ      *ON
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXSCR IFEQ
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   If        @@mode='A'
     C                   CLEAR                   DBRCDF
     C                   EndIf
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1PGM         RRPGM
     C                   MOVEL     Q1USER        RRUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDT
     C                   MOVEL     DSTIME        RRTIME
     C                   MOVEL     Q1PGM         RRIPGM
     C                   MOVEL     Q1USER        RRIUSR
     C                   MOVEL     SRDTO         RRIDT
     C                   MOVEL     DSTIME        RRITIM
     C*
     C                   WRITE     DBRCDF
     C                   eval      ##rrn=dbrrn
      * Copied From record was getting corrupted when screen 4 was passed,
      * record was already written and cancel was pressed to go back to 4th screen.
      * For Copy mode, change the @@rrn with new added rrn to stop record
      * corruption.
     C                   If        @@mode = 'P'
     C                   Eval      @@rrn=dbrrn
     C                   Endif
     C*
     C                   FEOD      WDRRP
     C*
     C                   MOVEL     'C'           @@MODE
     C*
     C                   CALL      'W0114R02'                           99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@mode        ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      @@co#         ##CO#             3 0
     C                   PARM      @@fnd         ##FND             3 0
     C*
     C                   MOVE      *INKL         *IN90
     C     *INKC         IFEQ      *ON
     C                   RETURN
     C                   ENDIF
     C*
      /Free
       If cpyFlg = *on and not (*inkc or *inkl);
          Chain(n) (svRRco#:svRRfnd:0:0:0:svRRst:svRRefDt) pld03;
          If %found;
             LargeDeduct(##kc:##kl:'P':LDrrn3:##rrn:'G');
          Endif;
          // Copy Carrier Rating Default records for Connect
          //  if factor/fee/discount is set up for the new record
          Setll (svRRco#:svRRfnd:svRRst:svRRefDt) pdf00;
          Dou %Eof(WIRATDFTL);
             Reade (svRRco#:svRRfnd:svRRst:svRRefDt) pdf00;
             If %Eof;
                Leave;
             Endif;
             // Ensure new WDRRP record has set up for the default, but
             //  don't check the std limits default record
             If DFpgm <> 'WICARRAT' And
              DefaultNotSetUp(DFkey) = *On;
                Iter;
             Endif;
             DFefdt = RRefdt;
             DFst = RRst;
            // Standard Limits record
             If DFpgm = 'WICARRAT' And
              (RRdis <> 0 Or RRacc <> 0 or RRagg <> 0);
                Write pdf00;
                Iter;
             Endif;
             DFuser = Q1user;
             DFpgm  = Q1pgm;
             DFdt   = RRdt;
             DFtime = RRtime;
             DFiusr = Q1user;
             DFipgm = Q1pgm;
             DFidt  = RRdt;
             DFitim = RRitim;
             Write pdf00;
          Enddo;
          // Copy Carrier Rating Default Detail records for Connect
          //  if factor/fee/discount is set up for the new record
          Setll (svRRco#:svRRfnd:svRRst:svRRefDt) pdt00;
          Dou %Eof(WIRATDDTL);
             Reade (svRRco#:svRRfnd:svRRst:svRRefDt) pdt00;
             If %Eof;
                Leave;
             Endif;
             // Don't check std limits default record
             If DTpgm <> 'WICARRAT' And
              DefaultNotSetUp(DTkey) = *On;
                Iter;
             Endif;
             DTefdt = RRefdt;
             DTst = RRst;
             Select;
             When DTpgm = 'WICARRAT' And
              (RRdis <> 0 Or RRacc <> 0 or RRagg <> 0);
               // Standard Limits record
                DTdesc = RRemlb;
                Write pdt00;
                Iter;
             When DTkey = 'INCLIMITS';
                If DefaultNotSetUp(DTkey:DTentry) = *On;
                   Iter;
                Endif;
             When DTkey = 'DEDUCT';
                If DefaultNotSetUp(DTkey:DTentry) = *On;
                   Iter;
                Endif;
             Endsl;
             DTuser = Q1user;
             DTpgm  = Q1pgm;
             DTdt   = RRdt;
             DTtime = RRtime;
             DTiusr = Q1user;
             DTipgm = Q1pgm;
             DTidt  = RRdt;
             DTitim = RRitim;
             Write pdt00;
          Enddo;
       Endif;
      /End-free
     C*
     C                   ENDSR                                                  WRITER
      //====================================================
      // Check if Factor is set up in Rating and Stat
      //====================================================
     pDefaultNotSetUp...
     p                 b
     dDefaultNotSetUp...
     d                 pi              n
     d inEntry                       10a
     d inCode                        10a   Options(*Nopass)
     d str10a          s             10a
     d str3a           s              3a
      /Free
       If %Parms > 1;
          str10a = inCode;
          str3a = inCode;
       Endif;
       Select;
       When inEntry = 'CONCREDIT';
          If RRcccd = *Zero;
             Return *On;
          EndIf;
       When inEntry = 'DEV1';
          If RRdv1c = *Zero;
             Return *On;
          Endif;
       When inEntry = 'DEV2';
          If  RRdv2c = *Zero;
             Return *On;
          Endif;
       When inEntry = 'REINSUR ';
          If RRarcc = *Zero;
             Return *On;
          Endif;
       When inEntry = 'SAFETYFEE';
          If RRsfpc = *Zero;
             Return *On;
          EndIf;
       When inEntry = 'SCHCREDIT';
          If RRcrcd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'SCHDEBIT';
          If RRdbcd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'SUBJECT1A';
          If RR1acd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'SUBJECT1B';
          If RR1bcd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'VOLUNTARY';
          If RRvlcc = *Zero;
             Return *On;
          Endif;
       When inEntry = 'WAIVER';
          If RRwvcd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'EXPMOD';
          If RReccd = *Zero;
             Return *On;
          Endif;
       When inEntry = 'MEMBERFEE';
          Setgt (RRco#:RRfnd:RRefdt) pf101;
          Readpe (RRco#:RRfnd) pf101;
          If %Eof Or F1mfee = 0;
             Return *On;
          Endif;
       When inEntry = 'INCLIMIT';
          Select;
          When str10a = *Blanks;
             Chain (RRco#:RRfnd:RRst) plm03;
             If Not(%Found);
                Return *On;
             EndIf;
          Other;
             Chain (RRco#:RRfnd:RRst:str10a) plm03;
             If Not(%Found);
                Return *On;
             Endif;
          EndSl;
       When inEntry = 'DEDUCT';
          Select;
          When str3a = *Blanks;
             Setll (RRco#:RRfnd:RRst) pdd03;
             Dou (%Eof);
                Reade (RRco#:RRfnd:RRst) pdd03;
                If %Eof Or DDstat = 'A';
                   Leave;
                EndIf;
             Enddo;
             Return %Eof;
          Other;
             Setll (RRco#:RRfnd:RRst:str3a) pdd03;
             Dou (%Eof);
                Reade (RRco#:RRfnd:RRst:str3a) pdd03;
                If %Eof Or DDstat = 'A';
                   Leave;
                EndIf;
             Enddo;
             Return %Eof;
          Endsl;
       When inEntry = 'PRMDISC';
       EndSl;
       Return *Off;
      /End-Free
     pDefaultNotSetUp...
     p                 e
