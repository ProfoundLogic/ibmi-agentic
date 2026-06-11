     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H dftName(F0340R)
     H/define CONVERT
     H*****************************************************************
     H*  F0340R - WCMC EOMB PRINT PROGRAM - V4 VERSION                *
     H*  Created: 09/30/98  C:56 Project: MC152                       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                                                               *
     H*  04/26/23 C:079 Project: WC2838                               *
     H*                 Pass Claim RRN to STLETTER not EOB RRN        *
     H*                                                               *
     H*  08/22/22 C:333 Project: MC443                                *
     H*                 Modernized EOB Print.                         *
     H*                                                               *
     H*  11/18/21 C:062 Ticket: 98681(146)                            *
     H*                 Populate descriptions.                        *
     H*                                                               *
     H*  07/20/20 C:079 Project: WC2749                               *
     H*                 Suppress Zero EOB Print in batch mode if      *
     H*                 EOB qualifies for E-Pay.                      *
     H*                                                               *
     H*  07/18/19 C:079 Support: 152/81115                            *
     H*                 Print EOB code 93 in the legend for FL        *
     H*                                                               *
     H*  06/12/19 C:079 Project: 152MAP                               *
     H*                 Display Review # for client 152               *
     H*                 Suppress insurer phone number for client 152  *
     H*                                                               *
     H*  08/28/17 C:079 PROJECT: SY395                                *
     H*                 Additional changes                            *
     H*                                                               *
     H*  07/19/17 C:079 PROJECT: SY395                                *
     H*                 Add adj state and recon flag to header file   *
     H*                                                               *
     H*  06/26/17 C:079 PROJECT: SY395                                *
     H*                 Add Scriptura xmlprep and S4401C logic.       *
     H*                                                               *
     H*  05/30/17 C:079 PROJECT: SY395                                *
     H*                 Add initial maintenance date to workfiles     *
     H*                                                               *
     H*  05/24/17 C:079 Project: SY371                                *
     H*                 Add fields/file(s) missing from workfile      *
     H*                                                               *
     H*  03/01/16 C:079 Project: 45MKI                                *
     H*                 Print review number for client 45             *
     H*                                                               *
     H*  01/11/16 C:307 SUPPORT 39207                                 *
     H*                 EXPLANATION OF BENEFITS CLAIM # AND INT FILE  *
     H*                 # ON REPRINTS OF EOBS                         *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:' and routing page logic     *
     H*                                                               *
     H*  11/02/15 C:309   SUPPORT: 37884 (EOR missing information)    *
     H*                                                               *
     H*  11/04/15 C:079 PROJECT: SY371                                *
     H*                 Add desision logic for Scriptura              *
     H*                                                               *
     H*  10/15/15 C:079 PROJECT: SY371                                *
     H*                 Add Scriptura Document workfile build         *
     H*                                                               *
     H*  05/27/15 C:062 PROJECT: 114MRH                               *
     H*                 Add eft processing.                           *
     H*                                                               *
     H*  08/13/14 C:298 PROJECT: SY354                                *
     H*                 Supressed the Routinng info while generating  *
     H*                 the single EOB from the claim.                *
     H*                                                               *
     H*  06/02/14 C:212 PROJECT: MC329_PH2                            *
     H*                 expand xxds1/2/3/4 to accomodate ICD10        *
     H*                                                               *
     H*                                                               *
     H*  06/05/14 C:296 Redmine 4243                                  *
     H*                 Correct line numbering for Eomp print.        *
     H*                                                               *
     H*  05/20/14 C:296 PROJECT: MC378                                *
     H*                 Print NDC code and description in place       *
     H*                 of the CPT/HCPCS for client 133.              *
     H*                                                               *
     H*  12/23/13 C:296 PROJECT: 129MSD                               *
     H*                 BACK TO BASE INITIATIVE-MANAGED CARE          *
     H*                 In EOB Report, print Public Good              *
     H*                 Pool Surcharge Amount                         *
     H*                                                               *
     H*  10/21/13 C:079 SUPPORT MC114 83870                           *
     H*                 Initialize lineed and numlin                  *
     H*                                                               *
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add ICD9 and ICD10 diagnosis desc population  *
     H*                                                               *
     H*  04/30/13 C:277 PROJECT 147MAS                                *
     H*                 Add Call of program FA783R if EOBCODE is 93   *
     H*                                                               *
     H*  06/11/12 C:277 PROJECT 114MER                                *
     H*                 Comment out all logic  for date 11/25/02      *
     H*                                                               *
     H*  05/30/12 C:277 SUPPORT 79844                                 *
     H*                 Change the printing of DETL2 before writing   *
     H*                 next page header.                             *
     H*                                                               *
     H*  05/16/12 C:280 SUPPORT 79900                                 *
     H*                 RECON DATE IS PRINTING OUT ON THE EOB         *
     H*                 ONLY PRINT WHEN RECON IS NOT ZERO             *
     H*                                                               *
     H*  05/07/12 C:251 PROJECT 133MMH Priss 11                       *
     H*                 Print LA Pharmacy Claimant info on same page  *
     H*                                                               *
     H*  04/02/12 C:280 SUPPORT 79500                                 *
     H*                 change for overflow indicator                 *
     H*                                                               *
     H*  02/06/12 C:251 PROJECT: 133MMK                               *
     H*                 Print LA Claimant info on EOB                 *
     H*                                                               *
     H*  12/13/11 C:191 PROJECT: LA025
     H*                 Print LA Parish sales tax on EOB              *
     H*                                                               *
     H*  07/14/11 C:257 SUPPORT MC139 77407                           *
     H*                 For recompile only                            *
     H*                                                               *
     H*  05/23/11 C:251 PROJECT: FS389                                *
     H*                 Add Division number, Mask SSN for all clients *
     H*                                                               *
     H*  05/05/11 C:251 PROJECT: NC032                                *
     H*                 Print Group Fax number for NC bills           *
     H*                                                               *
     H*  02/23/11 C:227 SUPPORT: 75402  MC133                         *
     H*                 Print DRG code and description                *
     H*                                                               *
     H*  02/10/11 C:220 SUPPORT: 73744                                *
     H*                 B2B for CSI. Added changes under tags -       *
     H*                 12/07/10, 11/30/10 and  01/04/10              *
     H*                                                               *
     H*  01/28/11 C:227 PROJECT: MC353                                *
     H*                 Print pharmacy# on the EOB if it is entered on*
     H*                 pharmacy bills.                               *
     H*                                                               *
     H*  09/29/10 C:257 PROJECT: 133MJN                               *
     H*                 ADD option 10 for W/W Provider sales tax      *
     H*                                                               *
     H*  12/07/10 C:079 PROJECT: 114MWS PRISS 32 / 73744              *
     H*                 Allow for negative sign to print              *
     H*                                                               *
     H*  11/30/10 C:079 PROJECT: 114MWS PRISS 32 / 73744              *
     H*                 Print DRG Code and Description                *
     H*                                                               *
     H*  03/11/10 C:227 Project: FL083                                *
     H*                 Added capbility to print all modifers & print *
     H*                 Billing provider NPI#                         *
     H*                                                               *
     H*  02/08/10 C:227 SUPPORT 70675                                 *
     H*                 Changed size of field                         *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  01/04/10 C:198 Project: 114MVF / 73744                       *
     H*                -Added a group parameter                       *
     H*                -Print EOB only for that group if group        *
     H*                 parameter has a value.                        *
     H*                                                               *
     H*  11/27/09 C:189 SUPPORT 70675                                 *
     H*                 Medical analyst name is not printing          *
     H*                 (Recompiled only)                             *
     H*                                                               *
     H*  06/01/09 C:33  Project: 114WTQ                               *
     H*                -Always use internal cliam # for CLT114        *
     H*                                                               *
     H*  05/27/09 C:33  Project: 114WTQ                               *
     H*                 Mask the SS# for CLT114.                      *
     H*                                                               *
     H*  03/11/09 C:189 SUPPORT 67496                                 *
     H*                 Reviewed by field - Compile only              *
     H*                                                               *
     H*  02/16/09 C:189 SUPPORT 66439                                 *
     H*                 Page numbering should restart for every       *
     H*                 separate Bill                                 *
     H*                                                               *
     H*  11/25/08 C:189 SUPPORT 65749                                 *
     H*                 Unknown chracters appearing on the EOBS report*
     H*                                                               *
     H*  10/24/07 C:62  PROJECT 139MIF                                *
     H*                 Add network verbiage.                         *
     H*                                                               *
     H*  09/25/07 C:47  PROJECT FL069                                 *
     H*                 Print state comments in bold if requested.    *
     H*                                                               *
     H*  09/06/07 C:47  PROJECT FL069                                 *
     H*                 Convert output specs and adjust rpg code      *
     H*                 to use an external printer file.              *
     H*                                                               *
     H*  08/07/07 C:189 PROJECT FL072                                 *
     H*                 Supply bills not pulling fee schedule rates   *
     H*                 from HCPCS/CPT file                           *
     H*                                                               *
     H*  06/15/07 C:188 PROJECT 10MKP                                 *
     H*                 Add state order processing.                   *
     H*                                                               *
     H*  06/07/07 C:188 PROJECT 114WMF                                *
     H*                 Back to base, client flagged 114MBY, 114MDC   *
     H*                                                               *
     H*  05/18/07 C:188 SUPPORT 58395                                 *
     H*                 Add functionality to print all comments for   *
     H*                 state/effective date. Was only printing       *
     H*                 first comment found.                          *
     H*                                                               *
     H*  04/05/07 C:062 PROJECT 139MHG                                *
     H*                 Print review number                           *
     H*                                                               *
     H*02/07/07 C:182PROJECT TX030                                  *
     H*               Made changes to set the font back to COURIER   *
     H*               after printing the BARCODE.                    *
     H*                                                               *
     H*12/18/06 C:182PROJECT TX030                                  *
     H*               Add printing of Bar Code at the Stub of        *
     H*               Bills first page.                              *
     H*                                                               *
     H*  08/28/06 C:079 SUPPORT 54416                                 *
     H*                 Make WMGCL user controlled                    *
     H*                                                               *
     H*  11/15/06 C:181 PROJECT 135MFX                               *
     H*                 Note: C:079 tweeked this code slightly       *
     H*                 Print Sub NTWK on EOB (PPC)                  *
     H*                                                               *
     H*  11/17/05 C:163 PROJECT MI002                                 *
     H*                 Michigan EOMB Req's                           *
     H*                                                               *
     H*  02/17/05 C:61 PROJECT MC266                                  *
     H*                Add printing of Procedure Code Modifier        *
     H*                FDDUP/DUPMOD for hospital bills.               *
     H*                                                               *
     H*  06/03/04 C:62 PROJECT FL046                                  *
     H*                USE EOB X-REF                                  *
     H*                                                               *
     H*  10/10/03 C:79 SUPPORT MC025 39479                            *
     H*                CORRECT SPELLING OF "CLAIMAINT"                *
     H*                                                               *
     H*  09/12/03 C:79 SUPPORT MC025 39032                            *
     H*           C:61 CHECK STATUS WHEN READING FDF4P, FDF3P         *
     H*                                                               *
     H*  07/24/03 C:79 SUPPORT MC051 38217                            *
     H*                PRINT EXPANDED LICENSE# FIELD IF NOT BLANKS    *
     H*                                                               *
     H*  07/23/03 C:62 PROJECT TX022                                  *
     H*                FOR TEXAS BILLS PRINT THE X-REF INSTEAD OF THE *
     H*                EOB CODE                                       *
     H*                                                               *
     H*  01/29/03 C:79 SUPPORT:MC055 35674                            *
     H*                ONLY PRINT ACTIVE EOB CODES                    *
     H*                                                               *
     H*  11/25/02 C:62 PROJECT 114MDC                                 *
     H*                IF CODEREVIEW, GETEOB DESC FROM MESSAGE.       *
     H*                                                               *
     H*  02/22/02 C:62 PROJECT:MC250                                  *
     H*                LOAD EPPHAR INTO LICENCE NUMBER IF AST = FL    *
     H*                AND BILL TYPE IS 'X'.                          *
     H*                                                               *
     H*  01/07/02 C:62 PROJECT:FS235                                  *
     H*                MAKE SURE ONLY TO PRINT EOB/NOTES IF PRINT     *
     H*                FLAG IS "Y" OR " ".                            *
     H*                                                               *
     H*  10/02/01 C:62 PROJECT:FS231                                  *
     H*                MAKE SURE ROUTING PAGE IS PRINTED FOR SUMMARY  *
     H*                REPORT WHEN NO RECORDS SELECTED.               *
     H*                                                               *
     H*  07/20/01 C:62 PROJECT:114WBY                                 *
     H*                - IF CLAIM# NOT BLANK USE IT, ELSE NORMAL      *
     H*                - PRINT VOUCHER# IF GROUP 440                  *
     H*                                                               *
     H*  11/22/00 C:62 PROJECT:FS212                                  *
     H*                EXPAND PATIENT ACCOUNT FIELD.                  *
     H*                                                               *
     H*  11/08/00 C:62 PROJECT:FS146                                  *
     H*                CHANGE 'BLTY' TO 'ZZBT' AND 'FLBT'.            *
     H*                                                               *
     H*  03/09/00  C:062 Support: MCMNT                               *
     H*                  EXPAND FYR IN CASE# TO 3.0.                  *
     H*                                                               *
     H*  02/08/00  C:079 Support: MC048/#21699                        *
     H*                  Move license number to XXLIC# based on Adj   *
     H*                  state                                        *
     H*                                                               *
     H*  02/07/00  C:062 PROJECT: Y2K                                 *
     H*                  CHANGE EAWC DATES TO YYMD.                   *
     H*                                                               *
     H* 02/04/00 C:62  SUPPORT MC044 #21354                           *
     H*                CHANGE EAWC DATE FIELDS.                       *
     H*                                                               *
     H* 12/15/99 C:56  SUPPORT MC044 #20524                           *
     H*                Get header EOB description for header EOB file *
     H*                instead of using FTABL.  This will avoid       *
     H*                overiding and opening and closeing FTABL in    *
     H*                batch mode due to eombs for different state.   *
     H*                                                               *
     H* 11/10/99 C:56  Support: MC044 #20192/20223                    *
     H*                Add clearing of work fields for batch printing *
     H*                                                               *
     H* 10/01/99 C:56  MC152                                          *
     H*                Print the dates fo service range at top of each*
     H*                page during line item printing.  This will     *
     H*                allow the user to see dates of service without *
     H*                flipping back to previous page.                *
     H*                                                               *
     H* 09/23/99 C:56  MC152                                          *
     H*                Correct page numbering for summary report.     *
     H*                                                               *
     H* 09/09/99 C:56  Project: MC208                                 *
     H*            1.  Add Date of Service range in header portion of *
     H*                bill.                                          *
     H*                                                               *
     H*            2.  Add 'Billed Code' on each line item.           *
     H*                                                               *
     H* 08/31/99 C:62  PROJECT: MC211                                 *
     H*                IMPLEMENTING NEW FIELD COMMENTS.               *
     H*                                                               *
     H* 08/28/99 C:56 PROJECT: MC152                                  *
     H*               1. CLEAR RECON RECEIVED DATE SO WILL NOT SPILL  *
     H*                  OVER DURING BATCH
     H*
     H* 07/26/99 C:56 MC208                                           *
     H*               1. Add network disclaimer                       *
     H*               2. Do not print medical examiner heading        *
     H*                  when 'medical examiner user value is '0'     *
     H*               3. Only EOB Reason codes for Header EOB codes.  *
     H*                                                               *
     H* 07/15/99 C:56 MC152                                           *
     H*               Add EOMB Print Summary                          *
     H*
     H* 05/05/99 C:56 MC152
     H*               Change FAEAL1 to FAEAP and FAEAL to FAEAL1
     H*               because FAEAL is being removed from system
     H*               because it is a duplicate of FAEAL1
     H*                                                               *
     H* 04/22/99 C:56 25MLR                                           *
     H*            1. Change 'Patient' to 'Claimaint'  (OEOMB)        *
     H*            2. Change 'Federal ID#' to 'Provider ID#'  (OEOMB) *
     H*            3. Add 'Date Recvd'                                *
     H*            4. Display decimal version of Diagnosis Code       *
     H*            5. Replace display of Diagnosis Code on each line  *
     H*               item with the corresponding number from Diag.   *
     H*               listing in heading                              *
     H*            6. Display 'To Service Date' for all bill types    *
     H*                (OEOMB)                                        *
     H*            7. Move printing of Hospital specific information  *
     H*               after amount totals.  Also add print of proc-   *
     H*               edure codes with the Hospital specific info.    *
     H*                                                               *
     H*            8. Reduce font for printing Group and State notes  *
     H*            9. Add Group name at top of page                   *
     H*           10. Reduce 'Patient Acct#' and 'Provider ID#'       *
     H*               headings and move all data at left top to the   *
     H*               the left.  Move SS# and DOI up one line         *
     H*               Remove Claims adjustor                          *
     H*           11. Move EOMB# under page #                         *
     H*                                                               *
     H*                                                               *
     H*       Additional changes per Lorri Dapper (MC152)             *
     H*         1.  Indent Description Line and EOB Codes line        *
     H*         2.  Put line items in Service Date order and do not   *
     H*             repeat print of Service Date.                     *
     H*                                                               *
     H* 04/07/99 C:56 MC152                                           *
     H*               Clear EOB array before loading                  *
     H*                                                               *
     H*  03/23/99  C:94  PROJECT: SUP17234                            *
     H*                  CHANGED WHERE ADDRESS IS BEING LOADED FROM   *
     H*                                                               *
     H*  02/16/99  C:56  PROJECT: 25MKH                               *
     H*                  Additional fields on EOMB for hospital bill  *
     H*                  Admission Date                               *
     H*                  Coverage                                     *
     H*                  Submission Date                              *
     H*                  Type of service                              *
     H*                  Type of Bill                                 *
     H*                                                               *
     H*  01/22/99  C:94  PROJECT: 25MKL                               *
     H*                  CHANGE PROGRAMS SO THAT IT WILL RUN IN BATCH *
     H*                                                               *
     H*****************************************************************
     F*
     F*  MAINLINE (INTERNAL) INDICATOR REFERENCE SUMMARY
     F*
     F*  48 - Used to print Title 'EOB Codes' for EOB codes
     F*  99 - General chain indicator
     H*****************************************************************
     FFAEOP     IF   E             DISK
     FFAEAP     IF   E             DISK
     F                                     RENAME(PEA01:PEA02)
     FFAEOLG    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FFMEML     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** Beg Add *** 04/26/23 ****************************************
     F                                     InFds(dbFds)
     F*** End Add *** 04/26/23 ****************************************
     FFAESL     IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     FFMCXL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFDCALR    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDPL     IF   E           K DISK
     FFDDDL     IF   E           K DISK
     FFDDEL     IF   E           K DISK
     FFAEEL     IF   E           K DISK
     FFDF3L     IF   E           K DISK
     FFDF4L     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     F*
     FFMPRL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FNMNWL     IF   E           K DISK
     F**** Beg Chg *** 07/18/23 **************************************
     F**** FAEEL1    IF   E           K DISK
     FFAEEL5    IF   E           K DISK
     F**** End Chg *** 07/18/23 **************************************
     F                                     RENAME(PEE01:PEE02)
     FEOBS      O    E             PRINTER OFLIND(*IN01)
     F                                     INFDS(LINE)
     FEOBSSUM   O    E             PRINTER INFDS(PR1DS)
     F                                     OFLIND(*IN02)
     F*
     FSMCOL     IF   E           K DISK
     F*
     F** Group Carrier Information File
     FWMGCL     IF   E           K DISK    USROPN
     F*
     F** Claimant demographics
     FFMCDL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     F* Group information file
     FFDF2L     IF   E           K DISK
     F* FDCAL5 for EOB/DSEQ sorting
     FFDCAL5    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFDDAL2    IF   E           K DISK
     Ffmicd10l1 if   e           k disk
     Ffaedl     if   e           k disk
     F*
     F** Beginning of Scriptura Workfiles
     FSXCNEOBP  uf a e             disk
     FSXDECEOBP uf a e             disk
     FSXDNTEOBP uf a e             disk
     FSXDCNTEOBPuf a e             disk
     FSXDTEOBP  uf a e             disk
     FSXFTEOBP  uf a e             disk
     FSXGDEOBP  uf a e             disk
     FSXGNEOBP  uf a e             disk
     FSXGTEOBP  uf a e             disk
     FSXHDEOBP  uf a e             disk
     FSXRTEOBP  uf a e             disk
     FSXSMEOBP  uf a e             disk
     FSXSNEOBP  uf a e             disk
     FSXTOTEOBP uf a e             disk
     FXMLPREL1  UF   E           K DISK
     FSXRHEOBP  uf a e             disk
     FSXDGEOBP  uf a e             disk
      /INCLUDE QSYSINC/QRPGLESRC,QUSEC
     D EOBdSeq         S              5s 0 DIM(20) ASCEND                       EOB codes array
     D EOBseq#         S              5s 0 DIM(20) ASCEND                       EOB codes array
     D sys_date        s               D   datfmt(*USA)
     D xxIdt           s                   like(ESiDt)
     D xxSS#           s             11A
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFlowName     s                   like(XPXML)
     d xmlFileSum      s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     D isUseScript     S              1n   inz
     D xxPre           s              2    inz('PB')
     d processType     s            100A
     d x1Rrn1          s              9A
     D message         s            250    inz(*blanks)
     D xxUserData      s             10a
     D x1UserData      s             10a
     D xxOutType       s              1a
     D xxDxrA          s              1a
     D xxDxrB          s              1a
     D xxDxrC          s              1a
     D xxDxrD          s              1a
     D xxDxrE          s              1a
     D xxDxrF          s              1a
     D xxDxrG          s              1a
     D xxDxrH          s              1a
     D xxDxrI          s              1a
     D xxDxrJ          s              1a
     D xxDxrK          s              1a
     D xxDxrL          s              1a
     D start           s              2  0
     D position        s              2  0
     d xmlFlowNmEob    s                   like(XPXML)
     d xmlFlowNmSum    s                   like(XPXML)
     d xxICDind        s                   like(EHicdInd)
     d DescE           s             30A
     d xxDSe           s              8A
     d DescF           s             30A
     d xxDSf           s              8A
     d DescG           s             30A
     d xxDSg           s              8A
     d DescH           s             30A
     d xxDSh           s              8A
     d DescI           s             30A
     d xxDSi           s              8A
     d DescJ           s             30A
     d xxDSj           s              8A
     d DescK           s             30A
     d xxDSk           s              8A
     d DescL           s             30A
     d xxDSl           s              8A
     d**** Beg Chg *** 07/18/23 **************************************
     D**** EOB             S              3    DIM(20) ASCEND                       EOB codes array
     D EOB             S              6    DIM(20) ASCEND                       EOB codes array
     d**** End Chg *** 07/18/23 **************************************
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D NWMSG           S             65    DIM(2) CTDATA PERRCD(1)              NETWORK MESSAGE
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     DFLAG01                          1
     D LINE            DS
     D  CURLIN               367    368B 0
     D  CURPAG               369    372B 0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D  DSYEAR                11     14  0
     D                 DS
     D  XXLIN1                 1    105
     D  XXPRTA                 1     70
     D  XXPRTB                71    105
     D                 DS
     D  XXLIN2                 1    105
     D  XXPRTC                 1     35
     D  XXPRTD                36    105
     D                 DS
     D  EOSS#                  1      9
     D  XXSS#1                 1      3
     D  XXSS#2                 4      5
     D  XXSS#3                 6      9
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D*
     D* DATA STRUCTURES FOR STATE COMMENTS AND GROUP COMMENTS
     D*
     D                 DS
     D  STCOMM                 1    140
     D  STCOM1                 1     70
     D  STCOM2                71    140
     D*
     D                 DS
     D  GRCOMM                 1    140
     D  GRCOM1                 1     70
     D  GRCOM2                71    140
     D                 DS
     D  XZCASE                 1     21
     D  XXSUB#                 1      5
     D  XDASH1                 6      6
     D  XXFND                  7      9
     D  XDASH2                10     10
     D  XXFYR                 11     13
     D  XDASH3                14     14
     D  XXCASE                15     21
     D PR1DS           DS
     D  FILL3                  1    200
     D  FILL4                201    366
     D  X1LINE               367    368B 0
     D  X1PAGE               369    372B 0
     D                 DS
     D  XXNWM                  1    180
     D  XXNWM1                 1     39
     D  XXNWM2                40    180
     D                 DS
     D  TNOTE                  1    210
     D  WVAR1                  1     70
     D  WVAR2                 71    140
     D  WVAR3                141    210
     D*** Beg Add *** 04/26/23 ***********************************
     D dbfds           ds
     D dbrrn2                397    400b 0
     D*** End Add *** 04/26/23 ***********************************
     D kkicd10         s                   like(iccode)
     D isEft           s               n   Inz(*Off)
     D #1rtcd          s              1
     D #1co#           s              3
     D #1sub#          s              5
     D #1fnd           s              3
     D #1outs          s              1
     D #1type          s              1
     D #1key           s             30
     D #1date          s              7
     D #1eft           s              1
     D #1pre           s              1
     D #1rrn           s              9
     D #1eobType       s              3

     D ##qrls          s              1                                         16
     D ##ccod          s              1                                         17
     D ##cchk          s              1                                         18
     D ##prom          s              1                                         19
     D ##mcrv          s              1                                         20
     D ##freq          s              1                                         21
     D ##crac          s              1                                         22
     D ##preo          s              1                                         23
     D ##parn          s              1                                         24
     D ##mgri          s              1                                         25
     D ##kcif          s              1                                         26
     D ##curs          s              1                                         27
     D ##pcib          s              1                                         28
     D ##clt#          s              3                                         29
     D ##ucpf          s              1                                         30
     D ##dcf           s              1                                         31
     D ##autb          s              1                                         32
     D ##deif          s              1                                         33
     D ##aure          s              1                                         34
     D ##cred          s              1                                         35
     D ##idrt          s              1                                         36
     D ##cprv          s              1                                         37
     D ##eft           s              1                                         38
     D ##govr          s              1                                         39
     D ##ncdc          s              1                                         40
     D ##splt          s              1                                         41
     D #1last          s              1                                         42
     D ##pblt          s              1                                         43
     D ##inze          s              1                                         44
     D isScriptEOB     s              1A   inz('N')
     D outCall         s             11
     D InReturn        s            250
     D OutMode         s              1    inz(' ')
     D OutRrn          s              9
     D OutPrefix       s              2
     D OutString       s             10
     D OutCo#          s              3
     D OutFnd          s              3
     d*** Beg Add *** 08/22/22 ************************************************
     d X               s              2s 0
     d X2              s              2s 0
     D ModernEOB       s              1A
     d*** End Add *** 08/22/22 ************************************************
     d*** Beg Add *** 07/18/23 ***********************************************
     d $$CO#           s              3  0
     d $$DESC          s             70
     d $$EOB           s             10
     d $$XREF          s              3
     d*** End Add *** 07/18/23 ***********************************************
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     D   outCall_                    10    const
     D   InXML_                      60
     D   InReturn_                  250
     D   OutMode_                     1    const
     D   OutRrn_                      9    const
     D   OutPrefix_                   2    const
     D   OutString_                  10    const
     D   OutCo#_                      3    const
     D   OutFnd_                      3    const
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     D*
     d GenScriptOut    pr                  ExtPgm('S4401C')
     d inXml_                        61
     d inXml           s             61
     d
     dObjExists        pr             1n
     d object                        10    const
     d objType                        7    const
     d
     d GenEOBnote      pr           280
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D/copy sPrModule
     D/copy sPrXML
     I*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN1            9            faeop
     C                   PARM                    @@MODE            1
     C                   PARM                    @@OUTQ           10
     C                   PARM                    @@HOLD            4
     C                   PARM                    @@GRP             3
     C                   PARM                    @@USERDATA       10
     C                   PARM                    @@OUTTYPE         1
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
     C     DTYYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C* KEY LIST FOR CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* KEY LIST FOR PROVIDER DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C* KEY LIST FOR WMCM EOMB STATUS TRANSACTION FILE
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*
     C     ESKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC EOMB HEADER AMONT TRANACTION FILE
     C*
     C     EAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C* KEY LIST FOR WCMC EOMB PHYS/HCFA HEADER FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC EOMB HOSP/UB HEADER FILE
     C*
     C     EUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC EOMB PHARMACY HEADER FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC CONTROL FILE LOGICAL
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* KEY LIST FOR DIAGNOSIS CODE FILE
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C* KEY LIST FOR DETAIL CHARGE FILE
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKFDAT
     C                   KFLD                    KKTDAT
     C     DAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKRCN#
     C*
     C* KEY LIST FOR WCMC EOMB PHYS/HCFA BILL DETAIL TRANSACTION FILE
     C*
     C     DHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* KEY LIST FOR WCMC EOMB HOSP/UB BILL DETAIL TRANSACTION FILE
     C*
     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* KEY LIST FOR WCMC EOMB PHARMACY BILL DETAIL TRANSACTION FILE
     C*
     C     DPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* KEY LIST FOR WCMC EOMB DENTAL BILL DETAIL TRANSACTION FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* KEY LIST FOR WCMC EOMB DETAIL EOB/NOTES TRANSACTION FILE
     C*
     C     DEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* KEY LIST FOR WCMC GROUP EOMB COMMENTS FILE
     C*
     C     F3KY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     F3KY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEFDT
     C*
     C* KEY LIST FOR EOMB HEADER EOB/NOTES TRANSACTION FILE
     C*
     C     EEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C     EEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBC
     C*
     C**KEY*LIST FOR EOB DISCRIPTION FILE
     C*
     C* KEY LIST FOR WCMC STATE COMMENTS FILE
     C*
     C     F4KY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C*
     C     F4KY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C*
     C* KEY LIST FOR FUND AUXILIARY FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* KEY LIST FOR COMPANY AUXILIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    KKCO#
     C*
     C* KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKUSER
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKFIL#
     C*
     C* KEY LIST FOR WCMC EOMB FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key list for Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KXKEY
     C*
     C     GTKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C* KEY LIST FOR GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*  KEY LIST FOR SUBSCRIBER MASTER
     C*
     C     SMKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C*
     C     NWKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKNWCD
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  GROUP CARRIER INFORMATION FILE
     C*
     C     GCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*** Key list for claimant demographics file.
     C*
     C     CDKY          KLIST
     C                   KFLD                    EASS#
     C*
     C     F2KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KF2ST
     C*
     C     DEKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** KEY LIST FOR DIAGNOSIS CODE 10 MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kklost
     C*
     C     faedlkey      klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C     *LIKE         DEFINE    ESIDT         KKIDT
     C     *LIKE         DEFINE    ESITIM        KKITIM
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    DHDSEQ        KKDSEQ
     C     *LIKE         DEFINE    F4ST          KKST
     C     *LIKE         DEFINE    F1ACTY        XXCTY
     C     *LIKE         DEFINE    F1AST         XXSTAT
     C     *LIKE         DEFINE    F4EFDT        KKEFDT
     C     *LIKE         DEFINE    ORFIL#        KKFIL#
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KXKEY
     C     *LIKE         DEFINE    EOCO#         ##CO#
     C     *LIKE         DEFINE    EOSUB#        ##SUB#
     C     *LIKE         DEFINE    EOFND         ##FND
     C     *LIKE         DEFINE    EOFYR         ##FYR
     C     *LIKE         DEFINE    EOCASE        ##CASE
     C     *LIKE         DEFINE    EOEOB#        ##EOB#
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CAFDAT        KKTDAT
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CATDAT        HLTDAT
     C     *LIKE         DEFINE    EASUB#        HLSUB#
     C     *LIKE         DEFINE    EAFND         HLFND
     C     *LIKE         DEFINE    EAPOST        HLPOST
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    EEEOB         KKEOBC
     C     *LIKE         DEFINE    EEEOBCD       KKEOBC
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    STKEY         XXKEY
     C                   MOVEL     '@@@@@'       XXSTNM
     C     *LIKE         DEFINE    F1AARA        SVFARA
     C     *LIKE         DEFINE    F2ST          KF2ST
     C     *LIKE         DEFINE    DEDSEQ        TMDSEQ
     C     *LIKE         DEFINE    DENOT1        WRNOT1
     C     *LIKE         DEFINE    DENOT2        WRNOT2
     C     *LIKE         DEFINE    DENOT3        WRNOT3
     C     *LIKE         DEFINE    DENOT4        WRNOT4
     C     *LIKE         DEFINE    DARCN#        KKRCN#
     C*
     C* INITIALIZATION OF VARIABLES
     C*
     C                   Z-ADD     0             LIDIFF            3 0
     C                   MOVE      @@RRN1        XXRRN1            9 0
     C                   Z-ADD     *ZERO         CURLIN
     C                   MOVE      *BLANK        X1HEX            11
     C                   MOVE      *BLANK        X1HEXB           11
     C                   MOVE      *BLANK        X1CSZ2
     C                   MOVE      *BLANK        X1AREA
     C                   MOVE      *BLANK        X1PHN1
     C                   MOVE      *BLANK        X1PHN2
     C*
     C                   MOVE      *ZERO         *IN52
     C                   MOVE      *ZERO         *IN53
     C                   TIME                    DSDTTM
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C                   PARM                    ##OS              1            OUTOME/SAVINGS
16   C                   Parm                    ##qrls                         Auto Release Flag
17   C                   Parm                    ##ccod                         Correct Coding Flag
18   C                   Parm                    ##cchk                         Claim Check Flag
19   C                   Parm                    ##prom                         ProMatch Flag
20   C                   Parm                    ##mcrv                         Medicare CR Flag
21   C                   Parm                    ##freq                         Frequency Flag
22   C                   Parm                    ##crac                         CR Auto Complete
23   C                   Parm                    ##preo                         CR Auto Complete
24   C                   Parm                    ##parn                         Par/Non-Par/Both
25   C                   Parm                    ##mgri                         Multi Group Restr
26   C                   Parm                    ##kcif                         Keep Claimant info
27   C                   Parm                    ##curs                         PBILLS entry cursor
28   C                   Parm                    ##pcib                         Prov Check in Batch
29   C                   Parm                    ##clt#                         Client Number
30   C                   Parm                    ##ucpf                         UCR Percentile
31   C                   Parm                    ##dcf                          Data Center
32   C                   Parm                    ##autb                         Auto Batch   Flag
33   C                   Parm                    ##deif                         DE/Image Flag
34   C                   Parm                    ##aure                         Auto-Reinstate
35   C                   Parm                    ##cred                         Credit Module
36   C                   Parm                    ##idrt                         Idarts Module
37   C                   Parm                    ##cprv                         Create Provider
38   C                   Parm                    ##eft                          EFT Flag
39   C                   Parm                    ##govr                         GO VERSION
40   C                   Parm                    ##ncdc                         NCCI data
41   C                   Parm                    ##splt                         SPLIT
42   C                   Parm                    ##last                         LA PARISH SALES TAX
43   C                   Parm                    ##pblt                         Provider bill light
44   C                   Parm                    ##inze                         Include $0 EOB
     C*
     C*
     C* Check data client data area
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C     ##CLT#        IFEQ      '114'
     C                   SETON                                        76
     C                   ELSE
     C                   SETOFF                                       76
     C                   ENDIF
      /Free
                         If ##clt# = '152';
                            *in77 = *on;
                         Else;
                            *in77 = *off;
                         Endif;
      /End-free
     C*
      //*** Beg Add *** 08/22/22 ***************************
        ModernEOB = ' ';
        *in88 = *off;
        If isModEnabled('Modernized EOB Print');
           ModernEOB = 'Y';
           *in88 = *on;
        Endif;
      //*** End Add *** 08/22/22 ***************************
     C****** PERFORM CVTHEX TO GET ATRN VALUE TO PASS TO THE PRINTER **
     C*
     C                   MOVEL     '03011B'      CHR             256
     C                   MOVE      *BLANKS       HEX             128
     C                   MOVE      *BLANK        RTCODE            1
     C                   CALL      'CVTHEX'
     C                   PARM                    CHR
     C                   PARM                    HEX
     C                   PARM                    RTCODE
     C                   MOVEL     HEX           ESC               3
    C*** HAD TO ADD THESE FIELDS TO SATISFY PRTF REQUIREMENTS.        **
     C                   MOVEL     HEX           ESC1              3
     C                   MOVEL     HEX           ESC2              3
     C                   MOVEL     HEX           ESC3              3
     C                   MOVEL     HEX           ESC4              3
     C                   MOVEL     HEX           ESC5              3
    C*
    C*
     C     ##NTWK        IFEQ      'Y'
     C                   SETON                                        66
     C                   ELSE
     C                   SETOFF                                       66
     C                   ENDIF                                                  ##NTWK = Y
     C*
     C* Check data client data area
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C                   SETOFF                                       79
     C                   SELECT
     C*
     C* Print Review Number
     C*
     C     ##CLT#        WHENEQ    '139'
     C     ##CLT#        OREQ      '045'
     C     ##CLT#        OREQ      '152'
     C                   SETON                                        79        GIC
     C     ##CLT#        WHENEQ    '010'
     C                   SETON                                        18        Summit
     C                   ENDSL
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C                   OPEN      WMGCL
     C                   ENDIF
     C* Determine if this module get spool file and/or XML output
      /Free
                         isUseSpoolOut = UseSpoolOut('EOB Print');
                         isUseXML = UseScriptura('EOB Print')
                                    or Use3rdXml('EOB Print');
                         if isUseXML;
                           isScriptEOB = 'Y';
                           if Q1prms > 5;
                             xxUserData = @@UserData;
                           endif;
                           if Q1prms > 6;
                             xxOutType = @@OutType;
                           endif;
                         else;
                           isScriptEOB = 'N';
                         Endif;
      /End-free
     C* Get the system date and store in initial date field
     C*
     C                   eval      sys_date = %date
     C     *cymd         Move      sys_date      xxIdt
     C*
     C* GET FAEOP RECORD
     C*
     C* CALLED TO WRITE ONE EOMB
     C*
     C     XXRRN1        IFNE      *ZEROS
     C*
     C     XXRRN1        CHAIN     PEO01                              99
     C* Get the Scriptura XML file name for the detail report
     C                   If        isScriptEOB = 'Y'
      /free

                         isMassReq = *off;
                         massFlag = ' ';
    //*** Beg Chg *** 04/26/23 **************************************
    //***************    x1Rrn1 = @@rrn1;
                         Chain(n) (EOco#:EOsub#:EOfnd:EOfyr:EOcase) pcm01;
                         If %Found(FMCML1);
                            x1Rrn1 = %EditC(DbRrn2:'Z');
                         Endif;
    //*** End Chg *** 04/26/23 **************************************

                         Clear xmlLink;
                         Clear xmlFlowName;

                         processType = 'EOB Print';
                         xxUserData = processType;

                         Exsr      ScriptXml;
                         xmlFlowNmEOB = xmlFlowName;
      /end-free
     C                   Endif
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EOCO#         KKCO#
     C     EOAST         CAT(P)    'BT':0        BTTBID            4
     C                   MOVEL(P)  BTTBID        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        BTTBID
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   ENDIF
     C                   ENDIF
    C*
    C* INITIALIZE THE BAR CODE PRINTING FLAG AT STUB OF CHECK.
    C*
     C                   MOVE      '0'           BARCDF            1            BAR CD PRNT FLG
     C*
     C* Check to see if this is an eft
     C*
     C                   ExSr      SrProvEFT
     C*
    C*
     C                   If        isScriptEOB <> 'Y'
     C*
      *** Beg Chg *** 08/22/22 *****************************
     C***  EOAST         IFEQ      'FL'
     C                   If        ModernEOB = 'Y' or EOast = 'FL'
      *** End Chg *** 08/22/22 *****************************
     C                   EXSR      SRPRTL
     C                   ELSE
     C                   EXSR      SRPRT
     C                   ENDIF
     C*
     C                   Endif
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      SRprtL
     C                   Endif
    C*
     C     BARCDF        IFEQ      '0'
     C     EOAST         ANDEQ     'TX'
     C                   If        isUseSpoolout
     C                   WRITE     BARCD1
     C                   Endif
     C                   MOVE      '1'           BARCDF
     C                   ENDIF
    C*
    C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   ZZRRN1 NE *ZEROS
     C*
     C* CALLED FROM BATCH TO WRITE MULTIPLE EOMBS
     C*
     C* Get the Scriptura XML file name for the Summary Report.
     C                   If        isScriptEOB = 'Y'
     C                   eval      xxUserData = 'EOB_Sum'
     C*
     C                   select
     C*
     C                   when      xxOutType = '1'
     C                   eval      xxUserData = 'EOBSumStan'
     C                   eval      x1UserData = 'EOBStandAl'
     C*
     C                   when      xxOutType = '2'
     C                   eval      xxUserData = 'EOBSumGT0'
     C                   eval      x1UserData = 'EOBGTZero'
     C*
     C                   when      xxOutType = '3'
     C                   eval      xxUserData = 'EOBSumZero'
     C                   eval      x1UserData = 'EOBZero'
     C*
     C                   endsl
     C*
     C                   Clear                   xmlLink
     C                   eval      processType = 'EOB Summary'
     C                   Exsr      ScriptXml
     C                   eval      xmlFlowNmSum = xmlFlowName
     C*
     C                   eval      isMassReq = *on
     C                   eval      massFlag = 'Y'
     C*
     C                   Endif
     C                   CLEAR                   XGEOB#
     C                   CLEAR                   XGCHRG
     C                   CLEAR                   XGMRA
     C                   CLEAR                   XGOTHR
     C                   CLEAR                   XGOTH2
     C                   CLEAR                   XGNDIS
     C                   CLEAR                   XGPAID
     C                   CLEAR                   XGPAI2
     C                   CLEAR                   XSEOB#
     C                   CLEAR                   XSCHRG
     C                   CLEAR                   XSMRA
     C                   CLEAR                   XSOTHR
     C                   CLEAR                   XSOTH2
     C                   CLEAR                   XSNDIS
     C                   CLEAR                   XSPAID
     C                   CLEAR                   XSPAI2
     C                   CLEAR                   XTEOB#
     C                   CLEAR                   XTCHRG
     C                   CLEAR                   XTMRA
     C                   CLEAR                   XTOTHR
     C                   CLEAR                   XTOTH2
     C                   CLEAR                   XTNDIS
     C                   CLEAR                   XTPAID
     C     Q1PRMS        IFGT      4
     C     @@GRP         ANDNE     ' '
     C                   MOVE      @@GRP         PRFND             3 0
     C                   ENDIF
     C                   READ      PEA02                                  99
     C                   SETOFF                                       16
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVE      '1'           *IN86                          NO RECORDS
     C                   SETON                                        16
     C                   MOVE      '1'           KKFIL#
     C     ORKY          CHAIN     POR01                              98
     C                   MOVEL(P)  ORTITL        X2TITL           60
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C                   If        isUseSpoolout
     C                   Eval      Flag01 = '1'
     C                   WRITE     RPHD1
     C                   WRITE     ERRMSG
     C                   Endif
     C                   GOTO      TERM
     C                   ENDIF                                                  *IN99 = *OFF
     C  N99              MOVE      *BLANK        ROUTPT            1
     C     *IN99         DOWEQ     *OFF
      /Free
       // Skip zero EOB if E-Pay
       If EApLnk = '2' and EAeftC = 'V';
          Read pea02;
          If %Eof(faeap);
             *in99 = *on;
          Else;
             *In99 = *off;
          Endif;
          Iter;
       Endif;
      /End-Free
     C* Get the Scriptura XML file name for the detail report
     C                   If        isScriptEOB = 'Y'
      /free


                         processType = 'EOB Print';
                         xxUserData  = x1UserData;

                         Exsr      ScriptXml;
                         eval      xmlFlowNmEob = xmlFlowName;
      /end-free
     C                   Endif
     C*
     C* Check to see if this is an eft
     C*
     C                   ExSr      SrProvEFT
     C*
     C* If not in EFT mode and is an eft item then skip
     C* If in EFT mode and is not an eft item then skip
     C*
     C                   If        (@@Mode <> 'E' and isEft = *Off) Or
     C                             (@@Mode =  'E' and isEft = *On)
     C*
     C* CLEAR WORK FIELDS FOR PRINTING
     C*
     C*    EOMB HEADER
     C                   CLEAR                   XXBLTY
     C                   CLEAR                   X1NAME
     C                   CLEAR                   XXNAME
     C                   CLEAR                   XXSS#1
     C                   CLEAR                   XXSS#2
     C                   CLEAR                   XXSS#3
     C                   CLEAR                   XXACDT
     C                   CLEAR                   XXMUSR
     C                   CLEAR                   XXACDT
     C                   CLEAR                   XXFRDT
     C                   CLEAR                   XXTODT
     C                   CLEAR                   XXTRDT
     C                   CLEAR                   XXLIC#
     C                   CLEAR                   XXCRDT
     C                   CLEAR                   XXRCDT
     C                   CLEAR                   XXWCPD
     C                   CLEAR                   XXCSZ1
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXDRG#
     C                   CLEAR                   XXDGDS
     C                   CLEAR                   XXADDT
     C                   CLEAR                   XXSCPF
     C                   CLEAR                   XXSCPT
     C                   CLEAR                   XXSBDT
     C                   CLEAR                   XXICN
     C                   CLEAR                   XXICDS
     C                   CLEAR                   XXBDSC
     C                   CLEAR                   XXPRD1
     C                   CLEAR                   XXPRD2
     C                   CLEAR                   XXPRD3
     C                   CLEAR                   XXPRD4
     C                   CLEAR                   XXNWM
     C                   CLEAR                   YYEOB
     C                   CLEAR                   YYDESC
     C                   CLEAR                   X1NOTE
     C                   CLEAR                   XXANME
     C                   CLEAR                   XXAAD1
     C                   CLEAR                   XXAAD2
     C                   CLEAR                   XXCSZ2
     C                   CLEAR                   XXAREA
     C                   CLEAR                   XXPHN1
     C                   CLEAR                   XXPHN2
     C                   CLEAR                   XXMRA
     C                   CLEAR                   XXOTH2
     C                   CLEAR                   XXPAID
     C                   CLEAR                   XXPAI2
     C                   CLEAR                   X1CSZ2
     C                   CLEAR                   X1AREA
     C                   CLEAR                   X1PHN1
     C                   CLEAR                   X1PHN2
     C                   CLEAR                   X1PAID
     C                   CLEAR                   X2PAID
     C                   CLEAR                   XXENAM
     C                   CLEAR                   XXENAM1
     C                   CLEAR                   XXFARA
     C                   CLEAR                   XXFPH1
     C                   CLEAR                   XXFPH2
     C                   CLEAR                   SVFARA
     C                   CLEAR                   SVFPH1
     C                   CLEAR                   SVFPH2
      /free
            Clear LiNeed;
            Clear NumLin;
      /End-Free
     C*
     C*   SUMMARY HEADER FIELDS
     C*
     C                   CLEAR                   XXSBNM
     C                   CLEAR                   XXHMRA
     C                   CLEAR                   XXHOT1
     C                   CLEAR                   XXHOT2
     C                   CLEAR                   XXHNET
     C                   CLEAR                   XXCO#
     C                   CLEAR                   XZCASE
     C                   CLEAR                   XXEOB#
     C                   CLEAR                   XXSS#
     C                   CLEAR                   XXACDT
     C                   CLEAR                   XZPNAM
     C                   CLEAR                   XXCHRG
     C                   CLEAR                   XXOTHR
     C                   CLEAR                   XXOTH2
     C                   CLEAR                   XXNDIS
     C*
     C                   Z-ADD     EACO#         KKCO#
     C                   Z-ADD     EASUB#        KKSUB#
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   If        ##CLT# = '129'
     C                   SetOff                                           11
     C     EAOCA1        IfGT      *ZEROS
     C                   SETON                                            11
     C                   EndIf
     C                   EndIf
     C*
     C     EOKY          CHAIN     PEO02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EOCO#         KKCO#
     C     EOAST         CAT(P)    'BT':0        BTTBID            4
     C                   MOVEL(P)  BTTBID        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        BTTBID
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   ENDIF
     C                   ENDIF
     C                   SETON                                        74        RESET PAGE NUMBER
     C     Q1PRMS        IFGT      4
     C     @@GRP         ANDNE     ' '
     C     PRFND         IFEQ      KKFND
     C                   If        isScriptEOB <> 'Y'
      *** Beg Chg *** 08/22/22 *****************************
     C***  EOAST         IFEQ      'FL'
     C                   If        ModernEOB = 'Y' or EOast = 'FL'
      *** End Chg *** 08/22/22 *****************************
     C                   EXSR      SRPRTL
     C                   ELSE
     C                   EXSR      SRPRT
     C                   ENDIF
     C                   Endif
     C*
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      SRprtL
     C                   Endif
     C                   EXSR      SRPSUM
     C                   MOVE      'Y'           XXSUMM            1
     C                   ENDIF                                                  PRFND = KKFND
     C                   ELSE
     C                   If        isScriptEOB <> 'Y'
     C*** Beg Chg *** 08/22/22 *****************************
     C***  EOAST         IFEQ      'FL'
     C                   IF        ModernEOB = 'Y' or EOast = 'FL'
     C*** End Chg *** 08/22/22 *****************************
     C                   EXSR      SRPRTL
     C                   ELSE
     C                   EXSR      SRPRT
     C                   ENDIF
     C                   Endif
     C*
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      SRprtL
     C                   Endif
     C                   EXSR      SRPSUM
     C                   ENDIF                                                  Q1PRMS > 4
     C                   ENDIF                                                  *IN99 = *OFF
     C                   EndIf                                                  @@Mode = 'E'
     C*
     C                   READ      PEA02                                  99
     C* Get the Scriptura XML file name for the next EOB.
     C                   If        isScriptEOB = 'Y'
     C                   Endif
     C     EAFND         IFNE      PRFND
     C                   ENDIF
     C                   ENDDO                                                  *IN99 = *OFF
     C     Q1PRMS        IFGT      4
     C     @@GRP         ANDNE     ' '
     C     XXSUMM        IFEQ      'Y'
     C                   MOVE      'N'           XXSUMM
     C                   EXSR      OVFLOW
     C* Get the Scriptura XML file name for the Summary Report.
     C                   If        isScriptEOB = 'Y'
     C                   Endif
     C                   If        isUseSpoolout
     C                   WRITE     RPUTOT
     C                   Endif
     C*
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpTotWrk
     C                   Endif
     C                   SETON                                        OB
     C                   If        isUseSpoolout
     C                   WRITE     RPHD1
     C                   WRITE     RPFTOT
     C                   Endif
     C*
     C*** Write to Scriptura EOB Batch Summary Totals Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEobSmWrk
     C                   Endif
     C                   ENDIF                                                  XXSUMM = 'Y'
     C                   ELSE
     C                   EXSR      OVFLOW
     C                   If        isUseSpoolout
     C                   WRITE     RPUTOT
     C                   Endif
     C*
     C*** Write to Scriptura EOB Group Totals Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpTotWrk
     C                   Endif
     C                   SETON                                        OB
     C                   If        isUseSpoolout
     C                   WRITE     RPHD1
     C                   WRITE     RPFTOT
     C                   Endif
     C*
     C*** Write to Scriptura EOB Batch Summary Totals Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEobSmWrk
     C                   Endif
     C                   ENDIF                                                  Q1PRMS > 4
     C*
     C                   CLEAR                   XTEOB#
     C                   CLEAR                   XTCHRG
     C                   CLEAR                   XTMRA
     C                   CLEAR                   XTOTHR
     C                   CLEAR                   XTOTH2
     C                   CLEAR                   XTNDIS
     C                   CLEAR                   XTPAID
     C*
     C                   ENDIF                                                  XXRRN NE *ZEROS
     C*
     C* END PROGRAM *
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C                   CLOSE     WMGCL
     C                   ENDIF
     C*
     C     TERM          TAG
      /Free
       if isUseSpoolOut;
       KkFil# = '1';
       Chain ORKY POR01;
       If %found;
          If Flag01 = '1';
          Write Route1;
          Endif;
          If Flag01 = '2';
             If rtptb <> 'Y';
             Write route1;
             EndIf;
          EndIf;
       EndIf;
       KkFil# = '0';
       Chain ORKY POR01;
       If %found;
          Write Route;
       EndIf;
       EndIf;
      /End-free
       // Call process to generate Scriptura output/Darts if requested for
       // the EOB Summary Report
      /Free
          If        isUseXml;
             Select;
             When xmlFlowNmEob = *blanks;
             When objExists('PDSDEV':'*LIB');
                inXml = xmlFlowNmEob + '$';
                Monitor;
                Callp     GenScriptOut(inXml);
                On-error;
                EndMon;
             Other;
                inXml = xmlFlowNmEob + '$';
                Callp     GenScriptOut(inXml);
             EndSl;
          Endif;
      /end-free
     C                   MOVE      '1'           *INLR
     C*
     C*================================================================
     C*
     C*  AssgnChkHdrs - Assign Check Headers
     C*
     C*================================================================
     C     AssgnChkHdrs  BegSr
     C*
     C* Load Check Heading Fields
     C*
     C                   Select
     C                   When      isEft = *On
     C                   EvalR     hdrChkDate = 'Eft date   :'
     C                   EvalR     hdrCheck# = 'Eft#'
     C                   When      *in16 = *On
     C                   EvalR     hdrChkDate = 'Voucher date:'
     C                   EvalR     hdrCheck# = 'Vch#'
     C                   When      *in16 = *Off
     C                   EvalR     hdrChkDate = 'Check date :'
     C                   EvalR     hdrCheck# = 'Chk#'
     C                   EndSl
     C*
     C                   EndSr
       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
      /Free
       Begsr ScriptXML;

       xmlFile = ' ';
       OutString = ' ';
       message = %trim(processType);
       xxCo# = %char(EAco#);
       xxfnd = %char(EAfnd);
       XPCat = 'MC';

       If isMassReq;

          StLetter('XMLREQUEST':xmlFile:message:outMode:'000000000':'  ':
           xxUserData:'000':'000':massFlag:xmlLink);

          isMassReq = *off;
          xmlFlowName = xmlFile;
          xmlLink = xmlFile;
          Chain (xmlFile) xp100;
          Update xp100;

       Endif;

       if processType = 'EOB Print';
          outMode = 'M';
       endif;

       StLetter('XMLREQUEST':xmlFile:message:outMode:x1Rrn1:xxPre:
        xxUserData:xxCo#:xxFnd:massFlag:xmlLink);

       If xmlFlowName = ' ';
          xmlFlowName = xmlFile;
       Endif;

       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
          if processType = 'EOB Summary';
             xmlFileSum = XPxmlt;
          endif;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       XPData = 'Y';

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DsTime;
       XPDt   = xxIdt;

       Update xp100;

       Endsr;
      /end-Free
     C*================================================================
     C*
     C*  SRPRT - SUBROUTINE TO PRINT A SINGLE EOMB
     C*
     C*================================================================
     C     SRPRT         BEGSR
     C*
     C*
     C* GET USER FOR ROUTING  (ONLY ONCE FOR BATCH)
     C*
     C     ROUTPT        IFNE      'Y'
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  Q1USER        KKUSER
     C                   ELSE
     C                   MOVEL     EOIUSR        KKUSER
     C                   ENDIF                                                  @@MODE = 'P'
     C     UPKY          CHAIN     PUP01                              98
     C     *IN98         IFEQ      *ON
     C                   MOVE      *BLANKS       XX13A            13
     C                   MOVEL     'FEE SCED'    XX13A
     C                   MOVE      'USER'        XX13A
     C                   MOVE      *BLANKS       UPUSNM
     C                   MOVEL     XX13A         UPUSNM
     C                   ENDIF                                                  *IN98 = *ON
     C*
     C* GET OUTPUT ROUTING INFORMATION
     C*
     C                   MOVE      '0'           KKFIL#
     C     ORKY          CHAIN     POR01                              98
     C*
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  @@OUTQ        OROUTQ
     C                   MOVEL(P)  @@HOLD        ORHOLD
     C                   ENDIF                                                  @@MODE = *OFF
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
      /Free
       If xxRrn1 = 0;
      /End-free
     C                   If        isUseSpoolout
     C                   Endif
     C*
     C*** Write to Scriptura EOB Routing Workfiles
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtRtWrk
     C                   Endif
      /Free
       Endif;
      /End-free
     C                   MOVE      *OFF          *IN01
     C     @@MODE        IFEQ      'P'
     C                   SETON                                        74        RESET PAGE NUMBER
     C                   ENDIF                                                  MODE
     C*
     C                   MOVE      'Y'           ROUTPT
     C                   ENDIF                                                  ROUTPT NE 'Y'
     C*
     C*
     C* CHECK TO SEE IF GROUP 440
     C*
     C     EOFND         IFEQ      440
     C     ##CLT#        ANDEQ     '114'
     C     EOFND         OREQ      460
     C     ##CLT#        ANDEQ     '114'
     C     EOFND         OREQ      600
     C                   SETON                                        16
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF
     C*
     C* Load Check Heading Fields
     C*
     C                   ExSr      AssgnChkHdrs
     C
     C*
     C* SET KEY FIELDS
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   MOVE      EOAST         KKST
     C                   MOVEL     EOSINV        XXTRID           15
     C*
     C* GET STATE NAME
     C*
     C     *IN18         IFEQ      *ON
     C                   MOVEL(P)  EAPOST        XXKEY
     C     XXKEY         CHAIN     PST01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVE      STNAME        XXSTNM
     C                   ELSE
     C                   CLEAR                   XXSTNM
     C                   ENDIF                                                  *IN98 = OFF
     C                   ELSE
     C                   CLEAR                   XXSTNM
     C                   ENDIF                                                  *IN18 = ON
     C*
     C* SET INDICATORS FOR CONDITIONAL DISPLAY OF DATA
     C*
     C                   SETOFF                                       62
     C     EAKY2         CHAIN     PEA01                              98
     C     EPKY          CHAIN     PEP01                              99
     C     *IN98         IFEQ      *OFF
     C     *IN99         ANDEQ     *OFF
     C     EPTAX         IFEQ      'Y'
     C                   SETON                                        62
     C                   ENDIF
     C                   ENDIF
     C* IF BILL TYPE IS 'HOSPITAL', THEN DISPLAY 'TO SERVICE DATE'
     C*
     C     EOBTYP        IFEQ      'H'
     C                   SETON                                        40
     C                   ELSE
     C                   SETOFF                                       40
     C                   ENDIF                                                  EOBTYP = 'N'
     C*
     C* IF RECON, THEN DISPLAY 'PREVIOUSLY PAID' AND 'THIS PAYMENT'
     C* DATA
     C*
     C     EORCN         IFGT      *ZEROS
     C                   SETON                                        33
     C                   ELSE
     C                   SETOFF                                       33
     C                   ENDIF                                                  EORCN GT *ZEROS
     C*
     C* GET GROUP NAME
     C*
     C                   MOVE      *OFF          *IN52
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FNNAME        X1NAME
     C     EOAST         IFEQ      'NC'
     C                   MOVE      *ON           *IN52
     C                   MOVEL     FNFARA        SVFARA
     C                   MOVEL     FNFAX         SVFPH1            3
     C                   MOVE      FNFAX         SVFPH2            4
     C                   ENDIF
     C                   ELSE
     C                   CLEAR                   X1NAME
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* FORMAT LASTNAME, FIRSTNAME
     C*
     C                   MOVEL(P)  EOLSN         ##LAST           56
     C                   MOVEL(P)  EOFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL(P)  ##LAST        XXNAME           40
     C*
     C* GET EMPLOYER AND DIVISION FOR CLAIM MASTER FILE
     C*
     C     CMKY          CHAIN     PCM01                              97
     C     *IN97         IFEQ      *OFF
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C     EMKY          CHAIN     PEM01                              97
     C     *IN97         IFEQ      *OFF
     C                   MOVEL     EMNAME        XXENAM
     C                   MOVEL     EMNAME        XXENAM1
     C                   ELSE
     C                   CLEAR                   XXENAM
     C                   CLEAR                   XXENAM1
     C                   ENDIF                                                  *IN97 = *OFF
     C                   ELSE
     C                   SETOFF                                       17
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C* PUT ACCIDENT DATE IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C* PUT TRANSACTION DATE IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C*
     C* PUT DATE RECEIVED IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCRDT            8 0
     C*
     C*
     C* GET PROVIDER NAME, ADDRESS
     C*
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C*
     C     EOADR2        IFNE      *BLANKS
     C                   SETON                                        32
     C                   ELSE
     C                   SETOFF                                       32
     C                   ENDIF                                                  PDADR2 = *BLANKS
     C*
     C                   MOVEL(P)  EOCTY         XXCTY
     C                   MOVE      EOST          XXSTAT
     C                   MOVE      EOZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ1           40
     C*
     C* GET LATEST RECON DATE
     C*
     C                   CLEAR                   XXRCDT
     C                   MOVE      *HIVAL        KKIDT
     C                   MOVE      *HIVAL        KKITIM
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  98
     C     *IN98         DOWEQ     *OFF
     C*
     C     ESBLST        IFEQ      'R'
     C                   LEAVE
     C                   ENDIF                                                  ESBLST = 'R'
     C*
     C     ESKY2         READPE    PES01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     ESBLST        IFEQ      'R'
     C     EORCN         ANDGT     *ZEROS
     C                   Z-ADD     ESIDT         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCDT            8 0
     C                   ENDIF                                                  ESBLST = 'R'
     C*
     C* GET CHECK DATE AND NUMBER
     C*
     C                   Z-ADD     EORCN         KKRCN
     C     EAKY2         CHAIN     PEA01                              98
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYM2
     C                   MOVEL     SRDTO         XXWCPD            8 0
     C                   ENDIF                                                  *IN98 = *OFF
     C                   SetOff                                       96
     C                   If        xxWcPd = 0
     C                   SetOn                                        96
     C                   EndIf
     C*
     C* GET 'FROM' AND 'TO' SERVICE DATE
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C*
     C                   SETOFF                                       36
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C     EHKY          CHAIN     PEH01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C* PATIENT ACCOUNT#
     C*
     C                   MOVEL(P)  EHACCT        XXACCT
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'H'
     C     EUKY          CHAIN     PEU01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C* PATIENT ACCOUNT#
     C*
     C                   MOVEL(P)  EUACCT        XXACCT
     C     EUICN         IFEQ      'IM'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IR'
     C     EUICN         OREQ      'IS'
     C                   SETON                                        76
     C                   ENDIF                                                  EUICN = IM/IP/IR/IS
     C*
     C*
     C* GET OTHER HOSPITAL SPECIFIC INFORMATION FOR DISPLAY
     C*
     C*
     C* DRG Code and Description
     C*
     C                   Z-ADD     EUDRG#        XXDRG#
     C                   MOVEL     EUDGDS        XXDGDS
     C*
     C*
     C* ADMISSION DATE
     C                   Z-ADD     EUADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXADDT            8 0
     C*
     C* STATEMENT COVERS DATES
     C*
     C                   Z-ADD     EUSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSCPF            8 0
     C*
     C                   Z-ADD     EUSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSCPT            8 0
     C*
     C* SUBMISSION DATE
     C*
     C                   Z-ADD     EUSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSBDT            8 0
     C*
     C* TYPE OF SERVICE AND DESCRIPTION
     C*
     C                   MOVEL     EUICN         XXICN             2
     C                   MOVEL     'SVTY'        KKTBID
     C                   MOVEL(P)  XXICN         KXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXICDS           30
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* TYPE OF BILL AND DESCRIPTION
     C*
     C                   MOVEL     EUBLTY        XXBLTY            3
     C                   MOVEL(P)  BTTBID        KKTBID
     C                   MOVEL(P)  XXBLTY        KXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXBDSC           30
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* GET PROCEDURE CODE DESCRIPTIONS
     C*
     C                   MOVE      *BLANKS       XXPRD1           35
     C                   MOVE      *BLANKS       XXPRD2           35
     C                   MOVE      *BLANKS       XXPRD3           35
     C                   MOVE      *BLANKS       XXPRD4           35
     C*
     C     EUPRC1        IFNE      *BLANKS
     C                   MOVEL(P)  EUPRC1        KKCPT4
     C     PRKY          CHAIN     PPR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        XXPRD1
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EUPRC1 NE *BLANKS
     C*
     C     EUPRC2        IFNE      *BLANKS
     C                   MOVEL(P)  EUPRC2        KKCPT4
     C     PRKY          CHAIN     PPR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        XXPRD2
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EUPRC2 NE *BLANKS
     C*
     C     EUPRC3        IFNE      *BLANKS
     C                   MOVEL(P)  EUPRC3        KKCPT4
     C     PRKY          CHAIN     PPR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        XXPRD3
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EUPRC2 NE *BLANKS
     C*
     C     EUPRC4        IFNE      *BLANKS
     C                   MOVEL(P)  EUPRC4        KKCPT4
     C     PRKY          CHAIN     PPR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        XXPRD4
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EUPRC2 NE *BLANKS
     C*
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'X'
     C     EPKY          CHAIN     PEP01                              99
     C                   MOVE      *BLANKS       XXACCT
     C                   MOVE      *BLANKS       XXPHAR
     C     EPPHAR        IFNE      *BLANKS
     C                   MOVEL(P)  EPPHAR        XXPHAR
     C                   SETON                                        36
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'D'
     C                   MOVE      *BLANKS       XXACCT
     C                   ENDSL
     C*
     C* GET MEDICAL EXAMINER HEADING, MEDICAL EXAMINER VALUES,
     C* AND REDUCTIONS HEADINGS
     C*
     C                   MOVE      *BLANKS       XXMUSR           20
     C*
     C                   Z-ADD     EOCO#         ##CO#
     C                   Z-ADD     EOSUB#        ##SUB#
     C                   Z-ADD     EOFND         ##FND
     C                   Z-ADD     EOFYR         ##FYR
     C                   Z-ADD     EOCASE        ##CASE
     C                   Z-ADD     EOEOB#        ##EOB#
     C                   MOVE      *BLANKS       ##USER           40
     C                   CALL      'FA723R'
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##EOB#
     C                   PARM                    ##USER
     C*
     C     ##USER        IFEQ      'NONE'
     C                   SETON                                        67
     C                   ELSE
     C                   SETOFF                                       67
     C                   MOVEL(P)  ##USER        XXMUSR
     C                   ENDIF                                                  ##USER = 'NONE'
     C*
     C     CXKY          CHAIN     PCX01                              98
     C*
     C* GET LICENCE #/MEDICARE # BASED ON TYPE OF BILL/ADJ STATE
     C*
     C                   SELECT
     C*
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C                   MOVEL(P)  EHLIC#        XXLIC#           15
     C     EHFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EHFLIC        XXLIC#
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'H'
     C     EOAST         IFEQ      'TX'
     C                   MOVEL(P)  EUMCAR        XXLIC#
     C*
     C                   ELSE
     C                   MOVEL(P)  EULIC#        XXLIC#
     C     EUFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EUFLIC        XXLIC#
     C                   ENDIF
     C*
     C                   ENDIF                                                  EOAST=TX
     C     EOBTYP        WHENEQ    'X'
     C     EOAST         ANDEQ     'FL'
     C                   MOVEL     EPPHAR        XXLIC#
     C*
     C                   OTHER
     C                   MOVE      *BLANKS       XXLIC#
     C*
     C                   ENDSL
     C*
     C*
     C* GET DECIMAL VERSIONS OF DIAGNOSIS CODES
     C*
     C                   MOVE      *BLANKS       XXDS1             8
     C                   MOVE      *BLANKS       XXDS2             8
     C                   MOVE      *BLANKS       XXDS3             8
     C                   MOVE      *BLANKS       XXDS4             8
     C*
     C*
     C                   MOVE      *BLANKS       XXNUM1            2
     C                   MOVE      *BLANKS       XXNUM2            2
     C                   MOVE      *BLANKS       XXNUM3            2
     C                   MOVE      *BLANKS       XXNUM4            2
     C*
     C* GET DIAGNOSIS CODE DESCRIPTIONS
     C*
     C                   MOVE      *BLANKS       DESC1
     C                   MOVE      *BLANKS       DESC2
     C                   MOVE      *BLANKS       DESC3
     C                   MOVE      *BLANKS       DESC4
     C*
      /Free
       kklost = 'D';
       Select;

       // Medical Bills

       When EobTyp = 'N';
       // ICD10 is active then populates ICD10 description else ICD9
       If EHIcdInd = 'Y';
          If EHICD1001 <> *blanks;
             kkicd10 = EHICD1001;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC1 = ICdsc1;
                XXDS1 = ICfcode;
                XXNUM1= '1.';
             Endif;
          Endif;

          If EHICD1002 <> *blanks;
             kkicd10 = EHICD1002;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC2 = ICdsc1;
                XXDS2 = ICfcode;
                XXNUM2= '2.';
             Endif;
          Endif;

          If EHICD1003 <> *blanks;
             kkicd10 = EHICD1003;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC3 = ICdsc1;
                XXDS3 = ICfcode;
                XXNUM3= '3.';
             Endif;
          Endif;

          If EHICD1004 <> *blanks;
             kkicd10 = EHICD1004;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC4 = ICdsc1;
                XXDS4 = ICfcode;
                XXNUM4= '4.';
             Endif;
          Endif;
          If EHicdE <> *blanks;
             kkicd10 = EHicdE;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                descE = ICdsc1;
                xxDSe = ICfcode;
             Endif;
          Endif;
       Else;
         If EHDIA1 <> *blanks;
            kkicd9 = EHDIA1;
            Chain diky pdi01;
            If %found(fmdil);
               DESC1 = DIdsc1;
               XXDS1 = DIds1;
               XXNUM1= '1.';
            Endif;
         Endif;

         If EHDIA2 <> *blanks;
            kkicd9 = EHDIA2;
            Chain diky pdi01;
            If %found(fmdil);
               DESC2 = DIdsc1;
               XXDS2 = DIds1;
               XXNUM2= '2.';
            Endif;
         Endif;

         If EHDIA3 <> *blanks;
            kkicd9 = EHDIA3;
            Chain diky pdi01;
            If %found(fmdil);
               DESC3 = DIdsc1;
               XXDS3 = DIds1;
               XXNUM3= '3.';
            Endif;
         Endif;

         If EHDIA4 <> *blanks;
            kkicd9 = EHDIA4;
            Chain diky pdi01;
            If %found(fmdil);
               DESC4 = DIdsc1;
               XXDS4 = DIds1;
               XXNUM4= '4.';
            Endif;
          Endif;
       Endif;

       // Hospital Bills

       When EobTyp = 'H';
       // ICD10 is active then populates ICD10 description else ICD9
       If EUicdInd = 'Y' ;
          If EUICD1001 <> *blanks;
             kkicd10 = EUicd1001;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC1 = ICdsc1;
                XXDS1 = ICfcode;
                XXNUM1= '1.';
             Endif;
          Endif;

          If EUicd1002 <> *blanks;
             kkicd10 = EUicd1002;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC2 = ICdsc1;
                XXDS2 = ICfcode;
                XXNUM2= '2.';
             Endif;
          Endif;

          If EUicd1003 <> *blanks;
             kkicd10 = EUicd1003;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC3 = ICdsc1;
                XXDS3 = ICfcode;
                XXNUM3= '3.';
             Endif;
          Endif;

          If EUicd1004 <> *blanks;
             kkicd10 = EUicd1004;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC4 = ICdsc1;
                XXDS4 = ICfcode;
                XXNUM4= '4.';
             Endif;
          Endif;
       Else;
          If EUdia1 <> *blanks;
             kkicd9 = EUdia1;
             Chain diky pdi01;
             If %found(fmdil);
                DESC1 = DIdsc1;
                XXDS1 = DIds1;
                XXNUM1= '1.';
             Endif;
          Endif;

          If EUdia2 <> *blanks;
             kkicd9 = EUdia2;
             Chain diky pdi01;
             If %found(fmdil);
                DESC2 = DIdsc1;
                XXDS2 = DIds1;
               XXNUM2= '2.';
             Endif;
          Endif;

          If EUdia3 <> *blanks;
             kkicd9 = EUdia3;
             Chain diky pdi01;
             If %found(fmdil);
                DESC3 = DIdsc1;
                XXDS3 = DIds1;
               XXNUM3= '3.';
             Endif;
          Endif;

          If EUdia4 <> *blanks;
             kkicd9 = EUdia4;
             Chain diky pdi01;
             If %found(fmdil);
                DESC4 = DIdsc1;
                XXDS4 = DIds1;
               XXNUM4= '4.';
             Endif;
          Endif;
       Endif;

       // Dental Bills

       When EobTyp = 'D';
       Chain faedlkey ped01;
       If %found(faedl);
       // ICD10 is active then populates ICD10 description else ICD9
          If EDicdInd = 'Y';
             If EDICD1001 <> *blanks;
                kkicd10 = EDicd1001;
                Chain fmicd10l1key pic01;
                  If %found(fmicd10l1);
                     DESC1 = ICdsc1;
                     XXDS1 = ICfcode;
                     XXNUM1= '1.';
                  Endif;
             Endif;

             If EDicd1002 <> *blanks;
               kkicd10 = EDicd1002;
               Chain fmicd10l1key pic01;
                 If %found(fmicd10l1);
                    DESC2 = ICdsc1;
                    XXDS2 = ICfcode;
                    XXNUM2= '2.';
                 Endif;
             Endif;

             If EDicd1003 <> *blanks;
                kkicd10 = EDicd1003;
                Chain fmicd10l1key pic01;
                  If %found(fmicd10l1);
                     DESC3 = ICdsc1;
                     XXDS3 = ICfcode;
                     XXNUM3= '3.';
                  Endif;
             Endif;

             If EDicd1004 <> *blanks;
                kkicd10 = EDicd1004;
                Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   DESC4 = ICdsc1;
                   XXDS4 = ICfcode;
                   XXNUM4= '4.';
                Endif;
             Endif;
          Else;
             If EDdia1 <> *blanks;
                kkicd9 = EDdia1;
                Chain diky pdi01;
                  If %found(fmdil);
                     DESC1 = DIdsc1;
                     XXDS1 = DIds1;
                     XXNUM1= '1.';
                  Endif;
             Endif;

             If EDdia2 <> *blanks;
                kkicd9 = EDdia2;
                Chain diky pdi01;
                  If %found(fmdil);
                     DESC2 = DIdsc1;
                     XXDS2 = DIds1;
                     XXNUM2= '2.';
                  Endif;
             Endif;

             If EDdia3 <> *blanks;
                kkicd9 = EDdia3;
                Chain diky pdi01;
                  If %found(fmdil);
                     DESC3 = DIdsc1;
                     XXDS3 = DIds1;
                     XXNUM3= '3.';
                  Endif;
             Endif;

             If EDdia4 <> *blanks;
                kkicd9 = EDdia4;
                Chain diky pdi01;
                  If %found(fmdil);
                     DESC4 = DIdsc1;
                     XXDS4 = DIds1;
                     XXNUM4= '4.';
                  Endif;
             Endif;
          Endif;
       Endif;
       Endsl;

      /End-Free
     C                   if        isScriptEOB = 'Y'
     C                   exsr      wrtDGwrk
     C                   endif
     C*
     C* PUT FROM/TO SERVICE DATES IN SCREEN DISPLAY FORMAT
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C                   TIME                    DSDTTM
     C                   SETOFF                                       8081
     C*
     C* SET INDICATOR TO DISPLAY NETWORK DISCLAIMER IF NECESSARY
     C*
     C     EONTWK        IFNE      *BLANKS
     C     EONDIS        ANDNE     *ZEROS
     C                   SETON                                        47
     C     EONDSC        IFEQ      *BLANKS
     C                   MOVE      EONTWK        KKNWCD
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     NWNAME        XXNWNM           40
     C                   MOVEL     NWMSG(1)      XXNWM1
     C     XXNWNM        CAT       NWMSG(2):1    XXNWM2
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C*
     C                   CALL      'FA752R'
     C                   PARM      EONDSC        ##INPT          255
     C     XXNWM         PARM      *BLANKS       ##OUT1          140
     C     XXNW2         PARM      *BLANKS       ##OUT2          140
     C     XXNW3         PARM      *BLANKS       ##OUT3          140
     C*
     C     XXNW2         IFNE      *BLANKS
     C                   SETON                                        80
     C                   ENDIF
     C     XXNW3         IFNE      *BLANKS
     C                   SETON                                        81
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF                                                  EONTWK NE *BLANKS
     C*
     C* Use the Network Name passed in from the upload
     C*
     C     ##CLT#        IFEQ      '135'
     C     EOMBUP        ANDEQ     'C'
     C     EONDIS        IFNE      *ZEROS
     C                   SETON                                        47
     C                   MOVEL     EONWNM        XXNWNM
     C                   MOVEL     NWMSG(1)      XXNWM1
     C     XXNWNM        CAT       NWMSG(2):1    XXNWM2
     C*
     C                   ELSE                                                   EONDIS <> 0
     C                   SETOFF                                       47
     C*
     C                   ENDIF                                                  EONDIS <> 0
     C*
     C                   ENDIF                                                  ##CLT# = 135
     C*
     C*
     C* PREPARE TO CHANGE FONT
     C*
     C                   EXSR      MSKSS#
     C                   If        isUseSpoolout
     C                   WRITE     HEAD1
     C                   Endif
     C                   SETOFF                                       74
     C                   MOVEL(P)  XXCSZ1        X1CSZ1
     C                   MOVE      '0'           *IN35
     C     EOBNPI        IFNE      *BLANKS
     C                   MOVEL(P)  EOBNPI        XXBNPI
     C                   SETON                                        35
     C                   ELSE
     C                   SETOFF                                       35
     C                   ENDIF
     C                   MOVEL(P)  CXHEX         X1HEX
     C                   MOVEL(P)  CXHEX         X1HEXB
     C                   If        isUseSpoolout
     C                   WRITE     HEAD2
     C                   WRITE     HEAD3
     C                   Endif
     C*
     C*** Write to Scriptura Header EOB workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtHdWrk
     C                   Endif
     C*
     C* CYLCE THROUGH FDCAP FILE TO GET ALL CHARGES FOR THE BILL
     C                   CLEAR                   EOB
     C*
     C                   MOVE      *LOVAL        KKFDAT
     C                   MOVE      *LOVAL        KKTDAT
     C     CAKY1         SETLL     PCA01
     C     CAKY          READE     PCA01                                  98
     C     *IN98         IFEQ      *OFF
     C                   CLEAR                   XXFDAT
     C                   CLEAR                   XXTDAT
     C                   CLEAR                   XXCPT4
     C                   CLEAR                   XXMOD1
     C                   CLEAR                   XXMOD2
     C                   CLEAR                   XXMOD3
     C                   CLEAR                   XXMOD4
     C                   CLEAR                   XXPOS
     C                   CLEAR                   XXDRF1
     C                   CLEAR                   XXDRF2
     C                   CLEAR                   XXDRF3
     C                   CLEAR                   XXDRF4
     C                   CLEAR                   xxDxrA
     C                   CLEAR                   xxDxrB
     C                   CLEAR                   xxDxrC
     C                   CLEAR                   xxDxrD
     C                   CLEAR                   xxDxrE
     C                   CLEAR                   xxDxrF
     C                   CLEAR                   xxDxrG
     C                   CLEAR                   xxDxrH
     C                   CLEAR                   xxDxrI
     C                   CLEAR                   xxDxrJ
     C                   CLEAR                   xxDxrK
     C                   CLEAR                   xxDxrL
     C                   CLEAR                   XXPRDS
     C                   CLEAR                   XXBILD
     C                   CLEAR                   X1EOB
     C                   CLEAR                   X2EOB
     C                   CLEAR                   X3EOB
     C                   CLEAR                   X4EOB
     C                   CLEAR                   XNOTE
     C                   CLEAR                   XXUNTS
     C                   Z-ADD     CAFDAT        HLFDAT
     C                   Z-ADD     CATDAT        HLTDAT
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDAT            8 0
     C*
     C                   Z-ADD     CATDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTDAT            8 0
     C                   If        isUseSpoolout
     C                   WRITE     DETL12
     C                   Endif
     C                   ENDIF
     C     *IN98         DOWEQ     *OFF                                         *IN98 = *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   Z-ADD     CARCN         KKRCN#
     C     DAKY          CHAIN     PDA01                              60
     C     CAFDAT        IFNE      HLFDAT
     C     CATDAT        ORNE      HLTDAT
     C                   Z-ADD     CAFDAT        HLFDAT
     C                   Z-ADD     CATDAT        HLTDAT
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDAT            8 0
     C*
     C                   Z-ADD     CATDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTDAT            8 0
     C                   If        isUseSpoolout
     C                   WRITE     DETL12
     C                   Endif
     C                   ENDIF
     C*
     C* GET CPT4/PROCEDURE CODE, PROCEDURE MODIFIERS AND
     C* PLACE OF SERVICE BASED ON THE TYPE OF BILL
     C*
     C* Initialize diag cross reference fields.
     C                   eval      xxDxrA = ' '
     C                   eval      xxDxrB = ' '
     C                   eval      xxDxrC = ' '
     C                   eval      xxDxrD = ' '
     C                   eval      xxDxrE = ' '
     C                   eval      xxDxrF = ' '
     C                   eval      xxDxrG = ' '
     C                   eval      xxDxrH = ' '
     C                   eval      xxDxrI = ' '
     C                   eval      xxDxrJ = ' '
     C                   eval      xxDxrK = ' '
     C                   eval      xxDxrL = ' '
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C     DHKY          CHAIN     PDH01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  DHPRCD        XXCPT4           11
     C                   MOVE      DHMOD1        XXMOD1
     C                   MOVE      DHMOD2        XXMOD2
     C                   MOVE      DHMOD3        XXMOD3
     C                   MOVE      DHMOD4        XXMOD4
     C                   MOVE(P)   DHPOS         XXPOS
     C                   MOVEL     DHPRDS        XXPRDS           25
     C     DHBPRC        IFNE      *BLANKS
     C                   MOVEL(P)  DHBPRC        XXBILD           11
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DHBRPC <> *BLANKS
      /Free

         If ( ##CLT# = '133' Or ##CLT# = '136' ) and
              DHNDC <> *Blanks ;

            XXBILD = *Blanks;
            XXCPT4 = DHNDC ;
            xxprds =DHNDCD;
         Endif ;
      /End-Free
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DHUNTS        XXUNTS            5 2
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'H'
     C     DUKY          CHAIN     PDU01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVE      DUPMOD        XXMOD1
     C                   MOVE      DUMOD2        XXMOD2
     C                   MOVE      DUMOD3        XXMOD3
     C                   MOVE      DUMOD4        XXMOD4
     C                   MOVEL(P)  DURVCD        XXCPT4
     C                   MOVE      DURATE        XXCPT4
     C                   MOVE(P)   DUPOS         XXPOS
     C                   MOVEL     DUDESC        XXPRDS
     C     DUBPRC        IFNE      *ZEROS
     C     DUBRAT        ORNE      *BLANKS
     C                   MOVEL(P)  DUBPRC        X1BILD            8
     C                   MOVE      DUBRAT        X1BILD
     C                   MOVEL(P)  X1BILD        XXBILD
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DUBRPC <> *BLANKS
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DUUNTS        XXUNTS
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'X'
     C     DPKY          CHAIN     PDP01                              98
     C     *IN98         IFEQ      *OFF
     C     DPHCPC        IFNE      *BLANKS
     C                   MOVEL(P)  DPHCPC        XXCPT4
     C                   ELSE
     C                   MOVEL(P)  DPNDC         XXCPT4
     C                   ENDIF
     C                   MOVEL     DPDESC        XXPRDS
     C     DPBNDC        IFNE      *BLANKS
     C     DPHCPC        IFNE      *BLANKS
     C                   MOVEL(P)  DPHCPC        XXBILD
     C                   ELSE
     C                   MOVEL(P)  DPBNDC        XXBILD
     C                   ENDIF
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DPBNDC <> *BLANKS
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DPUNTS        XXUNTS
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'D'
     C     DDKY          CHAIN     PDD01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  DDPRCD        XXCPT4
     C                   MOVEL     DDPRDS        XXPRDS
     C     DDBPRC        IFNE      *BLANKS
     C                   MOVEL(P)  DDBPRC        XXBILD
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DPBNDC <> *BLANKS
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DDUNTS        XXUNTS
     C                   if        DDdxr1 = 'X'
     C                   eval      xxDxrA = 'A'
     C                   endif
     C                   if        DDdxr2 = 'X'
     C                   eval      xxDxrB = 'B'
     C                   endif
     C                   if        DDdxr3 = 'X'
     C                   eval      xxDxrC = 'C'
     C                   endif
     C                   if        DDdxr4 = 'X'
     C                   eval      xxDxrD = 'D'
     C                   endif
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C                   ENDSL
     C*
     C* GET DIAGNOSIS CODE BASED ON CROSS REFERENCE
     C*
     C                   MOVE      *BLANKS       XXDRF1            1
     C                   MOVE      *BLANKS       XXDRF2            1
     C                   MOVE      *BLANKS       XXDRF3            1
     C                   MOVE      *BLANKS       XXDRF4            1
     C     EOBTYP        IFEQ      'N'
     C     EOBTYP        OREQ      'V'
     C*
     C     DHDXR1        IFEQ      'X'
     C                   SELECT
     C     XXDRF1        WHENEQ    ' '
     C                   MOVE      '1'           XXDRF1
     C     XXDRF2        WHENEQ    ' '
     C                   MOVE      '1'           XXDRF2
     C     XXDRF3        WHENEQ    ' '
     C                   MOVE      '1'           XXDRF3
     C     XXDRF4        WHENEQ    ' '
     C                   MOVE      '1'           XXDRF4
     C                   ENDSL
     C                   ENDIF                                                  DHDXR1 = 'X'
     C*
     C     DHDXR2        IFEQ      'X'
     C                   SELECT
     C     XXDRF1        WHENEQ    ' '
     C                   MOVE      '2'           XXDRF1
     C     XXDRF2        WHENEQ    ' '
     C                   MOVE      '2'           XXDRF2
     C     XXDRF3        WHENEQ    ' '
     C                   MOVE      '2'           XXDRF3
     C     XXDRF4        WHENEQ    ' '
     C                   MOVE      '2'           XXDRF4
     C                   ENDSL
     C                   ENDIF                                                  DHDXR2 = 'X'
     C*
     C     DHDXR3        IFEQ      'X'
     C                   SELECT
     C     XXDRF1        WHENEQ    ' '
     C                   MOVE      '3'           XXDRF1
     C     XXDRF2        WHENEQ    ' '
     C                   MOVE      '3'           XXDRF2
     C     XXDRF3        WHENEQ    ' '
     C                   MOVE      '3'           XXDRF3
     C     XXDRF4        WHENEQ    ' '
     C                   MOVE      '3'           XXDRF4
     C                   ENDSL
     C                   ENDIF                                                  DHDXR3 = 'X'
     C*
     C     DHDXR4        IFEQ      'X'
     C                   SELECT
     C     XXDRF1        WHENEQ    ' '
     C                   MOVE      '4'           XXDRF1
     C     XXDRF2        WHENEQ    ' '
     C                   MOVE      '4'           XXDRF2
     C     XXDRF3        WHENEQ    ' '
     C                   MOVE      '4'           XXDRF3
     C     XXDRF4        WHENEQ    ' '
     C                   MOVE      '4'           XXDRF4
     C                   ENDSL
     C                   ENDIF                                                  DHDXR4 = 'X'
     C*
     C                   if        DHdxr1 = 'X'
     C                   eval      xxDxrA = 'A'
     C                   endif
     C                   if        DHdxr2 = 'X'
     C                   eval      xxDxrB = 'B'
     C                   endif
     C                   if        DHdxr3 = 'X'
     C                   eval      xxDxrC = 'C'
     C                   endif
     C                   if        DHdxr4 = 'X'
     C                   eval      xxDxrD = 'D'
     C                   endif
     C                   if        DHdxrE = 'X'
     C                   eval      xxDxrE = 'E'
     C                   endif
     C                   if        DHdxrF = 'X'
     C                   eval      xxDxrF = 'F'
     C                   endif
     C                   if        DHdxrG = 'X'
     C                   eval      xxDxrG = 'G'
     C                   endif
     C                   if        DHdxrH = 'X'
     C                   eval      xxDxrH = 'H'
     C                   endif
     C                   if        DHdxrI = 'X'
     C                   eval      xxDxrI = 'I'
     C                   endif
     C                   if        DHdxrJ = 'X'
     C                   eval      xxDxrJ = 'J'
     C                   endif
     C                   if        DHdxrK = 'X'
     C                   eval      xxDxrK = 'K'
     C                   endif
     C                   if        DHdxrL = 'X'
     C                   eval      xxDxrL = 'L'
     C                   endif
     C                   ENDIF                                                  EOBTYP = 'N'
     C* GET EOB CODES
     C*
     C*** Beg Chg *** 08/22/22 ***********************************************
     C****************   Z-ADD     1             X                 1 0
     C                   Z-ADD     1             X
     C*** End Chg *** 08/22/22 ***********************************************
     C                   MOVE      *BLANKS       X1EOB
     C                   MOVE      *BLANKS       X2EOB
     C                   MOVE      *BLANKS       X3EOB
     C                   MOVE      *BLANKS       X4EOB
     C                   MOVE      *BLANKS       XNOTE
     C                   SETOFF                                       424344
     C                   SETOFF                                       45
     C     DEKY          CHAIN     PDE01                              98
     C     *IN98         DOWEQ     *OFF
     C     X             ANDLE     4
     C*
     C* SKIP INACTIVE CODES
     C*
     C*
     C     DEPRNT        IFNE      'N'
     C     DESTAT        ANDNE     'I'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*
     C* GET THE X-REF FOR TEXAS BILLS
     C*
     C     EOAST         IFEQ      'TX'
     C**** Beg Del *** 07/18/23 **************************************
     C****               CALL      'FA738R'
     C****               PARM      KKCO#         $$CO#             3 0
     C****               PARM      DEEOB         $$EOB            10
     C****               PARM      *BLANKS       $$XREF            3
     C**** End Del *** 07/18/23 **************************************
     C*
     C     $$XREF        IFNE      *BLANKS
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  $$XREF        DEEOB
     C                   MOVEL(P)  $$XREF        DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C* GET THE X-REF FOR FLORIDA BILLS
     C*
     C     EOAST         IFEQ      'FL'
     C**** Beg Del *** 07/18/23 **************************************
     C****               CALL      'FA741R'
     C****               PARM      KKCO#         $$CO#
     C****               PARM      DEEOB         $$EOB
     C****               PARM      *BLANKS       $$XREF
     C****               PARM      *BLANKS       $$DESC           70
     C**** End Del *** 07/18/23 **************************************
     C*
     C     $$XREF        IFNE      *BLANKS
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  $$XREF        DEEOB
     C                   MOVEL(P)  $$XREF        DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   ENDIF
     C                   ENDIF
     C                   SELECT
     C     X             WHENEQ    1
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X1EOB
     C                   MOVE      DEEOBCD       X1EOB
     C**** End Chg *** 07/18/23 **************************************
     C     X1EOB         IFNE      *BLANKS
     C                   SETON                                        42
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF                                                  X1EOB NE *BLANKS
     C     X             WHENEQ    2
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X2EOB
     C                   MOVE      DEEOBCD       X2EOB
     C**** End Chg *** 07/18/23 **************************************
     C     X2EOB         IFNE      *BLANKS
     C                   SETON                                        43
     C                   ELSE
     C                   SETOFF                                       43
     C                   ENDIF                                                  X2EOB NE *BLANKS
     C     X             WHENEQ    3
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X3EOB
     C                   MOVE      DEEOBCD       X3EOB
     C**** End Chg *** 07/18/23 **************************************
     C     X3EOB         IFNE      *BLANKS
     C                   SETON                                        44
     C                   ELSE
     C                   SETOFF                                       44
     C                   ENDIF                                                  X3EOB NE *BLANKS
     C     X             WHENEQ    4
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X4EOB
     C                   MOVE      DEEOBCD       X4EOB
     C**** End Chg *** 07/18/23 **************************************
     C     X4EOB         IFNE      *BLANKS
     C                   SETON                                        45
     C                   ELSE
     C                   SETOFF                                       45
     C                   ENDIF                                                  X4EOB NE *BLANKS
     C                   ENDSL
     C                   ADD       1             X
     C*
     C* LOAD ALL DETAIL EOB CODES INTO ARRAY 'EOB'.  THE HEADER EOB
     C* CODES WILL BE ADDED TO THIS ARRAY LATER.  THIS ARRAY IS USED
     C* TO DISPLAY THE DESCRIPTIONS OF ALL EOB CODES FOR THE BILL
     C* IN ONE LOCATION.
     C*
     C*** Write to Scriptura Detail EOB Code Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetCdWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     DEKY          READE     PDE01                                  98
     C                   ENDDO
     C*
     C* COUNT THE NUMBER OF NOTES; DETERMINE IF HAVE ENOUGH SPACE
     C* TO PRINT THE DETAIL LINE'S INFORMATION.  DO NOT SPLIT-UP.
     C*
     C                   Z-ADD     4             NUMLIN            3 0
     C     DEKY          CHAIN     PDE01                              98
     C     *IN98         DOWEQ     *OFF
     C     DEPRNT        IFNE      'N'
     C     DENOT1        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF                                                  DENOT1 = *BLANKS
     C     DENOT2        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF                                                  DENOT2 = *BLANKS
     C                   ENDIF
     C     DEKY          READE     PDE01                                  98
     C                   ENDDO
     C*
     C* SET INDICATORS TO PRINT EOB CODE/NOTE TITLES
     C*
     C     X1EOB         IFNE      *BLANKS
     C     X2EOB         ORNE      *BLANKS
     C     X3EOB         ORNE      *BLANKS
     C     X4EOB         ORNE      *BLANKS
     C                   SETON                                        48
     C                   ELSE
     C                   SETOFF                                       48
     C                   ENDIF                                                  X1EOB NE BLANKS
     C*
     C     NUMLIN        IFGT      4
     C                   SETON                                        46
     C                   ELSE
     C                   SETOFF                                       46
     C                   ENDIF                                                  NUMLIN IFGT 0
     C     NUMLIN        ADD       CURLIN        LINEED            3 0
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG13
     C                   If        isUseSpoolout
     C                   WRITE     DETL12
     C                   Endif
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL1
     C                   Endif
     C*
     C* GET ALL EOB DETAIL NOTES
     C*
     C     DEKY          CHAIN     PDE01                              98
     C                   SETON                                        99
     C     *IN98         DOWEQ     *OFF
     C     DEPRNT        IFNE      'N'
     C                   SETOFF                                       99
     C                   EXSR      NWPG13
     C*
     C* GET THE X-REF FOR FLORIDA BILLS
     C*
     C     EOAST         IFEQ      'FL'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C**** Beg Del *** 07/18/23 **************************************
     C****               CALL      'FA741R'
     C****               PARM      KKCO#         $$CO#
     C****               PARM      DEEOB         $$EOB
     C****               PARM      *BLANKS       $$XREF
     C****               PARM      *BLANKS       $$DESC
     C**** End Del *** 07/18/23 **************************************
     C*
     C     $$DESC        IFNE      *BLANKS
     C                   MOVEL(P)  $$DESC        DENOT1
     C                   MOVEL(P)  *BLANKS       DENOT2
     C                   ENDIF
     C                   ENDIF
     C     DENOT1        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT1        XNOTE            70
     C                   If        isUseSpoolout
     C                   WRITE     DETL2
     C                   Endif
     C                   ENDIF                                                  DENOT1 NE *BLANKS
     C     DENOT2        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT2        XNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL2
     C                   Endif
     C                   ENDIF                                                  DENOT2 NE *BLANKS
     C                   ENDIF
     C     DEKY          READE     PDE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C                   If        isUseSpoolout
     C   99              WRITE     SPACE
     C                   Endif
     C                   SETOFF                                       99
     C*
     C*** Write to Scriptura Detail Charges Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDtWrk
     C                   Endif
     C*
     C     CAKY          READE     PCA01                                  98
     C                   ENDDO                                                  *IN9= *OFF
     C*
     C                   Z-ADD     EOMRA         XXMRA             9 2
     C                   Z-ADD     EOOTH2        XXOTH2            9 2
     C                   Z-ADD     EOPAID        XXPAID            9 2
     C                   Z-ADD     EOPAID        XXPAI2            9 2
     C*
     C* CALCULATE 'PREVIOUSLY PAID' AND 'THIS PAYMENT' IF RECON NUMBER
     C* IS GREATER THAN ZERO
     C*
     C                   SETOFF                                       33
     C     EORCN         IFGT      0
     C                   SETON                                        33
     C*
     C                   Z-ADD     0             X1PAID            9 2          PREV PAID
     C                   Z-ADD     0             X2PAID            9 2          THIS PAYMENT
     C     EAKY1         CHAIN     PEA01                              98
     C     *IN98         DOWEQ     *OFF
     C     EARCN         IFLT      EORCN
     C                   ADD       EAPAID        X1PAID
     C                   ENDIF                                                  EARCN LT EORCN
     C     EAKY1         READE     PEA01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     EAKY2         CHAIN     PEA01                              98
     C     *IN98         DOWEQ     *OFF
     C                   ADD       EAPAID        X2PAID
     C     EAKY2         READE     PEA01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C                   ENDIF                                                  EORCN GT 0
     C*
     C* DETERMINE IF ENOUGH LINES ARE AVALAIBLE TO PRINT TOTALS
     C*
     C     *IN33         IFEQ      *ON
     C     CURLIN        ADD       4             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   ENDIF                                                  *IN33 = *ON
     C*
     C     *IN33         IFEQ      *OFF
     C     CURLIN        ADD       2             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   ENDIF                                                  *IN33 = *OFF
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL3
     C                   Endif
     C*
     C*
     C* PRINT HOSPITAL INFO
     C*
     C     *IN40         IFEQ      *ON
     C*
     C* NEED FOUR LINES TO PRINT HOSPITAL INFORMATION
     C*
     C     CURLIN        ADD       5             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL11
     C                   Endif
     C                   ENDIF                                                  *IN40 = *ON
     C*
     C* LOAD HEADER EOB CODES TO ARRAY 'EOB'
     C*
     C     EEKY          CHAIN     PEE01                              98
     C*
     C* PRINT 'EOB REASON CODES' HEADING IF ANY FOUND
     C*
     C*
     C     *IN98         DOWEQ     *OFF
     C     EEPRNT        IFNE      'N'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** EEEOB         IFNE      *BLANKS
     C     EEEOBCD       IFNE      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C                   Z-ADD     1             Y                 2 0
     C**** Beg Chg *** 07/18/23 **************************************
     C**** EEEOB         LOOKUP    EOB(Y)                                 41
     C     EEEOBCD       LOOKUP    EOB(Y)                                 41
     C**** End Chg *** 07/18/23 **************************************
     C     *IN41         IFEQ      *OFF                                         not in array
     C                   Z-ADD     1             Z                 2 0
     C     *BLANKS       LOOKUP    EOB(Z)                                 31
     C     *IN31         IFEQ      *ON
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      EEEOB         EOB(Z)
     C                   MOVE      EEEOBCD       EOB(Z)
     C**** End Chg *** 07/18/23 **************************************
     C                   ENDIF                                                  *IN31 = *ON
     C                   ENDIF                                                  *IN41 = *OFF
     C                   ENDIF                                                  EEEOB NE *BLANKS
     C                   ENDIF
     C     EEKY          READE     PEE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C* DISPLAY THE EOB CODES AND DESCRIPTIONS
     C*
     C     EOB(1)        IFNE      *BLANKS
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL4
     C                   Endif
     C                   SORTA     EOB
     C                   Z-ADD     1             T                 2 0
     C                   DO        20            T
     C     EOB(T)        IFNE      *BLANKS
     C                   MOVEL(P)  EOB(T)        YYEOB
     C*
     C* GET THE X-REF FOR TEXAS BILLS
     C*
     C     EOAST         IFEQ      'TX'
     C**** Beg Del *** 07/18/23 **************************************
     C****               CALL      'FA738R'
     C****               PARM      KKCO#         $$CO#             3 0
     C****               PARM      EOB(T)        $$EOB            10
     C****               PARM      *BLANKS       $$XREF            3
     C**** End Del *** 07/18/23 **************************************
     C*
     C     $$XREF        IFNE      *BLANKS
     C                   MOVEL(P)  $$XREF        YYEOB
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL(P)  EOB(T)        KKEOBC
     C     EEKY1         CHAIN     PEE02                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  EENOT1        YYDESC           70
     C                   ELSE
     C                   MOVE      *BLANKS       YYDESC
     C                   ENDIF                                                  *IN98 = *OFF
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETL5
     C                   Endif
     C                   ENDIF                                                  EOB,T = *BLANKS
     C                   ENDDO
     C*
     C                   ENDIF                                                  EOB NE *BLANKS
     C*
     C* GET/DISPLAY HEADER NOTES
     C*
     C                   CLEAR                   XPFLAG
     C     EEKY          CHAIN     PEE01                              98
     C*
     C* DISPLAY 'ADDITIONAL EOMB NOTES' HEADER IS NECESSARY
     C*
     C*
     C     *IN98         DOWEQ     *OFF
     C     EEPRNT        IFNE      'N'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** EEEOB         IFEQ      *BLANKS
     C     EEEOBCD       IFEQ      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C     EENOT1        IFNE      *BLANKS
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETL6
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C                   MOVEL(P)  EENOT1        X1NOTE           70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETL7
     C                   Endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfil
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT1 NE *BLANKS
     C     EENOT2        IFNE      *BLANKS
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETL6
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C                   MOVEL(P)  EENOT2        X1NOTE
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETL7
     C                   Endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfil
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT2 NE *BLANKS
     C                   ENDIF                                                  EEEOB = *BLANKS
     C                   ENDIF
     C     EEKY          READE     PEE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C* GET COMPANY INFORMATION - BASED ON ALTERNATE ADDRESS FLAG
     C*
     C     F1KY          CHAIN     PF101                              98
     C     *IN98         IFEQ      *OFF
     C     F1FLG1        IFEQ      'Y'
     C                   MOVEL(P)  F1ANME        XXANME
     C                   MOVEL(P)  XXANME        XXCONM           40
     C                   MOVEL(P)  F1AAD1        XXAAD1
     C     F1AAD2        IFNE      *BLANKS
     C                   MOVEL(P)  F1AAD2        XXAAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF                                                  F1AAD2 NE *BLANKS
     C                   MOVEL(P)  F1ACTY        XXCTY
     C                   MOVE      F1AST         XXSTAT
     C                   MOVE      F1AZIP        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2           40
     C                   MOVEL     F1AARA        XXAREA
     C                   MOVEL     F1APHN        XXPHN1            3
     C                   MOVE      F1APHN        XXPHN2            4
     C     *IN52         IFEQ      *ON
     C                   MOVEL     SVFARA        XXFARA
     C                   MOVEL     SVFPH1        XXFPH1            3
     C                   MOVE      SVFPH2        XXFPH2            4
     C                   ENDIF
     C                   MOVEL(P)  XXCSZ2        X1CSZ2
     C                   MOVEL     XXAREA        X1AREA
     C                   MOVEL     XXPHN1        X1PHN1
     C                   MOVE      XXPHN2        X1PHN2
     C                   ENDIF                                                  FLFLG1 = 'Y'
     C     F1FLG1        IFEQ      'N'
     C     C1KY          CHAIN     PC101                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  C1NAME        XXANME
     C                   MOVEL(P)  C1NAME        XXCONM
     C                   MOVEL(P)  C1ADR1        XXAAD1
     C     C1ADR2        IFNE      *BLANKS
     C                   MOVEL(P)  C1ADR2        XXAAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF
     C                   MOVEL(P)  C1CTY         XXCTY
     C                   MOVEL     C1ST          XXSTAT
     C                   MOVE      C1ZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2
     C                   MOVE      C1AREA        XXAREA
     C                   MOVEL     C1PHN#        XXPHN1
     C                   MOVE      C1PHN#        XXPHN2
     C                   MOVEL(P)  XXCSZ2        X1CSZ2
     C                   MOVEL     XXAREA        X1AREA
     C                   MOVEL     XXPHN1        X1PHN1
     C                   MOVE      XXPHN2        X1PHN2
     C     *IN52         IFEQ      *ON
     C                   MOVE      C1FARA        XXFARA
     C                   MOVEL     C1FAX#        XXFPH1
     C                   MOVE      C1FAX#        XXFPH2
     C                   ENDIF
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  F1FLG1 = 'N'
     C*
     C* START NEW PAGE IF CANNOT PRINT ENTIRE ADDRESS ON ONE PAGE
     C*
     C     CURLIN        ADD       5             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL10
     C*
     C* GET STATE COMMENTS
     C*
     C                   WRITE     SPACE
     C                   Endif
     C                   Z-ADD     EOFRDT        KKEFDT
     C     F4KY2         SETGT     PF401
     C     F4KY1         READPE    PF401                                  98
     C*
     C* SKIP INACTIVE COMMENTS
     C*
     C*
     C     *IN98         DOWEQ     *OFF
     C     F4STAT        IFNE      'I'
     C                   Z-ADD     0             NUMLIN
     C*
     C* LOAD VALUES FOR LINES OF STATE COMMENTS
     C*
     C                   EXSR      FILLS
     C*
     C* DETERMINE THE NUMBER OF LINES THAT WILL BE NEEDED
     C*
     C                   EXSR      NMBRLN
     C*
     C*
     C     NUMLIN        ADD       CURLIN        LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     SPACE
     C                   Endif
     C                   ENDIF                                                  LINEED GT 58
     C*
     C*
     C* CHANGE FONT TO SMALLER PRINT IF NEED TO PRINT STATE COMMENTS
     C*
     C*
     C*
     C* PRINT LINES FOR COMMENTS
     C*
     C                   EXSR      PRTSC
     C*
     C                   ENDIF                                                  STAT <> 'I'
     C*
     C     F4KY1         READPE    PF401                                  98
     C*
     C     *IN98         IFEQ      *OFF
     C                   If        isUseSpoolout
     C                   WRITE     SPACE
     C                   Endif
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* GET GROUP COMMENTS BASED ON FROM SERVICE DATE IN FAEOP
     C*
     C                   If        isUseSpoolout
     C                   WRITE     SPACE
     C                   Endif
     C     F3KY2         SETGT     PF301
     C     F3KY1         READPE    PF301                                  98
     C*
     C* SKIP INACTIVE COMMENTS
     C*
     C     *IN98         IFEQ      *OFF
     C     F3STAT        ANDEQ     'I'
     C*
     C     *IN98         DOWEQ     *OFF
     C     F3STAT        ANDEQ     'I'
     C     F3KY1         READPE    PF301                                  98
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     0             NUMLIN
     C*
     C* FILL LINE VALUES FOR GROUP COMMENTS
     C*
     C                   EXSR      FILLG
     C*
     C* DETERMINE THE NUMBER OF LINES FOR GROUP
     C*
     C                   EXSR      NMBRLN
     C*
     C     NUMLIN        ADD       CURLIN        LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   EXSR      PRTGC
     C*
     C* DETERMINE THE NUMBER OF LINES FOR GROUP
     C*
     C                   CLEAR                   GRCOMM
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C*** Write to Scriptura EOB Totals and Workfiles
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEOBTotWrk
     C                   Exsr      wrtFtWrk
     C                   Endif
     C*
     C* Additional information requested by Michigan
     C*
     C     EOAST         IFEQ      'MI'
     C                   EXSR      MIINFO
     C                   ENDIF
     C     ##CLT#        IFEQ      '133'
     C     EOAST         ANDEQ     'LA'
     C     EOBTYP        ANDEQ     'X'
     C                   EXSR      LAINFO
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* NWPG1  - Start new page if overflow indicator is on
     C*================================================================
     C     NWPG1         BEGSR
     C*
     C     *IN01         IFEQ      *ON
     C*
    C*
     C     BARCDF        IFEQ      '0'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE      0             *IN01
     C                   If        isUseSpoolout
     C                   WRITE     BARCD1
     C                   Endif
     C                   MOVE      1             *IN01
     C                   MOVE      '1'           BARCDF
     C                   ENDIF
    C*
     C                   EXSR      MSKSS#
     C                   If        isUseSpoolout
     C                   WRITE     HEAD1
     C                   Endif
     C                   MOVE      0             *IN01
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* NWPG13 - Start new page if overflow indicator is on
     C*================================================================
     C     NWPG13        BEGSR
     C*
     C*
     C     *IN01         IFEQ      *ON
     C*
     C*
    C*
     C     BARCDF        IFEQ      '0'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE      0             *IN01
     C                   If        isUseSpoolout
     C                   WRITE     BARCD1
     C                   Endif
     C                   MOVE      1             *IN01
     C                   MOVE      '1'           BARCDF
     C                   ENDIF
    C*
     C                   EXSR      MSKSS#
     C                   If        isUseSpoolout
     C                   WRITE     HEAD1
     C                   WRITE     HEAD3
     C                   Endif
     C                   MOVE      0             *IN01
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* SUBROUTINE TO LOAD THE CITY STATE & ZIP
     C*================================================================
     C     SRADDR        BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C                   ENDSR
     C*
     C*----------------------------------------------------------------
     C*  SRPSUM - SUMMARY REPORT SUBROUTINE
     C*----------------------------------------------------------------
     C     SRPSUM        BEGSR
     C*
     C*
     C* GET SUBSCRIBER NAME
     C*
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *BLANKS       XXSBNM           40
     C                   ELSE
     C                   MOVEL(P)  SMNAME        XXSBNM           40
     C                   ENDIF                                                  *IN99 = *OFF
     C* GET FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FNNAME        X1NAME
     C                   ELSE
     C                   CLEAR                   X1NAME
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* GET REDUCTIONS FIELD NAMES
     C*
     C     CXKY          CHAIN     PCX01                              98
     C                   CLEAR                   XXHMRA
     C                   CLEAR                   XXHOT1
     C                   CLEAR                   XXHOT2
     C                   CLEAR                   XXHNET
     C                   MOVEL     CXHMRA        XXHMRA           10
     C                   MOVEL     CXHOT1        XXHOT1           10
     C                   MOVEL     CXHOT2        XXHOT2           10
     C                   MOVEL     CXHNET        XXHNET           10
     C*
     C* PRINT ROUTING PAGE ONCE
     C*
     C     RTPTB         IFNE      'Y'
     C                   MOVE      '1'           KKFIL#
     C     ORKY          CHAIN     POR01                              98
     C                   MOVEL(P)  ORTITL        X2TITL           60
     C*
     C                   If        isUseSpoolout
     C                   Eval      Flag01 = '2'
     C                   WRITE     RPHD1
     C                   WRITE     RPHD2
     C                   Endif
     C*
     C*** Write to Scriptura EOB Group Detail Headings Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEOBRhWrk
     C                   Endif
     C                   MOVE      EOSUB#        HLSUB#
     C                   MOVE      EOFND         HLFND
     C                   MOVE      EAPOST        HLPOST
     C                   MOVE      'Y'           RTPTB             1
     C                   ENDIF                                                  RTPTB NE 'Y'
     C*
     C* CHECK FOR SUBSCRIBER OR GROUP CHANGE. IF THERE IS A CHANGE,
     C* PRINT GROUP TOTALS AND START NEW PAGE FOR NEXT GROUP
     C*
     C     *IN18         IFEQ      *OFF
     C     EOSUB#        IFNE      HLSUB#
     C     EOFND         ORNE      HLFND
     C                   EXSR      OVFLOW
     C                   If        isUseSpoolout
     C                   WRITE     RPUTOT
     C                   Endif
     C* Write to the Scriptura Group Totals Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGpTotWrk
     C                   Endif
     C                   CLEAR                   XGEOB#
     C                   CLEAR                   XGCHRG
     C                   CLEAR                   XGMRA
     C                   CLEAR                   XGOTHR
     C                   CLEAR                   XGOTH2
     C   66              CLEAR                   XGNDIS
     C                   CLEAR                   XGPAID
     C                   CLEAR                   XGPAI2
     C                   SETON                                        OB
     C                   If        isUseSpoolout
     C                   WRITE     RPHD1
     C                   WRITE     RPHD2
     C                   Endif
     C*
     C*** Write to Scriptura EOB Group Detail Headings Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEOBRhWrk
     C                   Endif
     C                   Z-ADD     EOSUB#        HLSUB#
     C                   Z-ADD     EOFND         HLFND
     C                   ENDIF                                                  EOSUB# NE HLSUB#
     C                   ELSE
     C     EAPOST        IFNE      HLPOST
     C                   EXSR      OVFLOW
     C                   If        isUseSpoolout
     C                   WRITE     RPUTOT
     C                   Endif
     C                   CLEAR                   XSEOB#
     C                   CLEAR                   XSCHRG
     C                   CLEAR                   XSMRA
     C                   CLEAR                   XSOTHR
     C                   CLEAR                   XSOTH2
     C   66              CLEAR                   XSNDIS
     C                   CLEAR                   XSPAID
     C                   CLEAR                   XSPAI2
     C                   SETON                                        OB
     C                   If        isUseSpoolout
     C                   WRITE     RPHD1
     C                   WRITE     RPHD2
     C                   Endif
     C*
     C*** Write to Scriptura EOB Group Detail Headings Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEOBRhWrk
     C                   Endif
     C                   MOVEL     EAPOST        HLPOST
     C                   ENDIF                                                  EAPOST NE HLPOST
     C                   ENDIF
     C*
     C*
     C                   MOVE      EACO#         XXCO#             3
     C                   MOVE      EASUB#        XXSUB#
     C                   MOVE      '-'           XDASH1
     C                   MOVE      EAFND         XXFND
     C                   MOVE      '-'           XDASH2
     C                   MOVE      EAFYR         XXFYR
     C                   MOVE      '-'           XDASH3
     C                   MOVE      EACASE        XXCASE
     C                   MOVE      EAEOB#        XXEOB#            9 0
     C                   eval      xxSS# = xxSS#1 + '-' + xxSS#2 +
     C                              '-' + xxSS#3
     C                   Z-ADD     EOCHRG        XXCHRG            9 2
     C                   Z-ADD     EOMRA         XXMRA             9 2
     C                   Z-ADD     EOOTHR        XXOTHR            9 2
     C                   Z-ADD     EOOTH2        XXOTH2            9 2
     C                   Z-ADD     EONDIS        XXNDIS            9 2
     C                   Z-ADD     EOPAID        XXPAID            9 2
     C                   Z-ADD     EOPAID        XXPAI2
     C* FORMAT DATE
     C                   Z-ADD     EAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT
     C* GET CLAIMANT NAME
     C                   MOVEL     EOLSN         XXLAST           56
     C                   MOVEL     EOFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XZPNAM           22
     C*
     C* ACCUMULTE TOTALS (BY GROUP AND OVERALL)
     C*
     C                   ADD       1             XGEOB#            4 0
     C                   ADD       XXCHRG        XGCHRG           10 2
     C                   ADD       XXMRA         XGMRA            10 2
     C                   ADD       XXOTHR        XGOTHR           10 2
     C                   ADD       XXOTH2        XGOTH2           10 2
     C                   ADD       XXNDIS        XGNDIS           10 2
     C                   ADD       XXPAID        XGPAID           10 2
     C                   Z-ADD     XGPAID        XGPAI2           10 2
     C                   ADD       1             XSEOB#            4 0
     C                   ADD       XXCHRG        XSCHRG           10 2
     C                   ADD       XXMRA         XSMRA            10 2
     C                   ADD       XXOTHR        XSOTHR           10 2
     C                   ADD       XXOTH2        XSOTH2           10 2
     C                   ADD       XXNDIS        XSNDIS           10 2
     C                   ADD       XXPAID        XSPAID           10 2
     C                   Z-ADD     XSPAID        XSPAI2           10 2
     C                   ADD       1             XTEOB#            4 0
     C                   ADD       XXCHRG        XTCHRG           10 2
     C                   ADD       XXMRA         XTMRA            10 2
     C                   ADD       XXOTHR        XTOTHR           10 2
     C                   ADD       XXOTH2        XTOTH2           10 2
     C                   ADD       XXNDIS        XTNDIS           10 2
     C                   ADD       XXPAID        XTPAID           10 2
     C* WRITE DETAIL
     C                   EXSR      OVFLOW
     C                   If        isUseSpoolout
     C                   WRITE     RPDETL
     C                   Endif
     C*
     C*** Write to Scriptura EOB Group Detail Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpDetWrk
     C                   Endif
     C*
     C                   ENDSR
     C*----------------------------------------------------------------
     C*   OVFLOW - OVERFLOW SUBROUTINE FOR SUMMARY REPORT
     C*----------------------------------------------------------------
     C     OVFLOW        BEGSR
     C*
     C     *IN02         IFEQ      *ON
     C                   If        isUseSpoolout
     C                   WRITE     RPHD1
     C                   WRITE     RPHD2
     C                   Endif
     C*
     C*** Write to Scriptura EOB Group Detail Headings Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEOBRhWrk
     C                   Endif
     C                   MOVE      '0'           *IN02
     C                   ENDIF                                                  *IN02 = *ON
     C*
     C                   ENDSR
     C* FILLG - FILL THE GROUP COMMENTS
     C     FILLG         BEGSR
     C                   MOVEL(P)  F3EC01        XXPRTA
     C                   MOVEL(P)  F3EC02        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN01          105
     C                   MOVE(P)   F3EC02        XXPRTC
     C                   MOVEL(P)  F3EC03        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN02          105
     C                   MOVEL(P)  F3EC04        XXPRTA
     C                   MOVEL(P)  F3EC05        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN03          105
     C                   MOVE(P)   F3EC05        XXPRTC
     C                   MOVEL(P)  F3EC06        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN04          105
     C                   MOVEL(P)  F3EC07        XXPRTA
     C                   MOVEL(P)  F3EC08        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN05          105
     C                   MOVE(P)   F3EC08        XXPRTC
     C                   MOVEL(P)  F3EC09        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN06          105
     C                   MOVEL(P)  F3EC10        XXPRTA
     C                   MOVEL(P)  F3EC11        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN07          105
     C                   MOVE(P)   F3EC11        XXPRTC
     C                   MOVEL(P)  F3EC12        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN08          105
     C                   MOVEL(P)  F3EC13        XXPRTA
     C                   MOVEL(P)  F3EC14        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN09          105
     C                   MOVE(P)   F3EC14        XXPRTC
     C                   MOVEL(P)  F3EC15        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN10          105
     C                   MOVEL(P)  F3EC16        XXPRTA
     C                   MOVEL(P)  F3EC17        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN11          105
     C                   MOVE(P)   F3EC17        XXPRTC
     C                   MOVEL(P)  F3EC18        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN12          105
     C                   MOVEL(P)  F3EC19        XXPRTA
     C                   MOVEL(P)  F3EC20        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN13          105
     C                   MOVE(P)   F3EC20        XXPRTC
     C                   MOVEL(P)  F3EC21        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN14          105
     C                   MOVEL(P)  F3EC22        XXPRTA
     C                   MOVEL(P)  F3EC23        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN15          105
     C                   MOVE(P)   F3EC23        XXPRTC
     C                   MOVEL(P)  F3EC24        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN16          105
     C                   MOVEL(P)  F3EC25        XXPRTA
     C                   MOVEL(P)  F3EC26        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN17          105
     C                   MOVE(P)   F3EC26        XXPRTC
     C                   MOVEL(P)  F3EC27        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN18          105
     C                   MOVEL(P)  F3EC28        XXPRTA
     C                   MOVEL(P)  F3EC29        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN19          105
     C                   MOVE(P)   F3EC29        XXPRTC
     C                   MOVEL(P)  F3EC30        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN20          105
     C                   ENDSR
     C* FILLS - FILL THE STATE COMMENTS
     C     FILLS         BEGSR
     C*
     C                   MOVEL(P)  F4EC01        XXPRTA
     C                   MOVEL(P)  F4EC02        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN01
     C                   MOVE(P)   F4EC02        XXPRTC
     C                   MOVEL(P)  F4EC03        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN02
     C                   MOVEL(P)  F4EC04        XXPRTA
     C                   MOVEL(P)  F4EC05        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN03
     C                   MOVE(P)   F4EC05        XXPRTC
     C                   MOVEL(P)  F4EC06        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN04
     C                   MOVEL(P)  F4EC07        XXPRTA
     C                   MOVEL(P)  F4EC08        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN05
     C                   MOVE(P)   F4EC08        XXPRTC
     C                   MOVEL(P)  F4EC09        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN06
     C                   MOVEL(P)  F4EC10        XXPRTA
     C                   MOVEL(P)  F4EC11        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN07
     C                   MOVE(P)   F4EC11        XXPRTC
     C                   MOVEL(P)  F4EC12        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN08
     C                   MOVEL(P)  F4EC13        XXPRTA
     C                   MOVEL(P)  F4EC14        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN09
     C                   MOVE(P)   F4EC14        XXPRTC
     C                   MOVEL(P)  F4EC15        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN10
     C                   MOVEL(P)  F4EC16        XXPRTA
     C                   MOVEL(P)  F4EC17        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN11
     C                   MOVE(P)   F4EC17        XXPRTC
     C                   MOVEL(P)  F4EC18        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN12
     C                   MOVEL(P)  F4EC19        XXPRTA
     C                   MOVEL(P)  F4EC20        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN13
     C                   MOVE(P)   F4EC20        XXPRTC
     C                   MOVEL(P)  F4EC21        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN14
     C                   MOVEL(P)  F4EC22        XXPRTA
     C                   MOVEL(P)  F4EC23        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN15
     C                   MOVE(P)   F4EC23        XXPRTC
     C                   MOVEL(P)  F4EC24        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN16
     C                   MOVEL(P)  F4EC25        XXPRTA
     C                   MOVEL(P)  F4EC26        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN17
     C                   MOVE(P)   F4EC26        XXPRTC
     C                   MOVEL(P)  F4EC27        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN18
     C                   MOVEL(P)  F4EC28        XXPRTA
     C                   MOVEL(P)  F4EC29        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN19
     C                   MOVE(P)   F4EC29        XXPRTC
     C                   MOVEL(P)  F4EC30        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN20
     C                   ENDSR
     C*
     C*====================================================================
     C* Mask the SS# to be printed
     C*====================================================================
     C     MSKSS#        BEGSR
     C*
     C                   MOVEL     '***'         XXSS#1
     C                   MOVEL     '**'          XXSS#2
     C                   ENDSR
     C* NMBRLN - DETERMINES NUMBER OF LINES IN GROUP AND STATE COMMENTS
     C     NMBRLN        BEGSR
     C*
     C     XXLN01        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN02        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN03        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN04        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN05        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN06        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN07        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN08        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN09        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN10        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN11        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN12        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN13        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN14        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN15        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN16        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN17        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN18        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN19        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C     XXLN20        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF
     C*
     C                   ENDSR
     C* PRTGC - PRINT GROUP COMMENTS
     C     PRTGC         BEGSR
     C*
     C     XXLN01        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN01        GPNOTE          105
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN02        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN02        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN03        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN03        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN04        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN04        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN05        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN05        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN06        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN06        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C     XXLN07        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN07        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN08        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN08        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN09        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN09        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN10        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN10        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN11        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN11        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN12        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN12        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN13        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN13        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C     XXLN14        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN14        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN15        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN15        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN16        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN16        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN17        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN17        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN18        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN18        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN19        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN19        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN20        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN20        GPNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL9
     C                   Endif
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C* PRTSC - PRINT STATE COMMENTS
     C     PRTSC         BEGSR
     C*
     C                   MOVE      *OFF          *IN73
     C     F4BLD1        IFEQ      'Y'
     C                   MOVE      *ON           *IN73
     C                   ENDIF
     C*
     C     XXLN01        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN01        STNOTE          105
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C*
     C                   ENDIF
     C*
     C     XXLN02        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN02        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN03        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN03        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN04        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN04        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN05        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN05        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN06        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN06        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN07        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN07        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN08        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN08        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN09        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN09        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN10        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN10        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN11        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN11        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN12        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN12        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN13        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN13        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN14        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN14        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN15        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN15        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN16        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN16        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN17        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN17        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN18        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN18        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN19        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN19        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     XXLN20        IFNE      *BLANKS
     C                   MOVEL(P)  XXLN20        STNOTE
     C                   If        isUseSpoolout
     C                   WRITE     DETL8
     C                   Endif
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C* MIINFO - ADDITIONAL INFORMATION
     C     MIINFO        BEGSR
     C*
     C* Header information
     C*
     C* Print
     C                   If        isUseSpoolout
     C                   WRITE     ADINH1
     C                   Endif
     C*
     C* Carrier, Address and Phone
     C*
     C                   CLEAR                   XXANME
     C                   CLEAR                   XXAAD1
     C                   CLEAR                   XXAAD2
     C                   CLEAR                   XXCTY
     C                   CLEAR                   XXSTAT
     C                   CLEAR                   XXZIP
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXCSZ2
     C                   CLEAR                   X1CSZ2
     C                   CLEAR                   X1AREA
     C                   CLEAR                   X1PHN1
     C                   CLEAR                   X1PHN2
     C                   CLEAR                   XXAREA
     C                   CLEAR                   XXPHN1
     C                   CLEAR                   XXPHN2
     C                   SETOFF                                       31
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        XXANME
     C                   MOVEL(P)  COMAD1        XXAAD1
     C*
     C     COMAD2        IFNE      *BLANKS
     C                   MOVEL(P)  COMAD2        XXAAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF                                                  COMAD2 IFNE *BLANK
     C*
     C                   MOVEL(P)  COMCTY        XXCTY
     C                   MOVEL     COMST         XXSTAT
     C                   MOVE      COMZIP        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2
     C                   MOVE      COAREA        XXAREA
     C                   MOVEL     COPHN#        XXPHN1
     C                   MOVE      COPHN#        XXPHN2
     C                   MOVEL(P)  XXCSZ2        X1CSZ2
     C                   MOVEL     XXAREA        X1AREA
     C                   MOVEL     XXPHN1        X1PHN1
     C                   MOVE      XXPHN2        X1PHN2
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* Provider bill date
     C*
     C                   CLEAR                   XXCSDT
     C*
     C                   Z-ADD     EOCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCSDT            8 0
     C*
     C* Print
     C                   If        isUseSpoolout
     C                   WRITE     ADIND1
     C                   Endif
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C* NAICS/Self Insured
     C*
     C                   MOVE      *BLANKS       XXNAIC            9
     C                   MOVE      *BLANKS       GCDCAR
     C*
     C     GCKY          CHAIN     PGC01                              90
     C                   SELECT
     C     GCDCAR        WHENEQ    'PPC'
     C                   MOVEL     287113499     XXNAIC
     C     GCDCAR        WHENEQ    'ICI'
     C                   MOVEL     114873499     XXNAIC
     C                   ENDSL
     C*
     C* Print
     C                   If        isUseSpoolout
     C                   WRITE     ADIND2
     C                   Endif
     C                   ENDIF
     C*
     C* Service company
     C*
     C                   CLEAR                   XXANME
     C                   CLEAR                   XXAAD1
     C                   CLEAR                   XXAAD2
     C                   CLEAR                   XXCTY
     C                   CLEAR                   XXST
     C                   CLEAR                   XXZIP
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXCSZ2
     C                   CLEAR                   X1CSZ2
     C                   CLEAR                   X1AREA
     C                   CLEAR                   X1PHN1
     C                   CLEAR                   X1PHN2
     C                   SETOFF                                       31
     C* Print
     C                   If        isUseSpoolout
     C                   WRITE     ADIND3
     C                   Endif
     C*
     C* Employee Name and Address
     C*
     C                   CLEAR                   QQLAST
     C                   CLEAR                   QQFRST
     C                   CLEAR                   XXCLNM
     C                   CLEAR                   XXAAD1
     C                   CLEAR                   XXAAD2
     C                   CLEAR                   XXCTY
     C                   CLEAR                   XXST
     C                   CLEAR                   XXZIP
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXCSZ2
     C                   CLEAR                   X1CSZ2
     C                   CLEAR                   X1AREA
     C                   CLEAR                   X1PHN1
     C                   CLEAR                   X1PHN2
     C                   SETOFF                                       31
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCLNM           30
     C                   MOVEL     CDADR1        XXAAD1
     C*
     C     CDADR2        IFNE      *BLANKS
     C                   MOVEL     CDADR2        XXAAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF                                                  CDADR2 IFNE *BLANKS
     C*
     C                   MOVEL(P)  CDCTY         XXCTY
     C                   MOVEL     CDST          XXST
     C                   MOVEL     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2
     C                   MOVEL(P)  XXCSZ2        X1CSZ2
     C*
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C* Print
     C                   If        isUseSpoolout
     C                   WRITE     ADIND4
     C                   endif
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*
     C*  LAINFO- Print LA Pharmacy Claimant Info
     C*
     C*================================================================
     C     LAINFO        BEGSR
     C*
     C**Header information
     C**Print
     C                   EXSR      NWPG1
     C*
     C                   CLEAR                   XXAAD1
     C                   CLEAR                   XXAAD2
     C                   CLEAR                   XXCTY
     C                   CLEAR                   XXSTAT
     C                   CLEAR                   XXZIP
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXCSZ2
     C                   SETOFF                                       82
     C*
     C     CDKY          CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CDADR1        XXAAD1
     C*
     C     CDADR2        IFNE      *BLANKS
     C                   MOVEL     CDADR2        XXAAD2
     C                   SETON                                        82
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       82
     C                   ENDIF                                                  CDADR2 IFNE *BLANKS
     C*
     C                   MOVEL(P)  CDCTY         XXCTY
     C                   MOVEL     CDST          XXSTAT
     C                   MOVEL     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2
     C*
     C                   If        isUseSpoolout
     C                   WRITE     ADDLAD1
     C                   Endif
     C                   ENDIF                                                  NO RECORD IN FMCDP
     C*
     C                   ENDSR
     C*================================================================
     C*
     C*  SrProvEft - Check to see if provider is an eft provider
     C*
     C*================================================================
     C     SrProvEft     BegSr
     C*
     C                   Eval      isEft = *Off
     C*
     C                   If        xxRrn1 = 0
     C                   Eval      kkCo# = EAco#
     C                   Eval      kkSub# = EAsub#
     C                   Eval      kkFnd = EAfnd
     C                   Eval      kkFyr = EAfyr
     C                   Eval      kkCase = EAcase
     C                   Eval      kkEob# = EAeob#
     C                   Eval      kkRcn  = EArcn
     C                   Else
     C                   Eval      kkCo# = EOco#
     C                   Eval      kkSub# = EOsub#
     C                   Eval      kkFnd = EOfnd
     C                   Eval      kkFyr = EOfyr
     C                   Eval      kkCase = EOcase
     C                   Eval      kkEob# = EOeob#
     C                   Eval      kkRcn  = EOrcn
     C     eaky2         Chain     pea01                              98
     C                   EndIf
     C*
     C                   move      EAco#         #1co#
     C                   move      EAsub#        #1sub#
     C                   move      EAfnd         #1fnd
    C*
     C                   call      'FA701R'
     C                   parm      *blanks       #1rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm      *blanks       #1outs
    C*
    C* Only Check EFT if Workers Comp is inst and linked case
    C*
B04  C                   if        ##wcin = 'Y' and #1rtcd = *blank and
     C                             #1outs <> 'Y' and ##npwc <> 'Y' and
     C                             ##wcif = 'Y'
     C*
     C                   Eval      #1key = %EditC(EAfein:'X') +
     C                                     %EditC(EAmod:'X')
     C*
     C                   Z-Add     dsDate        srDtI
     C                   Call      'SRCVTDAT'    dtCYMD
     C                   MoveL     srDtO         #1Date
     C*
     C                   call      'WA023R3'
     C                   parm                    #1co#
     C                   parm      'P'           #1type
     C                   parm                    #1fnd
     C                   parm                    #1key
     C                   parm                    #1date
     C                   parm      ' '           #1eft
     C                   parm      ' '           #1pre
     C                   parm      '000000000'   #1rrn
     C                   parm      '   '         #1eobType
     C                   If        #1eft = *On And #1pre = *Off And
     C                             #1eobType = 'PDF'

     C                   If        EApaid <> 0 Or (EApaid = 0 and ##inze = 'Y')
     C                   Eval      isEft = *on
     C                   EndIf

     C                   EndIf
     C                   EndIf
     C                   EndSr
     C*================================================================
     C*
     C*  SRPRTL- SUBROUTINE TO PRINT A SINGLE EOMB FOR 'FL'
     C*
     C*================================================================
     C     SRPRTL        BEGSR
     C*
     C*
     C                   MOVE      *OFF          *IN01
     C*
     C*
     C*
       //*** Beg Chg *** 08/22/22 **************************
     C****               MOVE      *ON           *IN53
        If EOast = 'FL';
           *in53 = *on;
        Else;
           *in53 = *off;
        Endif;
       //*** End Chg *** 08/22/22 **************************
     C*
     C* GET USER FOR ROUTING  (ONLY ONCE FOR BATCH)
     C*
     C     ROUTPT        IFNE      'Y'
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  Q1USER        KKUSER
     C                   ELSE
     C                   MOVEL     EOIUSR        KKUSER
     C                   ENDIF                                                  @@MODE = 'P'
     C     UPKY          CHAIN     PUP01                              98
     C     *IN98         IFEQ      *ON
     C                   MOVE      *BLANKS       XX13A            13
     C                   MOVEL     'FEE SCED'    XX13A
     C                   MOVE      'USER'        XX13A
     C                   MOVE      *BLANKS       UPUSNM
     C                   MOVEL     XX13A         UPUSNM
     C                   ENDIF                                                  *IN98 = *ON
     C*
     C* GET OUTPUT ROUTING INFORMATION
     C*
     C                   MOVE      '0'           KKFIL#
     C     ORKY          CHAIN     POR01                              98
     C*
     C     @@MODE        IFEQ      'P'
     C                   MOVEL(P)  @@OUTQ        OROUTQ
     C                   MOVEL(P)  @@HOLD        ORHOLD
     C                   ENDIF                                                  @@MODE = *OFF
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
      /Free
       If xxRrn1 = 0;
      /End-free
     C                   If        isUseSpoolout
     C                   Endif
     C*
     C*** Write to Scriptura EOB Routing Workfiles
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtRtWrk
     C                   Endif
      /Free
       Endif;
      /End-free
     C     @@MODE        IFEQ      'P'
     C                   SETON                                        74        RESET PAGE NUMBER
     C                   ENDIF                                                  MODE
     C*
     C                   MOVE      'Y'           ROUTPT
     C                   ENDIF                                                  ROUTPT NE 'Y'
     C*
     C* CHECK TO SEE IF GROUP 440
     C*
     C     EOFND         IFEQ      440
     C     ##CLT#        ANDEQ     '114'
     C     EOFND         OREQ      460
     C     ##CLT#        ANDEQ     '114'
     C     EOFND         OREQ      600
     C                   SETON                                        16
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF
     C*
     C* Load Check Heading Fields
     C*
     C                   ExSr      AssgnChkHdrs
     C*
     C* SET KEY FIELDS
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   MOVE      EOAST         KKST
     C                   MOVEL     EOSINV        XXTRID           15
     C*
     C* GET STATE NAME
     C*
     C     *IN18         IFEQ      *ON
     C                   MOVEL(P)  EAPOST        XXKEY
     C     XXKEY         CHAIN     PST01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVE      STNAME        XXSTNM
     C                   ELSE
     C                   CLEAR                   XXSTNM
     C                   ENDIF                                                  *IN98 = OFF
     C                   ELSE
     C                   CLEAR                   XXSTNM
     C                   ENDIF                                                  *IN18 = ON
     C*
     C* SET INDICATORS FOR CONDITIONAL DISPLAY OF DATA
     C*
     C     EAKY2         CHAIN     PEA01                              98
     C     EPKY          CHAIN     PEP01                              99
     C     *IN98         IFEQ      *OFF
     C     *IN99         ANDEQ     *OFF
     C     EPTAX         IFEQ      'Y'
     C                   SETON                                        62
     C                   ENDIF
     C                   ENDIF
     C*
     C* IF BILL TYPE IS 'HOSPITAL', THEN DISPLAY 'TO SERVICE DATE'
     C*
     C     EOBTYP        IFEQ      'H'
     C                   SETON                                        40
     C                   ELSE
     C                   SETOFF                                       40
     C                   ENDIF                                                  EOBTYP = 'N'
     C*
     C* IF RECON, THEN DISPLAY 'PREVIOUSLY PAID' AND 'THIS PAYMENT'
     C* DATA
     C*
     C     EORCN         IFGT      *ZEROS
     C                   SETON                                        33
     C                   ELSE
     C                   SETOFF                                       33
     C                   ENDIF                                                  EORCN GT *ZEROS
     C*
     C* GET GROUP NAME
     C*
     C                   MOVE      *OFF          *IN52
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FNNAME        X1NAME
     C                   ELSE
     C                   CLEAR                   X1NAME
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* FORMAT LASTNAME, FIRSTNAME
     C*
     C                   MOVEL(P)  EOLSN         ##LAST           56
     C                   MOVEL(P)  EOFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL(P)  ##LAST        XXNAME           40
     C*
     C* GET EMPLOYER AND DIVISION FOR CLAIM MASTER FILE
     C*
     C     CMKY          CHAIN     PCM01                              97
     C     *IN97         IFEQ      *OFF
     C*
     C* Always print internal claim #, for CLT114
     C*
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C     EMKY          CHAIN     PEM01                              97
     C*
     C     *IN97         IFEQ      *OFF
     C                   MOVEL     EMNAME        XXENAM
     C                   MOVEL     EMNAME        XXENAM1
     C                   ELSE
     C                   CLEAR                   XXENAM
     C                   CLEAR                   XXENAM1
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C                   ELSE
     C                   SETOFF                                       17
     C*
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C* PUT ACCIDENT DATE IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C* PUT TRANSACTION DATE IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C*
     C* PUT DATE RECEIVED IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCRDT            8 0
     C*
     C* GET PROVIDER NAME, ADDRESS
     C*
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C*
     C     EOADR2        IFNE      *BLANKS
     C                   SETON                                        32
     C                   ELSE
     C                   SETOFF                                       32
     C                   ENDIF                                                  PDADR2 = *BLANKS
     C*
     C                   MOVEL(P)  EOCTY         XXCTY
     C                   MOVE      EOST          XXSTAT
     C                   MOVE      EOZIP         XXZIP
     C*
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ1           40
     C*
     C* GET LATEST RECON DATE
     C*
     C                   CLEAR                   XXRCDT
     C                   MOVE      *HIVAL        KKIDT
     C                   MOVE      *HIVAL        KKITIM
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  98
     C     *IN98         DOWEQ     *OFF
     C*
     C     ESBLST        IFEQ      'R'
     C                   LEAVE
     C                   ENDIF                                                  ESBLST = 'R'
     C*
     C     ESKY2         READPE    PES01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     ESBLST        IFEQ      'R'
     C     EORCN         ANDGT     *ZEROS
     C                   Z-ADD     ESIDT         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCDT            8 0
     C                   ENDIF                                                  ESBLST = 'R'
     C*
     C* GET CHECK DATE AND NUMBER
     C*
     C                   Z-ADD     EORCN         KKRCN
     C     EAKY2         CHAIN     PEA01                              98
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYM2
     C                   MOVEL     SRDTO         XXWCPD            8 0
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C* GET 'FROM' AND 'TO' SERVICE DATE
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C*** GET HEADER INFORMATION BASED ON BILL TYPE
     C*
     C                   SETOFF                                       36
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C     EHKY          CHAIN     PEH01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C* PATIENT ACCOUNT#
     C*
     C                   MOVEL(P)  EHACCT        XXACCT
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'H'
     C     EUKY          CHAIN     PEU01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C* PATIENT ACCOUNT#
     C*
     C                   MOVEL(P)  EUACCT        XXACCT
     C     EUICN         IFEQ      'IM'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IR'
     C     EUICN         OREQ      'IS'
     C                   SETON                                        76
     C                   ENDIF                                                  EUICN = IM/IP/IR/IS
     C*
     C* GET OTHER HOSPITAL SPECIFIC INFORMATION FOR DISPLAY
     C*
     C*
     C* DRG Code and Description
     C*
     C                   Z-ADD     EUDRG#        XXDRG#
     C                   MOVEL     EUDGDS        XXDGDS
     C*
     C*
     C* ADMISSION DATE
     C                   Z-ADD     EUADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXADDT            8 0
     C*
     C* STATEMENT COVERS DATES
     C*
     C                   Z-ADD     EUSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSCPF            8 0
     C*
     C                   Z-ADD     EUSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSCPT            8 0
     C*
     C* SUBMISSION DATE
     C*
     C                   Z-ADD     EUSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXSBDT            8 0
     C*
     C* TYPE OF SERVICE AND DESCRIPTION
     C*
     C                   MOVEL     EUICN         XXICN             2
     C                   MOVEL     'SVTY'        KKTBID
     C                   MOVEL(P)  XXICN         KXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXICDS           30
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* TYPE OF BILL AND DESCRIPTION
     C*
     C                   MOVEL     EUBLTY        XXBLTY            3
     C                   MOVEL(P)  BTTBID        KKTBID
     C*
     C                   MOVEL(P)  XXBLTY        KXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXBDSC           30
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* GET PROCEDURE CODE DESCRIPTIONS
     C*
     C                   MOVE      *BLANKS       XXPRD1           35
     C                   MOVE      *BLANKS       XXPRD2           35
     C                   MOVE      *BLANKS       XXPRD3           35
     C                   MOVE      *BLANKS       XXPRD4           35
     C*
     C     EUPRC1        IFNE      *BLANKS
     C                   MOVEL(P)  EUPRC1        KKCPT4
     C     PRKY          CHAIN     PPR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        XXPRD1
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EUPRC1 NE *BLANKS
     C*
     C     EUPRC2        IFNE      *BLANKS
     C                   MOVEL(P)  EUPRC2        KKCPT4
     C     PRKY          CHAIN     PPR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        XXPRD2
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EUPRC2 NE *BLANKS
     C*
     C     EUPRC3        IFNE      *BLANKS
     C                   MOVEL(P)  EUPRC3        KKCPT4
     C     PRKY          CHAIN     PPR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        XXPRD3
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EUPRC2 NE *BLANKS
     C*
     C     EUPRC4        IFNE      *BLANKS
     C                   MOVEL(P)  EUPRC4        KKCPT4
     C     PRKY          CHAIN     PPR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        XXPRD4
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  EUPRC2 NE *BLANKS
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'X'
     C     EPKY          CHAIN     PEP01                              99
     C                   MOVE      *BLANKS       XXACCT
     C                   MOVE      *BLANKS       XXPHAR
     C     EPPHAR        IFNE      *BLANKS
     C                   MOVEL(P)  EPPHAR        XXPHAR
     C                   SETON                                        36
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'D'
     C                   MOVE      *BLANKS       XXACCT
     C                   ENDSL
     C*
     C* GET MEDICAL EXAMINER HEADING, MEDICAL EXAMINER VALUES,
     C* AND REDUCTIONS HEADINGS
     C*
     C                   MOVE      *BLANKS       XXMUSR           20
     C*
     C                   Z-ADD     EOCO#         ##CO#
     C                   Z-ADD     EOSUB#        ##SUB#
     C                   Z-ADD     EOFND         ##FND
     C                   Z-ADD     EOFYR         ##FYR
     C                   Z-ADD     EOCASE        ##CASE
     C                   Z-ADD     EOEOB#        ##EOB#
     C                   MOVE      *BLANKS       ##USER           40
     C                   CALL      'FA723R'
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##EOB#
     C                   PARM                    ##USER
     C*
     C     ##USER        IFEQ      'NONE'
     C                   SETON                                        67
     C                   ELSE
     C                   SETOFF                                       67
     C                   MOVEL(P)  ##USER        XXMUSR
     C                   ENDIF                                                  ##USER = 'NONE'
     C*
     C     CXKY          CHAIN     PCX01                              98
     C*
     C* GET LICENCE #/MEDICARE # BASED ON TYPE OF BILL/ADJ STATE
     C*
     C                   SELECT
     C*
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C                   MOVEL(P)  EHLIC#        XXLIC#           15
     C*
     C     EHFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EHFLIC        XXLIC#
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL(P)  EULIC#        XXLIC#
     C*
     C     EUFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EUFLIC        XXLIC#
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL     EPPHAR        XXLIC#
     C*
     C                   ENDSL
     C*
     C* GET DECIMAL VERSIONS OF DIAGNOSIS CODES
     C*
     C                   MOVE      *BLANKS       XXDS1             8
     C                   MOVE      *BLANKS       XXDS2             8
     C                   MOVE      *BLANKS       XXDS3             8
     C                   MOVE      *BLANKS       XXDS4             8
     C*
     C*
     C                   MOVE      *BLANKS       XXNUM1            2
     C                   MOVE      *BLANKS       XXNUM2            2
     C                   MOVE      *BLANKS       XXNUM3            2
     C                   MOVE      *BLANKS       XXNUM4            2
     C*
     C* GET DIAGNOSIS CODE DESCRIPTIONS
     C*
     C                   MOVE      *BLANKS       DESC1
     C                   MOVE      *BLANKS       DESC2
     C                   MOVE      *BLANKS       DESC3
     C                   MOVE      *BLANKS       DESC4
     C*
     C                   MOVE      'D'           KKLOST
      /Free
       Select;

       // Medical Bills

       When EobTyp = 'N';
       // ICD10 is active then populates ICD10 description else ICD9
       If EHIcdInd = 'Y';
          If EHICD1001 <> *blanks;
             kkicd10 = EHICD1001;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC1 = ICdsc1;
                XXDS1 = ICfcode;
                XXNUM1= '1.';
             Endif;
          Endif;

          If EHICD1002 <> *blanks;
             kkicd10 = EHICD1002;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC2 = ICdsc1;
                XXDS2 = ICfcode;
                XXNUM2= '2.';
             Endif;
          Endif;

          If EHICD1003 <> *blanks;
             kkicd10 = EHICD1003;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC3 = ICdsc1;
                XXDS3 = ICfcode;
                XXNUM3= '3.';
             Endif;
          Endif;

          If EHICD1004 <> *blanks;
             kkicd10 = EHICD1004;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC4 = ICdsc1;
                XXDS4 = ICfcode;
                XXNUM4= '4.';
             Endif;
          Endif;
          If EHicdE <> *blanks;
             kkicd10 = EHicdE;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                descE = ICdsc1;
                xxDSe = ICfcode;
             Endif;
          Endif;
       Else;
         If EHDIA1 <> *blanks;
            kkicd9 = EHDIA1;
            Chain diky pdi01;
            If %found(fmdil);
               DESC1 = DIdsc1;
               XXDS1 = DIds1;
               XXNUM1= '1.';
            Endif;
         Endif;

         If EHDIA2 <> *blanks;
            kkicd9 = EHDIA2;
            Chain diky pdi01;
            If %found(fmdil);
               DESC2 = DIdsc1;
               XXDS2 = DIds1;
               // ***** Bed Chg ****** 06/05/14 *****************
               // XXNUM1= '2.';
               XXNUM2= '2.';
               // ***** Bed Chg ****** 06/05/14 *****************
            Endif;
         Endif;

         If EHDIA3 <> *blanks;
            kkicd9 = EHDIA3;
            Chain diky pdi01;
            If %found(fmdil);
               DESC3 = DIdsc1;
               XXDS3 = DIds1;
               // ***** Bed Chg ****** 06/05/14 *****************
               // XXNUM1= '3.';
               XXNUM3= '3.';
               // ***** Bed Chg ****** 06/05/14 *****************
            Endif;
         Endif;

         If EHDIA4 <> *blanks;
            kkicd9 = EHDIA4;
            Chain diky pdi01;
            If %found(fmdil);
               DESC4 = DIdsc1;
               XXDS4 = DIds1;
               // ***** Bed Chg ****** 06/05/14 *****************
               // XXNUM1= '4.';
               XXNUM4= '4.';
               // ***** Bed Chg ****** 06/05/14 *****************
            Endif;
          Endif;
       Endif;

       // Hospital Bills

       When EobTyp = 'H';
       // ICD10 is active then populates ICD10 description else ICD9
       If EUicdInd = 'Y' ;
          If EUICD1001 <> *blanks;
             kkicd10 = EUicd1001;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC1 = ICdsc1;
                XXDS1 = ICfcode;
                XXNUM1= '1.';
             Endif;
          Endif;

          If EUicd1002 <> *blanks;
             kkicd10 = EUicd1002;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC2 = ICdsc1;
                XXDS2 = ICfcode;
                XXNUM2= '2.';
             Endif;
          Endif;

          If EUicd1003 <> *blanks;
             kkicd10 = EUicd1003;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC3 = ICdsc1;
                XXDS3 = ICfcode;
                XXNUM3= '3.';
             Endif;
          Endif;

          If EUicd1004 <> *blanks;
             kkicd10 = EUicd1004;
             Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
                DESC4 = ICdsc1;
                XXDS4 = ICfcode;
                XXNUM4= '4.';
             Endif;
          Endif;
       Else;
          If EUdia1 <> *blanks;
             kkicd9 = EUdia1;
             Chain diky pdi01;
             If %found(fmdil);
                DESC1 = DIdsc1;
                XXDS1 = DIds1;
                XXNUM1= '1.';
             Endif;
          Endif;

          If EUdia2 <> *blanks;
             kkicd9 = EUdia2;
             Chain diky pdi01;
             If %found(fmdil);
                DESC2 = DIdsc1;
                XXDS2 = DIds1;
               // ***** Bed Chg ****** 06/05/14 *****************
               // XXNUM1= '2.';
               XXNUM2= '2.';
               // ***** Bed Chg ****** 06/05/14 *****************
             Endif;
          Endif;

          If EUdia3 <> *blanks;
             kkicd9 = EUdia3;
             Chain diky pdi01;
             If %found(fmdil);
                DESC3 = DIdsc1;
                XXDS3 = DIds1;
               // ***** Bed Chg ****** 06/05/14 *****************
               // XXNUM1= '3.';
               XXNUM3= '3.';
               // ***** Bed Chg ****** 06/05/14 *****************
             Endif;
          Endif;

          If EUdia4 <> *blanks;
             kkicd9 = EUdia4;
             Chain diky pdi01;
             If %found(fmdil);
                DESC4 = DIdsc1;
                XXDS4 = DIds1;
               // ***** Bed Chg ****** 06/05/14 *****************
               // XXNUM1= '4.';
               XXNUM4= '4.';
               // ***** Bed Chg ****** 06/05/14 *****************
             Endif;
          Endif;
       Endif;

       // Dental Bills

       When EobTyp = 'D';
       Chain faedlkey ped01;
       If %found(faedl);
       // ICD10 is active then populates ICD10 description else ICD9
          If EDicdInd = 'Y';
             If EDICD1001 <> *blanks;
                kkicd10 = EDicd1001;
                Chain fmicd10l1key pic01;
                  If %found(fmicd10l1);
                     DESC1 = ICdsc1;
                     XXDS1 = ICfcode;
                     XXNUM1= '1.';
                  Endif;
             Endif;

             If EDicd1002 <> *blanks;
               kkicd10 = EDicd1002;
               Chain fmicd10l1key pic01;
                 If %found(fmicd10l1);
                    DESC2 = ICdsc1;
                    XXDS2 = ICfcode;
                    XXNUM2= '2.';
                 Endif;
             Endif;

             If EDicd1003 <> *blanks;
                kkicd10 = EDicd1003;
                Chain fmicd10l1key pic01;
                  If %found(fmicd10l1);
                     DESC3 = ICdsc1;
                     XXDS3 = ICfcode;
                     XXNUM3= '3.';
                  Endif;
             Endif;

             If EDicd1004 <> *blanks;
                kkicd10 = EDicd1004;
                Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   DESC4 = ICdsc1;
                   XXDS4 = ICfcode;
                   XXNUM4= '4.';
                Endif;
             Endif;
          Else;
             If EDdia1 <> *blanks;
                kkicd9 = EDdia1;
                Chain diky pdi01;
                  If %found(fmdil);
                     DESC1 = DIdsc1;
                     XXDS1 = DIds1;
                     XXNUM1= '1.';
                  Endif;
             Endif;

             If EDdia2 <> *blanks;
                kkicd9 = EDdia2;
                Chain diky pdi01;
                  If %found(fmdil);
                     DESC2 = DIdsc1;
                     XXDS2 = DIds1;
                     XXNUM2= '2.';
                  Endif;
             Endif;

             If EDdia3 <> *blanks;
                kkicd9 = EDdia3;
                Chain diky pdi01;
                  If %found(fmdil);
                     DESC3 = DIdsc1;
                     XXDS3 = DIds1;
                     XXNUM3= '3.';
                  Endif;
             Endif;

             If EDdia4 <> *blanks;
                kkicd9 = EDdia4;
                Chain diky pdi01;
                  If %found(fmdil);
                     DESC4 = DIdsc1;
                     XXDS4 = DIds1;
                     XXNUM4= '4.';
                  Endif;
             Endif;
          Endif;
       Endif;
       Endsl;

      /End-Free
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   SETOFF                                       8081
     C*
     C* SET INDICATOR TO DISPLAY NETWORK DISCLAIMER IF NECESSARY
     C*
     C     EONTWK        IFNE      *BLANKS
     C     EONDIS        ANDNE     *ZEROS
     C                   SETON                                        47
     C*
     C     EONDSC        IFEQ      *BLANKS
     C*
     C                   MOVE      EONTWK        KKNWCD
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     NWNAME        XXNWNM           40
     C                   MOVEL     NWMSG(1)      XXNWM1
     C     XXNWNM        CAT       NWMSG(2):1    XXNWM2
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C*
     C                   CALL      'FA752R'
     C                   PARM      EONDSC        ##INPT          255
     C     XXNWM         PARM      *BLANKS       ##OUT1          140
     C     XXNW2         PARM      *BLANKS       ##OUT2          140
     C     XXNW3         PARM      *BLANKS       ##OUT3          140
     C*
     C     XXNW2         IFNE      *BLANKS
     C                   SETON                                        80
     C                   ENDIF
     C     XXNW3         IFNE      *BLANKS
     C                   SETON                                        81
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF                                                  EONTWK NE *BLANKS
     C*
     C* Use the Network Name passed in from the upload
     C*
     C     ##CLT#        IFEQ      '135'
     C     EOMBUP        ANDEQ     'C'
     C*
     C     EONDIS        IFNE      *ZEROS
     C                   SETON                                        47
     C                   MOVEL     EONWNM        XXNWNM
     C                   MOVEL     NWMSG(1)      XXNWM1
     C     XXNWNM        CAT       NWMSG(2):1    XXNWM2
     C*
     C                   ELSE                                                   EONDIS <> 0
     C                   SETOFF                                       47
     C*
     C                   ENDIF                                                  EONDIS <> 0
     C*
     C                   ENDIF                                                  ##CLT# = 135
     C*
     C* PREPARE TO CHANGE FONT
     C*
     C                   EXSR      MSKSS#
     C                   If        isUseSpoolout
     C                   WRITE     HEAD1
     C                   Endif
     C*
     C                   SETOFF                                       74
     C*
     C                   MOVEL(P)  XXCSZ1        X1CSZ1
     C*
     C                   MOVE      '0'           *IN35
     C     EOBNPI        IFNE      *BLANKS
     C                   MOVEL(P)  EOBNPI        XXBNPI
     C                   SETON                                        35
     C                   ELSE
     C                   SETOFF                                       35
     C                   ENDIF
     C                   MOVEL(P)  CXHEX         X1HEX
     C                   MOVEL(P)  CXHEX         X1HEXB
     C*
     C                   If        isUseSpoolout
     C                   WRITE     HEAD2
     C                   WRITE     HEAD3
     C                   Endif
     C*
     C*** Write to Scriptura Header EOB workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtHdWrk
     C                   Endif
     C*
     C* CYLCE THROUGH FDCAP FILE TO GET ALL CHARGES FOR THE BILL
     C*
     C                   CLEAR                   EOB
     C                   CLEAR                   XLINE
     C                   CLEAR                   TMDSEQ
     C                   CLEAR                   XPFL1
     C*
     C*                    MOVE *LOVAL    KKFDAT
     C*                    MOVE *LOVAL    KKTDAT
     C*          CAKY      SETLLPCA02
     C*          CAKY      READEPCA02                    98
     C*          *IN98     IFEQ *OFF
     C*
     C                   CLEAR                   XXFDAT
     C                   CLEAR                   XXTDAT
     C                   CLEAR                   XXCPT4
     C                   CLEAR                   XXMOD1
     C                   CLEAR                   XXMOD2
     C                   CLEAR                   XXMOD3
     C                   CLEAR                   XXMOD4
     C                   CLEAR                   XXPOS
     C                   CLEAR                   XXDRF1
     C                   CLEAR                   XXDRF2
     C                   CLEAR                   XXDRF3
     C                   CLEAR                   XXDRF4
     C                   CLEAR                   xxDxrA
     C                   CLEAR                   xxDxrB
     C                   CLEAR                   xxDxrC
     C                   CLEAR                   xxDxrD
     C                   CLEAR                   xxDxrE
     C                   CLEAR                   xxDxrF
     C                   CLEAR                   xxDxrG
     C                   CLEAR                   xxDxrH
     C                   CLEAR                   xxDxrI
     C                   CLEAR                   xxDxrJ
     C                   CLEAR                   xxDxrK
     C                   CLEAR                   xxDxrL
     C                   CLEAR                   XXPRDS
     C                   CLEAR                   XXBILD
     C                   CLEAR                   X1EOB
     C                   CLEAR                   X2EOB
     C                   CLEAR                   X3EOB
     C                   CLEAR                   X4EOB
       //*** Beg Add *** 08/22/22 *********************************************
       If ModernEOB = 'Y';
          Clear x5Eob;
          Clear x6Eob;
          Clear x7Eob;
          Clear x8Eob;
          Clear x9Eob;
          Clear x10Eob;
          Clear x11Eob;
          Clear x12Eob;
       Endif;
       //*** End Add *** 08/22/22 *********************************************
     C                   CLEAR                   XNOTE
     C                   CLEAR                   XXUNTS
     C     CAKY          SETLL     PCA02
     C     CAKY          READE     PCA02                                  98
     C     *IN98         DOWEQ     *OFF                                         *IN98 = *OFF
     C*
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   Z-ADD     CARCN         KKRCN#
     C     DAKY          CHAIN     PDA01                              60
     C*
     C                   Z-ADD     CAFDAT        HLFDAT
     C                   Z-ADD     CATDAT        HLTDAT
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDAT            8 0
     C*
     C                   Z-ADD     CATDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTDAT            8 0
     C*
     C     XPFL1         IFNE      'Y'
     C                   MOVE      CADSEQ        TMDSEQ
     C                   MOVE      'Y'           XPFL1             1
     C                   ENDIF
     C*
     C     CADSEQ        SUB       TMDSEQ        XLINE
     C                   ADD       1             XLINE
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL12FL
     C                   Endif
     C* Initialize diag cross reference fields.
     C                   eval      xxDxrA = ' '
     C                   eval      xxDxrB = ' '
     C                   eval      xxDxrC = ' '
     C                   eval      xxDxrD = ' '
     C                   eval      xxDxrE = ' '
     C                   eval      xxDxrF = ' '
     C                   eval      xxDxrG = ' '
     C                   eval      xxDxrH = ' '
     C                   eval      xxDxrI = ' '
     C                   eval      xxDxrJ = ' '
     C                   eval      xxDxrK = ' '
     C                   eval      xxDxrL = ' '
     C*
     C* GET CPT4/PROCEDURE CODE, PROCEDURE MODIFIERS AND
     C* PLACE OF SERVICE BASED ON THE TYPE OF BILL
     C*
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C     DHKY          CHAIN     PDH01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  DHPRCD        XXCPT4           11
     C                   MOVE      DHMOD1        XXMOD1
     C                   MOVE      DHMOD2        XXMOD2
     C                   MOVE      DHMOD3        XXMOD3
     C                   MOVE      DHMOD4        XXMOD4
     C                   MOVE(P)   DHPOS         XXPOS
     C                   MOVEL     DHPRDS        XXPRDS           25
     C*
     C     DHBPRC        IFNE      *BLANKS
     C                   MOVEL(P)  DHBPRC        XXBILD           11
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DHBRPC <> *BLANKS
     C*
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DHUNTS        XXUNTS            5 2
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'H'
     C     DUKY          CHAIN     PDU01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVE      DUPMOD        XXMOD1
     C                   MOVE      DUMOD2        XXMOD2
     C                   MOVE      DUMOD3        XXMOD3
     C                   MOVE      DUMOD4        XXMOD4
     C*
     C                   MOVEL(P)  DURVCD        XXCPT4
     C                   MOVE      DURATE        XXCPT4
     C                   MOVE(P)   DUPOS         XXPOS
     C                   MOVEL     DUDESC        XXPRDS
     C*
     C     DUBPRC        IFNE      *ZEROS
     C     DUBRAT        ORNE      *BLANKS
     C                   MOVEL(P)  DUBPRC        X1BILD            8
     C                   MOVE      DUBRAT        X1BILD
     C                   MOVEL(P)  X1BILD        XXBILD
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DUBRPC <> *BLANKS
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DUUNTS        XXUNTS
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'X'
     C     DPKY          CHAIN     PDP01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C     DPHCPC        IFNE      *BLANKS
     C                   MOVEL(P)  DPHCPC        XXCPT4
     C                   ELSE
     C*
     C                   MOVEL(P)  DPNDC         XXCPT4
     C                   ENDIF
     C*
     C                   MOVEL     DPDESC        XXPRDS
     C*
     C     DPBNDC        IFNE      *BLANKS
     C*
     C     DPHCPC        IFNE      *BLANKS
     C                   MOVEL(P)  DPHCPC        XXBILD
     C                   ELSE
     C*
     C                   MOVEL(P)  DPBNDC        XXBILD
     C                   ENDIF
     C*
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DPBNDC <> *BLANKS
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DPUNTS        XXUNTS
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'D'
     C     DDKY          CHAIN     PDD01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  DDPRCD        XXCPT4
     C                   MOVEL     DDPRDS        XXPRDS
     C*
     C     DDBPRC        IFNE      *BLANKS
     C                   MOVEL(P)  DDBPRC        XXBILD
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DPBNDC <> *BLANKS
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DDUNTS        XXUNTS
     C                   if        DDdxr1 = 'X'
     C                   eval      xxDxrA = 'A'
     C                   endif
     C                   if        DDdxr2 = 'X'
     C                   eval      xxDxrB = 'B'
     C                   endif
     C                   if        DDdxr3 = 'X'
     C                   eval      xxDxrC = 'C'
     C                   endif
     C                   if        DDdxr4 = 'X'
     C                   eval      xxDxrD = 'D'
     C                   endif
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C                   ENDSL
     C*
     C* GET DIAGNOSIS CODE BASED ON CROSS REFERENCE
     C*
     C                   MOVE      *BLANKS       XXDRF1            1
     C                   MOVE      *BLANKS       XXDRF2            1
     C                   MOVE      *BLANKS       XXDRF3            1
     C                   MOVE      *BLANKS       XXDRF4            1
     C*
     C     EOBTYP        IFEQ      'N'
     C     EOBTYP        OREQ      'V'
     C*
     C     DHDXR1        IFEQ      'X'
     C                   SELECT
     C     XXDRF1        WHENEQ    ' '
     C                   MOVE      '1'           XXDRF1
     C     XXDRF2        WHENEQ    ' '
     C                   MOVE      '1'           XXDRF2
     C     XXDRF3        WHENEQ    ' '
     C                   MOVE      '1'           XXDRF3
     C     XXDRF4        WHENEQ    ' '
     C                   MOVE      '1'           XXDRF4
     C                   ENDSL
     C                   ENDIF                                                  DHDXR1 = 'X'
     C*
     C     DHDXR2        IFEQ      'X'
     C                   SELECT
     C     XXDRF1        WHENEQ    ' '
     C                   MOVE      '2'           XXDRF1
     C     XXDRF2        WHENEQ    ' '
     C                   MOVE      '2'           XXDRF2
     C     XXDRF3        WHENEQ    ' '
     C                   MOVE      '2'           XXDRF3
     C     XXDRF4        WHENEQ    ' '
     C                   MOVE      '2'           XXDRF4
     C                   ENDSL
     C                   ENDIF                                                  DHDXR2 = 'X'
     C*
     C     DHDXR3        IFEQ      'X'
     C                   SELECT
     C     XXDRF1        WHENEQ    ' '
     C                   MOVE      '3'           XXDRF1
     C     XXDRF2        WHENEQ    ' '
     C                   MOVE      '3'           XXDRF2
     C     XXDRF3        WHENEQ    ' '
     C                   MOVE      '3'           XXDRF3
     C     XXDRF4        WHENEQ    ' '
     C                   MOVE      '3'           XXDRF4
     C                   ENDSL
     C                   ENDIF                                                  DHDXR3 = 'X'
     C*
     C     DHDXR4        IFEQ      'X'
     C                   SELECT
     C     XXDRF1        WHENEQ    ' '
     C                   MOVE      '4'           XXDRF1
     C     XXDRF2        WHENEQ    ' '
     C                   MOVE      '4'           XXDRF2
     C     XXDRF3        WHENEQ    ' '
     C                   MOVE      '4'           XXDRF3
     C     XXDRF4        WHENEQ    ' '
     C                   MOVE      '4'           XXDRF4
     C                   ENDSL
     C                   ENDIF                                                  DHDXR4 = 'X'
     C                   if        DHdxr1 = 'X'
     C                   eval      xxDxrA = 'A'
     C                   endif
     C                   if        DHdxr2 = 'X'
     C                   eval      xxDxrB = 'B'
     C                   endif
     C                   if        DHdxr3 = 'X'
     C                   eval      xxDxrC = 'C'
     C                   endif
     C                   if        DHdxr4 = 'X'
     C                   eval      xxDxrD = 'D'
     C                   endif
     C                   if        DHdxrE = 'X'
     C                   eval      xxDxrE = 'E'
     C                   endif
     C                   if        DHdxrF = 'X'
     C                   eval      xxDxrF = 'F'
     C                   endif
     C                   if        DHdxrG = 'X'
     C                   eval      xxDxrG = 'G'
     C                   endif
     C                   if        DHdxrH = 'X'
     C                   eval      xxDxrH = 'H'
     C                   endif
     C                   if        DHdxrI = 'X'
     C                   eval      xxDxrI = 'I'
     C                   endif
     C                   if        DHdxrJ = 'X'
     C                   eval      xxDxrJ = 'J'
     C                   endif
     C                   if        DHdxrK = 'X'
     C                   eval      xxDxrK = 'K'
     C                   endif
     C                   if        DHdxrL = 'X'
     C                   eval      xxDxrL = 'L'
     C                   endif
     C*
     C                   ENDIF                                                  EOBTYP = 'N'
     C*
     C* GET EOB CODES
     C*
     C*** Beg Chg *** 08/22/22 ************************************************
     C*******            Z-ADD     1             X                 1 0
     C*******            MOVE      *BLANKS       X1EOB
     C*******            MOVE      *BLANKS       X2EOB
     C*******            MOVE      *BLANKS       X3EOB
     C*******            MOVE      *BLANKS       X4EOB
     C*******            MOVE      *BLANKS       XNOTE
       x = 1;
       x1Eob  = *blanks;
       x2Eob  = *blanks;
       x3Eob  = *blanks;
       x4Eob  = *blanks;
       x5Eob  = *blanks;
       x6Eob  = *blanks;
       x7Eob  = *blanks;
       x8Eob  = *blanks;
       x9Eob  = *blanks;
       x10Eob = *blanks;
       x11Eob = *blanks;
       x12Eob = *blanks;
       xNote  = *blanks;
     C*** End Chg *** 08/22/22 ************************************************
     C                   SETOFF                                       424344
     C                   SETOFF                                       45
      //**** Beg Add *** 08/22/22 *********************************************
       *in49 = *off;
       *in50 = *off;
       *in51 = *off;
       *in54 = *off;
       *in55 = *off;
       *in56 = *off;
       *in57 = *off;
       *in59 = *off;
      //**** End Add *** 08/22/22 *********************************************
     C*
     C*
     C                   SETOFF                                       01
     C*
     C*
     C*
     C*** Beg Add *** 08/22/22 ************************************************
       If ModernEOB = 'Y';
          X2 = 12;
       Else;
          X2 = 4;
       Endif;
     C*** End Add *** 08/22/22 ************************************************
     C     DEKY          CHAIN     PDE01                              98
     C     *IN98         DOWEQ     *OFF
     C*** Beg Chg *** 08/22/22 ************************************************
     C**** X             ANDLE     4
     C     X             ANDLE     X2
     C*** End Chg *** 08/22/22 ************************************************
     C*
     C* SKIP INACTIVE CODES
     C*
     C*
     C     DEPRNT        IFNE      'N'
     C     DESTAT        ANDNE     'I'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*
     C                   SELECT
     C     X             WHENEQ    1
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X1EOB
     C                   MOVE      DEEOBCD       X1EOB
     C**** Beg Add *** 08/22/22 **********************************************
     C****               MOVE      DEEOB         X1EOBR
     C                   MOVE      DEEOBCD       X1EOBR
     C**** End Add *** 08/22/22 **********************************************
     C**** End Chg *** 07/18/23 **************************************
     C     X1EOB         IFNE      *BLANKS
     C                   SETON                                        42
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF                                                  X1EOB NE *BLANKS
     C*
     C     X             WHENEQ    2
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X2EOB
     C                   MOVE      DEEOBCD       X2EOB
     C**** End Chg *** 07/18/23 **************************************
     C**** Beg Add *** 08/22/22 **********************************************
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X2EOBR
     C                   MOVE      DEEOBCD       X2EOBR
     C**** End Chg *** 07/18/23 **************************************
     C**** End Add *** 08/22/22 **********************************************
     C     X2EOB         IFNE      *BLANKS
     C                   SETON                                        43
     C                   ELSE
     C                   SETOFF                                       43
     C                   ENDIF                                                  X2EOB NE *BLANKS
     C*
     C     X             WHENEQ    3
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X3EOB
     C                   MOVE      DEEOBCD       X3EOB
     C**** End Chg *** 07/18/23 **************************************
     C**** Beg Add *** 08/22/22 **********************************************
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X3EOBR
     C                   MOVE      DEEOBCD       X3EOBR
     C**** End Chg *** 07/18/23 **************************************
     C**** End Add *** 08/22/22 **********************************************
     C     X3EOB         IFNE      *BLANKS
     C                   SETON                                        44
     C                   ELSE
     C                   SETOFF                                       44
     C                   ENDIF                                                  X3EOB NE *BLANKS
     C*
     C     X             WHENEQ    4
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X4EOB
     C                   MOVE      DEEOBCD       X4EOB
     C**** End Chg *** 07/18/23 **************************************
     C**** Beg Add *** 08/22/22 **********************************************
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X4EOBR
     C                   MOVE      DEEOBCD       X4EOBR
     C**** End Chg *** 07/18/23 **************************************
     C**** End Add *** 08/22/22 **********************************************
     C     X4EOB         IFNE      *BLANKS
     C                   SETON                                        45
     C                   ELSE
     C                   SETOFF                                       45
     C                   ENDIF                                                  X4EOB NE *BLANKS
     C*
     C                   ENDSL
       //*** Beg Add *** 08/22/22 **********************************************
       If ModernEOB = 'Y';
          select;
          When X = 5;
             //*** Beg Chg *** 07/18/23 *********************
             //*** x5Eob = DEeob;
             //*** x5EobR= DEeob;
             x5Eob = DEeobCd;
             x5EobR= DEeobCd;
             //*** End Chg *** 07/18/23 *********************
             If x5Eob <> *Blanks;
                *in49 = *On;
             Else;
                *in49 = *off;
             Endif;
          When X = 6;
             //*** Beg Chg *** 07/18/23 *********************
             //*** x6Eob = DEeob;
             //*** x6EobR= DEeob;
             x6Eob = DEeobCd;
             x6EobR= DEeobCd;
             //*** End Chg *** 07/18/23 *********************
             If x6Eob <> *Blanks;
                *in50 = *On;
             Else;
                *in50 = *off;
             Endif;
          When X = 7;
             //*** Beg Chg *** 07/18/23 *********************
             //*** x7Eob = DEeob;
             //*** x7EobR= DEeob;
             x7Eob = DEeobCd;
             x7EobR= DEeobCd;
             //*** End Chg *** 07/18/23 *********************
             If x7Eob <> *Blanks;
                *in51 = *On;
             Else;
                *in51 = *off;
             Endif;
          When X = 8;
             //*** Beg Chg *** 07/18/23 *********************
             //*** x8Eob = DEeob;
             //*** x8EobR= DEeob;
             x8Eob = DEeobCd;
             x8EobR= DEeobCd;
             //*** End Chg *** 07/18/23 *********************
             If x8Eob <> *Blanks;
                *in54 = *On;
             Else;
                *in54 = *off;
             Endif;
          When X = 9;
             //*** Beg Chg *** 07/18/23 *********************
             //*** x9Eob = DEeob;
             //*** x9EobR= DEeob;
             x9Eob = DEeobCd;
             x9EobR= DEeobCd;
             //*** End Chg *** 07/18/23 *********************
             If x9Eob <> *Blanks;
                *in55 = *On;
             Else;
                *in55 = *off;
             Endif;
          When X = 10;
             //*** Beg Chg *** 07/18/23 *********************
             //*** x10Eob = DEeob;
             //*** x10EobR= DEeob;
             x10Eob = DEeobCd;
             x10EobR= DEeobCd;
             //*** End Chg *** 07/18/23 *********************
             If x10Eob <> *Blanks;
                *in56 = *On;
             Else;
                *in56 = *off;
             Endif;
          When X = 11;
             //*** Beg Chg *** 07/18/23 *********************
             //*** x11Eob = DEeob;
             //*** x11EobR= DEeob;
             x11Eob = DEeobCd;
             x11EobR= DEeobCd;
             //*** End Chg *** 07/18/23 *********************
             If x11Eob <> *Blanks;
                *in57 = *On;
             Else;
                *in57 = *off;
             Endif;
          When X = 12;
             //*** Beg Chg *** 07/18/23 *********************
             //*** x12Eob = DEeob;
             //*** x12EobR= DEeob;
             x12Eob = DEeobCd;
             x12EobR= DEeobCd;
             //*** End Chg *** 07/18/23 *********************
             If x12Eob <> *Blanks;
                *in59 = *On;
             Else;
                *in59 = *off;
             Endif;
          Endsl;
       Endif;
       //*** End Add *** 08/22/22 **********************************************
     C                   ADD       1             X
     C*
     C* LOAD ALL DETAIL EOB CODES INTO ARRAY 'EOB'.  THE HEADER EOB
     C* CODES WILL BE ADDED TO THIS ARRAY LATER.  THIS ARRAY IS USED
     C* TO DISPLAY THE DESCRIPTIONS OF ALL EOB CODES FOR THE BILL
     C* IN ONE LOCATION.
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C                   Z-ADD     1             Y                 2 0
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         LOOKUP    EOB(Y)                                 41
     C     DEEOBCD       LOOKUP    EOB(Y)                                 41
     C**** End Chg *** 07/18/23 **************************************
     C     *IN41         IFEQ      *OFF                                         not in array
     C                   Z-ADD     1             Z                 2 0
     C     *BLANKS       LOOKUP    EOB(Z)                                 31
     C     *IN31         IFEQ      *ON
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         EOB(Z)
     C                   MOVE      DEEOBCD       EOB(Z)
     C**** End Chg *** 07/18/23 **************************************
     C                   Eval      EOBdSeq(Z) = DEdSeq
     C                   Eval      EOBseq#(Z) = DEseq#
     C                   ENDIF                                                  *IN31 = *ON
     C                   ENDIF                                                  *IN41 = *OFF
     C                   ENDIF                                                  DEEOB NE *BLANKS
     C*
     C*** Write to Scriptura Detail EOB Code Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetCdWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C     DEKY          READE     PDE01                                  98
     C                   ENDDO
     C*
     C* SET INDICATORS TO PRINT EOB CODE/NOTE TITLES
     C*
       //*** Beg Chg 08/22/22 *************************************************
     C**** X1EOB         IFNE      *BLANKS
     C**** X2EOB         ORNE      *BLANKS
     C**** X3EOB         ORNE      *BLANKS
     C**** X4EOB         ORNE      *BLANKS
     C****               SETON                                        48
     C****               ELSE
     C****               SETOFF                                       48
     C****               ENDIF                                                  X1EOB NE BLANKS
          If x1Eob <> *blanks or x2Eob <> *blanks or x3Eob <> *blanks
          or x4Eob <> *blanks or x5Eob <> *blanks or x6Eob <> *blanks
          or x7Eob <> *blanks or x8Eob <> *blanks or x9Eob <> *blanks
          or x10Eob <> *blanks or x11Eob <> *blanks or x12Eob <> *blanks;
             *In48 = *on;
          Else;
             *In48 = *off;
          EndIf;
       //*** End Chg 08/22/22 *************************************************
     C*
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG13
     C                   If        isUseSpoolout
     C                   WRITE     DETL12FL
     C                   Endif
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL1
     C   99              WRITE     SPACE
     C                   Endif
     C                   SETOFF                                       99
     C*
     C*
     C     *IN01         IFEQ      *ON
     C                   EXSR      NWPG13
     C                   SETOFF                                       01
     C                   ENDIF
     C*
     C*
     C*** Write to Scriptura Detail Charges Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDtWrk
     C                   Endif
     C*
     C*
     C     CAKY          READE     PCA02                                  98
     C                   ENDDO                                                  *IN9= *OFF
     C                   if        isScriptEOB = 'Y'
     C                   exsr      wrtDGwrk
     C                   endif
     C*
     C                   Z-ADD     EOMRA         XXMRA             9 2
     C                   Z-ADD     EOOTH2        XXOTH2            9 2
     C                   Z-ADD     EOPAID        XXPAID            9 2
     C                   Z-ADD     EOPAID        XXPAI2            9 2
     C*
     C* CALCULATE 'PREVIOUSLY PAID' AND 'THIS PAYMENT' IF RECON NUMBER
     C* IS GREATER THAN ZERO
     C*
     C                   SETOFF                                       33
     C     EORCN         IFGT      0
     C                   SETON                                        33
     C*
     C                   Z-ADD     0             X1PAID            9 2          PREV PAID
     C                   Z-ADD     0             X2PAID            9 2          THIS PAYMENT
     C     EAKY1         CHAIN     PEA01                              98
     C     *IN98         DOWEQ     *OFF
     C     EARCN         IFLT      EORCN
     C                   ADD       EAPAID        X1PAID
     C                   ENDIF                                                  EARCN LT EORCN
     C     EAKY1         READE     PEA01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     EAKY2         CHAIN     PEA01                              98
     C     *IN98         DOWEQ     *OFF
     C                   ADD       EAPAID        X2PAID
     C     EAKY2         READE     PEA01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C                   ENDIF                                                  EORCN GT 0
     C*
     C* DETERMINE IF ENOUGH LINES ARE AVALAIBLE TO PRINT TOTALS
     C*
     C     *IN33         IFEQ      *ON
     C     CURLIN        ADD       4             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   ENDIF                                                  *IN33 = *ON
     C*
     C     *IN33         IFEQ      *OFF
     C     CURLIN        ADD       2             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   ENDIF                                                  *IN33 = *OFF
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL3
     C                   Endif
     C*
     C*
     C*** Write to Scriptura EOB Totals and Workfiles
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEOBTotWrk
     C                   Endif
     C*
     C* PRINT HOSPITAL INFO
     C*
     C     *IN40         IFEQ      *ON
     C*
     C* NEED FOUR LINES TO PRINT HOSPITAL INFORMATION
     C*
     C     CURLIN        ADD       5             LINEED
     C*
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL11
     C                   Endif
     C                   ENDIF                                                  *IN40 = *ON
     C*
     C* LOAD HEADER EOB CODES TO ARRAY 'EOB'
     C*
     C     EEKY          CHAIN     PEE01                              98
     C*
     C*
     C     *IN98         DOWEQ     *OFF
     C     EEPRNT        IFNE      'N'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** EEEOB         IFNE      *BLANKS
     C     EEEOBCD       IFNE      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*
     C                   Z-ADD     1             Y                 2 0
     C**** Beg Chg *** 07/18/23 **************************************
     C**** EEEOB         LOOKUP    EOB(Y)                                 41
     C     EEEOBCD       LOOKUP    EOB(Y)                                 41
     C**** End Chg *** 07/18/23 **************************************
     C     *IN41         IFEQ      *OFF                                         not in array
     C                   Z-ADD     1             Z                 2 0
     C     *BLANKS       LOOKUP    EOB(Z)                                 31
     C     *IN31         IFEQ      *ON
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      EEEOB         EOB(Z)
     C                   MOVE      EEEOBCD       EOB(Z)
     C**** End Chg *** 07/18/23 **************************************
     C                   ENDIF                                                  *IN31 = *ON
     C                   ENDIF                                                  *IN41 = *OFF
     C                   ENDIF                                                  EEEOB NE *BLANKS
     C*
     C                   ENDIF
     C*
     C     EEKY          READE     PEE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C* GET ALL LINE LEVEL/FAEEP EOB CODE DESCRIPTIONS
     C*
     C* DISPLAY THE EOB CODES AND DESCRIPTIONS
     C*
     C                   CLEAR                   XPFLAG
     C*
     C     EOB(1)        IFNE      *BLANKS
     C*
     C                   Z-ADD     1             T                 2 0
     C                   DO        20            T
     C*
     C     EOB(T)        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  EOB(T)        YYEOB
     C                   MOVEL(P)  EOB(T)        KKEOBC
     C*
     C* GET THE DESCRIPTION FOR FLORIDA BILLS
     C*
     C                   If        YYEOB = ' 93' and EOAST = 'FL'
     C     DEKY3         SETGT     PDE01                              98
     C     DEKY3         READPE    PDE01
     C                   DOW       Not %EOF(FDDEL)
     C**** Beg Chg *** 07/18/23 **************************************
     C****               IF        YYEOB = DEEOB
     C                   IF        YYEOB = DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL     DENOT1        $$DESC
     C                   MOVEL     DENOT2        $DESC2
     C                   MOVEL     DENOT3        $DESC3
     C                   MOVEL     DENOT4        $DESC4
     C                   ENDIF
     C     DEKY3         READPE    PDE01
     C                   ENDDO
     C                   Else
     C                   CALL      'FA769R'
     C                   PARM      KKCO#         $$CO#
     C                   PARM      YYEOB         $$EOB
     C                   PARM      *BLANKS       $$DESC
     C                   PARM      *BLANKS       $DESC2           70
     C                   PARM      *BLANKS       $DESC3           70
     C                   PARM      *BLANKS       $DESC4           70
     C*** Beg Add *** 08/22/22 ******************************************
     C                   PARM      EOast         ##ast             2
     C*** End Add *** 08/22/22 ******************************************
     C                   Endif
     C*
     C     $$DESC        IFEQ      *BLANKS
     C     DEKY3         SETGT     PDE01                              98
     C     DEKY3         READPE    PDE01
     C                   DOW       Not %EOF(FDDEL)
     C**** Beg Chg *** 07/18/23 **************************************
     C****               IF        YYEOB = DEEOB
     C                   IF        YYEOB = DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL     DENOT1        $$DESC
     C                   MOVEL     DENOT2        $DESC2
     C                   MOVEL     DENOT3        $DESC3
     C                   MOVEL     DENOT4        $DESC4
     C                   ENDIF
     C     DEKY3         READPE    PDE01
     C                   ENDDO
     C                   ENDIF
     C     $$DESC        IFNE      *BLANKS
     C*
     C** Need four lines to print.
     C*
     C     CURLIN        ADD       5             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLEOB
     C                   Endif
     C                   ELSE
     C     XPFLAG        IFNE      'Y'
     C                   If        isUseSpoolout
     C                   WRITE     DETLEOB
     C                   Endif
     C                   MOVE      'Y'           XPFLAG
     C                   ENDIF
     C                   ENDIF                                                  LINEED GT 58
     C*
     C*
     C                   MOVEL(P)  $$DESC        DENOT1
     C                   MOVEL(P)  $DESC2        DENOT2
     C                   MOVEL(P)  $DESC3        DENOT3
     C                   MOVEL(P)  $DESC4        DENOT4
     C*
     C     DENOT1        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT1        XNOTE            70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLEOBN
     C                   Endif
     C*
     C                   ENDIF                                                  DENOT1 NE *BLANKS
     C*
     C     DENOT2        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT2        XNOTE
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLEOBD
     C                   Endif
     C*
     C                   ENDIF                                                  DENOT2 NE *BLANKS
     C*
     C     DENOT3        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT3        XNOTE
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLEOBD
     C                   Endif
     C*
     C                   ENDIF                                                  DENOT3 NE *BLANKS
     C*
     C     DENOT4        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT4        XNOTE
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLEOBD
     C                   Endif
     C                   ENDIF                                                  DENOT4 NE *BLANKS
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y' and xNote <> *blanks
     C                   Exsr      wrtDetCdNtWrk
     C                   Endif
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  $$DESC <> *BLANKS
     C*
     C                   ENDDO                                                  EOB ARRAY
     C                   ENDIF
     C                   If        isUseSpoolout
     C                   WRITE     BRKLIN
     C                   Endif
     C*
     C* GET/DISPLAY LINE NOTES
     C*
     C* DISPLAY 'CARRIER NOTES' HEADER IF NECESSARY
     C*
     C                   CLEAR                   XPFLAG
     C                   CLEAR                   XPFL1
     C                   CLEAR                   XLINE
     C                   CLEAR                   PRVLIN
     C                   CLEAR                   TMDSEQ
     C                   CLEAR                   WVAR1
     C                   CLEAR                   WVAR2
     C                   CLEAR                   WVAR3
     C                   CLEAR                   WVAR4
     C*
     C     DEKY3         SETLL     PDE01
     C     DEKY3         READE     PDE01                                  98
     C     *IN98         DOWEQ     *OFF
     C*
     C     XPFL1         IFNE      'Y'
     C                   MOVE      DEDSEQ        TMDSEQ
     C                   MOVE      'Y'           XPFL1             1
     C                   ENDIF
     C*
     C                   MOVE      XLINE         PRVLIN            5 0
     C*
     C     DEDSEQ        SUB       TMDSEQ        XLINE
     C                   ADD       1             XLINE
     C*
     C     XLINE         IFNE      PRVLIN
     C                   CLEAR                   XNO1
     C                   ENDIF
     C*
     C     DEPRNT        IFNE      'N'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFEQ      *BLANKS
     C     DEEOBCD       IFEQ      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*
     C                   MOVE      DENOT1        WVAR1            70
     C                   MOVE      DENOT2        WVAR2            70
     C                   MOVE      DENOT3        WVAR3            70
     C                   MOVE      DENOT4        WVAR4            70
     C*
     C                   EXSR      SRWRAP
     C*
     C* START NEW PAGE IF CANNOT PRINT ENTIRE NOTE ON ONE PAGE
     C*
     C     CURLIN        ADD       5             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   CLEAR                   XPFLAG
     C                   ENDIF                                                  LINEED GT 58
     C*
     C*
     C     WRNOT1        IFNE      *BLANKS
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLLNOT
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT1        X1NOTE           70
     C                   ADD       1             XNO1
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLLINE
     C                   Endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT1 NE *BLANKS
     C*
     C     WRNOT2        IFNE      *BLANKS
     C*
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLLNOT
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT2        X1NOTE           70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLNOT1
     C                   Endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT2 NE *BLANKS
     C*
     C     WRNOT3        IFNE      *BLANKS
     C*
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLLNOT
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT3        X1NOTE           70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLNOT1
     C                   Endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT3 NE *BLANKS
     C*
     C     WRNOT4        IFNE      *BLANKS
     C*
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLLNOT
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT4        X1NOTE           70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLNOT1
     C                   Endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT4 NE *BLANKS
     C*
     C                   ENDIF                                                  DEEOB = *BLANKS
     C                   ENDIF
     C*
     C     DEKY3         READE     PDE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     XPFLAG        IFNE      *BLANKS
     C                   If        isUseSpoolout
     C                   WRITE     BRKLIN
     C                   Endif
     C                   ENDIF
     C* GET/DISPLAY HEADER NOTES
     C*
     C                   CLEAR                   XPFLAG
     C                   CLEAR                   XNO1
     C                   CLEAR                   WVAR1
     C                   CLEAR                   WVAR2
     C                   CLEAR                   WVAR3
     C                   CLEAR                   WVAR4
     C*
     C     EEKY          CHAIN     PEE01                              98
     C*
     C* DISPLAY 'GENERAL CARRIER NOTES' HEADER IF NECESSARY
     C*
     C     *IN98         DOWEQ     *OFF
     C     EEPRNT        IFNE      'N'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** EEEOB         IFEQ      *BLANKS
     C     EEEOBCD       IFEQ      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*
     C                   MOVE      EENOT1        WVAR1
     C                   MOVE      EENOT2        WVAR2
     C                   MOVE      EENOT3        WVAR3
     C                   MOVE      EENOT4        WVAR4
     C*
     C                   EXSR      SRWRAP
     C*
     C* START NEW PAGE IF CANNOT PRINT ENTIRE NOTE ON ONE PAGE
     C*
     C     CURLIN        ADD       5             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   CLEAR                   XPFLAG
     C                   ENDIF                                                  LINEED GT 58
     C*
     C     WRNOT1        IFNE      *BLANKS
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLHNOT
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT1        X1NOTE           70
     C                   ADD       1             XNO1
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLNOH1
     C                   Endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT1 NE *BLANKS
     C*
     C     WRNOT2        IFNE      *BLANKS
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLHNOT
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT2        X1NOTE
     C                   EXSR      NWPG1
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETLNOT1
     C                   Endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT2 NE *BLANKS
     C*
     C*
     C     WRNOT3        IFNE      *BLANKS
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLHNOT
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT3        X1NOTE
     C                   EXSR      NWPG1
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETLNOT1
     C                   Endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT3 NE *BLANKS
     C*
     C     WRNOT4        IFNE      *BLANKS
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     DETLHNOT
     C                   Endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT4        X1NOTE
     C                   EXSR      NWPG1
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETLNOT1
     C                   Endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT4 NE *BLANKS
     C*
     C                   ENDIF                                                  EEEOB = *BLANKS
     C                   ENDIF
     C*
     C     EEKY          READE     PEE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     XPFLAG        IFNE      *BLANKS
     C                   If        isUseSpoolout
     C                   WRITE     BRKLIN
     C                   Endif
     C                   ENDIF
     C*************************************************************************
     C*
     C* GET COMPANY INFORMATION - BASED ON ALTERNATE ADDRESS FLAG
     C*
     C     F1KY          CHAIN     PF101                              98
     C     *IN98         IFEQ      *OFF
     C     F1FLG1        IFEQ      'Y'
     C                   MOVEL(P)  F1ANME        XXANME
     C                   MOVEL(P)  XXANME        XXCONM           40
     C                   MOVEL(P)  F1AAD1        XXAAD1
     C     F1AAD2        IFNE      *BLANKS
     C                   MOVEL(P)  F1AAD2        XXAAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF                                                  F1AAD2 NE *BLANKS
     C                   MOVEL(P)  F1ACTY        XXCTY
     C                   MOVE      F1AST         XXSTAT
     C                   MOVE      F1AZIP        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2           40
     C                   MOVEL     F1AARA        XXAREA
     C                   MOVEL     F1APHN        XXPHN1            3
     C                   MOVE      F1APHN        XXPHN2            4
     C*
     C     *IN52         IFEQ      *ON
     C                   MOVEL     SVFARA        XXFARA
     C                   MOVEL     SVFPH1        XXFPH1            3
     C                   MOVE      SVFPH2        XXFPH2            4
     C                   ENDIF
     C*
     C*** I had to create dup variables to satisfy prtf requirements.
     C                   MOVEL(P)  XXCSZ2        X1CSZ2
     C                   MOVEL     XXAREA        X1AREA
     C                   MOVEL     XXPHN1        X1PHN1
     C                   MOVE      XXPHN2        X1PHN2
     C*
     C                   ENDIF                                                  FLFLG1 = 'Y'
     C     F1FLG1        IFEQ      'N'
     C     C1KY          CHAIN     PC101                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  C1NAME        XXANME
     C                   MOVEL(P)  C1NAME        XXCONM
     C                   MOVEL(P)  C1ADR1        XXAAD1
     C     C1ADR2        IFNE      *BLANKS
     C                   MOVEL(P)  C1ADR2        XXAAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF
     C                   MOVEL(P)  C1CTY         XXCTY
     C                   MOVEL     C1ST          XXSTAT
     C                   MOVE      C1ZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2
     C                   MOVE      C1AREA        XXAREA
     C                   MOVEL     C1PHN#        XXPHN1
     C                   MOVE      C1PHN#        XXPHN2
     C                   MOVEL(P)  XXCSZ2        X1CSZ2
     C                   MOVEL     XXAREA        X1AREA
     C                   MOVEL     XXPHN1        X1PHN1
     C                   MOVE      XXPHN2        X1PHN2
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  F1FLG1 = 'N'
     C*
     C* START NEW PAGE IF CANNOT PRINT ENTIRE ADDRESS ON ONE PAGE
     C*
     C     CURLIN        ADD       6             LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   If        isUseSpoolout
     C                   WRITE     DETL10
     C                   Endif
     C*
     C* Get Division insured  number from Group info file.
     C*
     C                   MOVE      EOFND         KKFND
     C                   MOVE      'FL'          KF2ST
     C*
     C     F2KY          CHAIN     PF201                              98
     C     *IN98         IFEQ      *OFF
     C     F25N01        ANDNE     *ZEROS
     C                   MOVE      F25N01        XXDIV
     C*
     C                   If        isUseSpoolout
       //*** Beg Chg *** 08/22/22 *****************************
     C***                WRITE     DETLDIV
     C***                WRITE     BRKLIN
       If *in53 = *on;
          write DETLDIV;
          write BRKLIN;
       Endif;
       //*** End Chg *** 08/22/22 *****************************
     C                   Endif
     C                   ENDIF
     C*
     C*** Write to Scriptura Footer Info Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtFtWrk
     C                   Endif
     C*
     C* GET STATE COMMENTS
     C*
     C                   If        isUseSpoolout
     C                   WRITE     SPACE
     C                   Endif
     C                   Z-ADD     EOFRDT        KKEFDT
     C     F4KY2         SETGT     PF401
     C     F4KY1         READPE    PF401                                  98
     C*
     C* SKIP INACTIVE COMMENTS
     C*
     C*
     C     *IN98         DOWEQ     *OFF
     C     F4STAT        IFNE      'I'
     C                   Z-ADD     0             NUMLIN
     C*
     C* LOAD VALUES FOR LINES OF STATE COMMENTS
     C*
     C                   EXSR      FILLS
     C*
     C* DETERMINE THE NUMBER OF LINES THAT WILL BE NEEDED
     C*
     C                   EXSR      NMBRLN
     C*
     C     NUMLIN        ADD       CURLIN        LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   If        isUseSpoolout
     C                   WRITE     SPACE
     C                   Endif
     C                   ENDIF                                                  LINEED GT 58
     C*
     C* CHANGE FONT TO SMALLER PRINT IF NEED TO PRINT STATE COMMENTS
     C*
     C* PRINT LINES FOR COMMENTS
     C*
     C                   EXSR      PRTSC
     C                   ENDIF                                                  STAT <> 'I'
     C*
     C     F4KY1         READPE    PF401                                  98
     C*
     C     *IN98         IFEQ      *OFF
     C                   If        isUseSpoolout
     C                   WRITE     SPACE
     C                   Endif
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* GET GROUP COMMENTS BASED ON FROM SERVICE DATE IN FAEOP
     C*
     C                   If        isUseSpoolout
     C                   WRITE     SPACE
     C                   Endif
     C*
     C     F3KY2         SETGT     PF301
     C     F3KY1         READPE    PF301                                  98
     C*
     C* SKIP INACTIVE COMMENTS
     C*
     C     *IN98         IFEQ      *OFF
     C     F3STAT        ANDEQ     'I'
     C*
     C     *IN98         DOWEQ     *OFF
     C     F3STAT        ANDEQ     'I'
     C     F3KY1         READPE    PF301                                  98
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     0             NUMLIN
     C*
     C* FILL LINE VALUES FOR GROUP COMMENTS
     C*
     C                   EXSR      FILLG
     C*
     C* DETERMINE THE NUMBER OF LINES FOR GROUP
     C*
     C                   EXSR      NMBRLN
     C*
     C     NUMLIN        ADD       CURLIN        LINEED
     C     LINEED        IFGT      58
     C                   SETON                                        01
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   EXSR      PRTGC
     C*
     C* DETERMINE THE NUMBER OF LINES FOR GROUP
     C*
     C                   CLEAR                   GRCOMM
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  *IN98 = *OFF
       //*** Beg Add *** 08/22/22 ********************************
       // Additional information requested by Michigan
       If EOast = 'MI';
          Exsr miInfo;
       Endif;
       // Additional information requested by Louisiana Pharmacy Bills
       If EOast = 'LA' and ##clt# = '133' and EObTyp = 'X';
          Exsr laInfo;
       Endif;
       //*** End Add *** 08/22/22 ********************************
     C*
     C                   ENDSR
     C*--------------------------------------------------------------------
     C* SRWRAP -  WRAPPING LOGIC SO THAT WORDS DON'T CUT OFF
     C*--------------------------------------------------------------------
     C     SRWRAP        BEGSR
     C*
     C* If there is no blank in the FIRST note , then we don't wrap
     C*
     C     ' '           SCAN      TNOTE         POS                      98
     C     *IN98         IFNE      *ON
     C     POS           ORGT      70
     C                   MOVEL     WVAR1         WRNOT1
     C                   MOVEL     WVAR2         WRNOT2
     C                   MOVEL     WVAR3         WRNOT3
     C                   MOVEL     WVAR4         WRNOT4
     C                   ELSE
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     1             STR               3 0
     C                   Z-ADD     0             END               3 0
     C                   Z-ADD     0             N1                3 0
     C                   Z-ADD     1             LOOP              1 0
     C*
     C     LOOP          DOWNE     4
     C*
     C     70            ADD       STR           END               3 0
     C*
     C     END           IFGE      210
     C     1             SUBST     WVAR4:1       XXBLK
     C                   Z-ADD     210           END
     C     END           DOWNE     0
     C     XXBLK         IFNE      ' '
     C                   SUB       1             END
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C     1             SUBST     TNOTE:END     XXBLK
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C     END           DOWNE     0
     C*
     C     1             SUBST     TNOTE:END     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             END
     C*                    SUB  1         X1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C     END           SUB       STR           N1
     C     N1            IFGT      0
     C     N1            SUBST(P)  TNOTE:STR     XNOTE
     C                   ELSE
     C     70            SUBST(P)  TNOTE:STR     XNOTE
     C                   ENDIF
     C*
     C** REMOVE LEADING BLANKS FROM THE NOTE
     C     ' '           CHECK     XNOTE         POS                      98
     C     *IN98         IFEQ      *ON
     C     POS           ANDGT     *ZEROS
     C*
     C                   CLEAR                   TMP1
     C                   MOVEL     XNOTE         TMP1            140
     C                   CLEAR                   XNOTE
     C*
     C     70            SUBST(P)  TMP1:POS      XNOTE
     C                   ENDIF
     C*
     C                   SELECT
     C     LOOP          WHENEQ    1
     C                   MOVEL     XNOTE         WRNOT1
     C     LOOP          WHENEQ    2
     C                   MOVEL     XNOTE         WRNOT2
     C     LOOP          WHENEQ    3
     C                   MOVEL     XNOTE         WRNOT3
     C                   ENDSL
     C*
     C     END           IFGT      0
     C                   Z-ADD     END           STR
     C     N1            IFLT      70
     C                   ADD       1             STR
     C                   ENDIF
     C                   ELSE
     C                   ADD       70            STR
     C                   ENDIF
     C*
     C*
     C                   ADD       1             LOOP
     C                   ENDDO
     C                   Z-ADD     0             POS               3 0
     C                   MOVE      *BLANKS       TMNOT1          140
     C*
     C     ' '           CHECKR    TNOTE:210     POS                      98
     C     *IN98         IFEQ      *ON
     C     POS           SUB       STR           POS
     C                   ADD       1             POS
     C     POS           IFGT      *ZEROS
     C     POS           SUBST     TNOTE:STR     TMNOT1
     C                   ENDIF
     C                   ENDIF
     C*
     C     ' '           CHECKR    TMNOT1:140    POS                      98
     C     *IN98         IFEQ      *ON
     C                   CAT(P)    WVAR4:0       TMNOT1
     C                   ELSE
     C                   MOVEL     WVAR4         TMNOT1
     C                   ENDIF
     C*
     C     70            SUBST(P)  TMNOT1:1      WRNOT4
     C*
     C** REMOVE LEADING BLANKS FROM THE NOTE
     C*
     C     ' '           CHECK     WRNOT4        POS                      98
     C     *IN98         IFEQ      *ON
     C     POS           ANDGT     *ZEROS
     C*
     C                   CLEAR                   TMP1
     C                   MOVEL     WRNOT4        TMP1
     C                   CLEAR                   WRNOT4
     C*
     C     70            SUBST(P)  TMP1:POS      WRNOT4
     C                   ENDIF
     C*
     C                   ENDIF                                                  ' ' NOT FOUND
     C                   ENDSR
      /Free
       //-------------------------------------------------------------
       // WrtDetCdWrk -  Write to the Detail EOB Code workfile
       //--------------------------------------------------------------
           Begsr WrtDetCdWrk;
             clear pDec01;
             DECxml = xmlFile;
             DECco# = EOco#;
             DECsub# = EOsub#;
             DECfnd = EOfnd;
             DECfyr = EOfyr;
             DECcase = EOcase;
             DECeob# = EOeob#;
             DECrcn# = EOrcn;
             DECemp# = EMemp#;
             DECdiv = EMdiv;
             DECdSeq = CAdSeq;
             DEClin# = CAlin#;
             //*** Beg Chg *** 07/18/23 **************************************
             //*** DECeob  = DEeob;
             DECeob  = DEeobCd;
             //*** End Chg *** 07/18/23 **************************************
             DECnote1 = DEnot1;
             DECnote2 = DEnot2;
             DECnote3 = DEnot3;
             DECnote4 = DEnot4;
             DECiDt   = xxIdt;
             Write pDEC01;
           Endsr;
       //-------------------------------------------------------------
       // WrtEOBTotWrk -  Write to the EOB Total Amount workfile
       //--------------------------------------------------------------
           Begsr WrtEobTotWrk;
             clear pTot01;
             TOTxml = xmlFile;
             TOTco# = EOco#;
             TOTsub# = EOsub#;
             TOTfnd = EOfnd;
             TOTfyr = EOfyr;
             TOTcase = EOcase;
             TOTeob# = EOeob#;
             TOTrcn# = EOrcn;
             TOTemp# = EMemp#;
             TOTdiv = EMdiv;
             TOTchrg = EOchrg;
             TOTmra  = EOmra;
             TOTothr = EOothr;
             TOToth2 = EOoth2;
             TOTnDis = EOnDis;
             TOTpaid = EOpaid;
             TOTpPaid = x1Paid;
             TOTthPaid = x2Paid;
             TOTtax = EAtax;
             TOToCa1 = EAoCa1;
             TOTiDt   = xxIdt;
             Write pTOT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtDgWrk -  Write to the EOB Diagnosis Header WorkFile
       //--------------------------------------------------------------
           Begsr WrtDgWrk;
             clear pDg01;
             DGxml = xmlFile;
             DGco# = EOco#;
             DGsub# = EOsub#;
             DGfnd = EOfnd;
             DGfyr = EOfyr;
             DGcase = EOcase;
             DGeob# = EOeob#;
             DGRcn# = EOrcn;
             DGemp# = EMemp#;
             DGdiv = EMdiv;
             xxICDind = ' ';
             If EObtyp = 'N' and EHicdInd = 'Y';
                xxICDind = 'Y';
             Endif;
             If EObtyp = 'H' and EUicdInd = 'Y';
                xxICDind = 'Y';
             Endif;
             If EObtyp = 'D' and EDicdInd = 'Y';
                xxICDind = 'Y';
             Endif;
             if xxDs1 <> ' ';
                If xxIcdind = 'Y';
                   DGdNum = 'A';
                Else;
                   DGdNum = '1';
                Endif;
                DGdiag = xxDs1;
                DGdesc = Desc1;
                DGiDt  = xxIdt;
                Write pDG01;
             endif;
             if xxDs2 <> ' ';
                If xxIcdind = 'Y';
                   DGdNum = 'B';
                Else;
                   DGdNum = '2';
                Endif;
                DGdiag = xxDs2;
                DGdesc = Desc2;
                DGiDt  = xxIdt;
                Write pDG01;
             endif;
             if xxDs3  <> ' ';
                If xxIcdind = 'Y';
                   DGdNum = 'C';
                Else;
                   DGdNum = '3';
                Endif;
                DGdiag = xxDs3;
                DGdesc = Desc3;
                DGiDt  = xxIdt;
                Write pDG01;
             endif;
             if xxDs4  <> ' ';
                If xxIcdind = 'Y';
                   DGdNum = 'D';
                Else;
                   DGdNum = '4';
                Endif;
                DGdiag = xxDs4;
                DGdesc = Desc4;
                DGiDt  = xxIdt;
                Write pDG01;
             Endif;
             if xxDsE  <> ' ';
                DGdNum = 'E';
                DGdiag = xxDsE;
                DGdesc = DescE;
                DGiDt  = xxIdt;
                Write pDG01;
             Endif;
           Endsr;
       //-------------------------------------------------------------
       // WrtHdWrk -  Write to the EOB header workfile
       //--------------------------------------------------------------
           Begsr WrtHdWrk;
             clear pHd01;
             HDxml = xmlFile;
             HDco# = EOco#;
             HDsub# = EOsub#;
             HDfnd = EOfnd;
             HDfyr = EOfyr;
             HDcase = EOcase;
             HDeob# = EOeob#;
             HDRcn# = EOrcn;
             HDemp# = EMemp#;
             HDdiv = EMdiv;
             HDgNam = x1Name;
             HDeNam = x1Name;
             HDrvw# = xxTrid;
             HDss#  = xxss#1 + '-' + xxss#2 + '-' + xxss#3;
             HDcdName = xxName;
             monitor;
               HDacDt = %dec(%date(xxAcDt: *usa));
             on-error;
               HDacDt = 0;
             endmon;
             HDclaim# = %Char(EOsub#) + %Editc(EOfnd:'X') + %Editc(EOfyr:'X')+
                        %Editc(EOcase:'X');
             HDcxHex = x1Hex;
             HDmUser = xxMusr;
             HDacct = xxAcct;
             HDphar = xxPhar;
             HDbNPI = xxBnpi;
             Monitor;
               HDtrDt = %dec(%date(xxTrDt: *usa));
             On-error;
               HDtrDt = 0;
             endmon;
             HDfein = EOfein;
             HDmod  = EOmod;
             HDpNam = EOpNam;
             HDlic# = xxLic#;
             monitor;
               HDcrDt = %dec(%date(xxCrDt: *usa));
             on-error;
               HDcrDt = 0;
             endmon;
             monitor;
               HDprDt = %dec(%date(DSdate: *usa));
             on-error;
               HDprDt = 0;
             endmon;
             HDadr1 = EOadr1;
             HDadr2 = EOadr2;
             HDcty  = EOcty;
             HDst = EOst;
             HDzip = EOzip;
             If *in32;
               HDcsz = x1Csz1;
             Else;
               HDcsz = xxCsz1;
             Endif;
             monitor;
               HDrcDt = %dec(%date(xxRcDt: *usa));
             on-error;
               HDrcDt = 0;
             endmon;
             HDckDtHd = hdrChkDate;
             If *in96;
               HDwcPd = 'N/A';
             Else;
               monitor;
                 HDwcPd = %char(%date(xxWcPd: *usa));
               on-error;
                 HDwcPd = *blanks;
               endmon;
             Endif;
             HDchk#Hd = hdrCheck#;
             If *in96;
               HDwcPc = 'N/A';
             Else;
               HDwcPc = %Char(EAwcPc);
             Endif;
             HDdnum1 = xxNum1;
             HDdnum2 = xxNum2;
             HDdnum3 = xxNum3;
             HDdnum4 = xxNum4;
             HDds1  = xxDs1;
             HDDesc1 = Desc1;
             HDds2  = xxDs2;
             HDDesc2 = Desc2;
             HDds3  = xxDs3;
             HDDesc3 = Desc3;
             HDds4  = xxDs4;
             HDDesc4 = Desc4;
             monitor;
               HDfrDt = %dec(%date(xxFrDt: *usa));
             on-error;
               HDfrDt = 0;
             endmon;
             monitor;
               HDtoDt = %dec(%date(xxToDt: *usa));
             on-error;
               HDtoDt = 0;
             endmon;
             HDcxHMra = CXhMra;
             HDcxHOt1 = CXhOt1;
             HDcxHOt2 = CXhOt2;
             HDcxHNet = CXhNet;
             HDiDt   = xxIdt;
             HDast   = EOast;
             if EOrcn > 0;
               HDrcn = 'Y';
             endif;
             Write pHD01;
           Endsr;
       //-------------------------------------------------------------
       // WrtDetNtWrk -  Write to the Detail EOB Note workfile
       //--------------------------------------------------------------
           Begsr WrtDetNtWrk;
             clear pDnt01;
             DNTxml = xmlFile;
             DNTco# = EOco#;
             DNTsub# = EOsub#;
             DNTfnd = EOfnd;
             DNTfyr = EOfyr;
             DNTcase = EOcase;
             DNTeob# = EOeob#;
             DNTRcn# = EOrcn;
             DNTemp# = EMemp#;
             DNTdiv = EMdiv;
             DNTdSeq= DEdSeq;
             DNTseq#= DEseq#;
             //*** Beg Chg *** 07/18/23 **************************************
             //*** DNTeob  = DEeob;
             DNTeob  = DEeobCd;
             //*** End Chg *** 07/18/23 **************************************
             DNTnote = x1Note;
             DNTiDt   = xxIdt;
             Write pDNT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtDetCdNtWrk -  Write to the Detail EOB Code/Note workfile
       //--------------------------------------------------------------
           Begsr WrtDetCdNtWrk;
             clear pDcnt01;
             DCNTxml = xmlFile;
             DCNTco# = EOco#;
             DCNTsub# = EOsub#;
             DCNTfnd = EOfnd;
             DCNTfyr = EOfyr;
             DCNTcase = EOcase;
             DCNTeob# = EOeob#;
             DCNTRcn# = EOrcn;
             DCNTemp# = EMemp#;
             DCNTdiv = EMdiv;
             DCNiDt   = xxIdt;
             Write pDCNT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtDtWrk -  Write to the Detail Line Items Workfile
       //--------------------------------------------------------------
           Begsr WrtDtWrk;
             clear pDt01;
             DTxml = xmlFile;
             DTco# = EOco#;
             DTsub# = EOsub#;
             DTfnd = EOfnd;
             DTfyr = EOfyr;
             DTcase = EOcase;
             DTeob# = EOeob#;
             DTRcn# = EOrcn;
             DTemp# = EMemp#;
             DTdiv = EMdiv;
             DTdseq= CAdSeq;
             DTlin#= CAlin#;
             monitor;
               DTfDat = %dec(%date(xxFdat: *usa));
             on-error;
               DTfDat = 0;
             endmon;
             monitor;
               DTtDat = %dec(%date(xxtdat: *usa));
             on-error;
               DTtDat = 0;
             endmon;
             DTcode = xxCPT4;
             DTpos = xxPos;
         // Build field for modifiers

          // Modifier 1 not blank - all combinations
             if xxMod1 <> ' ' and xxMod2 = ' ' and xxMod3 = ' '
                 and xxMod4 = ' ';
                DTmods = xxMod1;
             Endif;
             if xxMod1 <> ' ' and xxMod2 <> ' ' and xxMod3 = ' '
                 and xxMod4 = ' ';
                DTmods = xxMod1 + ' ' + xxMod2;
             Endif;
             if xxMod1 <> ' ' and xxMod2 <> ' ' and xxMod3 <> ' '
                 and xxMod4 = ' ';
                DTmods = xxMod1 + ' ' + xxMod2 + ' ' + xxMod3;
             Endif;
             if xxMod1 <> ' ' and xxMod2 <> ' ' and xxMod3 <> ' '
                 and xxMod4 <> ' ';
                DTmods = xxMod1 + ' ' + xxMod2 + ' ' + xxMod3 + ' '
                          + xxMod4;
             Endif;
             if xxMod1 <> ' ' and xxMod2 = ' ' and xxMod3 = ' '
                 and xxMod4 <> ' ';
                DTmods = xxMod1 + ' ' + xxMod4;
             Endif;
          // Modifier 1 blank  and 2 not blank - all combinations
             if xxMod1 = ' ' and xxMod2 <> ' ' and xxMod3 = ' '
                 and xxMod4 = ' ';
                DTmods = xxMod2;
             Endif;
             if xxMod1 = ' ' and xxMod2 <> ' ' and xxMod3 <> ' '
                 and xxMod4 = ' ';
                DTmods = xxMod2 + ' ' + xxMod3;
             Endif;
             if xxMod1 = ' ' and xxMod2 <> ' ' and xxMod3 <> ' '
                 and xxMod4 <> ' ';
                DTmods = xxMod2 + ' ' + xxMod3 + ' ' + xxMod4;
             Endif;
             if xxMod1 = ' ' and xxMod2 <> ' ' and xxMod3 = ' '
                 and xxMod4 <> ' ';
                DTmods = xxMod2 + ' ' +  xxMod4;
             Endif;
          // Modifier 1 blank and 2 blank and 3 not blank - all
          // combinations
             if xxMod1 = ' ' and xxMod2 = ' ' and xxMod3 <> ' '
                 and xxMod4 = ' ';
                DTmods = xxMod3;
             Endif;
             if xxMod1 = ' ' and xxMod2 = ' ' and xxMod3 <> ' '
                 and xxMod4 <> ' ';
                DTmods = xxMod3 + ' ' + xxMod4;
             Endif;
          // Modifier 1 blank and 2 blank and 3 blank and
          // 4 not blank - all combinations
             if xxMod1 = ' ' and xxMod2 = ' ' and xxMod3 = ' '
                 and xxMod4 <> ' ';
                DTmods = xxMod4;
             Endif;
          // All modifiers are blank - send blanks
             if xxMod1 = ' ' and xxMod2 = ' ' and xxMod3 = ' '
                 and xxMod4 = ' ';
                DTmods = *blanks;
             Endif;
         // Build diagnosis cross reference field

          // ICD9 Diag cross reference 1 not blank - all combinations
          if eobtyp = 'N' and EHIcdInd <> 'Y' or
             eobtyp = 'D' and EDIcdInd <> 'Y';
             if xxDrf1 <> ' ' and xxDrf2 = ' ' and xxDrf3 = ' '
                 and xxDrf4 = ' ';
                DTxRef = xxDrf1;
             Endif;
             if xxDrf1 <> ' ' and xxDrf2 <> ' ' and xxDrf3 = ' '
                 and xxDrf4 = ' ';
                DTxRef = xxDrf1 + ' ' + xxDrf2;
             Endif;
             if xxDrf1 <> ' ' and xxDrf2 <> ' ' and xxDrf3 <> ' '
                 and xxDrf4 = ' ';
                DTxRef = xxDrf1 + ' ' + xxDrf2 + ' ' + xxDrf3;
             Endif;
             if xxDrf1 <> ' ' and xxDrf2 <> ' ' and xxDrf3 <> ' '
                 and xxDrf4 <> ' ';
                DTxRef = xxDrf1 + ' ' + xxDrf2 + ' ' + xxDrf3 + ' '
                          + xxDrf4;
             Endif;
          // Diag cross reference 1 blank  and 2 not blank - all
          // combinations
             if xxDrf1 = ' ' and xxDrf2 <> ' ' and xxDrf3 = ' '
                 and xxDrf4 = ' ';
                DTxRef = xxDrf2;
             Endif;
             if xxDrf1 = ' ' and xxDrf2 <> ' ' and xxDrf3 <> ' '
                 and xxDrf4 = ' ';
                DTxRef = xxDrf2 + ' ' + xxDrf3;
             Endif;
             if xxDrf1 = ' ' and xxDrf2 <> ' ' and xxDrf3 <> ' '
                 and xxDrf4 <> ' ';
                DTxRef = xxDrf2 + ' ' + xxDrf3 + ' ' + xxDrf4;
             Endif;
          // Diag cross reference 1 blank and 2 blank and 3 not
          // blank - all combinations
             if xxDrf1 = ' ' and xxDrf2 = ' ' and xxDrf3 <> ' '
                 and xxDrf4 = ' ';
                DTxRef = xxDrf3;
             Endif;
             if xxDrf1 = ' ' and xxDrf2 = ' ' and xxDrf3 <> ' '
                 and xxDrf4 <> ' ';
                DTxRef = xxDrf3 + ' ' + xxDrf4;
             Endif;
          // Diag cross reference 1 blank and 2 blank and 3 blank and
          // 4 not blank - all combinations
             if xxDrf1 = ' ' and xxDrf2 = ' ' and xxDrf3 = ' '
                 and xxDrf4 <> ' ';
                DTxRef = xxDrf4;
             Endif;
          // All diag cross references are blank - send blanks
             if xxDrf1 = ' ' and xxDrf2 = ' ' and xxDrf3 = ' '
                 and xxDrf4 = ' ';
                DTxRef = *blanks;
             Endif;
          endif;
          // ICD10 Diag cross reference 1 not blank - all combinations
          if eobtyp = 'N' and EHIcdInd = 'Y' or
             eobtyp = 'D' and EDIcdInd = 'Y';
             DTxRef = xxDxrA + ' ' + xxDxrB + ' ' + xxDxrC + ' ' +
                      xxDxrD + ' ' + xxDxrE + ' ' + xxDxrF + ' ' +
                      xxDxrG + ' ' + xxDxrH + ' ' + xxDxrI + ' ' +
                      xxDxrJ + ' ' + xxDxrK + ' ' + xxDxrL;
             position = %Scan( ' ,': DTxRef);
          //Remove the extra blank and comma characters
             DoW position > 0;
               DTxRef = %Replace('': DTxRef: position:2);
               position = %Scan( ' ,': DTxRef: 1);
             EndDo;
          endif;

             DTchrg = CAchrg;
             DTmra  = CAmra;
             DTothr = CAothr;
             DToth2 = CAoth2;
             DTnDis = CAnDis;
             DTpaid = CApaid;
             DTdesc = xxPrDs;
             DTtax  = DAtax;
             DTunts = xxUnts;
             DTbCode = xxBild;
             if x1EOB <> ' ';
                DTeobCd = %trim(x1EOB);
                if x2EOB <> ' ';
                   DTeobCd = %trim(DTeobCd) + ' ' + %trim(x2EOB);
                   if x3EOB <> ' ';
                      DTeobCd = %trim(DTeobCd) + ' ' + %trim(x3EOB);
                      if x4EOB <> ' ';
                         DTeobCd = %trim(DTeobCd) + ' ' + %trim(x4EOB);
                      endif;
                   endif;
                else;
                   if x3EOB <> ' ';
                      DTeobCd = %trim(DTeobCd) + ' ' + %trim(x3EOB);
                   else;
                      if x4EOB <> ' ';
                         DTeobCd = %trim(DTeobCd) + ' ' + %trim(x4EOB);
                      endif;
                   endif;
                endif;
             else;
                if x2EOB <> ' ';
                   DTeobCd = %trim(x2EOB);
                else;
                   if x3EOB <> ' ';
                      DTeobCd = %trim(DTeobCd) + ' ' + %trim(x3EOB);
                   else;
                      if x4EOB <> ' ';
                         DTeobCd = %trim(DTeobCd) + ' ' + %trim(x4EOB);
                      endif;
                   endif;
                endif;
             endif;
             DTiDt   = xxIdt;
             Write pDT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtGenCarNtWrk - Write to the General Carrier Notes Workfile
       //--------------------------------------------------------------
           Begsr WrtGenCarNtWrk;
             clear pCn01;
             CNxml = xmlFile;
             CNco# = EOco#;
             CNsub# = EOsub#;
             CNfnd = EOfnd;
             CNfyr = EOfyr;
             CNcase = EOcase;
             CNeob# = EOeob#;
             CNRcn# = EOrcn;
             CNemp# = EMemp#;
             CNdiv = EMdiv;
             CNnote = x1note;
             CNiDt   = xxIdt;
             Write pCN01;
           Endsr;
       //-------------------------------------------------------------
       // WrtStNtWrk - Write to the State Notes Workfile
       //--------------------------------------------------------------
            Begsr WrtStNtWrk;
             clear pSn01;
             SNxml = xmlFile;
             SNco# = EOco#;
             SNsub# = EOsub#;
             SNfnd = EOfnd;
             SNfyr = EOfyr;
             SNcase = EOcase;
             SNeob# = EOeob#;
             SNrcn# = EOrcn;
             SNemp# = EMemp#;
             SNdiv = EMdiv;
             SNnote = stNote;
             SNiDt   = xxIdt;
             Write pSN01;
           Endsr;
       //-------------------------------------------------------------
       // WrtGpNtWrk - Write to the Group Notes Workfile
       //--------------------------------------------------------------
            Begsr WrtGpNtWrk;
             clear pGn01;
             GNxml = xmlFile;
             GNco# = EOco#;
             GNsub# = EOsub#;
             GNfnd = EOfnd;
             GNfyr = EOfyr;
             GNcase = EOcase;
             GNeob# = EOeob#;
             GNrcn# = EOrcn;
             GNemp# = EMemp#;
             GNdiv = EMdiv;
             GNnote = gpNote;
             GNiDt   = xxIdt;
             Write pGN01;
           Endsr;
       //-------------------------------------------------------------
       // WrtGdDetWrk - Write to the Group Summary Details Workfile
       //--------------------------------------------------------------
            Begsr WrtGpDetWrk;
             clear pGd01;
             GDxml = xmlFileSum;
             GDco# = EOco#;
             GDsub# = EOsub#;
             GDfnd = EOfnd;
             GDfyr = EOfyr;
             GDcase = EOcase;
             GDeob# = EOeob#;
             GDrcn# = EOrcn;
             GDemp# = EMemp#;
             GDdiv = EMdiv;
             GDeobChrg = xxChrg;
             GDeobMra = xxMra;
             GDeobothr = xxOthr;
             GDeoboth2 = xxOth2;
             GDeobNdis = xxNdis;
             GDeobPaid = xxPaid;
             GDfmtCase = xzCase;
             GDss#     = xxSS#;
             GDacDt    = xxAcDt;
             GDname    = xzPnam;
             GDeobIdt= xxIdt;
             Write pGD01;
           Endsr;
       //-------------------------------------------------------------
       // WrtGpTotWrk - Write to the Group Summary Totals Workfile
       //--------------------------------------------------------------
            Begsr WrtGpTotWrk;
             clear pGpt01;
             GTxml = xmlFileSum;
             GTco# = EOco#;
             GTsub# = HLsub#;
             GTfnd = HLfnd;
             GTeobCnt = xgEob#;
             GTeobChrg = xgChrg;
             GTeobMra = xgMra;
             GTeobOthr = xgOthr;
             GTeobOth2 = xgOth2;
             GTeobNdis = xgNdis;
             GTeobPaid = xgPaid;
             GTeobIdt = xxIdt;
             Write pGPT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtEOBSmWrk - Write to the Batch EOB Summary Workfile
       //--------------------------------------------------------------
            Begsr WrtEOBSmWrk;
             clear pSum01;
             SMxml = xmlFileSum;
             SMeobCnt = xtEob#;
             SMeobChrg = xtChrg;
             SMeobMra = xtMra;
             SMeobothr = xtOthr;
             SMeoboth2 = xtOth2;
             SMeobNdis = xtNdis;
             SMeobPaid = xtPaid;
             SMeobIdt = xxIdt;
             Write pSUM01;
           Endsr;
       //-------------------------------------------------------------
       // WrtRtWrk - Write to the EOB Routing Workfile
       //--------------------------------------------------------------
            Begsr WrtRtWrk;
             clear pRt01;
             RTxml = xmlFile;
             RTco# = EOco#;
             RTsub# = EOsub#;
             RTfnd = EOfnd;
             RTfyr = EOfyr;
             RTcase = EOcase;
             RTeob# = EOeob#;
             RTrcn# = EOrcn;
             RTemp# = EMemp#;
             RTdiv = EMdiv;
             RTuser = kkUser;
             RTuserNm = UPusNm;
             RTrpTitle  = ORtitl;
             RTpgm  = ORpNam;
             RTcpy1  = #Brnd1;
             RTcpy2  = #Brnd2;
             RTiDt   = xxIdt;
             Write pRT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtFtWrk - Write to the EOB Footer Workfile
       //--------------------------------------------------------------
            Begsr WrtFtWrk;
             clear pFt01;
             FTxml = xmlFile;
             FTco# = EOco#;
             FTsub# = EOsub#;
             FTfnd = EOfnd;
             FTfyr = EOfyr;
             FTcase = EOcase;
             FTeob# = EOeob#;
             FTrcn# = EOrcn;
             FTemp# = EMemp#;
             FTdiv = EMdiv;
             FTnwM = xxNwM;
             FTnw2 = xxNw2;
             FTnw3 = xxNw3;
             FTaNme = xxANme;
             FTaAd1 = xxAAd1;
             FTaAd2 = xxAAd2;
             FTaSt = F1aSt;
             FTaCty = F1aCty;
             FTaZip = F1aZip;
             FTarea = xxArea;
             FTaPhn = xxphn1 + xxphn2;
             FTfAra = xxFAra;
             FTfax  = xxFph1 + xxFph2;
             FTins# = xxDiv;
             FTdrg = xxDrg#;
             FTdgDs = xxDgDs;
             FTadDt = xxAdDt;
             FTscPf = xxScPf;
             FTscPt = xxScPt;
             If EObtyp = 'H';
               If EUicdInd = 'Y';
                 FTprc1 = EUprc1001;
                 FTprc2 = EUprc1002;
                 FTprc3 = EUprc1003;
                 FTprc4 = EUprc1004;
               Else;
                 FTprc1 = EUprc1;
                 FTprc2 = EUprc2;
                 FTprc3 = EUprc3;
                 FTprc4 = EUprc4;
               Endif;
             Endif;
             FTprd2 = xxPrD2;
             FTprd3 = xxPrD3;
             FTprd4 = xxPrD4;
             monitor;
               FTsbDt = %dec(%date(xxSbDt: *usa));
             on-error;
               FTsbDt = 0;
             endmon;
             FTicn = xxICN;
             FTicnDs = xxICDs;
             FTblTy = xxBlTy;
             Chain (EUCo#:btTbid:xxBlty) ftgtl;
             if %found(ftgtl) and GTstat <> 'I';
                FTbDsc = GTdesc;
             endif;
             FTcNam = xxName;
             FTcAdr1 = xxAad1;
             FTcAdr2 = xxAad2;
             FTcCsz = xxCsz2;
             FTcCty = CDcty;
             FTcSt = CDst;
             FTcZip = CDzip;
             FTiDt   = xxIdt;
             Write pFT01;
           Endsr;
      /End-Free
       //----------------------------------------------------------------------
       // WrtEOBRhWrk - Write to the Batch EOB Summary Report Headings Workfile
       //----------------------------------------------------------------------
      /free
            Begsr WrtEOBRhWrk;
             clear pRh01;
             RHxmlfile = xmlFileSum;
             RHcoNm    = xxCoNm;
             RHtitl    = x2Titl;
             RHsub#    = EOsub#;
             RHsbNm    = xxSbNm;
             RHfnd     = EOfnd;
             RHfnNm    = x1Name;
             RHhMra    = xxHmra;
             RHhOt1    = xxHoT1;
             RHhOt2    = xxHoT2;
             RHhNet    = xxHnet;
             RHiDt   = xxIdt;
             Write pRH01;
           Endsr;
      /End-Free
     P*====================================================================
     P* Determine if the object exists
     P*====================================================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
     P*====================================================================
     P* Build the EOB Note Field for Scriptura
     P*====================================================================
     PGenEOBnote       b
     DGenEOBnote       pi           280
      /Free
         Return GenEOBnote;
      /End-Free
     PGenEOBnote       e
** (NWMSG)
"Network reductions have been made per
contractual agreement."
