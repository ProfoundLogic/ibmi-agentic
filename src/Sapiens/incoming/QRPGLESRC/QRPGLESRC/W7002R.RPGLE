     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7002R - CLAIMS CHECKS
     H*****************************************************************
     H*               -------------------                             *
     H*               * PROGRAMMER NOTE *                             *
     H*               -------------------                             *
     H**** IMPORTANT *** IMPORTANT *** IMPORTANT *** IMPORTANT ****** *
     H*                                                               *
     H* THIS PROGRAM USES THE SUBMIT DATE OF THE BATCH TO PRINT THE   *
     H* THE CHECK DATE ON THE EOMB.  IF THIS PROGRAM IS USED FOR A    *
     H* NON-BATCH RUN, I.E. FILECOPY FROM WCMS MENU, THE CHECK DATE   *
     H* MUST COME FROM THE FAEAP FILE.                                *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/18/23 C:342 Project: NY060                                *
     H*                 NY Compliance: CARC/RARC Update               *
     H*                 RM: Added Modernized Output to FILECOPY       *
     H*                                                               *
     H*  08/22/22 C:333 Project: MC443                                *
     H*                 Modernized EOB Print.                         *
     H*                                                               *
     H*  11/18/21 C:062 Ticket: 98681(146)                            *
     H*                 Populate descriptions.                        *
     H*                                                               *
     H*  06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H*  07/23/19 C:079 Project: 152MAP                               *
     H*                 Display Review # for client 152               *
     H*                 Suppress insurer phone number for client 152  *
     H*
     H*  06/14/19 C:053 PROJECT: 152WAO *** REMOVED DUE TO CLIENT PRINTER **
     H*                 Print 1st signature line for S1 Medical
     H*
     H*  04/10/19 C:062 PROJECT: 152WAB
     H*                 Apply CSI mod to S1 Medical
     H*
     H*  03/06/19 C:180 PROJECT: 126WFR
     H*                 Add dashes to Void after x days for Client 126.
     H*
     H*  08/21/18 C:180 PROJECT: 126WFR
     H*                 Use payee's font for VOID AFTER x DAYS.
     H*                 Add variable text on check.
     H*
     H*  07/25/18 C:053 PROJECT: 129WVC
     H*                 Initialize special processing.
     H*
     H*  06/18/18 C:062 PROJECT: 129WVB
     H*                 Move the micr line to the left for CoveRisk
     H*
     H*  05/21/18 C:062 PROJECT: 129WVC
     H*                 Use print blank lines flag.
     H*
     H*  04/11/18 C:053 PROJECT: 129WVC
     H*                 Special printing of lines & no signature on $ limit
     H*                                                               *
     H*  07/19/17 C:079 PROJECT: SY395                                *
     H*                 Add adj state and recon flag to header file   *
     H*                 Remove dashes from claim number               *
     H*                                                               *
     H*  06/26/17 C:079 PROJECT: SY395                                *
     H*                 Add Scriptura xmlprep and S4401C logic.       *
     H*
     H*  06/01/17 C: 62 PROJECT SY371 From 12/09/15 changes
     H*                 Add Scriptura processing.
     H*
     H*  05/30/17 C: 79 Project SY395
     H*                 Add EOB Stub workfiles for Scriptura
     H*
     H*  10/20/16 C: 53 PROJECT 34WCA
     H*                 Add Pay To info that was removed from clients
     H*                 version of overlay
     H*
     H*  03/10/16 C:309 SUPPORT 39961
     H*                 Recompile only (done change in copy book OCHKDETG)
     H*
     H*  01/21/16 C: 53 SUPPORT 39472
     H*                 BTB Check changes for CLT065 Use *IN23 for ERS
     H*                 Apply prior items: 11600-Move amount down for box
     H*                                    12219-Move MICR line down for bank
     H*                                                               *
     H*  11/02/15 C:309   SUPPORT: 37884 (EOR missing information)    *
     H*
     H*  10/19/15 C:53  PROJECT: 126WFB
     H*                 Print Group change banner from tray 2
     H*
     H*  06/02/14 C:212 PROJECT: MC329_PH2                            *
     H*                 expand xxds1/2/3/4 to accomodate ICD10        *
     H*                                                               *
     H*  06/24/14 C:079 Redmine: 4395                                 *
     H*                 If client 129 print heading further down      *
     H*                                                               *
     H*  05/20/14 C:296 PROJECT: MC378                                *
     H*                 NDC Code Process Change                       *
     H*                                                               *
     H*  04/16/14 C:280 Support: 85366                                *
     H*                 add user open for fmicd10l1 and faedl         *
     H*                                                               *
     H*  11/25/13 C:282 PROJECT: 129WSC                               *
     H*                 Retrofit Changes for CltId 129 only.          *
     H*
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add ICD9 and ICD10 diagnosis desc population  *
     H*                                                               *
     H*  02/05/13 C:283 Support: WC046 81382                          *
     H*                 *IN78 Condition 1st Signature Line to Print.  *
     H*
     H*  06/11/12 C:277 PROJECT 114MER                                *
     H*                 Comment out all logic for date 11/25/02       *
     H*                                                               *
     H*  05/07/12 C:251 PROJECT 133MMH Priss 11                       *
     H*                 Print LA Pharmacy Claimant info on same page  *
     H*                                                               *
     H*  02/06/12 C:251 PROJECT: 133MMK                               *
     H*                 Print LA Claimant info on Check               *
     H*                                                               *
     H*  01/13/12 C: 62 SUPPORT WC063 78947
     H*                 Make sure FAH1L5 is openned user defined.
     H*                                                               *
     H*  12/13/11 C:191 Project: LA025                                *
     H*                 Print LA Parish tax on check stub             *
     H*                                                               *
     H*  07/14/11 C:257 SUPPORT MC139 77407                           *
     H*                 Change the EOB print heading for all EOBS     *
     H*                 with the exception of Florida EOBRs.          *
     H*                                                               *
     H*  05/23/11 C:251 PROJECT: FS389                                *
     H*                 Add Division number, Mask SSN for all clients *
     H*                 Change Logic for 'FL' bills.                  *
     H*                                                               *
     H*  05/05/11 C:251 PROJECT: NC032                                *
     H*                 Print Group Fax number for NC bills           *
     H*                                                               *
     H*  03/25/11 C: 53 SUPPORT 75860                                 *
     H*                 Check changes to MICR line for CLT146         *
     H*                 Change *IN20 to be used for base printing     *
     H*                                                               *
     H* 01/28/11 C:227 PROJECT: MC353                                 *
     H*                ADD PHARMACY# FOR PHARMACY BILLS               *
     H*                                                               *
     H* 09/29/10 C:257 PROJECT: 133MJN                                *
     H*                ADD PHARMACY TAX FIELD AND TAX INCLUDE FLAG    *
     H*                                                               *
     H* 03/11/10 C:227 PROJECT: FL083                                 *
     H*                ADD LOGIC TO ADD MODIFIERS                     *
     H*                                                               *
     H* 11/30/09 C:227 PROJECT: 135MMX (CLIENT SPECIFIC)              *
     H*                HES RECONCILIATION REPORT                      *
     H*                                                               *
     H* 09/18/09 C:79  PROJECT FL069                                  *
     H*                Use smaller bold font for state comments       *
     H*                                                               *
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                ALLOW FOR XML TYPE TRANSACTIONS                *
     H*                                                               *
     H* 05/27/09 C:33  Project: 114WTQ                                *
     H*                Mask the SS# for CLT114.                       *
     H*                                                               *
     H* 03/20/09 C:062 PROJECT: MC139 67349                           *
     H*                CONDITON USAGE OF FMPRP                        *
     H*                                                               *
     H* 09/19/08 C:197 PROJECT: 71WJE                                 *
     H*                CHANGE PRINTER ORDER OF CLAIM CHECKS           *
     H*                                                               *
     H* 08/27/08 C:062 PROJECT MC133 64957
     H*                CHECK TO SEE IF GROUP CHECK IS ACTIVE
     H*                                                               *
     H* 03/13/08 C:180 PROJECT 136MDK
     H*                Donot print EOB on check stub for clt 136.
     H*
     H* 01/09/08 C:47  SUPPORT 61258                                  *
     H*                The claim# on the check stub is only displaying*
     H*                2 digits for the fund year.  It should be 3.   *
     H*                                                               *
     H* 10/12/07 C:190 Project: WC1557                                *
     H*                Condition the load of field @@SS# on the       *
     H*                new flag.                                      *
     H*                   1. If COPSS# = 'Y', execute the current     *
     H*                      load processing.                         *
     H*                   2. If COPSS# = blank, load blanks to        *
     H*                      @@SS#.                                   *
     H*                                                               *
     H*  10/24/07 C:62  PROJECT 139MIF                                *
     H*                 Add network verbiage.                         *
     H*                                                               *
     H* 10/03/07 C:33 Project: 45WFO Back to Base                     *
     H*                *IN78 condition 1st signature line to print    *
     H*                                                               *
     H* 10/02/07 C:53 Support 59530                                   *
     H*               Correct tray issue for group eomb detail        *
     H*                                                               *
     H*  09/12/07 C:47  PROJECT FL069                                 *
     H*                 Print state comments in bold if requested.    *
     H*                                                               *
     H* 08/22/07 C:15 Project: 114WMF                                 *
     H*               Added code so the MICR line prints 1/8" lower   *
     H*               for client 114.                                 *
     H*                                                               *
     H*  08/07/07 C:189 PROJECT FL072                                 *
     H*                 Supply bills not pulling fee schedule rates   *
     H*                 from HCPCS/CPT file                           *
     H*                                                               *
     H*                                                               *
     H* 06/20/07 C:53 114WMF  BACK TO BASE-114WBY & 114WED            *
     H*             01/08/02 C:15 Project: 114WBY   Supp#: 30314      *
     H*               Only load the CMACL# on the EOMB for group 440. *
     H*             06/18/01 C:15 Project: 114WBY                     *
     H*               Added code to print CMACL# as the claim# if     *
     H*               not blank.                                      *
     H*             11/25/02 C:62  PROJECT 114MDC                     *
     H*               IF CODEREVIEW, GETEOB DESC FROM MESSAGE.        *
     H*             07/26/02 C:61  Project: 114WED                    *
     H*               Changed program to print 'Void after x days',   *
     H*               where x is BI50N3 (a value moved to a 3.0       *
     H*               numeric field for output purposes).  If the     *
     H*               number of days is zero, the verbiage will not   *
     H*               be printed on the checks.                       *
     H*                                                               *
     H*  05/18/07 C:188 SUPPORT 58395                                 *
     H*                 Add functionality to print all comments for   *
     H*                 state/effective date. Was only printing       *
     H*                 first comment found.                          *
     H*                                                               *
     H* 04/27/07 C:15  Project: 71WHI                                 *
     H*               -Chgd pgm to print 1st signature line for clt   *
     H*                71.                                            *
     H*               -71WCI: Add void verbiage.                      *
     H*               -71WCO: Removed "Paid by" text in OCHKDET2 and  *
     H*                       OFILDET2.                               *
     H*                                                               *
     H*04/25/07 C:053 SUPPORT 57936                                 *
     H*                Re-apply 16422 under original date 02/10/99   *
     H*           02/10/99 C:15 Support: WC090/16422                *
     H*             Changed pgm to use the second signature line if *
     H*             the base overlay is used.                       *
     H*                                                               *
     H*04/11/07 C:053 SUPPORT 57936                                 *
     H*                Check corrections for client 90               *
     H*                                                               *
     H*04/05/07 C:062 PROJECT 139MHG                                *
     H*                Print review number                           *
     H*                                                               *
     H*02/26/07 C:132 PROJECT 90WEY                                 *
     H*                Add "Void after 90 days" for client 090 to    *
     H*                move MOD back to base, like client 139.       *
     H*                                                               *
     H* 11/15/06 C:181 PROJECT 135MFX                                *
     H*                Note: C:079 tweeked this code slightly        *
     H*                Print Sub NTWK on EOB (Providence Property &  *
     H*                Casualty Co.                                   *
     H*                                                               *
     H* 08/31/06 C:15  Project: 139WFC                                *
     H*                Change to output to print the system date on   *
     H*                the checks instead of the batch date (client   *
     H*                flagged).                                      *
     H*                                                               *
     H* 08/23/06 C:163 Project: 139MCV                                *
     H*                Medical Bill Interface w/FirstHealth           *
     H*                                                               *
     H* 11/29/05 C:53  Project: 139WAD                                *
     H*                Format changes for void msg.                   *
     H*                *IN78 condition 1st signature line to print    *
     H*                                                               *
     H* 11/28/05 C:53  Project: 136WAH                                *
     H*                Format changes for payee # add void msg.
     H*                *IN15 condition xxx in numbers
     H*                *IN78 condition 1st signature line to print
     H*                                                               *
     H* 12/06/05 C:079 SUPPORT: NNNNN                                 *
     H*                Michigan EOMB Req's only print if EOB attached *
     H*                                                               *
     H* 11/17/05 C:163 PROJECT: MI002                                 *
     H*                Michigan EOMB Req's                            *
     H*                                                               *
     H* 07/22/05 C:062 PROJECT: WC132 47658                           *
     H*                Print claim info correctly.                    *
     H*                                                               *
     H* 04/25/05 C:156 PROJECT: 132WCN                                *
     H*                Print CA fraud statement on checks.            *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.   EOADR3                              *
     H*                                                               *
     H*  02/17/05 C:61 PROJECT MC266                                  *
     H*                Add printing of Procedure Code Modifier        *
     H*                FDDUP/DUPMOD for hospital bills.               *
     H*                                                               *
     H*  06/03/04 C:62 PROJECT FL046                                  *
     H*                USE EOB X-REF                                  *
     H*                                                               *
     H* 04/07/04 C:062 PROJECT: WC1220                                *
     H*                Add display detail proccess                    *
     H*                                                               *
     H* 10/20/03 C:53 Project: WC909                                  *
     H*               Added country info.                             *
     H*                                                               *
     H* 10/10/03 C:79 SUPPORT MC025 39479                             *
     H*               CORRECT SPELLING OF "CLAIMAINT"                 *
     H*                                                               *
     H* 09/12/03 C:79 SUPPORT MC025 39032                             *
     H*          C:61 CHECK STATUS WHEN READING FDF4P, FDF3P          *
     H*                                                               *
     H* 07/24/03 C:79 SUPPORT MC051 38217                             *
     H*               PRINT EXPANDED LICENSE# FIELD IF NOT BLANKS     *
     H*                                                               *
     H* 07/23/03 C:62 PROJECT TX022                                   *
     H*               FOR TEXAS BILLS PRINT THE X-REF INSTEAD OF THE  *
     H*               EOB CODE                                        *
     H*                                                               *
     H* 01/28/03 C:79  Support: MC055/35674                           *
     H*                Only print active EOB codes                    *
     H*                                                               *
     H* 05/14/02 C:15  Support: WC100/32510                           *
     H*                Added code to only open file FAEPL if WCMC is  *
     H*                installed.                                     *
     H*                                                               *
     H* 04/11/02 C:62  PROJECT FSMNT                                  *
     H*                Get header EOB description for header EOB file *
     H*                instead of using FTABL.  This will avoid       *
     H*                overiding and opening and closeing FTABL in    *
     H*                batch mode due to eombs for different state.   *
     H*                                                               *
     H* 02/22/02 C:62 PROJECT:MC250                                   *
     H*               LOAD EPPHAR INTO LICENCE NUMBER IF AST = FL     *
     H*               AND BILL TYPE IS 'X'.                           *
     H*                                                               *
     H* 01/07/02 C:62 PROJECT:FS235                                   *
     H*               MAKE SURE ONLY TO PRINT EOB/NOTES IF PRINT      *
     H*               FLAG IS "Y" OR " ".                             *
     H*                                                               *
     H* 11/19/01 C:15 Project: WC1020                                 *
     H*               Changed parm list for WACPL4.                   *
     H*                                                               *
     H* 07/27/01 C:15  Project: WC1020                                *
     H*                Removed code which resets the group name and   *
     H*                address to the WCMC group info.                *
     H*                                                               *
     H* 05/01/01 C:15  Project: WC1020                                *
     H*                Added the bank code (QQCKTP) to the parm list. *
     H*                Only checks for the specific bank code will be *
     H*                processed.                                     *
     H*                                                               *
     H* 04/18/01 C:79  Project: 96MDS                                 *
     H*                Make the provider bills version WCMS base      *
     H*                                                               *
     H*  11/22/00 C:62 PROJECT:FS212                                  *
     H*                EXPAND PATIENT ACCOUNT FIELD.                  *
     H*                                                               *
     H* 04/27/00 C:15  Support: WC071/20315                           *
     H*                Chgd pgm to use a full key when retrieving the *
     H*                EOMB record.                                   *
     H*                                                               *
     H* 04/26/00 C:15  Support: WC044/22642                           *
     H*                Chgd pgm to allow the EOMB to print for a      *
     H*                group type provider as long as the group type  *
     H*                is 'O'.  EOMB won't print is group type is 'N' *
     H*                or 'B'.                                        *
     H*                                                               *
     H* 04/19/00 C:15  Support: WC044/21961                           *
     H*                Chgd pgm to prevent checks, which were entered *
     H*                or released while the batch was running, from  *
     H*                printing.                                      *
     H*                                                               *
     H*                                                               *
     H* 03/06/00 C:15  Support: WC044/22096                           *
     H*                Chgd pgm to prevent overflow issue.            *
     H*                                                               *
     H*  02/08/00  C:079 Support: MC048/#21699                        *
     H*                  Move license number to XXLIC# based on Adj   *
     H*                  state                                        *
     H*                                                               *
     H* 01/26/00 C:118 SUPPORT: MC044/20791                           *
     H*                MOD SPACING OF PHN# OUTPUT                     *
     H*                                                               *
     H* 11/10/99 C:56  Support: MC044 #20192/20223                    *
     H*                Add clearing of work fields for batch printing *
     H*
     H* 10/01/99 C:56  MC152                                          *
     H*                Print the dates fo service range at top of each*
     H*                page during line item printing.  This will     *
     H*                allow the user to see dates of service without *
     H*                flipping back to previous page.                *
     H*
     H* 09/09/99 C:56  Project: MC208                                 *
     H*            1.  Add Date of Service range in header portion of *
     H*                bill.                                          *
     H*                                                               *
     H*            2.  Add 'Billed Code' on each line item.           *
     H*                                                               *
     H*  08/31/99 C:62  PROJECT: MC211                                *
     H*                 IMPLEMENTING NEW FIELD COMMENTS.              *
     H* 08/28/99 C:56 PROJECT: MC152                                  *
     H*               1. CLEAR RECON RECEIVED DATE SO WILL NOT SPILL  *
     H*                  OVER DURING BATCH
     H*                                                               *
     H*  08/17/99 C:15  Support: WC055/18543                          *
     H*                 Removed condition which required the reserve  *
     H*                 type to equal 'M' to print the EOMB on the    *
     H*                 check stub.                                   *
     H*                                                               *
     H* 07/26/99 C:56 MC208                                           *
     H*               1. Add network disclaimer                       *
     H*               2. Do not print medical examiner heading        *
     H*                  when 'medical examiner user value is '0'     *
     H*               3. Only EOB Reason codes for Header EOB codes.  *
     H*                                                               *
     H* 07/14/99 C:56 Support: MC072 #18835                           *
     H*               Move client address to top of EOMB.             *
     H*                                                               *
     H* 07/09/99 C:56 MC152                                           *
     H*               Temporarly remove font changes for Group and    *
     H*               State comments.                                 *
     H*                                                               *
     H* 06/30/99 C:15 Project: 25WLS                                  *
     H*               If the address line 2 which prints on the stub  *
     H*               is blank, do not print a blank line.  The city  *
     H*               state and zip should be moved up one line.      *
     H*               This change will involve OCHKDET2, also.        *
     H*                                                               *
     H*  05/05/99  C:56   MC152
     H*                   Change FAEAL to FAEAL1, duplicate logicals
     H*
     H* 04/22/99 C:56 Project: 25MLR                                  *
     H*            1. Change 'Patient' to 'Claimaint'                 *
     H*            2. Change 'Federal ID#' to 'Provider ID#'          *
     H*            3. Add 'Date Recvd'                                *
     H*            4. Display decimal version of Diagnosis Code       *
     H*            5. Replace display of Diagnosis Code on each line  *
     H*               item with the corresponding number from Diag.   *
     H*               listing in heading                              *
     H*            6. Display 'To Service Date' for all bill types    *
     H*            7. Move printing of Hospital specific information  *
     H*               after amount totals.  Also add print of proc-   *
     H*               edure codes with the Hospital specific info.    *
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
     H*                  CHANGE WHERE ADDRESS GET LOADED FROM         *
     H*                                                               *
     H*  02/16/99  C:56  PROJECT: 25MKH                               *
     H*                  Additional filed on EOMB for hospital bill   *
     H*                  Admission Date                               *
     H*                  Coverage                                     *
     H*                  Submission Date                              *
     H*                  Type of service                              *
     H*                  Type of Bill                                 *
     H*                                                               *
     H*  01/21/99 C:56  Project: MC152                                *
     H*                 Change printing of EOMB stub to Provider      *
     H*                 Bills version.                                *
     H*                                                               *
     H*  01/18/99 C:15  Support: WC021/16039                          *
     H*                 Changed code which determines which subroutine*
     H*                 to use to print the checks.  If WCMC was not  *
     H*                 installed and an EOB# was entered on the pmt, *
     H*                 the pgm would not print the check.            *
     H*                                                               *
     H*  12/11/98 C:31  Project: NC005
     H*                 Chg NC EOMB wording
     H*
     H*  11/16/98 C:15  Support: WC090/13926
     H*                 Changed the filecopy overflow to print from
     H*                 drawer 2.
     H*
     H*  10/29/98 C:15  Project:
     H*                 Re-compiled due to a change in OCHKCHECK.
     H*
     H*  10/26/98 C:79  Support: 13503
     H*                 Add code to exclude FSMRA from being added to
     H*                 the Amount Billed column.
     H*
     H*  10/09/98 C:79  Support: 12953
     H*                 Clear procedure code print field XXCMSV
     H*                 before processing each service date to avoid
     H*                 printing unwanted values.
     H*
     H*  09/28/98 C:53  PROJ: MC145
     H*                 CORRECT AND ADD STATE WORDING
     H*
     H*  10/05/98 C:15  Support
     H*                 Load the claimant SS# into the payee box on
     H*                 the check if RPFEIN is blank.
     H*
     H*  08/11/98 C:33  SEQ#: WC051/12045
     H*                 CHANGE OVERFLOW PROCESSING FOR EOMB CODE
     H*                 DESCRIPTIONS
     H*
     H*  05/04/98 C:31  Support: 11001
     H*                 Base EOMB Print Issue - Medical Examiner
     H*                 1. Use EOIUSR for Medical Examiner
     H*
     H* ------------------------------------------------------------- *
      * MODIFICATIONS:   --V3-- Re-arranged latest on top of list======
      *
      *================================================================
      *                       V E R S I O N    3                      *
      *
      * 04/01/98  58  SUPPORT: MC095  Sequence # 10837
      *       o Use the user name (UPUSNM) for Medical Examiner.
      *       o Add overflow heading for EOMB code listing.
      *
      * 02/02/98  47  PROJECT: 91xxx                                  *
      *           PRINT GROUP (FUND) NAME/ADDRESS IN UPPER LEFT       *
      *           CORNER OF CHECK AND FILECOPY.  THIS IS A FREE FORM  *
      *           ENTRY IN THE BANK INFO MAINT IF BILOGO = N.         *
      *                                                               *
      *  08/27/97 C:30 Project: WC659                                 *
      *           Check/Voucher Process                               *
      *        1) Only process checks for a Fund.                     *
      *           SIG array removed.  Signature info                  *
      *           stored in Bank Information record:  BICMD1 (PCL5    *
      *           Font Command String for Sig #1), BICMD2 (PCL5 Font  *
      *           Command String for Sig #2).                         *
      *                                                               *
      *        2) EOMB Alternate Address logic added.                 *
      *                                                               *
      *        3) If FEIN > 0 & EOB# > 0, if FEIN is "Group" Provider *
      *           then do NOT print EOMB.  Instead print check stub.  *
      *                                                               *
      * 05/15/97 C:62 PROJECT WC640                                   *
      *               "CAT1" TO "LOCATION"; "CAT2" = CXCAT2;          *
      *               "ADJUSTER" TO CXADJ.                            *
      *                                                               *
      * 04/10/97 C:30 Project: WC602                                  *
      *       1) EOMB Recommended Payment (XXRPAY) field length       *
      *          changed from 7,2 to 9,2.  Output specs changed       *
      *          to accomodate new length.                            *
      *       2) Change the PCL5 Font for Check Addressee to a        *
      *          Non-proportional Font:                               *
      *          Courier 10, 12 point, 10 pitch  "Bitmapped" font     *
      *                                                               *
      * 04/07/97 C:30 Project: WC679                                  *
      *       1) Print FEIN# if check issued to provider.             *
      *          Print SS# if check issued to claimant.               *
      *       2) Correct Group Check and File Copy O specs.           *
      *          Group Check XXCSZ (City State Zip) corrected.        *
      *          If no 2nd Address, XXCSZ was not printed.            *
      *          "Old" specs physically deleted.                      *
      *       3) EOMB FILECOPY additional page was using upper Tray   *
      *          which is Check Stock.  Corrected to use Lower Tray.  *
      *       4) EOMB phrase 'CLM ADJ' & 'CLAIM ADJUSTER' changed to  *
      *          'MEDICAL EXAMINER'.                                  *
      *       5) Tests for Page break for "EOMB Totals" and "EOMB     *
      *          Code Descriptions" changed to minimize number of     *
      *          pages.                                               *
      * ***************************************************************
      *  INDICATOR USAGE:                                             *
      *    INDICATOR            FUNCTION DESCRIPTION                  *
      *       67      PRINT EODIA1 & DIAGNOSIS DESCRIPTION            *
      *       68      PRINT EODIA2 & DIAGNOSIS DESCRIPTION            *
      *       69      PRINT EODIA3 & DIAGNOSIS DESCRIPTION            *
      *       70      PRINT EODIA4 & DIAGNOSIS DESCRIPTION            *
      * del > 73      USE FNSHNM ON CHECK (TRUST NO LOGO)
      * add > 73      IF BILOGO = N, USE BI40A1-4:
      *                 4 40A free form lines
      *                 for top left of check and filecopy
      *       74      NETWORK PPO IS INSTALLED/PRINT NETWORK EOMB     *
      *       75      VOUCHER******************************
      *       91      HOSPITAL BILL CHARGE/SERVICE CODE               *
      *      N91      PROCEDURE CODE                                  *
      *       92      PROCEDURE CODE MODIFIER NOT EQUAL TO ZERO       *
      *       94      2ND ADDRESS NOT EQUAL TO BLANK                  *
      *       95      EOB CODE 1  NOT EQUAL TO BLANK                  *
      *       96      EOB CODE 2  NOT EQUAL TO BLANK                  *
      *       97      EOB CODE 3  NOT EQUAL TO BLANK                  *
      * **************************************************************
     F******WARPL6  IF  E           K        DISK
     F******WACPL   IF  E           K        DISK
     F******* WARPLR  IF  E           K        DISK
     FWARPL10   IF   E           K DISK
     FWACPL4    IF   E           K DISK
     FWARPLU    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWARPLW    IF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
     F**********FAH1L5  UF  E           K        DISK
     FFAH1L5    UF   E           K DISK    USROPN
     FWMCML1    IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
      *______________________________________________________________________
      *  Bank Information file - CO#/APPL/GRP/APPL/ACCT#/EFDT(D)
     F*******WMBIL1  IF  E           K        DISK
     FWMBIL2    IF   E           K DISK
      *______________________________________________________________________
      *  Bank Account  file    - CO#/ACCT
     FWSCKL     IF   E           K DISK
      *______________________________________________________________________
     FWMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFNWC)
      *______________________________________________________________________
      * Network Review file (Group Provicder file)
     FWMNRL     IF   E           K DISK
      *______________________________________________________________________
     FWTABL     IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FWTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FWMCXL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F**********SMUPL   IF  E           K        DISK
     F**********  PUP01                             KRENAMEPUP02
      *______________________________________________________________________
      * WCMC                                                                _
      *                                                                     _
     F******FAEOL3  IF  E           K        DISK                           UC
     FFAEOLG    IF   E           K DISK    USROPN
   F F*DCAL2**IF  E           K        DISK                           UC
   F F*MRCL1**IF  E           K        DISK                           UC
     F**********  PRC01                             KRENAMEPRC02
   F F*MSML***IF  E           K        DISK                           UC
   F F*DHCL***IF  E           K        DISK                           UC
   F F*MRXL***IF  E           K        DISK                           UC
     FFMDIL     IF   E           K DISK    USROPN
   F F*MCDL***IF  E           K        DISK                           UC
     F**********  PCD01                             KRENAMEPCD02
     FFMCML1    IF   E           K DISK    USROPN
     F                                     RENAME(PCM01:PCM02)
      *
     FFMFNL1    IF   E           K DISK    USROPN
     FFMF1L     IF   E           K DISK    USROPN
     FFMC1L     IF   E           K DISK    USROPN
   F F*MSVL1**IF  E           K        DISK                           UC
     F**********FTABL   IF  E           K        DISK                 UC
     F**********  PRC01                             KRENAMEPRC0W
     F**********  PCS01                             KRENAMEPCS0W
     F**********  PPA01                             KRENAMEPPA0W
     F**********  PSP01                             KRENAMEPSP0W
     F**********  PAT01                             KRENAMEPAT0W
     F**********  PCC01                             KIGNORE
     F**** Beg Chg *** 07/18/23 **************************************
     F**** FAEEL1    IF   E           K DISK    USROPN
     FFAEEL5    IF   E           K DISK    USROPN
     F**** End Chg *** 07/18/23 **************************************
     F                                     RENAME(PEE01:PEE02)
     FFMPRL     IF   E           K DISK    USROPN
   F F*TEBLOLDIF**E******     K        DISK                           UC
     F**********  PEB01                             KRENAMEPEB02
     F*********FMPDL   IF  E           K        DISK                 UC
     FFAESL     IF   E           K DISK    USROPN
     F*** BEC CHG *** 05/05/99 ****************************************
    FF*AEAL** IF  E           K        DISK                           UC
     FFAEAL1    IF   E           K DISK    USROPN
     FFAEHL     IF   E           K DISK    USROPN
     FFAEUL     IF   E           K DISK    USROPN
     F*****FAEPL   IF  E           K        DISK
     FFAEPL     IF   E           K DISK    USROPN
     FFDDHL     IF   E           K DISK    USROPN
     FFDDUL     IF   E           K DISK    USROPN
     FFDDPL     IF   E           K DISK    USROPN
     FFDDDL     IF   E           K DISK    USROPN
     FFDDEL     IF   E           K DISK    USROPN
     F*****                                INFDS(DBFDS)
     FFAEEL     IF   E           K DISK    USROPN
     FFDF3L     IF   E           K DISK    USROPN
     FFDF4L     IF   E           K DISK    USROPN
     FFMCXL     IF   E           K DISK    USROPN
     F                                     RENAME(PCX01:PCX02)
     FFMEML     IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:PEM02)
    FF*DCAL13*IF  E           K        DISK                           UC
     FFDCALR    IF   E           K DISK    USROPN
     F                                     INFDS(CAFDS)
     FFTGTL     IF   E           K DISK    USROPN
     FNMNWL     IF   E           K DISK    USROPN
     FSMCOL     IF   E           K DISK
     F*
     F** GROUP CARRIER INFORMATION FILE
     FWMGCL     IF   E           K DISK
     F*
     F* Group information file
     FFDF2L     IF   E           K DISK    USROPN
     F                                     RENAME(PF201:PF202)
     F* FDCAL5 for EOB/DSEQ sorting
     FFDCAL5    IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCA03)
     F                                     INFDS(C3FDS)
      *______________________________________________________________________
      *                                                                     _
     F**********CHECKS  O   F     145     OF     PRINTER
     FCHECKS    O    F  198        PRINTER OFLIND(*INOF)
     F                                     INFDS(PIDS01)
     F                                     PRTCTL(LINE:*compat)
     FFILECOPY  O    F  198        PRINTER OFLIND(*INOA)
     F****QSYSPRT3O   F     132     OB     PRINTER                        UC
      *
     F***QSYSPRT3O   F     198     OB     PRINTER                        UC
     F***fmicd10l1 if   e           k disk
     Ffmicd10l1 if   e           k disk    usropn
     F***faedl     if   e           k disk
     Ffaedl     if   e           k disk    usropn
     FSXCCP     uf a e             disk    Rename(pcc01:scc0p)
     F                                     Prefix(s_)
     FSXCNEOBP  uf a e             disk    rename(pcn01:pecn01)
     FSXDECEOBP uf a e             disk
     FSXDNTEOBP uf a e             disk
     FSXDCNTEOBPuf a e             disk
     FSXDTEOBP  uf a e             disk
     FSXFTEOBP  uf a e             disk
     FSXGDEOBP  uf a e             disk
     FSXGNEOBP  uf a e             disk
     FSXGTEOBP  uf a e             disk
     FSXHDEOBP  uf a e             disk
     FSXRTEOBP  uf a e             disk    rename(pRT01:pErt01)
     FSXSMEOBP  uf a e             disk
     FSXSNEOBP  uf a e             disk
     FSXTOTEOBP uf a e             disk
     FSXRHEOBP  uf a e             disk
     FSXDGEOBP  uf a e             disk
     FXMLPREL1  UF   E           K DISK
     D EOBdSeq         S              5s 0 DIM(20) ASCEND                       EOB codes array
     D EOBseq#         S              5s 0 DIM(20) ASCEND                       EOB codes array
     D xxDRG#          S                   like(EUdrg#)                         EOB codes array
     D xxDgDs          S                   like(EUdgDs)                         EOB codes array
     D hdrChkDate      s                   like(HDckDtHd)                       EOB codes array
     D hdrCheck#       s                   like(HDchk#Hd)                       EOB codes array
     D outCall         s             11
     D InXML           s             60
     D InReturn        s            250
     D OutMode         s              1
     D OutRrn          s              9
     D OutPrefix       s              2
     D OutString       s             10
     D OutCo#          s              3
     D OutFnd          s              3
     d*** Beg Add *** 08/22/22 *******************************
     d J               s              2s 0
     d J2              s              2s 0
     d ModernEOB       s              1A
     d*** End Add *** 08/22/22 *******************************
     d*** Beg Add *** 07/18/23 ***********************************************
     d $$CO#           s              3  0
     d $$DESC          s             70
     d $$EOB           s             10
     d $$XREF          s              3
     d*** End Add *** 07/18/23 ***********************************************
     D* Scripture Processing Program
     D StLetter        pr                  ExtPgm('STLETTER')
     D   outCall_                    11    const
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
     D sys_date        s               D   datfmt(*USA)
     D xxIdt           s                   like(ESiDt)
     d isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFileName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     d isUseScript     S              1n   inz
     D xxPre           s              2    inz('PB')
     d processType     c                   const('Claim Checks')
     d x1Fnd           s              3A
     d x1Co#           s              3A
     d message         s            250    inz(*blanks)
     D Lines129        S              1n   inz
     D @@Void23        S             23A   INZ
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D**********          QCA     1   6 55               CMD EXEC/QCMDEXC
     D**********          QCA     1   2 55               CMD EXEC/QCMDEXC
     D*
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D CD              S              3    DIM(40) ASCEND
     D*************       WCA     1   2 35               CONSTANTS     XC
     D WCA             S             35    DIM(3) CTDATA PERRCD(1)              CONSTANTS     XC
     D SIGMS1          S              4    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 1
     D SIGMS2          S             26    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 2
     D**********          NM      1   5 65               NM STATE MSG
     D**********          SC      1   6 65               SC STATE MSG
     D********            NC      1  22 65               NC STATE MSG
     D**********          NC      1  24 65               NC STATE MSG
     D**********          FL      1   7 65               FL STATE MSG
     D*
     C**** Beg Chg *** 07/18/23 **************************************
     D**** EOB             S              3    DIM(20) ASCEND                       EOB codes array
     D EOB             S              6    DIM(20) ASCEND                       EOB codes array
     C**** End Chg *** 07/18/23 **************************************
     D STZ             S              1    DIM(13)                              STATE & ZIP
     D*
     D NWMSG           S             65    DIM(2) CTDATA PERRCD(1)              NETWORK MESSAGE
     D*
     D* CLAIM EXAMINER HEADING
     D*
     D XXADJ           C                   CONST('ADJUSTER:')
     D***********   'VOUCHER'             C         VOUCHR
     D VCHR1           C                   CONST('VOUCHER')
     D VCHR2           C                   CONST('XML    ')
     D NF              C                   CONST('NOT FOUND')
     D VOID90          C                   CONST('VOID AFTER 90 DAYS')
     D XVMSG1          C                   CONST('Void after')
     D XVMSG2          C                   CONST('days')
     D UPPER           C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D LOWER           C                   CONST('abcdefghijklmnopqrstuvwxyz')
     D @@PAY1          C                   CONST('PAY TO THE')
     D @@PAY2          C                   CONST('ORDER OF')
      *
     D                 DS
     D  EOS                    1      9
     D                                     DIM(3)
     D  EOEOB1                 1      3
     D  EOEOB2                 4      6
     D  EOEOB3                 7      9
     D                 DS
     D  TTM                    1    210
     D                                     DIM(3)
     D  TTMSG1                 1     70
     D  TTMSG2                71    140
     D  TTMSG3               141    210
     D                 DS
     D  XSZIP                  1      9  0
     D  C1ZPP1                 1      5  0
     D  C1ZPP2                 6      9  0
     D                 DS
     D  XCNM                   1     40
     D                                     DIM(40) ASCEND
     D  XSNAME                 1     40
     D                 DS
     D  PRNM                   1     40
     D                                     DIM(40) ASCEND
     D  EOPNAM                 1     40
     D***         DS
     D***                                     1  20 XF#
     D***                                     1   50XFSUB
     D***                                     6   6 XFHYP1
     D***                                     7   90XFFND
     D***                                    10  10 XFHYP2
     D***                                    11  120XFYR
     D***                                    13  13 XFHYP3
     D***                                    14  200XFCASE
     D                 DS
     D  XF#                    1     21
     D  XFSUB                  1      5  0
     D  XFHYP1                 6      6
     D  XFFND                  7      9  0
     D  XFHYP2                10     10
     D  XFYR                  11     13  0
     D  XFHYP3                14     14
     D  XFCASE                15     21  0
     D                 DS
     D  XXSS#                  1      9
     D  SSPRE                  1      3
     D  SSMID                  4      5
     D  SSSUF                  6      9
     D                 DS
     D  XCPHN#                 1     14
     D  XXAREA                 2      4  0
     D  XXCPRN                 5      5
     D  XXPH1                  7      9  0
     D  XXHP1                 10     10
     D  XXPH2                 11     14  0
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  38     39
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD1                 1     55
     D  @@ST1                 40     41
     D*
     D**********  DS
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXLIN1                 1    105
     D  XXPRTA                 1     70
     D  XXPRTB                71    105
     D                 DS
     D  XXLIN2                 1    105
     D  XXPRTC                 1     35
     D  XXPRTD                36    105
     D                 DS
     D  XXCLM#                 1     13  0
     D  RPFND                  1      3  0
     D  X1FYR                  4      6  0
     D  RPCASE                 7     13  0
     D                 DS
     D  XXPOL#                 1     17  0
     D  CMFND                  1      3  0
     D  CMEMP#                 4     12  0
     D  CMDIV                 13     17  0
     D                 DS
     D  RPFYR                  1      3  0
     D  XXCTRY                 1      1  0
     D  XXYR                   2      3  0
     D                 DS
     D  RPFRDT                 1      7  0
     D  XXFRYR                 2      3  0
     D  XXFRMO                 4      5  0
     D  XXFRDA                 6      7  0
     D                 DS
     D  RPTODT                 1      7  0
     D  XXTOYR                 2      3  0
     D  XXTOMO                 4      5  0
     D  XXTODA                 6      7  0
     D                 DS
     D  CMACDT                 1      7  0
     D  XXACYR                 2      3  0
     D  XXACMO                 4      5  0
     D  XXACDA                 6      7  0
     D                 DS
     D  RPZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
     D                 DS
     D  XXCNBR                 1     10
     D  XXSLSH                 1      1
     D  RPCHK#                 2      8  0
     D  XXSLS2                 9      9
     D  XXBLK1                10     10
     D*
     D                 DS
     D  XXTRAN                 1     11
     D  XXAMPR                 1      1
     D  BITRNS                 2      5
     D  BIBNK#                 6      9
     D  BICKDG                10     10
     D  XXAMP2                11     11
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
      *
     D*** LINE            DS
     D line            DS                  INZ
     D  SPBEFR                 1      1
     D  SPAFTR                 2      2
     D  SKBEFR                 3      4
     D  SKAFTR                 5      6
     D  CURLIN                 7      9  0
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
     D  XXNWM                  1    180
     D  XXNWM1                 1     39
     D  XXNWM2                40    180
     D                 DS
     D  VDY                    1      3
     D  VDY1                   1      1
     D  VDY2                   2      2
     D  VDY3                   3      3
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D PIDS01        E DS                  EXTNAME(SZQ4P)
     D***** DBFDS           DS
     D*****  DBRRN                397    400B 0
     D CAFDS           DS
     D  CARRN                397    400B 0
     D C3FDS           DS
     D  C3RRN                397    400B 0
     D                 DS
     D  TNOTE                  1    210
     D  WVAR1                  1     70
     D  WVAR2                 71    140
     D  WVAR3                141    210
     D kkicd10         s                   like(iccode)
     D DESC1           s             30
     D DESC2           s             30
     D DESC3           s             30
     D DESC4           s             30
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
     D isScriptEOB     s              1A   inz
     D isScriptChk     s              1A   inz
     D isUseSpoolEOB   s              1n   inz
     D isUseSpoolChk   s              1n   inz
     D isUseXML        S              1n   inz
     D/copy sPrModule
     D/copy sPrXML
     I**********PCS0W
     I**********    CSKEY                           CZKEY
     I**********    CSDESC                          CZDESC
     I**********    CSDT                            CZDT
     I**********PRC0W
     I**********    RCKEY                           RZKEY
     I**********    RCDT                            RZDT
     I**********    RCIDT                           RZIDT
      *
     I**********PAT0W
     I**********    ATDT                            A2DT
      *
     I**********PSP0W
     I**********    SPDT                            S2DT
      *
     I**********PPA0W
     I**********    PADT                            P3DT
      *
     IPCA01
     I              CADT                        C3DT
      *
     IPFN01
     I              FNEFDT                      F3EFDT
     I              FNDT                        F3DT
      *
     I***********PRC02
     I**********    RCDT                            R4DT
     I**********    RCIDT                           R4IDT
      *
     ISPC01
     I              PCKEY                       P2KEY
     ISRC01
     I              RCKEY                       R2KEY
     ISBC01
     I              BCKEY                       B3KEY
     ISSY01
     I              SYKEY                       S2KEY
     ISCS01
     I              CSKEY                       C3KEY
     I              CSDT                        C4DT
     I              CSIDT                       C4IDT
      *
     IPCX02
     I              CXDCCT                      C2DCCT
     I*PCD02****
     I**********    CDSS#                           C2SS#
     I**********    CDLSN                           C2LSN
     I**********    CDFSN                           C2FSN
     I**********    CDMID                           C2MID
     I**********    CDADR1                          C2ADR1
     I**********    CDADR2                          C2ADR2
     I**********    CDCTY                           C2CTY
     I**********    CDST                            C2ST
     I**********    CDZIP                           C2ZIP
     I**********    CDDTOB                          C2DTOB
     I**********    CDSEX                           C2SEX
     I**********    CDSTAT                          C2STAT
     I**********    CDUSER                          C2USER
     I**********    CDPGM                           C2PGM
     I**********    CDTIME                          C2TIME
     I**********    CDDT                            C2DT
     IPC101
     I*
     I              C1CO#                       CXCO#
     I              C1NAME                      CXNAME
     I              C1ADR1                      CXADR1
     I              C1ADR2                      CXADR2
     I              C1CTY                       CXCTY
     I              C1ST                        CXST
     I              C1ZIP                       CXZIP
     I              C1AREA                      CXAREA
     I              C1PHN#                      CXPHN#
     I              C1FEIN                      CXFEIN
     I              C1ANER                      CXANER
     I              C1LCS#                      CXLCS#
     I              C1EOB#                      CXEOB#
     I              C1STAT                      CXSTAT
     I              C1USER                      CXUSER
     I              C1PGM                       CXPGM
     I              C1TIME                      CXTIME
     I              C1DT                        CZDT
     I              C1IUSR                      CXIUSR
     I              C1SUBM                      CXSUBM
     I              C1CRA#                      CXCRA#
     I              C1AGA#                      CXAGA#
     I              C1TAPE                      CXTAPE
     I              C1DEV                       CXDEV
     I              C1VOL                       CXVOL
     I              C1LTR#                      CXLTR#
     I              C1LIN#                      CXLIN#
     I              C1FARA                      CXFARA
     I              C1FAX#                      CXFAX#
     IPCM02
     I**********    CMCO#                           C2CO#
     I**********    CMSUB#                          C2SUB#
     I**********    CMFND                           C2FND
     I**********    CMFYR                           C2FYR
     I**********    CMEMP#                          C2EMP#
     I**********    CMDIV                           C2DIV
     I**********    CMCASE                          C2CASE
     I**********    CMSS#                           C2SS#
     I**********    CMACDT                          C2ACDT
     I**********    CMACTM                          C2ACTM
     I**********    CMDTRP                          C2DTRP
     I**********    CMDTLW                          C2DTLW
     I**********    CMACDS                          C2ACDS
     I**********    CMMCON                          C2MCON
     I**********    CMRT01                          C2RT01
     I**********    CMAR01                          C2AR01
     I**********    CMDTRE                          C2DTRE
     I**********    CMSTAT                          C2STAT
     I**********    CMUSER                          C2USER
     I**********    CMPGM                           C2PGM
     I**********    CMTIME                          C2TIME
     I**********    CMDT                            C2DT
     I**********    CMENTD                          C2ENTD
     I**********    CMRCVD                          C2RCVD
     I**********    CMIDT                           C2IDT
     I**********    CMMMI                           C2MMI
     I              CMOSCL                      C2OSCL
     C*
     C*  SUBSCRIBER MASTER
     C     SMKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
      *
      *
     C*  COMPANY CROSS REFERENCE FILE TO WMCMS FUND
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CASE DETAIL FILE.
     C     CAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    EOTRCD
     C                   KFLD                    EOEOB#
      *
     C*  COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
      *
     C*  DIAGNOSIS CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIAC            5
     C*  EOB WORK FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXMFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPEOB#
      *
     C*  FS BASE CONTROL FILE
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*  HOSPITAL DETAIL CHARGE FILE
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*  CPT4 CODE FILE
     C*
     C********** PRKEY     KLIST
     C**********           KFLD           EOCO#
     C**********           KFLD           CACPT4
     C*********************KFLD***********XXEFDT  70
     C                   KFLD                    XXEFDT
      *
     C********** RXKY1     KLIST
     C**********           KFLD           EOCO#
     C**********           KFLD           HCNDC
     C*
     C     RCKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    CACPT4
     C*
      *
     C*  TABLE FILE KEY LIST
     C*
     C     TBLKEY        KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXKEY            10
     C*  USER PARAMETER FILE
     C*
     C********** UPKY      KLIST
     C**********           KFLD           XXSTAF
     C*
     C     UPKY1         KLIST
     C                   KFLD                    EOIUSR
     C*
     C     UPKY2         KLIST
     C                   KFLD                    XXCLEX
     C*
      *
     C*  SERVICE CODE FILE
     C     SVKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    XXEFDT
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQCKTP
     C                   KFLD                    XXFND
     C*
     C     RPKY2         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPCKTP
     C                   KFLD                    RPFND
     C                   KFLD                    RPCHK#
     C*
     C     RPKYH         KLIST
     C                   KFLD                    @HCO#
     C                   KFLD                    @HCKTP
     C                   KFLD                    @HFND
     C                   KFLD                    @HCHK#
     C*
     C     RPKY3         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     H1KY          KLIST
     C                   KFLD                    XHSINV
     C*
     C*  CLAIM FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
      *
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CHECK PROCESSING FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT              7 0
     C                   KFLD                    QQACCT
     C******************** KFLD           QQCKTP
     C                   KFLD                    XXFND
     C                   KFLD                    QQCKTP
     C*
     C*  REMARK CODE FILE
     C*
     C     RCKY1         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  BANK INFORMAITON FILEZ
     C*
     C     BIKY1         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    RPFND
     C******************   KFLD           QQACCT
     C                   KFLD                    QQCKTP
     C                   KFLD                    XXDT
      *
     C     BIKY2         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    RPFND
     C******************   KFLD           QQACCT
     C                   KFLD                    QQCKTP
     C*
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   CLAIMS/DARTS CONTROL KEY LIST
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  Bank Account Master File
      *
     C     CKKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQACCT
      *
      *   KEY LIST FOR THE FUND AUXILIARY FILE
      *
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
      *
      *   Network Review file (Group Provider)
      *
     C     NRKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
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
     C*****DEKY2         KLIST
     C*****              KFLD                    KKCO#
     C*****              KFLD                    KKSUB#
     C*****              KFLD                    KKFND
     C*****              KFLD                    KKFYR
     C*****              KFLD                    KKCASE
     C*****              KFLD                    KKEOB#
     C*****              KFLD                    KKDSEQ
     C*****              KFLD                    KKSEQ#
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
     C* KEY LIST FOR WCMC CONTROL FILE LOGICAL
     C*
     C     CXKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* KEY LIST FOR WCMC CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* KEY LIST FOR COMPANY AUXILIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    KKCO#
     C*
     C* KEY LIST FOR DIAGNOSIS CODE FILE
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C* KEY LIST FOR WCMC EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C* KEY LIST FOR EOB DISCRIPTION FILE
     C*
     C********** TBKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKKEY
     C*
     C* KEY LIST FOR DETAIL CHARGE FILE
     C*
     C     CAKY2         KLIST
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
     C*
     C*
     C* Key list for Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KXKEY
     C*
     C     GTKY2         KLIST
     C                   KFLD                    K2CO#
     C                   KFLD                    K2TBID
     C                   KFLD                    K2KEY
     C*
     C     GTKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C*
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C     NWKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKNWCD
     C*
     C*  WORK COMP CONTROL FILE
     C*
     C     WCKY          KLIST
     C                   KFLD                    XCKEY
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
     C*
     C     CAKY3         KLIST
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
      *
     C*
     C*  INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EATAX         XXTAX
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C     *LIKE         DEFINE    CXADJ         X1ADJ
     C     *LIKE         DEFINE    RPFND         XXFND
     C     *LIKE         DEFINE    RPMOD         @@MOD
     C     *LIKE         DEFINE    RPCO#         @@CO#
     C     *LIKE         DEFINE    RPFND         @@FND
     C     *LIKE         DEFINE    RPFYR         @@FYR
     C     *LIKE         DEFINE    RPCASE        @@CASE
     C     *LIKE         DEFINE    RPBR2         @@BR2
     C     *LIKE         DEFINE    RPRECD        @@RECD
     C     *LIKE         DEFINE    RPWKS         @@WKS
     C     *LIKE         DEFINE    RPDAYS        @@DAYS
     C     *LIKE         DEFINE    RPCHK#        @@CHK#
     C     *LIKE         DEFINE    XXDATE        @@DATE
     C     *LIKE         DEFINE    XXNAME        @@NAME
     C*******    *LIKE     DEFN RPFEIN    @@FEIN
     C     *LIKE         DEFINE    RPCO#         @HCO#
     C     *LIKE         DEFINE    RPCKTP        @HCKTP
     C     *LIKE         DEFINE    RPFND         @HFND
     C     *LIKE         DEFINE    RPCHK#        @HCHK#
     C     *LIKE         DEFINE    RPFEIN        XXFEIN
     C     *LIKE         DEFINE    RPMOD         XXMOD
     C     *LIKE         DEFINE    RPFND         XHFND
     C     *LIKE         DEFINE    RPAMT         XHAMT
     C     *LIKE         DEFINE    RPAMT         XGAMT
     C     *LIKE         DEFINE    RPAMT         XTAMT
     C     *LIKE         DEFINE    EOCHRG        XHCHRG
     C     *LIKE         DEFINE    EOCHRG        XGCHG
     C     *LIKE         DEFINE    EOCHRG        XTCHG
     C     *LIKE         DEFINE    EOPAID        XHPAID
     C     *LIKE         DEFINE    EOPAID        XGPAID
     C     *LIKE         DEFINE    EOPAID        XTPAID
     C     *LIKE         DEFINE    CMCASE        XHCASE
     C     *LIKE         DEFINE    CMFYR         XHFYR
     C     *LIKE         DEFINE    EMRGN#        XHRGN
     C     *LIKE         DEFINE    RPSINV        XHSINV
     C     *LIKE         DEFINE    FNNAME        XHDSC
     C     *LIKE         DEFINE    XXACDT        @@ACDT
     C     *LIKE         DEFINE    RPAMT         @@AMT
     C     *LIKE         DEFINE    XXAMT1        @@AMT1
     C     *LIKE         DEFINE    XXAMT2        @@AMT2
     C     *LIKE         DEFINE    RPPNAM        @@PNAM
     C     *LIKE         DEFINE    RPADR1        @@ADR1
     C     *LIKE         DEFINE    RPADR2        @@ADR2
     C     *LIKE         DEFINE    RPACCT        @@ACCT
     C     *LIKE         DEFINE    BIEFDT        XXCYMD
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C********** *LIKE     DEFN PDFEIN    KKFEIN
     C********** *LIKE     DEFN PDMOD     KKMOD
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    DHDSEQ        KKDSEQ
     C     *LIKE         DEFINE    F4ST          KKST
     C     *LIKE         DEFINE    F4EFDT        KKEFDT
     C     *LIKE         DEFINE    DHMOD1        XXMOD1
     C     *LIKE         DEFINE    DHMOD2        XXMOD2
     C     *LIKE         DEFINE    DHMOD3        XXMOD3
     C     *LIKE         DEFINE    DHMOD4        XXMOD4
     C     *LIKE         DEFINE    DHPOS         XXPOS
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         X1EOB
     C**** *LIKE         DEFINE    DEEOB         X2EOB
     C**** *LIKE         DEFINE    DEEOB         X3EOB
     C**** *LIKE         DEFINE    DEEOB         X4EOB
     C     *LIKE         DEFINE    DEEOBCD       X1EOB
     C     *LIKE         DEFINE    DEEOBCD       X2EOB
     C     *LIKE         DEFINE    DEEOBCD       X3EOB
     C     *LIKE         DEFINE    DEEOBCD       X4EOB
     C**** End Chg *** 07/18/23 **************************************
     C**** Beg Add *** 08/22/22 **********************************
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         X5EOB
     C**** *LIKE         DEFINE    DEEOB         X6EOB
     C**** *LIKE         DEFINE    DEEOB         X7EOB
     C**** *LIKE         DEFINE    DEEOB         X8EOB
     C**** *LIKE         DEFINE    DEEOB         X9EOB
     C**** *LIKE         DEFINE    DEEOB         X10EOB
     C**** *LIKE         DEFINE    DEEOB         X11EOB
     C**** *LIKE         DEFINE    DEEOB         X12EOB
     C**** *LIKE         DEFINE    DEEOB         X1EOBR
     C**** *LIKE         DEFINE    DEEOB         X2EOBR
     C**** *LIKE         DEFINE    DEEOB         X3EOBR
     C**** *LIKE         DEFINE    DEEOB         X4EOBR
     C**** *LIKE         DEFINE    DEEOB         X5EOBR
     C**** *LIKE         DEFINE    DEEOB         X6EOBR
     C**** *LIKE         DEFINE    DEEOB         X7EOBR
     C**** *LIKE         DEFINE    DEEOB         X8EOBR
     C**** *LIKE         DEFINE    DEEOB         X9EOBR
     C**** *LIKE         DEFINE    DEEOB         X10EOBR
     C**** *LIKE         DEFINE    DEEOB         X11EOBR
     C**** *LIKE         DEFINE    DEEOB         X12EOBR
     C     *LIKE         DEFINE    DEEOBCD       X5EOB
     C     *LIKE         DEFINE    DEEOBCD       X6EOB
     C     *LIKE         DEFINE    DEEOBCD       X7EOB
     C     *LIKE         DEFINE    DEEOBCD       X8EOB
     C     *LIKE         DEFINE    DEEOBCD       X9EOB
     C     *LIKE         DEFINE    DEEOBCD       X10EOB
     C     *LIKE         DEFINE    DEEOBCD       X11EOB
     C     *LIKE         DEFINE    DEEOBCD       X12EOB
     C     *LIKE         DEFINE    DEEOBCD       X1EOBR
     C     *LIKE         DEFINE    DEEOBCD       X2EOBR
     C     *LIKE         DEFINE    DEEOBCD       X3EOBR
     C     *LIKE         DEFINE    DEEOBCD       X4EOBR
     C     *LIKE         DEFINE    DEEOBCD       X5EOBR
     C     *LIKE         DEFINE    DEEOBCD       X6EOBR
     C     *LIKE         DEFINE    DEEOBCD       X7EOBR
     C     *LIKE         DEFINE    DEEOBCD       X8EOBR
     C     *LIKE         DEFINE    DEEOBCD       X9EOBR
     C     *LIKE         DEFINE    DEEOBCD       X10EOBR
     C     *LIKE         DEFINE    DEEOBCD       X11EOBR
     C     *LIKE         DEFINE    DEEOBCD       X12EOBR
     C**** End Chg *** 07/18/23 **************************************
     C**** End Add *** 08/22/22 **********************************
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         YYEOB
     C     *LIKE         DEFINE    DEEOBCD       YYEOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    F1ACTY        XXCTY
     C     *LIKE         DEFINE    F1AST         XXSTAT
     C********** *LIKE     DEFN F1AAD1    XXAAD1
     C********** *LIKE     DEFN F1AAD2    XXAAD2
     C     *LIKE         DEFINE    F1AARA        XXARA
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    ESIDT         KKIDT
     C     *LIKE         DEFINE    ESITIM        KKITIM
     C********** *LIKE     DEFN EBKEY     KKKEY
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    EHACCT        XXACCT
     C********** *LIKE     DEFN F1ANME    XXANME
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KXKEY
     C     *LIKE         DEFINE    EOCO#         ##CO#
     C     *LIKE         DEFINE    EOSUB#        ##SUB#
     C     *LIKE         DEFINE    EOFND         ##FND
     C     *LIKE         DEFINE    EOFYR         ##FYR
     C     *LIKE         DEFINE    EOCASE        ##CASE
     C     *LIKE         DEFINE    EOEOB#        ##EOB#
     C*
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFDAT        KKTDAT
     C     *LIKE         DEFINE    CAFDAT        HLTDAT
     C     *LIKE         DEFINE    NWNWCD        KKNWCD
     C     *LIKE         DEFINE    RPFND         XXMFND
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    EEEOB         KKEOBC
     C     *LIKE         DEFINE    EEEOBCD       KKEOBC
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    GTCO#         K2CO#
     C     *LIKE         DEFINE    GTTBID        K2TBID
     C     *LIKE         DEFINE    GTKEY         K2KEY
     C     *LIKE         DEFINE    WCKEY         XCKEY
     C     *LIKE         DEFINE    RPCHK#        XXCHK#
      *
     C******LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    EOBNPI        XXBNPI
     C     *LIKE         DEFINE    EPPHAR        XXPHAR
     C     *LIKE         DEFINE    F1AARA        SVFARA
     C     *LIKE         DEFINE    F1AARA        XXFARA
     C     *LIKE         DEFINE    RPINV#        @@INV#
     C     *LIKE         DEFINE    F2ST          KF2ST
     C     *LIKE         DEFINE    DEDSEQ        TMDSEQ
     C     *LIKE         DEFINE    DENOT1        WRNOT1
     C     *LIKE         DEFINE    DENOT2        WRNOT2
     C     *LIKE         DEFINE    DENOT3        WRNOT3
     C     *LIKE         DEFINE    DENOT4        WRNOT4
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQACCT           20
     C                   PARM                    QQCKTP           10
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C*
     C     DTYYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      *ALL'-'       DASH            115
     C*
      //*** Beg Add *** 08/22/22 *****************************
        ModernEOB = ' ';
        *in88 = *off;
        If isModEnabled('Modernized EOB Print');
           ModernEOB = 'Y';
           *in88 = *on;
        Endif;
      //*** Beg Add *** 08/22/22 *****************************
      /Free
       RtvCltID(CltId);
       If CltId = '129';
          *In71 = *On;
       Else;
          *In71 = *Off;
       EndIf;

       If CltId = '126';
          *In19 = *On;
       Else;
          *In19 = *Off;
       EndIf;
      /End-Free
     C                   MOVE      *ZERO         *IN72
     C                   MOVE      *ZERO         *IN53
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2DATE            6 0
      *
      *  Assign 'WC' for "WCMS" Bank Info file
      *
     C                   MOVE      'WC'          XXAPPL
     C*
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
     C*
     C* Check data client data area
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C                   MOVEL     WCA(3)        XXTXT1            7
     C                   MOVE      *BLANKS       @@VOID           19
      /Free
       @@Void23 = *blanks;
      /End-Free
     C                   MOVE      'N'           CKVOID            1            CK VOID DAYS
     C*
     C                   SELECT
     C     ##CLT#        WHENEQ    '136'
     C                   MOVE      *ON           *IN15                          COND XXX
     C                   MOVE      *BLANKS       @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
     C     ##CLT#        WHENEQ    '139'
     C                   MOVEL     VOID90        @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C                   SETON                                        79        GIC
     C*
     C     ##CLT#        WHENEQ    '090'
     C                   MOVEL     VOID90        @@VOID
     C*************        MOVE *ON       *IN78            1ST UNDERLINE
     C                   MOVE      *OFF          *IN78
     C*
     C     ##CLT#        WHENEQ    '071'
     C                   MOVEL     VOID90        @@VOID
     C                   MOVE      *BLANKS       XXTXT1            7
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
     C     ##CLT#        WHENEQ    '045'
     C     ##CLT#        OREQ      '046'
     C*****##CLT#********OREQ      '152'
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C                   MOVE      *ON           *IN79                          1ST UNDERLINE
     C*
     C                   OTHER
      *
     C                   MOVE      'Y'           CKVOID                         CK VOID DAYS
      *
      *
      * This code initializes the 'Void after x days' display field
      * If the number of days (BI50N3) is *zero the message
      * field (XVOAFT) is blanked out
      *
     C                   MOVE      *BLANKS       XVOAFT
      *
     C     BI50N3        IFGT      *ZERO
     C                   MOVE      BI50N3        XVDAYS            3 0
     C                   MOVE      XVDAYS        VDY
     C                   MOVE      *BLANKS       XXVDAY            3
      *
     C     XVDAYS        IFLT      10
     C                   MOVE      *BLANK        VDY1
     C                   MOVE      *BLANK        VDY2
     C                   MOVEL     VDY3          XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFLT      100
     C                   MOVE      *BLANK        VDY1
     C                   MOVEL     VDY2          XXVDAY
     C     XXVDAY        CAT       VDY3:0        XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFGT      99
     C                   MOVE      VDY           XXVDAY
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     XVMSG1        XVOAFT           19
     C     XVOAFT        CAT       XXVDAY:1      XVOAFT
     C     XVOAFT        CAT       XVMSG2:1      XVOAFT
     C                   MOVEL     XVOAFT        @@VOID
     C                   MOVE      *OFF          *IN78
      /Free
       @@Void23 = '- ' + %xlate(Lower:Upper:xvmsg1) + ' ' +
                   %trim(XXVDAY) + ' ' +  %xlate(Lower:Upper:XVMSG2) + ' -';
      /End-Free
     C                   ENDIF
      *
      *
     C                   MOVE      *OFF          *IN78
      *
     C***********          MOVE *BLANKS   @@VOID 18
     C                   ENDSL
     C*
     C*********  ##CLT#    IFEQ '114'
     C*********            MOVE *OFF      *IN21
     C*********            ENDIF
     C*
     C*** INDICATORS TO CONTROL MICR LINE FOR BASE/CLIENTS
     C*** *IN20 MUST BE ON FOR BASE TO PRINT. OFF TO PRINT MODS.
     C*
     C                   SELECT
     C*
     C* 114 REVERSED AS THEY HAVE A CLIENT VERSION
     C*
     C     ##CLT#        WHENEQ    '114'
     C                   MOVE      *ON           *IN20
     C                   MOVE      *OFF          *IN21
     C* CLT146
     C     ##CLT#        WHENEQ    '146'
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *ON           *IN22
     C* CLT146
     C     ##CLT#        WHENEQ    '065'
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *ON           *IN23
     C* CLT129
     C     ##CLT#        WHENEQ    '129'
     C                   MOVE      *OFF          *IN20
     C                   MOVE      *ON           *IN24
     C* BASE
     C                   OTHER
     C                   MOVE      *ON           *IN20
     C                   ENDSL
     C*
     C*
     C***** BUILD MACRO CALL FROM FUND NUMBER
     C*****
     C*****      '&f'      CAT  QQFND     TEMP5   5
     C*****      TEMP5     CAT  'y3X'     MACRO   8
     C*****
     C*****************************************************************
     C*
     C*
     C                   MOVE      QQDATE        X1DATE            8 0
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      '/'           XXSLSH            1
     C                   MOVE      '/'           XXSLS2            1
     C                   MOVE      '&'           XXAMPR            1
     C                   MOVE      '&'           XXAMP2            1
     C                   MOVE      *BLANKS       XXBLK2            2
     C                   MOVE      *BLANKS       XXBLK1            1
      *
      /Free
          *In62 = *Off;
       If CltId = '034' and XXFND=015;
          *In62 = *On;
       EndIf;
      /End-Free
      * GROUP CHECK INDICATOR
      *
     C     QQTYPE        COMP      'G'                                    07
      *
     C*
     C*  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE.
     C*
     C     CXKY          CHAIN     PCX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     CXADJ         ANDNE     *BLANKS
     C*
     C                   MOVEL     CXADJ         X1ADJ
     C                   ELSE
     C*
     C                   MOVEL     XXADJ         X1ADJ
     C*
     C                   ENDIF
     C*
     C* CHAIN OUT TO SYSTEM CONTROL FILE TO SEE IF WCMC IS INSTALLED
     C*
     C*
     C                   MOVE      '0'           XXWCMC            1
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'SY'          XSKEY
     C     FBKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C     SYFL02        IFEQ      'Y'
     C                   MOVE      '1'           XXWCMC
     C                   END                                                    END SYFL02
     C                   END                                                    END *IN60
      *
     C*
     C*  GET SYSTEM VALUE FOR GROUP CHECKS.
     C*
     C                   SETOFF                                       16
     C                   MOVEL(P)  'WC'          XCKEY
     C     WCKY          CHAIN     SWC01                              99
     C     *IN99         IFEQ      *OFF
     C     WCFL27        ANDEQ     'Y'
     C                   SETON                                        16
     C                   ENDIF
      *
     C*
     C*   GET CHECK TYPE (V=VOUCHER, B=BLUE STOCK, G=GREEN STOCK)
     C*   FORCE "REGULAR" IF FND RCD NOT FOUND
     C     FNKEY         CHAIN     PFNWC                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '0'           *IN75
     C                   ELSE
     C                   MOVE      *BLANKS       XXFN             60
     C                   MOVE      *BLANKS       XXFAD1           40
     C                   MOVE      *BLANKS       XXFAD2           40
     C                   MOVE      *BLANKS       XXFCT            25
     C                   MOVE      *BLANKS       XXFST             2
     C                   MOVE      *BLANKS       XXFZIP            5 0
     C                   MOVEL     FNNAME        XXFN
     C                   MOVE      FNMAD1        XXFAD1
     C                   MOVE      FNMAD2        XXFAD2
     C                   MOVE      FNMCTY        XXFCT
     C                   MOVE      FNMST         XXFST
     C                   MOVEL     FNMZIP        XXFZIP
     C     XXFAD2        COMP      *BLANKS                                65
     C*
     C                   END
      *
      *  Access Fund Master to obtain Fund Short Name
      *
     C     FNKEY         CHAIN     PFNWC                              60
      *
     C                   MOVE      *OFF          *IN75                          Regular Check
      *
      *  Default the EOB check date and check number headings.
      *
     C                   EvalR     hdrChkDate = 'Check date :'
     C                   EvalR     hdrCheck# = 'Chk#'
      *
      *  Access Bank Account Master to obtain Voucher Flag (Y/N)
      *
     C     CKKEY         CHAIN     PCK01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     CKVCHR        ANDEQ     'Y'
     C     *IN60         OREQ      *OFF
     C     CKVCHR        ANDEQ     'X'
     C                   SELECT
     C     CKVCHR        WHENEQ    'Y'
     C                   MOVEL     VCHR1         VOUCHR            7
     C                   evalR     hdrCheck# = 'Vch#'
     C                   evalR     hdrChkDate = 'Voucher Date:'
     C     CKVCHR        WHENEQ    'X'
     C                   MOVEL     VCHR2         VOUCHR            7
     C                   ENDSL
     C                   MOVE      *ON           *IN75                          Voucher NO MICR
     C                   ELSE
     C                   MOVE      *OFF          *IN75                          Check   MICR
     C                   ENDIF
      *
      *  Access Fund Auxiliary to obtain F2RXA#
      *
     C     FNKEY         CHAIN     PF201                              60
     C*
     C***OPEN THE APPROPRIATE PRINTER FILES
     C***
     C***                  OPEN CHECKS
     C**N07                OPEN FILECOPY
     C***07                OPEN QSYSPRT3
     C*
     C                   MOVE      *ALL'-'       XXDIV           120
     C*
     C*  GET THE ENTRY FROM THE CHECK PROCESSING FILE
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C     CPKEY         CHAIN     PCP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ' '           XXSTRT
        //  Determine if this module get spool file and/or XML output
      /Free
                         clear xmlFileName;
                         clear xmlLink;
                         isMassReq = *on;
                         massFlag = 'Y';
                         isUseSpoolEOB = UseSpoolOut('EOB Print');
                         isUseXML = UseScriptura('EOB Print')
                                    or Use3rdXml('EOB Print');
                         if isUseXML;
                           isScriptEOB = 'Y';
                         else;
                           isScriptEOB = 'N';
                         Endif;
                         isUseSpoolChk = UseSpoolOut('Claim Checks');
                         isUseXML = UseScriptura('Claim Checks')
                                    or Use3rdXml('Claim Checks');
                         if isUseXML;
                           isScriptChk = 'Y';
                         else;
                           isScriptChk = 'N';
                         Endif;
      /end-free
     C* Get the system date and store in initial date field
     C*
     C                   eval      sys_date = %date
     C     *cymd         Move      sys_date      xxIdt
     C*
     C     RPKEY         CHAIN     PRP01                              50        |
     C     *IN50         DOWEQ     '0'
     C*
     C     RPTRDT        IFLE      XXDT
     C     RPCHK#        ANDNE     0
       //* Get the XML file name to be used for checks and EOB's
      /Free
       //*********       exsr      GetXmlFile'
      /end-free
       // Get the Scriptura XML file name
      /free
           If        isScriptChk = 'Y';
              Exsr      ScriptXml;
           Endif;
      /end-free
     C*
     C*  IF THE REPLACEMENT CHECK IS NOT ZERO, WE NEED TO START WITH
     C*  A CERTAIN CHECK
     C*
     C     CPRSTR        IFNE      *ZERO
     C     CPRSTR        IFNE      RPCHK#
     C     XXSTRT        IFEQ      ' '
     C                   GOTO      NCHK#
     C                   END
     C                   ELSE
     C                   MOVE      'Y'           XXSTRT            1
     C                   END
     C                   END
      *
     C                   MOVE      *OFF          *IN44
     C     RPFEIN        IFGT      *ZERO
     C                   MOVE      *ON           *IN44
     C                   ENDIF
     C*
     C*** DETERMINE TYPE OF CHECK TO PROCESS...IF MED AND EOB# EXISTS THEN
     C***   WE NEED TO PRINT EOMB INFO ON FORM
     C     RPRETP        COMP      'C'                                    30    *IN30=1 IF COMP
     C*
     C                   MOVEL     RPZIP         XXRZIP            5 0
     C*
     C*  RETRIEVE CLAIM RECORD TO GET SS#
     C*
     C     CMKEY         CHAIN     WMCML1                             51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       CMSS#
     C                   Z-ADD     0             CMACDT
     C                   Z-ADD     0             CMEMP#
     C                   Z-ADD     0             CMDIV
     C                   Z-ADD     0             AR
     C                   MOVEL     *BLANK        RT
     C                   MOVEL     *BLANK        CMCTG1
     C                   MOVEL     *BLANK        CMCTG2
     C                   MOVE      *BLANKS       CMACL#
     C                   END
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1ACDT            8 0
     C*
      * GET CLAIMS EXAMINER TITLE
     C                   MOVE      CMCLEX        XXCLEX            3
     C     UPKY2         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       UPTITL
     C                   MOVEL     *BLANKS       UPUSNM
     C                   MOVEL     *BLANKS       XXUSNM
     C                   ELSE
     C                   MOVEL     UPUSNM        XXUSNM           30
     C                   ENDIF
     C*
     C**********           ELSE
     C**********           MOVELUPUSNM    XXUSNM 50
     C**********           MOVELUPUSNM    XXUSNM 40
     C**********           END
      *
     C*  IF GROUP CHECK THEN USE THE FROM AND TO DATES FROM THE CHECK
     C*   PROCESSING FILE
     C*
     C     QQTYPE        IFEQ      'G'
     C                   MOVE      *ZERO         QQFRDT
     C                   MOVE      *ZERO         QQTODT
     C                   CALL      'W7002R1'
     C                   PARM                    QQCO#
     C                   PARM                    QQDATE
     C                   PARM                    QQFRDT            6
     C                   PARM                    QQTODT            6
     C                   PARM                    QQFND
     C*
     C                   MOVE      QQFRDT        X1FRDT            6 0
     C                   MOVE      QQTODT        X1TODT            6 0
     C                   Z-ADD     X1FRDT        XXFRDT
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X2FRDT            8 0
     C                   Z-ADD     X1TODT        XXTODT
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X2TODT            8 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     RPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     RPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2FRDT            8 0
     C                   Z-ADD     RPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C                   Z-ADD     RPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2TODT            8 0
     C*
     C                   END
     C                   MOVE      RPFYR         X1FYR             3 0
     C                   Z-ADD     *ZEROS        TLAMT
     C*
     C*  RETRIEVE PAYMENT VALUE 1 DESCRIPTION
     C*
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     RPBR2         XXKEY
     C     RCKY1         CHAIN     PB201                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     B2DESC        XXB2DS           30
     C                   MOVEL     B2DESC        XXB2D2            3
     C                   ELSE
     C                   MOVE      *BLANKS       XXB2DS
     C                   MOVEL     *BLANKS       XXB2D2            3
     C                   END
     C                   MOVEL     *BLANKS       XXKEY
     C*
     C*  RETRIEVE RESERVE CODE DESCRIPTION
     C*
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     RPRECD        XXKEY
     C     RCKY1         CHAIN     PRT01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     RTDESC        XXRTDS           15
     C                   MOVEL     RTDESC        XXRTD2           30
     C                   ELSE
     C                   MOVE      *BLANKS       XXRTDS
     C                   END
     C                   MOVEL     *BLANKS       XXKEY
     C*
     C*** GET EMPLOYER NAME
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXENAM           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXENAM
     C                   END
     C*
     C     EMPAD2        COMP      *BLANKS                                64
     C*
     C*    GET TOTAL PAID FOR THIS TYPE OF RESERVE FOR THIS CLAIM
     C                   Z-ADD     1             Q                 5 0
     C     RPRECD        LOOKUP    RT(Q)                                  31
     C     *IN31         IFEQ      '1'
     C                   Z-ADD     AR(Q)         TLAMT            11 2
     C                   ELSE
     C                   Z-ADD     0             TLAMT
     C                   END
     C*
     C*
     C*** ADD THIS CHECK TO TOTAL PAID FIELD ***
     C                   ADD       RPAMT         TLAMT
     C*
     C*
     C*  LOAD DOLLAR AMOUNT SPELLED OUT
     C*
     C                   MOVE      RPAMT         QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       XXAMT1           60
     C                   MOVE      *BLANKS       XXAMT2           60
     C                   MOVE      QQAMT1        XXAMT1
     C                   MOVE      QQAMT2        XXAMT2
     C*
     C*  RETRIEVE CLAIMANT DEMOGRAPHICS RECORD
     C*
     C     CDKEY         CHAIN     WMCDL                              51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       XXNAME           47
     C                   ELSE
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME
     C                   END
     C*  DUMMY FILL RECORDS TO REPLACE O SPECS FOR REMARKS ON CHK
     C                   MOVE      *BLANKS       XXFIL1           40
     C                   MOVE      *BLANKS       XXFIL2           40
     C                   MOVE      *BLANKS       XXFIL3           40
     C                   MOVE      *BLANKS       XXFIL4           40
     C*
     C*  RETRIEVE REMARKS
     C*
     C                   MOVEL     RPRMK1        XXKEY
     C     RCKY1         CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM1           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM1
     C                   END
     C*
     C                   MOVEL     RPRMK2        XXKEY
     C     RCKY1         CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM2           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM2
     C                   END
     C*
     C                   MOVEL     RPRMK3        XXKEY
     C     RCKY1         CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM3           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM3
     C                   END
     C*
     C                   MOVEL     RPCKCN        XXKEY                          JJJ 1-6-89
     C     RCKY1         CHAIN     PRC01                              51        |  ADDED
     C     *IN51         IFEQ      '0'                                          |
     C                   MOVE      RCDESC        XXREM4           40            |
     C                   ELSE                                                   |
     C                   MOVE      *BLANKS       XXREM4                         |
     C                   END                                                    |
     C*
     C*  SET UP FIELD TO PRINT FUND YEAR
     C*
     C                   MOVE      RPFYR         XXFYR             4
     C     XXCTRY        IFEQ      0
     C                   MOVEL     '19'          XXFYR
     C                   ELSE
     C                   MOVEL     '20'          XXFYR
     C                   END
     C*
     C*  LOAD CITY, STATE AND ZIP
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     RPST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     RPCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C                   MOVE      *BLANKS       XXADR4
     C*
     C     RPCTRY        IFNE      *BLANKS
     C     RPCTRY        ANDNE     'USA'
     C                   MOVE(P)   QQCO#         K2CO#
     C                   MOVEL(P)  RPADR3        XXCSZ
     C                   MOVEL(P)  'CTRY'        K2TBID
     C                   MOVEL(P)  RPCTRY        K2KEY
     C     GTKY2         CHAIN     PGT02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXADR4           40
     C                   ELSE
     C                   MOVEL(P)  NF            XXADR4
     C                   ENDIF
     C*
     C                   END
     C*
     C*  CHECK FOR A BLANK SECOND ADDRESS LINE
     C*
     C                   MOVE      '0'           *IN61
     C     RPADR2        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN61
     C                   END
     C*  GET FUND INFORMATION FOR CHECK
     C*******************  MOVE *BLANKS   XXFN   60
     C*******************  MOVE *BLANKS   XXFAD1 40
     C*******************  MOVE *BLANKS   XXFAD2 40
     C*******************  MOVE *BLANKS   XXFCT  25
     C*******************  MOVE *BLANKS   XXFST   2
     C*******************  MOVE *BLANKS   XXFZIP  50
     C*
     C     XXWCMC        IFEQ      '1'
     C     XXFOPN        IFNE      '1'
     C******************** OPEN FAEOL3
     C                   OPEN      FAEOLG
     C**********           OPEN FDCAL2                     DEL 01/21/99
     C**********           OPEN FMRCL1                     DEL 01/21/99
     C**********           OPEN FMSML                      DEL 01/21/99
     C**********           OPEN FDHCL                      DEL 01/21/99
     C**********           OPEN FMRXL                      DEL 01/21/99
     C                   OPEN      FMDIL
     C**********           OPEN FMCDL                      DEL 01/21/99
     C                   OPEN      FMCML1
     C                   OPEN      FMFNL1
     C                   OPEN      FMF1L
     C                   OPEN      FMC1L
     C*********            OPEN FTEBLOLD                   DEL 01/21/99
     C**********           OPEN FMPDL                      DEL 03/23/99
     C                   OPEN      FAESL
     C**********           OPEN FAEAL
     C                   OPEN      FAEAL1
     C                   OPEN      FAEHL
     C                   OPEN      FAEUL
     C                   OPEN      FDDHL
     C                   OPEN      FDDUL
     C                   OPEN      FDDPL
     C                   OPEN      FDDDL
     C                   OPEN      FDDEL
     C                   OPEN      FAEEL
     C**** Beg Chg *** 07/18/23 **************************************
     C****               OPEN      FAEEL1
     C                   OPEN      FAEEL5
     C**** End Chg *** 07/18/23 **************************************
     C                   OPEN      FDF3L
     C                   OPEN      FDF4L
     C                   OPEN      FMCXL
     C                   OPEN      FMEML
     C**********           OPEN FDCAL13
     C                   OPEN      FDCALR
     C**********           OPEN FTABL
     C                   OPEN      FTGTL
     C                   OPEN      NMNWL
     C                   OPEN      FAEPL
     C                   OPEN      FAH1L5
     C                   OPEN      FDF2L
     C                   OPEN      FDCAL5
     C                   OPEN      FMICD10L1
     C                   OPEN      FAEDL
     C                   MOVE      '1'           XXFOPN            1
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   ENDIF
     C*
     C**********           MOVELFNNAME    XXFN
     C**********           MOVE FNMAD1    XXFAD1
     C**********           MOVE FNMAD2    XXFAD2
     C**********           MOVE FNMCTY    XXFCT
     C**********           MOVE FNMST     XXFST
     C**********           MOVELFNMZIP    XXFZIP
     C********** XXFAD2    COMP *BLANKS                  65
     C*
     C**** GET BANK INFORMATION TO PRINT ON CHECK ****
     C*
     C*********  BIKEY     CHAIN$BI01                76
      *
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCYMD                         Batch Date CYMD
      *
     C     BIKY1         SETLL     PBI01
     C     BIKY2         READE     PBI01                                  76
      *
     C     *IN76         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKY2         READE     PBI01                                  76
     C                   ENDDO
      *
     C     *IN76         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
      *
      *
      ** ONLY CHECK FOR # OF VOID DAYS IF NO CLIENT MOD ALREADY EXISTS
      *
     C     CKVOID        IFEQ      'Y'
      *
      * This code initializes the 'Void after x days' display field
      * If the number of days (BI50N3) is *zero the message
      * field (XVOAFT) is blanked out
      *
     C                   MOVE      *BLANKS       XVOAFT
      /Free
       @@Void23 = *blanks;
      /End-Free
      *
     C     BI50N3        IFGT      *ZERO
     C                   MOVE      BI50N3        XVDAYS            3 0
     C                   MOVE      XVDAYS        VDY
     C                   MOVE      *BLANKS       XXVDAY            3
      *
     C     XVDAYS        IFLT      10
     C                   MOVE      *BLANK        VDY1
     C                   MOVE      *BLANK        VDY2
     C                   MOVEL     VDY3          XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFLT      100
     C                   MOVE      *BLANK        VDY1
     C                   MOVEL     VDY2          XXVDAY
     C     XXVDAY        CAT       VDY3:0        XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFGT      99
     C                   MOVE      VDY           XXVDAY
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     XVMSG1        XVOAFT           19
     C     XVOAFT        CAT       XXVDAY:1      XVOAFT
     C     XVOAFT        CAT       XVMSG2:1      XVOAFT
     C                   MOVEL     XVOAFT        @@VOID
     C                   MOVE      *OFF          *IN78
      /Free
       @@Void23 = '- ' + %xlate(Lower:Upper:xvmsg1) + ' ' +
                   %trim(XXVDAY) + ' ' +  %xlate(Lower:Upper:XVMSG2) + ' -';
      /End-Free
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  Assign MACRO to PCL5 control characters with Bank Information Macro ID
      *
     C                   MOVE      BIMACR        TEMP3             3
     C     '&f'          CAT       TEMP3         TEMP5             5
     C     TEMP5         CAT       'y3X'         MACRO             8
      *
     C     BIAXOU        IFEQ      *BLANK
     C                   MOVE      *BLANKS       XXAXOU
     C                   MOVE      XXCNBR        XXAXOU           20
     C                   ELSE
     C                   MOVE      BIAXOU        XXAXOU
     C                   END
     C*
     C     BIBNK#        CAT(P)    '/'           XXBT#5            5
     C     XXBT#5        CAT(P)    BITRNS        XXBT#             9
     C*
     C*
     C     BILOGO        IFEQ      'N'
     C                   MOVE      '1'           *IN73
     C                   END
     C*
     C     BITRST        IFEQ      'Y'
     C                   MOVEL     WCA(2)        XXWCA            35
     C                   ELSE
     C                   MOVEL     WCA(1)        XXWCA            35
     C                   END
      *
      *  Determine if Second Signature Verbage should Print
      *
      /Free
          Lines129 = *Off;
      /End-Free
     C                   MOVE      *OFF          *IN85
     C                   MOVE      *OFF          *IN83
     C     BI$LMT        IFGT      0
     C     RPAMT         ANDGT     BI$LMT
     C                   MOVE      '1'           *IN85
     C                   MOVE      *ON           *IN83
      /Free
       RtvCltID(CltId);
       //*** If CltId = '129';
       If CltId = '129' And BI1A2 = 'Y';
          Lines129 = *On;
       Else;
          Lines129 = *Off;
       EndIf;
      /End-Free
     C                   END
      *
      * Check for First Signature entry
      *
     C     BI10A1        IFNE      *BLANK
     C                   MOVEL     BI10A1        CH1               7
     C                   MOVE      '1'           *IN86
     C                   MOVE      *ON           *IN84
     C                   END
      *
      * Check for Second Signature entry
      * If $$$ Limit entered and Check amount greater than limit,
      * then do NOT print signature
      *
     C                   MOVE      *OFF          *IN87
     C     BI10A2        IFNE      *BLANK
     C                   MOVEL     BI10A2        CH2               7
     C                   MOVE      '1'           *IN87
     C                   MOVE      *ON           *IN83
     C                   END
      *
     C*  Do NOT print the second signature line if the MACRO ID is
     C*    something other than 2.
     C*
     C     ##CLT#        IFEQ      '090'
     C     BIMACR        IFNE      2
     C                   MOVE      *OFF          *IN83
     C                   END
     C                   ENDIF
     C*
      /Free
       // On indicators to print lines, Off to suppress signatures.
       If Lines129 = *on;
          *In84 = *On;
          *In78 = *On;
          *In86 = *Off;
          *In87 = *Off;
       EndIf;
      /End-Free
     C*
      *
      *  Assign "O" spec /COPY fields
      *
     C                   Z-ADD     RPCASE        @@CASE
     C                   Z-ADD     RPCHK#        @@CHK#
B04  C     RPINV#        IFNE      *BLANK
     C                   MOVEL     RPINV#        @@INV#
     C                   MOVE      '1'           *IN63
E04  C                   ENDIF
     C                   Z-ADD     XXDATE        @@DATE
     C     ##CLT#        IFEQ      '139'
     C                   Z-ADD     X2DATE        @@DATE
     C                   ENDIF
     C                   MOVEL     XXNAME        @@NAME
     C*******              Z-ADDRPFEIN    @@FEIN
     C                   MOVE(P)   RPFEIN        @@FEIN            9
     C   15              MOVEL     'XXXXX'       @@FEIN
     C                   MOVE      CDSS#         XXSS#
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYP            25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C*******              MOVELSSPRE     @@SS#  11 P
     C*******              CAT  '-':0     @@SS#
     C*******              CAT  SSMID:0   @@SS#
     C     C1KEY         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C     COPSS#        ANDEQ     'Y'
     C  N15              MOVEL(P)  SSPRE         @@SS#            11
     C   15              MOVEL(P)  'XXX'         @@SS#            11
     C                   CAT       '-':0         @@SS#
     C  N15              CAT       SSMID:0       @@SS#
     C   15              CAT       'XX':0        @@SS#
     C                   CAT       '-':0         @@SS#
     C                   CAT       SSSUF:0       @@SS#
     C                   ELSE
     C                   EVAL      XXSS#=*BLANKS
     C                   MOVEL(P)  *BLANKS       @@SS#
     C                   ENDIF                                                  COPSS# = ' '
     C**N15**              MOVELSSPRE     @@SS#  11 P
     C***15**              MOVEL'XXX'     @@SS#  11 P
     C*******              CAT  '-':0     @@SS#
     C**N15**              CAT  SSMID:0   @@SS#
     C***15**              CAT  'XX':0    @@SS#
     C*******              CAT  '-':0     @@SS#
     C*******              CAT  SSSUF:0   @@SS#
     C                   Z-ADD     XXACDT        @@ACDT
     C                   Z-ADD     RPAMT         @@AMT
     C                   MOVEL     XXAMT1        @@AMT1
     C                   MOVEL     XXAMT2        @@AMT2
     C                   MOVEL     RPPNAM        @@PNAM
     C                   MOVEL     RPADR1        @@ADR1
     C                   MOVEL     RPADR2        @@ADR2
     C                   MOVEL     RPACCT        @@ACCT
     C                   MOVEL     RPMOD         @@MOD
     C                   MOVEL     RPCO#         @@CO#
     C                   MOVEL     RPFND         @@FND
     C                   MOVEL     RPFYR         @@FYR
     C                   MOVEL     RPBR2         @@BR2
     C                   MOVEL     RPRECD        @@RECD
     C                   MOVEL     RPWKS         @@WKS
     C                   MOVEL     RPDAYS        @@DAYS
     C*
     C*  PRINT CHECK
     C*
     C                   MOVE      '0'           *IN66
     C     QQTYPE        IFNE      'G'
     C*
     C********** RPRETP    IFEQ 'M'
     C********** RPEOB#    ANDNE*ZERO
     C     RPEOB#        IFNE      *ZERO
     C     XXWCMC        ANDEQ     '1'
     C     ##CLT#        ANDNE     '136'
     C                   EXSR      MEDCK
     C********** XXWCMC    IFEQ '1'
     C**********           EXSR MEDCK
     C**********           ENDIF
     C*
     C                   ELSE
     C                   EXSR      OTHRCK
     C                   END
     C*
     C                   ELSE
     C*******************  EXCPTFILE2
     C                   If        isUseSpoolChk
     C                   EXCEPT    CHECK
     C                   EXCEPT    DETG
     C                   endif
     C*
     C* CHECK FOR DETAIL PROCESSING
     C*
     C                   EXSR      GETDTL
     C     ##CLT#        IFEQ      '135'
     C                   MOVEL     421571782     XXFEIN
     C                   MOVEL     000           XXMOD
     C                   EXSR      GETHES
     C                   ENDIF
     C                   END
     C*********
     C**Additional information requested by Michigan
     C*********
     C*********  EOAST     IFEQ 'MI'
     C*********            EXSR MIINFO
     C*********            ENDIF
     C*
     C*** Write to Scriptura 1099 Workfile
     C                   If        isScriptChk = 'Y'
     C                   Exsr      wrtSXCCP
     C                   Endif
     C*
     C     NCHK#         TAG
     C                   MOVE      '1'           *IN77
     C*
     C                   END
     C     RPKEY         READE     PRP01                                  50
     C*
     C                   END                                                     END DOWEQ'0'
     C                   END                                                     ENDIF *IN60
     C*
     C*
     C***CLOSE THE PRINTER FILES
     C***
     C***                  CLOSECHECKS
     C**N07                CLOSEFILECOPY
     C***07                CLOSEQSYSPRT3
     C*
     C     TERM          TAG
     C* Call process to generate Scriptura output/Darts if requested.
     C***********        If        xmlFileName <> ' '
     C***********        Eval      QQXml = xmlFileName + '$'
     C***********        Call      'S4401C'
     C***********        Parm                    QQXml            61
     C***********        Endif
     C*
     C                   SETON                                        LR
     C*================================================================
     C* GETHES - HES Group Check Reconciliation Processing
     C*================================================================
     C     GETHES        BEGSR
     C*
     C     RPFEIN        IFEQ      XXFEIN
     C     RPMOD         ANDEQ     XXMOD
     C                   Z-ADD     0             XGCHG
     C                   Z-ADD     0             XTCHG
     C                   Z-ADD     0             XGPAID
     C                   Z-ADD     0             XTPAID
     C                   Z-ADD     0             XGAMT
     C                   Z-ADD     0             XTAMT
     C     RPKY2         CHAIN     PRP02                              99
     C  N99              MOVEL     RPFND         XHFND
     C     FNKEY         CHAIN     PFNWC                              90
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     FNNAME        XHDSC
     C                   ENDIF
     C     EMKEY         CHAIN     PEM01                              90
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     EMRGN#        XHRGN
     C                   ENDIF
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   EXCEPT    HEADH
     C     *IN99         DOWEQ     *OFF
     C     RPFND         IFNE      XHFND
     C                   EXCEPT    HESGP
     C                   Z-ADD     0             XGCHG
     C                   Z-ADD     0             XGPAID
     C                   Z-ADD     0             XGAMT
     C                   EXSR      NWPHES
     C                   ENDIF
     C                   EXSR      HESDTL
     C*
     C  N99              MOVEL     RPFND         XHFND
     C     RPKY2         READE     PRP02                                  99
     C                   ENDDO
     C                   EXCEPT    HESGP
     C                   EXCEPT    HESTOT
     C*
     C                   EXSR      HESDEL
     C                   ENDIF
     C*
     C*Update the Reconciliation Report Date
     C     ENDHES        ENDSR
      /Free

       //-------------------------------------------------------------
       // WrtSXCCP - Write to Scriptura Claim Check workfile
       //--------------------------------------------------------------
           Begsr WrtSXCCP;

              Clear scc0p;

              s_CCXML  = XPxmlT;
              s_CCAXOU = xxAxou;
              s_CCTRAN = xxTran;
              s_CCONUS = BIonus;
              s_CCRTUP = BIrtUp;
              s_CCRTLW = BIrtLw;
              s_CCCHK# = RPchk#;
              s_CC40A1 = BI40A1;
              s_CC40A2 = BI40A2;
              s_CC40A3 = BI40A3;
              s_CC40A4 = BI40A4;
        //*** s_CCDATE = x1Date;
              monitor;
                s_CCDATE = %dec(%date(x1Date: *usa));
              on-error;
                s_CCDATE = 0;
              endmon;
              s_CCCLM# = xxClm#;
              s_CCCLNM = @@name;
      //****  s_CCSS#  = @@ss#;
              s_CCSS#  = xxss#;
              s_CCFEIN = @@fein;
        //*** s_CCACDT = x1AcDt;
              monitor;
                s_CCACDT = %dec(%date(x1AcDt: *usa));
              on-error;
                s_CCACDT = 0;
              endmon;
              s_CCAI01 = BIai01;
              s_CCAMT  = RPamt;
              s_CCAMT1 = @@amt1;
              s_CCAMT2 = @@amt2;
              s_CCPNAM = @@pnam;
              s_CCPAD1 = @@adr1;
              s_CCPAD2 = @@adr2;
              s_CCPAD3 = xxCSZ;
              s_CCPAD4 = xxAdr4;
              s_CCVOID = @@void;
              s_CCSGMS1= SIGMS1(1);
              s_CC$LMT = BI$lmt;
              s_CCSGMS2= SIGMS2(1);
              s_CCCMD1 = BIcmd1;
              s_CCCMD2 = BIcmd2;
              s_CCACCT = @@Acct;
              s_CCVCHR = vouchr;
              s_CCTXT1 = xxTxt1;
              s_CCFNAME= xxFn;
              s_CCFAD1 = xxFad1;
              s_CCFAD2 = xxFad2;
              s_CCFCTY = xxFct;
              s_CCFST  = xxFst;
              s_CCFZIP = xxFzip;
              s_CCMOD  = @@mod;
              s_CCEMP# = EMemp#;
              s_CCDIV  = EMdiv;
              s_CCEMNM = EMname;
              s_CCCO#  = RPco#;
              s_CCFND  = RPfnd;
              s_CCFYR  = RPfyr;
              s_CCCASE = RPcase;
              s_CCADJH = x1Adj;
              s_CCCLEX = CMClex;
              s_CCUSNM = xxUsNm;
              s_CCBR2  = RPbr2;
              s_CCB2DS = xxB2Ds;
              s_CCRECD = @@reCd;
              s_CCRCDS = xxRtD2;
        //*** s_CCFRDT = x2Frdt;
              monitor;
                s_CCFRDT = %dec(%date(x2FrDt: *usa));
              on-error;
                s_CCFRDT = 0;
              endmon;
              s_CCTODT = x2ToDt;
              monitor;
                s_CCTODT = %dec(%date(x2ToDt: *usa));
              on-error;
                s_CCTODT = 0;
              endmon;
              s_CCWKS  = @@wks;
              s_CCDAYS = @@days;
              s_CCREM1 = xxRem1;
              s_CCREM2 = xxRem2;
              s_CCREM3 = xxRem3;
              s_CCREM4 = xxRem3;
              s_CCRMDS = RPrmds;
              s_CCiDt  = xxIdt;
              Write sCC0p;
           Endsr;
       //-------------------------------------------------------------
       // GetXMLFile - Get the XML File Name for the Check/EOB
       //--------------------------------------------------------------
       //**
       //**Begsr GetXMLFile;
       //** outCall = 'XMLREQUEST';
       //** OutPrefix = 'PB';
       //** OutString = 'Claim Check';
       //** StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
       //**  OutPrefix:OutString:OutCo#:OutFnd);
       //**Endsr;
      /end-free
       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
      /Free
       Begsr ScriptXML;

       xmlFile = ' ';
       message = %trim(processType);
       outstring = ' ';
       x1Co# = %char(EAco#);
       x1fnd = %char(EAfnd);

       If isMassReq;

          StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
           'ClaimCheck':'000':'000':massFlag:xmlLink);

          isMassReq = *off;
          xmlFileName = xmlFile;
          xmlLink = xmlFile;
          Chain (xmlFile) xp100;
          XPCat = 'PS';
          Update xp100;

       Endif;

       StLetter('XMLREQUEST':xmlFile:message:'M':'000000000':xxPre:
        outString:x1Co#:x1Fnd:massFlag:xmlLink);

       If xmlFileName = ' ';
          xmlFileName = xmlFile;
       Endif;

       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
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
     C* HESDEL - HES Deleted Records
     C*================================================================
     C     HESDEL        BEGSR
     C*
     C                   Z-ADD     0             XGCHG
     C                   Z-ADD     0             XGPAID
     C                   Z-ADD     0             XGAMT
     C                   Z-ADD     0             XTCHG
     C                   Z-ADD     0             XTPAID
     C                   Z-ADD     0             XTAMT
     C     RPKY3         CHAIN     PRP03                              51
     C**********           MOVELRPFND     XHFND
     C                   SETON                                        18
     C     *IN51         IFEQ      *OFF
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   EXCEPT    HEADH
     C                   ENDIF
     C     *IN51         DOWEQ     *OFF
     C     RPSTAT        IFEQ      'D'
     C                   Z-ADD     0             XGCHG
     C                   Z-ADD     0             XTCHG
     C                   Z-ADD     0             XGPAID
     C                   Z-ADD     0             XTPAID
     C                   Z-ADD     0             XGAMT
     C                   Z-ADD     0             XTAMT
     C     FNKEY         CHAIN     PFNWC                              90
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     FNNAME        XHDSC
     C                   ENDIF
     C     EMKEY         CHAIN     PEM01                              90
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     EMRGN#        XHRGN
     C                   ENDIF
     C********** RPFND     IFNE XHFND
     C**********           EXCPTHESGP
     C**********           Z-ADD0         XGCHG
     C**********           Z-ADD0         XGPAID
     C**********           Z-ADD0         XGAMT
     C**********           EXSR NWPHES
     C**********           ENDIF
     C                   EXSR      HESDTL
     C*
     C                   ENDIF
     C  N99              MOVEL     RPFND         XHFND
     C     RPKY3         READE     PRP03                                  51
     C                   ENDDO
     C**********           EXCPTHESGP
     C                   EXCEPT    HESTOT
     C                   MOVE      '0'           *IN18
     C*
     C     ENDHDL        ENDSR
     C*================================================================
     C* HESDTL - HES Records
     C*================================================================
     C     HESDTL        BEGSR
     C*
     C* Get subscriber #
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXMFND
     C                   ENDIF
     C*
     C* Let's get the claim information
     C*
     C     CMKEY         CHAIN     WMCML1                             51
     C     *IN51         IFEQ      *ON
     C                   MOVE      *BLANKS       CMSS#
     C                   Z-ADD     0             CMFND
     C                   Z-ADD     0             CMFYR
     C                   Z-ADD     0             CMCASE
     C                   ENDIF
     C*
     C***                  Z-ADDCMFND     XHFND
     C                   Z-ADD     CMFYR         XHFYR
     C                   Z-ADD     CMCASE        XHCASE
     C*
     C*  RETRIEVE CLAIMANT DEMOGRAPHICS RECORD
     C*
     C     CDKEY         CHAIN     WMCDL                              51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       XHFSN            10
     C                   MOVE      *BLANKS       XHLSN            10
     C                   ELSE
     C                   MOVE      *BLANKS       XHFSN
     C                   MOVE      *BLANKS       XHLSN
     C                   MOVEL     CDLSN         XHLSN
     C                   MOVEL     CDFSN         XHFSN
     C                   ENDIF
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* SET KEY FIELDS
     C* Invoice Date
     C                   Z-ADD     EOCSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XHCSDT            6 0
     C* Process Date
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XHTRDT            6 0
     C* Billed Amount
     C                   Z-ADD     EOCHRG        XHCHRG
     C                   ADD       EOCHRG        XGCHG
     C                   ADD       EOCHRG        XTCHG
     C* Claim Fee
     C                   Z-ADD     EOPAID        XHPAID
     C                   ADD       EOPAID        XGPAID
     C                   ADD       EOPAID        XTPAID
     C* Check Amount
     C     *IN18         IFEQ      '1'
     C                   Z-ADD     0             XHAMT
     C                   ELSE
     C                   Z-ADD     RPAMT         XHAMT
     C                   ENDIF
     C                   ADD       XHAMT         XGAMT
     C                   ADD       XHAMT         XTAMT
     C* HES Bill ID
     C                   MOVEL     EOSINV        XHSINV
     C                   EXCEPT    DETHES
     C*
     C                   ENDIF
     C     H1KY          CHAIN     PH101                              56
     C     *IN56         IFEQ      *OFF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1RRDT
     C                   UPDATE    PH101
     C                   ENDIF
     C     ENDHDT        ENDSR
     C*================================================================
     C* GETDTL - CHECKS AND PROCESSES DETAIL FOR GROUP CHECK
     C*================================================================
     C     GETDTL        BEGSR
     C*
     C                   SETOFF                                       17
     C     *IN16         IFEQ      *ON
     C     NRKY          CHAIN     PNR01                              60
     C     *IN60         IFEQ      *OFF
     C     NRCHKD        ANDEQ     'Y'
     C     NRSTAT        ANDNE     'I'
     C                   SETON                                        17
     C                   Z-ADD     RPCHK#        XXCHK#
     C                   MOVEL     RPCO#         @HCO#
     C                   Z-ADD     RPCHK#        @HCHK#
     C                   MOVEL     RPFND         @HFND
     C                   MOVEL     RPCKTP        @HCKTP
     C*
     C     RPKY2         CHAIN     PRP02                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL     RPCO#         @@CO#
     C                   MOVEL     RPFND         @@FND
     C                   MOVEL     RPFYR         @@FYR
     C                   MOVEL     RPCASE        @@CASE
     C                   Z-ADD     RPCHK#        @@CHK#
     C***************      Z-ADDXXDATE    @@DATE
     C                   Z-ADD     RPAMT         @@AMT
     C                   MOVEL     RPBR2         @@BR2
     C                   MOVEL     RPRECD        @@RECD
     C                   MOVEL     RPWKS         @@WKS
     C                   MOVEL     RPDAYS        @@DAYS
     C*
     C*  RETRIEVE PAYMENT VALUE 1 DESCRIPTION
     C*
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     RPBR2         XXKEY
     C     RCKY1         CHAIN     PB201                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     B2DESC        XXB2DS           30
     C                   MOVEL     B2DESC        XXB2D2            3
     C                   ELSE
     C                   MOVE      *BLANKS       XXB2DS
     C                   MOVEL     *BLANKS       XXB2D2            3
     C                   END
     C                   MOVEL     *BLANKS       XXKEY
     C*
     C*  RETRIEVE RESERVE CODE DESCRIPTION
     C*
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     RPRECD        XXKEY
     C     RCKY1         CHAIN     PRT01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     RTDESC        XXRTDS           15
     C                   MOVEL     RTDESC        XXRTD2           30
     C                   ELSE
     C                   MOVE      *BLANKS       XXRTDS
     C                   END
     C                   MOVEL     *BLANKS       XXKEY
     C*
     C* Let's get the claim information
     C*
     C     CMKEY         CHAIN     WMCML1                             99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *BLANKS       CMSS#
     C                   Z-ADD     0             CMACDT
     C                   Z-ADD     0             CMEMP#
     C                   Z-ADD     0             CMDIV
     C                   Z-ADD     0             AR
     C                   MOVEL     *BLANK        RT
     C                   MOVEL     *BLANK        CMCTG1
     C                   MOVEL     *BLANK        CMCTG2
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1ACDT
     C                   Z-ADD     XXACDT        @@ACDT
     C*
     C* GET CLAIMS EXAMINER TITLE
     C*
     C                   MOVE      CMCLEX        XXCLEX            3
     C     UPKY2         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       UPTITL
     C                   MOVEL     *BLANKS       UPUSNM
     C                   MOVEL     *BLANKS       XXUSNM
     C                   ELSE
     C                   MOVEL     UPUSNM        XXUSNM           30
     C                   ENDIF
     C*
     C*  RETRIEVE CLAIMANT DEMOGRAPHICS RECORD
     C*
     C     CDKEY         CHAIN     WMCDL                              51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       XXNAME           47
     C                   ELSE
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME
     C                   ENDIF
     C                   MOVEL     XXNAME        @@NAME
     C*
     C*** GET EMPLOYER NAME
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXENAM           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXENAM
     C                   CLEAR                   EMEMP#
     C                   CLEAR                   EMDIV
     C                   END
     C*
     C     RPEOB#        IFNE      *ZEROS
     C     XXWCMC        ANDEQ     '1'
     C     ##CLT#        ANDNE     '136'
     C                   If        isUseSpoolEOB
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   endif
     C                   EXSR      SRSTB1
     C                   ELSE
     C                   Z-ADD     RPAMT         @@AMT
     C                   If        isUseSpoolEOB
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   EXCEPT    DET2
     C                   endif
      * PRINT FRAUD STATEMENT IF INDICATED
     C     B2FLG1        IFEQ      'Y'
     C     CMJRST        ANDEQ     'CA'
     C                   If        isUseSpoolEOB
     C                   EXCEPT    FRAUD
     C                   endif
     C                   ENDIF
     C                   ENDIF
     C*
     C     RPKY2         READE     PRP02                                  99
     C                   ENDDO
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C******  MEDCK  SUBROUTINE ******************************************
     C*
     C     MEDCK         BEGSR
      *
      *  Determine if FEIN is a "Group" Provider
      *  EOMB is printed for "Non Group" Providers only.
      *  If "Group" Provider, print normal stub.
      *
     C     NRKY          CHAIN     PNR01                              60
      *
     C     *IN60         IFEQ      *ON
     C     NRGRTY        OREQ      'O'
     C     *IN60         ANDEQ     *OFF
     C     NRSTAT        OREQ      'I'
     C     *IN60         ANDEQ     *OFF
      *  "Non Group" Provider, print Check and EOMB
      *
     C                   MOVE      '1'           *IN66
     C*
     C                   EXSR      TOPCK
     C*
     C******* CREATE EOMB STUB FOR THIS CHECK ....
     C*
     C                   EXSR      SRSTB1
      *
    C*
     C                   ELSE
      *
      *  "Group" Provider, print Check and stub
      *
     C                   EXSR      OTHRCK
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*
     C******  OTHRCK  SUBROUTINE *****************************************
     C*
     C     OTHRCK        BEGSR
     C*** MED CHECK GETS NO DAYS/WEEKS
     C     RPRETP        COMP      'M'                                    66
     C*
     C                   MOVE      '1'           *IN48
     C                   EXSR      TOPCK
     C*
     C*** DO REGULAR FORMAT *****
     C                   EXSR      SRSTB2
     C*
     C                   ENDSR
     C*
      *
      ******  TOPCK  SUBROUTINE ******************************************
      *
     C     TOPCK         BEGSR
     C*
     C                   If        isUseSpoolEOB
     C  N77              EXCEPT    BANNER
     C*
     C*
     C                   EXCEPT    CHECK
     C                   endif
     C*
     C                   ENDSR
     C*
     **------------------------------------------------------------------*
     C     SRSTB1        BEGSR
     **------------------------------------------------------------------*
     C**********           MOVE *ALL'-'   XXLIN  93
     C**********           MOVE *BLANKS   CD
     C**********
     C* GET SUBSCRIBER #
     C********** FNKY      CHAINPFN01                60
     C********** *IN60     IFEQ '0'
     C**********           Z-ADDFNSUB#    XXSUB#
     C**********           ENDIF
     C**********
     C**********
     C*   GET THE SUBSCRIBER INFORMATION
     C**********
     C********** SMKEY     CHAINPSM01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *BLANKS   XXSBNM 40
     C**********           ELSE
     C**********           MOVELSMNAME    XXSBNM 40
     C**********           END
     C**********
     C* * * GET FUND INFORMATION.
     C**********
     C* CHAIN OUT TO SYSTEM CONTROL FILE TO SEE IF NETWORK IS INSTALLED
     C* IF IT IS(I.E. FBFL06 = Y) MOVE '1' TO *IN74
     C**********
     C**********           MOVE '0'       *IN74
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C********** FBFL06    IFEQ 'Y'
     C**********           MOVE '1'       *IN74
     C**********           END                             END FBFL06
     C**********           END                             END *IN60
      **********
      *  IF THE ALTERNATE NAME FLAG IN WCMC FUND DETAIL IS 'Y'; THE
      *  COMPANY NAME/ADDRESS WILL COME FROM THERE, OTHERWISE FROM THE
      *  COMPANY AUXILARY FILE
      **********
     C**********           MOVE '0'       *IN84
      **********
     C********** F1KY      CHAINPF101                50
      **********
     C********** *IN50     IFEQ '0'
     C********** F1FLG1    IFEQ 'Y'
     C**********           MOVE '1'       *IN84
     C**********           MOVE F1AZIP    XSZIP
     C**********           MOVE F1ANME    XSNAME           FOR EOB FORM
     C**********           MOVE F1ANME    XXCONM 40        FOR REPORT
     C**********           MOVEL'('       XCPHN#
     C**********           Z-ADDF1AARA    XXAREA
     C**********           MOVE ')'       XXCPRN
     C**********           MOVELF1APHN    XXPH1
     C**********           MOVE F1APHN    XXPH2
     C**********           MOVE '-'       XXHP1
     C**********           MOVE '-'       XFHYP1
     C**********           MOVE '-'       XFHYP2
     C**********           MOVE '-'       XFHYP3
      **********
      **********
      *** REMOVE LEADING BLANKS FROM THIS NAME
      **********
     C**********           Z-ADD1         Z       50
     C********** *BLANK    LOKUPXCNM,Z               60
     C********** *IN60     IFEQ '1'
     C**********           MOVEAXCNM,Z    XCNAME
     C**********           ELSE
     C**********           MOVE *BLANKS   XCNAME
     C**********           ENDIF
      **********
     C**********           MOVELF1AAD1    XCADR1 30
     C**********           MOVELF1AAD2    XCADR2 30
      **********
     C**********           MOVE *BLANKS   SZ
     C**********           MOVE *BLANKS   ST
     C**********           MOVE *BLANKS   XXZP1
     C**********           MOVE *BLANKS   XXZP2
     C**********           MOVELF1AST     XXST   13
     C**********           MOVEAXXST      SZ
     C**********           MOVELF1AZIP    C1ZPP1
     C**********           MOVELF1AZIP    XXZP1   5
     C**********           MOVEAXXZP1     SZ,4
     C**********           MOVE F1AZIP    C1ZPP2
     C**********           MOVE F1AZIP    XXZP2   4
     C********** XXZP2     IFNE '0000'
     C**********           MOVE '-'       SZ,9
     C**********           MOVEAXXZP2     SZ,10
     C**********           ENDIF
      **********
     C**********           MOVE *BLANKS   XXSZ   13
     C**********           MOVEASZ        XXSZ
     C**********           MOVE *BLANKS   XXLAST 56
     C**********           MOVE *BLANKS   XXFRST 15
     C**********           MOVELF1ACTY    XXLAST
     C**********           MOVELXXSZ      XXFRST
     C**********           MOVE '1'       XXTERM  1
     C**********           CALL 'SRNAME'
     C**********           PARM           XXLAST
     C**********           PARM           XXFRST
     C**********           PARM           XXTERM
     C**********           MOVELXXLAST    XCCSZ  30
     C**********           ENDIF
     C**********           ENDIF
      **********
     C********** *IN84     IFEQ '0'
      **********
      * GET COMPANY NAME & ADDRESS
     C********** C1KEY     CHAINPC101                60
     C********** *IN60     IFEQ '0'
     C**********
     C**********           MOVE CXZIP     XSZIP
     C**********           MOVE CXNAME    XSNAME
     C**********           MOVEL'('       XCPHN#
     C**********           Z-ADDCXAREA    XXAREA
     C**********           MOVE ')'       XXCPRN
     C**********           MOVELCXPHN#    XXPH1
     C**********           MOVE CXPHN#    XXPH2
     C**********           MOVE '-'       XXHP1
     C**********           MOVE '-'       XFHYP1
     C**********           MOVE '-'       XFHYP2
     C**********           MOVE '-'       XFHYP3
     C**********
     C*** REMOVE LEADING BLANKS FROM THIS NAME
     C**********
     C**********           Z-ADD1         Z       50
     C********** *BLANK    LOKUPXCNM,Z               60
     C********** *IN60     IFEQ '1'
     C**********           MOVEAXCNM,Z    XCNAME 30
     C**********           ELSE
     C**********           MOVE *BLANKS   XCNAME
     C**********           END
     C**********
     C**********           MOVELCXADR1    XCADR1 30
     C**********           MOVELCXADR2    XCADR2 30
     C**********
     C**********           MOVE *BLANKS   SZ
     C**********           MOVE *BLANKS   ST
     C**********           MOVE *BLANKS   XXZP1
     C**********           MOVE *BLANKS   XXZP2
     C**********           MOVELCXST      XXST   13
     C**********           MOVEAXXST      SZ
     C**********           MOVELCXZIP     C1ZPP1
     C**********           MOVELCXZIP     XXZP1   5
     C**********           MOVEAXXZP1     SZ,4
     C**********           MOVE CXZIP     C1ZPP2
     C**********           MOVE CXZIP     XXZP2   4
     C********** XXZP2     IFNE '0000'
     C**********           MOVE '-'       SZ,9
     C**********           MOVEAXXZP2     SZ,10
     C**********           END
     C**********
     C**********           MOVE *BLANKS   XXSZ   13
     C**********           MOVEASZ        XXSZ
     C**********           MOVE *BLANKS   XXLAST 56
     C**********           MOVE *BLANKS   XXFRST 15
     C**********           MOVELCXCTY     XXLAST
     C**********           MOVELXXSZ      XXFRST
     C**********           MOVE '1'       XXTERM  1
     C**********           CALL 'SRNAME'
     C**********           PARM           XXLAST
     C**********           PARM           XXFRST
     C**********           PARM           XXTERM
     C**********           MOVELXXLAST    XCCSZ  30
     C**********           END                             ENDIF *IN60
     C**********
     C**********           ENDIF                           *IN84
     C**********
     C**********
     C**INDEX*FOR CODE ARRAY
     C**********           Z-ADD0         ZZ      20
     ************ CLEAR CODE ARRAY?????*************************
     C**********
     C**********
     C********** EOKEY     CHAINPEO01                60
     C**********
     C********** *IN60     IFEQ '0'
     C**********           MOVE *BLANKS   XXPRNM 30
     C**********           MOVELEOPNAM    XXPRNM
     C**********           MOVE EOFEIN    XXPID   90
     C**********           MOVE EOMOD     XXPIDM  30
     C**********           MOVELEOADR1    XXPADD 30
     C**********           MOVELEOCTY     XXPCTY 25
     C**********           MOVELEOST      XXPST   2
     C**********           MOVELEOZIP     XXPZP1  50
     C**********           MOVE EOZIP     XXPZP2  40
     C**********           MOVE EOAST     XXAST   2        ADD 09/28/98
     C********** XXPZP2    IFNE *ZERO
     C**********           MOVE '1'       *IN66
     C**********           ELSE
     C**********           MOVE '0'       *IN66
     C**********           END
     C**********
     C**********           ENDIF
     C**********
     C**GET*STAFF CODE OF INITIAL EOMB USER
     C********** UPKY1     CHAINPUP02                60
     C********** *IN60     IFEQ '1'
     C**********           MOVEL*BLANKS   UPSTAF
     C**********           MOVEL*BLANKS   XXMEDX 30
     C**********           ELSE                            *IN60 = ON
     C**********           MOVELUPUSNM    XXMEDX    P
     C**********           END
     C**********
     C********** CMKY1     CHAINPCM02                51
     C**********
     C********** *IN51     IFEQ '0'
     C**********
     C**********           Z-ADDC2ACDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     X2ACDT  60
     C**********
     C**********           MOVE C2SS#     X2SS#   9
     C**********
     C********** C2SS#     CHAINPCD02                52
     C********** *IN52     IFEQ '0'
     C**********           MOVE *BLANKS   XXLAST 56
     C**********           MOVE *BLANKS   XXFRST 15
     C**********           MOVELC2LSN     XXLAST 56
     C********** C2FSN     IFNE *BLANK
     C**********           MOVELC2FSN     XXFRST 15
     C**********           MOVE '1'       XXTERM  1
     C**********           CALL 'SRFRLS'
     C**********           PARM           XXLAST
     C**********           PARM           XXFRST
     C**********           PARM           XXTERM
     C**********           END
     C**********           MOVELXXLAST    XXPNAM 40
     C**********           MOVELXXLAST    XZPNAM 30        LJD 8/25/88
     C**********           ELSE
     C**********           MOVEL*BLANKS   XXPNAM 40
     C**********           MOVEL*BLANKS   XZPNAM           LJD 8/25/88
     C**********           END
     C**********
     C**********           ELSE
     C**********           Z-ADD0         XXACDT  60
     C**********           MOVE *BLANKS   XXSS#   9
     C**********           MOVE *BLANKS   XXCCLM 13
     C**********           MOVEL*BLANKS   XXPNAM 40
     C**********           MOVEL*BLANKS   XZPNAM
     C**********           END
     C**********
     C**COMBINE*CITY, STATE, ZIP FOR PROVIDER
     C**********           MOVE *BLANKS   SZ
     C**********           MOVE *BLANKS   ST
     C**********           MOVE *BLANKS   XXZP1
     C**********           MOVE *BLANKS   XXZP2
     C**********           MOVELXXPST     XXST   13
     C**********           MOVEAXXST      SZ
     C**********           MOVE XXPZP1    XXZP1   5
     C**********           MOVEAXXZP1     SZ,4
     C**********           MOVE XXPZP2    XXZP2   4
     C********** XXZP2     IFNE '0000'
     C**********           MOVE '-'       SZ,9
     C**********           MOVEAXXZP2     SZ,10
     C**********           END
     C**********
     C**********           MOVE *BLANKS   XXSZ   13
     C**********           MOVEASZ        XXSZ
     C**********           MOVE *BLANKS   XXLAST 56
     C**********           MOVE *BLANKS   XXFRST 15
     C**********           MOVELXXPCTY    XXLAST
     C**********           MOVELXXSZ      XXFRST
     C**********           MOVE '1'       XXTERM  1
     C**********           CALL 'SRNAME'
     C**********           PARM           XXLAST
     C**********           PARM           XXFRST
     C**********           PARM           XXTERM
     C**********           MOVELXXLAST    XPCSZ  30
     C**********
     C**********           MOVE EOEOB#    XXEOB#
     C**********
     C**FILE*NUMBER
     C**********           Z-ADDEOSUB#    XFSUB
     C**********           Z-ADDEOFND     XFFND
     C**********           Z-ADDEOFYR     XFYR
     C**********           Z-ADDEOCASE    XFCASE
     C**********
     C********** 100.0001  MULT EOFRDT    XXFRD   60
     C********** 100.0001  MULT EOTODT    XXTOD   60
     C**********
     C**GET*CLAIMS EXAMINER TITLE
     C**********           MOVE CMCLEX    XXSTAF  3
     C********** UPKY      CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVEL*BLANKS   UPTITL
     C**********           MOVEL*BLANKS   UPUSNM
     C**********           END
     C**********
     C**GET*4*DIAGNOSIS CODES & DESCRIPTIONS
     C********** EODIA1    IFNE *BLANKS
     C**********           MOVE '1'       *IN67
     C**********           MOVE EODIA1    XXDIAC
     C********** DIKEY     CHAINPDI01                60
     C********** *IN60     IFEQ '0'
     C**********           MOVE DIDSC1    XXDSC1 50
     C**********           END
     C**********           ELSE
     C**********           MOVE '0'       *IN67
     C**********           END
     C********** EODIA2    IFNE *BLANKS
     C**********           MOVE '1'       *IN68
     C**********           MOVE EODIA2    XXDIAC
     C********** DIKEY     CHAINPDI01                60
     C********** *IN60     IFEQ '0'
     C**********           MOVE DIDSC1    XXDSC2 50
     C**********           END
     C**********           ELSE
     C**********           MOVE '0'       *IN68
     C**********           END
     C********** EODIA3    IFNE *BLANKS
     C**********           MOVE '1'       *IN69
     C**********           MOVE EODIA3    XXDIAC
     C********** DIKEY     CHAINPDI01                60
     C********** *IN60     IFEQ '0'
     C**********           MOVE DIDSC1    XXDSC3 50
     C**********           END
     C**********           ELSE
     C**********           MOVE '0'       *IN69
     C**********           END
     C********** EODIA4    IFNE *BLANKS
     C**********           MOVE '1'       *IN70
     C**********           MOVE EODIA4    XXDIAC
     C********** DIKEY     CHAINPDI01                60
     C********** *IN60     IFEQ '0'
     C**********           MOVE DIDSC1    XXDSC4 50
     C**********           END
     C**********           ELSE
     C**********           MOVE '0'       *IN70
     C**********           END
     C**********
     C**********
     C**********           EXSR SRDET
     C**********
     C**********           ENDSR
     C*
     C                   MOVE      *ZERO         *IN72
     C                   MOVE      *BLANKS       TMPVAR
     C                   CLEAR                   SVFARA
     C                   CLEAR                   SVFPH1
     C                   CLEAR                   SVFPH2
     C* GET SUBSCRIBER #
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXMFND
     C                   MOVE      'Y'           TMPVAR            1
     C                   MOVEL     FNFARA        SVFARA
     C                   MOVEL     FNFAX         SVFPH1            3
     C                   MOVE      FNFAX         SVFPH2            4
     C                   ENDIF
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
     C                   CLEAR                   XXCSZ1
     C                   CLEAR                   XXCSZ
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
     C                   CLEAR                   X1PAID
     C                   CLEAR                   X2PAID
     C                   CLEAR                   XXENAM
     C                   CLEAR                   XXFARA
     C                   CLEAR                   XXFPH1
     C                   CLEAR                   XXFPH2
     C                   clear                   xxDrg#
     C                   clear                   xxDgDs
     C* GET BILL
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
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
     C     EAKY2         CHAIN     PEA01                              98
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     EAWCPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYM2
     C                   MOVEL     SRDTO         XXWCPD            8 0
     C                   ENDIF
      //**** Beg Add *** 08/22/22 *************************************
        If EOast = 'FL';
           *in53 = *on;
        Else;
           *in53 = *off;
        EndIf;
      //**** End Add *** 08/22/22 *************************************
     C**** Beg Chg *** 08/22/22 *************************************
     C**** EOAST         IFEQ      'FL'
     C****               MOVE      *ON           *IN53
     C     EOAST         IFEQ      'FL'
     C     ModernEob     OREQ      'Y'
     C**** End Chg *** 08/22/22 *************************************
     C                   EXSR      SRSTBL
     C                   ELSE
     C                   MOVE      *OFF          *IN53
     C     EOAST         IFEQ      'NC'
     C     TMPVAR        ANDEQ     'Y'
     C                   MOVE      *ON           *IN72
     C                   ELSE
     C                   MOVE      *OFF          *IN72
     C                   ENDIF

     C*
     C*  Load the claim# field which prints on the EOMB stub.
     C*
     C     ##CLT#        IFEQ      '114'
     C     CMACL#        ANDNE     *BLANKS
     C     CMFND         ANDEQ     440
     C                   MOVEL(P)  CMACL#        XXCL#            21
     C*
     C                   ELSE
     C                   Z-ADD     EOSUB#        XFSUB
     C                   MOVE      '-'           XFHYP1
     C                   Z-ADD     EOFND         XFFND
     C                   MOVE      '-'           XFHYP2
     C                   Z-ADD     EOFYR         XFYR
     C                   MOVE      '-'           XFHYP3
     C                   Z-ADD     EOCASE        XFCASE
     C                   MOVEL(P)  XF#           XXCL#
     C                   ENDIF
     C                   MOVE      '0'           *IN35
     C     EOBNPI        IFNE      *BLANKS
     C                   MOVEL(P)  EOBNPI        XXBNPI
     C                   SETON                                        35
     C                   ELSE
     C                   SETOFF                                       35
     C                   ENDIF
     C*
     C* SET INDICATORS FOR CONDITIONAL DISPLAY OF DATA
     C*
     C* IF BILL TYPE IS 'HOSPITAL', THEN DISPLAY 'TO SERVICE DATE'
     C*
     C                   MOVE      *OFF          PCM               1
     C     EOBTYP        IFEQ      'H'
     C                   SETON                                        40
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C     EUPCM         ANDNE     *BLANKS
     C                   MOVE      *ON           PCM               1
     C                   ENDIF
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
     C*** CLOSE FILE IF OPENED
     C     FLOPN         IFEQ      '1'
     C     PCM           ANDEQ     *ON
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           FLOPN             1
     C                   END                                                    STOPN = '1'
     C*
     C* DELETE OVERRIDE ON FMPRL
     C*
     C     FLOVR         IFEQ      '1'
     C     PCM           ANDEQ     *ON
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           FLOVR             1
     C                   ENDIF                                                  STOVR = '1'
     C*
     C* OVERRIDE FMPRL TO ADJUDICATION STATE
     C*
     C     FLOVR         IFEQ      ' '
     C     PCM           ANDEQ     *ON
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(4)        @@CMD
     C                   MOVEL     EOAST         @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           FLOVR
     C                   ENDIF                                                  STOVR = ' '
     C*
     C*** OPEN FILE FOR PROCESSING
     C     FLOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C     PCM           ANDEQ     *ON
     C                   OPEN      FMPRL
     C                   MOVE      '1'           FLOPN
     C                   END                                                      STOPN NE '1'
     C*
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
     C                   MOVEL(P)  ##LAST        XXNAM1           40
     C*
     C* GET EMPLOYER AND DIVISION FOR CLAIM MASTER FILE
     C*
     C     CMKY          CHAIN     PCM02                              97
     C     *IN97         IFEQ      *OFF
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C     EMKY          CHAIN     PEM02                              97
     C     *IN97         IFEQ      *OFF
     C                   MOVEL     EMNAME        XXENAM
     C                   ELSE
     C                   CLEAR                   XXENAM
     C                   ENDIF                                                  *IN97 = *OFF
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C* PUT ACCIDENT DATE IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOI             8 0
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
     C********** PDKY      CHAINPPD01                98
     C**********
     C********** *IN98     IFEQ *OFF
     C     EOADR2        IFNE      *BLANKS
     C********** PDADR2    ANDNE*BLANKS
     C                   SETON                                        32
     C                   ELSE
     C                   SETOFF                                       32
     C                   ENDIF                                                  PDADR2 = *BLANKS
     C*
     C                   MOVEL(P)  EOCTY         XXCTY
     C                   MOVE      EOST          XXSTAT
     C                   MOVE      EOZIP         XXZIP
     C**********           MOVELPDCTY     XXCTY     P
     C**********           MOVE PDST      XXSTAT
     C**********           MOVE PDZIP     XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ1           40
     C     EOADR3        IFNE      *BLANKS
     C                   MOVEL(P)  EOADR3        XXCSZ1
     C                   ENDIF
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
     C********** EAKY2     CHAINPEA01                98
     C********** *IN98     IFEQ *OFF
     C**********           Z-ADDX1DATE    SRDTI
     C**********           MOVE *BLANKS   SRDTO
     C**********           MOVE *BLANKS   SRERR
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     XXWCPD  80
     C**********           ENDIF                           *IN98 = *OFF
     C*
     C*
     C* GET 'FROM' AND 'TO' SERVICE DATE
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZFRDT            8 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZTODT            8 0
     C* GET PATIENT ACCOUNT# BASE ON TYPE OF BILL
     C*
     C                   SETOFF                                       37
     C                   MOVE      *OFF          *IN36
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C     EHKY          CHAIN     PEH01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  EHACCT        XXACCT
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'H'
     C     EUKY          CHAIN     PEU01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  EUACCT        XXACCT
     C*
     C*
     C* GET OTHER HOSPITAL SPECIFIC INFORMATION FOR DISPLAY
     C*
     C* DRG code
     C                   eval      xxDRG# = EUdrg#
     C                   eval      xxDgDs = EUdgDs
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
     C                   MOVE      'BLTY'        KKTBID
     C     EOAST         CAT(P)    'BT':0        KKTBID            4
     C                   MOVEL(P)  KKTBID        KKTBID
     C                   MOVEL(P)  KKTBID        BTTBID
     C     GTKY3         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C     GTKY3         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        KKTBID            4
     C                   MOVEL(P)  'BLTY'        BTTBID            4
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL(P)  XXBLTY        KXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXBDSC           30
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C*
     C* GET PROCEDURE CODE DESCRIPTIONS
     C*
     C                   MOVE      *BLANKS       XXPRD1           35
     C                   MOVE      *BLANKS       XXPRD2           35
     C                   MOVE      *BLANKS       XXPRD3           35
     C                   MOVE      *BLANKS       XXPRD4           35
     C*
     C     PCM           IFEQ      *ON
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
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C********** EOBTYP    WHEQ 'X'
     C********** EOBTYP    OREQ 'D'
     C**********           MOVE *BLANKS   XXACCT
     C     EOBTYP        WHENEQ    'X'
     C     EPKY          CHAIN     PEP01                              99
     C                   MOVE      *BLANKS       XXACCT
     C                   MOVE      *BLANKS       XXPHAR
     C     EPPHAR        IFNE      *BLANKS
     C                   MOVEL(P)  EPPHAR        XXPHAR
     C                   SETON                                        37
     C                   ENDIF
     C                   MOVE      *OFF          *IN36
     C********** ##CLT#    IFEQ '133'
     C     EPTAX         IFEQ      'Y'
     C                   MOVE      *ON           *IN36
     C                   ENDIF
     C**********           ENDIF
     C*
     C     EOBTYP        WHENEQ    'D'
     C                   MOVE      *BLANKS       XXACCT
     C                   ENDSL
     C*
     C* GET MEDICAL EXAMINER HEADING, MEDICAL EXAMINER VALUES,
     C* AND REDUCTIONS HEADINGS
     C*
     C                   MOVE      *BLANKS       XXMUSR           15
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
     C                   MOVEL     ##USER        XXMUSR
     C                   ENDIF                                                  ##USER = 'NONE'
     C*
     C     CXKY1         CHAIN     PCX02                              98
     C*
     C* GET LICENCE #/MEDICARE # BASED ON TYPE OF BILL
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
     C******************** MOVELEUMCAR    XXLIC#    P
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
     C***                MOVE      *BLANKS       XXDS1             6
     C***                MOVE      *BLANKS       XXDS2             6
     C***                MOVE      *BLANKS       XXDS3             6
     C***                MOVE      *BLANKS       XXDS4             6
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
       If EUicdInd = 'Y';
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
     C***  EODIA1        IFNE      *BLANKS
     C***                MOVEL(P)  EODIA1        KKICD9
     C***  DIKY          CHAIN     PDI01                              98
     C***  *IN98         IFEQ      *OFF
     C***                MOVEL(P)  DIDSC1        DESC1            30
     C***                MOVEL(P)  DIDS1         XXDS1
     C***                MOVE      '1.'          XXNUM1
     C***                ENDIF                                                  *IN98 = *OFF
     C***                ENDIF                                                  EODIA1 NE *BLANKS
     C*
     C***  EODIA2        IFNE      *BLANKS
     C***                MOVEL(P)  EODIA2        KKICD9
     C***  DIKY          CHAIN     PDI01                              98
     C***  *IN98         IFEQ      *OFF
     C***                MOVEL(P)  DIDSC1        DESC2            30
     C***                MOVEL(P)  DIDS1         XXDS2
     C***                MOVE      '2.'          XXNUM2
     C***                ENDIF                                                  *IN98 = *OFF
     C***                ENDIF                                                  EODIA2 NE *BLANKS
     C*
     C***  EODIA3        IFNE      *BLANKS
     C***                MOVEL(P)  EODIA3        KKICD9
     C***  DIKY          CHAIN     PDI01                              98
     C***  *IN98         IFEQ      *OFF
     C***                MOVEL(P)  DIDSC1        DESC3            30
     C***                MOVEL(P)  DIDS1         XXDS3
     C***                MOVE      '3.'          XXNUM3
     C***                ENDIF                                                  *IN98 = *OFF
     C***                ENDIF                                                  EODIA3 NE *BLANKS
     C*
     C***  EODIA4        IFNE      *BLANKS
     C***                MOVEL     EODIA4        KKICD9
     C***  DIKY          CHAIN     PDI01                              98
     C***  *IN98         IFEQ      *OFF
     C***                MOVEL(P)  DIDSC1        DESC4            30
     C***                MOVEL(P)  DIDS1         XXDS4
     C***                MOVE      '4.'          XXNUM4
     C***                ENDIF                                                  *IN98 = *OFF
     C***                ENDIF                                                  EODIA4 NE *BLANKS
     C                   if        isScriptEOB = 'Y'
     C                   exsr      wrtDGwrk
     C                   endif
     C*
     C* PUT FROM/TO SERVICE DATES IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOI
     C*
     C                   TIME                    DSDTTM
     C                   SETOFF                                       8081
     C                   MOVEL     *BLANKS       XXNW2           140
     C                   MOVEL     *BLANKS       XXNW3           140
     C*
     C* SET INDICATOR TO DISPLAY NETWORK DISCLAIMER IF NECESSARY
     C*
     C     EONTWK        IFNE      *BLANKS
     C     EONDIS        ANDNE     *ZEROS
     C     EONTWK        ORNE      *BLANKS
     C     EONDSC        ANDNE     *BLANKS
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
     C* GET COMPANY INFORMATION - BASED ON ALTERNATE ADDRESS FLAG
     C*
     C     F1KY          CHAIN     PF101                              98
     C     *IN98         IFEQ      *OFF
     C     F1FLG1        IFEQ      'Y'
     C******************** MOVELF1ANME    XXANME 25 P
     C******************** MOVELF1AAD1    XXAAD1 25 P
     C                   MOVEL(P)  F1ANME        XXANME           40
     C                   MOVEL(P)  F1AAD1        XXAAD1           40
     C     F1AAD2        IFNE      *BLANKS
     C******************** MOVELF1AAD2    XXAAD2 25 P
     C                   MOVEL(P)  F1AAD2        XXAAD2           40
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF                                                  F1AAD2 NE *BLANKS
     C                   MOVEL(P)  F1ACTY        XXCTY
     C                   MOVE      F1AST         XXSTAT
     C                   MOVE      F1AZIP        XXZIP
     C                   EXSR      SRADDR
     C******************** MOVELXXCSZ     XXCSZ2 27 P
     C                   MOVEL(P)  XXCSZ         XXCSZ2           40
     C                   MOVEL     F1AARA        XXARA
     C                   MOVEL     F1APHN        XXPHN1            3
     C                   MOVE      F1APHN        XXPHN2            4
     C     *IN72         IFEQ      *ON
     C                   MOVEL     SVFARA        XXFARA
     C                   MOVEL     SVFPH1        XXFPH1            3
     C                   MOVE      SVFPH2        XXFPH2            4
     C                   ENDIF
     C                   ENDIF                                                  FLFLG1 = 'Y'
     C     F1FLG1        IFEQ      'N'
     C     C1KY          CHAIN     PC101                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  CXNAME        XXANME
     C                   MOVEL(P)  CXADR1        XXAAD1
     C     CXADR2        IFNE      *BLANKS
     C                   MOVEL(P)  CXADR2        XXAAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF
     C                   MOVEL(P)  CXCTY         XXCTY
     C                   MOVEL     CXST          XXSTAT
     C                   MOVE      CXZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2
     C                   MOVE      CXAREA        XXARA
     C                   MOVEL     CXPHN#        XXPHN1
     C                   MOVE      CXPHN#        XXPHN2
     C     *IN72         IFEQ      *ON
     C                   MOVE      CXFARA        XXFARA
     C                   MOVEL     CXFAX#        XXFPH1
     C                   MOVE      CXFAX#        XXFPH2
     C                   ENDIF
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  F1FLG1 = 'N'
     C                   ENDIF                                                  *IN98 = *OFF
     C**********           EXCPTNORMAL
     C                   EXSR      MSKSS#
     C                   If        isUseSpoolEOB
     C                   EXCEPT    HEADA
     C                   EXCEPT    HEADB
     C                   EXCEPT    HEADC
     C                   endif
     C*
     C*** Write to Scriptura Header EOB workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtHdWrk
     C                   Endif
     C*
     C* CYCLE THROUGH FDCAP FILE TO GET ALL CHARGES FOR THE BILL
     C*
     C                   CLEAR                   EOB
     C********** CAKY2     CHAINPCA01                98
     C********** *IN98     DOWEQ*OFF
     C*
     C*
     C                   MOVE      *LOVAL        KKFDAT
     C     CAKY1         SETLL     PCA01
     C     CAKY2         READE     PCA01                                  98
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
     C                   CLEAR                   XXPRDS
     C                   CLEAR                   XXBILD
     C                   CLEAR                   X1EOB
     C                   CLEAR                   X2EOB
     C                   CLEAR                   X3EOB
     C                   CLEAR                   X4EOB
     C                   CLEAR                   XNOTE
     C                   CLEAR                   X1UNTS
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
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL15
     C                   Endif
     C                   ENDIF
     C     *IN98         DOWEQ     *OFF                                         *IN98 = *OFF
     C                   Z-ADD     0             X1TAX             9 2
     C                   CALL      'FA774R'
     C                   PARM      CARRN         QQRRN             9 0
     C     X1TAX         PARM      0             QQTAX             9 2
     C     CAFDAT        IFNE      HLFDAT
     C     CATDAT        ORNE      HLTDAT
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
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL15
     C                   endif
     C                   ENDIF
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
     C     DHBPRC        IFNE      *BLANKS
     C                   MOVEL(P)  DHBPRC        XXBILD           11
     C                   SETON                                        34
     C                   ELSE
     C                   CLEAR                   XXBILD
     C                   SETOFF                                       34
     C                   ENDIF                                                  DHBRPC <> *BLANKS
     C*
     C* UNITS OF THE CHARGES
     C*
     C                   Z-ADD     DHUNTS        X1UNTS            5 2
     C*
     C* For client id 133 and state LA, populate NDC codes.
     C*
      /Free
         //If ##CLT# = '133' and EOAST = 'LA' and DHNDC <> *blanks ;

         If (##CLT# = '133' Or ##CLT# = '136') and
             DHNDC <> *blanks ;

            XXBILD = *Blanks;
         // XXBILD = DHNDCD ;
            XXCPT4 = DHNDC ;
         Endif ;
      /End-Free
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
     C                   Z-ADD     DUUNTS        X1UNTS
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
     C                   Z-ADD     DPUNTS        X1UNTS
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
     C                   Z-ADD     DDUNTS        X1UNTS
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
     C**********           SELEC
     C********** DHDXR1    WHEQ 'X'
     C**********           MOVELEODIA1    XXICD9    P
     C********** DHDXR2    WHEQ 'X'
     C**********           MOVELEODIA2    XXICD9    P
     C********** DHDXR3    WHEQ 'X'
     C**********           MOVELEODIA3    XXICD9    P
     C********** DHDXR4    WHEQ 'X'
     C**********           MOVELEODIA4    XXICD9    P
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
     C*****                ENDSL
     C                   ENDIF                                                  EOBTYP = 'N'
     C*
     C* GET EOB CODES
     C*
      //**** Beg Chg *** 08/22/22 *************************************
     C****               Z-ADD     1             J                 1 0
       J = 1;
      //**** End Chg *** 08/22/22 *************************************
     C                   MOVE      *BLANKS       X1EOB
     C                   MOVE      *BLANKS       X2EOB
     C                   MOVE      *BLANKS       X3EOB
     C                   MOVE      *BLANKS       X4EOB
     C                   MOVE      *BLANKS       XNOTE
     C                   SETOFF                                       424344
     C                   SETOFF                                       45
     C     DEKY          CHAIN     PDE01                              98
     C**N98*****           SETON                     46
     C***98*****           SETOF                     46
     C******************** SETOF                     46
     C     *IN98         DOWEQ     *OFF
     C********** J         ANDLT4
     C     J             ANDLE     4
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
     C******************** SETON                     46
     C                   SELECT
     C     J             WHENEQ    1
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X1EOB
     C                   MOVE      DEEOBCD       X1EOB
     C**** End Chg *** 07/18/23 **************************************
     C     X1EOB         IFNE      *BLANKS
     C                   SETON                                        42
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF                                                  X1EOB NE *BLANKS
     C     J             WHENEQ    2
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X2EOB
     C                   MOVE      DEEOBCD       X2EOB
     C**** End Chg *** 07/18/23 **************************************
     C     X2EOB         IFNE      *BLANKS
     C                   SETON                                        43
     C                   ELSE
     C                   SETOFF                                       43
     C                   ENDIF                                                  X2EOB NE *BLANKS
     C     J             WHENEQ    3
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X3EOB
     C                   MOVE      DEEOBCD       X3EOB
     C**** End Chg *** 07/18/23 **************************************
     C     X3EOB         IFNE      *BLANKS
     C                   SETON                                        44
     C                   ELSE
     C                   SETOFF                                       44
     C                   ENDIF                                                  X3EOB NE *BLANKS
     C     J             WHENEQ    4
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
     C                   ADD       1             J
     C*
     C* LOAD ALL DETAIL EOB CODES INTO ARRAY 'EOB'.  THE HEADER EOB
     C* CODES WILL BE ADDED TO THIS ARRAY LATER.  THIS ARRAY IS USED
     C* TO DISPLAY THE DESCRIPTIONS OF ALL EOB CODES FOR THE BILL
     C* IN ONE LOCATION.
     C*
     C********** DEEOB     IFNE *BLANKS
     C**********           Z-ADD1         Y       20
     C********** DEEOB     LOKUPEOB,Y                    41
     C********** *IN41     IFEQ *OFF                       not in array
     C**********           Z-ADD1         Z       20
     C********** *BLANKS   LOKUPEOB,Z                    31
     C********** *IN31     IFEQ *ON
     C**********           MOVE DEEOB     EOB,Z
     C**********           ENDIF                           *IN31 = *ON
     C**********           ENDIF                           *IN41 = *OFF
     C**********           ENDIF                           DEEOB NE *BLANKS
     C*
     C*** Write to Scriptura Detail EOB Code Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetCdWrk
     C                   Endif
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
     C     DESTAT        ANDNE     'I'
     C*****              MOVEL(P)  DEEOB         XXTYPE            1
     C*****XXTYPE        IFEQ      'P'
     C*****XXTYPE        OREQ      'R'
     C*****
     C*****              Z-ADD     DESEQ#        KKSEQ#
     C*****DEKY2         CHAIN     PDE01                              99
     C*****
     C*****              CALL      'FA737R'
     C*****              PARM      DBRRN         ##RRN             9 0
     C*****              PARM      *BLANKS       ##NOT1           70
     C*****              PARM      *BLANKS       ##NOT2           70
     C*****              PARM      *BLANKS       ##RTCD            1
     C*
     C*****##RTCD        IFEQ      'F'
     C*****DENOT1        IFNE      *BLANKS
     C*****              ADD       1             NUMLIN
     C*****              ENDIF                                                  DENOT1 = *BLANKS
     C*****DENOT2        IFNE      *BLANKS
     C*****              ADD       1             NUMLIN
     C*****              ENDIF                                                  DENOT2 = *BLANKS
     C*****              ELSE
     C*****##NOT1        IFNE      *BLANKS
     C*****              ADD       1             NUMLIN
     C*****              ENDIF                                                  DENOT1 = *BLANKS
     C*****##NOT2        IFNE      *BLANKS
     C*****              ADD       1             NUMLIN
     C*****              ENDIF                                                  DENOT2 = *BLANKS
     C*****              ENDIF
     C*
     C*****              ELSE
     C     DENOT1        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF                                                  DENOT1 = *BLANKS
     C     DENOT2        IFNE      *BLANKS
     C                   ADD       1             NUMLIN
     C                   ENDIF                                                  DENOT2 = *BLANKS
     C*****              ENDIF
     C                   ENDIF
     C     DEKY          READE     PDE01                                  98
     C                   ENDDO
     C*
     C* SET INDICATOR TO PRINT EOB NOTE TITLE
     C*
     C     NUMLIN        IFGT      4
     C*** Beg Add *** 08/22/22 ************************************************
     C     ModernEOB     AndNe     'Y'
     C*** End Add *** 08/22/22 ************************************************
     C                   SETON                                        46
     C                   ELSE
     C                   SETOFF                                       46
     C                   ENDIF                                                  NUMLIN IFGT 0
     C     NUMLIN        ADD       CURLIN        LINEED            3 0
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL15
     C                   endif
     C                   ENDIF                                                  LINEED GT 58
     C*
     C**********           EXSR NWPG13
     C**********           EXCPTDETL11
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL12
     C                   endif
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL13
     C                   endif
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL14
     C                   endif
     C*
     C* GET ALL EOB DETAIL NOTES
     C*
     C     DEKY          CHAIN     PDE01                              98
     C***98*****           EXCPTSPACE
     C                   SETON                                        99
     C     *IN98         DOWEQ     *OFF
     C     DEPRNT        IFNE      'N'
     C     DESTAT        ANDNE     'I'
     C                   SETOFF                                       99
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
     C*****              MOVEL(P)  DEEOB         XXTYPE            1
     C*****XXTYPE        IFEQ      'P'
     C*****XXTYPE        OREQ      'R'
     C*****
     C*****              Z-ADD     DESEQ#        KKSEQ#
     C*****DEKY2         CHAIN     PDE01                              99
     C*****
     C*****              CALL      'FA737R'
     C*****              PARM      DBRRN         ##RRN             9 0
     C*****              PARM      *BLANKS       ##NOT1           70
     C*****              PARM      *BLANKS       ##NOT2           70
     C*****              PARM      *BLANKS       ##RTCD            1
     C*****
     C*****##RTCD        IFEQ      'F'
     C*****DENOT1        IFNE      *BLANKS
     C*****              MOVEL(P)  DENOT1        XNOTE            70
     C*****              EXSR      NWPG13
     C*****              EXCEPT    DETL2
     C*****              ENDIF                                                  DENOT1 NE *BLANKS
     C*****DENOT2        IFNE      *BLANKS
     C*****              MOVEL(P)  DENOT2        XNOTE
     C*****              EXSR      NWPG13
     C*****              EXCEPT    DETL2
     C*****              ENDIF                                                  DENOT2 NE *BLANKS
     C*****              ELSE
     C*****##NOT1        IFNE      *BLANKS
     C*****              MOVEL(P)  ##NOT1        XNOTE            70
     C*****              EXSR      NWPG13
     C*****              EXCEPT    DETL2
     C*****              ENDIF                                                  DENOT1 NE *BLANKS
     C*****##NOT2        IFNE      *BLANKS
     C*****              MOVEL(P)  ##NOT2        XNOTE
     C*****              EXSR      NWPG13
     C*****              EXCEPT    DETL2
     C*****              ENDIF                                                  DENOT2 NE *BLANKS
     C*****              ENDIF
     C*
     C*****              ELSE
     C     DENOT1        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT1        XNOTE            70
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL2
     C                   endif
     C                   ENDIF                                                  DENOT1 NE *BLANKS
     C     DENOT2        IFNE      *BLANKS
     C                   EXSR      NWPG13
     C                   MOVEL(P)  DENOT2        XNOTE
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL2
     C                   endif
     C                   ENDIF                                                  DENOT2 NE *BLANKS
     C*****              ENDIF
     C                   ENDIF
     C     DEKY          READE     PDE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C                   If        isUseSpoolEOB
     C   99              EXCEPT    SPACE
     C                   SETOFF                                       99
     C                   endif
     C*
     C*** Write to Scriptura Detail Charges Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDtWrk
     C                   Endif
     C*
     C     CAKY2         READE     PCA01                                  98
     C                   ENDDO                                                  *IN9= *OFF
     C*
     C                   Z-ADD     EOMRA         XXMRA             9 2
     C                   Z-ADD     EOOTH2        XXOTH2            9 2
     C                   Z-ADD     EOPAID        XXPAID            9 2
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
     C*********  ##CLT#    IFEQ '133'
     C     *IN36         IFEQ      *ON
     C     EAKY2         CHAIN     PEA01                              98
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     EATAX         XXTAX
     C                   ENDIF
     C                   ENDIF
     C*
     C* DETERMINE IF ENOUGH LINES ARE AVALAIBLE TO PRINT TOTALS
     C*
     C     *IN33         IFEQ      *ON
     C     CURLIN        ADD       4             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   ENDIF                                                  *IN33 = *ON
     C*
     C     *IN33         IFEQ      *OFF
     C     CURLIN        ADD       2             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   ENDIF                                                  *IN33 = *OFF
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL3
     C                   endif
     C*
     C* PRINT HOSPITAL INFO
     C*
     C     *IN40         IFEQ      *ON
     C*
     C* NEED FOUR LINES TO PRINT HOSPITAL INFORMATION
     C*
     C     CURLIN        ADD       4             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C******************** SETON                       OA
     C                   SETON                                          OF
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL16
     C                   endif
     C                   ENDIF                                                  *IN40 = *ON
     C*
     C* LOAD HEADER EOB CODES TO ARRAY 'EOB'
     C*
     C     EEKY          CHAIN     PEE01                              98
     C*
     C* PRINT 'EOB REASON CODES' HEADING IF ANY FOUND
     C*
     C**N98*****           EXSR NWPG1
     C**N98*****           EXCPTDETL4
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
     C*** CLOSE FILE IF OPENED
     C********** STOPN     IFEQ '1'
     C**********           CLOSEFTABL
     C**********           MOVE ' '       STOPN   1
     C**********           END                             STOPN = '1'
     C*
     C* DELETE OVERRIDE ON FTABL
     C*
     C********** STOVR     IFEQ '1'
     C**********           Z-ADD55        LENGTH
     C**********           CALL 'QCMDEXC'                60
     C**********           PARM           QCA,1
     C**********           PARM           LENGTH 155
     C**********           MOVE ' '       STOVR   1
     C**********           ENDIF                           STOVR = '1'
     C*
     C* OVERRIDE FTABL TO ADJUDICATION STATE
     C*
     C********** STOVR     IFEQ ' '
     C**********           Z-ADD55        LENGTH
     C**********           MOVELQCA,2     @@CMD
     C**********           MOVELEOAST     @@ST
     C**********           CALL 'QCMDEXC'
     C**********           PARM           @@CMD
     C**********           PARM           LENGTH 155
     C**********           MOVE '1'       STOVR
     C**********           ENDIF                           STOVR = ' '
     C*
     C*** OPEN FILE FOR PROCESSING
     C********** STOPN     IFNE '1'                          NOT CURRENTLY OPEN
     C**********           OPEN FTABL
     C**********           MOVE '1'       STOPN
     C**********           END                               STOPN NE '1'
     C*
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL4
     C                   endif
     C                   SORTA     EOB
     C                   Z-ADD     1             T                 2 0
     C                   DO        20            T
     C     EOB(T)        IFNE      *BLANKS
     C**********           MOVELEOB,T     KKKEY     P
     C**********           MOVELEOB,T     YYEOB     P
     C********** TBKY      CHAINPEB01                98
     C********** *IN98     IFEQ *OFF
     C**********           MOVELEBDESC    YYDESC 70 P
     C**********           ELSE
     C**********           MOVE *BLANKS   YYDESC
     C**********           ENDIF                           *IN98 = *OFF
     C**********           EXSR NWPG1
     C**********           EXCPTDETL5
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
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL5
     C                   endif
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
     C**N98*****           EXSR NWPG1
     C**N98*****           EXCPTDETL6
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
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL6
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C                   MOVEL(P)  EENOT1        X1NOTE           70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL7
     C                   endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfil
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT1 NE *BLANKS
     C     EENOT2        IFNE      *BLANKS
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL6
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C                   MOVEL(P)  EENOT2        X1NOTE
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL7
     C                   endif
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
     C* MOVED PRINTING OF COMPANY ADDRESS TO TOP OF EOMB
     C* GET COMPANY INFORMATION - BASED ON ALTERNATE ADDRESS FLAG
     C**********
     C********** F1KY      CHAINPF101                98
     C********** *IN98     IFEQ *OFF
     C********** F1FLG1    IFEQ 'Y'
     C**********           MOVELF1ANME    XXANME    P
     C**********           MOVELF1AAD1    XXAAD1    P
     C********** F1AAD2    IFNE *BLANKS
     C**********           MOVELF1AAD2    XXAAD2    P
     C**********           SETON                     31
     C**********           ELSE
     C**********           MOVE *BLANKS   XXAAD2
     C**********           SETOF                     31
     C**********           ENDIF                           F1AAD2 NE *BLANKS
     C**********           MOVELF1ACTY    XXCTY     P
     C**********           MOVE F1AST     XXSTAT
     C**********           MOVE F1AZIP    XXZIP
     C**********           EXSR SRADDR
     C**********           MOVELXXCSZ     XXCSZ2 40 P
     C**********           MOVELF1AARA    XXARA
     C**********           MOVELF1APHN    XXPHN1  3
     C**********           MOVE F1APHN    XXPHN2  4
     C**********           ENDIF                           FLFLG1 = 'Y'
     C********** F1FLG1    IFEQ 'N'
     C********** C1KY      CHAINPC101                98
     C********** *IN98     IFEQ *OFF
     C**********           MOVELCXNAME    XXANME    P
     C**********           MOVELCXADR1    XXAAD1    P
     C********** CXADR2    IFNE *BLANKS
     C**********           MOVELCXADR2    XXAAD2    P
     C**********           SETON                     31
     C**********           ELSE
     C**********           MOVE *BLANKS   XXAAD2
     C**********           SETOF                     31
     C**********           ENDIF
     C**********           MOVELCXCTY     XXCTY     P
     C**********           MOVELCXST      XXSTAT
     C**********           MOVE CXZIP     XXZIP
     C**********           EXSR SRADDR
     C**********           MOVELXXCSZ     XXCSZ2    P
     C**********           MOVE CXAREA    XXARA
     C**********           MOVELCXPHN#    XXPHN1
     C**********           MOVE CXPHN#    XXPHN2
     C**********           ENDIF                           *IN98 = *OFF
     C**********           ENDIF                           F1FLG1 = 'N'
     C**********
     C**START*NEW PAGE IF CANNOT PRINT ENTIRE ADDRESS ON ONE PAGE
     C**********
     C********** CURLIN    ADD  5         LINEED
     C********** LINEED    IFGT 58
     C**********           SETON                       OF
     C**********           EXSR NWPG1
     C**********           ENDIF                           LINEED GT 58
     C**********
     C**********           EXSR NWPG1
     C**********           EXCPTDETL10
     C**********           ENDIF                           *IN98 = *OFF
     C*
     C* GET STATE COMMENTS
     C*
     C                   If        isUseSpoolEOB
     C                   EXCEPT    SPACE
     C                   endif
     C                   Z-ADD     EOFRDT        KKEFDT
     C     F4KY2         SETGT     PF401
     C     F4KY1         READPE    PF401                                  98
     C*
     C* SKIP INACTIVE COMMENTS
     C*
     C*****      *IN98     IFEQ *OFF
     C*****      F4STAT    ANDEQ'I'
     C*
     C     *IN98         DOWEQ     *OFF
     C     F4STAT        IFNE      'I'
     C*****      F4STAT    ANDEQ'I'
     C*****      F4KY1     REDPEPF401                    98
     C*****                ENDDO
     C*****
     C*****                ENDIF                           *IN98 = *OFF
     C*****      *IN98     IFEQ *OFF
     C                   Z-ADD     0             NUMLIN
     C********** F4EC01    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC02    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC03    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC04    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC05    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC06    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC07    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC08    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC09    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC10    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC11    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC12    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC13    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC14    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC15    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC16    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC17    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC18    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC19    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC20    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC21    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC22    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC23    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC24    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC25    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC26    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC27    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC28    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC29    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F4EC30    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C* LOAD VALUES FOR LINES OF STATE COMMENTS
     C*
     C                   EXSR      FILLS
     C*
     C* DETERMINE THE NUMBER OF LINES THAT WILL BE NEEDED
     C*
     C                   EXSR      NMBRLN
     C*
     C**********
     C*
     C     NUMLIN        ADD       CURLIN        LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    SPACE
     C                   endif
     C                   ENDIF                                                  LINEED GT 58
     C*
     C*
     C* CHANGE FONT TO SMALLER PRINT IF NEED TO PRINT STATE COMMENTS
     C*
     C                   CLEAR                   STCOMM
     C********** NUMLIN    IFGT *ZEROS
     C**********           EXCPTSMALL
     C**********           ENDIF
     C**********
     C********** F4EC01    IFNE *BLANKS
     C**********           MOVELF4EC01    STCOM1    P
     C**********           MOVELF4EC02    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC03    IFNE *BLANKS
     C**********           MOVELF4EC03    STCOM1    P
     C**********           MOVELF4EC04    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC05    IFNE *BLANKS
     C**********           MOVELF4EC05    STCOM1    P
     C**********           MOVELF4EC06    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC07    IFNE *BLANKS
     C**********           MOVELF4EC07    STCOM1    P
     C**********           MOVELF4EC08    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC09    IFNE *BLANKS
     C**********           MOVELF4EC09    STCOM1    P
     C**********           MOVELF4EC10    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC11    IFNE *BLANKS
     C**********           MOVELF4EC11    STCOM1    P
     C**********           MOVELF4EC12    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC13    IFNE *BLANKS
     C**********           MOVELF4EC13    STCOM1    P
     C**********           MOVELF4EC14    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC15    IFNE *BLANKS
     C**********           MOVELF4EC15    STCOM1    P
     C**********           MOVELF4EC16    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC17    IFNE *BLANKS
     C**********           MOVELF4EC17    STCOM1    P
     C**********           MOVELF4EC18    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC19    IFNE *BLANKS
     C**********           MOVELF4EC19    STCOM1    P
     C**********           MOVELF4EC20    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC21    IFNE *BLANKS
     C**********           MOVELF4EC21    STCOM1    P
     C**********           MOVELF4EC22    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC23    IFNE *BLANKS
     C**********           MOVELF4EC23    STCOM1    P
     C**********           MOVELF4EC24    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC25    IFNE *BLANKS
     C**********           MOVELF4EC25    STCOM1    P
     C**********           MOVELF4EC26    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC27    IFNE *BLANKS
     C**********           MOVELF4EC27    STCOM1    P
     C**********           MOVELF4EC28    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC29    IFNE *BLANKS
     C**********           MOVELF4EC29    STCOM1    P
     C**********           MOVELF4EC30    STCOM2    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C*
     C********** F4EC01    IFNE *BLANKS
     C**********           MOVELF4EC01    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC02    IFNE *BLANKS
     C**********           MOVELF4EC02    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC03    IFNE *BLANKS
     C**********           MOVELF4EC03    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC04    IFNE *BLANKS
     C**********           MOVELF4EC04    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC05    IFNE *BLANKS
     C**********           MOVELF4EC05    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC06    IFNE *BLANKS
     C**********           MOVELF4EC06    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC07    IFNE *BLANKS
     C**********           MOVELF4EC07    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC08    IFNE *BLANKS
     C**********           MOVELF4EC08    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC09    IFNE *BLANKS
     C**********           MOVELF4EC09    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC10    IFNE *BLANKS
     C**********           MOVELF4EC10    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC11    IFNE *BLANKS
     C**********           MOVELF4EC11    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC12    IFNE *BLANKS
     C**********           MOVELF4EC12    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC13    IFNE *BLANKS
     C**********           MOVELF4EC13    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC14    IFNE *BLANKS
     C**********           MOVELF4EC14    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC15    IFNE *BLANKS
     C**********           MOVELF4EC15    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC16    IFNE *BLANKS
     C**********           MOVELF4EC16    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC17    IFNE *BLANKS
     C**********           MOVELF4EC17    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC18    IFNE *BLANKS
     C**********           MOVELF4EC18    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC19    IFNE *BLANKS
     C**********           MOVELF4EC19    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC20    IFNE *BLANKS
     C**********           MOVELF4EC20    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC21    IFNE *BLANKS
     C**********           MOVELF4EC21    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC22    IFNE *BLANKS
     C**********           MOVELF4EC22    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC23    IFNE *BLANKS
     C**********           MOVELF4EC23    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC24    IFNE *BLANKS
     C**********           MOVELF4EC24    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC25    IFNE *BLANKS
     C**********           MOVELF4EC25    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC26    IFNE *BLANKS
     C**********           MOVELF4EC26    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC27    IFNE *BLANKS
     C**********           MOVELF4EC27    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC28    IFNE *BLANKS
     C**********           MOVELF4EC28    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC29    IFNE *BLANKS
     C**********           MOVELF4EC29    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C********** F4EC30    IFNE *BLANKS
     C**********           MOVELF4EC30    STCOMM    P
     C**********           EXCPTDETL8
     C**********           ENDIF
     C**********
     C*
     C* PRINT LINES FOR COMMENTS
     C*
     C                   EXSR      PRTSC
     C**********           EXCPTNORMAL
     C*****                ENDIF                           *IN98 = *OFF
     C                   ENDIF                                                  STAT <> 'I'
     C*
     C     F4KY1         READPE    PF401                                  98
     C*
     C     *IN98         IFEQ      *OFF
     C                   If        isUseSpoolEOB
     C                   EXCEPT    SPACE
     C                   endif
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* GET GROUP COMMENTS BASED ON FROM SERVICE DATE IN FAEOP
     C*
     C                   If        isUseSpoolEOB
     C                   EXCEPT    SPACE
     C                   endif
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
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     0             NUMLIN
     C**********
     C********** F3EC01    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC02    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC03    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC04    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC05    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC06    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC07    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC08    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC09    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC10    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC11    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC12    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC13    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC14    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC15    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC16    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC17    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC18    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC19    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC20    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC21    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC22    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC23    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC24    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC25    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC26    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC27    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC28    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC29    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
     C*
     C********** F3EC30    IFNE *BLANKS
     C**********           ADD  1         NUMLIN
     C**********           ENDIF
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
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C**********           EXCPTNORMAL                     REGULAR PRINT
     C                   EXSR      NWPG1
     C**********           EXCPTSMALL                      SMALL PRINT
     C                   ENDIF                                                  LINEED GT 58
     C*
     C********** F3EC01    IFNE *BLANKS
     C**********           MOVELF3EC01    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC02    IFNE *BLANKS
     C**********           MOVELF3EC02    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC03    IFNE *BLANKS
     C**********           MOVELF3EC03    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC04    IFNE *BLANKS
     C**********           MOVELF3EC04    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC05    IFNE *BLANKS
     C**********           MOVELF3EC05    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC06    IFNE *BLANKS
     C**********           MOVELF3EC06    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC07    IFNE *BLANKS
     C**********           MOVELF3EC07    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC08    IFNE *BLANKS
     C**********           MOVELF3EC08    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC09    IFNE *BLANKS
     C**********           MOVELF3EC09    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC10    IFNE *BLANKS
     C**********           MOVELF3EC10    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC11    IFNE *BLANKS
     C**********           MOVELF3EC11    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC12    IFNE *BLANKS
     C**********           MOVELF3EC12    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC13    IFNE *BLANKS
     C**********           MOVELF3EC13    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC14    IFNE *BLANKS
     C**********           MOVELF3EC14    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC15    IFNE *BLANKS
     C**********           MOVELF3EC15    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC16    IFNE *BLANKS
     C**********           MOVELF3EC16    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC17    IFNE *BLANKS
     C**********           MOVELF3EC17    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC18    IFNE *BLANKS
     C**********           MOVELF3EC18    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC19    IFNE *BLANKS
     C**********           MOVELF3EC19    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC20    IFNE *BLANKS
     C**********           MOVELF3EC20    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC21    IFNE *BLANKS
     C**********           MOVELF3EC21    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC22    IFNE *BLANKS
     C**********           MOVELF3EC22    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC23    IFNE *BLANKS
     C**********           MOVELF3EC23    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC24    IFNE *BLANKS
     C**********           MOVELF3EC24    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC25    IFNE *BLANKS
     C**********           MOVELF3EC25    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC26    IFNE *BLANKS
     C**********           MOVELF3EC26    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC27    IFNE *BLANKS
     C**********           MOVELF3EC27    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC28    IFNE *BLANKS
     C**********           MOVELF3EC28    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC29    IFNE *BLANKS
     C**********           MOVELF3EC29    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC30    IFNE *BLANKS
     C**********           MOVELF3EC30    GRCOMM    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C**********
     C*
     C* PRINT GROUP COMMENTS
     C*
     C                   EXSR      PRTGC
     C*
     C*
     C                   CLEAR                   GRCOMM
     C********** F3EC01    IFNE *BLANKS
     C**********           EXCPTSMALL
     C**********
     C**********           MOVELF3EC01    GRCOM1    P
     C**********           MOVELF3EC02    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC03    IFNE *BLANKS
     C**********           MOVELF3EC03    GRCOM1    P
     C**********           MOVELF3EC04    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC05    IFNE *BLANKS
     C**********           MOVELF3EC05    GRCOM1    P
     C**********           MOVELF3EC06    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC07    IFNE *BLANKS
     C**********           MOVELF3EC07    GRCOM1    P
     C**********           MOVELF3EC08    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC09    IFNE *BLANKS
     C**********           MOVELF3EC09    GRCOM1    P
     C**********           MOVELF3EC10    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC11    IFNE *BLANKS
     C**********           MOVELF3EC11    GRCOM1    P
     C**********           MOVELF3EC12    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC13    IFNE *BLANKS
     C**********           MOVELF3EC13    GRCOM1    P
     C**********           MOVELF3EC14    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC15    IFNE *BLANKS
     C**********           MOVELF3EC15    GRCOM1    P
     C**********           MOVELF3EC16    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC17    IFNE *BLANKS
     C**********           MOVELF3EC17    GRCOM1    P
     C**********           MOVELF3EC18    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC19    IFNE *BLANKS
     C**********           MOVELF3EC19    GRCOM1    P
     C**********           MOVELF3EC20    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC21    IFNE *BLANKS
     C**********           MOVELF3EC21    GRCOM1    P
     C**********           MOVELF3EC22    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC23    IFNE *BLANKS
     C**********           MOVELF3EC23    GRCOM1    P
     C**********           MOVELF3EC24    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC25    IFNE *BLANKS
     C**********           MOVELF3EC25    GRCOM1    P
     C**********           MOVELF3EC26    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC27    IFNE *BLANKS
     C**********           MOVELF3EC27    GRCOM1    P
     C**********           MOVELF3EC28    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C********** F3EC29    IFNE *BLANKS
     C**********           MOVELF3EC29    GRCOM1    P
     C**********           MOVELF3EC30    GRCOM2    P
     C**********           EXCPTDETL9
     C**********           ENDIF
     C**********           EXCPTNORMAL
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C*
     C*
     C*** Write to Scriptura EOB Totals and Workfiles
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtEOBTotWrk
     C                   Exsr      wrtFtWrk
     C                   Endif
     C**********           ENDIF                           *IN99 = *OFF
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
     C                   ENDIF                                                  STATE = FL
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDSR
     **------------------------------------------------------------------*
     C     SRSTBL        BEGSR
     **------------------------------------------------------------------*
     C*
     C                   MOVE      *ZERO         *IN72
     C                   MOVE      *BLANKS       TMPVAR
     C                   CLEAR                   SVFARA
     C                   CLEAR                   SVFPH1
     C                   CLEAR                   SVFPH2
     C* GET SUBSCRIBER #
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXMFND
     C                   MOVE      'Y'           TMPVAR            1
     C                   MOVEL     FNFARA        SVFARA
     C                   MOVEL     FNFAX         SVFPH1            3
     C                   MOVE      FNFAX         SVFPH2            4
     C                   ENDIF
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
     C                   CLEAR                   XXCSZ1
     C                   CLEAR                   XXCSZ
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
     C                   CLEAR                   X1PAID
     C                   CLEAR                   X2PAID
     C                   CLEAR                   XXENAM
     C                   CLEAR                   XXFARA
     C                   CLEAR                   XXFPH1
     C                   CLEAR                   XXFPH2
     C                   clear                   xxDrg#
     C                   clear                   xxDgDs
     C* GET BILL
     C*
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
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
     C*  Load the claim# field which prints on the EOMB stub.
     C*
     C     ##CLT#        IFEQ      '114'
     C     CMACL#        ANDNE     *BLANKS
     C     CMFND         ANDEQ     440
     C                   MOVEL(P)  CMACL#        XXCL#            21
     C*
     C                   ELSE
     C                   Z-ADD     EOSUB#        XFSUB
     C                   MOVE      '-'           XFHYP1
     C                   Z-ADD     EOFND         XFFND
     C                   MOVE      '-'           XFHYP2
     C                   Z-ADD     EOFYR         XFYR
     C                   MOVE      '-'           XFHYP3
     C                   Z-ADD     EOCASE        XFCASE
     C                   MOVEL(P)  XF#           XXCL#
     C                   ENDIF
     C                   MOVE      '0'           *IN35
     C     EOBNPI        IFNE      *BLANKS
     C                   MOVEL(P)  EOBNPI        XXBNPI
     C                   SETON                                        35
     C                   ELSE
     C                   SETOFF                                       35
     C                   ENDIF
     C*
     C* SET INDICATORS FOR CONDITIONAL DISPLAY OF DATA
     C*
     C* IF BILL TYPE IS 'HOSPITAL', THEN DISPLAY 'TO SERVICE DATE'
     C*
     C                   MOVE      *OFF          PCM               1
     C     EOBTYP        IFEQ      'H'
     C                   SETON                                        40
     C*
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C     EUPCM         ANDNE     *BLANKS
     C                   MOVE      *ON           PCM               1
     C                   ENDIF
     C*
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
     C*
     C*** CLOSE FILE IF OPENED
     C     FLOPN         IFEQ      '1'
     C*
     C     PCM           ANDEQ     *ON
     C                   CLOSE     FMPRL
     C                   MOVE      ' '           FLOPN             1
     C                   END                                                    STOPN = '1'
     C*
     C* DELETE OVERRIDE ON FMPRL
     C*
     C     FLOVR         IFEQ      '1'
     C*
     C     PCM           ANDEQ     *ON
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           FLOVR             1
     C                   ENDIF                                                  STOVR = '1'
     C*
     C* OVERRIDE FMPRL TO ADJUDICATION STATE
     C*
     C     FLOVR         IFEQ      ' '
     C*
     C     PCM           ANDEQ     *ON
     C*
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(4)        @@CMD
     C                   MOVEL     EOAST         @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           FLOVR
     C                   ENDIF                                                  STOVR = ' '
     C*
     C*** OPEN FILE FOR PROCESSING
     C     FLOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C*
     C     PCM           ANDEQ     *ON
     C*
     C                   OPEN      FMPRL
     C                   MOVE      '1'           FLOPN
     C                   END                                                      STOPN NE '1'
     C*
     C*
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
     C                   MOVEL(P)  ##LAST        XXNAM1           40
     C*
     C* GET EMPLOYER AND DIVISION FOR CLAIM MASTER FILE
     C*
     C     CMKY          CHAIN     PCM02                              97
     C     *IN97         IFEQ      *OFF
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C     EMKY          CHAIN     PEM02                              97
     C*
     C     *IN97         IFEQ      *OFF
     C                   MOVEL     EMNAME        XXENAM
     C                   ELSE
     C                   CLEAR                   XXENAM
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C* PUT ACCIDENT DATE IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOI             8 0
     C*
     C* PUT TRANSACTION DATE IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
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
     C     EOADR3        IFNE      *BLANKS
     C                   MOVEL(P)  EOADR3        XXCSZ1
     C                   ENDIF
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
     C*
     C*
     C* GET 'FROM' AND 'TO' SERVICE DATE
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZFRDT            8 0
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZTODT            8 0
     C*
     C* GET PATIENT ACCOUNT# BASE ON TYPE OF BILL
     C*
     C                   SETOFF                                       37
     C                   MOVE      *OFF          *IN36
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     EOBTYP        OREQ      'V'
     C     EHKY          CHAIN     PEH01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  EHACCT        XXACCT
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'H'
     C     EUKY          CHAIN     PEU01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  EUACCT        XXACCT
     C*
     C* GET OTHER HOSPITAL SPECIFIC INFORMATION FOR DISPLAY
     C*
     C*
     C* DRG code
     C                   eval      xxDRG# = EUdrg#
     C                   eval      xxDgDs = EUdgDs
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
     C                   MOVE      'BLTY'        KKTBID
     C     EOAST         CAT(P)    'BT':0        KKTBID            4
     C                   MOVEL(P)  KKTBID        KKTBID
     C                   MOVEL(P)  KKTBID        BTTBID
     C     GTKY3         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C     GTKY3         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        KKTBID            4
     C                   MOVEL(P)  'BLTY'        BTTBID            4
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL(P)  XXBLTY        KXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXBDSC           30
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C* GET PROCEDURE CODE DESCRIPTIONS
     C*
     C                   MOVE      *BLANKS       XXPRD1           35
     C                   MOVE      *BLANKS       XXPRD2           35
     C                   MOVE      *BLANKS       XXPRD3           35
     C                   MOVE      *BLANKS       XXPRD4           35
     C*
     C*
     C     PCM           IFEQ      *ON
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
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'X'
     C     EPKY          CHAIN     PEP01                              99
     C                   MOVE      *BLANKS       XXACCT
     C                   MOVE      *BLANKS       XXPHAR
     C*
     C     EPPHAR        IFNE      *BLANKS
     C                   MOVEL(P)  EPPHAR        XXPHAR
     C                   SETON                                        37
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN36
     C********** ##CLT#    IFEQ '133'
     C     EPTAX         IFEQ      'Y'
     C                   MOVE      *ON           *IN36
     C                   ENDIF
     C**********           ENDIF
     C*
     C     EOBTYP        WHENEQ    'D'
     C                   MOVE      *BLANKS       XXACCT
     C*
     C                   ENDSL
     C*
     C* GET MEDICAL EXAMINER HEADING, MEDICAL EXAMINER VALUES,
     C* AND REDUCTIONS HEADINGS
     C*
     C                   MOVE      *BLANKS       XXMUSR           15
     C*
     C                   Z-ADD     EOCO#         ##CO#
     C                   Z-ADD     EOSUB#        ##SUB#
     C                   Z-ADD     EOFND         ##FND
     C                   Z-ADD     EOFYR         ##FYR
     C                   Z-ADD     EOCASE        ##CASE
     C                   Z-ADD     EOEOB#        ##EOB#
     C                   MOVE      *BLANKS       ##USER           40
     C*
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
     C*
     C                   MOVEL     ##USER        XXMUSR
     C*
     C                   ENDIF                                                  ##USER = 'NONE'
     C*
     C     CXKY1         CHAIN     PCX02                              98
     C*
     C* GET LICENCE #/MEDICARE # BASED ON TYPE OF BILL
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
     C*
     C     EOBTYP        WHENEQ    'H'
     C*
     C                   MOVEL(P)  EULIC#        XXLIC#
     C*
     C     EUFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EUFLIC        XXLIC#
     C                   ENDIF
     C*
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL     EPPHAR        XXLIC#
     C*
     C                   OTHER
     C                   MOVE      *BLANKS       XXLIC#
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
     C***                MOVE      *BLANKS       XXDS1             6
     C***                MOVE      *BLANKS       XXDS2             6
     C***                MOVE      *BLANKS       XXDS3             6
     C***                MOVE      *BLANKS       XXDS4             6
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
       If EUicdInd = 'Y';
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
     C***  EODIA1        IFNE      *BLANKS
     C***                MOVEL(P)  EODIA1        KKICD9
     C***  DIKY          CHAIN     PDI01                              98
     C***  *IN98         IFEQ      *OFF
     C***                MOVEL(P)  DIDSC1        DESC1            30
     C***
     C***                MOVEL(P)  DIDS1         XXDS1
     C***                MOVE      '1.'          XXNUM1
     C*
     C***                ENDIF                                                  *IN98 = *OFF
     C***                ENDIF                                                  EODIA1 NE *BLANKS
     C*
     C***  EODIA2        IFNE      *BLANKS
     C***                MOVEL(P)  EODIA2        KKICD9
     C***  DIKY          CHAIN     PDI01                              98
     C***  *IN98         IFEQ      *OFF
     C***                MOVEL(P)  DIDSC1        DESC2            30
     C*
     C***                MOVEL(P)  DIDS1         XXDS2
     C***                MOVE      '2.'          XXNUM2
     C***
     C***                ENDIF                                                  *IN98 = *OFF
     C***                ENDIF                                                  EODIA2 NE *BLANKS
     C*
     C***  EODIA3        IFNE      *BLANKS
     C***                MOVEL(P)  EODIA3        KKICD9
     C***  DIKY          CHAIN     PDI01                              98
     C***  *IN98         IFEQ      *OFF
     C***                MOVEL(P)  DIDSC1        DESC3            30
     C*
     C***                MOVEL(P)  DIDS1         XXDS3
     C***                MOVE      '3.'          XXNUM3
     C*
     C***                ENDIF                                                  *IN98 = *OFF
     C***                ENDIF                                                  EODIA3 NE *BLANKS
     C*
     C***  EODIA4        IFNE      *BLANKS
     C***                MOVEL     EODIA4        KKICD9
     C***  DIKY          CHAIN     PDI01                              98
     C***  *IN98         IFEQ      *OFF
     C***                MOVEL(P)  DIDSC1        DESC4            30
     C*
     C***                MOVEL(P)  DIDS1         XXDS4
     C***                MOVE      '4.'          XXNUM4
     C*
     C***                ENDIF                                                  *IN98 = *OFF
     C***                ENDIF                                                  EODIA4 NE *BLANKS
     C                   if        isScriptEOB = 'Y'
     C                   exsr      wrtDGwrk
     C                   endif
     C*
     C* PUT FROM/TO SERVICE DATES IN SCREEN DISPLAY FORMAT
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOI
     C*
     C                   TIME                    DSDTTM
     C*
     C                   SETOFF                                       8081
     C                   MOVEL     *BLANKS       XXNW2           140
     C                   MOVEL     *BLANKS       XXNW3           140
     C*
     C* SET INDICATOR TO DISPLAY NETWORK DISCLAIMER IF NECESSARY
     C*
     C     EONTWK        IFNE      *BLANKS
     C     EONDIS        ANDNE     *ZEROS
     C*
     C     EONTWK        ORNE      *BLANKS
     C     EONDSC        ANDNE     *BLANKS
     C*
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
     C* GET COMPANY INFORMATION - BASED ON ALTERNATE ADDRESS FLAG
     C*
     C     F1KY          CHAIN     PF101                              98
     C     *IN98         IFEQ      *OFF
     C     F1FLG1        IFEQ      'Y'
     C*
     C                   MOVEL(P)  F1ANME        XXANME           40
     C                   MOVEL(P)  F1AAD1        XXAAD1           40
     C*
     C     F1AAD2        IFNE      *BLANKS
     C                   MOVEL(P)  F1AAD2        XXAAD2           40
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
     C                   MOVEL     F1AARA        XXARA
     C                   MOVEL     F1APHN        XXPHN1            3
     C                   MOVE      F1APHN        XXPHN2            4
     C*
     C     *IN72         IFEQ      *ON
     C                   MOVEL     SVFARA        XXFARA
     C                   MOVEL     SVFPH1        XXFPH1            3
     C                   MOVE      SVFPH2        XXFPH2            4
     C                   ENDIF
     C*
     C                   ENDIF                                                  FLFLG1 = 'Y'
     C     F1FLG1        IFEQ      'N'
     C     C1KY          CHAIN     PC101                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  CXNAME        XXANME
     C                   MOVEL(P)  CXADR1        XXAAD1
     C     CXADR2        IFNE      *BLANKS
     C                   MOVEL(P)  CXADR2        XXAAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXAAD2
     C                   SETOFF                                       31
     C                   ENDIF
     C                   MOVEL(P)  CXCTY         XXCTY
     C                   MOVEL     CXST          XXSTAT
     C                   MOVE      CXZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ2
     C                   MOVE      CXAREA        XXARA
     C                   MOVEL     CXPHN#        XXPHN1
     C                   MOVE      CXPHN#        XXPHN2
     C*
     C     *IN72         IFEQ      *ON
     C                   MOVE      CXFARA        XXFARA
     C                   MOVEL     CXFAX#        XXFPH1
     C                   MOVE      CXFAX#        XXFPH2
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  F1FLG1 = 'N'
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C* Get Division insured  number from Group info file.
     C*
     C                   MOVE      EOFND         KKFND
     C                   MOVE      'FL'          KF2ST
     C*
     C     F2KY          CHAIN     PF202                              98
     C     *IN98         IFEQ      *OFF
     C     F25N01        ANDNE     *ZEROS
     C                   MOVE      F25N01        XXDIV1            5 0
     C*
     C                   ENDIF
     C*
     C                   EXSR      MSKSS#
     C*
     C                   If        isUseSpoolEOB
     C                   EXCEPT    HEADA
     C                   EXCEPT    HEADB
     C                   EXCEPT    HEADC
     C                   endif
     C*
     C*** Write to Scriptura Header EOB workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtHdWrk
     C                   Endif
     C*
     C* CYCLE THROUGH FDCAP FILE TO GET ALL CHARGES FOR THE BILL
     C*
     C                   CLEAR                   EOB
     C                   CLEAR                   XLINE
     C                   CLEAR                   TMDSEQ
     C                   CLEAR                   XPFL1
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
     C*
     C     CAKY3         SETLL     PCA03
     C     CAKY3         READE     PCA03                                  98
     C     *IN98         DOWEQ     *OFF                                         *IN98 = *OFF
     C*
     C                   Z-ADD     0             X1TAX
     C                   CALL      'FA774R'
     C                   PARM      C3RRN         QQRRN
     C     X1TAX         PARM      0             QQTAX
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
     C                   ADD       1             XLINE             5 0
     C*
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DE15FL
     C                   endif
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
     C                   Z-ADD     DHUNTS        X1UNTS            5 2
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C     EOBTYP        WHENEQ    'H'
     C     DUKY          CHAIN     PDU01                              98
     C     *IN98         IFEQ      *OFF
      *
     C                   MOVE      DUPMOD        XXMOD1
     C*
     C                   MOVE      DUMOD2        XXMOD2
     C                   MOVE      DUMOD3        XXMOD3
     C                   MOVE      DUMOD4        XXMOD4
     C*
      *
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
     C                   Z-ADD     DUUNTS        X1UNTS
     C*
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
     C*
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
     C*
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
     C                   Z-ADD     DPUNTS        X1UNTS
     C*
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
     C                   Z-ADD     DDUNTS        X1UNTS
     C*
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C                   ENDSL
     C*
     C* GET DIAGNOSIS CODE BASED ON CROSS REFERENCE
     C*
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
     C*
     C                   ENDIF                                                  EOBTYP = 'N'
     C*
     C* GET EOB CODES
     C*
     C**** Beg Chg *** 08/22/22 ***************************************
     C***                Z-ADD     1             J                 1 0
     C***                MOVE      *BLANKS       X1EOB
     C***                MOVE      *BLANKS       X2EOB
     C***                MOVE      *BLANKS       X3EOB
     C***                MOVE      *BLANKS       X4EOB
     C***                MOVE      *BLANKS       XNOTE
       J = 1;
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
     C**** End Chg *** 08/22/22 ***************************************
     C                   SETOFF                                       424344
     C                   SETOFF                                       45
      //**** Beg Add *** 08/22/22 *************************************
       *in49 = *off;
       *in52 = *off;
       *in54 = *off;
       *in55 = *off;
       *in57 = *off;
       *in58 = *off;
       *in59 = *off;
       *in68 = *off;
       If ModernEOB = 'Y';
          J2 = 12;
       Else;
          J2 = 4;
       Endif;
     C*** End Add *** 08/22/22 ************************************************
     C*
     C     DEKY          CHAIN     PDE01                              98
     C     *IN98         DOWEQ     *OFF
     C**** Beg Chg *** 08/22/22 ***************************************
     C**** J             ANDLE     4
     C     J             ANDLE     J2
     C**** End Chg *** 08/22/22 ***************************************
     C*
     C     DEPRNT        IFNE      'N'
     C     DESTAT        ANDNE     'I'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*
     C                   SELECT
     C     J             WHENEQ    1
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X1EOB
     C                   MOVE      DEEOBCD       X1EOB
     C**** End Chg *** 07/18/23 **************************************
     C**** Beg Add *** 08/22/22 **********************************************
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      DEEOB         X1EOBR
     C                   MOVE      DEEOBCD       X1EOBR
     C**** End Chg *** 07/18/23 **************************************
     C**** End Add *** 08/22/22 **********************************************
     C     X1EOB         IFNE      *BLANKS
     C                   SETON                                        42
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF                                                  X1EOB NE *BLANKS
     C     J             WHENEQ    2
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
     C     J             WHENEQ    3
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
     C     J             WHENEQ    4
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
     C                   ENDSL
       //*** Beg Add *** 08/22/22 **********************************************
       If ModernEOB = 'Y';
          select;
          When J = 5;
             //*** Beg Chg *** 07/18/23 ***********************
             //*** x5Eob = DEeob;
             //*** x5EobR= DEeob;
             x5Eob = DEeobCd;
             x5EobR= DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             If x5Eob <> *Blanks;
                *in49 = *On;
             Else;
                *in49 = *off;
             Endif;
          When J = 6;
             //*** Beg Chg *** 07/18/23 ***********************
             //*** x6Eob = DEeob;
             //*** x6EobR= DEeob;
             x6Eob = DEeobCd;
             x6EobR= DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             If x6Eob <> *Blanks;
                *in52 = *On;
             Else;
                *in52 = *off;
             Endif;
          When J = 7;
             //*** Beg Chg *** 07/18/23 ***********************
             //*** x7Eob = DEeob;
             //*** x7EobR= DEeob;
             x7Eob = DEeobCd;
             x7EobR= DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             If x7Eob <> *Blanks;
                *in54 = *On;
             Else;
                *in54 = *off;
             Endif;
          When J = 8;
             //*** Beg Chg *** 07/18/23 ***********************
             //*** x8Eob = DEeob;
             //*** x8EobR= DEeob;
             x8Eob = DEeobCd;
             x8EobR= DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             If x8Eob <> *Blanks;
                *in55 = *On;
             Else;
                *in55 = *off;
             Endif;
          When J = 9;
             //*** Beg Chg *** 07/18/23 ***********************
             //*** x9Eob = DEeob;
             //*** x9EobR= DEeob;
             x9Eob = DEeobCd;
             x9EobR= DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             If x9Eob <> *Blanks;
                *in57 = *On;
             Else;
                *in57 = *off;
             Endif;
          When J = 10;
             //*** Beg Chg *** 07/18/23 ***********************
             //*** x10Eob = DEeob;
             //*** x10EobR= DEeob;
             x10Eob = DEeobCd;
             x10EobR= DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             If x10Eob <> *Blanks;
                *in58 = *On;
             Else;
                *in58 = *off;
             Endif;
          When J = 11;
             //*** Beg Chg *** 07/18/23 ***********************
             //*** x11Eob = DEeob;
             //*** x11EobR= DEeob;
             x11Eob = DEeobCd;
             x11EobR= DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             If x11Eob <> *Blanks;
                *in59 = *On;
             Else;
                *in59 = *off;
             Endif;
          When J = 12;
             //*** Beg Chg *** 07/18/23 ***********************
             //*** x12Eob = DEeob;
             //*** x12EobR= DEeob;
             x12Eob = DEeobCd;
             x12EobR= DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             If x12Eob <> *Blanks;
                *in68 = *On;
             Else;
                *in68 = *off;
             Endif;
          Endsl;
       Endif;
       //*** End Add *** 08/22/22 **********************************************
     C                   ADD       1             J
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
     C*
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
     C     NUMLIN        ADD       CURLIN        LINEED            3 0
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG13
     C*
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DE15FL
     C                   endif
     C*
     C                   ENDIF                                                  LINEED GT 58
     C*
     C*
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL12
     C                   EXSR      NWPG13
     C                   EXCEPT    DETL13
     C                   EXSR      NWPG13
     C                   EXCEPT    DETL14
     C*
     C   99              EXCEPT    SPACE
     C                   SETOFF                                       99
     C                   endif
     C*
     C*
     C*** Write to Scriptura Detail Charges Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDtWrk
     C                   Endif
     C*
     C     CAKY3         READE     PCA03                                  98
     C                   ENDDO                                                  *IN9= *OFF
     C*
     C                   Z-ADD     EOMRA         XXMRA             9 2
     C                   Z-ADD     EOOTH2        XXOTH2            9 2
     C                   Z-ADD     EOPAID        XXPAID            9 2
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
     C********** ##CLT#    IFEQ '133'
     C     *IN36         IFEQ      *ON
     C     EAKY2         CHAIN     PEA01                              98
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     EATAX         XXTAX
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C* DETERMINE IF ENOUGH LINES ARE AVALAIBLE TO PRINT TOTALS
     C*
     C     *IN33         IFEQ      *ON
     C     CURLIN        ADD       4             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   ENDIF                                                  *IN33 = *ON
     C*
     C     *IN33         IFEQ      *OFF
     C     CURLIN        ADD       2             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C                   ENDIF                                                  *IN33 = *OFF
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL3
     C                   endif
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
     C     CURLIN        ADD       4             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C*
     C                   SETON                                          OF
     C                   EXSR      NWPG1
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL16
     C                   endif
     C                   ENDIF                                                  *IN40 = *ON
     C*
     C* LOAD HEADER EOB CODES TO ARRAY 'EOB'
     C*
     C     EEKY          CHAIN     PEE01                              98
     C*
     C     *IN98         DOWEQ     *OFF
     C*
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
     C                   EXSR      NWPG1
     C*
     C                   Z-ADD     1             T                 2 0
     C                   DO        20            T
     C*
     C     EOB(T)        IFNE      *BLANKS
     C                   MOVEL(P)  EOB(T)        YYEOB
     C*
     C* GET THE X-REF FOR FLORIDA BILLS
     C*
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
     C*
     C     $$DESC        IFNE      *BLANKS
     C*
     C* START NEW PAGE IF CANNOT PRINT ENTIRE NOTE ON ONE PAGE
     C*
     C     CURLIN        ADD       5             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                          OF
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOB
     C                   endif
     C                   MOVE      'Y'           XPFLAG
     C                   ELSE
     C     XPFLAG        IFNE      'Y'
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOB
     C                   endif
     C                   MOVE      'Y'           XPFLAG
     C                   ENDIF
     C                   ENDIF                                                  LINEED GT 58
     C*
     C                   MOVEL(P)  $$DESC        DENOT1
     C                   MOVEL(P)  $DESC2        DENOT2
     C                   MOVEL(P)  $DESC3        DENOT3
     C                   MOVEL(P)  $DESC4        DENOT4
     C*
     C* GET ALL EOB DETAIL NOTES
     C     DENOT1        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT1        XNOTE            70
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEON
     C                   endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetCdNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT1 NE *BLANKS
     C*
     C     DENOT2        IFNE      *BLANKS
     C                   EXSR      NWPG13
     C                   MOVEL(P)  DENOT2        XNOTE
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOD
     C                   endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetCdNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT2 NE *BLANKS
     C*
     C     DENOT3        IFNE      *BLANKS
     C                   MOVEL(P)  DENOT3        XNOTE            70
     C                   EXSR      NWPG13
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOD
     C                   endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetCdNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT3 NE *BLANKS
     C*
     C     DENOT4        IFNE      *BLANKS
     C                   EXSR      NWPG13
     C                   MOVEL(P)  DENOT4        XNOTE
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOD
     C                   endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetCdNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT4 NE *BLANKS
     C*
     C                   ENDIF                                                  $$DESC <> *BLANKS
     C*
     C                   ENDIF                                                  EOB,T = *BLANKS
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  EOB NE *BLANKS
     C                   If        isUseSpoolEOB
     C                   EXCEPT    BRKLIN
     C                   endif
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
     C*
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
     C                   MOVEL     DENOT1        WVAR1            70
     C                   MOVEL     DENOT2        WVAR2            70
     C                   MOVEL     DENOT3        WVAR3            70
     C                   MOVEL     DENOT4        WVAR4            70
     C*
     C                   EXSR      SRWRAP
     C*
     C* START NEW PAGE IF CANNOT PRINT ENTIRE NOTE ON ONE PAGE
     C*
     C     CURLIN        ADD       5             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                          OF
     C                   EXSR      NWPG1
     C                   CLEAR                   XPFLAG
     C                   ENDIF                                                  LINEED GT 58
     C*
     C     WRNOT1        IFNE      *BLANKS
     C*
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTNOT
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT1        X1NOTE           70
     C                   ADD       1             XNO1              3 0
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTLIN
     C                   endif
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
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTNOT
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT2        X1NOTE           70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTNO1
     C                   endif
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
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTNOT
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT3        X1NOTE           70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTNO1
     C                   endif
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
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTNOT
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT4        X1NOTE           70
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTNO1
     C                   endif
     C*
     C*** Write to Scriptura Detail EOB/Note Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtDetNtWrk
     C                   Endif
     C                   ENDIF                                                  DENOT4 NE *BLANKS
     C                   ENDIF                                                  DEEOB = *BLANKS
     C                   ENDIF
     C*
     C     DEKY3         READE     PDE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     XPFLAG        IFNE      *BLANKS
     C                   If        isUseSpoolEOB
     C                   EXCEPT    BRKLIN
     C                   endif
     C                   ENDIF
     C*
     C* GET/DISPLAY HEADER NOTES
     C*
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
     C                   MOVEL     EENOT1        WVAR1
     C                   MOVEL     EENOT2        WVAR2
     C                   MOVEL     EENOT3        WVAR3
     C                   MOVEL     EENOT4        WVAR4
     C*
     C                   EXSR      SRWRAP
     C*
     C* START NEW PAGE IF CANNOT PRINT ENTIRE NOTE ON ONE PAGE
     C*
     C     CURLIN        ADD       5             LINEED
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                          OF
     C                   EXSR      NWPG1
     C                   CLEAR                   XPFLAG
     C                   ENDIF                                                  LINEED GT 58
     C*
     C     WRNOT1        IFNE      *BLANKS
     C*
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOH
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT1        X1NOTE           70
     C                   ADD       1             XNO1
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTLIH
     C                   endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT1 NE *BLANKS
     C*
     C     WRNOT2        IFNE      *BLANKS
     C*
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOH
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT2        X1NOTE
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTLIH1
     C                   endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT2 NE *BLANKS
     C*
     C     WRNOT3        IFNE      *BLANKS
     C*
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOH
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT3        X1NOTE
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTLIH1
     C                   endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT3 NE *BLANKS
     C*
     C     WRNOT4        IFNE      *BLANKS
     C*
     C     XPFLAG        IFNE      'Y'
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTEOH
     C                   endif
     C                   MOVE      'Y'           XPFLAG            1
     C                   ENDIF                                                  XPFLAG <> Y
     C*
     C                   MOVEL(P)  WRNOT4        X1NOTE
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DTLIH1
     C                   endif
     C*
     C*** Write to Scriptura General Carrier Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtGenCarNtWrk
     C                   Endif
     C                   ENDIF                                                  EENOT4 NE *BLANKS
     C*
     C*
     C                   ENDIF                                                  EEEOB = *BLANKS
     C                   ENDIF                                                  EEPRNT
     C*
     C     EEKY          READE     PEE01                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C     XPFLAG        IFNE      *BLANKS
     C                   If        isUseSpoolEOB
     C                   EXCEPT    BRKLIN
     C                   endif
     C                   ENDIF
     C*
     C*** Write to Scriptura Footer Info Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      WrtFtWrk
     C                   Endif
     C*
kdc  C     CURLIN        ADD       5             LINEED
     C*
B02  C     LINEED        IFGT      78
     C                   SETON                                          OF
     C                   EXSR      NWPG1
E02  C                   ENDIF                                                  LINEED GT 58
     C*
B02  C     XXAAD2        IFNE      *BLANKS
     C                   MOVE      *ON           *IN31
X02  C                   ELSE
     C                   MOVE      *OFF          *IN31
E02  C                   ENDIF
     C*
     C                   EXCEPT    INSURED
     C* GET STATE COMMENTS
     C*
     C                   If        isUseSpoolEOB
     C                   EXCEPT    SPACE
     C                   endif
     C                   Z-ADD     EOFRDT        KKEFDT
     C     F4KY2         SETGT     PF401
     C     F4KY1         READPE    PF401                                  98
     C*
     C* SKIP INACTIVE COMMENTS
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C     F4STAT        IFNE      'I'
     C*
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
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG1
     C                   If        isUseSpoolEOB
     C                   EXCEPT    SPACE
     C                   endif
     C                   ENDIF                                                  LINEED GT 58
     C*
     C* CHANGE FONT TO SMALLER PRINT IF NEED TO PRINT STATE COMMENTS
     C*
     C                   CLEAR                   STCOMM
     C*
     C* PRINT LINES FOR COMMENTS
     C*
     C                   EXSR      PRTSC
     C                   ENDIF                                                  STAT <> 'I'
     C*
     C     F4KY1         READPE    PF401                                  98
     C*
     C     *IN98         IFEQ      *OFF
     C                   If        isUseSpoolEOB
     C                   EXCEPT    SPACE
     C                   endif
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C* GET GROUP COMMENTS BASED ON FROM SERVICE DATE IN FAEOP
     C*
     C                   If        isUseSpoolEOB
     C                   EXCEPT    SPACE
     C                   endif
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
     C*
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
     C*****LINEED        IFGT      58
      /Free
       If (LINEED > 58 And Cltid <> '129') Or
          (LINEED > 80 And Cltid = '129');
      /End-Free
     C                   SETON                                        OF
     C                   EXSR      NWPG1
     C*
     C                   ENDIF                                                  LINEED GT 58
     C*
     C* PRINT GROUP COMMENTS
     C*
     C                   EXSR      PRTGC
     C                   CLEAR                   GRCOMM
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
     C                   ENDIF                                                  *IN98 = *OFF
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDSR
     C*================================================================
     C* SRWRAP - WRAPPING LOGIC SO THAT WORDS DON'T CUT OFF
     C*================================================================
     C     SRWRAP        BEGSR
     C*
     C** If there is no blank in the FIRST note , then we don't wrap
     C*
     C     ' '           SCAN      TNOTE         POS                      98
     C*
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
     C*
     C                   ELSE
     C                   ADD       70            STR
     C                   ENDIF
     C*
     C                   ADD       1             LOOP
     C                   ENDDO
     C*
     C* CATER TO LAST DESCRIPTION
     C*
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
     C*
     C                   ENDSR
     C*================================================================
     C* NWPG1  - Start new page if overflow indicator is on
     C*================================================================
     C     NWPG1         BEGSR
     C********** *INOA     IFEQ *ON
     C     *INOF         IFEQ      *ON
     C                   If        isUseSpoolEOB
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   EXCEPT    HEADE
     C                   endif
     C**********           MOVE 0         *INOA
     C                   MOVE      0             *INOF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* NWPG13 - Start new page if overflow indicator is on
     C*================================================================
     C     NWPG13        BEGSR
     C********** *INOA     IFEQ *ON
     C     *INOF         IFEQ      *ON
     C                   If        isUseSpoolEOB
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEADC
     C                   endif
     C**********           MOVE 0         *INOA
     C                   MOVE      0             *INOF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* NWPHES - Start new page if overflow indicator is on
     C*================================================================
     C     NWPHES        BEGSR
     C     *INOF         IFEQ      *ON
     C                   If        isUseSpoolEOB
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   EXCEPT    HEADH
     C                   endif
     C                   MOVE      0             *INOF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* SUBROUTINE TO LOAD THE CITY STATE & ZIP
     C*================================================================
     C     SRADDR        BEGSR
     C                   MOVE      *BLANKS       STZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXSTAT        XXST             13
     C                   MOVEA     XXST          STZ
     C                   MOVE      XXZPM1        XXZP1
     C                   MOVEA     XXZP1         STZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           STZ(9)
     C                   MOVE      XXZPM2        XXZP2
     C                   MOVEA     XXZP2         STZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     STZ           XXSZ
     C**********           MOVEASZ        XXSZ
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
      *
     C**********
     **------------------------------------------------------------------*
     C********** SRDET     BEGSR
     **------------------------------------------------------------------*
     C**********
     C**********           Z-ADD0         TTABIL  92
     C**********           Z-ADD0         TTARED  92
     C**********           Z-ADD0         TTAALW  92
     C**********           Z-ADD0         TOTDUE  92
     C**********           Z-ADD0         XXTNR
     C**********           Z-ADD0         XXTOTH
     C**********           Z-ADD0         XXTPAY
     C**********
     C********** CAKY      CHAINPCA01                52
     C***TEST*FOR A HOSPTIAL PROVIDER TYPE.
     C**********
     C********** CAPRTY    COMP '4'                      91
     C********** CARCFL    COMP 'X'                      92ADD 04/23/93
     C**********
     C**********           EXCPTHEAD
     C**N74*****           EXCPTHEAD1                      ADD 10/14/92
     C***74*****           EXCPTHEAD2                      ADD 10/14/92
     C**********
     C***READ*ALL THE CASE RECORDS IN ORDER TO PRINT THEM NOW.
     C**********
     C********** *IN52     DOWEQ'0'
     C**********
     C**Initialize procedure code data structure
     C**********
     C**********           CLEARXXCMSV
     C**********
     C***CONVERT ALL THE DATES TO MM/DD/YY FORMAT.
     C**********
     C**********           Z-ADDCAFDAT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXSVDT  60
     C**********
     C***GET*THE CODE DESCRIPTION.
     C**********
     C****CLOSE*FILE IF OPENED
     C********** STOPN     IFEQ '1'
     C**********           CLOSEFMPRL
     C**********           CLOSEFTABL
     C**********           CLOSEFMSVL1
     C**********           MOVE ' '       STOPN   1
     C**********           END
     C**********
     C****DELETE OVERRIDE
     C**FMPRL*FILE
     C********** STOVR     IFEQ '1'
     C**********           Z-ADD55        LENGTH
     C**********           CALL 'QCMDEXC'                60
     C**********           PARM           QCA,1
     C**********           PARM           LENGTH 155
     C**FTABL*FILE
     C**********           Z-ADD55        LENGTH
     C**********           CALL 'QCMDEXC'                60
     C**********           PARM           QCA,3
     C**********           PARM           LENGTH 155
     C**FMSVL1*FILE
     C**********           Z-ADD55        LENGTH
     C**********           CALL 'QCMDEXC'                60
     C**********           PARM           QCA,5
     C**********           PARM           LENGTH 155
     C**********           MOVE ' '       STOVR   1
     C**********           END
     C**********
     C****OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C**FMPRL*FILE
     C********** STOVR     IFEQ ' '
     C**********           Z-ADD55        LENGTH
     C**********           MOVELQCA,2     @@CMD
     C**********           MOVELEOAST     @@ST             EOMB ADJUD STATE
     C**********           CALL 'QCMDEXC'
     C**********           PARM           @@CMD
     C**********           PARM           LENGTH 155
     C**FTABL*FILE
     C**********           Z-ADD55        LENGTH
     C**********           MOVELQCA,4     @@CMD
     C**********           MOVELEOAST     @@ST
     C**********           CALL 'QCMDEXC'
     C**********           PARM           @@CMD
     C**********           PARM           LENGTH 155
     C**FMSVL1*FILE
     C**********           Z-ADD55        LENGTH
     C**********           MOVELQCA,6     @@CMD1
     C**********           MOVELEOAST     @@ST1
     C**********           CALL 'QCMDEXC'
     C**********           PARM           @@CMD1
     C**********           PARM           LENGTH 155
     C**********           MOVE '1'       STOVR   1
     C**********           END
     C**********
     C****OPEN*FILE FOR PROCESSING
     C********** STOPN     IFNE '1'                          NOT CURRENTLY OPEN
     C**********           OPEN FMPRL
     C**********           OPEN FTABL
     C**********           OPEN FMSVL1
     C**********           MOVE '1'       STOPN
     C**********           END
     C**********
     C**********           MOVE *BLANKS   XXDESC 36
     C********** CAPRTY    IFEQ '4'
     C**********           MOVE *BLANK    XXCODE 11
     C**********           Z-ADDCASVCD    XXRVCD  30
     C**********           MOVE CACPT4    XXHCPC  5
     C**********           Z-ADD*ZEROS    XXUNTS  30       ADD 05/31/92
     C**********           Z-ADDCAFDAT    XXEFDT
     C********** SVKEY     SETGTPSV01
     C********** NXTSV1    TAG
     C**********           READPPSV01                    55
     C********** *IN55     IFEQ '0'
     C********** SVCO#     ANDEQUPCO#
     C********** SVCMSV    ANDEQXXCMSV                     CHG 04/28/92
     C********** SVSTAT    IFNE 'I'
     C**N55*****           MOVELSVDESC    XXDESC
     C**********           ELSE
     C**********           GOTO NXTSV1
     C**********           END
     C**********           ELSE
     C********** HCKY      CHAINPHC01                55
     C**********           MOVEL'UNLISTED'XXDESC
     C********** *IN55     IFEQ '0'
     C**********           MOVE HCDESC    XXDESC
     C**********           END
     C**********           END
     C**********           MOVE *BLANK    XXCODE
     C**Establish XXCODE for printing (will print correct UNITS)
     C**********           Z-ADDCASVCD    XXRVCD
     C**********           MOVE CACPT4    XXHCPC
     C**********           Z-ADDCAUNTS    XXUNTS
     C**********           MOVE XXCMSV    XXCODE
     C**********           ELSE
     C********** CARCFL    IFNE 'X'
     C****PROCEDURE TYPE
     C**********           Z-ADDCAFDAT    XXEFDT
     C********** PRKEY     SETGTPPR01
     C**********           READPPPR01                    55
     C**********           MOVE *BLANKS   XXDESC
     C********** *IN55     IFEQ '0'
     C********** PRCO#     ANDEQEOCO#
     C********** PRCPT4    ANDEQCACPT4
     C**********           MOVELPRDSC1    XXD100100
     C**********           MOVE PRDSC2    XXD100
     C**********           MOVELXXD100    XXDESC
     C**********           ELSE
     C*****************************************************
     C****CHK*REAS/CUST FILE FOR HIT
     C********** RCKY      SETGTPRC02
     C********** NXTRC5    TAG
     C**********           READPPRC02                    60
     C********** *IN60     IFEQ '0'
     C********** RCCO#     ANDEQUPCO#
     C********** RCCPT4    ANDEQCACPT4
     C********** RCSTAT    IFNE 'I'
     C**********
     C**********           MOVELRCDSC1    XXD100100
     C**********           MOVE RCDSC2    XXD100
     C**********           MOVELXXD100    XXDESC
     C**********
     C**********           ELSE
     C**********           GOTO NXTRC5
     C**********           END
     C**********           ELSE
     C**********           MOVEL*BLANK    XXDESC
     C**********           END
     C*****************************************************
     C**********           END
     C**********           MOVELCACPT4    XXCODE
     C**********           ELSE                            CARCFL = X
     C********** HCKY      CHAINPHC01                40
     C********** *IN40     IFEQ '0'
     C**********           MOVELHCNDC     XXCODE
     C********** RXKY1     CHAINPRX01                41
     C********** *IN41     IFEQ '0'
     C**********           MOVELRXNAME    XXDESC
     C**********           ELSE
     C**********           MOVE *BLANKS   XXDESC
     C**********           END
     C**********           ELSE
     C**********           MOVE *BLANKS   XXCODE
     C**********           MOVE *BLANKS   XXDESC
     C**********           END
     C**********           END
     C**********           END
     C**********
     C**********           MOVELCAPMOD    XXPMOD  2
     C******************** Z-ADDCATOTC    XXABIL  92
     C********** CACPT4    IFNE 'FSMRA'
     C**********           Z-ADDCATOTC    XXABIL  92
     C**********           ELSE                            ELSE NE 'FSMRA'
     C**********           Z-ADD*ZEROS    XXABIL
     C**********           END                             END NE 'FSMRA'
     C********** *IN74     IFEQ '0'
     C**********           Z-ADDCATOTD    XXARED  92
     C**********           Z-ADDCATOTP    XXAALW  92
     C**********           END                             END *IN74
     C**********           MOVE CAEOMB    XXEOMB  3
     C********** *IN74     IFEQ '1'
     C**********           Z-ADDCANDIS    XXNRED  72
     C**********           ADD  CANDIS    XXTNR  102
     C**********
     C****IF*OTHER REDUCTIONS - CAOTHR - IS BLANK, SUBTRACT MRA DEDUCTIONS
     C****IF*OTHER REDUCTIONS - CAOTHR - IS NOT BLANK, SUB OTHER DEDUCTIONS
     C****TO*GET THE REC PAYMENT ...
     C**********
     C**********           Z-ADD*ZEROS    XXRPAY
     C**********           Z-ADD*ZEROS    XXOTHR
     C**********           Z-ADD*ZEROS    XXARED
     C**********
     C***********CATOTC****SUB**CAOTHR****XXRPAY**72
     C**********
     C********** CATOTC    SUB  CAOTHR    XXRPAY  92
     C**********           Z-ADDCAOTHR    XXOTHR  72
     C********** XXRPAY    SUB  CATOTD    XXRPAY           ADD 03/17/93
     C**********           Z-ADDCATOTD    XXARED
     C**********
     C***NOW*SUBTRACT THE NETWORK DISCOUNT
     C**********
     C********** XXRPAY    SUB  CANDIS    XXRPAY
     C**********
     C***ACCUMULATE THE TOTALS FOR THIS EOB
     C**********
     C********** XXRPAY    ADD  XXTPAY    XXTPAY  92
     C********** XXOTHR    ADD  XXTOTH    XXTOTH 102
     C********** XXARED    ADD  TTARED    TTARED
     C**********           END                             END *IN74
     C**********
     C**********
     C**********
     C**ESTABLISH CODE ARRAY
     C**IGNORE*CODES THAT EXIST IN CD ARRAY
     C**********           MOVE ' '       XXFLAG  1
     C********** 1         DO   40        XX      20
     C********** CAEOMB    IFEQ CD,XX
     C**********           MOVE 'Y'       XXFLAG  1
     C**********           ENDIF
     C**********           ENDDO
     C********** XXFLAG    IFEQ ' '
     C**********           ADD  1         ZZ      20
     C**********           Z-ADDZZ        ZZHOLD  20       HOLD LAST ARRAY ELEME
     C**********           MOVE CAEOMB    CD,ZZ
     C**********           ENDIF
     C**********
     C***IF*THE*SEQ# OF THE CHARGE FILE IS 0 THEN MOVE THE EOMB CODE DESCRIPTION
     C***INTO*THE SERVICE FIELD DESCRIPTION.
     C**********
     C********** CADSEQ    IFEQ *ZEROS
     C**********           MOVE *BLANK    XXDESC
     C**********           MOVELXXEBDE    XXDESC
     C**********           END
     C**********
     C**********
     C**********
     C***ADD*TOTALS FOR THE PROVIDER TOTALS LINE.
     C**********
     C******************** ADD  CATOTC    TTABIL  92
     C********** CACPT4    IFNE 'FSMRA'
     C**********
     C**********           ADD  CATOTC    TTABIL
     C**********
     C**********           ENDIF                           END NE 'FSMRA'
     C**N74*****           ADD  CATOTD    TTARED  92
     C**N74*****           ADD  CATOTP    TTAALW  92
     C***74*****           ADD  XXRPAY    TTAALW  92
     C**********
     C***OUTPUT*DETAIL LINE ITEM
     C**********
     C********** *INOF     IFEQ '1'
     C**********           EXCPTCNT
     C**********           EXCPTCNTHD
     C**N74*****           EXCPTCNTHD1
     C***74*****           EXCPTCNTHD2
     C**********           MOVE '0'       *INOF
     C**********           END
     C****SET*ANY CONTROLING INDICATORS HERE
     C**********
     C********** XXPMOD    COMP *BLANKS              9292
     C**********
     C**********
     C**N74*****           EXCPTDET
     C***74*****           EXCPTDET1                       ADD 10/14/92
     C**********
     C***READ*THE NEXT CASE DETAIL RECORD FOR THIS CLAIM.
     C**********
     C********** CAKY      READEPCA01                    52
     C**********
     C***END*OF*READE LOOP CONTROLLED BY *IN52.
     C**********
     C**********           ENDDO
     C**********
     C**********           EXSR TOTEOB
     C**********
     **------------------------------------------------------------------*
     ****EOMB*FORM TOTALS                                    T O T E O B *
     **------------------------------------------------------------------*
     C**********
     C********** TOTEOB    BEGSR
     C**********
     C***GET*THE EOMB CODE DESCRIPTION.
     C**********           DO   3         X       50
     C********** EOS,X     IFNE *BLANK
     C**********           MOVELEOS,X     XXKEY
     C********** TBLKEY    CHAINPEB01                55
     C**********
     C********** X         ADD  94        I       50
     C********** *IN55     IFEQ '0'                        DESCRIPTION IS
     C**********           MOVELEBDESC    TTM,X            PRESENT
     C**********           MOVE '1'       *IN,I
     C**********           ELSE
     C**********           MOVE *BLANK    TTM,X
     C**********           MOVE '0'       *IN,I
     C**********           END
     C**********
     C**********           END
     C**********           END
     C**********
     C********** XCADR2    COMP *BLANKS              9494
     C**********
     C***********Q4LIN#****IFGE 50*************************NOT ENOUGH ROOM
     C**********
     C********** *IN74     IFEQ *OFF
     C********** Q4LIN#    ANDGT52                         NOT ENOUGH ROOM
     C**********
     C********** *IN74     OREQ *ON
     C********** Q4LIN#    ANDGT51                         NOT ENOUGH ROOM
     C**********
     C**********           EXCPTCNT
     C**********           EXCPTCNTHD
     C**N74*****           EXCPTCNTHD1
     C***74*****           EXCPTCNTHD2
     C**********           END                             ING
     C**********
     C**N74*****           EXCPTTOT
     C***74*****           EXCPTTOT1                       ADD 10/14/92
     C**********
     C**********
     C********** If Network: Print network information
     C********** ------------------------------------------------------
     C**********
     C**********    If New Mexico: Print state unique message
     C**********    ---------------------------------------------------
     C********** XXAST     IFEQ 'NM'
     C**********           DO   5         I1      30
     C**********
     C**********           EXSR OVFLOW                     ADD 09/28/98
     C**********
     C**********           MOVELNM,I1     XXSTM  65 P
     C**********           EXCPTTOT1ST
     C**********           ENDDO                           I1 <= 5
     C**********           ENDIF                           AST = 'NM'
     C**********
     C**********    If South Carolina: Print state unique message
     C**********    ---------------------------------------------------
     C********** XXAST     IFEQ 'SC'
     C**********           DO   6         I1               CHG 09/28/98
     C**********
     C**********           EXSR OVFLOW                     ADD 09/28/98
     C**********
     C**********           MOVELSC,I1     XXSTM
     C**********           EXCPTTOT1ST
     C**********           ENDDO                           I1 <= 6
     C**********           ENDIF                           AST = 'SC'
     C**********
     C**********    If North Carolina: Print state unique message
     C**********    ---------------------------------------------------
     C********** XXAST     IFEQ 'NC'
     C**********           DO   22        I1
     C**********           DO   24        I1
     C**********
     C**********           EXSR OVFLOW
     C**********
     C**********           MOVELNC,I1     XXSTM
     C**********           EXCPTTOT1ST
     C**********           ENDDO                           I1 <= 22
     C**********           ENDIF                           AST = 'SC'
     C**********
     C**********    If Florida: Print state unique message
     C**********    ---------------------------------------------------
     C********** XXAST     IFEQ 'FL'
     C**********           DO   7         I1      30
     C**********
     C**********           EXSR OVFLOW                     ADD 09/28/98
     C**********
     C**********           MOVELFL,I1     XXSTM  65 P
     C**********           EXCPTTOT1ST
     C**********           ENDDO                           I1 <= 7
     C**********           ENDIF                           AST = 'FL'
     C**********
     C***74*****           EXSR OVFLOW
     C***74*****           EXCPTTOT1B
     C**********           MOVE *BLANKS   TTM
     C**********
     C**++++++++++++++++++++++++++++++
     C**SORT*CODE ARRAY IN ASCENDING ORDER
     C**********           SORTACD
     C**********
     C***********50********SUB  Q4LIN#    XXNUM   20
     C**********
     C********** 58        SUB  Q4LIN#    XXNUM   20
     C**********
     C********** XXNUM     IFLT ZZHOLD
     C********** CD,40     ANDNE*BLANKS
     C**********           EXCPTCNT
     C**********           ENDIF
     C**********
     C**FIRST*LINE OF BOX;CD,40 <> BLANKS IF ANY CODE EXISTS IN ARRAY AFTER SORT
     C**********           MOVE '0'       *IN62
     C********** CD,40     IFNE *BLANKS
     C**********           EXCPTTABL1
     C********** XXNUM     IFLT ZZHOLD
     C**********           EXCPTTABL5
     C**********           ELSE
     C**********           EXCPTTABL2
     C**********           ENDIF
     C**********           MOVE '1'       *IN62
     C**********           ENDIF
     C**********
     C**********           MOVE *BLANKS   XXCD    3
     C********** 1         DO   40        XX      20
     C********** CD,XX     IFNE *BLANKS
     C**********
     C**********           MOVE CD,XX     XXCD
     C**********
     C**********           MOVE *BLANK    XXEBDE
     C***GET*THE EOMB CODE DESCRIPTION.
     C**********           MOVE *BLANKS   XXKEY
     C**********           MOVELCD,XX     XXKEY
     C**********
     C**Get*date of change to new EOMB codes
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                55
     C**Based*upon EOMB Trans Date vs. date of change, use correct file
     C********** EOTRDT    IFLT FBDT01
     C********** TBLKEY    CHAINPEB02                55
     C**********           ELSE
     C********** TBLKEY    CHAINPEB01                55
     C**********           END
     C***55*****           MOVE *BLANK    XXEBDE
     C**N55*****           MOVELEBDESC    XXEBDE 70
     C**********
     C**********
     C**********           EXCPTTABL3
     C**********
     C**********           ENDIF                           ENDIF CD,XX<> BLANKS
     C**********           ENDDO
     C**LAST*LINE OF BOX
     C***62*****           EXCPTTABL1
     C***+++++++++++++++++++++++++++++
     C**********
     C***CHECK*FOR EXISTENCE OF ANY CODES AFTER SORT
     C**********
     C********** CD,40     IFNE *BLANKS
     C**********
     C********** 59        SUB  Q4LIN#    XXNUM   20
     C**********
     C********** XXNUM     IFLT ZZHOLD
     C**********
     C********** ZZHOLD    IFEQ 1
     C********** Q4LIN#    ORGT 60
     C**********           EXCPTCNT
     C**********           EXCPTCNTHD
     C**********           EXCPTTABL4
     C**********           ENDIF                           ZZHOLD=1
     C**********
     C**********           ENDIF                           XXNUM<ZZHOLD
     C**********
     C**********           MOVE *OFF      HDRFLG  1
     C**********           MOVE *ON       HDR1ST  1
     C**********
     C***FIRST*LINE OF BOX
     C**********
     C**********           EXCPTTABL1
     C**********
     C**********           MOVE *BLANKS   XXCD    3
     C**********
     C********** 1         DO   40        XX
     C**********
     C********** CD,XX     IFNE *BLANKS
     C**********
     C********** HDRFLG    IFEQ *OFF
     C**********
     C********** HDR1ST    IFEQ *OFF
     C**********           EXCPTTABL5
     C**********           ELSE
     C**********           EXCPTTABL2
     C**********           MOVE *OFF      HDR1ST
     C**********           ENDIF                           HDR1ST=*OFF
     C**********
     C**********           MOVE *ON       HDRFLG
     C**********
     C**********           ENDIF                           HDRFLG=*OFF
     C**********
     C**********           MOVE CD,XX     XXCD
     C**********
     C***GET*THE EOMB CODE DESCRIPTION
     C**********
     C**********           MOVELCD,XX     XXKEY     P
     C**********
     C**Get*date of change to new EOMB codes
     C**********
     C**********           MOVEL'FB'      XSKEY     P
     C**********
     C********** FBKY      CHAINSFB01                55
     C**********
     C**Based*upon EOMB Trans Date vs. date of change, use correct file
     C**********
     C********** EOTRDT    IFLT FBDT01
     C********** TBLKEY    CHAINPEB02                55
     C**********           ELSE
     C********** TBLKEY    CHAINPEB01                55
     C**********           ENDIF                           EOTRDT<FBDT01
     C**********
     C***55*****           MOVE *BLANK    XXEBDE
     C**N55*****           MOVELEBDESC    XXEBDE 70 P
     C**********
     C**********           EXCPTTABL3
     C**********
     C********** Q4LIN#    IFGE 61
     C********** XX        ANDNE40
     C**********           EXCPTCNT
     C**********           EXCPTCNTHD
     C**********           EXCPTTABL4
     C**********           MOVE *OFF      HDRFLG
     C**********           ENDIF                           Q4LIN#>=61
     C**********
     C**********           ENDIF                           CD,XX<>*BLANKS
     C**********
     C**********           ENDDO
     C**********
     C**LAST*LINE OF BOX
     C**********
     C**********           EXCPTTABL1
     C**********
     C**********           ENDIF                           CD,40<>*BLANKS
     C**********
     C**********           ENDSR
     C*
     **------------------------------------------------------------------*
     C     SRSTB2        BEGSR
     **------------------------------------------------------------------*
     C                   MOVEL     XXNAM1        X1NAME           20
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DET2
     C                   endif
      * PRINT FRAUD STATEMENT IF INDICATED
     C     B2FLG1        IFEQ      'Y'
     C     CMJRST        ANDEQ     'CA'
     C                   If        isUseSpoolEOB
     C                   EXCEPT    FRAUD
     C                   endif
     C                   ENDIF
     C                   ENDSR
      *
     C     OVFLOW        BEGSR
     C     *INOF         IFEQ      '1'
     C**********           EXCPTHEAD
     C**N74*****           EXCPTHEAD1
     C***74*****           EXCPTHEAD2                      ADD 10/14/92
     C                   EXSR      MSKSS#
     C                   If        isUseSpoolEOB
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   EXCEPT    HEADE
     C                   EXCEPT    HEADB
     C                   EXCEPT    HEADC                                        ADD 10/14/92
     C                   endif
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ENDSR
     C*****************************************************************
     C* FILLG - FILL THE GROUP COMMENTS
     C*****************************************************************
     C     FILLG         BEGSR
     C*
     C                   MOVEL(P)  F3EC01        XXPRTA
     C                   MOVEL(P)  F3EC02        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN01          105
     C*
     C                   MOVE(P)   F3EC02        XXPRTC
     C                   MOVEL(P)  F3EC03        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN02          105
     C*
     C                   MOVEL(P)  F3EC04        XXPRTA
     C                   MOVEL(P)  F3EC05        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN03          105
     C*
     C                   MOVE(P)   F3EC05        XXPRTC
     C                   MOVEL(P)  F3EC06        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN04          105
     C*
     C                   MOVEL(P)  F3EC07        XXPRTA
     C                   MOVEL(P)  F3EC08        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN05          105
     C*
     C                   MOVE(P)   F3EC08        XXPRTC
     C                   MOVEL(P)  F3EC09        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN06          105
     C*
     C                   MOVEL(P)  F3EC10        XXPRTA
     C                   MOVEL(P)  F3EC11        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN07          105
     C*
     C                   MOVE(P)   F3EC11        XXPRTC
     C                   MOVEL(P)  F3EC12        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN08          105
     C*
     C                   MOVEL(P)  F3EC13        XXPRTA
     C                   MOVEL(P)  F3EC14        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN09          105
     C*
     C                   MOVE(P)   F3EC14        XXPRTC
     C                   MOVEL(P)  F3EC15        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN10          105
     C*
     C                   MOVEL(P)  F3EC16        XXPRTA
     C                   MOVEL(P)  F3EC17        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN11          105
     C*
     C                   MOVE(P)   F3EC17        XXPRTC
     C                   MOVEL(P)  F3EC18        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN12          105
     C*
     C                   MOVEL(P)  F3EC19        XXPRTA
     C                   MOVEL(P)  F3EC20        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN13          105
     C*
     C                   MOVE(P)   F3EC20        XXPRTC
     C                   MOVEL(P)  F3EC21        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN14          105
     C*
     C                   MOVEL(P)  F3EC22        XXPRTA
     C                   MOVEL(P)  F3EC23        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN15          105
     C*
     C                   MOVE(P)   F3EC23        XXPRTC
     C                   MOVEL(P)  F3EC24        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN16          105
     C*
     C                   MOVEL(P)  F3EC25        XXPRTA
     C                   MOVEL(P)  F3EC26        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN17          105
     C*
     C                   MOVE(P)   F3EC26        XXPRTC
     C                   MOVEL(P)  F3EC27        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN18          105
     C*
     C                   MOVEL(P)  F3EC28        XXPRTA
     C                   MOVEL(P)  F3EC29        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN19          105
     C*
     C                   MOVE(P)   F3EC29        XXPRTC
     C                   MOVEL(P)  F3EC30        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN20          105
     C*
     C                   ENDSR
     C*****************************************************************
     C* FILLS - FILL THE STATE COMMENTS
     C*****************************************************************
     C     FILLS         BEGSR
     C*
     C                   MOVEL(P)  F4EC01        XXPRTA
     C                   MOVEL(P)  F4EC02        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN01
     C*
     C                   MOVE(P)   F4EC02        XXPRTC
     C                   MOVEL(P)  F4EC03        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN02
     C*
     C                   MOVEL(P)  F4EC04        XXPRTA
     C                   MOVEL(P)  F4EC05        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN03
     C*
     C                   MOVE(P)   F4EC05        XXPRTC
     C                   MOVEL(P)  F4EC06        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN04
     C*
     C                   MOVEL(P)  F4EC07        XXPRTA
     C                   MOVEL(P)  F4EC08        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN05
     C*
     C                   MOVE(P)   F4EC08        XXPRTC
     C                   MOVEL(P)  F4EC09        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN06
     C*
     C                   MOVEL(P)  F4EC10        XXPRTA
     C                   MOVEL(P)  F4EC11        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN07
     C*
     C                   MOVE(P)   F4EC11        XXPRTC
     C                   MOVEL(P)  F4EC12        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN08
     C*
     C                   MOVEL(P)  F4EC13        XXPRTA
     C                   MOVEL(P)  F4EC14        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN09
     C*
     C                   MOVE(P)   F4EC14        XXPRTC
     C                   MOVEL(P)  F4EC15        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN10
     C*
     C                   MOVEL(P)  F4EC16        XXPRTA
     C                   MOVEL(P)  F4EC17        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN11
     C*
     C                   MOVE(P)   F4EC17        XXPRTC
     C                   MOVEL(P)  F4EC18        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN12
     C*
     C                   MOVEL(P)  F4EC19        XXPRTA
     C                   MOVEL(P)  F4EC20        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN13
     C*
     C                   MOVE(P)   F4EC20        XXPRTC
     C                   MOVEL(P)  F4EC21        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN14
     C*
     C                   MOVEL(P)  F4EC22        XXPRTA
     C                   MOVEL(P)  F4EC23        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN15
     C*
     C                   MOVE(P)   F4EC23        XXPRTC
     C                   MOVEL(P)  F4EC24        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN16
     C*
     C                   MOVEL(P)  F4EC25        XXPRTA
     C                   MOVEL(P)  F4EC26        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN17
     C*
     C                   MOVE(P)   F4EC26        XXPRTC
     C                   MOVEL(P)  F4EC27        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN18
     C*
     C                   MOVEL(P)  F4EC28        XXPRTA
     C                   MOVEL(P)  F4EC29        XXPRTB
     C                   MOVEL(P)  XXLIN1        XXLN19
     C*
     C                   MOVE(P)   F4EC29        XXPRTC
     C                   MOVEL(P)  F4EC30        XXPRTD
     C                   MOVEL(P)  XXLIN2        XXLN20
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Mask the SS# to be printed
     C*====================================================================
     C     MSKSS#        BEGSR
     C*
     C*******              SELEC
     C*******    ##CLT#    WHEQ '114'
     C                   MOVEL     '***'         XXSS#1
     C                   MOVEL     '**'          XXSS#2
     C******               ENDSL
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* NMBRLN - DETERMINES NUMBER OF LINES IN GROUP AND STATE COMMENTS
     C*****************************************************************
     C     NMBRLN        BEGSR
     C*
     C     XXLN01        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN02        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN03        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN04        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN05        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN06        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN07        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN08        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN09        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN10        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN11        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN12        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN13        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN14        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN15        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN16        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN17        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN18        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN19        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C     XXLN20        IFNE      *BLANKS
     C*
     C                   ADD       1             NUMLIN
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* PRTGC - PRINT GROUP COMMENTS
     C*****************************************************************
     C     PRTGC         BEGSR
     C*
     C     XXLN01        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN01        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN02        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN02        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN03        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN03        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN04        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN04        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN05        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN05        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN06        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN06        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN07        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN07        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN08        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN08        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN09        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN09        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN10        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN10        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN11        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN11        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN12        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN12        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN13        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN13        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN14        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN14        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN15        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN15        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN16        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN16        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN17        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN17        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN18        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN18        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN19        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN19        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN20        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN20        GRCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL9
     C                   endif
     C*
     C                   ENDIF
     C*
     C*** Write to Scriptura Group Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtGpNtWrk
     C                   Endif
     C*
     C                   ENDSR
     C*****************************************************************
     C* PRTSC - PRINT STATE COMMENTS
     C*****************************************************************
     C     PRTSC         BEGSR
     C*
     C                   MOVE      *OFF          *IN74
     C     F4BLD1        IFEQ      'Y'
     C                   MOVE      *ON           *IN74
     C                   ENDIF
     C*
     C     XXLN01        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN01        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN02        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN02        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN03        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN03        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN04        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN04        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN05        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN05        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN06        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN06        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN07        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN07        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN08        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN08        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN09        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN09        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN10        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN10        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN11        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN11        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN12        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN12        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN13        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN13        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN14        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN14        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN15        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN15        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN16        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN16        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN17        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN17        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN18        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN18        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN19        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN19        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C     XXLN20        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  XXLN20        STCOMM
     C                   If        isUseSpoolEOB
     C                   EXCEPT    DETL8
     C                   endif
     C*
     C                   ENDIF
     C*
     C*** Write to Scriptura State Notes Workfile
     C                   If        isScriptEOB = 'Y'
     C                   Exsr      wrtStNtWrk
     C                   Endif
     C*
     C                   ENDSR
     C*****************************************************************
     C* MIINFO - ADDITIONAL INFORMATION
     C*****************************************************************
     C     MIINFO        BEGSR
     C*
     C* Header information
     C*
     C* Print
     C                   If        isUseSpoolEOB
     C                   EXCEPT    CNT
     C                   EXCEPT    CNTHD
     C                   EXCEPT    ADINH1
     C                   endif
     C*
     C* Carrier, Address and Phone
     C*
     C                   CLEAR                   XXNAM1
     C                   CLEAR                   XXFAD1
     C                   CLEAR                   XXFAD2
     C                   CLEAR                   XXCTY
     C                   CLEAR                   XXSTAT
     C                   CLEAR                   XXZIP
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXCSZ1
     C                   CLEAR                   XXAREA
     C                   CLEAR                   XXPHN1
     C                   CLEAR                   XXPHN2
     C                   SETOFF                                       31
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        XXNAM1
     C                   MOVEL(P)  COMAD1        XXFAD1
     C*
     C     COMAD2        IFNE      *BLANKS
     C                   MOVEL(P)  COMAD2        XXFAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXFAD2
     C                   SETOFF                                       31
     C                   ENDIF                                                  COMAD2 IFNE *BLANK
     C*
     C                   MOVEL(P)  COMCTY        XXCTY
     C                   MOVEL     COMST         XXSTAT
     C                   MOVE      COMZIP        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ1
     C                   MOVE      COAREA        XXAREA
     C                   MOVEL     COPHN#        XXPHN1
     C                   MOVE      COPHN#        XXPHN2
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
     C                   If        isUseSpoolEOB
     C                   EXCEPT    ADIND1
     C                   endif
     C*
     C* NAICS/Self Insured
     C*
     C                   MOVE      *BLANKS       XXNAIC            9
     C                   MOVE      *BLANKS       GCDCAR
     C*
     C     GCKY          CHAIN     PGC01                              90
     C*
     C                   SELECT
     C*
     C     GCDCAR        WHENEQ    'PPC'
     C                   MOVEL     287113499     XXNAIC
     C*
     C     GCDCAR        WHENEQ    'ICI'
     C                   MOVEL     114873499     XXNAIC
     C*
     C                   ENDSL
     C*
     C* Print
     C                   If        isUseSpoolEOB
     C                   EXCEPT    ADIND2
     C                   endif
     C*
     C* Service company
     C*
     C                   CLEAR                   XXNAM1
     C                   CLEAR                   XXFAD1
     C                   CLEAR                   XXFAD2
     C                   CLEAR                   XXCTY
     C                   CLEAR                   XXST
     C                   CLEAR                   XXZIP
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXCSZ1
     C                   SETOFF                                       31
     C* Print
     C                   If        isUseSpoolEOB
     C                   EXCEPT    ADIND3
     C                   endif
     C*
     C* Employee Name and Address
     C*
     C                   CLEAR                   QQLAST
     C                   CLEAR                   QQFRST
     C                   CLEAR                   XXCLNM
     C                   CLEAR                   XXFAD1
     C                   CLEAR                   XXFAD2
     C                   CLEAR                   XXCTY
     C                   CLEAR                   XXST
     C                   CLEAR                   XXZIP
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXCSZ1
     C                   SETOFF                                       31
     C*
     C     CDKEY         CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCLNM           30
     C*
     C                   MOVEL     CDADR1        XXFAD1
     C*
     C     CDADR2        IFNE      *BLANKS
     C                   MOVEL     CDADR2        XXFAD2
     C                   SETON                                        31
     C                   ELSE
     C                   MOVE      *BLANKS       XXFAD2
     C                   SETOFF                                       31
     C                   ENDIF                                                  CDADR2 IFNE *BLANKS
     C*
     C                   MOVEL(P)  CDCTY         XXCTY
     C                   MOVEL     CDST          XXST
     C                   MOVEL     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ1
     C*
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C* Print
     C                   If        isUseSpoolEOB
     C                   EXCEPT    ADIND4
     C                   endif
     C*
     C                   ENDSR
     C*================================================================
     C*
     C*  LAINFO- Print LA Pharmacy Claimant Info
     C*
     C*================================================================
     C     LAINFO        BEGSR
     C*
     C**Header information
     C**Print
     C******               EXCPTADINH2
     C*
     C                   If        isUseSpoolEOB
     C                   EXSR      NWPG1
     C                   EXCEPT    SPACE
     C                   EXCEPT    SPACE
     C                   endif
     C                   CLEAR                   XXCLNM
     C                   CLEAR                   XXNAM1
     C                   CLEAR                   XXFAD1
     C                   CLEAR                   XXFAD2
     C                   CLEAR                   XXCTY
     C                   CLEAR                   XXSTAT
     C                   CLEAR                   XXZIP
     C                   CLEAR                   XXCSZ
     C                   CLEAR                   XXCSZ1
     C                   SETOFF                                       82
     C*
     C     CDKEY         CHAIN     PCD01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCLNM           30
     C*
     C                   MOVEL     CDADR1        XXFAD1
     C*
     C     CDADR2        IFNE      *BLANKS
     C                   MOVEL     CDADR2        XXFAD2
     C                   SETON                                        82
     C                   ELSE
     C                   MOVE      *BLANKS       XXFAD2
     C                   SETOFF                                       82
     C                   ENDIF                                                  CDADR2 IFNE *BLANKS
     C*
     C                   MOVEL(P)  CDCTY         XXCTY
     C                   MOVEL     CDST          XXSTAT
     C                   MOVEL     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL(P)  XXCSZ         XXCSZ1
     C*
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C* Print
     C                   If        isUseSpoolEOB
     C                   EXCEPT    ADIND5
     C                   endif
     C*
     C                   ENDSR
      /Free
       //-------------------------------------------------------------
       // WrtDetCdWrk -  Write to the Detail EOB Code workfile
       //--------------------------------------------------------------
           Begsr WrtDetCdWrk;
             clear pDec01;
             DECxml = XPxmlT;
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
             //*** Beg Chg *** 07/18/23 ***********************
             //*** DECeob  = DEeob;
             DECeob  = DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
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
             TOTxml = XPxmlT;
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
             DGxml = XPxmlT;
             DGco# = EOco#;
             DGsub# = EOsub#;
             DGfnd = EOfnd;
             DGfyr = EOfyr;
             DGcase = EOcase;
             DGeob# = EOeob#;
             DGRcn# = EOrcn;
             DGemp# = EMemp#;
             DGdiv = EMdiv;
             if xxDs1 <> ' ';
                DGdNum = xxnum1;
                DGdiag = xxDs1;
                DGdesc = Desc1;
                DGiDt  = xxIdt;
                Write pDG01;
             endif;
             if xxDs2 <> ' ';
                DGdNum = xxnum2;
                DGdiag = xxDs2;
                DGdesc = Desc2;
                DGiDt  = xxIdt;
                Write pDG01;
             endif;
             if xxDs3  <> ' ';
                DGdNum = xxnum3;
                DGdiag = xxDs3;
                DGdesc = Desc3;
                DGiDt  = xxIdt;
                Write pDG01;
             endif;
             if xxDs4  <> ' ';
                DGdNum = xxnum4;
                DGdiag = xxDs4;
                DGdesc = Desc4;
                DGiDt  = xxIdt;
                Write pDG01;
             endif;
           Endsr;
       //-------------------------------------------------------------
       // WrtHdWrk -  Write to the EOB header workfile
       //--------------------------------------------------------------
           Begsr WrtHdWrk;
             clear pHd01;
             HDxml = XPxmlT;
             HDco# = EOco#;
             HDsub# = EOsub#;
             HDfnd = EOfnd;
             HDfyr = EOfyr;
             HDcase = EOcase;
             HDeob# = EOeob#;
             HDRcn# = EOrcn;
             HDemp# = EMemp#;
             HDdiv = EMdiv;
        //***HDgNam = x1Name;
        //***HDeNam = x1Name;
             HDgNam = xxFn;
             HDeNam = xxEnam;
             HDrvw# = xxTrid;
             HDss#  = xxss#1 + '-' + xxss#2 + '-' + xxss#3;
        //***HDcdName = xxName;
             HDcdName = xxNam1;
        //***HDacDt = x1AcDt;
             monitor;
               HDacDt = %dec(%date(x1AcDt: *usa));
             on-error;
               HDacDt = 0;
             endmon;
        //** HDclaim# = %Char(EOsub#) + '-' + %Char(EOfnd) + '-' +%Char(EOfyr)+
        //**            '-' + %Char(EOcase);
             HDclaim# = %Char(EOsub#) + %Editc(EOfnd:'X') + %Editc(EOfyr:'X')+
                        %Editc(EOcase:'X');
        //***HDsCl# = xxSCL#;
        //***HDint# = %Char(x1Sub#) + '-' + %Char(x1Fnd) + '-' +
        //***         %Char(x1Fyr) + '-' + %Char(x1Case);
             HDcxHex = cxHex;
             HDmUser = xxMusr;
             HDacct = xxAcct;
             HDphar = xxPhar;
             HDbNPI = xxBnpi;
        //***HDtrDt = xxTrDt;
             Monitor;
               HDtrDt = %dec(%date(xxTrDt: *usa));
             On-error;
               HDtrDt = 0;
             endmon;
             HDfein = EOfein;
             HDmod  = EOmod;
             HDpNam = EOpNam;
             HDlic# = xxLic#;
        //***HDcrDt = xxCrDt;
             monitor;
               HDcrDt = %dec(%date(xxCrDt: *usa));
             on-error;
               HDcrDt = 0;
             endmon;
        //***HDprDt = DSdate;
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
        //***HDrcDt = xxRcDt;
             monitor;
               HDrcDt = %dec(%date(xxRcDt: *usa));
             on-error;
               HDrcDt = 0;
             endmon;
             HDckDtHd = hdrChkDate;
             If x1Date = 0;
               HDwcPd = 'N/A';
             Else;
        //***  HDwcPd = %Char(xxWcPd);
               monitor;
                 HDwcPd = %char(%dec(%date(x1Date: *usa)));
               on-error;
                 HDwcPd = *blanks;
               endmon;
             Endif;
             HDchk#Hd = hdrCheck#;
             If RPchk# = 0;
               HDwcPc = 'N/A';
             Else;
        //**** HDwcPc = %Char(EAwcPc);
               HDwcPc = %Char(RPchk#);
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
        //***HDfrDt = x2FrDt;
             monitor;
               HDfrDt = %dec(%date(x2FrDt: *usa));
             on-error;
               HDfrDt = 0;
             endmon;
        //***HDtoDt = x2ToDt;
             monitor;
               HDtoDt = %dec(%date(x2ToDt: *usa));
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
             DNTxml = XPxmlT;
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
             //*** Beg Chg *** 07/18/23 ***********************
             //*** DNTeob  = DEeob;
             DNTeob  = DEeobCd;
             //*** End Chg *** 07/18/23 ***********************
             DNTnote = x1Note;
             DNTiDt   = xxIdt;
             Write pDNT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtDetCdNtWrk -  Write to the Detail EOB Code/Note workfile
       //--------------------------------------------------------------
           Begsr WrtDetCdNtWrk;
             clear pDcnt01;
             DCNTxml = XPxmlT;
             DCNTco# = EOco#;
             DCNTsub# = EOsub#;
             DCNTfnd = EOfnd;
             DCNTfyr = EOfyr;
             DCNTcase = EOcase;
             DCNTeob# = EOeob#;
             DCNTRcn# = EOrcn;
             DCNTemp# = EMemp#;
             DCNTdiv = EMdiv;
             If EOast = 'FL';
               DCNTeob  = EOB(T);
             Endif;
             DCNTnote = xNote;
             DCNiDt   = xxIdt;
             Write pDCNT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtDtWrk -  Write to the Detail Line Items Workfile
       //--------------------------------------------------------------
           Begsr WrtDtWrk;
             clear pDt01;
             DTxml = XPxmlT;
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
        //***DTfDat = xxFdat;
        //***DTtDat = xxTdat;
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
         //***    DTmod1 = xxMod1;
         //***    DTmod2 = xxMod2;
         //***    DTmod3 = xxMod3;
         //***    DTmod4 = xxMod4;
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
         //** DTdRf1 = xxDrf1;
         //** DTdRf2 = xxDrf2;
         //** DTdRf3 = xxDrf3;
         //** DTdRf4 = xxDrf4;
         // Build diagnosis cross reference field

          // Diag cross reference 1 not blank - all combinations
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
             DTchrg = CAchrg;
             DTmra  = CAmra;
             DTothr = CAothr;
             DToth2 = CAoth2;
             DTnDis = CAnDis;
             DTpaid = CApaid;
             DTdesc = xxPrDs;
             DTtax  = x1Tax;
             DTunts = x1Unts;
             DTbCode = xxBild;
         //***    DTeob1 = x1EOB;
         //***    DTeob2 = x2EOB;
         //***    DTeob3 = x3EOB;
         //***    DTeob4 = x4EOB;
             DTiDt   = xxIdt;
             Write pDT01;
           Endsr;
       //-------------------------------------------------------------
       // WrtGenCarNtWrk - Write to the General Carrier Notes Workfile
       //--------------------------------------------------------------
           Begsr WrtGenCarNtWrk;
             clear pEcn01;
             CNxml = XPxmlT;
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
             Write pECN01;
           Endsr;
       //-------------------------------------------------------------
       // WrtStNtWrk - Write to the State Notes Workfile
       //--------------------------------------------------------------
            Begsr WrtStNtWrk;
             clear pSn01;
             SNxml = XPxmlT;
             SNco# = EOco#;
             SNsub# = EOsub#;
             SNfnd = EOfnd;
             SNfyr = EOfyr;
             SNcase = EOcase;
             SNeob# = EOeob#;
             SNrcn# = EOrcn;
             SNemp# = EMemp#;
             SNdiv = EMdiv;
             SNnote = xxLn01;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn02;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn03;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn04;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn05;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn06;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn07;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn08;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn09;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn10;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn11;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn12;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn13;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn14;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn15;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn16;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn17;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn18;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn19;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
             SNnote = xxLn20;
             SNiDt   = xxIdt;
             if SNnote <> *blanks;
               Write pSN01;
             endif;
           Endsr;
       //-------------------------------------------------------------
       // WrtGpNtWrk - Write to the Group Notes Workfile
       //--------------------------------------------------------------
            Begsr WrtGpNtWrk;
             clear pGn01;
             GNxml = XPxmlT;
             GNco# = EOco#;
             GNsub# = EOsub#;
             GNfnd = EOfnd;
             GNfyr = EOfyr;
             GNcase = EOcase;
             GNeob# = EOeob#;
             GNrcn# = EOrcn;
             GNemp# = EMemp#;
             GNdiv = EMdiv;
             GNnote = xxLn01;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn02;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn03;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn04;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn05;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn06;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn07;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn08;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn09;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn10;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn11;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn12;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn13;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn14;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn15;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn16;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn17;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn18;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn19;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
             GNnote = xxLn20;
             GNiDt   = xxIdt;
             if GNnote <> *blanks;
               Write pGN01;
             endif;
           Endsr;
       //-------------------------------------------------------------
       // WrtFtWrk - Write to the EOB Footer Workfile
       //--------------------------------------------------------------
            Begsr WrtFtWrk;
             clear pFt01;
             FTxml = XPxmlT;
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
          //***   FTaCsz = xxCsz2;
             FTaSt = F1aSt;
             FTaCty = F1aCty;
             FTaZip = F1aZip;
             FTarea = xxArea;
             FTaPhn = xxphn1 + xxphn2;
             FTfAra = xxFAra;
             FTfax  = xxFph1 + xxFph2;
             FTins# = xxDiv1;
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
        //***FTsbDt = xxSbDt;
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
      ********************************************************************
      *                      O U T P U T
      ********************************************************************
      *
     OCHECKS    E   N77      BANNER      0  3 01
      *                                           RESET PRINTER TO DFT
     O              N77      ESC
     O              N77                             'E'
      *                                           Portriat Orientation
     O              N77      ESC
     O              N77                             '&l0O'
     O              N77      ESC
     O              N77                             '&l0E'
     O              N77      ESC
     O              N77                             '&a0L'
     O*
     O                       ESC
     O********************************************* '&l1H'
     O                                              '&l4H'
     O*** Font 158 Helv. Bold 14.4
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O*
     O                       ESC
     O                                              '*p1000x1000Y'
     O********************************************  'CHECKS FOR FUND: '
     O                                              'CHECKS FOR GROUP: '
     O                       XXFND
      *
     O/COPY OCHKCHECK
      *_________________________________________________________________
      *
     O**********
     O**********CHECKS  E 0225           HEAD
      *
      *                                            FONT TIMES ROMAN MED.
     O*** Font 159 Times Roman Med 12pt
     O**********               ESC
     O**********                            '(0U'
     O**********               ESC
     O**********                            '(s1p12vsb5T'
     O**********
     O**********
     O**********                         90 'EXPLANATION OF MEDICAL '
     O**********                            'BENEFITS'
     O********E*00             HEAD
     O**********
     O**********                                   FONT MED/COR. 8.5PT/16.66
     O*** Font 226 Line Printer 16.66
     O**********               ESC
     O**********                            '(0U'
     O**********               ESC
     O**********                            '(s0p16.66hsb0T'
     O**********
     O**********
     O********E* 1             HEAD
     O**********                          9 'PATIENT :'
     O**********               XXPNAM    51
     O**********               XCNAME    90
     O**********                        105 'PROVIDER ID:'
     O**********               XXPID Z  114
     O**********               XXPIDMZ  118
     O********E* 1             HEAD
     O**********                          9 'SOC SEC :'
     O**********               X2SS#     20
     O**********               XCADR1    90
     O**********                        105 'PROVIDER   :'
     O**********               XXPRNM   137
     O********E* 1             HEAD
     O**********                          9 'DOA     :'
     O**********               X2ACDT    19 '  /  /  '
     O**********       94      XCADR2    90
     O**********      N94      XCCSZ     90
     O**********               XXPADD   137
     O********E* 1             HEAD
     O**********                          9 'CASE #  :'
     O**********               XF#       31
     O**********       94      XCCSZ     90
     O**********      N94      XCPHN#    74
     O**********               XPCSZ    137
     O********E* 1             HEAD
     O**********                          9 'SRV DTS :'
     O**********               XXFRD Y   19
     O**********                         21 '-'
     O**********               XXTOD Y   30
     O**********       94      XCPHN#    74
     O**********************************105 'CLM ADJ    :'
     O**********                        105 'MEDICAL EXAMINER:'
     O********                 XXUSNM   137
     O**********               XXMEDX   137
     O**********               UPTITL   122
     O********E*1              HEAD
     O**********                         09 'DIAG CD :'
     O**********       67      EODIA1    16
     O**********       68      EODIA2    26
     O**********       69      EODIA3    36
     O**********       70      EODIA4    46
     O**********                        105 'EMPLR#:'
     O**********               EMEMP#Z  114
     O**********                        120 'DEPT:'
     O**********                        100 'EMPLR#:'
     O**********               EMEMP#Z  110
     O**********                        120 'LOCATION:'
     O**********               CMCTG1   131
     O********E*1              HEAD
     O**********                          9 'EOMB:'
     O**********               XXEOB#Z   18
     O**********                         19 '-'
     O**********               UPSTAF    22
     O********E*2              HEAD1
     O**********                         08 'DATE OF'
     O**********         N92N91          20 'PROCEDURE'
     O**********         N92 91          20 ' SERVICE '
     O**********          92N91          20 '    NDC  '
     O**********                         88 'AMOUNT'
     O**********                        124 'EOMB'
     O********E*12             HEAD1
     O**********            N92          08 'SERVICE'
     O**********             92          08 '  FILL '
     O**********                         20 '  CODE   '
     O**********                         47 'DESCRIPTION OF SERVICES'
     O**********                         89 'CHARGED'
     O**********                        103 'DISALLOWED*'
     O**********                        117 'ALLOWANCE'
     O**********                        124 'CODE'
     O********E* 1             DET
     O**********               XXSVDTY    8
     O**********               XXCODE    20
     O**********       92                21 '-'
     O**********       92      XXPMOD    23
     O**********               XXDESC    64
     O**********               XXABIL4   89
     O**********               XXAREDM  103
     O**********               XXAALWL  117
     O**********               XXEOMB   124
     O********E*21             TOT
     O**********                         43 'TOTAL:'
     O**********                         79 'AMOUNT CHARGED.....:'
     O**********               TTABIL4   89
     O********E* 1             TOT
     O**********                         79 'DISALLOWED.........:'
     O**********               TTAREDM  103
     O********E* 1             TOT
     O**********                         79 'ALLOWANCE..........:'
     O**********               TTAALWM  118 '$'
     O********E* 2             TOT
     O**********                        119 '==============='
     O********E* 1             TOT
     O**********                         44 'IF YOU HAVE ANY QUESTION'
     O**********                         67 'S REGARDING THIS REVIEW'
     O**********                         90 ' PLEASE CALL OR MAIL A '
     O**********                         97 'COPY OF'
     O********E* 2             TOT
     O**********
     O**********                         40 'YOUR BILL AND THIS REVIE'
     O**********                         42 'W.'
     O**********                         65 'PLEASE REFERENCE THE '
     O***********************************88 'CLAIMS ADJUSTER ON ALL '
     O***********************************98 'INQUIRIES.'
     O**********                         89 'MEDICAL EXAMINER ON ALL '
     O**********                         99 'INQUIRIES.'
     O*
     O********E*2              HEAD2
     O**********
     O********E*1              HEAD2
     O**********                         08 'DATE OF'
     O**********         N92N91          20 'PROCEDURE'
     O**********         N92 91          20 'SERVICE  '
     O**********          92N91          20 '   NDC   '
     O**********                         74 'AMOUNT'
     O**********                         87 ' "M.R.A."'
     O**********                         96 'OTHER'
     O**********                        108 'NETWORK'
     O**********************************118 'RECOM'
     O**********************************124 'EOMB'
      *
     O**********                        121 'RECOM'
     O**********                        127 'EOMB'
     O********E*12             HEAD2
     O**********            N92          08 'SERVICE'
     O**********             92          08 '  FILL '
     O**********                         17 'CODE'
     O**********                         47 'DESCRIPTION OF SERVICES'
     O**********                         74 'BILLED'
     O**********                         87 'ADJUSTMENT'
     O**********                         98 'REDUCTIONS'
     O**********                        108 'ADJUSTMNT'
     O**********************************119 'PAYMENT'
     O**********************************124 'CODE'
      *
     O**********                        122 'PAYMENT'
     O**********                        127 'CODE'
     O********E* 1             DET1
     O**********               XXSVDTY   08
     O**********               XXCODE    20
     O**********       92                21 '-'
     O**********       92      XXPMOD    23
     O**********               XXDESC    64
     O**********               XXABIL1   74
     O**********               XXNREDA  110
     O**********               XXOTHRA  100
     O**********               XXAREDA   90
     O*************************XXRPAY1  119
     O*************************XXEOMB   124
      *
     O**********               XXRPAY1  122
     O**********               XXEOMB   127
     O********E*21             TOT1
     O**********                         18 'TOTAL:'
     O**********                         50 'AMOUNT BILLED .....:'
     O**********               TTABIL1   74
     O********E* 1             TOT1
     O**********                         50 '"MRA" ADJUSTMENT ......:'
     O**********               TTAREDA   90
     O********E* 1             TOT1
     O**********                         37 'OTHER REDUCTIONS'
     O**********                         50 '...........:'
     O**********               XXTOTHA  100
     O********E* 1             TOT1
     O**********                         37 'NETWORK ADJUSTME'
     O**********                         50 'NT .........:'
     O**********               XXTNR A  110
     O********E* 1             TOT1
     O**********                         37 'RECOMMENDED PAYM'
     O**********                         50 'ENT ........:'
     O*************************XXTPAY1  119
     O**********               XXTPAY1  122
     O********E* 1             TOT1
     O**********                        122 '_______________'
     O********E* 1             TOT1ST
     O**********               XXSTM     95
     O***
     O*** CHANGED EXCPT TOT1 TO TOT1B NOTHING ELSE
     O***
     O********E* 1             TOT1B
     O**********                         44 'IF YOU HAVE ANY QUESTION'
     O**********                         67 'S REGARDING THIS REVIEW'
     O**********                         90 ' PLEASE CALL OR MAIL A '
     O**********                         97 'COPY OF'
     O********E* 2             TOT1B
     O**********                         44 'YOUR BILL AND THIS REVIE'
     O**********                         46 'W.'
     O********E* 1             TOT1B
     O**********               XXREM4    60
     O********E* 2             TOT1B
     O**********               RPRMDS    50
     O********E* 1             TABL1
     O**********                         31 '¦'
     O**********               XXLIN    124
     O**********                        125 '¦'
     O********E* 1             TABL2
     O**********                         31 '¦'
     O**********                         45 '* EOMB CODE'
     O**********                         57 'DESCRIPTION'
     O**********                        125 '¦'
     O********E* 1             TABL3
     O**********                         31 '¦'
     O**********               XXCD      40
     O**********               XXEBDE   120
     O**********                        125 '¦'
     O********E*11             TABL4
     O**********                          9 'EOMB:'
     O**********               XXEOB#Z   18
     O**********                         19 '-'
     O**********               UPSTAF    22
     O********E* 125           TABL5
     O********E* 1             TABL5
     O**********                         31 '¦'
     O**********                         45 '* EOMB CODE'
     O**********                         57 'DESCRIPTION'
     O**********                         80 '(continued)'
     O**********                        125 '¦'
     O*
     O********E*  01           CNT
     O**********                                  TOP MARGIN = 0
     O**********               ESC
     O**********                            '&l0E'
     O********E*00             CNT
     O**********               ESC
     O**********                            ' '
     O**********               ESC
     O**********                            '&l4H'
     O********E*00             CNTHD
     O**********                                  TOP OF PAGE
     O**********               ESC
     O**********                            '*p0x0Y'
     O********E*0124           CNTHD
     O**********                         80 'EXPLANATION OF MEDICAL'
     O**********                         90 'BENEFITS'
     O********E* 1             CNTHD
     O**********                          9 'PATIENT :'
     O**********               XXPNAM    51
     O**********               XCNAME    90
     O**********                        105 'PROVIDER ID:'
     O**********               XXPID Z  114
     O**********               XXPIDMZ  118
     O********E* 1             CNTHD
     O**********                          9 'SOC SEC :'
     O**********               X2SS#     20
     O**********               XCADR1    90
     O**********                        105 'PROVIDER   :'
     O**********               XXPRNM   137
     O********E* 1             CNTHD
     O**********                          9 'DOA     :'
     O**********               X2ACDT    19 '  /  /  '
     O**********       94      XCADR2    90
     O**********      N94      XCCSZ     90
     O**********               XXPADD   137
     O********E* 1             CNTHD
     O**********                          9 'CASE #  :'
     O**********               XF#       31
     O**********       94      XCCSZ     90
     O**********      N94      XCPHN#    74
     O**********               XPCSZ    137
     O********E* 1             CNTHD
     O**********                          9 'SRV DTS :'
     O**********               XXFRD Y   19
     O**********                         21 '-'
     O**********               XXTOD Y   30
     O**********       94      XCPHN#    74
     O**********************************105 'CLAIMS ADJUSTER :'
     O**********                        105 'MEDICAL EXAMINER:'
     O********                 XXUSNM   137
     O**********               XXMEDX   137
     O**********               UPTITL   122
     O********E*1              CNTHD
     O**********                         09 'DIAG CD :'
     O**********       67      EODIA1    16
     O**********       68      EODIA2    26
     O**********       69      EODIA3    36
     O**********       70      EODIA4    46
     O**********                        105 'EMPLR#:'
     O**********               EMEMP#Z  114
     O**********                        120 'DEPT:'
     O**********                        100 'EMPLR#:'
     O**********               EMEMP#Z  110
     O**********                        120 'LOCATION:'
     O**********               CMCTG1   131
     O********E*1              CNTHD1
     O**********                          9 'EOMB:'
     O**********               XXEOB#Z   18
     O**********                         19 '-'
     O**********               UPSTAF    22
     O********E*2              CNTHD1
     O**********                         08 'DATE OF'
     O**********         N92N91          20 'PROCEDURE'
     O**********         N92 91          20 ' SERVICE '
     O**********          92N91          20 '    NDC  '
     O**********                         88 'AMOUNT'
     O**********                        124 'EOMB'
     O********E*12             CNTHD1
     O**********            N92          08 'SERVICE'
     O**********             92          08 '  FILL '
     O**********                         20 '  CODE   '
     O**********                         47 'DESCRIPTION OF SERVICES'
     O**********                         89 'CHARGED'
     O**********                        103 'DISALLOWED*'
     O**********                        117 'ALLOWANCE'
     O**********                        124 'CODE'
     O**********
     O********E*1              CNTHD2
     O**********                          9 'EOMB:'
     O**********               XXEOB#Z   18
     O**********                         19 '-'
     O**********               UPSTAF    22
     O********E*2              CNTHD2
     O**********
     O********E*1              CNTHD2
     O**********                         08 'DATE OF'
     O**********         N92N91          20 'PROCEDURE'
     O**********         N92 91          20 'SERVICE  '
     O**********          92N91          20 '   NDC   '
     O**********                         74 'AMOUNT'
     O**********                         87 ' "M.R.A."'
     O**********                         96 'OTHER'
     O**********                        124 'EOMB'
     O**********                        108 'NETWORK'
     O**********                        118 'RECOM'
     O********E*12             CNTHD2
     O**********            N92          08 'SERVICE'
     O**********             92          08 '  FILL '
     O**********                         17 'CODE'
     O**********                         47 'DESCRIPTION OF SERVICES'
     O**********                         74 'BILLED'
     O**********                         87 'ADJUSTMENT'
     O**********                         98 'REDUCTIONS'
     O**********                        124 'CODE'
     O**********                        108 'REDUCTION'
     O**********                        108 'ADJUSTMNT'
     O**********                        119 'PAYMENT'
     O*
     O*
     O*CHECKS  E 0225           HEADA
     O*****CHECKS    E   N17      HEADA       0  2 25
     OCHECKS    E   N17N71   HEADA       0  2 25
     OCHECKS    E   N17 71   HEADA       0  2 32
     O*******                            68 'EXPLANATION OF MEDICAL'
     O*******                            77 'BENEFITS'
     O*******                            65 'EXPLANATION OF BILL'
     O*******                            72 'REVIEW'
     O               53                          65 'EXPLANATION OF BILL'
     O               53                          72 'REVIEW'
     O              N53                          68 'EXPLANATION OF MEDICAL'
     O              N53                          77 'BENEFITS'
     OCHECKS    E    17      HEADA       2  1  3
     O*******                            68 'EXPLANATION OF MEDICAL'
     O*******                            77 'BENEFITS'
     O*******                            65 'EXPLANATION OF BILL'
     O*******                            72 'REVIEW'
     O               53                          65 'EXPLANATION OF BILL'
     O               53                          72 'REVIEW'
     O              N53                          68 'EXPLANATION OF MEDICAL'
     O              N53                          77 'BENEFITS'
     O*****                              89 'Page:'
     O*****                    PAGE     107
     O          E            HEADA          1
     O                                           14 'Employer:'
     O**********               EMNAME    56
     O                       XXENAM              56
     O*******                            90 'EOMB#:'
     O               53                          90 'EOBR#:'
     O              N53                          90 'EOMB#:'
     O                       EOEOB#        Z    107
     O          E            HEADA          1
     O**********                         13 'Patient:'
     O********************************** 15 'Claimaint:'
     O                                           14 'Claimant:'
     O                       XXNAM1              56
     O               79                          93 'Review #:'
     O               79      XXTRID            +001
     O          E            CNT              01
     O*                                           TOP MARGIN = 0
     O                       ESC
     O                                              '&l0E'
     O          E            CNT         0  0
     O                       ESC
     O                                              ' '
     O                       ESC
     O                                              '&l4H'
     O          E            CNTHD       0  0
     O*                                           TOP OF PAGE
     O                       ESC
     O                                              '*p0x0Y'
HEADEO          E            HEADE       2  1  3
     O******                             68 'EXPLANATION OF MEDICAL'
     O******                             77 'BENEFITS'
     O               53                          65 'EXPLANATION OF BILL'
     O               53                          72 'REVIEW'
     O              N53                          68 'EXPLANATION OF MEDICAL'
     O              N53                          77 'BENEFITS'
     O          E            HEADE          1
     O                                           14 'Employer:'
     O**********               EMNAME    56
     O                       XXENAM              56
     O******                             90 'EOMB#:'
     O               53                          90 'EOBR#:'
     O              N53                          90 'EOMB#:'
     O                       EOEOB#        Z    107
     O          E            HEADE          1
     O**********                         13 'Patient:'
     O********************************** 15 'Claimaint:'
     O                                           14 'Claimant:'
     O                       XXNAM1              56
     O               79                          93 'Review #:'
     O               79      XXTRID            +001
HEADBO          E            HEADB          1
     O                                           12 'Claim#:'
     O                       XXCL#               37
     O**********************   EOSUB#    21
     O**********************             22 '-'
     O**********************   EOFND     25
     O**********************             26 '-'
     O**********************   EOFYR Z   29
     O**********************             30 '-'
     O**********************   EOCASE    37
     O**********                         45 '('
     O**********               CMCLAD    48
     O**********                         49 ')'
     O                                           43 'SS#:'
     O                       XXSS#1              47
     O                                           48 '-'
     O                       XXSS#2              50
     O                                           51 '-'
     O                       XXSS#3              55
     O                                           61 'DOI:'
     O                       XXDOI               72 '  /  /    '
     O* ADD INDICATOR 67 TO CONDITION DISPLAY
     O              N67      CXHEX               99
     O              N67                         100 ':'
     O              N67      XXMUSR             116
     O          E            HEADB          1
     O**********                         43 'SS#:'
     O**********               XXSS#1    47
     O**********                         48 '-'
     O**********               XXSS#2    50
     O**********                         51 '-'
     O**********               XXSS#3    55
     O**********                         61 'DOI:'
     O**********               XXDOI     72 '  /  /    '
     O**********                         19 'Patient Acct#:'
     O***********                        15 'Pat Acct#:'
     O              N37                          15 'Pat Acct#:'
     O**********               XXACCT    31
     O***********              XXACCT    46
     O              N37      XXACCT              46
     O               37                          15 'Pharmacy#:'
     O               37      XXPHAR              32
     O               35                          55 'Billing NPI#:'
     O               35      XXBNPI              70
     O**********                         46 'Dates of Svc:'
     O**********               ZZFRDT    57 '  /  /    '
     O**********                         59 '-'
     O**********               ZZTODT    70 '  /  /    '
     O                                           91 'Review date:'
     O                       XXTRDT             102 '  /  /    '
     O********E* 1             HEADB
     O**********                         19 'Patient Acct#:'
     O**********               XXACCT    36
     O**********                         95 'Print date:'
     O**********               DSDATE   107 '  /  /    '
     O********E* 1             HEADB
     O**********                         17 'Federal ID#:'
     O**********               EOFEIN    30
     O**********                         32 '-'
     O**********               EOMOD     36
     O**********                         47 'License#:'
     O**********               PDFLIC    57
     O**********               XXLIC#    63
     O*
     O*
     O          E            HEADB          1
     O**********                         18 'Provider ID#:'
     O                                           14 'Prov ID#:'
     O                       EOFEIN              25
     O                                           27 '-'
     O                       EOMOD               31
     O                                           47 'License#:'
     O                       XXLIC#              63
     O                                           90 'Bill Recvd:'
     O                       XXCRDT             102 '  /  /    '
     O          E            HEADB          1
     O                                           18 'Dates of Svc:'
     O                       ZZFRDT              29 '  /  /    '
     O                                           31 '-'
     O                       ZZTODT              42 '  /  /    '
     O                                           90 'Print date:'
     O                       DSDATE             102 '  /  /    '
     O          E            HEADB          1
     O                                           90 'Recon date:'
     O                       XXRCDT             102 '  /  /    '
     O          E            HEADB          1
     O                                           90 'Check date:'
     O                       X1DATE             102 '  /  /    '
     O                                          109 'Chk#:'
     O                       RPCHK#        Z    116
     O          E            HEADB          1
     O                       EOPNAM              45
     O                                           45 '            '
     O**********               PDNAME    54
     O***********              XXANME    73
     O                       XXANME              74
     O**********               EODIA1    89
     O                                           89 'Diagnosis:'
     O          E            HEADB          1
     O                       EOADR1              45
     O                                           45 '            '
     O**********               PDADR1    54
     O**********               XXAAD1    73
     O                       XXAAD1              74
     O**********               EODIA2    89
     O                       XXNUM1              84
     O********************   XXDS1               91
     O                       XXDS1               94
     O                       DESC1              125
     O          E            HEADB          1
     O               32      EOADR2              45
     O               32                          45 '            '
     O**********       32      PDADR2    54
     O              N32      XXCSZ1              45
     O              N32                          45 '            '
     O**********       31      XXAAD2    73
     O**********      N31      XXCSZ2    75
     O               31      XXAAD2              74
     O              N31      XXCSZ2              74
     O**********               EODIA3    89
     O                       XXNUM2              84
     O********************** XXDS2               91
     O                       XXDS2               94
     O                       DESC2              125
     O          E            HEADB          1
     O               32      XXCSZ1              45
     O               32                          45 '            '
     O**********      N31                49 '('
     O**********      N31      XXARA     52
     O**********      N31                53 ')'
     O**********      N31      XXPHN1    57
     O**********      N31                58 '-'
     O**********      N31      XXPHN2    62
     O**********       31      XXCSZ2    75
     O              N31                          35 '('
     O              N31      XXARA               38
     O              N31                          39 ')'
     O              N31      XXPHN1              43
     O              N31                          44 '-'
     O              N31      XXPHN2              48
     O              N31 72                       58 'Fax: '
     O              N31 72                       59 '('
     O              N31 72   XXFARA              62
     O              N31 72                       63 ')'
     O              N31 72   XXFPH1              67
     O              N31 72                       68 '-'
     O              N31 72   XXFPH2              72
     O               31      XXCSZ2              74
     O**********               EODIA4    89
     O                       XXNUM3              84
     O********************   XXDS3               91
     O                       XXDS3               94
     O                       DESC3              125
     O*****   E  2             HEADB
     O          E            HEADB          1
     O**********       31                49 '('
     O**********       31      XXARA     52
     O**********       31                53 ')'
     O**********       31      XXPHN1    57
     O**********       31                58 '-'
     O**********       31      XXPHN2    61
     O**********       31      XXPHN2    62
     O               31                          35 '('
     O               31      XXARA               38
     O               31                          39 ')'
     O               31      XXPHN1              43
     O               31                          44 '-'
     O               31      XXPHN2              48
     O               31 72                       58 'Fax: '
     O               31 72                       59 '('
     O               31 72   XXFARA              62
     O               31 72                       63 ')'
     O               31 72   XXFPH1              67
     O               31 72                       68 '-'
     O               31 72   XXFPH2              72
     O                       XXNUM4              84
     O*********************  XXDS4               91
     O                       XXDS4               94
     O                       DESC4              125
     O*----------------------------------------------------------------
     O          E    53      HEADB          1
     O                                           58 'Division Insured Insurer'
     O                                           66 'Number:'
     O                       XXDIV1              72
HEADCO******* E  1             HEADC
HEADCO          E            HEADC       1  1
     O                                           29 '------------------------'
     O                                           53 '------------------------'
     O                                           77 '------------------------'
     O                                          101 '------------------------'
     O                                          120 '-------------------'
     O          E            HEADC          1
     O                                           12 'Service'
     O                                           34 'Modifiers   POS'
     O                                           44 'Diag Cd'
     O                                           54 'Billed'
     O                                          118 'Payment'
     O                       CXHMRA              69
     O                       CXHOT1              82
     O                       CXHOT2              95
     O                       CXHNET             108
     O          E            HEADC
     O                                           29 '------------------------'
     O                                           53 '------------------------'
     O                                           77 '------------------------'
     O                                          101 '------------------------'
     O                                          120 '-------------------'
     O********E*1              DETL11
     O          E            DETL15      1
     O                                           21 'Date of Service:'
     O                       XXFDAT              32 '  /  /    '
     O**********       40                34 '-'
     O**********       40      XXTDAT    45 '  /  /    '
     O                                           34 '-'
     O                       XXTDAT              45 '  /  /    '
     O          E            DE15FL      1
     O                                           10 'LINE#'
     O                       XLINE         Z     15
     O                                           37 'Date of Service:'
     O                       XXFDAT              48 '  /  /    '
     O                                           50 '-'
     O                       XXTDAT              61 '  /  /    '
     O          E            DETL12      1  1
     O                       XXCPT4              16
     O                       XXMOD1              20
     O                       XXMOD2              23
     O                       XXMOD3              26
     O                       XXMOD4              29
     O                       XXPOS               34
     O**********               XXICD9    43
     O                       XXDRF1              37
     O                       XXDRF2              39
     O                       XXDRF3              41
     O                       XXDRF4              43
     O                       CACHRG        J     56
     O                       CAMRA         J     69
     O                       CAOTHR        J     82
     O                       CAOTH2        J     95
     O                       CANDIS        J    108
     O                       CAPAID        J    120
     O          E            DETL13         1
     O                                           27 'Description:'
     O                       XXPRDS              53
     O**************** 34                88 'Billed Code:'
     O**************** 34      XXBILD   100
     O               36                          61 'Rx tax:'
     O               36      X1TAX         J     75
     O                                           82 'Units:'
     O                       X1UNTS        J     90
     O               34                         104 'Billed Code:'
     O               34      XXBILD             116
     O          E            DETL14      0  0
     O*******          42                25 'EOB Codes:'
     O               42N53                       25 'EOB Codes:'
     O               42 53                       26 'EOBR Codes:'
     O*** BEG CHG *** 08/22/22 **************************************
     O****           42      X1EOB               29
     O****           43      X2EOB               33
     O****           44      X3EOB               37
     O****           45      X4EOB               41
     O               42N53   X1EOBR              32
     O               42 53   X1EOB               31
     O*** Beg chg *** 07/18/23 **************************************
     O****           43N53   X2EOBR              34
     O****           43 53   X2EOB               33
     O****           44N53   X3EOBR              38
     O****           44 53   X3EOB               37
     O****           45N53   X4EOBR              42
     O****           45 53   X4EOB               41
     O****           49N53 88X5EOBR              46
     O****           49 53 88X5EOB               45
     O****           52N53 88X6EOBR              50
     O****           52 53 88X6EOB               49
     O****           54N53 88X7EOBR              54
     O****           54 53 88X7EOB               53
     O****           55N53 88X8EOBR              58
     O****           55 53 88X8EOB               57
     O****           57N53 88X9EOBR              62
     O****           57 53 88X9EOB               61
     O****           58N53 88X10EOBR             66
     O****           58 53 88X10EOB              65
     O****           59N53 88X11EOBR             70
     O****           59 53 88X11EOB              69
     O****           68N53 88X12EOBR             74
     O****           68 53 88X12EOB              73
     O               43N53   X2EOBR              39
     O               43 53   X2EOB               38
     O               44N53   X3EOBR              46
     O               44 53   X3EOB               45
     O               45N53   X4EOBR              53
     O               45 53   X4EOB               52
     O               49N53 88X5EOBR              60
     O               49 53 88X5EOB               59
     O               52N53 88X6EOBR              67
     O               52 53 88X6EOB               66
     O               54N53 88X7EOBR              74
     O               54 53 88X7EOB               73
     O               55N53 88X8EOBR              81
     O               55 53 88X8EOB               80
     O               57N53 88X9EOBR              88
     O               57 53 88X9EOB               87
     O               58N53 88X10EOBR             95
     O               58 53 88X10EOB              94
     O               59N53 88X11EOBR            102
     O               59 53 88X11EOB             101
     O               68N53 88X12EOBR            109
     O               68 53 88X12EOB             111
     O*** End chg *** 07/18/23 **************************************
     O*** END CHG *** 08/22/22 **************************************
     O               46                          47 'Note:'
     O          E            DETL2
     O*** Beg Chg *** 08/22/22 ***********************************************
     O********************   XNOTE              120
     O              N88      XNOTE              120
     O*** End Chg *** 08/22/22 ***********************************************
     O          E            DETL3       2  1
     O                                           38 'Billed Charges:'
     O                       EOCHRG        J     56
     O          E            DETL3          1
     O                                           34 'Reductions:'
     O                       XXMRA         J     69
     O                       EOOTHR        J     82
     O                       XXOTH2        J     95
     O                       EONDIS        J    108
     O                                          120 '__________'
     O          E            DETL3          1
     O                                           37 'Total Payment:'
     O                       XXPAID        J    120
     O          E    33      DETL3          0
     O                                           39 'Previously Paid:'
     O********E  0     33      DETL3
     O                                          120 '__________'
     O          E    33      DETL3       0
     O                       X1PAID        J    120
     O          E    33      DETL3       1  1
     O                                           36 'This Payment:'
     O                       X2PAID        J    120
     O          E    36      DETL3          1
     O                                           47 'Payment Includes Louisia'
     O                                           71 'na Pharmacy Provider Par'
     O                                           88 'ish sales tax of:'
     O                                          106 '$'
     O                       XXTAX         J    120
     O          E    47      DETL3       1  1
     O                       XXNWM              185
     O          E    47 80   DETL3          1
     O                       XXNW2              145
     O          E    47 81   DETL3          1
     O                       XXNW3              145
     O          E            DETL4       1  1
     O*********                          27 'EOB Reason Codes:'
     O**** BEG CHG **** 08/22/22 **************************************
     O****                                       27 'Additional EOBR Reason'
     O               53                          27 'Additional EOBR Reason'
     O              N53                          27 'Additional EOB Reason'
     O**** END CHG **** 08/22/22 **************************************
     O                                           33 ' Codes'
     O          E            DTEOB       1  1
     O**** BEG CHG **** 08/22/22 **************************************
     O****                                       29 'EOBR Codes/Descriptions:'
     O               53                          29 'EOBR Codes/Descriptions:'
     O              N53                          29 'EOB Codes/Descriptions:'
     O**** END CHG **** 08/22/22 **************************************
     O          E            DTEON          1
     O                       YYEOB               13
     O                       XNOTE               96
     O*
     O          E            DTEOD          1
     O                       XNOTE               96
     O*
     O          E            DTNOT       1  1
     O**** Beg Chg *** 08/22/22 **************************************
     O****                                       19 'Carrier Notes:'
     O               53                          19 'Carrier Notes:'
     O              N53                          19 'Notes:'
     O**** End Chg *** 08/22/22 **************************************
     O*
     O          E            DTLIN          1
     O                                           10 'LINE#'
     O                       XLINE         Z     15
     O                                           17 '-'
     O                                           22 'NOTE'
     O                       XNO1          Z     25
     O                       X1NOTE              96
     O*
     O          E            DTNO1          1
     O                       X1NOTE              96
     O*
     O          E            DTEOH       1  1
     O**** Beg Chg *** 08/22/22 **************************************
     O****                                       27 'General Carrier Notes:'
     O               53                          27 'General Carrier Notes:'
     O              N53                          27 'General Notes:'
     O**** End Chg *** 08/22/22 **************************************
     O          E            DTLIH          1
     O                                            9 'NOTE'
     O                       XNO1          Z     13
     O                       X1NOTE              96
     O*
     O          E            DTLIH1         1
     O                       X1NOTE              96
     O*
     O          E            BRKLIN
     O*
     O                                           28 '-----------------------'
     O                                           52 '------------------------'
     O                                           76 '------------------------'
     O                                          100 '------------------------'
     O                                          120 '--------------------'
     O          E            DETL5
     O                       YYEOB               13
     O                       YYDESC              84
     O          E            DETL6       1  1
     O*******                            32 'Additional EOMB Notes:'
     O**** BEG CHG *** 08/22/22 **************************************
     O****                                       27 'Additional EOBR Notes:'
     O               53                          27 'Additional EOBR Notes:'
     O              N53                          27 'Additional EOB Notes:'
     O**** END CHG *** 08/22/22 **************************************
     O          E            DETL7
     O                       X1NOTE              80
     O          E            SPACE          1
     O                                           10 '  '
     O*
     O          E            INSURED        2
     O                                           25 'Insurer Information:'
     O*
     O          E            INSURED        1
     O                       XXANME              45
     O*
     O          E            INSURED        1
     O                       XXAAD1              45
     O*
     O          E    31      INSURED        1
     O                       XXAAD2              45
     O*
     O          E            INSURED        1
     O                       XXCSZ2              45
     O*
     O          E   N79      INSURED        1
     O              N79                           6 '('
     O              N79      XXARA                9
     O              N79                          10 ')'
     O              N79      XXPHN1              14
     O              N79                          15 '-'
     O              N79      XXPHN2              19
     O*
     O          E            INSURED        1
     O                                           29 'Division Insured Insurer'
     O                                           37 'Number:'
     O                       XXDIV1              43
     O*
     O          E            INSURED        1
     O                       DASH               120
     O********E*               DETL8
     O**********               STNOTE    75
     O*
     O          E            DETL8       1
 |    *** Change Font to COURIER 12 BOLD
     O*****
     O*****   E 0      74      DETL8
 |   O*****                    ESC
 |   O*****                                 '(0U'
 |   O*****                    ESC
 |   O*****                                 '(s0p12h10v0s3b3T'
     O*****   E 0              DETL8
 |    *** Change Font to Courier 10 BOLD
     O          E    74      DETL8       0
 |   O                       ESC
 |   O                                              '(0U'
 |   O                       ESC
 |   O                                              '(s0p14h0s3b4099T'
     O          E            DETL8       0
     O                       STCOMM             145
 |    *   Change back to NORMAL FONT MED/COR. 8.5PT/16.66
     O          E    74      DETL8       0
 |   O                       ESC
 |   O                                              '(0U'
 |   O                       ESC
     O                                              '(s0p16.66hsb0T'
     O*
     O          E            DETL9       1
     O**********               GPNOTE    75
     O                       GRCOMM             145
     O********E*21             DETL10
     O**********               XXANME    45
     O**********       40                70 'Admission date:'
     O**********       40      XXADDT    81 '  /  /    '
     O********E* 1             DETL10
     O**********               XXAAD1    45
     O**********       40                64 'Coverage:'
     O**********       40      XXSCPF    75 '  /  /    '
     O**********       40                77 '-'
     O**********       40      XXSCPT    88 '  /  /    '
     O**********
     O********E* 1             DETL10
     O**********       31      XXAAD2    45
     O**********       40                71 'Submission date:'
     O**********       40      XXSBDT    82 '  /  /    '
     O********E* 1             DETL10
     O**********               XXCSZ2    45
     O**********       40                71 'Type of Service:'
     O**********       40      XXICN     74
     O**********       40      XXICDS   106
     O********E*               DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O**********
     O**********       40                68 'Type of Bill:'
     O**********       40      XXBLTY    74
     O**********       40      XXBDSC   106
     O*----------------------------------------------------------------
     O* NON-HOSPITAL AND NO SECOND ADDRESS
     O**********
     O**********
     O********E* 1    N40N31   DETL10
     O**********               XXCSZ2    45
     O********E*      N40N31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O*----------------------------------------------------------------
     O**NON-HOSPITAL AND SECOND ADDRESS
     O**********
     O********E* 1    N40 31   DETL10
     O**********               XXAAD2    45
     O********E* 1    N40 31   DETL10
     O**********               XXCSZ2    45
     O********E*      N40 31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O*----------------------------------------------------------------
     O**HOSPITAL BILL AND NO SECOND ADDRESS
     O**********
     O********E* 1     40N31   DETL10
     O**********               XXCSZ2    45
     O**********                         71 'Submission date:'
     O**********               XXSBDT    82 '  /  /    '
     O********E* 1     40N31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O**********                         71 'Type of Service:'
     O**********               XXICN     74
     O**********               XXICDS   106
     O********E*       40N31   DETL10
     O**********                         68 'Type of Bill:'
     O**********               XXBLTY    74
     O**********               XXBDSC   106
     O*----------------------------------------------------------------
     O**HOSPITAL AND 2ND ADDRESS
     O**********
     O********E* 1     40 31   DETL10
     O**********               XXAAD2    45
     O**********                         71 'Submission date:'
     O**********               XXSBDT    82 '  /  /    '
     O********E* 1     40 31   DETL10
     O**********               XXCSZ2    45
     O**********                         71 'Type of Service:'
     O**********               XXICN     74
     O**********               XXICDS   106
     O********E*       40 31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O**********                         68 'Type of Bill:'
     O**********               XXBLTY    74
     O**********               XXBDSC   106
     O* NO SECOND ADDRESS
     O**********
     O********E* 1       N31   DETL10
     O**********               XXCSZ2    45
     O********E*         N31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O*----------------------------------------------------------------
     O* SECOND ADDRESS
     O**********
     O********E* 1        31   DETL10
     O**********               XXAAD2    45
     O********E* 1        31   DETL10
     O**********               XXCSZ2    45
     O********E*          31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O**********
     O*----------------------------------------------------------------
     O* HOSPITAL SPECIFIC INFORMATION
     O*
     O          E       31   DETL16      1  1
     O                                           20 'Admission date:'
     O                       XXADDT              31 '  /  /    '
     O                                           80 'Proc codes:'
     O          E            DETL16         1
     O                                           14 'Coverage:'
     O                       XXSCPF              25 '  /  /    '
     O                                           27 '-'
     O                       XXSCPT              38 '  /  /    '
     O                       EUPRC1              80
     O                       XXPRD1             117
     O          E            DETL16         1
     O                                           21 'Submission date:'
     O                       XXSBDT              32 '  /  /    '
     O                       EUPRC2              80
     O                       XXPRD2             117
     O          E            DETL16         1
     O                                           21 'Type of Service:'
     O                       XXICN               24
     O                       XXICDS              56
     O                       EUPRC3              80
     O                       XXPRD3             117
     O          E            DETL16
     O                                           18 'Type of Bill:'
     O                       XXBLTY              24
     O                       XXBDSC              56
     O                       EUPRC4              80
     O                       XXPRD4             117
      *_________________________________________________________________
      *
      *  Copybook for file CHECKS except name DET2
     O/COPY OCHKDET2
     O          E            ADINH1         1  3
     O******                             68 'EXPLANATION OF MEDICAL'
     O******                             77 'BENEFITS'
     O******                             65 'EXPLANATION OF BILL'
     O******                             72 'REVIEW'
     O                                           68 'EXPLANATION OF MEDICAL'
     O                                           77 'BENEFITS'
     O*
     O********E* 2             ADINH1
     O          E   N79      ADINH1         2
     O                                           72 'ADDITIONAL INFORMATION'
     O******                             90 'EOMB#:'
     O******                             90 'EOBR#:'
     O                                           90 'EOMB#:'
     O                       EOEOB#        Z    107
     O          E    79      ADINH1         1
     O                                           72 'ADDITIONAL INFORMATION'
     O******                             90 'EOMB#:'
     O******                             90 'EOBR#:'
     O                                           90 'EOMB#:'
     O                       EOEOB#        Z    107
     O          E    79      ADINH1         1
     O                                           93 'Review #:'
     O                       XXTRID            +001
     O*
     O*----------------------------------------------------------------
     O          E            ADIND1         1
     O                                           13 'Carrier:'
     O                       XXNAM1              66
     O                                           96 'Provider Bill Date:'
     O                       XXCSDT             107 '  /  /    '
     O*
     O          E            ADIND1         1
     O                                           13 'Address:'
     O                       XXFAD1              66
     O*
     O* NO SECOND ADDRESS
     O*
     O          E      N31   ADIND1         2
     O                       XXCSZ1              66
     O          E      N31   ADIND1         2
     O                                           11 'Phone:'
     O                                           27 '('
     O                       XXAREA              30
     O                                           31 ')'
     O                       XXPHN1              35
     O                                           36 '-'
     O                       XXPHN2              40
     O*
     O* SECOND ADDRESS
     O*
     O          E       31   ADIND1         1
     O                       XXFAD2              66
     O          E       31   ADIND1         2
     O                       XXCSZ1              66
     O          E       31   ADIND1         2
     O                                           11 'Phone:'
     O                                           27 '('
     O                       XXAREA              30
     O                                           31 ')'
     O                       XXPHN1              35
     O                                           36 '-'
     O                       XXPHN2              40
     O*----------------------------------------------------------------
     O          E            ADIND2         2
     O                                           24 'NAICS/Self Insured:'
     O                       XXNAIC              35
     O*----------------------------------------------------------------
     O          E            ADIND3         1
     O                                           21 'Service Company:'
     O                       XXNAM1              66
     O          E            ADIND3         3
     O                                           21 'Service Address:'
     O                       XXFAD1              66
     O*
     O* NO SECOND ADDRESS
     O*
     O          E      N31   ADIND3         2
     O                       XXCSZ1              66
     O*
     O* SECOND ADDRESS
     O*
     O          E       31   ADIND3         1
     O                       XXFAD2              66
     O          E       31   ADIND3         2
     O                       XXCSZ1              66
     O*----------------------------------------------------------------
     O          E            ADIND4         1
     O                                           14 'Employee:'
     O                       XXCLNM              56
     O          E            ADIND4         1
     O                                           13 'Address:'
     O                       XXFAD1              66
     O*
     O* NO SECOND ADDRESS
     O*
     O          E      N31   ADIND4         2
     O                       XXCSZ1              66
     O*
     O* SECOND ADDRESS
     O*
     O          E       31   ADIND4         1
     O                       XXFAD2              66
     O          E       31   ADIND4         2
     O                       XXCSZ1              66
     O*
     O******  E  1 3           ADINH2
     O******                             68 'EXPLANATION OF MEDICAL'
     O******                             77 'BENEFITS'
     O******  E  2    N79      ADINH2
     O******                             72 'ADDITIONAL INFORMATION'
     O******                             90 'EOMB#:'
     O******                   EOEOB#Z  107
     O*
     O          E            ADIND5         1
     O                                           22 'Claimant Address:'
     O          E            ADIND5         1
     O                       XXCLNM              56
     O*
     O          E            ADIND5         1
     O                       XXFAD1              66
     O*
     O* NO SECOND ADDRESS
     O*
     O          E      N82   ADIND5         2
     O                       XXCSZ1              66
     O*
     O* SECOND ADDRESS
     O*
     O          E       82   ADIND5         1
     O                       XXFAD2              66
     O          E       82   ADIND5         2
     O                       XXCSZ1              66
     O*
      *_________________________________________________________________
      *
      *  Copybook for file FILECOPY except name CHECK
     O/COPY OFILCHECK
      *_________________________________________________________________
      *
      *  Copybook for file FILECOPY except name DET2
     O/COPY OFILDET2
      *
      *_________________________________________________________________
     O**********FILECOPYE 0224           HEAD
      *
     O**********                         70 'EXPLANATION OF MEDICAL '
     O**********                            'BENEFITS'
     O**********
     O********E* 1             HEAD
     O**********                         12 'PATIENT:'
     O**********               XXPNAM    53
     O**********               XCNAME    80
     O**********                         95 'PROVIDER ID:'
     O**********               XXPID Z  104
     O**********               XXPIDMZ  108
     O********E* 1             HEAD
     O**********                         12 'SOC SEC:'
     O**********               X2SS#     22
     O**********               XCADR1    80
     O**********                         95 'PROVIDER   :'
     O**********               XXPRNM   127
     O********E* 1             HEAD
     O**********                         12 'DOA    :'
     O**********               X2ACDT    21 '  /  /  '
     O**********       94      XCADR2    80
     O**********      N94      XCCSZ     80
     O**********               XXPADD   127
     O********E* 1             HEAD
     O**********                         12 'CASE # :'
     O**********               XF#       33
     O**********       94      XCCSZ     80
     O**********      N94      XCPHN#    64
     O**********               XPCSZ    127
     O********E* 1             HEAD
     O**********                         12 'SRV DTS:'
     O**********               XXFRD Y   21
     O**********                         23 '-'
     O**********               XXTOD Y   32
     O**********       94      XCPHN#    64
     O**********                         95 'CLM ADJ    :'
     O**********               XXUSNM   127
     O**********               UPTITL   112
     O********E*1              HEAD      70
     O**********                         12
     O**********       67      EODIA1    18
     O**********       68      EODIA2    28
     O**********       69      EODIA3    38
     O**********       70      EODIA4    48
     O**********                         95 'EMPLR#:'
     O**********               EMEMP#Z  104
     O**********                        110 'DEPT:'
     O**********               CMCTG1   121
     O**********                        100 'EMPLR#:'
     O**********               EMEMP#Z  110
     O**********                        120 'LOCATION:'
     O**********               CMCTG1   131
     O********E*1              HEAD
     O**********                         12 'EOMB:'
     O**********               XXEOB#Z   20
     O**********                         21 '-'
     O**********               UPSTAF    24
     O********E*2              HEAD1
     O**********                         12 'DATE OF'
     O**********         N92N91          24 'PROCEDURE'
     O**********         N92 91          24 ' SERVICE '
     O**********          92N91          24 '    NDC  '
     O**********                         88 'AMOUNT'
     O**********                        122 'EOMB'
     O********E*12             HEAD1
     O**********            N92          12 'SERVICE'
     O**********             92          12 '  FILL '
     O**********                         24 '  CODE   '
     O**********                         51 'DESCRIPTION OF SERVICES'
     O**********                         89 'CHARGED'
     O**********                        103 'DISALLOWED*'
     O**********                        117 'ALLOWANCE'
     O**********                        122 'CODE'
     O********E* 1             DET
     O**********               XXSVDTY   12
     O**********               XXCODE    24
     O**********       92                25 '-'
     O**********       92      XXPMOD    27
     O**********               XXDESC    64
     O**********               XXABIL4   89
     O**********               XXAREDM  103
     O**********               XXAALWL  117
     O**********               XXEOMB   122
     O********E*21             TOT
     O**********                         43 'TOTAL:'
     O**********                         79 'AMOUNT CHARGED.....:'
     O**********               TTABIL4   89
     O********E* 1             TOT
     O**********                         79 'DISALLOWED.........:'
     O**********               TTAREDM  103
     O********E* 1             TOT
     O**********                         79 'ALLOWANCE..........:'
     O**********               TTAALWM  118 '$'
     O********E* 2             TOT
     O**********                        119 '==============='
     O********E* 1             TOT
     O**********                         44 'IF YOU HAVE ANY QUESTION'
     O**********                         67 'S REGARDING THIS REVIEW'
     O**********                         90 ' PLEASE CALL OR MAIL A '
     O**********                         97 'COPY OF'
     O********E* 2             TOT
     O**********
     O**********                         40 'YOUR BILL AND THIS REVIE'
     O**********                         42 'W.'
     O**********                         65 'PLEASE REFERENCE THE '
     O**********                         88 'CLAIMS ADJUSTER ON ALL '
     O**********                         98 'INQUIRIES.'
     O**********
     O********E*2              HEAD2
     O**********
     O********E*1              HEAD2
     O**********                         12 'DATE OF'
     O**********         N92N91          24 'PROCEDURE'
     O**********         N92 91          24 'SERVICE  '
     O**********          92N91          24 '   NDC   '
     O**********                         74 'AMOUNT'
     O**********                         87 ' "M.R.A."'
     O**********                         96 'OTHER'
     O**********                        108 'NETWORK'
     O**********************************118 'RECOM'
     O**********************************124 'EOMB'
     O**********                        121 'RECOM'
     O**********                        127 'EOMB'
     O********E*12             HEAD2
     O**********            N92          12 'SERVICE'
     O**********             92          12 '  FILL '
     O**********                         21 'CODE'
     O**********                         51 'DESCRIPTION OF SERVICES'
     O**********                         74 'BILLED'
     O**********                         87 'ADJUSTMENT'
     O**********                         98 'REDUCTIONS'
     O**********                        108 'ADJUSTMNT'
     O**********************************119 'PAYMENT'
     O**********************************124 'CODE'
     O**********                        122 'PAYMENT'
     O**********                        127 'CODE'
     O********E* 1             DET1
     O**********               XXSVDTY   12
     O**********               XXCODE    24
     O**********       92                25 '-'
     O**********       92      XXPMOD    27
     O**********               XXDESC    64
     O**********               XXABIL1   74
     O**********               XXNREDA  110
     O**********               XXOTHRA  100
     O**********               XXAREDA   90
     O*************************XXRPAY1  119
     O*************************XXEOMB   124
     O**********               XXRPAY1  122
     O**********               XXEOMB   127
     O********E*21             TOT1
     O**********                         18 'TOTAL:'
     O**********                         50 'AMOUNT BILLED .....:'
     O**********               TTABIL1   74
     O********E* 1             TOT1
     O**********                         50 '"MRA" ADJUSTMENT ......:'
     O**********               TTAREDA   90
     O********E* 1             TOT1
     O**********                         37 'OTHER REDUCTIONS'
     O**********                         50 '...........:'
     O**********               XXTOTHA  100
     O********E* 1             TOT1
     O**********                         37 'NETWORK ADJUSTME'
     O**********                         50 'NT .........:'
     O**********               XXTNR A  110
     O********E* 1             TOT1
     O**********                         37 'RECOMMENDED PAYM'
     O**********                         50 'ENT ........:'
     O*************************XXTPAY1  119
     O**********               XXTPAY1  122
     O********E* 1             TOT1
     O**********                        122 '_______________'
     O********E* 1             TOT1
     O**********                         44 'IF YOU HAVE ANY QUESTION'
     O**********                         67 'S REGARDING THIS REVIEW'
     O**********                         90 ' PLEASE CALL OR MAIL A '
     O**********                         97 'COPY OF'
     O********E* 2             TOT1
     O**********                         44 'YOUR BILL AND THIS REVIE'
     O**********                         46 'W.'
     O********E* 1             TOT1
     O**********               XXREM4    60
     O********E* 2             TOT1
     O**********               RPRMDS    50
     O********E* 1             TABL1
     O**********                         31 '¦'
     O**********               XXLIN    124
     O**********                        125 '¦'
     O********E* 1             TABL2
     O**********                         31 '¦'
     O**********                         45 '* EOMB CODE'
     O**********                         57 'DESCRIPTION'
     O**********                        125 '¦'
     O********E* 1             TABL3
     O**********                         31 '¦'
     O**********               XXCD      40
     O**********               XXEBDE   120
     O**********                        125 '¦'
     O********E*11             TABL4
     O**********                          9 'EOMB:'
     O**********               XXEOB#Z   18
     O**********                         19 '-'
     O**********               UPSTAF    22
     O********E* 1             TABL5
     O**********                         31 '¦'
     O**********                         45 '* EOMB CODE'
     O**********                         57 'DESCRIPTION'
     O**********                         80 '(continued)'
     O**********                        125 '¦'
     O*
     O********E*  01           CNT
     O**********                                  TOP MARGIN = 0
     O**********               ESC
     O**********                            '&l0E'
     O********E*00             CNT
     O**********               ESC
     O**********                            ' '
     O**********               ESC
     O**********                            '&l4H'
     O********E*00             CNTHD
     O**********                                  TOP OF PAGE
     O**********               ESC
     O**********                            '*p0x0Y'
     O******* E   01           CNT
     O********E*0124           CNTHD
     O**********                         80 'EXPLANATION OF MEDICAL'
     O**********                         90 'BENEFITS'
     O********E* 1             CNTHD
     O**********                         12 'PATIENT:'
     O**********               XXPNAM    53
     O**********               XCNAME    80
     O**********                         95 'PROVIDER ID:'
     O**********               XXPID Z  104
     O**********               XXPIDMZ  108
     O********E* 1             CNTHD
     O**********                         12 'SOC SEC:'
     O**********               X2SS#     22
     O**********               XCADR1    80
     O**********                         95 'PROVIDER   :'
     O**********               XXPRNM   127
     O********E* 1             CNTHD
     O**********                         12 'DOA    :'
     O**********               X2ACDT    21 '  /  /  '
     O**********       94      XCADR2    80
     O**********      N94      XCCSZ     80
     O**********               XXPADD   127
     O********E* 1             CNTHD
     O**********                         12 'CASE # :'
     O**********               XF#       33
     O**********       94      XCCSZ     80
     O**********      N94      XCPHN#    64
     O**********               XPCSZ    127
     O********E* 1             CNTHD
     O**********                         12 'SRV DTS:'
     O**********               XXFRD Y   21
     O**********                         23 '-'
     O**********               XXTOD Y   32
     O**********       94      XCPHN#    64
     O**********                         95 'CLAIMS ADJUSTER :'
     O**********               XXUSNM   127
     O**********               UPTITL   112
     O********E*1              CNTHD
     O**********                         12 'DIAG CD:'
     O**********       67      EODIA1    18
     O**********       68      EODIA2    28
     O**********       69      EODIA3    38
     O**********       70      EODIA4    48
     O**********                         95 'EMPLR#:'
     O**********               EMEMP#Z  104
     O**********                        110 'DEPT:'
     O**********               CMCTG1   121
     O**********                        100 'EMPLR#:'
     O**********               EMEMP#Z  110
     O**********                        120 'LOCATION:'
     O**********               CMCTG1   131
     O********E*1              CNTHD1
     O**********                         12 'EOMB:'
     O**********               XXEOB#Z   20
     O**********                         21 '-'
     O**********               UPSTAF    24
     O********E*2              CNTHD1
     O**********                         12 'DATE OF'
     O**********         N92N91          24 'PROCEDURE'
     O**********         N92 91          24 ' SERVICE '
     O**********          92N91          24 '    NDC  '
     O**********                         88 'AMOUNT'
     O**********                        122 'EOMB'
     O********E*12             CNTHD1
     O**********            N92          12 'SERVICE'
     O**********             92          12 '  FILL '
     O**********                         24 '  CODE   '
     O**********                         51 'DESCRIPTION OF SERVICES'
     O**********                         89 'CHARGED'
     O**********                        103 'DISALLOWED*'
     O**********                        117 'ALLOWANCE'
     O**********                        122 'CODE'
     O**********
     O********E*1              CNTHD2
     O**********                         13 'EOMB:'
     O**********               XXEOB#Z   22
     O**********                         23 '-'
     O**********               UPSTAF    26
     O********E*2              CNTHD2
     O**********
     O********E*1              CNTHD2
     O**********                         12 'DATE OF'
     O**********         N92N91          24 'PROCEDURE'
     O**********         N92 91          24 'SERVICE  '
     O**********          92N91          24 '   NDC   '
     O**********                         74 'AMOUNT'
     O**********                         87 ' "M.R.A."'
     O**********                         96 'OTHER'
     O**********                        124 'EOMB'
     O**********                        108 'NETWORK'
     O**********                        118 'RECOM'
     O********E*12             CNTHD2
     O**********            N92          12 'SERVICE'
     O**********             92          12 '  FILL '
     O**********                         21 'CODE'
     O**********                         51 'DESCRIPTION OF SERVICES'
     O**********                         74 'BILLED'
     O**********                         87 'ADJUSTMENT'
     O**********                         98 'REDUCTIONS'
     O**********                        124 'CODE'
     O**********                        108 'REDUCTION'
     O**********                        108 'ADJUSTMNT'
     O**********                        119 'PAYMENT'
     O***FILECOPYE 0224           HEADA
     O******  E  2 6           HEADA
     O***** FILECOPY  E   N17      HEADA       0  2 24
     OFILECOPY  E   N17N71   HEADA       0  2 24
     OFILECOPY  E   N17 71   HEADA       0  2 32
     O                                           68 'EXPLANATION OF MEDICAL'
     O                                           77 'BENEFITS'
     OFILECOPY  E    17      HEADA       2  1  3
     O                                           68 'EXPLANATION OF MEDICAL'
     O                                           77 'BENEFITS'
     O*****                              89 'Page:'
     O*****                    PAGE     107
     O          E            HEADA          1
     O                                           14 'Employer:'
     O**********               EMNAME    56
     O                       XXENAM              56
     O                                           90 'EOMB#:'
     O                       EOEOB#        Z    107
     O          E            HEADA          1
     O**********                         13 'Patient:'
     O********************************** 15 'Claimaint:'
     O                                           14 'Claimant:'
     O                       XXNAM1              56
     O               79                          93 'Review #:'
     O               79      XXTRID            +001
     O          E            CNT              01
     O*                                           TOP MARGIN = 0
     O                       ESC
     O                                              '&l0E'
     O          E            CNT         0  0
     O                       ESC
     O                                              ' '
     O                       ESC
     O                                              '&l4H'
     O          E            CNTHD       0  0
     O*                                           TOP OF PAGE
     O                       ESC
     O                                              '*p0x0Y'
HEADEO          E            HEADE       2  1  3
     O                                           68 'EXPLANATION OF MEDICAL'
     O                                           77 'BENEFITS'
     O          E            HEADE          1
     O                                           14 'Employer:'
     O**********               EMNAME    56
     O                       XXENAM              56
     O                                           90 'EOMB#:'
     O                       EOEOB#        Z    107
     O          E            HEADE          1
     O**********                         13 'Patient:'
     O********************************** 15 'Claimaint:'
     O                                           14 'Claimant:'
     O                       XXNAM1              56
     O               79                          93 'Review #:'
     O               79      XXTRID            +001
HEADBO          E            HEADB          1
     O                                           12 'Claim#:'
     O                       XXCL#               37
     O**********************   EOSUB#    21
     O**********************             22 '-'
     O**********************   EOFND     25
     O**********************             26 '-'
     O**********************   EOFYR Z   29
     O**********************             30 '-'
     O**********************   EOCASE    37
     O**********                         45 '('
     O**********               CMCLAD    48
     O**********                         49 ')'
     O                                           43 'SS#:'
     O                       XXSS#1              47
     O                                           48 '-'
     O                       XXSS#2              50
     O                                           51 '-'
     O                       XXSS#3              55
     O                                           61 'DOI:'
     O                       XXDOI               72 '  /  /    '
     O* ADD INDICATOR 67 TO CONDITION DISPLAY
     O              N67      CXHEX               99
     O              N67                         100 ':'
     O              N67      XXMUSR             116
     O          E            HEADB          1
     O**********                         43 'SS#:'
     O**********               XXSS#1    47
     O**********                         48 '-'
     O**********               XXSS#2    50
     O**********                         51 '-'
     O**********               XXSS#3    55
     O**********                         61 'DOI:'
     O**********               XXDOI     72 '  /  /    '
     O**********                         19 'Patient Acct#:'
     O***********                        15 'Pat Acct#:'
     O**********               XXACCT    31
     O***********              XXACCT    46
     O              N37                          15 'Pat Acct#:'
     O              N37      XXACCT              46
     O               37                          15 'Pharmacy#:'
     O               37      XXPHAR              46
     O**********                         46 'Dates of Svc:'
     O**********               ZZFRDT    57 '  /  /    '
     O**********                         59 '-'
     O**********               ZZTODT    70 '  /  /    '
     O                                           91 'Review date:'
     O                       XXTRDT             102 '  /  /    '
     O********E* 1             HEADB
     O**********                         19 'Patient Acct#:'
     O**********               XXACCT    36
     O**********                         95 'Print date:'
     O**********               DSDATE   107 '  /  /    '
     O********E* 1             HEADB
     O**********                         17 'Federal ID#:'
     O**********               EOFEIN    30
     O**********                         32 '-'
     O**********               EOMOD     36
     O**********                         47 'License#:'
     O**********               PDFLIC    57
     O**********               XXLIC#    63
     O*
     O*
     O          E            HEADB          1
     O**********                         18 'Provider ID#:'
     O                                           14 'Prov ID#:'
     O                       EOFEIN              25
     O                                           27 '-'
     O                       EOMOD               31
     O                                           47 'License#:'
     O                       XXLIC#              63
     O                                           90 'Bill Recvd:'
     O                       XXCRDT             102 '  /  /    '
     O          E            HEADB          1
     O                                           18 'Dates of Svc:'
     O                       ZZFRDT              29 '  /  /    '
     O                                           31 '-'
     O                       ZZTODT              42 '  /  /    '
     O                                           90 'Print date:'
     O                       DSDATE             102 '  /  /    '
     O          E            HEADB          1
     O                                           90 'Recon date:'
     O                       XXRCDT             102 '  /  /    '
     O          E            HEADB          1
     O                                           90 'Check date:'
     O                       X1DATE             102 '  /  /    '
     O                                          109 'Chk#:'
     O                       RPCHK#        Z    116
     O          E            HEADB          1
     O                       EOPNAM              45
     O                                           45 '            '
     O**********               PDNAME    54
     O***********              XXANME    73
     O                       XXANME              74
     O**********               EODIA1    89
     O                                           89 'Diagnosis:'
     O          E            HEADB          1
     O                       EOADR1              45
     O                                           45 '            '
     O**********               PDADR1    54
     O**********               XXAAD1    73
     O                       XXAAD1              74
     O**********               EODIA2    89
     O                       XXNUM1              84
     O*********************  XXDS1               91
     O                       XXDS1               94
     O                       DESC1              125
     O          E            HEADB          1
     O               32      EOADR2              45
     O               32                          45 '            '
     O**********       32      PDADR2    54
     O              N32      XXCSZ1              45
     O              N32                          45 '            '
     O**********       31      XXAAD2    73
     O**********      N31      XXCSZ2    75
     O               31      XXAAD2              74
     O              N31      XXCSZ2              74
     O**********               EODIA3    89
     O                       XXNUM2              84
     O********************   XXDS2               91
     O                       XXDS2               94
     O                       DESC2              125
     O          E            HEADB          1
     O               32      XXCSZ1              45
     O               32                          45 '            '
     O**********      N31                49 '('
     O**********      N31      XXARA     52
     O**********      N31                53 ')'
     O**********      N31      XXPHN1    57
     O**********      N31                58 '-'
     O**********      N31      XXPHN2    62
     O**********       31      XXCSZ2    75
     O              N31                          35 '('
     O              N31      XXARA               38
     O              N31                          39 ')'
     O              N31      XXPHN1              43
     O              N31                          44 '-'
     O              N31      XXPHN2              48
     O               31      XXCSZ2              74
     O**********               EODIA4    89
     O                       XXNUM3              84
     O*********************  XXDS3               91
     O                       XXDS3               94
     O                       DESC3              125
     O          E            HEADB          2
     O               31                          49 '('
     O               31      XXARA               52
     O               31                          53 ')'
     O               31      XXPHN1              57
     O               31                          58 '-'
     O               31      XXPHN2              62
     O               31                          35 '('
     O               31      XXARA               38
     O               31                          39 ')'
     O               31      XXPHN1              43
     O               31                          44 '-'
     O               31      XXPHN2              48
     O                       XXNUM4              84
     O*********************  XXDS4               91
     O                       XXDS4               94
     O                       DESC4              125
HEADCO          E            HEADC          1
     O                                           29 '------------------------'
     O                                           53 '------------------------'
     O                                           77 '------------------------'
     O                                          101 '------------------------'
     O                                          120 '-------------------'
     O          E            HEADC          1
     O                                           12 'Service'
     O                                           34 'Modifiers   POS'
     O                                           44 'Diag Cd'
     O                                           54 'Billed'
     O                                          118 'Payment'
     O                       CXHMRA              69
     O                       CXHOT1              82
     O                       CXHOT2              95
     O                       CXHNET             108
     O          E            HEADC
     O                                           29 '------------------------'
     O                                           53 '------------------------'
     O                                           77 '------------------------'
     O                                          101 '------------------------'
     O                                          120 '-------------------'
     O********E*1              DETL11
     O          E            DETL15      1
     O                                           21 'Date of Service:'
     O                       XXFDAT              32 '  /  /    '
     O**********       40                34 '-'
     O**********       40      XXTDAT    45 '  /  /    '
     O                                           34 '-'
     O                       XXTDAT              45 '  /  /    '
     O          E            DETL12      1  1
     O                       XXCPT4              16
     O                       XXMOD1              20
     O                       XXMOD2              23
     O                       XXMOD3              26
     O                       XXMOD4              29
     O                       XXPOS               34
     O**********               XXICD9    43
     O                       XXDRF1              37
     O                       XXDRF2              39
     O                       XXDRF3              41
     O                       XXDRF4              43
     O                       CACHRG        J     56
     O                       CAMRA         J     69
     O                       CAOTHR        J     82
     O                       CAOTH2        J     95
     O                       CANDIS        J    108
     O                       CAPAID        J    120
     O          E            DETL13         1
     O                                           27 'Description:'
     O                       XXPRDS              53
     O**************** 34                88 'Billed Code:'
     O**************** 34      XXBILD   100
     O               36                          61 'Rx tax:'
     O               36      X1TAX         J     75
     O                                           82 'Units:'
     O                       X1UNTS        J     90
     O               34                         104 'Billed Code:'
     O               34      XXBILD             116
     O          E            DETL14      0  0
     O               42                          25 'EOB Codes:'
     O*** Beg chg *** 07/18/23 **************************************
     O*********      42      X1EOB               29
     O*********      43      X2EOB               33
     O*********      44      X3EOB               37
     O*********      45      X4EOB               41
     O               42N53   X1EOBR              32
     O               42 53   X1EOB               31
     O               43N53   X2EOBR              39
     O               43 53   X2EOB               38
     O               44N53   X3EOBR              46
     O               44 53   X3EOB               45
     O               45N53   X4EOBR              53
     O               45 53   X4EOB               52
     O               49N53 88X5EOBR              60
     O               49 53 88X5EOB               59
     O               52N53 88X6EOBR              67
     O               52 53 88X6EOB               66
     O               54N53 88X7EOBR              74
     O               54 53 88X7EOB               73
     O               55N53 88X8EOBR              81
     O               55 53 88X8EOB               80
     O               57N53 88X9EOBR              88
     O               57 53 88X9EOB               87
     O               58N53 88X10EOBR             95
     O               58 53 88X10EOB              94
     O               59N53 88X11EOBR            102
     O               59 53 88X11EOB             101
     O               68N53 88X12EOBR            109
     O               68 53 88X12EOB             111
     O*** End chg *** 07/18/23 **************************************
     O               46                          47 'Note:'
     O*** Beg Chg *** 07/18/23 *********************************************
     O***
     O***       E            DETL2
     O***                    XNOTE              120
     O***       E            DETL3       2  1
     O***                                        38 'Billed Charges:'
     O***                    EOCHRG        J     56
     O***       E            DETL3          1
     O***                                        34 'Reductions:'
     O***                    XXMRA         J     69
     O***                    EOOTHR        J     82
     O***                    XXOTH2        J     95
     O***                    EONDIS        J    108
     O***                                       120 '__________'
     O***       E            DETL3          1
     O***                                        37 'Total Payment:'
     O***                    XXPAID        J    120
     O***       E    33      DETL3          0
     O***                                        39 'Previously Paid:'
     O********E  0     33      DETL3
     O***                                       120 '__________'
     O***       E    33      DETL3       0
     O***                    X1PAID        J    120
     O***       E    33      DETL3       1  1
     O***                                        36 'This Payment:'
     O***                    X2PAID        J    120
     O***       E    47      DETL3       1  1
     O***                    XXNWM              185
     O***       E    47 80   DETL3          1
     O***                    XXNW2              145
     O***       E    47 81   DETL3          1
     O***                    XXNW3              145
     O***       E            DETL4       1  1
     O***                                        27 'EOB Reason Codes:'
     O***       E            DETL5
     O***                    YYEOB               13
     O***                    YYDESC              84
     O***       E            DETL6       1  1
     O***                                        32 'Additional EOMB Notes:'
     O***       E            DETL7
     O***                    X1NOTE              80
     O***       E            SPACE          1
     O***                                        10 '  '
     O          E            DETL2
     O              N88      XNOTE              120
     O          E            DETL3       2  1
     O                                           38 'Billed Charges:'
     O                       EOCHRG        J     56
     O          E            DETL3          1
     O                                           34 'Reductions:'
     O                       XXMRA         J     69
     O                       EOOTHR        J     82
     O                       XXOTH2        J     95
     O                       EONDIS        J    108
     O                                          120 '__________'
     O          E            DETL3          1
     O                                           37 'Total Payment:'
     O                       XXPAID        J    120
     O          E    33      DETL3          0
     O                                           39 'Previously Paid:'
     O                                          120 '__________'
     O          E    33      DETL3       0
     O                       X1PAID        J    120
     O          E    33      DETL3       1  1
     O                                           36 'This Payment:'
     O                       X2PAID        J    120
     O          E    36      DETL3          1
     O                                           47 'Payment Includes Louisia'
     O                                           71 'na Pharmacy Provider Par'
     O                                           88 'ish sales tax of:'
     O                                          106 '$'
     O                       XXTAX         J    120
     O          E    47      DETL3       1  1
     O                       XXNWM              185
     O          E    47 80   DETL3          1
     O                       XXNW2              145
     O          E    47 81   DETL3          1
     O                       XXNW3              145
     O          E            DETL4       1  1
     O               53                          27 'Additional EOBR Reason'
     O              N53                          27 'Additional EOB Reason'
     O                                           33 ' Codes'
     O          E            DTEOB       1  1
     O               53                          29 'EOBR Codes/Descriptions:'
     O              N53                          29 'EOB Codes/Descriptions:'
     O          E            DTEON          1
     O                       YYEOB               13
     O                       XNOTE               96
     O*
     O          E            DTEOD          1
     O                       XNOTE               96
     O*
     O          E            DTNOT       1  1
     O               53                          19 'Carrier Notes:'
     O              N53                          19 'Notes:'
     O*
     O          E            DTLIN          1
     O                                           10 'LINE#'
     O                       XLINE         Z     15
     O                                           17 '-'
     O                                           22 'NOTE'
     O                       XNO1          Z     25
     O                       X1NOTE              96
     O*
     O          E            DTNO1          1
     O                       X1NOTE              96
     O*
     O          E            DTEOH       1  1
     O               53                          27 'General Carrier Notes:'
     O              N53                          27 'General Notes:'
     O          E            DTLIH          1
     O                                            9 'NOTE'
     O                       XNO1          Z     13
     O                       X1NOTE              96
     O*
     O          E            DTLIH1         1
     O                       X1NOTE              96
     O*
     O          E            BRKLIN
     O*
     O                                           28 '-----------------------'
     O                                           52 '------------------------'
     O                                           76 '------------------------'
     O                                          100 '------------------------'
     O                                          120 '--------------------'
     O          E            DETL5
     O                       YYEOB               13
     O                       YYDESC              84
     O          E            DETL6       1  1
     O               53                          27 'Additional EOBR Notes:'
     O              N53                          27 'Additional EOB Notes:'
     O          E            DETL7
     O                       X1NOTE              80
     O          E            SPACE          1
     O                                           10 '  '
     O*
     O          E            INSURED        2
     O                                           25 'Insurer Information:'
     O*
     O          E            INSURED        1
     O                       XXANME              45
     O*
     O          E            INSURED        1
     O                       XXAAD1              45
     O*
     O          E    31      INSURED        1
     O                       XXAAD2              45
     O*
     O          E            INSURED        1
     O                       XXCSZ2              45
     O*
     O          E   N79      INSURED        1
     O              N79                           6 '('
     O              N79      XXARA                9
     O              N79                          10 ')'
     O              N79      XXPHN1              14
     O              N79                          15 '-'
     O              N79      XXPHN2              19
     O*
     O          E            INSURED        1
     O                                           29 'Division Insured Insurer'
     O                                           37 'Number:'
     O                       XXDIV1              43
     O*
     O          E            INSURED        1
     O                       DASH               120
     O*** End Chg *** 07/18/23 *********************************************
     O********E*               DETL8
     O**********               STNOTE    75
     O*
     O          E            DETL8       1
 |    *** Change Font to COURIER 12 BOLD
     O*****
     O*****   E 0      74      DETL8
 |   O*****                    ESC
 |   O*****                                 '(0U'
 |   O*****                    ESC
 |   O*****                                 '(s0p12h10v0s3b3T'
     O*****   E 0              DETL8
 |    *** Change Font to COURIER 10 BOLD
     O          E    74      DETL8       0
 |   O                       ESC
 |   O                                              '(0U'
 |   O                       ESC
 |   O                                              '(s0p14h0s3b4099T'
     O          E            DETL8       0
     O                       STCOMM             145
 |    *   Change back to NORMAL FONT MED/COR. 8.5PT/16.66
     O          E    74      DETL8       0
 |   O                       ESC
 |   O                                              '(0U'
 |   O                       ESC
     O                                              '(s0p16.66hsb0T'
     O*
     O          E            DETL9       1
     O**********               GPNOTE    75
     O                       GRCOMM             145
     O********E*21             DETL10
     O**********               XXANME    45
     O**********       40                70 'Admission date:'
     O**********       40      XXADDT    81 '  /  /    '
     O********E* 1             DETL10
     O**********               XXAAD1    45
     O**********       40                64 'Coverage:'
     O**********       40      XXSCPF    75 '  /  /    '
     O**********       40                77 '-'
     O**********       40      XXSCPT    88 '  /  /    '
     O**********
     O********E* 1             DETL10
     O**********       31      XXAAD2    45
     O**********       40                71 'Submission date:'
     O**********       40      XXSBDT    82 '  /  /    '
     O********E* 1             DETL10
     O**********               XXCSZ2    45
     O**********       40                71 'Type of Service:'
     O**********       40      XXICN     74
     O**********       40      XXICDS   106
     O********E*               DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O**********       40                68 'Type of Bill:'
     O**********       40      XXBLTY    74
     O**********       40      XXBDSC   106
     O*----------------------------------------------------------------
     O* NON-HOSPITAL AND NO SECOND ADDRESS
     O**********
     O**********
     O********E* 1    N40N31   DETL10
     O**********               XXCSZ2    45
     O********E*      N40N31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O*----------------------------------------------------------------
     O**NON-HOSPITAL AND SECOND ADDRESS
     O**********
     O********E* 1    N40 31   DETL10
     O**********               XXAAD2    45
     O********E* 1    N40 31   DETL10
     O**********               XXCSZ2    45
     O********E*      N40 31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O*----------------------------------------------------------------
     O**HOSPITAL BILL AND NO SECOND ADDRESS
     O**********
     O********E* 1     40N31   DETL10
     O**********               XXCSZ2    45
     O**********                         71 'Submission date:'
     O**********               XXSBDT    82 '  /  /    '
     O********E* 1     40N31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O**********                         71 'Type of Service:'
     O**********               XXICN     74
     O**********               XXICDS   106
     O********E*       40N31   DETL10
     O**********                         68 'Type of Bill:'
     O**********               XXBLTY    74
     O**********               XXBDSC   106
     O*----------------------------------------------------------------
     O**HOSPITAL AND 2ND ADDRESS
     O**********
     O********E* 1     40 31   DETL10
     O**********               XXAAD2    45
     O**********                         71 'Submission date:'
     O**********               XXSBDT    82 '  /  /    '
     O********E* 1     40 31   DETL10
     O**********               XXCSZ2    45
     O**********                         71 'Type of Service:'
     O**********               XXICN     74
     O**********               XXICDS   106
     O********E*       40 31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O**********                         68 'Type of Bill:'
     O**********               XXBLTY    74
     O**********               XXBDSC   106
     O*
     O* NO SECOND ADDRESS
     O**********
     O********E* 1       N31   DETL10
     O**********               XXCSZ2    45
     O********E*         N31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O*----------------------------------------------------------------
     O* SECOND ADDRESS
     O**********
     O********E* 1        31   DETL10
     O**********               XXAAD2    45
     O********E* 1        31   DETL10
     O**********               XXCSZ2    45
     O********E*          31   DETL10
     O**********                          6 '('
     O**********               XXARA      9
     O**********                         10 ')'
     O**********               XXPHN1    14
     O**********                         15 '-'
     O**********               XXPHN2    19
     O**********
     O*----------------------------------------------------------------
     O* HOSPITAL SPECIFIC INFORMATION
     O*
     O          E       31   DETL16      1  1
     O                                           20 'Admission date:'
     O                       XXADDT              31 '  /  /    '
     O                                           80 'Proc codes:'
     O          E            DETL16         1
     O                                           14 'Coverage:'
     O                       XXSCPF              25 '  /  /    '
     O                                           27 '-'
     O                       XXSCPT              38 '  /  /    '
     O                       EUPRC1              80
     O                       XXPRD1             117
     O          E            DETL16         1
     O                                           21 'Submission date:'
     O                       XXSBDT              32 '  /  /    '
     O                       EUPRC2              80
     O                       XXPRD2             117
     O          E            DETL16         1
     O                                           21 'Type of Service:'
     O                       XXICN               24
     O                       XXICDS              56
     O                       EUPRC3              80
     O                       XXPRD3             117
     O          E            DETL16
     O                                           18 'Type of Bill:'
     O                       XXBLTY              24
     O                       XXBDSC              56
     O                       EUPRC4              80
     O                       XXPRD4             117
     O*****************************************************************
     O* CHANGE FONT TO PRINT IN SMALL TYPE
     O*****************************************************************
     O********E* 1             SMALL
     O**********               ESC
     O**********                            '(0U'
     O**********               ESC
     O**********                            '(s1p07vs0b6T'
     O* *************************************************************
     O*    REST OF PAGE IS COURIER 16.66 PITCH (17)
     O* *************************************************************
     O********E* 1             NORMAL
     O**********               ESC
     O**********                            '(0U'
     O**********               ESC
     O**********                            '(s0p17.66hsb0T'
     O          E            ADINH1         1  3
     O                                           68 'EXPLANATION OF MEDICAL'
     O                                           77 'BENEFITS'
     O*
     O********E* 2             ADINH1
     O          E   N79      ADINH1         2
     O                                           72 'ADDITIONAL INFORMATION'
     O                                           90 'EOMB#:'
     O                       EOEOB#        Z    107
     O          E    79      ADINH1         1
     O                                           72 'ADDITIONAL INFORMATION'
     O                                           90 'EOMB#:'
     O                       EOEOB#        Z    107
     O          E    79      ADINH1         1
     O                                           93 'Review #:'
     O                       XXTRID            +001
     O*
     O*----------------------------------------------------------------
     O          E            ADIND1         1
     O                                           13 'Carrier:'
     O                       XXNAM1              66
     O                                           96 'Provider Bill Date:'
     O                       XXCSDT             107 '  /  /    '
     O*
     O          E            ADIND1         1
     O                                           13 'Address:'
     O                       XXFAD1              66
     O*
     O* NO SECOND ADDRESS
     O*
     O          E      N31   ADIND1         2
     O                       XXCSZ1              66
     O          E      N31   ADIND1         2
     O                                           11 'Phone:'
     O                                           27 '('
     O                       XXAREA              30
     O                                           31 ')'
     O                       XXPHN1              35
     O                                           36 '-'
     O                       XXPHN2              40
     O*
     O* SECOND ADDRESS
     O*
     O          E       31   ADIND1         1
     O                       XXFAD2              66
     O          E       31   ADIND1         2
     O                       XXCSZ1              66
     O          E       31   ADIND1         2
     O                                           11 'Phone:'
     O                                           27 '('
     O                       XXAREA              30
     O                                           31 ')'
     O                       XXPHN1              35
     O                                           36 '-'
     O                       XXPHN2              40
     O*----------------------------------------------------------------
     O          E            ADIND2         2
     O                                           24 'NAICS/Self Insured:'
     O                       XXNAIC              35
     O*----------------------------------------------------------------
     O          E            ADIND3         1
     O                                           21 'Service Company:'
     O                       XXNAM1              66
     O          E            ADIND3         3
     O                                           21 'Service Address:'
     O                       XXFAD1              66
     O*
     O* NO SECOND ADDRESS
     O*
     O          E      N31   ADIND3         2
     O                       XXCSZ1              66
     O*
     O* SECOND ADDRESS
     O*
     O          E       31   ADIND3         1
     O                       XXFAD2              66
     O          E       31   ADIND3         2
     O                       XXCSZ1              66
     O*----------------------------------------------------------------
     O          E            ADIND4         1
     O                                           14 'Employee:'
     O                       XXCLNM              56
     O          E            ADIND4         1
     O                                           13 'Address:'
     O                       XXFAD1              66
     O*
     O* NO SECOND ADDRESS
     O*
     O          E      N31   ADIND4         2
     O                       XXCSZ1              66
     O*
     O* SECOND ADDRESS
     O*
     O          E       31   ADIND4         1
     O                       XXFAD2              66
     O          E       31   ADIND4         2
     O                       XXCSZ1              66
     O*
     OCHECKS    E            HEADH       2  1  3
     O                                           68 'HES Group Check Reconc'
     O                                           83 'iliation Report'
     O          E    18      HEADH          1
     O                                           73 'Deleted Check Requests'
     O          E            HEADH          1
     O                                           14 'Report Date:'
     O                       DSDATE              25 '  /  /    '
     O          E            HEADH          1
     O                                           15 'Group/Region:'
     O                                           40 'Description:'
     O                       XHFND               19
     O                                           20 '/'
     O                       XHRGN               24
     O                       XHDSC              100
     O          E            HEADH
     O                                           26 '------------------------'
     O                                           50 '------------------------'
     O                                           74 '------------------------'
     O                                           98 '------------------------'
     O                                          120 '----------------------'
     O          E            HEADH
     O                                           43 'Claimant'
     O                                           55 'Claimant'
     O                                           66 'Invoice'
     O                                           76 'Process'
     O          E            HEADH
     O                                           13 'HES Bill ID'
     O                                           29 'Grp/Yr/Case'
     O                                           45 'First Name'
     O                                           56 'Last Name'
     O                                           63 'Date'
     O                                           73 'Date'
     O                                           90 'Billed Amt'
     O                                          103 'Claim Fee'
     O                                          120 'Check Amount'
     O          E            HEADH
     O                                           26 '------------------------'
     O                                           50 '------------------------'
     O                                           74 '------------------------'
     O                                           98 '------------------------'
     O                                          120 '----------------------'
     O          E            DETHES
     O                       XHSINV              16
     O                       XHFND               21
     O                                           22 '/'
     O                       XHFYR               25
     O                                           26 '/'
     O                       XHCASE              33
     O                       XHFSN               45
     O                       XHLSN               57
     O                       XHCSDT              67 '  /  /  '
     O                       XHTRDT              77 '  /  /  '
     O                       XHCHRG        J     92
     O                       XHPAID        J    106
     O                       XHAMT         J    120
     O          E            HESGP
     O                                           27 'HES Group Summary Total:'
     O          E            HESGP
     O                                           36 'Billed Amt.'
     O                       XGCHG         J     51
     O          E            HESGP
     O                                           36 'Claim Fee  '
     O                       XGPAID        J     51
     O          E            HESGP
     O                                           36 'Check Amt. '
     O                       XGAMT         J     51
     O          E            HESTOT
     O                                           27 'HES Group Grand Total:  '
     O          E            HESTOT
     O                                           36 'Billed Amt.'
     O                       XTCHG         J     51
     O          E            HESTOT
     O                                           36 'Claim Fee  '
     O                       XTPAID        J     51
     O          E            HESTOT
     O                                           36 'Check Amt. '
     O                       XTAMT         J     51
      *
      ***************************************************************
      *
     O/COPY OCHKDETG
      *
     O/COPY OFILDETG
      *
      ***************************************************************
**  QCMDEXC TABLE
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
**  CONSTANTS
WORKERS' COMPENSATION ACCOUNT
DAVIS WORKERS' COMPENSATION ACCOUNT
Non-Negotiable after 90 Days
** SIGNATURE MESSAGE PART ONE
OVER
** SIGNATURE MESSAGE PART TWO
 REQUIRES SECOND SIGNATURE
** (NWMSG)
"Network reductions have been made per
contractual agreement."
