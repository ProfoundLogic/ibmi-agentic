     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('STBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE Company Configuration- Prompt
     H*********************************************************************
    H*  DESCRIPTION:  This program prompts for Company setup and maint-  *
    H*                enance.                                            *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU     : Application Setup >> Company Level Processing >>    *
    H*               Company configuration                               *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*  REWRITE:               C:306     PROJECT: WC2397                 *
     H*********************************************************************
     H* REVISIONS:                                                        *
     H*                                                                   *
     H*  06/13/25 C:062 Project: 153WBD                                   *
     H*                 Condition Managed Care Edits.                     *
     H*                                                                   *
     H*  05/08/25 C:333 Project: 153WBD                                   *
     H*                 Note of Cancel Format-Contact Info Updt.          *
     H*                 DSPF Changes.                                     *
     H*                 - Adding EMail Address field in Add/Edit Company  *
     H*                   Information Screen.                             *
     H*                 - Adding Email Address in detail box.             *
     H*                                                                   *
     H*  09/25/24 C:65  Project: SY446 (Design and Compose revamp)        *
     H*                 When D&C module is enabled display a URL logo     *
     H*                 field in addition to path field.  Only require    *
     H*                 edits based on D&C enabled status.                *
     H*                                                                   *
     H* 09/13/24 C:333 Support: 117592                                    *
     H*                Internal support - Missing Scriptura output logo path
     H*                - Adding LblOutputlogoPath, TxbOutputlogoPath,     *
     H*                   ImgAddOutputlogoPath (for Logo path),           *
     H*                   onKeyUp (TxbOutputlogo), onload (RcdCompAdd).   *
     H*                - Removing ImgAddOutputlogo (as no edit for blank  *
     H*                   logo name exits, assuming it's not mandatory)   *
     H*                - Moving 30px down, all elements from line1        *
     H*                  (horizontal line).                               *
     H*                - Increased msSLogo length from 10 to 250.         *
     H*                                                                   *
     H* 05/20/24 C:342 Project: MC446                                     *
     H*                Managed Care CO Maintenance - UX Updates           *
     H*                1. Existing issue is fixed when there is no setup  *
     H*                   available in file WTSYP and WTTTP file          *
     H*                                                                   *
     H* 11/29/22 C:300 Support: 105386 (Vital 153)                        *
     H*                Phone number not adding/saving for Company 153     *
     H*                                                                   *
     H* 10/13/22 C:291 Project: 129WXD                                    *
     H*                Add Checkbox "Print Federal Name on Refund Checks" *
     H*                on Add/Edit window and in the detail box.          *
     H*                                                                   *
     H* 06/21/22 C:333 Support: 99423                                     *
     H*                - Suppressed following Fields in detail box:       *
     H*                   - manual check numbers                          *
     H*                   - next available claim number                   *
     H*                   - ISO Transaction Type                          *
     H*                   - ISO Assigned Company/Group                    *
     H*                   - Policy Trans Code, Deletion                   *
     H*                                                                   *
     H*                DSPF Changes:                                      *
     H*                -Introduced Visibility indictor for above fields.  *
     H*                -added containers and varaibles (scTop1/2/3/4/5)   *
     H*                 in RcdCompMst.                                    *
     H*                -added container and variable scTop6 in RcdCompAdd.*
     H*                                                                   *
     H*                RPG Changes:                                       *
     H*                - Initializing Containers Top variables and reduce *
     H*                  according to Isvisible1 indicator.               *
     H*                                                                   *
     H* 04/11/22 C:333 Project: SY434                                     *
     H*                - Update Grids for Sorting Filtering Upgrade       *
     H*                                                                   *
     H* 03/10/22 C:340 Support: WC2843                                    *
     H*                Add Program CVTWMTMP to covert/merge 2 ETR records *
     H*                into one                                           *
      //
      // 03/10/20 C:33  Project: SY420
      //                Add Masking variables
     H*                                                                   *
     H* 02/13/20 C:33  Project: SY406
     H*                Add Portal Configuration
     H*                                                                   *
     H* 12/14/18 C:33  Project: SY406
     H*                Add Template Associations
     H*                                                                   *
     H* 09/19/18 C:053 Support: 73434
     H*                Correct BUL# Initialazation
     H*                These are not accessible to the client and shouldn't
     H*                be updated or initialized.
     H*                                                                   *
     H* 06/08/18 C:136 Project: 150WAQ                                    *
     H*                Add flag for collateral processing (Sunz Only)     *
     H*                                                                   *
     H* 05/10/18 C:TXO PROJECT: PHW70121                                  *
     H*                AutoTransfer A/R billing batch                     *
     H*                                                                   *
     H* 05/07/18 C:291 Project: C150WBV                                   *
     H*                Change the "Generate Premium Notice" label to      *
     H*                "Billing Invoice" on Vendor Information popup,     *
     H*                available for Sunz only.                           *
     H*                                                                   *
     H* 02/21/18 C:136 Project: WC2588                                    *
     H*                Add flags to Include Non Billed Divisions and      *
     H*                to Exclude Non Reporting Locations                 *
     H*                                                                   *
     H* 08/08/17 C:291 Project: 150WAT                                    *
     H*                Add policy export fields.                          *
     H*          C:136 Reworked vendor and TPA edits                      *
     H*                Hide credentials per AO when storing to MoveIt.    *
     H*                The credentials will be stored in the FTPMoveIt    *
     H*                data area. The values stored in the TPA user ID    *
     H*                and password fields will be used for FTP servers   *
     H*                other than MoveIt. Reference W8371C.               *
     H*                                                                   *
     H* 08/03/17 C:291 Phaseware: 48330                                   *
     H*                Fixed Add/Edit Company Informattion screen fields  *
     H*                company name and Physical/Mailing Address          *
     H*                Text Transform property from Uppercase to Capitalize
     H*                Changed Output Logo file field to be Optional.     *
     H*                Fixed Add/Edit Vendor Informattion screen fields   *
     H*                Vendor Name and Address to Capitalize.             *
     H*                                                                   *
     H* 07/24/17 C:267 PROJECT: 150WAN                                    *
     H*                Fix tabbing order in display file for new fields.  *
     H*                                                                   *
     H* 07/21/17 C:267 PROJECT: 150WAN                                    *
     H*                ADD EMPLOYEE INFO REQUIRED CHECKBOX                *
     H*                ADD '# OF DAYS FOR CHECK ISSUANCE'                 *
     H*                ADD GENERATE PREMIUM NOTICE CHECKBOX               *
     H*                to Vendor Information pop up screen                *
     H*                                                                   *
     H*                                                                   *
     H*********************************************************************
     H* INDICATORS:
     H*********************************************************************
     * Files Declaration                                                 *
     fstcmpcfg  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflCompInf:RrnCmpinf)
     f                                     sfile(sflVenInfo:RrnVeninf)
     f                                     sfile(sflTpaInfo:RrnTPAinf)
     ****  Beg Add  ***  03/10/22  *********************************************
     f                                     sfile(sflCarrInf:RrnCarinf)
     ****  End Add  ***  03/10/22  *********************************************
     ***** Beg Add *** 05/20/24 *******************************************
     f                                     sfile(sflStInfo:rrnState)
     ***** End Add *** 05/20/24 *******************************************
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
    f*** System & Application Control File
     fssylf     if   e           k disk
     f                                     prefix(ss_)
    F*** Workers' Comp Logical over All Table Files
     fwtabl     if   e           k disk
     f                                     ignore(ppc01)
     f                                     ignore(prc01)
     f                                     ignore(pbc01)
     f                                     ignore(pcs01)
     f                                     ignore(prs01)
     f
    f*** Workers Comp Rate Set Code Master File
     fwmrsl     if   e           k disk    rename(prs01:prs00)
    f*** Remark Type Table File
     fstctl1    if   e           k disk    rename(pct01:pct00)
    f*** Vendor History File
     fwdvhl     if   e           k disk    rename(pvh01:pvh00)
     f
    f*** System company file for Work Compensation
     fsmcop     uf a e             disk    rename(pco01:pco0p)
     f
     fsmcol     if   e           k disk    rename(pco01:pco00)
     f                                     infds(dbfds)
    f*** Company Contact Information
     fsmccp     uf a e             disk    rename(pcc01:pcc0p)
     f
     f*
     fsmccl     if   e           k disk    rename(pcc01:pcc00)
     f                                     prefix(cp_)
     f                                     infds(dbfds4)
    f*** Payroll Vendor File
     fwmvpp     uf a e             disk    rename(pvp01:pvp0p)
     f
     fwmvpl     if   e           k disk    rename(pvp01:pvp00)
     f                                     prefix(v1_)
     f                                     infds(dbfds1)
    f*** TPA Contact Master Information
     fwmtcp     uf a e             disk    rename(ptc01:ptc0p)
     f
     fwmtcl     if   e           k disk    rename(ptc01:ptc00)
     f                                     prefix(t1_)
     f                                     infds(dbfds2)
    f*** WCPOLS/WCSTAT Transmittal Information Default File
     fwmtmp     uf a e             disk    rename(ptm01:ptm0p)
     f
     ****  Beg Add  ***  03/10/22  *********************************************
     f***   wmtml     if   e           k disk    rename(ptm01:ptm00)
     f***                                        infds(dbfds3)
     fwmtml1    Uf   e           k disk    rename(ptm01:ptm00)
     f                                     infds(dbfds3)
     f
     fWMCGP     uf a e             disk    rename(PCG01:PCG0P)
     fWMCGL     Uf   e           k disk    rename(PCG01:PCG00)
     f                                     infds(dbfds5)
     ****  End Add  ***  03/10/22  *********************************************
     f**** Beg Add *** 05/20/24 *******************************************
     ffmc1l     uf a e           k disk    rename(pc101:pc101) usropn
     fststl2    uf a e           k disk    rename(pst01:pst02)
     f**** End Add *** 05/20/24 *******************************************
     f*** begin add ***  09/25/24  ******************************************
     fstscp     if   e             disk    rename(psc01:psc0p)
     f*** end add *****  09/25/24  ******************************************

     ***********************************************************
     * Copybook having Prototypes.
     ***********************************************************
     d
     d/copy SPrDate
     d/copy sPrMask
     d/copy SprModule
     d/copy SprSql
     d/copy sPrCompany
     d/copy wPrUsrprms
     d/copy wPrPolErr
     d/copy sPrString
     d/copy scopypr01d
     d
     d/copy sprGenPop
      *** Beg Add ***** 05/08/25 ********************************
     d/copy sPrGenErr
      *** End Add ***** 05/08/25 ********************************
     d
     * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d ##CltId                        3
     d
     * Check Object.
     dGetObject        pr                  extpgm('SRCHKOBJ')
     d outName                             like(outName)
     d outType                             like(outType)
     d outRtcd                             like(outRtcd)
     d
     * Out suffix.
     d sa030r          pr                  extpgm('SA030R')
     d outsufx                        3

     *---------------------------------------------------------------------
     **** Prototype Declarations *******************************
     *---------------------------------------------------------------------
     * Clear the subfiles
     d ClrSubFiles     pr
     d
     * Execute Screen record Formats.
     d Exfmtscr        pr
     d
     d
     * Intialize the Screen fields
     d IntializeScVal  pr
     d
     * Loading the action panel.
     d LoadActPnl      pr
     d
     * Main Processing
     d Main            pr
     d
    d* Company ADD/EDIT procedure.
     d PrcAddComp      pr
     d inrrn                          9  0
     d inMode                         1    const
     d
    d* Vender ADD/EDIT/DELETE procedure.
     d PrcAddVndr      pr
     d inrrn                          9  0
     d inMode                         1    const
     d
    d* TPA (Third Party) ADD/CHANGE STATUS procedure.
     d PrcAddTPA       pr
     d inrrn                          9  0
     d inMode                         1    const
     d
    d* Company Contact inforamtion ADD/EDIT procedure.
     d PrcCmpCont      pr
     d inComp#                        3  0
     d
    d* Comapany detail box loading procedure.
     d PrcCmpDtl       pr
     d inSflrrn                       9  0
     d
    d* Company Grid Loading procedure.
     d PrcCmpGrd       pr
     d
    d* Vendor Grid Loading procedure.
     d PrcVenGrd       pr
     d inSflrrn                       9  0
     d
    d* TPA Grid Loading procedure.
     d PrcTPAGrd       pr
     d inSflrrn                       9  0
     d
    d* Validate Company Addition info. procedure.
     d ValidateCmpny   pr              n
     d inMode                         1    const
     d
    d* Validate Vendor Addition info. procedure.
     d ValidateVndr    pr              n
     d inMode                         1    const
     d
    d* Validate TPA Addition info. procedure.
     d ValidateTPA     pr              n
     d inMode                         1    const
     d
    d* Validate Company Contact info. procedure.
     d ValidateCmpCon  pr              n
     d

    d* Check for Document Authorities
     D access          pr            10I 0 ExtProc('access')
     D   path                          *   Value Options(*string)
     D   aMode                       10I 0 Value

    d* Retrieve Document Stats
     d stat            PR            10I 0 ExtProc('stat')
     d   path                          *   value options(*string)
     d   buf                           *   value
     ****  Beg Add  ***  03/10/22  *********************************************
     d* Carrier Grid Loading procedure.
     d DspCarriers     pr
     d inRrn                          9  0

     d* Carrier Grid processing procedure.
     d LoadCarrGrid    pr
     d inSflrrn                       9  0

     d* Carrier detail box loading procedure.
     d PrcCarDtl       pr
     d inSflrrn                       9  0
     d
     ****  End Add  ***  03/10/22  *********************************************
      /copy WITMPLTD
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d @@call          s              1
     d @@prds          s             40
     D #1mode          s              1
     d i1              s             10i 0 inz
     disValid          s              1n   inz('1')
     d intSavCrrn      s              9  0 inz(0)
     d RrnCmpInf       s              9  0 inz
     d RrnVenInf       s              9  0 inz
     d RrnTPAInf       s              9  0 inz
     d intFrstRnn      s              9  0 inz(1)
     d inSflrrn        s              9  0
     ****  Beg Add  ***  03/10/22  *********************************************
     d rrnCarInf       s              9  0 inz
     d savCarRrn       s              9  0 inz(0)
     ****  Beg Add  ***  03/10/22  *********************************************
     DstrPortalName    s             40    varying inz
     d
     d outName         s             10
     d outType         s              7
     d outRtcd         s              1
     d
     d inComp#         s              3  0
     d inRrn           s              9  0
     d inMode          s              1
     d
     d outsufx         s              3
     d ##CltId         s              3
     d
     d strMsg          s            100
     d*intCo#          s              3  0 inz(0)
     d
     d p_statds        S               *
     d myStat          S                   like(statds)
     d PathNameErr     s              1n
     d PathNmLen       s              3  0
     d fullPathName    s            256
     d xxDirectory     s            256
     d lastSlash       s              3  0
     d X               s              3  0
      *** Beg Add ***** 09/13/24 *******************************
     d strSlash        s              1
     d strSlash2       s              1
      *** End Add ***** 09/13/24 *******************************

     d docExists       c                   0                                    Exists
     d docAccess       c                   4                                    Access
     D lo              c                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D up              c                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     d dbfds           ds
     d  corrn                397    400b 0
     d
     d dbfds1          ds
     d  vnrrn                397    400b 0
     d
     d dbfds2          ds
     d  tprrn2               397    400b 0
     d
     d dbfds3          ds
     d  tmrrn3               397    400b 0
     d
     d dbfds4          ds
     d  cprrn4               397    400b 0
     d
     ****  Beg Add  ***  03/10/22  *********************************************
     d dbfds5          ds
     d  carrrn               397    400b 0
     ****  Beg Add  ***  03/10/22  *********************************************
     d
     DdsSMCIP        e ds                  extname(SMCIP)  inz qualified
     DdsSMCIPsave    e ds                  extname(SMCIP)  inz qualified

     d statds          DS                  BASED(p_statds)
     d  st_mode                      10U 0
     d  st_ino                       10U 0
     d  st_nlink                      5U 0
     d  st_pad                        2A
     d  st_uid                       10U 0
     d  st_gid                       10U 0
     d  st_size                      10I 0
     d  st_atime                     10I 0
     d  st_mtime                     10I 0
     d  st_ctime                     10I 0
     d  st_dev                       10U 0
     d  st_blksize                   10U 0
     d  st_allocsize                 10U 0
     d  st_objtype                   10A
     d  st_codepage                   5U 0
     d  st_reserved1                 62A
     d  st_ino_gen_id                10U 0
     DdsLibFile        ds            20
     D strFile                       10    inz('*ALL')
     D strLib                        10    inz('*LIBL')

     D aryMaskSC       ds                  likeds(dsMaskingGlobal) dim(#ofMasks)
     D dsMaskingSC     ds           350
     D   scMsk01                      1    overlay(dsMaskingSC:    1)
     D   scMskUsr01                  10    overlay(dsMaskingSC:    6)
     D   scMsk02                      1    overlay(dsMaskingSC:   36)
     D   scMskUsr02                  10    overlay(dsMaskingSC:   41)
     D   scMsk03                      1    overlay(dsMaskingSC:   71)
     D   scMskUsr03                  10    overlay(dsMaskingSC:   76)
     D   scMsk04                      1    overlay(dsMaskingSC:  106)
     D   scMskUsr04                  10    overlay(dsMaskingSC:  111)
     D   scMsk05                      1    overlay(dsMaskingSC:  141)
     D   scMskUsr05                  10    overlay(dsMaskingSC:  146)
     D   scMsk06                      1    overlay(dsMaskingSC:  176)
     D   scMskUsr06                  10    overlay(dsMaskingSC:  181)
     D   scMsk07                      1    overlay(dsMaskingSC:  211)
     D   scMskUsr07                  10    overlay(dsMaskingSC:  216)
     D   scMsk08                      1    overlay(dsMaskingSC:  246)
     D   scMskUsr08                  10    overlay(dsMaskingSC:  251)
     D   scMsk09                      1    overlay(dsMaskingSC:  281)
     D   scMskUsr09                  10    overlay(dsMaskingSC:  256)
     D   scMsk10                      1    overlay(dsMaskingSC:  316)
     D   scMskUsr10                  10    overlay(dsMaskingSC:  321)
     D   aryMaskingSC                35    overlay(dsMaskingSC:    1)
     D                                     dim(#ofMasks)
     D aryMaskCO       ds                  likeds(dsMaskingGlobal) dim(#ofMasks)
     D dsMaskingCO     ds           350
     D   COmk01                      35    overlay(dsMaskingCO:    1)
     D   COmk02                      35    overlay(dsMaskingCO:   36)
     D   COmk03                      35    overlay(dsMaskingCO:   71)
     D   COmk04                      35    overlay(dsMaskingCO:  106)
     D   COmk05                      35    overlay(dsMaskingCO:  141)
     D   COmk06                      35    overlay(dsMaskingCO:  176)
     D   COmk07                      35    overlay(dsMaskingCO:  211)
     D   COmk08                      35    overlay(dsMaskingCO:  246)
     D   COmk09                      35    overlay(dsMaskingCO:  281)
     D   COmk10                      35    overlay(dsMaskingCO:  316)
     D   aryMaskingCO                35    overlay(dsMaskingCO:    1)
     D                                     dim(#ofMasks)
     D aryMaskSY       ds                  likeds(dsMaskingGlobal) dim(#ofMasks)
     D dsMaskingSY     ds           350
     D   ss_SYmk01                   35    overlay(dsMaskingSY:    1)
     D   ss_SYmk02                   35    overlay(dsMaskingSY:   36)
     D   ss_SYmk03                   35    overlay(dsMaskingSY:   71)
     D   ss_SYmk04                   35    overlay(dsMaskingSY:  106)
     D   ss_SYmk05                   35    overlay(dsMaskingSY:  141)
     D   ss_SYmk06                   35    overlay(dsMaskingSY:  176)
     D   ss_SYmk07                   35    overlay(dsMaskingSY:  211)
     D   ss_SYmk08                   35    overlay(dsMaskingSY:  246)
     D   ss_SYmk09                   35    overlay(dsMaskingSY:  281)
     D   ss_SYmk10                   35    overlay(dsMaskingSY:  316)
     D   aryMaskingSY                35    overlay(dsMaskingSY:    1)
     D                                     dim(#ofMasks)
     D   scSYmk01                    35    overlay(dsMaskingSY:    1)
     D   scSYmk02                    35    overlay(dsMaskingSY:   36)
     D   scSYmk03                    35    overlay(dsMaskingSY:   71)
     D   scSYmk04                    35    overlay(dsMaskingSY:  106)
     D   scSYmk05                    35    overlay(dsMaskingSY:  141)
     D   scSYmk06                    35    overlay(dsMaskingSY:  176)
     D   scSYmk07                    35    overlay(dsMaskingSY:  211)
     D   scSYmk08                    35    overlay(dsMaskingSY:  246)
     D   scSYmk09                    35    overlay(dsMaskingSY:  281)
     D   scSYmk10                    35    overlay(dsMaskingSY:  316)
     D  dsMskER        ds                  inz
     D   erMsk01                      1
     D   erMsk02                      1
     D   erMsk03                      1
     D   erMsk04                      1
     D   erMsk05                      1
     D   erMsk06                      1
     D   erMsk07                      1
     D   erMsk08                      1
     D   erMsk09                      1
     D   erMsk10                      1
     D   aryMskER                     1    overlay(dsMskER) dim(#ofMasks)
     D  dsMskMT        ds                  inz
     D   mtMsk01                    100
     D   mtMsk02                    100
     D   mtMsk03                    100
     D   mtMsk04                    100
     D   mtMsk05                    100
     D   mtMsk06                    100
     D   mtMsk07                    100
     D   mtMsk08                    100
     D   mtMsk09                    100
     D   mtMsk10                    100
     D   aryMskMT                   100    overlay(dsMskMT) dim(#ofMasks)
     D  dsMskUsrER     ds                  inz
     D   erMskUsr01                   1
     D   erMskUsr02                   1
     D   erMskUsr03                   1
     D   erMskUsr04                   1
     D   erMskUsr05                   1
     D   erMskUsr06                   1
     D   erMskUsr07                   1
     D   erMskUsr08                   1
     D   erMskUsr09                   1
     D   erMskUsr10                   1
     D   aryMskUsrER                  1    overlay(dsMskUsrER) dim(#ofMasks)
     DaryMaskProcess   s             10    dim(#ofMasks) inz
     daryMasks         s             25    dim(#ofMasks) ctdata perrcd(1)
     D dsMaskInfo      ds                  inz
     D   dsMsk01                      1    overlay(dsMaskInfo:    1)
     D   dsMsk02                      1    overlay(dsMaskInfo:    2)
     D   dsMsk03                      1    overlay(dsMaskInfo:    3)
     D   dsMsk04                      1    overlay(dsMaskInfo:    4)
     D   dsMsk05                      1    overlay(dsMaskInfo:    5)
     D   dsMsk06                      1    overlay(dsMaskInfo:    6)
     D   dsMsk07                      1    overlay(dsMaskInfo:    7)
     D   dsMsk08                      1    overlay(dsMaskInfo:    8)
     D   dsMsk09                      1    overlay(dsMaskInfo:    9)
     D   dsMsk10                      1    overlay(dsMaskInfo:   10)
     D   aryMaskDisable...
     D                                1    overlay(dsMaskInfo:    1)
     D                                     dim(#ofMasks)
     D   tpMsk01                    100    overlay(dsMaskInfo:   11)
     D   tpMsk02                    100    overlay(dsMaskInfo:  111)
     D   tpMsk03                    100    overlay(dsMaskInfo:  211)
     D   tpMsk04                    100    overlay(dsMaskInfo:  311)
     D   tpMsk05                    100    overlay(dsMaskInfo:  411)
     D   tpMsk06                    100    overlay(dsMaskInfo:  511)
     D   tpMsk07                    100    overlay(dsMaskInfo:  611)
     D   tpMsk08                    100    overlay(dsMaskInfo:  711)
     D   tpMsk09                    100    overlay(dsMaskInfo:  811)
     D   tpMsk10                    100    overlay(dsMaskInfo:  911)
     D   aryMaskToolTip...
     D                              100    overlay(dsMaskInfo:   11)
     D                                     dim(#ofMasks)

     d**** Beg Add *** 05/20/24 *******************************************
     d isUsedWcMc      s              1n   inz('0')
     d**** End Add *** 05/20/24 *******************************************

     * Entry parameter list.
     c     *entry        plist
     c                   parm                    @@prds
     c                   parm                    @@call

     **************************************************************************
     *                       P R O G R A M   M A I N L I N E
     **************************************************************************
      /Free
       Exec Sql Set Option Commit = *None;

      // Intialize the screen link (Company Contact info.)
       IntializeScVal();

      // Main Procesing
       Main();

       //*** Beg Add *** 05/20/24 *******************************************
        If %open(fmc1l);
           close fmc1l;
        EndIf;
       //*** End Add *** 05/20/24 *******************************************

       *Inlr = *on;

      /End-Free

      //===================================================================
      // ClrSubFiles - Clears SubFiles
      //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
      /Free

        clrCompSfl = *on;
        dspComp = *off;

        clrVenSfl  = *on;
        dspVen  = *off;

        clrTPASfl  = *on;
        dspTPA  = *off;

        //*** Beg Add *** 05/20/24 *******************************************
        clrState  = *on;
        dspState = *off;
        //*** End Add *** 05/20/24 *******************************************
        Write RcdCompMst;

        clrCompSfl = *off;
        dspComp = *on;
        rrnCmpInf = 0;

        clrVenSfl  = *off;
        dspVen  = *on;
        rrnVenInf = 0;

        clrTPASfl  = *off;
        dspTPA  = *on;
        rrnTpaInf = 0;

        //*** Beg Add *** 05/20/24 *******************************************
        clrState  = *off;
        dspState = *on;
        rrnState = 0;
        //*** End Add *** 05/20/24 *******************************************

      /End-Free
     p ClrSubfiles     e

      //===================================================================
      // Execute Screen Record Formats.
      //===================================================================
     p Exfmtscr        b
      /Free
        //*** Beg Add *** 06/21/22 *****************************************
        If Isvisible1 = *off;  // shifting containers above by 30px.
           sctop1 -= 30;
           sctop2 -= 30;
           sctop3 -= 30;
           sctop4 -= 30;
           sctop5 -= 30;
           //*** Beg Add *** 05/20/24 *******************************************
           sctop7 -= 30;
           sctop9 -= 30;
           sctop10 -= 30;
           //*** End Add *** 05/20/24 *******************************************
        Endif;
        //*** End Add *** 06/21/22 *****************************************
        Write RcdSysTbl;
        Write RcdTabCmp;
        Exfmt RcdCompMst;
        Read RcdTabCmp;
        Read RcdSysTbl;
      /End-Free
     p Exfmtscr        e

      //===================================================================
      // IntializeScVal - Intialize the Screen fields and indicators
      //===================================================================
     p IntializeScVal  b
     d IntializeScVal  pi
     d
      /Free
       isPortal = IsModEnabled('Portal Module');
       strPortalName = GetModuleProperty('Portal Module':'Name');
       scPortHdg = strPortalName + ' Configuration';
       scPortHdgD = strPortalName;
        is1099Tcc = *Off;
        isvisible1 = *Off;
        isvisCSIOP = *Off;
        isRelayMod = *Off;
        isVisEftBt = *Off;
        isScript   = *Off;
        isCntInfVs = *Off;

        //*** Beg Add *** 06/21/22 *****************************************
        // initializing Top for container
        //*** Beg Chg ***** 09/13/24 *******************************
        //*scTop6 = 730;
        scTop6 = 760;
        //*** End Chg ***** 09/13/24 *******************************
        //*** End Add *** 06/21/22 *****************************************

        Select;
        When %parms < 2 or @@call = ' ';
           #1Mode = 'C';
        Other;
           #1Mode = 'D';
        Endsl;

       // Get the Client Id for further processing on basis of Client number.
        GetClientID(##CltId);

       // Dividend Plan Default Remark Visiblity. (Only for AHC/CSI client.)
        If ##CltId = '114';
           isvisCSIOP = *On;
           isCntInfVs = *On;
        Endif;

       // Dividend Plan Default Remark Visiblity. (Only for TASB client.)
        If ##CltId = '025';
           isVisEftBt = *On;
        Endif;

        // Portal enabled
        isTempVis = isModEnabled('Portal Module');

       // Network ID Visiblity.
        isRelayMod = isModEnabled('RelaySMS');


        Chain ('SY') ssy01;
        If not %found();
           is1099Tcc = *on;
        Else;
           If ss_sypcif = 'Y';
              is1099Tcc = *on;
              Isvisible1 = *on;
           EndIf;
           If ss_sywcif = 'Y';
              is1099Tcc = *on;
           EndIf;
        EndIf;

      // Checking the ACH module is installed or not
       Chain ('WC') swc01;
       If ss_wcFL24 = 'Y';
          isAchFlg = *on;
       Else;
          isAchFlg = *Off;
       EndIf;

       isScript = isModEnabled('Scriptura-Documents')
        or isModEnabled('Scriptura-Various')
        or isModEnabled('3rd Party XML');

       tipSLogo = 'The output logo file name must have a valid image ' +
        'file name and extension.';
       //*** Beg Add ***** 09/13/24 ************************
       tipSLogoPt = 'The output logo path must contain a valid file path ' +
        'and begin/end with a ''/'' character.';
       //*** End Add ***** 09/13/24 ************************

       If ##CltId = '150';
          vis150 = *On;
       Else;
          vis150 = *Off;
       Endif;

       vsMasking = *off;
       If IsMaskingActive('*ANY');
          vsMasking = *on;
       Endif;
       For i1 = 1 to #ofMasks;
          If %subst(aryMaskingSY(i1):1:1) = *blanks;
             aryMaskDisable(i1) = *on;
             aryMaskToolTip(i1) = 'Masking has not been enabled. ' +
              'Please contact system administrator.';
          Endif;
       Endfor;

      /End-Free
     p IntializeScVal  e

     //===================================================================
     // Load Action panel Procedure.
     //===================================================================
     p LoadActPnl      b
      /Free
     ****  Beg Add  ***  03/10/22  *********************************************
      //***  TabHeight = 1800;
        //*** Beg Chg *** 05/20/24 *******************************************
        //*** TabHeight = 1590;
        TabHeight = 2000;
        //*** End Chg *** 05/20/24 *******************************************
     ****  End Add  ***  03/10/22  *********************************************
        aPnlHeight = TabHeight + 60;

       // Load Action panel with the Company Name.
        Clear scCompName;

      /End-Free
     p LoadActPnl      e

      //===================================================================
      // Main - Main processing
      //===================================================================
     p Main            b
      /free

      //*  Process until cancel/exit button is pressed.
       Dou ExitNow = *On;

         // Clearing all the subfiles.
          ClrSubFiles();
          //*** Beg Add *** 06/21/22 *****************************************
          // Initiallizing Top for containers
          scTop1 = 610;
          scTop2 = 895;
          scTop3 = 1015;
          scTop4 = 1425;
          sctop5 = 1545;
          //*** End Add *** 06/21/22 *****************************************
          //*** Beg Add *** 05/20/24 *******************************************
          scTop10 = scTop2 - 30;
          isUsedWcMc = isModEnabled('Managed Care');
          visbWcMc   = isUsedWcMc;
          If isUsedWcMc = *on;
             scTop2 += 190;
             scTop3 += 190;
             scTop4 += 190;
             sctop5 += 190;
             scTop10 += 190;
             sctop7  = 865;
             sctop9  = 1845;
             If not %Open(fmc1l);
                Open fmc1l;
             Endif;
          Endif;
          //*** End Add *** 05/20/24 *******************************************
         // Load action panel values.
          LoadActPnl();
         // Load the Company grid.
          PrcCmpGrd();

         // Load the Company detail box,Vendor and TPA grid based on Row selected.
          If intSavCrrn > 1;
             PrcCmpDtl(intSavCrrn);
             PrcVenGrd(intSavCrrn);
             PrcTPAGrd(intSavCrrn);
          Else;
              PrcCmpDtl(intFrstRnn);
              PrcVenGrd(intFrstRnn);
              PrcTPAGrd(intFrstRnn);
          EndIf;
          //*** Beg Add *** 05/20/24 *******************************************
         // Load the WCMC State Information Grid.
          If isUsedWcMc = *on;
             PrcStGrd();
          Endif;
          //*** End Add *** 05/20/24 *******************************************

         // Execute all the record formats.
          Exfmtscr();

          Select;

            // Add Button click on Company Grid.
             When btnCompAdd = *on;
                btnCompAdd = *off;
                inrrn = 0;
                PrcAddComp(inrrn :'A');

            // Add Button click on Vendor Grid.
             When btnVenAdd = *on;
                btnVenAdd = *off;
                inrrn = 0;
                PrcAddVndr(inrrn :'A');

            // Add Button click on TPA Grid.
             When btnTPAAdd = *on;
                btnTPAAdd = *off;
                inrrn = 0;
                PrcAddTPA(inrrn :'A');

            // Other Processing on Company, Vendor and TPA Grid.
             Other;

              // Company Grid subfile change record logic
                Dou %Eof;
                  Readc sflCompInf;
                  If %eof();
                     Leave;
                  Endif;

               // Row click on company grid.
                  If BtnCmSflRw = *on;
                     BtnCmSflRw = *off;
                     intSavCrrn = CRrnAddcom;
                     Leave;
                  EndIf;

                  // Template Associations
                  If lnkTempAsc = *on;
                     lnkTempAsc = *off;
                     Reset dsWITMPLTparms;
                     dsWITMPLTparms.co# = scCmpNum;
                     TemplateAssociations(dsWITMPLTparms:'Company':#1Mode);
                  Endif;

              // Company Edit hyperlink
                  If lnkCmpEdt = *on;
                     lnkCmpEdt = *off;
                     inrrn = scDbRrn;
                     PrcAddComp(inrrn :'E');
                     Leave;
                  EndIf;

     ****  Beg Add  ***  03/10/22  *********************************************
               // Carrier Display hyperlink
                  If lnkCarrier = *on;
                     inrrn = CRrnAddcom;
                     DspCarriers(inrrn);
                     lnkCarrier = *off;
                     Leave;
                  EndIf;
     ****  End Add  ***  03/10/22  *********************************************

              // Company Contact Information hyperlink
                  If lnkCmpCont = *on;
                     lnkCmpCont = *off;
                     inComp# = scCmpNum;
                     PrcCmpCont(inComp#);
                     Leave;
                  EndIf;
               EndDo;    // Company ReadC


            // Vendor Grid subfile change record logic
               Dou %Eof;
                  Readc SflVenInfo;
                  If %eof();
                     Leave;
                  Endif;

              // Vendor Edit hyperlink
                  If lnkVndEd = *on;
                     lnkVndEd = *off;
                     inrrn = scDbVenRrn;
                     PrcAddVndr(inrrn :'E');
                     Leave;
                  EndIf;

              // Vendor Delete hyperlink
                  If lnkVndDl = *on;
                     lnkVndDl = *off;
                     inrrn = scDbVenRrn;
                     PrcAddVndr(inrrn :'D');
                     Leave;
                  EndIf;

               EndDo;    // VenDor ReadC EndDo.

            // TPA Grid subfile change record logic
               Dou %Eof;
                  Readc SflTPAInfo;
                  If %eof();
                     Leave;
                  Endif;

              // TPA Edit hyperlink
                  If lnkTPAEdt = *on;
                     lnkTPAEdt = *off;
                     inrrn = scDbTPARrn;
                     PrcAddTPA(inrrn :'E');
                     Leave;
                  EndIf;

              // TPA Change Status hyperlink
                  If lnkTPASts = *on;
                     lnkTPASts = *off;
                     inrrn = scDbTPARrn;
                     PrcAddTPA(inrrn :'S');
                     Leave;
                  EndIf;

               EndDo;    // TPA ReadC

               //*** Beg Add *** 05/20/24 *************************************
              // WCMC State Information Grid
               If isUsedWcmC = *on;
                  Dou %Eof;
                     Readc SflStInfo;
                     If %eof();
                        Leave;
                     Endif;
                     If lnkStEdt = *on;
                        lnkStEdt = *off;
                        inComp# = scCmpNum;
                        PrcStEdit(inComp#);
                        Leave;
                     EndIf;
                  EndDo;
               Endif;
               //*** End Add *** 05/20/24 *************************************
          EndSl;

       Enddo;    // ExitNow = *On;

      /End-Free
     p Main            e

      //=============================================================
      // Check for object existence in LIBL
      //=============================================================
     PObjExists        b
     dObjExists        pi             1n
     d inObj                         10    const
     d inType                         7    const

     dChkObject        pr                  extpgm('SRCHKOBJ')
     d                               10    const
     d                                7    const
     d                                1

     disError          s              1n

      /Free
       ChkObject(inObj:inType:isError);
       Return not isError;
      /End-free
     PObjExists        e

     //===================================================================
     // New Company Addition Procedure.
     //===================================================================
     p PrcAddComp      b
     d PrcAddComp      pi
     d  inRrn                         9  0
     d  inMode                        1    const
     d*
     d  isexit         s               n   Inz(*off)
     d  inSvPolRrn     s              9  0 Inz(0)
     d  inSvStRrn      s              9  0 Inz(0)
      /Free

      // Intialize the screen format/fields.
       Clear RcdCompAdd;
       BtnAddCan = *off;
       isDisComp# = *off;
       //*** begin add ***  09/25/24  *************************************
       //* Read Scriptura module file.
       Read(n) psc0p;
       isScriptDC = IsModEnabled('Design and Compose');
       //*** end add *****  09/25/24  *************************************

      // Loading intial values
       IntializeScVal();

       //*** Beg Add *** 05/20/24 *******************************************
       scTopCan    =  1030;
       scTopSav    =  1030;
       If isUsedWcMc  =  *on;
          scTopCan    =  1300;
          scTopSav    =  1300;
          //*** Beg Chg ***** 09/13/24 *******************************
          //*scTop8      =  1025;
          scTop8      =  1055;
          //*** End Chg ***** 09/13/24 *******************************
          visbWcMc    =  *on;
       Endif;
       If Isvisible1 = *off;          // shifting containers above by 55px.
          sctop6   -= 55;
          sctop8   -= 55;
          scTopCan -= 55;
          scTopSav -= 55;
       Endif;
       If IsvisCSIOP = *off;          // shifting containers above by 65px.
          sctop8   -= 65;
          scTopCan -= 65;
          scTopSav -= 65;
       Endif;
       //*** End Add *** 05/20/24 *******************************************

       Select;
      // ADD Mode.
       When inmode = 'A';
          scWcmsbill = 'N';
          scSepQueue = 'N';
          scPss#     = 'N';
          scMaIntiDt = 'N';
          scMaintNew = 'N';
          scErSd     = 'N';
          dsMaskingCO = dsMaskingSY;
          For i1 = 1 to 35*#ofMasks by 35;
             %subst(dsMaskingCO:i1:5) = *blanks;
          Endfor;

       // EDIT Mode.
       When inmode = 'E';
          isDisComp# = *on;
          Exsr MoveDbSc;
       Endsl;

       aryMaskCO = aryMaskingCO;

      // Looping for the popup window untill cancel button and exit is Off.
       Dow BtnAddCan = *off and isexit = *off;

          //*** Beg Del *** 05/20/24 *******************************************
          //*** Beg Add *** 06/21/22 *****************************************
          //*** If Isvisible1 = *off;          // shifting containers above by 55px.
          //***    sctop6 -= 55;
          //*** Endif;
          //*** End Add *** 06/21/22 *****************************************
          //*** End Del *** 05/20/24 *******************************************

          Select;
         // ADD Mode.
          When inmode = 'A';
             Exfmt RcdCompAdd;
             Exsr ProcessCompany;

         // EDIT Mode.
          When inmode = 'E';
             Exfmt RcdCompAdd;
             Exsr ProcessCompany;

          Other;
          Endsl;
       EndDo;

       BegSr ProcessCompany;
         Select;
        // ADD Mode.
         When BtnAddSav = *On and inmode = 'A';
            BtnAddSav =*off;
            If ValidateCmpny('A');
               Exsr WrtCompany;
               //*** Beg Add *** 05/20/24 *******************************************
               Exsr WrtUpdFMC1P;
               //*** End Add *** 05/20/24 *******************************************
               Exsr UpdateSMCIP;
               Exsr UpdMasking;
               isexit = *on;
            EndIf;

        // EDIT Mode.
         When BtnAddSav = *On and inmode = 'E';
            BtnAddSav =*off;
            If ValidateCmpny('E');
               Exsr UpdCompany;
               //*** Beg Add *** 05/20/24 *******************************************
               Exsr WrtUpdFMC1P;
               //*** End Add *** 05/20/24 *******************************************
               Exsr UpdateSMCIP;
               Exsr UpdMasking;
               isexit = *on;
            EndIf;

         Other;
         EndSl;

       EndSr;

      // Writing new company subroutine.
       BegSr WrtCompany;
        Clear pco0p;
        Exsr MoveScDb;
        Write pco0p;

        Exsr WrtWCPOLS;
        Exsr WrtWCSTAT;

       EndSr;

       //===============================================================
       // UpdateSMCIP - Update/Write records to SMCIP
       //===============================================================
       Begsr UpdateSMCIP;
        If dsSMCIP = dsSMCIPsave;
           Leavesr;
        Endif;
        dsSMCIP.CIuser = COuser;
        dsSMCIP.CIpgm  = COpgm;
        dsSMCIP.CIdt   = COdt;
        dsSMCIP.CItime = COtime;
        If dsSMCIP.CIco# <> *zero;
           Exec sql
            update SMCIP
             set row = :dsSMCIP
             where CIco# = :dsSMCIP.CIco#;
        Else;
           dsSMCIP.CIco#  = COco#;
           dsSMCIP.CIiUsr = COuser;
           dsSMCIP.CIiPgm = COpgm;
           dsSMCIP.CIiDt  = COdt;
           dsSMCIP.CIiTim = COtime;
           Exec sql
            insert into SMCIP
             values(:dsSMCIP);
        Endif;
       Endsr;

      // Editing/Updatng company subroutine.
       BegSr UpdCompany;

       // Checking the company record.
        Chain inRrn pco0p;  // Exclusive lock on the company record.
        Exsr MoveScdb;
        Update pco0p;
        Unlock smcop;

       EndSr;

       //===============================================
       BegSr UpdMasking;
        For i1 = 1 to #ofMasks;
           If aryMaskProcess(i1) <> *blank;
              //SYMASK(%char(scComp#):aryMasks(i1):aryMaskProcess(i1):dsLibFile);
           Endif;
        Endfor;
       EndSr;
       //===============================================

      // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        COco#   = scComp#;               // Company Number
        COname  = scCmpNm;               // Company Name
        COadr1  = scCmPhyAd1;            // Address Line 1
        COadr2  = scCmPhyAd2;            // Address Line 2
        COcty   = scCmPhyCty;            // City
        COst    = scCmPhySt;             // State
        COzip   = scCmPhyZip;            // Zip Code
        COfein  = scFein;                // Federal Tax Id
        CO1099  = sc1099;                // 1099 Minimum Amount
        COstat  = *blanks;               // Status

        COuser  = q1user;                // User
        COpgm   = q1pgm;                 // Program
        COtime  = Timeto6(SystemTime()); // Time
        COdt    = Dateto7(SystemDate()); // Date Maintained
        COiUsr  = q1user;                // Initial User
        COiDt   =Dateto7(SystemDate());  // Initial Date

        If scCmPhone > *zeros;
           COarea  = %dec(%subst(%editc(scCmPhone:'X'):1:3):3:0); // Area Code
           COphn#  = %dec(%subst(%editc(scCmPhone:'X'):4):7:0);  // Phone Number
        EndIf;

        If scCmFax > *zeros;
           COfAra  = %dec(%subst(%editc(scCmFax:'X'):1:3):3:0); // FAX AREA CODE
           COfax#  = %dec(%subst(%editc(scCmFax:'X'):4):7:0);   // FAX PHONE #
        EndIf;

        COaNam  = scCmpEFTnm;            // Admin Co Name
        COshNm  = scCmpShrt;             // Co#  Short Name
        COmAd1  = scCmpMlAd1;            // Mailing Address 1
        COmAd2  = scCmpMlAd2;            // Mailing Address 2
        COmCty  = scCmpMlCty;            // Mailing City
        COmSt   = scCmpMlSt;             // Mailing State
        COmZip  = scCmpMlZip;            // Mailing Zip Code

        COpAd1  = scCmPhyAd1;            // Physical Address 1
        COpAd2  = scCmPhyAd2;            // Physical Address 2
        COpCty  = scCmPhyCty;            // Physical City
        COpSt   = scCmPhySt;             // Physical State
        COpZip  = scCmPhyZip;            // Physical Zip Code
        //*** Beg Add ***** 05/08/25 **************************
        COmail  = scCmpEmlAd;            // Company Email
        //*** End Add ***** 05/08/25 **************************

        dsMaskingCO = dsMaskingSC;

        If scTollph > *zeros;
           COph1a  = %dec(%subst(%editc(scTollph:'X'):1:3):3:0); // Area Code
           COph1  = %dec(%subst(%editc(scTollph:'X'):4):7:0);  // Phone Number
        EndIf;

        If scLocalph > *zeros;
           COarea  = %dec(%subst(%editc(scLocalph:'X'):1:3):3:0); // Area Code
           //*** Beg Chg ***** 11/29/22 *************************
           //*** COphn#  = %dec(%subst(%editc(scLocalph:'X'):4):7:0);  // Phone Number
           COph2  = %dec(%subst(%editc(scLocalph:'X'):4):7:0);  // Phone Number
           //*** End Chg ***** 11/29/22 *************************
        EndIf;

        COchk1  = scChk1;                // From Check Nbr

        COchk2  = scChk2;                // To Check Nbr
        COnacs  = scCase;                // Next Avail Case Number
        COcoi   = scCoi;                 // Assigned Co# Iso
        COiso   = scIso;                 // Iso Transaction Type
        COtcc   = scTcc;                 // Transmitter Control Code
        COtrdl  = ScTrdl;                // Trans Cd Deletion

        CObflg = scWcmsbill;             // SEPARATE JOBQ FLAG
        COsRpl = scSrpl;                 // SR batch security Level
        COsRpt = scSrpt;                 // SR batch security Type

        COcart = scCart;                 // CA Pure Rate Cd
        COpss# = scPss#;                 // Print SS# on checks
        COarSq = scArSq;                 // A/R Batch Edit/ Tranf Rep
        COrfBs = scOrFbs;                // Return Fund Batch Sort
        //*** Beg Add ***** 10/13/22 *************************
        COfedrc= scPrFdNm;               //Print Federal Name on Refund checks
        //*** End Add ***** 10/13/22 *************************
        COsyCd = scSyCd;                 // A/R BATCH SYS CD
        COarCd = scArcd;                 // PREMIUM PAY CD
        COecAr = scEcar;                 // EXP CON PAY CD
        COifAr = scIfar;                 // INSTALL FEE PAY CD
        COmfAr = scMfar;                 // MEMBER FEE PAY CD

        COsfAr = scSfar;                 // SAFETY FEE PAY CD
        COtxAr = scTxar;                 // PREMIUM TAX PAY CD
        CO1aAr = sc1Aar;                 // 1ST ASST PAY CD
        CO2aAr = sc2Aar;                 // 2ST ASST PAY CO
        CO3aAr = sc3Aar;                 // 3ST ASST PAY
        CO4aAr = sc4Aar;                 // 4ST ASST PAY CD
        CO5aAr = sc5Aar;                 // 5ST ASST PAY CD
        CO6aAr = sc6Aar;                 // 6ST ASST PAY CD
        CO7aAr = sc7Aar;                 // 7ST ASST PAY CD
        COachl = scAchl;                 // ACH BATCH SEC LVL

        COacht = scAcht;                 // ACH BATCH SEC TYP
        COnmad = scNmad;                 // PROV. NAME/ADDRESS CHANGE
        COfySm = scFySm;                 // FISCAL YR STR MTH
        COckFe = scCkFe;                 // Print checks in FEIN orde
        CObrnd = ' ';                    // COMPANY BRAND NAME
        COpflg = scSepQueue;             // Sep policy page jobq
        COpltr = scDivPlnLt;             // Div Plan Letter
        COprth = scDivPrmTh;             // Div Prem Thrshld
        COcapd = scActDart;              // Clm Action Plan Dart
        COerSd = scErSd;                 // Exclude Rsv CD in  Dup Py

        COmaintIdt = scMaIntiDt;         // Emp copy - Maintain  Ince
        COmaintNew = scMaintNew;         // Emp copy - Maintain  New/
        COrcdI     = ' ';                // Require CMS DOI >=  Claim
        CONETID  = scNetID;              // Relay Network ID
        COSLOGO  = scSLogo;              // SCRIPTURA LOGO NAME
        //*** Beg Add ***** 09/13/24 *******************************
       //*** begin chg ***  09/25/24  *************************************
       //***** COSPATH  = scSLogoPth;           // SCRIPTURA LOGO PATH
        COSURL = scSLogoURL;
        COSPath = scSLogoPth;
       //*** end chg *****  09/25/24  *************************************
        //*** End Add ***** 09/13/24 *******************************

        dsSMCIP.CIasOf = DtCYMD(scAsOfP);
        dsSMCIP.CIdays = scDaysP;

       EndSr;

      // Subroutine to move database fields into Screen fields.
       BegSr MoveDbSc;
        Chain(n) inRrn pco0p;
        scComp#    = COco#;              // Company Number
        scCmpNm    = COname;             // Company Name
        scCmPhyAd1 = COpAd1;             // Address Line 1
        scCmPhyAd2 = COpAd2;             // Address Line 2
        scCmPhyCty = COpCty;              // City
        scCmPhySt  = COpst;               // State
        scCmPhyZip = COpZip;              // Zip Code
        scFein     = COfein;             // Federal Tax Id
        sc1099     = CO1099;             // 1099 Minimum Amount

        If COarea <> *zeros and COphn# <> *zeros;
           scCmPhone = %dec(%char(COarea) + %char(COphn#):10:0);
        EndIf;

        If COfAra <> *zeros and COfax# <> *zeros;
           scCmFax = %dec(%char(COfAra) + %char(COfax#):10:0);
        EndIf;

        scCmpEFTnm = COaNam;             // Admin Co Name
        scCmpShrt  = COshNm;             // Co#  Short Name
        scCmpMlAd1 = COmAd1;             // Mailing Address 1
        scCmpMlAd2 = COmAd2;             // Mailing Address 2
        scCmpMlCty = COmCty;             // Mailing City
        scCmpMlSt  = COmSt;              // Mailing State
        scCmpMlZip = COmZip;             // Mailing Zip Code

        scCmPhyAd1 = COpAd1;             // Physical Address 1
        scCmPhyAd2 = COpAd2;             // Physical Address 2
        scCmPhyCty = COpCty;             // Physical City
        scCmPhySt  = COpSt;              // Physical State
        scCmPhyZip = COpZip;             // Physical Zip Code
        //*** Beg Add ***** 05/08/25 **************************
        scCmpEmlAd = COmail;             // Company Email
        //*** End Add ***** 05/08/25 **************************

        dsMaskingSC = dsMaskingCO;

        If COph1a <> *zeros and  COph1 <> *zeros;
           scTollph = %dec(%char(COph1a) + %char(COph1):10:0);
        EndIf;

        If COarea <> *zeros and COph2 <> *zeros;
           scLocalph = %dec(%char(COarea) + %char(COph2):10:0);
        EndIf;

        scChk1 = COchk1;                 // From Check Nbr

        scChk2  =  COchk2;               // To Check Nbr
        scCase  =  COnacs;               // Next Avail Case Number
        scCoi   =  COcoi;                // Assigned Co# Iso
        scIso   =  COiso;                // Iso Transaction Type
        scTcc   =  COtcc;                // Transmitter Control Code
        scTrdl  =  COtrdl;               // Trans Cd Deletion

        scWcmsbill =  CObflg;            // SEPARATE JOBQ FLAG
        scSrpl     =  COsRpl;            // SR batch security Level
        scSrpt     =  COsRpt;            // SR batch security Type

        scCart     =  COcart;            // CA Pure Rate Cd
        scPss#     =  COpss#;            // Print SS# on checks
        scArSq     =  COarSq;            // A/R Batch Edit/ Tranf Rep
        scOrFbs    =  COrfBs;            // Return Fund Batch Sort
        //*** Beg Add ***** 10/13/22 *************************
        scPrFdNm   =  COfedrc;           //Print Federal Name on Refund checks
        //*** End Add ***** 10/13/22 *************************
        scSyCd     =  COsyCd;            // A/R BATCH SYS CD
        scArcd     =  COarCd;            // PREMIUM PAY CD
        scEcar     =  COecAr;            // EXP CON PAY CD
        scIfar     =  COifAr;            // INSTALL FEE PAY CD
        scMfar     =  COmfAr;            // MEMBER FEE PAY CD

        scSfar     =  COsfAr;            // SAFETY FEE PAY CD
        scTxar     =  COtxAr;            // PREMIUM TAX PAY CD
        sc1Aar     =  CO1aAr;            // 1ST ASST PAY CD
        sc2Aar     =  CO2aAr;            // 2ST ASST PAY CO
        sc3Aar     =  CO3aAr;            // 3ST ASST PAY
        sc4Aar     =  CO4aAr;            // 4ST ASST PAY CD
        sc5Aar     =  CO5aAr;            // 5ST ASST PAY CD
        sc6Aar     =  CO6aAr;            // 6ST ASST PAY CD
        sc7Aar     =  CO7aAr;            // 7ST ASST PAY CD
        scAchl     =  COachl;            // ACH BATCH SEC LVL

        scAcht     = COacht;             // ACH BATCH SEC TYP
        scNmad     = COnmad;             // PROV. NAME/ADDRESS CHANGE
        scFySm     = COfySm;             // FISCAL YR STR MTH
        scCkFe     = COckFe;             // Print checks in FEIN orde

        scSepQueue = COpflg;             // Sep policy page jobq
        scActDart  = COcapd;             // Clm Action Plan Dart
        scErSd     = COerSd;             // Exclude Rsv CD in  Dup Py scErSd;

        scMaIntiDt = COmaintIdt;         // Emp copy - Maintain  Ince
        scMaintNew = COmaintNew;         // Emp copy - Maintain  New/

        scNetID   = COnetId;            // Relay Network ID
        scSLogo   = COsLogo;            // SCRIPTURA LOGO NAME
        //*** Beg Add ***** 09/13/24 *******************************
        scSLogoPth = COsPath;            // SCRIPTURA LOGO PATH
        //*** End Add ***** 09/13/24 *******************************
       //*** begin add ***  09/25/24  *************************************
        scSLogoURL  = COSURL;
       //*** end add *****  09/25/24  *************************************

        Clear scAsOfP;
        Clear scDaysP;
        Reset dsSMCIP;
        Exec sql
         select * into :dsSMCIP from SMCIP
          where CIco# = :scComp#;
        dsSMCIPsave = dsSMCIP;
        If IsValidFetch(sqlstt);
           scAsOfP = DtMDYY(dsSMCIP.CIasOf);
           scDaysP = dsSMCIP.CIdays;
        Endif;

        scDivPlnLt = COpltr;            // Div Plan Letter
        Chain scDivPlnLt pct00;
        If %found(stctl1);
           scPlnLtDes = CTdesc;
        Endif;

        scDivPrmTh = COprth;            // Div Prem Thrshld
        Chain scDivPrmTh pct00;
        If %found(stctl1);
           scPrmThDes = CTdesc;
        Endif;

      // WCPOLS Transmittal Default

        //*** Beg Add *** 05/20/24 *******************************************
        If isUsedWcMc  =  *on;
           Chain(n) COco# pc101;
           If %found(Fmc1l);

              If C1name = *blanks;
                 scMCCmpNmE =  COname;
              Else;
                 scMCCmpNmE =  C1name;
              Endif;

              IF C1Adr1  = *blanks;
                 scMCCAd1E  = COAdr1;            // Address Line 1
                 scMCCAd2E  = COAdr2;            // Address Line 2
                 scMCPhyCty = COCty;             // City
                 scMCPhySt  = COst;              // State
                 scMCPhyZip = COZip;             // Zip Code
              Else;
                 scMCCAd1E  = C1Adr1;            // Address Line 1
                 scMCCAd2E  = C1Adr2;            // Address Line 2
                 scMCPhyCty = C1Cty;             // City
                 scMCPhySt  = C1st;              // State
                 scMCPhyZip = C1Zip;             // Zip Code
              Endif;

              If C1area = *zeros or C1phn# = *zeros;
                 scMCPhoneE = %dec(%char(COarea) + %char(COphn#):10:0);
              Elseif C1area <> *zeros or C1phn# <> *zeros;
                 scMCPhoneE = %dec(%char(C1area) + %char(C1phn#):10:0);
              Endif;

              If C1fara = *zeros or C1fax# = *zeros;
                 scMCFaxE = %dec(%char(COfAra) + %char(COfax#):10:0);
              Elseif C1fara <> *zeros or C1fax# <> *zeros;
                 scMCFaxE = %dec(%char(C1fAra) + %char(C1fax#):10:0);
              Endif;

              If C1fein = *zeros;
                 scMCFeinE =  COfein;
              Else;
                 scMCFeinE =  C1fein;
              Endif;

              Monitor;
                 scTNSubIDE  =  %dec(C110a3:10:0);
              On-Error;
                 scTNSubIDE  =  *zeros;
              EndMon;

              Monitor;
                 scNCCICdE   =  %dec(%subst(C110a4:6:5):5:0);
              On-Error;
                 scNCCICdE   =  *zeros;
              EndMon;

              If C1lcs# <= 0;
                 scCaseNoE =  Cocase;
              Else;
                 scCaseNoE =  C1lcs#;
              Endif;

              If C1eob# <= 0;
                 scEOMB#E = *zeros;
              Else;
                 scEOMB#E = C1eob#;
              Endif;

              If C1lin# <= 0;
                 scInvNoE = COinv#;
              Else;
                 scInvNoE = C1lin#;
              Endif;

              If C1ltr# <= 0;
                 scLLettNoE = *zeros;
              Else;
                 scLLettNoE = C1ltr#;
              Endif;

              sc837IDE = C1tsetID#;
              sc835IDE = C1raid;
           Else;
              scMCCmpNmE =  COname;
              scMCCAd1E  = COAdr1;            // Address Line 1
              scMCCAd2E  = COAdr2;            // Address Line 2
              scMCPhyCty = COCty;             // City
              scMCPhySt  = COst;              // State
              scMCPhyZip = COZip;             // Zip Code

              If COarea = *zeros or COphn# = *zeros;
                 scMCPhoneE = %dec(%char(COarea) + %char(COphn#):10:0);
              Endif;

              If COfara = *zeros or COfax# = *zeros;
                 scMCFaxE = %dec(%char(COfAra) + %char(COfax#):10:0);
              Endif;

              scMCFeinE = COfein;
              scTNSubIDE = *zeros;
              scNCCICdE  = *zeros;

              If COcase <= 0;
                 scCaseNoE = *zeros;
              Else;
                 scCaseNoE =  COcase - 1;
              Endif;

              If COinv# <= 0;
                 scInvNoE = *zeros;
              Else;
                 scInvNoE = COinv# - 1;
              Endif;

              scEOMB#E = *zeros;
              scLLettNoE = *zeros;
              sc837IDE = *zeros;
              sc835IDE =  *zeros;
           Endif;
        Endif;
        //*** End Add *** 05/20/24 *******************************************
       EndSr;

       BegSr WrtWCPOLS;
      // WCPOLS Transmittal Default
       EndSr;

       BegSr WrtWCSTAT;
      // WCSTAT Transmittal Default
       EndSr;

       //*** Beg Add *** 05/20/24 *******************************************
       //===============================================================
       // WrtUpdFMC1P - Update/Write records to FMC1P
       //===============================================================
       Begsr WrtUpdFMC1P;
        If isUsedWcMc  =  *on;
           Chain COco# pc101;
           If (inMode = 'E' and not %found(fmc1l)) or inMode = 'A';
              Clear pc101;
           Endif;
           C1co#   =  COco#;
           C1stat  =  'A';
           C1name  =  scMCCmpNmE;
           C1Adr1  =  scMCCAd1E;
           C1Adr2  =  scMCCAd2E;
           C1Cty   =  scMCPhyCty;
           C1st    =  scMCPhySt;
           C1Zip   =  scMCPhyZip;
           If scMCPhoneE > *zeros;
              C1area  = %dec(%subst(%editc(scMCPhoneE:'X'):1:3):3:0); // Area Code
              C1phn#  = %dec(%subst(%editc(scMCPhoneE:'X'):4):7:0);  // Phone Number
           EndIf;

           If scMCFaxE > *zeros;
              C1fAra  = %dec(%subst(%editc(scMCFaxE:'X'):1:3):3:0); // FAX AREA CODE
              C1fax#  = %dec(%subst(%editc(scMCFaxE:'X'):4):7:0);   // FAX PHONE #
           EndIf;

           Monitor;
              C110a3  =   %subst('0000000000': 1: 10 - %len(%char(scTNSubIDE)))
                   +  %char(scTNSubIDE);                            //TN Submission ID
           On-Error;
              C110a3     = *blanks;
           EndMon;

           C110a4     = '     ' + %editc(scNCCICdE:'X');            //NCCI Carrier Grp.Cd
           C1lcs#     =   scCaseNoE;                                //Standalone last case number
           C1eob#     =   scEOMB#E;                                 //Last EOMB number used
           C1lin#     =   scInvNoE;                                 //Last invoice number used
           C1ltr#     =   scLLettNoE;                               //Last letter number used
           C1tsetID#  =   sc837IDE;                                 //Last 837 EDI transaction set
           C1raid     =   sc835IDE;                                 //Last 835 ASC X12 Payment Ad
           C1fein     =   scMCFeinE;                                //Fed ID

           If inMode  = 'E' and %found(fmc1l);
              C1user  = q1user;                                     // User Maintained
              C1pgm   = q1pgm;                                      // Program Maintained
              C1time  = Timeto6(SystemTime());                      // Time Maintained
              C1dt    = Dateto7(SystemDate());                      // Date Maintained
              Update pc101;
           Else;
              C1user  = q1user;                                     // User Maintained
              C1pgm   = q1pgm;                                      // Program Maintained
              C1time  = Timeto6(SystemTime());                      // Time Maintained
              C1dt    = Dateto7(SystemDate());                      // Date Maintained
              C1iUsr  = q1user;                                     // Initial User
              C1iDt   = Dateto7(SystemDate());                      // Initial Date
              C1ipgm  = q1pgm;                                      // Initial Program
              C1itim  = Timeto6(SystemTime());                      // Initial Time
              Write pc101;
           Endif;
        Endif;
       EndSr;
       //*** End Add *** 05/20/24 *******************************************

      /End-Free
     p PrcAddComp      e

     //===================================================================
     // Vendor Addition procedure.
     //===================================================================
     p PrcAddVndr      b
     d PrcAddVndr      pi
     d  inRrn                         9  0
     d  inMode                        1    const
 
     d  isexit         s               n   inz(*off)
     d  inVenCmp#      s              3  0 inz(0)
      /Free
       Clear RcdVenAdd;
       BtnVenCan = *off;
       isDisVenCd = *off;

       If ##CltId = '150';
          vis150 = *On;
       Else;
          vis150 = *Off;
       Endif;

      // Selection criteria building.
       selPrPyCd = 'TTCTYP = ''P'' ' + ' and TTTYPE = ''P'' ' +
        ' and TTSTAT = ''A'' ' + ' and TTCO# = ' + %char(scCmpNum);

       selCmPyCd = 'TTCTYP = ''P'' ' + ' and TTTYPE = ''P'' ' +
        ' and TTEXDS = ''Y'' ' +
        ' and TTSTAT = ''A'' ' + ' and TTCO# = ' + %char(scCmpNum);
       selPrBlCd = 'TTCTYP = ''P'' ' + ' and TTTYPE = ''B'' ' +
        ' and TTSTAT = ''A'' ' + ' and TTCO# = ' + %char(scCmpNum);

       Select;
      // ADD Mode.
       when inmode = 'A';
          inVenCmp# = scCmpNum;
          scBtnVenTx = 'Save';
          scVenHdTxt = 'Add Vendor Information';

      // EDIT Mode.
       when inmode = 'E';
          isDisVenCd = *on;
          ExSr MoveDbSc;
          scBtnVenTx = 'Save';
          scVenHdTxt = 'Edit Vendor Information';

      // DELETE Mode.
       when inmode = 'D';
          isDisVenCd = *on;
          isDisabled = *on;
          ExSr MoveDbSc;
          scBtnVenTx = 'Delete';
          scVenHdTxt = 'Delete Vendor Information';
       EndSl;

      // Looping for the popup window untill cancel button and exit is Off.
       Dow BtnVenCan = *off And isExit = *off;
          Select;
         // ADD Mode.
          When inmode = 'A';
             Exfmt RcdVenAdd;
             Exsr ProcessVendor;

         // EDIT Mode.
          When inmode = 'E';
             Exfmt RcdVenAdd;
             Exsr ProcessVendor;

         // DELETE Mode.
          When inmode = 'D';
             Exfmt RcdVenAdd;
             Exsr ProcessVendor;

          Other;
          Endsl;
       EndDo;

       BegSr ProcessVendor;
        Select;

       // ADD Mode.
        When BtnVenSav = *On And inmode = 'A';
           BtnVenSav =*off;
           If ValidateVndr('A');
              Exsr WriteVendor;
              isexit = *on;
           EndIf;

       // EDIT Mode.
        When BtnVenSav = *On And inmode = 'E';
           BtnVenSav =*off;
           If ValidateVndr('E');
              Exsr UpdateVendor;
              isexit = *on;
           EndIf;

       // DELETE Mode.
        When BtnVenSav = *On And inmode = 'D';
           BtnVenSav =*off;
           Chain inrrn pvp0p;  // to get exclusive record lock.
           Delete pvp0p;
           isexit = *on;
        other;
        EndSl;
       EndSr;

      // Writing new vendor subroutine.
       BegSr WriteVendor;
        Clear pvp0p;

        VPCo# =  inVenCmp#;      // COMPANY NUMBER
        ExSr MoveScDb;
        VPuser = q1user;
        VPpgm  = q1pgm;
        VPdt   = Dateto7(SystemDate());
        VPtime = Timeto6(SystemTime());
        VPiusr = q1user;
        VPipgm = q1pgm;
        VPidt  = Dateto7(SystemDate());
        VPitm  = Timeto6(SystemTime());

        Write pvp0p;
       EndSr;

      // Updating/Editing vendor subroutine.
       BegSr UpdateVendor;
        Chain inrrn pvp0p;  // to get exclusive record lock.
        ExSr MoveScDb;
        VPuser = q1user;
        VPpgm  = q1pgm;
        VPdt   = Dateto7(SystemDate());
        VPtime = Timeto6(SystemTime());
        Update pvp0p;
        Unlock wmvpp;
       EndSr;

      // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        VPvdr# = scVdr#;        // VENDOR #
        VPname = scVenNam;      // VENDOR NAME
        VPadr1 = scVenAd1;      // ADDRESS LINE ONE
        VPadr2 = scVenAd2;      // ADDRESS LINE TWO
        VPcity = scVenCty;      // VENDOR CITY
        VPst   = scVenSt;       // VENDOR STATE
        VPzip  = scVenZip;      // VENDOR ZIP
        VPefDt = DateTo7(cvtToDate(scVenEfDat));    // EFF DATE
        VPexDt = DateTo7(cvtToDate(scVenExpDt));    // EXP DATE
        VPiAdr = scVenIntr;     // INTERNET ADDRESS
        VPemal = scVenEml;      // E-MAIL ADDRESS
        VPpyFr = scPayFrq;      // PAYROLL UPLOAD FREQ
        VPpmFr = scPmFr;        // PREM UPLOAD FREQ
        VPdnLf = scDnLf;        // VENDOR DOWNLOAD FREQ
        VPblnd = scBlNd;        // BLEND RATE ON ARD FLAG
        VPnet  = scNet;         // NET RATE FLAG
        VPfTyp = scFTyp;        // FILE TYPE INTERNAL TRACKING
        VPdTyp = scDateTyp;     // DATA TYPE INTERNAL TRACKING

        VPexnp = scExnp;        // INCLUDE NON-PREMIUM BEARING ENDORSEMENTS
        VPexal = scExal;        // INCLUDE ADDITIONAL LOCATIONS/DIVISIONS
        VPdFmt = scDfmt;        // DATE FORMAT INTERNAL TRACKING
        VPpPcd = scPrPyCd;      // PREMIUM PAYMENT'A/R CODE
        VPcPcd = scCmPyCd;      // COMMISSION PAYMENT'A/R CODE
        VPpBcd = scPrBlCd;      // PREMIUM BILLING CODE
        VPautr = scTranBill;    // Auto-transfer billing batch (Y/N)
        VPpyUp = scPyup;        // Generate A/R from Payroll upload
        VPmThi = scMthIn;       // INCLUDE IN MONTHLY INTERIM
        VPpTyc = scPtyc;        // P-TYPE CONTROL FLAG
        VPEMPREQ = scEmpReq;    // Employee Information Required
        VPGENPRM = scGenPrmNt;  // Generate Premium Notice
        VP#OFDAYS= scDaysCkIs;  // # of days for Check Issuance
        VPtranPay = scTranPay;  // Auto-transfer payment batch (Y/N)
        VPWrtColl  = scWrtColl;  // Write Collateral A/R
        VPAuTrCBil = scAuTrCBil; // Auto Transfer Collateral Billings
        VPAuTrCPay = scAuTrCPay; // Auto Transfer Collateral Payments
       EndSr;


      // Subroutine to move database fields into Screen fields.
       BegSr MoveDbSc;
        Chain(n) inrrn pvp0p;

        scVdr#    =  VPvdr#;
        scVenNam  =  VPname;
        scVenAd1  =  VPadr1;
        scVenAd2  =  VPadr2;
        scVenCty  =  VPcity;
        scVenSt   =  VPst;
        scVenZip  =  VPzip;
        scVenEfDat = dtMDYY(VPefdt);
        scVenExpDt = dtMDYY(VPexdt);

        scVenIntr =  VPiadr;
        scVenEml  =  VPemal;
        scPayFrq  =  VPpyfr;
        scPmFr    =  VPpmfr;
        scDnLf    =  VPdnlf;
        scBlNd    =  VPblnd;
        scNet     =  VPnet;
        scFTyp    =  VPftyp;
        scDateTyp =  VPdtyp;
        scExnp    =  VPexnp;
        scExal    =  VPexal;
        scDfmt    =  VPdfmt;
        scPrPyCd  =  VPppcd;
        scCmPyCd  =  VPcpcd;
        scPrBlCd  =  VPpbcd;
        scTranBill =  VPautr;
        scTranPay =  VPTranPay;
        scPyup    =  VPpyup;
        scMthIn   =  VPmthi;
        scPtyc    =  VPptyc;
        scEmpReq  = VPEMPREQ;
        scGenPrmNt= VPGENPRM;
        scDaysCkIs= VP#OFDAYS;
        scTranPay =  VPTranPay;
        scWrtColl  = VPWrtColl;  // Write Collateral A/R
        scAuTrCBil = VPAuTrCBil; // Auto Transfer Collateral Billings
        scAuTrCPay = VPAuTrCPay; // Auto Transfer Collateral Payments
       EndSr;
      /End-Free

     p PrcAddVndr      e

      //===================================================================
      // TPA (Third Party) Addition procedure.
      //===================================================================
     p PrcAddTPA       b
     d PrcAddTPA       pi
     d  inRrn                         9  0
     d  inMode                        1    const
     d
     d  isexit         s               n   inz(*off)
      /Free
      // Intialize the screen indicators and record format.
       Clear RcdTpaAdd;
       BtnTPACan = *off;
       isDisTPACd = *off;

      // Protected the TPA code in case od edit and change status.
       If inmode = 'E' or inmode = 'S';
          isDisTPACd = *on;
          ExSr MoveDbSc;
       Endif;

      // Looping for the popup window untill cancel button and exit is Off.
       Dow BtnTPACan = *off And isExit = *off;
          Select;
         // ADD mode
          When inmode = 'A';
             Exfmt RcdTPAAdd;
             Exsr VenProcess;

         // Edit mode
          When inmode = 'E';
             Exfmt RcdTPAAdd;
             Exsr VenProcess;

         // Change Status mode
          When inmode = 'S';
             Exsr ChgTPAStatus;
             isexit = *on;

          Other;

          Endsl;
       EndDo;

       BegSr VenProcess;
        Select;

       // ADD mode
        When BtnTPASav = *On And inmode = 'A';
           BtnTPASav =*off;
           If ValidateTPA('A');
              Exsr WriteTPA;
              isexit = *on;
           EndIf;

       // EDIT mode
        When BtnTPASav = *On And inmode = 'E';
           BtnTPASav =*off;
           If ValidateTPA('E');
              Exsr UpdateTPA;
              isexit = *on;
           EndIf;

        other;
        EndSl;
       EndSr;

      // Writing new TPA subroutine.
       BegSr WriteTPA;
        Clear ptc0p;
        ExSr MoveScDb;
        Write ptc0p;
       EndSr;

       BegSr UpdateTPA;
        Chain inrrn ptc0p;  // Get exclusive record lock.
        ExSr MoveScdb;
        Update ptc0p;
        Unlock wmtcp;
       EndSr;

      // TPA Change status subroutine.
       BegSr ChgTPAStatus;
        Chain inrrn ptc0p;  // Get exclusive record lock.
        If TCstat = 'A';
           TCstat = 'I';        // Status - InActive
        Elseif TCstat = 'I';
           TCstat = 'A';        // Status - Active.
        EndIf;

        TCuser = q1user;                 // Maint User
        TCpgm  = q1pgm;                  // Maint PGM
        TCdt   = Dateto7(SystemDate());  // Maint DAT
        TCtime = Timeto6(SystemTime());  // Maint Time


        Update ptc0p %fields(TCuser:TCpgm:TCdt:TCtime:TCstat);
        Unlock wmtcp;
       EndSr;

      // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        TCco#  = scCmpNum;   // CO#
        TCtpaC = scTpaCd;    // TPA Code
        TCdesc = scTpaDsc;   // TPA Description
        TCip   = scTpaIp;    // Site IP
        TCfldr = scTpaStFl;  // Site Folder
        TCusId = scTpaUsId;  // Site User ID
        TCSpsw = scTpaPwd;   // Site Password
        TCstat = 'A';        // Status
        TCpath = fullPathName;  // Export file Path Name
        TCatgn = scTAutoGen; // Auto generate policy export
        TCeprm = scTExcPrm; // Exclude premium
        TCeprl = scTExcPyrl; // Exclude payroll
        TCnbdv = scIncNBDiv;
        TCenrl = scExcNrLoc;

        select;
        when  inmode = 'A';
           TCstat = 'A';        // Status
           TCuser = q1user;                 // Maint User
           TCpgm  = q1pgm;                  // Maint PGM
           TCdt   = Dateto7(SystemDate());  // Maint DAT
           TCtime = Timeto6(SystemTime());  // Maint Time
           TCiUsr = q1user;                 // Initial User
           TCiPgm = q1pgm;                  // Initial PGM
           TCiDt  = Dateto7(SystemDate());  // Initial DAT
           TCiTim = Timeto6(SystemTime());  // Initial Time
        when  inmode = 'E' or inmode = 'S';
           TCUser = q1user;                 // Maint User
           TCPgm  = q1pgm;                  // Maint PGM
           TCDt   = Dateto7(SystemDate());  // Maint DAT
           TCTime = Timeto6(SystemTime());  // Maint Time
        EndSl;
       EndSr;

      // Subrotine to move Database fields into screen fields.
       BegSr MoveDbSc;

        Chain(n) inrrn ptc0p;
        If %found(wmtcp);
           scCmpNum  = TCco#;   // CO#
           scTpaCd   = TCtpaC;  // TPA Code
           scTpaDsc  = TCdesc;  // TPA Description
           scTpaIp   = TCip;    // Site IP
           scTpaStFl = TCfldr;  // Site Folder
           scTpaUsId = TCusId;  // Site User ID
           scTpaPwd  = TCSpsw;  // Site Password
           scTpaPath = TCPath;  // Export file Path Name
           scTAutoGen= TCAtgn;  // Auto generate policy export
           scTExcPrm = TCEPrm;  // Exclude premium
           scTExcPyrl = TCEPrl; // Exclude payroll
           scIncNBDiv = TCnbdv;
           scExcNrLoc = TCenrl;
        EndIf;
       EndSr;

      /End-Free
     p PrcAddTPA       e

      //*** Beg Add *** 05/20/24 *******************************************
     //===================================================================
     // WCMC State Information EDIT Procedure.
     //===================================================================
     p PrcStEdit       b
     d PrcStEdit       pi
     d inComp#                        3  0
     d
     d isexit          s               n   Inz(*off)
     d intCmpnum#      s              3  0 inz
     d
      /Free

      // Intialize the screen format/fields.
       Clear RcdStEdit;
       intCmpnum# = inComp#;
       Chain intCmpnum# pco00;
       Chain(n) intCmpnum# pc101;
       Exsr Movedbsc;
       Dow isExit = *off;
          Exfmt RcdStEdit;

          Select;

         // Save Button
          When BtnStSav = *On;
             BtnStSav = *off;
             If ValidateStInfo();
                Exsr MoveScDb;
                isexit = *on;
             Endif;

         // Cancel Button
          When BtnStCan = *On;
             BtnStCan =*off;
             isexit = *on;

          Other;
          Endsl;
       EndDo;

      // Subroutine to move Database fields into screen fields.
       BegSr MoveDbSc;
        If scState = 'FL';
           scSubmCd  =  C1subm;
           visSubmCd =  *on;
        Endif;
        If scState = 'TX';
           scLocCd  =  C1loc;
           visLocCd =  *on;
        Endif;
       EndSr;

      // Subroutine to move Screen fields into Database fields.
       BegSr MoveScDb;
        Chain intCmpnum# pc101;
        If %found(fmc1l);
           If scState = 'FL';
              C1subm = scSubmCd;
           Endif;
           If scState = 'TX';
              C1loc  = scLocCd;
           Endif;
           C1user = q1user;                   // User
           C1pgm  = q1pgm;                    // Program
           C1time = Timeto6(SystemTime());    // Time
           C1dt   = Dateto7(SystemDate());    // Date Maintained
           update pc101;
        Else;
           clear pc101;
           C1co#  = intCmpnum#;
           If scState = 'FL';
              C1subm = scSubmCd;
           Endif;
           If scState = 'TX';
              C1loc  = scLocCd;
           Endif;

           C1name =  COname;
           C1adr1 =  COadr1;
           C1adr2 =  COadr2;
           C1cty  =  COcty;
           C1st   =  COst;
           C1zip  =  COzip;
           C1stat = 'A';
           C1user = q1user;                   // User
           C1pgm  = q1pgm;                    // Program
           C1time = Timeto6(SystemTime());    // Time
           C1dt   = Dateto7(SystemDate());    // Date Maintained
           C1iUsr = q1user;                   // Intial User
           C1iPgm = q1pgm;                    // Intial Program
           C1iTim = Timeto6(SystemTime());    // Intial Time
           C1iDt  = Dateto7(SystemDate());    // Intial Date Maintained
           write pc101;
        Endif;
       EndSr;

      /End-Free
     p PrcStEdit       e

     //===================================================================
     // Validate State Information
     //===================================================================
     p ValidateStInfo  b
     d ValidateStInfo  pi              n
     d isError         s               n   inz

      /Free

      // Initialize all the messageId and error indicators.
       isError = *off;
       erSubmCd   = *off;
       msSubmCd   = *blanks;
       erLocCd    = *off;
       msLocCd    = *blanks;

       //Submitter Code cannot be zero
       If scSubmCd = *zeros and visSubmCd = *on;
          erSubmCd = *on;
          msSubmCd = 'WCR0556';
          isError = *on;
       Endif;

       //Location code cannot be blank
       If scLocCd = *blanks and visLocCd = *on;
          erLocCd = *on;
          msLocCd = 'WCR0503';
          isError = *on;
       Endif;

       // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateStInfo  e

      //*** End Add *** 05/20/24 *******************************************

     //===================================================================
     // Company Contact Information ADD/EDIT Procedure.
     //===================================================================
     p PrcCmpCont      b
     d PrcCmpCont      pi
     d inComp#                        3  0
     d
     d isexit          s               n   Inz(*off)
     d intCmpnum#      s              3  0 inz
     d intmode         s              1a   Inz('A')
     d
      /Free

      // Intialize the screen format/fields.
       Clear RcdContInf;
       BtnContCan = *off;
       intCmpnum# = inComp#;
       intmode   = 'A';

      // If Company Contact record exist, then update the existing one.
       Chain intCmpnum# pcc00;
       If %found(smccl);
         ExSr MoveDbSc;
         intmode = 'E';
       EndIf;

      // Looping for the popup window untill cancel button and exit is Off.
       Dow BtnContCan = *off and isexit = *off;

          Exfmt RcdContInf;

          Select;
         // ADD Mode.
          When BtnContSav = *On and intmode = 'A';
             BtnContSav =*off;
             If ValidateCmpCon();
                Exsr WrtCompCont;
                isexit = *on;
             EndIf;

         // EDIT Mode.
          When BtnContSav = *On and intmode = 'E';
             BtnContSav = *off;
             If ValidateCmpCon();
                Exsr UpdCompCont;
                isexit = *on;
             EndIf;

          Other;
          Endsl;
       EndDo;

      // Subroutine to move Database fields into screen fields.
       BegSr MoveDbSc;
        scCon1 = cp_CCcon1;
        scTtl1 = cp_CCttl1;
        scAra1 = cp_CCara1;
        scConPhn1 = cp_CCphn1;

        scCon2 = cp_CCcon2;
        scTtl2 = cp_CCttl2;
        scAra2 = cp_CCara2;
        scConPhn2 = cp_CCphn2;

        scCon3 = cp_CCcon3;
        scTtl3 = cp_CCttl3;
        scAra3 = cp_CCara3;
        scConPhn3 = cp_CCphn3;
       EndSr;

      // Subroutine to move Screen fields into Database fields.
       BegSr MoveScDb;
        CCco#  = intCmpnum#;
        CCcon1 = scCon1;
        CCttl1 = scTtl1;
        CCara1 = scAra1;
        CCphn1 = scConPhn1;

        CCcon2 = scCon2;
        CCttl2 = scTtl2;
        CCara2 = scAra2;
        CCphn2 = scConPhn2;

        CCcon3 = scCon3;
        CCttl3 = scTtl3;
        CCara3 = scAra3;
        CCphn3 = scConPhn3;

        CCstat = 'A';

        CCusr  = q1user;                // User
        CCpgm  = q1pgm;                 // Program
        CCtim  = Timeto6(SystemTime()); // Time
        CCdt   = Dateto7(SystemDate()); // Date Maintained

        If intmode = 'A';
           CCiUsr = q1user;                // Intial User
           CCiPgm = q1pgm;                 // Intial Program
           CCiTim = Timeto6(SystemTime()); // Intial Time
           CCiDt  = Dateto7(SystemDate()); // Intial Date Maintained
        EndIf;

       EndSr;

      // Writing Company Contact info. subroutine.
       BegSr WrtCompCont;

        Clear pcc0p;
        Exsr MoveScDb;
        Write pcc0p;

       EndSr;

      // Updating Company Contact info. subroutine.
       BegSr UpdCompCont;

        Chain cprrn4 pcc0p;
        Exsr MoveScDb;
        Update pcc0p;
        Unlock smccp;

       EndSr;


      /End-Free
     p PrcCmpCont      e

     //===================================================================
     // Company Detail Box loading Procedure.
     //===================================================================
     p PrcCmpDtl       b
     d PrcCmpDtl       pi
     d inSflrrn                       9  0
     d inprrn          s              9  0 Inz(0)
     d
     d strAdL1         s             40    inz
     d strAdL2         s             40    inz
     d strAdL3         s             40    inz
      /Free
        scDtlCmpNm = *blanks;
        scDtCmpSht = *blanks;
        scDtCmEFTN = *blanks;
        scDtCmPyA1 = *blanks;
        scDtCmPyA2 = *blanks;
        scDtCmPyA3 = *blanks;
        scDtCmMA1  = *blanks;
        scDtCmMA2  = *blanks;
        scDtCmMA3  = *blanks;
        //*** Beg Add ***** 05/08/25 *************************
        scDtCmEMA  = *blanks;
        //*** End Add ***** 05/08/25 *************************
        scDtCmPhon = *blanks;
        scDtCmFax  = *blanks;
        scDtTcc    = *blanks;
        scDtTollph = *blanks;
        scDtLoclph = *blanks;
        scDtCoi    = *blanks;
        scDtTrdl   = *blanks;
        scDtSrpt   = *blanks;
        scCart     = *blanks;
        scDtNmad   = *blanks;
        scDtActDrt = *blanks;
        scDtMIntDt = *blanks;
        scDtMntNew = *blanks;
        scDtOrFbs  = *blanks;
        //*** Beg Add ***** 10/13/22 *************************
        scPrFdNmD  = *blanks;
        //*** End Add ***** 10/13/22 *************************
        scDtAcht   = *blanks;
        scDtErSd   = *blanks;
        scDtDivPlL = *blanks;
        scDtDvPrTh = *blanks;
        scDtSyCd   = *blanks;
        scDtArcd   = *blanks;
        scDtEcar   = *blanks;
        scDtIfar   = *blanks;
        scDtSfar   = *blanks;
        scDtMfar   = *blanks;
        scDtTxar   = *blanks;
        scDt1Aar   = *blanks;
        scDt2Aar   = *blanks;
        scDt3Aar   = *blanks;
        scDt4Aar   = *blanks;
        scDt5Aar   = *blanks;
        scDt6Aar   = *blanks;
        scDt7Aar   = *blanks;
        //*** Beg Add *** 05/20/24 *******************************************
        If isUsedWcMc = *on;
           scMCCmpNm  = *blanks;
           scMCCAd1   = *blanks;
           scMCCAd2   = *blanks;
           scMCCAd3   = *blanks;
           scTNSubID  = *zeros;
           scNCCICd   = *zeros;
           scCaseNo   = *zeros;
           scEOMB#    = *zeros;
           scInvNo    = *zeros;
           scLLettNo  = *zeros;
           sc837ID    = *zeros;
           sc835ID    = *zeros;
        Endif;
        //*** End Add *** 05/20/24 *******************************************

        inprrn = inSflrrn;
        Chain inprrn sflCompInf;
        Chain scCmpNum pco00;
        If %found(smcol);
           scDtlComp# = COco#;
           scDtlCmpNm = COname;
           scDtCmpSht = COshNm;
           scDtCmEFTN = COaNam;

          // Physical Address.
           FormatAddress(COpAd1:COpAd2:COpCty:COpSt:COpZip:strAdL1:strAdL2:
            strAdL3);
           scDtCmPyA1  =  strAdL1;
           scDtCmPyA2  =  strAdL2;
           scDtCmPyA3  =  strAdL3;

          // Mailing Address.
           FormatAddress(COmAd1:COmAd2:COmCty:COmSt:COmZip:strAdL1:strAdL2:
            strAdL3);
           scDtCmMA1   =  strAdL1;
           scDtCmMA2   =  strAdL2;
           scDtCmMA3   =  strAdL3;

           //*** Beg Add ***** 05/08/25 *************************
          // Email Address.
           scDtCmEMA   =  COmail;
           //*** End Add ***** 05/08/25 *************************

           dsMaskingSC = dsMaskingCO;

           If COarea <> *zeros or COphn# <> *zeros;
              scDtCmPhon = '(' + %editc(COarea:'X') + ')' +
               %subst(%editc(COphn#:'X'):1:3) + '-' +
               %subst(%editc(COphn#:'X'):4:4);
           Endif;

           If COfAra <> *zeros or COfax# <> *zeros;
              scDtCmFax = '(' + %editc(COfAra:'X') + ')' +
               %subst(%editc(COfax#:'X'):1:3) + '-' +
               %subst(%editc(COfax#:'X'):4:4);
           Endif;

           scDtFein   = COfein;
           scDt1099   = CO1099;
           scDtTcc    = COtcc;

           If COph1a <> *zeros or COph1 <> *zeros;
              scDtTollph = '(' + %editc(COph1a:'X') + ')' +
               %subst(%editc(COph1:'X'):1:3) + '-' +
               %subst(%editc(COph1:'X'):4:4);
           Endif;

           If COarea <> *zeros or COph2 <> *zeros;
              //*** Beg Chg ***** 11/29/22 *************************
              //*** scDtLoclph = '(' + %editc(COph1a:'X') + ')' +
              scDtLoclph = '(' + %editc(COarea:'X') + ')' +
              //*** End Chg ***** 11/29/22 *************************
               %subst(%editc(COph2:'X'):1:3) + '-' +
               %subst(%editc(COph2:'X'):4:4);
           Endif;

           scDtChk1 = COchk1;
           scDtChk2 = COchk2;
           scDtCase = COnaCs;
           scDtCoi  = COcoi;
           scDtTrdl = COtrDl;
           scDtSrpl = COsrpl;
           scDtSrpt = COsrpT;
           scCart   = COcart;
           scDtNmad = COnmad;

           Select;
            When COckFe = 'P';
               scDtCkFe = 'P-Number of Pages';
            When COckFe = 'Y';
               scDtCkFe = 'Y-Federal#';
            Other;
               scDtCkFe = 'Blank-Check#';
           EndSl;

           Select;
            When COarSq = 'C';
               scDtArSq = 'C-Check # order';
            When COarSq = 'E';
               scDtArSq = 'E-Employer/div # order';
            Other;
               scDtArSq = 'Blank-Order entered';
           EndSl;


           scDtActDrt = COcapd;
           scDtMIntDt = COmainTidt;
           scDtMntNew = COmainTnew;

           scDtOrFbs = COrfbs;
           //*** Beg Add ***** 10/13/22 *************************
           scPrFdNmD  = COfedrc;
           //*** End Add ***** 10/13/22 *************************

           If COsyCd <> *blanks;
              chain (COco#:COsyCd) psy01;
              If %found;
                 scDtSyCd = COsyCd + '-' + %trim(SYdesc);
              Else;
                 scDtSyCd = *blanks;
              EndIf;
           EndIf;

           If COarCd <> *blanks;
              chain (COco#:COarCd) ptt01;
              If %found;
                 scDtArcd  = COarCd + '-' + %trim(TTdesc);
              Else;
                 scDtArcd  = *blanks;
              EndIf;
           EndIf;

           If COecAr <> *blanks;
              chain (COco#:COecAr) ptt01;
              If %found;
                 scDtEcar  = COecAr + '-' + %trim(TTdesc);
              Else;
                 scDtEcar  = *blanks;
              EndIf;
           EndIf;

           If COifAr <> *blanks;
              chain (COco#:COifAr) ptt01;
              If %found;
                 scDtIfar  = COifAr + '-' + %trim(TTdesc);
              Else;
                 scDtIfar  = *blanks;
              EndIf;
           EndIf;

           If COsfAr <> *blanks;
              chain (COco#:COsfAr) ptt01;
              If %found;
                 scDtSfar  = COsfAr + '-' + %trim(TTdesc);
              Else;
                 scDtSfar  = *blanks;
              EndIf;
           EndIf;

           If COmfAr <> *blanks;
              chain (COco#:COmfAr) ptt01;
              If %found;
                 scDtMfar  = COmfAr + '-' + %trim(TTdesc);
                 Else;
                 scDtMfar  = *blanks;
              EndIf;
           EndIf;

           If COtxAr <> *blanks;
              chain (COco#:COtxAr) ptt01;
              If %found;
                 scDtTxar  = COtxAr + '-' + %trim(TTdesc);
                 Else;
                 scDtTxar  = *blanks;
              EndIf;
           EndIf;

           If CO1Aar <> *blanks;
              chain (COco#:CO1Aar) ptt01;
              If %found;
                 scDt1Aar  = CO1Aar + '-' + %trim(TTdesc);
              Else;
                 scDt1Aar  = *blanks;
              EndIf;
           EndIf;

           If CO2Aar <> *blanks;
              chain (COco#:CO2Aar) ptt01;
              If %found;
                 scDt2Aar  = CO2Aar + '-' + %trim(TTdesc);
              Else;
                 scDt2Aar  = *blanks;
              EndIf;
           EndIf;

           If CO3Aar <> *blanks;
              chain (COco#:CO3Aar) ptt01;
              If %found;
                 scDt3Aar  = CO3Aar + '-' + %trim(TTdesc);
              Else;
                 scDt3Aar  = *blanks;
              EndIf;
           EndIf;

           If CO4Aar <> *blanks;
              chain (COco#:CO4Aar) ptt01;
              If %found;
                 scDt4Aar  = CO4Aar + '-' + %trim(TTdesc);
              Else;
                 scDt4Aar  = *blanks;
              EndIf;
           EndIf;

           If CO5Aar <> *blanks;
              chain (COco#:CO5Aar) ptt01;
              If %found;
                 scDt5Aar  = CO5Aar + '-' + %trim(TTdesc);
              Else;
                 scDt5Aar  = *blanks;
              EndIf;
           EndIf;

           If CO6Aar <> *blanks;
              chain (COco#:CO6Aar) ptt01;
              If %found;
                 scDt6Aar  = CO6Aar + '-' + %trim(TTdesc);
              Else;
                 scDt6Aar  = *blanks;
              EndIf;
           EndIf;

           If CO7Aar <> *blanks;
              chain (COco#:CO7Aar) ptt01;
              If %found;
                 scDt7Aar  = CO7Aar + '-' + %trim(TTdesc);
              Else;
                 scDt7Aar  = *blanks;
              EndIf;
           EndIf;

           scDtAchl  = COachL;
           scDtAcht  = COachT;
           scDtFySm  = COfySm;
           scDtErSd  = COerSd;
           scDtDivPlL = COpLtr;
           scDtDvPrTh = COprtH;
        EndIf;

        Clear scDtlAsOf;
        Clear scDtlDays;
        Reset dsSMCIP;
        Exec sql
         select * into :dsSMCIP from SMCIP
          where CIco# = :scCmpNum;
        If IsValidFetch(sqlstt);
           scDtlAsOf = AlphaUSA(CvtToDate(dsSMCIP.CIasOf));
           scDtlDays = dsSMCIP.CIdays;
        Endif;

         // WCPOLS Transmittal Default

         // WCSTAT Transmittal Default


        //*** Beg Add *** 05/20/24 *******************************************
        If isUsedWcMC = *on;
           Chain(n) COco# pc101;
           If %found(Fmc1l);
              If C1name = *blanks;
                 scMCCmpNm =  COname;
              Else;
                 scMCCmpNm =  C1name;
              Endif;
              If C1adr1 = *blanks;
                 FormatAddress(COadr1:COadr2:COcty:COSt:COZip:strAdL1:strAdL2:
                  strAdL3);
              Else;
                 FormatAddress(C1adr1:C1adr2:C1cty:C1St:C1Zip:strAdL1:strAdL2:
                  strAdL3);
              Endif;
              scMCCAd1   =  strAdL1;
              scMCCAd2   =  strAdL2;
              scMCCAd3   =  strAdL3;

              If C1area = *zeros or C1phn# = *zeros;
                 scMCPhone = '(' + %editc(COarea:'X') + ')' +
                  %subst(%editc(COphn#:'X'):1:3) + '-' +
                  %subst(%editc(COphn#:'X'):4:4);
              Elseif C1area <> *zeros or C1phn# <> *zeros;
                 scMCPhone = '(' + %editc(C1area:'X') + ')' +
                  %subst(%editc(C1phn#:'X'):1:3) + '-' +
                  %subst(%editc(C1phn#:'X'):4:4);
              Endif;

              If C1fara = *zeros or C1fax# = *zeros;
                 scMCFax = '(' + %editc(COfAra:'X') + ')' +
                  %subst(%editc(COfax#:'X'):1:3) + '-' +
                  %subst(%editc(COfax#:'X'):4:4);
              Elseif C1fara <> *zeros or C1fax# <> *zeros;
                 scMCFax = '(' + %editc(C1fAra:'X') + ')' +
                  %subst(%editc(C1fax#:'X'):1:3) + '-' +
                  %subst(%editc(C1fax#:'X'):4:4);
              Endif;

              If C1fein = *zeros;
                 scMCFein    =  COfein;
              Else;
                 scMCFein    =  C1fein;
              Endif;

              scTNSubID   =  C110a3;
              scNCCICd    =  C110a4;

              If C1lcs# <= 0;
                 scCaseNo =  Cocase;
              Else;
                 scCaseNo =  C1lcs#;
              Endif;

              If C1eob# <= 0;
                 scEOMB# = *zeros;
              Else;
                 scEOMB# = C1eob#;
              Endif;

              If C1lin# <= 0;
                 scInvNo = COinv#;
              Else;
                 scInvNo = C1lin#;
              Endif;

              If C1ltr# <= 0;
                 scLLettNo = *zeros;
              Else;
                 scLLettNo = C1ltr#;
              Endif;

              sc837ID = C1tsetID#;
              sc835ID = C1raid;
           Else;
              scMCCmpNm =  COname;
              FormatAddress(COadr1:COadr2:COcty:COSt:COZip:strAdL1:strAdL2:
               strAdL3);
              scMCCAd1   =  strAdL1;
              scMCCAd2   =  strAdL2;
              scMCCAd3   =  strAdL3;

              If COarea <> *zeros or COphn# <> *zeros;
                 scMCPhone = '(' + %editc(COarea:'X') + ')' +
                  %subst(%editc(COphn#:'X'):1:3) + '-' +
                  %subst(%editc(COphn#:'X'):4:4);
              Endif;

              If COfara <> *zeros or COfax# <> *zeros;
                 scMCFax = '(' + %editc(COfAra:'X') + ')' +
                  %subst(%editc(COfax#:'X'):1:3) + '-' +
                  %subst(%editc(COfax#:'X'):4:4);
              Endif;

              scMCFein = COfein;

              scTNSubID = *zeros;
              scNCCICd  = *zeros;

              If COcase <= 0;
                 scCaseNo = *zeros;
              Else;
                 scCaseNo =  COcase - 1;
              Endif;

              If COinv# <= 0;
                 scInvNo = *zeros;
              Else;
                 scInvNo = COinv# - 1;
              Endif;

              scEOMB# = *zeros;
              scLLettNo = *zeros;
              sc837ID = *zeros;
              sc835ID =  *zeros;
           Endif;
        Endif;
        //*** End Add *** 05/20/24 *******************************************

      /End-Free
     p PrcCmpDtl       e

      //===================================================================
      // Company information grid loading
      //===================================================================
     p PrcCmpGrd       b
      /Free

        Setll *loval pco00;
        Read pco00;

        Dow not %eof(smcol);
           scCmpNum  = COco#;
           scCmpName = COname;
           scDbRrn    = corrn;
           RrnCmpInf =  RrnCmpInf + 1;

          // Highlighted the select row.
           Clear scCmpRowSl;
           If RrnCmpInf = 1 and intSavCrrn = 0;
              scCmpRowSl = 'A';
           Elseif RrnCmpInf = intSavCrrn;
              scCmpRowSl = 'A';
           Endif;

           Write sflCompInf;
           Read pco00;
         EndDo;

        If RrnCmpInf > 0;
           inSflrrn = 1;
        EndIf;

      /End-Free
     p PrcCmpGrd       e

      //===================================================================
      // Vendor Information grid loading.
      //===================================================================
     p PrcVenGrd       b
     d PrcVenGrd       pi
     d inSflrrn                       9  0
     d inCmprrn        s              9  0 Inz(0)
      /Free

        Chain inSflrrn sflCompInf;
        Chain(n) scDbRrn pco0p;
        If %found(smcop);
           Setll (COco#:*loval) pvp00;
           Reade (COco#) pvp00;
           Dow not %eof(wmvpl);
              scVenCmp# = v1_VPco#;
              scVenNum = v1_VPvdr#;
              scVenName = %trim(v1_VPname);

              Select;
              When v1_VPpyFr = 'W';
                 scPyUpFry = v1_VPpyFr + '-' + 'Weekly';
              When v1_VPpyFr = 'M';
                 scPyUpFry = v1_VPpyFr + '-' + 'Monthly';
              When v1_VPpyFr = 'A';
                 scPyUpFry = v1_VPpyFr + '-' + 'Annually';
              Other;
                   scPyUpFry = v1_VPpyFr;
              EndSl;

              Select;
              When v1_VPpmFr = 'W';
                 scPmUpFry = v1_VPpmFr + '-' + 'Weekly';
              When v1_VPpmFr = 'M';
                 scPmUpFry = v1_VPpmFr + '-' + 'Monthly';
              Other;
                 scPmUpFry = v1_VPpmFr;
              EndSl;

              Select;
              When v1_VPdnLf = 'D';
                 scDwnFry = v1_VPdnLf + '-' + 'Daily';
              When v1_VPdnLf = 'W';
                 scDwnFry = v1_VPdnLf + '-' + 'Weekly';
              When v1_VPdnLf = 'M';
                 scDwnFry = v1_VPdnLf + '-' + 'Monthly';
              Other;
                 scDwnFry = v1_VPdnLf;
              EndSl;

              scEfDt  =  dtMDYY(v1_VPefDt);
              scExDt  =  dtMDYY(v1_VPexDt);

              rrnVenInf =  rrnVenInf + 1;
              scDbVenRrn = vnrrn;

              Write SflVenInfo;

              Reade (COco#) pvp00;
           EndDo;
        EndIf;


      /End-Free
     p PrcVenGrd       e

     //===================================================================
     // TPA Information grid loading.
     //===================================================================
     p PrcTPAGrd       b
     d PrcTPAGrd       pi
     d inSflrrn                       9  0
     d inTPArrn        s              9  0 Inz(0)
      /Free

        Chain inSflrrn sflCompInf;
        Chain(n) scDbRrn pco0p;
        If %found(smcop);

           Setll (coCo#:*loval) ptc00;
           Reade (coCo#) ptc00;
           Dow not %eof(wmtcl);

              scTPACmpCd = t1_tcCo#;
              scTPACd = t1_tcTpaC;
              scTPADescp = t1_tcDesc;

              If t1_tcStat = 'A';
                 scTPAStat = 'Active';
              else;
                 scTPAStat = 'Inactive';
              Endif;

              rrnTPAInf =  rrnTPAInf + 1;
              scDbTPARrn = tprrn2;

              Write SflTPAInfo;

              Reade (coCo#) ptc00;
           EndDo;
        EndIf;

      /End-Free
     p PrcTPAGrd       e

       //*** Beg Add *** 05/20/24 *******************************************
     //===================================================================
     // WCMC State Information Grid Loading
     //===================================================================
     p PrcStGrd        b
     d PrcStGrd        pi
      /Free

        Setll *loval ststl2;
        Read ststl2;
        Clear pc101;
        Chain(n) scCmpNum pc101;
        Dow not %eof(ststl2);
           rrnState += 1;
           scStateSel = *off;

           If rrnState = 1;
              scStateSel  = *on;
           Endif;
           //scRRN  = dbrrn;
           visFLTX    =  *off;
           scState    =  STkey;
           scStateNm  =  STname;
           scSubCd    =  *zeros;
           scLocCd    =  *blanks;
           If STkey   = 'FL';
              scSubCd    =  C1subm;
              visFLTX    =  *on;
           Endif;
           If STkey   = 'TX';
              scLocCd    =  C1loc;
              visFLTX    =  *on;
           Endif;
           Write sflStInfo;
           Read ststl2;
        Enddo;
        rrnState  = 1;

      /End-Free
     p PrcStGrd        e

       //*** End Add *** 05/20/24 *******************************************

      //===================================================================
      // Validate the company addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateCmpny   b
     d ValidateCmpny   pi              n
     d inMode                         1    const
       //*** Beg Add *** 05/20/24 *******************************************
     d isError         s               n   inz('0')
       //*** End Add *** 05/20/24 *******************************************
      /Free

       // Intialize all the messageId and error indicators.
        erComp#1 = *off;
        msComp#1 = *blanks;
        erCompNam = *off;
        msCompNam = *blanks;
        erEftBtch = *off;
        msEftBtch = *blanks;
        erPhyadd1 = *off;
        msPhyadd1 = *blanks;
        erPhycity = *off;
        msPhycity = *blanks;
        erPhyStat1= *off;
        msPhyStat1= *blanks;
        erPhyZip  = *off;
        msPhyZip  = *blanks;
        Reset aryMskER;
        Reset aryMskUsrER;
        er1099min = *off;
        ms1099min = *blanks;
        erSbmQue1 = *off;
        msSLogo   = *blanks;
        erSLogo   = *off;
        //*** Beg Add ***** 09/13/24 *******************************
        msSLogoPth = *blanks;
        erSLogoPth = *off;
        //*** End Add ***** 09/13/24 *******************************
        //*** Beg Add ***** 05/08/25 *******************************
        msEmladd = *blanks;
        erEmladd = *off;
        //*** End Add ***** 05/08/25 *******************************

        erChk1    = *off;
        msChk1    = *blanks;
        erChk2    = *off;
        msChk2    = *blanks;
        erCase    = *off;
        msCase    = *blanks;
        erIso     = *off;
        msIso     = *blanks;
        erCoi     = *off;
        msCoi     = *blanks;

        erCart    = *off;
        msCart    = *blanks;
        erNMad    = *off;
        msNMad    = *blanks;

        erOrFbs   = *off;
        erFySm    = *off;
        erSyCd    = *off;
        msSyCd    = *blanks;
        msArcd    = *blanks;
        erArcd    = *off;
        ms1Aar    = *blanks;
        er1Aar    = *off;
        msEcar    = *blanks;
        erEcar    = *off;
        ms2Aar    = *blanks;
        er2Aar    = *off;
        msIfar    = *blanks;
        erIfar    = *off;
        ms3Aar    = *blanks;
        er3Aar    = *off;
        msSfar    = *blanks;
        erSfar    = *off;
        ms4Aar    = *blanks;
        er4Aar    = *off;
        msMfar    = *blanks;
        erMfar    = *off;
        ms5Aar    = *blanks;
        er5Aar    = *off;
        msTxar    = *blanks;
        erTxar    = *off;
        er6Aar    = *off;
        er7Aar    = *off;
        msDivPlnLt = *blanks;
        erDivPlnLt = *off;
        msDivPrmTh = *blanks;
        erDivPrmTh = *off;
        erAsOfP = *off;

        //*** Beg Add *** 05/20/24 *******************************************
        msMCCmpNmE = *off;
        msMCCAd1E  = *off;
        msMCPhyCty = *off;
        msMCPhySt  = *off;
        msMCPhyZip = *off;
        msCaseNoE  = *off;
        msEOMB#E   = *off;
        msInvNoE   = *off;
        msLLettNoE = *off;
        erMCCmpNmE = *off;
        erMCCAd1E  = *off;
        erMCPhyCty = *off;
        erMCPhySt  = *off;
        erMCPhyZip = *off;
        erCaseNoE  = *off;
        erEOMB#E   = *off;
        erInvNoE   = *off;
        erLLettNoE = *off;
        //*** End Add *** 05/20/24 *******************************************

       // Company number is invalid.
        If scComp# <= *zero;
           erComp#1 = *on;
           msComp#1 = 'MCR0507';
           return *off;
        Endif;

       // Duplicate company entry
        If inmode = 'A';
          chain scComp# pco00;
           If %found(smcol);
              erComp#1 = *on;
              msComp#1 = 'MC30006';
              return *off;
           Endif;
        Endif;

       // company name cannot be blank.
        If scCmpNm = *blanks;
           erCompNam = *on;
           msCompNam = 'MCR0500';
           return *off;
        Endif;

        If isVisEftBt = *on and scCmpEFTnm = *blanks;
           erEftBtch = *on;
           msEftBtch = 'MCR0500';
           return *off;
        Endif;

      // Physical address line 1 canot be blank
       If scCmPhyAd1 = *Blanks;
          erPhyadd1 = *on;
          msPhyadd1 = 'MCR0500';
          return *off;
       EndIf;

      // Physical address city cannot be blank.
       If scCmPhyCty = *Blanks;
          erPhycity = *on;
          msPhycity = 'MCR0500';
          return *off;
       EndIf;

      // Physical address state cannot be blank.
       If scCmPhySt  = *Blanks;
          erPhyStat1= *on;
          msPhyStat1= 'MCR0500';
          return *off;
       EndIf;

      //Zip cannot be Zeros.
       If scCmPhyZip <= *zeros;
          erPhyZip  = *on;
          msPhyZip  = 'MCR0500';
          return *off;
       EndIf;

       //*** Beg Add ***** 05/08/25 *************************
      //Email Address validation
       If scCmpEMlAd <> *Blanks;
          erEmladd  = Check_Email(scCmpEMlAd:msEmladd);
          If erEmladd = *On;
             return *off;
          Endif;
       EndIf;
       //*** End Add ***** 05/08/25 *************************

       aryMaskSC = aryMaskingSC;
       isValid = *on;
       Reset aryMaskProcess;
       For i1 = 1 to #ofMasks;
          If aryMaskSC(i1).flag1 <> aryMaskCO(i1).flag1;
             Select;
             When aryMaskSC(i1) = 'Y';
                aryMaskProcess(i1) = '*ADD';
             Other;
                aryMaskProcess(i1) = '*REMOVE';
             Endsl;
          Endif;
          If aryMaskSC(i1).user1 <> *blanks;
             aryMaskSC(i1).user1 = Upper(aryMaskSC(i1).user1);
             If not ObjExists(aryMaskSC(i1).user1:'*USRPRF');
                aryMskUsrER(i1) = *on;
                isValid = *off;
             Endif;
          Endif;
       Endfor;
       aryMaskingSC = aryMaskSC;
       If not isValid;
          Return *off;
       Endif;

      // 1099 min amt. cannot be zero
       If is1099Tcc = *On and sc1099 <= *zeros;
          er1099min = *on;
          ms1099min = 'MCR0513';
          return *off;
       EndIf;

      // Mass policy pace to seperate job queue.
       If scSepQueue = 'Y';

          SA030R(outsufx);

         // Determine Sep JOBQ Name for Data Center and non-data center clients
          If outsufx <> *blanks;
           // Data Center client Sep JOBQ e.g. PDSJOBO132.
            outName  ='PDSJOBP' + outsufx;
          Else;
           //  Non-data Center client Sep JOBQ
            outName  = 'QBATCHP';

          EndIf;

          outType = '*JOBQ';
          GetObject(outName:outType:outRtcd);
          If outRtcd = '1';
             erSbmQue1 = *on;
             return *off;
          EndIf;

       EndIf;
       //*** begin add ***  09/25/24  *************************************
       //* Only perform logo editing if a logo is specified.
       If scSLogo <> ' ';
       //*** end add *****  09/25/24  *************************************

      // Output logo file name check.
          If (scSLogo <> *Blank and %scan('.':scSLogo) = 0);
             msSLogo = 'Logo file name must have a file name extension.';
             erSlogo = *on;
             return *off;
       //*** Beg Add ***** 09/13/24 **************************************
       //*** begin chg ***  09/25/24  *************************************
       //*****Else;
          Endif;
       //*** end chg *****  09/25/24  *************************************
          Clear erSLogoPth;
          Clear msSLogoPth;

       //*** begin add ***  09/25/24  *************************************
          Clear erSLogoURL;
          Clear msSLogoURL;
         If SCFlg6 <> 'Y';
       //*** end add *****  09/25/24  *************************************
          // Image path must be entered.
          If scSLogoPth = ' ' and scSLogo <> ' ';
             msSLogoPth = 'Logo image file path must be entered.';
             erSLogoPth = *on;
             return *off;
          Else;

          // Path must begin and end with a '/' character.
             strSlash  = %subst(scSLogoPth:1:1);
             If %len(%trim(scSLogoPth)) <> 0;
                strSlash2 = %subst(scSLogoPth:%len(%trim(scSLogoPth)):1);

                If strSlash <> '/' or strSlash2 <> '/';
                   msSLogoPth = 'Logo image file path must begin and end ' +
                    'with a ''/''.';
                   erSLogoPth = *on;
                   return *off;
                Endif;
             EndIf;
          //*** begin add ***  09/25/24  *************************************
          Endif;
         Else;
         //* Design and Compose enabled.
          // Image URL must be entered.
             If scSLogoURL = ' ' and scSLogo <> ' ';
                msSLogoURL = 'Logo image URL must be entered.';
                erSLogoURL = *on;
                return *off;
             Else;
                If %subst(scSLogoURL:1:7) <> 'http://' OR
                 %subst(scSLogoURL:%len(%trim(scSLogoURL)):1) <> '/';
                   erSLogoURL = *on;
                   msSLogoURL = 'Logo image URL must begin with http:// and' +
                    ' must end with a ''/''.';
                   return *off;
                Endif;
             Endif;
         //*** end add *****  09/25/24  *************************************
         Endif;
       //*** End Add ***** 09/13/24 **************************************
       //*** begin chg ***  09/25/24  *************************************
       //*****Endif;
       Else;
          // Do not allow image path/url if no logo.
          If scSLogoPth <> ' ';
             msSLogoPth = 'Logo image file path must be blank.';
             erSLogoPth = *on;
             return *off;
          Endif;
          If scSLogoURL <> ' ';
             msSLogoURL = 'Logo image URL must be blank.';
             erSLogoURL = *on;
             return *off;
          Endif;
       Endif;
       //*** end chg *****  09/25/24  *************************************

       If isPortal = *on;
          If scAsOfP <> *zero and DtCheck(scAsOfP) = *zero;
             erAsOfP = *on;
             msAsOfP = 'SYM0500';
             Return *off;
           Endif;
           scAsOfP = DtCheck(scAsOfP);
        Endif;

      // Error checking 2

      // Manaul check From and To cannot be zero.
       If Isvisible1 = *on and scChk1 = *zeros;
          erChk1 = *on;
          msChk1 = 'MCR0700';
          return *off;
       endIf;

       If Isvisible1 = *on and scChk2 = *zeros;
          erChk2   = *on;
          msChk2   = 'MCR0700';
          return *off;
       EndIf;

       If scCase = *zeros and Isvisible1 = *on;
          erCase = *on;
          msCase = 'MCR0513';
          return *off;
       EndIf;

       If scIso <> '1' and scIso <> '2' and scIso <> '3'
          and Isvisible1 = *on;
          erIso = *on;
          msIso = 'MCR0701';
          return *off;
       EndIf;

       If scCoi = ' ' and isvisible1 = *on;
          erCoi = *on;
          msCoi = 'MCR0500';
          return *off;
       EndIf;


       //*CA Pure Rate Code must be a valid CA rate set code.
       If scCart <> *blanks;
          chain (scComp#:scCart) prs00;
          If not %found(wmrsl) or RSjrst <> 'CA';
             erCart = *on;
             msCart = 'WC30376';
             return *off;
          EndIf;
       EndIf;

       // Provider Name/addr chg remark validation.
       If scNMad <> *blanks;
          chain (scNMad) pct00;
          If not %found(stctl1);
             erNMad = *on;
             msNMad = 'WCR0505';
             return *off;
          else;
          If CTstat = 'I';
             erNMad = *on;
             msNMad = 'WCR2347';
             return *off;
          EndIf;
          EndIf;
       EndIf;

      // Error checking 2

      // To check for 'N' 'E' or ' ' entry
       If scOrFbs <> 'N' And scOrFbs <> 'E' And scOrFbs <> ' ';
          erOrFbs = *on;
          return *off;
       EndIf;

      // Don't execute the following below edits if ACH module is not activated.
       If isAchFlg  = *off;
             return *on;
       Endif;

      // Batch system code check.
       //*** Beg Chg *** 05/20/24 *******************************************
       //*** If scSyCd = *blank;
       chain scComp# psy01;
       If scSyCd = *blank and %found(wtabl);
       //*** End Chg *** 05/20/24 *******************************************
          erSyCd = *on;
          msSyCd  = 'WCR0503';
          return *off;
       Endif;

     // Invalid Batch system code check.
       If scSyCd <> *blank;
          chain (scComp#:scSyCd) psy01;
          If not %found or SYstat <> 'A' or SYtype <> 'Y';
             erSyCd = *on;
             msSyCd  = 'WC30270';
             return *off;
          EndIf;
       Endif;

      //  Edit the A/R payment codes.
       //*** Beg Chg *** 05/20/24 *******************************************
       //*** If scArcd = *blanks;
       chain scComp# ptt01;
       If scArcd = *blanks and %found(wtabl);
       //*** End Chg *** 05/20/24 *******************************************
          msArcd = 'WCR0503';
          erArcd = *on;
          return *off;
       EndIf;

       If scArcd <> *blanks;
          chain (scComp#:scArcd) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> 'P' or TTtype <> 'P');
             msArcd = 'WC30259';
             erArcd = *on;
             return *off;
          EndIf;
       EndIf;

       If sc1Aar <> *blanks;
          chain (scComp#:sc1Aar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> '1' or TTtype <> 'Q');
             ms1Aar = 'WC30329';
             er1Aar = *on;
             return *off;
          EndIf;
       EndIf;

       If scEcar <> *blanks;
          chain (scComp#:scEcar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> 'E' or TTtype <> 'P');
             msEcar = 'WC30334';
             erEcar = *on;
             return *off;
          EndIf;
       EndIf;

       If sc2Aar <> *blanks;
          chain (scComp#:sc2Aar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> '2' or TTtype <> 'Q');
             ms2Aar = 'WC30330';
             er2Aar = *on;
             return *off;
          EndIf;
       EndIf;

       If scIfar <> *blanks;
          chain (scComp#:scIfar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> 'F' or TTtype <> 'Q');
             msIfar = 'WC30325';
             erIfar = *on;
             return *off;
          EndIf;
       EndIf;

       If sc3Aar <> *blanks;
          chain (scComp#:sc3Aar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> '3' or TTtype <> 'Q');
             ms3Aar = 'WC30331';
             er3Aar = *on;
             return *off;
          EndIf;
       EndIf;

       If scSfar <> *blanks;
          chain (scComp#:scSfar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> 'S' or TTtype <> 'Q');
             msSfar = 'WC30326';
             erSfar = *on;
             return *off;
          EndIf;
       EndIf;

       If sc4Aar <> *blanks;
          chain (scComp#:sc4Aar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> '4' or TTtype <> 'Q');
             ms4Aar = 'WC30332';
             er4Aar = *on;
             return *off;
          EndIf;
       EndIf;

       If scMfar <> *blanks;
          chain (scComp#:scMfar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> 'M' or TTtype <> 'Q');
             msMfar = 'WC30327';
             erMfar = *on;
             return *off;
          EndIf;
       EndIf;

       If sc5Aar <> *blanks;
          chain (scComp#:sc5Aar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> '5' or TTtype <> 'Q');
             ms5Aar = 'WC30333';
             er5Aar = *on;
             return *off;
          EndIf;
       EndIf;

       If scTxar <> *blanks;
          chain (scComp#:scTxar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> 'T' or TTtype <> 'Q');
             msTxar = 'WC30328';
             erTxar = *on;
             return *off;
          EndIf;
       EndIf;

       If sc6Aar <> *blanks;
          chain (scComp#:sc6Aar) ptt01;
          If %found and TTstat = 'A' and (TTctyp <> '6' or TTtype <> 'Q');
             er6Aar = *on;
             return *off;
          EndIf;
       EndIf;

       If sc7Aar <> *blanks;
          chain (scComp#:sc7Aar) ptt01;
          If %found and TTstat = 'A' and (TTcTyp <> '7' or TTtype <> 'Q');
             er7Aar = *on;
             return *off;
          EndIf;
       EndIf;

      // Ensure valid month for fiscal year start month
       If scfysm < 0 or scfysm > 12;
          erFySm = *on;
          return *off;
       EndIf;

      // Validate Div Plan Letter Dart.
       If scDivPlnLt <> ' ';
          Chain scDivPlnLt pct00;
          If not %found;
             msDivPlnLt = 'WCR0505';
             erDivPlnLt = *on;
             return *off;
          Endif;
          If ctstat = 'I';
             msDivPlnLt = 'WCR2347';
             erDivPlnLt = *on;
             return *off;
          Endif;
       Endif;

      // Validate Prem Thrshld
       If scDivPrmTh <> ' ';
          Chain scDivPrmTh pct00;
          If not %found;
             msDivPrmTh = 'WCR0505';
             erDivPrmTh = *on;
             return *off;
          Endif;

          If CTstat = 'I';
             msDivPrmTh = 'WCR2347';
             erDivPrmTh = *on;
             return *off;
          Endif;
       Endif;

       //*** Begin Add *** 06/13/25 ****************************************
       //
       // Only do Managed Care Edits If Managed Care installed
       If isUsedWcMc = *On;
       //*** End   Add *** 06/13/25 ****************************************
       //*** Beg Add *** 05/20/24 *******************************************
       //Alternate company name cannot be blank
       If scMCCmpNmE = *blanks;
          erMCCmpNmE = *on;
          msMCCmpNmE = 'WCR0503';
          isError = *on;
       Endif;

       //Alternate company Address cannot be blank
       If scMCCAd1E  = *blanks;
          erMCCAd1E  = *on;
          msMCCAd1E  = 'WCR0503';
          isError = *on;
       Endif;

       //Alternate company Address City cannot be blank
       If scMCPhyCty = *blanks;
          erMCPhyCty = *on;
          msMCPhyCty = 'WCR0503';
          isError = *on;
       Endif;

       //Alternate company State cannot be blank
       If scMCPhySt = *blanks;
          erMCPhySt = *on;
          msMCPhySt = 'WCR0503';
          isError = *on;
       Endif;

       //Alternate company Zip cannot be zero
       If scMCPhyZip = *zeros;
          erMCPhyZip = *on;
          msMCPhyZip = 'WCR0556';
          isError = *on;
       Endif;

       //Standalone last case number used cannot be zero
       If scCaseNoE = *zeros;
          erCaseNoE = *on;
          msCaseNoE = 'WCR0556';
          isError = *on;
       Endif;

       //Last EOMB Number used cannot be zero
       If scEOMB#E = *zeros;
          erEOMB#E = *on;
          msEOMB#E = 'WCR0556';
          isError = *on;
       Endif;

       //Last Invoice number used cannot be zero
       If scInvNoE = *zeros;
          erInvNoE = *on;
          msInvNoE = 'WCR0556';
          isError = *on;
       Endif;

       //Last Letter number used cannot be zero
       If scLLettNoE = *zeros;
          erLLettNoE = *on;
          msLLettNoE = 'WCR0556';
          isError = *on;
       Endif;
       //*** Begin Add *** 06/13/25 ****************************************
       EndIf;
       //*** End   Add *** 06/13/25 ****************************************

       // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;
       //*** End Add *** 05/20/24 *******************************************
      // Return *on indicator when there is No error.
       return *on;

      /End-Free
     p ValidateCmpny   e

      //===================================================================
      // Validate the Company Contact information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateCmpCon  b
     d ValidateCmpCon  pi              n
      /Free

       // Intialize all the messageId and error indicators.
        erCon1 = *off;
        msCon1 = *blanks;
        erTtl1 = *off;
        msTtl1 = *blanks;
        erara1 = *off;
        msara1 = *blanks;
        erConPhn1 = *off;
        msConPhn1 = *blanks;

       // Contact 1
        If scCon1 = *blanks;
           erCon1 = *on;
           msCon1 = 'WC40059';
           return *off;
        Endif;

       // Contact title 1
        If scTtl1 = *blanks;
           erTtl1 = *on;
           msTtl1 = 'WC40060';
           return *off;
        Endif;

       // Contact 1 area code.
        If scara1 = *zeros;
           erara1 = *on;
           msara1 = 'WC40061';
           return *off;
        Endif;

       // Contact 1 Phone#.
        If scConPhn1 = *zeros;
           erConPhn1 = *on;
           msConPhn1 = 'WC40062';
           return *off;
        Endif;

      // Return *on indicator when No error exists.
       return *on;

      /End-Free
     p ValidateCmpCon  e

     //===================================================================
     // Validate Vendor information Procedure.
     // Returns *on  ----> No errors
     // Returns *off ----> Any errors
     //===================================================================
     p ValidateVndr    b
     d ValidateVndr    pi              n
     d inMode                         1    const

     d dt7EfDt         s              7  0
     d dt7ExDt         s              7  0
     d indError        s               n

      /Free
      // Intialize all the messageId and error indicators.
       msVdr#     = ' ';
       erVdr#     = *off;
       msVenNam   = ' ';
       erVenNam   = *off;
       msVenSt    = ' ';
       erVenSt    = *off;
       erVenEfDat = ' ';
       msVenEfDat = *off;
       msVenExpDt = ' ';
       erVenExpDt = *off;
       msExNp     = ' ';
       erExNp     = *off;
       msExAl     = ' ';
       erExAl     = *off;
       msPayFrq   = ' ';
       erPayFrq   = *off;
       msPmFr    = ' ';
       erPmFr    = *off;
       msFTyp    = ' ';
       erFTyp    = *off;
       msDateTyp = ' ';
       erDateTyp = *off;
       msDnLf     = ' ';
       erDnLf     = *off;
       msDFmt    =  ' ';
       erDFmt    = *off;
       msBlnd    = ' ';
       erBlnd    = *off;
       msNet     = ' ';
       erNet     = *off;
       msAutr    = ' ';
       erAutr    = *off;
       msPyUp     = ' ';
       erPyUp     = *off;
       msMthIn  = ' ';
       erMthIn  = *off;
       msPTyc   = ' ';
       erPTyc   = *off;
       msPrBlCd = ' ';
       erPrBlCd = *off;

      // Vendor number required.
       If scVdr# = *Zero;
          msVdr# = 'WCR0503';
          erVdr# = *on;
          indError = *On;
       EndIf;

      // Vendor number duplicate.
       If inMode = 'A';
          chain(n) (COco#:scVdr#) pvp00;
          If %found(wmvpl);
             msVdr# = 'WC32380';
             erVdr# = *on;
             indError = *On;
          EndIf;
       EndIf;

      // Vendor name is required.
       If scVenNam = *blanks;
          msVenNam = 'WCR0503';
          erVenNam = *on;
          indError = *On;
       EndIf;

       // Validate vendor eff date
       If scVenEfDat <> 0;
          dt7EfDt = dtCYMD(scVenEfDat);
          If dt7EfDt = 0;
             indError = *On;
             erVenEfDat = *On;
             msVenEfDat = 'WCR0500';
          Endif;
       Endif;

       // Validate vendor exp date
       If scVenExpDt <> 0;
          dt7ExDt = dtCYMD(scVenExpDt);
          If dt7ExDt = 0;
             indError = *On;
             erVenExpDt = *On;
             msVenExpDt = 'WCR0500';
          Endif;
       Endif;

       // Ensure ascending order if populated
       Select;
       When dt7ExDt = 0;
       When dt7EfDt = 0;
       When dt7EfDt <= dt7ExDt;
       Other;
          indError = *On;
          msVenExpDt = 'WCR0529';
          erVenExpDt = *on;
          msVenEfDat = 'WCR0529';
          erVenEfDat = *on;
       EndSl;

       // Don't allow expiration if vendor history exists after exp date
       If dt7ExDt <> 0;
          Setll (COco#) pvh00;
          Reade (COco#) pvh00;
          Dow Not %Eof;
             If dt7ExDt < VHefDt or
              dt7exDt < VHexDt;
                msVenExpDt = 'WC30301';
                erVenExpDt = *on;
                indError = *On;
                Leave;
             EndIf;
          Reade (COco#) pvh00;
          EndDo;
       Endif;

      // Valid values for premium frequency
       If scPmFr <> *blanks  and scPmFr <> 'W' and scPmFr <> 'M';
          msPmFr = 'WC30247';
          erPmFr = *on;
          indError = *On;
       EndIf;

      // Date type
       If scDateTyp <> *blanks and scDateTyp <> 'N' and scDateTyp <> 'T';
          msDateTyp = 'WC30257';
          erDateTyp = *on;
          indError = *On;
       EndIf;

       If scPyUp <> *blanks  and scPrBlCd = *blanks;
          msPrBlCd  = 'WC40165';
          erPrBlCd  = *on;
          indError = *On;
       EndIf;

       If indError = *On;
          Return *Off;
       Else;
          Return *On;
       EndIf;

      /End-Free
     p ValidateVndr    e


      //===================================================================
      // Validate the TPA addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateTPA     b
     d ValidateTPA     pi              n
     d inMode                         1    const

     d indError        s               n
      /Free
       // Intialize all the messageId and error indicators.
        msTpaCd   = *blanks;
        erTpaCd   = *off;
        msTpaDsc  = *blanks;
        erTpaDsc  =  *off;
        msTpaIp   = *blanks;
        erTpaIp   =  *off;
        msTpaStFl = *blanks;
        erTpaStFl = *off;
        msTpaUsId = *blanks;
        erTpaUsId = *off;
        msTpaPwd  = *blanks;
        erTpaPwd  = *off;

      // TPA Code required.
       If scTpaCd = *blanks;
          msTpaCd = 'WC40053';
          erTpaCd = *on;
          return *off;
       EndIf;

      // Duplicate TPA Code.
       If scTpaCd <> *blanks and inmode = 'A';
          chain(n) (scTPACmpCd:scTPACd) ptc00;
          If %found(wmtcl);
             msTpaCd = 'WC30661';
             erTpaCd = *on;
             indError = *On;
          EndIf;
       EndIf;

      // TPA Description required.
       If scTpaDsc = *blanks;
          msTpaDsc = 'WC40054';
          erTpaDsc = *on;
          indError = *On;
       EndIf;

      // TPA Site IP required.
       If scTpaIp = *blanks;
          msTpaIp = 'WC40055';
          erTpaIp = *on;
          indError = *On;
       EndIf;

      // TPA Site Folder required.
       If scTpaStFl  = *blanks;
          msTpaStFl = 'WC40056';
          erTpaStFl = *on;
          indError = *On;
       EndIf;

      // TPA Site User Id required.
       If scTpaUsId  = *blanks;
          msTpaUsId = 'WC40057';
          erTpaUsId = *on;
          indError = *On;
       EndIf;

      // TPA Site Password required.
       If scTpaPwd  = *blanks;
          msTpaPwd  = 'WC40058';
          erTpaPwd  = *on;
          indError = *On;
       EndIf;

      // IFS path is optional but if populated must be formatted correctly
       // (/directoryPathName/documentName.txt) and directory must exist and can
       // be accessed by user
       Clear fullPathName;
       fullPathName = %Xlate(lo:up:scTpaPath);
       PathNmLen = %scan(' ':fullPathName);
       fullPathName = %replace(' ':fullPathName:PathNmLen+1);
       If scTPAPath <> ' ';
           If %subst(scTpaPath:1:1) <> '/';
              erTPAPath = *On;
              msTPAPath = 'The iSeries file path name must start with a ''/''.';
              indError = *On;
           Else;
           // Must be a .TXT file
              If %scan('.TXT':fullPathName) = 0;
                 erTPAPath = *On;
                 msTPAPath = 'The file extension must be .TXT.';
                 indError = *On;
              Else;
                 //Directory must exist.
                 Clear X;
                 Dou X = 0;
                     X = %scan('/':fullPathName:x+1);
                     If X <> 0;
                        lastSlash = X;
                     Endif;
                 Enddo;
                 xxDirectory = %subst(fullPathName:1:lastSlash-1);
                 If Stat(%Trim(xxDirectory):%Addr(myStat)) < 0;
                    erTPAPath = *On;
                    msTPAPath = 'Invalid directory.';
                    indError = *On;
                 Else;
                    If Access(%Trim(xxDirectory):docAccess) < 0;
                       erTPAPath = *On;
                       msTPAPath ='Not authorized to directory.';
                       indError = *On;
                    EndIf;
                 Endif;
              Endif;
           Endif;
       Endif;

      // Return *on indicator when No error exists.
       return *on;

      /End-Free
     p ValidateTPA     e
     ****  Beg Add  ***  03/10/22  *********************************************
       //===================================================================
       // DspCarriers  Display Carriers Panel
       //===================================================================
     p DspCarriers     b
     d DspCarriers     pi
     d  inRrn                         9  0

     d  isexit         s               n   inz(*off)
     d  RrnCar         S                   Like(inRrn)
      /free

       Clear RcdCarMst;
       rrnCar = inRrn;
       savCarRrn = 0 ;
       btnCarCls = *Off;

       //*  Process until Close button is pressed.

       Dow isexit = *off And BtnCarCls = *Off;

          // Clearing the Carriers subfiles.

          clrCarSfl = *on;
          dspCar = *off;
          Write RcdCarMst;
          clrCarSfl = *off;
          dspCar = *on;
          rrnCarInf = 0;

          // Load the Carriers grid.
          LoadCarrGrid(RrnCar);

         // Load the Carrier Details based on Row selected.
          If SavCarRrn > 1;
             PrcCarDtl(SavCarRrn);
          Else;
             PrcCarDtl(intFrstRnn);
          EndIf;

         // Execute Carrier the record formats.
          Exfmt RcdCarMst;

          Select;

         // Add Button click on Carrier Grid.
          When btnCarAdd = *on;
             btnCarAdd = *off;
             inrrn = 0;
             PrcAddCar(inrrn :'A');

         // Cancel Button click on Carrier Information
          When btnCarcls = *on;
             isexit = *on;

         // Other Processing on Company, Vendor and TPA Grid.
          Other;

            // Company Grid subfile change record logic
             Dou %Eof;
                Readc SflCarrInf;
                If %eof();
                   Leave;
                Endif;

               // Row click on Carrier grid.
                If btnCrSflRw = *on;
                   btnCrSflRw = *off;
                   SavCarRrn  = CRrnAddCar;
                   Leave;
                EndIf;

               // Carrier Edit hyperlink
                If lnkCarEdt = *on;
                   inrrn = scDbCarRrn;
                   PrcAddCar(inrrn :'E');
                   lnkCarEdt = *off;
                   Leave;
                EndIf;

               // Carrier Delete hyperlink
                If lnkCarDlt = *on;
                   lnkCarDlt = *off;
                   inrrn = scDbCarRrn;
                   PrcAddCar(inrrn :'D');
                   Leave;
                EndIf;

             EndDo;    // Carrier ReadC

          EndSl;

       Enddo;    // ExitNow = *On;

      /End-Free
     p DspCarriers     e
       //===================================================================
      // Carrier Information Grid loading.
      //===================================================================
     p LoadCarrGrid    b
     d LoadCarrGrid    pi
     d inSflrrn                       9  0
     d inCmprrn        s              9  0 Inz(0)
      /Free

        Chain inSflrrn sflCompInf;
        Chain(n) scDbRrn pco0p;
        If %Found(Smcop);
           ScCarCmpNm = 'Company: ' + %Char(COCO#) + ' - ' + %Trim(CONAME);
           Setll (COco#:*loval) ptm00;
           Reade(n) (COco#) ptm00;
           Dow not %eof(wmtml1);
              HdCarCode = TMCar# ;
              If TMCar# = 0 ;
                 scCarCode = 'Default';
                 scCarDelV = *On;
                 scAlwPerDs= TMApnd;
              Else;
                 scCarCode = %Char(TMCar#) ;
                 scCarDelV = *Off;
              EndIf;
              Chain(n) (COco# : TMCar#) PCG00 ;
              If %found(WmCgL);
                 scCarName = ' ' ;
                 scCarName = %Trim(CGName);
              Else ;
                 scCarName = ' ';
              EndIf;
              rrnCarInf  = rrnCarInf + 1;
              scDbCarRrn = rrnCarInf;

              // Highlighted the select row.
              Clear scCarRowSl;
              If RrnCarInf = 1 and SavCarRrn = 0;
                 scCarRowSl = 'A';
              Elseif RrnCarInf = SavCarRrn;
                 scCarRowSl = 'A';
              Endif;

              Write SflCarrInf;

              Reade(n) (COco#) ptm00;
           EndDo;
        EndIf;

      /End-Free
     p LoadCarrGrid    e

     //===================================================================
     // Carrier Addition procedure.
     //===================================================================
     p PrcAddCar       b
     d PrcAddCar       pi
     d  inRrn                         9  0
     d  inMode                        1    const
 
     d  isexit         s               n   inz(*off)
     d  inCarCmp#      s              3  0 inz(0)
      /Free
       Clear RcdCarAdd;
       btnCarCan = *off;
       btnSavAdd = *off;
       btnSavCar = *off;
       isDisCarCd = *off;

       Select;
      // ADD Mode.
       When inmode = 'A';
          inCarCmp# = scCmpNum;
          btnCarCanL = 705;
          scCarHdTxt = 'Add Carrier Information';
          cbxAlwPerV = *On;

      // EDIT Mode.
       When inmode = 'E';
          If scCarCode = 'Default';
            sCCarCdV = *On;
          Else;
            CbxAlwPerV = *On;
          EndIf;

          isDisCarCd = *on;
          btnSavAddV = *on;
          btnCarCanL = 815;
          scCarHdTxt = 'Edit Carrier Information';
          ExSr MoveDbSc;

      // DELETE Mode.
       When inmode = 'D';
          isDisCarCd = *on;
          isDisabled = *on;
          btnSavAddV = *on;
          btnCarCanL = 815;
          ExSr MoveDbSc;
          scCarHdTxt = 'Delete Carrier Information';
       EndSl;

      // Looping for the popup window untill cancel button and exit is Off.
       Dow btnCarCan = *off And isExit = *off;
          Select;
         // ADD Mode.
          When inmode = 'A';
             Exfmt RcdCarAdd;
             Exsr ProcessCarrier;

         // EDIT Mode.
          When inmode = 'E';
             Exfmt RcdCarAdd;
             Exsr ProcessCarrier;

         // DELETE Mode.
          When inmode = 'D';
             Write RcdCarAdd;

             strMsg = 'Please confirm deletion.' ;
             If Not PopTwoBtn(strmsg:'Delete Carrier':
              'Cancel':'Confirm');
                Chain (scCmpNum : scCarNum) PCG00;
                 If %found;
                    Delete PCG00;
                 Endif;
                Chain (scCmpNum : scCarNum) PTM00;
                Delete PTM00;
                isexit = *on;
             Else;
                isexit = *on;
             Endif;
             Exsr ProcessCarrier;

          Other;
          Endsl;
       EndDo;

       BegSr ProcessCarrier;
        Select;

       // ADD Mode.
        When btnSavCar = *On And inmode = 'A';
           btnSavCar =*off;
           If ValidateCarrier('A');
              Exsr WriteCarrier;
              isexit = *on;
           EndIf;

        When btnSavAdd = *On And inmode = 'A';
           btnSavAdd = *off;
           If ValidateCarrier('A');
              Exsr WriteCarrier;
              Exsr ClearCarrier;
           EndIf;

       // EDIT Mode.
        When btnSavCar = *On And inmode = 'E';
           btnSavCar =*off;
           If ValidateCarrier('E');
              Exsr UpdateCarrier;
              isexit = *on;
           EndIf;

        Other;
        EndSl;
       EndSr;

      // Writing new Carrier subroutine.
       BegSr WriteCarrier;
        Clear PCG0P;
        CGCo#  = inCarCmp#;
        CGCar# = scCarNum ;
        CGName = scCarName ;
        CGiusr = q1user;
        CGipgm = q1pgm;
        CGidt  = Dateto7(SystemDate());
        CGitm  = Timeto6(SystemTime());
        Write PCG0P;

        Clear PTM0P;
        TMCo#  = inCarCmp#;             // Company
        TMCar# = scCarNum;              // Carrier
        TMiUsr = q1user;                // USER
        TMiPgm = q1pgm;                 // PROGRAM
        TMiDat = Dateto7(SystemDate()); // DATE MAINTAINED
        TMiTim = Timeto6(SystemTime()); // TIME
        ExSr MoveScDb;
        Write PTM0P;

       EndSr;

      // Updating/Editing Carrier subroutine.
       BegSr UpdateCarrier;

        Chain (scCmpNum : scCarNum) PCG00;  // to get exclusive record lock.
        If %found(WMCGL) Or scCarNum = 0 ;
           CGName = scCarName ;
           CGuser = q1user;
           CGpgm  = q1pgm;
           CGdt   = Dateto7(SystemDate());
           CGtime = Timeto6(SystemTime());
           If scCarNum <> 0 ;
              Update PCG00;
           EndIf;

           Chain (scCmpNum : scCarNum)  ptm00;
           If %found(wmtml1);
              If TMmail  <> scCarMailP Or TMname <> scCarNameP  Or
                 TMphn#  <> scPhnP     Or TMext# <> scExtP      Or
                 TmDtype <> scDatPvdP  Or TmFein <> scTFienP    Or
                 TMfax#  <> scFaxP     Or TMadr1 <> scCarAddP   Or
                 TMcty   <> scCarCityP or TMst   <> scCarStatP  Or
                 TMzip   <> scZipP                              Or
                 TMSmail <> scCarMailS Or TMSname <> scCarNameS Or
                 TMSphn# <> scPhnS     Or TMSext# <> scExtS     Or
                 TmSDtype<> scDatPvdP  Or TmSFein <> scTFienP   Or
                 TMSfax# <> scFaxS     Or TMSadr1 <> scCarAddS  Or
                 TMScty  <> scCarCityS or TMSst   <> scCarStatS Or
                 TMSzip  <> scZipS or scAlwPerDs <> TMApnd;
                 ExSr MoveScDb;
                 Update PTM00;
              EndIf;
           EndIf;
        EndIf;

       EndSr;

      // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;

        TMmail   = scCarMailP;           // WCPOLS CONTACT EMAIL ADDR
        TMname   = scCarNameP;           // WCPOLS CONTACT NAME
        TMphn#   = scPhnP;               // WCPOLS CONTACT PHONE#
        TMext#   = scExtP;               // WCPOLS CONTACT EXTENSION
        TMfax#   = scFaxP;               // WCPOLS CONTACT FAX#
        TMadr1   = scCarAddP;            // WCPOLS CONTACT STREET ADDR
        TMcty    = scCarCityP;           // WCPOLS CONTACT CITY
        TMst     = scCarStatP;           // WCPOLS CONTACT STATE
        TMzip    = scZipP;               // WCPOLS CONTACT ZIP CODE
        TmDType  = scDatPvdP;            // WCPOLS DATA PROVIDER TYPE
        TmFein   = scTFienP;             // WCPOLS TPE FIEN
        TMSmail  = scCarMailS;           // WCSTAT CONTACT EMAIL ADDR
        TMSname  = scCarNameS;           // WCSTAT CONTACT NAME
        TMSphn#  = scPhnS;               // WCSTAT CONTACT PHONE#
        TMSext#  = scExtS;               // WCSTAT CONTACT EXTENSION
        TMSfax#  = scFaxS;               // WCSTAT CONTACT FAX#
        TMSadr1  = scCarAddS;            // WCSTAT CONTACT STREET ADDR
        TMScty   = scCarCityS;           // WCSTAT CONTACT CITY
        TMSst    = scCarStatS;           // WCSTAT CONTACT STATE
        TMSzip   = scZipS;               // WCSTAT CONTACT ZIP CODE
        TMSDType = scDatPvdS;            // WCSTAT DATA PROVIDER TYPE
        TMSFein  = scTFienS;             // WCSTAT TPE FIEN
        TMapnD   = scAlwPerDs;           // ALLOW PERSONAL DESI
        TMusr   = q1user;                // USER
        TMpgm   = q1pgm;                 // PROGRAM
        TMdat   = Dateto7(SystemDate()); // DATE MAINTAINED
        TMtim   = Timeto6(SystemTime()); // TIME

       EndSr;

      // Subroutine to move database fields into Screen fields.
       BegSr MoveDbSc;

        Chain inrrn SflCarrInf;
        Chain(n) (scCmpNum: HdCarCode) ptm00;
        If %found(wmtml1);
           scDtlCar#  = scCarCode ;
           scAlwPerDs= TMApnd;
           If TMCAR# <> 0;
              scCarNum = TmCar#;
              Chain(n) (COco# : TMCar#) PCG00 ;
              If %Found(WMCgl);
                 scCarName = %Trim(CgName);
              EndIf;
           Else;
              scDtlCar#  = scCarCode ;
           EndIf;

          // WCPOLS Details
           scCarNameP  = TMname;
           scCarMailP  = TMmail;
           scDatPvdP   = TmDtype;
           scTFienP    = TmFein;
           scPhnP      = TMphn#;
           scExtP      = TMext#;
           scFaxP      = TMfax#;
           scCarAddP   = TMadr1;
           scCarCityP  = TMcty;
           scCarStatP  = TMst;
           scZipP      = TMzip;

          // WCSTAT Details
           scCarNameS  = TMSname;
           scCarMailS  = TMSmail;
           scDatPvdS   = TmSDtype;
           scTFienS    = TmSFein;
           scPhnS      = TMSphn#;
           scExtS      = TMSext#;
           scFaxS      = TMSfax#;
           scCarAddS   = TMSadr1;
           scCarCityS  = TMScty;
           scCarStatS  = TMSst;
           scZipS      = TMSzip;
           lnkCarEdt   = *Off;
        Endif;

       EndSr;

      // Clear Carriers
       BegSr ClearCarrier;

          scCarNum    = 0  ;
          scCarName   = ' ';

         // WCPOLS Deta
          scCarNameP  = ' ';
          scCarMailP  = ' ';
          scDatPvdP   = ' ';
          scTFienP    = 0  ;
          scPhnP      = 0  ;
          scExtP      = 0  ;
          scCarAddP   = ' ';
          scCarCityP  = ' ';
          scCarStatP  = ' ';
          scZipP      = 0  ;

         // WCSTAT Details
          scCarNameS  = ' ';
          scCarMailS  = ' ';
          scDatPvdS   = ' ';
          scTFienS    = 0  ;
          scPhnS      = 0  ;
          scExtS      = 0  ;
          scCarAddS   = ' ';
          scCarCityS  = ' ';
          scCarStatS  = ' ';
          scZipS      = 0  ;

       EndSr;

      /End-Free

     p PrcAddCar       e

      //===================================================================
      // Validate the Carrier information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     pValidateCarrier  b
     dValidateCarrier  pi              n
     d inMode                         1    const

     d indError        s               n
      /Free

       // Intialize all the messageId and error indicators.
        erCarNum     = *off;
        erCarName    = *off;
        erCarNameP   = *off;
        erDtaPvdrP   = *off;
        erCarMailP   = *off;
        erFeinP      = *off;
        erCarPhnP    = *off;
        erCarFaxP    = *off;
        erCarAddP    = *off;
        erCarCityP   = *off;
        erCarStatP   = *off;
        erCarZipP    = *off;
        erCarNameS   = *off;
        erDtaPvdrS   = *off;
        erCarMailS   = *off;
        erFeinS      = *off;
        erCarPhnS    = *off;
        erCarFaxS    = *off;
        erCarAddS    = *off;
        erCarCityS   = *off;
        erCarStatS   = *off;
        erCarZipS    = *off;

        msCarNum     = ' ' ;
        msCarName    = ' ' ;
        msCarNameP   = ' ' ;
        msDtaPvdrP   = ' ' ;
        msCarMailP   = ' ' ;
        msFeinP      = ' ' ;
        msCarPhnP    = ' ' ;
        msCarFaxP    = ' ' ;
        msCarAddP    = ' ' ;
        msCarCityP   = ' ' ;
        msCarStatP   = ' ' ;
        msCarZipP    = ' ' ;
        msCarNameS   = ' ' ;
        msDtaPvdrS   = ' ' ;
        msCarMailS   = ' ' ;
        msFeinS      = ' ' ;
        msCarPhnS    = ' ' ;
        msCarFaxS    = ' ' ;
        msCarAddS    = ' ' ;
        msCarCityS   = ' ' ;
        msCarStatS   = ' ' ;
        msCarZipS    = ' ' ;

        If inMode = 'A';

          // Carrier #
           If scCarNum = *Zeros;
              msCarNum = 'Carrier # cannot be left Empty';
              erCarNum = *on;
              indError = *On;
           Endif;

          // Carrier # Duplicate Check
           Chain(n) (scCmpNum : scCarNum) ptm00;
           If %found(WMTML1);
              msCarNum = 'Carrier # already exists';
              erCarNum = *on;
              indError = *On;
           EndIf;

          // Carrier Name
           If scCarName = *Blanks;
              erCarName = *on;
              msCarName = 'Carrier Name cannot be left Empty';
              indError = *On;
           Endif;

        EndIf;

       // WCPOLS - TFEIN
        If scTFienP   = *Zeros and scDatPvdP = 'T';
           erFeinP    = *on;
           msFeinP    = 'WCPOLS TPE Fein cannot be left Empty';
           indError = *On;
        Endif;

       // WCSTAT - TFEIN
        If scTFienS   = *Zeros and scDatPvdS = 'T';
           erFeinS    = *on;
           msFeinS    = 'WCSTAT TPE Fein cannot be left Empty';
           indError = *On;
        Endif;

      // Return *on indicator when No error exists.
       If indError = *On;
          Return *Off;
       Else;
          Return *On;
       EndIf;

      /End-Free
     pValidateCarrier  e

     //===================================================================
     // Carrier Detail loading Procedure.
     //===================================================================
     p PrcCarDtl       b
     d PrcCarDtl       pi
     d inSflrrn                       9  0
     d inprrn          s              9  0 Inz(0)
     d
     d strAdL1         s             40    inz
     d strAdL2         s             40    inz
     d strAdL3         s             40    inz
      /Free
        scDtlCar#   = *blanks;
        scDtlCarNm  = *blanks;
        scDtPerNam  = *blanks;

        scDtNameP   = *blanks;
        scDtMailP   = *blanks;
        scDtPhnP    = *Zeros;
        scDtExtP    = *Zeros;
        scDtFaxP    = *Zeros;
        scDtAdrP    = *blanks;
        scDtCtyP    = *blanks;
        scDtStP     = *blanks;
        scDtZipCdP  = *Zeros;
        scDtDPP     = *blanks;
        scDtTFeinP  = *Zeros;

        scDtNameS   = *blanks;
        scDtMailS   = *blanks;
        scDtPhnS    = *Zeros;
        scDtExtS    = *Zeros;
        scDtFaxS    = *Zeros;
        scDtAdrS    = *blanks;
        scDtCtyS    = *blanks;
        scDtStS     = *blanks;
        scDtZipCdS  = *Zeros;
        scDtDPS     = *blanks;
        scDtTFeinS  = *Zeros;

        inprrn = scDbCarRrn;
        inprrn = inSflrrn;
        Chain inprrn SflCarrInf;
        Chain(n) (scCmpNum: HdCarCode) ptm00;
        If %found(wmtml1);
           scDtlCar#  = scCarCode ;
           scDtlCarNm = scCarName ;
           If TMapnD = 'Y';
              scDtPerNam = 'Yes';
           Else;
              scDtPerNam = 'No';
           EndIf;


           If TMCar# = 0 ;
              lblAlwPerV = *On;
           Else;
              lblAlwPerV = *Off;
           EndIf;

          // WCPOLS Details
           scDtNameP   = TMname;
           scDtMailP   = TMmail;
           scDtPhnP    = TMphn#;
           scDtExtP    = TMext#;
           scDtFaxP    = TMfax#;
           scDtAdrP    = TMadr1;
           scDtCtyP    = TMcty;
           scDtStP     = TMst;
           scDtZipCdP  = TMzip;
           scDtTFeinP  = TMFein;
           Select;
           When TMDType = 'C';
              scDtDPP = 'Insurance Carrier';
           When TMDType = 'D';
              scDtDPP = 'Data Collection Organization';
           When TMDType = 'T';
              scDtDPP = 'Third Party Entity';
           EndSL;

          // WCSTAT Details
           scDtNameS   = TMSname;
           scDtMailS   = TMSmail;
           scDtPhnS    = TMSphn#;
           scDtExtS    = TMSExt#;
           scDtFaxS    = TMSfax#;
           scDtAdrS    = TMSadr1;
           scDtCtyS    = TMScty;
           scDtStS     = TMSst;
           scDtZipCdS  = TMSzip;
           scDtTFeinP  = TMSFein;
           Select;
           When TMSDType = 'C';
              scDtDPS = 'Insurance Carrier';
           When TMSDType = 'D';
              scDtDPS = 'Data Collection Organization';
           When TMSDType = 'T';
              scDtDPS = 'Third Party Entity';
           EndSL;

        Endif;

      /End-Free
     p PrcCarDtl       e

     ****  End Add  ***  03/10/22  *********************************************
** Masking data
SSN
BANK
