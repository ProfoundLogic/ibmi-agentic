     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H BNDDIR('WTBNDDIR')
     h dftactgrp(*no) actgrp(*caller)
     h**********************************************************************
     h/TITLE WTCLMCON - Claims Contact Information
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Claims Contact Info.
     h*
     h*  Called from: Profound UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 7/9/2012 by C:279 for Project WC2115
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h* 05/26/25 C:333 Project: WC3009 (Recompile)                         *
     h*                Clms, Policy, Agncy contacts: Maint Info            *
     h*                - DSPF changes.                                     *
     h*                                                                    *
     h* 10/28/24 C:342 Ticket: 117560/WC2826                               *
     h*                1.refresh values of wmemp and wmcmp file if updates *
     h*                  in other session. It will load latest values in   *
     h*                  SflConInfo and SflPolCont if refresh buttion is   *
     h*                  clicked.                                          *
     h*                2.Claims Contacts.                                  *
     h*                  - Writing/Updating/Deleting Staff Assignments     *
     h*                  to WMECP with unique seq#.                        *
     h*                  - Commented LoadUsrParmt procedure and            *
     h*                    accommodate it with StaffAssign and             *
     h*                    LoadPolContact                                  *
     h*                3.Added Distribution Type in Detail box of both grid*
     h*                4.Loaded Distribution Type for 2nd grid for contact *
     h*                  loaded from WMA5PCRT/WMAPPCRT                     *
     h*                5.Clear stcnl fields if record not found. Loading   *
     h*                  blank value in case cancel pressed on edit screen *
     h*                                                                    *
     h* 10/28/24 C:342 Ticket: 117560/WC2826                               *
     h*                DSPF Changes                                        *
     h*                1.Added Division colomn in Policy Contact grid      *
     h*                  sflPolCont.                                       *
     h*                2.Added Distribution Types and Contact Type colomn  *
     h*                  in Policy Contact grid sflPolCont.                *
     h*                                                                    *
     h* 09/24/24 C:342 Ticket: 117552                                      *
     h*                DSPF Changes                                        *
     h*                1.Increase the width of Distribution Type,          *
     h*                  Sub Categories and Process Output Control         *
     h*                  multi select box on rcdAddAgCo                    *
     h*                                                                    *
     h* 09/18/24 C:342 WC2826/117560                                       *
     h*                Claims Contacts.                                    *
     h*                1.Load Contact Type  Column with description of     *
     h*                  Non-system contact and blank for Contact For column
     h*                2.Add/Edit- Load Contact Type with blank for System *
     h*                  Contact.                                          *
     h*                3.Set index correctly as it is set in Policy Contact*
     h*                  program. Set it with 1 when writing to subfile.   *
     h*                  Due to other write operation of SflPolCont with   *
     h*                  scsflRRN indx is not set properly.                *
     h*                DSPF Changes-                                       *
     h*                1.Added Distribution Type Column in Contact Type    *
     h*                  grid and loaded the same in LoadDistType          *
     h*                2.Added Contact Type Column in Contact Type         *
     h*                  grid.                                             *
     h*                                                                    *
     h* 07/30/24 C:342 WC2826/115874/116975/116976/116980/117373           *
     h*                Claims Contacts.                                    *
     h*                1. Dynamic label for Name field in add/edit mode    *
     h*                2.Load Claimant file Information in Claim Contact   *
     h*                  Grid using WMCDP file.                            *
     h*                3.Treating Physician Selection criteria changed     *
     h*                4.Additional Payee Name same as  Additional Payee   *
     h*                  Grid                                              *
     h*                5.Load name in grid as First_Name Mid_Initial       *
     h*                  Last_name when applicable.                        *
     h*                6.Initialize arrary element for aryDiv              *
     h*                7.Load Agency Internal Contacts in Policy Contacts  *
     h*                  Grid.                                             *
     h*                8.Load Claimant Information in Claim Contact Grid.  *
     h*                9.Dynamic label name based on System and Non-System *
     h*                  contacts.                                         *
     h*                                                                    *
     h* 03/04/24 C:342 Project: WC2826                                     *
     h*                Claims Contacts                                     *
     h*                DSPF Changes-                                       *
     h*                1. Added System and Staff Code colomn in Claims     *
     h*                Contacts grid.                                      *
     h*                2. Added Policy Contact grid.                       *
     h*                3. Added rcdClmCAdd record format.                  *
     h*                                                                    *
     h* 03/25/22 C:333 Project: SY434                                      *
     h*                - Update Grids for Sorting Filtering Upgrade        *
     h*                                                                    *
     h*  08/10/20 C:300 Support: 90306 (In-House)                         *
     h*                 Correct 'Physian' with 'Physician'                 *
     h*                                                                    *
     h*  06/09/20  C:298 Project: SY420                                    *
     h*                  Mask SSN                                          *
     h*                                                                    *
     h*  03/23/16 C:308 Project: ELEVATE                                   *
     h*                 Recompile only                                     *
     h*                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
      *                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
     h*
     h*  04/16/15 C:65  Project: SY251
     h*                 Add API call to launch new Department letters.
     h*
     h* 03/19/15  C:279 Support: 32825 (Phaseware)
     h*                 Restructured program logic to not exit on enter.
     h*
     h* 02/03/15  C:288 Phaseware: 29629 inhouse
     h*                 Change to display defense and plaintiff attorny record
     h*
     h* 01/21/15  C:282 Redmine: 4728
     h*                 Read the Treating Physician history file and if record
     h*                 doesn't exist then check the Payee Demographics
     h*                 master file.
     h*
     h* 10/29/14  C:295 Redmine: 4728
     h*                 There is a  Blank Record in top of the contacts
     h*                 in contact information.
     h*
     h* 10/01/14  C:280 Support: 84843
     h*                 Correct the display of attorney                anel
     h*
     h* 07/28/14  C:295 Project: 114WMD
     h*                 Compilation only as the changes are in Action Panel
     h*
     h* 02/28/14  C:290 Support: 84927
     h*           Changed Ajax call to display dates corresponding to the
     h*           Attorney selected from the Contacts Information grid.
     h*           Contains Display file changes also.
     h*
     h* 02/17/14  C:295 Support: 84830
     h*                 Fix to retrive the information to be shown on grid
     h*                 For Defense attorney.
     h*
     h* 09/04/13  C:267 Redmine: 2255 (Haifa Kealy)
     h*                 When a record in the subfile is clicked, do not go
     h*                 to the server to retrieve the data.  Use an AJAX
     h*                 program WTCLMCON01 called from the onrowclick event
     h*                 to populate the detail data underneath the subfile.
     h*
     h* 09/03/13  C:189 Redmine: 3015
     h*                 Use include instead of ignore for action panel record
     h*                 format
     h*
     h* 08/13/13  C:291 Redmine: 2970
     h*                 Modified to display correct Attorneys phone number
     h*
     h* 04/29/13  C:279 Redmine: 2814
     h*                 Modified to display hover for email envelope.
     h*
     h**********************************************************************
     fwtClmCon  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflConInfo:rrnContact)
     f                                     Sfile(sflPolCont:scSflRRN)
     f                                     InfDs(fids01)
     fwtActPnl  cf   e             WorkStn
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f*********** beg chg ****** 09/03/13 *************************
     f                                     Include(rcdClmPnl)
     f*********                            Ignore(rcdPolPnl)
     f*********** end chg ****** 09/03/13 *************************
     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
     fwmllp     if   e             Disk    ReName(pll01:pll0p)
     fwmtpp     if   e             Disk    ReName(ptp01:ptp0p)
     fwmecp     if   e             Disk    ReName(pec01:pec0p)
     fwmlll     if   e           k Disk    ReName(pll01:pll00)
     f                                     InfDs(dbfds)
     fwmtpl1    if   e           k Disk    InfDs(dbfds2)
     fwmecl     if   e           k Disk    ReName(pec01:pec00)
     f                                     InfDs(dbfds3)
     Fwmexl     if   e           k disk    ReName(pex01:pex00)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     fwmpdl     if   e           k Disk    ReName(ppd01:ppd00)
      **** Beg Add *** 03/04/24 ****************************
     fwmsbp     if   e             Disk    ReName(psb01:psb0p)
     fwmsbl     if   e           k Disk    ReName(psb01:psb00)
     f                                     InfDs(dbfds4)
     fwdacp     if   e             Disk    ReName(pac01:pac0p)
     fwdacl     if   e           k Disk    ReName(pac01:pac00)
     f                                     InfDs(dbfds5)
     fwmcnp     uf a e             Disk    ReName(pcn01:pcn0p)
     f                                     InfDs(dbfds7)
     fwmcnl     if   e           k Disk    ReName(pcn01:pcn00)
     f                                     InfDs(dbfds6)
     fwmcnpcrt  uf a e           k Disk    ReName(pcn01:pcn0p_cr)
     f                                     prefix(CR_)
     fstcnl     if   e           k Disk    ReName(pcn01:pcn01)
     f                                     prefix(S_)
      * files for agency contact
     fwmahl     if   e           k disk    rename(pah01:pah00)
     fwmaal1    if   e           k disk    rename(paa01:paa01)
     fwmagl     if   e           k disk    rename(pag01:pag00)
     fwmapl     if   e           k Disk    rename(pap01:pap01)
     f                                     infds(dbfds8)
     fwtayl1    if   e           k disk    rename(pay01:pay01)
      * files to load Internal Claim/Policy Contacts from SMUPP file
     fsmupl1    if   e           k disk    rename(pup01:pup01)
     f                                     infds(dbfds9)
     fsmupp     if   e             disk    rename(pup01:pup0p)
     fwmcxl     if   e           k disk    rename(pcx01:pcx00)
      **** End Add *** 03/04/24 ****************************
      **** Beg Add *** 07/30/24 ****************************
     fwmcdp     if   e             Disk    ReName(pcd01:pcd0p)
     fwmcdl     if   e           k Disk    ReName(pcd01:pcd00)
     f                                     Infds(dbfds10)
     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
      **** End Add *** 07/30/24 ****************************
      **** Beg Add *** 09/18/24 ****************************
     fstdsl     if   e           k Disk    ReName(pds01:pds00)
      **** End Add *** 09/18/24 ****************************
      **** Beg Add *** 10/28/24 ****************************
     fwmeclcrt  if   e           k disk    rename(pec01:pec0p_cl)
     f                                     prefix(CL_)
     fwmecpcrt  uf a e           k disk    rename(pec01:pec0p_cr)
     f                                     prefix(CR_)
     fwmecl1    uf a e           k disk    rename(pec01:pec01)
     fwma5pcrt  if   e           k disk    rename(pac01:pac0p_cr)
     f                                     prefix(CR_)
     fwmappcrt  if   e           k disk    rename(pap01:pap0p_cr)
     f                                     prefix(CR_)
      **** Beg Add *** 10/28/24 ****************************
     d* Proto-types

     d* Clear detail box
       //******** Beg dlt ****** 09/04/13 *************************
     d* //*** ClearScreen     pr
       //******** End dlt ****** 09/04/13 *************************

     d* Display main screen
     d DisplayMain     pr

     d* Load Contact Information
     d LoadContact     pr

       //*** Beg Add *** 03/04/24 *********************************
     d* Load agency contact information
     d loadAContact    pr

     d* Load Contact Information for Policy
     d LoadPolContact  pr

     d* Process for Addition of Claim contacts
     d ProcessAddCon   pr

     d* Process other option for Claim contacts
     d ProcOtherCon    pr

     d* Validate Claim contacts
     d ValidateClm     pr              n
     d strmode                        1
       //*** End Add *** 03/04/24 *********************************
       //*** Beg Add *** 07/30/24 *********************************
      * Load Internal Agency Contacts
     dLoadWmagp        pr
     d inCo#                               like(AGco#)
     d inFein                              like(AGfein)
     d inMod                               like(AGmod)
       //*** End Add *** 07/30/24 *********************************
       //*** Beg Add ***** 10/28/24 ****************************************
     d* Staff Assignment
     d StaffAssign     pr
       //*** Beg Add ***** 10/28/24 ****************************************

     d* Load Defense Attorney Contact Info detail box
       //******** Beg dlt ****** 09/04/13 *************************
     d*** LoadDefAtt      pr

     d* Load Plaintiff Attorney Contact Info detail box
     d*** LoadPlnAtt      pr

     d* Load Treating Physian Contact Info detail box
     d*** LoadTrtPhy      pr

     d* Load Employer Contact Info detail box
     d*** LoadEmpCon      pr

     d* Load Contact Information
     d*** Process         pr
       //******** End dlt ****** 09/04/13 *************************

     d* General Prototypes
     d/copy sPrAtrium
     d/copy sCopyDsPhx
     d/copy sPrString
     d/copy wPrGroup
     d/copy wPrUsrPrms
     d/copy sPrDate
     d/copy wPrClaims
     d/copy wPrAgent
     d/copy wPrContact
     d/copy wPrLoc
     d/copy wPrEmplr
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************
       //*** Beg Add ***  03/04/24  ****************************************
     d/copy sPrGenErr
     d/copy sPrGenPop
     d/copy sPrSql
       //*** End Add ***  03/04/24  ****************************************

     d* Load Action Panel procedure Prototype and variables
     d/Copy sCopyClmPr

     D* System Data Structures
     D dbfds           ds
     D  rrnWmlll             397    400b 0

     D dbfds2          ds
     D  rrnWmtpl1            397    400b 0

     D dbfds3          ds
     D  rrnWmecl             397    400b 0
      **** Beg Add *** 03/04/24 ****************************
     d dbfds4          ds
     d  rrnWmsbl             397    400b 0
     d dbfds5          ds
     d  rrnWdacl             397    400b 0
     d dbfds6          ds
     d  rrnWmcnl             397    400b 0
     d dbfds7          ds
     d  rrnWmcnp             397    400b 0
     d dbfds8          ds
     d  rrnwmapl             397    400b 0
     d dbfds9          ds
     d  rrnsmupl1            397    400b 0
      **** Beg Add *** 07/30/24 ****************************
     d dbfds10         ds
     d  rrnWMCDL             397    400b 0
      **** End Add *** 07/30/24 ****************************

     d strSelDSPval    s           1024
     d conDist         c                   CONST('Distribution')
     d conSubCat       c                   CONST('Sub Category')
     d conDProcOp      c                   CONST('Process Output Control')
     d ConCtxt         c                   CONST('Claims')
     d ConCtxtPol      c                   CONST('Policy')
     d conType         s             25
     d svSeq#          s                   like(CNseq#)
     d strSelDistT     s           1024
     d strSelSubCat    s           2000
     d strSelProcOp    s           3000
     d svsflrrn1       s              9  0
     d isError         s               n   inz
     d flgRcdUpdated   s              1    inz
     d ##type          s              1
     d saveDT          s                   like(CNdt)
     d saveTM          s                   like(CNtime)
     d svLnameE        s                   like(scLnameE)
     d svFnameE        s                   like(scFnameE)
     d svConTypE       s                   like(scConTypE)
     d isDelResp       s               n   inz
     d maxseq#         s             15  0

      *Work variable to load agency contacts

     ddsWMAHP          ds                  inz qualified
     d co#                            3  0
     d fnd                            3  0
     d emp#                           9  0
     d div                            5  0
     d fyr                            3  0
     d efDt                           7  0
     d agt#1                          5  0
     d prod1                          5  0
     d agt#2                          5  0
     d prod2                          5  0
     d agt#3                          5  0
     d prod3                          5  0

     d strSql          s           5000    varying inz
     d ##co            s                   like(CMco#)
     d ##fnd           s                   like(CMfnd)
     d ##emp           s              9  0

     d#ofDiv           c                   99999
     d#ofProd          c                   25
     daryAgtProd       ds                  likeds(dsAgtProd) dim(#ofProd) inz
     daryDiv           ds                  likeds(dsDiv) dim(#ofProd)
     ddsAgtProd        ds                  qualified
     d key                           23
     d group                          3  0 overlay(key:1)
     d agt#                           5  0 overlay(key:4)
     d prod                           5  0 overlay(key:9)
     d seq#                          15  0 overlay(key:9)
     d dsDiv           ds                  qualified
     d div                            5  0 dim(#ofDiv)

     d isValid         s               n   inz(*off)
     d kkmod           s              5  0
     d kkFYR           s              3  0
     d meetAgn         s              1

     d kkprod          s                   like(AHprod)
     d kkFein          s              9  0
     d kkMod1          s              3  0
     d intDiv          s              5  0 inz
     d agt             s              5  0 DIM(999)
     d found           s              2  0 inz
     d indx            s              3  0
     dindxDiv          s              6  0
      **** Beg Add *** 09/18/24 ****************************
      // set index to 1 as it is set in Policy Contact program.
      // Due to other write operation of SflPolCont. indx is not set properly
     d SetIndx         s              3  0 inz(*zeros)
      **** End Add *** 09/18/24 ****************************

      *Work varibale for Internal Claims Contacts (SMUPP file)
      // Staff codes for fields from WMCNP file for Internal Claim Contacts
     d                 ds                  inz
     d  StafCode               1     24
     d                                     dim(8)
     d  CMaman                 1      3
     d  CMnrse                 4      6
     d  CMuman                 7      9
     d  CMfead                10     12
     d  CMesup                13     15
     d  CMsdfa                16     18
     d  CMsupr                19     21
     d  CMclex                22     24

      // Staff codes for fields from WMEMP file for Internal Policy Contacts
     d                 ds                  inz
     d  StafCodeP              1     21
     d                                     dim(7)
     d  EMpsrv                 1      3
     d  EMarvw                 4      6
     d  EMclmc                 7      9
     d  EMuwtr                10     12
     d  EMamgr                13     15
     d  EMpacc                16     18
     d  EMlrep                19     21
     d  i              s              1  0
      **** End Add *** 03/04/24 ****************************
      **** Beg Add *** 07/30/24 ****************************
     d                 ds                  inz
     d  StafCodeAgt            1      6
     d                                     dim(2)
     d  AGsvcr                 1      3
     d  AGmktr                 4      6
     d  inCo#          s                   like(AGco#)
     d  inFein         s                   like(AGfein)
     d  inMod          s                   like(AGmod)
     d  kkTbId         s                   like(GTTbId)
     d  kkkey          s                   like(GTKey)
      **** End Add *** 07/30/24 ****************************
     D*
     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)

     D*Variables
     D @@RRN           s              9
     D @@Mode          s              1
     D @@Next          s             10
     D outClientID     s              3    inz('000')
     D addMode         s               n   inz(*off)
     D chgMode         s               n   inz(*off)
     D dspMode         s               n   inz(*off)
     D first           s               n   inz(*off)
     D inRRNClm        s              9  0
     D inMode          s              1
     D ##fein          s              9  0
     D ##mod           s              3  0
     D kkFrDt          s                   like(TPfrDt) Inz(*Hival)

      **** Beg Add *** 09/18/24 ****************************
     D xxConTyp        s                   like(CNtype) Inz
     D xxSeq           s                   like(CNseq#) Inz
      **** End Add *** 09/18/24 ****************************
      **** Beg Add *** 10/28/24 ****************************
     d xxfsn           s             15
     d xxlsn           s             30
     d xxlen           s              2  0
     d spacePos        s              2  0
     d xxusnm          s             50A
      **** End Add *** 10/28/24 ****************************
     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Next
      *

     C     keyWMECL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

     C     keyWMEXL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd

     C     keyWMLLL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

      //*** Beg Add *** 03/04/24 *******************************************
     c     keyWMSBL      Klist
     c                   Kfld                    CMco#
     c                   Kfld                    CMfnd
     c                   Kfld                    CMfyr
     c                   Kfld                    CMcase

     c     keyWDACL      Klist
     c                   Kfld                    CMco#
     c                   Kfld                    CMfnd
     c                   Kfld                    CMfyr
     c                   Kfld                    CMcase

     c     keyWMCNL      Klist
     c                   Kfld                    CMco#
     c                   Kfld                    CMfnd
     c                   Kfld                    CMfyr
     c                   Kfld                    CMcase

     c     keyWMAPl      Klist
     c                   Kfld                    ##co
     c                   Kfld                    kkfein
     c                   Kfld                    kkmod1
     c                   Kfld                    kkprod

      //*** End Add *** 03/04/24 *******************************************
     C     key1WMTPL1    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMTPL1    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkFrDt

     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

     C     keyWMPDL      Klist
     C                   Kfld                    ##Fein
     C                   Kfld                    ##Mod

      /free

       ExSr  ProgramStart;

       // Load the action panel
       LoadActPan();

       //*** Beg Del *** 03/04/24 *******************************************
       // Load data in the tab
       //*** Exsr TabContactinfo;
       //*** End Del *** 03/04/24 *******************************************

       Dou scMenuRsp <> *Blanks;

          //*** Beg Add *** 03/04/24 *******************************************
          // Load data in the tab
          Exsr TabContactinfo;
          //*** End Add *** 03/04/24 *******************************************
          DisplayMain();

          Select;
       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****   When btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'CS');
       //*** end add *****  04/16/15  ****************************************
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;
       //*** end chg *****  01/07/16  ****************************************

          When TabClick = *On;
             TabClick = *Off;

          When refrshData=*On;
               refrshData=*Off;
               //*** Beg Del *** 03/04/24 ************************************
               //*** Exsr TabContactinfo;
               //*** End Del *** 03/04/24 ************************************
               //*** Beg Add *** 10/28/24 ************************************
               Chain(n) inRRNClm pcm0p;
               Chain keyWMEML pem00;
               //*** End Add *** 10/28/24 ************************************

          When scMenuRsp <> *Blanks;
             @@next = scMenuRsp;
             Leave;

          //*** Beg Add *** 03/04/24 *******************************************
          // Process To add non system contacts
          When BtnContAdd = *on;
             BtnContAdd = *off;
             ProcessAddCon();

          // Process Other options for Claims Contacts
          Other;
             ProcOtherCon();

          //*** End Add *** 03/04/24 *******************************************

       //******** beg dlt ****** 09/04/13 *************************
       //***   Other;
       //***      Process();
       //******** end dlt ****** 09/04/13 *************************
          Endsl;
       Enddo;

       ExSr  ProgramEnd;

       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

       //Load default values or passed in values
       If %parms()>0;
          inRRNClm = %int(@@RRN);
          inMode = @@Mode;
       Else;
          inRRNClm = 41;
          inMode = 'C';
       Endif;

       // Access the claim record
       Chain(n) inRRNClm pcm0p;

       // Get the client id
       GetClientID(outClientID);

       // Populate hidden fields
       scActCltId=outClientID;
       scActRrn=inRrnClm;

       // Set the Mode
       Select;
       When inMode = 'D';
          dspMode = *on;
       When inMode = 'A';
          addMode = *on;
       When inMode = 'C';
          chgMode = *on;
       Endsl;
       scActMode = inMode;

       // Set the Action Panel Height
       aPnlHeight = 1100;

       TabIndex  = 0;

       endsr;
       //===================================================================
       // TabContactinfo - Processing for Claims Contact Tab
       //===================================================================
       Begsr TabContactinfo;

       // Load the subfile grid for Addtional payee Tab
       LoadContact();
       //*** Beg Add ***** 10/28/24 ************************************
       StaffAssign();
       //*** End Add ***** 10/28/24 ************************************
       //*** Beg Add *** 03/04/24 *******************************************
       // Load the subfile grid for Policy Contact
       LoadPolContact();

       // Load Agency Contact Information
       LoadAContact();

       // Load Internal Policy Contact Information from SMUPP User Parameters File
       //*** Beg Del *** 10/28/24 *******************************************
       //*** LoadUsrParm();   //Accommodate this in LoadPolContact as we introduce StaffAssign
       //*** End Del *** 10/28/24 *******************************************
       //*** End Add *** 03/04/24 *******************************************

       endsr;
       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;

       *inlr = *on;
       Return;

       endsr;
      /end-Free
       //===================================================================
       // clearScreen - Clear detail box
       //===================================================================
       //******** Beg dlt ****** 09/04/13 *************************
     P* //*** clearScreen     B
     D* //*** clearScreen     PI
      * //*** /Free
        //*** Clear DtlConFor;
        //*** Clear DtlCnEffDt;
        //*** Clear DtlCnExpDt;
        //*** Clear DtlCnRprDt;
        //*** Clear DtlCnAsnDt;
        //*** Clear DtlConPst;
        //*** Clear DtlConName;
        //*** Clear DtlConAdr1;
        //*** Clear DtlConAdr2;
        //*** Clear DtlConAdr3;
        //*** Clear DtlConPhn#;
        //*** Clear DtlConFax#;
        //*** Clear DtlConCell;
      * //*** /End-Free
     P* //*** clearScreen     E
       //******** End dlt ****** 09/04/13 *************************
       //===================================================================
       // DisplayMain - Display main screen
       //===================================================================
     P DisplayMain     B
     D DisplayMain     PI
      /Free

       APnlHeight = 1100;

       Select;
       When TabIndex = 0;
          Write rcdClmPnl;
          Write rcdTabCPnl;
          Exfmt rcdConInfo;
       Endsl;
       //****** beg chg ****** 03/19/15 *************************************
       // Read rcdTabCPnl;

       Read rcdClmPnl;
       Read rcdTabCPnl;
       //****** end chg ****** 03/19/15 *************************************

      /End-Free
     P DisplayMain     E
        //===================================================================
        // Process - Process the selection made on screen
        //===================================================================
        //******** Beg dlt ****** 09/04/13 *************************
     P* //*** Process         B
     D* //*** Process         PI
      *
      * //*** /Free
        //***
        //***   Chain rrnConRcd sflConInfo;
        //***   If %found;
        //***
        //***      If %trim(scConfile) = 'WMLLLDF';
        //***         LoadDefAtt();
        //***      Elseif %trim(scConfile) = 'WMLLLPF';
        //***         LoadPlnAtt();
        //***      Elseif %trim(scConfile) = 'WMTPL1';
        //***         LoadTrtPhy();
        //***      Elseif %trim(scConfile) = 'WMECL';
        //***         LoadEmpCon();
        //***      Endif;
        //***
        //***      rrnRcdNbr = rrnConRcd;
        //***   Endif;
        //***
      * //*** /End-Free
     P* //***  Process         E
      * //******** End dlt ****** 09/04/13 *************************
       //===================================================================
       // LoadContact - Load Contact information on screen
       //===================================================================
     P LoadContact     B
     D LoadContact     PI
      /Free

       first = *off;
       rrnContact = 0;
       dspContact = *off;
       ClrContact = *on;
       Write rcdConInfo;
       dspContact = *on;
       ClrContact = *off;

       //Retrive the information to be shown on grid
       //*** Beg Add *** 09/18/24 *************************************
       Clear scConTyp;
       //*** Beg Add *** 09/18/24 *************************************

       Chain KeyWMLLL  pll00;
       If %found(Wmlll);

          // Write Defense attorney contact info
          //***** Beg add ****** 02/17/14 *************************
          //***** Beg Chg ****** 02/03/15 *************************
          //***** If LLdefa <> 0  ;
          If LLdefa <> 0  or lldefn <> *blanks;
          //***** End Chg ****** 02/03/15 *************************
          //***** End add ****** 02/17/14 *************************
          ##fein = LLdefa;
          ##mod  = LLdefm;
          //***** beg add ****** 04/29/13 *************************
          scConEmad  = LLemdd;
          scConEmadT = LLemdd;
          //***** end add ****** 04/29/13 *************************
          //***** beg add ****** 08/13/13 *************************
          Clear scConPhn#;
          If LLdefr <> *zeros or LLdefp <> *zeros;
            scConPhn# = '(' + %editc(LLdefr:'X') + ')' +
            %subst(%editc(LLdefp:'X'):1:3) + '-' +
            %subst(%editc(LLdefp:'X'):4:4);
          Endif;
          //***** end add ****** 08/13/13 *************************
          //*** Beg Add *** 03/04/24 *******************************************
          scSystem  = 'Y';
          scToolTip = 'To update this information go to system location';
          scStfCode = ' ';
          scConPst  = ' ';
          dsbDlt    = *on;
          scConPNam = LLdefn;
          //*** End Add *** 03/04/24 *******************************************

          Chain keyWMPDL ppd00;
          If %found(Wmpdl);
             //*** Beg Add *** 03/04/24 *******************************************
            If scConPNam = *blanks;
             //*** End Add *** 03/04/24 *******************************************
             If PDdba1 = 'Y';
                scConPNam = PDadr1;
             Else;
                scConPNam = PDname;
             Endif;
             //*** Beg Add *** 03/04/24 *******************************************
            Endif;
             //*** End Add *** 03/04/24 *******************************************
              //***** beg add ****** 08/13/13 *************************
            If scConPhn# = *Blanks;
              //***** end add ****** 08/13/13 *************************
             If PDarea <> *zeros or PDphn# <> *zeros;
                scConPhn# = '(' + %editc(PDarea:'X') + ')' +
                 %subst(%editc(PDphn#:'X'):1:3) + '-' +
                 %subst(%editc(PDphn#:'X'):4:4);
             Endif;
             //***** beg add ****** 08/13/13 *************************
            Endif;
             //***** end add ****** 08/13/13 *************************
             //***** beg add ****** 04/29/13 *************************
             If LLemdd = *Blanks;
                scConEmad  = PDemad;
                scConEmadT = PDemad;
             Endif;
             //***** end add ****** 04/29/13 *************************
          Else;
             scConPNam = LLdefn;
             If LLdefr <> *zeros or LLdefp <> *zeros;
                scConPhn# = '(' + %editc(LLdefr:'X') + ')' +
                 %subst(%editc(LLdefp:'X'):1:3) + '-' +
                 %subst(%editc(LLdefp:'X'):4:4);
             Endif;
          Endif;
          scConFor  = 'Defense Attorney';
          //***** beg add ****** 04/29/13 ****************************
          If scConEmadT = *Blanks;
             scConEmadT = noEmail;
          EndIf;
          //***** end add ****** 04/29/13 ****************************

          // Write Defense attorney contact info detail box
       //******** beg dlt ****** 09/04/13 *************************
          //***If first = *off;
             //*** first = *on;
             //*** DtlConFor  = scConFor;
             //*** DtlCnAsnDt = CvtToDate(dtMDYY(LLdedt));
             //*** DtlConName  = scConPNam;
             //*** ##fein = LLdefa;
             //*** ##mod  = LLdefm;
             //*** Chain keyWMPDL ppd00;
             //*** If %found(Wmpdl);
             //***   If PDdba1 = 'Y';
             //***      DtlConAdr1  = PDadr2;
             //***   Else;
             //***      DtlConAdr1  = PDadr1;
             //***      DtlConAdr2  = PDadr2;
             //***   Endif;
             //***   If DtlConAdr2<>*Blanks;
             //***      DtlConAdr3 = %Trim(PDcty) + ', ' + PDst + ' ' +
             //***       %EditW(PDzip:'     -    ');
             //***   Else;
             //***      DtlConAdr2 = %Trim(PDcty) + ', ' + PDst + ' ' +
             //***       %EditW(PDzip:'     -    ');
             //***      DtlConAdr3 = *Blanks;
             //***   EndIf;
             //***   If PDfxar <> *zeros or PDfax# <> *zeros;
             //***      DtlConFax#  = '(' + %editc(PDfxar:'X') + ')' +
             //***       %subst(%editc(PDfax#:'X'):1:3) + '-' +
             //***       %subst(%editc(PDfax#:'X'):4:4);
             //***   Endif;
             //***Else;
             //***   DtlConAdr1  = LLdad1;
             //***   DtlConAdr2  = LLdad2;
             //***   If DtlConAdr2<>*Blanks;
             //***      DtlConAdr3 = %Trim(LLdCty) + ', ' + LLdSt + ' ' +
             //***       %EditW(LLdZip:'     -    ');
             //***   Else;
             //***      DtlConAdr2 = %Trim(LLdCty) + ', ' + LLdSt + ' ' +
             //***       %EditW(LLdZip:'     -    ');
             //***      DtlConAdr3 = *Blanks;
             //***   EndIf;
             //***   If LLdefe <> *zeros or LLdeff <> *zeros;
             //***      DtlConFax#  = '(' + %editc(LLdefe:'X') + ')' +
             //***       %subst(%editc(LLdeff:'X'):1:3) + '-' +
             //***       %subst(%editc(LLdeff:'X'):4:4);
             //***   Endif;
             //***Endif;
             //*** If scConPhn# <> *blanks;
             //***    DtlConPhn#  = scConPhn#;
             //*** Endif;
             //***Endif;
       //******** end dlt ****** 09/04/13 *************************

          scConRRN = rrnWmlll;
          scConFile= 'WMLLLDF';
          //*** Beg Add*** 09/18/24 *******************************************
          xxConTyp = 'DEFENSE ATTORNEY';
          xxSeq = 1;
          Exsr LoadDistType;
          //*** End Add*** 09/18/24 *******************************************

          rrnContact  = rrnContact + 1 ;
          selContact=*Off;
          //*** Beg Chg *** 03/04/24 *******************************************
          //*** If rrnContact=1;
          //***   selContact=*On;
          //*** EndIf;
          If svsflrrn1 = 0;
             If rrnContact  = 1;
                selContact = *On;
             Endif;
          Else;
             If rrnContact = svsflrrn1;
                selContact = *On;
             Endif;
          Endif;
          //*** End Chg *** 03/04/24 *******************************************

          Write sflConInfo;
       //***** Beg add ****** 02/17/14 *************************
        Endif;
       //***** End add ****** 02/17/14 *************************

          // Write Plaintiff attorney contact info
          //***** Beg add ****** 02/17/14 *************************
          //***** Beg Chg ****** 02/03/15 *************************
          //***** If LLplaa <> 0  ;
        If LLplaa <> 0 or llplan <> *blanks ;
          //***** End Chg ****** 02/03/15 *************************
          //***** End add ****** 02/17/14 *************************
          ##fein = LLplaa;
          ##mod  = LLplam;
          //***** beg chg ****** 10/01/14 *************************
          //***** beg chg ****** 10/29/14 *************************
          //****** Endif;
          //***** End chg ****** 10/29/14 *************************
          //***** end chg ****** 10/01/14 *************************
          //***** beg add ****** 04/29/13 *************************
          scConEmad  = LLemdp;
          scConEmadT = LLemdp;
          //***** end add ****** 04/29/13 *************************
          //***** beg add ****** 08/13/13 *************************
          Clear scConPhn#;
          If LLplar <> *zeros or LLplap <> *zeros;
             scConPhn# = '(' + %editc(LLplar:'X') + ')' +
              %subst(%editc(LLplap:'X'):1:3) + '-' +
              %subst(%editc(LLplap:'X'):4:4);
          Endif;
          //***** end add ****** 08/13/13 *************************
          //*** Beg Add *** 03/04/24 *******************************************
          scSystem  = 'Y';
          scToolTip = 'To update this information go to system location';
          scStfCode = ' ';
          scConPst  = ' ';
          dsbDlt    = *on;
          scConPNam = LLplan;
          //*** End Add *** 03/04/24 *******************************************

          Chain keyWMPDL ppd00;
          If %found(Wmpdl);
             //*** Beg Add *** 03/04/24 *******************************************
            If scConPNam = *blanks;
             //*** End Add *** 03/04/24 *******************************************
             If PDdba1 = 'Y';
                scConPNam = PDadr1;
             Else;
                scConPNam = PDname;
             Endif;
             //*** Beg Add *** 03/04/24 *******************************************
            Endif;
             //*** End Add *** 03/04/24 *******************************************
              //***** beg add ****** 08/13/13 *************************
            If scConPhn# = *Blanks;
              //***** end add ****** 08/13/13 *************************
             If PDarea <> *zeros or PDphn# <> *zeros;
                scConPhn# = '(' + %editc(PDarea:'X') + ')' +
                 %subst(%editc(PDphn#:'X'):1:3) + '-' +
                 %subst(%editc(PDphn#:'X'):4:4);
             Endif;
              //***** beg add ****** 08/13/13 *************************
            Endif;
             //***** end add ****** 08/13/13 *************************
             //***** beg add ****** 04/29/13 *************************
             If LLemdp = *Blanks;
                scConEmad  = PDemad;
                scConEmadT = PDemad;
             Endif;
             //***** end add ****** 04/29/13 *************************
          Else;
             scConPNam = LLplan;
             If LLplar <> *zeros or LLplap <> *zeros;
                scConPhn# = '(' + %editc(LLplar:'X') + ')' +
                 %subst(%editc(LLplap:'X'):1:3) + '-' +
                 %subst(%editc(LLplap:'X'):4:4);
             Endif;
          Endif;
          //***** Beg add ****** 10/01/14 *************************
          //***** Beg Chg ****** 02/03/15 *************************
          //***** If LLplaa <> 0  ;
          If LLplaa <> 0 or llplan <> *blanks ;
          //***** End Chg ****** 02/03/15 *************************
          //***** End add ****** 02/17/14 *************************
          //***** end add ****** 10/01/14 *************************
          scConFor  = 'Plaintiff Attorney';
          //***** Beg add ****** 10/01/14 *************************
          Endif;
          //***** end add ****** 10/01/14 *************************
          //***** beg add ****** 04/29/13 ****************************
          If scConEmadT = *Blanks;
             scConEmadT = noEmail;
          EndIf;
          //***** end add ****** 04/29/13 ****************************

          scConRRN = rrnWmlll;
          scConFile= 'WMLLLPF';
          //*** Beg Add*** 09/18/24 *******************************************
          xxConTyp = 'PLAINTIFF ATTORNEY';
          xxSeq = 1;
          Exsr LoadDistType;
          //*** End Add*** 09/18/24 *******************************************

          rrnContact  = rrnContact + 1 ;
          selContact=*Off;
          //*** Beg Chg *** 03/04/24 *******************************************
          //*** If rrnContact=1;
          //***   selContact=*On;
          //*** EndIf;
          If svsflrrn1 = 0;
             If rrnContact  = 1;
                selContact = *On;
             Endif;
          Else;
             If rrnContact = svsflrrn1;
                selContact = *On;
             Endif;
          Endif;
          //*** End Chg *** 03/04/24 *******************************************
          Write sflConInfo;
       //***** Beg add ****** 02/17/14 *************************
       //***** beg chg ****** 10/01/14 *************************
       //***** beg chg ****** 10/29/14 *************************
       //***** Endif;
        Endif;
       //***** End chg ****** 10/29/14 *************************
       //***** beg chg ****** 10/01/14 *************************
       //***** End add ****** 02/17/14 *************************

       Endif;

       kkFrDt=DateTo7(SystemDate());
       //*** Beg Chg *** 07/30/24 *************************************
       //*** SetLL  Key2WMtpl1  ptp01;
       //*** ReadE  Key1WMtpl1  ptp01;
       //*** DoW Not %Eof(Wmtpl1);
          //*** If TPpRol='TR' And (TPtoDt>=DateTo7(SystemDate()) or TPtoDt=0);
          //***   Leave;
          //*** EndIf;
          //*** ReadE  Key1WMtpl1  ptp01;
       //*** EndDo;

       //*** If Not %Eof(Wmtpl1);

       SetLL  Key1WMtpl1  ptp01;
       ReadE  Key1WMtpl1  ptp01;
       DoW Not %Eof(Wmtpl1);
       //*** End Chg *** 07/30/24 *************************************
          // Write Trating physian contact info
          ##fein = TPphyf;
          ##mod  = TPphym;
          //***** beg add ****** 04/29/13 *************************
          scConEmad  = TPemad;
          scConEmadT = TPemad;
          //***** end add ****** 04/29/13 *************************
          //***** Beg add ****** 01/21/15 *************************
          Clear scConPNam;
          Clear scConPhn#;
          scConPNam = TPphyn;
          If TPphyr <> *zeros or TPphyp <> *zeros;
               scConPhn# = '(' + %editc(TPphyr:'X') + ')' +
                %subst(%editc(TPphyp:'X'):1:3) + '-' +
                %subst(%editc(TPphyp:'X'):4:4);
          Endif;

          //***** End add ****** 01/21/15 *************************
          //*** Beg Add *** 03/04/24 *******************************************
          scSystem  = 'Y';
          scToolTip = 'To update this information go to system location';
          scStfCode = ' ';
          scConPst  = ' ';
          dsbDlt    = *on;
          //*** End Add *** 03/04/24 *******************************************

          Chain keyWMPDL ppd00;
          If %found(Wmpdl);
             //***** Beg add ****** 01/21/15 *************************
             If scConPNam = *blanks and scConPhn# = *blanks;
             //***** End add ****** 01/21/15 *************************
             If PDdba1 = 'Y';
                scConPNam = PDadr1;
             Else;
                scConPNam = PDname;
             Endif;
             //***** Beg add ****** 02/17/14 *************************
             //***** Beg del ****** 01/21/15 *************************
             // Clear scConPhn#;
             //***** End del ****** 01/21/15 *************************
             //***** End add ****** 02/17/14 *************************
             If PDarea <> *zeros or PDphn# <> *zeros;
                scConPhn# = '(' + %editc(PDarea:'X') + ')' +
                %subst(%editc(PDphn#:'X'):1:3) + '-' +
                %subst(%editc(PDphn#:'X'):4:4);
             Endif;
             //***** beg add ****** 04/29/13 *************************
             //***** Beg add ****** 01/21/15 *************************
             Endif;
             //***** End add ****** 01/21/15 *************************
             If TPemad = *Blanks;
                scConEmad  = PDemad;
                scConEmadT = PDemad;
             Endif;
             //***** end add ****** 04/29/13 *************************
             //***** Beg del ****** 01/21/15 *************************
             //Else;
             //scConPNam = TPphyn;
             //If TPphyr <> *zeros or TPphyp <> *zeros;
             //   scConPhn# = '(' + %editc(TPphyr:'X') + ')' +
             //   %subst(%editc(TPphyp:'X'):1:3) + '-' +
             //   %subst(%editc(TPphyp:'X'):4:4);
             //Endif;
             //***** End del ****** 01/21/15 *************************
          Endif;
          //***** beg chg ****** 08/10/20 ****************************
          //***** scConFor  = 'Treating Physian';
          //***** beg chg ****** 07/30/24 ****************************
          //*** scConFor  = 'Treating Physician';
          If TPprol <> *blanks;
             kkTbId = 'PHCD';
             kkkey  = TPprol;
             chain (TPco#: kkTbId: kkkey) pgt00;
             If %found(wtgtl);
                scConFor =  %trim(GTdesc);
             Endif;
          Else;
             scConFor  = 'Treating Physician';
          Endif;
          //***** end chg ****** 07/30/24 ****************************
          //***** end chg ****** 08/10/20 ****************************
          //***** beg add ****** 04/29/13 ****************************
          If scConEmadT = *Blanks;
             scConEmadT = noEmail;
          EndIf;
          //***** end add ****** 04/29/13 ****************************

          // Write Treating physian contact info detail box
       //******** beg dlt ****** 09/04/13 *************************
          //*** If first = *off;
          //***   first = *on;
          //***   DtlConFor   = scConFor;
          //***   DtlCnEffDt  = CvtToDate(dtMDYY(TPfrdt));
          //***   DtlCnExpDt  = CvtToDate(dtMDYY(TPtodt));
          //***   DtlConName  = scConPnam;
          //***   ##fein = TPphyf;
          //***   ##mod  = TPphym;
          //***   Chain keyWMPDL ppd00;
          //***   If %found(Wmpdl);
          //***      If PDdba1 = 'Y';
          //***         DtlConAdr1  = PDadr2;
          //***      Else;
          //***         DtlConAdr1  = PDadr1;
          //***         DtlConAdr2  = PDadr2;
          //***      Endif;
          //***      If DtlConAdr2<>*Blanks;
          //***         DtlConAdr3 = %Trim(PDcty) + ', ' + PDst + ' ' +
          //***          %EditW(PDzip:'     -    ');
          //***      Else;
          //***         DtlConAdr2 = %Trim(PDcty) + ', ' + PDst + ' ' +
          //***          %EditW(PDzip:'     -    ');
          //***         DtlConAdr3 = *Blanks;
          //***      EndIf;
          //***      If PDarea <> *zeros or PDphn# <> *zeros;
          //***         scConPhn# = '(' + %editc(PDarea:'X') + ')' +
          //***          %subst(%editc(PDphn#:'X'):1:3) + '-' +
          //***          %subst(%editc(PDphn#:'X'):4:4);
          //***      Endif;
          //***      If PDfxar <> *zeros or PDfax# <> *zeros;
          //***         DtlConFax#  = '(' + %editc(PDfxar:'X') + ')' +
          //***          %subst(%editc(PDfax#:'X'):1:3) + '-' +
          //***          %subst(%editc(PDfax#:'X'):4:4);
          //***      Endif;
          //***   Else;
          //***      DtlConAdr1  = TPphya;
          //***      DtlConAdr2 = %Trim(TPphyC) + ', ' + TPphyS + ' ' +
          //***       %EditW(TPphyZ:'     -    ');
          //***      DtlConAdr3 = *Blanks;
          //***      If TPfxar <> *zeros or TPfax# <> *zeros;
          //***         DtlConFax#  = '(' + %editc(TPfxar:'X') + ')' +
          //***          %subst(%editc(TPfax#:'X'):1:3) + '-' +
          //***          %subst(%editc(TPfax#:'X'):4:4);
          //***      Endif;
          //***   Endif;
          //***    If scConPhn# <> *blanks;
          //***       DtlConPhn#  = scConPhn#;
          //***    Endif;
          //***Endif;
       //******** end dlt ****** 09/04/13 *************************

          scConRRN = rrnWmtpl1;
          scConFile= 'WMTPL1';
          //*** Beg Add*** 09/18/24 *******************************************
          xxConTyp = 'TREATING PHYSICIAN';
          xxSeq = TPseq#;
          Exsr LoadDistType;
          //*** End Add*** 09/18/24 *******************************************

          rrnContact  = rrnContact + 1 ;
          selContact=*Off;
          //*** Beg Chg *** 03/04/24 *******************************************
          //*** If rrnContact=1;
          //***   selContact=*On;
          //*** EndIf;
          If svsflrrn1 = 0;
             If rrnContact  = 1;
                selContact = *On;
             Endif;
          Else;
             If rrnContact = svsflrrn1;
                selContact = *On;
             Endif;
          Endif;
          //*** End Chg *** 03/04/24 *******************************************
          Write sflConInfo;

       //*** Beg Chg *** 07/30/24 *************************************
       //*** Endif;
          ReadE  Key1WMtpl1  ptp01;
       Enddo;
       //*** End Chg *** 07/30/24 *************************************

       //*** Beg Del *** 03/04/24 *******************************************
       //***Chain keyWMEXL pex00;

       //***Setll KeyWMECL pec00;
       //***Reade KeyWMECL pec00;
       //***dow not %eof(Wmecl);

       //***   // Write Employer contact info
       //***   clear scConPhn#;
       //***   clear scConCell#;
       //***   clear scConFor;
       //***   If EClsn <> *blanks;
       //***      scConPNam =%trim(EClsn)+',' + %trim(ECfsn);
       //***   Else;
       //***      scConPNam =%trim(ECfsn);
       //***   Endif;
       //***   If ECmid <> *blanks;
       //***      scConPNam =%trim(scConPNam) + ',' + %trim(ECmid);
       //***   Endif;
       //***   If ECPCNT = '1';
       //***      scConFor = EXcnt1;
       //***   ElseIf ECPCNT = '2';
       //***      scConFor = EXcnt2;
       //***   ElseIf ECPCNT = '3';
       //***      scConFor = EXcnt3;
       //***   Endif;

       //***   scConEmad=ECemal;
       //***   scConEmadT=ECemal;
       //***   If scConEmadT=*Blanks;
       //***      scConEmadT=noEmail;
       //***   EndIf;

       //***   scConPst = ECposn;
       //***   If ECarea <> *zeros or ECphn# <> *zeros or ECext# <> *zeros;
       //***      scConPhn# = '(' + %editc(ECarea:'X') + ')' +
       //***       %subst(%editc(ECphn#:'X'):1:3) + '-' +
       //***       %subst(%editc(ECphn#:'X'):4:4) + 'x' +
       //***       %trim(%char(ECext#));
       //***   Endif;
       //***   If ECcara <> *zeros or ECcphn <> *zeros;
       //***      scConCell# = '(' + %editc(ECcara:'X') + ')' +
       //***       %subst(%editc(ECcphn:'X'):1:3) + '-' +
       //***       %subst(%editc(ECcphn:'X'):4:4);
       //***   Endif;

          // Write Employer contact info detail box
       //******** beg dlt ****** 09/04/13 *************************
          //***If first = *off;
          //***   first = *on;
          //***   DtlConFor   = scConFor;
          //***   DtlConPst   = scConPst;
          //***   DtlConName  = scConPnam;
          //***   DtlConAdr1  = ECadr1;
          //***   DtlConAdr2  = ECadr2;
          //***   If DtlConAdr2<>*Blanks;
          //***      DtlConAdr3 = %Trim(ECcty) + ', ' + ECst + ' ' +
          //***       %EditW(ECzip:'     -    ');
          //***   Else;
          //***      DtlConAdr2 = %Trim(ECcty) + ', ' + ECst + ' ' +
          //***       %EditW(ECzip:'     -    ');
          //***      DtlConAdr3 = *Blanks;
          //***   EndIf;
          //***   If scConPhn# <> *blanks;
          //***      DtlConPhn#  = scConPhn#;
          //***   Endif;
          //***   If ECfara <> *zeros or ECfphn <> *zeros;
          //***      DtlConFax#  = '(' + %editc(ECfara:'X') + ')' +
          //***       %subst(%editc(ECfphn:'X'):1:3) + '-' +
          //***       %subst(%editc(ECfphn:'X'):4:4);
          //***   Endif;
          //***   If scConCell# <> *blanks;
          //***      DtlConCell  = scConCell#;
          //***   Endif;
          //***Endif;
       //******** end dlt ****** 09/04/13 *************************
       //***   scConRRN = rrnWmecl;
       //***   scConFile= 'WMECL';

       //***   rrnContact  = rrnContact + 1 ;
       //***   selContact=*Off;
       //***   If rrnContact=1;
       //***      selContact=*On;
       //***   EndIf;
       //***   Write sflConInfo;
       //***Reade KeyWMECL pec00;

       //***Enddo;
       //*** End Del *** 03/04/24 *******************************************

       //*** Beg Add *** 03/04/24 *******************************************
       //Retrive the information to be shown on grid from WMSBP (Subrogation Info)

       Chain KeyWMSBL psb00;
       If %found(Wmsbl);

          // Write Responsible Party contact info
          If SBrnam <> *blanks;
             scConEmad  = SBrmad;
             scConEmadT = SBrmad;
             scConPNam  = SBrnam;
             scSystem   = 'Y';
             scToolTip  = 'To update this information go to system location';
             scConFor   = 'Responsible Party';
             dsbDlt     = *on;
             scStfCode  = ' ';
             scConPst = ' ';
             Clear scConPhn#;
             If SBrphn <> *zeros;
                scConPhn# = '(' + %subst(%editc(SBrphn:'X'):1:3) + ')' +
                %subst(%editc(SBrphn:'X'):4:3) + '-' +
                %subst(%editc(SBrphn:'X'):7:4);
             Endif;
             If scConEmadT = *Blanks;
                scConEmadT = noEmail;
             EndIf;

             scConRRN = rrnWmsbl;
             scConFile= 'WMSBLRF';
             //*** Beg Add*** 09/18/24 *******************************************
             xxConTyp = 'RESPONSIBLE PARTY';
             xxSeq = 1;
             Exsr LoadDistType;
             //*** End Add*** 09/18/24 *******************************************

             rrnContact  = rrnContact + 1 ;
             selContact = *Off;
             If svsflrrn1 = 0;
                If rrnContact  = 1;
                   selContact = *On;
                Endif;
             Else;
                If rrnContact = svsflrrn1;
                   selContact = *On;
                Endif;
             Endif;
             Write sflConInfo;
          Endif;

          // Write Insurance Company contact info
          If SBinam <> *blanks;
             scConEmad  = SBimad;
             scConEmadT = SBimad;
             scConPNam  = SBinam;
             scSystem   = 'Y';
             scToolTip  = 'To update this information go to system location';
             scConFor   = 'Insurance Company';
             dsbDlt     = *on;
             scStfCode  = ' ';
             scConPst   = ' ';
             Clear scConPhn#;
             If SBiphn <> *zeros;
                scConPhn# = '(' + %subst(%editc(SBiphn:'X'):1:3) + ')' +
                %subst(%editc(SBiphn:'X'):4:3) + '-' +
                %subst(%editc(SBiphn:'X'):7:4);
             Endif;
             If scConEmadT = *Blanks;
                scConEmadT = noEmail;
             EndIf;

             scConRRN = rrnWmsbl;
             scConFile= 'WMSBLIF';
             //*** Beg Add*** 09/18/24 *******************************************
             xxConTyp = 'INSURANCE COMPANY';
             xxSeq = 1;
             Exsr LoadDistType;
             //*** End Add*** 09/18/24 *******************************************

             rrnContact  = rrnContact + 1 ;
             selContact = *Off;
             If svsflrrn1 = 0;
                If rrnContact  = 1;
                   selContact = *On;
                Endif;
             Else;
                If rrnContact = svsflrrn1;
                   selContact = *On;
                Endif;
             Endif;
             Write sflConInfo;
          Endif;
       Endif;

       //Retrive the information to be shown on grid from WDACP (Additional Payee)

       Setll KeyWDACL pac00;
       Reade KeyWDACL pac00;
       Dow not %eof(Wdacl);

          // Write Additional Payee record
          If ACpnam <> *blanks;
             scConEmad  = ACemdc;
             scConEmadT = ACemdc;
             //*** Beg Chg *** 07/30/24 *************************************
             //*** scConPNam  = ACpnam;
             If ACfNam <> ' ';
                scConPNam =  %Trim(ACfNam) + ' ' + %Trim(ACpNam);
             Else;
                scConPNam =  %Trim(ACpNam);
             Endif;
             //*** End Chg *** 07/30/24 *************************************
             scSystem   = 'Y';
             scToolTip  = 'To update this information go to system location';
             scConFor   = 'Additional Payee';
             dsbDlt     = *on;
             scConPst = ' ';
             scStfCode  = ' ';
             Clear scConPhn#;
             If ACarea <> *zeros or ACphn# <> *zeros;
                scConPhn# = '(' + %editc(ACarea:'X') + ')' +
                 %subst(%editc(ACphn#:'X'):1:3) + '-' +
                 %subst(%editc(ACphn#:'X'):4:4);
             Endif;
             If scConEmadT = *Blanks;
                scConEmadT = noEmail;
             EndIf;

             scConRRN = rrnWdacl;
             scConFile= 'WDACL';
             //*** Beg Add*** 09/18/24 *******************************************
             xxConTyp = 'ADDITIONAL PAYEE';
             xxSeq = ACseq#;
             Exsr LoadDistType;
             //*** End Add*** 09/18/24 *******************************************

             rrnContact  = rrnContact + 1 ;
             selContact = *Off;
             If svsflrrn1 = 0;
                If rrnContact  = 1;
                   selContact = *On;
                Endif;
             Else;
                If rrnContact = svsflrrn1;
                   selContact = *On;
                Endif;
             Endif;
             Write sflConInfo;
          Endif;
          Reade KeyWDACL pac00;
       Enddo;

       // Write Internal Claims Contacts from SMUPP User Parameter File.
       i = 1;
       Dow i <= 8 ;
          If StafCode(i) <> *blanks;
             Chain StafCode(i) pup01;
             If %found(Smupl1);
                // Write Claim Contact info
                clear scConPhn#;
                clear scConCell#;
                clear scConFor;
                clear scConPNam;
                scConPNam =%trim(UPusnm);

                scConEmad = UPmail;
                scConEmadT = UPmail;
                If scConEmadT = *Blanks;
                   scConEmadT = noEmail;
                EndIf;

                scSystem   = 'Y';
                scToolTip  = 'To update this information go to system location';
                dsbDlt     = *on;
                scConPst   = ' ';
                scStfCode  = UPstaf;

                If UPphna <> *zeros or UPphn# <> *zeros;
                   scConPhn# = '(' + %editc(UPphna:'X') + ')' +
                    %subst(%editc(UPphn#:'X'):1:3) + '-' +
                    %subst(%editc(UPphn#:'X'):4:4);
                Endif;

                // Get contact type depending on internal claim contacts
                Select;
                When i = 1;
                   scConFor   = 'Account Manager';
                When i = 2;
                   scConFor   = 'Case Nurse';
                When i = 3;
                   scConFor   = 'Claim Manager';
                When i = 4;
                   scConFor   = 'Outside Adjuster';
                When i = 5;
                   scConFor   = 'Reinsurance Adjuster';
                When i = 6;
                   scConFor   = 'Special Injury/Disability Fund Adjuster';
                When i = 7;
                   scConFor   = 'Supervisor';
                When i = 8;
                   Chain (CMco#: CMfnd) pcx00;
                   If %found(Wmcxl) and CXadj <> *blanks;
                      scConFor = %trim(CXadj);
                   Else;
                      scConFor = 'Adjuster';
                   Endif;
                Endsl;

                scConRRN = rrnSmupl1;
                scConFile= 'SMUPL1';
                //*** Beg Add*** 09/18/24 *******************************************
                xxConTyp = Upper(scConFor);
                xxSeq = 1;
                Exsr LoadDistType;
                //*** End Add*** 09/18/24 *******************************************

                rrnContact  = rrnContact + 1 ;
                selContact = *Off;
                If svsflrrn1 = 0;
                   If rrnContact  = 1;
                      selContact = *On;
                   Endif;
                Else;
                   If rrnContact = svsflrrn1;
                     selContact = *On;
                   Endif;
                Endif;
                Write sflConInfo;
             Endif;
          Endif;
          i = i + 1;
       Enddo;

       //*** Beg Add *** 07/30/24 *******************************************
       // Write System Contact for Claimant demographics WMCDP
       Setll CMss# Wmcdl;
       Reade CMss# Wmcdl;
       dow not %eof(Wmcdl);

          // Write Claimant demographics
          clear scConPhn#;
          clear scConCell#;
          clear scConFor;
          clear scConPNam;
          scConPNam = FormatName(CDlsn:CDfsn:CDmid:'*FML');

          dsbDlt    = *on;
          scStfCode = ' ';

          scConEmad = CDemad;
          scConEmadT = CDemad;
          If scConEmadT = *Blanks;
             scConEmadT = noEmail;
          EndIf;

          //scConPst = CNposn;
          scSystem   = 'Y';
          scToolTip  = 'To update this information go to system location';
          If CDwph# <> *zeros or CDwara <> *zeros;
             scConPhn# = '(' + %editc(CDwara:'X') + ')' +
              %subst(%editc(CDwph#:'X'):1:3) + '-' +
              %subst(%editc(CDwph#:'X'):4:4);
          ElseIf CDphn# <> *zeros or CDarea <> *zeros;
             scConPhn# = '(' + %editc(CDarea:'X') + ')' +
              %subst(%editc(CDphn#:'X'):1:3) + '-' +
              %subst(%editc(CDphn#:'X'):4:4);
          Endif;

          If CDcphn <> *zeros or CDcara <> *zeros;
             scConCell# = '(' + %editc(CDcara:'X') + ')' +
              %subst(%editc(CDcphn:'X'):1:3) + '-' +
              %subst(%editc(CDcphn:'X'):4:4);
          Endif;

          // Get Contact for description from STCNL file
          //chain (CNco#:CNtype:ConCtxt) pcn01;
          //If %found(stcnl);
               scConFor =  'Claimant';
          //Endif;

          scConRRN = rrnWMCDL;
          scConFile= 'WMCDL';
          //*** Beg Add*** 09/18/24 *******************************************
          xxConTyp = 'CLAIMANT';
          xxSeq = 1;
          Exsr LoadDistType;
          //*** End Add*** 09/18/24 *******************************************

          rrnContact  = rrnContact + 1 ;
          selContact = *Off;
          If svsflrrn1 = 0;
             If rrnContact  = 1;
                selContact = *On;
             Endif;
          Else;
             If rrnContact = svsflrrn1;
               selContact = *On;
             Endif;
          Endif;
          Write sflConInfo;
          Reade CMss# Wmcdl;

       Enddo;
       //*** End Add *** 07/30/24 *******************************************

       // Write Non-System Contact for Claim Contacts from WMCNP file.
       // Load Non-System Contact in Claims Contacts grid.
       Setll Keywmcnl pcn00;
       Reade Keywmcnl pcn00;
       dow not %eof(Wmcnl);

          // Write Claim Contact info
          clear scConPhn#;
          clear scConCell#;
          clear scConFor;
          clear scConPNam;
          //*** Beg Add*** 09/18/24 *******************************************
          clear scConTyp;
          //*** End Add*** 09/18/24 *******************************************
          //*** Beg Chg *** 07/30/24 *************************************
          //*** If CNlsn <> *blanks;
          //***    scConPNam =%trim(CNlsn)+',' + %trim(CNfsn);
          //*** Else;
          //***    scConPNam =%trim(CNfsn);
          //*** Endif;
          //*** If CNmid <> *blanks;
          //***    scConPNam =%trim(scConPNam) + ',' + %trim(CNmid);
          //*** Endif;

          scConPNam = FormatName(CNlsn:CNfsn:CNmid:'*FML');
          //*** End Chg *** 07/30/24 *************************************

          dsbDlt    = *off;
          scStfCode = ' ';

          scConEmad = CNemal;
          scConEmadT = CNemal;
          If scConEmadT = *Blanks;
             scConEmadT = noEmail;
          EndIf;

          scConPst = CNposn;
          scSystem = 'N';
          scToolTip = *blanks;
          If CNarea <> *zeros or CNphn# <> *zeros or CNext# <> *zeros;
             scConPhn# = '(' + %editc(CNarea:'X') + ')' +
              %subst(%editc(CNphn#:'X'):1:3) + '-' +
              %subst(%editc(CNphn#:'X'):4:4) + 'x' +
              %trim(%char(CNext#));
          Endif;
          If CNcara <> *zeros or CNcphn <> *zeros;
             scConCell# = '(' + %editc(CNcara:'X') + ')' +
              %subst(%editc(CNcphn:'X'):1:3) + '-' +
              %subst(%editc(CNcphn:'X'):4:4);
          Endif;

          // Get Contact for description from STCNL file
          chain (CNco#:CNtype:ConCtxt) pcn01;
          If %found(stcnl);
             //*** Beg Chg*** 09/18/24 *******************************************
             //*** scConFor =  S_CNDesc;
             scConTyp =  S_CNDesc;
             //*** End Chg*** 09/18/24 *******************************************
          Endif;

          scConRRN = rrnWmcnl;
          scConFile= 'WMCNL';
          //*** Beg Add*** 09/18/24 *******************************************
          //*** Beg Chg*** 10/28/24 *******************************************
          //*** xxConTyp = S_CNtype;
          xxConTyp = CNtype;
          //*** End Chg*** 10/28/24 *******************************************
          xxSeq = CNseq#;
          Exsr LoadDistType;
          //*** End Add*** 09/18/24 *******************************************

          rrnContact  = rrnContact + 1 ;
          selContact = *Off;
          If svsflrrn1 = 0;
             If rrnContact  = 1;
                selContact = *On;
             Endif;
          Else;
             If rrnContact = svsflrrn1;
                selContact = *On;
             Endif;
          Endif;
          Write sflConInfo;
          Reade KeyWMCNL pcn00;

       Enddo;

       rrnRcdNbr = 1;
       scRowSel = 1;
       If svsflrrn1<> 0;
          rrnRcdNbr  = svsflrrn1;
          scRowSel   = svsflrrn1;
          svsflrrn1  = 0;
       Endif;

       //*** End Add *** 03/04/24 *******************************************
       //*** Beg Add *** 09/18/24 *******************************************
       Begsr LoadDistType;
        // Populate the Distribution Types
        Clear scDistTyp;
        Setll (CMco#: CMfnd: CMcase: xxConTyp :xxSeq) pcn0p_cr;
        Reade(n) (CMco#: CMfnd: CMcase: xxConTyp :xxSeq) pcn0p_cr;
        Dow not %eof(WMCNPCRT);
           If CR_CNcritid  = 'Distribution';
              Chain (CMco#:CR_CNcritval) STDsl;
              If %Found(stdsl);
                 If scDistTyp <> ' ';
                    scDistTyp = %Trim(scDistTyp) + ', ' + DsDesc;
                 Else;
                    scDistTyp = DsDesc;
                 Endif;
              Endif;
           Endif;
           Reade(n) (CMco#: CMfnd: CMcase: xxConTyp :xxSeq) pcn0p_cr;
        Enddo;
       Endsr;
       //*** End Add *** 09/18/24 *******************************************
      /End-Free
     P LoadContact     E

       //*** Beg Add *** 03/04/24 *******************************************
       //===================================================================
       // Load Contact Information for Policy
       //===================================================================
     p LoadPolContact  b
     d LoadPolContact  pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       dspContact = *off;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdConInfo;
       scSflClr = *off;
       scSflDsp = *on;
       dspContact = *on;

       Chain keyWMEXL pex00;

       Setll KeyWMECL pec00;
       Reade KeyWMECL pec00;
       dow not %eof(Wmecl);

          // Write Employer contact info
          clear scpConPhn#;
          clear scPConCel#;
          clear scPConFor;
          clear scPStfCode;
          //*** Beg Chg *** 07/30/24 *************************************
          //*** If EClsn <> *blanks;
          //***    scPConPNam =%trim(EClsn)+',' + %trim(ECfsn);
          //*** Else;
          //***    scPConPNam =%trim(ECfsn);
          //*** Endif;
          //*** If ECmid <> *blanks;
          //***    scPConPNam =%trim(scPConPNam) + ',' + %trim(ECmid);
          //*** Endif;
             scPConPNam = FormatName(EClsn:ECfsn:ECmid:'*FML');
          //*** End Chg *** 07/30/24 *************************************
          If ECpcnt = '1';
             scPConFor = EXcnt1;
          ElseIf ECpcnt = '2';
             scPConFor = EXcnt2;
          ElseIf ECpcnt = '3';
             scPConFor = EXcnt3;
          //*** Beg Add *** 10/28/24 *******************************************
          ElseIf ECpcnt = 'A';
             scPConFor = 'Policy Services';
          ElseIf ECpcnt = 'B';
             scPConFor = 'Audit Reviewer';
          ElseIf ECpcnt = 'C';
             scPConFor = 'Claim Contact';
          ElseIf ECpcnt = 'D';
             scPConFor = 'Underwriter';
          ElseIf ECpcnt = 'E';
             scPConFor = 'Audit Manager';
          ElseIf ECpcnt = 'F';
             scPConFor = 'Premium Accountant';
          ElseIf ECpcnt = 'G';
             Chain (CMco#: CMfnd) pex00;
             If %found(Wmexl) and EXahlc <> *blanks;
                scPConFor  = %trim(EXahlc);
             Else;
                scPConFor  = 'Loss Control Representative';
             Endif;
          //*** End Add *** 10/28/24 *******************************************
          Endif;

          //*** Beg Add *** 10/28/24 *******************************************
          // Fetching Staff Code from SMUPP for System Contacts (Staff Assignments)
          If ECpcnt = 'A' or ECpcnt = 'B' or ECpcnt = 'C' or ECpcnt = 'D'
           or ECpcnt = 'E' or ECpcnt = 'F' or ECpcnt = 'G';

             xxusnm = FormatName(EClsn:ECfsn:' ':'*FML');

             Exec sql DECLARE mycursor Cursor For
              Select UPSTAF From SMUPP
               Where UPUSNM =:xxusnm;

             Exec sql OPEN mycursor;

             Exec sql FETCH NEXT from mycursor
               INTO :scPStfCode;

             Exec sql CLOSE mycursor;

          Endif;
          //*** End Add *** 10/28/24 *******************************************
          scPConEmad = ECemal;
          scPConEmdT = ECemal;
          If scPConEmdT = *Blanks;
             scPConEmdT = noEmail;
          EndIf;

          scPConPst = ECposn;
          If ECarea <> *zeros or ECphn# <> *zeros or ECext# <> *zeros;
             scPConPhn# = '(' + %editc(ECarea:'X') + ')' +
              %subst(%editc(ECphn#:'X'):1:3) + '-' +
              %subst(%editc(ECphn#:'X'):4:4) + 'x' +
              %trim(%char(ECext#));
          Endif;
          If ECcara <> *zeros or ECcphn <> *zeros;
             scPConCel# = '(' + %editc(ECcara:'X') + ')' +
              %subst(%editc(ECcphn:'X'):1:3) + '-' +
              %subst(%editc(ECcphn:'X'):4:4);
          Endif;

          //*** Beg Add *** 10/28/24 *************************************
          scPConDiv = %editc(ECdiv:'X');
          // Get Contact for description from STCNL file
          scPConTyp = *blanks;
          If ECtype <> *blanks;
             chain (ECco#:ECtype:ConCtxtPol) pcn01;
             If %found(stcnl);
                scPConTyp = S_CNDesc;
             Endif;
          Endif;
          // Populate the Distribution Types
          scPDistTyp = ' ';
          Setll (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cl;
          Reade (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cl;
          Dow not %eof(wmeclcrt);
             If CL_ECcritid = 'Distribution';
                Chain (EcCo#:CL_ECcritVal) Stdsl;
                If %Found(Stdsl);
                   If scPDistTyp <> ' ';
                      scPDistTyp = %Trim(scPDistTyp) + ', ' + DsDesc;
                   Else;
                      scPDistTyp = DsDesc;
                   Endif;
                Endif;
             Endif;
             Reade (EcCo#:EcFnd:EcEmp#:EcDiv:EcSeq#) pec0p_cl;
          Enddo;
          //*** End Add ***** 10/28/24 *****************************

          scPolRRN = rrnWmecl;
          scPolFile= 'WMECL';

          scSflRRN += 1;
          scSflSel = *off;
          If scSflRRN = 1;
             scSflSel = *on;
          EndIf;
          Write sflPolCont;
          If ECpcnt = EXecnt and EXecnt <> *blanks;
             scPConFor = 'EDI Contact';
             scSflRRN += 1;
             scSflSel = *off;
             Write sflPolCont;
          Endif;
          Reade KeyWMECL pec00;
       Enddo;
       //*** Beg Add *** 10/28/24 *************************************
       Clear scPConDiv;
       Clear scPDistTyp;
       Clear scPConTyp;
       //*** End Add *** 10/28/24 *************************************

      /end-free
     p LoadPolContact  e

       //===================================================================
       // ProcessAddCon - Process Addition of Claims Contacts.
       //===================================================================
     P ProcessAddCon   B
     D ProcessAddCon   PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdClmCAdd;
       mode = 'A';
       isexit = *off;
       visNonSys = *on;
       dsbNonSys = *off;
       leftFax# = 630;
       SelChoices = 'CNco# = ' + %char(CMco#) + ' and '  +
                    'CNctxt = ''Claims'' and CNsys <> ''Y''';

       //*** Beg Add *** 07/30/24 *************************************
       scLblName = 'Last Name';
       //*** End Add *** 07/30/24 *************************************
       Dow isexit = *off;

          scClmCHdg = 'Add Claims Contact';

          Exfmt rcdClmCAdd;
          Select;

          // Save Button
          When BtnSaveClm = *on;
             BtnSaveClm = *off;
             If ValidateClm(mode);
                Exsr WrtClmCon;
                Exsr Wrt_Wmcnpcrt;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnCanClm = *on;
             BtnCanClm = *off;
               isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        CNco#   = CMCo#;
        CNfnd   = CMfnd;
        CNfyr   = CMfyr;
        CNcase  = CMcase;
        CNtitl  = scCourTitE;
        CNfsn   = scFnameE;
        CNmid   = scMidInitE;
        CNlsn   = scLnameE;
        CNposn  = scPostE;
        CNtype  = scConTypE;
        CNadr1  = scConAdr1E;
        CNadr2  = scConAdr2E;
        CNcty   = scConCtyE;
        CNst    = scConStE;
        CNzip   = scConZipE;
        CNext#  = scConExtE;
        If scConPhnE > *zeros;
           CNarea  = %dec(%subst(%editc(scConPhnE:'X'):1:3):3:0); // Phone  Area
           CNphn#  = %dec(%subst(%editc(scConPhnE:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scConFaxE > *zeros;
           CNfara  = %dec(%subst(%editc(scConFaxE:'X'):1:3):3:0); // Fax Area
           CNfphn  = %dec(%subst(%editc(scConFaxE:'X'):4):7:0);   // Fax Num
        EndIf;
        If scConCelE > *zeros;
           CNcara  = %dec(%subst(%editc(scConCelE:'X'):1:3):3:0); // CellPhone Area
           CNcphn  = %dec(%subst(%editc(scConCelE:'X'):4):7:0);   // CellPhone Num
        EndIf;
        CNpphf  = scPriPhnDE;
        CNemal  = scConMailE;
        CNuser  = q1user;
        CNpgm   = q1pgm;
        CNtime  = Timeto6(SystemTime());
        CNdt    = Dateto7(SystemDate());
        CNiusr  = q1user;
        CNipgm  = q1pgm;
        CNitim  = Timeto6(SystemTime());
        CNidt   = Dateto7(SystemDate());
        maxseq# = *zeros;

        Exec Sql
           Select Max(CNseq#) Into :maxseq#  From Wmcnl
           Where CNco# = :CMco# And CNfnd = :CMfnd
           and CNcase = :CMcase;
        CNseq# = maxseq# + 1;
       EndSr;

       // Writing new Claims Contact
       BegSr WrtClmCon;
        Clear pcn0p;
        Exsr MoveScDb;
        Write pcn0p;
       EndSr;

       // Set Values for CR_CNtype/CR_CNcritid/CR_CNcritval
       BegSr Wrt_Wmcnpcrt;
        Clear pcn0p_cr;

        Exsr SetDSP;
        // write Distribution Records
        strSelDSPval= scDistTypE;
        Dow strSelDSPval <> ' ';
           CR_CNcritid  = 'Distribution';
           CR_CNcritval = ParseString(strSelDSPval:',':1:'1');
           Write pcn0p_cr;
        Enddo;

        // write Sub Category Records
        strSelDSPval= scSubCatE;
        Dow strSelDSPval <> ' ';
           CR_CNcritid  = 'Sub Categories';
           CR_CNcritval = ParseString(strSelDSPval:',':1:'1');
           Write pcn0p_cr;
        Enddo;

        // write Process Output Control Records
        strSelDSPval= scPrOpCE;
        Dow strSelDSPval <> ' ';
           CR_CNcritid  = 'Process Output Control';
           CR_CNcritval = ParseString(strSelDSPval:',':1:'1');
           Write pcn0p_cr;
        Enddo;

       EndSr;

       // Writing Distribution type, Sub Categories, Process Output Control in WMCNPCRT
       BegSr SetDSP;
        CR_CNco#    =  CNco#;
        CR_CNfnd    =  CNfnd;
        CR_CNcase   =  CNcase;
        CR_CNtype   =  CNtype;
        CR_CNseq#   =  CNseq#;
        CR_CNuser   =  q1user;
        CR_CNpgm    =  q1pgm;
        CR_CNtime   =  Timeto6(SystemTime());
        CR_CNdt     =  Dateto7(SystemDate());
        CR_CNiusr   =  q1user;
        CR_CNipgm   =  q1pgm;
        CR_CNitim   =  Timeto6(SystemTime());
        CR_CNidt    =  Dateto7(SystemDate());
       EndSr;

      /end-free
     P ProcessAddCon   E

       //===================================================================
       // ProcOtherCon - Process other Options for Claims Contact.
       //===================================================================
     P ProcOtherCon    B
     D ProcOtherCon    PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdClmCAdd;
       isexit = *off;

       ReadC sflConInfo;
       Dow not %eof;

          Select;
          When scContEdt = *On and scConFile <> 'WMCNL'; // Processing for Edit
             scContEdt = *Off;
             mode = 'C';
             dsbNonSys = *on;
             visNonSys = *off;
             leftFax#  = 460;
             SelChoices = 'CNco# = ' + %char(CMco#) + ' and '  +
                          'CNctxt = ''Claims'' and CNsys = ''Y''';
             svsflrrn1 = rrnContact;                     //save rrn value
             scClmCHdg = 'Edit System Contact';
             //*** Beg Add *** 07/30/24 *************************************
             scLblName = 'Name';
             //*** End Add *** 07/30/24 *************************************
             Exsr ProcClmCon;

          When scContEdt = *On and scConFile = 'WMCNL'; // Processing for Edit
             scContEdt = *Off;
             mode = 'C';
             dsbNonSys = *off;
             visNonSys = *on;
             leftFax#  = 630;
             SelChoices = 'CNco# = ' + %char(CMco#) + ' and '  +
                          'CNctxt = ''Claims'' and CNsys <> ''Y''';
             svsflrrn1 = rrnContact;                     //save rrn value
             scClmCHdg = 'Edit Claim Contact';
             //*** Beg Add *** 07/30/24 *************************************
             scLblName = 'Last Name';
             //*** End Add *** 07/30/24 *************************************
             Exsr ProcClmCon;

          When scContDlt = *On and scConFile = 'WMCNL'; // Processing for Edit
             scContDlt = *Off;

             Chain(e) scConRRN WMCNP;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;
             mode = 'D';
             dsbNonSys = *off;
             visNonSys = *on;
             leftFax#  = 630;
             SelChoices = 'CNco# = ' + %char(CMco#) + ' and '  +
                          'CNctxt = ''Claims'' and CNsys <> ''Y''';
             svsflrrn1 = rrnContact;                     //save rrn value
             scClmCHdg = 'Delete Claim Contact';
             Exsr ProcWMCNL;
             Exsr Movedbsc;

             Dow isexit = *off;
                Write rcdClmCAdd;

                isDelresp = PopTwoBtn('Do you want to delete this Claim Contact+
                   ?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelresp = *On;
                   Exsr Delete_Wmcnpcrt;
                   Chain scConRRN pcn0p;
                   Delete pcn0p;
                   PopOneBtn('Claim Contact Deleted!':
                      'Delete':'Ok');
                   isexit = *on;
                   svsflrrn1 = *zeros;
                When isDelresp = *Off;
                   svsflrrn1 = rrnContact;      //save rrn value
                   isexit = *on;
                EndSl;
             Enddo;
          Endsl;

          //Unlock Wtcbp;

          ReadC sflConInfo;
       Enddo;

       //===================================================================
       // Subrotine to process System/Non-System contacts
       //===================================================================
       BegSr ProcClmCon;
        Select;
        When scConFile = 'WMLLLDF';
           Exsr ProcWMLLLDF;
        When scConFile = 'WMLLLPF';
           Exsr ProcWMLLLPF;
        When scConFile = 'WMTPL1';
           Exsr ProcWMTPL1;
        When scConFile = 'WMSBLRF';
           Exsr ProcWMSBLRF;
        When scConFile = 'WMSBLIF';
           Exsr ProcWMSBLIF;
        When scConFile = 'WDACL';
           Exsr ProcWDACL;
        When scConFile = 'SMUPL1';
           Exsr ProcSMUPL1;
        When scConFile = 'WMCNL';
           Exsr ProcWMCNL;
        //*** Beg Add *** 07/30/24 *************************************
        When scConFile = 'WMCDL';
           Exsr ProcWMCDL;
        //*** End Add *** 07/30/24 *************************************
        Endsl;
        //*** Beg Add*** 09/18/24 *******************************************
        If scConFile <> 'WMCNL';
           scConTypE = *blanks;
        Endif;
        //*** End Add*** 09/18/24 *******************************************
        Exsr Movedbsc;
        Dow isexit = *off;
           Exfmt rcdClmCAdd;
           Select;
           When BtnSaveClm = *on;
              BtnSaveClm = *off;
              flgRcdUpdated = '0';
              If scConFile = 'WMCNL';
                 flgRcdUpdated = RcdUpdatedErr('WMCNP':rrnWmcnp:'CNdt':'CNtime':
                  'CNuser':saveDT:saveTM:'1');
                 if flgRcdUpdated = '2'; // Error with Refresh Button
                    Exsr ProcWMCNL;
                    Exsr Movedbsc;
                 EndIf;
              EndIf;
              If flgRcdUpdated = '0'; // No error
                 If ValidateClm(mode);
                    Chain scConRRN pcn0p;
                    Exsr Delete_Wmcnpcrt;
                    Exsr MoveScdb;
                    isExit = *on;
                 Endif;
              Endif;
           When BtnCanClm = *on;
              BtnCanClm = *off;
              isExit = *on;
           EndSl;
        Enddo;
       Endsr;

       //===================================================================
       // Subrotine to process WMLLP file for Defence Attoreny
       //===================================================================
       BegSr ProcWMLLLDF;
        Chain(n) scConRRN pll0p;
        scLnameE    = LLdefn;
        scConAdr1E  = LLdad1;
        scConAdr2E  = LLdad2;
        scConCtyE   = LLdcty;
        scConStE    = LLdst;
        scConZipE   = LLdzip;
        If LLdefr <> *zeros or LLdefp <> *zeros;
           scConPhnE = LLdefr * 10000000 + LLdefp;
        Endif;
        If LLdefe <> *zeros or LLdeff <> *zeros;
           scConFaxE = LLdefe * 10000000 + LLdeff;
        Endif;
        scConMailE = LLemdd;
        scConTypE  = 'DEFENSE ATTORNEY';
        ConType    = 'DEFENSE ATTORNEY';
        svSeq#     = 1;
        ##fein     = LLdefa;
        ##mod      = LLdefm;
        Exsr loadWMPDL;
       EndSr;

       //===================================================================
       // Subrotine to process WMLLP file for Plaintiff Attorney
       //===================================================================
       Begsr ProcWMLLLPF;
        Chain(n) scConRRN pll0p;
        scLnameE    = LLplan;
        scConAdr1E  = LLpad1;
        scConAdr2E  = LLpad2;
        scConCtyE   = LLpcty;
        scConStE    = LLpst;
        scConZipE   = LLpzip;
        If LLplar <> *zeros or LLplap <> *zeros;
           scConPhnE = LLplar * 10000000 + LLplap;
        Endif;
        If LLplae <> *zeros or LLplaf <> *zeros;
           scConFaxE = LLplae * 10000000 + LLplaf;
        Endif;
        scConMailE = LLemdp;
        scConTypE  = 'PLAINTIFF ATTORNEY';
        ConType    = 'PLAINTIFF ATTORNEY';
        svSeq#     = 1;
        ##fein     = LLplaa;
        ##mod      = LLplam;
        Exsr loadWMPDL;
       EndSr;

       //===================================================================
       // Subrotine to process WMTPL1 file for Treating Physician
       //===================================================================
       Begsr ProcWMTPL1;
        Chain(n) scConRRN ptp0p;
        scLnameE    = TPphyn;
        scConAdr1E  = TPphya;
        scConCtyE   = TPphyc;
        scConStE    = TPphys;
        scConZipE   = TPphyz;
        If TPphyr <> *zeros or TPphyp <> *zeros;
           scConPhnE = TPphyr * 10000000 + TPphyp;
        Endif;
        If TPfxar <> *zeros or TPfax# <> *zeros;
           scConFaxE = TPfxar * 10000000 + TPfax#;
        Endif;
        scConMailE = TPemad;
        scConTypE  = 'TREATING PHYSICIAN';
        ConType    = 'TREATING PHYSICIAN';
        svSeq#     = TPseq#;
        ##fein     = TPphyf;
        ##mod      = TPphym;
        Exsr loadWMPDL;
       EndSr;

       //===================================================================
       // Subrotine to process WMSBL file for Responsible Party
       //===================================================================
       Begsr ProcWMSBLRF;
        Chain(n) scConRRN psb0p;
        scLnameE    = SBrnam;
        scConAdr1E  = SBrad1;
        scConAdr2E  = SBrad2;
        scConCtyE   = SBrcty;
        scConStE    = SBrst;
        scConZipE   = SBrzip;
        scConPhnE   = SBrphn;
        scConFaxE   = *zeros;
        scConMailE  = SBrmad;
        scConTypE   = 'RESPONSIBLE PARTY';
        ConType     = 'RESPONSIBLE PARTY';
        svSeq#      = 1;
       EndSr;

       //===================================================================
       // Subrotine to process WMSBL file for Insurance Company
       //===================================================================
       Begsr ProcWMSBLIF;
        Chain(n) scConRRN psb0p;
        scLnameE    = SBinam;
        scConAdr1E  = SBiad1;
        scConAdr2E  = SBiad2;
        scConCtyE   = SBicty;
        scConStE    = SBist;
        scConZipE   = SBizip;
        scConPhnE   = SBiphn;
        scConFaxE   = *zeros;
        scConMailE  = SBimad;
        scConTypE   = 'INSURANCE COMPANY';
        ConType     = 'INSURANCE COMPANY';
        svSeq#      = 1;
       EndSr;

       //===================================================================
       // Subrotine to process WDACL file for Additional Payee
       //===================================================================
       Begsr ProcWDACL;
        Chain(n) scConRRN pac0p;
        scLnameE    = ACpnam;
        scConAdr1E  = ACadr1;
        scConAdr2E  = ACadr2;
        scConCtyE   = ACcty;
        scConStE    = ACst;
        scConZipE   = ACzip;
        If ACarea <> *zeros or ACphn# <> *zeros;
           scConPhnE = ACarea * 10000000 + ACphn#;
        Endif;
        scConFaxE   = *zeros;
        scConMailE  = ACemdc;
        scConTypE   = 'ADDITIONAL PAYEE';
        ConType     = 'ADDITIONAL PAYEE';
        svSeq#      = ACseq#;
       EndSr;

       //===================================================================
       // Subrotine to process SMUPL1 file for Internal Claim Contacts
       //===================================================================
       Begsr ProcSMUPL1;
        Chain scConRRN pup0p;
        scLnameE    = UPusnm;
        scConAdr1E  = UPadr1;
        scConAdr2E  = UPadr2;
        scConCtyE   = UPcty;
        scConStE    = UPst;
        scConZipE   = UPzip;
        If UPphna <> *zeros or UPphn# <> *zeros;
           scConPhnE = UPphna * 10000000 + UPphn#;
        Endif;
        If UPfaxa <> *zeros or UPfax# <> *zeros;
           scConFaxE = UPfaxa * 10000000 + UPfax#;
        Endif;
        scConMailE  = UPmail;
        scConTypE   = Upper(scConFor);
        ConType     = scConTypE;
        If scConFor = 'Special Injury/Disability Fund Adjuster';
           scConTypE   = 'SIDF ADJUSTER';
           ConType     = scConTypE;
        Endif;
        svSeq#      = 1;
       EndSr;

       //===================================================================
       // Subrotine to process WMCNL file for Claim Contact - Non System
       //===================================================================
       Begsr ProcWMCNL;
        Chain(n) scConRRN pcn0p;
        scLnameE    = CNlsn;
        scFnameE    = CNFSN;
        scMidInitE  = CNmid;
        scCourTitE  = CNTitl;
        scPostE     = CNposn;
        scConAdr1E  = CNadr1;
        scConAdr2E  = CNadr2;
        scConCtyE   = CNcty;
        scConStE    = CNst;
        scConZipE   = CNzip;
        If CNarea <> *zeros or CNphn# <> *zeros;
           scConPhnE = CNarea * 10000000 + CNphn#;
        Endif;
        scConExtE   = CNext#;
        If CNcara <> *zeros or CNcphn <> *zeros;
           scConCelE = CNcara * 10000000 + CNcphn;
        Endif;
        If CNfara <> *zeros or CNfphn <> *zeros;
           scConFaxE = CNfara * 10000000 + CNfphn;
        Endif;
        scConMailE  = CNemal;
        scPriPhnDE  = CNpphf;
        scConTypE   = CNtype;
        saveDT      = CNdt;
        saveTM      = CNtime;
        ConType     = CNtype;
        svSeq#      = CNseq#;
        svLnameE    = CNlsn;
        svFnameE    = CNFSN;
        svConTypE   = CNtype;
       EndSr;

       //===================================================================
       // Subrotine to load data from WMPDL file
       //===================================================================
       Begsr loadWMPDL;
        Chain keyWMPDL ppd00;
        If %found(Wmpdl);
           If scLnameE = *blanks and scConPhnE = *zeros;
              If PDdba1 = 'Y';
                 scLnameE = PDadr1;
              Else;
                 scLnameE = PDname;
              Endif;
              If PDarea <> *zeros or PDphn# <> *zeros;
                 scConPhnE = PDarea * 10000000 + PDphn#;
              Endif;
           Endif;

           If scConMailE = *Blanks;
              scConMailE = PDemad;
           Endif;
           If scConFaxE = *zeros;
              If PDfxar <> *zeros or PDfax# <> *zeros;
                 scConFaxE = PDfxar * 10000000 + PDfax#;
              Endif;
           Endif;
           If scConAdr1E = *Blanks and scConAdr1E = *blanks and
              scConCtyE = *blanks and scConStE = *blanks and
              scConZipE = *zeros;
              If PDdba1 = 'Y';
                 scConAdr1E = PDadr2;
              Else;
                 scConAdr1E = PDadr1;
                 scConAdr2E = PDadr2;
              Endif;
              scConCtyE  = PDcty;
              scConStE   = PDst;
              scConZipE  = PDzip;
           Endif;
        Endif;
       EndSr;

       //*** Beg Add *** 07/30/24 *************************************
       //===================================================================
       // Subrotine to process WMCDL file for Claimant demographics
       //===================================================================
       Begsr ProcWMCDL;
        Chain(n) scConRRN pcd0p;
        scLblName   = 'Last/First/Mid Name';
        scLnameE    = %trim(CDlsn) + ',' + %trim(CDfsn) + ',' + %trim(CDmid);
        scConAdr1E  = CDadr1;
        scConAdr2E  = CDadr2;
        scConCtyE   = CDcty;
        scConStE    = CDst;
        scConZipE   = CDzip;
        If CDwph# <> *zeros or CDwara <> *zeros;
           scConPhnE = CDwara * 10000000 + CDwph#;
        ElseIf CDphn# <> *zeros or CDarea <> *zeros;
           scConPhnE = CDarea * 10000000 + CDphn#;
        Endif;
        scConFaxE   = *zeros;
        scConMailE  = CDemad;
        scConTypE   = 'CLAIMANT';
        ConType     = 'CLAIMANT';
        svSeq#      = 1;
       EndSr;
       //*** End Add *** 07/30/24 *************************************

       //===================================================================
       // Move Database file fields to screen fields
       //===================================================================
       Begsr MoveDbsc;
        Clear CR_CNcritval;
        Clear scDistTypE;
        Clear scSubCatE;
        Clear scPrOpCE;
        Setll (CMco#: CMfnd: CMcase: conType: svSeq#) pcn0p_cr;
        Reade(n) (CMco#: CMfnd: CMcase: conType:svSeq#) pcn0p_cr;
        Dow not %eof(WMCNPCRT);
           If CR_CNcritid = 'Distribution';
              If scDistTypE = ' ';
                 scDistTypE  = CR_CNcritval;
              Else;
                 scDistTypE = %trim(scDistTypE) + ',' + %trim(CR_CNcritval);
              Endif;
           Endif;
           If CR_CNcritid = 'Sub Categories';
              If scSubCatE = ' ';
                 scSubCatE  = CR_CNcritval;
              Else;
                 scSubCatE = %trim(scSubCatE) + ',' + %trim(CR_CNcritval);
              Endif;
           Endif;
           If CR_CNcritid = 'Process Output Control';
              If scPrOpCE = ' ';
                 scPrOpCE = CR_CNcritval;
              Else;
                 scPrOpCE = %trim(scPrOpCE) + ',' + %trim(CR_CNcritval);
              Endif;
           Endif;
           Reade(n) (CMco#: CMfnd: CMcase: conType:svSeq#) pcn0p_cr;
        Enddo;
       EndSr;

       //===================================================================
       // Delete Database file records
       //===================================================================
       Begsr Delete_Wmcnpcrt;
          // Clear then add Dist records in WMCNPCRT
          Setll (CMco#: CMfnd: CMcase: conType:svSeq#) pcn0p_cr;
          Reade (CMco#: CMfnd: CMcase: conType:svSeq#) pcn0p_cr;
          Dow not %eof(WMCNPCRT);
             Delete pcn0p_cr;
             Reade (CMco#: CMfnd: CMcase: conType:svSeq#) pcn0p_cr;
          Enddo;
       Endsr;

       //===================================================================
       // Move Screen fields to database file fields
       //===================================================================
       Begsr Movescdb;
        // Update record in WMCNP file
        If scConFile = 'WMCNL';
           CNtitl  = scCourTitE;
           CNfsn   = scFnameE;
           CNmid   = scMidInitE;
           CNlsn   = scLnameE;
           CNposn  = scPostE;
           CNtype  = scConTypE;
           conType = scConTypE;
           CNadr1  = scConAdr1E;
           CNadr2  = scConAdr2E;
           CNcty   = scConCtyE;
           CNst    = scConStE;
           CNzip   = scConZipE;
           CNext#  = scConExtE;
           CNarea  = %dec(%subst(%editc(scConPhnE:'X'):1:3):3:0); // Phone  Area
           CNphn#  = %dec(%subst(%editc(scConPhnE:'X'):4):7:0);   // Phone  Num
           CNfara  = %dec(%subst(%editc(scConFaxE:'X'):1:3):3:0); // Fax Area
           CNfphn  = %dec(%subst(%editc(scConFaxE:'X'):4):7:0);   // Fax Num
           CNcara  = %dec(%subst(%editc(scConCelE:'X'):1:3):3:0); // CellPhone Area
           CNcphn  = %dec(%subst(%editc(scConCelE:'X'):4):7:0);   // CellPhone Num
           CNpphf  = scPriPhnDE;
           CNemal  = scConMailE;
           CNuser  = q1user;
           CNpgm   = q1pgm;
           CNtime  = Timeto6(SystemTime());
           CNdt    = Dateto7(SystemDate());
           Update pcn0p;
        Endif;


        // write record for Dist records in WMCNPCRT
        CR_CNco#    =  CMco#;
        CR_CNfnd    =  CMfnd;
        CR_CNcase   =  CMcase;
        CR_CNtype   =  conType;
        CR_CNuser   =  q1user;
        CR_CNpgm    =  q1pgm;
        CR_CNtime   =  Timeto6(SystemTime());
        CR_CNdt     =  Dateto7(SystemDate());
        CR_CNiusr   =  q1user;
        CR_CNipgm   =  q1pgm;
        CR_CNitim   =  Timeto6(SystemTime());
        CR_CNidt    =  Dateto7(SystemDate());
        If scDistTypE <> ' ';
           strSelDistT = scDistTypE;
           CR_CNcritid = 'Distribution';
           CR_CNseq#   =  svSeq#;
           Dow strSelDistT <> ' ';
              CR_CNcritval = ParseString(strSelDistT:',':1:'1');
              write pcn0p_cr;
           Enddo;
        Endif;
        If scSubCatE <> ' ';
           strSelSubCat = scSubCatE;
           CR_CNcritid  = 'Sub Categories';
           CR_CNseq#    =  svSeq#;
           Dow strSelSubCat <> ' ';
              CR_CNcritval = ParseString(strSelSubCat:',':1:'1');
              write pcn0p_cr;
           Enddo;
        Endif;
        If scPrOpCE <> ' ';
           strSelProcOp = scPrOpCE;
           CR_CNcritid  = 'Process Output Control';
           CR_CNseq#    =  svSeq#;
           Dow strSelProcOp <> ' ';
              CR_CNcritval = ParseString(strSelProcOp:',':1:'1');
              write pcn0p_cr;
           Enddo;
        Endif;
       EndSr;

      /end-free
     P ProcOtherCon    E

       //===================================================================
       // Validate Claim Contacts
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateClm     b
     d ValidateClm     pi              n
     d strmode                        1

      /Free

        // Return *on if System Contact
        If scConFile <> 'WMCNL' and strmode = 'C';
           Return *on;
        Endif;

        isError = *off;
        erLnameE   = *off;
        msLnameE   = *blanks;
        erCourTitE = *off;
        msCourTitE = *blanks;
        erFnameE   = *off;
        msFnameE   = *blanks;
        erConMailE = *off;
        msgmailE   = *blanks;
        erPriPhnDE = *off;
        msPriPhnDE = *blanks;

        //If strmode = 'A';

        // Last Name is required entry
        If scLnameE = *blanks;
           msLnameE = 'WCR0503';
           erLnameE = *on;
           isError  = *on;
        Endif;

        // Either Title or First Name must be entered.
        If scCourTitE = *blanks and scFnameE = *blanks and erLnameE = *off;
           msCourTitE = 'WC31407';
           erCourTitE = *on;
           msFnameE   = 'WC31407';
           erFnameE   = *on;
           isError    = *on;
        Endif;

        // If Phone Number entered has 10 digit
        ##type = 'P';
        erConPhnE = Check_Phone#(scConPhnE:##type:msConPhnE);
        If erConPhnE = *on;
           isError   = *on;
        Endif;

        // If Cell Number entered has 10 digit
        ##type = 'C';
        erConCelE = Check_Phone#(scConCelE:##type:msConCelE);
        If erConPhnE = *on;
           isError   = *on;
        Endif;

        // If Fax Number entered has 10 digit
        ##type = 'F';
        erConFaxE = Check_Phone#(scConFaxE:##type:msConFaxE);
        If erConFaxE = *on;
           isError   = *on;
        Endif;

        //Claim contacts Mail should be valid
        If scConmailE <> *blanks;
           erConmailE = Check_Email(scConmailE:msgmailE);
           If erConmailE  = *on;
              msgmailE = 'Please Enter Valid Email.';
              isError = *on;
           Endif;
        Endif;

        // If Phone Number, Cell Number and Primary Contact entered THEN
        // Primary Phone Flag must be entered.
        If scPriPhnDE = ' ' and (scConPhnE > 0 and
         %rem(scConPhnE:10000000) <> *zeros) and (scConCelE > 0 and
         %rem(scConCelE:10000000) <> *zeros);
           msPriPhnDE = 'WC31410';
           erPriPhnDE = *on;
           isError = *on;
        Endif;

        // Validate if duplicate records exist
        If isError = *off and ( strmode = 'A' or
           scLnameE <> svLnameE or scFnameE <> svFnameE or
           scConTypE <> svConTypE);
           Chain (CMco#:CMfnd:CMfyr:CMcase:scLnameE:scFnameE:
                     scConTypE) pcn00;
           If %found(Wmcnl);
              msLNameE  = 'WC30661';
              erLNameE  = *on;
              msFNameE  = 'WC30661';
              erFNameE  = *on;
              msConTypE = 'WC30661';
              erConTypE = *on;
              isError   = *on;
           Endif;
        Endif;


       // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateClm     e

       //===================================================================
       // Load agency contact information
       //===================================================================
     P LoadAContact    B
     D LoadAContact    PI

     disEofWMAHP       s              1n   inz
      /free

       //*** Beg Add *** 07/30/24 *************************************
       For indx = 1 to #ofProd;
          For indxDiv = 1 to #ofDiv;
             aryDiv(indx).div(indxDiv) = *loval;
          Endfor;
       Endfor;
       //*** End Add *** 07/30/24 *************************************
       ##co  = CMco#;
       ##fnd = CMfnd;
       ##emp = CMemp#;
       kkfyr = CMfyr;
       Clear aryAgtProd;
       Clear agt;
       indx = 1;
       strSql = 'with ' + SqlWithPolicyInfo +
        'select AHco#, AHfnd, AHemp#, AHdiv, AHfyr, AHefDt, ' +
        ' AHagt#, AHprod, AHsAgt, AHsPrd, AH3agt, AH3prd ' +
        ' from policy_info' +
        ' where ELco#=' + %char(##co) + ' and ELfnd=' + %char(##fnd) +
        '  and ELemp#=' + %char(##emp) + ' and Ahfyr=' + %char(kkFyr) +
        ' order by AHco#, AHfnd, AHemp#, AHdiv';
       Exec sql close cursorWMAHP;
       Exec sql prepare sqlWMAHP From :strSql;
       Exec sql declare cursorWMAHP cursor for sqlWMAHP;
       Exec sql open cursorWMAHP;
       Reset isEofWMAHP;
       Dou isEofWMAHP;
          Exec sql fetch next from cursorWMAHP into :dsWMAHP;
          Select;
          When not IsValidFetch(sqlstt);
             isEofWMAHP = *on;
             Iter;
          Endsl;
          // Primary Agent
          If dsWMAHP.agt#1 > 0;
             LoadAgent(dsWMAHP.agt#1:'1':dsWMAHP.prod1:dsWMAHP.efDt:
              dsWMAHP.div);
          Endif;
          // Secondary Agent
          If dsWMAHP.agt#2 > 0;
             LoadAgent(dsWMAHP.agt#2:'2':dsWMAHP.prod2:dsWMAHP.efDt:
              dsWMAHP.div);
          Endif;
          // Tertiary Agent
          If dsWMAHP.agt#3 > 0;
             LoadAgent(dsWMAHP.agt#3:'3':dsWMAHP.prod3:dsWMAHP.efDt:
              dsWMAHP.div);
          Endif;
       Enddo;
       Clear aryAgtProd;
       Clear agt;
       Exec sql close cursorWMAHP;
       Exec sql open cursorWMAHP;
       Reset isEofWMAHP;
       Dou isEofWMAHP;
           Exec sql fetch next from cursorWMAHP into :dsWMAHP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofWMAHP = *on;
              Iter;
           Endsl;
           // Primary producer
           If dsWMAHP.prod1 > 0;
              LoadProd(dsWMAHP.prod1:'1':dsWMAHP.agt#1:dsWMAHP.efDt
               :dsWMAHP.div);
           Endif;
           If dsWMAHP.prod2 > 0;
              LoadProd(dsWMAHP.prod2:'2':dsWMAHP.agt#2:dsWMAHP.efDt
               :dsWMAHP.div);
           Endif;
           If dsWMAHP.prod3 > 0;
              LoadProd(dsWMAHP.prod3:'3':dsWMAHP.agt#3:dsWMAHP.efDt
               :dsWMAHP.div);
           Endif;
       Enddo;

      /end-free
     P LoadAContact    E

       //===================================================================
       // LoadAgent - Load Agent contact records
      //===================================================================
     P LoadAgent       B
     d LoadAgent       PI
     d  ##agt#                        5  0 const
     d  ##rank                        1    const
     d  ##prod                        5  0 const
     d  ##efDt                        7  0 const
     d  ##div                         5  0 const

     ddecCount         s             10i 0 inz
     disEofWMA5P       s              1n   inz
     drrnWMA5P         s              9  0 inz
     ddsWMA5P        e ds                  extname('WMA5P')
     ddsWMAAP        e ds                  extname('WMAAP')

      /free
       Exec sql close cursorWMA5P;
       Exec sql declare cursorWMA5P cursor for
        select a5.*, WMAAP.*, rrn(a5)
         from WMA5P a5
         join WMAAP on
          a5.A5co#=AAco# and a5.A5fein=AAfein and a5.A5mod=AAmod
         join WMA5PCRT crt on
          a5.A5co#=crt.A5co# and a5.A5fein=crt.A5fein
           and a5.A5mod=crt.A5mod and a5.A5seq#=crt.A5seq#
         where AAco#=:##co and AAfnd=:##fnd and AAagt#=:##Agt#
          and crt.A5critId='Producer' and crt.A5critVal=:##prod
          and a5.A5stat <> 'I';
       Exec sql open cursorWMA5P;
       Dou isEofWMA5P;
          Exec sql fetch next from cursorWMA5P into :dsWMA5P,:dsWMAAP,:rrnWMA5P;
          Select;
          When not IsValidFetch(sqlstt);
             isEofWMA5P = *on;
             Iter;
          Endsl;
          dsAgtProd.group = AHfnd;
          dsAgtProd.agt# = ##agt#;
          dsAgtProd.seq# = A5seq#;
          found = %lookup(dsAgtProd:aryAgtProd(*).key);
          Select;
          When found > 0;
             Dou found = 0;
                indxDiv = %lookup(##div:aryDiv(found).div);
                  indxDiv = 0;
                Select;
                When indxDiv = 0;
                   indxDiv = %lookup(*loval:aryDiv(found).div);
                   aryDiv(found).div(indxDiv) = ##div;
                Endsl;
                found = %lookup(dsAgtProd:aryAgtProd(*).key:found+1);
             Enddo;
          //When found = 0 and MeetAgnFlt();
          When found = 0;
             ClrPolCont();
             // Write Employer contact info
             // name
             //*** Beg Chg *** 07/30/24 *************************************
             //*** If A5lsn <> *blanks;
             //***    scPConPNam =%trim(A5lsn)+',' + %trim(A5fsn);
             //*** Else;
             //***    scPConPNam =%trim(A5fsn);
             //*** Endif;
             //*** If A5mid <> *blanks;
             //***    scPConPNam =%trim(scPConPNam) + ',' + %trim(A5mid);
             //*** Endif;
             scPConPNam = FormatName(A5lsn:A5fsn:A5mid:'*FML');
             //*** End Chg *** 07/30/24 *************************************

             // Email
             scPConEmad = A5emal;
             scPConEmdT = A5emal;
             If scPConEmdT = *blanks;
                scPConEmdT = 'No Email available';
             Endif;

             // Type
             scPConFor = 'Agent Contact';
             scPConPst = 'Agent';
             scPStfCode = *blanks;
             scPolFile= 'WMA5P';
             //*** Beg Add *** 10/28/24 ************************************
             // Load Dist types from WMA5PCRT - Agent Contacts File - Criteria
             Setll (A5Co#:A5fein:A5mod:A5seq#) pac0p_cr;
             Reade (A5Co#:A5fein:A5mod:A5seq#) pac0p_cr;
             Dow not %eof(wma5pcrt);
                If CR_A5critid = 'Distribution';
                   Chain (A5co#:CR_A5critVal) Stdsl;
                   If %Found(Stdsl);
                      If scPDistTyp <> ' ';
                         scPDistTyp = %Trim(scPDistTyp) + ', ' + DsDesc;
                      Else;
                         scPDistTyp = DsDesc;
                      Endif;
                   Endif;
                Endif;
                Reade (A5Co#:A5fein:A5mod:A5seq#) pac0p_cr;
             Enddo;
             //*** End Add *** 10/28/24 ************************************

             // Phone#
             If A5area<>*zeros or A5phn#<>*zeros or A5ext#<>*zeros;
                scPConPhn# = '(' + %editc(A5area:'X') + ')' +
                 %subst(%editc(A5phn#:'X'):1:3) + '-' +
                 %subst(%editc(A5phn#:'X'):4:4) + 'x' +
                 %trim(%editc(A5ext#:'X'));
             Endif;

             // Cell phone#
             If A5cara <> *zeros or A5cphn <> *zeros;
                scPConCel# = '(' + %editc(A5cara:'X') + ')' +
                 %subst(%editc(A5cphn:'X'):1:3) + '-' +
                 %subst(%editc(A5cphn:'X'):4:4);
             Endif;

             //*** Beg Chg *** 09/18/24 *************************************
             //*** indx = scSflRRN + 1;
             SetIndx += 1;
             indx = SetIndx;
             //*** End Chg *** 09/18/24 *************************************
             aryAgtProd(indx) = dsAgtProd;
             aryDiv(indx).div(1) = ##div;

             scPolRRN = rrnWMA5P;

             scSflRRN = scSflRRN + 1;
             If scSflRRN = 1;
                scSflSel = *on;
             Else;
                Clear scSflSel;
             Endif;
             Write sflPolCont;
             //*** Beg Add *** 07/30/24 *************************************
             inCo#  = A5co#;
             inFein = A5fein;
             inMod  = A5Mod;
             LoadWmagp(inCo#:inFein:inMod);
             //*** End Add *** 07/30/24 *************************************
          Endsl;
       Enddo;
       Return;

      /end-free
     P LoadAgent       E


       //===================================================================
       // LoadProducer - Load Agent contact records
      //===================================================================
     P LoadProd        B
     d LoadProd        PI
     d  ##Prod                        5  0 const
     d  ##rank                        1    const
     d  ##agt#                        5  0 const
     d  ##efDt                        7  0 const
     d  ##div                         5  0 const
      /Free
       intDiv = AHdiv;
       Chain (##co:##fnd:##agt#) paa01;
       If %found(wmaal1);
          kkfein = AAfein;
          kkmod1 = AAmod;
          kkprod = ##Prod;
          Chain keyWMAPl pap01;
          If %found(wmapl);
             dsAgtProd.group = AAfnd;
             dsAgtProd.agt# = AAagt#;
             dsAgtProd.prod = APprod;
             found = %lookup(dsAgtProd:aryAgtProd(*).key);
             Select;
             When found > 0;
                indxDiv = %lookup(##div:aryDiv(found).div);
                Select;
                When indxDiv = 0;
                   indxDiv = %lookup(*loval:aryDiv(found).div);
                   aryDiv(found).div(indxDiv) = ##div;
                Endsl;
             When found = 0;
                If APstat <> 'I';
                   meetAgn = *on;
                   If meetAgn = *on;
                      ClrPolCont();
                      // Write Employer contact info
                      // name
                      //*** Beg Chg *** 07/30/24 *************************************
                      //*** scPConPNam = FormatName(APlsn:APfsn);
                      scPConPNam = FormatName(APlsn:APfsn:' ':'*FML');
                      //*** End Chg *** 07/30/24 *************************************

                      // Email
                      scPConEmad = APEmal;
                      scPConEmdT = APEmal;
                      If scPConEmdT = *blanks;
                         scPConEmdT = 'No Email available';
                      Endif;

                      // Type
                      scPConPst = 'Producer';
                      scPConFor = 'Agent Contact';
                      scPStfCode = *blanks;
                      scPolFile= 'WMAPL';
                      //*** Beg Add *** 10/28/24 ********************************
                      // Load Dist types from WMAPPCRT - Agent Producer File - Cr
                      Setll (APCo#:APfein:APmod:APprod:APseq#) pap0p_cr;
                      Reade (APCo#:APfein:APmod:APprod:APseq#) pap0p_cr;
                      Dow not %eof(wmappcrt);
                         If CR_APcritid = 'Distribution';
                            Chain (AAco#:CR_APcritVal) Stdsl;
                            If %Found(Stdsl);
                               If scPDistTyp <> ' ';
                                  scPDistTyp= %Trim(scPDistTyp) + ', ' + DsDesc;
                               Else;
                                  scPDistTyp= DsDesc;
                               Endif;
                            Endif;
                         Endif;
                         Reade (APCo#:APfein:APmod:APprod:APseq#) pap0p_cr;
                      Enddo;
                      //*** End Add *** 10/28/24 ********************************

                      // Phone#
                      Chain (##co:kkfein:kkmod) pag00;
                      If %Found(wmagl);
                         If AGphn1<>*Zeros;
                            scPConPhn# = '(' + %EditC(AGare1:'X') + ') ' +
                             %Subst(%EditC(AGphn1:'X'):1:3) + '-' +
                             %Subst(%EditC(AGphn1:'X'):4:4);
                         EndIf;
                      EndIf;

                      // Cell phone#
                      scPConCel# = *blanks;

                      //*** Beg Chg *** 09/18/24 *************************************
                      //*** indx = scSflRRN + 1;
                      SetIndx += 1;
                      indx = SetIndx;
                      //*** End Chg *** 09/18/24 *************************************
                      aryAgtProd(indx) = dsAgtProd;
                      aryDiv(indx).div(1) = ##div;
                      indx = indx + 1;

                      scPolRRN = rrnwmapl;

                      scSflRRN = scSflRRN + 1;
                      If scSflRRN = 1;
                         scSflSel = *on;
                      Else;
                         clear scSflSel;
                      Endif;
                      Write sflPolCont;
                      //*** Beg Add *** 07/30/24 ******************************
                      inCo#  = APco#;
                      inFein = APfein;
                      inMod  = APMod;
                      LoadWmagp(inCo#:inFein:inMod);
                      //*** End Add *** 07/30/24 ******************************
                   Endif;
                Endif;
             Endsl;
          Endif;
       Endif;
      /end-free
     P LoadProd        E

       //==========================================================================
       // LoadUsrParm- Load Internal Policy Contacts from SMUPP User Parameter file
       //==========================================================================
     P LoadUsrParm     B
     d LoadUsrParm     PI
      /free

       // Write Internal Policy Contacts from SMUPP User Paramter File.
       i = 1;
       Dow i <= 7 ;
          If StafCodeP(i) <> *blanks;
             Chain StafCodeP(i) pup01;
             If %found(Smupl1);
                // Write Claim Contact info
                ClrPolCont();
                scPConPNam =%trim(UPusnm);

                scPConEmad = UPmail;
                scPConEmdT = UPmail;
                If scPConEmdT = *Blanks;
                   scPConEmdT = noEmail;
                EndIf;

                scPConPst   = ' ';
                scPStfCode  = UPstaf;

                If UPphna <> *zeros or UPphn# <> *zeros;
                   scPConPhn# = '(' + %editc(UPphna:'X') + ')' +
                    %subst(%editc(UPphn#:'X'):1:3) + '-' +
                    %subst(%editc(UPphn#:'X'):4:4);
                Endif;

                // Get contact type depending on internal policy contacts
                Select;
                When i = 1;
                   scPConFor  = 'Policy Services';
                When i = 2;
                   scPConFor  = 'Audit Reviewer';
                When i = 3;
                   scPConFor  = 'Claim Contact';
                When i = 4;
                   scPConFor  = 'Underwriter';
                When i = 5;
                   scPConFor  = 'Audit Manager';
                When i = 6;
                   scPConFor  = 'Premium Accountant';
                When i = 7;
                   Chain (CMco#: CMfnd) pex00;
                   If %found(Wmexl) and EXahlc <> *blanks;
                      scPConFor  = %trim(EXahlc);
                   Else;
                      scPConFor  = 'Loss Control Representative';
                   Endif;
                Endsl;

                scPolFile= 'SMUPL1';
                scPolRRN = rrnSmupl1;

                scSflRRN = scSflRRN + 1;
                If scSflRRN = 1;
                   scSflSel = *on;
                Else;
                   Clear scSflSel;
                Endif;

                Write sflPolCont;
             Endif;
          Endif;
          i = i + 1;
       Enddo;
       scSflRRN = 1;

      /end-free
     P LoadUsrParm     E

       //===================================================================
       // ClrPolCont - Clear all Policy contact fields
      //===================================================================
     P ClrPolCont      B
     d ClrPolCont      PI
      /free

       clear scPConEmad;
       clear scPConEmdT;
       clear scPConPNam;
       clear scPConFor;
       clear scPConPst;
       clear scPStfCode;
       clear scPConPhn#;
       clear scPConCel#;
       clear scPolRRN;
       clear scPolFile;
       //*** Beg Add *** 10/28/24 *******************************************
       clear scPConDiv;
       clear scPDistTyp;
       clear scPConTyp;
       //*** End Add *** 10/28/24 *******************************************

      /end-free
     P ClrPolCont      E
       //*** End Add *** 03/04/24 *******************************************
       //*** Beg Add *** 07/30/24 *******************************************
       //==========================================================================
       // LoadWmagp - Load Internal Agency Contacts from SMUPP User Parameter file
       //==========================================================================
     P LoadWmagp       B
     d LoadWmagp       PI
     d inCo#                          3  0
     d inFein                         9  0
     d inMod                          3  0
      /free

       // Write Internal Policy Contacts from SMUPP User Paramter File.
       chain (inCo#:inFein:inMod) wmagl;
       If %found(wmagl);
          i = 1;
          Dow i <=2;
             If StafCodeAgt(i) <> *blanks;
                Chain StafCodeAgt(i) pup01;
                If %found(Smupl1);
                   // Write Claim Contact info
                   ClrPolCont();
                   scPConPNam =%trim(UPusnm);

                   scPConEmad = UPmail;
                   scPConEmdT = UPmail;
                   If scPConEmdT = *Blanks;
                      scPConEmdT = noEmail;
                   EndIf;

                   scPConPst   = ' ';
                   scPStfCode  = UPstaf;

                   If UPphna <> *zeros or UPphn# <> *zeros;
                      scPConPhn# = '(' + %editc(UPphna:'X') + ')' +
                       %subst(%editc(UPphn#:'X'):1:3) + '-' +
                       %subst(%editc(UPphn#:'X'):4:4);
                   Endif;

                   // Get contact type depending on internal policy contacts
                   Select;
                   When i = 1;
                      scPConFor  = 'Agency Services Representative';
                   When i = 2;
                      scPConFor  = 'Agency Marketing Representative';
                   Endsl;

                   scPolFile= 'SMUPL1';
                   scPolRRN = rrnSmupl1;

                   scSflRRN = scSflRRN + 1;
                   If scSflRRN = 1;
                      scSflSel = *on;
                   Else;
                      Clear scSflSel;
                   Endif;

                   Write sflPolCont;
                Endif;
             Endif;
             i = i + 1;
          Enddo;
       Endif;

      /end-free
     P LoadWmagp       E

       //*** End Add *** 07/30/24 *******************************************
       //*** Beg Add *** 10/28/24 *******************************************
       //===================================================================
       // StaffAssign - Assigning Staff to WMECP
      //===================================================================
     P StaffAssign     B
     d StaffAssign     PI

       // We are updating only single division and not all div like WTPOLCONT program.
       Setll (CMco#:CMfnd:CMemp#:CMdiv) pem00;
       Reade (CMco#:CMfnd:CMemp#:CMdiv) pem00;
       If not %EOF(WMEML);
          //*** Writing/Updating/Deleting record from WMECP
          // - POLICY SERVICES
          If EMpsrv <> *blanks;
             //* fetching user name
             Chain EMpsrv pup01;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                   xxfsn = %trim(UPusnm);
                   xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'A') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'A';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             Else;

                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECuser = q1user;
                ECpgm  = q1pgm;
                ECdt   = Dateto7(SystemDate());
                ECtime = Timeto6(SystemTime());

                Update pec01;

             Endif;
          Endif;

          If EMpsrv = *blanks;
             Chain (EMco#:EMfnd:EMemp#:EMdiv:'A') pec01;
             If %found(WMECL1);
                Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Dow not %eof(wmecpcrt);
                   Delete wmecpcrt;
                   Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Enddo;
                Unlock wmecpcrt;

                Delete pec01;
             Endif;
          Endif;

          // - AUDIT REVIEWER
          If EMarvw <> *blanks;
             //* fetching user name
             Chain EMarvw pup01;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                   xxfsn = %trim(UPusnm);
                   xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'B') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'B';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             Else;

                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECuser = q1user;
                ECpgm  = q1pgm;
                ECdt   = Dateto7(SystemDate());
                ECtime = Timeto6(SystemTime());

                Update pec01;

             Endif;
          Endif;

          If EMarvw = *blanks;
             Chain (EMco#:EMfnd:EMemp#:EMdiv:'B') pec01;
             If %found(WMECL1);
                Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Dow not %eof(wmecpcrt);
                   Delete wmecpcrt;
                   Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Enddo;
                Unlock wmecpcrt;

                Delete pec01;
             Endif;
          Endif;

          // - CLAIM CONTACT
          If EMclmc <> *blanks;
             //* fetching user name
             Chain EMclmc pup01;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                   xxfsn = %trim(UPusnm);
                   xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'C') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'C';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             Else;

                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECuser = q1user;
                ECpgm  = q1pgm;
                ECdt   = Dateto7(SystemDate());
                ECtime = Timeto6(SystemTime());

                Update pec01;

             Endif;
          Endif;

          If EMclmc = *blanks;
             Chain (EMco#:EMfnd:EMemp#:EMdiv:'C') pec01;
             If %found(WMECL1);
                Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Dow not %eof(wmecpcrt);
                   Delete wmecpcrt;
                   Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Enddo;
                Unlock wmecpcrt;

                Delete pec01;
             Endif;
          Endif;

          // - UNDERWRITER
          If EMuwtr <> *blanks;
             //* fetching user name
             Chain EMuwtr pup01;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                   xxfsn = %trim(UPusnm);
                   xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'D') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'D';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             Else;

                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECuser = q1user;
                ECpgm  = q1pgm;
                ECdt   = Dateto7(SystemDate());
                ECtime = Timeto6(SystemTime());

                Update pec01;

             Endif;
          Endif;

          If EMuwtr = *blanks;
             Chain (EMco#:EMfnd:EMemp#:EMdiv:'D') pec01;
             If %found(WMECL1);
                Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Dow not %eof(wmecpcrt);
                   Delete wmecpcrt;
                   Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Enddo;
                Unlock wmecpcrt;

                Delete pec01;
             Endif;
          Endif;

          // - AUDIT MANAGER
          If EMamgr <> *blanks;
             //* fetching user name
             Chain EMamgr pup01;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                   xxfsn = %trim(UPusnm);
                   xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'E') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'E';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             Else;

                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECuser = q1user;
                ECpgm  = q1pgm;
                ECdt   = Dateto7(SystemDate());
                ECtime = Timeto6(SystemTime());

                Update pec01;

             Endif;
          Endif;

          If EMamgr = *blanks;
             Chain (EMco#:EMfnd:EMemp#:EMdiv:'E') pec01;
             If %found(WMECL1);
                Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Dow not %eof(wmecpcrt);
                   Delete wmecpcrt;
                   Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Enddo;
                Unlock wmecpcrt;

                Delete pec01;
             Endif;
          Endif;

          // - PREMIUM ACCOUNTANT
          If EMpacc <> *blanks;
             //* fetching user name
             Chain EMpacc pup01;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                   xxfsn = %trim(UPusnm);
                   xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'F') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'F';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             Else;

                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECuser = q1user;
                ECpgm  = q1pgm;
                ECdt   = Dateto7(SystemDate());
                ECtime = Timeto6(SystemTime());

                Update pec01;

             Endif;
          Endif;

          If EMpacc = *blanks;
             Chain (EMco#:EMfnd:EMemp#:EMdiv:'F') pec01;
             If %found(WMECL1);
                Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Dow not %eof(wmecpcrt);
                   Delete wmecpcrt;
                   Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Enddo;
                Unlock wmecpcrt;

                Delete pec01;
             Endif;
          Endif;

          // - LOSS CONTROL REPRESENTATIVE
          If EMlrep <> *blanks;
             //* fetching user name
             Chain EMlrep pup01;
             If %found(SMUPL1);
                xxlen = %len(%trim(UPusnm));
                If xxlen > 15;
                   spacePos = %scan(' ': UPusnm);
                   If spacePos > 0;
                      xxfsn = %subst(UPusnm: 1: spacePos - 1);
                      xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                   Endif;
                Else;
                   xxfsn = %trim(UPusnm);
                   xxlsn = '';
                Endif;
             Endif;

             Chain (EMco#:EMfnd:EMemp#:EMdiv:'G') pec01;

             If not %found(WMECL1);
                ECco# = EMco#;
                ECfnd = EMfnd;
                Ecemp# = EMemp#;
                ECdiv = EMdiv;
                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECpcnt = 'G';
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECdivspec = 'Y';
                ECiusr = q1user;
                ECipgm = q1pgm;
                ECidt  = Dateto7(SystemDate());
                ECitim = Timeto6(SystemTime());
                Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
                 Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
                ECseq# = maxseq# + 1;

                Write pec01;
             Else;

                ECfsn = xxfsn;
                EClsn = xxlsn;
                ECADR1 = UPADR1;
                ECADR2 = UPADR2;
                ECCTY  = UPCTY;
                ECST   = UPST;
                ECZIP  = UPZIP;
                ECEMAL = UPMAIL;
                ECFARA = UPFAXA;
                ECFPHN = UPFAX#;
                ECAREA = UPPHNA;
                ECPHN# = UPPHN#;
                ECuser = q1user;
                ECpgm  = q1pgm;
                ECdt   = Dateto7(SystemDate());
                ECtime = Timeto6(SystemTime());

                Update pec01;

             Endif;
          Endif;

          If EMlrep = *blanks;
             Chain (EMco#:EMfnd:EMemp#:EMdiv:'G') pec01;
             If %found(WMECL1);
                Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Dow not %eof(wmecpcrt);
                   Delete wmecpcrt;
                   Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
                Enddo;
                Unlock wmecpcrt;

                Delete pec01;
             Endif;
          Endif;
          Reade (CMco#:CMfnd:CMemp#:CMdiv) pem00;
       Endif;

     P StaffAssign     E
       //*** End Add *** 10/28/24 *******************************************

       //===================================================================
       // LoadDefAtt - Load Defense Attorney Contact info detail box
       //===================================================================
       //******** Beg dlt ****** 09/04/13 *************************
     P*//*** LoadDefAtt      B
     D*//*** LoadDefAtt      PI
      *//*** /Free
       //***
       //***   ClearScreen();
       //***
       //***   Chain scConRRN pll0p;
       //***   If %found(Wmllp);
       //***
       //***     // Write Defense attorney contact details
       //***      DtlConFor = 'Defense Attorney';
       //***      DtlCnAsnDt = CvtToDate(dtMDYY(LLdedt));
       //***
       //***      ##fein = LLdefa;
       //***      ##mod  = LLdefm;
       //***** beg add ****** 08/13/13 *************************
       //*** Clear DtlConPhn#;
       //*** If LLdefr <> *zeros or LLdefp <> *zeros;
       //***  DtlConPhn# = '(' + %editc(LLdefr:'X') + ')' +
       //***  %subst(%editc(LLdefp:'X'):1:3) + '-' +
       //***  %subst(%editc(LLdefp:'X'):4:4);
       //*** Endif;
       //***** end add ****** 08/13/13 *************************
       //***     Chain keyWMPDL ppd00;
       //***     If %found(Wmpdl);
       //***        If PDdba1 = 'Y';
       //***           DtlConName = PDadr1;
       //***           DtlConAdr1 = PDadr2;
       //***        Else;
       //***           DtlConName = PDname;
       //***           DtlConAdr1 = PDadr1;
       //***           DtlConAdr2 = PDadr2;
       //***        Endif;
       //***        If DtlConAdr2<>*Blanks;
       //***           DtlConAdr3 = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***           %EditW(PDzip:'     -    ');
       //***        Else;
       //***           DtlConAdr2 = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***           %EditW(PDzip:'     -    ');
       //***           DtlConAdr3 = *Blanks;
       //***        EndIf;
       //***** beg add ****** 08/13/13 *************************
       //***       If DtlConPhn# = *Blanks;
       //***** end add ****** 08/13/13 *************************
       //***          If PDarea <> *zeros or PDphn# <> *zeros;
       //***             DtlConPhn# = '(' + %editc(PDarea:'X') + ')' +
       //***             %subst(%editc(PDphn#:'X'):1:3) + '-' +
       //***             %subst(%editc(PDphn#:'X'):4:4);
       //***          Endif;
       //***** beg add ****** 08/13/13 *************************
       //***       Endif;
       //***** end add ****** 08/13/13 *************************
       //***        If PDfxar <> *zeros or PDfax# <> *zeros;
       //***           DtlConFax#  = '(' + %editc(PDfxar:'X') + ')' +
       //***            %subst(%editc(PDfax#:'X'):1:3) + '-' +
       //***            %subst(%editc(PDfax#:'X'):4:4);
       //***        Endif;
       //***     Else;
       //***        DtlConName = LLdefn;
       //***        DtlConAdr1  = LLdad1;
       //***        DtlConAdr2  = LLdad2;
       //***        If DtlConAdr2<>*Blanks;
       //***           DtlConAdr3 = %Trim(LLdCty) + ', ' + LLdSt + ' ' +
       //***            %EditW(LLdZip:'     -    ');
       //***        Else;
       //***           DtlConAdr2 = %Trim(LLdCty) + ', ' + LLdSt + ' ' +
       //***            %EditW(LLdZip:'     -    ');
       //***           DtlConAdr3 = *Blanks;
       //***        EndIf;
       //***         If LLdefr <> *zeros or LLdefp <> *zeros;
       //***            DtlConPhn# = '(' + %editc(LLdefr:'X') + ')' +
       //***            %subst(%editc(LLdefp:'X'):1:3) + '-' +
       //***            %subst(%editc(LLdefp:'X'):4:4);
       //***         Endif;
       //***        If LLdefe <> *zeros or LLdeff <> *zeros;
       //***           DtlConFax#  = '(' + %editc(LLdefe:'X') + ')' +
       //***            %subst(%editc(LLdeff:'X'):1:3) + '-' +
       //***            %subst(%editc(LLdeff:'X'):4:4);
       //***        Endif;
       //***     Endif;
       //***  Endif;
       //***
      *//*** /End-Free
     P*//***  LoadDefAtt      E
      *//******** End dlt ****** 09/04/13 *************************
       //===================================================================
       // LoadPlnAtt - Load Plaintiff Attorney Contact info detail box
       //===================================================================
      *//******** Beg dlt ****** 09/04/13 *************************
     P*//*** LoadPlnAtt      B
     D*//*** LoadPlnAtt      PI
      *//*** /Free
       //***
       //***  ClearScreen();
       //***
       //***  Chain scConRRN pll0p;
       //***  If %found(Wmllp);
       //***
       //***     // Write Plaintiff attorney contact details
       //***     DtlConFor  = 'Plaintiff Attorney';
       //***     DtlCnRprDt = CvtToDate(dtMDYY(LLdtap));
       //***     ##fein = LLplaa;
       //***     ##mod  = LLplam;
       //***** beg add ****** 08/13/13 *************************
       //***      If LLplar <> *zeros or LLplap <> *zeros;
       //***         DtlConPhn# = '(' + %editc(LLplar:'X') + ')' +
       //***         %subst(%editc(LLplap:'X'):1:3) + '-' +
       //***         %subst(%editc(LLplap:'X'):4:4);
       //***      Endif;
       //***** end add ****** 08/13/13 *************************
       //***     Chain keyWMPDL ppd00;
       //***     If %found(Wmpdl);
       //***        If PDdba1 = 'Y';
       //***           DtlConName = PDadr1;
       //***           DtlConAdr1 = PDadr2;
       //***        Else;
       //***           DtlConName = PDname;
       //***           DtlConAdr1 = PDadr1;
       //***           DtlConAdr2 = PDadr2;
       //***        Endif;
       //***        If DtlConAdr2<>*Blanks;
       //***           DtlConAdr3 = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***            %EditW(PDzip:'     -    ');
       //***        Else;
       //***           DtlConAdr2 = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***            %EditW(PDzip:'     -    ');
       //***           DtlConAdr3 = *Blanks;
       //***        EndIf;
       //***** beg add ****** 08/13/13 *************************
       //***       If DtlConPhn# = *Blanks;
       //***** end add ****** 08/13/13 *************************
       //***         If PDarea <> *zeros or PDphn# <> *zeros;
       //***            DtlConPhn# = '(' + %editc(PDarea:'X') + ')' +
       //***            %subst(%editc(PDphn#:'X'):1:3) + '-' +
       //***            %subst(%editc(PDphn#:'X'):4:4);
       //***         Endif;
       //***** beg add ****** 08/13/13 *************************
       //***        Endif;
       //***** end add ****** 08/13/13 *************************
       //***        If PDfxar <> *zeros or PDfax# <> *zeros;
       //***           DtlConFax#  = '(' + %editc(PDfxar:'X') + ')' +
       //***            %subst(%editc(PDfax#:'X'):1:3) + '-' +
       //***            %subst(%editc(PDfax#:'X'):4:4);
       //***        Endif;
       //***     Else;
       //***        DtlConAdr1  = LLpad1;
       //***        DtlConAdr2  = LLpad2;
       //***        If DtlConAdr2<>*Blanks;
       //***           DtlConAdr3 = %Trim(LLpCty) + ', ' + LLpSt + ' ' +
       //***            %EditW(LLpZip:'     -    ');
       //***        Else;
       //***           DtlConAdr2 = %Trim(LLpCty) + ', ' + LLpSt + ' ' +
       //***            %EditW(LLpZip:'     -    ');
       //***           DtlConAdr3 = *Blanks;
       //***        EndIf;
       //***        DtlConName = LLplan;
       //***         If LLplar <> *zeros or LLplap <> *zeros;
       //***             DtlConPhn# = '(' + %editc(LLplar:'X') + ')' +
       //***             %subst(%editc(LLplap:'X'):1:3) + '-' +
       //***             %subst(%editc(LLplap:'X'):4:4);
       //***         Endif;
       //***        If LLplae <> *zeros or LLplaf <> *zeros;
       //***           DtlConFax#  = '(' + %editc(LLplae:'X') + ')' +
       //***            %subst(%editc(LLplaf:'X'):1:3) + '-' +
       //***            %subst(%editc(LLplaf:'X'):4:4);
       //***        Endif;
       //***     Endif;
       //***
       //***  Endif;
       //***
      *//*** /End-Free
     P*//***  LoadPlnAtt      E
      *//******** End dlt ****** 09/04/13 *************************
       //===================================================================
       // LoadTrtPhy - Load Treating Physian Contact info detail box
       //===================================================================
       //******** Beg dlt ****** 09/04/13 *************************
     P*//*** LoadTrtPhy      B
     D*//*** LoadTrtPhy      PI
      *//*** /Free
       //***
       //***  ClearScreen();
       //***
       //***  Chain scConRRN ptp0p;
       //***  If %found(Wmtpp);
       //***
          // Write Trating physian contact info
       //***     DtlConFor  = 'Treating Physian';
       //***     DtlCnEffDt  = CvtToDate(dtMDYY(TPfrdt));
       //***     DtlCnExpDt  = CvtToDate(dtMDYY(TPtodt));
       //***     ##fein = TPphyf;
       //***     ##mod  = TPphym;
       //***     Chain keyWMPDL ppd00;
       //***     If %found(Wmpdl);
       //***        If PDdba1 = 'Y';
       //***           DtlConName = PDadr1;
       //***           DtlConAdr1 = PDadr2;
       //***        Else;
       //***           DtlConName = PDname;
       //***           DtlConAdr1 = PDadr1;
       //***           DtlConAdr2 = PDadr2;
       //***        Endif;
       //***        If DtlConAdr2<>*Blanks;
       //***           DtlConAdr3 = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***            %EditW(PDzip:'     -    ');
       //***        Else;
       //***           DtlConAdr2 = %Trim(PDcty) + ', ' + PDst + ' ' +
       //***            %EditW(PDzip:'     -    ');
       //***           DtlConAdr3 = *Blanks;
       //***        EndIf;
       //***         If PDarea <> *zeros or PDphn# <> *zeros;
       //***            DtlConPhn# = '(' + %editc(PDarea:'X') + ')' +
       //***            %subst(%editc(PDphn#:'X'):1:3) + '-' +
       //***            %subst(%editc(PDphn#:'X'):4:4);
       //***         Endif;
       //***        If PDfxar <> *zeros or PDfax# <> *zeros;
       //***           DtlConFax#  = '(' + %editc(PDfxar:'X') + ')' +
       //***            %subst(%editc(PDfax#:'X'):1:3) + '-' +
       //***            %subst(%editc(PDfax#:'X'):4:4);
       //***        Endif;
       //***     Else;
       //***        DtlConName = TPphyn;
       //***        DtlConAdr1  = TPphya;
       //***        DtlConAdr2 = %Trim(TPphyC) + ', ' + TPphyS + ' ' +
       //***        %EditW(TPphyZ:'     -    ');
       //***        DtlConAdr3 = *Blanks;
       //***          If TPphyr <> *zeros or TPphyp <> *zeros;
       //***             DtlConPhn# = '(' + %editc(TPphyr:'X') + ')' +
       //***             %subst(%editc(TPphyp:'X'):1:3) + '-' +
       //***             %subst(%editc(TPphyp:'X'):4:4);
       //***          Endif;
       //***        If TPfxar <> *zeros or TPfax# <> *zeros;
       //***           DtlConFax#  = '(' + %editc(TPfxar:'X') + ')' +
       //***            %subst(%editc(TPfax#:'X'):1:3) + '-' +
       //***            %subst(%editc(TPfax#:'X'):4:4);
       //***        Endif;
       //***     Endif;
       //***  Endif;
       //***
      *//*** /End-Free
     P*//*** LoadTrtPhy      E
       //******** End dlt ****** 09/04/13 *************************
       //===================================================================
       // LoadEmpCon - Load Employer Contact info detail box
       //===================================================================
       //******** Beg dlt ****** 09/04/13 *************************
     P*//*** LoadEmpCon      B
     D*//*** LoadEmpCon      PI
      *//*** /Free
       //***
       //***  ClearScreen();
       //***
       //***  Chain keyWMEXL pex00;
       //***
       //***  Chain scConRRN pec0p;
       //***  If %found(Wmecp);
       //***
          // Write Employer contact info detail box
       //***  If EClsn <> *blanks;
       //***     DtlConName = %trim(EClsn)+','+ %trim(ECfsn);
       //***  Else;
       //***     DtlConName = %trim(ECfsn);
       //***  Endif;
       //***  If ECmid <> *blanks;
       //***     DtlConName =%trim(DtlConName) + ',' + %trim(ECmid);
       //***  Endif;
       //*** If ECPCNT = '1';
       //***    DtlConFor = EXcnt1;
       //*** ElseIf ECPCNT = '2';
       //***    DtlConFor = EXcnt2;
       //*** ElseIf ECPCNT = '3';
       //***    DtlConFor = EXcnt3;
       //*** Endif;
       //***
       //***     DtlConPst = ECposn;
       //***     DtlConAdr1  = ECadr1;
       //***     DtlConAdr2  = ECadr2;
       //***     If DtlConAdr2<>*Blanks;
       //***        DtlConAdr3 = %Trim(ECcty) + ', ' + ECst + ' ' +
       //***         %EditW(ECzip:'     -    ');
       //***     Else;
       //***        DtlConAdr2 = %Trim(ECcty) + ', ' + ECst + ' ' +
       //***        %EditW(ECzip:'     -    ');
       //***        DtlConAdr3 = *Blanks;
       //***     EndIf;
      //***     If ECarea <> *zeros or ECphn# <> *zeros or ECext# <> *zeros;
      //***        DtlConPhn# = '(' + %editc(ECarea:'X') + ')' +
       //***         %subst(%editc(ECphn#:'X'):1:3) + '-' +
       //***         %subst(%editc(ECphn#:'X'):4:4) + 'x' +
       //***         %trim(%char(ECext#));
       //***     Endif;
       //***    If ECcara <> *zeros or ECcphn <> *zeros;
       //***         DtlConCell = '(' + %editc(ECcara:'X') + ')' +
       //***          %subst(%editc(ECcphn:'X'):1:3) + '-' +
       //***          %subst(%editc(ECcphn:'X'):4:4);
       //***    Endif;
       //***    If ECfara <> *zeros or ECfphn <> *zeros;
       //***        DtlConFax#  = '(' + %editc(ECfara:'X') + ')' +
       //***         %subst(%editc(ECfphn:'X'):1:3) + '-' +
       //***         %subst(%editc(ECfphn:'X'):4:4);
       //***    Endif;
       //***
       //***  Endif;
       //***
      *//*** /End-Free
     P*//*** LoadEmpCon      E
       //******** End dlt ****** 09/04/13 *************************
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy sCopyClmPi
     *========================================================================*
