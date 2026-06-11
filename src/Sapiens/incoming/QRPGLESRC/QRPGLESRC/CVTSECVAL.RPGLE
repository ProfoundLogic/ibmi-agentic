     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
      *=================================================================================
     H* CODE THIS SO THAT IT RUNS WITH SYPOSTCV PART OF BASE RELEASE
     H* THIS PROGRAM WILL RUN WITH EVERY BASE RELEASE SO THAT NEW
     H* APPLICATION SECURITY RECORDS ARE WRITTEN WITH EVERY BASE
     H* RELEASE.
     H*
     H* THE MST TABLE WILL LOAD THE SMASP AND SAASP RECORDS. PARSE IT
     H* OUT BY AREA (1-10), SCREEN FILE (11-20), RCD FORMAT (21-30)
     H*
     H* THE DTL TABLE WILL LOAD THE SDASP RECORDS. PARSE IT
     H* OUT BY AREA (1-10), FIELD (11-50).
     H*
     H*****************************************************************
     H* Revised:                                                      *
     H*
     H*  08/05/24 C:65  Project: WC2951
     H*                 Add security for exp rating and report to ncci
     H*                 fields as new area.  Add the not final checkboxes
     H*                 to existing security area.
     H*
     H*  08/01/24 C:053 Project: WC2975
     H*                 Inforce Premium Calc Config
     H*
     H*  06/24/24 C:300 Project: 146WKR (Prescient)
     H*                 Implement Application Security Editor for the new
     H*                 collateral sections in group configuration and policy detail
     H*
     H*  04/01/24 C:333 Project: WC2826
     H*                 Implement Application Security Editor on button and
     H*                 links, created for Staff Roles Tab and Claim Contacts
     H*
     H*  03/27/24 C:300 Project: WC2676
     H*                 Implement Application Security Editor on each button
     H*                 and link, created for Return Funds Process/CS Standards
     H*
     H*  03/22/24 C:300 Project: WC2433
     H*                 Implement Application Security Editor on each button
     H*                 and link, created for Policy Endrs Forms/CS Standards
     H*
     H*  11/28/23 C:300 Project: WC2675, Support: 114247
     H*                 Since ASE on 'List action drop-down' items is
     H*                 setup through 'Application Configuration' screen
     H*                 So, Remove ARBTCHAR06 area items (imgSelectAction,
     H*                 DrpSelectAction and chkSelectRow)
     H*
     H*  10/21/23 C:300 Project: WC2910
     H*                 ASE not working for 'Print FROI' checkbox
     H*
     H*  10/09/23 C:300 Project: 153WAL
     H*                 Apply ASE on new 'safety fee stat code' field
     H*                 on tab2 of new Group Statistics and Rating screen
     H*
     H*  10/09/23 C:300 Project: WC2820
     H*                 Field Ids got changed of following 4 fields
     H*                 on tab1 of new Group Statistics and Rating screen
     H*                 Gradient Discount Calc and Minimum Premium to Offer
     H*
     H*  10/05/23 C:300 Project: WC2395
     H*                 Clear SMASP and SDASP files
     H*                 Not to clear SAASP file because it holds
     H*                 user defined Security Type/Level data
     H*
     H*  09/26/23 C:300 Project: WC2395, Support: 114000
     H*                 Implement ASE on few missing fields on
     H*                 Group and Agency Configuration Screens
     H*
     H*  09/20/23 C:300 Project: WC2906
     H*                 Implement Application Security Editor on each
     H*                 screen and link, created for Cancellation Batches
     H*
     H*  08/14/23 C:342 Support : 113261
     H*                 Implement Application Security Editor on online
     H*                 FROI interface screen and link.
     H*
     H*  07/03/23 C:300 Support: 107702 (SUNZ)
     H*                 Add Delete from Quote Sec lvl and type on
     H*                 Group Administration / DARTS Configuration Screen
     H*
     H*  06/22/23 C:300 Project: WC2910, Support: 111869
     H*                 Include the iDARTS link in Groups Grid on Main
     H*                 Group Configuration Screen
     H*
     H*  03/20/23 C:300 Project: WC2675
     H*                 Implement Application Security Editor on each
     H*                 screen and link, created for A/R Batch Entry
     H*
     H*  02/15/23 C:300 Project: WC2196
     H*                 Merge WTGRPPRI DSPF into WTGRPCFG DSPF
     H*                 Replace WTGRPPRI with WTGRPCFG
     H*                 Replace RCDGRPINF with RCDGRPADD
     H*
     H*  02/15/23 C:300 Project: WC2395
     H*                 Merge WTAGTPRI DSPF into WTAGTCFG DSPF
     H*                 Replace WTAGTPRI with WTAGTCFG
     H*                 Replace RCDAGTINF with RCDAGTADD
     H*
     H*  12/06/22 C:291 Project: C152WBZ
     H*                 Implement Application Security Editor on Update
     H*                 button for Settlement Grid.
     H*
     H*  09/26/22 C:344 Project: WC2838
     H*                 Add records to the conversion program to secure
     H*                 the Policy Contacts. Added records to the MST
     H*                 and DTL compile time arrays.
     H*
     H*  06/21/22 C:300 Project: WC2395
     H*                 Implement Application Security Editor on each
     H*                 screen and link, created for Agency Maintenance
     H*
     H*  02/14/22 C:079 Project: WC2797
     H*                 Implement Application Security Editor on each
     H*                 field for IAIABC 3.1.4.
     H*
     H*  08/04/21 C:300 Project: WC2196
     H*                 Implement Application Security Editor on each
     H*                 screen and link, created for Group Configuration
     H*
     H*  06/24/20 C:295 Support: 87880
     H*                 Application Security Editor fields are missing
     H*                 for below elements.
     H*                 -Employer Agrees to Compensate
     H*                 -Employer paid salary prior to acquisition
     H*                 -Full Suspension reason code
     H*                 -Partial suspension reason code
     H*                 -Temporary disability benefit ext. code
     H*                 -Vocational rehabilitation
     H*
     H*  06/22/18 C:136 Project: 150WAQ
     H*                 Add collateral grid add button and edit link
     H*                 elements.
     H*
     H*  06/07/18 C:291 Project: 150WCR                               *
     H*                 Fix id Name for Override to Report as PEO     *
     H*                                                               *
     H*  05/17/18 C:291 Project: 150WCR                               *
     H*                 Secure Personal Name, Individual Name and     *
     H*                 Override to Report as PEO.                    *
     H*                                                               *
     H*  04/10/18 C:33  Project: 150WCF                               *
     H*                 Secure Entity/Location ID                     *
     H*                                                               *
     H*  10/31/17 C:136 Project: 150WBW                               *
     H*                 Secure Endorsement Signature grid, links,     *
     H*                 and add button in Application Configuration   *
     H*                                                               *
     h*  09/28/17 C:33  Project: WC2410  48667                        *
     h*                -Secure Claim Detail/Medical/MCO Code          *
     h*                -Secure Claim Detail/Medical/Inj Severity Code *
     H*                                                               *
     h*  09/06/17 C:33  Project: WC2405                               *
     h*                 Ensure CO# is loaded.                         *
     H*                                                               *
     h*  08/09/17 C:267 Project: WC2259                               *
     h*                 Add security area poldtlar21 for Policy Detail*
     H*                 Employee Payroll add,edit,delete buttons;     *
     H*                 Vendors add,edit buttons                      *
     H*                                                               *
     h*  07/20/17 C:33  Project: WC2405                               *
     h*                 Added security area for SQL on Rule setup.    *
     H*                                                               *
     h*  05/09/17 C:180 Support: 46889                                *
     h*                 Added security areas POLLOCAR21-25 for        *
     h*                 Location Master EFT tab.                      *
     h*                                                               *
     h*  03/15/17 C:136 Project: SY371                                *
     h*                 Add security area for Application             *
     h*                 Configuration Signature Grid                  *
     h*                                                               *
     h*  09/09/16 C:65  Project: SY371                                *
     h*                 Change area on secure signature fields.       *
     h*                                                               *
     h*  08/04/16 C:65  Project: SY371                                *
     h*                 Add security area on secure signature fields. *
     h*                                                               *
     h*  05/09/16 C:300 Project: 114WXV                               *
     h*                 Add Cell Number to Claimant Demographics      *
     h*                 Add to CLMDTLAR02 security area               *
     h*                                                               *
     h*  12/11/15 C:53  Project: 133WPK                               *
     h*                 Added new records to DTL compile time array   *
     h*                                                               *
     h*  12/02/15 C:309 Project: 25MUD                                *
     h*                 Added new records to DTL compile time array   *
     h*                                                               *
     h*  10/07/15 C:308 Project: C114WVO                              *
     h*                 Added new wtclmdtl areas                      *
     h*                                                               *
     h*  09/17/15 C:302 Project: WC2254                               *
     h*                 Added new wpipolcpy areas                     *
     h*                                                               *
     h*  06/19/15 C:062 Support: 34349(133)                           *
     h*                 Added new wpipolpop2 security areas           *
     h*                                                               *
     h*  05/21/15 C:295 Project: 114WMZ Priss #14                     *
     h*                 To make different Application Security Editor *
     h*                 lock it differently and allow separate        *
     h*                 security control point.                       *
     h*                 Area  -AUDITRQST1  DrpAuditRequestProc        *
     h*                 Area  -AUDITRQST2  CbxUpdPrevAuditReq         *
     h*                                                               *
     h*  05/07/15 C:295 Project: 114WMZ Priss #14                     *
     h*                 Added the correct feild id for the feilds     *
     h*                       -AUDITRQST1  DrpAuditRequestProc        *
     h*                       -AUDITRQST1  CbxUpdAuditTrack           *
     h*                                                               *
     h*  03/13/15 C:300 Project: 114WLF Priss #36                     *
     h*                 Application Security 0n Claim Configuration/  *
     h*                 Reserve Worksheet - should use 1 lock for all *
     h*                 the applicable fields on screen               *
     h*                                                               *
     h*  02/25/15 C:242 Project: 114WGU                               *
     h*                 Added the following fields:                   *
     h*                      - Area VSTTRK0001                        *
     h*                      - Link lnkEdit                           *
     h*                      - Link lnkDeleteUndelete                 *
     h*                                                               *
     h*  02/23/15 C:282 Project: 114WMZ                               *
     h*                 Added the following fields:                   *
     h*                      - Area AUDITRQST1                        *
     h*                      - Audit Request to Process               *
     h*                      - Previously Run                         *
     h*                                                               *
     h*  12/01/14 C:300 Project: 114WLF                               *
     h*                 Add application security information for the  *
     h*                 editable fields and grid links in WTCLMCFG    *
     h*                                                               *
     h*  09/04/14 C:279 Project: 114WOE                               *
     h*                 Added the following fields:                   *
     h*                      - Area POLLOCAR21                        *
     h*                      - Link (LnkClaimHandlingInstructions)    *
     h*                                                               *
     h*  07/28/14 C:279 Project: 114WMD                               *
     h*                 Added the following fields:                   *
     h*                      - Area CLMPLNAR01                        *
     h*                      - Delete Link                            *
     h*                      - Area CLMPLNAR02                        *
     h*                      - Add Button                             *
     h*                                                               *
     h*  06/09/14 C:267 Support: 85792                                *       *
     h*                 Added the following fields to CLMDTLAR06:     *
     h*                      -new label for Employer Premises checkbox*       *
     h*                      -place zip text box                      *
     h*                                                               *
     H*                                                               *
     H*****************************************************************
     f***  Begin Chg  ***  09/06/17  **************************************
     f**********smupl     if   e           k disk    rename(pup01:pup00)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     f***  End   Chg  ***  09/06/17  **************************************
     fsmasl     uf a e           k disk    rename(pas01:pas00)
     fsaasl     uf a e           k disk    rename(pas01:paa00)
     fsdasl1    uf a e           k disk    rename(pas01:pad01)
     fstprp     if a e           k disk
     f*
     d/copy sPrDate
     d psds          esds                  extname(SZQ1P)
       //***  Begin Chg  ***  08/04/21  ***********************************
     d***** sizeMst         c                   225
     d***** sizeDtl         c                   2000
     d sizeMst         c                   500
     d sizeDtl         c                   5000
       //***  End Chg  ***  08/04/21  ***********************************
     d i               s              5  0 inz
     d x               s              5  0 inz
     d areaID          s                   like(ASAREA)
     d dspfile         s                   like(ASDSPF)
     d formatName      s                   like(ASFRMT)
     d fieldID         s                   like(ASfld)
     d kkco#           s              3  0
     d mst             s             30    dim(sizeMst) ctdata perrcd(1)
     d dtl             s             60    dim(sizeDtl) ctdata perrcd(1)
      ***  Begin Add  ***  10/05/23  ***********************************
     d strCommand      s            600
      * Run Sql Statements
     d RunSql          pr                  ExtPgm('RUNSQL')
     d  strCommand                  600
      ***  End   Add  ***  10/05/23  ***********************************

      /free
       //***  Begin Add  ***  10/05/23  ***********************************
        Monitor;
           clear strCommand;
           strCommand = 'DELETE FROM SMASP';
           RunSql(strCommand);

           clear strCommand;
           strCommand = 'DELETE FROM SDASP';
           RunSql(strCommand);
        On-error;
        Endmon;
       //***  End   Add  ***  10/05/23  ***********************************

       Chain ('SecurityEditorLevelType') ppr01;
       If not(%found);
          PRkey = 'SecurityEditorLevelType';
          PRproperty = '999*';
          Write ppr01;
       Endif;
       //***  Begin Chg  ***  09/06/17  ***********************************
       //**********Chain Q1user PUP00;
       //**********kkCO# = UPco#;
       Setll *loval pco00;
       Read pco00;
       If %eof;
          COco# = 777;
       Endif;

       Dou %eof;
          kkCO# = COco#;
          Exsr ProcessCo;
          Read pco00;
       Enddo;

       *inlr = *on;

       //==================================================================
       Begsr ProcessCo;
       //***  End   Chg  ***  09/06/17  ***********************************

       //Loop Thru MST
       //Read thru **MST compile time data area
       For i = 1 to sizeMst;
         If mst(i) = *blanks;
            Iter;
         Endif;
         Clear areaID;
         Clear dspfile;
         Clear formatName;
         areaID  = %subst(mst(i):1:10);
         dspfile = %subst(mst(i):11:10);
         formatName = %subst(mst(i):21:10);

       //If SMASP record doesn't exist, write records.
         Chain(n) (kkCO#:dspfile:formatName:areaID) PAS00;
         If not %found;
           Exsr WrtsmaslRec;
         Endif;

       //If SAASP record doesn't exist, write records.
         Chain(n) (kkCO#:areaID) PAA00;
         If not %found;
           Exsr WrtsaaslRec;
         Endif;
       Endfor;



       //Loop Thru DTL
       //Read thru **DTL compile time data area
       For x = 1 to sizeDtl;
         If dtl(x) = *blanks;
            Iter;
         Endif;
         Clear areaID;
         Clear fieldID;
         areaID  = %subst(dtl(x):1:10);
         fieldID = %subst(dtl(x):11:50);

       //If SDASP record doesn't exist, write records.
         Chain(n) (kkCO#:areaID:fieldID) PAD01;
         If not %found;
           Exsr Wrtsdasl1Rec;
         Endif;
       Endfor;

       //***  Begin Chg  ***  09/06/17  ***********************************
       //***********inlr = *on;
       Endsr;
       //***  End   Chg  ***  09/06/17  ***********************************

       //////////////////////////////////////////////////////////////

       //Write records to SMASL
       Begsr WrtsmaslRec;
          Clear PAS00;
          ASco# =  kkCO#;
          ASdspf = dspfile;
          ASfrmt = formatName;
          ASarea = areaID;
          ASstat = 'A';
          ASiusr = Q1user;
          ASipgm = Q1pgm;
          ASitm  = Timeto6(SystemTime());
          ASidt  = Dateto7(SystemDate());
          Write PAS00;
       Endsr;

  //*  //Write records to SAASL
       Begsr WrtsaaslRec;
          Clear PAA00;
          ASco# =  kkCO#;
          ASarea = areaID;
          ASENV  = 'L';
          ASstat = 'A';
          ASiusr = Q1user;
          ASipgm = Q1pgm;
          ASitm  = Timeto6(SystemTime());
          ASidt  = Dateto7(SystemDate());
          Write PAA00;
       Endsr;

       //Write records to SDASL1
       Begsr Wrtsdasl1Rec;
          Clear PAD01;
          ASco# =  kkCO#;
          ASarea = areaID;
          ASfld = fieldID;
          ASstat = 'A';
          ASiusr = Q1user;
          ASipgm = Q1pgm;
          ASitm  = Timeto6(SystemTime());
          ASidt  = Dateto7(SystemDate());
          Write PAD01;
       Endsr;
      /end-free
      *
** MST
CLMPRVAR01WTCLMPRV  RCDPAYINFO
CLMPRVAR02WTCLMPRV  RCDPAYINFO
CLMPRVAR03WTCLMPRV  RCDPAYINFO
CLMPRVAR04WTCLMPRV  RCDPAYINFO
CLMRSVAR01WTCLMRSV  RCDREQEDIT
CLMRSVAR02WTCLMRSV  RCDPNDCHK
CLMRSVAR03WTCLMRSV  RCDREQEDIT
CLMRSVAR04WTCLMRSV  RCDEFTINFO
CLMRSVAR05WTCLMRSV  RCDEFTINFO
CLMRSVAR06WTCLMRSV  RCDEFTINFO
CLMRSVAR07WTCLMRSV  RCDEFTINFO
CLMRSVAR08WTCLMRSV  RCDPNDCHK
CLMRSVAR09WTCLMRSV  RCDPNDCHK
POLAUDAR01WTPOLTRACKRCDAUDTRCK
POLAUDAR02WTPOLTRACKRCDAUDTRCK
POLAUDAR03WTPOLTRACKRCDAUDTRK
POLAUDAR04WTPOLTRACKRCDAUDTRK
POLAUDAR05WTPOLTRACKRCDQTETRCK
POLAUDAR06WTPOLTRACKRCDQTETRCK
POLDTLAR01WTPOLDTL  RCDRATING
POLDTLAR02WTPOLDTL  RCDRATING
POLDTLAR03WTPOLDTL  RCDRATING2
POLDTLAR04WTPOLDTL  RCDADDINFO
POLDTLAR05WTPOLDTL  RCDADDINFO
POLDTLAR06WTPOLDTL  RCDADDINFO
POLDTLAR07WTPOLDTL  RCDADDINFO
POLDTLAR08WTPOLDTL  RCDADDINFO
POLDTLAR09WTPOLDTL  RCDADDINFO
POLDTLAR10WTPOLDTL  RCDADDINFO
POLDTLAR11WTPOLDTL  RCDAGENCY
POLDTLAR12WTPOLDTL  RCDAGENCY
POLDTLAR13WTPOLDTL  RCDPAYROLL
POLDTLAR14WTPOLDTL  RCDPAYROLL
POLDTLAR15WTPOLDTL  RCDPAYROLL
POLDTLAR16WTPOLDTL  RCDPAYROLL
POLDTLAR17WTPOLDTL  RCDPAYROLL
POLDTLAR18WTPOLDTL  RCDPAYROLL
POLDTLAR19WTPOLDTL  RCDPAYROLL
POLDTLAR20WTPOLDTL  RCDPAYROLL
POLLOCAR01WPILOCMAINRCDLOCENT
POLLOCAR02WPILOCMAINRCDLOCENT
POLLOCAR03WPILOCMAINRCDLOCENT
POLLOCAR04WPILOCMST RCDLOCWDW
POLLOCAR06WPILOCMAINRCDOWNOFF
POLLOCAR07WPILOCMAINRCDOWNOFF
POLLOCAR08WPILOCMAINRCDOWNOFF
POLLOCAR09WPILOCMAINRCDOWNOFF
POLLOCAR10WPILOCMAINRCDDEPTGRD
POLLOCAR11WPILOCMAINRCDDEPTGRD
POLLOCAR12WPILOCMAINRCDADDNAM
POLLOCAR13WPILOCMAINRCDADDNAM
POLLOCAR14WPILOCMST RCDBILLDIV
POLLOCAR15WPILOCMST RCDBILLDIV
POLLOCAR16WPILOCMST RCDBILLDIV
POLLOCAR17WPILOCMST RCDBILLDIV
POLLOCAR18WPILOCMAINRCDLOCENT
POLLOCAR19WPILOCMAINRCDLOCENT
POLPOPAR01WPIPOLPOP RCDPAYEDT
POLPOPAR02WPIPOLPOP RCDPAYEDT
POLQTEAR01WTPOLQTE  RCDPOLICY
POLQTEAR02WTPOLQTE  RCDPOLICY
POLQTEAR03WTPOLQTE  RCDPOLICY
POLQTEAR04WTPOLQTE  RCDPOLICY
POLSUMAR01WTPOLSUM  RCDALLINFO
POLSUMAR02WTPOLSUM  RCDDIVSUM
POLSUMAR03WTPOLSUM  RCDDIVPREM
POLSUMAR04WTPOLSUM  RCDDIVPREM
POLSUMAR05WTPOLSUM  RCDDIVPREM
CLMDTLAR01WTCLMDTL  RCDCLMDT0
CLMSUMAR01WTCLMSUM  RCDCLMSUM0
CLMSUMAR01WTCLMSUM  RCDCLMSum0
CLMDTLAR02WTCLMDTL  RCDCLMDT0
CLMDTLAR03WTCLMDTL  RCDCLMDT0
CLMDTLAR04WTCLMDTL  RCDCLMDT0
CLMDTLAR05WTCLMDTL  RCDCLMDT0
CLMDTLAR06WTCLMDTL  RCDCLMDT0
CLMDTLAR07WTCLMDTL  RCDCLMDT0
CLMDTLAR08WTCLMDTL  RCDCLMDT0
CLMDTLAR09WTCLMDTL  RCDCLMDT0
CLMDTLAR10WTCLMDTL  RCDCLMDT0
CLMDTLAR11WTCLMDTL  RCDCLMDT0
CLMDTLAR12WTCLMDTL  RCDCLMDT0
CLMDTLAR13WTCLMDTL  RCDCLMDT0
CLMDTLAR14WTCLMDTL  RCDCLMDT1
CLMDTLAR15WTCLMDTL  RCDCLMDT1
CLMDTLAR16WTCLMDTL  RCDCLMDT1
CLMDTLAR17WTCLMDTL  RCDCLMDT1
CLMDTLAR18WTCLMDTL  RCDCLMDT1
CLMDTLAR19WTCLMDTL  RCDCLMDT2
CLMDTLAR20WTCLMDTL  RCDCLMDT2
CLMDTLAR21WTCLMDTL  RCDCLMDT2
CLMDTLAR22WTCLMDTL  RCDCLMDT2
CLMDTLAR23WTCLMDTL  RCDCLMDT2
CLMDTLAR24WTCLMDTL  RCDCLMDT2
CLMDTLAR25WTCLMDTL  RCDCLMDT2
CLMDTLAR26WTCLMDTL  RCDCLMDT2
CLMDTLAR27WTCLMDTL  RCDCLMDT2
CLMDTLAR28WTCLMDTL  RCDCLMDT2
CLMDTLAR29WTCLMDTL  RCDCLMDT2
CLMDTLAR30WTCLMDTL  RCDCLMDT2
CLMDTLAR31WTCLMDTL  RCDCLMDT2
CLMDTLAR32WTCLMDTL  RCDCLMDT3
CLMDTLAR33WTCLMDTL  RCDCLMDT3
CLMDTLAR34WTCLMDTL  RCDCLMDT3
CLMDTLAR35WTCLMDTL  RCDCLMDT3
CLMDTLAR36WTCLMDTL  RCDCLMDT3
CLMDTLAR37WTCLMDTL  RCDCLMDT3
CLMDTLAR38WTCLMDTL  RCDCLMDT3
CLMDTLAR39WTCLMDTL  RCDCLMDT3
CLMDTLAR40WTCLMDTL  RCDCLMDT3
CLMDTLAR41WTCLMDTL  RCDCLMDT3
CLMDTLAR42WTCLMDTL  RCDCLMDT3
CLMDTLAR43WTCLMDTL  RCDCLMDT3
CLMDTLAR44WTCLMDTL  RCDCLMDT3
CLMDTLAR45WTCLMDTL  RCDCLMDT3
CLMDTLAR46WTCLMDTL  RCDCLMDT3
CLMDTLAR47WTCLMDTL  RCDCLMDT3
CLMDTLAR48WTCLMDTL  RCDCLMDT3
CLMDTLAR49WTCLMDTL  RCDCLMDT3
CLMDTLAR50WTCLMDTL  RCDCLMDT3
CLMDTLAR51WTCLMDTL  RCDCLMDT3
CLMDTLAR52WTCLMDTL  RCDCLMDT3
CLMDTLAR53WTCLMDTL  RCDCLMDT3
CLMDTLAR54WTCLMDTL  RCDCLMDT4
CLMDTLAR55WTCLMDTL  RCDCLMDT4
CLMDTLAR56WTCLMDTL  RCDCLMDT4
CLMDTLAR57WTCLMDTL  RCDCLMDT4
CLMDTLAR58WTCLMDTL  RCDCLMDT4
CLMDTLAR59WTCLMDTL  RCDCLMDT4
CLMDTLAR60WTCLMDTL  RCDCLMDT4
CLMDTLAR61WTCLMDTL  RCDCLMDT4
CLMDTLAR62WTCLMDTL  RCDCLMDT4
CLMDTLAR63WTCLMDTL  RCDCLMDTA1
CLMDTLAR64WTCLMDTL  RCDCLMDTA1
CLMDTLAR65WTCLMDTL  RCDCLMDTAL
CLMDTLAR66WTCLMDTL  RCDCLMDTAL
CLMDTLAR67WTCLMDTL  RCDCLMDTAL
CLMDTLAR68WTCLMDTL  RCDCLMDTCA
CLMDTLAR69WTCLMDTL  RCDCLMDTCA
CLMDTLAR70WTCLMDTL  RCDCLMDTDE
CLMDTLAR71WTCLMDTL  RCDCLMDTDE
CLMDTLAR72WTCLMDTL  RCDCLMDTDE
CLMDTLAR73WTCLMDTL  RCDCLMDTDE
CLMDTLAR74WTCLMDTL  RCDCLMDTDE
CLMDTLAR75WTCLMDTL  RCDCLMDTFL
CLMDTLAR76WTCLMDTL  RCDCLMDTGA
CLMDTLAR77WTCLMDTL  RCDCLMDTGA
CLMDTLAR78WTCLMDTL  RCDCLMDTGA
CLMDTLAR79WTCLMDTL  RCDCLMDTLA
CLMDTLAR80WTCLMDTL  RCDCLMDTLA
CLMDTLAR81WTCLMDTL  RCDCLMDTLA
CLMDTLAR82WTCLMDTL  RCDCLMDTLA
CLMDTLAR83WTCLMDTL  RCDCLMDTMA
CLMDTLAR84WTCLMDTL  RCDCLMDTMA
CLMDTLAR85WTCLMDTL  RCDCLMDTMA
CLMDTLAR86WTCLMDTL  RCDCLMDTNC
CLMDTLAR87WTCLMDTL  RCDCLMDTNC
CLMDTLAR88WTCLMDTL  RCDCLMDTNY
CLMDTLAR89WTCLMDTL  RCDCLMDTNY
CLMDTLAR90WTCLMDTL  RCDCLMDTNY
CLMDTLAR91WTCLMDTL  RCDCLMDTNY
CLMDTLAR92WTCLMDTL  RCDCLMDTPA
CLMDTLAR93WTCLMDTL  RCDCLMDTPA
CLMDTLAR94WTCLMDTL  RCDCLMDTWV
CLMDTLAR95WTCLMDTL  RCDCLMDTNI
CLMDTLQD01WTCLMDTL  RCDCLMDT6
CLMDTLQD02WTCLMDTL  RCDCLMDT6
CLMDTLQD03WTCLMDTL  RCDCLMDT6
CLMDTLQD04WTCLMDTL  RCDCLMDT6
CLMDTLQD05WTCLMDTL  RCDCLMDT6
CLMRSVAR10WTCLMRSV  RCDRSVDTL
CLMRSVAR11WTCLMRSV  RCDRSVDTL
CLMRSVAR12WTCLMRSV  RCDRSVDTL
CLMRSVAR13WTCLMRSV  RCDRSVDTL
CLMRSVAR14WTCLMRSV  RCDRSVDTL
CLMRSVAR15WTCLMRSV  RCDPAYDTL
CLMRSVAR16WTCLMRSV  RCDPAYDTL
CLMRSVAR17WTCLMRSV  RCDPAYDTL
CLMRSVAR18WTCLMRSV  RCDPAYDTL
CLMRSVAR19WTCLMRSV  RCDDEDHST
CLMCMPAR01WTCLMCMP  RCDEDIINFO
CLMCMPAR02WTCLMCMP  RCDEDIINFO
CLMCMPAR03WTCLMCMP  RCDEDIINFO
CLMCMPAR04WTCLMCMP  RCDCMSINFO
CLMCMPAR05WTCLMCMP  RCDCMSINFO
CLMACTAR01WTACTPNL  RCDCLMPNL
POLLOCAR20WPILOCMAINRCDLOCENT
POLSUMAR06WTPOLSUM  RCDALLINFO
POLSUMAR07WTPOLSUM  RCDDIVSUM
CLMPLNAR01WTCLMACTP RCDACTINFO
CLMPLNAR02WTCLMACTP RCDACTINFO
POLLOCAR21WPILOCMST RCDBILLDIV
CLMCFGAR01WTCLMCFG  RCDREQRSVW                                                    ** 12/01/14
AUDITRQST1W7230F    RCDAUDRQ                                                      ** 02/23/15
AUDITRQST2W7230F    RCDAUDRQ                                                      ** 05/21/15
VSTTRK0001WTVSTTRK  RCDVSTTRK
VSTTRK0002WTVSTTRK  RCDVSTTRK                                                     ** 09/26/23
VSTTRK0003WTVSTTRK  RCDVSTTRK                                                     ** 09/26/23
POLPOP2A01WPIPOLPOP2RCDAUDTPYR
POLPOP2A02WPIPOLPOP2RCDSLFRPT
EMPCOPY001WPIPOLCPY RECCPYEMP                                                     ** 09/17/15
EMPCOPY002WPIPOLCPY RECCPYEMP                                                     ** 09/17/15
CLMDTLEM01WTCLMDTL  RCDCLMDT0                                                     ** 10/07/15
CLMDTLAR96WTCLMDTL  RCDCLMDT1                                                     ** 10/07/15
OUTSECSIG STAPPCFG  RCDREPLACE                                                    ** 08/04/16
OUTPUTCTRLSTAPPCFG  RCDOUTCFG                                                     ** 03/21/17
POLLOCAR22WPILOCMAINRCDEFTINFO
POLLOCAR23WPILOCMAINRCDEFTINFO
POLLOCAR24WPILOCMAINRCDEFTINFO
POLLOCAR25WPILOCMAINRCDEFTINFO
RULESQL   STAPPCFG  RCDTRIGFIL                                                    ** 08/04/16
POLDTLAR21WTPOLDTL  RCDPAYROLL                                                    ** 08/09/17
OUTPENDSIGWSDOCPOLP RCDENDSIG                                                     ** 10/31/17
POLENTLOC1WPILOCMST RCDLOCWDW
POLLOCAR26WPILOCMST RCDBILLDIV                                                    ** 05/17/18
POLLOCAR27WPILOCMST RCDBILLDIV                                                    ** 05/17/18
COLLATERALWPILOCMAINRCDCOLLAT                                                     ** 06/22/18
GRPCFGAR01WTGRPCFG  RCDGRPCFG                                                     ** 08/04/21
GRPCFGAR02WTGRPCFG  RCDGRPCFG                                                     ** 08/04/21
GRPCFGAR03WTGRPCFG  RCDGRPCFG                                                     ** 08/04/21
GRPCFGAR04WTGRPCFG  RCDGRPCFG                                                     ** 06/22/23
GRPPRIAR01WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR02WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR03WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR04WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR05WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR06WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR07WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR08WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR09WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR10WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR11WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR12WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR13WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR14WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR15WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR16WTGRPCFG  RCDGRPADD                                                     ** 08/04/21
GRPPRIAR17WTGRPCFG  RCDADDINF                                                     ** 08/04/21
GRPPRIAR18WTGRPCFG  RCDADDINF                                                     ** 08/04/21
GRPPRIAR19WTGRPCFG  RCDADDINF                                                     ** 08/04/21
GRPPRIAR20WTGRPCFG  RCDADDINF                                                     ** 08/04/21
GRPPRIAR21WTGRPCFG  RCDADDINF                                                     ** 08/04/21
GRPPRIAR22WTGRPCFG  RCDADDINF                                                     ** 08/04/21
GRPPRIAR23WTGRPCFG  RCDADDINF                                                     ** 08/04/21
GRPPRIAR24WTGRPCFG  RCDFININF                                                     ** 08/04/21
GRPPRIAR25WTGRPCFG  RCDFININF                                                     ** 08/04/21
GRPPRIAR26WTGRPCFG  RCDFININF                                                     ** 08/04/21
GRPADMAR01WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR02WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR03WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR04WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR05WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR06WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR07WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR08WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR09WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR10WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR11WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR12WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR13WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR14WTGRPADMINRCDPOLINF                                                     ** 08/04/21
GRPADMAR15WTGRPADMINRCDCLMINF                                                     ** 08/04/21
GRPADMAR16WTGRPADMINRCDCLMINF                                                     ** 08/04/21
GRPADMAR17WTGRPADMINRCDCLMINF                                                     ** 08/04/21
GRPADMAR18WTGRPADMINRCDCLMINF                                                     ** 08/04/21
GRPADMAR19WTGRPADMINRCDCLMINF                                                     ** 08/04/21
GRPADMAR20WTGRPADMINRCDCLMINF                                                     ** 08/04/21
GRPADMAR21WTGRPADMINRCDCLMINF                                                     ** 08/04/21
GRPADMAR22WTGRPADMINRCDCLMINF                                                     ** 08/04/21
GRPPOLAR01WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR02WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR03WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR04WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR05WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR06WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR07WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR08WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR09WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR10WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR11WTGRPPOLCGRCDPOLINF                                                     ** 08/04/21
GRPPOLAR12WTGRPPOLCGRCDPANEL                                                      ** 08/04/21
GRPPOLAR13WTGRPPOLCGRCDACCCNT                                                     ** 08/04/21
GRPPOLAR14WTGRPPOLCGRCDACCCNT                                                     ** 08/04/21
GRPPOLAR15WTGRPPOLCGRCDACCCNT                                                     ** 08/04/21
GRPPOLAR16WTGRPPOLCGRCDACCCNT                                                     ** 08/04/21
GRPPOLAR17WTGRPPOLCGRCDCOMM                                                       ** 08/04/21
GRPPOLAR18WTGRPPOLCGRCDCOMM                                                       ** 08/04/21
GRPPOLAR19WTGRPPOLCGRCDCOMM                                                       ** 08/04/21
GRPPOLAR20WTGRPPOLCGRCDCANCEL                                                     ** 08/04/21
GRPPOLAR21WTGRPPOLCGRCDACCCNT                                                     ** 09/26/23
GRPPOLAR22WTGRPPOLCGRCDACCCNT                                                     ** 09/26/23
GRPPOLAR23WTGRPPOLCGRCDACCCNT                                                     ** 09/26/23
GRPPOLAR24WTGRPPOLCGRCDPOLINF                                                     ** 09/26/23
MIDENDAR01WTGRPMEND RCDMENDMST                                                    ** 08/04/21
MIDENDAR02WTGRPMEND RCDMENDMST                                                    ** 08/04/21
MIDENDAR03WTGRPMEND RCDMENDMST                                                    ** 08/04/21
MIDENDAR04WTGRPMEND RCDMENDMST                                                    ** 08/04/21
MIDENDAR05WTGRPMEND RCDMENDMST                                                    ** 08/04/21
ENDFRMAR01WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
ENDFRMAR02WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
ENDFRMAR03WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
ENDFRMAR04WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
ENDFRMAR05WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
ENDFRMAR06WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
ENDFRMAR07WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
ENDFRMAR08WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
ENDFRMAR09WTGRPEND  RCDENDRFRM                                                    ** 08/04/21
TMPLATAR01WITMPLT   RCDTEMPASC                                                    ** 08/04/21
TMPLATAR02WITMPLT   RCDTEMPASC                                                    ** 08/04/21
TMPLATAR03WITMPLT   RCDTEMPASC                                                    ** 08/04/21
CARDFTAR01WITBLRDFT RCDDFTM                                                       ** 08/04/21
CARDFTAR02WITBLRDFT RCDDFTM                                                       ** 08/04/21
CARDFTAR03WITBLRDFT RCDDFTM                                                       ** 08/04/21
GRPCLMAR01WTGRPCLMCGRCDRSVCUS                                                     ** 08/04/21
GRPCLMAR02WTGRPCLMCGRCDRSVCUS                                                     ** 08/04/21
GRPCLMAR03WTGRPCLMCGRCDCOMPPAY                                                    ** 08/04/21
GRPCLMAR04WTGRPCLMCGRCDMCOGRP                                                     ** 08/04/21
GRPCLMAR05WTGRPCLMCGRCDMCOGRP                                                     ** 08/04/21
GRPCLMAR06WTGRPCLMCGRCDMCOGRP                                                     ** 08/04/21
GRPCLMAR07WTGRPCLMCGRCDLETTER                                                     ** 08/04/21
GRPCLMAR08WTGRPCLMCGRCDLETTER                                                     ** 08/04/21
GRPCLMAR09WTGRPCLMCGRCDLETTER                                                     ** 08/04/21
GRPCLMAR10WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR11WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR12WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR13WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR14WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR15WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR16WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR17WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR18WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR19WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR20WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR21WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR22WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPCLMAR23WTGRPCLMCGRCDCOMPPAY                                                    ** 09/26/23
GRPDTAAR01WTGRPDATA RCDPOLEXP                                                     ** 08/04/21
GRPDTAAR02WTGRPDATA RCDEDIINF                                                     ** 08/04/21
GRPDTAAR03WTGRPDATA RCDEDIINF                                                     ** 08/04/21
GRPDTAAR04WTGRPDATA RCDEDIINF                                                     ** 08/04/21
GRPDTAAR05WTGRPDATA RCDEDIINF                                                     ** 08/04/21
GRPSSIAR01WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR02WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR03WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR04WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR05WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR06WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR07WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR08WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR09WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR10WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR11WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR12WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR13WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR14WTGRPSSI  RCDRATING                                                     ** 08/04/21
GRPSSIAR15WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR16WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR17WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR18WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR19WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR20WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR21WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR22WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR23WTGRPSSI  RCDADDPRM                                                     ** 08/04/21
GRPSSIAR24WTGRPSSI  RCDPOLPAG                                                     ** 08/04/21
GRPSSIAR25WTGRPSSI  RCDPOLPAG                                                     ** 08/04/21
GRPSSIAR26WTGRPSSI  RCDPOLPAG                                                     ** 08/04/21
GRPSSIAR27WTGRPSSI  RCDPOLPAG                                                     ** 08/04/21
GRPSSIAR28WTGRPSSI  RCDCXREIN                                                     ** 08/04/21
GRPSSIAR29WTGRPSSI  RCDCXREIN                                                     ** 08/04/21
GRPSSIAR30WTGRPSSI  RCDCXREIN                                                     ** 08/04/21
GRPSSIAR31WTGRPSSI  RCDCXREIN                                                     ** 08/04/21
GRPSSIAR32WTGRPSSI  RCDCXREIN                                                     ** 08/04/21
GRPSSIAR33WTGRPSSI  RCDCXREIN                                                     ** 08/04/21
GRPSSIAR34WTGRPSSI  RCDCXREIN                                                     ** 08/04/21
GRPSSIAR35WTGRPSSI  RCDENDORS                                                     ** 08/04/21
GRPSSIAR36WTGRPSSI  RCDENDORS                                                     ** 08/04/21
GRPSSIAR37WTGRPSSI  RCDENDORS                                                     ** 08/04/21
GRPSSIAR38WTGRPSSI  RCDENDORS                                                     ** 08/04/21
GRPSSIAR39WTGRPSSI  RCDINSFEE                                                     ** 08/04/21
GRPSSIAR40WTGRPSSI  RCDINSFEE                                                     ** 08/04/21
GRPSSIAR41WTGRPSSI  RCDSTCOMP                                                     ** 08/04/21
GRPSSIAR42WTGRPSSI  RCDSTCOMP                                                     ** 08/04/21
GRPSSIAR43WTGRPSSI  RCDSTCOMP                                                     ** 08/04/21
GRPSSIAR44WTGRPSSI  RCDSTCOMP                                                     ** 08/04/21
GRPSTRAR01WTGRPSTRATRCDPANEL                                                      ** 08/04/21
GRPSTRAR02WTGRPSTRATRCDPRMINFO                                                    ** 08/04/21
GRPSTRAR03WTGRPSTRATRCDPRMINFO                                                    ** 08/04/21
GRPSTRAR04WTGRPSTRATRCDPRMINFO                                                    ** 08/04/21
GRPSTRAR05WTGRPSTRATRCDPRMINFO                                                    ** 08/04/21
GRPSTRAR06WTGRPSTRATRCDPRMINFO                                                    ** 08/04/21
GRPSTRAR07WTGRPSTRATRCDFEES                                                       ** 08/04/21
GRPSTRAR08WTGRPSTRATRCDFEES                                                       ** 08/04/21
GRPSTRAR09WTGRPSTRATRCDTAXES                                                      ** 08/04/21
GRPSTRAR10WTGRPSTRATRCDTAXES                                                      ** 08/04/21
GRPSTRAR11WTGRPSTRATRCDTAXES                                                      ** 08/04/21
GRPSTRAR12WTGRPSTRATRCDTAXES                                                      ** 08/04/21
GRPSTRAR13WTGRPSTRATRCDLRGDED                                                     ** 08/04/21
GRPSTRAR14WTGRPSTRATRCDGEN                                                        ** 08/04/21
CLMDTLAR97WTCLMDTL  RCDCLMDT3                                                     ** 10/07/15
AGTCFGAR01WTAGTCFG  RCDAGTCFG                                                     ** 06/21/22
AGTCFGAR02WTAGTCFG  RCDAGTCFG                                                     ** 06/21/22
AGTCFGAR03WTAGTCFG  RCDAGTCFG                                                     ** 06/21/22
AGTPRIAR01WTAGTCFG  RCDAGTADD                                                     ** 06/21/22
AGTPRIAR02WTAGTCFG  RCDAGTADD                                                     ** 06/21/22
AGTPRIAR03WTAGTCFG  RCDAGTADD                                                     ** 09/26/23
AGTASNAR01WTAGTASN  RCDAGTASN                                                     ** 06/21/22
AGTASNAR02WTAGTASN  RCDAGTASN                                                     ** 06/21/22
AGTASNAR03WTAGTASN  RCDAGTASN                                                     ** 06/21/22
AGTASNAR04WTAGTASN  RCDQTETRCK                                                    ** 06/21/22
AGTASNAR05WTAGTASN  RCDQTETRCK                                                    ** 06/21/22
AGTASNAR06WTAGTASN  RCDEFTINFO                                                    ** 06/21/22
AGTASNAR07WTAGTASN  RCDEFTINFO                                                    ** 06/21/22
AGTASNAR08WTAGTASN  RCDEFTINFO                                                    ** 06/21/22
AGTASNAR09WTAGTASN  RCDEFTINFO                                                    ** 06/21/22
AGTPROAR01WTAGTPROD RCDAGTPROD                                                    ** 06/21/22
AGTPROAR02WTAGTPROD RCDAGTPROD                                                    ** 06/21/22
AGTPROAR03WTAGTPROD RCDAGTPROD                                                    ** 06/21/22
AGTPROAR04WTAGTPROD RCDAGTPROD                                                    ** 06/21/22
AGTPROAR05WTAGTPROD RCDAGTPROD                                                    ** 06/21/22
AGTPROAR06WTAGTPROD RCDAGTPROD                                                    ** 06/21/22
AGTPROAR07WTAGTPROD RCDAGTPROD                                                    ** 09/26/23
AGTCONAR01WTAGTCON  RCDAGTCONT                                                    ** 06/21/22
AGTCONAR02WTAGTCON  RCDAGTCONT                                                    ** 06/21/22
AGTCONAR03WTAGTCON  RCDAGTCONT                                                    ** 06/21/22
AGTCONAR04WTAGTCON  RCDAGTCONT                                                    ** 06/21/22
AGTCONAR05WTAGTCON  RCDADDAGCO                                                    ** 06/21/22
AGTCONAR06WTAGTCON  RCDADDAGCO                                                    ** 06/21/22
AGTCONAR07WTAGTCON  RCDADDAGCO                                                    ** 06/21/22
AGTCONAR08WTAGTCON  RCDADDAGCO                                                    ** 06/21/22
AGTCONAR09WTAGTCON  RCDADDAGCO                                                    ** 06/21/22
POLCNTAR01WTPOLCONT RCDPOLCONT                                                    ** 09/26/22
POLCNTAR02WTPOLCONT RCDADDCON                                                     ** 09/26/22
POLCNTAR03WTPOLCONT RCDADDCON                                                     ** 09/26/22
POLCNTAR04WTPOLCONT RCDADDCON                                                     ** 09/26/22
POLCNTAR05WTPOLCONT RCDADDCON                                                     ** 09/26/22
AGTCNTAR01WTAGTCON  RCDADDAGCO                                                    ** 09/26/22
AGTCNTAR02WTAGTCON  RCDADDAGCO                                                    ** 09/26/22
AGTCNTAR03WTAGTCON  RCDADDAGCO                                                    ** 09/26/22
AGTCNTAR04WTAGTCON  RCDADDAGCO                                                    ** 09/26/22
AGTCNTAR05WTAGTCON  RCDADDAGCO                                                    ** 09/26/22
ARBTCHAR01WTARBTCH  RCDARBTCH                                                     ** 03/20/23
ARBTCHAR02WTARBTCH  RCDARBTCH                                                     ** 03/20/23
ARBTCHAR03WTARBTCH  RCDARBTCH                                                     ** 03/20/23
ARBTCHAR04WTARBTCH  RCDARBTCH                                                     ** 03/20/23
ARBTCHAR05WTARBTCH  RCDARBTCH                                                     ** 03/20/23
ARBTCHAR08WTARBTCH  RCDARBTCH                                                     ** 03/20/23
ARBTCHAR09WTARBTCH  RCDARBTCH                                                     ** 03/20/23
ARBTCHAR10WTARBTCH  RCDARBTCH                                                     ** 03/20/23
FROILIST01WTCLMFROI RCDFROIINT                                                    ** 08/14/23
FROILIST02WTCLMFROI RCDFROIINT                                                    ** 08/14/23
FROILIST03WTCLMFROI RCDFROIINT                                                    ** 08/14/23
CXBTCHAR01WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
CXBTCHAR02WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
CXBTCHAR03WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
CXBTCHAR04WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
CXBTCHAR05WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
CXBTCHAR06WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
CXBTCHAR07WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
CXBTCHAR08WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
CXBTCHAR09WTCXBTCH  RCDCXBTCH                                                     ** 09/20/23
ENDFRMAR01WTPOLDTL  RCDENDFRM                                                     ** 03/22/24
ENDFRMAR02WTPOLDTL  RCDENDFRM                                                     ** 03/22/24
ENDFRMAR03WTPOLDTL  RCDENDFRM                                                     ** 03/22/24
ENDFRMAR04WTPOLDTL  RCDENDFRM                                                     ** 03/22/24
ENDFRMAR05WTPOLDTL  RCDENDFRM                                                     ** 03/22/24
ENDFRMAR06WTPOLDTL  RCDENDFRM                                                     ** 03/22/24
RFBTCHAR01WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFBTCHAR02WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFBTCHAR03WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFBTCHAR04WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFBTCHAR05WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFBTCHAR06WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFBTCHAR07WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFBTCHAR08WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFBTCHAR09WTRFBTCH  RCDRFBTCH                                                     ** 03/27/24
RFCHKAR01 WTRFCHK   RCDRFCHKS                                                     ** 03/27/24
RFCHKAR02 WTRFCHK   RCDRFCHKS                                                     ** 03/27/24
RFCHRQAR01WTRFCHKREQRCDRFCHKR                                                     ** 03/27/24
RFCHRQAR02WTRFCHKREQRCDRFCHKR                                                     ** 03/27/24
RFCHRQAR03WTRFCHKREQRCDRFCHKR                                                     ** 03/27/24
RFCHRQAR04WTRFCHKREQRCDRFCHKR                                                     ** 03/27/24
RFCHRQAR05WTRFCHKREQRCDRFCHKR                                                     ** 03/27/24
RFCHRQAR06WTRFCHKREQRCDRFCHKR                                                     ** 03/27/24
RFCHRQAR07WTRFCHKREQRCDRFCHKR                                                     ** 03/27/24
CLMCONAR01WTCLMCON  RCDCONINFO                                                    ** 04/01/24
CLMCONAR02WTCLMCON  RCDCONINFO                                                    ** 04/01/24
CLMCONAR03WTCLMCON  RCDCONINFO                                                    ** 04/01/24
DSTTYPAR01STDSTTYPE RCDDISTY                                                      ** 04/01/24
DSTTYPAR02STDSTTYPE RCDDISTY                                                      ** 04/01/24
DSTTYPAR03STDSTTYPE RCDDISTY                                                      ** 04/01/24
DSTTYPAR01STDSTTYPE RCDCONTTYP                                                    ** 04/01/24
DSTTYPAR02STDSTTYPE RCDCONTTYP                                                    ** 04/01/24
DSTTYPAR03STDSTTYPE RCDCONTTYP                                                    ** 04/01/24
GRPPOLAR25WTGRPPOLCGRCDPOLINF                                                     ** 06/24/24
POLDTLAR22WTPOLDTL  RCDADDINFO                                                    ** 06/24/24
POLDTLAR23WTPOLDTL  RCDRATING2                                                    ** 08/05/24
** DTL
CLMPRVAR01LnkAddPEdit
CLMPRVAR02LnkEFTinfo
CLMPRVAR03BtnAddpAdd
CLMPRVAR03CxbGrpChk
CLMPRVAR04OutAddPName
CLMPRVAR04OutAddPAdd
CLMPRVAR04OutAddPCity
CLMPRVAR04OutAddpChk
CLMPRVAR04OutAddpSts
CLMPRVAR04EmailHyperlink1_copy
CLMPRVAR04LblDtlcheck
CLMPRVAR04LblDtlBO
CLMPRVAR04LblDtlDep
CLMPRVAR04LblDtlDoB
CLMPRVAR04LblDtlReln
CLMPRVAR04LblDtlSts
CLMPRVAR04LblDtlSS#
CLMPRVAR04OutDetAdr1
CLMPRVAR04OutDetAdr2
CLMPRVAR04OutDetAllCheck
CLMPRVAR04OutDetCity
CLMPRVAR04OutDetCC
CLMPRVAR04OutDetDep
CLMPRVAR04OutDetDepBO
CLMPRVAR04OutDetDepDOB
CLMPRVAR04OutDetDepReln
CLMPRVAR04OutDetName
CLMPRVAR04OutDetProv
CLMPRVAR04OutDetSt
CLMPRVAR04OutDetSts
CLMPRVAR04OutDetSS#
CLMPRVAR04OutDetZip
CLMRSVAR01CxbGrpChk
CLMRSVAR02LnkReqEdit
CLMRSVAR04LnkEFTEdt
CLMRSVAR05LnkEFTBnkInfo
CLMRSVAR06LnkRqPNote
CLMRSVAR07BtnEFTAdd
CLMRSVAR08LnkPendRelease
CLMRSVAR09LnkPenChkReqDlt
POLAUDAR01Hyperlink1
POLAUDAR02Hyperlink2
POLAUDAR03Hyperlink1_copy2
POLAUDAR04Hyperlink2_copy
POLAUDAR05Hyperlink1
POLAUDAR06Hyperlink2
POLDTLAR01BtnProrate
POLDTLAR01BtnShortRate
POLDTLAR01CbxARating
POLDTLAR01CbxConcurrency
POLDTLAR01CbxExcludefrom2500
POLDTLAR01CbxOverrideProrata
POLDTLAR01DrpRateSetCode
POLDTLAR01Image1_copy
POLDTLAR01Lbl#ofEmployeesLabel
POLDTLAR01LblAnniversaryDateLabel
POLDTLAR01LblRateSetCodeLabel
POLDTLAR01LblShortRateProrateLabel
POLDTLAR01LblTypeofBuildingLabel
POLDTLAR01LblTypeofPolicyLabel
POLDTLAR01TxaPolicyDateRangeDash
POLDTLAR01Txb#EmployeesBox
POLDTLAR01TxbAnniversaryDateBox
POLDTLAR01TxbPolicyEffectiveDateBox
POLDTLAR01TxbPolicyExpirationDateBox
POLDTLAR01TxbTypeofBuildingBox
POLDTLAR01TxbTypeofPolicyBox
POLDTLAR02CbxA/RSummary
POLDTLAR02CbxBillEffDay
POLDTLAR02CbxExemptInstallFees
POLDTLAR02CbxSpreadAdjustment
POLDTLAR02DrpBillAux
POLDTLAR02LblActivityStatusLabel
POLDTLAR02LblBillingStartDateLabel
POLDTLAR02LblBSCDLabel
POLDTLAR02LblBSCDLabel_copy
POLDTLAR02LblPremFinanceCoLabel
POLDTLAR02TxbActivityStatus
POLDTLAR02TxbBillingStartDateBox
POLDTLAR02TxbBSCDBox
POLDTLAR02TxbPremiumFinanceCo
POLDTLAR03lblContractorsCredit
POLDTLAR03lblDeductibleDiscountHeader
POLDTLAR03lblIncreasedLimitsHeader
POLDTLAR03lblMembershipDues
POLDTLAR03lblSafetyFees
POLDTLAR03lblSubjectCredit1B
POLDTLAR03lblWaiverSurcharge
POLDTLAR03lbl1stDeviation
POLDTLAR03lbl2ndDeviation
POLDTLAR03BtnRateCancel
POLDTLAR03BtnRateSave
POLDTLAR03CbxBillDeducCXHeader
POLDTLAR03DrpExpModStatus
POLDTLAR03DrpWaiverPremiumLevel
POLDTLAR03DrpWaiverType
POLDTLAR03LblARAPMods
POLDTLAR03LblContractorsCreditPercentage
POLDTLAR03LblCCEffectiveDate
POLDTLAR03LblCCExpirationDate
POLDTLAR03LblDeductibleDiscountCode
POLDTLAR03LblDeductibleDiscountCode_copy
POLDTLAR03LblDeviation1Percentage
POLDTLAR03LblDeviation1Percentage_copy
POLDTLAR03LblDeviation2Percentage
POLDTLAR03LblDeviation2Percentage_copy
POLDTLAR03LblDev1_1
POLDTLAR03LblDev1_2
POLDTLAR03LblDev1EffectiveDate
POLDTLAR03LblDev1EffectiveDate_copy
POLDTLAR03LblDev1ExpirationDate
POLDTLAR03LblDev1ExpirationDate_copy
POLDTLAR03LblDev2_1
POLDTLAR03LblDev2_2
POLDTLAR03LblDev2EffectiveDate
POLDTLAR03LblDev2EffectiveDate_copy
POLDTLAR03LblDev2ExpirationDate
POLDTLAR03LblDev2ExpirationDate_copy
POLDTLAR03LblDDEffectiveDate
POLDTLAR03LblDDEffectiveDate_copy
POLDTLAR03LblDDExpirationDate
POLDTLAR03LblDDExpirationDate_copy
POLDTLAR03LblDD1
POLDTLAR03LblDD2
POLDTLAR03LblExperienceModifiers
POLDTLAR03LblExperienceModifiersDates
POLDTLAR03LblExperienceStatus
POLDTLAR03LblExpMod1
POLDTLAR03LblExpMod2
POLDTLAR03LblExpMod3
POLDTLAR03LblFacReinEffDate
POLDTLAR03LblFacReinExpDate
POLDTLAR03LblFacReinPercentage
POLDTLAR03LblFacReinPercentage_copy
POLDTLAR03LblIncreasedLimitsCode
POLDTLAR03LblIncreasedLimitsCode_copy
POLDTLAR03LblILEffectiveDate
POLDTLAR03LblILEffectiveDate_copy
POLDTLAR03LblILExpirationDate
POLDTLAR03LblILExpirationDate_copy
POLDTLAR03LblIL1
POLDTLAR03LblIL2
POLDTLAR03LblMCOGroupCode
POLDTLAR03LblMCOTypeCode
POLDTLAR03LblSafetyFeeMinimum
POLDTLAR03LblSafetyFeePercentage
POLDTLAR03LblSbj1AEffectiveDate
POLDTLAR03LblSbj1AExpirationDate
POLDTLAR03LblSbj1BEffectiveDate
POLDTLAR03LblSbj1BExpirationDate
POLDTLAR03LblScheduleDB/CR
POLDTLAR03LblScheduleDBCREffectiveDate
POLDTLAR03LblScheduleDBCRExpirationDate
POLDTLAR03LblScheduleDBCRFactor
POLDTLAR03LblStock Discount
POLDTLAR03LblStock Discount%
POLDTLAR03LblStock DiscountCalculated%
POLDTLAR03LblStock DiscountOr
POLDTLAR03LblStock DiscountType
POLDTLAR03LblSubject1APercentage
POLDTLAR03LblSubject1BPercentage
POLDTLAR03LblVolLaborAmount
POLDTLAR03LblWaiverSurcharge
POLDTLAR03LblWaiverSurchargeFlatCharge
POLDTLAR03LblWaiverSurchargeMaximum
POLDTLAR03LblWaiverSurchargeMinimum
POLDTLAR03LblWaiverSurchargePercentage
POLDTLAR03LblWaiverSurchargePremiumLevel
POLDTLAR03LnkCalcContractorsCR
POLDTLAR03LnkScheduleDBCR
POLDTLAR03OutputField1
POLDTLAR03RbxBillDeducCX
POLDTLAR03RbxBillDeducCXNo
POLDTLAR03TxbARAPMod1
POLDTLAR03TxbARAPMod2
POLDTLAR03TxbARAPMod3
POLDTLAR03TxbContractorsCREffDate
POLDTLAR03TxbContractorsCRPercentage
POLDTLAR03TxbDeductibleDiscountCode1
POLDTLAR03TxbDeductibleDiscountCode2
POLDTLAR03TxbDev1EffDate
POLDTLAR03TxbDev1EffDate2
POLDTLAR03TxbDev1ExpDate
POLDTLAR03TxbDev1ExpDate2
POLDTLAR03TxbDev1Percentage
POLDTLAR03TxbDev1Percentage2
POLDTLAR03TxbDev2EffDate
POLDTLAR03TxbDev2EffDate2
POLDTLAR03TxbDev2ExpDate
POLDTLAR03TxbDev2ExpDate2
POLDTLAR03TxbDev2Percentage
POLDTLAR03TxbDev2Percentage2
POLDTLAR03TxbDD1EffDate
POLDTLAR03TxbDD1ExpDate
POLDTLAR03TxbDD2EffDate
POLDTLAR03TxbDD2ExpDate
POLDTLAR03TxbExpMod1
POLDTLAR03TxbExpMod1Date
POLDTLAR03TxbExpMod2
POLDTLAR03TxbExpMod2Date
POLDTLAR03TxbExpMod3
POLDTLAR03TxbExpMod3Date
POLDTLAR03TxbFacReinAmount
POLDTLAR03TxbFacReinEffDate
POLDTLAR03TxbFacReinExpDate
POLDTLAR03TxbFacReinPercentage
POLDTLAR03TxbIncrLimitsCode1
POLDTLAR03TxbIncrLimitsCode2
POLDTLAR03TxbIL1EffDate
POLDTLAR03TxbIL1ExpDate
POLDTLAR03TxbIL2EffDate
POLDTLAR03TxbIL2ExpDate
POLDTLAR03TxbMembershipDuesAmount
POLDTLAR03TxbMCOGroupCode
POLDTLAR03TxbMCOTypeCode
POLDTLAR03TxbSafetyFeesAmount
POLDTLAR03TxbSafetyFeesMinimum
POLDTLAR03TxbScheduleDBCREffDate
POLDTLAR03TxbScheduleDBCRExpDate
POLDTLAR03TxbScheduleDBCRFactor
POLDTLAR03TxbStockDiscount%
POLDTLAR03TxbStockDiscountCode
POLDTLAR03TxbSubject1AEffDate
POLDTLAR03TxbSubject1AExpDate
POLDTLAR03TxbSubject1APercentage
POLDTLAR03TxbSubject1BEffDate
POLDTLAR03TxbSubject1BExpDate
POLDTLAR03TxbSubject1BPercentage
POLDTLAR03TxbVolLaborAmount
POLDTLAR03TxbWaiverFlatCharge
POLDTLAR03TxbWaiverMaximum
POLDTLAR03TxbWaiverMinimum
POLDTLAR03TxbWaiverPercentage
POLDTLAR04CbxPolicyPage3CDefault
POLDTLAR04LblPolicyPage3C
POLDTLAR04TxbPolicyPage3CLine1
POLDTLAR04TxbPolicyPage3CLine2
POLDTLAR05BtnAddExcludedStates
POLDTLAR05LnkExcludedStateDelete
POLDTLAR05TxaExcludedState
POLDTLAR05TxaExcludedStateDesc
POLDTLAR06CbxAdverseAccount
POLDTLAR06CbxDoNotRenew
POLDTLAR06LblCXNoticeDays
POLDTLAR06LblCXNoticeDays_copy
POLDTLAR06TxbCXAddMailDays
POLDTLAR06TxbCXNoticeDays
POLDTLAR07CbxExcludeMedicalCoverage
POLDTLAR08CbxAuditCompanybyDivisions
POLDTLAR08CbxAuditCompanyAccumulatePremium
POLDTLAR08CbxContestedAudit
POLDTLAR08OutputField1
POLDTLAR09DrpFlexDetail1
POLDTLAR09DrpFlexDetail2
POLDTLAR09DrpFlexDetail3
POLDTLAR09DrpFlexDetail4
POLDTLAR09DrpFlexDetail5
POLDTLAR09ImgFlex1
POLDTLAR09ImgFlex2
POLDTLAR09ImgFlex3
POLDTLAR09ImgFlex4
POLDTLAR09ImgFlex5
POLDTLAR09LblFlexHdg1
POLDTLAR09LblFlexHdg2
POLDTLAR09LblFlexHdg3
POLDTLAR09LblFlexHdg4
POLDTLAR09LblFlexHdg5
POLDTLAR10BtnAddCancel
POLDTLAR10BtnAddSave
POLDTLAR10LblCarrierBranchOffice
POLDTLAR10LblCarrierClientID
POLDTLAR10LblCarrierCode
POLDTLAR10LblCarrierContract#
POLDTLAR10LblCarrierEndtDate
POLDTLAR10LblCarrierIssueDate
POLDTLAR10LblCarrierPol#
POLDTLAR10LblCarrierStartDate
POLDTLAR10LblCarrierTPAID
POLDTLAR10TxbCarrierBranchOffice
POLDTLAR10TxbCarrierClientID
POLDTLAR10TxbCarrierCode
POLDTLAR10TxbCarrierPol#
POLDTLAR10TxbCarrierTPAID
POLDTLAR10TxbIssueDate
POLDTLAR10TxbStartDate
POLDTLAR11LnkAgtEdit
POLDTLAR12BtnAddAgency
POLDTLAR13LnkPayrollEdit
POLDTLAR14Button2
POLDTLAR15BtnPayrollAdd
POLDTLAR16Button7
POLDTLAR17Hyperlink2
POLDTLAR18Hyperlink3
POLDTLAR19Button1
POLDTLAR20Hyperlink1
POLLOCAR01LnkSEdit
POLLOCAR02LnkSExpire
POLLOCAR03BtnLocAdd
POLLOCAR04ChkRepToNCCI
POLLOCAR06LnkOEdit
POLLOCAR07LnkOExpire
POLLOCAR08LnkOIE
POLLOCAR09BtnOAdd
POLLOCAR10lnkDeptEdit
POLLOCAR11BtnDeptAdd
POLLOCAR12lnkANEdit
POLLOCAR13BtnANadd
POLLOCAR14ChkStateRep
POLLOCAR15btnLossControl
POLLOCAR15Image1_copy11
POLLOCAR15OutputField10
POLLOCAR15OutputField10_copy
POLLOCAR15OutputField10_copy2
POLLOCAR15OutputField11
POLLOCAR15OutputField12
POLLOCAR15OutputField13
POLLOCAR15OutputField2
POLLOCAR15OutputField3
POLLOCAR15OutputField30
POLLOCAR15OutputField30_copy2
POLLOCAR15OutputField4
POLLOCAR15OutputField5
POLLOCAR15OutputField7
POLLOCAR15OutputField7_copy
POLLOCAR15OutputField7_copy2
POLLOCAR15OutputField8_copy
POLLOCAR15OutputField8_copy2
POLLOCAR15OutputField9
POLLOCAR15OutputField9_copy
POLLOCAR15OutputField9_copy2
POLLOCAR15RadioButton1
POLLOCAR15RadioButton2
POLLOCAR15SelCounty
POLLOCAR15SlbBState
POLLOCAR15SlbMState
POLLOCAR15SlbPState
POLLOCAR15TxbBAdd1
POLLOCAR15TxbBAdd2
POLLOCAR15TxbBCity
POLLOCAR15TxbBContact
POLLOCAR15TxbBZip
POLLOCAR15TxbDBANam
POLLOCAR15TxbEmpWeb
POLLOCAR15TxbFax#
POLLOCAR15TxbFebNam
POLLOCAR15TxbMAdd1
POLLOCAR15TxbMAdd2
POLLOCAR15TxbMCity
POLLOCAR15TxbMZip
POLLOCAR15TxbOutAdd
POLLOCAR15TxbPhn#
POLLOCAR15TxbPAdd1
POLLOCAR15TxbPAdd2
POLLOCAR15TxbPCity
POLLOCAR15TxbPZip
POLLOCAR15TxbStUnEmp_copy
POLLOCAR14TxbTypOfBuilding
POLLOCAR15TxbTypOfPolicy
POLLOCAR16ChkAutoCash
POLLOCAR16ChkDCREmpExp
POLLOCAR16ChkSplAcc
POLLOCAR16ChkVIP
POLLOCAR16ChkWatchList
POLLOCAR16OutputField14
POLLOCAR16OutputField15
POLLOCAR16OutputField16
POLLOCAR16OutputField17
POLLOCAR16OutputField18
POLLOCAR16OutputField19
POLLOCAR16OutputField20
POLLOCAR16OutputField21
POLLOCAR16OutputField22
POLLOCAR16OutputField23
POLLOCAR16OutputField24
POLLOCAR16OutputField25
POLLOCAR16OutputField26
POLLOCAR16OutputField27
POLLOCAR16OutputField28
POLLOCAR16OutputField29
POLLOCAR16OutputField30_copy2
POLLOCAR16SelChapter
POLLOCAR16SelExpRating
POLLOCAR16SelRegion
POLLOCAR16SelServOff
POLLOCAR16SelTypBus
POLLOCAR16TxbCustId
POLLOCAR16TxbEmpLnk
POLLOCAR16TxbEmpLsId
POLLOCAR16TxbNatBus
POLLOCAR16TxbNAICS
POLLOCAR16TxbNRisk
POLLOCAR16TxbShiftEmp
POLLOCAR16TxbStUnEmp
POLLOCAR16TxbStUnEmp_copy
POLLOCAR16TxbSIC
POLLOCAR16TxbSRisk
POLLOCAR16TxbTaxId
POLLOCAR17OutputField32
POLLOCAR17OutputField33
POLLOCAR17OutputField34
POLLOCAR17OutputField35
POLLOCAR17OutputField36
POLLOCAR17OutputField37
POLLOCAR17OutputField38
POLLOCAR17TxaComments
POLLOCAR17TxbStfAdtMgr
POLLOCAR17TxbStfAdtRvw
POLLOCAR17TxbStfLosRep
POLLOCAR17TxbStfPolSrv
POLLOCAR17TxbStfPrmAcc
POLLOCAR17TxbStfUndWtr
POLPOPAR01DrpPayDetailStatus_copy
POLPOPAR01LblPayDetailStatus_copy
POLPOPAR02BtnPayEditSaveAdd
POLQTEAR01BtnScr1ModifyDates
POLQTEAR02LnkDiv1DivEdit
POLQTEAR03LnkDiv1DivDelete
POLQTEAR04BtnDiv1AddDiv
POLSUMAR01Hyperlink3
POLSUMAR02Hyperlink1
POLSUMAR03 OutputField59
POLSUMAR03sflCurPyrl
POLSUMAR03DtlOutAuditPayroll
POLSUMAR03DtlOutCancelDt
POLSUMAR03DtlOutClassCode
POLSUMAR03DtlOutEffDate
POLSUMAR03DtlOutEstPayroll
POLSUMAR03DtlOutExtension
POLSUMAR03DtlOutEPDiv
POLSUMAR03DtlOutPayHZGR
POLSUMAR03DtlOutRate
POLSUMAR03DtlOutRepPayroll
POLSUMAR03DtlOutSpec
POLSUMAR03DtlOutStatus
POLSUMAR03Hyperlink3
POLSUMAR03OutputField116
POLSUMAR03OutputField116_copy
POLSUMAR03OutputField118
POLSUMAR03OutputField120
POLSUMAR03OutputField123
POLSUMAR03OutputField127
POLSUMAR03OutputField139
POLSUMAR03OutputField141
POLSUMAR03OutputField175
POLSUMAR03OutputField177
POLSUMAR03OutputField179
POLSUMAR03OutputField193
POLSUMAR03OutputField197
POLSUMAR03OutputField201
POLSUMAR03OutputField207
POLSUMAR03OutputField212
POLSUMAR03OutputField232
POLSUMAR03OutputField236
POLSUMAR03OutputField240
POLSUMAR03OutputField60
POLSUMAR03OutputField62
POLSUMAR03OutputField63
POLSUMAR03OutputField64
POLSUMAR03OutputField66
POLSUMAR03OutputField90
POLSUMAR03OutputField91
POLSUMAR03OutputField95
POLSUMAR03OutputField97
POLSUMAR03OutAud#Employees
POLSUMAR03OutAudManPremium
POLSUMAR03OutEst#Employees
POLSUMAR03OutEstManPremium
POLSUMAR03OutPayrollStatus
POLSUMAR03OutPayDetailAddlLoad
POLSUMAR04OutputField245
POLSUMAR04OutputField246
POLSUMAR04OutBrokerFee
POLSUMAR04OutBrokerFeeD
POLSUMAR04OutMemberFee
POLSUMAR04Out1stAssmnt
POLSUMAR04Out1stAssmntD
POLSUMAR04Out2ndAssmnt
POLSUMAR04Out2ndAssmntD
POLSUMAR04Out3rdAssmnt
POLSUMAR04Out3rdAssmntD
POLSUMAR04Out4thAssmnt
POLSUMAR04Out4thAssmntD
POLSUMAR04Out5thAssmnt
POLSUMAR04Out5thAssmntD
POLSUMAR04Out6thAssmnt
POLSUMAR04Out6thAssmntD
POLSUMAR04Out7thAssmnt
POLSUMAR04Out7thAssmntD
POLSUMAR05sflPrmHist
POLSUMAR05OutdecTA01
POLSUMAR05OutdecTA02
POLSUMAR05OutdecTA03
POLSUMAR05OutdecTA04
POLSUMAR05OutdecTA05
POLSUMAR05OutdecTA06
POLSUMAR05OutdecTA07
POLSUMAR05OutdecTA08
POLSUMAR05OutdecTA09
POLSUMAR05OutdecTA10
POLSUMAR05OutdecTA11
POLSUMAR05OutdecTA12
POLSUMAR05OutdecTA13
POLSUMAR05OutdecTA14
POLSUMAR05OutdecTA15
POLSUMAR05OutdecTA16
POLSUMAR05OutdecTA17
POLSUMAR05OutdecTA18
POLSUMAR05OutdecTA19
POLSUMAR05OutdecTA20
POLSUMAR05OutdecTA21
POLSUMAR05OutdecTA22
POLSUMAR05OutdecTA23
POLSUMAR05OutdecTA24
POLSUMAR05OutdecTA25
POLSUMAR05OutdecTA26
POLSUMAR05OutdecTA27
POLSUMAR05OutdecTA28
POLSUMAR05OutdecTA29
POLSUMAR05OutdecTA30
POLSUMAR05OutdecTA31
POLSUMAR05OutdecTA32
POLSUMAR05OutdecTA33
POLSUMAR05OutdecTA34
POLSUMAR05OutdecTA35
POLSUMAR05OutdecTA36
POLSUMAR05OutdecTA37
POLSUMAR05OutdecTA38
POLSUMAR05OutdecTA39
POLSUMAR05OutdecTA40
POLSUMAR05OutdecTA41
POLSUMAR05OutdecTA42
POLSUMAR05OutdecTA43
POLSUMAR05OutdecTA44
POLSUMAR05OutdecTA45
POLSUMAR05OutdecTA46
POLSUMAR05OutdecTA47
POLSUMAR05OutdecTA48
POLSUMAR05OutdecTA49
POLSUMAR05OutdecTA50
POLSUMAR05OutdecTI01
POLSUMAR05OutdecTI02
POLSUMAR05OutdecTI03
POLSUMAR05OutdecTI04
POLSUMAR05OutdecTI05
POLSUMAR05OutdecTI06
POLSUMAR05OutdecTI07
POLSUMAR05OutdecTI08
POLSUMAR05OutdecTI09
POLSUMAR05OutdecTI10
POLSUMAR05OutdecTI11
POLSUMAR05OutdecTI12
POLSUMAR05OutdecTI13
POLSUMAR05OutdecTI14
POLSUMAR05OutdecTI15
POLSUMAR05OutdecTI16
POLSUMAR05OutdecTI17
POLSUMAR05OutdecTI18
POLSUMAR05OutdecTI19
POLSUMAR05OutdecTI20
POLSUMAR05OutdecTI21
POLSUMAR05OutdecTI22
POLSUMAR05OutdecTI23
POLSUMAR05OutdecTI24
POLSUMAR05OutdecTI25
POLSUMAR05OutdecTI26
POLSUMAR05OutdecTI27
POLSUMAR05OutdecTI28
POLSUMAR05OutdecTI29
POLSUMAR05OutdecTI30
POLSUMAR05OutdecTI31
POLSUMAR05OutdecTI32
POLSUMAR05OutdecTI33
POLSUMAR05OutdecTI34
POLSUMAR05OutdecTI35
POLSUMAR05OutdecTI36
POLSUMAR05OutdecTI37
POLSUMAR05OutdecTI38
POLSUMAR05OutdecTI39
POLSUMAR05OutdecTI40
POLSUMAR05OutdecTI41
POLSUMAR05OutdecTI42
POLSUMAR05OutdecTI43
POLSUMAR05OutdecTI44
POLSUMAR05OutdecTI45
POLSUMAR05OutdecTI46
POLSUMAR05OutdecTI47
POLSUMAR05OutdecTI48
POLSUMAR05OutdecTI49
POLSUMAR05OutdecTI50
POLSUMAR05Outfact01
POLSUMAR05Outfact02
POLSUMAR05Outfact03
POLSUMAR05Outfact04
POLSUMAR05Outfact05
POLSUMAR05Outfact06
POLSUMAR05Outfact07
POLSUMAR05Outfact08
POLSUMAR05Outfact09
POLSUMAR05Outfact10
POLSUMAR05Outfact11
POLSUMAR05Outfact12
POLSUMAR05Outfact13
POLSUMAR05Outfact14
POLSUMAR05Outfact15
POLSUMAR05Outfact16
POLSUMAR05Outfact17
POLSUMAR05Outfact18
POLSUMAR05Outfact19
POLSUMAR05Outfact20
POLSUMAR05Outfact21
POLSUMAR05Outfact22
POLSUMAR05Outfact23
POLSUMAR05Outfact24
POLSUMAR05Outfact25
POLSUMAR05Outfact26
POLSUMAR05Outfact27
POLSUMAR05Outfact28
POLSUMAR05Outfact29
POLSUMAR05Outfact30
POLSUMAR05Outfact31
POLSUMAR05Outfact32
POLSUMAR05Outfact33
POLSUMAR05Outfact34
POLSUMAR05Outfact35
POLSUMAR05Outfact36
POLSUMAR05Outfact37
POLSUMAR05Outfact38
POLSUMAR05Outfact39
POLSUMAR05Outfact40
POLSUMAR05Outfact41
POLSUMAR05Outfact42
POLSUMAR05Outfact43
POLSUMAR05Outfact44
POLSUMAR05Outfact45
POLSUMAR05Outfact46
POLSUMAR05Outfact47
POLSUMAR05Outfact48
POLSUMAR05Outfact49
POLSUMAR05Outfact50
POLSUMAR05Outhead01
POLSUMAR05Outhead02
POLSUMAR05Outhead03
POLSUMAR05Outhead04
POLSUMAR05Outhead05
POLSUMAR05Outhead06
POLSUMAR05Outhead07
POLSUMAR05Outhead08
POLSUMAR05Outhead09
POLSUMAR05Outhead10
POLSUMAR05Outhead11
POLSUMAR05Outhead12
POLSUMAR05Outhead13
POLSUMAR05Outhead14
POLSUMAR05Outhead15
POLSUMAR05Outhead16
POLSUMAR05Outhead17
POLSUMAR05Outhead18
POLSUMAR05Outhead19
POLSUMAR05Outhead20
POLSUMAR05Outhead21
POLSUMAR05Outhead22
POLSUMAR05Outhead23
POLSUMAR05Outhead24
POLSUMAR05Outhead25
POLSUMAR05Outhead26
POLSUMAR05Outhead27
POLSUMAR05Outhead28
POLSUMAR05Outhead29
POLSUMAR05Outhead30
POLSUMAR05Outhead31
POLSUMAR05Outhead32
POLSUMAR05Outhead33
POLSUMAR05Outhead34
POLSUMAR05Outhead35
POLSUMAR05Outhead36
POLSUMAR05Outhead37
POLSUMAR05Outhead38
POLSUMAR05Outhead39
POLSUMAR05Outhead40
POLSUMAR05Outhead41
POLSUMAR05Outhead42
POLSUMAR05Outhead43
POLSUMAR05Outhead44
POLSUMAR05Outhead45
POLSUMAR05Outhead46
POLSUMAR05Outhead47
POLSUMAR05Outhead48
POLSUMAR05Outhead49
POLSUMAR05Outhead50
POLSUMAR05OutputField76
POLSUMAR05OutputField78
POLSUMAR05OutputField80
POLSUM001 Hyperlink1
POLSUM001 OutputField181
POLSUM001 OutLDedAmt
POLSUM002 OutputField189_copy4
POLSUM002 OutputField189_copy5
POLSUM002 OutAunPrm
POLSUM002 OutExpCon
POLSUM003 OutputField187
POLSUM003 OutputField189
POLSUM003 OutRetMaxF
POLSUM003 OutRetMinF
CLMDTLAR01LblDepatment
CLMDTLAR01DrpDepartment
CLMSUMAR01Button2
CLMSUMAR01Button2
CLMDTLAR02LblClaimName
CLMDTLAR02LnkClaimEmail
CLMDTLAR02TxbLastName
CLMDTLAR02LblLast
CLMDTLAR02Image1
CLMDTLAR02TxbFirstName
CLMDTLAR02LblFirst
CLMDTLAR02Image1_copy
CLMDTLAR02TxbMidInit
CLMDTLAR02LblMid
CLMDTLAR02LblClaimantContact
CLMDTLAR02TxbClaimantAddress1
CLMDTLAR02LblAddress1
CLMDTLAR02Image1_copy2
CLMDTLAR02TxbClaimantAddress2_copy
CLMDTLAR02LblAddress2
CLMDTLAR02TxbClaimantCity
CLMDTLAR02DrpClaimantState
CLMDTLAR02TxbClaimantZip
CLMDTLAR02LblCity
CLMDTLAR02Image1_copy3
CLMDTLAR02DrpClaimantState
CLMDTLAR02TxbClaimantZip
CLMDTLAR02LblState
CLMDTLAR02LblZip
CLMDTLAR02Image1_copy4
CLMDTLAR02LblZip
CLMDTLAR02Image1_copy5
CLMDTLAR02LblOrText
CLMDTLAR02DrpCountry
CLMDTLAR02TxbClaimantAddress1_copy3
CLMDTLAR02TxbClaimantCounty
CLMDTLAR02LblCountry
CLMDTLAR02LblProvince
CLMDTLAR02LblClmCounty
CLMDTLAR02TxbClaimantPhone
CLMDTLAR02TxbClaimantWorkPhone_copy2
CLMDTLAR02TxbClaimEmail
CLMDTLAR02LblHomePhone#
CLMDTLAR02LblWorkPhone#
CLMDTLAR02LblEMail
CLMDTLAR03LblAccidentDate_copy2
CLMDTLAR03LblAccidentTime_copy2
CLMDTLAR03LblBirthDate
CLMDTLAR03Image1_copy142
CLMDTLAR03LblAge
CLMDTLAR03TxbInjuryDate_copy2
CLMDTLAR03TxbInjurytime
CLMDTLAR03TxbBirthDate
CLMDTLAR03TxbAge
CLMDTLAR03LblGender
CLMDTLAR03Image1_copy14
CLMDTLAR03TxbGender
CLMDTLAR03LblMaritalStatus
CLMDTLAR03DrpMarStatus
CLMDTLAR03LblDependents
CLMDTLAR03LblHeight
CLMDTLAR03LblWeight
CLMDTLAR03Txb#Dep
CLMDTLAR03TxtFeet
CLMDTLAR03LblFeet
CLMDTLAR03TxtInch
CLMDTLAR03LblInch
CLMDTLAR03LblWeight
CLMDTLAR03TxtWeight
CLMDTLAR03LblPound
CLMDTLAR04OutDevereux
CLMDTLAR04LblDevereuxCauseCode
CLMDTLAR04Image1_copy144
CLMDTLAR04DrpDevereuxCauseCode
CLMDTLAR04LblDevereuxRestraintType
CLMDTLAR04Image1_copy1442
CLMDTLAR04DrpDevereuxRestraintType
CLMDTLAR04LblDevereuxProgramNumber
CLMDTLAR04TxbDevereuxProgram
CLMDTLAR05DrpPayrollClass_copy
CLMDTLAR05OutSicCode
CLMDTLAR05TxbInsolventInsurerFein
CLMDTLAR05TxbEmpClm#_copy
CLMDTLAR05TxbSuperName_copy
CLMDTLAR05TxbCampus_code
CLMDTLAR05LblLocationInStore
CLMDTLAR05DrpLocationInStore
CLMDTLAR05LblClassCode_copy
CLMDTLAR05LblEmpSIC_copy
CLMDTLAR05LblEmpClm#_copy2
CLMDTLAR05LblEmpClm#_copy
CLMDTLAR05LblSuperName_copy
CLMDTLAR05LblCampus_code
CLMDTLAR05Image1_copy17
CLMDTLAR05TxbCampus_code
CLMDTLAR05LblLocationInStore
CLMDTLAR06LblAccidentDesc
CLMDTLAR06Image1_copy15
CLMDTLAR06TxbAccidentDesc1
CLMDTLAR06TxbAccidentDesc2
CLMDTLAR06CbxPremises
CLMDTLAR06LblLocationInfo
CLMDTLAR06TxbPlaceName
CLMDTLAR06LblPlace
CLMDTLAR06TxbPlaceAddress
CLMDTLAR06LblPlaceAddress
CLMDTLAR06TxbPlaceCity
CLMDTLAR06DrpPlaceState
CLMDTLAR06TxbPlaceCounty
CLMDTLAR06LblPlaceCity
CLMDTLAR06LblPlaceState
CLMDTLAR06LblPlaceZip
CLMDTLAR06LblPlcCounty
CLMDTLAR06LblLocationInput
CLMDTLAR06LblNarrative
CLMDTLAR06DrpLocation
CLMDTLAR06TxbNarrative
CLMDTLAR07OutEmploymentInfo
CLMDTLAR07LblJurState
CLMDTLAR07LblTPA#
CLMDTLAR07LblStateClaim#_copy
CLMDTLAR07DrpJurState
CLMDTLAR07DrpTPA#
CLMDTLAR07TxbStateClm#_copy
CLMDTLAR07LblOtherEmployerId
CLMDTLAR07TxbOtherEmployeeId
CLMDTLAR07LblIDTypeQualifier
CLMDTLAR07DrpIdTypeQualifier
CLMDTLAR07CbxReleased
CLMDTLAR07LblOccupationCode
CLMDTLAR07LblDescOccupation
CLMDTLAR07DrpOccCode
CLMDTLAR07LblDescOccupation
CLMDTLAR07TxbOccupationDesc
CLMDTLAR07LblDepatment
CLMDTLAR07DrpDepartment
CLMDTLAR07LblHireDate
CLMDTLAR07TxbHireDate
CLMDTLAR07OutTenure
CLMDTLAR07LblDateReported
CLMDTLAR07TxbDateReported
CLMDTLAR07LblCategory2
CLMDTLAR07TxbCatagory2
CLMDTLAR07LblEmpStatus
CLMDTLAR07DrpEmpStatus
CLMDTLAR07LblFroiReceivedDate
CLMDTLAR07TxbRecDate_copy
CLMDTLAR07LblLateReason
CLMDTLAR07DrpLateReason
CLMDTLAR07LblDaysPerWeek
CLMDTLAR07TxbDaysPerWeek
CLMDTLAR07LblHoursPerDay
CLMDTLAR07TxbClaimantAddress2_copy9
CLMDTLAR07LblShift
CLMDTLAR07DrpShift
CLMDTLAR07LblClaimantContactDate
CLMDTLAR07TxbConDate_copy
CLMDTLAR07LblMethodContact
CLMDTLAR07DrpContact
CLMDTLAR08OutClaimCodes
CLMDTLAR08LblLostTimeStatus_copy
CLMDTLAR08DrpLostTime_copy
CLMDTLAR08LblNature_copy
CLMDTLAR08TxbNature
CLMDTLAR08LblPart_copy
CLMDTLAR08TxbPart
CLMDTLAR08LblCause_copy
CLMDTLAR08TxbCause
CLMDTLAR08CbxPossLoss_copy
CLMDTLAR08CbxDaysLt
CLMDTLAR08CbxDaysMedical
CLMDTLAR08LblNCCICode_copy
CLMDTLAR08DrpNcciCode_copy
CLMDTLAR08LblLossCoverage_copy
CLMDTLAR08DrpCovCode_copy
CLMDTLAR08LbReserveCode_copy
CLMDTLAR08DrpRsvType_copy
CLMDTLAR08LblCarrierCode
CLMDTLAR08RadioBtnCarCode1
CLMDTLAR08RadioBtnCarCode2
CLMDTLAR08LblCoverageType_copy
CLMDTLAR08DrpTypeCov_copy
CLMDTLAR08LblLossRecovery_copy
CLMDTLAR08DrpRecType_copy
CLMDTLAR08LblActivityStatus_copy
CLMDTLAR08DrpActStat_copy
CLMDTLAR08LblPosition
CLMDTLAR08TxbPosition
CLMDTLAR08LblPoints
CLMDTLAR08TxbPoints
CLMDTLAR09OutWitnessInformation
CLMDTLAR09GrdWitness
CLMDTLAR10LnkEditWitness
CLMDTLAR11LnkDeleteWitness
CLMDTLAR12BtnAddWithness
CLMDTLAR13OutImrHistory
CLMDTLAR13GrdImrHistory
CLMDTLAR14OutStaffAssignments
CLMDTLAR14LblAdjuster
CLMDTLAR14TxbAdjuster
CLMDTLAR14LblOutsideAdjuster
CLMDTLAR14TxbOutsideAdjuster
CLMDTLAR14LblFundAdjuster
CLMDTLAR14TxbFundAdjuster
CLMDTLAR14LblSupervisor
CLMDTLAR14TxbSupervisor
CLMDTLAR14LblReinsuranceAdjuster
CLMDTLAR14TxbReinsuranceAdjuster
CLMDTLAR14LblCaseNurse
CLMDTLAR14TxbCaseNurse
CLMDTLAR14TxbCaseNurseSave
CLMDTLAR14LblAccountManager
CLMDTLAR14TxbAccountManager
CLMDTLAR14LblClaimNurse
CLMDTLAR14TxbClaimNurse
CLMDTLAR14LblClaimEnteredBy
CLMDTLAR14OutClaimEnteredBy
CLMDTLAR15OutClaimIndicators
CLMDTLAR15CbxCmsSub_copy
CLMDTLAR15CbxUSLH_copy
CLMDTLAR15CbxOSHA_copy
CLMDTLAR15CbxMedBen_copy
CLMDTLAR15CbxSeasonalEmployee
CLMDTLAR15CbxExcessReinsurance
CLMDTLAR15CbxSpecDis_copy
CLMDTLAR15CbxEmployerLiabilitty
CLMDTLAR15DrpSpcDisFund
CLMDTLAR15CbxControverted_copy
CLMDTLAR15CbxAcquired_copy
CLMDTLAR15LblSpcDisFund
CLMDTLAR16OutReinsurance
CLMDTLAR16CbxReinsuranceReportable
CLMDTLAR16LblCaseNurse_copy6
CLMDTLAR16OutSpecificRetention
CLMDTLAR16LblCaseNurse_copy7
CLMDTLAR16TxbReinsurerClaim
CLMDTLAR16LblCaseNurse_copy8
CLMDTLAR16OutCatastrophe#
CLMDTLAR17OutDenial
CLMDTLAR17CbxDenied
CLMDTLAR17LblFullDenialEffDate
CLMDTLAR17TxbFullDenialEffDate
CLMDTLAR17LblDenialReason
CLMDTLAR17TxbDenailReason1
CLMDTLAR17TxbDenailReason2
CLMDTLAR17TxbDenailReason3
CLMDTLAR17LblFullDenialRecDate
CLMDTLAR17TxbFullDenialRecDate
CLMDTLAR17LblFullDenialReasoncodes
CLMDTLAR17DrpFullDenailreasoncode1
CLMDTLAR17DrpFullDenailreasoncode2
CLMDTLAR17DrpFullDenailreasoncode5
CLMDTLAR17DrpFullDenailreasoncode3
CLMDTLAR17DrpFullDenailreasoncode4
CLMDTLAR17LblPartialDenialReasonCode
CLMDTLAR17DrpPartialDenailreasonCode
CLMDTLAR18OutSubrogation
CLMDTLAR18LblCaseNurse_copy9
CLMDTLAR18DrpSubrogated
CLMDTLAR18LblResponsibleParty
CLMDTLAR18TxbRespPartyName
CLMDTLAR18LblRespPartyName
CLMDTLAR18TxbRespPartyAddress1
CLMDTLAR18LblRespPartyAddress1
CLMDTLAR18TxbRespPartyAddress2
CLMDTLAR18LblRespPartyAddress2
CLMDTLAR18TxbRespPartyCity
CLMDTLAR18DrpRespPartyState
CLMDTLAR18TxbRespPartyZip
CLMDTLAR18TxbRespPartyPhone
CLMDTLAR18LblRespPartyCity
CLMDTLAR18LblRespPartyState
CLMDTLAR18LblRspPartyZip
CLMDTLAR18LblRespPartyPhone
CLMDTLAR18TxbRespPartyEmail
CLMDTLAR18LblRespPartyEmail
CLMDTLAR18LblStatuteLimitation
CLMDTLAR18TxbStatuteLimitation
CLMDTLAR18LblInsuranceCompany
CLMDTLAR18TxbInsCompanyName
CLMDTLAR18LblInsCompanyName
CLMDTLAR18TxbInsuranceCompanyAddress1
CLMDTLAR18LblInsuranceCompanyAddress1
CLMDTLAR18TxbInsuranceCompanyAddress2
CLMDTLAR18LbHInsuranceCompanyAddress2
CLMDTLAR18TxbInsuranceCompanyCity
CLMDTLAR18DrpInsuranceCompanyState
CLMDTLAR18TxbInsuranceCompanyZip
CLMDTLAR18TxbInsuranceCompanyPhone
CLMDTLAR18LblInsuranceCompanyCity
CLMDTLAR18LblInsuranceCompanyState
CLMDTLAR18LblInsuranceCompanyZip
CLMDTLAR18LblInsuranceCompanyPhone
CLMDTLAR18TxbInsuranceCompanyEmail
CLMDTLAR18LblInsuranceCompanyEmail
CLMDTLAR18LblPolicy#
CLMDTLAR18TxbPolicy#
CLMDTLAR18LblInsuranceCompanyAdjuster
CLMDTLAR18TxbInsuranceCompanyAdjuster
CLMDTLAR19OutStaffAssignments_copy
CLMDTLAR19LblTreatingPhys
CLMDTLAR19OutPhyFein
CLMDTLAR19LnkTreatingPhysEmail
CLMDTLAR19OutPhysName
CLMDTLAR19OutPhysAddress
CLMDTLAR19OutPhysCity
CLMDTLAR19DrpPhytState
CLMDTLAR19OutPhysZip
CLMDTLAR19OutPhysPhone
CLMDTLAR19OutPhysFax#
CLMDTLAR19LblPhysPhone#
CLMDTLAR19LblWorkPhone#_copy
CLMDTLAR19LblHospital
CLMDTLAR19TxbHosFein
CLMDTLAR19LblHosFein
CLMDTLAR19TxbHosName
CLMDTLAR19LblHosName
CLMDTLAR19TxbHosAddress
CLMDTLAR19LbHoslAddress
CLMDTLAR19TxbHosCity
CLMDTLAR19DrpHosState
CLMDTLAR19TxbHosZip
CLMDTLAR19TxbHosPhone
CLMDTLAR19LblHosCity
CLMDTLAR19LblHosState
CLMDTLAR19LblHosZip
CLMDTLAR19LblHosPhone#
CLMDTLAR20LblTreatingPhys_copy2
CLMDTLAR20DrpInitialTreatment
CLMDTLAR20LblHICN#
CLMDTLAR20TxbHicn#
CLMDTLAR20LblYearLastExposed
CLMDTLAR20TxbYearLastExposed
CLMDTLAR20LblSurgeryDate
CLMDTLAR20TxbSurgeryDate
CLMDTLAR20LblCmsDateIncident
CLMDTLAR20TxbCmsDate
CLMDTLAR20LblFinalMmiDate_copy
CLMDTLAR20TxbCmsAllegedCause
CLMDTLAR20LblDateRehab
CLMDTLAR20TxbRehabDate
CLMDTLAR20LblReleasedDate
CLMDTLAR20TxbReleaseDate
CLMDTLAR20LblDeathDate
CLMDTLAR20TxbDeathDate
CLMDTLAR20LblMMIDate
CLMDTLAR20TxbAnticipatedMmiDate
CLMDTLAR20LblFinalMmiDate
CLMDTLAR20TxbFinalMmiDate
CLMDTLAR20LblOrmDate
CLMDTLAR20TxbOrmDate
CLMDTLAR20LblYearLastExposed_copy
CLMDTLAR20TxbPctImpaired
CLMDTLAR20LblMCOName
CLMDTLAR20TxbMcoName
CLMDTLAR20LblMCOFein
CLMDTLAR20TxbMcoFein
CLMDTLAR20LblPhysChange
CLMDTLAR20TxbChgPhys
CLMDTLAR20LblPhysRequest
CLMDTLAR20TxbRequestPhys
CLMDTLAR20LblPhysDenial
CLMDTLAR20TxbDenyPhys
CLMDTLAR20CbxMinPayment
CLMDTLAR20CbMRAuthorization
CLMDTLAR20CbxHandicapClaim
CLMDTLAR20CbxDeathClaim
CLMDTLAR20CbxOrmIndicator
CLMDTLAR20CbxPreExisting
CLMDTLAR20CbxOCCDisease
CLMDTLAR20CbxImpDetermined
CLMDTLAR20CbxRxEl
CLMDTLAR20LblInjurySeverityCode                                                          09/28/17
CLMDTLAR20DrpInjurySeverityCode                                                          09/28/17
CLMDTLAR20LblMCOName_copy                                                                09/28/17
CLMDTLAR20DrpMcoCode                                                                     09/28/17
CLMDTLAR21OutTreatingPhys
CLMDTLAR21GrdTreatingPhysician
CLMDTLAR21LblTreatFeinModOut
CLMDTLAR21OutPhysFeinO
CLMDTLAR21OutPhysModO
CLMDTLAR21LblTreatNameOut
CLMDTLAR21LnkTreatingPhysEmailO
CLMDTLAR21OutPhysNameO
CLMDTLAR21LblPhysRoleOut
CLMDTLAR21OutPhysRoleO
CLMDTLAR21OutputField1
CLMDTLAR21OutPhysAdrO
CLMDTLAR21OutPhysCityO
CLMDTLAR21OutPhysStateO
CLMDTLAR21OutPhysZipO
CLMDTLAR21OutPhysPhoneO
CLMDTLAR21OutPhysFaxO
CLMDTLAR21LblPhysPhone#_copy
CLMDTLAR21LblWorkPhone#_copy2
CLMDTLAR21LblClinicNameOut
CLMDTLAR21OutClinicNameO
CLMDTLAR21LblTreatServiceDtOut
CLMDTLAR21OutPhysDtRangeO
CLMDTLAR21CbxPhysEndDateO
CLMDTLAR22LnkPhysEdit
CLMDTLAR23LnkPhysDelete
CLMDTLAR24BtnAddPhys
CLMDTLAR25OutImpairmentPart
CLMDTLAR25LblBodyPart
CLMDTLAR25OutBodyPart
CLMDTLAR25LblPermImpairment
CLMDTLAR25SflParts
CLMDTLAR26LnkPartEdit
CLMDTLAR27BtnAddPart
CLMDTLAR28OutDiagnosis
CLMDTLAR28GrdDiagnosis
CLMDTLAR28LblDiagOut
CLMDTLAR28OutDiagOut
CLMDTLAR28OutTypeOut
CLMDTLAR28CbxPrimaryOut
CLMDTLAR28LblStatusO
CLMDTLAR28OutStatusOut
CLMDTLAR28LblDescO
CLMDTLAR28OutDescOut
CLMDTLAR28LblEffectiveO
CLMDTLAR28OutEffDateOut
CLMDTLAR28LblExpirationO
CLMDTLAR28OutExpDateOut
CLMDTLAR28LblLocationO
CLMDTLAR28OutLocationOut
CLMDTLAR28LblSiteO
CLMDTLAR28OutSiteOut
CLMDTLAR29LnkDiagEdit
CLMDTLAR30LnkDiagDelete
CLMDTLAR31BtnAddDiagnosis
CLMDTLAR32OutContinuedSalaryInfo
CLMDTLAR32CbxFullSalaryContinued
CLMDTLAR32CbxEmpAgreesCompensation
CLMDTLAR32CbxFullSalaryVoucher
CLMDTLAR32LblAgreementCompCode
CLMDTLAR32DrpAgreementCompCode
CLMDTLAR32LblPaidThruDate
CLMDTLAR32TxbPaidThruDate
CLMDTLAR32LblAmountPaid
CLMDTLAR32TxbAmountPaid
CLMDTLAR32LblWeek/days Paid
CLMDTLAR32TxbWeekPaid
CLMDTLAR32LblSlash1
CLMDTLAR32TxbDaysPaid
CLMDTLAR33OutWorkInfo
CLMDTLAR33LblDarteLastWorked
CLMDTLAR33TxbDarteLastWorked
CLMDTLAR33LblCurrentLastDateWorked
CLMDTLAR33TxbCurrentLastDateWorked
CLMDTLAR33LblDisabilityDate
CLMDTLAR33TxbDisabilityDate
CLMDTLAR33LblDisabilityNoticeDate
CLMDTLAR33TxbDisabilityNoticeDate
CLMDTLAR33LblCurrentDateDisability
CLMDTLAR33TxbCurrentDateDisability
CLMDTLAR33LblInitialDateLT
CLMDTLAR33TxbInitialDateLT
CLMDTLAR33LblDateNotified
CLMDTLAR33TxbDateNotified
CLMDTLAR33CbxPaidOnInjuryDate
CLMDTLAR34OutWorkHistory
CLMDTLAR34GrdWorkHistoryC
CLMDTLAR34LblEstimatedRTW
CLMDTLAR34TxbEstimatedRTW
CLMDTLAR34LblReleaseToWorkDate
CLMDTLAR34TxbReleaseToWorkDate
CLMDTLAR34LblActualRTW
CLMDTLAR34TxbActualRTW
CLMDTLAR34LblRtwQualifier
CLMDTLAR34DrpRtwQualifier
CLMDTLAR34CbxRtwSameEmployer
CLMDTLAR34LblTaxFilingStatus
CLMDTLAR34DrpTaxFilingStatus
CLMDTLAR34LblRtwStatus
CLMDTLAR34DrpRtwStatus
CLMDTLAR34LblNonConsecPeriodcode
CLMDTLAR34DrpNonConsecPeriodcode
CLMDTLAR34LblNumOfExemptions
CLMDTLAR34TxbNumOfExemptions
CLMDTLAR34LblNonConsecPeriodcode_copy
CLMDTLAR34TxaSuspensionNarrative
CLMDTLAR34LblSuspensionEffectiveDate
CLMDTLAR34TxbSuspensionEffectiveDate
CLMDTLAR34LblTotalAccumulated
CLMDTLAR34OutTotalAccumulated
CLMDTLAR34LblCompdaysPaid
CLMDTLAR34OutCompdaysPaid
CLMDTLAR35LnkEditWorkHistory_copy2
CLMDTLAR36LnkEditWorkHistory_copy4
CLMDTLAR37BtnAddWorkHistory
CLMDTLAR38OutAverageWeeklyWage
CLMDTLAR38GrdAverageWeeklyWage
CLMDTLAR38CbxUnemployment
CLMDTLAR38CbxSocialSecurity
CLMDTLAR38CbxSpecialDisability
CLMDTLAR38CbxPension
CLMDTLAR38CbxOther
CLMDTLAR38LblMethofPreInjuryWage
CLMDTLAR38DrpMethofPreInjuryWage
CLMDTLAR38LblWagePeriod
CLMDTLAR38DrpWagePeriod
CLMDTLAR38LblOtherWeeklyPmts
CLMDTLAR38TxbOtherWeeklyPmts
CLMDTLAR38LblReducedBenAmtcode
CLMDTLAR38DrpReducedBenAmtcode
CLMDTLAR38LblDiscontinuedFringeBenefits
CLMDTLAR38TxbDiscontinuedFringeBenefits
CLMDTLAR38LblWagesPreInjury
CLMDTLAR38TxbWagesPreInjury
CLMDTLAR38LblWagePostInjury
CLMDTLAR38TxbWagePostInjury
CLMDTLAR39LnkEditWorkHistory_copy
CLMDTLAR40BtnAddWageInformation
CLMDTLAR41OutBenefitAdjustment
CLMDTLAR41GrdBenefitAdjustment
CLMDTLAR42LnkEditWorkHistory_copy3
CLMDTLAR43LnkDeleteBenefitAdjustments
CLMDTLAR44BtnAddWageInformation_copy
CLMDTLAR45OutConcurrentEmployer
CLMDTLAR45LblConcurEmpName
CLMDTLAR45TxbConEmpNam1
CLMDTLAR45TxbConEmpNam2
CLMDTLAR45LblConcurEmpPhone
CLMDTLAR45TxbConEmpPhone1
CLMDTLAR45TxbConEmpPhone2
CLMDTLAR45LblConcurEmpWage
CLMDTLAR45TxbConcurEmpWage1
CLMDTLAR45TxbConcurEmpWage2
CLMDTLAR46OutEmployerPaidInfo
CLMDTLAR46GrdEmployerPaid
CLMDTLAR47LnkEmployerPaidEdit
CLMDTLAR48LnkEmployerPaidDelete
CLMDTLAR49BtnAddEmployerPaid
CLMDTLAR50OutReducedEarnings
CLMDTLAR50GrdReducedEarnings
CLMDTLAR51LnkEditReducedEarnings
CLMDTLAR52LnkDeleteReducedEarnings
CLMDTLAR53BtnAddReducedEarnings
CLMDTLAR54OutLegalInfo_copy
CLMDTLAR54LblDefeneAttorney
CLMDTLAR54TxbDefAttFeinj
CLMDTLAR54LblOrText6
CLMDTLAR54TxbDefAttName
CLMDTLAR54LblDefAttFein
CLMDTLAR54LblDefAttName
CLMDTLAR54TxbDefAttAssignedDate
CLMDTLAR54LblDefAttAssignDate
CLMDTLAR54TxbDefAddress1
CLMDTLAR54LbDefAttAddress1
CLMDTLAR54TxbDefAddress2
CLMDTLAR54LbDefAttAddress2
CLMDTLAR54TxbDefAttCity
CLMDTLAR54LblDefCity
CLMDTLAR54DrpDefAttState
CLMDTLAR54LblDefState
CLMDTLAR54TxbDefAttZip
CLMDTLAR54LblDefZip
CLMDTLAR54TxbDefAttPhone
CLMDTLAR54LblDefPhone#
CLMDTLAR54TxbDefEmail
CLMDTLAR54LbDefEmail
CLMDTLAR55LblPlaintiffAttorney
CLMDTLAR55TxbPlaAttFeinj
CLMDTLAR55LblOrText7
CLMDTLAR55TxbPlaAttName
CLMDTLAR55LblPlaAttFein
CLMDTLAR55LblPlaAttName
CLMDTLAR55TxbPlaRepName
CLMDTLAR55LblPlaRepName
CLMDTLAR55TxbPlaRepDate
CLMDTLAR55DrpPlaRepIndicator
CLMDTLAR55LblDefAttAssignDate_copy
CLMDTLAR55LblPlaRepIndicator
CLMDTLAR55TxbPlaAddress1
CLMDTLAR55LbPlaAttAddress1
CLMDTLAR55TxbPlaAddress2
CLMDTLAR55LbPlaAttAddress2
CLMDTLAR55TxbPlaAttCity
CLMDTLAR55DrpPlaAttState
CLMDTLAR55TxbPlaAttZip
CLMDTLAR55TxbPlaAttPhone
CLMDTLAR55LblPlaCity
CLMDTLAR55LblPlaState
CLMDTLAR55LblPlaZip
CLMDTLAR55LblDefPhone#_copy
CLMDTLAR55TxbPlaEmail
CLMDTLAR55LbPlaEmail
CLMDTLAR56OutTrialInfo
CLMDTLAR56GrdTrialInformation
CLMDTLAR57LnkEditTrialInfo
CLMDTLAR58BtnAddTrialInfo
CLMDTLAR59OutMultipleSettlement
CLMDTLAR59GrdMultiple
CLMDTLAR60LnkEditSettlement
CLMDTLAR61BtnAddSettlement
CLMDTLAR62LblOriginalSuitDate
CLMDTLAR62TxbOriginalSuitDate
CLMDTLAR62LblFundingDelayed
CLMDTLAR62TxbFundDelayed
CLMDTLAR62LblMethodOfSettlement
CLMDTLAR62DrpMedthodOfSettlement
CLMDTLAR62LblMethodOfSettlement_copy
CLMDTLAR62DrpFraudulentIndicator
CLMDTLAR62CbxLumpSum
CLMDTLAR62CbxLitigation
CLMDTLAR62LblSettlementDate
CLMDTLAR62TxbSettlementDate
CLMDTLAR62LblSettlementAmount
CLMDTLAR62TxbSettlementAmount
CLMDTLAR62LblDateMailed
CLMDTLAR62TxbSettlementIndemnityDate
CLMDTLAR62TxbSettlementMedicalDate
CLMDTLAR62OutputField1
CLMDTLAR62OutputField2
CLMDTLAR63OutAccidentInfoA1
CLMDTLAR63LblAllEquipmentA1
CLMDTLAR63TxbEquipment1A1
CLMDTLAR63TxbEquipment2A1
CLMDTLAR63LblSpecificActivityA1
CLMDTLAR63TxbActivity1A1
CLMDTLAR63TxbActivity2A1
CLMDTLAR63LblWorkProcessA1
CLMDTLAR63TxbProcess1A1
CLMDTLAR63TxbProcess2A1
CLMDTLAR63LblHowInjuryOccurredA1
CLMDTLAR63LblDescribeEventsA1
CLMDTLAR63TxbEvents1A1
CLMDTLAR63TxbEvents2A1
CLMDTLAR63CbxSafetyEquipmentA1
CLMDTLAR63CbxSafetyUsedA1
CLMDTLAR63LblWitnessNamePhoneA1
CLMDTLAR63TxbWitnesses1A1
CLMDTLAR63TxbWitnesses2A1
CLMDTLAR63LblStateOfHireA1
CLMDTLAR63TxbHireStateA1
CLMDTLAR63LblTimeBeganWorkA1
CLMDTLAR63TxbTimeBeganWorkA1
CLMDTLAR63OutputField1
CLMDTLAR63OutputField2
CLMDTLAR64OutMiscelaneousInfoA1
CLMDTLAR64LblPreparerNameA1
CLMDTLAR64TxbPreparerA1
CLMDTLAR64LblPreparerTitleA1
CLMDTLAR64TxbPreparerTitleA1
CLMDTLAR64LblPreparerPhone#A1
CLMDTLAR64TxbPreparerPhoneA1
CLMDTLAR65OutEmployeeInfoAL
CLMDTLAR65LblEmployerIdAL
CLMDTLAR65TxbEmployerIdAL
CLMDTLAR65LblSpecificProductsAL
CLMDTLAR65TxbSpecificProdcutAL
CLMDTLAR65LblWorkCompProvidedByAL
CLMDTLAR65TxbWorkCompProvidedByAL
CLMDTLAR65LblCarrierSelfInsurance#AL
CLMDTLAR65TxbCarrierSelfInsuranceAL
CLMDTLAR65LblClaimantAgeAL
CLMDTLAR65TxbClaimantAgeAL
CLMDTLAR66OutWageInformationAL
CLMDTLAR66LblRtwOccupationAL
CLMDTLAR66TxbRtwOccupationAL
CLMDTLAR66LblLengthOfWmploymentAL
CLMDTLAR66LblYearsLengthEmploymentAL
CLMDTLAR66TxbYearLengthEmploymentAL
CLMDTLAR66LblMonthsLengthEmploymentAL_copy
CLMDTLAR66TxbMonthrLengthEmploymentAL_copy
CLMDTLAR66LblTimePresentJobAL
CLMDTLAR66LblYearsTimePresentAL
CLMDTLAR66TxbYearTimePresentJobAL
CLMDTLAR66LblMonthsTimePresentAL_copy
CLMDTLAR66TxbMonthTimePresentJobAL_copy
CLMDTLAR66LblWeeklyValueAL
CLMDTLAR66TxbOtherWagesAL
CLMDTLAR67OutInjuryIllness
CLMDTLAR67LblInjuryCauseAL
CLMDTLAR67TxbCause1AL
CLMDTLAR67TxbCause2AL
CLMDTLAR67LblHowAccidentOccurredAL
CLMDTLAR67TxbAccidentOccur1AL
CLMDTLAR67TxbAccidentOccur2AL
CLMDTLAR67LblHowAccidentOccurred2AL
CLMDTLAR67TxbDescriveEvents1AL
CLMDTLAR67TxbDescriveEvents2AL
CLMDTLAR68OutEmployerInfoCA
CLMDTLAR68LblStateUnemploymentCA
CLMDTLAR68TxbUnemploymentInsuranceAccount#CA
CLMDTLAR68LblTypeOfEmployerCa
CLMDTLAR68TxbTypeOfEmployerCA
CLMDTLAR69OutInjuryIllnessInfoCA
CLMDTLAR69LblDateEmployeeProvidedFormCa
CLMDTLAR69TxbDateFormProvidedCA
CLMDTLAR69LblTimeEmployeeBeganWorkCA
CLMDTLAR69TxbTimeBeganWorkCA
CLMDTLAR69LblGrossWagesCA
CLMDTLAR69TxbGrossWagesCA
CLMDTLAR69LblPerCA
CLMDTLAR69TxbWagesPerCA
CLMDTLAR69LblEquipmentCA
CLMDTLAR69TxbEquipment1CA
CLMDTLAR69TxbEquipment2CA
CLMDTLAR69LblActivityCA
CLMDTLAR69TxbActivity1CA
CLMDTLAR69TxbActivity2CA
CLMDTLAR69LblCountyCA
CLMDTLAR69TxbCountyCa
CLMDTLAR69LblDepartmentCA
CLMDTLAR69TxbExposureDepartmentCA
CLMDTLAR69CbxHospitalizedCA
CLMDTLAR69CbxEmergencyRoomCA
CLMDTLAR70OutEmployeeDE
CLMDTLAR70LblCountyDE
CLMDTLAR70TxbCountyDE
CLMDTLAR70LblAgeDE
CLMDTLAR70TxbClaimantAgeDE
CLMDTLAR70LblDepartmentDE
CLMDTLAR70TxbDepartmentEmployedDE
CLMDTLAR70LblHowLongEmployedDE
CLMDTLAR70TxbHowLongEmployedDE
CLMDTLAR71OutInjuryDiseaseDE
CLMDTLAR71LblInjuryDepartmentDE
CLMDTLAR71TxbInuryDepartmentDE
CLMDTLAR71LblWorkProcessInvolvedDE
CLMDTLAR71TxbWorkProcess1DE
CLMDTLAR71TxbWorkProcess2DE
CLMDTLAR72OutEmployerDE
CLMDTLAR72LblReportMakerDE
CLMDTLAR72TxbPersonMakingReportDE
CLMDTLAR72LblPositionDE
CLMDTLAR72TxbPositionDE
CLMDTLAR73OutOccurrenceDE
CLMDTLAR73LblListEquipmentDE
CLMDTLAR73TxbEquipment1DE
CLMDTLAR73TxbEquipment2DE
CLMDTLAR73LblActivityDe
CLMDTLAR73TxbActivity1DE
CLMDTLAR73TxbActivity2DE
CLMDTLAR74OutDatesDE
CLMDTLAR74LblDateOfReportDE
CLMDTLAR74TxbReportDateDE
CLMDTLAR74LblStartTimeDE
CLMDTLAR74TxbStartTimeDE
CLMDTLAR74CbxSameWageDE
CLMDTLAR74LblLastFullDayDE
CLMDTLAR74TxbLastFullDayPaidDateDE
CLMDTLAR75OutClaimInfoFL
CLMDTLAR75CbxDeniedFL
CLMDTLAR75CbxIndemnityFL
CLMDTLAR75CbxMedicalOnlyFL
CLMDTLAR75CbxLostTimeFL
CLMDTLAR75LblEighthDayFL
CLMDTLAR75Txb8thDayDisabilityFL
CLMDTLAR75LblEntityKnowledgeFL
CLMDTLAR75TxbKnowledgeOf8thFL
CLMDTLAR75LblDateFirstPaymentFL
CLMDTLAR75TxbDateMailedFL
CLMDTLAR75LblDisabilityTypeFL
CLMDTLAR75TxbDisabilityTypeFL
CLMDTLAR75LblPenaltyFL
CLMDTLAR75LblInterestFL
CLMDTLAR75TxbPenaltyAmountFL
CLMDTLAR75TxbInterestAmountFL
CLMDTLAR75LblRemarksFL
CLMDTLAR75TxbRemark1FL
CLMDTLAR75TxbRemark2FL
CLMDTLAR75LblRateofPayFL
CLMDTLAR75TxbRateofPayFL
CLMDTLAR75LblPerFL
CLMDTLAR75TxbPerFL
CLMDTLAR75LblHoursPerWkFL
CLMDTLAR75TxbHoursPerWkFL
CLMDTLAR76OutClaimInfoGA
CLMDTLAR76LblEmployeeEmailGA
CLMDTLAR76TxbEmpEmailGA
CLMDTLAR76LblClaimsOfficeEmailGA
CLMDTLAR76TxbClaimEmailGA
CLMDTLAR76LblSbwcId#GA
CLMDTLAR76TxbSbwcId#GA
CLMDTLAR76LblFilingEmailGA
CLMDTLAR76TxbPersEmailGA
CLMDTLAR76LblDaysOffGA
CLMDTLAR76TxbDaysOffGA
CLMDTLAR77OutBenefitInfoGA
CLMDTLAR77LblWeeklyBenefitAmountGA
CLMDTLAR77TxbWeeklyBenefitAmountGA
CLMDTLAR77LblBenefitsPayableGA
CLMDTLAR77TxbPayableFromGA
CLMDTLAR77TxbTtTpPpGA
CLMDTLAR77TxbPercentageGA
CLMDTLAR77OutputField1
CLMDTLAR77TxbBodyPartGA
CLMDTLAR77LblPctBodyPartGA
CLMDTLAR77OutputField2
CLMDTLAR77LblForGA
CLMDTLAR77TxbNumberOfWeeksGA
CLMDTLAR77OutputField3
CLMDTLAR77LblWeeksUntilGA
CLMDTLAR77TxbUntilDateGA
CLMDTLAR77OutputField4
CLMDTLAR78OutConrovertReasonGA
CLMDTLAR78LblBenefitsNotPaidGA
CLMDTLAR78TxbBenefitsGA
CLMDTLAR79OutClaimStatusInfoLA
CLMDTLAR79LblReportDateLA
CLMDTLAR79TxbReportDateLA
CLMDTLAR79LblBackToWorkLA
CLMDTLAR79TxbRtwDateLA
CLMDTLAR79CbxAtSameWageLA
CLMDTLAR79CbxAtSameWageLA
CLMDTLAR79LblNormalStartTimeLA
CLMDTLAR79TxbNormalStartTimeLA
CLMDTLAR79LblLastFullDayLA_copy
CLMDTLAR79TxbLastFullDayLA
CLMDTLAR79LblEmployerUiLA
CLMDTLAR79TxbUiAccount#LA
CLMDTLAR80OutReportPurposeLA
CLMDTLAR80CbxMoreThan7LA
CLMDTLAR80CbxInjuryDeathLA
CLMDTLAR80CbxLumpSumLA
CLMDTLAR80CbxOtherLA
CLMDTLAR80CbxAmputationLA
CLMDTLAR80CbxPossibleDispute
CLMDTLAR80CbxMedicalOnlyLA
CLMDTLAR81OutAccidentInfoLA
CLMDTLAR81LblParishLA
CLMDTLAR81TxbParishInjuryLA
CLMDTLAR81LblPlaceOfInjuryLA
CLMDTLAR81TxbPlaceInjuryParishLA
CLMDTLAR81LblDepartmentLA
CLMDTLAR81TxbDepartmentLA
CLMDTLAR81LblSpecificActivityLA
CLMDTLAR81TxbActivity1LA
CLMDTLAR81TxbActivity2LA
CLMDTLAR81LblInjuryCauseLA
CLMDTLAR81TxbCausedInjury1LA
CLMDTLAR81TxbCausedInjury2LA
CLMDTLAR82OutMiscellaneousLA
CLMDTLAR82LblDateDiagnosed
CLMDTLAR82TxbOccupationalDiseaseDateLA
CLMDTLAR82LblEmployersAddressLA
CLMDTLAR82TxbEmpAddressLA
CLMDTLAR82LblPersonCompletingLA
CLMDTLAR82TxbPersonCompletingFormLA
CLMDTLAR82LblAddressLA
CLMDTLAR82TxbEmpCityLA
CLMDTLAR82LblCityLA
CLMDTLAR82DrpEmpStateLA
CLMDTLAR82LblStateLA
CLMDTLAR82TxbEmpZipLA
CLMDTLAR82LblLA
CLMDTLAR83OutEmployeeInfoMA
CLMDTLAR83LblAwwIndicatorMA
CLMDTLAR83TxbAwwIndicatorMA
CLMDTLAR83OutEmployerInformationMA
CLMDTLAR83LblIndustryCodeMA
CLMDTLAR83TxbIndustryCodeMA
CLMDTLAR83CbxSelfInsuredMA
CLMDTLAR83LblSelfInsuredNumberMA
CLMDTLAR83TxbSelfInsuredNumberMA
CLMDTLAR83LblBusinessTypeMA
CLMDTLAR83TxbBusinessTypeMA
CLMDTLAR84OutInjuryInfoMA
CLMDTLAR84Lbl5thDayMA
CLMDTLAR84Txb5thdayMA
CLMDTLAR84LblSourceOfInjuryMA
CLMDTLAR84TxbSourceInjury1MA
CLMDTLAR84TxbSourceInjury2MA
CLMDTLAR84LblDescribeInjuryMA
CLMDTLAR84TxbHowInjuryOccurred1MA
CLMDTLAR84TxbHowInjuryOccurred2MA
CLMDTLAR84LblReportedPersonMA
CLMDTLAR84TxbPersonReportedMA
CLMDTLAR84LblDateReportedMA
CLMDTLAR84TxbDateReportedMA
CLMDTLAR84LblInjuryCodesMA
CLMDTLAR84TxbInjurycode1MA
CLMDTLAR84TxbInjurycode2MA
CLMDTLAR84TxbInjurycode3MA
CLMDTLAR84LblBodyPartCodesMA
CLMDTLAR84TxbBodycode1MA
CLMDTLAR84TxbBodycode2MA
CLMDTLAR84TxbBodycode3MA
CLMDTLAR85OutMiscellaneousMA
CLMDTLAR85LblWitnessesMA
CLMDTLAR85TxbWitness1MA
CLMDTLAR85TxbWitness2MA
CLMDTLAR85CbxReturnOccupationMA
CLMDTLAR85LblPreparerNameMA
CLMDTLAR85TxbPreparerNameMA
CLMDTLAR85LblPreparerTitleMA
CLMDTLAR85TxbPreparerTitleMA
CLMDTLAR85LblPreparerDateMA
CLMDTLAR85TxbPreparerDateMA
CLMDTLAR86OutEmployerInformationNC
CLMDTLAR86LblNatureNC
CLMDTLAR86TxbBusinessNature1NC
CLMDTLAR86TxbBusinessNature2NC
CLMDTLAR86LblLocationNC
CLMDTLAR86TxbPlantLocationNC
CLMDTLAR86LblCountyNC
CLMDTLAR86TxbCountyNC
CLMDTLAR86LblDepartmentNC
CLMDTLAR86TxbDepartmentNC
CLMDTLAR86LblStatePremisesNC
CLMDTLAR86TxbEmployerPremisesNC
CLMDTLAR87OutAccidentInfoNC
CLMDTLAR87LblDisabilityDateNC
CLMDTLAR87OutDateDisabilityBeganNC
CLMDTLAR87TxbDisAmPmNC
CLMDTLAR87TxbEmployedDayNC
CLMDTLAR87LblWagesPerHourNC
CLMDTLAR87TxbWagePerHourNC
CLMDTLAR87LblWagesPerDayNC
CLMDTLAR87TxbWagePerDayNC
CLMDTLAR87LblEstimatedValueNC
CLMDTLAR87TxbEstimatedValueNC
CLMDTLAR87LblPerNC
CLMDTLAR87TxbPerPeriodNC
CLMDTLAR87LblListInjuriesNC
CLMDTLAR87TxbInjuryList1NC
CLMDTLAR87TxbInjuryList2NC
CLMDTLAR87LblHourReturnedWorkNC
CLMDTLAR87TxbHorRtwNC
CLMDTLAR87TxbRtwAmPmNC
CLMDTLAR87LblReturnWagesNC
CLMDTLAR87TxbRtwWagesNC
CLMDTLAR87LblPer2NC
CLMDTLAR87TxbRtwWagesPerNC
CLMDTLAR87LblOccupationNC
CLMDTLAR87TxbRtwOccupationNC
CLMDTLAR87CbxTreatedNC
CLMDTLAR88OutEmployeeInfoNY
CLMDTLAR88LblHourBeganWorkNY
CLMDTLAR88TxbHourBeganNY
CLMDTLAR88LblTotalEarningsNY
CLMDTLAR88TxbTotalEarningsNY
CLMDTLAR88LblInjuredWorkWeekNY
CLMDTLAR88CbxMonNY
CLMDTLAR88CbxTuesNY
CLMDTLAR88CbxWedNY
CLMDTLAR88CbxThursNY
CLMDTLAR88CbxFriNY
CLMDTLAR88CbxSatNY
CLMDTLAR88CbxSunNY
CLMDTLAR88LblAccidentCountyNY
CLMDTLAR88TxbAccidentCountyNY
CLMDTLAR88LblDepartmentNY
CLMDTLAR88TxbDepartmentNY
CLMDTLAR89OutInjuryInfoNY
CLMDTLAR89CbxProvidedMedicalNY
CLMDTLAR89CbxTreatedInErNY
CLMDTLAR89CbxHospitalizedNY
CLMDTLAR89LblProvidedDateNY
CLMDTLAR89TxbMedCareDateNY
CLMDTLAR89LblEmployeeDoingNY
CLMDTLAR89TxbEmployeeDoing1NY
CLMDTLAR89TxbEmployeeDoing2NY
CLMDTLAR89LblHowAccidentNY
CLMDTLAR89TxbHowOccur1NY
CLMDTLAR89TxbHowOccur2NY
CLMDTLAR89LblObjectInjuredNY
CLMDTLAR89TxbObjectInjuredEmployee1NY
CLMDTLAR89TxbObjectInjuredEmployee2NY
CLMDTLAR90OutThirdPartyNY
CLMDTLAR90LblCompanyNameAddressNY
CLMDTLAR90TxbCompanyNameNY
CLMDTLAR90LblCompanyNameNY
CLMDTLAR90TxbCompanyAddress1NY
CLMDTLAR90LblCompanyAddress1NY
CLMDTLAR90TxbCompanyAddress2NY
CLMDTLAR90LblCompanyAddress2NY
CLMDTLAR90TxbCompanyCityNY
CLMDTLAR90LblCompanyCityNY
CLMDTLAR90DrpCompanyStateNY
CLMDTLAR90LblCompanyStateNY
CLMDTLAR90TxbCompanyZipNY
CLMDTLAR90LblCompanyZipNY
CLMDTLAR90LblContactNameNY
CLMDTLAR90TxbContactNameNY
CLMDTLAR90LblContactPhone#NY
CLMDTLAR90TxbContactPhoneNY
CLMDTLAR91OutMiscellaneousNY
CLMDTLAR91LblRelativeNameAddressNY
CLMDTLAR91TxbRelativeNameNY
CLMDTLAR91LblRelativeNameNY
CLMDTLAR91TxbRelativeAddress1NY
CLMDTLAR91LblRelativeAddress1NY
CLMDTLAR91TxbRelativeAddress2NY
CLMDTLAR91LblRelativeAddress2NY
CLMDTLAR91TxbRelativeCityNY
CLMDTLAR91DrpRelativeStateNY
CLMDTLAR91TxbRelativeZipNY
CLMDTLAR91LblRelativeCityNY
CLMDTLAR91LblRelativeStateNY
CLMDTLAR91LblRelativeZipNY
CLMDTLAR91LblPreparerNameNY
CLMDTLAR91TxbPreparerNameNY
CLMDTLAR91LblPreparerTitleNY
CLMDTLAR91TxbPreparerTitleNY
CLMDTLAR91LblPreparerpHONENY
CLMDTLAR91TxbPreparerPhoneNY
CLMDTLAR91LblRelativeNY
CLMDTLAR91TxbRelativeRelationNY
CLMDTLAR92OutAccidentInfoNC_copy
CLMDTLAR92LblBeginShiftPA
CLMDTLAR92TxbShiftBeganPA
CLMDTLAR92LblContactFirstNamePA
CLMDTLAR92TxbContactFirstNamePA
CLMDTLAR92LblContactLastNamePA
CLMDTLAR92TxbContactLastNamePA
CLMDTLAR92LblContactPhonePA
CLMDTLAR92TxbContactPhonePA
CLMDTLAR92CbxToolsInvolvedPA
CLMDTLAR92CbxMechanicalDefectPA
CLMDTLAR92CbxAmputationPA
CLMDTLAR92CbxCoveredByPanelPA
CLMDTLAR92CbxUnsafeActPA
CLMDTLAR92CbxUnsafeConditionPA
CLMDTLAR92CbxVehicleAccidentPA
CLMDTLAR92CbxSafeguardsProvidedPA
CLMDTLAR92CbxSafeguardsUsedPA
CLMDTLAR92LblInjuredCountyPA
CLMDTLAR92TxbInjuredCountyPA
CLMDTLAR92LblMaterialsPA
CLMDTLAR92TxbMaterialsUsed1PA
CLMDTLAR92TxbMaterialsUsed2PA
CLMDTLAR92LblDescribeEventsPA
CLMDTLAR92TxbDescribeSequence1PA
CLMDTLAR92TxbDescribeSequence2PA
CLMDTLAR93OutMiscellaneousNY_copy
CLMDTLAR93LblWitnessFirstNamePA_copy
CLMDTLAR93TxbWitnessFirstNamePA
CLMDTLAR93LblWitnessLastNamePA_copy
CLMDTLAR93TxbWitnessLastNamePA
CLMDTLAR93LblWitnessPhonePA_copy
CLMDTLAR93TxbWitnessPhonePA
CLMDTLAR93LblCarrierBureauCodePA
CLMDTLAR93TxbCarrierBureauCode
CLMDTLAR93LblPersonFormPA
CLMDTLAR93TxbPreparerNamePA
CLMDTLAR93OutputField1
CLMDTLAR93TxbPreparerTitlePA
CLMDTLAR93OutputField2
CLMDTLAR93TxbPreparerPhonePA
CLMDTLAR93OutputField3
CLMDTLAR93LblFroiDatePA
CLMDTLAR93TxbDatePreparedPA
CLMDTLAR94OutAccidentInfoWV
CLMDTLAR94LblInjuredEmployeeIs
CLMDTLAR94CbxOwnerPartner
CLMDTLAR94CbxOfficer
CLMDTLAR94CbxRetired
CLMDTLAR94LblRetiredDate
CLMDTLAR94TxbRetiredDate
CLMDTLAR94LblSupervisorReported
CLMDTLAR94TxbSupervisorReported
CLMDTLAR94LblDiseaseOccur
CLMDTLAR94TxbDiseaseOccur1
CLMDTLAR94TxbDiseaseOccur
CLMDTLAR94CbxSuspect
CLMDTLAR94CbxReasonQuestion
CLMDTLAR94LblLocation
CLMDTLAR94TxbLocation
CLMDTLAR94CbxEmergencyRoom
CLMDTLAR94CbxHospitalized
CLMDTLAR94OutWorkWageInfo
CLMDTLAR94LblWorkDaysLost
CLMDTLAR94TxbWorkDaysLost
CLMDTLAR94CbxLightDuty
CLMDTLAR94CbxModifiedWork
CLMDTLAR94LblDailyRate
CLMDTLAR94TxbDailyRate
CLMDTLAR94LblWageDateOfInjury
CLMDTLAR94TxbWageDateOfInjury
CLMDTLAR94LblInjuryWagePer
CLMDTLAR94SbxInjuryWagePer
CLMDTLAR94LblCurrentWage
CLMDTLAR94TxbCurrentWage
CLMDTLAR94LblCurrentWagePer
CLMDTLAR94SbxCurrentWagePer
CLMDTLAR94LblBestQuarterWages
CLMDTLAR94TxbBestQuaterWages
CLMDTLAR95LblNoInfoNI
CLMDTLQD01LblQuickDiaryMedical
CLMDTLQD01TxbMedicalDate
CLMDTLQD01DrpMedicalRemarkCode
CLMDTLQD01TxaMedicalNote
CLMDTLQD02LblQuickDiaryCompensation
CLMDTLQD02TxbCompensationDate
CLMDTLQD02DrpCompensationRemarkCode
CLMDTLQD02TxaCompensationNote
CLMDTLQD03LblQuickdiaryStateForm
CLMDTLQD03TxbStateFormDate
CLMDTLQD03DrpStateFormRemarkCode
CLMDTLQD03TxaStateFormNote
CLMDTLQD04LblQuickDiarySupervisor
CLMDTLQD04TxbSupervisorDate
CLMDTLQD04DrpSupervisorRemarkCode
CLMDTLQD04TxaSupervisorNote
CLMDTLQD05LblQuickDiaryUnitManager
CLMDTLQD05TxbUnitManagerDate
CLMDTLQD05DrpUnitManagerRemarkCode
CLMDTLQD05TxaUnitManagerNote
CLMRSVAR10OutReservePmt
CLMRSVAR10sflPmtSum
CLMRSVAR10Lbl Total
CLMRSVAR10outRsvTtlRR
CLMRSVAR10outRsvTtlPR
CLMRSVAR10OutRsvTtlTI
CLMRSVAR11BtnAddEdit
CLMRSVAR12OutReserveHst
CLMRSVAR12SflReserveHst
CLMRSVAR12LblDateChg
CLMRSVAR12OutDtlDatCh
CLMRSVAR12LblDtlOutstd
CLMRSVAR12LblTotalincurred
CLMRSVAR12LblDtlComp
CLMRSVAR12OutDtlComp
CLMRSVAR12OutDtloutc
CLMRSVAR12OutDtlTtlC
CLMRSVAR12LblDtlMed
CLMRSVAR12OutDtlOutM
CLMRSVAR12OutDtlTtlM
CLMRSVAR12LblDtlLgl
CLMRSVAR12OutDtlOutL
CLMRSVAR12OutDtlTtlL
CLMRSVAR12LblDtlEx
CLMRSVAR12OutDtlExpen
CLMRSVAR12OutDtlOutE
CLMRSVAR12OutDtlTtlE
CLMRSVAR12LblDtlITttl
CLMRSVAR12OutDtlTotal
CLMRSVAR12LblDtlUsr
CLMRSVAR12OutDtlUser
CLMRSVAR12LblDtlRsn
CLMRSVAR12OutDtlRsn
CLMRSVAR13OutStsHst
CLMRSVAR13SflStatusH
CLMRSVAR14BtnClmSts
CLMRSVAR15LnkChk#
CLMRSVAR16LnkChgPmtExcll
CLMRSVAR17DtlOutChk#
CLMRSVAR18LnkEob#
CLMRSVAR19LblDedHst
CLMRSVAR19SflDedHstr
CLMRSVAR19OutputField2
CLMRSVAR19OutputField3
CLMRSVAR19OutputField3_copy
CLMRSVAR19OutputField3_copy2
CLMCMPAR01LnkEDILogHst
CLMCMPAR02LnkEDIA49AddChng
CLMCMPAR03LnkEdi148Chng
CLMCMPAR04BtnCmsLog
CLMCMPAR05LnkCmsAddClm
CLMACTAR01OutEmployerName
POLLOCAR18LnkLEdit
POLLOCAR19LnkLDelete
POLLOCAR20BtnLYAdd
POLSUMAR06LnkClaim
POLSUMAR07Hyperlink2
CLMDTLAR07DrpWWTC
CLMDTLAR07WorkDaysSch
CLMDTLAR07WorkDaysSch_copy
CLMDTLAR07WorkDaysSch_copy2
CLMDTLAR07WorkDaysSch_copy3
CLMDTLAR07WorkDaysSch_copy4
CLMDTLAR07WorkDaysSch_copy5
CLMDTLAR07WorkDaysSch_copy6
CLMDTLAR07TxtEmployeeSecurityID
CLMDTLAR15CbxLifePen
CLMDTLAR15CbxSw132a
CLMDTLAR15CbxDelayed
CLMDTLAR15CbxDwc1
CLMDTLAR62TxbWcab#
CLMDTLAR62TxbIaPhn#
CLMDTLAR62LblWcab#
CLMDTLAR62LblIaPhn#
CLMDTLAR16TxbReinsurerClaim#
CLMDTLAR17CbxDenied
CLMDTLAR17CbxDenied_copy
CLMDTLAR17TxbDecisionDt
CLMDTLAR17LblDecisionDt
CLMDTLAR17LblIsClmDenied
CLMDTLAR07OutputField1
CLMDTLAR07OutputField2
CLMDTLAR07OutputField3
CLMDTLAR06TxbPlaceZip
CLMDTLAR06LblEmplrPremises
CLMPLNAR01LnkClmActDel
CLMPLNAR02BtnClmActAdd
POLLOCAR21LnkClaimHandlingInstructions
CLMCFGAR01cbxMedClaims                                                            ** 12/01/14
CLMCFGAR01cbxIndClaims                                                            ** 12/01/14
CLMCFGAR01cbxUserFlag                                                             ** 12/01/14
CLMCFGAR01TxbMedicalAmt                                                           ** 12/01/14
CLMCFGAR01TxbIndemnityAmt                                                         ** 12/01/14
CLMCFGAR01TxbApprovalCode                                                         ** 12/01/14
CLMCFGAR01TxbRemarkCode                                                           ** 12/01/14
CLMCFGAR01LnkHdgStatus                                                            ** 03/13/15
CLMCFGAR01LnkHdgEdit                                                              ** 03/13/15
CLMCFGAR01BtnHdgAdd                                                               ** 03/13/15
AUDITRQST1DrpAuditRequestProc                                                     ** 02/23/15
AUDITRQST1CbxUpdAuditTrack                                                        ** 02/23/15
VSTTRK0001lnkEdit
VSTTRK0002lnkDeleteUndelete                                                       ** 09/26/23
VSTTRK0003BtnVstAdd                                                               ** 09/26/23
POLPOP2A01BtnFinalAudAdd
POLPOP2A02BtnSelfRptAdd
EMPCOPY001CbxMainOrgIncDt
EMPCOPY002CbxMainNwRnw
CLMDTLEM01LblAltStateClaim#
CLMDTLEM01TxbAltStClm#
CLMDTLEM01LblPriorClaim#                                                          ** 12/02/15
CLMDTLEM01TxbPrClm#                                                               ** 12/02/15
CLMDTLAR96DrpDCIsts
CLMDTLAR96DrpValLvl
CLMDTLAR02TxbClaimantCellPhone                                                    ** 05/09/16
CLMDTLAR02LblCellPhone#                                                           ** 05/09/16
OUTSECSIG LblSecureSignatureFileName                                              ** 08/04/16
OUTSECSIG LblSecureSignatureFolder                                                ** 08/04/16
OUTSECSIG TxbSecureSignatureFileName                                              ** 08/04/16
OUTSECSIG TxbSecureSignatureFolder                                                ** 08/04/16
OUTPUTCTRLHdgSignatureGrid                                                        ** 03/21/17
OUTPUTCTRLSflSignatureGrid                                                        ** 03/21/17
OUTPUTCTRLLnkSignatureEdit                                                        ** 03/21/17
OUTPUTCTRLLnkSignatureDelete                                                      ** 03/21/17
OUTPUTCTRLBtnSignatureGridAdd                                                     ** 03/21/17
POLLOCAR22LnkEFTEdt
POLLOCAR23LnkEFTBnkInfo
POLLOCAR24LnkRqPNote
POLLOCAR25BtnEFTAdd
RULESQL   ConRulesSQL                                                             ** 08/04/16
RULESQL   line2                                                                   ** 08/04/16
RULESQL   LblTrigFileSQLAction                                                    ** 08/04/16
RULESQL   TxaTrigFileSQLAction                                                    ** 08/04/16
POLDTLAR21LnkEmpPayEdit                                                           ** 08/09/17
POLDTLAR21LnkEmpPayDelete                                                         ** 08/09/17
POLDTLAR21BtnEmpPayrAdd                                                           ** 08/09/17
POLDTLAR21LnkPayrVndrEdit                                                         ** 08/09/17
POLDTLAR21BtnPayrVndrAdd                                                          ** 08/09/17
OUTPENDSIGsflEndorsementSignature                                                 ** 10/31/17
OUTPENDSIGlnkEndSigEdit                                                           ** 10/31/17
OUTPENDSIGlnkEndSigDelete                                                         ** 10/31/17
OUTPENDSIGlnkEndSigCopy                                                           ** 10/31/17
OUTPENDSIGBtnEndorsementSignatureGridAdd                                          ** 10/31/17
POLENTLOC1TxbDElid
POLLOCAR15ChkPerBusName                                                           ** 05/17/18
POLLOCAR15ChkPerFedName                                                           ** 05/17/18
POLLOCAR26ChkIndBusName                                                           ** 05/17/18
POLLOCAR26ChkIndFedName                                                           ** 05/17/18
POLLOCAR27CbxOvrRptAsPEO                                                          ** 06/07/18
COLLATERALLnkCollateralEdit                                                       ** 06/22/18
COLLATERALBtnCollateralAdd                                                        ** 06/22/18
CLMDTLAR32CbxEmplrPdSalPriorAcq                                                   ** 06/24/20
CLMDTLAR34DrpFullSuspReasCode                                                     ** 06/24/20
CLMDTLAR34LblFullSuspReasCode                                                     ** 06/24/20
CLMDTLAR34DrpPartSuspReasCode                                                     ** 06/24/20
CLMDTLAR34LblPartSuspReasCode                                                     ** 06/24/20
CLMDTLAR34DrpTempDisBenExtCode                                                    ** 06/24/20
CLMDTLAR34LblTempDisBenExtCode                                                    ** 06/24/20
CLMDTLAR38CbxVocationalRehabilitation                                             ** 06/24/20
GRPCFGAR01LnkGroupEdit                                                            ** 08/04/21
GRPCFGAR02LnkGroupCopy                                                            ** 08/04/21
GRPCFGAR03BtnUserAdd                                                              ** 08/04/21
GRPCFGAR04LnkGrpDartURL                                                           ** 06/22/23
GRPPRIAR01LnkPolYrEdt                                                             ** 08/04/21
GRPPRIAR02BtnPolicyYearAdd                                                        ** 08/04/21
GRPPRIAR03txtGroupName                                                            ** 08/04/21
GRPPRIAR03txtShortName                                                            ** 08/04/21
GRPPRIAR03TxbAddMailAddress1                                                      ** 08/04/21
GRPPRIAR03TxbAddMailAddress2                                                      ** 08/04/21
GRPPRIAR03TxbAddMailCity                                                          ** 08/04/21
GRPPRIAR03TxtAddMailState                                                         ** 08/04/21
GRPPRIAR03TxbAddMailZip                                                           ** 08/04/21
GRPPRIAR03TxbAddPhyAddress1                                                       ** 08/04/21
GRPPRIAR03TxbAddPhyAddress2                                                       ** 08/04/21
GRPPRIAR03TxbAddPhyCity                                                           ** 08/04/21
GRPPRIAR03DrpAddPhyState                                                          ** 08/04/21
GRPPRIAR03TxbAddPhyZip                                                            ** 08/04/21
GRPPRIAR03TxbAddPhone                                                             ** 08/04/21
GRPPRIAR03TxbAddPhone2                                                            ** 08/04/21
GRPPRIAR03TxbAddFaxNumber                                                         ** 08/04/21
GRPPRIAR03TxbEffDate                                                              ** 08/04/21
GRPPRIAR03TxbExpDate                                                              ** 08/04/21
GRPPRIAR03TxbInternetadd                                                          ** 08/04/21
GRPPRIAR03txtMaxExtYr                                                             ** 08/04/21
GRPPRIAR04txtNextEmpNum                                                           ** 08/04/21
GRPPRIAR04txtNextQuotNum                                                          ** 08/04/21
GRPPRIAR04CbxAlwDivZer                                                            ** 08/04/21
GRPPRIAR04CbxAuAsEmpNum                                                           ** 08/04/21
GRPPRIAR04CbxAuAsDivNum                                                           ** 08/04/21
GRPPRIAR05DrpMedTransCod                                                          ** 08/04/21
GRPPRIAR05DrpCompTransCod                                                         ** 08/04/21
GRPPRIAR05DrpExpTransCod                                                          ** 08/04/21
GRPPRIAR05DrpLegTransCod                                                          ** 08/04/21
GRPPRIAR05DrpMedPmtVal                                                            ** 08/04/21
GRPPRIAR05DrpCompPmtVal                                                           ** 08/04/21
GRPPRIAR05DrpExpPmtVal                                                            ** 08/04/21
GRPPRIAR05DrpLegPmtVal                                                            ** 08/04/21
GRPPRIAR06CbxPortDft                                                              ** 08/04/21
GRPPRIAR06CbxEmpSelfIns                                                           ** 08/04/21
GRPPRIAR06CbxIncCICenter                                                          ** 08/04/21
GRPPRIAR06CbxExlEDIProc                                                           ** 08/04/21
GRPPRIAR06CbxExlClmElg                                                            ** 08/04/21
GRPPRIAR06CbxExsInsGrp                                                            ** 08/04/21
GRPPRIAR06chkCommGroup                                                            ** 08/04/21
GRPPRIAR06CbxTestGrp                                                              ** 08/04/21
GRPPRIAR07DrpPEOPolGrp                                                            ** 08/04/21
GRPPRIAR07CbxCharExpCon                                                           ** 08/04/21
GRPPRIAR07CbxCalStkDis                                                            ** 08/04/21
GRPPRIAR08DrpMedResvCod                                                           ** 08/04/21
GRPPRIAR08DrpExpResvCod                                                           ** 08/04/21
GRPPRIAR08txtMedResvAmt                                                           ** 08/04/21
GRPPRIAR08txtExpResvAmt                                                           ** 08/04/21
GRPPRIAR09CbxPortQuote                                                            ** 08/04/21
GRPPRIAR09CbxPortFROI                                                             ** 08/04/21
GRPPRIAR09CbxPortSR                                                               ** 08/04/21
GRPPRIAR09CbxPortPolicy                                                           ** 08/04/21
GRPPRIAR09CbxPortClaim                                                            ** 08/04/21
GRPPRIAR09DtPortalAsOfDate                                                        ** 08/04/21
GRPPRIAR09TbxLookbackDays                                                         ** 08/04/21
GRPPRIAR09LnkPortalCheckAll                                                       ** 08/04/21
GRPPRIAR09LnkPortalUncheckAll                                                     ** 08/04/21
GRPPRIAR09LnkOverride                                                             ** 08/04/21
GRPPRIAR10CbxAutEndorse                                                           ** 08/04/21
GRPPRIAR10CbxSbmMTEnd                                                             ** 08/04/21
GRPPRIAR10CbxPrtSchOpr                                                            ** 08/04/21
GRPPRIAR11DrpAutCrtLoc                                                            ** 08/04/21
GRPPRIAR11RadioPremVerbP                                                          ** 08/04/21
GRPPRIAR11RadioPremVerbC                                                          ** 08/04/21
GRPPRIAR12DrpClmResvType1                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType2                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType3                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType4                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType5                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType6                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType7                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType8                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType9                                                         ** 08/04/21
GRPPRIAR12DrpClmResvType10                                                        ** 08/04/21
GRPPRIAR13DrpDftVendNum                                                           ** 08/04/21
GRPPRIAR13DrpEDIfrmt                                                              ** 08/04/21
GRPPRIAR13txtAltrComNam                                                           ** 08/04/21
GRPPRIAR13TxbAltrComAdd1                                                          ** 08/04/21
GRPPRIAR13TxbAltrComAdd2                                                          ** 08/04/21
GRPPRIAR13TxbAltrComCity                                                          ** 08/04/21
GRPPRIAR13DrpAltrComState                                                         ** 08/04/21
GRPPRIAR13TxbAltrComZip                                                           ** 08/04/21
GRPPRIAR13DrpOrgType                                                              ** 08/04/21
GRPPRIAR13DrpAuthType                                                             ** 08/04/21
GRPPRIAR13txtAttName                                                              ** 08/04/21
GRPPRIAR13txtDayToLost                                                            ** 08/04/21
GRPPRIAR13RadioSchOprOrdA                                                         ** 08/04/21
GRPPRIAR13RadioSchOprOrdD                                                         ** 08/04/21
GRPPRIAR13txtFromCheck                                                            ** 08/04/21
GRPPRIAR13txtToCheck                                                              ** 08/04/21
GRPPRIAR13txtDueDtMthPay                                                          ** 08/04/21
GRPPRIAR13txtDueDtWkPay                                                           ** 08/04/21
GRPPRIAR13DrpGrpAsc                                                               ** 08/04/21
GRPPRIAR14txtFedTaxId                                                             ** 08/04/21
GRPPRIAR14txtResRepEnt                                                            ** 08/04/21
GRPPRIAR14txtFreeFormtext                                                         ** 08/04/21
GRPPRIAR14txtMinAmtCommChk                                                        ** 08/04/21
GRPPRIAR14txtLockBoxNam                                                           ** 08/04/21
GRPPRIAR14TxbLockBoxAdd1                                                          ** 08/04/21
GRPPRIAR14TxbLockBoxAdd2                                                          ** 08/04/21
GRPPRIAR14TxbLockBoxCity                                                          ** 08/04/21
GRPPRIAR14DrpLockBoxState                                                         ** 08/04/21
GRPPRIAR14TxbLockBoxZip                                                           ** 08/04/21
GRPPRIAR14txtIRSTransCode                                                         ** 08/04/21
GRPPRIAR14txtSrvCoTpaCode                                                         ** 08/04/21
GRPPRIAR14txtNCCICarrier#                                                         ** 08/04/21
GRPPRIAR14txtIntAudit#                                                            ** 08/04/21
GRPPRIAR14TxbSrvCoTpaPhone                                                        ** 08/04/21
GRPPRIAR14txtNAICCode                                                             ** 08/04/21
GRPPRIAR14txtBusSegIden                                                           ** 08/04/21
GRPPRIAR14DrpTPAContCode                                                          ** 08/04/21
GRPPRIAR14txtNCCICarrierGroup                                                     ** 08/04/21
GRPPRIAR14txtPolPage3C                                                            ** 08/04/21
GRPPRIAR14txtPolPageC3                                                            ** 08/04/21
GRPPRIAR14DrpCertInsOvr                                                           ** 08/04/21
GRPPRIAR14txtCertPagSeg                                                           ** 08/04/21
GRPPRIAR14txtBillLogo                                                             ** 08/04/21
GRPPRIAR14CbxGrpSrvByTPA                                                          ** 08/04/21
GRPPRIAR14CbxProcAgtCommChk                                                       ** 08/04/21
GRPPRIAR14CbxWCPOLElecRep                                                         ** 08/04/21
GRPPRIAR14CbxDisbSMSText                                                          ** 08/04/21
GRPPRIAR14CbxProcClmPay                                                           ** 08/04/21
GRPPRIAR14RadioCheck                                                              ** 08/04/21
GRPPRIAR14RadioVoucher                                                            ** 08/04/21
GRPPRIAR14RadioXML                                                                ** 08/04/21
GRPPRIAR14txtOutLogoFileName                                                      ** 08/04/21
GRPPRIAR14txaOutLogoPath                                                          ** 08/04/21
GRPPRIAR14txtFisStrMth                                                            ** 08/04/21
GRPPRIAR14txtGrpLogoFolder                                                        ** 08/04/21
GRPPRIAR14txtGrpLogoDoc                                                           ** 08/04/21
GRPPRIAR14CbxSupHdgLogo                                                           ** 08/04/21
GRPPRIAR14CbxSupFootSig                                                           ** 08/04/21
GRPPRIAR14txt1099OverFEIN                                                         ** 08/04/21
GRPPRIAR14txtARThrshLmt1                                                          ** 08/04/21
GRPPRIAR14txtARThrshLmt2                                                          ** 08/04/21
GRPPRIAR14txtJobQ                                                                 ** 08/04/21
GRPPRIAR14txtJobQSbs                                                              ** 08/04/21
GRPPRIAR14DrpDepPayCode                                                           ** 08/04/21
GRPPRIAR14DrpPremPayCode                                                          ** 08/04/21
GRPPRIAR14DrpFeePayCode                                                           ** 08/04/21
GRPPRIAR15DrpAssPayCode1                                                          ** 08/04/21
GRPPRIAR15DrpAssPayCode2                                                          ** 08/04/21
GRPPRIAR15DrpAssPayCode3                                                          ** 08/04/21
GRPPRIAR15DrpAssPayCode4                                                          ** 08/04/21
GRPPRIAR15DrpAssPayCode5                                                          ** 08/04/21
GRPPRIAR15DrpAssPayCode6                                                          ** 08/04/21
GRPPRIAR15DrpAssPayCode7                                                          ** 08/04/21
GRPPRIAR16DrpDftCarrCode                                                          ** 08/04/21
GRPPRIAR16DrpPolSym                                                               ** 08/04/21
GRPPRIAR16DrpCompCode                                                             ** 08/04/21
GRPPRIAR16txtProgID                                                               ** 08/04/21
GRPPRIAR16txtCarrNumFmt                                                           ** 08/04/21
GRPPRIAR16txtCarrNumFrom                                                          ** 08/04/21
GRPPRIAR16txtCarrNumTo                                                            ** 08/04/21
GRPPRIAR16txtNxtAvlCarrNum                                                        ** 08/04/21
GRPPRIAR16CbxCarrNumReq                                                           ** 08/04/21
GRPPRIAR16CbxAssignCarrNum                                                        ** 08/04/21
GRPPRIAR16CbxUpdCarrNum                                                           ** 08/04/21
GRPPRIAR16CbxEdtDupCarrNum                                                        ** 08/04/21
GRPPRIAR17txtHeading1                                                             ** 08/04/21
GRPPRIAR17txtHeading2                                                             ** 08/04/21
GRPPRIAR17txtHeading3                                                             ** 08/04/21
GRPPRIAR17txtHeading4                                                             ** 08/04/21
GRPPRIAR17txtHeading5                                                             ** 08/04/21
GRPPRIAR17txtHeading6                                                             ** 08/04/21
GRPPRIAR17CbxReqQte1                                                              ** 08/04/21
GRPPRIAR17CbxReqWrtn1                                                             ** 08/04/21
GRPPRIAR17CbxReqQte2                                                              ** 08/04/21
GRPPRIAR17CbxReqWrtn2                                                             ** 08/04/21
GRPPRIAR17CbxReqQte3                                                              ** 08/04/21
GRPPRIAR17CbxReqWrtn3                                                             ** 08/04/21
GRPPRIAR17CbxReqQte4                                                              ** 08/04/21
GRPPRIAR17CbxReqWrtn4                                                             ** 08/04/21
GRPPRIAR17CbxReqQte5                                                              ** 08/04/21
GRPPRIAR17CbxReqWrtn5                                                             ** 08/04/21
GRPPRIAR17CbxReqQte6                                                              ** 08/04/21
GRPPRIAR17CbxReqWrtn6                                                             ** 08/04/21
GRPPRIAR17DrpTableID1                                                             ** 08/04/21
GRPPRIAR17DrpTableID2                                                             ** 08/04/21
GRPPRIAR17DrpTableID3                                                             ** 08/04/21
GRPPRIAR17DrpTableID6                                                             ** 08/04/21
GRPPRIAR17DrpDftValue1                                                            ** 08/04/21
GRPPRIAR17DrpDftValue2                                                            ** 08/04/21
GRPPRIAR17DrpDftValue3                                                            ** 08/04/21
GRPPRIAR17DrpDftValue6                                                            ** 08/04/21
GRPPRIAR18LnkRegEdt                                                               ** 08/04/21
GRPPRIAR19LnkActRegStatus                                                         ** 08/04/21
GRPPRIAR19LnkInActRegStatus                                                       ** 09/26/23
GRPPRIAR20BtnRegionAdd                                                            ** 08/04/21
GRPPRIAR21LnkChpEdt                                                               ** 08/04/21
GRPPRIAR22LnkActChpStatus                                                         ** 08/04/21
GRPPRIAR22LnkInActChpStatus                                                       ** 09/26/23
GRPPRIAR23BtnCahpterAdd                                                           ** 08/04/21
GRPPRIAR24LnkBankEdit                                                             ** 08/04/21
GRPPRIAR25LnkActBankStatus                                                        ** 08/04/21
GRPPRIAR25LnkInActBankStatus                                                      ** 09/26/23
GRPPRIAR26BtnBankAdd                                                              ** 08/04/21
GRPADMAR01TxbEmpDeletionLevel                                                     ** 08/04/21
GRPADMAR01TxbEmpDeletionType                                                      ** 08/04/21
GRPADMAR01TxbEmpDeletionQteLevel                                                  ** 07/03/23
GRPADMAR01TxbEmpDeletionQteType                                                   ** 07/03/23
GRPADMAR01TxbOverrideCxDtLevel                                                    ** 08/04/21
GRPADMAR01TxbOverrideCxDtType                                                     ** 08/04/21
GRPADMAR01TxbAllowSubMassBillLevel                                                ** 08/04/21
GRPADMAR01TxbAllowSubMassBillType                                                 ** 08/04/21
GRPADMAR01TxbAgtLicenseStEdtOvrLevel                                              ** 08/04/21
GRPADMAR01TxbAgtLicenseStEdtOvrType                                               ** 08/04/21
GRPADMAR01TxbAddLiveEnvLevel                                                      ** 08/04/21
GRPADMAR01TxbAddLiveEnvType                                                       ** 08/04/21
GRPADMAR01TxbTrfQuoteToLiveImmedLevel                                             ** 08/04/21
GRPADMAR01TxbTrfQuoteToLiveImmedType                                              ** 08/04/21
GRPADMAR01TxbUpdtNxtEmpLevel                                                      ** 08/04/21
GRPADMAR01TxbUpdtNxtEmpType                                                       ** 08/04/21
GRPADMAR01TxbOverrideAccidentDtLevel                                              ** 08/04/21
GRPADMAR01TxbOverrideAccidentDtType                                               ** 08/04/21
GRPADMAR01TxbAudtBillLargeDeductAccLevel                                          ** 08/04/21
GRPADMAR01TxbAudtBillLargeDeductAccType                                           ** 08/04/21
GRPADMAR01TxbAgtComPlnChgOvrLevel                                                 ** 08/04/21
GRPADMAR01TxbAgtComPlnChgOvrType                                                  ** 08/04/21
GRPADMAR01TxblAddQuoteEnvLevel                                                    ** 08/04/21
GRPADMAR01TxblAddQuoteEnvType                                                     ** 08/04/21
GRPADMAR01TxbQuoteToLiveBatchLevel                                                ** 08/04/21
GRPADMAR01TxbQuoteToLiveBatchType                                                 ** 08/04/21
GRPADMAR02CbxDeleteFutureArDurCan                                                 ** 08/04/21
GRPADMAR02CbxChgRtDurRenProcToDft                                                 ** 08/04/21
GRPADMAR02CbxPrintS/RInfoWithAuditRequest                                         ** 08/04/21
GRPADMAR02CbxPromptForSecAgt                                                      ** 08/04/21
GRPADMAR02CbxCustomerIdrequired                                                   ** 08/04/21
GRPADMAR02CbxEmployerFedIdRequired                                                ** 08/04/21
GRPADMAR02CbxEmpSICCode                                                           ** 08/04/21
GRPADMAR02CbxReqEmpLinkCode                                                       ** 08/04/21
GRPADMAR02CbxARinqSubDftDepPolYrBal                                               ** 08/04/21
GRPADMAR02CbxAgtDftrtPopupWdwForQteTrf                                            ** 08/04/21
GRPADMAR02CbxPrntCxReForStRptDiv                                                  ** 08/04/21
GRPADMAR02CbxPrntEmpLnkCdOnSchedule                                               ** 08/04/21
GRPADMAR03TxbEmployerContHdg1                                                     ** 08/04/21
GRPADMAR03TxbEmployerContHdg2                                                     ** 08/04/21
GRPADMAR03TxbEmployerContHdg3                                                     ** 08/04/21
GRPADMAR03DrpCrossRef1                                                            ** 08/04/21
GRPADMAR03DrpCrossRef2                                                            ** 08/04/21
GRPADMAR03DrpCrossRef3                                                            ** 08/04/21
GRPADMAR03RadioButtonEDI1                                                         ** 08/04/21
GRPADMAR03RadioButtonEDI2                                                         ** 08/04/21
GRPADMAR03RadioButtonEDI3                                                         ** 08/04/21
GRPADMAR03DrpDefaultRegion#                                                       ** 08/04/21
GRPADMAR03DrpDefaultOffice#                                                       ** 08/04/21
GRPADMAR03DrpPolicyPagePayrollPrnt                                                ** 08/04/21
GRPADMAR03DrpQteTrfToLiveTrType                                                   ** 08/04/21
GRPADMAR03TxbAgtHeader                                                            ** 08/04/21
GRPADMAR03TxbAgentOutputQueue                                                     ** 08/04/21
GRPADMAR03TxbStateOutputQueue                                                     ** 08/04/21
GRPADMAR03TxbMemberOutputQueue                                                    ** 08/04/21
GRPADMAR03TxbCxCert-HolderLetterOutputQueue                                       ** 08/04/21
GRPADMAR04TxbAltHdgForLossControlReport                                           ** 08/04/21
GRPADMAR04TxbAltHdgForWatchList                                                   ** 08/04/21
GRPADMAR04TxbAltHdgForVipMbr                                                      ** 08/04/21
GRPADMAR04TxbAltHdgForSplAcceptance                                               ** 08/04/21
GRPADMAR04CbxPromptForChangesAcrossAll1                                           ** 08/04/21
GRPADMAR04TxbLblField#1heading                                                    ** 08/04/21
GRPADMAR04DrpLblField#1generitbID                                                 ** 08/04/21
GRPADMAR04DrpNewCoverLetter                                                       ** 08/04/21
GRPADMAR04DrpRenewCoverLetter                                                     ** 08/04/21
GRPADMAR05DrpOfficeChangeRemark                                                   ** 08/04/21
GRPADMAR05DrpLblActivityStatusChangeRemark                                        ** 08/04/21
GRPADMAR05DrpLblCompanyNameChangeRemark                                           ** 08/04/21
GRPADMAR05DrpCompNameChgStaff                                                     ** 08/04/21
GRPADMAR05DrpLateNoticeRemark                                                     ** 08/04/21
GRPADMAR05DrpPremiumNoticeRemark                                                  ** 08/04/21
GRPADMAR05DrpPolicypPageRemark                                                    ** 08/04/21
GRPADMAR05DrpAuditRequesrRemark                                                   ** 08/04/21
GRPADMAR05DrpLblAuditActivity                                                     ** 08/04/21
GRPADMAR05TxbNumberOfMonths                                                       ** 08/04/21
GRPADMAR05DrpCancellationRemark                                                   ** 08/04/21
GRPADMAR05DrpRevisedCxRemark                                                      ** 08/04/21
GRPADMAR05DrpReinstatementRemark                                                  ** 08/04/21
GRPADMAR05DrpRevisedReinstatementRemark                                           ** 08/04/21
GRPADMAR05DrpLblCertificateOfInsuranceRemark                                      ** 08/04/21
GRPADMAR05DrpLbllDeductibleInvoiceRemark                                          ** 08/04/21
GRPADMAR05DrpLblAgtCommissionStatement                                            ** 08/04/21
GRPADMAR05DrpLblAgtCommPlanChangeRemark                                           ** 08/04/21
GRPADMAR05DrpMonthlyInterimRemark                                                 ** 08/04/21
GRPADMAR05DrpLblAuditTrackingDelete/Inactivate                                    ** 08/04/21
GRPADMAR05DrpLblACORDApplication                                                  ** 08/04/21
GRPADMAR05DrpLblRevisedBillRenewedPolicy                                          ** 08/04/21
GRPADMAR05DrpLblAlternateEmployerRemark                                           ** 08/04/21
GRPADMAR05DrpLblFeeInvoiceRemark                                                  ** 08/04/21
GRPADMAR05DrpLblACHOneTimeRemarkCode                                              ** 08/04/21
GRPADMAR05DrpLblDocUpl                                                            ** 08/04/21
GRPADMAR06DrpEmpNamChg                                                            ** 08/04/21
GRPADMAR06DrpMailAddChg                                                           ** 08/04/21
GRPADMAR06DrpPhyAddChg                                                            ** 08/04/21
GRPADMAR06DrpAgtNamChg                                                            ** 08/04/21
GRPADMAR06DrpLegStsChg                                                            ** 08/04/21
GRPADMAR06DrpFedIdChg                                                             ** 08/04/21
GRPADMAR06DrpAddDiv                                                               ** 08/04/21
GRPADMAR06DrpDelDiv                                                               ** 08/04/21
GRPADMAR07DrpExpModChg                                                            ** 08/04/21
GRPADMAR07DrpIncLimChg                                                            ** 08/04/21
GRPADMAR07DrpClsRatChg                                                            ** 08/04/21
GRPADMAR07DrpIntAdjChg                                                            ** 08/04/21
GRPADMAR07DrpConModChg                                                            ** 08/04/21
GRPADMAR08DrpAddWavSub                                                            ** 08/04/21
GRPADMAR08DrpWavSubChg                                                            ** 08/04/21
GRPADMAR08DrpDelWavSub                                                            ** 08/04/21
GRPADMAR09DrpAddEnt                                                               ** 08/04/21
GRPADMAR09DrpDelEnt                                                               ** 08/04/21
GRPADMAR09DrpEntNamCHg                                                            ** 08/04/21
GRPADMAR09DrpEntMailAddChg                                                        ** 08/04/21
GRPADMAR09DrpEntPhyAddChg                                                         ** 08/04/21
GRPADMAR10DrpAddLoc                                                               ** 08/04/21
GRPADMAR10DrpDelLoc                                                               ** 08/04/21
GRPADMAR10DrpLocNamChg                                                            ** 08/04/21
GRPADMAR10DrpLocMailAddChg                                                        ** 08/04/21
GRPADMAR10DrpLocPhyAddChg                                                         ** 08/04/21
GRPADMAR11DrpAddOwnOfc                                                            ** 08/04/21
GRPADMAR11DrpNomOwnChg                                                            ** 08/04/21
GRPADMAR12DrpCirtins                                                              ** 08/04/21
GRPADMAR12DrpMisc                                                                 ** 08/04/21
GRPADMAR13DrpAudtAtmpt                                                            ** 08/04/21
GRPADMAR13DrpAudtContested                                                        ** 08/04/21
GRPADMAR13DrpClrCntInfo                                                           ** 08/04/21
GRPADMAR13DrpAudtMailed                                                           ** 08/04/21
GRPADMAR13DrpAudtRcvd                                                             ** 08/04/21
GRPADMAR13DrpReassignAudtRep                                                      ** 08/04/21
GRPADMAR14DrpInitialAudt                                                          ** 08/04/21
GRPADMAR14DrpStaffCode                                                            ** 08/04/21
GRPADMAR14DrpRevisedAudt                                                          ** 08/04/21
GRPADMAR15TxbClmDelLvl                                                            ** 08/04/21
GRPADMAR15TxbClmDelTyp1                                                           ** 08/04/21
GRPADMAR15TxbClmDelTyp2                                                           ** 08/04/21
GRPADMAR15TxbClmDelTyp3                                                           ** 08/04/21
GRPADMAR15TxbClmDelTyp4                                                           ** 08/04/21
GRPADMAR15TxbClmDelTyp5                                                           ** 08/04/21
GRPADMAR15TxbEOMBChkReqDelLvl                                                     ** 08/04/21
GRPADMAR15TxbEOMBChkReqDelTyp1                                                    ** 08/04/21
GRPADMAR15TxbEOMBChkReqDelTyp2                                                    ** 08/04/21
GRPADMAR15TxbEOMBChkReqDelTyp3                                                    ** 08/04/21
GRPADMAR15TxbEOMBChkReqDelTyp4                                                    ** 08/04/21
GRPADMAR15TxbEOMBChkReqDelTyp5                                                    ** 08/04/21
GRPADMAR15TxbPayDenDelClmLvl                                                      ** 08/04/21
GRPADMAR15TxbPayDenDelClmTyp1                                                     ** 08/04/21
GRPADMAR15TxbPayDenDelClmTyp2                                                     ** 08/04/21
GRPADMAR15TxbPayDenDelClmTyp3                                                     ** 08/04/21
GRPADMAR15TxbPayDenDelClmTyp4                                                     ** 08/04/21
GRPADMAR15TxbPayDenDelClmTyp5                                                     ** 08/04/21
GRPADMAR15TxbFullSalVouchLvl                                                      ** 08/04/21
GRPADMAR15TxbFullSalVouchTyp1                                                     ** 08/04/21
GRPADMAR15TxbFullSalVouchTyp2                                                     ** 08/04/21
GRPADMAR15TxbFullSalVouchTyp3                                                     ** 08/04/21
GRPADMAR15TxbFullSalVouchTyp4                                                     ** 08/04/21
GRPADMAR15TxbFullSalVouchTyp5                                                     ** 08/04/21
GRPADMAR15TxbChgDecDtLvl                                                          ** 08/04/21
GRPADMAR15TxbChgDecDtTyp1                                                         ** 08/04/21
GRPADMAR15TxbChgDecDtTyp2                                                         ** 08/04/21
GRPADMAR15TxbChgDecDtTyp3                                                         ** 08/04/21
GRPADMAR15TxbChgDecDtTyp4                                                         ** 08/04/21
GRPADMAR15TxbChgDecDtTyp5                                                         ** 08/04/21
GRPADMAR15TxbPsblDupPmtLvl                                                        ** 08/04/21
GRPADMAR15TxbPsblDupPmtTyp1                                                       ** 08/04/21
GRPADMAR15TxbPsblDupPmtTyp2                                                       ** 08/04/21
GRPADMAR15TxbPsblDupPmtTyp3                                                       ** 08/04/21
GRPADMAR15TxbPsblDupPmtTyp4                                                       ** 08/04/21
GRPADMAR15TxbPsblDupPmtTyp5                                                       ** 08/04/21
GRPADMAR15TxbCMSReSubLvl                                                          ** 08/04/21
GRPADMAR15TxbCMSReSubTyp1                                                         ** 08/04/21
GRPADMAR15TxbCMSReSubTyp2                                                         ** 08/04/21
GRPADMAR15TxbCMSReSubTyp3                                                         ** 08/04/21
GRPADMAR15TxbCMSReSubTyp4                                                         ** 08/04/21
GRPADMAR15TxbCMSReSubTyp5                                                         ** 08/04/21
GRPADMAR15TxbOvrCanAccDtLvl                                                       ** 08/04/21
GRPADMAR15TxbOvrCanAccDtTyp1                                                      ** 08/04/21
GRPADMAR15TxbOvrCanAccDtTyp2                                                      ** 08/04/21
GRPADMAR15TxbOvrCanAccDtTyp3                                                      ** 08/04/21
GRPADMAR15TxbOvrCanAccDtTyp4                                                      ** 08/04/21
GRPADMAR15TxbOvrCanAccDtTyp5                                                      ** 08/04/21
GRPADMAR15TxbDelPendPmtLvl                                                        ** 08/04/21
GRPADMAR15TxbDelPendPmtTyp1                                                       ** 08/04/21
GRPADMAR15TxbDelPendPmtTyp2                                                       ** 08/04/21
GRPADMAR15TxbDelPendPmtTyp3                                                       ** 08/04/21
GRPADMAR15TxbDelPendPmtTyp4                                                       ** 08/04/21
GRPADMAR15TxbDelPendPmtTyp5                                                       ** 08/04/21
GRPADMAR15TxbVoidStopPmtLvl                                                       ** 08/04/21
GRPADMAR15TxbVoidStopPmtTyp1                                                      ** 08/04/21
GRPADMAR15TxbVoidStopPmtTyp2                                                      ** 08/04/21
GRPADMAR15TxbVoidStopPmtTyp3                                                      ** 08/04/21
GRPADMAR15TxbVoidStopPmtTyp4                                                      ** 08/04/21
GRPADMAR15TxbVoidStopPmtTyp5                                                      ** 08/04/21
GRPADMAR15TxbZeroEstAudPayLvl                                                     ** 08/04/21
GRPADMAR15TxbZeroEstAudPayTyp1                                                    ** 08/04/21
GRPADMAR15TxbZeroEstAudPayTyp2                                                    ** 08/04/21
GRPADMAR15TxbZeroEstAudPayTyp3                                                    ** 08/04/21
GRPADMAR15TxbZeroEstAudPayTyp4                                                    ** 08/04/21
GRPADMAR15TxbZeroEstAudPayTyp5                                                    ** 08/04/21
GRPADMAR15TxbExctDupPmtLvl                                                        ** 08/04/21
GRPADMAR15TxbExctDupPmtTyp1                                                       ** 08/04/21
GRPADMAR15TxbExctDupPmtTyp2                                                       ** 08/04/21
GRPADMAR15TxbExctDupPmtTyp3                                                       ** 08/04/21
GRPADMAR15TxbExctDupPmtTyp4                                                       ** 08/04/21
GRPADMAR15TxbExctDupPmtTyp5                                                       ** 08/04/21
GRPADMAR15TxbCMSSubLvl                                                            ** 08/04/21
GRPADMAR15TxbCMSSubTyp1                                                           ** 08/04/21
GRPADMAR15TxbCMSSubTyp2                                                           ** 08/04/21
GRPADMAR15TxbCMSSubTyp3                                                           ** 08/04/21
GRPADMAR15TxbCMSSubTyp4                                                           ** 08/04/21
GRPADMAR15TxbCMSSubTyp5                                                           ** 08/04/21
GRPADMAR15TxbPBMEligibleLvl                                                       ** 08/04/21
GRPADMAR15TxbPBMEligibleTyp1                                                      ** 08/04/21
GRPADMAR15TxbPBMEligibleTyp2                                                      ** 08/04/21
GRPADMAR15TxbPBMEligibleTyp3                                                      ** 08/04/21
GRPADMAR15TxbPBMEligibleTyp4                                                      ** 08/04/21
GRPADMAR15TxbPBMEligibleTyp5                                                      ** 08/04/21
GRPADMAR16CbxReqLocEntry                                                          ** 08/04/21
GRPADMAR16CbxAlwClmClose                                                          ** 08/04/21
GRPADMAR16CbxEnterRsvChrg                                                         ** 08/04/21
GRPADMAR16CbxRecordOnlyClmWhnPrnt                                                 ** 08/04/21
GRPADMAR16CbxOpClStDsTbDrv                                                        ** 08/04/21
GRPADMAR16CbxMthdDetPIWgReq                                                       ** 08/04/21
GRPADMAR16CbxLossRecTyp                                                           ** 08/04/21
GRPADMAR16CbxExNCCIMed                                                            ** 08/04/21
GRPADMAR16CbxDtBirthRq                                                            ** 08/04/21
GRPADMAR16CbxGndrReq                                                              ** 08/04/21
GRPADMAR16CbxCVTClmNbr                                                            ** 08/04/21
GRPADMAR16CbxDtHireReq                                                            ** 08/04/21
GRPADMAR16CbxAWWReq                                                               ** 08/04/21
GRPADMAR16CbxPBBElgbl                                                             ** 08/04/21
GRPADMAR17TxbPmtStsCdPend                                                         ** 08/04/21
GRPADMAR17DrpPmtStsCdRls                                                          ** 08/04/21
GRPADMAR17TxbPmtStsCdDup                                                          ** 08/04/21
GRPADMAR17DrpCCRsvTyp1                                                            ** 08/04/21
GRPADMAR17DrpCCRsvTyp2                                                            ** 08/04/21
GRPADMAR17DrpCCRsvTyp3                                                            ** 08/04/21
GRPADMAR17DrpCCRsvTyp4                                                            ** 08/04/21
GRPADMAR17DrpMajPerilCdDft                                                        ** 08/04/21
GRPADMAR17TxbHdgCatg                                                              ** 08/04/21
GRPADMAR17TxbHdgAdjuster                                                          ** 08/04/21
GRPADMAR17TxbHdgLglRsv                                                            ** 08/04/21
GRPADMAR17TxbEDIMailDt                                                            ** 08/04/21
GRPADMAR18DrpChgAdjusterFr                                                        ** 08/04/21
GRPADMAR18DrpNameChg                                                              ** 08/04/21
GRPADMAR18DrpReOpen                                                               ** 08/04/21
GRPADMAR18DrpNmAddChgDftRmk                                                       ** 08/04/21
GRPADMAR18DrpAdlAdDpChg                                                           ** 08/04/21
GRPADMAR18CbxGenOnAdd                                                             ** 08/04/21
GRPADMAR18DrpChgAdjusterTo                                                        ** 08/04/21
GRPADMAR18DrpDecisionDtChg                                                        ** 08/04/21
GRPADMAR18TxbDyFrSyDtReOpn                                                        ** 08/04/21
GRPADMAR18DrpClmActPlan                                                           ** 08/04/21
GRPADMAR18DrpWageCalRvw                                                           ** 08/04/21
GRPADMAR19DrpAdjMedMOClm                                                          ** 08/04/21
GRPADMAR19DrpSuprvrLTClm                                                          ** 08/04/21
GRPADMAR19DrpMgrLTClm                                                             ** 08/04/21
GRPADMAR19DrpAdjCmpDiary                                                          ** 08/04/21
GRPADMAR19TxbDyFrSyDtAdjMed                                                       ** 08/04/21
GRPADMAR19TxbDyFrRcvDtSup                                                         ** 08/04/21
GRPADMAR19TxbDyFrRcvDtMgr                                                         ** 08/04/21
GRPADMAR19DrpStateReport                                                          ** 08/04/21
GRPADMAR19TxbApprmntRmkCd                                                         ** 08/04/21
GRPADMAR19CbxClmDryAcSmScr                                                        ** 08/04/21
GRPADMAR19CbxExGrpClmExp                                                          ** 08/04/21
GRPADMAR19CbxDevereuxGrp                                                          ** 08/04/21
GRPADMAR20CbxClmHndIns                                                            ** 08/04/21
GRPADMAR20CbxMedInfo                                                              ** 08/04/21
GRPADMAR20CbxBenftInfo                                                            ** 08/04/21
GRPADMAR20CbxRsvPmtSum                                                            ** 08/04/21
GRPADMAR20CbxStSpecific                                                           ** 08/04/21
GRPADMAR20CbxPrintFROI                                                            ** 10/21/23
GRPADMAR20CbxDrtBnftDry                                                           ** 08/04/21
GRPADMAR20CbxDARTS                                                                ** 08/04/21
GRPADMAR20CbxLoadPreInjuryWage                                                    ** 08/04/21
GRPADMAR20DrpLossCondCode                                                         ** 08/04/21
GRPADMAR20DrpLossRecovery                                                         ** 08/04/21
GRPADMAR20DrpTypeOfCoverage                                                       ** 08/04/21
GRPADMAR20DrpLossCovgCode                                                         ** 08/04/21
GRPADMAR20CbxMedOnlyClm                                                           ** 08/04/21
GRPADMAR20CbxActClmClosgMed                                                       ** 08/04/21
GRPADMAR20CbxIndemnityClm                                                         ** 08/04/21
GRPADMAR20CbxActClmClosgIndm                                                      ** 08/04/21
GRPADMAR20TxbNoFinActMedDy                                                        ** 08/04/21
GRPADMAR20TxbNoFinActIndmDy                                                       ** 08/04/21
GRPADMAR20DrpAdjDryRmk1                                                           ** 08/04/21
GRPADMAR20DrpAdjDryRmk2                                                           ** 08/04/21
GRPADMAR21DrpAtrnyPVCd1                                                           ** 08/04/21
GRPADMAR21DrpAtrnyPVCd2                                                           ** 08/04/21
GRPADMAR21DrpAtrnyPVCd3                                                           ** 08/04/21
GRPADMAR21DrpAtrnyPVCd4                                                           ** 08/04/21
GRPADMAR21DrpAtrnyPVCd5                                                           ** 08/04/21
GRPADMAR22DrpClmInErDryCd                                                         ** 08/04/21
GRPADMAR22DrpClmInErDryUsr                                                        ** 08/04/21
GRPADMAR22DrpClmInErSpfUsr                                                        ** 08/04/21
GRPADMAR22DrpQkRspDrCd                                                            ** 08/04/21
GRPADMAR22DrpQkRspDryUsr                                                          ** 08/04/21
GRPADMAR22DrpQkRspSpfUsr                                                          ** 08/04/21
GRPADMAR22DrpRmvCSbFlDrCd                                                         ** 08/04/21
GRPADMAR22DrpRmvCSbFlDrUs                                                         ** 08/04/21
GRPADMAR22DrpRmvSbFlSpfUs                                                         ** 08/04/21
GRPADMAR22CbxIncExMedBenQF                                                        ** 08/04/21
GRPPOLAR01CbxSuppDbMsg                                                            ** 08/04/21
GRPPOLAR01CbxSuppCrMsg                                                            ** 08/04/21
GRPPOLAR01CbxSupGrpAdd                                                            ** 08/04/21
GRPPOLAR01CbxSupPrYrBl                                                            ** 08/04/21
GRPPOLAR01CbxPrOprBlSum                                                           ** 08/04/21
GRPPOLAR01CbxThrdPBl                                                              ** 08/04/21
GRPPOLAR01CbxGenPdf                                                               ** 08/04/21
GRPPOLAR01CbxBlExpMPr                                                             ** 08/04/21
GRPPOLAR01CbxBlExpOrSt                                                            ** 08/04/21
GRPPOLAR01CbxBlAsDiv                                                              ** 08/04/21
GRPPOLAR01CbxRndBlTax                                                             ** 08/04/21
GRPPOLAR01CbxSpRevBl                                                              ** 08/04/21
GRPPOLAR01CbxpPriorYr                                                             ** 08/04/21
GRPPOLAR01CbxS/Ronline                                                            ** 08/04/21
GRPPOLAR01CbxPrevBal                                                              ** 08/04/21
GRPPOLAR01CbxDfrAudCr                                                             ** 08/04/21
GRPPOLAR01CbxDfrAudDbt                                                            ** 08/04/21
GRPPOLAR01CbxEstBl                                                                ** 08/04/21
GRPPOLAR01CbxRevBl                                                                ** 08/04/21
GRPPOLAR01CbxAudBl                                                                ** 08/04/21
GRPPOLAR01CbxBypInfPrmC                                                           ** 08/01/24
GRPPOLAR02TxbDirAudBill                                                           ** 08/04/21
GRPPOLAR02TxbAccAudBill                                                           ** 08/04/21
GRPPOLAR02TxbDirRevBl                                                             ** 08/04/21
GRPPOLAR02TxbAccRevBil                                                            ** 08/04/21
GRPPOLAR02TxbDirFees                                                              ** 08/04/21
GRPPOLAR02TxbAccFees                                                              ** 08/04/21
GRPPOLAR02TxbNumDays                                                              ** 08/04/21
GRPPOLAR03DrpSetRmkCd                                                             ** 08/04/21
GRPPOLAR03txtLossLmt                                                              ** 08/04/21
GRPPOLAR03TxbFClass                                                               ** 08/04/21
GRPPOLAR03TxbExLoss                                                               ** 08/04/21
GRPPOLAR03TxbDevFac1                                                              ** 08/04/21
GRPPOLAR03TxbDevFac2                                                              ** 08/04/21
GRPPOLAR03TxbDevFac3                                                              ** 08/04/21
GRPPOLAR03CbxPrcRet                                                               ** 08/04/21
GRPPOLAR04DrpPrmPmtA/R                                                            ** 08/04/21
GRPPOLAR04DrpAudPmt                                                               ** 08/04/21
GRPPOLAR04DrpDedPmt                                                               ** 08/04/21
GRPPOLAR05DrpQuoteBl                                                              ** 08/04/21
GRPPOLAR05DrpOrgBl                                                                ** 08/04/21
GRPPOLAR05DrpRevBill                                                              ** 08/04/21
GRPPOLAR05DrpFinBill                                                              ** 08/04/21
GRPPOLAR05DrpRevFinAud                                                            ** 08/04/21
GRPPOLAR05DrpSRAud                                                                ** 08/04/21
GRPPOLAR06DrpEstimating                                                           ** 08/04/21
GRPPOLAR06DrpDepAdj                                                               ** 08/04/21
GRPPOLAR06DrpSelfRep                                                              ** 08/04/21
GRPPOLAR06DrpAudAdj                                                               ** 08/04/21
GRPPOLAR06DrpEstadj                                                               ** 08/04/21
GRPPOLAR06DrpTaxBlAud                                                             ** 08/04/21
GRPPOLAR07cbxExludePolicyFromARBilling                                            ** 08/04/21
GRPPOLAR07DrpDelA/R                                                               ** 08/04/21
GRPPOLAR07TxbDefTxb1                                                              ** 08/04/21
GRPPOLAR07TxbDefTxb2                                                              ** 08/04/21
GRPPOLAR07TxbDefTxb3                                                              ** 08/04/21
GRPPOLAR07CbxDisRatSplt                                                           ** 08/04/21
GRPPOLAR07RadioPost                                                               ** 08/04/21
GRPPOLAR07RadioDueRec                                                             ** 08/04/21
GRPPOLAR07TxbQtBillAdd1                                                           ** 08/04/21
GRPPOLAR07TxbQtBillAdd2                                                           ** 08/04/21
GRPPOLAR07TxbQtBillCity                                                           ** 08/04/21
GRPPOLAR07DrpQtBillState                                                          ** 08/04/21
GRPPOLAR07TxbQtBillZip                                                            ** 08/04/21
GRPPOLAR08LnkRateSetEdit                                                          ** 08/04/21
GRPPOLAR09LnkActRtStCdStatus                                                      ** 08/04/21
GRPPOLAR09LnkInActRtStCdStatus                                                    ** 09/26/23
GRPPOLAR10BtnRtStAdd                                                              ** 08/04/21
GRPPOLAR11cbxIncludeGroupInvoiceCloud                                             ** 08/04/21
GRPPOLAR11cbxIncludeQuoteBillsInvoiceCloud                                        ** 08/04/21
GRPPOLAR11cbxInvoiceCloudPaymentARorBatch                                         ** 08/04/21
GRPPOLAR11cbxPayasYouGoPaymenttoARorBatch                                         ** 08/04/21
GRPPOLAR11cbxAdhocMakePaymenttoARorBatch                                          ** 08/04/21
GRPPOLAR11cbxInvoiceCloudInactiveInvoicesCX                                       ** 08/04/21
GRPPOLAR12BtnMidTermEndorse                                                       ** 08/04/21
GRPPOLAR12BtnPolPageEndorse                                                       ** 08/04/21
GRPPOLAR13LnkPrmFnCoEdt                                                           ** 08/04/21
GRPPOLAR14LnkPrmFnCoDlt                                                           ** 08/04/21
GRPPOLAR15BtnPrmFnCompAdd                                                         ** 08/04/21
GRPPOLAR16CbxPriorCash                                                            ** 08/04/21
GRPPOLAR16CbxCurCash                                                              ** 08/04/21
GRPPOLAR16CbxRenCash                                                              ** 08/04/21
GRPPOLAR16RadioCurrentCash                                                        ** 08/04/21
GRPPOLAR16RadioBillingCash                                                        ** 08/04/21
GRPPOLAR16CbxPriorAud                                                             ** 08/04/21
GRPPOLAR16CbxCurAudit                                                             ** 08/04/21
GRPPOLAR16CbxRenAudit                                                             ** 08/04/21
GRPPOLAR16RadioCurrentAudit                                                       ** 08/04/21
GRPPOLAR16RadioBillingAudit                                                       ** 08/04/21
GRPPOLAR16CbxPriorEstRev                                                          ** 08/04/21
GRPPOLAR16CbxCurEstRev                                                            ** 08/04/21
GRPPOLAR16CbxRenEstRev                                                            ** 08/04/21
GRPPOLAR16RadioCurrentEst                                                         ** 08/04/21
GRPPOLAR16RadioBillingEst                                                         ** 08/04/21
GRPPOLAR16CbxPriorLock                                                            ** 08/04/21
GRPPOLAR16CbxCurLock                                                              ** 08/04/21
GRPPOLAR16CbxRenLock                                                              ** 08/04/21
GRPPOLAR16RadioCurrentLock                                                        ** 08/04/21
GRPPOLAR16RadioBillingLock                                                        ** 08/04/21
GRPPOLAR16CbxAutoCash                                                             ** 08/04/21
GRPPOLAR16DrpNBPayARCode                                                          ** 08/04/21
GRPPOLAR16DrpNBPayTransCode                                                       ** 08/04/21
GRPPOLAR16DrpDedPayARCode                                                         ** 08/04/21
GRPPOLAR16DrpRetroPayARCode                                                       ** 08/04/21
GRPPOLAR16TransCode                                                               ** 08/04/21
GRPPOLAR16CashCode                                                                ** 08/04/21
GRPPOLAR16DrpNBCash                                                               ** 08/04/21
GRPPOLAR16DrpCashcash                                                             ** 08/04/21
GRPPOLAR16DrpcashAudit                                                            ** 08/04/21
GRPPOLAR16DrpCashSelfRep                                                          ** 08/04/21
GRPPOLAR17LnkAgtRateEdit                                                          ** 08/04/21
GRPPOLAR18LnkActAgtRtStatus                                                       ** 08/04/21
GRPPOLAR18LnkInActAgtRtStatus                                                     ** 09/26/23
GRPPOLAR19BtnAgtRtAdd                                                             ** 08/04/21
GRPPOLAR20TxbCanEff                                                               ** 08/04/21
GRPPOLAR20CbxGenCnfNt                                                             ** 08/04/21
GRPPOLAR20CbxGenAudReq                                                            ** 08/04/21
GRPPOLAR20CbxUpdQtTrk                                                             ** 08/04/21
GRPPOLAR20DrpCnfRmkCd                                                             ** 08/04/21
GRPPOLAR20CbxRslvDoc                                                              ** 08/04/21
GRPPOLAR20CbxAlCnfDt                                                              ** 08/04/21
GRPPOLAR20CbxGen301EndForm                                                        ** 08/04/21
GRPPOLAR20CbxGen311EndForm                                                        ** 08/04/21
GRPPOLAR20CbxPrtS/R                                                               ** 08/04/21
GRPPOLAR20RadioEachDivNotice                                                      ** 08/04/21
GRPPOLAR20RadioStateReportDiv                                                     ** 08/04/21
GRPPOLAR21LnkBilAuxEdt                                                            ** 09/26/23
GRPPOLAR22LnkBilAuxDlt                                                            ** 09/26/23
GRPPOLAR23BtnBilAuxAdd                                                            ** 09/26/23
GRPPOLAR24DrpScoringOverrideVendor                                                ** 09/26/23
GRPPOLAR24TxbVendorEffectiveDate                                                  ** 09/26/23
GRPPOLAR24TxbVendorExpirationDate                                                 ** 09/26/23
MIDENDAR01BtnCopyAll                                                              ** 08/04/21
MIDENDAR02LnkM1Status                                                             ** 08/04/21
MIDENDAR03LnkM1Edit                                                               ** 08/04/21
MIDENDAR04LnkM1Copy                                                               ** 08/04/21
MIDENDAR05LnkM1Delete                                                             ** 08/04/21
ENDFRMAR01DrpSelectAction                                                         ** 08/04/21
ENDFRMAR01imgSelectAction                                                         ** 08/04/21
ENDFRMAR01chkSelectRow                                                            ** 08/04/21
ENDFRMAR02LinkEndrEdit                                                            ** 08/04/21
ENDFRMAR03LinkDelete                                                              ** 08/04/21
ENDFRMAR04BtnEndrIncludeAll                                                       ** 08/04/21
ENDFRMAR05BtnEndrExcludeAll                                                       ** 08/04/21
ENDFRMAR06BtnEndrAdd                                                              ** 08/04/21
ENDFRMAR07LinkAprExclStateEdit                                                    ** 08/04/21
ENDFRMAR08LinkAprExclStateDelete                                                  ** 08/04/21
ENDFRMAR09BtnAprExclSt                                                            ** 08/04/21
TMPLATAR01HypTemplaeEdit                                                          ** 08/04/21
TMPLATAR02HypDeleteTemplates                                                      ** 08/04/21
TMPLATAR03BtnTemplateAdd                                                          ** 08/04/21
CARDFTAR01HypTemplaeEdit
CARDFTAR02HypDeleteTemplates
CARDFTAR03BtnPortalDefaultAdd
GRPCLMAR01LnkRsvCusEdit                                                           ** 08/04/21
GRPCLMAR02BtnRsvCusAdd                                                            ** 08/04/21
GRPCLMAR03TxtAdvance                                                              ** 08/04/21
GRPCLMAR03TxbMaxChk                                                               ** 08/04/21
GRPCLMAR03TxtPrinted                                                              ** 08/04/21
GRPCLMAR03TxtFuture                                                               ** 08/04/21
GRPCLMAR03DrpPayment                                                              ** 08/04/21
GRPCLMAR03DrpLastPayment                                                          ** 08/04/21
GRPCLMAR03ChkAutoFlag                                                             ** 08/04/21
GRPCLMAR04LnkEdit                                                                 ** 08/04/21
GRPCLMAR05LnkDelete                                                               ** 08/04/21
GRPCLMAR06BtnMCOAdd                                                               ** 08/04/21
GRPCLMAR07LnkATLEdit                                                              ** 08/04/21
GRPCLMAR08LnkSCBDelete                                                            ** 08/04/21
GRPCLMAR09BtnACLAdd                                                               ** 08/04/21
GRPCLMAR10DrpSelectAction                                                         ** 09/26/23
GRPCLMAR10imgSelectAction                                                         ** 09/26/23
GRPCLMAR10chkSelectRow                                                            ** 09/26/23
GRPCLMAR11LnkSCBEdit                                                              ** 09/26/23
GRPCLMAR12LnkStCompBenStatus                                                      ** 09/26/23
GRPCLMAR12LnkInStCompBenStatus                                                    ** 09/26/23
GRPCLMAR13LnkSCBDelete                                                            ** 09/26/23
GRPCLMAR14BtnSCBAdd                                                               ** 09/26/23
GRPCLMAR15BtnSCBIncludeAll                                                        ** 09/26/23
GRPCLMAR16BtnSCBExcludeAll                                                        ** 09/26/23
GRPCLMAR17DrpSelectActionforSCBR                                                  ** 09/26/23
GRPCLMAR17imgSelectAction1                                                        ** 09/26/23
GRPCLMAR17chkSelectRowSCBR                                                        ** 09/26/23
GRPCLMAR18LnkSCBREdit                                                             ** 09/26/23
GRPCLMAR19LnkStCompBenByRateStatus                                                ** 09/26/23
GRPCLMAR19LnkInStCompBenByRateStatus                                              ** 09/26/23
GRPCLMAR20LnkSCBRDelete                                                           ** 09/26/23
GRPCLMAR21BtnSCBRAdd                                                              ** 09/26/23
GRPCLMAR22BtnSCBRIncludeAll                                                       ** 09/26/23
GRPCLMAR23BtnSCBRExcludeAll                                                       ** 09/26/23
GRPDTAAR01TxbEffDate                                                              ** 08/04/21
GRPDTAAR01TxbExpDate                                                              ** 08/04/21
GRPDTAAR01CbxNetRate                                                              ** 08/04/21
GRPDTAAR01CbxBlendRate                                                            ** 08/04/21
GRPDTAAR01CbxPolicyHeader                                                         ** 08/04/21
GRPDTAAR01CbxIncEndorse                                                           ** 08/04/21
GRPDTAAR01CbxIncAddDivLoc                                                         ** 08/04/21
GRPDTAAR01CbxDefaultInclude                                                       ** 08/04/21
GRPDTAAR02LnkEDSMEdit                                                             ** 08/04/21
GRPDTAAR03LnkActEDSMStatus                                                        ** 08/04/21
GRPDTAAR03LnkInActEDSMStatus                                                      ** 09/26/23
GRPDTAAR04LnkEDSMDelete                                                           ** 08/04/21
GRPDTAAR05BtnEDSMAdd                                                              ** 08/04/21
GRPSSIAR01LnkStatRatingEdit                                                       ** 08/04/21
GRPSSIAR02LnkStatRatingDelete                                                     ** 08/04/21
GRPSSIAR03LnkStatRatingCopy                                                       ** 08/04/21
GRPSSIAR04BtnStatRatingAdd                                                        ** 08/04/21
GRPSSIAR05LnkPrmDisEdit                                                           ** 08/04/21
GRPSSIAR06LnkPrmDisDlt                                                            ** 08/04/21
GRPSSIAR07BtnPrmDisAdd                                                            ** 08/04/21
GRPSSIAR08LnkExpConEdit                                                           ** 08/04/21
GRPSSIAR09BtnExCoAdd                                                              ** 08/04/21
GRPSSIAR10LnkIncLmtEdit                                                           ** 08/04/21
GRPSSIAR11LnkActIncLmtStatus                                                      ** 08/04/21
GRPSSIAR11LnkInActIncLmtStatus                                                    ** 09/26/23
GRPSSIAR12LnkIncLmtDelete                                                         ** 08/04/21
GRPSSIAR13BtnIncLmtAdd                                                            ** 08/04/21
GRPSSIAR14LnkStRCEdit                                                             ** 08/04/21
GRPSSIAR15LnkSmDeEdit                                                             ** 08/04/21
GRPSSIAR16LnkActSmDeStatus                                                        ** 08/04/21
GRPSSIAR16LnkInActSmDeStatus                                                      ** 09/26/23
GRPSSIAR17LnkSmDeDelete                                                           ** 08/04/21
GRPSSIAR18BtnSmDeAdd                                                              ** 08/04/21
GRPSSIAR19LnkHrExEdit                                                             ** 08/04/21
GRPSSIAR20LnkHrExDelete                                                           ** 08/04/21
GRPSSIAR21BtnHrExAdd                                                              ** 08/04/21
GRPSSIAR22LnkLsCnEdit                                                             ** 08/04/21
GRPSSIAR23BtnLsCnAdd                                                              ** 08/04/21
GRPSSIAR24LnkPolPgDelete                                                          ** 08/04/21
GRPSSIAR25BtnPolPgAdd                                                             ** 08/04/21
GRPSSIAR26LnkExStDelete                                                           ** 08/04/21
GRPSSIAR27BtnExStAdd                                                              ** 08/04/21
GRPSSIAR28LnkCRSDEdit                                                             ** 08/04/21
GRPSSIAR29LnkCRSDDelete                                                           ** 08/04/21
GRPSSIAR30BtnCRSDAdd                                                              ** 08/04/21
GRPSSIAR31LnkSRTEdit                                                              ** 08/04/21
GRPSSIAR32LnkSRTDelete                                                            ** 08/04/21
GRPSSIAR33LnkSRTCopy                                                              ** 08/04/21
GRPSSIAR34BtnSRTAdd                                                               ** 08/04/21
GRPSSIAR35LnkEDCEdit                                                              ** 08/04/21
GRPSSIAR36LnkEndorsControlsStatus                                                 ** 08/04/21
GRPSSIAR36LnkInEndorsControlsStatus                                               ** 09/26/23
GRPSSIAR37LnkEDCDelete                                                            ** 08/04/21
GRPSSIAR38BtnEDCAdd                                                               ** 08/04/21
GRPSSIAR39LnkInFeEdit                                                             ** 08/04/21
GRPSSIAR40BtnInFeAdd                                                              ** 08/04/21
GRPSSIAR41LnkSCBEdit                                                              ** 08/04/21
GRPSSIAR42LnkStCompBenStatus                                                      ** 08/04/21
GRPSSIAR43LnkSCBDelete                                                            ** 08/04/21
GRPSSIAR44BtnSCBAdd                                                               ** 08/04/21
GRPSTRAR01BtnTempltAss
GRPSTRAR01BtnCrRtDft
GRPSTRAR02txtDev#1
GRPSTRAR02txtDev#2
GRPSTRAR02txtCntCrd
GRPSTRAR02txtSbjPrmCrA
GRPSTRAR02txtSbjPrmCrB
GRPSTRAR02txtSchCrdCd
GRPSTRAR02txtSchDbtCd
GRPSTRAR02txtWavSur
GRPSTRAR02txtVolLabChg
GRPSTRAR02txtHdg1
GRPSTRAR02txtHdg2
GRPSTRAR02txtHdg3
GRPSTRAR02txtHdg4
GRPSTRAR02txtHdg5
GRPSTRAR02txtHdg6
GRPSTRAR02txtHdg7
GRPSTRAR02txtHdg8
GRPSTRAR02txtHdg9
GRPSTRAR02CbxAutoRnw1
GRPSTRAR02CbxAutoRnw2
GRPSTRAR02CbxAutoRnw3
GRPSTRAR02CbxAutoRnw4
GRPSTRAR02CbxAutoRnw5
GRPSTRAR02CbxAutoRnw6
GRPSTRAR02CbxARD Split
GRPSTRAR02CbxARD Split2
GRPSTRAR02CbxDisAlChg
GRPSTRAR02CbxDisAlChg2
GRPSTRAR02CbxDefRen
GRPSTRAR02CbxDefRen2
GRPSTRAR02DrpPrmLvl
GRPSTRAR02DrpPrmLvl2
GRPSTRAR02txtDef
GRPSTRAR02txtDef2
GRPSTRAR03txtExpConst
GRPSTRAR03txtMinPrm
GRPSTRAR03txtFacChrg
GRPSTRAR03txtAirCrfSrchg
GRPSTRAR03txtMaxSrChg
GRPSTRAR03DrpPrmLvl3
GRPSTRAR03txtMrtRtDbt
GRPSTRAR03txt2ndRatDbt
GRPSTRAR03txt2ndDbtStCd
GRPSTRAR03txtMrtRatCrd
GRPSTRAR03txt2ndRatCrd
GRPSTRAR03txt2ndCrdStCd
GRPSTRAR03txtMrtRatNt
GRPSTRAR03txtAudNonCmplc
GRPSTRAR03txtANCMul
GRPSTRAR03txtILBal
GRPSTRAR04DrpDedDis
GRPSTRAR04DrpShrtRtPnl
GRPSTRAR04DrpLossCst
GRPSTRAR04TxtLossCstDt
GRPSTRAR04DrpStkDisApp
GRPSTRAR04txtstkDisHead
GRPSTRAR04txtDftDisPrc
GRPSTRAR04DrpStckDis
GRPSTRAR04txtMaxSchDbt
GRPSTRAR04txtMaxSchCrdt
GRPSTRAR04DrpGradientDiscountCalculationLogic                                ** 10/09/23
GRPSTRAR04txtMinimumFlatDiscount                                             ** 10/09/23
GRPSTRAR04txtMinimumScheduleDebit                                            ** 10/09/23
GRPSTRAR04txtMinimumScheduleCredit                                           ** 10/09/23
GRPSTRAR05txtTrrCode
GRPSTRAR05txtRate
GRPSTRAR05txtOptHead1
GRPSTRAR05txtNonPayRt
GRPSTRAR05txtCtsCode
GRPSTRAR05txtCatRate
GRPSTRAR05txtOptHead2
GRPSTRAR06CbxMono
GRPSTRAR06CbxIndStkDis
GRPSTRAR06CbxExcLCM
GRPSTRAR06CbxExcTerr
GRPSTRAR06CbxTerrDiff
GRPSTRAR07txtPercentage
GRPSTRAR07txtMinAmt
GRPSTRAR07DrpAppTo
GRPSTRAR07txtOptHead
GRPSTRAR07txtEndID
GRPSTRAR07TxbSafetyFeeStatCode                                               ** 10/09/23
GRPSTRAR07CbxPrFee
GRPSTRAR07CbxAdjYrEAd
GRPSTRAR07CbxAdjCanAud
GRPSTRAR07txtPerc
GRPSTRAR07txtStTaxHdg
GRPSTRAR07CbxBillDep
GRPSTRAR07CbxBillDed
GRPSTRAR07CbxIncTrgPrm
GRPSTRAR07CbxIncExpcnst
GRPSTRAR07CbxIncDev
GRPSTRAR07DrpWavType
GRPSTRAR07txtWavPrc
GRPSTRAR07txtWavFltChg
GRPSTRAR07txtWavMinChg
GRPSTRAR07DrpLblAppToPrmLvl
GRPSTRAR08CbxLAte
GRPSTRAR08txtFeeAmt
GRPSTRAR08DrpARCode
GRPSTRAR08txtPayRateDev
GRPSTRAR08txtAmt
GRPSTRAR08txtStRptCd
GRPSTRAR08txtOptHead2
GRPSTRAR09LnkTaxEdit1
GRPSTRAR10LnkDelTax
GRPSTRAR11BtnDltAll
GRPSTRAR12BtnEdtAll
GRPSTRAR13TxbLargeDeductible1Description
GRPSTRAR13TxbLargeDeductible1Percentage
GRPSTRAR13BtnLargeDeducType1_Gross
GRPSTRAR13BtnLargeDeducType1_Net
GRPSTRAR13TxbLargeDeductible1DeducAmount
GRPSTRAR13TxbLargeDeductible1AggregateAmount
GRPSTRAR13CbxVarAggregateAmountIndicator1
GRPSTRAR13TxbAggregateDeductible1Percentage
GRPSTRAR13DrpLargeDeductible1Plan
GRPSTRAR13BtnLargeDeducRegular1_PerClaim
GRPSTRAR13BtnLargeDeducRegular1_PerOccurrence
GRPSTRAR13TxbLargeDeductible1StatCode
GRPSTRAR13BtnLargeDeducDisease1_PerClaim
GRPSTRAR13BtnLargeDeducDisease1_PerOccurrence
GRPSTRAR13CbxLargeDeductible1TotalIncurred
GRPSTRAR13DrpLargeDeduc1PremiumLevel
GRPSTRAR13DrpLargeDeductible1Reserve01
GRPSTRAR13DrpLargeDeductible1Reserve02
GRPSTRAR13DrpLargeDeductible1Reserve03
GRPSTRAR13DrpLargeDeductible1Reserve04
GRPSTRAR13DrpLargeDeductible1Reserve05
GRPSTRAR13DrpLargeDeductible1Reserve06
GRPSTRAR13DrpLargeDeductible1Reserve07
GRPSTRAR13DrpLargeDeductible1Reserve08
GRPSTRAR13DrpLargeDeductible1Reserve09
GRPSTRAR13DrpLargeDeductible1Reserve10
GRPSTRAR13CbxLargeDeductible1ALAEIncluded
GRPSTRAR13CbxLargeDeductible1AutoRenew
GRPSTRAR14DrpRatStCde
GRPSTRAR14TxbBillSchCde
GRPSTRAR14DrpLeasingid
GRPSTRAR14txtFlexHead
GRPSTRAR14txtStdLia
GRPSTRAR14DrpGovCls
GRPSTRAR14txtInsCer
GRPSTRAR14txtSignPag
GRPSTRAR14CbxUnReq
GRPSTRAR14CbxNaic
GRPSTRAR14CbxRisk
GRPSTRAR14CbxAutoResbm
GRPSTRAR14CbxStBind
GRPSTRAR14CbxStQte
GRPSTRAR14CbxNcciBnd
GRPSTRAR14CbxNcciQte
GRPSTRAR14CbxExcSt
GRPSTRAR14CbxDebCrd
GRPSTRAR14txtByAccident
GRPSTRAR14txtByDisease
GRPSTRAR14txtDisAgg
GRPSTRAR14DrpDecPag
GRPSTRAR14txtPeoAcc
GRPSTRAR14DrpPeoTyp
GRPSTRAR14drpOverseeingPeoNamedInsured
CLMDTLAR32TxbKnewClaimIsReportable
CLMDTLAR97TxbOverPayAmountCurrent                                                 ** 10/07/15
CLMDTLAR97TxbWithheldAttFeesCurrent                                               ** 10/07/15
CLMDTLAR97CbxNetToZero                                                            ** 10/07/15
CLMDTLAR03TxtEducationLevel
AGTCFGAR01LnkAgencyEdit                                                           ** 06/21/22
AGTCFGAR02LnkChgFEIN                                                              ** 06/21/22
AGTCFGAR03BtnAgencyAdd                                                            ** 06/21/22
AGTPRIAR01txtAgencyName                                                           ** 06/21/22
AGTPRIAR01txtFederalName                                                          ** 06/21/22
AGTPRIAR01txtShortName                                                            ** 06/21/22
AGTPRIAR01TxbAddMailAddress1                                                      ** 06/21/22
AGTPRIAR01TxbAddMailCity                                                          ** 06/21/22
AGTPRIAR01DrpAddMailState                                                         ** 06/21/22
AGTPRIAR01TxbAddMailZip                                                           ** 06/21/22
AGTPRIAR01TxbAddPhyAddress1                                                       ** 06/21/22
AGTPRIAR01TxbAddPhyCity                                                           ** 06/21/22
AGTPRIAR01DrpAddPhyState                                                          ** 06/21/22
AGTPRIAR01TxbAddPhyZip                                                            ** 06/21/22
AGTPRIAR01TxbInternetadd                                                          ** 06/21/22
AGTPRIAR01TxbAddPhone                                                             ** 06/21/22
AGTPRIAR01TxbAddPhone2                                                            ** 06/21/22
AGTPRIAR01TxbAddFaxNumber                                                         ** 06/21/22
AGTPRIAR01TxbEmailadd                                                             ** 06/21/22
AGTPRIAR01TxbContact                                                              ** 06/21/22
AGTPRIAR01DrpServRep                                                              ** 06/21/22
AGTPRIAR01DrpMarketRep                                                            ** 06/21/22
AGTPRIAR01TxbSecretaryName                                                        ** 06/21/22
AGTPRIAR01TxbMarketingContact                                                     ** 06/21/22
AGTPRIAR01DrpMarketingRegion                                                      ** 06/21/22
AGTPRIAR01TxbComments1                                                            ** 06/21/22
AGTPRIAR01TxbComments2                                                            ** 06/21/22
AGTPRIAR01DrpAgtTypCode                                                           ** 06/21/22
AGTPRIAR01DrpAgtGrpCode                                                           ** 06/21/22
AGTPRIAR01TxbW9SentDate                                                           ** 06/21/22
AGTPRIAR01TxbW9ReceivDate                                                         ** 06/21/22
AGTPRIAR01DrpCountyCode                                                           ** 06/21/22
AGTPRIAR01DrpAreaCode                                                             ** 06/21/22
AGTPRIAR01txtAccCurDueDate                                                        ** 06/21/22
AGTPRIAR01txt1099MailtoMod                                                        ** 06/21/22
AGTPRIAR01txtChecksMailtoMod                                                      ** 06/21/22
AGTPRIAR01TxbBkChkCompDate                                                        ** 06/21/22
AGTPRIAR01TxbEOCovExpDate                                                         ** 06/21/22
AGTPRIAR01CbxBackupWithhold                                                       ** 06/21/22
AGTPRIAR01CbxSplAgtProcess                                                        ** 06/21/22
AGTPRIAR01Cbx1099Flag                                                             ** 06/21/22
AGTPRIAR01CbxSS#                                                                  ** 06/21/22
AGTPRIAR01CbxEOCov                                                                ** 06/21/22
AGTPRIAR01CbxExclExport                                                           ** 06/21/22
AGTPRIAR02TxbDate01                                                               ** 06/21/22
AGTPRIAR02TxbDate02                                                               ** 06/21/22
AGTPRIAR02TxbDate03                                                               ** 06/21/22
AGTPRIAR02TxbDate04                                                               ** 06/21/22
AGTPRIAR02TxbDate05                                                               ** 06/21/22
AGTPRIAR02TxbDate06                                                               ** 06/21/22
AGTPRIAR02DrpStaff01                                                              ** 06/21/22
AGTPRIAR02DrpStaff02                                                              ** 06/21/22
AGTPRIAR02DrpStaff03                                                              ** 06/21/22
AGTPRIAR02DrpStaff04                                                              ** 06/21/22
AGTPRIAR02DrpStaff05                                                              ** 06/21/22
AGTPRIAR02DrpStaff06                                                              ** 06/21/22
AGTPRIAR03TxbSizeMB                                                               ** 09/26/23
AGTPRIAR03TxbCount                                                                ** 09/26/23
AGTPRIAR03TxbSite                                                                 ** 09/26/23
AGTPRIAR03TxbFtpDirectory                                                         ** 09/26/23
AGTPRIAR03TxbUserName                                                             ** 09/26/23
AGTPRIAR03TxbPassword                                                             ** 09/26/23
AGTASNAR01LnkAsnEdit                                                              ** 06/21/22
AGTASNAR02LnkAsnCopy                                                              ** 06/21/22
AGTASNAR03BtnAsnAdd                                                               ** 06/21/22
AGTASNAR04LnkQteTrckEdit                                                          ** 06/21/22
AGTASNAR05LnkQteTrckStatus                                                        ** 06/21/22
AGTASNAR06LnkEftInfoEdit                                                          ** 06/21/22
AGTASNAR07LnkEFTBankInfo                                                          ** 06/21/22
AGTASNAR08LnkRqPNote                                                              ** 06/21/22
AGTASNAR09BtnEftInfoAdd                                                           ** 06/21/22
AGTPROAR01LnkProdEdit                                                             ** 06/21/22
AGTPROAR02BtnProdAdd                                                              ** 06/21/22
AGTPROAR03LnkNREdt                                                                ** 06/21/22
AGTPROAR04LnkActNRStatus                                                          ** 09/26/23
AGTPROAR04LnkInActNRStatus                                                        ** 09/26/23
AGTPROAR05LnkNRDelete                                                             ** 06/21/22
AGTPROAR06BtnNRAdd                                                                ** 06/21/22
AGTPROAR07LnkActProdStatus                                                        ** 09/26/23
AGTPROAR07LnkInActProdStatus                                                      ** 09/26/23
AGTCONAR01LnkAgtConEdit                                                           ** 06/21/22
AGTCONAR02LnkAgtConDlt                                                            ** 06/21/22
AGTCONAR03LnkActAgtConSts                                                         ** 09/26/23
AGTCONAR03LnkInActAgtConSts                                                       ** 09/26/23
AGTCONAR04BtnAgtConAdd                                                            ** 06/21/22
AGTCONAR05TxbAgtLastName                                                          ** 06/21/22
AGTCONAR05TxbAgtFirstName                                                         ** 06/21/22
AGTCONAR05TxbNameM/I                                                              ** 06/21/22
AGTCONAR05TxbAgtTitle                                                             ** 06/21/22
AGTCONAR05DrpAgtConStatus                                                         ** 06/21/22
AGTCONAR05TxbAgtPosition                                                          ** 06/21/22
AGTCONAR05DrpConType                                                              ** 06/21/22
AGTCONAR05RadioPrimaryPhnPhone#                                                   ** 06/21/22
AGTCONAR05RadioPrimaryPhnCell#                                                    ** 06/21/22
AGTCONAR05TxbAgtAddress1                                                          ** 06/21/22
AGTCONAR05TxbAgtAddress2                                                          ** 06/21/22
AGTCONAR05TxbAgtCity                                                              ** 06/21/22
AGTCONAR05DrpAgtState                                                             ** 06/21/22
AGTCONAR05TxbAgtZip                                                               ** 06/21/22
AGTCONAR05TxbAgtPhone                                                             ** 06/21/22
AGTCONAR05TxbAgtExtn                                                              ** 06/21/22
AGTCONAR05TxbAgtCellPhone                                                         ** 06/21/22
AGTCONAR05TxbFaxNumber                                                            ** 06/21/22
AGTCONAR05TxbEmailAddress                                                         ** 06/21/22
AGTCONAR06DrpProducers                                                            ** 06/21/22
AGTCONAR07DrpDisTyp                                                               ** 06/21/22
AGTCONAR08DrpSubCat                                                               ** 06/21/22
AGTCONAR09DrpOutCont                                                              ** 06/21/22
POLCNTAR01Button3                                                                 ** 09/26/22
POLCNTAR01Hyperlink1                                                              ** 09/26/22
POLCNTAR01Hyperlink2                                                              ** 09/26/22
POLCNTAR02chkDivSpCon                                                             ** 09/26/22
POLCNTAR02DrpDivi                                                                 ** 09/26/22
POLCNTAR02SelectBox1                                                              ** 09/26/22
POLCNTAR02TextBox3                                                                ** 09/26/22
POLCNTAR02TextBox3_copy                                                           ** 09/26/22
POLCNTAR02TextBox3_copy2                                                          ** 09/26/22
POLCNTAR02TextBox3_copy3                                                          ** 09/26/22
POLCNTAR02TextBox3_copy4                                                          ** 09/26/22
POLCNTAR02TextBox4                                                                ** 09/26/22
POLCNTAR02TextBox4_copy                                                           ** 09/26/22
POLCNTAR02TextBox4_copy2                                                          ** 09/26/22
POLCNTAR02TextBox4_copy3                                                          ** 09/26/22
POLCNTAR02TextBox4_copy4                                                          ** 09/26/22
POLCNTAR02TextBox4_copy5                                                          ** 09/26/22
POLCNTAR02TextBox1_copy4                                                          ** 09/26/22
POLCNTAR02TextBox1                                                                ** 09/26/22
POLCNTAR02TextBox1_copy6                                                          ** 09/26/22
POLCNTAR02TextBox1_copy                                                           ** 09/26/22
POLCNTAR02TextBox1_copy2                                                          ** 09/26/22
POLCNTAR02SelectBox1_copy                                                         ** 09/26/22
POLCNTAR02TextBox1_copy3                                                          ** 09/26/22
POLCNTAR02chkSREmail                                                              ** 09/26/22
POLCNTAR03DrpDisTyp                                                               ** 09/26/22
POLCNTAR04DrpSubCat                                                               ** 09/26/22
POLCNTAR05DrpOutCont                                                              ** 09/26/22
AGTCNTAR01TxbAgtLastName                                                          ** 09/26/22
AGTCNTAR01TxbAgtFirstName                                                         ** 09/26/22
AGTCNTAR01TxbNameM/I                                                              ** 09/26/22
AGTCNTAR01TxbAgtTitle                                                             ** 09/26/22
AGTCNTAR01DrpAgtConStatus                                                         ** 09/26/22
AGTCNTAR01TxbAgtPosition                                                          ** 09/26/22
AGTCNTAR01DrpConType                                                              ** 09/26/22
AGTCNTAR01RadioPrimaryPhnPhone#                                                   ** 09/26/22
AGTCNTAR01RadioPrimaryPhnCell#                                                    ** 09/26/22
AGTCNTAR01TxbAgtAddress1                                                          ** 09/26/22
AGTCNTAR01TxbAgtPhone                                                             ** 09/26/22
AGTCNTAR01TxbEmailAddress_copy                                                    ** 09/26/22
AGTCNTAR01TxbAgtCellPhone                                                         ** 09/26/22
AGTCNTAR01TxbFaxNumber                                                            ** 09/26/22
AGTCNTAR01TxbAgtAddress2                                                          ** 09/26/22
AGTCNTAR01TxbAgtCity                                                              ** 09/26/22
AGTCNTAR01DrpAgtState                                                             ** 09/26/22
AGTCNTAR01TxbAgtZip                                                               ** 09/26/22
AGTCNTAR01TxbEmailAddress                                                         ** 09/26/22
AGTCNTAR02DrpProducers                                                            ** 09/26/22
AGTCNTAR03DrpDisTyp                                                               ** 09/26/22
AGTCNTAR04DrpSubCat                                                               ** 09/26/22
AGTCNTAR05DrpOutCont                                                              ** 09/26/22
CLMDTLAR62BtnAddSettle                                                            ** 12/06/22
ARBTCHAR01LnkARBtchEdit                                                           ** 03/20/23
ARBTCHAR02LnkARBtchDelete                                                         ** 03/20/23
ARBTCHAR03BtnARBatchAdd                                                           ** 03/20/23
ARBTCHAR04BtnBatchIncludeAll                                                      ** 03/20/23
ARBTCHAR05BtnBatchExcludeAll                                                      ** 03/20/23
ARBTCHAR08LnkBtchEdit                                                             ** 03/20/23
ARBTCHAR09LnkBtchDelete                                                           ** 03/20/23
ARBTCHAR10BtnBtchAdd                                                              ** 03/20/23
FROILIST01LnkFroiAdd                                                              ** 08/14/23
FROILIST02HypDisplayFroi                                                          ** 08/14/23
FROILIST03HypDeleteFroi                                                           ** 08/14/23
CXBTCHAR01LnkCXBtchIssue                                                          ** 09/20/23
CXBTCHAR02LnkCXBtchDelete                                                         ** 09/20/23
CXBTCHAR03BtnCXBatchAdd                                                           ** 09/20/23
CXBTCHAR04DrpSelectAction                                                         ** 09/20/23
CXBTCHAR04imgSelectAction                                                         ** 09/20/23
CXBTCHAR04chkSelectRow                                                            ** 09/20/23
CXBTCHAR05LnkCXDEdit                                                              ** 09/20/23
CXBTCHAR06LnkCXDDelete                                                            ** 09/20/23
CXBTCHAR07BtnCXDIncludeAll                                                        ** 09/20/23
CXBTCHAR08BtnCXDExcludeAll                                                        ** 09/20/23
CXBTCHAR09LnkAmtDue                                                               ** 09/20/23
CXBTCHAR09LnkCXDEmpInfo                                                           ** 09/20/23
CXBTCHAR09LnkCXDARinfo                                                            ** 09/20/23
CXBTCHAR09LnkCXDHistory                                                           ** 09/20/23
CXBTCHAR09LnkCXDFinInfo                                                           ** 09/20/23
ENDFRMAR01LnkEndFrmEdit                                                           ** 03/22/24
ENDFRMAR02LnkEndFrmAddInfo                                                        ** 03/22/24
ENDFRMAR03BtnFormIncludeAll                                                       ** 03/22/24
ENDFRMAR04BtnFormExcludeAll                                                       ** 03/22/24
ENDFRMAR05BtnEndFrmReattach                                                       ** 03/22/24
ENDFRMAR06BtnEndFrmAdd                                                            ** 03/22/24
RFBTCHAR01LnkRFBtchDelete                                                         ** 03/27/24
RFBTCHAR02BtnRFBatchAdd                                                           ** 03/27/24
RFBTCHAR03BtnBatchIncludeAll                                                      ** 03/27/24
RFBTCHAR04BtnBatchExcludeAll                                                      ** 03/27/24
RFBTCHAR05LnkRFDEdit                                                              ** 03/27/24
RFBTCHAR06LnkRFDDelete                                                            ** 03/27/24
RFBTCHAR07BtnRFBatchDtlAdd                                                        ** 03/27/24
RFBTCHAR08BtnRFDIncludeAll                                                        ** 03/27/24
RFBTCHAR09BtnRFDExcludeAll                                                        ** 03/27/24
RFCHKAR01 LnkRFCVoid                                                              ** 03/27/24
RFCHKAR02 LnkRFCStop                                                              ** 03/27/24
RFCHRQAR01LnkRFCReqEdit                                                           ** 03/27/24
RFCHRQAR02LnkRFCReqDlt                                                            ** 03/27/24
RFCHRQAR03BtnRFCReqIncludeAll                                                     ** 03/27/24
RFCHRQAR04BtnRFCReqExcludeAll                                                     ** 03/27/24
RFCHRQAR05BtnRFCRWriteChk                                                         ** 03/27/24
RFCHRQAR06BtnRFCRPrtRpt                                                           ** 03/27/24
RFCHRQAR07chkSelectRow                                                            ** 03/27/24
RFCHRQAR07DrpSelectAction                                                         ** 03/27/24
RFCHRQAR07imgSelectAction                                                         ** 03/27/24
CLMCONAR01LnkContEdt                                                              ** 04/01/24
CLMCONAR02LnkContDlt                                                              ** 04/01/24
CLMCONAR03BtnContAdd                                                              ** 04/01/24
DSTTYPAR01LnkEdit                                                                 ** 04/01/24
DSTTYPAR02LnkDel                                                                  ** 04/01/24
DSTTYPAR03BtnDstAdd                                                               ** 04/01/24
DSTTYPAR01LnkConEdit                                                              ** 04/01/24
DSTTYPAR02LnkConDel                                                               ** 04/01/24
DSTTYPAR03BtnConAdd                                                               ** 04/01/24
GRPPOLAR25CbxOfferCollateral                                                      ** 06/24/24
GRPPOLAR25CbxApplyCollateral                                                      ** 06/24/24
GRPPOLAR25TxbWrtPercent                                                           ** 06/24/24
GRPPOLAR25TxbRepPercent                                                           ** 06/24/24
GRPPOLAR25DrpCollBillingA/R                                                       ** 06/24/24
GRPPOLAR25DrpCollPaymentA/R                                                       ** 06/24/24
GRPPOLAR25DrpCollTransferA/R                                                      ** 06/24/24
GRPPOLAR25DrpCollBillOffsetA/R                                                    ** 06/24/24
GRPPOLAR25DrpCollDedPayTransA/R                                                   ** 06/24/24
POLDTLAR22TxbWtnPercent                                                           ** 06/24/24
POLDTLAR22TxbWtnFC                                                                ** 06/24/24
POLDTLAR22TxbRptPercent                                                           ** 06/24/24
POLDTLAR22DrpCollateType                                                          ** 06/24/24
POLDTLAR22CbxCollateToDed                                                         ** 06/24/24
POLDTLAR22TxbLOCIssueBank                                                         ** 06/24/24
POLDTLAR22TxbLOCNumber                                                            ** 06/24/24
POLDTLAR22TxbCollateIssDt                                                         ** 06/24/24
POLDTLAR22TxbCollateExDt                                                          ** 06/24/24
POLDTLAR22TxbSuretyIssCar                                                         ** 06/24/24
POLDTLAR22TxbSuretyPolNo                                                          ** 06/24/24
POLDTLAR22TxbBondIssDt                                                            ** 06/24/24
POLDTLAR22TxbBondExDt                                                             ** 06/24/24
POLDTLAR03CbxNotFinal1                                                            ** 08/05/24
POLDTLAR03CbxNotFinal2                                                            ** 08/05/24
POLDTLAR03CbxNotFinal3                                                            ** 08/05/24
POLDTLAR23SelExperienceRating                                                     ** 08/05/24
POLDTLAR23CbxReportToNCCI                                                         ** 08/05/24
