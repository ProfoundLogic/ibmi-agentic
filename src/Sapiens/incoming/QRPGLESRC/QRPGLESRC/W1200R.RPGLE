     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
      /title W1200R - A/R Batch Entry
     h option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *** beg chg ***** 05/04/21 **************************
     h debug(*yes) bnddir('WTBNDDIR')
      *** end chg ***** 05/04/21 **************************
     H*****************************************************************
     H*  Created:                                                     *
     H*  01/16/98  C:69  Project: WC753                               *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 04/30/24 C:300 Project: WC2675
     H*                This program is no longer in use.
     H*                Pass-through this program to new screen.
     H*                Add extra entry parm to avoid Pass-through of old screen
     H*
     H* 07/29/22 C:340 Support: 103003
     H*                PayGo IVC
     H*
     H* 12/06/21 C:136 Support: 99810
     H*                Update EFT edit for starting policy year value
     H*
     H* 05/24/21 C:180 Support 95396
     C*                Prevent batch deletion while in use
     H*
     H* 05/18/21 C:295 Support: 94788 (SUNZ)
     H*                Do not allow options 8 (Flag for approved)  and
     H*                option 9 (Flag for transfer) for deleted employer.
     H*                In DSPF: Added Error indictor 80 on SCR01C record
     H*
     H* 07/29/20 C:300 Support: 90307 (CoveRisk)
     H*                Decimal data error in 'Posotion To' field
     H*
     H* 11/07/19 C:136 Project: SY407 (DE573)
     H*                Add 3rd Party transaction definitions for
     H*                display in grid
     H*
     H* 06/08/18 C:136 Project: 150WAQ
     H*                Add S/R Online and Collateral Flag to
     H*                grid and subset.
     H*
     H* 05/01/18 C:291 Support: PHW63926                              *
     H*                Fixed the issue with Subset to Transferred     *
     H*                batches and Change view #2.                    *
     H*                                                               *
     H* 10/31/16 C:136 Support: 44007                                 *
     H*                Give detailed information on which policy      *
     H*                doesn't have valid EFT set up                  *
     H*                                                               *
     H* 02/05/16 C:136 Support: 37550                                 *
     H*                Use Company Maintenance for EFT Batch Security *
     H*                                                               *
     H* 08/06/13 C:180 Support  WA146 83021                           *
     H*                Allow batch to be unapproved after EFT has     *
     H*                been submitted for Client 146.                 *
     H* 02/15/13 C:136 Support: WA133 81727                           *
     H*                Don't allow batch to be un-approved after      *
     H*                EFT has been submitted                         *
     H*                                                               *
     H* 01/05/12 C:180 SUPPORT  WC139 78871
     H*                Provide more detail on error message
     H*                WC30706 so user can correct error.
     H*
     H* 09/06/11 C:277 SIPPORT: 77890                                 *
     H*                Change the ACH file to EFT file.               *
     H*                Fix already applied under Project 114WYI       *
     H*                dated : 04/21/11                               *
     H*                                                               *
     H* 08/31/11 C:136 Support: WA099 77856                           *
     H*                EFT status needs to be checked as of the       *
     H*                trans date, not the post date                  *
     H*                                                               *
     H* 05/20/11 C:189 Project: WC1948 Priss 12                       *
     H*                If a user attempts to flag an ACH-type A/R     *
     H*                Batch for "Transfer", and the ACH batch option *
     H*                has not been run, send the error message.      *
     H*                                                               *
     H* 04/21/11 C:190 Project: 114WYI                                *
     H*                Load "EFT" instead of "ACH" when EAACHF= 'A'   *
     H*                                                               *
     H* 02/15/11 C:061 Project: WC1851                                *
     H*               -Add 'F'ee to possible A/R Batch Subset types.  *
     H*               -Convert to RPGIV.                              *
     H*                                                               *
     H* 01/13/11 C:257 Project: WC1948                                *
     H*                Do not allow options 2 (Change),4 (Delete) or  *
     H*                13 (Mass Update) if either the flag for        *
     H*                approved or the flag for transfer is set to "Y"*
     H*                Also allow option 12 if both flags are "Y"     *
     H*                                                               *
     H* 09/22/10 C:255 Project: SY190 (PRISS 189)                     *
     H*                Used Indicator 30 in place of Indicator 69.    *
     H*                                                               *
     H* 08/09/10 C:191 Project: 71WKU                                 *
     H*                Recognize Retro Credit Batches                 *
     H*                                                               *
     H* 05/31/10 C:255 Project: WC1806 (PRISS 47)                     *
     H*                Commented code of checking existence of        *
     H*                WXDB file.                                     *
     H*                Validate if all records in ACH batch have      *
     H*                valid ACH setup                                *
     H*                                                               *
     H* 03/25/10 C:33  Project: WC1838                                *
     H*                Add deductible batch type.                     *
     H*                                                               *
     H* 06/29/09 C:191 PROJECT: WC1806                                *
     H*                ADD EDIT - OPTION 12 SHOULD BE ALLOWED ONLY    *
     H*                ONCE PER BATCH                                 *
     H*                                                               *
     H* 03/28/08 C:65  PROJECT: CI019                                 *
     H*                ENABLE OPTION 26 FOR ACH IF ONLINE PROCESSING. *
     H*                                                               *
     H* 08/09/07 C:65  PROJECT:  WC1324                               *
     H*                DISPLAY 'LB' FOR LOCKBOX IN TYPE COLUMN IF     *
     H*                BATCH IS FLAGGED AS A LOCKBOX TYPE.            *
     H*                                                               *
     H* 06/28/07 C:136 SUPPORT: WA132 58967                           *
     H*                REMOVE EDIT IMPLEMENT BY SUPPORT ITEM 48463    *
     H*                PER GW AND CG                                  *
     H*                                                               *
     H* 01/16/07 C:136 SUPPORT: WA045 54308                           *
     H*                EDIT FOR TRANSFER BATCHES (*INKV = *ON)        *
     H*                STICKS IN LOOP.                                *
     H*                                                               *
     H* 12/19/06 C:33  Seq#: WA129/50740                              *
     H*                Recompile due to change in 'Position to' fields*
     H*                                                               *
     H* 09/07/06 C:136 SUPPORT: WA129 54532                           *
     H*                REMOVE EDIT ADDED ON 02/23/06 FOR FCC.         *
     H*                                                               *
     H* 04/07/06 C:136 SUPPORT: WA109 52148                           *
     H*                ADD EDIT ON TRANSFER APPROVAL OPTION THAT      *
     H*                WILL PREVENT A CASH DISSOLVE BATCH FROM BEING  *
     H*                APPROVED IF ANY OF THE RECORDS HAVE NOT BEEN   *
     H*                DISSOLVED.                                     *
     H*                                                               *
     H* 02/23/06 C:136 SUPPORT: WA129 48463                           *
     H*                ADD EDIT WINDOW TO TRANSFER BATCHES THAT       *
     H*                WILL DISPLAY IF ANY BATCH SELECTED FOR         *
     H*                TRANSFER HAS A TRANSACTION DATE IN A PREVIOUS  *
     H*                MONTH.                                         *
     H*                                                               *
     H* 10/20/05 C:053 PROJECT: WC1324                                *
     H*                Add SR logic.                                  *
     H*                                                               *
     H* 04/07/05 C:156 PROJECT: 126WDK
     H*                PROJECT: 126WAL                                *
     H*                ADD OPTION FOR MASS UPDATE OF CHECK# AND       *
     H*                POSTING DATE.                                  *
     H*                                                               *
     H* 03/03/05 C:062 PROJECT: WC1229                                *
     H*                Add group restriction logic.                   *
     H*                                                               *
     H* 02/22/05 C:062 PROJECT: 129WJO                                *
     H*                Check ACH Batch Security.                      *
     H*                                                               *
     H* 07/19/04 C:136 SUPPORT: WA129 43241                           *
     H*                Make the fields that were replaced by SCDATA   *
     H*                hidden so that the ERRCK1 validations would    *
     H*                work properly. Change spacing on VIEW2 so      *
     H*                that the negative sign on the amount entered   *
     H*                field wouldn't get chopped off.                *
     H*                                                               *
     H* 07/08/04 C:132 SUPPORT:  WA090  43096                         *
     H*                Correct batch number on Edit submit option 11  *
     H*                                                               *
     H* 06/16/04 C:136 SUPPORT:  WC1190                               *
     H*                Correct heading when WCFL24 = *off.            *
     H*                                                               *
     H* 04/13/04 C:132 PROJECT:  WC1190                               *
     H*                Modify iniital screen to display A/R batches   *
     H*                in Descending order by batch number. Provide   *
     H*                positon to a batch by date. Add F11-Change     *
     H*                view by date create/transfered.                *
     H*                                                               *
     H* 10/27/03 C:136 SUPPORT: 39709 (WC1116)                        *
     H*                CORRECT EDIT FOR LIMITING ACH BATCH CREATION   *
     H*                TO USE INITIAL DATE FIELD.                     *
     H*                                                               *
     H* 09/24/03 C:136 SUPPORT: 39075 (WC1116)                        *
     H*                ADD CODING FOR NEW 'INITIAL' MAINTENANCE       *
     H*                FIELDS.                                        *
     H*                                                               *
     H* 06/30/03 C:136 PROJECT: WC1116                                *
     H*                CHANGES ON 06/09/03 DELETED. COPYCODE CHANGED  *
     H*                TO CHAIN TO PHYSICAL WHEN USING AN OPTION.     *
     H*                PASS ADDITIONAL PARM ON OPT 12 TO HOLD DATE    *
     H*                BATCH CREATED.                                 *
     H*                                                               *
     H* 06/09/03 C:136 PROJECT: WC1116                                *
     H*                CHAIN TO PHYSICAL WHEN USING OPT 12.           *
     H*                                                               *
     H* 04/11/03 C:136 PROJECT: WC1116                                *
     H*                IF ACH PROCESSING ENABLED                      *
     H*                - ADD OPTION 12=CREATE ACH FILE                *
     H*                - ADD ACH COLUMN                               *
     H*                - ADD SUBSET OPTION                            *
     H*                - ADD EDITS ON OPTION 12                       *
     H*                                                               *
     H* 12/18/02 C:132 SUPPORT: WA099 334269                          *
     H*                CHANGE SCRR1 WHEN ENTER OTHFKS TO CORRECT IF   *
     H*                HAD PAGED DOWN AND PAGED BACK UP-AFTER F KEY   *
     H*                PRESSED, GOES TO LAST PAGED DOWN BATCH #       *
     H*                                                               *
     H* 11/20/02 C:136 SUPPORT: WA071 34934                           *
     H*                CHANGE SCREEN FILE TO ACCOMODATE THE AMOUNT    *
     H*                ENTERED AND AMOUNT TO BE POSTED FIELDS         *
     H*                BEING CHANGED FROM A 9.2 TO AN 11.2.           *
     H*                                                               *
     H* 04/19/02 C:136 PROJECT: WC577                                 *
     H*                CHANGE SCREEN FILE TO ACCOMODATE THE BATCH AMT *
     H*                FIELD BEING CHANGED FROM A 9.2 TO AN 11.2      *
     H*                                                               *
     H* 08/25/99  C:35   PROJECT: SUPPORT WA100 #19365                *
     H*                  IF A BATCH HAD ALREADY BEEN TRANSFERRED, BUT *
     H*                  THE SCREEN HADN'T BEEN REFRESHED, THE USER   *
     H*                  WAS ABLE TO ACCESS THE CHANGE, APPROVE AND   *
     H*                  TRANSFER OPTIONS.  CHANGED TO CHAIN TO THE   *
     H*                  FILE AND CHECK STATUS.                       *
     H*                                                               *
     H* 08/12/99 C:113 SUPPORT:                                       *
     H*                Changed to send user parameter outq to A/R     *
     H*                Batch Transfer and Edit programs.              *
     H*                                                               *
     H*  03/29/99  C:69  Support: 17360                               *
     H*                  Corrected the page up processing             *
     H*                                                               *
     H*  09/16/98  C:09  PROJECT: WC753  (WA091/SEQ# 13802)           *
     H*                  ADDRESS ISSUES WITH A/R TRANSFER RUNNING AND *
     H*                  USERS CHANGING FLAG FOR TRANSFER APPROVAL.   *
     H*                  (ALSO SEE CHANGES IN PROGRAM W9206C  ).      *
     H*                                                               *
     H*  09/09/98  C:69  Per: JJ (via CG)                             *
     H*                  Corrected a rolling problem by changing the  *
     H*                  SF1PAG value.                                *
     H*  06/08/98  C:33  PER: JJ                                      *
     H*                  CORRECT TOTAL # OF CHECKS AND EMPLOYER TOTAL *
     H*                                                               *
     H*  05/05/98  C:33  SEQ#: WA091/11440                            *
     H*                 -BE SURE THE TOTALS ARE LOADED TO THE SCREEN  *
     H*                  CORRECTLY.                                   *
     H*****************************************************************
     FW1200F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDEAP     IF   E             DISK
     F                                     RENAME(PEA01:PFRCDF)
     F*
      *** Begin Add *** 05/24/21 ************************************
     F                                     INFDS(FIDS02)
      *** End   Add *** 05/24/21 ************************************
     F*  Primary logical file definition (for displaying items)
      ***** BEG ADD ***** 04/13/04 ************************************
     F*******WDEAL   IF  E           K        DISK
     F* CO#/STAT/BAT#(DESCENDING)/CHK#/CHAM/DT/TIME
     FWDEAL9    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEA01:DBRCDF)
     F*
     F* CO#/STAT/IDT(DESCENDING)/CHK#/CHAM/DT/TIME
     FWDEALA    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEA01:DBRCD2)
     F*
     F* CO#/STAT/TFDT(DESCENDING)/CHK#/CHAM/DT/TIME
     FWDEALB    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PEA01:DBRCD3)
      ***  BEG ADD ***** 02/23/06 *************************************
     FWDEAL8    IF   E           K DISK
     F                                     RENAME(PEA01:PEA0L8)
      ***  END ADD ***** 02/23/06 *************************************
      ***  BEGIN CHG  ***  04/07/05 ***********************************
     F* FUND DESCRIPTION
     FWMFNL     IF   E           K DISK
      ***  END   CHG  ***  04/07/05 ***********************************
      ***** END ADD ***** 04/13/04 ************************************
     F*
     F***  BEGIN ADD  ***  05/05/98  *********************************
     FWTABL     IF   E           K DISK
     F***  END   ADD  ***  05/05/98  *********************************
     F*** BEGIN ADD *** 08/12/99 ***********************************************
     FWMUPL     IF   E           K DISK
     F*** END   ADD *** 08/12/99 ***********************************************
     F*** BEG ADD ***** 04/11/03 ***********************************************
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
     F*** END ADD ***** 04/11/03 ***********************************************
     F*** BEGIN ADD *** 02/22/05 ***********************************************
      *** beg del ***** 02/05/16 ***********************************************
     F*** WMCBL     IF   E           K DISK
      *** end del ***** 02/05/16 ***********************************************
     F*** END   ADD *** 02/22/05 ***********************************************
      *** BEG ADD *** 10/20/05 ************************************
     FSMCOL     IF   E           K DISK
      *** END ADD *** 10/20/05 ************************************
     F********* Beg Add ******** 05/18/21 ***********************************
     FWMEML     IF   E           K DISK    RENAME(PEM01:PEM0L)
     F********* End Add ******** 05/18/21 ***********************************
       //*** Begin Add *** 05/24/21 *****************************************
       Dcl-F WXRIL disk usage(*update) keyed;
       Dcl-F WXRIL3 disk usage(*update) keyed rename(PRI01:PRI03) prefix(L3);
       Dcl-Ds KeyWXRIL  likerec(PRI01:*KEY);
       Dcl-Ds KeyWXRIL3 likerec(PRI03:*KEY);
       Dcl-Ds FIDS02 ;
          dbrrnWdeap int(10) pos(397);
       End-Ds;
       //*** End   Add *** 05/24/21 *****************************************
      ***** BEG ADD ******* 02/15/11 **********************************
     dSubsetPrompts    pr
      ***** END ADD ******* 02/15/11 **********************************
     D*
     F*** BEG ADD ***** 04/11/03 ***********************************************
     D******************  HD      1   1 78               Column headings
     D******************  OF      1  18  1   OFD    35   Options, Function
      ***** BEG CHG ***** 04/13/04 ************************************
     D***************     HD      1   2 79               Column headings
      *** Beg Chg ***** 06/08/17 ************************************
     D*HD****          S             79    DIM(8) CTDATA PERRCD(1)              Column headings
     D HD              S             90    DIM(8) CTDATA PERRCD(1)              Column headings
      *** End Chg ***** 06/08/17 ************************************
     D**************      OF      1  19  1   OFD    35   Options, Function
     D*** BEGIN CHG ***  04/07/05  ************************************
     D*********           OF      1  20  1   OFD    35   Options, Function
     D OF              S              1    DIM(21) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(21) ALT(OF)
     D*** END CHG *****  04/07/05  ************************************
      ***** END CHG ***** 04/13/04 ************************************
     F*** END ADD ***** 04/11/03 ***********************************************
      ***** BEG ADD ******* 02/15/11 **********************************
     d start           s              3  0
      ***** END ADD ******* 02/15/11 **********************************
     D*
     D*****/COPY SCOPYWW01I
     D*
     D                 DS
     D  QB                     1      9
     D                                     DIM(9)                               BATCH #
     D  QQBAT#                 1      9
      *
      ***** BEG ADD ***** 04/13/04 ************************************
     D                 DS
     D  VIEW1                  1     72
     D  XXBAT#                 1      9
     D  XXBATA                11     25
     D  XXAMTE                27     41
     D  XXAMTP                43     57
     D  XXTFAP                59     59
     D  XXTFFL                63     63
     D  XXTFDT                65     72
     D                 DS
     D  VIEW2                  1     72
     D  X2TFDT                 1      8
     D  X2BAT#                10     18
     D  X2BATA                20     34
      ***** BEG CHG ***** 07/19/04 ************************************
     D*************                          36  49 X2AMTE
     D*************                          51  65 X2AMTP
     D*************                          68  68 X2TFAP
     D*************                          72  72 X2TFFL
     D  X2AMTE                36     50
     D  X2AMTP                51     65
     D  X2TFAP                67     67
     D  X2TFFL                71     71
      ***** END CHG ***** 07/19/04 ************************************
      ***** END ADD ***** 04/13/04 ************************************
      *** BEG ADD ***** 02/23/06 **************************************
     D                 DS
     D  EATRDT                 1      7  0
     D  EAYYYM                 1      5  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *** END ADD ***** 02/23/06 **************************************
     D*** BEGIN ADD ****** 06/29/09 ************************************
     D                 DS
     D  XXABAT                 1     30
     D  XXJAST                 1      1
     D  XXJUSR                 2     11
     D  XXJBAT                12     20  0
     D*** END   ADD ****** 06/29/09 ************************************
     D*** BEGIN ADD ****** 01/05/12 ************************************
     D                 DS
     D  XXEMP                  1     27
     D  XXCO                   1      3
     D  XXB1                   4      4
     D  XXFND                  5      7
     D  XXB2                   8      8
     D  XXEMP#                 9     17
     D  XXB3                  18     18
     D  XXDIV                 19     23
     D  XXB4                  24     24
     D  XXFYR                 25     27
     D*** END   ADD ****** 01/05/12 ************************************
      ***** BEG ADD ******* 02/15/11 **********************************

     D typDds          ds
     D  ssar01
     D  ssar02
     D  ssar03
     D  ssar04
     D  ssar05
     D  ssar06
    D typD                                OVERLAY(typDds)
     D                                     LIKE(ssar01) DIM(6)
      ***** END ADD ******* 02/15/11 **********************************
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('A/R Batches')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1200R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
      ***** BEG ADD ***** 04/13/04 ************************************
     D CHGNUM          C                   CONST('SA020R')
      ***** END ADD ***** 04/13/04 ************************************
     D INFOMX          C                   CONST(1)
      ***** BEG CHG ***** 04/13/04 ************************************
     D**********    1                     C         VIEWMX
     D VIEWMX          C                   CONST(2)
      ***** END CHG ***** 04/13/04 ************************************
     D SF1PAG          C                   CONST(10)
     D*
     D*  Other constants
     D PURGE           C                   CONST('A/R Batch Transfer P-
     D                                     urge')
      *** beg add ***** 12/06/21 *****************
     d/copy wPrBank
     d/copy sPrGenPop
       Dcl-S chrChkType Char(10);
       Dcl-S chrEftType Char(1) Inz('E');
       Dcl-S chrEftKey  Char(30);
       Dcl-S chrEfDt Char(7);
       Dcl-S chrEobType Char(3);
       Dcl-S chrCo# Char(3);
       Dcl-S chrFnd Char(3);
       Dcl-S chrFyr Char(3);
       Dcl-S chrPre Char(1);
       Dcl-S chrRRN Char(9);
       Dcl-S dbRRN1 Packed(9:0);
      *** end add ***** 12/06/21 *****************
     C/COPY SCOPYWW01C
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
      **** Begin Add **** 04/30/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 04/30/24 ******************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C***  BEGIN CHG  ***  04/07/05 ***********************************
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCNCHK        ##NCHK            7 0
     C                   PARM      SCPODT        ##PODT            8 0
     C***  END   CHG  ***  04/07/05  **********************************
      *
      ***** BEG ADD ***** 04/13/04 ************************************
     C*
     C*  File for changing values for display
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   PARM                    EDTDEC            3 0
     C                   PARM                    EDTHOU            1
      ***** END ADD ***** 04/13/04 ************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEGIN ADD *** 02/22/05 **************************************
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*** END   ADD *** 02/22/05 **************************************
      *
      *  Convert date from CYMD to MDY
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** BEG ADD ***** 06/30/03 **********************************
      *
      *  Convert date from CYMD to YMD
      *
     C     DTYMD         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YMD'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *** END ADD ***** 06/30/03 **********************************
      *
      ***** BEG ADD ***** 04/13/04 ************************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      ***** END ADD ***** 04/13/04 ************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKCHK#
     C                   KFLD                    KKCKAM
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
      *
      ***** BEG ADD ***** 04/13/04 ************************************
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
      *
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    KKTFDT                         ADDED 6/8/04
      *
      ***** END ADD ***** 04/13/04 ************************************
     C*
     C     BATKEY        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    EABAT#
      *
      ***** BEG ADD ***** 04/13/04 ************************************
      *
     C     BATKY2        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    EAIDT
     C                   KFLD                    EABAT#
      *
     C     BATKY3        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSTAT
     C                   KFLD                    EATFDT
     C                   KFLD                    EABAT#
      ***** END ADD ***** 04/13/04 ************************************
     C*
      ***** BEG ADD ***** 02/26/06 ************************************
     C     EAKY8         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SVBAT#
      ***** END ADD ***** 02/26/06 ************************************
     C*
     C***  BEGIN ADD  ***  05/05/98  *********************************
     C     TBKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    XXKEY
     C***  END   ADD  ***  05/05/98  *********************************
     C*
     C*** BEG ADD *******  04/11/03  *********************************
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** END ADD *******  04/11/03  *********************************
     C*** BEGIN ADD *** 02/22/05 ***********************************************
     C     CBKY          KLIST
     C                   KFLD                    KKCO#
     C*** END   ADD *** 02/22/05 ***********************************************
     C*** BEGIN ADD *** 04/07/05 ***********************************************
     C     WMFKEY        KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C*** END   ADD *** 04/07/05 ***********************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EASTAT        KKSTAT
     C     *LIKE         DEFINE    EABAT#        KKBAT#
     C     *LIKE         DEFINE    EACHK#        KKCHK#
     C     *LIKE         DEFINE    EACKAM        KKCKAM
     C     *LIKE         DEFINE    EADT          KKDT
     C     *LIKE         DEFINE    EATIME        KKTIME
     C***  BEGIN ADD  ***  05/05/98  *********************************
     C     *LIKE         DEFINE    SYKEY         XXKEY
     C***  END   ADD  ***  05/05/98  *********************************
      ***** BEG ADD ***** 04/13/04 ************************************
     C*******    *LIKE     DEFN EABAT#    SCBAT#
     C*******    *LIKE     DEFN EABATA    SCBATA
     C*******    *LIKE     DEFN EATFAP    SCTFAP
     C*******    *LIKE     DEFN EATFFL    SCTFFL
     C     *LIKE         DEFINE    EAIDT         KKIDT
     C     *LIKE         DEFINE    EATFDT        KKTFDT
      ***** END ADD ***** 04/13/04 ************************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     *LIKE         DEFINE    *IN99         MULTIG
     C     *LIKE         DEFINE    *IN99         FAILED
     C*** END   ADD *** 03/03/05 **************************************
     C*
     C*** BEGIN ADD *** 02/22/05 **************************************
     C*================================================================
     C* CHKSEC - Checks to see if there is sufficient security
     C*================================================================
     C     CHKSEC        BEGSR
     C*
     C                   MOVE      *OFF          SKIP              1
     C*
      *** beg del ***** 02/05/16 ***************************
     C*****CBKY**        CHAIN     PCB01                              99
     C******IN99*        IFEQ      *OFF
     C*****CBACHL        IFNE      *ZEROS
     C*****CBACHT        ORNE      *BLANKS
     C***********
     C***********        MOVE      ##LVL         ##ULVL
     C***********        MOVE      CBACHL        ##CLVL
     C***********
     C***********        CALL      'SRCCSC'
     C***********        PARM      ##TYPE        ##UTYP            1
     C***********        PARM                    ##ULVL            3
     C***********        PARM      CBACHT        ##CTYP            1
     C***********        PARM                    ##CLVL            3
     C***********        PARM      '1'           ##EXEC            1
     C***********        PARM      ' '           ##RES             1
     C*****##RES*        IFEQ      *ON
     C***********        MOVE      *ON           SKIP
     C***********        ENDIF
     C***********        ENDIF
     C***********        ENDIF
      *** end del ***** 02/05/16 ***************************

     C*
      *** BEG ADD *** 10/20/05 ************************************
     C*
     C                   MOVE      *OFF          SKIP1             1
     C*
     C     ##CO#         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF

      *** beg add ***** 02/05/16 *************************
     C     COACHL        IFNE      *ZEROS
     C     COACHT        ORNE      *BLANKS
     C*
     C                   MOVE      ##LVL         ##ULVL
     C                   MOVE      COACHL        ##CLVL
     C*
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM      COACHT        ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
     C*
     C     ##RES         IFEQ      *ON
     C                   MOVE      *ON           SKIP
     C                   ENDIF
     C                   ENDIF
      *** end add ***** 02/05/16 ***************************

     C     COSRPL        IFNE      *ZEROS
     C     COSRPT        ORNE      *BLANKS
     C*
     C                   MOVE      ##LVL         ##ULVL
     C                   MOVE      COSRPL        ##CLVL
     C*
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        ##UTYP
     C                   PARM                    ##ULVL
     C                   PARM      COSRPT        ##CTYP
     C                   PARM                    ##CLVL
     C                   PARM      '1'           ##EXEC
     C                   PARM      ' '           ##RES             1
     C*
     C     ##RES         IFEQ      *ON
     C                   MOVE      *ON           SKIP1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
      *** END ADD *** 10/20/05 ************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 02/22/05 **************************************
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
      *** BEG ADD *****04/11/03 ***************************************
      *** BEG ADD *****06/08/04 ***************************************
     C                   CALL      'SRRMVMSG'
      *** END ADD *****06/08/04 ***************************************
     C     SSACHF        IFNE      *BLANK
     C                   SETON                                        11
     C                   ENDIF
      *** END ADD *****04/11/03 ***************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEG ADD ***** 01/05/12 ************************************
     C                   MOVE      *BLANKS       XXEMP
     C                   MOVE      '-'           XXB1
     C                   MOVE      '-'           XXB2
     C                   MOVE      '-'           XXB3
     C                   MOVE      '-'           XXB4
     C*** END ADD ***** 01/05/12 ************************************
     C*** BEG ADD ***** 05/31/10 ************************************
     C                   MOVE      *OFF          *IN68
     C*** BEG CHG ***** 09/22/10 ************************************
     C*****                MOVE *OFF      *IN69
     C                   MOVE      *OFF          *IN30
     C*** END CHG ***** 09/22/10 ************************************
     C*** END ADD ***** 05/31/10 ************************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   DO
      *** BEG ADD ***** 02/15/13 ************************************
      *
      * Don't allow batch to unapprove if it has already been submitted
      * for EFT
      *
     c                   if        scsel  = '8' and
     c                             EAachf = 'A' and
     c                             EAachd <> 0
      *** BEG ADD ***** 08/06/13 ************************************
     c                   if        XXCLT# <> '146'
      *** END ADD ***** 08/06/13 ************************************
     c                   seton                                        6690
     c                   leavesr
      *** BEG ADD ***** 08/06/13 ************************************
     c                   endif
      *** END ADD ***** 08/06/13 ************************************
     c                   endif
      *** END ADD ***** 02/15/13 ************************************
     C*
     C* CHECK TO SEE IF THIS BATCH HAS ANY RESTRICTIONS
     C*
     C*** BEG ADD ***** 01/13/11 ************************************
     C* Not allow options 2, 4 or 13 if either the flag for approved or the
     C* flag for transfer is set to "Y"
     C*
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C     SCSEL         OREQ      '13'
     C*
     C     EATFFL        IFEQ      'Y'
     C     EATFAP        OREQ      'Y'
     C                   SETON                                        6390
     C                   ENDIF
     C                   ENDIF
     C*
     C* Only allow option 12 if both flags are set to "Y"
     C*
     C*** BEG CHG *****  05/20/11************************************
     C**************     SCSEL         IFEQ      '12'
     C*
     C**************     EATFFL        IFNE      'Y'
     C**************     EATFAP        ORNE      'Y'
     C**************     SETON                                        6490
     C**************     ENDIF
     C**************     ENDIF
     C     SCSEL         IFEQ      '12'
     C     EATFAP        IFNE      'Y'
     C     EAACHF        ANDEQ     'A'
     C                   SETON                                        6590
     C                   ENDIF
     C                   ENDIF
     C     SCSEL         IFEQ      '9'
     C     EAACHF        ANDEQ     'A'
     C     EAACHD        ANDEQ     0
     C                   SETON                                        6490
     C                   ENDIF
     C*** END CHG *****  05/20/11************************************
     C*** END ADD ***** 01/13/11 ************************************
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C     SCSEL         OREQ      '5'
     C     SCSEL         OREQ      '8'
     C     SCSEL         OREQ      '9'
     C     SCSEL         OREQ      '12'
     C     SCGRST        IFEQ      'Y'
     C                   SETON                                        8990
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C     SCSEL         OREQ      '8'
     C     SCSEL         OREQ      '9'
     C     SCSEL         OREQ      '12'
     C     SCGRST        IFEQ      'I'
     C                   SETON                                        8990
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
      **** BEGIN DEL **** 08/25/99 ************************************
     C********** SCSTAT    IFEQ 'T'
      **** END   DEL **** 08/25/99 ************************************
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '8'
     C     SCSEL         OREQ      '9'
      **** BEGIN ADD **** 08/25/99 ************************************
     C     SCDBRR        CHAIN     PFRCDF                             99                        1
     C     EASTAT        IFEQ      'T'
      **** END   ADD **** 08/25/99 ************************************
     C                   SETON                                        9071
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCBATA        ANDNE     SCAMTE
     C     SCSEL         IFEQ      '8'
     C     SCSEL         OREQ      '9'
     C*** BEG ADD *******  04/11/03  *********************************
     C     SCSEL         OREQ      '12'
     C*** END ADD *******  04/11/03  *********************************
     C                   SETON                                        9072
     C                   ENDIF
     C                   ENDIF
     C***  BEGIN ADD  ***  09/16/98  *********************************
     c********* Beg Add ********** 05/18/21 ***********************************
      * Don't allow batch to Approve and Transfer as employer is deleted
      *
      /free
          If *in90 = *off;
            If scsel  = '8' or scsel  = '9' ;
               setll BATKEY WDEAL9;
               reade BATKEY WDEAL9;
               Dow not %EOF(WDEAL9);
                  chain (Eaco#:Eafnd:Eaemp#:Eadiv) pem0l;
                  If Not %Found(wmeml);
                     *in80 = *on;
                     *in90 = *on;
                     Leave;
                  Endif;
               reade BATKEY WDEAL9;
               Enddo;
            Endif;
          Endif;
      /end-free
     c********* End Add ********** 05/18/21 **********************************
     C*
     C     *DTAARA       DEFINE                  TRNSFR            1
     C     *IN90         IFEQ      *OFF
     C     SCSEL         IFEQ      '8'
     C     SCSEL         OREQ      '9'
     C                   MOVE      '0'           *IN55
     C                   IN        TRNSFR                               55
     C     *IN55         IFEQ      '1'
     C                   SETON                                        9073
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C***  END ADD ******  09/16/98  *********************************
     C*
     C*** BEG ADD *******  04/11/03  *********************************
     C  N90SCSEL         IFEQ      '12'
      *** BEG ADD ***** 06/09/03 ************************************
      *** BEG DEL ***** 06/30/03 ************************************
     C********** SCDBRR    CHAINPFRCDF               99                    1
      *** END DEL ***** 06/30/03 ************************************
      *** END ADD ***** 06/09/03 ************************************
     C*
     C     EAACHF        IFEQ      *BLANK
     C     EASTAT        OREQ      'T'
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C* ENSURE THAT ACH BATCH IS NOT TRANSMITTED ALREADY
     C  N90EAACHD        IFNE      *ZERO
     C* SET MESSAGE VARIABLE DATA
     C                   Z-ADD     EAACHD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXTEMP           12
     C*
     C                   MOVE      ' by '        XXTEMP
     C                   MOVEL(P)  XXTEMP        MTXT
     C                   MOVE      EAACHU        MTXT
     C                   SETON                                        79  90
     C                   ENDIF
     C*
     C* ENSURE THAT ACH BATCH IS NOT IN PROGRESS
     C     *DTAARA       DEFINE    ACHBAT        XXABAT           30
     C                   MOVE      '0'           *IN55
     C                   IN        XXABAT                               55
     C  N90*IN55         IFEQ      '0'
     C     XXJAST        ANDNE     ' '
     C                   MOVEL(P)  XXJUSR        JUSR
     C                   SETON                                        9078
     C                   ENDIF
     C*** END   ADD ****** 06/29/09 ************************************
     C*** BEGIN ADD ***  03/28/08  *********************************
     C*
     C* ENSURE ALL RECORDS FOR ONLINE BATCH HAVE ACH PROCESSING.
     C*
     C  N90WCFL24        IFNE      'Y'
     C*
     C     BATKEY        SETLL     WDEAL9
     C     BATKEY        READE     WDEAL9                                 60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      EACO#         Q3CO#
     C                   MOVE      EAFND         Q3FND
     C                   MOVE      EAEMP#        Q3EMP#
     C                   MOVE      EADIV         Q3DIV
     C                   MOVE      EAFYR         Q3FYR
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         Q3EFDT
     C                   MOVE      '1'           Q3MODE
     C                   MOVE      *BLANK        Q3ERR
     C                   MOVE      *BLANKS       Q3DATA
     C*
     C                   CALL      'SRONLI'
     C                   PARM                    Q3CO#             3
     C                   PARM                    Q3FND             3
     C                   PARM                    Q3EMP#            9
     C                   PARM                    Q3DIV             5
     C                   PARM                    Q3FYR             3
     C                   PARM                    Q3EFDT            8
     C                   PARM                    Q3MODE            1
     C                   PARM                    Q3ERR             1
     C                   PARM                    Q3DATA          100
     C*
     C     Q3ERR         IFNE      '0'
     C                   SETON                                        68  90
     C                   LEAVE
     C                   ENDIF
     C*
     C     BATKEY        READE     WDEAL9                                 60
     C                   ENDDO
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C     *IN68         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
      *** BEG ADD ***** 05/31/10 ************************************
     C                   ELSE                                                   WCFL24 IFNE
     C* ENSURE ALL RECORDS FOR ACH BATCH HAVE ACH SETUP
     C     BATKEY        SETLL     WDEAL9
     C     BATKEY        READE     WDEAL9                                 60
     C     *IN60         DOWEQ     *OFF
     C*
      *** beg chg ***** 12/06/21 **************************
     C***                   MOVE      EACO#         Q3CO#
     C***                   MOVE      EAFND         Q3FND
     C***                   MOVEL(P)  EAEMP#        XXWK14           14
     C***                   MOVE      EADIV         XXWK14
     C***                   MOVEL(P)  XXWK14        Q3KEY
      ****** beg chg ***** 08/31/11 ************************************
     C**************        MOVE      EAPODT        Q3DT
     C***                   MOVE      EATRDT        Q3DT
      ****** end chg ***** 08/31/11 ************************************
     C***                   MOVE      *BLANKS       Q3EFT
     C***                   MOVE      *BLANKS       Q3PRE
     C****
     C***                   CALL      'WA023R3'
     C***                   PARM                    Q3CO#
     C***                   PARM      'E'           Q3TYPE            1
     C***                   PARM                    Q3FND
     C***                   PARM                    Q3KEY            30
     C***                   PARM                    Q3DT              7
     C***                   PARM                    Q3EFT             1
     C***                   PARM                    Q3PRE             1
     C****
     C***     Q3EFT         IFNE      *ON
       //erEftSetUp = 'Invalid EFT set up for ' +
       // 'Group ' +
       // %trim(%editc(EAfnd:'3'))  + ' ' +
       // 'Employer-Div ' +
       // %trim(%editc(EAemp#:'3')) + '-' +
       // %trim(%editc(EAdiv:'3'));
     C***                   SETON                                        30  90
     C***                   MOVE      EACO#         XXCO
     C***                   MOVE      EAFND         XXFND
     C***                   MOVE      EAEMP#        XXEMP#
     C***                   MOVE      EADIV         XXDIV
     C***                   MOVE      EAFYR         XXFYR
     C***                   MOVE      XXEMP         MEMP
       chrEfDt = %Editc(EAtrdt:'X');
       chrEftKey = %Editc(EAemp#:'X') + %Editc(EAdiv:'X');
       chrCo# = %Editc(EAco#:'X');
       chrFnd = %Editc(EAfnd:'X');
       chrFyr = %Editc(EAfyr:'X');
       If GetEftStatus(chrCo#:chrEftType:chrFnd:chrEftKey:
        chrEfDt:chrPre:chrRRN:chrEobType:chrChkType:chrFyr) = *Off;
          erEftSetUp = 'Invalid EFT set up for ' +
           'Group ' +  %trim(%editc(EAfnd:'3'))  + ' ' +
           %trim(%editc(EAfnd:'3'))  + ' ' +  'Employer-Div ' +
           %trim(%editc(EAemp#:'3')) + '-' +  %trim(%editc(EAdiv:'3'));
          PopOneBtn(erEftSetUp:'Error');
          *In90 = *On;
      *** end chg ***** 12/06/21 **************************
     C                   LEAVE
     C                   ENDIF
     C*
     C     BATKEY        READE     WDEAL9                                 60
     C                   ENDDO
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
      *** BEG CHG ***** 09/22/10 ************************************
     C*****      *IN69     IFEQ *ON
     C     *IN30         IFEQ      *ON
      *** END CHG ***** 09/22/10 ************************************
     C                   LEAVE
     C                   ENDIF
     C*
      *** END ADD ***** 05/31/10 ************************************
     C*
     C                   ENDIF                                                  WCFL24 IFNE
     C*** END ADD *****  03/28/08  *********************************
     C*
      *** BEG ADD ***** 06/30/03 ************************************
     C*
      *** BEG DEL ***** 05/31/10 ************************************
     C*********  *IN90     IFEQ *OFF
     C*
      *** BEG CHG ***** 10/27/03 ************************************
     C******************** Z-ADDEADT      SRDTI
     C*********            Z-ADDEAIDT     SRDTI
      *** END CHG ***** 10/27/03 ************************************
     C*********            CALL 'SRCVTDAT'DTYMD
     C*********            MOVELSRDTO     QQDAT6  6
     C*********  'WXDB'    CAT  QQDAT6    SRFNAM 10
     C*********            CALL 'SRCHKOBJ'
     C*********            PARM           SRFNAM
     C*********            PARM '*FILE'   SROTYP  7
     C*********            PARM           SRERR
     C*********
     C*********  SRERR     IFEQ '0'
     C*********            SETON                     75  90
     C*********            ENDIF
     C*********
     C*********            ENDIF
      *** END DEL ***** 05/31/10 ************************************
     C*
      *** END ADD ***** 06/30/03 ************************************
     C*
     C                   ENDIF
     C*** END ADD *******  04/11/03  *********************************
      *
     C*** BEG ADD *******  04/07/06  *********************************
      *
      *  ENSURE ALL RECORDS IN CASH DISSOLVE BATCH HAVE BEEN DISSOLVED
      *
     C     SCSEL         IFEQ      '8'
     C     EACASH        ANDNE     *BLANK
     C     BATKEY        SETLL     WDEAL9
     C     BATKEY        READE     WDEAL9                                 60
     C     *IN60         DOWEQ     *OFF
     C     EACASH        IFNE      'D'
     C                   SETON                                        77  90
     C                   LEAVE
     C                   ENDIF
     C     BATKEY        READE     WDEAL9                                 60
     C                   ENDDO
     C     SCDBRR        CHAIN     PFRCDF                             60
     C     *IN77         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *******  04/07/06  *********************************
      *
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   ENDDO
     C*** END   ADD *** 03/03/05 **************************************
       //*** Begin Add 05/24/21 *****************************************
       If scSel = '4' or scSel = '2' or scSel = '7';
          KeyWXRIL3.L3RInam = 'WDEAP';
          KeyWXRIL3.L3RIco# = EAco#;
          KeyWXRIL3.L3RIbat# = EAbat#;
          Monitor;
             Select;
             When scSel = '2' or scSel = '7';
                chain %kds(KeyWXRIL3) PRI03;
                If %found(WXRIL3);
                   update PRI03;
                Endif;

             When scSel = '4';
               setll %kds(KeyWXRIL3) PRI03;
               If %equal(WXRIL3);
                  reade %kds(KeyWXRIL3) PRI03;
                  Dow not %eof(WXRIL3);
                     update PRI03;
                     reade %kds(KeyWXRIL3) PRI03;
                  Enddo;
               Else;
                  KeyWXRIL.RInam = 'WDEAP';
                  KeyWXRIL.RIrrn = dbrrnWdeap;
                  Chain %kds(KeyWXRIL) PRI01;
               Endif;
               If *in90 = *Off;
                  chain %kds(KeyWXRIL3) PRI03;    //lock it
               Endif;
             Endsl;
          On-Error;
             *in31 = *On;
             *in90 = *On;
          EndMon;
       Endif;
       //*** End   Add *** 05/24/21 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C     SSSTAT        IFNE      '1'
     C     SSSTAT        ANDNE     '2'
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* GETTOT - Get totals for subfile display
     C*================================================================
     C     GETTOT        BEGSR
     C     *LIKE         DEFINE    EAEMP#        SVEMP#
     C     *LIKE         DEFINE    EADIV         SVDIV
     C     *LIKE         DEFINE    EACHK#        SVCHK#
     C                   SUB       SCAMTE        SCAMTE           11 2
     C                   SUB       SCAMTP        SCAMTP           11 2
     C                   SUB       SCCHKC        SCCHKC
     C                   SUB       SCEMSM        SCEMSM
     C                   SUB       SVEMP#        SVEMP#
     C                   SUB       SVDIV         SVDIV
     C                   SUB       SVCHK#        SVCHK#
      *
      ***** BEG CHG ***** 04/13/04 ************************************
     C*********  BATKEY    CHAINWDEAL9               98
     C   21BATKEY        CHAIN     WDEAL9                             98
     C   22
     CANN69BATKY2        CHAIN     WDEALA                             98
     C   22
     CAN 69BATKY3        CHAIN     WDEALB                             98
      ***** END CHG ***** 04/13/04 ************************************
     C     *IN98         DOWEQ     *OFF
     C     EACKAM        IFNE      *ALL'9'
     C***  BEGIN DEL  ***  05/05/98  *********************************
     C********** EAEMP#    IFNE SVEMP#
     C********** EADIV     ORNE SVDIV
     C********** EACHK#    ORNE SVCHK#
     C***  END   DEL  ***  05/05/98  *********************************
     C***  BEGIN ADD  ***  06/08/98  *********************************
     C     EAEMP#        IFNE      SVEMP#
     C     EADIV         ORNE      SVDIV
     C     EACHK#        ORNE      SVCHK#
     C***  END   ADD  ***  06/08/98  *********************************
     C                   MOVE      EAEMP#        SVEMP#
     C                   MOVE      EADIV         SVDIV
     C***  BEGIN ADD  ***  06/08/98  *********************************
     C                   ADD       EAEMP#        SCEMSM
     C                   ENDIF                                                  EAEMP#<>SVEMP#
     C***  END   ADD  ***  06/08/98  *********************************
     C***  BEGIN DEL  ***  06/08/98  *********************************
     C**********           MOVE EACHK#    SVCHK#
     C***  END   DEL  ***  06/08/98  *********************************
     C***  BEGIN CHG  ***  05/05/98  *********************************
     C**********           ADD  EACKAM    SCAMTE
     C                   ADD       EAAMT         SCAMTE           11 2
     C***  END   CHG  ***  05/05/98  *********************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C     SCGRST        IFNE      'Y'
     C     EAFND         ANDNE     0
     C     MULTIG        IFEQ      *ON
     C*
     C                   CALL      'SA008R'
     C                   PARM      ##CO#         $$CO#             3 0
     C                   PARM      Q1USER        $$USR            10
     C                   PARM      'W'           $$APPL            1
     C     FAILED        PARM      *OFF          $$RTCD            1
     C                   PARM      ' '           $$AUTH            1
     C                   PARM      EAFND         $$FND             3 0
     C                   PARM      EAEMP#        $$EMP#            9 0
     C*
     C     FAILED        IFEQ      *ON
     C                   MOVEL     'Y'           SCGRST
     C                   ELSE
     C     $$AUTH        IFEQ      'I'
     C                   MOVEL     $$AUTH        SCGRST
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C     UPXFND        IFEQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPEMP#        ANDNE     EAEMP#
     C     UPXEMP        OREQ      'Y'
     C     UPDIV         ANDNE     EADIV
     C                   MOVEL     'Y'           SCGRST
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C*
     C***  BEGIN ADD  ***  06/08/98  *********************************
     C     EACHK#        IFNE      SVCHK#
     C***  END   ADD  ***  06/08/98  *********************************
     C                   ADD       1             SCCHKC
     C***  BEGIN ADD  ***  06/08/98  *********************************
     C                   MOVE      EACHK#        SVCHK#
     C                   ENDIF                                                  EACHK#<>SVCHK#
     C***  END   ADD  ***  06/08/98  *********************************
     C*
     C***  BEGIN DEL  ***  06/08/98  *********************************
     C**********           ADD  EAEMP#    SCEMSM
     C***  END   DEL  ***  06/08/98  *********************************
     C***  BEGIN DEL  ***  05/05/98  *********************************
     C**********           ENDIF
     C***  END   DEL  ***  05/05/98  *********************************
     C***  BEGIN ADD  ***  05/05/98  *********************************
     C                   MOVEL(P)  EASYCD        XXKEY
     C*
     C     TBKY          CHAIN     PSY01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     SYSTAT        ANDNE     'I'
     C     SYTYPE        ANDEQ     'Y'
     C***  END   ADD  ***  05/05/98  *********************************
     C                   ADD       EAAMT         SCAMTP           11 2
     C***  BEGIN ADD  ***  05/05/98  *********************************
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   ADD  ***  05/05/98  *********************************
     C*
     C                   ENDIF
      *
      ***** BEG CHG ***** 04/13/04 ************************************
     C*********  BATKEY    READEWDEAL9                   98
     C   21BATKEY        READE     WDEAL9                                 98
     C   22
     CANN69BATKY2        READE     WDEALA                                 98
     C   22
     CAN 69BATKY3        READE     WDEALB                                 98
      ***** END CHG ***** 04/13/04 ************************************
     C                   ENDDO
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      ##CO#         KKCO#
     C                   SELECT
     C     SSSTAT        WHENEQ    '1'
     C                   MOVE      *BLANKS       KKSTAT
     C     SSSTAT        WHENEQ    '2'
     C                   MOVE      'T'           KKSTAT
     C                   ENDSL
      *
      ***** BEG CHG ***** 04/13/04 ************************************
     C                   MOVE      *OFF          *IN69
     C     *IN22         IFEQ      *ON
     C     SSSTAT        ANDEQ     '2'
     C                   MOVE      *ON           *IN69
     C                   ENDIF
      *
     C**********           MOVE *LOVAL    KKBAT#
     C     *IN22         IFEQ      *ON
     C  N69              MOVE      *HIVAL        KKIDT
     C   69              MOVE      *HIVAL        KKTFDT
     C   69              MOVE      'T'           KKSTAT
      *
     C                   ELSE
     C                   MOVE      *HIVAL        KKBAT#
     C                   MOVE      *LOVAL        KKCHK#
     C                   MOVE      *LOVAL        KKCKAM
     C                   MOVE      *LOVAL        KKDT
     C                   MOVE      *LOVAL        KKTIME
     C                   ENDIF
      *
     C*************        MOVE *LOVAL    KKCHK#
     C*************        MOVE *LOVAL    KKCKAM
     C*************        MOVE *LOVAL    KKDT
     C*************        MOVE *LOVAL    KKTIME
      ***** END CHG ***** 04/13/04 ************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *** Beg Add ***** 07/29/20 ***************************************
     C                   MOVE      *ZEROS        DECPOS            2 0
     C                   EVAL      DECPOS = %SCAN('.':SCPST1:1)
     C                   IF        DECPOS <> 0
     C                   EVALR     SCPST1 = %SUBST(SCPST1:1:DECPOS-1)
     C                   ENDIF
      *** End Add ***** 07/29/20 ***************************************
     C                   MOVE      SCPST1        KKBAT#
     C                   MOVE      *LOVAL        KKCHK#
     C                   MOVE      *LOVAL        KKCKAM
     C                   MOVE      *LOVAL        KKDT
     C                   MOVE      *LOVAL        KKTIME
      *
      ***** BEG ADD ***** 04/13/04 ************************************
     C     *IN22         IFEQ      *ON
     C                   MOVE      SCPST1        XXPDTE            8 0
     C                   Z-ADD     XXPDTE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKIDT
     C                   MOVEL     SRDTO         KKTFDT
     C                   ENDIF
      ***** END ADD ***** 04/13/04 ************************************
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      SCSTAT        KKSTAT
     C                   MOVE      SCBAT#        KKBAT#
     C                   MOVE      SCCHK#        KKCHK#
     C                   MOVE      SCCKAM        KKCKAM
     C                   MOVE      SCDT          KKDT
     C                   MOVE      SCTIME        KKTIME
     C                   MOVE      SCSTAT        EASTAT
     C                   MOVE      SCBAT#        EABAT#
      *
      ***** BEG ADD ***** 04/13/04 ************************************
     C     *IN22         IFEQ      *ON
     C                   MOVE      SCTFDT        ZZTFDT            6 0
     C                   Z-ADD     ZZTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAIDT
     C                   MOVEL     SRDTO         EATFDT
     C                   ENDIF
      ***** END ADD ***** 04/13/04 ************************************
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
      *
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   CLEAR                   SCGRST
     C     EAFND         IFNE      0
     C     MULTIG        IFEQ      *ON
     C*
     C                   CALL      'SA008R'
     C                   PARM      ##CO#         $$CO#
     C                   PARM      Q1USER        $$USR
     C                   PARM      'W'           $$APPL
     C     FAILED        PARM      *OFF          $$RTCD
     C                   PARM      ' '           $$AUTH
     C                   PARM      EAFND         $$FND
     C                   PARM      EAEMP#        $$EMP#
     C*
     C     FAILED        IFEQ      *ON
     C                   MOVEL     'Y'           SCGRST
     C                   ELSE
     C     $$AUTH        IFEQ      'I'
     C                   MOVEL     $$AUTH        SCGRST
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C     UPXFND        IFEQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPFND         ANDNE     EAFND
     C     UPXEMP        OREQ      'Y'
     C     UPEMP#        ANDNE     EAEMP#
     C     UPXEMP        OREQ      'Y'
     C     UPDIV         ANDNE     EADIV
     C                   MOVEL     'Y'           SCGRST
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 03/03/05 **************************************
     C                   MOVE      EABAT#        SCBAT#
     C                   MOVE      EABATA        SCBATA
     C                   MOVE      EASTAT        SCSTAT
      ***** BEG CHG ***** 04/13/04 ************************************
     C                   MOVE      EATFFL        XXTFFL
     C                   MOVE      EATFAP        XXTFAP
      ***** END CHG ***** 04/13/04 ************************************
     C*
     C                   MOVE      EACHK#        SCCHK#
     C                   MOVE      EACKAM        SCCKAM
     C                   MOVE      EADT          SCDT
     C                   MOVE      EATIME        SCTIME
     C                   MOVE      EATFAP        SCTFAP
     C                   MOVE      EATFFL        SCTFFL
      *
     C                   Z-ADD     EATFDT        SRDTI
      ***** BEG CHG ***** 04/13/04 ************************************
     C     EATFDT        IFEQ      *ZEROS
     C                   Z-ADD     EAIDT         SRDTI
     C                   ENDIF
      ***** END CHG ***** 04/13/04 ************************************
      *
      *** BEG ADD ****** 04/11/03 *************************************
     C******************   CALL 'SRCVTDAT'DTMDYY
     C                   CALL      'SRCVTDAT'    DTMDY
      *** END ADD ****** 04/11/03 *************************************
     C                   MOVEL     SRDTO         SCTFDT            6 0
      *
      *** BEG ADD ****** 04/11/03 *************************************
     C***  Begin Del  ***  03/25/10  **************************************
     C********** *IN41     IFEQ *ON
     C***  End   Del  ***  03/25/10  **************************************
      *
      ***** BEG CHG ***** 04/13/04 ************************************
      ***** BEG DEL ***** 06/16/04 ************************************
     C*****************    MOVELHD,4      SCHDL2
     C************IN21*    IFEQ *ON
     C************IN69*    IFEQ *ON
     C*****************    MOVELHD,7      SCHDL1
     C*****************    MOVELHD,8      SCHDL2
     C*****************    ELSE
     C*****************    MOVELHD,1      SCHDL1
     C*****************    ENDIF
     C*****************    ENDIF
      ***** END DEL ***** 06/16/04 ************************************
      ***** END CHG ***** 04/13/04 ************************************
      *
      *** BEG DEL *** 10/20/05 ************************************
     C********   EAACHF    IFEQ 'A'
     C********             MOVE 'ACH'     SCACHF
     C********             ELSE
     C********             MOVE *BLANKS   SCACHF
     C********             ENDIF                           EAACHF
      *** END DEL *** 10/20/05 ************************************
      *** BEG ADD *** 10/20/05 ************************************
     C                   SELECT
     C     EAACHF        WHENEQ    'A'
     C*** BEGIN CHG *** 04/21/11 **************************************
     C************       MOVE      'ACH'         SCACHF
     C                   MOVE      'EFT'         SCACHF
     C*** END   CHG *** 04/21/11 **************************************
      ***** BEG ADD ******* 02/15/11 **********************************
     C     EAACHF        WHENEQ    'F'
     C                   MOVE      'FEE'         SCACHF
      ***** END ADD ******* 02/15/11 **********************************
     C     EAACHF        WHENEQ    'S'
     C                   MOVE      'SR '         SCACHF
     C*** BEGIN ADD ***  08/09/07  *********************************
     C     EAACHF        WHENEQ    'L'
     C                   MOVE      'LB '         SCACHF
     C*** END ADD *****  08/09/07  *********************************
     C***  Begin Add  ***  03/25/10  **************************************
     C     EAACHF        WHENEQ    'D'
     C                   MOVE      'DED'         SCACHF
     C***  End   Add  ***  03/25/10  **************************************
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C     EAACHF        WHENEQ    'R'
     C                   MOVE      'RDP'         SCACHF
     C*** END   ADD ****** 08/09/10 ************************************
      *** Beg Add ***** 11/07/19 ***************************************
     C     EAACHF        WHENEQ    'B'
     C     EAACHF        OREQ      *Blank
     C     EAONLI        ANDEQ     'Y'
     C                   MOVE      'BIL'         SCACHF
     C     EAACHF        WHENEQ    'T'
     C                   MOVE      '3PY'         SCACHF
     C     EAACHF        WHENEQ    'U'
     C                   MOVE      '3BL'         SCACHF
      *** End Add ***** 11/07/19 ***************************************
      *** Beg Add ***** 11/07/19 ***************************************
     C     EAACHF        WHENEQ    'B'
     C     EAACHF        OREQ      *Blank
     C     EAONLI        ANDEQ     'Y'
     C                   MOVE      'BIL'         SCACHF
     C     EAACHF        WHENEQ    'T'
     C                   MOVE      '3PY'         SCACHF
     C     EAACHF        WHENEQ    'U'
     C                   MOVE      '3BL'         SCACHF
      *** End Add ***** 11/07/19 ***************************************
     C                   OTHER
     C                   MOVE      *BLANKS       SCACHF
     C                   ENDSL
      *** END ADD *** 10/20/05 ************************************

     C***  Begin Del  ***  03/25/10  **************************************
     C**********           ENDIF                           *IN41
     C***  End   Del  ***  03/25/10  **************************************
      *
      ***** BEG ADD ***** 06/16/04 ************************************
     C     *IN21         IFEQ      *ON
     C     *IN69         IFEQ      *ON
     C                   MOVEL     HD(7)         SCHDL1
     C                   MOVEL     HD(8)         SCHDL2
     C                   ELSE
     C                   MOVEL     HD(1)         SCHDL1
     C                   MOVEL     HD(4)         SCHDL2
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ***** 06/16/04 ************************************
      *
     C     *IN22         IFEQ      *ON
     C     *IN69         IFEQ      *ON
     C                   MOVEL     HD(3)         SCHDL1
     C                   MOVEL     HD(6)         SCHDL2
     C                   ELSE                                                   *IN22
     C                   MOVEL     HD(2)         SCHDL1
     C                   MOVEL     HD(5)         SCHDL2
     C                   ENDIF                                                  *IN69
     C                   ENDIF                                                  *IN22
      *
      *** END ADD ****** 04/11/03 *************************************
      *** Beg Add ***** 06/08/18 **********************************
      /Free
       scOnli = EAonli;
       scColl = EAcollFlg;
       If visOnli = *On;
      *** Beg Chg ***** 07/29/22 **********************************
          //*** %Subst(scHdL2:81:4) = 'Onli';
          %Subst(scHdL2:81:5) = 'PayGo';
      *** End Chg ***** 07/29/22 **********************************
       Endif;
       If vis150 = *On;
      *** Beg Chg ***** 07/29/22 **********************************
          //*** %Subst(scHdL2:86:4) = 'Coll';
          %Subst(scHdL2:86:4) = 'Col';
      *** End Chg ***** 07/29/22 **********************************
       Endif;
      /End-Free
      *** End Add ***** 06/08/18 **********************************

      *
     C                   EXSR      GETTOT
      ***** BEG CHG ***** 04/13/04 ************************************
     C*
     C*  Format numeric fields for output
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C                   Z-ADD     0             EDTDEC
     C                   MOVEL     '0'           EDTHOU
     C                   MOVEL(P)  EABAT#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXBAT#
     C*
     C                   MOVEL     'DATE6'       EDTMSK
     C*
     C                   MOVEL(P)  SCTFDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXTFDT
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  'J'           EDTMSK
     C                   Z-ADD     2             EDTDEC
     C                   MOVEL     '1'           EDTHOU
     C*
     C                   MOVEL(P)  EABATA        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXBATA
     C*
     C                   MOVEL(P)  SCAMTE        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXAMTE
     C*
     C                   MOVEL(P)  SCAMTP        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXAMTP
     C*
     C     *IN20         IFEQ      *ON
     C     *IN21         OREQ      *ON
     C                   MOVEL     VIEW1         SCDATA
     C                   ENDIF
     C*
     C     *IN22         IFEQ      *ON
     C                   MOVEL(P)  XXBAT#        X2BAT#
     C                   MOVEL(P)  XXBATA        X2BATA
     C                   MOVEL(P)  XXAMTE        X2AMTE
     C                   MOVEL(P)  XXAMTP        X2AMTP
     C                   MOVEL(P)  XXTFAP        X2TFAP
     C                   MOVEL(P)  XXTFFL        X2TFFL
     C                   MOVEL(P)  XXTFDT        X2TFDT
      *
     C                   MOVEL     VIEW2         SCDATA
     C                   ENDIF
     C*
      ***** END CHG ***** 04/13/04 ************************************
     C                   ENDSR
     C***  BEGIN CHG  ***  04/07/05 ***********************************
     C*================================================================
     C* OPT13 - PROCESS OPTION 13
     C*================================================================
     C     OPT13         BEGSR
     C*
     C                   Z-ADD     0             SCNCHK
     C                   Z-ADD     0             SCPODT
     C*
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C                   EXFMT     MASSWDW
     C*
     C                   MOVE      *OFF          *IN61
     C                   MOVE      *OFF          *IN62
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C  N90SCNCHK        IFLE      *ZERO
     C                   SETON                                        61  90
     C                   ENDIF                                                  SCCHK# IFLE 3/4
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C  N90SRERR         IFEQ      '1'
     C                   SETON                                        62  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDDO
     C*
     C* UPDATE THE BATCH.
     C*
     C     *INKL         IFEQ      *OFF
     C     *IN90         ANDEQ     *OFF
     C*
     C                   MOVE      'U'           ##MODE
     C                   CALL      ACDPGM        ACDPL2                 99
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C***  END   CHG  ***  04/07/05 ***********************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   CALL      'SRRMVMSG'                           99
     C                   SELECT
     C*
     C     *INKH         WHENEQ    *ON
     C                   MOVEL(P)  PURGE         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        QQPRDS
     C                   CALL      'W1206R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQRTC             1
     C                   SELECT
     C     QQRTC         WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C     QQRTC         WHENEQ    'B'
     C                   MOVE      *ON           *INKL
     C                   ENDSL
     C*
     C     *INKT         WHENEQ    *ON
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      *BLANKS       QQBAT#
     C                   CALL      'W9200C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQBAT#            9
     C*** BEGIN CHG *** 08/12/99 ***********************************************
     C**********           PARM *BLANKS   QQOUTQ 10
     C                   PARM      UPOUTQ        QQOUTQ           10
     C*** END   CHG *** 08/12/99 ***********************************************
     C*
     C     *INKV         WHENEQ    *ON
      *** BEG ADD ***** 02/23/06 ***********************************************
     C                   MOVE      *OFF          ERRFLG            1
     C*
      *** BEG ADD ***** 09/07/06 ***********************************************
     C     XXCLT#        IFNE      '129'
      *** END ADD ***** 09/07/06 ***********************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYYYM            5 0
     C                   Z-ADD     0             SVBAT#
     C     EAKY8         SETLL     PEA0L8
      *** BEG CHG ***** 01/17/07 *****************************************
     C***********          READ PEA0L8                   60
     C     KKCO#         READE     PEA0L8                                 60
      *** END CHG ***** 01/17/07 *****************************************
     C     *IN60         DOWEQ     *OFF
      *
     C     EABAT#        IFNE      SVBAT#
     C                   Z-ADD     EABAT#        SVBAT#            9 0
     C                   ELSE
     C     EAKY8         SETGT     PEA0L8
      *** BEG CHG ***** 01/17/07 *****************************************
     C***********          READ PEA0L8                   60
     C     KKCO#         READE     PEA0L8                                 60
      *** END CHG ***** 01/17/07 *****************************************
     C                   ENDIF
      *
     C     EATFAP        IFEQ      'Y'
     C     EATFFL        ANDEQ     'Y'
     C     EAYYYM        ANDLT     XXYYYM
     C     EAYYYM        ANDNE     *ZERO
      *** BEG DEL ***** 06/28/07 *****************************************
     C*****************    MOVE *ON       ERRFLG  1
      *** END DEL ***** 06/28/07 *****************************************
     C                   LEAVE
     C                   ENDIF
      *
      *** BEG CHG ***** 01/17/07 *****************************************
     C**N60******          READ PEA0L8                   60
     C  N60KKCO#         READE     PEA0L8                                 60
      *** END CHG ***** 01/17/07 *****************************************
     C                   ENDDO
      *
      *** BEG ADD ***** 09/07/06 ***********************************************
     C                   ENDIF                                                  XXCLT# <> '129'
      *** END ADD ***** 09/07/06 ***********************************************
      *
     C     ERRFLG        IFEQ      *ON
     C                   EXFMT     TRDTWD
     C                   ELSE
      *** END ADD ***** 02/23/06 ***********************************************
     C                   MOVE      ##CO#         QQCO#
     C                   CALL      'W9205C'
     C                   PARM                    QQCO#             3
     C                   PARM      *BLANKS       QQDATE            8
     C*** BEGIN CHG *** 08/12/99 ***********************************************
     C**********           PARM *BLANKS   QQOUTQ 10
     C                   PARM      UPOUTQ        QQOUTQ           10
     C*** END   CHG *** 08/12/99 ***********************************************
      *** BEG ADD ***** 02/23/06 ***********************************************
     C                   ENDIF
      *** END ADD ***** 02/23/06 ***********************************************
     C*
     C                   ENDSL
     C*
     C*** BEGIN CHG *** 12/18/02 ***********************************************
     C     SCRRN1        IFGT      Q2LSFR
     C     Q2LSFR        IFGT      *ZEROS
     C                   Z-ADD     Q2LSFR        SCRRN1
     C                   ELSE
     C                   Z-ADD     1             SCRRN1
     C                   ENDIF
     C                   EXSR      SETGT
     C                   ENDIF
     C*** END   CHG *** 12/18/02 ***********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   SELECT
     C     SCSEL         WHENEQ    '7'
     C     SCSTAT        IFEQ      'T'
     C     SCTFFL        OREQ      'Y'
     C     SCTFAP        OREQ      'Y'
     C                   MOVE      '1'           ##CALL
     C                   ELSE
     C                   MOVE      *BLANKS       ##CALL
     C                   ENDIF
     C                   CALL      'W1210R'                             99
     C                   PARM                    ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*** BEGIN ADD *** 03/03/05 **************************************
     C                   PARM      SCGRST        ##GRST            1
     C*** END   ADD *** 03/03/05 **************************************
      //*** Begin Add *** 05/24/21 ************************************
        Unlock WXRIL3;
      //*** End   Add *** 05/24/21 ************************************
     C     SCSEL         WHENEQ    '8'
     C                   MOVE      'W'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C     SCSEL         WHENEQ    '9'
     C                   MOVE      'X'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C     SCSEL         WHENEQ    '11'
     C                   MOVE      ##CO#         QQCO#
      ***** BEG CHG ***** 07/08/04 **************************************
     C**********           MOVE SCBAT#    QQBAT#
     C                   MOVE      EABAT#        QQBAT#
      ***** END ADD ***** 07/08/04 **************************************
     C                   Z-ADD     1             IX                1 0
     C     QB(IX)        DOWEQ     '0'
     C                   MOVE      ' '           QB(IX)
     C                   ADD       1             IX
     C                   ENDDO
     C                   CALL      'W9200C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQBAT#            9
     C*** BEGIN CHG *** 08/12/99 ***********************************************
     C**********           PARM *BLANKS   QQOUTQ 10
     C                   PARM      UPOUTQ        QQOUTQ           10
     C*** END   CHG *** 08/12/99 ***********************************************
     C*** BEG ADD ***** 04/11/03 ***********************************************
     C     SCSEL         WHENEQ    '12'
     C                   MOVE      ##CO#         QQCO#
      ***** BEG CHG ***** 07/08/04 **************************************
     C**********           MOVE SCBAT#    QQBAT#
     C                   MOVE      EABAT#        QQBAT#
      ***** END ADD ***** 07/08/04 **************************************
      *
      *** BEG ADD ***** 06/30/03 *********************************
      *** BEG CHG ***** 09/24/03 *********************************
     C******************** Z-ADDEADT      SRDTI
     C                   Z-ADD     EAIDT         SRDTI
      *** END CHG ***** 09/24/03 *********************************
     C                   CALL      'SRCVTDAT'    DTYMD
     C                   MOVEL     SRDTO         QQDAT6            6
      *** END ADD ***** 06/30/03 *********************************
     C                   Z-ADD     1             IX                1 0
     C     QB(IX)        DOWEQ     '0'
     C                   MOVE      ' '           QB(IX)
     C                   ADD       1             IX
     C                   ENDDO
     C                   CALL      'W1260C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQBAT#            9
      *** BEG ADD ***** 06/30/03 *********************************
     C                   PARM                    QQDAT6
      *** END ADD ***** 06/30/03 *********************************
     C*** END ADD ***** 04/11/03 ***********************************************
     C                   ENDSL
     C***  BEGIN CHG  ***  04/07/05 ***********************************
     C     SCSEL         IFEQ      '13'
     C     WMFKEY        CHAIN     WMFNL                              76
     C     *IN60         IFEQ      *OFF
     C     FNPEOF        IFEQ      *BLANK
     C                   SETON                                        7690
     C                   ELSE
     C                   EXSR      OPT13
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C***  END   CHG  ***  04/07/05 ***********************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      **** Begin Add **** 04/30/24 ******************************************
      * Do not Pass-Through to new screen, if 2nd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 2
     c                   Call      'WTARBTCH'
     c                   parm                    @@rrn             9            tab index
     c                   parm                    @@mode            1            tab index
     c                   parm                    @@next           10            tab index
     c                   parm                    @@TabInd          5            tab index
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 04/30/24 ******************************************
      *** BEG ADD  ***** 09/07/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
      *** END ADD  ***** 09/07/06 **************************************
     C                   MOVE      *ON           *IN12
     C                   MOVE      '1'           SSSTAT
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C                   MOVE      *OFF          *IN59
     C     XXCLT#        IFEQ      '071'
     C                   MOVE      *ON           *IN59
     C                   ENDIF
     C*** END   ADD ****** 08/09/10 ************************************
     C*** BEGIN ADD *** 08/12/99 ***********************************************
     C     Q1USER        CHAIN     PUP01                              60
     C*** END   ADD *** 08/12/99 ***********************************************
     C*** BEG ADD ***** 04/11/03 ***********************************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C  N60WCFL24        IFEQ      'Y'
     C*** BEGIN ADD ***  03/28/08  ************************************
     C     WCFL38        OREQ      'Y'
     C*** END ADD *****  03/28/08  ************************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '12'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   SETON                                        41
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF
     C*** END ADD ***** 04/11/03 ***********************************************
     C*** BEGIN ADD *** 02/22/05 **************************************
     C                   EXSR      CHKSEC
     C*** END   ADD *** 02/22/05 **************************************
     C*** BEGIN ADD *** 03/03/05 **************************************
     C*
     C* DOES MULTI GROUP RESTRICTIONS APPLY
     C*
     C                   CALL      'SA008R'
     C                   PARM      ##CO#         $$CO#
     C                   PARM      Q1USER        $$USR
     C                   PARM      'W'           $$APPL
     C     MULTIG        PARM      *OFF          $$RTCD
     C*** END   ADD *** 03/03/05 **************************************
      ***** BEG ADD ******* 02/15/11 **********************************
     C                   callp     SubsetPrompts
      ***** END ADD ******* 02/15/11 **********************************

      *** Beg Add ***** 06/08/18 ***************************
      /Free
       If xxClt# = '150';
          vis150 = *On;
       Endif;

       If WCfl38 = 'Y';
          visOnli = *On;
       Endif;
      /End-Free
      *** End Add ***** 06/08/18 ***************************

     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C     SFRRN1        IFNE      *ZEROS
      ***** BEG CHG ***** 04/13/04 ************************************
     C********   BATKEY    SETGTDBRCDF               96
      ***** Beg Chg ***** 05/01/18 ************************************
     C***21BATKEY        SETGT     DBRCDF                             96
     C***22
     C***ANN69BATKY2        SETGT     DBRCD2                             96
     C***22
     C***AN 69BATKY3        SETGT     DBRCD3                             96
     C                   Select
     C                   When      *In21 = *On
     C     BATKEY        Setgt     DBRCDF                             96
     C                   When      *In22 = *On and
     C                             *In69 = *Off
     C     BATKY2        Setgt     DBRCD2                             96
     C                   When      *In22 = *On and
     C                             *In69 = *On
     C     BATKY3        Setgt     DBRCD3                             96
     C                   Endsl
      ***** End Chg ***** 05/01/18 ************************************
      ***** END CHG ***** 04/13/04 ************************************
     C                   ENDIF
      ***** BEG CHG ***** 04/13/04 ************************************
     C********   KEY       READEDBRCDF                   95
      ***** Beg Chg ***** 05/01/18 ************************************
     C***21KEY           READE     DBRCDF                                 95
     C***22
     C***ANN69KEY           READE     DBRCD2                                 95
     C***22
     C***AN 69KEY           READE     DBRCD3                                 95
     C                   Select
     C                   When      *In21 = *On
     C     KEY           Reade     DBRCDF                                 95
     C                   When      *In22 = *On and
     C                             *In69 = *Off
     C     KEY           Reade     DBRCD2                                 95
     C                   When      *In22 = *On and
     C                             *In69 = *On
     C     KEY           Reade     DBRCD3                                 95
     C                   Eval      DBRRN2 = DBRRN3
     C                   Endsl
      ***** End Chg ***** 05/01/18 ************************************
      ***** END CHG ***** 04/13/04 ************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
      ***** BEG CHG ***** 04/13/04 ************************************
     C*** Begin add *** 03/29/99 **************************************
     C*******    BATKEY    SETLLDBRCDF               96
     C*** End   add *** 03/29/99 **************************************
      ***** Beg Chg ***** 05/01/18 ************************************
     C***21KEY           READPE    DBRCDF                                 95
     C***22
     C***ANN69KEY           READPE    DBRCD2                                 95
     C***22
     C***AN 69KEY           READPE    DBRCD3                                 95
     C                   Select
     C                   When      *In21 = *On
     C     KEY           Readpe    DBRCDF                                 95
     C                   When      *In22 = *On and
     C                             *In69 = *Off
     C     KEY           Readpe    DBRCD2                                 95
     C                   When      *In22 = *On and
     C                             *In69 = *On
     C     KEY           Readpe    DBRCD3                                 95
     C                   Eval      DBRRN2 = DBRRN3
     C                   Endsl
      ***** End Chg ***** 05/01/18 ************************************
      ***** END CHG ***** 04/13/04 ************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
      ***** Beg Chg ***** 05/01/18 ************************************
     C***21KEY1          SETGT     DBRCDF                             96
      ***** BEG ADD ***** 04/13/04 ************************************
     C***22
     C***ANN69KEY2          SETGT     DBRCD2                             96
     C***22
     C***AN 69KEY3          SETGT     DBRCD3                             96
      ***** END ADD ***** 04/13/04 ************************************
     C                   Select
     C                   When      *In21 = *On
     C     KEY1          Setgt     DBRCDF                             96
     C                   When      *In22 = *On and
     C                             *In69 = *Off
     C     KEY2          Setgt     DBRCD2                             96
     C                   When      *In22 = *On and
     C                             *In69 = *On
     C     KEY3          Setgt     DBRCD3                             96
     C                   Endsl
      ***** Beg Chg ***** 05/01/18 ************************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
      ***** Beg Del ***** 05/01/18 ************************************
     C***21KEY1          SETLL     DBRCDF                             96
      ***** BEG ADD ***** 04/13/04 ************************************
     C***22
     C***ANN69KEY2          SETLL     DBRCD2                             96
     C***22
     C***AN 69KEY3          SETLL     DBRCD3                             96
      ***** END ADD ***** 04/13/04 ************************************
      ***** End Del ***** 05/01/18 ************************************
      ***** Beg Add ***** 05/01/18 ************************************
     C                   Select
     C                   When      *In21 = *On
     C     KEY1          Setll     DBRCDF                             96
     C                   When      *In22 = *On and
     C                             *In69 = *Off
     C     KEY2          Setll     DBRCD2                             96
     C                   When      *In22 = *On and
     C                             *In69 = *On
     C     KEY3          Setll     DBRCD3                             96
     C                   Endsl
      ***** End Add ***** 05/01/18 ************************************

     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
      //*** Begin Add *** 05/24/21 ************************************
           Unlock WXRIL3;
      //*** End   Add *** 05/24/21 ************************************
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
      *
      *** BEG ADD ***** 04/13/03 **************************************
      *
     C                   SELECT
     C     SSSTAT        WHENEQ    '1'
     C                   MOVE      *OFF          *IN69
     C     SSSTAT        WHENEQ    '2'
     C                   MOVE      *ON           *IN69
     C                   OTHER
     C                   MOVE      *OFF          *IN69
     C                   ENDSL
      *** END ADD ***** 04/13/03 **************************************
      *
      *** BEG ADD ***** 04/11/03 **************************************
     C     SSACHF        IFNE      *BLANK
      *
     C     SSACHF        IFEQ      'A'
     C     SSACHF        ANDNE     EAACHF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSACHF        IFEQ      'L'
     C     SSACHF        ANDNE     EAACHF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SSACHF        IFEQ      'R'
     C     EAACHF        ANDNE     *BLANK
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEGIN ADD ****** 08/09/10 ************************************
     C     SSACHF        IFEQ      'D'
     C     EAACHF        ANDNE     'R'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD ****** 08/09/10 ************************************
      *
     C                   ENDIF
     C*** BEGIN ADD *** 02/22/05 **************************************
     C     EAACHF        IFEQ      'A'
     C     SKIP          ANDEQ     *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 02/22/05 **************************************
      *** END ADD ***** 04/11/03 **************************************
      *** BEG ADD *** 10/20/05 ************************************
     C     EAACHF        IFEQ      'S'
     C     SKIP1         ANDEQ     *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *** END ADD *** 10/20/05 ************************************
      ***** BEG ADD ******* 02/15/11 **********************************
     C     SSACHF        IFEQ      'F'
     C     SSACHF        ANDNE     EAACHF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      ***** END ADD ******* 02/15/11 **********************************
      *** Beg Add ***** 06/08/18 ***************************
      /Free
       If ssOnli = 'Y' and ssOnli <> EAonli;
          meetss = *Off;
       Endif;

       If ssColl = 'Y' and ssColl <> EAcollFlg;
          meetss = *Off;
       Endif;
      /End-Free
      *** End Add ***** 06/08/18 ***************************
      *
     C                   ENDSR
      ***** BEG ADD ******* 02/15/11 **********************************
                                                                                         : :

      *====================================================================
      * SubsetPrompts
      *====================================================================
     PSubsetPrompts    b
     DSubsetPrompts    pi

      * Set prompts for Subset SCR03

     c                   movel     '('           xxprmpt          30
     c                   eval      ssarpr = 'A/R batch type . . . . . . . . . .'

     c                   if        *in41 = *on
     c                   eval      xxprmpt = %trim(xxprmpt) + 'A/'
     c                   endif

     c                   if        *in59 = *on
     c                   eval      xxprmpt = %trim(xxprmpt) + 'D/'
     c                   endif

     c                   if        wcfl44 = 'Y'
     c                   eval      xxprmpt = %trim(xxprmpt) + 'F/'
     c                   endif

     c                   eval      xxprmpt = %trim(xxprmpt) + 'L/R/ ):'
     c                   eval      start  = %len(ssarpr) -
     c                                      %len(%trim(xxprmpt)) + 1
     c                   eval      ssarpr = %replace(xxprmpt:ssarpr:start)
      * zzz

      * Set Prompt Descriptions for SCR03

     c                   z-add     1             x1                3 0

     c                   if        *in41 = *on
     C*** BEGIN CHG *** 04/21/11 **************************************
     c*********          eval      typD(x1) = 'A=ACH processing'
     c                   eval      typD(x1) = 'A=EFT processing'
     C*** END   CHG *** 04/21/11 **************************************
     c                   eval      x1 = x1 + 1
     c                   endif

     c                   if        *in59 = *on
     c                   eval      typD(x1) = 'D=Retro'
     c                   eval      x1 = x1 + 1
     c                   endif

     c                   if        wcfl44 = 'Y'
     c                   eval      typD(x1) = 'F=Fee'
     c                   eval      x1 = x1 + 1
     c                   endif

     c                   eval      typD(x1) = 'L=Lockbox'
     c                   eval      x1 = x1 + 1
     c                   eval      typD(x1) = 'R=Regular'
     c                   eval      x1 = x1 + 1
     c                   eval      typD(x1) = 'Blank=Neither'
     c                   eval      x1 = x1 + 1


     pSubsetPrompts    e


      ***** END ADD ******* 02/15/11 **********************************
** Column headings
      Batch           Batch         Amount        Amount to          Date
     Date      Batch           Batch         Amount       Amount to
    Transfer   Batch           Batch         Amount       Amount to
Opt   number          total         entered       be posted App Xfr created Typ
Opt created    number          total         entered      be posted App Xfr Typ
Opt  date      number          total         entered      be posted App Xfr Typ
      Batch           Batch         Amount        Amount to         Transfer
Opt   number          total         entered       be posted App Xfr   date  Typ
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Detail                 YYY
1088=Flag for approved      YNN
1099=Flag for transfer      YNN
11111=Edit                  YYY
11212=Create EFT file       NNN                                              09/06/11
11313=Mass Update           YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
208F8=Purge                 YNN
211F11=Change view          YYN
212F12=Cancel               NNY
215F15=Fold/truncate        YYY
217F17=Subset               YYY
219F19=Edit all open batchesYNN
221F21=Transfer batches     YNN
3
