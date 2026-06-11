     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W7418R - GENERATE ENDORSEMENT TRANSACTIIONS FOR THE NCCI
     H*               ELECTRONIC SUBMISSION PROCESS                   *
      *===============================================================*
      *  PROGRAM DESCRIPTION:                                         *
      *    THIS PROGRAM WILL READ THROUGH ALL ENDORSEMENT RECORDS     *
      *    TO BE PROCESSED (WMENP) AND WILL GENERATE THE NECESSARY    *
      *    TRANSACTION RECORDS FOR THE NCCI ELECTRONIC SUBMISSION     *
      *    PROCESS.  RECORDS ARE WRITTEN/UPDATED FOR THE              *
      *    SUBMISSION HISTORY FILE (WMSHP) AND THE N/A TRANSACTION    *
      *    LOCATOR FILE (WXNAP1).                                     *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W7051C1 - PRINT ENDORSEMENTS - OVERRIDE                    *
      *                                                               *
      *  CALLS TO:                                                    *
      *    NONE                                                       *
      *                                                               *
      *  CREATED   : 04/05/95                                         *
      *  PROGRAMMER: 11                                               *
      *  PROJECT   : WC410                                            *
      *                                                               *
      *  REWRITE   : 08/31/99                                         *
      *  PROGRAMMER: C:113                                            *
      *  PROJECT   : CM027                                            *
      *===============================================================*
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/19/21 C:65  Project: WC2841                                *
     H*                Store the endorsement # passed into program in *
     H*                the WMSHP/SHREQ# field on the 'E' record. This *
     H*                will allow WCPOLS to check for attached forms. *
     H*                                                               *
     H* 03/23/18 C:65  Project: 150WAD/WC1966                         *
     H*                Update prevention of mid-term endorsement      *
     H*                records for MA to allow them if group is       *
     H*                set up to process them as resubmissions.       *
     H*                                                               *
     H* 01/19/18 C:65  Project: 150WAD/WC2587                         *
     H*                Change the update of policy submission record  *
     H*                to place newly added bureaus in a '4' status   *
     H*                as the '3' status would potentially report the *
     H*                policy with old data generated prior to the    *
     H*                state being added to the policy.  In addition  *
     H*                add NCCI to interstate rated policies for all  *
     H*                clients not just 093.                          *
     H*                                                               *
     H* 04/14/16 C:65  Project: WC2140                                *
     H*                Generate submission history data for every     *
     H*                mid-term endorsement if the group is set up to *
     H*                process them as resubmissions.  Call SRWRIT at *
     H*                the end of processing if it has not already    *
     H*                been called.                                   *
     H*                                                               *
     H* 06/27/14 C:242 Project: WC2274                                *
     H*                Do NOT set CA Endorsements to Sent('4') Status *
     H*                                                               *
     H* 01/20/12 C:65  Project: SUPP# WA139/77525                     *
     H*                GENERATE SUBMISSION HISTORY DATA FOR A FEIN    *
     H*                CHANGE ENDORSEMENT ON ADDITIONAL DIVISION, IF  *
     H*                GROUP IS FLAGGED TO PROCESS ENDORSEMENTS AS    *
     H*                RESUBMISSIONS.                                 *
     H*                                                               *
     H* 02/15/11 C:220 Project: WC1966
     H*                MA data reporting changes.
     H*
     H* 05/11/09 C:61  Project  93WHW
     H*                Change to enable NCCI submissions for non-NCCI
     H*                states and to control submission based upon
     H*                Inter or Intra-state ratings; for clt093.
     H*
     H* 04/29/09 C:65  Project: 114WTU                                *
     H*                Report full name for the expanded additional   *
     H*                location names.                                *
     H*                                                               *
     H*  12/13/07 C:183 PROJECT: 139WCI                               *
     H*                 IF THE GROUP BEING PROCESSED IS A PEO GROUP   *
     H*                 THEN CHECK THE PEO OUTPUT CONTROL FLAG FOR    *
     H*                 FOR THE STATE REPORTING DIVISION RATING       *
     H*                 STATE. IF THE OUTPUT CONTROL FLAG IS '1' OR   *
     H*                 '3', THE NAME DATE WILL NEED TO BE ADJUSTED   *
     H*                 TO INCLUDE THE PEO EMPLOYER NAME.             *
     H*                                                               *
     H*  07/23/07 C:65 PROJECT: SUPP# 58457                           *
     H*                ADD CODING TO SET PEO COMPANY CODE ON TYPE 88  *
     H*                RECORDS, USING THE PERSONAL NAME FIELD.        *
     H*                                                               *
     H*  12/13/06 C:65 PROJECT: SUPP# 51860                           *
     H*                LOAD NEPFDT WITH THE POLICY EFF DATE FROM      *
     H*                STATE REPORTING DIVISION, TO MATCH POLICY.     *
     H*                                                               *
     H*  10/16/06 C:65 PROJECT: SUPP# 54248                           *
     H*                POPULATE FEIN # ON THE SRWRT2 SUBROUTINE, TO   *
     H*                PREVENT MISSING FEIN ERRORS.                   *
     H*                                                               *
     H*  04/25/06 C:65 PROJECT: 132WAY                                *
     H*                LOAD PERSONAL NAME FLAG FOR WXNEP FOR TYPE 88  *
     H*                NAME CHANGE RECORDS.                           *
     H*                                                               *
     H* 04/20/06 C:65  PROJECT: WC1287                                *
     H*                Added processing for NAICS code requirements.  *
     H*                                                               *
     H* 08/18/05 C:61 PROJECT: WC1266                                 *
     H*               Add processing "Rescission Division Deletion".  *
     H*                                                               *
     H* 08/17/05 C:33 PROJECT: 114WGH                                 *
     H*               Load 'Exposure Link' code.                      *
     H*                                                               *
     H* 08/05/05 C:65 PROJECT: 114WGH                                 *
     H*               PULL NJ TAX ID FROM EMUE# INSTEAD OF EMSEMP,    *
     H*               AND PULL EMSEMP FOR ANY OTHER STATE.            *
     H*                                                               *
     H* 05/12/05 C:65 PROJECT: 132WAY                                 *
     H*               WHEN CREATING OR UPDATING WMSHP RECORD, SET CA  *
     H*               FIELD TO '4' STATUS TO PREVENT SENDING OF INFO. *
     H*                                                               *
     H* 09/22/04 C:33 Project: WC1202                                 *
     H*               Load Extended names correctly.                  *
     H*                                                               *
     H* 09/10/04 C:33  Project: WC1242                                *
     H*                Add 'Name Link Counter Identifier' field.      *
     H*                                                               *
     H* 07/07/04 C:65 PROJECT: 114WAK/SUPP#42762                      *
     H*               WHEN ADDING A NEW STATE TO A POLICY CHECK IF    *
     H*               ANY STATES ARE ON THE WMSHP RECORD BEFORE       *
     H*               ADDING AND UPDATING THE STATUS.                 *
     H*                                                               *
     H* 05/14/04 C:65 PROJECT: WC1202                                 *
     H*               REPORT EXTENDED EMPLOYER NAMES FOR NAME RECORDS.*
      *                                                               *
      * 05/10/04 C:33  Support: WA091/40129                           *
      *                Only process WC890605 for mailing address      *
      *                 listed on Policy Page.  The WC890605 should   *
      *                 only be produced for this address.  When      *
      *                 processing any other address change the       *
      *                 WC890608 is used and all addresses other than *
      *                 the address listed on Policy Page should be   *
      *                 listed.                                       *
      *                                                               *
      * 04/08/04 C:33  Support: WA091/40129                           *
      *                Because an address change will process both    *
      *                 mailing and physical address for the employer *
      *                 seperate processing does not need to occur    *
      *                 when both are changed simultaneously.         *
      *                                                               *
      * 02/26/04 C:33  Support: WA091/40129                           *
      *                Correct '88' & '89' processing.                *
      *                                                               *
      * 02/25/04 C:65  PROJECT: 114WBK/SUPPORT                        *
      *                POPULATE STATE FIELD ON NAME TYPE RECORDS.     *
     H*                                                               *
     H* 05/19/03 C:61  SUPPORT WA122 37201                            *
     H*              - If experience mod value 0 then report 1.0.     *
     H*              - Do not process if change is from 0->1 or 1->0. *
      *                                                               *
      * 09/10/02 C:65  PROJECT: SUPP#28063                            *
      *                Only write name endorsements(type 88) if the   *
      *                name does not match a previously entered name. *
      *                If the name does match then only write an      *
      *                address endorsement(type 89) reusing the       *
      *                entity code from the matching record.          *
      *                                                               *
      * 07/15/02 C:09  PROJECT: SEQ#28063                             *
      *                Changes made on 03/19/02 were resulting in     *
      *                no records being written to WXNEP on a NAME    *
      *                CHANGE endorsement.  Made fixes to code.       *
      *                                                               *
      * 03/19/02 C:90  PROJECT: SEQ#28063                             *
      *                On locations, assign the entity code belonging *
      *                to the EMP/DIV. Don't write WXNEP record if    *
      *                MA/PHY addresses are blank.                    *
      *                                                               *
      * 01/23/02 C:90  PROJECT: SEQ# 28063                            *
      *                Process both the mailing and the physical      *
      *                address of an entity/location.                 *
      *                                                               *
      * 08/24/01 C:09  PROJECT: SUPPORT/WA090/Seq# 28478              *
      *                Duplicate WMSHP/WXNEP records were written     *
      *                when user requested a MAILING or PHYSICAL      *
      *                address change but indicated to change BOTH    *
      *                addresses.                                     *
      *                                                               *
      * 07/23/01 C:65  PROJECT: 114WBK                                *
      *                LOAD THE STATE UNEMPLOYMENT NUMBER FOR ANY     *
      *                POLICIES INCLUDING NJ.                         *
      *                                                               *
      * 07/12/01 C:09  PROJECT: SUPPORT/WA091/Seq# 27236              *
      *                This is the same issue that was corrected below*
      *                (see 3/20/01 - Seq #26524) regarding an exp    *
      *                mod being removed and the mod effective date   *
      *                was not being reported properly.               *
      *                                                               *
      * 07/12/01 C:09  PROJECT: SUPPORT/WA091/Seq# 27278              *
      *                PROJECT: SUPPORT/WA091/Seq# 27321              *
      *                The first WXNEP record was not being loaded    *
      *                properly with address information when either  *
      *                a PHYSICAL ADDRESS CHANGE end was processed    *
      *                or an ADD OF ENTITY/LOCATION.  Corrected code  *
      *                to ensure intial WXNEP record has the address  *
      *                information.                                   *
      *                                                               *
      * 04/30/01 C:65  PROJECT: 114WAK                                *
      *                Load revised entity name with emplr name when  *
      *                writing the exp rating mod change endorsement. *
      *                ALSO LOAD ANY NEW STATES TO THE WMSHP POLICY   *
      *                RECORD ON AN ADD DIVISION ENDORSEMENT.         *
      *                                                               *
      * 06/27/01 C:09  PROJECT: WC410                                 *
      *                PROJECT: SUPPORT/WA101/Seq# 27754              *
      *                PROJECT: SUPPORT/WA091/Seq# 27322              *
      *                Issue with Add of Division that resulted in    *
      *                the wrong entity code (NAENCD,NEENCD) to be    *
      *                assigned for the new division.                 *
      *                Also, corrected code to load description for   *
      *                Add of Division into WXNEP record (NEDESC).    *
      *                No changes were made to address policy eff dt  *
      *                (NEPFDT) not being loaded but in testing it    *
      *                appears to be loading now (Seq #27322)         *
      *                                                               *
      * 06/01/01 C:35  PROJECT: WC878                                 *
      *                Load new fields for revised SIC code and       *
      *                revised number of employees.                   *
      *                                                               *
      * 03/20/01 C:09  PROJECT: WC410                                 *
      *                PROJECT: SUPPORT/WA095/Seq# 25258              *
      *                PROJECT: SUPPORT/WA096/Seq# 26524              *
      *                Previously, Physical Address changes were not  *
      *                reported to NCCI.  Per NCCI, any Physical Addr *
      *                change should now be reported the same as a    *
      *                change in location.                            *
      *                If exp mod change is made and the modifier     *
      *                is removed, we still need to report a valid    *
      *                modifier eff date (= policy effective date)    *
      *                                                               *
      * 09/14/00 C:09  PROJECT: SUPPORT/WA048/Seq# 23490              *
      *                         SUPPORT/WA095/Seq# 23754              *
      *                On Add of Location, make sure endorsement recs *
      *                link to the primary name (link code = 001)     *
      *                  On an Add of Entity, need to first write a   *
      *                record similar to name change (88) with the    *
      *                new entity code and also an address rec (89)   *
      *                with the new address linked to the new name.   *
      *                  When reporting all div/end/loc (SRALL) we    *
      *                will now force the link code to be 001 in      *
      *                order to be consistent with Add of Loc, any    *
      *                locations reported will have link code = 001.  *
      *                                                               *
      * 08/25/00 C:09  PROJECT: SUPPORT/WA095/Seq# 24025              *
      *                When assigning the next available entity code  *
      *                (NEENCD) we need to account for the fact that  *
      *                the policy information page program (W7413R)   *
      *                writes an agent record to WXNAP with NAENCD =  *
      *                999.  Previously, we added 1 to this and the   *
      *                result was 0!  We will make sure NAENCD = 999  *
      *                records are ignored when doing a SETGT/REDPE   *
      *                to determine the next available entity code.   *
      *                                                               *
      * 08/21/00 C:09  PROJECT: SUPPORT/WA096/Seq# 23928              *
      *                Make sure we report the state rpt division     *
      *                policy effective date on Add of Div, we were   *
      *                reporting 0 for this value.                    *
      *                                                               *
      * 07/31/00 C:09  PROJECT: SUPPORT/WA091/Seq# 23632              *
      *                Carrier # must be loaded into the before/after *
      *                data fields regardless of whether a change has *
      *                been made to this number (Per Carl G.)         *
      *                                                               *
      * 07/14/00 C:09  PROJECT: WC410                                 *
      *                Process was writing an extra blank address     *
      *                record when an entity/location add/del/chg or  *
      *                division add/del was processed.  Apply change  *
      *                to prevent this record from being written.     *
      *                Also, description field (NEDESC) was not being *
      *                loaded on an addition of entity/location.      *
      *                                                               *
      * 10/28/99 C:35  PROJECT: SUPPORT WA101 #20066                  *
      *                ALWAYS REPORT PHYSICAL ADDRESS FOR ENTITY/     *
      *                LOCATIONS.                                     *
      *                                                               *
      * 07/16/99 C:09  PROJECT: CM027                                 *
      *                PROCESS WRITING DUPLICATE INFORMATION; ONLY    *
      *                NEED TO PROCESS 1ST WMENP RECORD. THERE IS 1   *
      *                WMENP REC FOR EACH LINE PRINTED AT THE BOTTOM  *
      *                OF DOCUMENT WCPOLSV3/ENDORSE, PROCESS WAS      *
      *                WRITING INFO TO ELECTRONIC WORKFILES ONCE FOR  *
      *                EACH LINE PRINTED ON THE ENDORSEMENT!          *
      *                                                               *
      * 07/12/99 C:35  PROJECT: SUPPORT WA091 WCPOLS                  *
      *                USE THE LATEST EXPERIENCE MODIFIER INFO  FOR   *
      *                THE NCCI ENDORSEMENT RECORD.                   *
      *                                                               *
      * 06/21/99 C:35  PROJECT: SUPPORT WA096 #18378                  *
      *                PER NCCI, LOCATIONS SHOULD BE REPORTED WITH    *
      *                THE PHYSICAL ADDRESS; ENTITIES SHOULD BE       *
      *                REPORTED WITH THE MAILING ADDRESS.  IF THE     *
      *                THE MAILING ADDRESS IS SPECIFIED AND NOT       *
      *                AVAILABLE, USE THE PHYSICAL ADDRESS AND VICE   *
      *                VERSA.                                         *
      *                                                               *
      * 05/27/99 C:09  PROJECT: SUPPORT (WA048/SEQ# 17503)            *
      *                PROGRAM NEEDS TO RECOGNIZE ADDITION/DELETION   *
      *                OF DIVISION AS CHANGE IN WORKPLACE. CURRENTLY, *
      *                WE ONLY RECOGNIZE ADD/DELETE OF ENT/LOC.       *
      *                                                               *
      * 03/29/99 C:09  PROJECT: SUPPORT (WA091/SEQ# 17292)            *
      *                MODIFY PROGRAM TO ALSO RECOGNIZE A FEDERAL ID# *
      *                CHANGE AND REPORT TO NCCI LIKE A NAME CHANGE.  *
      *                                                               *
      * 01/20/99 C:113 PROJECT: SUPPORT MC099 #12406                  *
      *                Changed to not write 2 records to WXNEP when   *
      *                making an employer name change.                *
      *                                                               *
      * 01/05/99 C:35  PROJECT: SUPPORT WC091 15827                   *
      *                LOAD APPROPRIATE STATE CODE INTO WXNEP         *
      *                (FIELD NEST) FOR ADDRESS CHG ENDORSEMENTS.     *
      *                                                               *
      * 10/29/96 C:35  PROJECT WC525                                  *
      *                STATE CODE SHOULD COME FROM RATE SET MASTER    *
      *                FILE, IF AVAILABLE.  IF NOT, USE THE EMPLOYER  *
      *                PHYSICAL ADDRESS.  IF NO PHYSICAL ADDRESS, USE *
      *                MAILING.                                       *
      *                                                               *
      * 05/03/96 C:35  PROJECT WC525                                  *
      *                INIT NEW WMSHP FIELD SHPFDT FROM ENPFD1        *
      *                                                               *
      * 01/29/96 C:49  PROJECT WC410                                  *
      *                ONLY READ ENDORSEMENT RECORDS FOR THE CO#/FND  *
      *                BEING PROCESSED.  MOVE CHECK ON F2ESFL.        *
      *                MAINTAIN FILE POINTER WHEN WRITING RECORDS FOR *
      *                ALL ENTITIES AND LOCATIONS.                    *
      *                                                               *
      *===============================================================*
      *  INDICATOR USAGE:                                             *
      *    INDICATOR            FUNCTION DESCRIPTION                  *
      *                                                               *
      *       20      FUND ADDRESS LINE 2 IS NOT BLANK                *
      *===============================================================*
      *  FILE USAGE:                                                  *
      *    FILES:                                                     *
      *                                                               *
      *    WMENL3  (PEN01): CO#/FND/EMP#/DIV/FYR/JOB/END#             *
      *    WMEML   (PEM01): CO#/FND/EMP#/DIV                          *
      *    WXNAL11 (PNA01): CO#/FND/FYR/EMP#/DIV/ELID                 *
      *    WXNAL12 (PNA02): CO#/FND/EMP#/ENCD                         *
      *    SSYLF   (SWC01): WCKEY                                     *
      *    WMALL   (PAL01): CO#/FND/EMP#/DIV/ELID                     *
      *    WXNEP   (PNE01)                                            *
      *    WMSHP   (PSH01)                                            *
      *    WDF2L   (PF201): CO#/FND                                   *
      *===============================================================*
     FWMENL3    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWXNAL11   IF A E           K DISK    USROPN
     FWXNAL12   IF   E           K DISK    USROPN
     F                                     RENAME(PNA01:PNA02)
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FWMALL     IF   E           K DISK
     FWXNEP     O    E             DISK    USROPN
     FWMSHP     O    E             DISK
     FWDF2L     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
      **** BEGIN ADD **** 06/01/01 *********************************************
     FWDLDL     IF   E           K DISK
      **** END   ADD **** 06/01/01 *********************************************
      **** BEGIN ADD **** 04/30/01 *********************************************
     FWMSHL1    UF   E           K DISK
     F                                     RENAME(PSH01:PSH02)
     FWTGTL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      **** END   ADD **** 04/30/01 *********************************************
      **** BEGIN ADD **** 04/20/06 *********************************************
     FWDRRL     IF   E           K DISK
      **** END   ADD **** 04/20/06 *********************************************
     F***  Begin Del  ***  02/26/04  **************************************
      *****BEGIN ADD **** 09/10/02 *********************************************
     F**********WXNAL   IF  E           K        DISK
     F**********  PNA01                             KRENAMEPNA03
      *****END** ADD **** 09/10/02 *********************************************
     F***  End   Del  ***  02/26/04  **************************************
     F*** BEGIN ADD ***  07/23/07  ************************************
     FWMFNL     IF   E           K DISK

     D*** END ADD *****  07/23/07  ************************************
     *===============================================================*
     *  Array and Table Definitions
     *===============================================================*
     D ST              S              2    DIM(54) CTDATA PERRCD(27)
      **** BEGIN CHG **** 03/20/01 ************************************
     D***  Begin Chg  ***  04/09/04  **************************************
     D**********          MSG     1  20 30
     D MSG             S             30    DIM(30) CTDATA PERRCD(1)
     D***  End   Chg  ***  04/09/04  **************************************
      **** END CHG ****** 03/20/01 ************************************
     D*** BEGIN ADD ***  04/30/01  ************************************
     D*** END ADD *****  04/30/01  ************************************
     D***  Begin Add  ***  02/26/04  **************************************
     D***  End   Add  ***  02/26/04  **************************************
     D*** BEGIN ADD ***  04/14/16  ************************************
     D xxSubmit        s              1
     D xxExitSR        s              1
     D*** END ADD *****  04/14/16  ************************************
     *===============================================================*
     *  Data Structure and Constant Definitions
     *===============================================================*
     D***  Begin Add  ***  09/10/04  **************************************
     D RSTNLK          C                   CONST('WARSTNLKC')
     D***  End   Add  ***  09/10/04  **************************************
     D***  Begin Add  ***  02/26/04  **************************************
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               EFFCTV DATES
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANCEL DATES
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D***  End   Add  ***  02/26/04  **************************************
     D                 DS
     D  XXUNID                 1     12  0
     D  XXTIME                 1      6  0
     D  XXDATE                 7     12  0
     D*** BEGIN ADD ***  04/30/01  ************************************
     D                 DS
     D  SST                    1     30
     D                                     DIM(15)
     D  SHST01                 1      2
     D  SHST02                 3      4
     D  SHST03                 5      6
     D  SHST04                 7      8
     D  SHST05                 9     10
     D  SHST06                11     12
     D  SHST07                13     14
     D  SHST08                15     16
     D  SHST09                17     18
     D  SHST10                19     20
     D  SHST11                21     22
     D  SHST12                23     24
     D  SHST13                25     26
     D  SHST14                27     28
     D  SHST15                29     30
     D                 DS
     D  SSS                    1     15
     D                                     DIM(15)
     D  SHSS01                 1      1
     D  SHSS02                 2      2
     D  SHSS03                 3      3
     D  SHSS04                 4      4
     D  SHSS05                 5      5
     D  SHSS06                 6      6
     D  SHSS07                 7      7
     D  SHSS08                 8      8
     D  SHSS09                 9      9
     D  SHSS10                10     10
     D  SHSS11                11     11
     D  SHSS12                12     12
     D  SHSS13                13     13
     D  SHSS14                14     14
     D  SHSS15                15     15
     D*** BEG ADD *** 12/13/07 **********************************
     D PEOEXT          C                   CONST('L/C/F')
     D*** END ADD *** 12/13/07 **********************************
     D*** END ADD *****  04/30/01  ************************************
     D                 DS
     D  ENPOL1                 1     17
     D  XXFND1                 1      3
     D  XXEMP1                 5     13
     D  XXFYR1                15     17
     D                 DS
     D  ENPOL2                 1     17
     D  XXFND2                 1      3
     D  XXEMP2                 5     13
     D  XXFYR2                15     17
     D                 DS
     D  X1EMP#                 1     14  0
     D  X1EMP2                 1      9  0
     D  X1DIV2                10     14  0
     D                 DS
     D  X2EMP#                 1     14  0
     D  ALEMP#                 1      9  0
     D  ALDIV                 10     14  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     *===============================================================*
     *  KLIST Definitions
     *===============================================================*
      *

     C*** BEGIN ADD ***  07/23/07  ************************************
      *
      *  GROUP MASTER FILE
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*** END ADD *****  07/23/07  ************************************
      *
      *  EMPLOYER MASTER FILE
     C     EMKY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENEMP#
     C                   KFLD                    ENDIV
      *
     C     EMKY1         KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENEMP#
      *
     C*** BEGIN ADD ***  09/10/02  ************************************
     C     EMKY2         KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENEMP#
     C                   KFLD                    XXDIV
     C*** END ADD *****  09/10/02  ************************************
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C     EMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C***  End   Add  ***  02/26/04  **************************************
     C**** BEG ADD ******  12/13/07  **************************************
     C     EMKY4         KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    @EMP#1            9 0
     C                   KFLD                    @DIV#0            5 0
     C**** END ADD ******  12/13/07  **************************************
      ***** BEG ADD ******* 05/11/09 **********************************
      *
     C     EMKY5         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
      ***** END ADD ******* 05/11/09 **********************************
      *
     C*   NAME/ADDRESS TRANSACTION LOCATOR
     C     NAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
      *
     C     NAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C***  Begin Add  ***  09/10/04  **************************************
     C                   KFLD                    XXFYR
     C                   KFLD                    XXNLKC
     C***  End   Add  ***  09/10/04  **************************************
     C                   KFLD                    XXENCD
     C*
     C***  Begin Add  ***  09/10/04  **************************************
     C     *LIKE         DEFINE    NANLKC        XXNLKC
     C***  End   Add  ***  09/10/04  **************************************
      *
     C     NAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C***  Begin Add  ***  09/10/04  **************************************
     C                   KFLD                    XXFYR
     C***  End   Add  ***  09/10/04  **************************************
      **** BEGIN ADD **** 03/19/02 ************************************
      *
     C     NAKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      **** END   ADD **** 03/19/02 ************************************
     C**** BEGIN ADD **** 09/10/02 *********************************************
     C     NAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XAUNID           12 0
     C*
     C     NAKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C**** END ADD ****** 09/10/02 *********************************************
      *
      *  FUND AUXILIARY FILE
     C     F2KY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
      *
      *  ENTITY/LOCATION MASTER FILE
     C     ALKY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENEMP#
     C                   KFLD                    ENDIV
     C                   KFLD                    ENELID
      *
     C     ALKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
      *  ENDORSEMENT REQUEST FILE
     C     ENKY          KLIST
     C                   KFLD                    XQCO#
     C                   KFLD                    XQFND
     C                   KFLD                    XQEMP#
     C                   KFLD                    XQDIV
     C                   KFLD                    XQFYR
     C                   KFLD                    XQJOB
     C                   KFLD                    XQEND#
      *
      *  EMPLOYER DETAIL FILE
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C     ELKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C***  End   Add  ***  02/26/04  **************************************
      *
      *  RATE SET CODE MASTER FILE
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
      **** BEGIN ADD **** 06/01/01 *********************************************
      *
      * ENTITY/LOCATION DETAIL FILE
      *
     C     LDKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
     C                   KFLD                    ALELID
     C                   KFLD                    ELFYR
      **** END   ADD **** 06/01/01 *********************************************
      **** BEGIN ADD **** 04/30/01 *********************************************
      *
      * SUBMISSION HISTORY FILE
      *
     C     SHKY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENEMP#
     C                   KFLD                    ENFYR
     C                   KFLD                    XXTYPE            1
     C*
     C*  KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
      *  EMPLOYER DETAIL FILE
     C     ELKY1         KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFND
     C                   KFLD                    ENFYR
     C                   KFLD                    ENEMP#
      **** END   ADD **** 04/30/01 *********************************************
      **** BEGIN ADD **** 04/20/06 *********************************************
     C*
     C*  STAT RPT/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT            7 0
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      **** END   ADD **** 04/20/06 *********************************************
     *===============================================================*
     *  *LIKE DEFN Definitions
     *===============================================================*
      ***** BEG ADD ******* 05/19/03 **********************************
     C     *LIKE         DEFINE    ENEM32        XXNMOD
     C     *LIKE         DEFINE    ENEM31        XXOMOD
      ***** END ADD ******* 05/19/03 **********************************
     C     *LIKE         DEFINE    NAENCD        XXENCD
     C     *LIKE         DEFINE    NESEQ#        XXSEQ#
     C     *LIKE         DEFINE    ALELID        XXELID
     C     *LIKE         DEFINE    NACO#         XXCO#
     C     *LIKE         DEFINE    NAFND         XXFND
     C     *LIKE         DEFINE    NAEMP#        XXEMP#
     C     *LIKE         DEFINE    NADIV         XXDIV
     C     *LIKE         DEFINE    NAFYR         XXFYR
     C     *LIKE         DEFINE    EMPST         XXST
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    NACO#         XQCO#
     C     *LIKE         DEFINE    NAFND         XQFND
     C     *LIKE         DEFINE    ENEMP#        XQEMP#
     C     *LIKE         DEFINE    ENDIV         XQDIV
     C     *LIKE         DEFINE    ENFYR         XQFYR
     C     *LIKE         DEFINE    ENJOB         XQJOB
     C     *LIKE         DEFINE    ENEND#        XQEND#
      **** BEGIN ADD **** 04/30/01 *********************************************
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
      **** END   ADD **** 04/30/01 *********************************************
      **** BEGIN ADD **** 01/23/02 ************************************
     C     *LIKE         DEFINE    EMPAD1        XXPAD1
     C     *LIKE         DEFINE    EMPAD2        XXPAD2
     C     *LIKE         DEFINE    EMPCTY        XXPCTY
     C     *LIKE         DEFINE    EMPST         XXPST
     C     *LIKE         DEFINE    EMPZIP        XXPZIP
      **** END   ADD **** 01/23/02 ************************************
     C*** BEGIN ADD ***  09/10/02  ************************************
     C     *LIKE         DEFINE    EMNAME        XXEMNM
     C*** END ADD *****  09/10/02  ************************************
     *===============================================================*
     *  PLIST Definitions
     *===============================================================*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQJOB             6
     C                   PARM                    QQEND#            5
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     *===============================================================*
     *  Main
     *===============================================================*
      ***** BEG ADD ******* 05/11/09 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      ***** END ADD ******* 05/11/09 **********************************
     C                   MOVE      QQCO#         XQCO#
     C                   MOVE      QQFND         XQFND
     C                   MOVE      QQEMP#        XQEMP#
     C                   MOVE      QQDIV         XQDIV
     C                   MOVE      QQFYR         XQFYR
     C                   MOVE      QQJOB         XQJOB
     C                   MOVE      QQEND#        XQEND#
      **** BEGIN ADD **** 04/30/01 *********************************************
     C                   MOVE      'P'           XXTYPE
      **** END   ADD **** 04/30/01 *********************************************
      *
     C                   TIME                    XXUNID
     C     'WC'          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
     C     WCFL03        ANDEQ     'Y'
      *
     C                   OPEN      WXNAL11
     C                   OPEN      WXNAL12
     C                   OPEN      WXNEP
      *
      *  READ ALL ENDORSEMENT RECORDS TO BE PROCESSED
     C     ENKY          CHAIN     PEN01                              50
     C     *IN50         IFEQ      *OFF
     C                   Z-ADD     *ZEROS        NECAR1
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     F2CAR#        NECAR1
     C                   END                                                    *IN60=*OFF
     C                   END                                                    *IN50=*OFF
      *
     C*** BEGIN ADD ***  09/10/02  ************************************
     C                   MOVE      *BLANKS       XXEMNM
     C                   Z-ADD     ENAGT2        XXDIV
     C     EMKY2         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXEMNM
     C                   END
     C*** END ADD *****  09/10/02  ************************************
     C*** BEGIN ADD ***  04/14/16  ************************************
     C                   Clear                   xxSubmit
     C                   Clear                   xxExitSR
     C*** END ADD *****  04/14/16  ************************************
      *
     C     *IN50         DOWEQ     *OFF
      *
     C*** BEGIN ADD ***  12/13/06  *************************************
     C                   Z-ADD     0             SVPFDT            7 0
     C                   MOVE      ENFYR         XXFYR
      *
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMSTRP        IFEQ      'Y'
     C     ELKY2         CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C*** BEG ADD ****** 12/13/07 *************************************
     C*
     C                   MOVE      ENCO#         XXCO#
     C                   MOVE      ENFND         XXFND
     C*
     C     EMMAD1        IFNE      *BLANK
     C                   MOVEL     EMMST         XXST
     C                   ELSE
     C                   MOVEL     EMPST         XXST
     C                   ENDIF                                                  EMMAD1 IFNE
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      RRPGFL        @RRPGF            1
      *
     C     @RRPGF        IFEQ      '1'
     C     @RRPGF        OREQ      '3'
     C     @RRPGF        OREQ      ' '
      *
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     FNPEOF        IFNE      *BLANKS
      *
      * CHAIN TO WMEML TO GET THE FIRST EMPLOYER NAME/FEDERAL NAME
      *
     C                   MOVE      1             @EMP#1
     C                   MOVE      0             @DIV#0
      *
     C     EMKY4         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      '0'
      *
     C                   MOVEL     EMNAME        @EMNM            40
     C                   MOVEL     EMNAM2        @EMN2            60
      *
     C                   MOVEL     EMFDNM        @EMFD            40
     C                   MOVEL     EMFDN2        @EMF2            60
      *
     C                   ENDIF                                                  *IN60 IFEQ '0'
     C                   ENDIF                                                  FNPEOF IFNE
     C                   ENDIF                                                  RRPGFL IFEQ '1'
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*** END ADD ****** 12/13/07  ************************************
     C                   Z-ADD     ELPFDT        SVPFDT
     C                   LEAVE
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  EMSTRP IFEQ
      *
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  12/13/06  *************************************
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD ***  01/20/12  *************************************
     C     FNKY          CHAIN     PFN01                              60
     C*** END ADD *****  01/20/12  *************************************
      *
      *  SELECTED ENDORSEMENT CHANGES WILL ONLY BE SUBMITTED TO NCCI
      *  IF THE CURRENT DIVISION IS STATE REPORTING
      *
     C     EMSTRP        IFEQ      'Y'
     C     ENADDF        OREQ      'Y'
     C     ENDLDF        OREQ      'Y'
      ***** BEG ADD ******* 08/18/05 **********************************
     C     ENRDDF        OREQ      'Y'
      ***** END ADD ******* 08/18/05 **********************************
      **** BEGIN ADD **** 01/23/02 ************************************
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      EMPAD1        XXPAD1
     C                   MOVE      EMPAD2        XXPAD2
     C                   MOVE      EMPCTY        XXPCTY
     C                   MOVE      EMPST         XXPST
     C                   MOVE      EMPZIP        XXPZIP
     C                   ENDIF
      **** END   ADD **** 01/23/02 ************************************
      *
      *  PROCESS FOR THE FOLLOWING ENDORSEMENTS: NAME CHANGE, EXP
      *  RATING MOD CHANGE, CONT. MOD CHG, CERT. OF INS CHANGE,
      *  NOMINAL CHG, ADD DIV, DEL DIV
     C     ENNCFL        IFEQ      'Y'
     C     ENERFL        OREQ      'Y'
     C     ENCMFL        OREQ      'Y'
     C     ENADFL        OREQ      'Y'
     C     ENMAFL        ANDEQ     'Y'
     C     ENADFL        OREQ      'Y'
     C     ENCBAF        ANDEQ     'Y'
     C     ENCELF        OREQ      'Y'
     C     ENAELF        OREQ      'Y'
     C     ENDELF        OREQ      'Y'
     C     ENCIFL        OREQ      'Y'
     C     ENNOFL        OREQ      'Y'
     C     ENFIFL        OREQ      'Y'
     C     ENADDF        OREQ      'Y'
     C     ENDLDF        OREQ      'Y'
      ***** BEG ADD ******* 08/18/05 **********************************
     C     ENRDDF        OREQ      'Y'
      ***** END ADD ******* 08/18/05 **********************************
      **** BEGIN ADD **** 03/20/01 ************************************
     C     ENADFL        OREQ      'Y'
     C     ENPAFL        ANDEQ     'Y'
      **** END ADD ****** 03/20/01 ************************************
      *
     C                   SELECT
      *
      *  EXPERIENCE RATING MOD CHANGE
     C     ENERFL        WHENEQ    'Y'
      *
     C     ENEM11        IFNE      ENEM12
     C     ENEM21        ORNE      ENEM22
     C     ENEM31        ORNE      ENEM32
     C     ENED11        ORNE      ENED12
     C     ENED21        ORNE      ENED22
     C     ENED31        ORNE      ENED32
      ***** BEG DEL ******* 05/19/03 **********************************
     C*********            EXSR SRWRIT
      ***** END DEL ******* 05/19/03 **********************************
      ***** BEG ADD ******* 05/19/03 **********************************
      *
     C                   SELECT
      *
     C     ENEM32        WHENNE    ENEM31
     C     ENED32        ORNE      ENED31
     C                   Z-ADD     ENEM32        XXNMOD
     C                   Z-ADD     ENEM31        XXOMOD
      *
     C     ENEM22        WHENNE    ENEM21
     C     ENED22        ORNE      ENED21
     C                   Z-ADD     ENEM22        XXNMOD
     C                   Z-ADD     ENEM21        XXOMOD
      *
     C                   OTHER
     C                   Z-ADD     ENEM12        XXNMOD
     C                   Z-ADD     ENEM11        XXOMOD
      *
     C                   ENDSL
      *
     C                   SELECT
      *
     C     XXOMOD        WHENEQ    1
     C     XXNMOD        ANDEQ     0
      *                     Do not submit
      *
     C     XXOMOD        WHENEQ    0
     C     XXNMOD        ANDEQ     1
      *                     Do not submit
      *
     C                   OTHER
     C                   EXSR      SRWRIT
      *
     C                   ENDSL
      *
      ***** END ADD ******* 05/19/03 **********************************
     C                   ENDIF
      *
      *  CONTINGENT MODIFIER CHANGE
     C     ENCMFL        WHENEQ    'Y'
      *
     C     ENEM11        IFNE      ENEM12
     C     ENED11        ORNE      ENED12
     C                   EXSR      SRWRIT
     C                   ENDIF
      *
      *  NAME CHANGE; ONLY PROCESS IF DBA NAME OR TYPE OF
      *  BUSINESS CHANGES
     C     ENNCFL        WHENEQ    'Y'
     C     ENFIFL        OREQ      'Y'
     C     ENTBFL        OREQ      'Y'
      *
     C***  Begin Chg  ***  09/22/04  **************************************
     C********** ENNAM1    IFNE ENNAM2
     C     ENENM1        IFNE      ENANM1
     C     ENENM2        ORNE      ENANM2
     C***  End   Chg  ***  09/22/04  **************************************
     C     ENTOB1        ORNE      ENTOB2
     C     ENFIFL        OREQ      'Y'
      *
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
      *
     C                   EXSR      SRWRIT
     C                   ENDIF
     C                   ENDIF
      *
      *  ADDRESS CHANGE; ONLY PROCESS IF MAILING ADDRESS CHANGES
     C     ENADFL        WHENEQ    'Y'
     C     ENMAFL        ANDEQ     'Y'
     C     ENADFL        OREQ      'Y'
     C     ENCBAF        ANDEQ     'Y'
      **** BEGIN ADD **** 03/20/01 ************************************
     C     ENADFL        OREQ      'Y'
     C     ENPAFL        ANDEQ     'Y'
      **** END ADD ****** 03/20/01 ************************************
      *
      **** BEGIN ADD **** 03/20/01 ************************************
     C     ENADFL        IFEQ      'Y'
     C     ENMAFL        ANDEQ     'Y'
     C     ENADFL        OREQ      'Y'
     C     ENCBAF        ANDEQ     'Y'
      **** END ADD ****** 03/20/01 ************************************
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
      *
     C     ENMA11        IFNE      ENMA12
     C     ENMA21        ORNE      ENMA22
     C     ENMCT1        ORNE      ENMCT2
     C     ENMST1        ORNE      ENMST2
     C     ENMZP1        ORNE      ENMZP2
     C                   EXSR      SRWRIT
     C                   END
     C                   END
      **** BEGIN ADD **** 03/20/01 ************************************
     C                   END                                                    MAIL ADDR CHG
      *
     C     ENADFL        IFEQ      'Y'
     C     ENPAFL        ANDEQ     'Y'
      **** BEGIN DEL **** 08/24/01 ************************************
     C***********ENADFL****OREQ 'Y'
     C***********ENCBAF****ANDEQ'Y'
      **** END DEL ****** 08/24/01 ************************************
      *
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
      **** BEGIN ADD **** 08/24/01 ************************************
     C     ENCBAF        IFNE      'Y'
      **** END ADD ****** 08/24/01 ************************************
      **** BEGIN ADD **** 07/12/01 ************************************
     C     ENPA11        IFNE      ENPA12
     C     ENPA21        ORNE      ENPA22
     C     ENPCT1        ORNE      ENPCT2
     C     ENPST1        ORNE      ENPST2
     C     ENPZP1        ORNE      ENPZP2
      **** END ADD ****** 07/12/01 ************************************
     C                   EXSR      SRWRIT
      **** BEGIN ADD **** 07/12/01 ************************************
     C                   END
      **** END ADD ****** 07/12/01 ************************************
      **** BEGIN ADD **** 08/24/01 ************************************
     C                   END
      **** END ADD ****** 08/24/01 ************************************
     C                   END
      *
     C                   END                                                    PHYS ADDR CHG
      **** END ADD ****** 03/20/01 ************************************
      *
      *
      * CHANGE/ADD/DELETE OF AN ENTITY OR LOCATION
      * CHANGE OF PHYSICAL ADDRESS
      *
     C     ENCELF        WHENEQ    'Y'
     C     ENAELF        OREQ      'Y'
     C     ENDELF        OREQ      'Y'
      *
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
     C                   EXSR      SRWRIT
     C                   END
      *
      * Addition or Deletion of Division
     C     ENADDF        WHENEQ    'Y'
     C     ENDLDF        OREQ      'Y'
      ***** BEG ADD ******* 08/18/05 **********************************
     C     ENRDDF        OREQ      'Y'
      ***** END ADD ******* 08/18/05 **********************************
      *
      **** BEGIN ADD **** 04/30/01 *********************************************
     C     ENADDF        IFEQ      'Y'
     C                   EXSR      UPDPSH
     C                   ENDIF
      **** END   ADD **** 04/30/01 *********************************************
      *
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
     C                   EXSR      SRWRIT
     C                   END
      *
      *  OTHER-NOMINAL TYPE CHANGE;  ONLY PROCESS IF TYPE OF
      *  BUSINESS CHANGES
     C     ENTBFL        WHENEQ    'Y'
      *
     C     ENTOB2        IFNE      ENTOB1
     C                   EXSR      SRWRIT
     C                   END
      *
      *  OTHER-CERT OF INSURANCE CHANGE
     C     ENCIFL        WHENEQ    'Y'
      *
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
      *
     C     ENNAM1        IFNE      ENNAM2
     C     ENPOL1        ORNE      ENPOL2
     C     ENCAR1        ORNE      ENCAR2
     C     ENTOB1        ORNE      ENTOB2
     C     ENPFD1        ORNE      ENPFD2
     C     ENPTD1        ORNE      ENPTD2
     C     ENAGT#        ORNE      ENAGT2
     C                   EXSR      SRWRIT
     C                   END
     C                   END
      *
     C                   ENDSL
      *
     C                   END                                                    ST RPT CHGS
      *
     C                   ELSE                                                   EMSTRP FLG
      *
      *  ADDRESS CHANGE ENDORSEMENTS ARE SUBMITTED TO NCCI REGARDLESS
      *  OF WHETHER THE CURRENT DIVISION IS STATE REPORTING
      *
     C                   SELECT
      *  ADDRESS CHANGE; ONLY PROCESS IF MAILING ADDRESS CHANGES
     C     ENADFL        WHENEQ    'Y'
     C     ENMAFL        ANDEQ     'Y'
     C     ENADFL        OREQ      'Y'
     C     ENCBAF        ANDEQ     'Y'
      **** BEGIN ADD **** 03/20/01 ************************************
     C     ENADFL        OREQ      'Y'
     C     ENPAFL        ANDEQ     'Y'
      **** END ADD ****** 03/20/01 ************************************
      *
      **** BEGIN ADD **** 03/20/01 ************************************
     C     ENADFL        IFEQ      'Y'
     C     ENMAFL        ANDEQ     'Y'
     C     ENADFL        OREQ      'Y'
     C     ENCBAF        ANDEQ     'Y'
      **** END ADD ****** 03/20/01 ************************************
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
      *
     C     ENMA11        IFNE      ENMA12
     C     ENMA21        ORNE      ENMA22
     C     ENMCT1        ORNE      ENMCT2
     C     ENMST1        ORNE      ENMST2
     C     ENMZP1        ORNE      ENMZP2
     C                   EXSR      SRWRIT
     C                   END
     C                   END
      **** BEGIN ADD **** 03/20/01 ************************************
     C                   END                                                    MAIL ADDR CHG
      *
     C     ENADFL        IFEQ      'Y'
     C     ENPAFL        ANDEQ     'Y'
      **** BEGIN DEL **** 08/24/01 ************************************
     C***********ENADFL****OREQ 'Y'
     C***********ENCBAF****ANDEQ'Y'
      **** END DEL ****** 08/24/01 ************************************
      *
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
      **** BEGIN ADD **** 08/24/01 ************************************
     C     ENCBAF        IFNE      'Y'
      **** END ADD ****** 08/24/01 ************************************
      **** BEGIN ADD **** 07/12/01 ************************************
     C     ENPA11        IFNE      ENPA12
     C     ENPA21        ORNE      ENPA22
     C     ENPCT1        ORNE      ENPCT2
     C     ENPST1        ORNE      ENPST2
     C     ENPZP1        ORNE      ENPZP2
      **** END ADD ****** 07/12/01 ************************************
     C                   EXSR      SRWRIT
      **** BEGIN ADD **** 07/12/01 ************************************
     C                   END
      **** END ADD ****** 07/12/01 ************************************
      **** BEGIN ADD **** 08/24/01 ************************************
     C                   END
      **** END ADD ****** 08/24/01 ************************************
     C                   END
      *
     C                   END                                                    PHYS ADDR CHG
      **** END ADD ****** 03/20/01 ************************************
      *
      * CHANGE/ADD/DELETE OF AN ENTITY OR LOCATION
      *
     C     ENCELF        WHENEQ    'Y'
     C     ENAELF        OREQ      'Y'
     C     ENDELF        OREQ      'Y'
     C***  Begin Add  ***  02/26/04  **************************************
     C     ENNCFL        OREQ      'Y'
     C***  End   Add  ***  02/26/04  **************************************
     C***  Begin Add  ***  01/20/12  **************************************
     C     ENFIFL        OREQ      'Y'
     C     FNPEOS        ANDEQ     'Y'
     C***  End   Add  ***  01/20/12  **************************************
      *
     C     ENREQ#        IFEQ      1
     C     ENSEQ#        ANDEQ     0
     C                   EXSR      SRWRIT
     C                   END
      **** BEGIN ADD **** 08/24/01 ************************************
      *
      *  THIS CODE IS ALREADY ACCOUNTED FOR (SEE ABOVE)
      *
      **** END ADD ****** 08/24/01 ************************************
      **** BEGIN DEL **** 08/24/01 ************************************
      **** BEGIN ADD **** 03/20/01 ************************************
      ***********
      ***ADDRESS*CHANGE; ONLY PROCESS IF MAILING ADDRESS CHANGES
      ***********
     C***********ENADFL    WHEQ 'Y'
     C***********ENPAFL    ANDEQ'Y'
     C***********ENADFL    OREQ 'Y'
     C***********ENCBAF    ANDEQ'Y'
      ***********
     C***********ENREQ#    IFEQ 1
     C***********ENSEQ#    ANDEQ0
      ***********
     C***********ENPA11    IFNE ENPA12
     C***********ENPA21    ORNE ENPA22
     C***********ENPCT1    ORNE ENPCT2
     C***********ENPST1    ORNE ENPST2
     C***********ENPZP1    ORNE ENPZP2
     C***********          EXSR SRWRIT
     C***********          END
     C***********          END
      **** END ADD ****** 03/20/01 ************************************
      **** END DEL ****** 08/24/01 ************************************
      *
     C                   ENDSL
      *
     C                   END                                                    EMSTRP='Y'
     C                   END                                                    *IN60=*OFF
     C*** BEGIN ADD ***  04/14/16  ************************************
      /free
                         Chain(n) (ENCo#:ENFnd) pfn01;
                         If %found and FNPeoS = 'Y' and xxSubmit = ' ';
                            xxExitSR = 'Y';
                            Exsr SRWrit;
                         Endif;
      /end-free
     C*** END ADD *****  04/14/16  ************************************
      *
     C     ENKY          READE     PEN01                                  50
     C                   END                                                    DOW *IN50 *OFF
      *
     C                   END
      *
     C                   MOVE      '1'           *INLR
     C***  Begin Add  ***  02/26/04  **************************************
    C*====================================================================
    C*  ADDDIV - Addition of Division processing
    C*====================================================================
     C     ADDDIV        BEGSR
     C*
     C                   MOVE      'Y'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C*
     C                   ENDSR                                                  ADDDIV
    C*====================================================================
    C*  ADDENT - Addition of Entity processing
    C*====================================================================
     C     ADDENT        BEGSR
     C*
     C                   MOVE      'Y'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C*
     C                   ENDSR                                                  ADDENT
    C*====================================================================
    C*  ADDLOC - Addition of Location processing
    C*====================================================================
     C     ADDLOC        BEGSR
     C*
     C                   MOVE      'N'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C*
     C                   ENDSR                                                  ADDLOC
    C*====================================================================
    C*  CHGENT - Change of Entity processing
    C*====================================================================
     C     CHGENT        BEGSR
     C*
     C     ENOEN1        IFNE      ENOEN2
     C     ENOEN2        IFNE      *BLANKS
     C                   MOVE      'Y'           ALLNAM
     C                   ENDIF
     C                   ENDIF                                                  Name Chg
     C*
     C     ENOM11        IFNE      ENOM12
     C     ENOM21        ORNE      ENOM22
     C     ENOMC1        ORNE      ENOMC2
     C     ENOMS1        ORNE      ENOMS2
     C     ENOMZ1        ORNE      ENOMZ2
     C     ENOM12        IFNE      *BLANKS
     C                   MOVE      'Y'           ALLLOC
     C                   ENDIF
     C                   ENDIF                                                  Mail Address Chg
     C*
     C     ENOP11        IFNE      ENOP12
     C     ENOP21        ORNE      ENOP22
     C     ENOPC1        ORNE      ENOPC2
     C     ENOPS1        ORNE      ENOPS2
     C     ENOPZ1        ORNE      ENOPZ2
     C     ENOP12        IFNE      *BLANKS
     C                   MOVE      'Y'           ALLLOC
     C                   ENDIF
     C                   ENDIF                                                  Phys Address Chg
     C*
     C                   ENDSR                                                  CHGENT
    C*====================================================================
    C*  CHGLOC - Change of Location processing
    C*====================================================================
     C     CHGLOC        BEGSR
     C*
     C                   MOVE      'N'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C*
     C                   ENDSR                                                  CHGLOC
    C*====================================================================
    C*  DLTDIV - Deletion of Division processing
    C*====================================================================
     C     DLTDIV        BEGSR
     C*
     C                   MOVE      'Y'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C*
     C                   ENDSR                                                  DLTDIV
    C*====================================================================
    C*  DLTENT - Deletion of Entity processing
    C*====================================================================
     C*
     C     DLTENT        BEGSR
     C*
     C                   MOVE      'Y'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C*
     C                   ENDSR                                                  DLTENT
    C*====================================================================
    C*  DLTLOC - Deletion of Location processing
    C*====================================================================
     C     DLTLOC        BEGSR
     C*
     C                   MOVE      'N'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C*
     C                   ENDSR                                                  DLTLOC
    C*====================================================================
    C*  GETCX - Determine if the employer is CX'd
    C*====================================================================
     C     GETCX         BEGSR
     C*
     C     ELKY2         CHAIN     PEL01                              61
     C*
     C                   MOVE      *ON           NOTCX             1
     C                   Z-ADD     1             I1                3 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF
     C*
     C     I1            IFNE      0
     C     CX(I1)        IFNE      *ZERO
     C     CX(I1)        ANDLT     ELPTDT
     C                   MOVE      *OFF          NOTCX
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  GETCX
     C***  End   Add  ***  02/26/04  **************************************
     C*
     C***  Begin Add  ***  08/17/05  **************************************
     C*====================================================================
     C* GETEXP - Get Exposure Link record
     C*====================================================================
     C     GETEXP        BEGSR
     C*
     C* Determine if Exposure Link will need to be incremented.
     C*
     C                   MOVE      *BLANK        XXADD1            1
     C                   MOVE      ENCO#         XXCO#
     C                   MOVE      ENFND         XXFND
     C                   MOVE      ENEMP#        XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   MOVE      ENFYR         XXFYR
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           XXADD1
     C                   ENDIF
     C*
     C                   Z-ADD     0             SVELCD            5 0
     C*
     C     ELKY1         SETGT     PEL02
     C     ELKY1         READPE    PEL02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     ELDIV         ADD       1             SVELCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXADD1        IFEQ      'Y'
     C                   ADD       1             SVELCD
     C                   ENDIF                                                  XXADD1 IFEQ
     C*
     C                   ENDSR                                                  GETEXP
     C***  End   Add  ***  08/17/05  **************************************
     C*
     C***  Begin Add  ***  09/10/04  **************************************
     C*====================================================================
     C* NAMLNK - Name link counter incremented
     C*====================================================================
     C*
     C     NAMLNK        BEGSR
     C*
     C     *LIKE         DEFINE    SHUNID        X2UNID
     C                   Z-ADD     XXUNID        X2UNID
     C*
     C     NANLKC        IFEQ      0
     C                   CALL      RSTNLK
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXEMP#
     C                   PARM                    XXFYR
     C                   PARM                    X2UNID
     C                   PARM                    NANLKC
     C                   ENDIF
     C*
     C                   ADD       1             NANLKC
     C                   Z-ADD     1             NEENCD
     C*
     C                   ENDSR                                                  NAMLNK
     C***  End  Add  ***  09/10/04  **************************************
     C*
     *===============================================================*
     *  SRALL - Subroutine to submit all divisions/entities/locations
     *===============================================================*
     C     SRALL         BEGSR
      *
      *  SUBMIT ALL DIVISIONS/ENTITIES/LOCATIONS
     C     EMKY1         CHAIN     PEM01                              40
     C     *IN40         DOWEQ     *OFF
     C**** BEG ADD ****** 12/13/07 ************************************
     C                   MOVEL     EMNAME        @@EMNM           40
     C                   MOVEL     EMNAM2        @@EMN2           40
     C**** END ADD ****** 12/13/07 ************************************
      *
     C     ENDIV         IFNE      EMDIV
      **** BEGIN ADD **** 09/14/00 ************************************
     C     ENELID        ORNE      *BLANKS
      **** END ADD ****** 09/14/00 ************************************
      **** BEGIN DEL **** 09/14/00 ************************************
     C***********ENCELF    OREQ 'Y'
     C***********ENAELF    OREQ 'Y'
     C***********ENDELF    OREQ 'Y'
     C***********ENADDF    OREQ 'Y'
     C***********ENDLDF    OREQ 'Y'
      **** END DEL ****** 09/14/00 ************************************
      *
     C***  Begin Add  ***  05/10/04  **************************************
     C                   MOVE      'N'           PAONLY
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      'Y'           PAONLY
     C                   ENDIF
     C***  End   Add  ***  05/10/04  **************************************
     C*
     C                   EXSR      SRWRT2
      *
     C                   END                                                    XXDIV<>SHDIV
     C     EMKY1         READE     PEM01                                  40
      *
     C                   END                                                    DOW *IN40 *OFF
      *
     C     EMKY1         CHAIN     PEM01                              45
     C     *IN45         DOWEQ     *OFF
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      GETCX
     C*
     C     NOTCX         IFEQ      *ON
     C***  End   Add  ***  02/26/04  **************************************
      *
     C     ALKY2         CHAIN     PAL01                              46
     C     *IN46         DOWEQ     *OFF
      *
     C                   Z-ADD     ENEMP#        X1EMP2
     C                   Z-ADD     ENDIV         X1DIV2
     C     ENELID        IFNE      ALELID
     C     ENNCFL        OREQ      'Y'
     C     X1EMP#        ORNE      X2EMP#
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C*  Do NOT write a name change record for Locations, PDS system does
     C*   NOT allow for the name to be different on a Location.
     C*
     C     ALELTP        IFEQ      'L'
     C     NETYPE        ANDEQ     '88'
     C                   ELSE
     C***  End   Add  ***  02/26/04  **************************************
      *
     C***  Begin Del  ***  02/26/04  **************************************
     C********** ALDLDT    IFEQ *ZEROS
     C***  End   Del  ***  02/26/04  **************************************
      *
      **** BEGIN ADD **** 01/23/02 ************************************
     C                   MOVE      'M'           XXMFLG            1
      **** END   ADD **** 01/23/02 ************************************
     C***  Begin Add  ***  02/26/04  **************************************
     C     ALMAD1        IFEQ      *BLANKS
     C                   MOVE      'P'           XXMFLG
     C                   ENDIF
     C***  End   Add  ***  02/26/04  **************************************
     C*
     C                   EXSR      SRWRT3
      **** BEGIN ADD **** 01/23/02 ************************************
      *
      *  IF PHYSICAL ADDRESS IS DIFFERENT, WRITE A RECORD
      *
     C***  Begin Chg  ***  02/26/04  **************************************
     C********** ALPAD1    IFNE EMPAD1
     C********** ALPAD1    ANDNE*BLANKS
     C********** ALPAD2    ORNE EMPAD2
     C********** ALPAD2    ANDNE*BLANKS
     C********** ALPCTY    ORNE EMPCTY
     C********** ALPCTY    ANDNE*BLANKS
     C********** ALPST     ORNE EMPST
     C********** ALPST     ANDNE*BLANKS
     C********** ALPZIP    ORNE EMPZIP
     C********** ALPZIP    ANDNE*ZERO
     C     NETYPE        IFEQ      '89'
     C     ALLLOC        ANDEQ     'Y'
     C     XXMFLG        ANDNE     'P'
     C***  End   Chg  ***  02/26/04  **************************************
     C***  Begin Add  ***  08/17/05  **************************************
     C     ALPAD1        ANDNE     *BLANKS
     C***  End   Add  ***  08/17/05  **************************************
      *
     C     ALPAD1        IFNE      ALMAD1
     C     ALPAD2        ORNE      ALMAD2
     C     ALPCTY        ORNE      ALMCTY
     C     ALPST         ORNE      ALMST
     C     ALPZIP        ORNE      ALMZIP
      *
     C                   MOVE      'P'           XXMFLG            1
     C                   EXSR      SRWRT3
     C                   ENDIF
     C                   ENDIF
      *
      **** END   ADD **** 01/23/02 ************************************
      *
     C***  Begin Del  ***  02/26/04  **************************************
     C**********           END                             ALDLDT = 0
     C***  End   Del  ***  02/26/04  **************************************
      *
     C                   END                                                    ENELID<>ALELID
     C*
     C***  Being Add  ***  02/26/04  **************************************
     C                   ENDIF                                                  ALELTP=L
     C***  End   Add  ***  02/26/04  **************************************
      *
     C     ALKY2         READE     PAL01                                  46
     C                   END                                                    DOW *IN46 *OFF
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   ENDIF                                                  NOTCX
     C***  End   Add  ***  02/26/04  **************************************
      *
     C     EMKY1         READE     PEM01                                  45
     C                   END                                                    DOW *IN45 *OFF
     C**** BEG ADD ****  12/13/07  ************************************
      *
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     @RRPGF        IFEQ      '1'
     C     FNPEOF        ANDNE     *BLANKS
      *
     C     ENEMP#        IFNE      1
     C     ENDIV         ORNE      0
      *
     C     EMKY4         CHAIN     PEM01                              60
      *
     C                   MOVE      'Y'           XXPEO
     C                   EXSR      SRWRT2
     C                   MOVE      ' '           XXPEO
     C                   ENDIF                                                  ENEMP# IFNE '1'
      *
     C                   ENDIF                                                  @RRPGF IFEQ '1'
     C**** END ADD ****  12/13/07  ************************************
      *
     C                   ENDSR                                                  SRALL
     *===============================================================*
     *  SRWRIT - Subroutine to write the records
     *===============================================================*
     C     SRWRIT        BEGSR
      *
     C*** BEGIN ADD ***  04/14/16  ************************************
     C                   Eval      xxSubmit = 'Y'
     C*** END ADD *****  04/14/16  ************************************
     C*** BEG ADD *****  12/13/07  ************************************
     C                   MOVE      *BLANKS       XXPEO             1
     C*** END ADD *****  12/13/07  ************************************
     C                   ADD       1             XXTIME
     C                   Z-ADD     *ZEROS        XXSEQ#
     C*** BEGIN ADD ***  07/23/01  ************************************
     C                   MOVE      *BLANKS       XXSST            30
     C     SHKY          CHAIN     PSH02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEA     SST           XXSST
     C                   ENDIF
     C                   CLEAR                   PSH02
     C                   MOVEA     XXSST         SST
     C     1             DO        15            I
     C     SST(I)        IFNE      *BLANKS
     C                   MOVE      '3'           SSS(I)
     C*** BEGIN CHG ***  06/27/14  ************************************
     C*** BEGIN ADD ***  05/12/05  ************************************
     C*****    SST(I)        IFEQ      'CA'
     C*** BEGIN ADD ***  02/15/11  ************************************
     C*****    SST(I)        OREQ      'MA'
     C*** END   ADD ***  02/15/11  ************************************
     C     SST(I)        IFEQ      'MA'
     C*** begin add ***  03/23/18  ************************************
     C     FNPEOS        ANDNE     'Y'
     C*** end add *****  03/23/18  ************************************
     C*** END   CHG ***  06/27/14  ************************************
     C                   MOVE      '4'           SSS(I)
     C                   ENDIF
     C*** END ADD *****  05/12/05  ************************************
     C                   ENDIF
     C                   ENDDO
     C*** END ADD *****  07/23/01  ************************************
      *
      *  WRITE A SUBMISSION HISTORY RECORD
     C                   Z-ADD     ENCO#         SHCO#
     C                   Z-ADD     ENFND         SHFND
     C                   Z-ADD     ENEMP#        SHEMP#
     C                   Z-ADD     ENDIV         SHDIV
     C                   Z-ADD     ENFYR         SHFYR
     C                   Z-ADD     XXUNID        SHUNID
     C                   Z-ADD     ENDT          SHTRDT
     C                   Z-ADD     *ZEROS        SHSBDT
     C                   MOVE      'E'           SHTYPE
     C                   MOVE      'O'           SHCODE
     C*** BEGIN CHG ***  05/12/05  ************************************
     C*********            MOVE '3'       SHSTAT
     C                   MOVE      '4'           SHSTAT
     C     1             DO        15            I
     C*
     C     SST(I)        IFNE      *BLANKS
     C     SSS(I)        ANDEQ     '3'
     C                   MOVE      '3'           SHSTAT
     C                   ENDIF
     C*
     C                   ENDDO
     C*** END ADD *****  05/12/05  ************************************
     C                   MOVE      *ZEROS        SHSEQ#
     C                   Z-ADD     ENTIME        SHTIME
     C*** begin chg ***  11/19/21  ************************************
     C*****              Z-ADD     ENREQ#        SHREQ#
     C                   Z-ADD     xqend#        SHREQ#
     C*** end chg *****  11/19/21  ************************************
     C                   MOVEL     Q1USER        SHUSER
     C                   MOVEL     Q1PGM         SHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C                   Z-ADD     SHDT          SHIDT
     C                   MOVEL     Q1USER        SHIUSR
     C                   MOVE      ENPFD1        SHPFDT
     C                   WRITE     PSH01
     C*** BEGIN ADD ***  04/14/16  ************************************
     C                   If        xxExitSR = 'Y'
     C                   LeaveSR
     C                   Endif
     C*** END ADD *****  04/14/16  ************************************
      *
     C                   CLEAR                   PNE01
      *
      *  RETREIVE UNIQUE ENTITY CODE
     C                   Z-ADD     ENCO#         XXCO#
     C                   Z-ADD     ENFND         XXFND
     C                   Z-ADD     ENEMP#        XXEMP#
      **** BEGIN ADD **** 09/14/00 ************************************
     C     ENADDF        IFEQ      'Y'
     C                   Z-ADD     ENAGT2        XXDIV
     C                   ELSE
      **** END ADD ****** 09/14/00 ************************************
     C                   Z-ADD     EMDIV         XXDIV
      **** BEGIN ADD **** 09/14/00 ************************************
     C                   END
      **** END ADD ****** 09/14/00 ************************************
     C                   MOVE      ENELID        XXELID
     C                   Z-ADD     ENFYR         XXFYR
     C***  Begin Del  ***  02/26/04  **************************************
     C****BEGIN*ADD ***  09/10/02  ************************************
     C**********           MOVE *BLANK    XXNAME  1
     C****END*ADD *****  09/10/02  ************************************
     C***  End   Del  ***  02/26/04  **************************************
     C     NAKY          CHAIN     PNA01                              60
      *
     C***  Begin Chg  ***  02/26/04  **************************************
     C********** *IN60     IFEQ *OFF
     C**********           Z-ADDNAENCD    NEENCD
     C**********           ELSE
     C****BEGIN*ADD ***  09/10/02  ************************************
     C********** NAKY5     SETGTPNA03
     C********** NAKY5     REDPEPNA03                    60
     C********** *IN60     IFEQ *OFF
     C**********
     C**********           Z-ADDNAUNID    XAUNID
     C********** NAKY4     CHAINPNA03                60
     C**********
     C********** *IN60     DOWEQ*OFF
     C**********
     C********** NANAME    IFEQ XXEMNM
     C**********           Z-ADDNAENCD    NEENCD
     C**********           MOVE 'Y'       XXNAME
     C**********           MOVE *ON       *IN60
     C**********           ENDIF
     C**********
     C**N60***** NAKY4     READEPNA03                    60
     C**********           ENDDO
     C**********
     C**********           ENDIF
     C********** XXNAME    IFNE 'Y'
     C****END*ADD *****  09/10/02  ************************************
     C                   SELECT
     C     *IN60         WHENEQ    *OFF
     C                   Z-ADD     NAENCD        NEENCD
     C     *IN60         WHENEQ    *ON
     C     ENELTP        ANDEQ     'L'
     C     NAKY3         CHAIN     PNA01                              60
     C                   Z-ADD     NAENCD        NEENCD
     C     *IN60         WHENEQ    *ON
     C***  End   Chg  ***  02/26/04  **************************************
      *
      *  IF THIS IS A NEW DIVISION THEN WE NEED TO ASSIGN
      *  THE NEXT AVAILABLE ENTITY CODE
     C                   MOVE      *HIVAL        XXENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   MOVE      *HIVAL        XXNLKC
     C***  End   Add  ***  09/10/04  **************************************
     C                   Z-ADD     ENCO#         XXCO#
     C                   Z-ADD     ENFND         XXFND
     C                   Z-ADD     ENEMP#        XXEMP#
     C     NAKY1         SETGT     PNA02
     C     NAKY2         READPE    PNA02                                  60
      **** BEGIN ADD **** 08/25/00 ************************************
     C     *IN60         DOWEQ     '0'
     C     NAENCD        ANDEQ     999
     C     NAKY2         READPE    PNA02                                  60
     C                   END
      **** END ADD ****** 08/25/00 ************************************
      *
     C     *IN60         IFEQ      *OFF
     C     NAENCD        ADD       1             NEENCD
     C                   ELSE
     C                   Z-ADD     *ZEROS        NEENCD
     C                   END                                                    *IN60=*OFF
     C***  Begin Add  ***  09/10/04  **************************************
     C     NEENCD        IFEQ      999
     C                   EXSR      NAMLNK
     C                   ENDIF
     C***  End   Add  ***  09/10/04  **************************************
     C***  Begin Del  ***  02/26/04  **************************************
     C****BEGIN*ADD ***  09/10/02  ************************************
     C**********           ENDIF                           *IN60=*OFF
     C****END*ADD *****  09/10/02  ************************************
     C***  End   Del  ***  02/26/04  **************************************
     C                   END
     C*
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NANLKC        NENLKC
     C***  End   Add  ***  09/10/04  **************************************
      *
     C     EMPST         IFNE      *BLANKS
     C                   MOVE      EMPST         XXST
     C                   ELSE
     C                   MOVE      EMMST         XXST
     C                   ENDIF                                                  EMPST IFNE
      *
     C                   Z-ADD     1             I                 3 0
     C     XXST          LOOKUP    ST(I)                                  60
     C*
     C***  Begin Add  ***  08/17/05  **************************************
     C                   EXSR      GETEXP
     C***  End   Add  ***  08/17/05  **************************************
     C*
     C                   MOVE      ENCO#         XXCO#
     C                   MOVE      ENFND         XXFND
     C                   MOVE      ENEMP#        XXEMP#
     C                   MOVE      ENDIV         XXDIV
     C                   MOVE      ENFYR         XXFYR
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     1             I                 3 0
     C     RSJRST        LOOKUP    ST(I)                                  60
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   Z-ADD     I             XXMST             2 0
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   MOVE      'N'           ALLNAM            1
     C                   MOVE      'N'           ALLLOC            1
     C***  End   Add  ***  02/26/04  **************************************
      *
     C                   SELECT
      *
     C     ENERFL        WHENEQ    'Y'
     C                   MOVE      '10'          NETYPE
     C                   MOVE      'WC890406'    NEEND#
     C                   MOVE      MSG(1)        NEDESC
     C                   MOVE      XXMST         NEST
      *
     C     ENEM32        IFNE      ENEM31
     C     ENED32        ORNE      ENED31
      *
     C                   Z-ADD     ENEM32        NEMOD
     C                   Z-ADD     ENED32        NEMEDT
      *
     C                   ELSE
      *
     C     ENEM22        IFNE      ENEM21
     C     ENED22        ORNE      ENED21
      *
     C                   Z-ADD     ENEM22        NEMOD
     C                   Z-ADD     ENED22        NEMEDT
      *
     C                   ELSE
     C                   Z-ADD     ENEM12        NEMOD
     C                   Z-ADD     ENED12        NEMEDT
     C                   ENDIF                                                  ENEM22 IFNE ENEM21
      *
     C                   ENDIF                                                  ENEM32 IFNE ENEM31
      *
      **** BEGIN ADD **** 03/20/01 ************************************
      *
      * If the experience modifier was removed, load policy effective
      * date as the modifier effective date
      *
     C     NEMEDT        IFEQ      *ZEROS
     C                   Z-ADD     ENPFD1        NEMEDT
     C                   END
      **** END ADD ****** 03/20/01 ************************************
     C*** BEGIN ADD ***  04/30/01  ************************************
     C                   MOVEL     EMNAME        NERNAM
     C*** END ADD *****  04/30/01  ************************************
     C*** BEGIN ADD ***  05/14/04  ************************************
     C                   MOVEL     EMNAM2        NERNM2
     C*** END ADD *****  05/14/04  ************************************
      ***** BEG ADD ******* 05/19/03 **********************************
      *
     C     NEMOD         IFEQ      0
     C                   Z-ADD     1             NEMOD
     C                   ENDIF
      *
      ***** END ADD ******* 05/19/03 **********************************
     C                   EXSR      SRWRT1
      *
     C     ENCMFL        WHENEQ    'Y'
     C                   MOVE      XXMST         NEST
     C                   MOVE      '42'          NETYPE
     C                   MOVE      'WC000412'    NEEND#
     C                   MOVE      MSG(2)        NEDESC
     C                   Z-ADD     ENEM12        NEMOD
     C                   Z-ADD     ENED12        NEMEDT
      *
     C                   EXSR      SRWRT1
      *
      *
     C     ENNCFL        WHENEQ    'Y'                                          Name Chg
     C     ENFIFL        OREQ      'Y'                                          Fein Chg
     C     ENTBFL        OREQ      'Y'                                          Type of Busi Chg
      *
     C                   MOVE      '88'          NETYPE
     C                   MOVE      'WC890601'    NEEND#
     C                   MOVE      MSG(3)        NEDESC
     C                   MOVEL     ENFEN2        NEFEID
     C                   MOVE      ENTOB2        NELGST
     C                   MOVEL     ENNAM2        NERNAM
     C*** BEGIN ADD ***  05/14/04  ************************************
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           MOVELENANM1    NERNM2    P
     C                   MOVE(P)   ENANM1        XXNM10           10
     C                   MOVEL     XXNM10        NERNM2
     C***  End   Chg  ***  09/22/04  **************************************
     C                   MOVE      ENANM2        NERNM2
     C*** END ADD *****  05/14/04  ************************************
     C*** BEGIN ADD ***  02/25/04  ************************************
     C                   MOVE      XXMST         NEST
     C*** END ADD *****  02/25/04  ************************************
     C***  Begin Add  ***  02/26/04  **************************************
     C                   MOVE      'Y'           ALLNAM
     C                   MOVE      'N'           ALLLOC
     C***  End   Add  ***  02/26/04  **************************************
      *
     C                   EXSR      SRWRT1
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      SRWRT2
     C***  End   Add  ***  02/26/04  **************************************
     C                   EXSR      SRALL
      *
     C     ENADFL        WHENEQ    'Y'                                          Address Chg
     C     ENMAFL        ANDEQ     'Y'                                          Mail Address
     C     ENADFL        OREQ      'Y'                                          Address Chg
     C     ENCBAF        ANDEQ     'Y'                                          Both Addresses
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   MOVE      'N'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C***  End   Add  ***  02/26/04  **************************************
     C*
     C***  Begin Add  ***  05/10/04  **************************************
     C                   MOVE      'N'           MAONLY            1
     C                   MOVE      'N'           PAONLY            1
     C*
     C     ENCBAF        IFNE      'Y'
     C     EMSTRP        ANDEQ     'Y'
     C                   MOVE      'Y'           MAONLY
     C                   ENDIF
     C***  End   Add  ***  05/10/04  **************************************
      **** BEGIN CHG **** 03/20/01 ************************************
     C*
     C*  EXECUTE PROCESSING FOR MAILING ADDRESS CHANGE
     C*
     C                   EXSR      SRMACG
     C***  Begin Del  ***  04/09/04  **************************************
     C**********
     C***IF*BOTH ADDRESSES WERE CHANGED ALSO EXECUTE PROCESSING
     C***FOR*PHYSICAL ADDRESS CHANGE
     C**********
     C********** ENCBAF    IFEQ 'Y'
     C**********           EXSR SRPACG
     C**********           END                             ENCBAF="Y"
     C***  End   Del  ***  04/09/04  **************************************
      **** END CHG ****** 03/20/01 ************************************
      *
     C     ENCELF        WHENEQ    'Y'                                          Chg of Ent/Loc
      **** BEGIN CHG **** 09/14/00 ************************************
     C***********ENAELF****OREQ 'Y'
     C     ENAELF        OREQ      'Y'                                          Add of Ent/Loc
     C     ENELTP        ANDEQ     'L'                                          Location Flag
      **** END CHG ****** 09/14/00 ************************************
     C     ENDELF        OREQ      'Y'                                          Dlt of Ent/Loc
      **** BEGIN DEL **** 09/14/00 ************************************
     C***********ENADDF****OREQ 'Y'
      **** END DEL ****** 09/14/00 ************************************
     C     ENDLDF        OREQ      'Y'                                          Dlt of Div
      ***** BEG ADD ******* 08/18/05 **********************************
     C     ENRDDF        OREQ      'Y'
      ***** END ADD ******* 08/18/05 **********************************
     C                   MOVE      '89'          NETYPE
     C                   MOVE      'WC890608'    NEEND#
      **** BEGIN DEL **** 06/27/01 ************************************
     C***********ENADDF****IFEQ 'Y'
     C*********************MOVE MSG,16    NEDESC
     C*********************ENDIF                           ENADDF = Y
      **** END DEL ****** 06/27/01 ************************************
     C*
     C***  Begin Chg  ***  02/26/04  **************************************
     C********** ENDLDF    IFEQ 'Y'
     C**********           MOVE MSG,17    NEDESC
     C**********           ENDIF                           ENDLDF = Y
     C********** ENDELF    IFEQ 'Y'
     C********** ENELTP    IFEQ 'E'
     C**********           MOVE MSG,5     NEDESC
     C**********           ELSE
     C**********           MOVE MSG,6     NEDESC
     C**********           END
     C**********           ELSE
     C********** ENCELF    IFEQ 'Y'
     C********** ENELTP    IFEQ 'E'
     C**********           MOVE MSG,18    NEDESC
     C**********           ELSE
     C**********           MOVE MSG,19    NEDESC
     C**********           END
     C**********           END
      *****BEGIN ADD **** 07/14/00 ************************************
     C********** ENAELF    IFEQ 'Y'
     C********** ENELTP    IFEQ 'E'
     C**********           MOVE MSG,7     NEDESC
     C**********           ELSE
     C**********           MOVE MSG,8     NEDESC
     C**********           END
     C**********           END
      *****END*ADD ****** 07/14/00 ************************************
     C**********           END
     C                   SELECT
     C     ENDLDF        WHENEQ    'Y'
      ***** BEG ADD ******* 08/18/05 **********************************
     C     ENRDDF        OREQ      'Y'
      ***** END ADD ******* 08/18/05 **********************************
     C                   EXSR      DLTDIV
     C                   MOVE      MSG(17)       NEDESC                         Dlt Division
     C     ENDELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'E'
     C                   EXSR      DLTENT
     C                   MOVE      MSG(5)        NEDESC                         Dlt Entity
     C     ENDELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'L'
     C                   EXSR      DLTLOC
     C                   MOVE      MSG(6)        NEDESC                         Dlt Location
     C     ENCELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'E'
     C                   EXSR      CHGENT
     C                   MOVE      MSG(18)       NEDESC                         Chg Entity
     C     ENCELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'L'
     C                   EXSR      CHGLOC
     C                   MOVE      MSG(19)       NEDESC                         Chg Location
     C     ENAELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'E'
     C                   EXSR      ADDENT
     C                   MOVE      MSG(7)        NEDESC                         Add Entity
     C     ENAELF        WHENEQ    'Y'
     C     ENELTP        ANDEQ     'L'
     C                   EXSR      ADDLOC
     C                   MOVE      MSG(8)        NEDESC                         Add Location
     C                   ENDSL
     C*
     C                   EXSR      SRWRT1
     C***  End   Chg  ***  02/26/04  **************************************
     C*
     C                   MOVE      '2'           NEADTP
     C                   MOVE      XXMST         NEST
     C     ALKY          CHAIN     PAL01                              60
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C     ALLLOC        IFEQ      'Y'
     C***  End   Add  ***  02/26/04  **************************************
     C*
     C***  Begin Chg  ***  02/26/04  **************************************
     C********** *IN60     IFEQ *OFF
      **** BEGIN CHG **** 10/28/99 ************************************
     C********** ALMAD1    IFNE *BLANKS
     C********** ALELTP    ANDEQ'E'
     C********** ALPAD1    OREQ *BLANKS
      **** BEGIN CHG **** 01/23/02 ************************************
     C********** ALPAD1    IFEQ *BLANKS
     C********** ALPAD2    ANDEQ*BLANKS
      **** END   CHG **** 10/28/99 ************************************
     C**********           MOVELALNAME    NERNAM
     C*****END** CHG **** 01/23/02 ************************************
     C**********           MOVE ALMAD1    NEMA12
     C**********           MOVE ALMAD2    NEMA22
     C**********           MOVE ALMCTY    NEMCT2
     C**********           MOVE ALMST     NEMST2
     C**********           MOVE ALMZIP    NEMZP2
     C*****BEGIN CHG **** 01/23/02 ************************************
     C******************** ELSE
     C**********           EXSR SRWRT1
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      'M'           XXMFLG
     C     ALMAD1        IFEQ      *BLANK
     C                   MOVE      'P'           XXMFLG
     C                   ENDIF
     C*
     C                   EXSR      SRWRT3
     C***  End   Chg  ***  02/26/04  **************************************
      *
      *  IF PHYSICAL ADDRESS IS DIFFERENT, WRITE A RECORD
      *
     C***  Begin Chg  ***  02/26/04  **************************************
     C********** ALPAD1    IFNE EMPAD1
     C********** ALPAD1    ANDNE*BLANKS
     C********** ALPAD2    ORNE EMPAD2
     C********** ALPAD2    ANDNE*BLANKS
     C********** ALPCTY    ORNE EMPCTY
     C********** ALPCTY    ANDNE*BLANKS
     C********** ALPST     ORNE EMPST
     C********** ALPST     ANDNE*BLANKS
     C********** ALPZIP    ORNE EMPZIP
     C********** ALPZIP    ANDNE*ZERO
     C     XXMFLG        IFNE      'P'
     C***  End   Chg  ***  02/26/04  **************************************
      *
     C     ALPAD1        IFNE      ALMAD1
     C     ALPAD2        ORNE      ALMAD2
     C     ALPCTY        ORNE      ALMCTY
     C     ALPST         ORNE      ALMST
     C     ALPZIP        ORNE      ALMZIP
      *
     C***  Begin Chg  ***  02/26/04  **************************************
     C**********
     C**********           MOVELALNAME    NERNAM
     C*****END** CHG **** 01/23/02 ************************************
     C**********
     C**********           MOVE ALPAD1    NEMA12
     C**********           MOVE ALPAD2    NEMA22
     C**********           MOVE ALPCTY    NEMCT2
     C**********           MOVE ALPST     NEMST2
     C**********           MOVE ALPZIP    NEMZP2
     C**********
     C*****BEGIN DEL **** 01/23/02 ************************************
     C******************** ENDIF                           ALMAD1 IFNE *BLANKS
     C******************** END
     C*****END** DEL **** 01/23/02 ************************************
     C**********           EXSR SRWRT1
     C*****BEGIN ADD **** 01/23/02 ************************************
     C                   MOVE      'P'           XXMFLG
     C                   EXSR      SRWRT3
     C***  End   Chg  ***  02/26/04  **************************************
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      **** END   ADD **** 01/23/02 ************************************
     C                   EXSR      SRALL                                        All locations
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   ENDIF                                                  ALLLOC
     C*
     C     ALLNAM        IFEQ      'Y'
     C*
     C     ALKY          CHAIN     PAL01                              60
     C*
     C                   MOVE      '88'          NETYPE
     C                   MOVE      'WC890601'    NEEND#
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      'M'           XXMFLG
     C     ALMAD1        IFEQ      *BLANKS
     C                   MOVE      'P'           XXMFLG
     C                   ENDIF
     C*
     C                   EXSR      SRWRT3
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   EXSR      SRALL                                        ALL names
     C*
     C                   ENDIF                                                  ALLNAM='Y'
     C***  End   Add  ***  02/26/04  **************************************
      *
     C     ENNOFL        WHENEQ    'Y'
     C                   MOVE      '87'          NETYPE
     C                   MOVE      'WC890610'    NEEND#
     C                   MOVE      MSG(9)        NEDESC
     C                   MOVE      ENTOB2        NELGST
     C                   EXSR      SRWRT1
      *
     C     ENCIFL        WHENEQ    'Y'
     C                   MOVE      '87'          NETYPE
     C     ENPOL1        IFNE      ENPOL2
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        NESEQ#
     C                   MOVE      'WC890602'    NEEND#
     C                   MOVE      MSG(10)       NEDESC
     C                   MOVE      XXFND2        NEPLC1
     C                   MOVE      XXEMP2        NEPLC2
     C                   MOVE      XXFYR2        NEPLC3
     C                   EXSR      SRWRT1
     C                   Z-ADD     *ZEROS        NEPLC1
     C                   Z-ADD     *ZEROS        NEPLC2
     C                   Z-ADD     *ZEROS        NEPLC3
     C                   END
     C     ENPFD1        IFNE      ENPFD2
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        NESEQ#
     C                   MOVE      'WC890603'    NEEND#
     C                   MOVE      MSG(11)       NEDESC
     C                   Z-ADD     ENPFD2        NEPFD2
     C                   Z-ADD     ENPTD2        NEPTD2
     C                   EXSR      SRWRT1
     C                   Z-ADD     *ZEROS        NEPFD2
     C                   Z-ADD     *ZEROS        NEPTD2
     C                   END
     C     ENPTD1        IFNE      ENPTD2
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        NESEQ#
     C                   MOVE      'WC890604'    NEEND#
     C                   MOVE      MSG(12)       NEDESC
     C                   Z-ADD     ENPFD2        NEPFD2
     C                   Z-ADD     ENPTD2        NEPTD2
     C                   EXSR      SRWRT1
     C                   Z-ADD     *ZEROS        NEPFD2
     C                   Z-ADD     *ZEROS        NEPTD2
     C                   END
     C     ENAGT#        IFNE      ENAGT2
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        NESEQ#
     C                   MOVE      'WC890607'    NEEND#
     C                   MOVE      MSG(13)       NEDESC
     C                   Z-ADD     ENAGT2        NEAGT#
     C                   MOVE      ENAGN2        NEAGNM
     C                   EXSR      SRWRT1
     C                   Z-ADD     *ZEROS        NEAGT#
     C                   MOVE      *BLANKS       NEAGNM
     C                   END
     C     ENTOB1        IFNE      ENTOB2
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        NESEQ#
     C                   MOVE      'WC890610'    NEEND#
     C                   MOVE      MSG(14)       NEDESC
     C                   MOVE      ENTOB2        NELGST
     C                   EXSR      SRWRT1
     C                   MOVE      *BLANK        NELGST
     C                   END
     C     ENCAR1        IFNE      ENCAR2
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        NESEQ#
     C                   MOVE      'WC890619'    NEEND#
     C                   MOVE      MSG(15)       NEDESC
     C                   Z-ADD     ENCAR2        NECAR2
     C                   EXSR      SRWRT1
     C                   Z-ADD     *ZEROS        NECAR2
     C                   END
      **** BEGIN ADD **** 09/14/00 ************************************
     C     ENAELF        WHENEQ    'Y'                                          Add of Ent/Loc
     C     ENELTP        ANDEQ     'E'                                          Entity Flag
     C*
      **** BEGIN DEL **** 06/27/01 ************************************
     C***********ENADDF    IFEQ 'Y'
     C*********************MOVE MSG,16    NEDESC
     C*********************ENDIF                           ENADDF = Y
     C***********ENAELF    IFEQ 'Y'
      **** END DEL ****** 06/27/01 ************************************
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      ADDENT
     C***  End   Add  ***  02/26/04  **************************************
     C                   MOVE      MSG(7)        NEDESC
      **** BEGIN DEL **** 06/27/01 ************************************
     C*********************END
      **** END DEL ****** 06/27/01 ************************************
     C*
     C                   MOVE      '88'          NETYPE
     C                   MOVE      'WC890601'    NEEND#
     C*
     C     ALKY          CHAIN     PAL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C***  Begin Chg  ***  02/26/04  **************************************
     C**********           MOVE ALFEIN    NEFEID
     C**********           MOVE ALTPOB    NELGST
     C**********           MOVELALNAME    NERNAM
     c*****BEGIN ADD **** 01/23/02 ************************************
     C**********           MOVE ALMAD1    NEMA12
     C**********           MOVE ALMAD2    NEMA22
     C**********           MOVE ALMCTY    NEMCT2
     C**********           MOVE ALMST     NEMST2
     C**********           MOVE ALMZIP    NEMZP2
     C****BEGIN*ADD ***  02/25/04  ************************************
     C**********           MOVE XXMST     NEST
     C****END*ADD *****  02/25/04  ************************************
     c**********
     C**********           EXSR SRWRT1
     c**********
     c***IF*PHYSICAL ADDRESS IS DIFFERENT, WRITE A RECORD
     c**********
     C********** ALPAD1    IFNE EMPAD1
     C********** ALPAD1    ANDNE*BLANKS
     C********** ALPAD2    ORNE EMPAD2
     C********** ALPAD2    ANDNE*BLANKS
     C********** ALPCTY    ORNE EMPCTY
     C********** ALPCTY    ANDNE*BLANKS
     C********** ALPST     ORNE EMPST
     C********** ALPST     ANDNE*BLANKS
     C********** ALPZIP    ORNE EMPZIP
     C********** ALPZIP    ANDNE*ZERO
     c**********
     C********** ALPAD1    IFNE ALMAD1
     C********** ALPAD2    ORNE ALMAD2
     C********** ALPCTY    ORNE ALMCTY
     C********** ALPST     ORNE ALMST
     C********** ALPZIP    ORNE ALMZIP
     c**********
     C**********           MOVE ALPAD1    NEMA12
     C**********           MOVE ALPAD2    NEMA22
     C**********           MOVE ALPCTY    NEMCT2
     C**********           MOVE ALPST     NEMST2
     C**********           MOVE ALPZIP    NEMZP2
     c**********
     C**********           EXSR SRWRT1
     c**********
     C**********           ENDIF
     C**********           ENDIF
     c*****END** ADD **** 01/23/02 ************************************
     C                   MOVE      'M'           XXMFLG
     C     ALMAD1        IFEQ      *BLANKS
     C                   MOVE      'P'           XXMFLG
     C                   ENDIF
     C*
     C                   EXSR      SRWRT1
     C                   EXSR      SRWRT3
     C***  End   Chg  ***  02/26/04  **************************************
     C*
     C                   END
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      SRALL
     C*
     C     EMKY          CHAIN     PEM01                              60
     C***  End   Add  ***  02/26/04  **************************************
      *
      **** BEGIN DEL **** 01/23/02 ************************************
     C******************** EXSR SRWRT1
     C******************** EXSR SRALL
      **** END   DEL **** 01/23/02 ************************************
     C                   MOVE      '89'          NETYPE
     C                   MOVE      'WC890608'    NEEND#
     C                   MOVE      '2'           NEADTP
     C                   MOVE      XXMST         NEST
      **** BEGIN DEL **** 01/23/02 ************************************
     C********** ALKY      CHAINPAL01                60
     C********** *IN60     IFEQ *OFF
     C********** ALPAD1    IFEQ *BLANKS
     C********** ALPAD2    ANDEQ*BLANKS
     C**********           MOVELALNAME    NERNAM
     C**********           MOVE ALMAD1    NEMA12
     C**********           MOVE ALMAD2    NEMA22
     C**********           MOVE ALMCTY    NEMCT2
     C**********           MOVE ALMST     NEMST2
     C**********           MOVE ALMZIP    NEMZP2
     C**********           ELSE
     C**********           MOVELALNAME    NERNAM
     C**********           MOVE ALPAD1    NEMA12
     C**********           MOVE ALPAD2    NEMA22
     C**********           MOVE ALPCTY    NEMCT2
     C**********           MOVE ALPST     NEMST2
     C**********           MOVE ALPZIP    NEMZP2
     C**********           ENDIF                           ALMAD1 IFNE *BLANKS
     C**********           END
      **** END   DEL **** 01/23/02 ************************************
     C***  Begin Del  ***  02/26/04  **************************************
     C**********           Z-ADDENCO#     XXCO#
     C**********           Z-ADDENFND     XXFND
     C**********           Z-ADDENEMP#    XXEMP#
     C**********           Z-ADDENDIV     XXDIV
     C**********           MOVE ALELID    XXELID
     C**********           Z-ADDNEFYR     XXFYR
     C********** NAKY      CHAINPNA01                60
     C********** *IN60     IFEQ *OFF
     C**********           Z-ADDNAENCD    NEENCD
     C**********           END
     C***  End   Del  ***  02/26/04  **************************************
      **** BEGIN DEL **** 01/23/02 ************************************
     C******************** EXSR SRWRT1
      **** END   DEL **** 01/23/02 ************************************
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C     ALKY          CHAIN     PAL01                              60
     C*
     C     ALMAD1        IFNE      *BLANKS
     C                   MOVE      'M'           XXMFLG
     C                   EXSR      SRWRT1
     C                   EXSR      SRWRT3
     C                   ENDIF
     C*
     C     ALPAD1        IFNE      ALMAD1
     C     ALPAD2        ORNE      ALMAD2
     C     ALPCTY        ORNE      ALMCTY
     C     ALPST         ORNE      ALMST
     C     ALPZIP        ORNE      ALMZIP
     C                   MOVE      'P'           XXMFLG
     C                   EXSR      SRWRT1
     C                   EXSR      SRWRT3
     C                   ENDIF
     C***  End   Add  ***  02/26/04  **************************************
     C*
     C                   EXSR      SRALL
     C*
     C     ENADDF        WHENEQ    'Y'                                          Add of Div
     C*
     C***  Begin Del  ***  02/26/04  **************************************
     C****BEGIN*ADD ***  09/10/02  ************************************
     C********** XXNAME    IFNE 'Y'
     C****END*ADD *****  09/10/02  ************************************
     C***  End   Del  ***  02/26/04  **************************************
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      ADDDIV
     C***  End   Add  ***  02/26/04  **************************************
      **** BEGIN ADD **** 06/27/01 ************************************
     C                   MOVE      MSG(16)       NEDESC
      **** END ADD ****** 06/27/01 ************************************
     C                   Z-ADD     ENAGT2        ENDIV
     C*
     C                   MOVE      '88'          NETYPE
     C                   MOVE      'WC890601'    NEEND#
     C                   MOVE      ENFEN2        NEFEID
     C                   MOVE      ENTOB2        NELGST
     C                   MOVEL     ENNAM2        NERNAM
     C*** BEGIN ADD ***  05/14/04  ************************************
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           MOVELENANM1    NERNM2    P
     C                   MOVE(P)   ENANM1        XXNM10           10
     C                   MOVEL     XXNM10        NERNM2
     C***  End   Chg  ***  09/22/04  **************************************
     C                   MOVE      ENANM2        NERNM2
     C*** END ADD *****  05/14/04  ************************************
      **** BEGIN ADD **** 06/27/01 ************************************
     C                   MOVE      '2'           NEADTP
     C                   MOVE      XXMST         NEST
     C     ENMA12        IFEQ      *BLANKS
     C     ENMA22        ANDEQ     *BLANKS
     C                   MOVEL     ENNAM2        NERNAM
     C                   MOVE      ENPA12        NEMA12
     C                   MOVE      ENPA22        NEMA22
     C                   MOVE      ENPCT2        NEMCT2
     C                   MOVE      ENPST2        NEMST2
     C                   MOVE      ENPZP2        NEMZP2
     C                   ELSE
     C                   MOVEL     ENNAM2        NERNAM
     C                   MOVE      ENMA12        NEMA12
     C                   MOVE      ENMA22        NEMA22
     C                   MOVE      ENMCT2        NEMCT2
     C                   MOVE      ENMST2        NEMST2
     C                   MOVE      ENMZP2        NEMZP2
     C                   ENDIF                                                  ALMAD1 IFNE *BLANKS
      **** END ADD ****** 06/27/01 ************************************
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C     EMKY          CHAIN     PEM01                              60
     C***  End   Add  ***  02/26/04  **************************************
      *
     C                   EXSR      SRWRT1
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      SRWRT2
     C***  End   Add  ***  02/26/04  **************************************
     C                   EXSR      SRALL
     C***  Begin Del  ***  02/26/04  **************************************
     C****BEGIN*ADD ***  09/10/02  ************************************
     C**********           ENDIF                           XXNAME IFNE
     C****END*ADD *****  09/10/02  ************************************
     C***  End   Del  ***  02/26/04  **************************************
      *
     C                   MOVE      '89'          NETYPE
     C                   MOVE      'WC890608'    NEEND#
     C                   MOVE      '2'           NEADTP
     C                   MOVE      XXMST         NEST
      **** BEGIN ADD **** 06/27/01 ************************************
     C***  Begin Del  ***  02/26/04  **************************************
     C********** ENMA12    IFEQ *BLANKS
     C********** ENMA22    ANDEQ*BLANKS
     C**********           MOVELENNAM2    NERNAM
     C**********           MOVE ENPA12    NEMA12
     C**********           MOVE ENPA22    NEMA22
     C**********           MOVE ENPCT2    NEMCT2
     C**********           MOVE ENPST2    NEMST2
     C**********           MOVE ENPZP2    NEMZP2
     C**********           ELSE
     C***  End   Del  ***  02/26/04  **************************************
     C                   MOVEL     ENNAM2        NERNAM
     C*** BEGIN ADD ***  05/14/04  ************************************
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           MOVELENANM1    NERNM2    P
     C                   MOVE(P)   ENANM1        XXNM10           10
     C                   MOVEL     XXNM10        NERNM2
     C***  End   Chg  ***  09/22/04  **************************************
     C                   MOVE      ENANM2        NERNM2
     C*** END ADD *****  05/14/04  ************************************
     C                   MOVE      ENMA12        NEMA12
     C                   MOVE      ENMA22        NEMA22
     C                   MOVE      ENMCT2        NEMCT2
     C                   MOVE      ENMST2        NEMST2
     C                   MOVE      ENMZP2        NEMZP2
     C***  Begin Del  ***  02/26/04  **************************************
     C**********           ENDIF                           ALMAD1 IFNE *BLANKS
     C***  End   Del  ***  02/26/04  **************************************
      **** END ADD ****** 06/27/01 ************************************
     C                   Z-ADD     ENCO#         XXCO#
     C                   Z-ADD     ENFND         XXFND
     C                   Z-ADD     ENEMP#        XXEMP#
      **** BEGIN CHG **** 06/27/01 ************************************
     C*********************Z-ADDENDIV     XXDIV
     C***********ENADDF****IFEQ 'Y'
     C                   Z-ADD     ENAGT2        XXDIV
     C*********************ELSE
     C*********************Z-ADDENDIV     XXDIV
     C*********************END
      **** END CHG ****** 06/27/01 ************************************
     C                   MOVE      *BLANKS       XXELID
     C                   Z-ADD     ENFYR         XXFYR
     C     NAKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     NAENCD        NEENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NANLKC        NENLKC
     C***  End   Add  ***  09/10/04  **************************************
     C                   END
     C                   EXSR      SRWRT1
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      SRWRT2
     C***  End   Add  ***  02/26/04  **************************************
     C                   EXSR      SRALL
      **** END ADD ****** 09/14/00 ************************************
      **** BEGIN ADD **** 03/20/01 ************************************
     C     ENADFL        WHENEQ    'Y'                                          Address Chg
     C     ENPAFL        ANDEQ     'Y'                                          Physical Address
     C     ENADFL        OREQ      'Y'                                          Address Chg
     C     ENCBAF        ANDEQ     'Y'                                          Both Addresses
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   MOVE      'N'           ALLNAM
     C                   MOVE      'Y'           ALLLOC
     C***  End   Add  ***  02/26/04  **************************************
     C***  Begin Add  ***  05/10/04  **************************************
     C                   MOVE      'N'           MAONLY            1
     C                   MOVE      'N'           PAONLY            1
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      'Y'           PAONLY
     C                   ENDIF
     C***  End   Add  ***  05/10/04  **************************************
     C*
     C*  EXECUTE PROCESSING FOR PHYSICAL ADDRESS CHANGE
     C*
     C                   EXSR      SRPACG
     C***  Begin Del  ***  04/09/04  **************************************
     C**********
     C***IF*BOTH ADDRESSES WERE CHANGED ALSO EXECUTE PROCESSING
     C***FOR*MAILING ADDRESS CHANGE
     C**********
     C********** ENCBAF    IFEQ 'Y'
     C**********           EXSR SRMACG
     C**********           END                             ENCBAF="Y"
     C***  End   Del  ***  04/09/04  **************************************
      **** END ADD ****** 03/20/01 ************************************
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  SRWRIT
     *===============================================================*
     *  SRWRT1 - Loads info directly from WMENP
     *===============================================================*
     C     SRWRT1        BEGSR
      *
      *  WRITE AN ENDORSEMENT TRANSACTION RECORD
     C                   Z-ADD     ENCO#         NECO#
     C                   Z-ADD     ENFND         NEFND
     C                   Z-ADD     ENEMP#        NEEMP#
     C                   Z-ADD     ENFYR         NEFYR
     C                   Z-ADD     XXUNID        NEUNID
     C*** BEGIN ADD ***  07/23/01  ************************************
     C     RSJRST        IFEQ      'NJ'
     C     NETYPE        ANDEQ     '88'
     C*** BEGIN CHG ***  08/05/05  ************************************
     C*********            MOVELEMSEMP    NEUE#
     C                   MOVEL     EMUE#         NEUE#
     C                   ELSE
     C     RSJRST        IFNE      'NJ'
     C     NETYPE        ANDEQ     '88'
     C                   MOVEL     EMSEMP        NEUE#
     C                   ENDIF
     C*** END CHG *****  08/05/05  ************************************
     C                   ENDIF
     C*** END ADD *****  07/23/01  ************************************
     C                   MOVEL     ENNAM1        NENAME
     C*** BEGIN ADD ***  05/14/04  ************************************
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           MOVELENENM1    NENAM2    P
     C                   MOVE(P)   ENENM1        XXNM10           10
     C                   MOVEL     XXNM10        NENAM2
     C***  End   Chg  ***  09/22/04  **************************************
     C                   MOVE      ENENM2        NENAM2
     C*** END ADD *****  05/14/04  ************************************
     C                   Z-ADD     ENPFD1        NEPFDT
     C*** BEGIN ADD ***  12/13/06  *************************************
     C     SVPFDT        IFNE      *ZERO
     C     SVPFDT        ANDLT     NEPFDT
     C                   Z-ADD     SVPFDT        NEPFDT
     C                   ENDIF                                                  SVPFDT IFNE
     C*** END ADD *****  12/13/06  *************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NETRDT
     C                   Z-ADD     ENEEDT        NEEEDT
     C                   MOVE      ENELID        NEELID
     C                   Z-ADD     ENCAR1        NECAR1
      **** BEGIN ADD **** 07/31/00 ************************************
     C     ENCAR2        IFEQ      *ZEROS
     C                   Z-ADD     ENCAR1        NECAR2
     C                   ELSE
     C                   Z-ADD     ENCAR2        NECAR2
     C                   END
      **** END ADD ****** 07/31/00 ************************************
      *
      *  WRITE NAME/ADDRESS TRANSACTION LOCATOR FILE
      **** BEGIN CHG **** 09/14/00 ************************************
     C***********ENDELF    IFNE 'Y'
      **** BEGIN ADD **** 07/14/00 ************************************
     C***********ENCELF    ANDNE'Y'
     C***********ENAELF    ANDNE'Y'
     C***********ENADDF    ANDNE'Y'
     C***********ENDLDF    ANDNE'Y'
      **** END ADD ****** 07/14/00 ************************************
     C     ENDELF        IFNE      'Y'
     C     ENDLDF        ANDNE     'Y'
      ***** BEG ADD ******* 08/18/05 **********************************
     C     ENRDDF        ANDNE     'Y'
      ***** END ADD ******* 08/18/05 **********************************
      *
      *  ON ADD OF A LOCATION, THE NEW LOCATION BEING ADDED NEEDS TO
      *  HAVE A LINK CODE (ENENCD) THAT REFERS BACK TO THE PRIMARY NM
      *
     C     ENAELF        IFEQ      'Y'
     C     ENELTP        ANDEQ     'L'
      **** BEGIN CHG **** 03/19/02 ************************************
     C******************** Z-ADD1         NEENCD
     C     NAKY3         CHAIN     PNA01                              60
     C                   Z-ADD     NAENCD        NEENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NANLKC        NENLKC
     C***  End   Add  ***  09/10/04  **************************************
      **** END   CHG **** 03/19/02 ************************************
     C                   END
      **** END CHG ****** 09/14/00 ************************************
      *
      *  IF CURRENT ENDORSEMENT IS THE DELETION OF AN ENTITY/LOCATION
      *  THERE IS NO NEED TO WRITE THESE RECORDS
     C                   MOVEL     'A'           NESTAT
     C                   MOVEL     Q1USER        NEUSER
     C                   MOVEL     'SRWRT1'      NEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NEDT
     C                   Z-ADD     DSTIME        NETIME
     C                   Z-ADD     SHDT          NEIDT
     C                   MOVEL     Q1USER        NEIUSR
     C                   Z-ADD     ENREQ#        NEREQ#
      **** BEGIN ADD **** 06/01/01 *********************************************
     C                   Z-ADD     ELEMPY        NEEMPY
     C                   MOVE      EMSIC         NESIC
      **** BEGIN ADD **** 04/20/06 *********************************************
      *
      * USE NAICS CODE IF FLAGGED AT STATE LEVEL.
      *
     C     EMMAD1        IFNE      *BLANK
     C                   MOVEL     EMMST         XXST
     C                   ELSE
     C                   MOVEL     EMPST         XXST
     C                   ENDIF                                                  EMMAD1 IFNE
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C     *IN60         IFEQ      *OFF
     C     RRNAIC        ANDEQ     'Y'
     C                   MOVEL     EMNAIC        NESIC
     C                   ENDIF
      **** END   ADD **** 04/20/06 *********************************************
      **** END   ADD **** 06/01/01 *********************************************
     C***  Begin Chg  ***  02/26/04  **************************************
     C*****BEGIN ADD **** 07/15/02 ************************************
     C**********
     C***ONLY*CHECK ADDRESS FIELD ON LOCATION CHANGE (ADD/CHG/DEL OF
     C***ENTITY/LOCATION/DIVISION
     C**********
     C********** ENCELF    IFEQ 'Y'
     C********** ENAELF    OREQ 'Y'
     C********** ENDELF    OREQ 'Y'
     C********** ENADDF    OREQ 'Y'
     C********** ENDLDF    OREQ 'Y'
     C*****END** ADD **** 07/15/02 ************************************
     C*****BEGIN ADD **** 03/19/02 ************************************
     C********** NEMA12    IFNE *BLANKS
     C********** NEMA22    ORNE *BLANKS
     C*****END** ADD **** 03/19/02 ************************************
     C**********           WRITEPNE01
     C*****BEGIN ADD **** 03/19/02 ************************************
     C**********           ENDIF
     C*****BEGIN ADD **** 07/15/02 ************************************
     C**********           ELSE
     C**********           WRITEPNE01
     C**********           ENDIF                           LOCATION CHG
     C*****END** ADD **** 03/19/02 ************************************
     C*****END** ADD **** 07/15/02 ************************************
     C*
     C*  SRWRT1 will only be used for '88' & '89' to load the header info
     C*
     C     NETYPE        IFNE      '88'
     C     NETYPE        ANDNE     '89'
     C                   WRITE     PNE01
     C                   ENDIF
     C***  End   Chg  ***  02/26/04  **************************************
      **** BEGIN CHG **** 09/14/00 ************************************
      *
      *  ON ADD OF A LOCATION, THE NEW LOCATION BEING ADDED NEEDS TO
      *  HAVE A LINK CODE (ENENCD) THAT REFERS BACK TO THE PRIMARY NM
      *  THEREFORE A NEW ENTITY CODE IS NOT ASSIGNED, NO NEED TO
      *  WRITE TO WXNAP1
      *
     C     ENAELF        IFNE      'Y'
     C     ENAELF        OREQ      'Y'
     C     ENELTP        ANDEQ     'E'
      **** END CHG ****** 09/14/00 ************************************
     C                   Z-ADD     NECO#         XXCO#
     C                   Z-ADD     NEFND         XXFND
     C                   Z-ADD     NEFYR         XXFYR
     C                   Z-ADD     NEEMP#        XXEMP#
      **** BEGIN CHG **** 06/27/01 ************************************
     C*********************Z-ADDEMDIV     XXDIV
     C     ENADDF        IFEQ      'Y'
     C                   Z-ADD     ENAGT2        XXDIV
     C                   ELSE
     C                   Z-ADD     EMDIV         XXDIV
     C                   END
      **** END CHG ****** 06/27/01 ************************************
     C                   MOVE      NEELID        XXELID
     C     NAKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     NECO#         NACO#
     C                   Z-ADD     NEFND         NAFND
     C                   Z-ADD     NEEMP#        NAEMP#
      **** BEGIN CHG **** 06/27/01 ************************************
     C*********************Z-ADDEMDIV     NADIV
     C                   Z-ADD     XXDIV         NADIV
      **** END CHG ****** 06/27/01 ************************************
     C                   MOVE      NEELID        NAELID
     C                   Z-ADD     NEFYR         NAFYR
     C                   Z-ADD     NEENCD        NAENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NENLKC        NANLKC
     C***  End   Add  ***  09/10/04  **************************************
     C                   MOVEL     'A'           NASTAT
     C                   MOVEL     Q1USER        NAUSER
     C                   MOVEL     Q1PGM         NAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NADT
     C                   Z-ADD     DSTIME        NATIME
     C                   Z-ADD     SHDT          NAIDT
     C                   MOVEL     Q1USER        NAIUSR
     C                   WRITE     PNA01
      **** BEGIN CHG **** 09/14/00 ************************************
     C                   END                                                    IGN ADD OF LOC
      **** END CHG ****** 09/14/00 ************************************
     C                   END
     C                   END                                                    ENDELF='Y'
      *
     C                   ENDSR                                                  SRWRT1
     *===============================================================*
     *  SRWRT2 - Loads info from WMEMP
     *===============================================================*
     C     SRWRT2        BEGSR
      *
     C***  BEG ADD ******  12/13/07  **************************************
     C                   CLEAR                   NERNM4
     C***  END ADD ******  12/13/07  **************************************
      *  WRITE AN ENDORSEMENT TRANSACTION RECORD
     C                   MOVE      '2'           NEADTP
     C*
     C***  Begin Add  ***  05/10/04  **************************************
     C     NEEND#        IFEQ      'WC890605'
     C                   MOVE      '1'           NEADTP
     C                   ENDIF
     C***  End   Add  ***  05/10/04  **************************************
     C*
     C                   MOVE      XXMST         NEST
     C                   MOVE      EMNAME        NERNAM
     C***  BEG ADD ******  12/13/07  **************************************
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     @RRPGF        IFEQ      '3'
     C     @RRPGF        OREQ      ' '
      *
     C     *IN60         IFEQ      *OFF
     C     FNPEOF        ANDNE     *BLANK
      *
     C                   MOVEL     @EMNM         NERNM4
     C     @EMN2         IFNE      *BLANKS
     C     NERNM4        CAT       @EMN2:1       NERNM4
     C                   ENDIF                                                  @EMN2 IFNE *BLANKS
     C     NERNM4        CAT       PEOEXT:1      NERNM4
      *
     C     XXPEO         IFEQ      'Y'
     C     NERNM4        CAT       @@EMNM:1      NERNM4
     C     @@EMN2        IFNE      *BLANKS
     C     NERNM4        CAT       @@EMN2:1      NERNM4
     C                   ENDIF                                                  @@EMN2 IFNE *BLANKS
      *
     C                   ELSE
      *
     C     NERNM4        CAT       EMNAME:1      NERNM4
     C     EMNAM2        IFNE      *BLANKS
     C     NERNM4        CAT(P)    EMNAM2:1      NERNM4
     C                   ENDIF                                                  EMNAM2 NE *BLANKS
      *
     C                   ENDIF                                                  XXPEO IFEQ 'Y'
     C                   ENDIF                                                  *IN60 IFEQ '0'
     C                   ENDIF                                                  @RRPGF IFEQ '3'
     C***  END ADD ** ***  12/13/07  **************************************
     C*** BEGIN ADD ***  05/14/04  ************************************
     C                   MOVEL(P)  EMNAM2        NERNM2
     C*** END ADD *****  05/14/04  ************************************
     C*** BEGIN ADD ***  04/25/06  ************************************
     C                   MOVE      EMNAMP        NERNMP
     C*** END ADD *****  04/25/06  ************************************
     C*** BEGIN ADD ***  07/23/07  ************************************
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     FNPEOF        IFNE      *BLANKS
     C*
     C                   SELECT
     C     NERNMP        WHENEQ    'Y'
     C     EMEMP#        ANDEQ     1
     C     EMDIV         ANDEQ     0
     C                   MOVE      'Q'           NERNMP
     C     NERNMP        WHENEQ    'Y'
     C                   MOVE      'D'           NERNMP
     C     EMEMP#        WHENEQ    1
     C     EMDIV         ANDEQ     0
     C                   MOVE      'P'           NERNMP
     C                   OTHER
     C                   MOVE      'C'           NERNMP
     C                   ENDSL
     C*
     C                   ENDIF                                                  FNPEOF IFNE
     C*** END ADD *****  07/23/07  ************************************
     C*** BEGIN ADD ***  10/16/06  ************************************
     C                   MOVE      EMFEIN        NEFEID
     C*** END ADD *****  10/16/06  ************************************
     C                   MOVE      EMMAD1        NEMA12
     C                   MOVE      EMMAD2        NEMA22
     C                   MOVE      EMMCTY        NEMCT2
     C                   MOVE      EMMST         NEMST2
     C                   MOVE      EMMZIP        NEMZP2
     C                   MOVE      *BLANKS       NEELID
      **** BEGIN ADD **** 07/31/00 ************************************
     C                   Z-ADD     ENCAR1        NECAR1
     C     ENCAR2        IFEQ      *ZEROS
     C                   Z-ADD     ENCAR1        NECAR2
     C                   ELSE
     C                   Z-ADD     ENCAR2        NECAR2
     C                   END
      **** END ADD ****** 07/31/00 ************************************
      *
      *  RETREIVE UNIQUE ENTITY CODE
     C                   Z-ADD     NECO#         XXCO#
     C                   Z-ADD     NEFND         XXFND
     C                   Z-ADD     NEEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      *BLANKS       XXELID
     C                   Z-ADD     NEFYR         XXFYR
     C***  Begin Del  ***  02/26/04  **************************************
     C****BEGIN*ADD ***  09/10/02  ************************************
     C**********           MOVE *BLANK    XXNAME
     C****END*ADD *****  09/10/02  ************************************
     C***  End   Del  ***  02/26/04  **************************************
     C*
     C     NAKY          CHAIN     PNA01                              60
     C*
     C***  Begin Chg  ***  02/26/04  **************************************
     C********** *IN60     IFEQ *OFF
     C**********           Z-ADDNAENCD    NEENCD
     C**********           ELSE
     C****BEGIN*ADD ***  09/10/02  ************************************
     C********** NAKY5     SETGTPNA03
     C********** NAKY5     REDPEPNA03                    60
     C********** *IN60     IFEQ *OFF
     C**********
     C**********           Z-ADDNAUNID    XAUNID
     C********** NAKY4     CHAINPNA03                60
     C**********
     C********** *IN60     DOWEQ*OFF
     C**********
     C********** NANAME    IFEQ EMNAME
     C**********           Z-ADDNAENCD    NEENCD
     C**********           MOVE 'Y'       XXNAME
     C**********           MOVE *ON       *IN60
     C**********           ENDIF
     C**********
     C**N60***** NAKY4     READEPNA03                    60
     C**********           ENDDO
     C**********
     C**********           ENDIF
     C********** XXNAME    IFNE 'Y'
     C****END*ADD *****  09/10/02  ************************************
     C                   SELECT
     C     *IN60         WHENEQ    *OFF
     C                   Z-ADD     NAENCD        NEENCD
     C     *IN60         WHENEQ    *ON
     C     ENELTP        ANDEQ     'L'
     C     NAKY3         CHAIN     PNA01                              60
     C                   Z-ADD     NAENCD        NEENCD
     C     *IN60         WHENEQ    *ON
     C***  End   Chg  ***  02/26/04  **************************************
      *
      *  IF THIS IS A NEW DIVISION THEN WE NEED TO ASSIGN
      *  THE NEXT AVAILABLE ENTITY CODE
     C                   MOVE      *HIVAL        XXENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   MOVE      *HIVAL        XXNLKC
     C***  End   Add  ***  09/10/04  **************************************
     C                   Z-ADD     NECO#         XXCO#
     C                   Z-ADD     NEFND         XXFND
     C                   Z-ADD     NEEMP#        XXEMP#
     C     NAKY1         SETGT     PNA02
     C     NAKY2         READPE    PNA02                                  60
      **** BEGIN ADD **** 08/25/00 ************************************
     C     *IN60         DOWEQ     '0'
     C     NAENCD        ANDEQ     999
     C     NAKY2         READPE    PNA02                                  60
     C                   END
      **** END ADD ****** 08/25/00 ************************************
     C     *IN60         IFEQ      *OFF
     C     NAENCD        ADD       1             NEENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C     NEENCD        IFEQ      999
     C                   MOVE      NEUNID        XXUNID
     C                   EXSR      NAMLNK
     C                   ENDIF
     C***  End   Add  ***  09/10/04  **************************************
     C                   ELSE
     C                   Z-ADD     *ZEROS        NEENCD
     C                   END                                                    *IN60=*OFF
     C***  Begin Del  ***  02/26/04  **************************************
     C****BEGIN*ADD ***  09/10/02  ************************************
     C**********           ENDIF                           *IN60=*OFF
     C****END*ADD *****  09/10/02  ************************************
     C***  End   Del  ***  02/26/04  **************************************
     C                   END
     C*
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NANLKC        NENLKC
     C***  End   Add  ***  09/10/04  **************************************
     C*
     C***  Begin Add  ***  08/17/05  **************************************
     C                   CLEAR                   NEELCD
     C***  End   Add  ***  08/17/05  **************************************
     C                   MOVEL     'A'           NESTAT
     C                   MOVEL     Q1USER        NEUSER
     C                   MOVEL     'SRWRT2'      NEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NEDT
     C                   Z-ADD     DSTIME        NETIME
     C                   Z-ADD     SHDT          NEIDT
     C                   MOVEL     Q1USER        NEIUSR
     C                   Z-ADD     ENREQ#        NEREQ#
      **** BEGIN ADD **** 06/01/01 *********************************************
     C                   MOVE      EMSIC         NESIC
      **** BEGIN ADD **** 04/20/06 *********************************************
      *
      * USE NAICS CODE IF FLAGGED AT STATE LEVEL.
      *
     C     EMMAD1        IFNE      *BLANK
     C                   MOVEL     EMMST         XXST
     C                   ELSE
     C                   MOVEL     EMPST         XXST
     C                   ENDIF                                                  EMMAD1 IFNE
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C     *IN60         IFEQ      *OFF
     C     RRNAIC        ANDEQ     'Y'
     C                   MOVEL     EMNAIC        NESIC
     C                   ENDIF
      **** END   ADD **** 04/20/06 *********************************************
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     ELEMPY        NEEMPY
      **** END   ADD **** 06/01/01 *********************************************
     C*** BEGIN ADD ***  07/23/01  ************************************
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     RSJRST        IFEQ      'NJ'
     C     NETYPE        ANDEQ     '88'
     C*** BEGIN CHG ***  08/05/05  ************************************
     C*********            MOVELEMSEMP    NEUE#
     C                   MOVEL     EMUE#         NEUE#
     C                   ELSE
     C     RSJRST        IFNE      'NJ'
     C     NETYPE        ANDEQ     '88'
     C                   MOVEL     EMSEMP        NEUE#
     C                   ENDIF
     C*** END CHG *****  08/05/05  ************************************
     C                   ENDIF
     C*** END ADD *****  07/23/01  ************************************
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      GETCX
     C*
     C     NOTCX         IFEQ      *ON
     C*
     C     NETYPE        IFEQ      '88'
     C     ALLNAM        ANDEQ     'Y'
     C                   WRITE     PNE01
     C                   ENDIF
     C*
     C***  Begin Add  ***  08/17/05  **************************************
     C     XXADD1        IFEQ      'Y'
     C     XXDIV         ADD       1             NEELCD
     C                   ELSE
     C                   Z-ADD     XXDIV         NEELCD
     C                   ENDIF
     C***  End   Add  ***  08/17/05  **************************************
     C*
     C     NETYPE        IFEQ      '89'
     C     ALLLOC        ANDEQ     'Y'
     C***  Begin Add  ***  05/10/04  **************************************
     C     PAONLY        ANDEQ     'N'
     C***  End   Add  ***  05/10/04  **************************************
     C***  End   Add  ***  02/26/04  **************************************
      **** BEGIN ADD **** 03/19/02 ************************************
     C     NEMA12        IFNE      *BLANKS
     C     NEMA22        ORNE      *BLANKS
      **** END   ADD **** 03/19/02 ************************************
     C                   WRITE     PNE01
     C***  Begin Add  ***  08/17/05  **************************************
     C                   Z-ADD     SVELCD        NEELCD
     C***  End   Add  ***  08/17/05  **************************************
      **** BEGIN ADD **** 03/19/02 ************************************
     C                   ENDIF
      **** END   ADD **** 03/19/02 ************************************
     C***  Begin Add  ***  02/26/04  **************************************
     C                   ENDIF
     C***  End   Add  ***  02/26/04  **************************************
     C*
     C***  Begin Chg  ***  02/26/04  **************************************
     C*****BEGIN ADD **** 01/23/02 ************************************
     C********** EMPAD1    IFNE EMMAD1
     C********** EMPAD1    ANDNE*BLANKS
     C********** EMPAD2    ORNE EMMAD2
     C********** EMPAD2    ANDNE*BLANKS
     C********** EMPCTY    ORNE EMMCTY
     C********** EMPCTY    ANDNE*BLANKS
     C********** EMPST     ORNE EMMST
     C********** EMPST     ANDNE*BLANKS
     C********** EMPZIP    ORNE EMMZIP
     C********** EMPZIP    ANDNE*ZERO
     C**********
     C********** EMPAD1    IFNE XXPAD1
     C********** EMPAD2    ORNE XXPAD2
     C********** EMPCTY    ORNE XXPCTY
     C********** EMPST     ORNE XXPST
     C********** EMPZIP    ORNE XXPZIP
     C     EMPAD1        IFNE      EMMAD1
     C     EMPAD2        ORNE      EMMAD2
     C     EMPCTY        ORNE      EMMCTY
     C     EMPST         ORNE      EMMST
     C     EMPZIP        ORNE      EMMZIP
     C***  End   Chg  ***  02/26/04  **************************************
      *
     C                   MOVE      EMPAD1        NEMA12
     C                   MOVE      EMPAD2        NEMA22
     C                   MOVE      EMPCTY        NEMCT2
     C                   MOVE      EMPST         NEMST2
     C                   Z-ADD     EMPZIP        NEMZP2
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C     NETYPE        IFEQ      '89'
     C     ALLLOC        ANDEQ     'Y'
     C***  Begin Add  ***  05/10/04  **************************************
     C     MAONLY        ANDEQ     'N'
     C***  End   Add  ***  05/10/04  **************************************
     C***  End   Add  ***  02/26/04  **************************************
      **** BEGIN ADD **** 03/19/02 ************************************
     C     NEMA12        IFNE      *BLANKS
     C     NEMA22        ORNE      *BLANKS
      **** END   ADD **** 03/19/02 ************************************
     C                   WRITE     PNE01
     C***  Begin Add  ***  08/17/05  **************************************
     C                   ADD       1             SVELCD
     C***  End   Add  ***  08/17/05  **************************************
      **** BEGIN ADD **** 03/19/02 ************************************
     C                   ENDIF
      **** END   ADD **** 03/19/02 ************************************
      *
     C                   ENDIF
     C                   ENDIF
      **** END   ADD **** 01/23/02 ************************************
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   ENDIF
     C***  End   Add  ***  02/26/04  **************************************
      *
      *  WRITE NAME/ADDRESS TRANSACTION LOCATOR FILE
     C                   Z-ADD     NECO#         XXCO#
     C                   Z-ADD     NEFND         XXFND
     C                   Z-ADD     NEFYR         XXFYR
     C                   Z-ADD     NEEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      *BLANKS       XXELID
     C     NAKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     NECO#         NACO#
     C                   Z-ADD     NEFND         NAFND
     C                   Z-ADD     NEEMP#        NAEMP#
     C                   Z-ADD     EMDIV         NADIV
     C                   MOVE      *BLANKS       NAELID
     C                   Z-ADD     NEFYR         NAFYR
     C                   Z-ADD     NEENCD        NAENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NENLKC        NANLKC
     C***  End   Add  ***  09/10/04  **************************************
     C                   MOVEL     'A'           NASTAT
     C                   MOVEL     Q1USER        NAUSER
     C                   MOVEL     Q1PGM         NAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NADT
     C                   Z-ADD     DSTIME        NATIME
     C                   Z-ADD     SHDT          NAIDT
     C                   MOVEL     Q1USER        NAIUSR
     C                   WRITE     PNA01
     C                   END
      *
     C                   ENDSR                                                  SRWRT2
     *===============================================================*
     *  SRWRT3 - Loads info from WMALP
     *===============================================================*
     C     SRWRT3        BEGSR
      *
     C***  BEG ADD ******  12/13/07  **************************************
     C                   CLEAR                   NERNM4
     C***  END ADD ******  12/13/07  **************************************
      *  DO NOT WRITE ANY TRANSACTION RECORDS IF THIS IS AN
      *  ENTITY/LOCATION AND THE "REPORT TO NCCI FLAG" = 'N'
     C     ALNCCI        IFEQ      'Y'
      *
      *  WRITE AN ENDORSEMENT TRANSACTION RECORD
     C                   MOVE      '2'           NEADTP
     C                   MOVE      XXMST         NEST
     C                   MOVE      ALNAME        NERNAM
     C*** BEGIN ADD ***  04/29/09  ****************************************
     C                   MOVEL(P)  ALNAM2        NERNM2
     C*** END ADD *****  04/29/09  ****************************************
     C***  BEG ADD ******  12/13/07  **************************************
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     @RRPGF        IFEQ      '3'
     C     @RRPGF        OREQ      ' '
      *
     C     *IN60         IFEQ      *OFF
     C     FNPEOF        ANDNE     *BLANKS
      *
     C                   MOVEL     @EMNM         NERNM4
     C     @EMN2         IFNE      *BLANKS
     C     NERNM4        CAT       @EMN2:1       NERNM4
     C                   ENDIF                                                  @EMN2 IFNE *BLANKS
     C     NERNM4        CAT       PEOEXT:1      NERNM4
      *
     C     XXPEO         IFEQ      'Y'
     C     NERNM4        CAT       @@EMNM:1      NERNM4
     C     @@EMN2        IFNE      *BLANKS
     C     NERNM4        CAT       @@EMN2:1      NERNM4
     C                   ENDIF                                                  @@EMN2 IFNE *BLANKS
      *
     C                   ELSE
      *
     C     NERNM4        CAT       EMNAME:1      NERNM4
     C     EMNAM2        IFNE      *BLANKS
     C     NERNM4        CAT(P)    EMNAM2:1      NERNM4
     C                   ENDIF                                                  EMNAM2 NE *BLANKS
      *
     C                   ENDIF                                                  XXPEO IFEQ 'Y'
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C                   ENDIF                                                  @RRPGF IFEQ '3'
     C***  END ADD ** ***  12/13/07  **************************************
     C*** BEGIN ADD ***  04/25/06  ************************************
     C                   MOVE      ALNAMP        NERNMP
     C*** END ADD *****  04/25/06  ************************************
     C*** BEGIN ADD ***  07/23/07  ************************************
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     FNPEOF        IFNE      *BLANKS
     C*
     C                   SELECT
     C     NERNMP        WHENEQ    'Y'
     C                   MOVE      'D'           NERNMP
     C                   OTHER
     C                   MOVE      'C'           NERNMP
     C                   ENDSL
     C*
     C                   ENDIF                                                  FNPEOF IFNE
     C*** END ADD *****  07/23/07  ************************************
     C***  Begin Add  ***  02/26/04  **************************************
     C                   MOVE      ALFEIN        NEFEID
     C                   MOVE      ALTPOB        NELGST
     C                   Z-ADD     ENCAR1        NECAR1
     C                   MOVE      XXMST         NEST
     C***  End   Add  ***  02/26/04  **************************************
      **** BEGIN CHG **** 10/28/99 ************************************
     C********** ALMAD1    IFNE *BLANKS
     C********** ALELTP    ANDEQ'E'
     C********** ALPAD1    OREQ *BLANKS
      **** BEGIN CHG **** 01/23/02 ************************************
     C********** ALPAD1    IFEQ *BLANKS
     C********** ALPAD2    ANDEQ*BLANKS
      **** END   CHG **** 10/28/99 ************************************
     C     XXMFLG        IFEQ      'M'
      **** END   CHG **** 01/23/02 ************************************
     C                   MOVE      ALMAD1        NEMA12
     C                   MOVE      ALMAD2        NEMA22
     C                   MOVE      ALMCTY        NEMCT2
     C                   MOVE      ALMST         NEMST2
     C                   MOVE      ALMZIP        NEMZP2
     C                   ELSE
      *
     C                   MOVE      ALPAD1        NEMA12
     C                   MOVE      ALPAD2        NEMA22
     C                   MOVE      ALPCTY        NEMCT2
     C                   MOVE      ALPST         NEMST2
     C                   MOVE      ALPZIP        NEMZP2
      *
     C                   ENDIF                                                  ALMAD1 IFNE *BLANKS
     C                   MOVE      ALELID        NEELID
      **** BEGIN ADD **** 09/14/00 ************************************
      *
      *  BE SURE TO REPORT ANY LOCATIONS USING A LINK CODE OF 001
      *
     C     ALELTP        IFEQ      'L'
      **** BEGIN CHG **** 03/19/02 ************************************
     C******************** Z-ADD1         NEENCD
     C     NAKY3         CHAIN     PNA01                              60
     C                   Z-ADD     NAENCD        NEENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NANLKC        NENLKC
     C***  End   Add  ***  09/10/04  **************************************
      **** END   CHG **** 03/19/02 ************************************
     C                   ELSE
      **** END ADD ****** 09/14/00 ************************************
      *
      *  RETREIVE UNIQUE ENTITY CODE
     C                   Z-ADD     NECO#         XXCO#
     C                   Z-ADD     NEFND         XXFND
     C                   Z-ADD     NEEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      ALELID        XXELID
     C                   Z-ADD     NEFYR         XXFYR
     C***  Begin Del  ***  02/26/04  **************************************
     C****BEGIN*ADD ***  09/10/02  ************************************
     C**********           MOVE *BLANK    XXNAME
     C****END*ADD *****  09/10/02  ************************************
     C***  End   Del  ***  02/26/04  **************************************
     C*
     C     NAKY          CHAIN     PNA01                              60
     C*
     C***  Begin Chg  ***  02/26/04  **************************************
     C********** *IN60     IFEQ *OFF
     C**********           Z-ADDNAENCD    NEENCD
     C**********           ELSE
     C****BEGIN*ADD ***  09/10/02  ************************************
     C********** NAKY5     SETGTPNA03
     C********** NAKY5     REDPEPNA03                    60
     C********** *IN60     IFEQ *OFF
     C**********
     C**********           Z-ADDNAUNID    XAUNID
     C********** NAKY4     CHAINPNA03                60
     C**********
     C********** *IN60     DOWEQ*OFF
     C**********
     C********** NANAME    IFEQ ALNAME
     C**********           Z-ADDNAENCD    NEENCD
     C**********           MOVE 'Y'       XXNAME
     C**********           MOVE *ON       *IN60
     C**********           ENDIF
     C**********
     C**N60***** NAKY4     READEPNA03                    60
     C**********           ENDDO
     C**********
     C**********           ENDIF
     C********** XXNAME    IFNE 'Y'
     C****END*ADD *****  09/10/02  ************************************
     C                   SELECT
     C     *IN60         WHENEQ    *OFF
     C                   Z-ADD     NAENCD        NEENCD
     C     *IN60         WHENEQ    *ON
     C     ENELTP        ANDEQ     'L'
     C     NAKY3         CHAIN     PNA01                              60
     C                   Z-ADD     NAENCD        NEENCD
     C     *IN60         WHENEQ    *ON
     C***  End   Chg  ***  02/26/04  **************************************
      *
      *  IF THIS IS A NEW ENTITY/LOC THEN WE NEED TO ASSIGN
      *  THE NEXT AVAILABLE ENTITY CODE
     C                   MOVE      *HIVAL        XXENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   MOVE      *HIVAL        XXNLKC
     C***  End   Add  ***  09/10/04  **************************************
     C                   Z-ADD     NECO#         XXCO#
     C                   Z-ADD     NEFND         XXFND
     C                   Z-ADD     NEEMP#        XXEMP#
     C     NAKY1         SETGT     PNA02
     C     NAKY2         READPE    PNA02                                  60
      **** BEGIN ADD **** 08/25/00 ************************************
     C     *IN60         DOWEQ     '0'
     C     NAENCD        ANDEQ     999
     C     NAKY2         READPE    PNA02                                  60
     C                   END
      **** END ADD ****** 08/25/00 ************************************
     C     *IN60         IFEQ      *OFF
     C     NAENCD        ADD       1             NEENCD
     C                   ELSE
     C                   Z-ADD     *ZEROS        NEENCD
     C                   END                                                    *IN60=*OFF
     C***  Begin Add  ***  09/10/04  **************************************
     C     NEENCD        IFEQ      999
     C                   MOVE      NEUNID        XXUNID
     C                   EXSR      NAMLNK
     C                   ENDIF
     C***  End   Add  ***  09/10/04  **************************************
     C***  Begin Del  ***  02/26/04  **************************************
     C****BEGIN*ADD ***  09/10/02  ************************************
     C**********           ENDIF                           *IN60=*OFF
     C****END*ADD *****  09/10/02  ************************************
     C***  End   Del  ***  02/26/04  **************************************
     C                   END
     C*
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NANLKC        NENLKC
     C***  End   Add  ***  09/10/04  **************************************
     C*
      **** BEGIN ADD **** 09/14/00 ************************************
     C                   END                                                    ALELTP 'L'
      **** END ADD ****** 09/14/00 ************************************
     C*
     C***  Begin Add  ***  08/17/05  **************************************
     C                   CLEAR                   NEELCD
     C***  End   Add  ***  08/17/05  **************************************
     C                   MOVEL     'A'           NESTAT
     C                   MOVEL     Q1USER        NEUSER
     C                   MOVEL     'SRWRT3'      NEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NEDT
     C                   Z-ADD     DSTIME        NETIME
     C                   Z-ADD     SHDT          NEIDT
     C                   MOVEL     Q1USER        NEIUSR
     C                   Z-ADD     ENREQ#        NEREQ#
      **** BEGIN ADD **** 06/01/01 *********************************************
     C                   MOVE      ALSIC         NESIC
      *
     C     LDKY          CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     LDEMPY        NEEMPY
     C                   ELSE
     C                   Z-ADD     0             NEEMPY
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      **** END   ADD **** 06/01/01 *********************************************
     C*** BEGIN ADD ***  07/23/01  ************************************
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     RSJRST        IFEQ      'NJ'
     C     NETYPE        ANDEQ     '88'
     C*** BEGIN CHG ***  08/05/05  ************************************
     C*********            MOVELEMSEMP    NEUE#
     C                   MOVEL     EMUE#         NEUE#
     C                   ELSE
     C     RSJRST        IFNE      'NJ'
     C     NETYPE        ANDEQ     '88'
     C                   MOVEL     EMSEMP        NEUE#
     C                   ENDIF
     C*** END CHG *****  08/05/05  ************************************
     C                   ENDIF
     C*** END ADD *****  07/23/01  ************************************
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C     ALDLDT        IFEQ      *ZERO
     C     ALDLDT        ORGT      ELPTDT
     C*
     C     NETYPE        IFEQ      '88'
     C     ALLNAM        ANDEQ     'Y'
     C                   WRITE     PNE01
     C                   ENDIF
     C*
     C     NETYPE        IFEQ      '89'
     C     ALLLOC        ANDEQ     'Y'
     C***  End   Add  ***  02/26/04  **************************************
     C*
     C***  Begin Add  ***  08/17/05  **************************************
     C                   Z-ADD     SVELCD        NEELCD
     C                   ADD       1             SVELCD
     C***  End   Add  ***  08/17/05  **************************************
     C*
     C***  Begin Chg  ***  02/26/04  **************************************
      *****BEGIN ADD **** 03/19/02 ************************************
     C********** NEMA12    IFNE *BLANKS
     C********** NEMA22    ORNE *BLANKS
      *****END** ADD **** 03/19/02 ************************************
     C*  Locations have the same name link as the main division attached to,
     C*   therefore, address records should not be written if the address
     C*   matches either of the division's addresses.
     C*
     C     ALELTP        IFEQ      'L'
     C*
     C     NEMA12        IFEQ      EMMAD1
     C     NEMA22        ANDEQ     EMMAD2
     C     NEMCT2        ANDEQ     EMMCTY
     C     NEMST2        ANDEQ     EMMST
     C     NEMZP2        ANDEQ     EMMZIP
     C     NEMA12        OREQ      EMPAD1
     C     NEMA22        ANDEQ     EMPAD2
     C     NEMCT2        ANDEQ     EMPCTY
     C     NEMST2        ANDEQ     EMPST
     C     NEMZP2        ANDEQ     EMPZIP
     C                   ELSE
     C***  End   Chg  ***  02/26/04  **************************************
     C                   WRITE     PNE01
     C***  Begin Add  ***  02/26/04  **************************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   WRITE     PNE01
     C***  End   Add  ***  02/26/04  **************************************
     C*
      **** BEGIN ADD **** 03/19/02 ************************************
     C                   ENDIF
      **** END   ADD **** 03/19/02 ************************************
     C*
     C***  Begin Add  ***  02/26/04  **************************************
     C                   ENDIF                                                  Address Rcd
     C*
     C                   ENDIF                                                  Deleted
     C***  End   Add  ***  02/26/04  **************************************
      *
      *  WRITE NAME/ADDRESS TRANSACTION LOCATOR FILE
     C                   Z-ADD     NECO#         XXCO#
     C                   Z-ADD     NEFND         XXFND
     C                   Z-ADD     NEFYR         XXFYR
     C                   Z-ADD     NEEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      ALELID        XXELID
     C     NAKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     NECO#         NACO#
     C                   Z-ADD     NEFND         NAFND
     C                   Z-ADD     NEEMP#        NAEMP#
     C                   Z-ADD     EMDIV         NADIV
     C                   MOVE      ALELID        NAELID
     C                   Z-ADD     NEFYR         NAFYR
     C                   Z-ADD     NEENCD        NAENCD
     C***  Begin Add  ***  09/10/04  **************************************
     C                   Z-ADD     NENLKC        NANLKC
     C***  End   Add  ***  09/10/04  **************************************
     C                   MOVEL     'A'           NASTAT
     C                   MOVEL     Q1USER        NAUSER
     C                   MOVEL     Q1PGM         NAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NADT
     C                   Z-ADD     DSTIME        NATIME
     C                   Z-ADD     SHDT          NAIDT
     C                   MOVEL     Q1USER        NAIUSR
     C                   WRITE     PNA01
     C                   END
     C                   ENDIF                                                  ALNCCI='Y'
      *
     C                   ENDSR                                                  SRWRT3
      **** BEGIN CHG **** 03/20/01 ************************************
     *===============================================================*
     *  SRMACG  - Subroutine for mailing address changes
     *===============================================================*
     C     SRMACG        BEGSR
      *
     C                   MOVE      '89'          NETYPE
     C                   MOVE      'WC890605'    NEEND#
     C*
     C***  Begin Add  ***  05/10/04  **************************************
     C*  Mailing address should only be reported on a WC890605 if St Rpt Div
     C*   otherwise it should be reported on a WC890608
     C     EMSTRP        IFNE      'Y'
     C                   MOVE      'WC890608'    NEEND#
     C                   ENDIF
     C***  End   Add  ***  05/10/04  **************************************
     C*
     C                   MOVE      MSG(4)        NEDESC
     C*
     C***  Begin Add  ***  04/09/04  **************************************
     C     ENCBAF        IFEQ      'Y'
     C                   MOVE      MSG(21)       NEDESC
     C                   ENDIF
     C***  End   Add  ***  04/09/04  **************************************
     C*
     C                   MOVE      '1'           NEADTP
     C                   MOVE      XXMST         NEST
     C                   MOVE      EMAREA        NEAREA
     C                   MOVE      EMPHN#        NEPHN#
     C                   MOVE      ENMA12        NEMA12
     C                   MOVE      ENMA22        NEMA22
     C                   MOVE      ENMCT2        NEMCT2
     C                   MOVE      ENMST2        NEMST2
     C                   MOVE      ENMZP2        NEMZP2
     C                   EXSR      SRWRT1
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      SRWRT2
     C***  Begin Del  ***  04/09/04  **************************************
     C********** ENCBAF    IFNE 'Y'
     C***  End   Del  ***  04/09/04  **************************************
     C***  Begin Add  ***  05/10/04  **************************************
     C*  If mailing address is not from St Rpt Div or both addresses were
     C*   changed report all remaining addresses under WC890608
     C     MAONLY        IFEQ      'N'
     C                   MOVE      'WC890608'    NEEND#
     C***  End   Add  ***  05/10/04  **************************************
     C                   EXSR      SRALL
     C***  Begin Add  ***  05/10/04  **************************************
     C                   ENDIF
     C***  End   Add  ***  05/10/04  **************************************
     C***  Begin Del  ***  04/09/04  **************************************
     C**********           ENDIF                           ENCBAF<>Y
     C***  End   Del  ***  04/09/04  **************************************
     C***  End   Add  ***  02/26/04  **************************************
      *
     C                   ENDSR                                                  SRMACG
     *===============================================================*
     *  SRPACG  - Subroutine for physical address changes
     *===============================================================*
     C     SRPACG        BEGSR
      *
     C                   MOVE      '89'          NETYPE
     C                   MOVE      'WC890608'    NEEND#
     C                   MOVE      MSG(20)       NEDESC
     C*
     C***  Begin Add  ***  04/09/04  **************************************
     C     ENCBAF        IFEQ      'Y'
     C                   MOVE      MSG(21)       NEDESC
     C                   ENDIF
     C***  End   Add  ***  04/09/04  **************************************
     C*
     C                   MOVE      '2'           NEADTP
     C                   MOVE      XXMST         NEST
      **** BEGIN CHG **** 07/12/01 ************************************
     C***********ALKY      CHAINPAL01                60
     C************IN60     IFEQ *OFF
     C***********ALPAD1    IFEQ *BLANKS
     C***********ALPAD2    ANDEQ*BLANKS
     C***********          MOVE ALMAD1    NEMA12
     C***********          MOVE ALMAD2    NEMA22
     C***********          MOVE ALMCTY    NEMCT2
     C***********          MOVE ALMST     NEMST2
     C***********          MOVE ALMZIP    NEMZP2
     C***********          ELSE
     C***********          MOVE ALPAD1    NEMA12
     C***********          MOVE ALPAD2    NEMA22
     C***********          MOVE ALPCTY    NEMCT2
     C***********          MOVE ALPST     NEMST2
     C***********          MOVE ALPZIP    NEMZP2
     C***********          ENDIF                           ALMAD1 IFNE *BLANKS
     C***********          END
     C                   MOVE      EMAREA        NEAREA
     C                   MOVE      EMPHN#        NEPHN#
     C                   MOVE      ENPA12        NEMA12
     C                   MOVE      ENPA22        NEMA22
     C                   MOVE      ENPCT2        NEMCT2
     C                   MOVE      ENPST2        NEMST2
     C                   MOVE      ENPZP2        NEMZP2
      **** END CHG ****** 07/12/01 ************************************
     C                   EXSR      SRWRT1
     C***  Begin Add  ***  02/26/04  **************************************
     C                   EXSR      SRWRT2
     C***  End   Add  ***  02/26/04  **************************************
     C                   EXSR      SRALL
      *
     C                   ENDSR                                                  SRPACG
      **** END CHG ****** 03/20/01 ************************************
      **** BEGIN ADD **** 04/30/01 *********************************************
     C*****************************************************************
      *
      * IF A NEW STATE IS ADDED TO THE POLICY THEN UPDATE THE WMSHP
      * POLICY RECORD.
      *
     C*****************************************************************
     C     UPDPSH        BEGSR
      *
     C     SHKY          CHAIN     PSH02                              60
     C*** BEGIN ADD ***  07/07/04  ************************************
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      *BLANKS       XXSST            30
     C                   MOVEA     SST           XXSST
     C*
     C* ONLY ADD STATE IF RECORD HAS BEEN UPDATED WITH ORIGINAL POLICY
     C* STATES.
     C*
     C     XXSST         IFNE      *BLANKS
     C*** END ADD *****  07/07/04  ************************************
     C                   MOVE      *BLANK        XXUPSH            1
     C                   Z-ADD     1             X                 2 0
     C     *BLANKS       LOOKUP    SST(X)                                 60
      *
     C     ELKY1         CHAIN     PEL02                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL(P)  'NCCI'        XXTBID
     C                   MOVEL(P)  RSJRST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'XX'          XZST              2
     C                   ELSE
     C                   MOVE      RSJRST        XZST              2
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     1             I
     C     XZST          LOOKUP    SST(I)                                 60
     C*
     C     *IN60         IFEQ      *OFF
     C     X             ANDLE     15
     C                   MOVE      XZST          SST(X)
     C*** begin chg ***  01/19/18  ************************************
     C*********          MOVE      '3'           SSS(X)
     C                   MOVE      '4'           SSS(X)
     C*** end chg *****  01/19/18  ************************************
     C*** BEGIN CHG ***  06/27/14  ************************************
     C*** BEGIN CHG ***  05/12/05  ************************************
     C*****    XZST          IFEQ      'CA'
     C*** BEGIN ADD ***  02/15/11  ************************************
     C*****    XZST          OREQ      'MA'
     C*** END   ADD ***  02/15/11  ************************************
     C     SST(I)        IFEQ      'MA'
     C*** begin add ***  03/23/18  ************************************
     C     FNPEOS        ANDNE     'Y'
     C*** end add *****  03/23/18  ************************************
     C*** END   CHG ***  06/27/14  ************************************
     C                   MOVE      '4'           SSS(X)
     C                   ENDIF
     C*** END ADD *****  05/12/05  ************************************
     C                   ADD       1             X
     C                   MOVE      'Y'           XXUPSH            1
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *** BEG ADD ***** 05/11/09 ***************************************
      *
     C     CLTID         IFEQ      '093'
     C*** begin add ***  01/19/18  ************************************
     C     CLTID         ORNE      '093'
     C*** end add *****  01/19/18  ************************************
      *
      * ADD NCCI STATE RECORD IF POLICY HAS EXPERIENCE RATING OF INTERSTATE ('1'
      * INTERSTATE AND INTRASTATE ('2'),  IF NEEDED
      *
     C     EMKY5         CHAIN     PEM01                              60
     C                   MOVE      EMEXRT        XXEXRT            1
      *
     C     XXEXRT        IFEQ      '1'
     C     XXEXRT        OREQ      '2'
      *
      *  CHECK IF 'XX' IS IN ARRAY
      *
     C                   MOVE      'XX'          XZST
     C                   Z-ADD     1             I2                2 0
     C     XZST          LOOKUP    SST(I2)                                60
      *
      *  ADD 'XX' RECORD TO FIRST BLANK POSITION IN ARAY
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     1             X
     C     SST(X)        DOUEQ     *BLANK
      *
     C     X             IFEQ      16
     C     SST(X)        OREQ      *BLANK
     C                   LEAVE
     C                   ENDIF
      *
     C                   ADD       1             X
     C                   ENDDO
      *
     C     X             IFLE      15
     C                   MOVE      XZST          SST(X)
     C*** begin chg ***  01/19/18  ************************************
     C*********          MOVE      '3'           SSS(X)
     C                   MOVE      '4'           SSS(X)
     C*** end chg *****  01/19/18  ************************************
     C                   MOVE      'Y'           XXUPSH
     C                   ENDIF                                                  IF X <= 15
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  XXEXRT IFEQ
      *
     C                   ENDIF                                                  CLTID = 093
      *** END ADD ***** 05/11/09 ***************************************
     C*
     C     XXUPSH        IFEQ      'Y'
     C*** BEGIN CHG ***  05/12/05  ************************************
     C*********            MOVE '3'       SHSTAT
     C                   MOVE      '4'           SHSTAT
     C     1             DO        15            I
     C*
     C     SST(I)        IFNE      *BLANKS
     C     SSS(I)        ANDEQ     '3'
     C                   MOVE      '3'           SHSTAT
     C                   ENDIF
     C*
     C                   ENDDO
     C*** END ADD *****  05/12/05  ************************************
     C                   UPDATE    PSH02
     C                   ENDIF
     C*** BEGIN ADD ***  07/07/04  ************************************
     C                   ENDIF                                                  XXSST IFNE
      *
     C                   MOVE      *BLANKS       XXSST
      *
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  07/07/04  ************************************
      *
     C                   CLEAR                   PSH02
      *
     C                   ENDSR                                                  UPDPSH
     C*****************************************************************
      **** END   ADD **** 04/30/01 *********************************************
      *
**  TABLE ST - STATES IN ENDORSEMENT NUMERICAL ORDER
ALAZARCACOCTDEDCFLGAIDILINIAKSKYLAMEMDMAMIMNMSMOMTNENV
NHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIWYPRZZHIZZAK
**
OTHER - EXP. RATING MOD CHG
OTHER - CONT. RATING MOD CHG
NAME CHANGE
MAILING ADDRESS CHANGE
ENTITY DELETION
LOCATION DELETION
ADDITION OF ENTITY
ADDITION OF LOCATION
OTHER - NOMINAL/OWNERSHIP
OTHER - COI - POLICY # CHG
OTHER - COI - EFF DATE CHG
OTHER - COI - EXP DATE CHG
OTHER - COI - AGENT CHANGE
OTHER - COI - LGL NATURE CHG
OTHER - COI - CARRIER # CHG
ADDITION OF EMPLR DIVISION
DELETION OF EMPLR DIVISION
ENTITY CHANGE
LOCATION CHANGE
PHYSICAL ADDRESS CHANGE
BOTH MAIL & PHYS ADDRESS CHG                                04/09/04
msg,22
msg,23
msg,24
msg,25
msg,26
msg,27
msg,28
msg,29
msg,30
