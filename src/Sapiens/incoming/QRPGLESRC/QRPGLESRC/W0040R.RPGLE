      * (c) 2021 Sapiens. All Rights Reserved.                             *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H*/define CONVERT
     H* ============================================================= *
     H/TITLE W0040R - WORK WITH AGENTS
     H* ============================================================= *
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL AGENTS *
     H*                BY NAME, AGENT# OR FED ID #.  EACH AGENT CAN BE*
     H*                ACCESSED FOR UPDATE, DISPLAY OR SELECTION      *
     H*                DEPENDING UPON THE CALLING PROGRAM.            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*    W0040R02 - AGENT MASTER INFORMATION (OPT 2/4/5/20)         *
     H*    W0041R   - WORK WITH AGENT ASSIGNMENTS (OPT 7)             *
     H*    W0042R   - WORK WITH AGENT PRODUCERS (OPT 8)               *
     H*    W4020R   - CHANGE FEDERAL ID # (OPT 10)                    *
     H*    SC010R   - DARTS (OPT 14)                                  *
     H*                                                               *
     H*  CREATED: 12/03/97  PROGRAMMER: C:33  PROJECT: WC683          *
     H* ============================================================= *
     H*  REVISIONS:                                                   *
     H*
     H* 01/08/24 C:342 Project: WC2676                                *
     H*                Change to search Agent FEIN/Mod in new Agency  *
     H*                configuration screen                           *
     H*                                                               *
     H* 06/21/21 C:298 Project: SY426
     H*                Re-Brand and Config for Visit Tracking
     H*                                                               *
     H* 11/27/18 C:33  Project: SY406
     H*                Add Option 75 = Template Associations
     H*                                                               *
     H* 10/24/18 C:180 Project: 129WVH                                *
     H*                Contacts to be updated in inquiry mode         *
     H*                if contacts security allow it.                 *
     H*                                                               *
     H* 04/04/16 C:136 PhaseWare: 40576                               *
     H*                Prevent inactivation of Agt Mst if active      *
     H*                Agt Assnment exists                            *
     H*                                                               *
     H* 01/24/15 C:062 Project 114WGU                                 *
     H*                Add parmaeters for name subsetting             *
     H*                Add visit tracking option                      *
     H*                                                               *
     H* 12/26/14 C:283 Support WA099 87243                            *
     H*                1) Use the Default Group when restriction      *
     H*                    flag is set to 'Y' and nothing is in the   *
     H*                    restriction table.                         *
     H*                2) When only single entry is there in the      *
     H*                    Group restriction table, then use the      *
     H*                    proper groups to set the Group Restrcition.*
     H*                                                               *
     H* 09/19/13 C:283 Support WA147 83464                            *
     H*                Correct the Group Restrictions handling if     *
     H*                there are multiple groups restricted.          *
     H*                                                               *
     H* 04/25/13 C:283 Support WA114 82188                            *
     H*                Use the WPIBANK to validate for EFT account.   *
     H*                                                               *
     H* 01/29/13 C:283 Project Transform Redmine 2070                 *
     H*                Corrected the code for view# 3 and 4 by read-  *
     H*                ing the suitable logical and populating the    *
     H*                required RRN.                                  *
     H*                                                               *
     H* 04/17/12 C:255 Project  WC2044                                *
     H*                Added new logical file WMBILA and key as well. *
     H*                                                               *
     H* 09/15/10 C:246 PROJECT: SY190  PRISS #187                     *
     H*                Fix opt 13 Not editable on Inquiry mode.       *
     H*                                                               *
     H* 12/15/09 C:232 PROJECT: 139WLX , PRISS #16                    *
     H*                ACTIVATE 'F12=CANCEL' OPTION IN "WORK WITH     *
     H*                AGENT MASTER INFORMATION" (CHANGE NNY TO NYY). *
     H*                                                               *
     H* 09/30/09 C:232 PROJECT: 139WLX                                *
     H*                INQUIRY ACCESS TO AGENT FROM AGENT HISTORY.    *
     H*                                                               *
     H* 06/29/09 C:191 WC1806                                         *
     H*                EFT - UPDATE CALL PARAMS TO W0028R            *
     H*                                                               *
     H* 04/01/08 C:189 WC997                                          *
     H*                IMPLEMENT EFT - AGENT COMMISSION PAYMENTS      *
     H*                (ADDITIONAL CHANGES FOR CONTROL FLAG WCFL17)   *
     H*                                                               *
     H* 03/19/08 C:189 WC997                                          *
     H*                IMPLEMENT EFT - AGENT COMMISSION PAYMENTS      *
     H*                (ADDITIONAL CHANGES FOR CONTROL FLAG WCFL38)   *
     H*                                                               *
     H* 01/28/08 C:33  Support                                        *
     H*                If the group is not passed in, ensure that the *
     H*                 RRN of WMAGP is returned.                     *
     H*                                                               *
     H* 09/20/07 C:136 WC997                                          *
     H*                IMPLEMENT EFT - AGENT COMMISSION PAYMENTS      *
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491                                *
     H*                ADD OPTION TO CALL RETURN FUNDS                *
     H*                                                               *
     H*  07/03/07 C:132 Project: WC1423                               *
     H*                 Change number of lines on F15=Fold to fille   *
     H*                 screen. NO CODE CHANGES IN PGM-COMPILE ONLY   *
     H*                                                               *
     H*  06/28/07 C:132 Project: WC1423                               *
     H*                 Change Option line from line 22 to line 23    *
     H*                 COMPILE ONLY - No changes in Code             *
     H*                                                               *
     H*  01/15/07 C:136 SUPPORT: WA071 56506                          *
     H*                 DISPLAY F23  (DISPLAY SCFKL2 IN W0040F)       *
     H*                                                               *
     H*  11/28/06 C:132 Project: WC1423                               *
     H*                 Added Option 15=Fold/truncate to display the  *
     H*                 mailing & physical address on the screen.     *
     H*                                                               *
     H*  12/18/06 C:53  Support: 54416                                *
     H*                 Correct bad RRN on return.                    *
     H*                                                               *
     H*  08/21/06 C:53  Support: 53318                                *
     H*                 Added check for additional restriction info   *
     H*                                                               *
     H*  01/18/06 C:168 Project: 93WFP                                *
     H*                 Added  option 13 (work with contacts) to the  *
     H*                 list of available top options                 *
     H*                                                               *
     H*  01/02/04 C:136 Support: OP093 40592                          *
     H*                 Refine subset on producer name to use less    *
     H*                 processing time.                              *
     H*                                                               *
     H*  10/09/03 C:136 Project: WC1158                               *
     H*                 Add subset selections for producer number     *
     H*                 and name.                                     *
     H*                                                               *
     H*  06/25/02 C:09  PROJECT WC683                                 *
     H*                 "Group Status" in subset (F17) to subset      *
     H*                 Agent Assignment records based on active/     *
     H*                 inactive status was not working.              *
     H*                                                               *
     H*  12/31/01 C:136 PROJECT WC1023                                *
     H*                 Add view to display Agent Federal Name        *
     H*                                                               *
     H*  03/17/00 C:61  PROJECT WC976                                 *
     H*               - Add parm to indicate when called from quote   *
     H*                 menu.                                         *
     H*               - Pass @@QTE to Work with Agent Assignments.    *
     H*                 (opt 7, W0041R).                              *
     H*                                                               *
     H*  11/10/98  C:78  PROJECT: WC577                               *
     H*                  OPT 7 AND OPT 8 ALLOWING CHANGE IN SELECT    *
     H*                  MODE. OPTION 1 AND 5 ARE THE ONLY ONE THAT   *
     H*                  SHOULD WORK IN SELECT MODE.                  *
     H*                                                               *
     H*  02/04/98  C:33  PROJECT: WC683                               *
     H*                  ADD OPTION 14-DARTS                          *
     H*****************************************************************
     FW0040F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMAAL3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAA01:DBRCDF)
     FWMAAL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAA01:DBRCDF2)
     FWMAAL     IF   E           K DISK
      *** BEG CHG **** 12/31/01 ************************************************
     F****************                              KINFDS DBFDS3
     F*********** PAA01                             KRENAMEDBRCDF3
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PAA01:DBRCDF4)
      *** BEG CHG **** 12/31/01 ************************************************
     FWMAAL4    IF   E           K DISK
     F                                     INFDS(DBFDB)
     F                                     RENAME(PAA01:DBRCDB)
     FWMAAL2    IF   E           K DISK
     F                                     INFDS(DBFDB3)
     F                                     RENAME(PAA01:DBRCDB3)
     FWMFNP     IF   E             DISK
     FWMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FWMAGL     IF   E           K DISK
     F                                     INFDS(DBFDBA)
      *** BEG ADD **** 12/31/01 ************************************************
     FWMAAL6    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PAA01:DBRCDF3)
     FWMAAL7    IF   E           K DISK
     F                                     INFDS(DBFDB4)
     F                                     RENAME(PAA01:DBRCDB4)
      *** END ADD **** 12/31/01 ************************************************
      *** BEG ADD **** 10/09/03 ************************************************
      * Agent producer file  (co#/prod)
     FWMAPL1    IF   E           K DISK
      *** END ADD **** 10/09/03 ************************************************
      *** BEG ADD **** 01/02/04 ************************************************
     FWMAPL2    IF   E           K DISK
     F                                     RENAME(PAP01:PAP02)
      *** END ADD **** 01/02/04 ************************************************
     F***  Beg   Chg  ***  04/17/12  *********************************
     F*****WMBIL2    IF   E           K DISK
     FWMBILA    IF   E           K DISK
     F***  End   Chg  ***  04/17/12  *********************************
     F                                     RENAME(PBI01:PBI02)
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  09/20/07  *********************************
     F***  BEGIN ADD  ***  09/30/09  *********************************
     FWMAHP     IF   E             DISK
     F***  END   ADD  ***  09/30/09  *********************************
     F***  BEGIN ADD  ***  10/24/18  *********************************
     FSMUPL     IF   E           K DISK
     F***  END   ADD  ***  10/24/1   *********************************
     D*
      *** BEG CHG **** 12/31/01 ************************************************
     D*****************   HD      1   3 78               Column headings
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              Column headings
      *** END CHG **** 12/31/01 ************************************************
     D***  BEGIN CHG  ***  01/13/06  *********************************
     D**********          OF      1  16  1   OFD    35   Options, Function
      ***** BEG CHG ***** 11/28/06 ************************************
     D**********          OF      1  17  1   OFD    35   Options, Function
      ***** BEG CHG ***** 07/18/07 ************************************
     D******************* OF      1  18  1   OFD    35   Options, Function
      *** BEG CHG ***** 09/20/07 **************************************
     D******************* OF      1  19  1   OFD    35   Options, Function
     D*** Begin Chg *** 01/24/15 ********************************************
     D*OF*******       S              1    DIM(20) CTDATA PERRCD(1)             Options, Function
     D*OFD******       S             35    DIM(20) ALT(OF)
     D***  Begin Chg  ***  11/27/18  ************************************
     D*OF*******       S              1    DIM(21) CTDATA PERRCD(1)             Options, Function
     D*OFD******       S             35    DIM(21) ALT(OF)
     D#ofOFD           C                   25
     D OF              S              1    DIM(#ofOFD) CTDATA PERRCD(1)         Options, Function
     D OFD             S             35    DIM(#ofOFD) ALT(OF)
     D***  End   Chg  ***  11/27/18  ************************************
     D*** End   Chg *** 01/24/15 ********************************************
      *** END CHG ***** 09/20/07 **************************************
      ***** END CHG ***** 07/18/07 ************************************
      ***** END CHG ***** 11/28/06 ************************************
     D***  END   CHG  ***  01/13/06  *********************************
     D PH              S              1    DIM(14)
     D*
     D*
     D*
     D DBFDB           DS
     D  DBRRB                397    400B 0
     D DBFDB3          DS
     D  DBRRB3               397    400B 0
     D DBFDBA          DS
     D  DBRRNA               397    400B 0
      *** BEG ADD **** 12/31/01 ************************************************
     D DBFDB4          DS
     D  DBRRB4               397    400B 0
      *** END ADD **** 12/31/01 ************************************************
     D                 DS
     D  VIEW1                  1     74
     D  V1NAME                 1     30
     D  V1ASTS                33     33
     D  V1PHN#                36     49
     D  V1FND                 51     53
     D  V1AGT#                55     59
     D  V1GSTS                62     62
     D  V1CNDT                65     74
     D                 DS
     D  VIEW2                  1     74
     D  V2FND                  1      3
     D  V2AGT#                 5      9
     D  V2NAME                11     40
     D  V2ASTS                43     43
     D  V2PHN#                46     59
     D  V2GSTS                62     62
     D  V2CNDT                65     74
      ** 12/31/01 *** VIEW3 will become the 4th view (*IN24)
     D*************** VIEW4 will become the 3rd view (*IN23)
     D                 DS
     D  VIEW3                  1     74
     D  V3FEIN                 1      9
     D  V3MOD                 11     13
     D  V3NAME                15     30
     D  V3ASTS                33     33
     D  V3PHN#                36     49
     D  V3FND                 51     53
     D  V3AGT#                55     59
     D  V3GSTS                62     62
     D  V3CNDT                65     74
      *** BEG ADD **** 12/31/01 ***********************************************
     D                 DS
     D  VIEW4                  1     74
     D  V4FDNM                 1     30
     D  V4ASTS                33     33
     D  V4PHN#                36     49
     D  V4FND                 51     53
     D  V4AGT#                55     59
     D  V4GSTS                62     62
     D  V4CNDT                65     74
      *** END ADD **** 12/31/01 ***********************************************
     D                 DS
     D  PHN                    1      7
     D  PHN1                   1      3
     D  PHN2                   4      7
     D***  BEGIN ADD  ***  02/04/98  *********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCFEIN                 1      9  0
     D  QCMOD                 10     12  0
     D***  END   ADD  ***  02/04/98  *********************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent Master Informa-
     D                                     tion')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0040R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
      *** BEG CHG **** 12/31/01 ************************************************
     D************* 3                     C         VIEWMX
     D VIEWMX          C                   CONST(4)
      *** END CHG **** 12/31/01 ************************************************
     D SF1PAG          C                   CONST(12)
      ***** BEG ADD ***** 11/28/06 ************************************
     D NOMADR          C                   CONST('No Mailing Address')
     D NOPADR          C                   CONST('No Physical Address')
      ***** Beg Add ***** 04/17/12 ************************************
     D xxeftc          s              1
      ***** End Add ***** 04/17/12 ************************************
      *******Beg Add********04/25/13**********************************
     D/copy WPRBANK
     d @@co            s              3
     d @@fnd           s              3
     d @@appl          s              2
     d @@srdt          s              8
     d @@acct          s             20
     d @@hdcy          s              1
     d @@chkq          s             10
     d @@ftyp          s             10
     d @@rtcd          s              1
     d @@efdt          s              7
     d @@cpyq          s             10
     d @@cktp          s             10
     d @@eftc          s              1
     d @@coid          s             20
      *******Beg Add********09/19/13**********************************
     d count           s              3P 0
     d fndary          s              3P 0 dim(999)
      *******End Add********09/19/13**********************************
      ***** Beg Add ***** 12/26/14 ************************************
     d savFnd          s              3P 0 inz(000)
      ***** End Add ***** 12/26/14 ************************************
     d*** Begin Add *** 01/24/15 ********************************************
     d #@Rrn           s              9a
     d ##Category      s             10a
     d ##Assign        s             10a
     d ##VstFrDt       s              8a
     d ##VstToDt       s              8a
     d*** End   Add *** 01/24/15 ********************************************

     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
      *
      *******End Add********04/25/13**********************************
      *** beg add ***** 04/04/16 *************************************
     d/copy wpragent
      *** end add ***** 04/04/16 *************************************
     D***  Begin Add  ***  11/27/18  *************************************
      /copy WITMPLTD
      /copy sPrModule
     D***  End   Add  ***  11/27/18  *************************************
     C*
     C/COPY SCOPYWW01C
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@VIEW            1 0
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   PARM                    @@QTE             1
      ***** END ADD ******* 03/17/00 **********************************
      ***** BEG ADD ******* 09/30/09 **********************************
     C                   PARM                    @@MODE            1
      ***** END ADD ******* 09/30/09 **********************************
     C*** Begin Add *** 01/24/15 ***************************************
     C                   PARM                    #@NAME           32
     C*** End   Add *** 01/24/15 ***************************************
     C*
     C* NOTE:-In order for the WW to be group specific, @@RRN needs to
     C*       be related to the group file.
     C*      -When searching:-If the @@RRN is passed in as the group
     C*                       file, @@RRN will return the rrn of WMAAP.
     C*                      -If the @@RRN is passed in *ZEROS, @@RRN
     C*                       will return the rrn of WMAGP.
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCAGRR        ##RRN             9 0
     C*
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C     EFTPL         PLIST
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C**************       PARM *ZEROS    ##RRN   90
     C                   PARM      SCDBRR        ##RRN             9 0
     C*** END   CHG ****** 06/29/09 ************************************
     C                   PARM      *BLANKS       ##CLM            13
     C                   PARM      *BLANKS       #1FEIN           12
     C                   PARM      *ZEROS        ##SEQ#            5 0
     C                   PARM      'A'           ##TYPE            1
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      SCAGT#        ##AGT#            5 0
     C***  END   ADD  ***  09/20/07  *********************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C                   PARM                    ##XCO#            1
     C                   PARM                    ##XFND            1
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNAME
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKFND
     C                   KFLD                    KKAGT#
     C*
     C     KEY1C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKFND
      *** BEG ADD **** 12/31/01 ************************************************
     C     KEY1D         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFDNM           40
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKFND
     C                   KFLD                    KKAGT#
      *** END ADD **** 12/31/01 ************************************************
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C     KEY2A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKNAME
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKAGT#
     C*
     C     KEY2B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKAGT#
     C*
     C     KEY2C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
      *** BEG ADD **** 12/31/01 ************************************************
     C     KEY2D         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFDNM           40
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKAGT#
      *** END ADD **** 12/31/01 ************************************************
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKYA         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCNAME
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    SCAGT#
     C*
     C     AAKYC         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
      *** BEG ADD **** 12/31/01 ************************************************
     C     AAKYD         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFDNM
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    SCAGT#
      *** END ADD **** 12/31/01 ************************************************
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C     AGKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFEIN
     C*
      *** BEG ADD ***** 10/09/03 ***********************************************
     C     APKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPROD            5 0
      *** END ADD ***** 10/09/03 ***********************************************
     C*
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C***  Begin Chg  ***  04/17/12  *********************************
     C*****BIKY2         KLIST
     C*****              KFLD                    ##CO#
     C*****              KFLD                    XXAPPL
     C*****              KFLD                    XXFND
     C*****              KFLD                    XXCKTP

     C     BIKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    XXCKTP
     C                   KFLD                    XXFND
     C                   KFLD                    XXEFTC
     C***  End   Chg  ***  04/17/12  *********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    AANAME        KKNAME
     C     *LIKE         DEFINE    AAFEIN        KKFEIN
     C     *LIKE         DEFINE    AAMOD         KKMOD
     C     *LIKE         DEFINE    AAFND         KKFND
     C     *LIKE         DEFINE    AAAGT#        KKAGT#
     C     *LIKE         DEFINE    AANAME        U1PDSC
     C     *LIKE         DEFINE    AANAME        U2PDSC
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C     *LIKE         DEFINE    BIAPPL        XXAPPL
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BICKTP        XXCKTP
     C***  END   ADD  ***  09/20/07  *********************************
     C*
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSNAME        IFNE      *BLANKS
     C     SSAGT#        ORNE      *ZERO
     C     SSASTS        ORNE      *BLANK
     C     SSGSTS        ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSNAME<>*BLANKS
     C*
     C  N31SSFND         IFNE      *ZERO
     C                   MOVE      *ON           *IN11
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *OFF          *IN32
     C                   ENDIF                                                  SSFND<>*ZERO
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '20'
     C     SCSTAT        ANDEQ     'A'
     C*
     C     AGKY1         CHAIN     WMAGL                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AGSTAT        IFNE      'I'
     C     AGMOD         ANDNE     SCMOD
     C*
     C     SCMOD         IFEQ      AG109M
     C     SCMOD         OREQ      AGCCKM
     C                   SETON                                        71  90
     C                   LEAVE
     C                   ENDIF                                                  AG109M=SCMOD=SCCCKM
     C*
     C                   ENDIF                                                  AGSTAT<>I
     C*
     C     AGKY1         READE     WMAGL                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF

      *** beg add ***** 04/04/16 **********************
      /free
       If not(*in90) and
        GetActAgtAsg(AGco#:AGfein:AGmod) = *on;
          errMsg73 = 'Agent Master cannot be inactivated ' +
           'because active Agent Assignment exists.';
          *in73 = *on;
          *in90 = *on;
       Endif;
      /end-free
      *** end add ***** 04/04/16 **********************

     C     SCSEL         WHENEQ    '70'
     C*
     C***  Beg   Chg  ***  04/25/13  *********************************
     C***********        MOVEL(P)  'AG'          XXAPPL
     C***********        Z-ADD     SCFND         XXFND
     C***  Beg   Chg  ***  04/17/12  *********************************
     C******             MOVEL(P)  'EFT'         XXCKTP
      /Free
       //********xxcktp = ' ' ;
       //********xxeftc = 'E';
      /End-Free
     C***  End   Chg  ***  04/17/12  *********************************
     C*
     C***********BIKY2         CHAIN     PBI02                              90
     C*
     C***********        MOVE      *IN90         *IN72
     C                   Move      ##co#         @@co
     C                   Move      scfnd         @@fnd
     C                   Move      'AG'          @@appl
     C                   Time                    dsdttm
     C                   Move      dsdate        @@srdt


      /Free
       @@acct = *Blanks;
       @@call = *Blanks;
       @@hdcy = *Blanks;
       @@chkq = *Blanks;
       @@ftyp = *Blanks;
       @@rtcd = *Blanks;
       @@efdt = *Zeros ;
       @@cpyq = *Zeros ;
       @@cktp = *Blanks;
       @@coid = *Blanks;
       @@eftc = 'E'    ;

       @@acct= GetBankAcc(@@co : @@fnd: @@appl: @@srdt: @@call:
                @@hdcy: @@chkq: @@ftyp: @@efdt: @@cpyq:
                @@cktp: @@eftc: @@coid: @@rtcd);
      /End-Free

     c*
     c                   if        @@rtcd='1'
     c                   seton                                        72  90
     c                   endif                                                  not %found
     c*** end   chg ***  04/25/13  *****************************************
     C***  END   ADD  ***  09/20/07  *********************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSNAME
     C                   CLEAR                   SSAGT#
     C                   CLEAR                   SSASTS
     C                   CLEAR                   SSGSTS
     C*
     C     @@CALL        IFEQ      '2'
     C  N31              MOVE      'A'           SSASTS
     C   31              MOVE      'A'           SSGSTS
     C                   ENDIF                                                  @@CALL=2
     c*** Begin Add *** 01/24/15 **************************************
     c                   If        q1Prms > 7
     c                   eval      ssName = #@Name
     c                   EndIf
     C*** End   Add *** 01/24/15 ***************************************
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C  N32              MOVE      *LOVAL        KKFND
     C   32              Z-ADD     SSFND         KKFND
     C                   MOVE      *LOVAL        KKNAME
     C                   MOVE      *LOVAL        KKFEIN
     C                   MOVE      *LOVAL        KKMOD
     C                   MOVE      *LOVAL        KKAGT#
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVE      *LOVAL        KKFDNM
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SCPST1        KKNAME
     C                   MOVE      *LOVAL        KKFEIN
     C                   MOVE      *LOVAL        KKMOD
     C  N32              MOVE      *LOVAL        KKFND
     C   32              Z-ADD     SSFND         KKFND
     C     *IN22         WHENEQ    *ON
     C     *IN32         ANDEQ     *OFF
     C                   MOVE      SCPST2        KKFND
     C                   MOVE      SCPSTA        KKAGT#
     C     *IN22         WHENEQ    *ON
     C     *IN32         ANDEQ     *ON
     C                   Z-ADD     SSFND         KKFND
     C                   MOVE      SCPST9        KKAGT#
      *** BEG ADD **** 12/31/01 ************************************************
     C     *IN23         WHENEQ    *ON
     C                   MOVEL(P)  SCPST3        KKFDNM
     C                   MOVE      *LOVAL        KKFEIN
     C                   MOVE      *LOVAL        KKMOD
     C  N32              MOVE      *LOVAL        KKFND
     C   32              Z-ADD     SSFND         KKFND
      *** END ADD **** 12/31/01 ************************************************
      *** BEG CHG **** 12/31/01 ************************************************
     C********** *IN23     WHEQ *ON
     C     *IN24         WHENEQ    *ON
     C                   MOVE      SCPST4        KKFEIN
      *** END CHG **** 12/31/01 ************************************************
     C                   MOVE      SCPSTB        KKMOD
     C  N32              MOVE      *LOVAL        KKFND
     C   32              Z-ADD     SSFND         KKFND
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCFND         KKFND
     C                   MOVEL(P)  SCNAME        KKNAME
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C                   Z-ADD     SCAGT#        KKAGT#
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVEL(P)  SCFDNM        KKFDNM
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     AGKY          CHAIN     WMAGL                              60
     C*
     C                   Z-ADD     AAFND         SCFND
     C                   MOVEL(P)  AANAME        SCNAME
     C                   Z-ADD     AAFEIN        SCFEIN
     C                   Z-ADD     AAMOD         SCMOD
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   Z-ADD     DBRRNA        SCAGRR
     C                   MOVE      AGSTAT        SCSTAT
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVEL(P)  AAFDNM        SCFDNM
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     *IN32         ANDEQ     *ON
     C     AAKYA         CHAIN     DBRCDB                             60
     C                   Z-ADD     DBRRB         SCDBRR
      *** BEG ADD **** 12/31/01 ************************************************
     C     *IN23         WHENEQ    *ON
     C     *IN32         ANDEQ     *ON
     C     AAKYD         CHAIN     DBRCDB4                            60
     C                   Z-ADD     DBRRB4        SCDBRR
      *** END ADD **** 12/31/01 ************************************************
      *** BEG CHG **** 12/31/01 ************************************************
     C*********  *IN23     WHEQ *ON
     C     *IN24         WHENEQ    *ON
      *** END CHG **** 12/31/01 ************************************************
     C     *IN32         ANDEQ     *ON
     C     AAKYC         CHAIN     DBRCDB3                            60
     C                   Z-ADD     DBRRB3        SCDBRR
     C                   ENDSL
     C*
     C                   MOVEL(P)  AANAME        V1NAME
     C                   MOVEL(P)  AANAME        V2NAME
     C                   MOVEL(P)  AANAME        V3NAME
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVEL(P)  AAFDNM        V4FDNM
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   MOVEL     AAFEIN        V3FEIN
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C*
     C                   MOVEL     AAFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1FND
     C                   MOVEL     XXNUM         V2FND
     C                   MOVEL     XXNUM         V3FND
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVEL     XXNUM         V4FND
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   MOVEL     AAAGT#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1AGT#
     C                   MOVEL     XXNUM         V2AGT#
     C                   MOVEL     XXNUM         V3AGT#
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVEL     XXNUM         V4AGT#
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   MOVEL     AAMOD         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V3MOD
     C*
     C                   Z-ADD     AACNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXNUM
     C*
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1CNDT
     C                   MOVEL     XXNUM         V2CNDT
     C                   MOVEL     XXNUM         V3CNDT
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVEL     XXNUM         V4CNDT
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   CLEAR                   PHN
     C                   CLEAR                   AREA
     C                   CLEAR                   PH
     C*
B01  C     AGPHN1        IFNE      *ZEROS
     C*
B01  C     AGARE1        IFNE      *ZEROS
     C                   MOVEL     AGARE1        AREA              3
E01  C                   ENDIF                                                  AGARE1<>*ZEROS
     C*
     C                   MOVEL     AGPHN1        PHN
     C                   MOVEA     '(   )'       PH(1)
     C                   MOVEA     AREA          PH(2)
     C                   MOVEA     PHN1          PH(7)
     C                   MOVEA     '-'           PH(10)
     C                   MOVEA     PHN2          PH(11)
     C*
E01  C                   ENDIF                                                  AGPHN1<>*ZEROS
     C*
      ***** BEG ADD ***** 11/28/06 ************************************
     C*
     C*  LOAD MAILING ADDRESS
     C*
     C                   MOVEL     'Mail:'       SCMAIL
     C*
     C     AGMAD1        IFEQ      *BLANKS
     C                   MOVEL(P)  NOMADR        SCMADR
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  AGMAD1        SCMADR
     C                   CAT       ',':0         SCMADR
     C                   CAT       AGMCTY:1      SCMADR
     C                   CAT       ',':0         SCMADR
     C                   CAT       AGMST:1       SCMADR
     C                   MOVEL(P)  AGMZIP        WRKA05            5
     C                   MOVE(P)   AGMZIP        WRKA04            4
     C                   CAT       WRKA05:1      SCMADR
     C     WRKA04        IFGT      *ZEROS
     C                   CAT       WRKA04:0      SCMADR
     C                   ENDIF
     C*
     C                   ENDIF                                                  NOMADR
     C*
     C*  LOAD PHYSICAL ADDRESS
      *
     C                   MOVEL     'Phys:'       SCPHYS
     C*
     C     AGPAD1        IFEQ      *BLANKS
     C                   MOVEL(P)  NOPADR        SCPADR
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  AGPAD1        SCPADR
     C                   CAT       ',':0         SCPADR
     C                   CAT       AGPCTY:1      SCPADR
     C                   CAT       ',':0         SCPADR
     C                   CAT       AGPST:1       SCPADR
     C                   MOVEL(P)  AGPZIP        WRKA05            5
     C                   MOVE(P)   AGPZIP        WRKA04            4
     C                   CAT       WRKA05:1      SCPADR
     C     WRKA04        IFGT      *ZEROS
     C                   CAT       WRKA04:0      SCPADR
     C                   ENDIF
     C*
     C                   ENDIF                                                  NOPADR
     C*
      ***** END ADD ***** 11/28/06 ************************************
     C*
     C                   MOVEA     PH            V1PHN#
     C                   MOVEA     PH            V2PHN#
     C                   MOVEA     PH            V3PHN#
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVEA     PH            V4PHN#
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   MOVE      AASTAT        V1GSTS
     C                   MOVE      AASTAT        V2GSTS
     C                   MOVE      AASTAT        V3GSTS
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVE      AASTAT        V4GSTS
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   MOVE      AGSTAT        V1ASTS
     C                   MOVE      AGSTAT        V2ASTS
     C                   MOVE      AGSTAT        V3ASTS
      *** BEG ADD **** 12/31/01 ************************************************
     C                   MOVE      AGSTAT        V4ASTS
      *** END ADD **** 12/31/01 ************************************************
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  VIEW1         SCDATA
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  VIEW2         SCDATA
      *** BEG ADD **** 12/31/01 ************************************************
     C     *IN23         WHENEQ    *ON
     C                   MOVEL(P)  VIEW4         SCDATA
      *** END ADD **** 12/31/01 ************************************************
      *** BEG CHG *** 12/31/01 *************************************************
     C*********  *IN23     WHEQ *ON
     C     *IN24         WHENEQ    *ON
      *** END CHG *** 12/31/01 *************************************************
     C                   MOVEL(P)  VIEW3         SCDATA
     C                   ENDSL
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT7 - SUBROUTINE FOR OPTION 7
     C*===============================================================
     C*
     C* OPTION 7=WORK WITH AGENT ASSIGNMENTS
     C*
     C     OPT7          BEGSR
     C*
     C  N09              MOVE      ' '           ##CALL
     C   09              MOVE      '1'           ##CALL
     C*
     C                   CALL      'W0041R'                             99
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      SCAGRR        ##RRN             9 0
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   PARM      *BLANK        ##DSPL            1
     C                   PARM                    ##QTE             1
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C                   ENDSR                                                  OPT7
     C*
     C*===============================================================
     C* OPT8 - SUBROUTINE FOR OPTION 8
     C*===============================================================
     C*
     C* OPTION 8=WORK WITH AGENT PRODUCERS
     C*
     C     OPT8          BEGSR
     C*
     C  N09              MOVE      ' '           ##CALL
     C   09              MOVE      '1'           ##CALL
     C*
     C                   CALL      'W0042R'                             99
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      SCAGRR        ##RRN             9 0
     C*
     C                   ENDSR                                                  OPT8
     C*
     C*===============================================================
     C* OPT10 - SUBROUTINE FOR OPTION 10
     C*===============================================================
     C*
     C* OPTION 10=CHANGE FEIN
     C*
     C     OPT10         BEGSR
     C*
     C                   CALL      'W4020R'                             99
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##MODE            1
     C                   PARM      SCAGRR        ##RRN             9 0
     C*
     C                   ENDSR                                                  OPT10
     C*
     C***  BEGIN ADD  ***  01/18/06  *********************************
     C*===============================================================
     C* OPT13 - SUBROUTINE FOR OPTION 13
     C*===============================================================
     C*
     C* OPTION 13=Contacts
     C*
     C     OPT13         BEGSR
      *** BEG ADD **** 09/15/10 ************************************************
     C                   MOVE      *BLANKS       ##MODE            1
     C     @@CALL        IFEQ      '1'
     C                   MOVE      '1'           ##MODE
     C                   ENDIF
      *** END ADD **** 09/15/10 ************************************************
      /Free
       //*** Begin Add *** 10/24/18 ********************************************
       If UPcontUpd = 'Y';
          ##Mode = *blanks;
       Endif;
       //*** End   Add *** 10/24/18 ********************************************

      /End-Free
     C*
     C                   CALL      'W0277R'                             99
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
      *** BEG CHG **** 09/15/10 ************************************************
     C*                    PARM *BLANKS   ##MODE  1
     C                   PARM                    ##MODE
      *** END CHG **** 09/15/10 ************************************************
     C                   PARM      ##CO#         ##CO              3 0
     C                   PARM      SCFEIN        ##FEIN            9 0
     C                   PARM      SCMOD         ##MOD             5 0
     C*
     C*
     C                   ENDSR                                                  OPT13
     C***  END   ADD  ***  01/18/06  *********************************
     C*
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C*===============================================================
     C* OPT14 - SUBROUTINE FOR OPTION 14
     C*===============================================================
     C*
     C* OPTION 14=DARTS
     C*
     C     OPT14         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     SCFEIN        QCFEIN
     C                   Z-ADD     SCMOD         QCMOD
     C*
     C*  CALL COMMENT PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   ENDSR                                                  OPT14
     C***  END   ADD  ***  02/04/98  *********************************
     C*** Begin Add *** 01/24/15 ********************************************
     C*===============================================================
     C* OPT15 - Visit Tracking
     C*===============================================================
     C     OPT15         BEGSR
     C*
     C                   Eval      #@rrn = %EditC(scAgRr:'X')
     C*
     C                   CALL      'WTVSTTRK'                           99
     C                   PARM                    #@Rrn
     C                   PARM      'C'           ##mode
     C                   PARM      'Agency'      ##Category
     C                   PARM      ' '           ##Assign
     C                   PARM      ' '           ##VstFrDt
     C                   PARM      ' '           ##VstToDt
     C*
     C                   ENDSR                                                  OPT15
     C*** End   Add *** 01/24/15 ********************************************
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*
      *** BEG ADD ***** 07/18/07 *************************************
     C*===============================================================
     C* OPT36 - SUBROUTINE FOR OPTION 36
     C*===============================================================
     C     OPT36         BEGSR
     C                   CALL      'W0505R'
     C                   PARM      'A'           ##CALL
     C                   PARM      SCDBRR        ##RRN
     C                   ENDSR                                                  OPT36
      *** END ADD ***** 07/18/07 *************************************
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C*===============================================================
     C* OPT70 - SUBROUTINE FOR OPTION 70
     C*===============================================================
     C*
     C* OPTION 70=EFT ACCOUNT INFO
     C*
     C     OPT70         BEGSR
     C*
     C                   CALL      'W0028R'      EFTPL                  99
     C*
     C                   ENDSR                                                  OPT70
     C***  END   ADD  ***  09/20/07  *********************************
     c***  Begin Add  ***  11/27/18  *************************************
    C*================================================================
    C* Opt75 - Process other function keys
    C*================================================================
      /Free
       Begsr Opt75;
        Select;
        When @@call = ' ';
           ##Mode = 'C';
        Other;
           ##Mode = 'D';
        Endsl;
        Reset dsWITMPLTparms;
        dsWITMPLTparms.co# = ##co#;
        dsWITMPLTparms.fein = scFein;
        dsWITMPLTparms.mod = scMod;
        TemplateAssociations(dsWITMPLTparms:'Agency':##Mode);
       Endsr;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '10'          OPT10
     C***  BEGIN ADD  ***  01/18/06  *********************************
     C     SCSEL         CASEQ     '13'          OPT13
     C***  END   ADD  ***  01/18/06  *********************************
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C     SCSEL         CASEQ     '14'          OPT14
     C***  END   ADD  ***  02/04/98  *********************************
     C*** Begin Add *** 01/24/15 ********************************************
     C     SCSEL         CASEQ     '15'          OPT15
     C*** End   Add *** 01/24/15 ********************************************
     C     SCSEL         CASEQ     '20'          OPT20
     C*** BEG ADD ***** 07/18/07 *************************************
     C     SCSEL         CASEQ     '36'          OPT36
     C*** END ADD ***** 07/18/07 *************************************
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C     SCSEL         CASEQ     '70'          OPT70
     C***  END   ADD  ***  09/20/07  *********************************
     c***  Begin Add  ***  11/27/18  *************************************
     c     scsel         caseq     '75'          opt75
     c***  End   Add  ***  11/27/18  *************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
      **** begin add **** 01/08/24 ******************************************
      * Pass-through this program, when called for a Search screen
     C                   IF        @@CALL = '2'
     C                   CALL      'WTAGTCFG'
     C                   PARM      *Blanks       @@RRN1            9
     C                   PARM      'S'           @@MODE1           1
     C                   PARM      'GetAgency'   @@NEXT1          10
     C                   PARM      *Blanks       @@TABIND1         5
     C
     C                   MONITOR
     C                   EVAL      @@RRN = %INT(@@RRN1)
     C                   ON-ERROR
     c                   EVAL      @@RRN = *zeros
     C                   ENDMON
     C                   RETURN
     C                   ENDIF
      **** end add **** 01/08/24 ********************************************
     C                   SETOFF                                       3132
     C                   CLEAR                   SSFND
     C*
     C***  Begin Add  ***  01/28/08  **************************************
     C                   MOVE      *ON           RTRNAG            1
     C***  End   Add  ***  01/28/08  **************************************
     C*
      ***** BEG ADD ***** 09/30/09 ************************************
     C*
     C     Q1PRMS        IFGT      6
     C     @@MODE        ANDEQ     '*'
      *
     C     @@RRN         IFNE      *ZERO
      *
     C     @@RRN         CHAIN     PAH01                              80
      *
     C                   MOVE      AHFND         XXFND#            3 0
     C                   MOVE      AHAGT#        XXAGT#            5 0
     C                   MOVE      AHAGRC        XXAOR#            5 0
     C                   MOVE      AHSAGT        XXSAGT            5 0
     C                   MOVE      AH2ARC        XXSARC            5 0
      *
     C                   MOVE      AHFND         KKFND
     C                   Z-ADD     AHFND         SSFND
     C                   Z-ADD     AHFND         SCHGRP
      *
     C                   CLEAR                   @@RRN
     C                   SETON                                        3132
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  Q1PRMS > 6
      *
      ***** END ADD ***** 09/30/09 ************************************
     C     Q1PRMS        IFGE      4
     C*
     C     @@RRN         IFNE      *ZERO
     C*
     C     @@RRN         CHAIN     WMFNP                              60
     C*
     C                   Z-ADD     FNCO#         ##CO#
     C                   Z-ADD     FNFND         SSFND
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNFND         SCHGRP
     C                   MOVEL(P)  FNNAME        SCGNAM
     C                   SETON                                        3132
     C***  Begin Add  ***  01/28/08  **************************************
     C                   MOVE      *OFF          RTRNAG            1
     C***  End   Add  ***  01/28/08  **************************************
     C*
     C                   ENDIF                                                  @@RRN<>*ZERO
     C*
     C                   CLEAR                   @@RRN
     C*
     C                   ENDIF                                                  Q1PRMS>=4
     C*
B01  C     ##XFND        IFEQ      'Y'
     C*
      *** Beg Add ***** 12/26/14 ************************************
     c                   Eval      savFnd = ##FND
      *** End Add ***** 12/26/14 ************************************
      *** Beg Add *** 09/19/13 ****************************
     C                   Eval      count = 0
     C                   Eval      *IN33 = *Off
     C     *Start        Setll     WMFNP
     C                   Read      WMFNP
     C                   Dow       Not %Eof(WMFNP)
      *** End Add *** 09/19/13 ****************************
      *** BEG ADD *** 08/21/06 ****************************
      *
      * Check for existence of Fund/Emplr restriction records for this
      *   user.  SMAZP
      *
     C                   CALL      'SA008R'
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
      *** Beg Chg ***** 12/26/14 ************************************
     C***********        PARM      *OFF          ##RTCD            1
     C                   PARM      *ON           ##RTCD            1
      *** End Chg ***** 12/26/14 ************************************
     C                   PARM      *BLANK        ##AUTH            1
      *** Beg Chg *** 09/19/13 ****************************
     C************       PARM                    ##FND
     C                   PARM      FNfnd         ##FND
      *** End Chg *** 09/19/13 ****************************
      *
      *** Beg Del *** 09/19/13 ****************************
     C************     ##RTCD        IFEQ      *OFF
      *
     C************       SETOFF                                       33
     C************       ELSE
      *** END ADD *** 08/21/06 ****************************
      *** BEG ADD *** 12/18/06 ****************************
     C************       SETON                                        33
     C************       ENDIF                                                  ##RTCD
      *** END ADD *** 12/18/06 ****************************
      *** End Del *** 09/19/13 ****************************
      *** Beg Add *** 09/19/13 ****************************
     C     ##RTCD        Ifeq      *Off
     C                   Eval      count = count + 1
     C                   Eval      fndary(count) = FNfnd
     C                   Endif
     C
     C                   Read      WMFNP
      *
     C                   Enddo
      *
      *** Beg Add ***** 12/26/14 ************************************
      /Free
       If count = 0;
          count = 1;
          fndary(count) = savFnd;
       Endif;
      /End-Free
      *** End Add ***** 12/26/14 ************************************
     C                   If        count = 1
     C                   Eval      *IN33 = *On
      *** Beg Add ***** 12/26/14 ************************************
      /Free
       ##FND = fndary(count);
      /End-Free
      *** End Add ***** 12/26/14 ************************************
      *** End Add *** 09/19/13 ****************************
     C     FNKY          CHAIN     WMFNL                              60
     C*
     C                   Z-ADD     FNFND         SSFND
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNFND         SCHGRP
     C                   MOVEL(P)  FNNAME        SCGNAM
     C                   SETON                                        3132
     C*
      *** BEG DEL *** 12/18/06 ****************************
     C*******              ENDIF                           ##RTCD
      *** END ADD *** 08/21/06 ****************************
      *** END DEL *** 12/18/06 ****************************
      *** Beg Add *** 09/19/13 ****************************
     C                   Endif                                                  count=1
      *** End Add *** 09/19/13 ****************************
E01  C                   ENDIF                                                  ##XFND=Y
     C*
     C     Q1PRMS        IFGE      5
     C     @@VIEW        IFNE      *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C                   ENDIF                                                  @@VIEW<>*ZERO
     C                   ENDIF                                                  Q1PRMS>=5
     C*
      ***** BEG ADD ******* 03/17/00 **********************************
      *
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@QTE         ##QTE
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        ##QTE
     C                   ENDIF                                                  Q1PRMS>=5
      *
      ***** END ADD ******* 03/17/00 **********************************
      *
      *
      ***** BEG ADD ***** 11/28/06 ************************************
     C                   MOVE      *ON           *IN12
      ***** END ADD ***** 11/28/06 ************************************
     C***  BEGIN ADD  ***  09/20/07  *********************************
     C     'WC'          CHAIN     SWC01                              60
     C*
     C***  BEG   CHG  ***  04/01/08  *********************************
     C***  BEG   CHG  ***  03/19/08  *********************************
     C*********  WCFL17    IFNE 'Y'
     C*********  WCFL38    IFNE 'Y'
     C     WCFL17        IFNE      'Y'
     C***  END   CHG  ***  03/19/08  *********************************
     C***  END   CHG  ***  04/01/08  *********************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '70'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C***  BEG   CHG  ***  04/01/08  *********************************
     C***  BEG   CHG  ***  03/19/08  *********************************
     C                   ENDIF                                                  WCFL17<>Y
     C**********           ENDIF                           WCFL38<>Y
     C***  END   CHG  ***  03/19/08  *********************************
     C***  END   CHG  ***  04/01/08  *********************************
     C***  END   ADD  ***  09/20/07  *********************************
     c***  Begin Add  ***  11/27/18  *************************************
     c* Opt 75 is only allowed if module installed
      /Free
       If isModEnabled('Portal Module');
          xxOpFn = '1';
          xxNmbr = '75';
          xxMIS  = 'YYN';
          Exsr UpdOfA;
       Endif;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************
     C*
     C***  BEG   ADD  ***  09/30/09  *********************************
     C     Q1PRMS        IFGT      6
     C     @@MODE        ANDEQ     '*'
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '17'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '01'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
      *
     C                   ELSE
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '17'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '01'          XXNMBR
     C                   MOVE      'NNY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C***  END   ADD  ***  09/30/09  *********************************
      /Free
       //***  Beg   Add  ***  10/24/18  *********************************
       chain Q1USER SMUPL;
       If %found(SMUPL) and UPcontUpd = 'Y';
       Else;
          UPcontUPd = *blanks;
       Endif;
       //***  End   Add  ***  10/24/18  *********************************
      /End-Free

     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C                   select
     C                   when      *in21 = *on And *In32 = *off
     C     KEY1          READE     DBRCDF                                 95
     C                   when      *in22 = *on And *In32 = *off
     C     KEY1          READE     DBRCDF2                                95
     C                   when      *in23 = *on And *In32 = *off
     C     KEY1          READE     DBRCDF3                                95
     C                   when      *in24 = *on And *In32 = *off
     C     KEY1          READE     DBRCDF4                                95
     c                   when      *in21 = *on And *In32 = *on
     C     KEY2          READE     DBRCDB                                 95
     C  n95              eval      dbrrn = DBRRB
     c                   when      *in22 = *on And *In32 = *on
     C     KEY2          READE     DBRCDF2                                95
      *** BEG CHG **** 01/29/13 ************************************************
     c*********                   when      *in23 = *on And *In32 = *on
     C*********     KEY2          READE     DBRCDB3                                95
     C*********  n95              eval      dbrrn = DBRRB3
     c*********                   when      *in24 = *on And *In32 = *on
     C*********     KEY2          READE     DBRCDB4                                95
     C*********  n95              eval      dbrrn = DBRRB4
     c                   when      *in23 = *on And *In32 = *on
     C     KEY2          READE     DBRCDB4                                95
     C  n95              eval      dbrrn3 = DBRRB4
     c                   when      *in24 = *on And *In32 = *on
     C     KEY2          READE     DBRCDB3                                95
     C  n95              eval      dbrrn4 = DBRRB3
      *** END CHG **** 01/29/13 ************************************************
     C                   endsl
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C                   select
     C                   when      *in21 and not *in32
     C     KEY1          READPE    DBRCDF                                 95
     C                   when      *in22 and not *in32
     C     KEY1          READPE    DBRCDF2                                95
     C                   when      *in23 and not *in32
     C     KEY1          READPE    DBRCDF3                                95
     C                   when      *in24 and not *in32
     C     KEY1          READPE    DBRCDF4                                95
     C                   when      *in21 and *in32
     C     KEY2          READPE    DBRCDB                                 95
     C  n95              eval      dbrrn = dbrrb
     C                   when      *in22 and *in32
     C     KEY2          READPE    DBRCDF2                                95
      *** BEG CHG **** 01/29/13 ************************************************
     C**********                   when      *in23 and *in32
     C**********     KEY2          READPE    DBRCDB3                                95
     C**********  n95              eval      dbrrn = DBRRB3
     C**********                   when      *in24 and *in32
     C**********     KEY2          READPE    DBRCDB4                                95
     C**********  n95              eval      dbrrn = DBRRB4
     C                   when      *in23 and *in32
     C     KEY2          READPE    DBRCDB4                                95
     C  n95              eval      dbrrn3 = DBRRB4
     C                   when      *in24 and *in32
     C     KEY2          READPE    DBRCDB3                                95
     C  n95              eval      dbrrn4 = DBRRB3
      *** END CHG **** 01/29/13 ************************************************
     C                   endsl
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21
     CANN32KEY1A         SETGT     DBRCDF                             96
      *** BEG CHG **** 12/31/01 ************************************************
     C***************** 23N32   KEY1C     SETGTDBRCDF3              96
     C   23
     CANN32KEY1D         SETGT     DBRCDF3                            96
     C   24
     CANN32KEY1C         SETGT     DBRCDF4                            96
      *** END CHG **** 12/31/01 ************************************************
     C*
     C   21
     CAN 32KEY2A         SETGT     DBRCDB                             96
     C   22KEY2B         SETGT     DBRCDF2                            96
      *** BEG CHG **** 12/31/01 ************************************************
     C******************   23 32   KEY2C     SETGTDBRCDB3              96
     C   23
     CAN 32KEY2D         SETGT     DBRCDB4                            96
     C   24
     CAN 32KEY2C         SETGT     DBRCDB3                            96
      *** END CHG **** 12/31/01 ************************************************
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21
     CANN32KEY1A         SETLL     DBRCDF                             96
      *** BEG CHG **** 12/31/01 ************************************************
     C******************   23N32   KEY1C     SETLLDBRCDF3              96
     C   23
     CANN32KEY1D         SETLL     DBRCDF3                            96
     C   24
     CANN32KEY1C         SETLL     DBRCDF4                            96
      *** END CHG **** 12/31/01 ************************************************
     C*
     C   21
     CAN 32KEY2A         SETLL     DBRCDB                             96
     C   22KEY2B         SETLL     DBRCDF2                            96
      *** BEG CHG **** 12/31/01 ************************************************
     C*******************  23 32   KEY2C     SETLLDBRCDB3              96
     C   23
     CAN 32KEY2D         SETLL     DBRCDB4                            96
     C   24
     CAN 32KEY2C         SETLL     DBRCDB3                            96
      *** END CHG **** 12/31/01 ************************************************
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C***  Begin Chg  ***  01/28/08  **************************************
     C**N31*****           Z-ADDSCAGRR    @@RRN
     C***31*****           Z-ADDSCDBRR    @@RRN
     C                   SELECT
     C     RTRNAG        WHENEQ    *ON
     C                   Z-ADD     SCAGRR        @@RRN
     C                   OTHER
     C                   Z-ADD     SCDBRR        @@RRN
     C                   ENDSL
     C***  End   Chg  ***  01/28/08  **************************************
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
      *** Beg Add *** 09/19/13 ****************************
     C                   If        ##xfnd = 'Y'
     C     AAfnd         LookUp    fndary                                 89
     C                   If        *IN89 = *Off
     C                   Eval      Meetss = *Off
     C                   Endif
     C                   Endif
      *** End Add *** 09/19/13 ****************************
     C
     C     AGKY          CHAIN     WMAGL                              60
     C*
     C     SSFND         IFNE      *ZERO
     C     SSFND         ANDNE     AAFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND<>*ZERO
     C*
     C*  CHECK AGENT NAME AGAINST SEARCH STRING
     C*
B01  C     SSNAME        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  AGNAME        U1PDSC
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSNAME<>*BLANKS
     C*
     C     SSAGT#        IFNE      *ZERO
     C     SSAGT#        ANDNE     AAAGT#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSAGT#<>*ZERO
     C*
     C*** BEG ADD ***** 10/09/03 ************************************
     C     SSPNAM        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  SSPNAM        U2PDSC
     C                   MOVE      *OFF          XXMEET            1
     C*
     C*** BEG CHG ***** 01/02/04 ************************************
     C********** ##CO#     SETLLPAP01
     C**********           READ PAP01                    60
     C     AGKY          CHAIN     PAP02                              60
     C*** END CHG ***** 01/02/04 ************************************
     C     *IN60         DOWEQ     *OFF
     C     LO:UP         XLATE(P)  APLSN         U1PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      32            STRLEN
     C                   PARM      1             START
     C                   PARM                    U2PDSC
     C                   PARM      32            PATLEN
     C                   PARM      '0'           TRANS
     C                   PARM      '1'           TRIM
     C                   PARM      ' '           WILD
     C                   PARM                    SCNRES
     C*
     C     SCNRES        IFNE      *ZERO
     C     APFEIN        ANDEQ     AAFEIN
     C     APMOD         ANDEQ     AAMOD
     C                   MOVE      *ON           XXMEET
     C                   LEAVE
     C                   ENDIF
     C*
     C*** BEG CHG ***** 01/02/04 ************************************
     C***************      READ PAP01                    60
     C     AGKY          READE     PAP02                                  60
     C*** END CHG ***** 01/02/04 ************************************
     C                   ENDDO
     C*
     C     XXMEET        IFNE      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF                                                  SSPNAM <> *BLANKS
     C*
     C     SSPROD        IFNE      *ZERO
     C*
     C                   Z-ADD     SSPROD        KKPROD
     C                   MOVE      *OFF          XXMEET
     C     APKY          CHAIN     PAP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     AAFEIN        IFEQ      APFEIN
     C     AAMOD         ANDEQ     APMOD
     C                   MOVE      *ON           XXMEET
     C                   LEAVE
     C                   ENDIF                                                  AAFEIN = *OFF
     C*
     C     APKY          READE     PAP01                                  60
     C                   ENDDO                                                  *IN60  = *OFF
     C*
     C     XXMEET        IFNE      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF                                                  SSPROD <> *ZERO
     C*** BEG ADD ***** 10/09/03 ************************************
     C*
     C     SSASTS        IFNE      *BLANK
     C     SSASTS        ANDNE     AGSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSASTS<>*BLANK
     C*
     C     SSGSTS        IFNE      *BLANK
     C***  BEGIN CHG  ***  06/25/02  *********************************
     C***********SSGSTS****ANDNEAGSTAT
     C     SSGSTS        ANDNE     AASTAT
     C***  END CHG ******  06/25/02  *********************************
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSGSTS<>*BLANK
     C*
     C*** BEG ADD ***** 09/30/09 ************************************
     C     Q1PRMS        IFGT      6
      *
     C     @@MODE        IFEQ      '*'
     C                   MOVE      *OFF          MEETSS
      *
     C                   SELECT
      *
     C     XXAGT#        WHENNE    0
     C     XXAGT#        ANDEQ     AAAGT#
     C     XXFND#        ANDEQ     AAFND
     C                   MOVE      *ON           MEETSS
      *
     C     XXAOR#        WHENNE    0
     C     XXAOR#        ANDEQ     AAAGT#
     C     XXFND#        ANDEQ     AAFND
     C                   MOVE      *ON           MEETSS
      *
     C     XXSAGT        WHENNE    0
     C     XXSAGT        ANDEQ     AAAGT#
     C     XXFND#        ANDEQ     AAFND
     C                   MOVE      *ON           MEETSS
      *
     C     XXSARC        WHENNE    0
     C     XXSARC        ANDEQ     AAAGT#
     C     XXFND#        ANDEQ     AAFND
     C                   MOVE      *ON           MEETSS
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
     C                   ENDIF
     C*** END CHG ***** 09/30/09  *********************************
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt Agent DBA name                 Sts Phone          Grp Agent Sts  Cancel Dt
Opt Grp Agent Name                           Sts Phone          Sts  Cancel Dt
Opt Agent Federal Name             Sts Phone          Grp Agent Sts  Cancel Dt
Opt  Fed Id # Mod Name             Sts Phone          Grp Agent Sts  Cancel Dt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1077=Work with Assignments  YYN
1088=Work with Producers    YYN
11010=Change Fed Id #       YNN
11313=Contacts              YYN
11414=DARTS                 YYN
11515=Service Request Track YYN
12020=Change Status         YNN
13636=Return Funds          YYN
17070=EFT Account info      YYN
17575=Template Associations NNN                            11/27/18
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
