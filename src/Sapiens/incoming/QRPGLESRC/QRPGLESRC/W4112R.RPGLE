     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* ***************************************************************
     H/TITLE W4112R - PRINT CLAIM DEPT LETTER & MAINTAIN DARTS
     H* ***************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*
     H* This program reads file WDCLP created in the prior step.  It  *
     H* then accesses other files to assign Fund, Employee, Claim     *
     H* data to the WDCLP record and updates the file prior to the    *
     H* PRTDOC command.  The program also writes a DARTS remark       *
     H* which points to the Claim letter that was generated.          *
     H* Also prior to the PRTDOC command, the document ID is checked  *
     H* to determine if an overlay is necessary and if so, an         *
     H* OVRPRTF command is executed.                                  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4111C                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     *NONE                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:30                                             *
     H*                                                               *
     H*  CREATED:    11/28/94               PROJECT: WC376            *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     h*                                                               *
     h* 04/10/19 C:062 Project: 152WAB                                *
     h*                Apply CSI mods for S1 Medical.                 *
     H*                                                               *
     H* 12/15/11 C:251 Support: 78660                                 *
     H*                Pass ' ' as MDOTYP to SMMDP file               *
     H*                                                               *
     H* 10/04/11 C:251 Project: 114IAH                                *
     H*                Append 'Denied Bill' to DODESC for 114         *
     H*                and add letter to same dart for denied bill    *
     H*                                                               *
     H* 09/14/09 C:062 SUPPORT 69870                                  *
     H*                PASS FOLDER AND DCID TO W4112C1                *
     H*                                                               *
     H* 04/20/09 C:190 Project: WC1473                                *
     H*                Use the new logical(s) to get the              *
     H*                description by extension.                      *
     H*                                                               *
     H* 11/04/08 C:180 SUPPORT 64052 - Write DARTS records correctly.
     H*                                                               *
     H* 07/17/08 C:191 Project: 81WZT                                 *
     H*               Updated NC claims forms                         *
     H*                                                               *
     H* 05/22/08 C:190 Project: 133WET                                *
     H*               The resolved document is not being saved with   *
     H*               a ".PDF" extension, even though it is a PDF     *
     H*               PDF document.                                   *
     H*                                                               *
     H* 01/14/08 C:190 Project: 133WET                                *
     H*                Changed the program to call the Claim Letter   *
     H*                RPG API for forms that use an RPG print prog-  *
     H*                ram instead of a DTM document.                 *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*
     H*  5/22/06  C:02   PROJECT: XXXXXX                              *
     H*                  ALLOW MSWORD EDTDTMDOC OR DSPDTMDO.         *
     H*                                                               *
     H* 03/07/05  C:65   PROJECT: WC1202                              *
     H*                  POPULATE EXTENDED EMPLR NAME FIELDS.         *
     H*                                                               *
     H* 12/07/04 C:53  SUPPORT: WC129 44915                           *
     H*                USE CL1A1 FOR PHY/MAIL ADDRESS FLAG            *
     H*                                                               *
     H* 01/30/04 C:136 Project: SY059                                 *
     H*                Replace SYCHGDOC with SYCHGDA for DTM.         *
     H*                                                               *
     H* 01/23/04  C:35   PROJECT: SY059                               *
     H*                  DTM REQUIRES JOBQ(*NO) ON SYPRTDOC COMMAND.  *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 11/18/02 C:61  PROJECT WC1088                                 *
     H*                Add user email addresses to department letter  *
     H*                work files.                                    *
     H*                                                               *
     H* 09/06/02 C:53  PROJECT WC577                                  *
     H*                Use CLDT20 for the system date.                *
     H*                                                               *
     H* 08/20/02 C:53  PROJECT WC577                                  *
     H*                Replaced license# w/license# file.             *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
     H*                                                               *
     H* 09/21/01 C:15  Support: WC048/29013                           *
     H*                Removed reference to CMRECL to prevent decimal *
     H*                data error.                                    *
     H*                                                               *
     H* 03/05/01 C:69  Project: SY033
     H*                Change OV commands to use external APIs
     H*                                                               *
     H* 02/05/01 C:53  PROJECT: WC577
     H*                Add code to support forms w/more than 1 page.
     H*                                                               *
     H* 08/15/00 C:53  PROJECT: WC577
     H*                Swap out wtscl for the generic table file
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 04/03/00 C:118 PROJECT: WC854                                 *
     H*                Load WMCMP/CMIMP to WDCLP/CLIMP.               *
     H*                                                               *
     H* 01/19/00 C:15 Support: WC048/20018                            *
     H*               Changed code which retrieves the next available *
     H*               DARTS seq# to be processed immediately before   *
     H*               the WRITE statement.                            *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  04/02/99 C:90 PROJECT: WC889                                 *
     H*                IF DBA FLAG = 'Y' LOAD PDADR2, ELSE LOAD PDADR1*
     H*                                                               *
     H*  03/25/99 C:113  PROJECT: SUPPORT 17322                       *
     H*                  Changed to NOT write DARTS record if the     *
     H*                  remark code is blank.                        *
     H*                                                               *
     H*  03/24/99  C:09  PROJECT: WC577                               *
     H*                  SINCE CHANGE BELOW WAS MADE, WE NEED TO      *
     H*                  ENSURE THE SUPERVISOR NAME (CLSPNM) IS NOW   *
     H*                  BEING LOADED PROPERLY AGAIN.                 *
     H*                                                               *
     H*  09/17/98  C:09  PROJECT: WC577                               *
     H*                  LOAD USER NAME OF LETTER REQUESTOR (NEW)     *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  DISPLAY EXTENSION DESCRIPTION IF NOT BLANK.  *
     H*                                                               *
     H*  05/28/98 C:77 Project: WC577                                 *
     H*               -ADD PROCESSING TO SUPPORT OPTION OF WHETHER OR *
     H*                NOT TO ALLOW CHANGES TO DOCUMENTS RESOLVED TO  *
     H*                DARTS.                                         *
     H*               -"HARD DELETE" ALL REFERENCES TO AREA OF        *
     H*                SERVICE, AND SUBAREA.                          *
     H*                                                               *
     H*  05/27/98 C:09 Project: WC577                                 *
     H*                USE SYSTEM FOLDER BUILD INSTEAD OF CURRENT     *
     H*                AUTO FOLDER BUILD.                             *
     H*                                                               *
     H*  05/13/98 C:65 Project: DR048/11595                           *
     H*                USE THE REMARK SETTING FOR DIARY FLAG.         *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H* 10/18/96 C:62 PROJECT: WC587                                  *
     H*               ADDED RATE CODE TO FIND PRDESC                  *
     H*                                                               *
     H* 12/15/95 C:33  - PROJ: SUPT                                   **
     H*                  -MODIFICATION TO GET SEQUENCE NUMBER DIRECTLY*
     H*                   BEFORE WRITING DARTS REMARK                 *
     H*                  -ADDED FILE SDCMLI(CMKEY/CMSEQ# ONLY)        *
     H*                                                               *
     H* 11/09/95 C:11 PROJECT WC376                                   *
     H*               LOAD COMPANY 2ND ADDRESS. CONVERT ALL DATES TO  *
     H*               MMDDYY FORM BEFORE LOADING TO WORKFILE.         *
     H*                                                               *
     H* 08/15/95 C:11 PROJECT WC376                                   *
     H*               SPECIFY SELECTED OUTQ ON PRTDOC COMMAND.        *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H* ***************************************************************
     FWDCLL     UF   E           K DISK
     FWDC2L     UF A E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
     FWMREL     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     F*** BEG DEL *** 08/15/00 **********************************
     F***WTSCL   IF  E           K        DISK
     F*** END DEL *** 08/15/00 **********************************
     FWDELL1    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
      *  SYSTEM TABLE FILE
     FWTGTL     IF   E           K DISK
      *  OCCUPATION MASTER
     FWTOCL     IF   E           K DISK
      *  WORKERS COMP DEPARTMENT MASTER
     FWMPDL     IF   E           K DISK
     FWMTPL1    IF   E           K DISK
     FWMDPL     IF   E           K DISK
      *  LEGAL MASTER
     FWMLLL     IF   E           K DISK
     F*                                                                *********
     F*** BEGIN ADD *** 02/23/98 ***********************************************
     FSTCTL1    IF   E           K DISK
     F*** END   ADD *** 02/23/98 ***********************************************
     F*                                                                *********
     F*** BEGIN DEL *** 04/26/00 **************************************
     F********SDCML   O   E           K        DISK
     F********    PCM01                             KRENAMEPCM02
     F********SDCMLI  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM0I
     F*** END DEL *** 04/26/00 ****************************************
     FWMUPL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMCDL     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     F*** BEGIN ADD *** 05/27/98 ***********************************************
     FSSYLF     IF   E           K DISK
     F*** END ADD ***** 05/27/98 ***********************************************
     F*** BEGIN ADD ***  08/11/98  ************************************
      **** BEGIN CHG **** 10/06/99 ************************************
     F****WMPXL   IF  E           K        DISK
     FWMPXL1    IF   E           K DISK
      **** END   CHG **** 10/06/99 ************************************
     F*** BEG CHG *****  04/20/09  ************************************
     F****** WMEPL7  IF  E           K        DISK
     FWMEPL7E   IF   E           K DISK
      **** END   CHG **** 04/20/09 ************************************
     F*** END ADD *****  08/11/98  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     FWMRSL     IF   E           K DISK
     F                                     RENAME(PRS01:PRS02)
      **** END   ADD **** 10/06/99 ************************************
     F*** BEG ADD *** 08/20/02 **********************************
     FSMALL     IF   E           K DISK
     F*** END ADD *** 08/20/02 **********************************
      ***** BEG ADD ******* 11/18/02 **********************************
      *
      *  Producer
     FWMAPL     IF   E           K DISK
      ***** END ADD ******* 11/18/02 **********************************
     F*** BEG ADD *** 10/04/11 ****************************************
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM0A)
     F*** END ADD *** 10/04/11 ****************************************
      *

     D CMD             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
     D CM2             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
     D OVR             S              1    DIM(75) CTDATA PERRCD(75)            OVRPRTF COMMAND
*****D***ADD 5/22/06 **************************************************
     D CM3             S              1    DIM(225) CTDATA PERRCD(75)           QCMDEXC COMMAND
*****D* END ADD 5/22/06 **************************************************
     D FLR             S              1    DIM(80)                              FOLDER ARRAY
     D CNB             S              1    DIM(40) ASCEND                       COMPANY NAME W/ BLAN
     D CN              S              1    DIM(40)                              COMPANY NAME
     D FNB             S              1    DIM(40) ASCEND                       FUND NAME W/ BLANKS
     D FN              S              1    DIM(40)                              FUND NAME
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D ED              S              8  0 DIM(5) ASCEND                        EFFECT. DATE
     D CD              S              8  0 DIM(5) ASCEND                        CANCEL DATE
     D DEN             S             60    DIM(16)                              DENIAL INFO
     D DT              S              8  0 DIM(16)                              WL/TP DT RANGES
     D*** BEGIN DEL *** 04/26/00 **************************************
     D********PCM0I
     D********      CMKEY                           PIKEY
     D********      CMSEQ#                          PISEQ#
     D*** END DEL *** 04/26/00 ****************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  CX                     1     30
     D  XXCLEX                 1     30
     D                 DS
     D  XXSS#                  1      9
     D  CFSS1                  1      3
     D  CFSS2                  4      5
     D  CFSS3                  6      9
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  CM                     1    275
     D                                     DIM(275)                             DARTS COMMENTS
     D  XXCOM1                 1    256
     D  XXCOM2               257    275
     D                 DS
     D  AR                     1     75
     D                                     DIM(75)                              ADDT'L REMARKS
     D  ARCM75                 1     75
     D                 DS
     D  XXBLNK                 1      3
     D  XXDG1                  1      1
     D  XXDG2                  2      2
     D  XXDG3                  3      3
     D                 DS
     D  DC                     1     12
     D                                     DIM(12)                              CLDCID
     D  CLDCID                 1     12
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** BEG ADD ******* 01/31/02 **********************************
      *
     D*** BEG ADD *** 10/04/11 ****************************************
     D                 DS
     D  XXARDT                 1      8  0
     D  XXMM                   1      2
     D  XXDD                   3      4
     D  XXYY                   7      8
     D*
     D                 DS
     D  XXARTM                 1      4  0
     D  XXHR                   1      2
     D  XXMN                   3      4
     D*  DENIED BILL
     D XDNY            C                   CONST('- DENIED BILL')
     D XDNY1           C                   CONST('DENIED BILL- ')

     I*** END ADD *** 10/04/11 ****************************************
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *
      ***** BEG CHG ******* 11/18/02 **********************************

     C*********  ECKY      KLIST
     C*********            KFLD           EMCO#
     C*********            KFLD           EMFND
     C*********            KFLD           EMEMP#
     C*********            KFLD           EMDIV
     C*********            KFLD           KKPCNT  1
     C     ECKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLEMP#
     C                   KFLD                    CLDIV
     C                   KFLD                    KKPCNT            1
      ***** END CHG ******* 11/18/02 **********************************
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLAAGT
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLAFID
     C                   KFLD                    CLAMOD
      ***** BEG ADD ******* 11/18/02 **********************************
      *
      *  Producer
      *
     C     APKY          KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLAFID
     C                   KFLD                    CLAMOD
     C                   KFLD                    CLAPRO
      ***** END ADD ******* 11/18/02 **********************************
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    CLFYR
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM HISTORY MASTER FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C                   KFLD                    XXDATE
     C*
     C*  DEPT LETTERS WORKFILE - SCREEN
     C*
     C     CLKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKY1         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C     CMKY2         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C*
     C*  COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    CLCO#
     C*
     C*  KEY LIST FOR CHAPTER
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CLFND
     C                   KFLD                    EMCHAP
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********
     C***DARTS FILE
     C********
     C********   C3KY      KLIST
     C********             KFLD           QCKEY
     C********             KFLD           ##SEQ#  50
     C*** END DEL *** 04/26/00 ****************************************
     C*
     C*  DEPT/DOC MASTER FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    CLDPCD
     C                   KFLD                    CLFLDR
     C                   KFLD                    CLDCID
     C*
     C*  WORKERS COMP DEPARTMENT MASTER FILE
     C*
     C     DPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLEMP#
     C                   KFLD                    CLDIV
     C                   KFLD                    X1DEPT
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      ***** BEG ADD ******* 11/18/02 **********************************
      *
     C     EMKY2         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLEMP#
     C                   KFLD                    CLDIV
      ***** END ADD ******* 11/18/02 **********************************
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C*
     C*   FUND AUXILIARY FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE LEGAL MASTER
     C*
     C     LLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C*   KEY LIST FOR OCCUPATION CODE FILE
     C*
     C     OCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHIC FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  PAYROLL CLASS KEY LIST
     C*
     C     PRKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    CLCLAS
     C                   KFLD                    XXEFDT
     C*
     C     PRKEY1        KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    CLCLAS
     C*
     C*  KEY LIST FOR THE REGION
     C*
     C     REKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CLFND
     C                   KFLD                    EMRGN#
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  TREATING PHYSICIAN
     C*
     C     TPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C*  USER PARM FILE
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     C*** BEGIN ADD *** 05/27/98 ***********************************************
     C*
     C*  KEY LIST FOR SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    ##KEY             2
     C*** END ADD ***** 05/27/98 ***********************************************
      *** BEGIN ADD ***  08/11/98  ************************************
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   KFLD                    XXST
     C*** BEGIN ADD ***  06/01/06  ************************************
     C                   KFLD                    EPEFDT
     C     PXKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C*** END ADD *****  06/01/06  ************************************
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      **** END   ADD **** 10/06/99 ************************************
      *
      * PAYROLL FILE
      *
     C     EPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCLAS
     C                   KFLD                    CMSPEC
      *** BEG ADD *****  04/20/09  ************************************
     C                   KFLD                    CMEXT
      *** END ADD *****  04/20/09  ************************************
      *** END ADD *****  08/11/98  ************************************
     C*** BEG ADD *** 08/20/02 **********************************
      *
      * LISCENSE FILE
      *
     C     ALKY          KLIST
     C                   KFLD                    UPUSR
     C                   KFLD                    CMJRST
     C*** END ADD *** 08/20/02 **********************************
     C*** BEG ADD *** 10/04/11 ****************************************
     C     SDKY          KLIST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKSEQ#
     C*** END ADD *** 10/04/11 ****************************************
     C*
     C     *LIKE         DEFINE    CLFCLM        XXFCLM
     C     *LIKE         DEFINE    CMDT          XXEFDT
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CLDT          XXDATE
     C     *LIKE         DEFINE    CLCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         HLFND
     C     *LIKE         DEFINE    CLREQ#        HLREQ#
     C     *LIKE         DEFINE    DODEPT        XXDEPT
     C     *LIKE         DEFINE    CMDEPT        X1DEPT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    LLDEFA        XXFEIN
     C     *LIKE         DEFINE    LLDEFM        XXMOD
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLFYR         XXFYR
     C     *LIKE         DEFINE    CLFCLM        XXCASE
     C     *LIKE         DEFINE    CLDCID        SVDCID                         ADD 02/05/01
     C*** BEG CHG ****** 07/17/08 ************************************
     C     *LIKE         DEFINE    CLDCID        QQDCID                         ADD 02/05/01
     C*** END CHG ****** 07/17/08 ************************************
     C*** BEG ADD *** 10/04/11 ***************************************
     C     *LIKE         DEFINE    CMKEY         KKKEY
     C     *LIKE         DEFINE    CMSEQ#        KKSEQ#
     C*** END ADD *** 10/04/11 ***************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C*
     C                   MOVE      QQCO#         XXCO#
     C*
     C                   MOVE      QQDATE        XXDATE                                     D
     C                   Z-ADD     XXDATE        SRDTI                          MDCY --> CYMD
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             Z-ADD*HIVAL    ##SEQ#
     C*** END DEL *** 04/26/00 ****************************************
     C                   MOVE      'CS'          XXDEPT
     C*** BEG ADD ****** 07/17/08 ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLID            3
     C*** END ADD ****** 07/17/08 ************************************
     C*
     C*   READ THE WORK FILE
     C*
     C     CLKEY         CHAIN     PCL01                              60
     C                   Z-ADD     *ZEROS        HLREQ#
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CMKY1         CHAIN     PCM01                              65        CO/FND/FYR
     C     *IN65         IFEQ      '0'
     C*
     C                   Z-ADD     CMCASE        CLFCLM
     C*** BEG ADD *** 02/05/01 ****************************
     C*** SAVE ORIGINAL DOC ID
     C                   MOVE      CLDCID        SVDCID
     C*** END ADD *** 02/05/01 ****************************
     C*
     C*  CHECK TO SEE IF CLAIMANT'S EMPLOYER MATCHES THE EMPLOYER NUMBER
     C*  ENTERED FROM SCREEN ONE
     C*
     C     CLEMP#        IFEQ      *ZEROS
     C     CMEMP#        OREQ      CLEMP#
     C     CMDIV         ANDEQ     CLDIV
     C                   EXSR      WRITE
     C*** BEG CHG ****** 07/17/08 ************************************
     C     XXCLID        IFEQ      '081'
     C     CLDCID        ANDEQ     'NC21V4  '
     C     XXCLID        OREQ      '081'
     C     CLDCID        ANDEQ     'NC60V4  '
     C*
     C                   MOVEL(P)  CLFND         QQFND
     C                   MOVEL(P)  CLFYR         QQFYR
     C                   MOVEL(P)  CLFCLM        QQCASE
     C                   MOVEL(P)  CLDCID        QQDCID
     C                   MOVEL(P)  CLSFLD        QQSFLD
     C                   MOVEL(P)  CLSDOC        QQSDOC
     C                   Z-ADD     CL#CPY        @@#CPY
     C                   MOVEL(P)  CLOUTQ        QQOUTQ
     C                   CALL      'W4113C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQDCID
     C                   PARM                    @@#CPY            5 0
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQSFLD           40
     C                   PARM                    QQSDOC           12
     C                   ELSE
     C*** END CHG ****** 07/17/08 ************************************
     C*** BEG ADD ***** 01/14/08 **************************************
     C* CALL RPG API IF FOLDER NAME IS BLANK
     C     CLFLDR        IFEQ      ' '
     C                   MOVEL(P)  CLDCID        QQPGID
     C*** BEGIN ADD *** 09/14/09 **************************************
     C                   MOVEL(P)  CLSFLD        QQSFLD
     C                   MOVEL(P)  CLSDOC        QQSDOC
     C*** END   ADD *** 09/14/09 **************************************
     C                   Z-ADD     CL#CPY        @@#CPY
     C                   MOVEL(P)  CLOUTQ        QQOUTQ
     C                   CALL      'W4112C1'
     C                   PARM                    QQPGID           12
     C                   PARM                    QQSFLD           40
     C                   PARM                    QQSDOC           12
     C                   PARM                    @@#CPY            5 0
     C                   PARM                    QQOUTQ           10
     C                   PARM      ' '           QQRTCD            1
     C                   ELSE
     C*** END ADD ***** 01/14/08 **************************************
     C                   EXSR      PRTDOC
     C*** BEG ADD ***** 01/14/08 **************************************
     C                   ENDIF                                                  CLFFDR = ' '
     C*** END ADD ***** 01/14/08 **************************************
     C*** BEG CHG ****** 07/17/08 ************************************
     C                   ENDIF                                                  CLDCID='NC21V4'
     C*** END CHG ****** 07/17/08 ************************************
     C*** BEG ADD *** 02/05/01 ****************************
     C     DOTPAG        IFNE      ' '
     C                   Z-ADD     1             Z                 1 0
     C                   TESTN                   DOTPAG               99
     C     *IN99         IFEQ      *ON
     C                   MOVE      DOTPAG        M                 1 0
     C                   SUB       1             M
     C*
     C                   DO        M
     C                   ADD       1             Z
     C                   MOVE      Z             A                 1
     C     CLDCID        CAT       A:0           CLDCID
     C                   EXSR      WRITE
     C     SVDCID        CAT       A:0           CLDCID
     C*** BEG CHG ****** 07/17/08 ************************************
     C     XXCLID        IFEQ      '081'
     C     CLDCID        ANDEQ     'NC21V4'
     C     XXCLID        OREQ      '081'
     C     CLDCID        ANDEQ     'NC60V4'
     C*
     C                   MOVEL(P)  CLFND         QQFND
     C                   MOVEL(P)  CLFYR         QQFYR
     C                   MOVEL(P)  CLFCLM        QQCASE
     C                   MOVEL(P)  CLDCID        QQDCID
     C                   Z-ADD     CL#CPY        @@#CPY
     C                   MOVEL(P)  CLOUTQ        QQOUTQ
     C                   MOVEL(P)  CLSFLD        QQSFLD
     C                   MOVEL(P)  CLSDOC        QQSDOC
     C                   CALL      'W4113C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQDCID
     C                   PARM                    @@#CPY            5 0
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQSFLD           40
     C                   PARM                    QQSDOC           12
     C                   ELSE
     C*** END CHG ****** 07/17/08 ************************************
     C*** BEG ADD ***** 01/14/07 **************************************
     C     CLFLDR        IFEQ      ' '
     C                   MOVEL(P)  CLDCID        QQPGID
     C                   MOVEL(P)  CLSFLD        QQSFLD
     C                   MOVEL(P)  CLSDOC        QQSDOC
     C                   Z-ADD     CL#CPY        @@#CPY
     C                   MOVEL(P)  CLOUTQ        QQOUTQ
     C                   CALL      'W4112C1'
     C                   PARM                    QQPGID           12
     C                   PARM                    QQSFLD           40
     C                   PARM                    QQSDOC           12
     C                   PARM                    @@#CPY            5 0
     C                   PARM                    QQOUTQ           10
     C                   PARM      ' '           QQRTCD            1
     C                   ELSE
     C*** END ADD ***** 01/14/07 **************************************
     C                   EXSR      PRTDOC
     C*** BEG ADD ***** 01/14/07 **************************************
     C                   ENDIF                                                  CLFFDR = ' '
     C*** END ADD ***** 01/14/07 **************************************
     C*** BEG CHG ****** 07/17/08 ************************************
     C                   ENDIF                                                  CLDCID='NC21V4'
     C*** END CHG ****** 07/17/08 ************************************
     C* RESET THE DOC ID
     C                   MOVE(P)   SVDCID        CLDCID
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 02/05/01 ****************************
     C                   END
     C                   END                                                    END 0 *IN65
     C*
     C*  READ NEXT LETTER REQUEST RECORD
     C*
     C     CLKEY         READE     PCL01                                  60
     C                   ENDDO                                                  END DOWEQ 60
     C*
     C*  END OF PROGRAM
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Names and phone
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      *
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C* **************************************************
     C*   SUBROUTINE TO GET FUND INFORMATION             *
     C* **************************************************
     C     INFOF         BEGSR
     C*
     C*  GET COMPANY INFORMATION
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*  GET COMPANY INFORMATION
     C*
     C     COKEY         CHAIN     PCO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEA     CONAME        CNB
     C                   Z-ADD     1             X                 3 0
     C     *BLANK        LOOKUP    CNB(X)                             50
     C     *IN50         IFEQ      '1'
     C                   MOVEA     CNB(X)        CN
     C                   ELSE
     C                   MOVEA     CONAME        CN
     C                   END
     C                   MOVEA     CN            CLCNME
     C*
     C*  COMPANY ADDRESS
     C*
     C                   MOVEL     COADR1        CLCAD1
     C                   MOVEL     *BLANKS       CLCAD2
     C                   MOVEL     COADR2        CLCAD2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      COST          XXST              2
     C                   Z-ADD     COZIP         XXZIP
     C                   MOVEL     COCTY         XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         CLCCSZ
     C*
     C                   Z-ADD     COFEIN        CLCFID
     C                   END                                                    END 0 *IN50
     C*
     C                   END                                                    END 0 *IN60
     C*
     C*  GET FUND INFORMATION
     C*
     C     FNKEY         CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEA     FNNAME        FNB
     C                   Z-ADD     1             X                 3 0
     C     *BLANK        LOOKUP    FNB(X)                             50
     C     *IN50         IFEQ      '1'
     C                   MOVEA     FNB(X)        FN
     C                   ELSE
     C                   MOVEA     FNNAME        FN
     C                   END
     C                   MOVEA     FN            CLFNME
     C                   MOVEL     FNSHNM        CLFSHN
     C*
     C*  FUND MAILING ADDRESS
     C*
     C                   MOVEL     FNMAD1        CLFMA1
     C                   MOVEL     FNMAD2        CLFMA2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      FNMST         XXST              2
     C                   Z-ADD     FNMZIP        XXZIP
     C                   MOVEL     FNMCTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         CLFMCS
     C*
     C*  FUND PHYSICAL ADDRESS
     C*
     C                   MOVEL     FNPAD1        CLFPA1
     C                   MOVEL     FNPAD2        CLFPA2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      FNPST         XXST              2
     C                   Z-ADD     FNPZIP        XXZIP
     C                   MOVEL     FNPCTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         CLFPCS
     C*
     C*  FUND PHONE NUMBERS
     C*
     C                   Z-ADD     FNPH1A        CLPH1A
     C                   Z-ADD     FNPH1         CLFPH1
     C                   Z-ADD     FNAREA        CLFARE
     C                   Z-ADD     FNPH2         CLFPH2
     C                   Z-ADD     FNFAXA        CLFAXA
     C                   Z-ADD     FNFAX         CLFAX
     C*
     C                   MOVEL     FNANAM        CLANAM
     C                   MOVEL     FNAAD1        CLAAD1
     C                   MOVEL     FNAAD2        CLAAD2
     C                   MOVEL     FNACTY        CLACTY
     C                   MOVEL     FNAST         CLAST
     C                   MOVEL     FNAZIP        CLAZIP
     C                   MOVEL     FNIADR        CLIADR
     C                   END                                                    END 0 *IN50
     C*
     C*  GET FUND AUXILIARY INFORMATION:
     C*
     C     F2KEY         CHAIN     PF201                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      F2UNM1        CLFUN1
     C                   MOVE      F2UNM2        CLFUN2
     C                   MOVE      F2UNM3        CLFUN3
     C                   MOVE      F2UNM4        CLFUN4
     C                   MOVE      F2UNM5        CLFUN5
     C                   MOVE      F2UNM6        CLFUN6
     C                   MOVE      F2UNM7        CLFUN7
     C                   MOVE      F2IAUD        CLIAUD
     C                   END                                                    END 0 *IN50
     C*
     C                   ENDSR
     C****************************************************
     C*   SUBROUTINE TO LOAD THE CITY STATE & ZIP        *
     C****************************************************
     C     SRADDR        BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     XXST          XXST13           13
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
     C****************************************************
     C*   SUBROUTINE TO EXECUTE PRTDOC COMMAND           *
     C****************************************************
     C     PRTDOC        BEGSR
     C*
     C*   IS AN OVERLAY FORM NECESSARY
     C*
     C     CLDOVR        IFGT      *BLANK
     C                   Z-ADD     32            I
     C                   MOVEA     CLDOVR        OVR(I)
     C                   ADD       12            I
     C                   MOVEA     ')'           OVR(I)
     C                   MOVEA     OVR           CMDSTR
     C                   Z-ADD     75            LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                            37
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C                   END
     C*
******* ADD 5/22/06 ****************************************************
     C****** HERE'S THE TEST FOR MSWORD PROCESSING VS. STD.
     *****      IF THE SYSVAL DEFAULT IS TO USE MSWORD, AND THE USER DEFAULT
     ***         IS MSWORD OR NOT SPECIFIED (*BLANKS) THEN DO THIS, ELSE
     ***         NORMAL PROCESSING SHOULD OCCUR
     **
     C                   MOVE      CLCO#         X@CO#
     C                   CALL      'DA511R'
     C                   PARM                    X@CO#             3
     C                   PARM      'CS'          XXXDPT            2
     C                   PARM                    CLDPCD
     C                   PARM                    CLFLDR
     C                   PARM                    CLDCID
     C                   PARM      *BLANKS       RTN1              1
     C*
     ***USE INTERACTIVE IF SPECIFIED, ELSE NORMAL (BATCH) PROCESSING
     C     RTN1          IFEQ      'I'                                           **INTERACTIVE**
     C                   EXSR      WRDDOC
     C                   GOTO      WASWRD
     C                   ENDIF
******* END ADD 5/22/06 ****************************************************
     C*   BUILD PRTDOC COMMAND
     C*
     C*** Begin chg *** 03/05/01 **************************************
     C                   MOVEA     CLDCID        CMD(15)                        DOCUMENT
     C*******              MOVEACLDCID    CMD,13           DOCUMENT
     C*** End   chg *** 03/05/01 **************************************
     C                   MOVE      CL#CPY        QQ#CPY            5
     C*** Begin chg *** 03/05/01 **************************************
     C                   MOVEA     QQ#CPY        CMD(37)                        COPIES
     C                   MOVEA     CLSDOC        CMD(67)                        DARTS DOC
     C*******              MOVEAQQ#CPY    CMD,35           COPIES
     C*******              MOVEACLSDOC    CMD,65           DARTS DOC
     C*** End   chg *** 03/05/01 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     CLFLDR        FLR
     C                   Z-ADD     1             I                 3 0
     C*** Begin chg *** 03/05/01 **************************************
     C                   Z-ADD     86            II                3 0
     C*******              Z-ADD84        II      30
     C*** End   chg *** 03/05/01 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CMD(II)
     C                   ADD       1             II
     C*
     C                   MOVEA     ')'           CMD(II)
     C                   ADD       2             II
     C                   MOVEA     'SAVFLR('     CMD(II)
     C*
     C                   ADD       7             II
     C                   MOVEA     ''''          CMD(II)
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     CLSFLD        FLR
     C                   ADD       1             II
     C                   Z-ADD     1             I                 3 0
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CMD(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   END
     C*
     C                   MOVEA     ''')'         CMD(II)
     C*
     C*   SPECIFY THE SELECTED OUTPUT QUEUE ON THE PRTDOC COMMAND
     C*
     C                   ADD       3             II
     C                   MOVEA     'OUTQ('       CMD(II)
     C                   ADD       5             II
     C                   MOVEA     CLOUTQ        CMD(II)
     C                   ADD       10            II
     C                   MOVEA     ')'           CMD(II)
     C*
      **** BEGIN ADD **** 01/23/04 *********************************************
     C                   ADD       2             II
     C                   MOVEA     'JOBQ'        CMD(II)
     C                   ADD       4             II
     C                   MOVEA     '(*NO)'       CMD(II)
      **** END   ADD **** 01/23/04 *********************************************
     C                   MOVEA     CMD           CMDSTR          225
     C                   Z-ADD     225           LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                            37
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
 **********  ADD 5/22/06 **********************************************
     **  BYPASSED THE PRTDOC
     C     WASWRD        TAG
 ********* END ADD 5/22/06 *******************************************
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C*
     C*  ONLY BUILD CHGDOCD COMMAND IF CHANGES ARE NOT TO BE ALLOWED
     C*  TO THE RESOLVED DOCUMENT.
     C*
     C     DOKEY         CHAIN     PDO01                              50
     C     *IN50         IFEQ      '0'
     C*
     C     DOALCH        IFEQ      'N'
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C*   BUILD CHGDOCD COMMAND
     C*
     C*** Begin chg *** 03/05/01 **************************************
     C                   MOVEA     CLSDOC        CM2(16)                        SAVE DOCUMENT
     C*******              MOVEACLSDOC    CM2,14           SAVE DOCUMENT
     C*** End   chg *** 03/05/01 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     CLSFLD        FLR
     C                   Z-ADD     1             I
     C*** Begin chg *** 03/05/01 **************************************
     C                   Z-ADD     48            II
     C*******              Z-ADD46        II
     C*** End   chg *** 03/05/01 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK                                       SAVE FOLDER
     C                   MOVEA     FLR(I)        CM2(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CM2(II)
     C                   ADD       1             II
     C                   MOVEA     ')'           CM2(II)
     C*
     C                   MOVEA     CM2           CM2STR          225
     C*
     C                   CALL      'QCMDEXC'                            37
     C                   PARM                    CM2STR
     C                   PARM                    LENGTH
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C                   ENDIF                                                  DOALCH = 'N'
     C*
     C                   ENDIF                                                  *IN50 = '0'
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C                   ENDSR
      *
      *
      *
     C****************************************************
     C*   SUBROUTINE TO WRITE RECORDS                    *
     C****************************************************
     C     WRITE         BEGSR
     C*
     C                   EXSR      INFOF
     C*
     C*** BEGIN ADD *** 05/28/98 **************************************
     C     DOKEY         CHAIN     PDO01                              50
     C*** END ADD ***** 05/28/98 **************************************
     C*
     C*  WRITE RECORD TO DARTS
     C*
     C*  ESTABLISH KEY
     C*
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             MOVE *BLANKS   CMKEY
     C********             MOVE *BLANKS   QCKEY
     C********             MOVE XXCO#     QCCO#
     C********             MOVE CLFND     QCFND
     C********             MOVE CMFYR     QCFYR
     C********             MOVE CMCASE    QCCASE
     C********
     C***ESTABLISH RECORD
     C********
     C********             MOVELQCKEY     CMKEY
     C********             MOVE *BLANKS   CMCSID
     C********             MOVEA*BLANKS   CM
     C********             MOVEADODESC    CM,1
     C********             MOVEACLDCID    CM,264
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********
     C********             MOVELDORMTY    CMCODE
     C********             MOVELDORMCD    CMRMCD
     C********
     C***GET*FOLDER NAME
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN DEL *** 05/27/98 ***********************************************
     C********
     C********             MOVE *BLANKS   QQFND   8
     C********             MOVE *BLANKS   QQSER   8
     C********             MOVE *BLANKS   QQSUB   8
     C********             MOVE *BLANKS   QQFOLD  8
     C********             MOVE *BLANKS   QQFLDX  7
     C********             MOVE *BLANKS   QQFPTH 40
     C********             MOVE *BLANKS   QQTERM  1
     C********
     C********             MOVELCLFND     QQFND            for Fund
     C********             MOVELDOASRV    QQSER            for Area of Service
     C********             MOVELDOSUBA    QQSUB            for Sub-Area
     C********
     C********             MOVEL'C'       QQFOLD           means "Member"
     C********             MOVELCMCASE    QQFLDX           CASE for Folder name
     C********             MOVE QQFLDX    QQFOLD
     C********
     C********             CALL 'SRFBLD'
     C********             PARM           QQFND
     C********             PARM           QQSER
     C********             PARM           QQSUB
     C********             PARM           QQFOLD
     C********             PARM           QQFPTH           Built, returned
     C********             PARM           QQTERM
     C********
     C********             MOVELQQFPTH    CMFOLD
     C********             MOVELQQFPTH    CLSFLD
     C********
     C*  GET DOCUMENT ID
     C********
     C********             MOVE *BLANKS   QQRT    2
     C********             MOVE *BLANKS   QQDOC  12
     C********             MOVE *BLANKS   QQTERM  1
     C********
     C********             MOVELDORMTY    QQRT             Remark Type
     C********             CALL 'SRDBLD'
     C********             PARM           QQFPTH           From SRFBLD routine
     C********             PARM           QQRT             Rmrk Type for Doc ID
     C********             PARM           QQDOC            Built, returned
     C********             PARM           QQTERM
     C********
     C********             MOVELQQDOC     CMDCID
     C********             MOVELQQDOC     CLSDOC
     C*** END DEL ***** 05/27/98 ***********************************************
     C*** BEGIN DEL *** 04/26/00 **************************************
     C****BEGIN ADD *** 05/27/98 ***********************************************
     C********
     C***DETERMINE IF CLIENT IS ENROLLED IN "OV/400"
     C********
     C********             MOVEL'SY'      ##KEY
     C********   SYKY      CHAINSSY01                60
     C********   *IN60     IFEQ *OFF
     C********
     C***GET*DOC ID AND FOLDER NAME FOR RESOLUTION OF DOCUMENT TO OV/400
     C********
     C********             MOVE CLCO#     Q2CO#
     C********             MOVE CLFND     Q2FND
     C********             CALL 'SNXTDCID'
     C********             PARM *BLANKS   Q2FLR  80
     C********             PARM *BLANKS   Q2DCID 12
     C********             PARM 'CL'      Q2PRE   2
     C********             PARM           Q2CO#   3
     C********             PARM           Q2FND   3
     C********             PARM *BLANKS   Q2RTCD  1
     C********             MOVELQ2FLR     CMFOLD
     C********             MOVELQ2FLR     CLSFLD
     C********             MOVELQ2DCID    CMDCID
     C********             MOVELQ2DCID    CLSDOC
     C********             ENDIF
     C****END*ADD ***** 05/27/98 ***********************************************
     C********
     C***W0070R IS KEYED PGM USED FOR SMKDP KEY DEFINITION FILE.
     C********
     C********             MOVEL'W0070R'  CMPNAM           KEYED PROGRAM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMTRDT
     C********
     C********             Z-ADDDSTIME    CMTRTM
     C********             MOVE *ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE *BLANKS   CMSTAT
     C********             MOVELCLIUSR    CMUSER
     C********             MOVEL'W4112R'  CMPGM
     C********             Z-ADDDSTIME    CMTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMDT
     C********
     C********             MOVELCLIUSR    CMIUSR
     C********             MOVE *ZEROS    CMDCDT
     C****BEGIN DEL *** 05/13/98 ***********************************************
     C********             MOVE *BLANKS   CMDIFL
     C****END*DEL ***** 05/13/98 ***********************************************
     C********             MOVE 'WC'      CMAPID
     C********             MOVE *BLANKS   CMIDOC
     C********             MOVE *BLANKS   CMIQUE
     C********             MOVE *BLANKS   CMIMGF
     C********             MOVE *BLANKS   CMIPTH
     C********             MOVE *ZEROS    CMPIDX
     C********             MOVE *BLANKS   CMQPTY
     C********             MOVE *BLANKS   CMDCTP
     C********             MOVEL'W4112R'  CMIPGM    P
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C****BEGIN ADD *** 05/13/98 ***********************************************
     C********             MOVE CTFL01    CMDIFL
     C****END*ADD ***** 05/13/98 ***********************************************
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********             MOVE *BLANKS   CMFL01
     C********             MOVE *BLANKS   CMFL02
     C********             MOVE *BLANKS   CMFL03
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMIDT
     C********
     C****BEGIN DLT *** 01/19/00 **************************************
     C***GET*LAST SEQ# USED
     C**********
     C********** C3KY      SETGTPCM0I
     C**********           READPPCM0I                    60
     C********** *IN60     IFEQ '0'
     C********** QCKEY     ANDEQPIKEY
     C********** PISEQ#    ADD  1         CMSEQ#
     C**********           ELSE
     C**********           Z-ADD0         CMSEQ#
     C**********           END
     C****END*DLT ***** 01/19/00 **************************************
     C********
     C********             MOVE *BLANKS   XXDCID  8
     C********             MOVELCLDCID    XXDCID
     C********             MOVELCLIUSR    CLRUSR
     C********
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVELXXCOM1    CMCOMM
     C********             MOVE XXCOM2    CMCOMM
     C****BEGIN CHG *** 03/25/99 **************************************
     C********   CMRMCD    IFNE *BLANKS
     C********
     C****BEGIN ADD *** 01/19/00 **************************************
     C***GET*LAST SEQ# USED
     C********
     C********   C3KY      SETGTPCM0I
     C********             READPPCM0I                    60
     C********   *IN60     IFEQ '0'
     C********   QCKEY     ANDEQPIKEY
     C********   PISEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD0         CMSEQ#
     C********             END
     C****END*ADD ***** 01/19/00 **************************************
     C********
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C********             ENDIF                           DORMCD<>BLAN
     C****END*  CHG *** 03/25/99 **************************************
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      CLFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEA     *BLANKS       CM
     C*** BEG ADD *** 02/05/01 ****************************
     C     DOTPAG        IFNE      ' '
     C     DODESC        CAT       'PAGE':2      DODESC
     C     A             IFEQ      ' '
     C     DODESC        CAT       '1':1         DODESC
     C                   ELSE
     C     DODESC        CAT       A:1           DODESC
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 02/05/01 ****************************
     C*** BEG ADD *** 10/04/11 *******************************************
     C     XXCLID        IFEQ      '114'
     C*** Begin Add *** 04/10/19 *****************************************
     C     XXCLID        OREQ      '152'
     C*** End   Add *** 04/10/19 *****************************************
     C     CLDCID        ANDEQ     'CLDENY'
     C     CLIPGM        ANDEQ     'W0083R'
     C*
     C                   MOVEL     DSTIME        XXARTM
     C*
     C                   Z-ADD     DSDATE        XXARDT
     C*
     C     '<'           CAT       Q1USER:0      XXPRF            25
     C     XXPRF         CAT       '>':0         XXPRF
     C*
     C     XXMM          CAT       '/':0         XXARD1            8
     C     XXARD1        CAT       XXDD:0        XXARD1
     C     XXARD1        CAT       '/':0         XXARD1
     C     XXARD1        CAT       XXYY:0        XXARD1
     C*
     C     XXPRF         CAT       XXARD1:1      XXPRF
     C*
     C     XXHR          CAT       ':':0         XXART1            5
     C     XXART1        CAT       XXMN:0        XXART1
     C*
     C     XXPRF         CAT       XXART1:1      XXPRF
     C     XXPRF         CAT       DODESC:1      Q#CM75
     C     Q#CM75        CAT       XDNY:1        Q#CM75
     C*
     C                   MOVE      'Y'           XXDENY            1
     C                   ELSE
     C                   MOVE      ' '           XXDENY
     C                   ENDIF
     C*** END ADD *** 10/04/11 *******************************************
     C                   MOVEA     DODESC        CM(1)
     C                   MOVEA     CLDCID        CM(264)
     C                   MOVEL(P)  XXCOM1        Q#COMM          256
     C                   MOVE(P)   XXCOM2        Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  CLIUSR        Q#USER           10
     C                   MOVEL     CLIUSR        CLRUSR
     C                   MOVEL(P)  'W4112R'      Q#PGM            10
     C*
     C*          DETERMINE IF CLIENT IS ENROLLED IN "OV/400"
     C*          ------------------------------------------------------
     C                   MOVEL     'SY'          ##KEY
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*          GET DOC ID AND FOLDER NAME
     C*          ------------------------------------------------------
     C                   MOVE      CLCO#         Q2CO#
     C                   MOVE      CLFND         Q2FND
     C*** BEG ADD ***** 05/22/08 **************************************
     C     CLFLDR        IFEQ      *BLANKS
     C                   MOVE      'P'           Q2RTCD
     C                   ELSE
     C                   MOVE      *BLANKS       Q2RTCD
     C                   ENDIF
     C*** END ADD ***** 05/22/08 **************************************
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       Q2FLR            80
     C                   PARM      *BLANKS       Q2DCID           12
     C                   PARM      'CL'          Q2PRE             2
     C                   PARM                    Q2CO#             3
     C                   PARM                    Q2FND             3
     C*** BEG ADD ***** 05/22/08 **************************************
     C**********           PARM *BLANKS   Q2RTCD  1
     C                   PARM                    Q2RTCD            1
     C*** END ADD ***** 05/22/08 **************************************
     C                   MOVEL(P)  Q2FLR         Q#FOLD           80
     C                   MOVEL     Q2FLR         CLSFLD
     C                   MOVEL(P)  Q2DCID        Q#DCID           12
     C                   MOVEL     Q2DCID        CLSDOC
     C                   ENDIF                                                  *IN60 = Off
     C*
     C                   MOVEL(P)  'W4112R'      Q#IPGM           10
     C                   MOVEL(P)  *BLANKS       Q#FL01            1
     C                   MOVEL(P)  *BLANKS       Q#FL02            1
     C                   MOVEL(P)  DORMTY        Q#CODE            2
     C                   MOVEL(P)  DORMCD        Q#RMCD            5
     C     Q#RMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   CTFL01        Q#DIFL            1
     C                   ELSE                                                   *IN60 = Off
     C                   MOVE      *BLANKS       Q#DIFL
     C                   ENDIF                                                  *IN60 = Off
     C*
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
      ******** Begin Add ************* 11/04/08 ***********************
     C     CLFLDR        IFEQ      *BLANKS
     C                   MOVE      'O'           Q#IMGF
     C                   ENDIF
      ******** End Add   ************* 11/04/08 ***********************
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C*
     C*** BEG ADD *** 10/04/11 *******************************************
     C*
     C** Generate a dart remark in SDCMP only when the bill is not denied
     C*
     C     XXDENY        IFEQ      *BLANKS
     C*** END ADD *** 10/04/11 *******************************************
     C     Q#RMCD        IFNE      *BLANKS
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   ENDIF                                                  RMCD <> blanks
     C*** END ADD *** 04/26/00 ****************************************
     C*** BEG ADD *** 10/04/11 *******************************************
     C                   ELSE
     C*
     C** Attach Denial Ltr to the same dart remark (as an additional dart remark
     C*
     C                   MOVE      CLDSQ#        Q#SEQ#
     C*
     C                   CALL      'DA701R'
     C                   PARM      CLDKEY        Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75           75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#            5
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*
     C                   MOVE      CLDSQ#        MDSEQ#
     C*
     C                   CALL      'ISEWMD'
     C                   PARM      CLDKEY        MDKEY            40
     C                   PARM                    MDSEQ#            5
     C                   PARM      Q#DCID        MDOID            40
     C                   PARM      Q#FOLD        MDOFLR           60
     C*** Beg Chg *** 12/15/11 ************************************
     C********             PARM 'O'       MDOTYP  1
     C                   PARM      ' '           MDOTYP            1
     C*** End Chg *** 12/15/11 ************************************
     C                   PARM      DODESC        MDODSC           40
     C                   PARM      Q#RMCD        MDRMCD            5
     C                   PARM      'AS400   '    MDSYSN           20
     C                   PARM      Q#PNAM        MDPNAM           10
     C                   PARM      Q1USER        MDUSR            10
     C                   PARM                    MDRTCD            1
     C                   PARM                    MDEDSC           80
     C                   PARM                    MDPATH           80
     C                   PARM                    MDRPYN            1
     C*
     C*** Attach 'Denied bill' as a prefix to original dart remark in sdcmp
     C*
     C                   MOVE      CLDKEY        KKKEY
     C                   MOVE      CLDSQ#        KKSEQ#
     C*
     C     SDKY          CHAIN     PCM0A                              60
     C     XDNY1         CAT       CMCOMM:1      CMCOMM
     C                   UPDATE    PCM0A
     C*
     C                   ENDIF                                                  XXDENY = blanks
     C*** END ADD *** 10/04/11 *******************************************
     C*
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C*   GET EMPLOYER DETAIL INFORMATION
     C*
     C                   Z-ADD     ELANPR        CLEEAP
     C*
     C*   CONVERT ELPFDT FROM YYMMDD TO MMDDYY
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLEPFD
     C*
     C*   CONVERT ELPTDT FROM YYMMDD TO MMDDYY
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLEPTD
     C*
     C                   Z-ADD     ELPROR        CLEPRF
     C*
     C*  GET EMPLOYER INFORMATION
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C                   Z-ADD     EMEMP#        CLEEMP
     C                   Z-ADD     EMDIV         CLEDIV
     C                   MOVEL     EMNAME        CLENME
     C*
     C*** BEG ADD *** 12/07/04 ***************************
     C                   MOVE      EMMPFL        CL1A1
     C*** END ADD *** 12/07/04 ***************************
     C*
     C*  EMPLOYER MAILING ADDRESS
     C*
     C                   MOVEL     EMMAD1        CLEMA1
     C                   MOVEL     EMMAD2        CLEMA2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMMST         XXST              2
     C                   Z-ADD     EMMZIP        XXZIP
     C                   MOVEL     EMMCTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         CLEMCS
     C*
     C*  EMPLOYER PHYSICAL ADDRESS
     C*
     C                   MOVEL     EMPAD1        CLEPA1
     C                   MOVEL     EMPAD2        CLEPA2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      EMPST         XXST              2
     C                   Z-ADD     EMPZIP        XXZIP
     C                   MOVEL     EMPCTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         CLEPCS
     C*
     C*  EMPLOYER ADDITIONAL INFORMATION
     C*
     C                   Z-ADD     EMAREA        CLEARE
     C                   Z-ADD     EMPHN#        CLEPH#
     C                   Z-ADD     EMFAXA        CLEFXA
     C                   Z-ADD     EMFAX#        CLEFX#
     C*
     C                   MOVE      EMRGN#        CLERGN
     C     REKY          CHAIN     PRE01                              60
     C     *IN60         IFEQ      *OFF
     C     RESTAT        ANDEQ     'A'
     C                   MOVEL(P)  REDESC        CLERGD
     C                   END
      *
     C                   Z-ADD     EMFEIN        CLEFID
      ***** BEG DEL ******* 01/31/02 **********************************
     C*********            MOVELEMCONT    CLECNT
     C*********            MOVELEMATTN    CLEATT
     C*********            MOVELEMCNT3    CLCNT3    P
      ***** END DEL ******* 01/31/02 **********************************
      ***** BEG ADD ******* 01/31/02 **********************************
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  *BLANKS       CLC1EM
     C                   MOVEL(P)  *BLANKS       CLC2EM
     C                   MOVEL(P)  *BLANKS       CLC3EM
      ***** END ADD ******* 11/18/02 **********************************
      *
      * Get Employer Contact Names / phone
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         CLECNT
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  ECEMAL        CLC1EM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVE      '2'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         CLEATT
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  ECEMAL        CLC2EM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVE      '3'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         CLCNT3
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  ECEMAL        CLC3EM
      ***** END ADD ******* 11/18/02 **********************************
      *
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVEL(P)  EMFDNM        CLEDBA
     C                   MOVEL     EMFDNM        CLEDBA
     C                   MOVEL     EMCOM1        CLECM1
     C                   MOVEL     EMCOM2        CLECM2
     C                   MOVEL     EMCOM3        CLECM3
     C                   MOVEL     EMSEMP        CLESEM
     C                   MOVEL     EMCNTY        CLECTY
     C     EMPST         IFNE      *BLANK
     C     EMPST         CAT(P)    EMCNTY:0      XXKEY
     C                   ELSE
     C     EMMST         CAT(P)    EMCNTY:0      XXKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C     CNSTAT        ANDEQ     'A'
     C                   MOVEL(P)  CNDESC        CLECTD
     C                   ENDIF
      *
     C                   MOVEL     EMCHAP        CLECHP
     C     CPKY          CHAIN     PCP01                              60
     C     *IN60         IFEQ      *OFF
     C     CPSTAT        ANDEQ     'A'
     C                   MOVEL(P)  CPDESC        CLECHD
     C                   ENDIF
      *
     C                   MOVEL(P)  EMUWTR        CLUWNC
     C                   MOVEL(P)  EMUWTR        XXSTAF
     C     UPKY1         CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        CLUWNM
     C  N60              Z-ADD     UPEXT#        CLUWNX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  UPMAIL        CLUWEM
     C   60              MOVEL(P)  *BLANKS       CLUWEM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL(P)  EMLREP        CLLREP
     C                   MOVEL(P)  EMLREP        XXSTAF
     C     UPKY1         CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        CLLREN
     C  N60              Z-ADD     UPEXT#        CLLREX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  UPMAIL        CLLREM
     C   60              MOVEL(P)  *BLANKS       CLLREM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL     EMSIC         CLSIC
     C                   MOVEL     EMSIC         XXKEY
     C*** BEG CHG *** 08/15/00 ****************************
     C                   MOVEL(P)  'SIC'         XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        CLSICD
     C                   ENDIF
     C********** TBKY      CHAINPSC01                60
     C********** *IN60     IFEQ *OFF
     C********** SCSTAT    ANDEQ'A'
     C**********           MOVELSCDESC    CLSICD    P
     C**********           END
     C*** END CHG *** 08/15/00 ****************************
      *
     C                   SELECT
     C     EMEFD5        WHENNE    *ZERO
     C                   Z-ADD     EMEFD5        CLEFDT
     C     EMEFD4        WHENNE    *ZERO
     C                   Z-ADD     EMEFD4        CLEFDT
     C     EMEFD3        WHENNE    *ZERO
     C                   Z-ADD     EMEFD3        CLEFDT
     C     EMEFD2        WHENNE    *ZERO
     C                   Z-ADD     EMEFD2        CLEFDT
     C     EMEFD1        WHENNE    *ZERO
     C                   Z-ADD     EMEFD1        CLEFDT
     C                   ENDSL
     C*
     C                   Z-ADD     CLEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLEFDT
      *
     C                   SELECT
     C     EMCAD5        WHENNE    *ZERO
     C                   Z-ADD     EMCAD5        CLCADT
     C     EMCAD4        WHENNE    *ZERO
     C                   Z-ADD     EMCAD4        CLCADT
     C     EMCAD3        WHENNE    *ZERO
     C                   Z-ADD     EMCAD3        CLCADT
     C     EMCAD2        WHENNE    *ZERO
     C                   Z-ADD     EMCAD2        CLCADT
     C     EMCAD1        WHENNE    *ZERO
     C                   Z-ADD     EMCAD1        CLCADT
     C                   ENDSL
     C*
     C                   Z-ADD     CLCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLCADT
      *
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  *BLANKS       CLAGEM
     C                   MOVEL(P)  *BLANKS       CLAPEM
      ***** END ADD ******* 11/18/02 **********************************
     C*
     C*  GET AGENT HISTORY INFORMATION:
     C*
     C     AHKEY         SETGT     PAH01                                        CO/FND/EMP/DIV/FYR
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   END                                                    END 0 *IN50
     C     *IN50         IFEQ      '0'
     C     AHCO#         ANDEQ     CLCO#
     C     AHFND         ANDEQ     CLFND
     C     AHEMP#        ANDEQ     XXEMP#
     C     AHDIV         ANDEQ     XXDIV
     C     AHFYR         ANDEQ     CLFYR
     C                   Z-ADD     AHAGT#        CLAAGT
     C*
     C*  CONVERT ELPTDT FROM YYMMDD TO MMDDYY
     C*
     C                   Z-ADD     AHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLAEFD
     C*
     C                   Z-ADD     AHPROD        CLAPRO
     C*
     C*  GET AGENT ASSIGNMENT INFORMATION
     C*
     C     AAKEY         CHAIN     PAA01                              50        CO/FND/AGT
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     AAFEIN        CLAFID
     C                   Z-ADD     AAMOD         CLAMOD
      ***** BEG ADD ******* 11/18/02 **********************************
      *
     C     APKY          CHAIN     PAP01                              60
     C  N60              MOVEL(P)  APEMAL        CLAPEM
     C   60              MOVEL(P)  *BLANKS       CLAPEM
      *
      ***** END ADD ******* 11/18/02 **********************************
     C                   END                                                    END 0 *IN50
     C*
     C*  GET AGENT MASTER INFORMATION
     C*
     C     AGKEY         CHAIN     PAG01                              50        CO/FEIN/MOD
      *
     C     *IN50         IFEQ      '0'
      ***** BEG ADD ******* 11/18/02 **********************************
     C                   MOVEL(P)  AGEMAL        CLAGEM
      ***** END ADD ******* 11/18/02 **********************************
     C                   MOVEL     AGNAME        CLANME
     C*
     C*  AGENT MAILING ADDRESS
     C*
     C                   MOVEL     AGMAD1        CLAMA1
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      AGMST         XXST              2
     C                   Z-ADD     AGMZIP        XXZIP
     C                   MOVEL     AGMCTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         CLAMCS
     C*
     C*  AGENT PHYSICAL ADDRESS
     C*
     C                   MOVEL     AGPAD1        CLAPA1
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      AGPST         XXST              2
     C                   Z-ADD     AGPZIP        XXZIP
     C                   MOVEL     AGPCTY        XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         CLAPCS
     C*
     C*  AGENT ADDITIONAL INFORMATION
     C*
     C                   Z-ADD     AGARE1        CLAAR1
     C                   Z-ADD     AGPHN1        CLAPH1
     C                   Z-ADD     AGARE2        CLAAR2
     C                   Z-ADD     AGPHN2        CLAPH2
     C                   MOVE      AGSECY        CLASEC
     C                   MOVE      AGCONT        CLACNT
     C                   MOVE      AGCOMM        CLACM1
     C                   MOVE      AGCOM2        CLACM2
     C                   END                                                    END 0 *IN50
     C                   END                                                    END 0 *IN50
     C*
     C*  GET CLAIM DEPT LETTER AUX FILE
     C*
     C     C2KEY         CHAIN     PC201                              51
     C*
     C*  GET CLAIMANT INFORMATION
     C*
     C     CDKEY         CHAIN     PCD01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      *BLANKS       CLCLNM
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        CLCLNM
     C                   END
     C*** BEGIN ADD ***  03/07/05  ************************************
     C                   MOVEL(P)  EMNAME        CLENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        CLENM1
     C                   MOVE(P)   EMNAM2        CLENM2
     C*
     C                   MOVEL(P)  EMFDNM        CLEFD1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        CLEFD1
     C                   MOVE(P)   EMFDN2        CLEFD2
     C*** END ADD *****  03/07/05  ************************************
     C*
     C*  GET CLAIMANT ADDRESS
     C*
     C                   MOVEL     CDADR1        CLLAD1
     C                   MOVEL     CDADR2        CLLAD2
     C*
     C                   MOVE      *BLANKS       XXCSZ
     C                   MOVE      CDST          XXST              2
     C                   Z-ADD     CDZIP         XXZIP
     C                   MOVEL     CDCTY         XXCTY            25
     C                   EXSR      SRADDR
     C                   MOVE      CMSS#         CLSS#
     C*
     C*** BEG ADD *** 10/20/03 *****************************
     C*
     C     CDCTRY        IFNE      *BLANKS
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        XXCSZ
     C                   MOVE      CDCTRY        XXCSZ
     C                   END
     C*** END ADD *** 10/20/03 *****************************
     C                   MOVEL     XXCSZ         CLLCSZ
     C                   Z-ADD     CDDTOB        CLDTOB
      *
     C                   Z-ADD     CDAREA        CLAREA
     C                   Z-ADD     CDPHN#        CLPHN#
      *
     C                   MOVEL     CDMSTS        CLMSTS
     C                   MOVEL(P)  'MSTS'        XXTBID
     C                   MOVEL(P)  CDMSTS        XXKEY
     C                   MOVE      *BLANK        CLMSTD
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        CLMSTD
     C                   END
      *
     C                   MOVE(P)   *BLANK        CLMTSD
     C                   MOVEL(P)  CDCNTY        CLCNTY
     C     CDST          CAT(P)    CDCNTY:0      XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C     CNSTAT        ANDEQ     'A'
     C                   MOVEL(P)  CNDESC        CLCNTD
     C                   END
      *
     C                   Z-ADD     CDWARA        CLWARA
     C                   Z-ADD     CDWPH#        CLWPH#
      *
      * CLAIM MASTER INFO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLACDT
     C*
     C                   Z-ADD     CMCLAS        CLCLAS
     C                   Z-ADD     ELPTDT        XXEFDT
     C     PRKEY         SETGT     PPR01
     C     PRKEY1        READPE    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRSTAT        ANDEQ     'A'
     C                   MOVEL     PRDESC        CLCLDS
     C                   ELSE
     C                   MOVE      *BLANKS       CLCLDS
     C                   END
     C*** BEGIN ADD ***  08/11/98  ************************************
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         IFEQ      *OFF
     C     EPEXT         ANDNE     *BLANKS
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 10/06/99 ************************************
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N60              MOVEL(P)  PXDESC        CLCLDS
      **** BEGIN ADD **** 10/06/99 ************************************
     C   60RSKY          CHAIN     PRS02                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N61
     CANN60              MOVEL(P)  PXDESC        CLCLDS
      **** END   ADD **** 10/06/99 ************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  08/11/98  ************************************
      *
     C                   MOVEL     CMSPEC        CLSPEC
     C                   Z-ADD     CMAWW         CLAWW
     C                   Z-ADD     CMACTM        CLACTM
     C                   Z-ADD     CMDTRP        CLDTRP
     C                   Z-ADD     CMDTLW        CLDTLW
     C                   MOVEL     CMACDS        CLACDS
     C                   MOVEL     CMLTFL        CLCLLT
      *
     C                   MOVEL     CMACL#        CLACL#
     C                   MOVEL     CMESPV        CLESPV
     C                   MOVEL     CMECS#        CLECS#
     C                   MOVEL     CMOCDS        CLOCDS
     C                   MOVEL     CMOCCD        CLOCCD
     C                   MOVEL(P)  CMOCCD        XXKEY
     C                   MOVE      *BLANK        CLOCCE
     C     OCKY          CHAIN     POC01                              60
     C     *IN60         IFEQ      *OFF
     C     OCSTAT        ANDEQ     'A'
     C                   MOVEL(P)  OCDESC        CLOCCE
     C                   END
      *
      *  GET DEPARTMENT DESC
     C                   MOVEL(P)  CMDEPT        CLDEPT
     C                   MOVEL(P)  CMDEPT        X1DEPT
     C                   MOVE      *BLANK        CLDEPD
     C     DPKY          CHAIN     PDP01                              60
     C     *IN60         IFEQ      *OFF
     C     OCSTAT        ANDEQ     'A'
     C                   MOVEL(P)  DPDESC        CLDEPD
     C                   END
      *
      *
     C                   Z-ADD     CMHIDT        CLHIDT
      *  HIRE DATE
      *
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLHIDT
      *
      *
      *  EMPLOYMENT STATUS, DESC
      *
     C                   MOVE      *BLANK        CLESTD
     C                   MOVEL     CMESTS        CLESTS
     C     CMESTS        IFNE      *BLANK
     C                   MOVEL(P)  'EMST'        XXTBID
     C                   MOVEL(P)  CMESTS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        CLESTD
     C                   END
     C                   END
      *
     C                   MOVEL(P)  CMSHFT        CLSHFT                         SHIFT
     C                   MOVE      *BLANK        CLSHFD
      *
     C     CMSHFT        IFNE      *BLANK
     C                   MOVEL(P)  'SHFT'        XXTBID
     C                   MOVEL(P)  CMSHFT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        CLSHFD
     C                   END
     C                   END
      *
     C                   MOVEL(P)  CMACD2        CLACD2
      *
     C                   MOVEL(P)  CMAMAN        CLAMAN
     C                   MOVEL(P)  CMAMAN        XXSTAF
     C     UPKY1         CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        CLAMNA
     C  N60              Z-ADD     UPEXT#        CLAMAX
      ***** BEG ADD ******* 11/18/02 **********************************
     C  N60              MOVEL(P)  UPMAIL        CLAMEM
     C   60              MOVEL(P)  *BLANKS       CLAMEM
      ***** END ADD ******* 11/18/02 **********************************
      *
     C                   MOVEL(P)  CMNRSE        CLNRSE
     C                   MOVEL(P)  CMNRSE        XXSTAF
     C     UPKY1         CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        CLNRSN
     C  N60              Z-ADD     UPEXT#        CLNRSX
      *
     C                   MOVEL(P)  CMDNY         CLDNY
     C                   MOVEL(P)  CMCVFL        CLCVFL
     C                   MOVEL(P)  CMSUBR        CLSUBR
     C                   MOVEL(P)  CMACQR        CLACQR
     C                   MOVEL(P)  CMSDF         CLSDF
      *
     C                   MOVEL(P)  CMMCO         CLMCO
     C                   MOVEL     CMMCTP        CLMCTP
      *
     C                   MOVE      *BLANK        CLMCOD
     C                   MOVEL(P)  'MCO '        XXTBID
     C                   MOVEL(P)  CMMCTP        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        CLMCOD
     C                   ENDIF
      *
     C     TPKY          CHAIN     PTP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   TPPHYF        CLPHYF
     C                   MOVE(P)   TPPHYM        CLPHYM
     C                   MOVE(P)   TPPHYF        XXFEIN
     C                   MOVE(P)   TPPHYM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        CLPHYN
      **** BEGIN ADD **** 04/02/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        CLPHYA
     C                   ELSE
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVEL(P)  PDADR1        CLPHYA
      **** BEGIN ADD **** 04/02/99 ************************************
     C                   ENDIF
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVEL(P)  PDCTY         CLPHYC
     C                   MOVEL(P)  PDST          CLPHYS
     C                   MOVE(P)   PDZIP         CLPHYZ
     C                   MOVE(P)   PDAREA        CLPHYR
     C                   MOVE      PDPHN#        CLPHYP
     C                   END
     C                   END
      *
     C                   MOVE(P)   CMHOSF        CLHOSF
     C                   MOVE(P)   CMHOSM        CLHOSM
     C                   MOVE(P)   CMHOSF        XXFEIN
     C                   MOVE(P)   CMHOSM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        CLHOSN
      **** BEGIN ADD **** 04/02/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR2        CLHOSA
     C                   ELSE
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVEL(P)  PDADR1        CLHOSA
      **** BEGIN ADD **** 04/02/99 ************************************
     C                   ENDIF
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVEL(P)  PDCTY         CLHOSC
     C                   MOVEL(P)  PDST          CLHOSS
     C                   MOVE(P)   PDZIP         CLHOSZ
     C                   MOVE(P)   PDAREA        CLHOSR
     C                   MOVE(P)   PDPHN#        CLHOSP
     C                   END
      *
     C                   MOVE      *BLANK        CLITRT
     C                   MOVEL     CMITRT        CLITRT
     C                   MOVE      *BLANK        CLITRD
     C                   MOVEL(P)  'INIT'        XXTBID
     C                   MOVEL(P)  CMITRT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL     GTDESC        CLITRD
     C                   ENDIF
      *  SURGERY
     C                   Z-ADD     CMSGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         CLSGDT
      *  RELEASED TO WORK
     C                   Z-ADD     CMRLW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         CLRLW
      *  MMI
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         CLMMI
      *
     C                   Z-ADD     CMIPCT        CLIPCT
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C                   MOVEL     CMIMP         CLIMP
     C***** END   ADD **** 04/03/00 ***********************************
     C                   MOVEL     CMODIS        CLODIS
     C                   Z-ADD     CMYRLX        CLYRLX
     C                   MOVEL     CMHCAP        CLHCAP
     C                   MOVEL     CMDTH         CLDTH
      *  DATE OF DEATH
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL     SRDTO         CLDOD
      *
     C                   MOVE      *BLANK        CLPOI
     C                   MOVEL(P)  CMPOI         CLPOI
      *
     C                   MOVE(P)   *BLANK        CLFSAL
     C                   MOVEL(P)  CMFSAL        CLFSAL
      *
     C                   MOVE      *BLANK        CLFSDT
     C                   Z-ADD     CMFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLFSDT
      *
     C                   Z-ADD     *ZERO         CLPRWW
     C                   Z-ADD     CMPRWW        CLPRWW
      *
     C                   MOVE(P)   *BLANK        CLWMTH
     C                   MOVEL(P)  CMWMTH        CLWMTH
     C                   MOVE(P)   *BLANK        CLWMTD
     C                   MOVEL(P)  'WMTH'        XXTBID
     C                   MOVEL(P)  CMWMTH        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        CLWMTD
     C                   ENDIF
      *
     C                   Z-ADD     *ZERO         CLPOWW
     C                   Z-ADD     CMPOWW        CLPOWW
     C                   Z-ADD     CMCRT         CLCRT                          COMP RATE
      *
     C                   MOVE      *BLANK        CLWGPD
     C                   MOVE      *BLANK        CLWGPE
     C                   MOVEL(P)  CMWGPD        CLWGPD
     C                   MOVEL(P)  'FRWP'        XXTBID
     C                   MOVEL(P)  CMWGPD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        CLWGPE
     C                   END
      *
     C                   MOVE      *BLANK        CLDBDT
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLDBDT
      *
     C                   MOVE      *BLANK        CLRLWK
     C                   Z-ADD     CMRLWK        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLRLWK
      *
     C                   MOVE      *BLANK        CLERTW
     C                   Z-ADD     CMERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLERTW
      *
     C                   MOVE      *BLANK        CLRTW
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLRTW
      *
     C                   MOVEL(P)  CMESUP        CLESUP
     C                   MOVEL(P)  CMESUP        XXSTAF
     C     UPKY1         CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        CLESUN
     C  N60              Z-ADD     UPEXT#        CLESUX
      *
     C                   MOVEL(P)  CMSDFA        CLSDFA
     C                   MOVEL(P)  CMSDFA        XXSTAF
     C     UPKY1         CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        CLSDFN
     C  N60              Z-ADD     UPEXT#        CLSDFX
      *
     C                   Z-ADD     CMHRSD        CLHRSD                         HOURS PER DAY
      *
      *  TPA CONTACT DATE
      *
     C                   Z-ADD     CMTPAD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLTPAD
      *
     C*** Begin dlt *** 09/21/01 **************************************
     C*******************  MOVELCMRECL    SRDTI
     C*** End dlt ***** 09/21/01 **************************************
      *
     C                   MOVEL     CMSUPR        CLSUPR
     C     CLSUPR        CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN CHG *** 03/24/99 ***********************************************
     C                   MOVEL     UPUSNM        CLSPNM
     C***********
     C***THE*FIELD CLSPNM MUST BE USED TEMPORARILY TO STORE THE VALUE OF
     C***THE*REQUESTOR'S NAME (SEE BELOW, CLRUSR) BECAUSE A FIELD FOR
     C***THE*REQUESTOR'S NAME WAS NOT ORIGINALLY IN THE WORK FILE
     C***********
     C*** END CHG ***** 03/24/99 ***********************************************
     C                   Z-ADD     UPEXT#        CLSPEX
     C                   ELSE
     C*** BEGIN ADD *** 03/24/99 ***********************************************
     C                   MOVE      *BLANKS       CLSPNM
     C*** END ADD ***** 03/24/99 ***********************************************
     C                   MOVE      *ZEROS        CLSPEX
     C                   END
     C                   MOVEL     CMCLEX        CLCLEX
     C     CLCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        CLCXNM
     C                   MOVEL     UPUSNM        CLADJN
     C                   Z-ADD     UPEXT#        CLCXEX
     C*** BEG CHG *** 08/20/02 **********************************
     C***********          MOVELUPALIC    CLAJL#
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  ALLIC         CLAJL#
     C                   ELSE
     C                   MOVE      *BLANKS       CLAJL#
     C                   ENDIF
     C*** END CHG *** 08/20/02 **********************************
     C                   MOVEL     UPTITL        CLTITL
     C                   ELSE
     C                   MOVE      *BLANKS       CLCXNM
     C                   MOVE      *BLANKS       CLADJN
     C                   MOVE      *ZEROS        CLCXEX
     C                   MOVE      *BLANKS       CLAJL#
     C                   MOVE      *BLANKS       CLTITL
     C                   END
     C                   MOVEL     CMFEAD        CLFEAD
     C     CLFEAD        CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        CLFDNM
     C                   Z-ADD     UPEXT#        CLFDEX
     C                   ELSE
     C                   MOVE      *BLANKS       CLFDNM
     C                   MOVE      *ZEROS        CLFDEX
     C                   END
     C                   MOVE      *BLANKS       CLRUSR
     C                   MOVEL     CLIUSR        CLRUSR
     C     CLRUSR        CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN DEL *** 03/24/99 ***********************************************
     C*********************MOVELUPUSNM    CLSPNM
     C*** END DEL ***** 03/24/99 ***********************************************
     C                   Z-ADD     UPEXT#        CLRUEX
     C                   ELSE
     C*** BEGIN DEL *** 03/24/99 ***********************************************
     C*********************MOVE *BLANKS   CLSPNM
     C*** END DEL ***** 03/24/99 ***********************************************
     C                   MOVE      *ZEROS        CLRUEX
     C                   END
     C                   Z-ADD     CMDTIC        CLDTIC
     C                   Z-ADD     CMRCVD        CLRCVD
     C                   MOVE      CMPLFL        CLPLFL
     C*
     C*   CONVERT ALL DATES TO MMDDYY FORMAT IN WORKFILE.
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLDTOB
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLDTRP
     C*
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLDTLW
     C*
     C                   Z-ADD     CMDTIC        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLDTIC
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLRCVD
      *
     C                   MOVEL(P)  CMUMAN        CLUMAN
     C                   MOVEL(P)  CMUMAN        XXSTAF
     C     UPKY1         CHAIN     PUP01                              60
     C  N60              MOVEL(P)  UPUSNM        CLUMNA
     C  N60              Z-ADD     UPEXT#        CLUMAX
      *
      * LEGAL INFO
      *
     C     LLKY          CHAIN     PLL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     LLDEFA        CLDEFA
     C                   Z-ADD     LLDEFM        CLDEFM
     C                   Z-ADD     LLDEFA        XXFEIN
     C                   Z-ADD     LLDEFM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        CLDEFN
     C                   ELSE
     C                   MOVEL(P)  LLDEFN        CLDEFN
     C                   ENDIF                                                  ENDIF *IN60
      *
     C                   MOVEL(P)  LLDEFR        CLDEFR
     C                   MOVE(P)   LLDEFP        CLDEFP
      *
     C                   Z-ADD     LLPLAA        CLPLAA
     C                   Z-ADD     LLPLAM        CLPLAM
     C                   Z-ADD     LLPLAA        XXFEIN
     C                   Z-ADD     LLPLAM        XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        CLPLAN
     C                   ELSE
     C                   MOVEL(P)  LLPLAN        CLPLAN
     C                   ENDIF                                                  ENDIF *IN60
      *
     C                   MOVEL(P)  LLPLAR        CLPLAR
     C                   MOVE(P)   LLPLAP        CLPLAP
      *
     C                   MOVE      *BLANK        CLFRAD
     C                   MOVEL(P)  LLFRAU        CLFRAU
     C                   MOVEL(P)  'FRAU'        XXTBID
     C                   MOVEL(P)  LLFRAU        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        CLFRAD
     C                   ENDIF
      *
     C                   Z-ADD     LLLWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLLWDT
      *
     C                   MOVEL(P)  LLLITG        CLLITG
      *
      *  METHOD OF SETTLEMENT
      *
     C                   MOVEL(P)  'LSST'        XXTBID                                4A
     C                   MOVEL(P)  LLMTST        CLMTST
     C                   MOVE(P)   *BLANK        CLMTSD
     C     CMJRST        CAT       LLMTST:0      XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        CLMTSD
     C                   ENDIF
      *
     C                   Z-ADD     LLSAMT        CLSAMT
      *
     C                   Z-ADD     LLDTAP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLDTAP
     C                   ENDIF                                                  CHAIN PLL01
     C*
     C*  ESTABLISH SEQ#
     C*
     C     XXSEQ#        ADD       1             XXSEQ#            5 0
     C                   Z-ADD     XXSEQ#        CLSEQ#
     C*
     C*  WRITE DEPARTMENT LETTERS WORKFILE RECORD FOR PRINTING
     C*
     C                   MOVEL     Q1USER        CLUSER
     C*** BEGIN ADD ***  09/17/98  ************************************
     C     CLUSER        CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        CLUSNM
     C                   ELSE
     C                   MOVE      *BLANKS       CLUSNM
     C                   END
     C*** END ADD *****  09/17/98  ************************************
     C                   MOVEL     Q1PGM         CLPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLDT
     C*
     C*** BEG ADD *** 09/06/02 ****************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CLDT20
     C*** END ADD *** 09/06/02 ****************************
     C*
     C                   Z-ADD     DSTIME        CLTIME
      ***** BEG ADD ******* 11/18/02 **********************************
      *
      *  User email
      *
     C     Q1USER        CHAIN     PUP02                              60
     C  N60              MOVEL(P)  UPMAIL        CLUPEM
     C   60              MOVEL(P)  *BLANKS       CLUPEM
      *
      *  Employer email
      *
     C     EMKY2         CHAIN     PEM01                              60
     C  N60              MOVEL(P)  EMIADR        CLEMEM
     C   60              MOVEL(P)  *BLANKS       CLEMEM
      *
      ***** END ADD ******* 11/18/02 **********************************
     C*
     C*  UPDATE/WRITE CLAIM AUX WORKFILE
     C*
     C     *IN51         IFEQ      *OFF
     C                   UPDATE    PC201
     C                   ELSE
     C                   MOVEL     Q1USER        CLIUSR
     C                   MOVEL     Q1PGM         CLIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CLIDT
     C*
     C                   Z-ADD     DSTIME        CLITIM
     C                   WRITE     PC201
     C                   END
     C*
     C                   UPDATE    PCL01                                61
     C*
     C                   ENDSR
******* ADD 5/22/06 ****************************************************
     ***=====================================================================
     C     WRDDOC        BEGSR
     ***=====================================================================
     C*** DETERMINE IF WE SHOULD DISPLAY OR EDIT FROM DO RECORD
     C*
     C                   MOVEL     *BLANK        DOALCH
     C     DOKEY         CHAIN     PDO01                              50
     C*
     C**C*****
     C                   MOVEA     CLDCID        CM3(15)                        DOCUMENT
     C*******              MOVEACLDCID    CM3,13           DOCUMENT
     C*** End   chg *** 03/05/01 **************************************
     C                   MOVE      CL#CPY        QQ#CPY            5
     C*** Begin chg *** 03/05/01 **************************************
     C                   MOVEA     QQ#CPY        CM3(37)                        COPIES
     C                   MOVEA     CLSDOC        CM3(67)                        DARTS DOC
     C*******              MOVEAQQ#CPY    CM3,35           COPIES
     C*******              MOVEACLSDOC    CM3,65           DARTS DOC
     C*** End   chg *** 03/05/01 **************************************
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     CLFLDR        FLR
     C                   Z-ADD     1             I                 3 0
     C*** Begin chg *** 03/05/01 **************************************
     C                   Z-ADD     86            II                3 0
     C*******              Z-ADD84        II      30
     C*** End   chg *** 03/05/01 **************************************
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CM3(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   ENDDO
     C*
     C                   MOVEA     ''''          CM3(II)
     C                   ADD       1             II
     C*
     C                   MOVEA     ')'           CM3(II)
     C                   ADD       2             II
     C                   MOVEA     'SAVFLR('     CM3(II)
     C*
     C                   ADD       7             II
     C                   MOVEA     ''''          CM3(II)
     C*
     C                   MOVEA     *BLANK        FLR
     C                   MOVEA     CLSFLD        FLR
     C                   ADD       1             II
     C                   Z-ADD     1             I                 3 0
     C*
     C     FLR(I)        DOWNE     *BLANK
     C                   MOVEA     FLR(I)        CM3(II)
     C                   ADD       1             I
     C                   ADD       1             II
     C                   END
     C*
     C                   MOVEA     ''')'         CM3(II)
     C*
     C*   SPECIFY THE SELECTED OUTPUT QUEUE ON THE PRTDOC COMMAND
     C*
     C                   ADD       3             II
     C                   MOVEA     'OUTQ('       CM3(II)
     C                   ADD       5             II
     C                   MOVEA     CLOUTQ        CM3(II)
     C                   ADD       10            II
     C                   MOVEA     ')'           CM3(II)
     C*
     C                   ADD       2             II
     C                   MOVEA     'JOBQ'        CM3(II)
     C                   ADD       4             II
     C                   MOVEA     '(*NO)'       CM3(II)
     ****
     C     DOALCH        IFNE      'Y'
     C                   ADD       6             II
     C                   MOVEA     'EDTDSP'      CM3(II)
     C                   ADD       6             II
     C                   MOVEA     '(*DSP)'      CM3(II)
     C*
     C                   ELSE
     C*
     C                   ADD       6             II
     C                   MOVEA     '      '      CM3(II)
     C                   ADD       6             II
     C                   MOVEA     '      '      CM3(II)
     C*
     C                   ENDIF
    *****
     C                   MOVE      *BLANKS       CMDSTR
     C                   MOVEA     CM3           CMDSTR          225
     C                   Z-ADD     225           LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'                            37
     C                   PARM                    CMDSTR
     C                   PARM                    LENGTH
     C*
     C                   MOVE      *BLANKS       CMDSTR
     C*
     C*
     C     ENDWRD        ENDSR
******* END ADD 5/22/06 ****************************************************
     ***=====================================================================
      *
     C*
**  (CMD) QCMDEXC COMMAND
SYPRTDOC DOC('            ') COPIES(     ) SAVOUTPUT(*YES) SAVDOC(
   ) FLR('

**  (CM2) QCMDEXC COMMAND
SYCHGDA   DOC('            ') ALWRPL(*NO) FLR('


**  (OVR) QCMDEXC COMMAND
OVRPRTF FILE(QSYSPRT) FRONTOVL(
**  (CM3) QCMDEXC COMMAND
DTWRDDOC DOC('            ') COPIES(     ) SAVOUTPUT(*YES) SAVDOC(
   ) FLR('

