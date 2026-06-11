     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*
     H/TITLE W7196R1 - MEMBER EXPERIENCE REPORT - BUILD WORKFILE
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM CREATES A WORKFILE THAT WILL BE READ AND         *
     H* PROCESSED BY THE REPORT PROGRAM FOR THE MEMBER EXPERIENCE     *
     H* REPORT.  ONE WORK FILE RECORD IS PRODUCED FOR EACH CLAIM      *
     H* RECORD READ.  THE ACCIDENT DATE RANGE DETERMINES WHICH CLAIM  *
     H* RECORDS ARE READ FOR THE GROUP PASSED IN.  ALL CLAIMS FOR     *
     H* THOSE YEARS COVERED UNDER THE ACCIDENT DATE RANGE ARE READ.   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W7196C1                                                     *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:33    CREATED: 05/17/98    PROJECT: WC701       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 12/01/10 C:235 PROJECT: 133UKG                                *
     H*                Add new field to hold 2nd line of accident     *
     H*                description                                    *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Load extension specific description to work    *
     H*                file fields.                                   *
     H*                                                               *
     H* 05/12/09 C:136 Project: 135UMB                                *
     H*                Add Carrier # to outfile                       *
     H*                                                               *
     H* 03/09/09 C:062 Project WC1611                                 *
     H*                 Get new/renewal flag from WDELP               *
     H*                                                               *
     H*  01/26/07  C:132 Project: 81WYS                               *
     H*                 Remove the blank of SS# output in workfile    *
     H*                 as per request of Walter Habermas.            *
     H*                                                               *
     H*  01/11/07  C:132 Project: 81WYS                               *
     H*                 Do not load the SS# if prompt select <> "Y"   *
     H*                                                               *
     H* 08/15/05  C:15  Support: WC102/48047                          *
     H*                 Change program name + suffix '1'              *
     H*                                                               *
     H* 10/04/04 C:15  Support: WC071/443995                          *
     H*                Policies outside of the "Policy eff dt" range  *
     H*                were being included on report.  Corrected code.*
     H*                                                               *
     H* 06/23/04 C:15  Project: 114WHK                                *
     H*                Load new fields to workfile.                   *
     H*                  E2SIC  - SIC Code                            *
     H*                  E2SICD - SIC Code Description                *
     H*                  E2UWTR - Underwriter's Staff Code            *
     H*                  E2UWNM - Undrewriter's Name                  *
     H*                                                               *
     H* 11/18/03 C:15  Project: 121WAS  Seq# 40002                    *
     H*                Corrected various code.                        *
     H*                                                               *
     H*  10/16/03  C:132  PROJECT 121WAS                              *
     H*                   Load new fields to workfile:                *
     H*                     E2GCLS - Governing Class Code             *
     H*                     E2SCLD - Governing Class Cose Description *
     H*                     E2EXMD - Experience Modifier              *
     H*                     E2WPRN - Written Premium                  *
     H*                     E2FPDT - From Policy Date                 *
     H*                     E2TPDT - To Policy Date                   *
     H*                     E2IEMP - Include employers w/out claims   *
     H*                                                               *
     H* 09/25/03 C:15  Support: WC126/39217                           *
     H*               -Added code to load the claimant name from WMCMP*
     H*                if no WMCDP record exists.                     *
     H*               -Chgd code to prevent an endless loop.          *
     H*                                                               *
     H* 09/18/03 C:15  Support: WC129/38820                           *
     H*                Added code to load the lost time flag using    *
     H*                the lost time history file.                    *
     H*                                                               *
     H* 08/13/03 C:15  Project: WC577                                 *
     H*                Load E2OPCL with 'O' for open claims.          *
     H*                                                               *
     H* 08/06/03 C:15  Project: WC577                                 *
     H*                Add code to load E2EARN (earned premium).      *
     H*                                                               *
     H* 07/17/03 C:53 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*                                                               *
     H* 02/20/03 C:132 PROJECT WC577                                  *
     H*                Add fields for producer number and producer    *
     H*                name to WFE2P file.                            *
     H*                                                               *
     H* 11/22/02 C:142 PROJECT WC1100                                 *
     H*                Fixed to process all groups, if group 000      *
     H*                is selected.                                   *
     H*                                                               *
     H* 10/25/01 C:62  PROJECT WC113 29362                            *
     H*                Correct Main Loop processing for blank         *
     H*                division scenario.                             *
     H*                                                               *
     H* 05/29/01 C:61  PROJECT WC577                                  *
     H*                Add field (WFE2P/E2NWRW) for New / Renewal.    *
     H*                                                               *
     H* 03/27/01 C:15  Support: WC096/26845                           *
     H*                Chgd code to retrieve the correct agent for    *
     H*                the policy year being processed.               *
     H*                                                               *
     H* 09/18/00 C:15  Support: WC102/24385                           *
     H*                Chgd the size of the YR array to allow more    *
     H*                than 20 reporting years.                       *
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*                                                               *
     H*  05/05/00 C:15 Support: WC096/22751                           *
     H*                Chgd pgm to use the reserve history eff dt     *
     H*                instead of the maint date to determine when    *
     H*                the record was written.                        *
     H*                                                               *
     H*  05/03/00 C:33 PROJECT: 96NCP                                 *
     H*                LOAD E2PREM FROM ALL DIVISIONS FOR THAT EMPLYR *
     H*                                                               *
     H*  12/08/99 C:87 PROJECT: 96NCP                                 *
     H*                Changed process to run for all groups (except  *
     H*                999) if the group parm is '000'. Load new      *
     H*                fields E2CXDT, E2PREM, E2CMP in WFE2P.         *
     H*                                                               *
     H*  11/24/99 C:33 PROJECT: WC960                                 *
     H*                MODIFY TO RUN FOR ONE OR ALL DIVISIONS         *
     H*                                                               *
     H*  05/13/99 C:90 PROJECT: 99-SUPP                               *
     H*                EXCLUDE CLAIMS WHERE CMIDT > LOSS AS OF DATE.  *
     H*                                                               *
     H*  02/24/99 C:90 PROJECT 96WBX                                  *
     H*                Load new fields to workfile:                   *
     H*                  E2RST  - Rating state                        *
     H*                  E2JRST - Claim jurisdication state           *
     H*                  E2CRSV - Comp rsv chg this period            *
     H*                  E2MRSV - Med rsv chg this period             *
     H*                  E2LRSV - Legal rsv chg this period           *
     H*                  E2ORSV - Other rsv chg this period           *
     H*                  E2PFDT - Policy from date                    *
     H*                  E2PTDT - Policy to date                      *
     H*                                                               *
     H*  02/01/99 C:61 PROJECT 96WBH                                  *
     H*                Load new fields to workfile:                   *
     H*                  E2CTG2 - Category 2                          *
     H*                  E2DEPT - Department Code                     *
     H*                  E2DPDS - Department Description              *
     H*                  E2CHAP - Chapter Code                        *
     H*                  E2CHDS - Chapter Description                 *
     H*                  E2SEX  - Sex                                 *
     H*                  E2AGE  - Age                                 *
     H*                                                               *
     H*  07/21/98  C:33  SUPPORT: WC091/12776                         *
     H*                  ALL *IN61'S USED TO BE *IN60 (NO COMMENTS)   *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
      *
      *
     FWMCMLS    IF   E           K DISK
     FWMCMLC    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCML5    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     F***  BEGIN ADD  ***  11/24/99  *********************************
     FWMCML11   IF   E           K DISK
     F                                     RENAME(PCM01:PCM11)
     F***  END   ADD  ***  11/24/99  *********************************
     FWMCDL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     F*** Begin add *** 09/18/03 **************************************
     FWMTHL     IF   E           K DISK
     F*** End add ***** 09/18/03 **************************************
     FWMPRL     IF   E           K DISK
     FWDPAL3    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWMRHL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
      ***** BEG ADD  **** 04/20/09 ************************************
     FWMPXL1    IF   E           K DISK
      ***** END ADD  **** 04/20/09 ************************************
     FWTABL     IF   E           K DISK
     FWFE2P     O    E             DISK
     FWMAHL     IF   E           K DISK
     FWACNL     IF   E           K DISK
     F                                     RENAME(PCN01:PCN02)
      ***** BEG ADD ******* 02/01/99 **********************************
      *
      *  Department Code
     FWMDPL     IF   E           K DISK
      *
      *  Chapter Code
     FWMCPL     IF   E           K DISK
      *
      * ZZZ
      ***** END ADD ******* 02/01/99 **********************************
      **** BEGIN ADD **** 02/24/99 ************************************
      *
      *  Rate Set Code File
     FWMRSL     IF   E           K DISK
     F                                     RENAME(PRS01:PRS02)
      *
      *  Employer Detail File
     F***  BEGIN CHG  ***  05/03/00  *********************************
     F**********WDELL   IF  E           K        DISK
     FWDELL1    IF   E           K DISK
     FWDELL3    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     F***  END   CHG  ***  05/03/00  *********************************
      **** END   ADD **** 02/24/99 ************************************
     F**** BEGIN ADD **** 12/08/99 ************************************
     F*  Agent Assignment file
     FWMAAL1    IF   E           K DISK
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F**** END   ADD **** 12/08/99 ************************************
      *
     F***** BEG ADD ***** 10/16/03 ************************************
      *
     FWDELL8    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      *
     F*  Employer Payroll file
     FWMEPL     IF   E           K DISK
      *
     F*  Payroll Description file
     FWMPRL1    IF   E           K DISK
     F                                     RENAME(PPR01:PPR02)
     F***** END ADD ***** 10/16/03 ************************************
     F*** Begin add *** 06/23/04 *************************************
     FWMUPL1    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     F*** End add ***** 06/23/04 *************************************
      *** BEG ADD ***** 05/12/09 **************************************
     FWDF2L     IF   E           K DISK
      *** END ADD ***** 05/12/09 **************************************
      *
     D*** BEGIN CHG *** 09/18/00 **************************************
     D******************* YR         20  3 0A            YR ARRAY
     D YR              S              3  0 DIM(100) ASCEND                      YR ARRAY
     D*** END CHG ***** 09/18/00 **************************************
      **** BEGIN ADD **** 12/08/99 ************************************
      **** END   ADD **** 12/08/99 ************************************
     D*
      ***** BEG ADD ***** 10/16/03 ***********************************
     D LOADED          S              1N                                        LOAD SWITCH
      ***** END ADD ***** 10/16/03 ***********************************
     D*
     D XXSORT          DS
     D  CMEMP#                 1      9  0
     D  CMDIV                 10     14  0
     D  CMFYR                 15     17  0
     D  CMCTG1                18     27
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              OUTSTAND RSV TYPE
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
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
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
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  RHS                    1     90  2
     D                                     DIM(10)                              CHGS TO RESERVE
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D                 DS
     D  RHR                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
      **** BEGIN ADD **** 12/08/99 ************************************
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      **** END   ADD **** 12/08/99 ************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C/EJECT
     C*
     C*  CLAIMS MASTER FILE
     C*
     C     CMKYS1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X7FRDT
     C*
     C     CMKYS2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     CMKYC1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    X7FRDT
     C*
     C     CMKYC2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C***  BEGIN ADD  ***  11/24/99  *********************************
     C     CMK111        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    X7FRDT
     C*
     C     CMK112        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C***  END   ADD  ***  11/24/99  *********************************
     C*
     C     CMKY51        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR3
     C*
     C     CMKY52        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C***  BEGIN ADD  ***  11/24/99  *********************************
     C     CMKY53        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR3
     C                   KFLD                    XXEMP#
     C***  END   ADD  ***  11/24/99  *********************************
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  CLAIM HISTORY FILE
     C*
     C     CHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    X7RNDT
     C*
     C     CHKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C*
      ***** BEG ADD  **** 04/20/09 ************************************
     C*  PAYROLL DESCRIPTION EXT FILE
     C*
     C     PXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    CMEXT
     C                   KFLD                    RSJRST
     C                   KFLD                    XXEFDT
     C*
     C     PXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    CMEXT
     C                   KFLD                    RSJRST
     C*
      ***** END ADD  **** 04/20/09 ************************************
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  CLAIMS PAYMENT FILE
     C*
     C     PAKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  RESERVE HISTORY FILE
     C*
     C     RHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     AHKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C                   KFLD                    X7RNDT
     C*
     C*  COUNTY CODE SELECT WORK FILE
     C*
     C     SLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *  Department Code
      *
     C     DPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMDEPT
      *
      *  Chapter Code
      *
     C     CPKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMCHAP
      *
      *  Rate set code
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *  Employer detail
      *
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     ELKY1         KLIST
      ***** BEG CHG ***** 10/16/03 ************************************
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMFYR
     C***********        KFLD                    CMCO#
     C***********        KFLD                    CMFND
     C***********        KFLD                    CMFYR
     C***********        KFLD                    CMEMP#
      ***** END CHG ***** 10/16/03 ************************************
      *
     C     ELKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    KKFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
     C*
     C*  Agent Assignment
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C*  Fund master file
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
      *
      ***** BEG ADD ***** 10/16/03 ***********************************
     C*
     C*  AGENT HISTORY FILE using employer detail fields
     C*
     C     AHKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     AHKY4         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    X7RNDT
     C*
      *
      *  Claims Master file
     C*
     C     CMKYE1        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*** Begin chg *** 11/18/03 *********************************************
     C****************** KFLD                    X7FPDT
     C                   KFLD                    X7FRDT
     C*** End chg ***** 11/18/03 *********************************************
     C*
     C     CMKYE2        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *  Employer Detail file
      *
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X7FPDT
      *
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  Employer Master file
     C     EMKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *  Employer Payroll file
      *
     C     EPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
      *  Class Code Description file
     C*
     C     PRKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    E2GCLS
     C                   KFLD                    ELPFDT
     C*
     C     PRKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    E2GCLS
     C*
      ***** END ADD ***** 10/16/03 ***********************************
     C*
     C*** Begin add *** 06/23/04 *************************************
     C*  System Table File key list
     C*
     C     GTKY          KLIST
     C                   KFLD                    xxco#
     C                   KFLD                    xxtbid
     C                   KFLD                    xxkey
     C*** End add ***** 06/23/04 *************************************
     C*
     C     *LIKE         DEFINE    E2CO#         XXCO#
     C     *LIKE         DEFINE    E2FND         XXFND
     C     *LIKE         DEFINE    E2FYR         XXFYR
     C     *LIKE         DEFINE    E2FYR         XXFYR3
     C     *LIKE         DEFINE    E2EMP#        XXEMP#
     C     *LIKE         DEFINE    E2DIV         XXDIV
     C     *LIKE         DEFINE    E2AGT#        XXAGT#
     C     *LIKE         DEFINE    E2AGT#        X2AGT#
      ***** BEG ADD ***** 02/20/03 *************************************
     C     *LIKE         DEFINE    E2PROD        XXPROD
     C     *LIKE         DEFINE    E2PROD        X2PROD
      ***** END ADD ***** 02/20/03 *************************************
     C     *LIKE         DEFINE    CMACDT        X7FRDT
     C     *LIKE         DEFINE    CMACDT        X7TODT
     C     *LIKE         DEFINE    CMACDT        X7RNDT
     C     *LIKE         DEFINE    E2CNTY        XXCNTY
     C     *LIKE         DEFINE    E2RGN#        XXRGN#
     C     *LIKE         DEFINE    E2ZIP         XXZIP
     C     *LIKE         DEFINE    E2ZIP         X2ZIP
     C     *LIKE         DEFINE    RTKEY         XXKEY
     C     *LIKE         DEFINE    PRCLAS        XXCLAS
     C     *LIKE         DEFINE    PREFDT        XXEFDT
     C     *LIKE         DEFINE    PAAMT         XXAMT
     C     *LIKE         DEFINE    *IN60         VALID
     C     *LIKE         DEFINE    E2ACDT        XXMDYY
     C     *LIKE         DEFINE    E2ACDT        X8FRDT
     C     *LIKE         DEFINE    E2ACDT        X8TODT
     C     *LIKE         DEFINE    E2ACDT        X8RNDT
     C     *LIKE         DEFINE    E2ACDT        ##DAT1
     C     *LIKE         DEFINE    E2ACDT        ##DAT2
     C     *LIKE         DEFINE    ELAAPR        XXAAPR
     C     *LIKE         DEFINE    ELANPR        XXANPR
     C     *LIKE         DEFINE    E2PREM        XXPREM
     C     *LIKE         DEFINE    ELFYR         KKFYR
      ***** BEG ADD ***** 10/16/03 ***********************************
     C     *LIKE         DEFINE    EPCLAS        XPCLS
     C     *LIKE         DEFINE    EPCLAS        ZPCLS
     C     *LIKE         DEFINE    ELAAPR        XXWPRM
     C     *LIKE         DEFINE    ELAAPR        ZZWPRM
     C     *LIKE         DEFINE    ELPFDT        X7FPDT
     C     *LIKE         DEFINE    ELPFDT        X7TPDT
      ***** END ADD ***** 10/16/03 ***********************************
     C*** Begin add *** 06/23/04 *************************************
     C     *like         define    gttbid        xxtbid
     C*** End add ***** 06/23/04 *************************************
     C*** beg add ***** 05/12/09 *************************************
     C     *like         define    F2CAR#        svcar#
     C*** end add ***** 05/12/09 *************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@AFDT            8
     C                   PARM                    @@ATDT            8
     C                   PARM                    @@RNDT            8
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@RGN#            3
     C                   PARM                    @@CNTY            2
     C                   PARM                    @@AGT#            5
     C                   PARM                    @@ZIP             5
     C                   PARM                    @@SELO            1
     C                   PARM                    @@SELC            1
     C                   PARM                    @@MNTH            3
     C                   PARM                    @@SEQ             1
     C                   PARM                    @@FPRM           11
     C                   PARM                    @@TPRM           11
      ***** BEG ADD ***** 10/16/03 ***********************************
     C                   PARM                    @@FPDT            8
     C                   PARM                    @@TPDT            8
     C                   PARM                    @@SELN            1
      ***** END ADD ***** 10/16/03 ***********************************
      ***** BEG ADD ***** 01/11/07 ***********************************
     C                   PARM                    @@PRSS            1
      ***** END ADD ***** 01/11/07 ***********************************
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
     C*_______________________________________________________________
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@AFDT        X8FRDT
     C                   MOVE      @@ATDT        X8TODT
     C*
     C                   Z-ADD     X8FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FRDT
     C*
     C                   Z-ADD     X8TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TODT
      ***** BEG ADD ***** 10/16/03 ***********************************
      * If either date ranges are zeros, load *hival into TO dates
     C                   IF        X7FRDT = *ZEROS AND X7TODT = *ZEROS
     C                   EVAL      X7TODT = *HIVAL
     C                   ENDIF
      ***** END ADD ***** 10/16/03 ***********************************
     C*
     C                   MOVE      @@RNDT        X8RNDT
     C*
     C                   Z-ADD     X8RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7RNDT
     C*
     C                   MOVEL     X7RNDT        XXCYM             5 0
     C*
     C                   MOVE      @@EMP#        XXEMP#
     C                   MOVE      @@DIV         XXDIV
     C                   MOVE      @@RGN#        XXRGN#
     C                   MOVE      @@CNTY        XXCNTY
     C                   MOVE      @@AGT#        XXAGT#
     C                   MOVE      @@ZIP         XXZIP
     C                   MOVE      @@MNTH        XXMNTH            3 0
     C                   MOVE      @@FPRM        XXFPRM           11 0
     C                   MOVE      @@TPRM        XXTPRM           11 0
      ***** BEG ADD ***** 10/16/03 ***********************************
     C                   MOVE      @@FPDT        XXFPDT            8 0
     C                   MOVE      @@TPDT        XXTPDT            8 0
     C                   MOVE      @@SELN        XXSELN            1
     C*
     C* If have policy effective date parms, convert to
     C                   IF        XXFPDT <> *ZEROS
     C                   Z-ADD     XXFPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FPDT                         POLICY EFF FROM DATE
     C*
     C                   Z-ADD     XXTPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TPDT                         POLICY EFF TO DATE
     C                   ELSE
     C                   EVAL      X7FPDT = *ZEROS
     C                   EVAL      X7TPDT = *HIVAL
     C                   ENDIF
      ***** END ADD ***** 10/16/03 ***********************************
     C*
      /EJECT
      ***  Begin Change ** 11/22/02  *********************************
      * The change here was to put the original "mainline" routine and
      * put it into subroutine PRFUND. Then added ability to process
      * all funds if fund 000 was selected.
      *
     C     XXFND         IFEQ      *ZERO
     C     FNKY          SETLL     WMFNL
      *
     C                   DOU       %Eof(Wmfnl)
     C     FNKY          READE     WMFNL
      *
     C                   IF        Not %Eof and
     C                             FNTEST <> 'Y'
     C                   Z-ADD     FNFND         XXFND
     C                   EXSR      PRFUND
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ELSE
     C                   EXSR      PRFUND
     C                   ENDIF
      ***  End   Change ** 11/22/02  *********************************
     C*
     C                   MOVE      *ON           *INLR
     C*_______________________________________________________________
      ***  Begin Add    ** 11/22/02  *********************************
     C/EJECT
     C*===============================================================
     C*  PROCESS EACH FUND
     C*===============================================================
     C*
     C     PRFUND        BEGSR
     C*
     C*** Begin dlt *** 09/25/03 **************************************
     C**** *IN60         DOWEQ     *OFF
     C*** End dlt ***** 09/25/03 **************************************
     C*** beg add ***** 05/12/09 **************************************
     c     cmkys2        chain     pf201
     c                   if        %found and F2CAR# <> 0
     c                   eval      svcar# = F2CAR#
     c                   else
     c                   eval      svcar# = 0
     c                   endif
     C*** end add ***** 05/12/09 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVEA     *HIVAL        YR
      *
      ***** BEG ADD ***** 10/16/03 ***********************************
      *
      * If Include Emps w/no claims, then process by Employer Detail file
      *
     C                   IF        XXSELN = 'Y'
      *
     C                   Z-ADD     *ZEROS        XF1FYR            3 0
      *
     C     ELKY3         SETLL     pel03
     C     ELKY4         READE     pel03                                  61
     C                   DOW        NOT *IN61
     C                              and ELPFDT <= X7TPDT
      *
     C*** Begin add *** 11/18/03 *************************************
     C     @@emp#        ifne      *zeros
      *
     C     elemp#        ifne      xxemp#
     C     @@div         orne      *blanks
     C     eldiv         andne     xxdiv
     C                   goto      tag1
     C                   endif
      *
     C                   endif
     C*** End add ***** 11/18/03 *************************************
     C*
     C*** Begin dlt *** 11/18/03 *************************************
     C**********         EVAL      XXEMP# = ELEMP#
     C**********         EVAL      XXDIV  = ELDIV
     C*** End dlt ***** 11/18/03 *************************************
     C                   EVAL      LOADED = *OFF
     C*
     C     CMKYE1        SETLL     PCM02                                        ALL EMPLRS
     C     CMKYE2        READE     PCM02                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  In the Date Range, process, else read another record
     C*
     C     CMACDT        IFGE      X7FRDT
     C     CMACDT        ANDLE     X7TODT
     C     CMFYR         ANDEQ     ELFYR
     C*
     C*  Process this employer's claim records
     C*
     C                   EVAL      XXFYR3 = CMFYR
     C*
     C*  CHECK INFO AGAINST SELECION CRITERIA
     C                   EXSR      CHKINF
     C*
     C     VALID         IFEQ      *ON
     C*
     C*  LOAD FIELDS FOR WFE2P
     C                   CLEAR                   PE201
     C*
     C                   EXSR      LOADE2
     C*
     C                   EVAL      LOADED = *ON                                 SET LOAD INDCTR *ON
     C*
     C                   WRITE     PE201
     C*
     C                   ENDIF                                                  VALID = *ON
     C*
     C                   ENDIF                                                  CMACDT >= X7FRDT
     C*
     C     CMKYE2        READE     PCM02                                  60
     C                   ENDDO                                                  *IN60 = *ON PCM02
      *
      * If the LOADED indicator is off, then No Claims for this employer, so just write record
      *
     C                   IF        NOT LOADED
     C*
     C                   EVAL      CMCO#  = *ZEROS
     C                   EVAL      CMFND  = *ZEROS
     C                   EVAL      CMEMP# = *ZEROS
     C                   EVAL      CMDIV  = *ZEROS
     C                   EVAL      CMIDT  = *ZEROS
     C*
     C                   EXSR      CHKINF
     C*
     C     VALID         IFEQ      *ON
     C                   EXSR      LOADEL
     C                   ENDIF                                                  LOADED = *OFF
     C*
     C                   ENDIF                                                  LOADED = *OFF
     C*
     C*** Begin add *** 11/18/03 *************************************
     C     tag1          tag
     C*** End add ***** 11/18/03 *************************************
      *
     C     elky4         READE     pel03                                  61
     C                   ENDDO                                                   *IN61 = *ON
      *
     C                   ELSE                                                   ELSE PROCESS BY CLM
      *
      ***** END ADD ***** 10/16/03 ***********************************
      *
     C*_______________________________________________________________
     C*
     C*  GET YEARS TO PROCESS
     C*
     C                   EXSR      GETYRS
     C*_______________________________________________________________
     C*
     C*  LOOP TO PROCESS FOR EACH YEAR STORED IN ARRAY YR
     C*
     C     1             DO        Z             Y                 3 0
     C*
     C                   MOVE      YR(Y)         XXFYR3
     C*
     C*  PROCESS CLAIMS DATA FOR A SPECIFIC YEAR
     C*
     C                   SELECT
     C     XXEMP#        WHENEQ    *ZERO
     C     CMKY51        SETLL     Wmcml5
     C     CMKY51        READE     Wmcml5                                 60
     C     @@DIV         WHENEQ    *BLANK
     C     CMKY53        SETLL     Wmcml5
     C     CMKY53        READE     Wmcml5                                 60
     C                   OTHER
     C     CMKY52        SETLL     Wmcml5
     C     CMKY52        READE     Wmcml5                                 60
     C                   ENDSL
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  CHECK INFO AGAINST SELECION CRITERIA
     C*
     C                   EXSR      CHKINF
     C*
     C     VALID         IFEQ      *ON
     C*
     C*  LOAD FIELDS FOR WFE2P
     C*
     C                   CLEAR                   PE201
     C*
     C                   EXSR      LOADE2
     C*
     C                   WRITE     PE201
     C*
     C                   ENDIF                                                  VALID=*ON
     C*
     C                   SELECT
     C     XXEMP#        WHENEQ    *ZERO
     C     CMKY51        READE     Wmcml5                                 60
     C     @@DIV         WHENEQ    *BLANK
     C     CMKY53        READE     Wmcml5                                 60
     C                   OTHER
     C     CMKY52        READE     Wmcml5                                 60
     C                   ENDSL
     C*
     C                   ENDDO                                                  *IN60=*ON
     C*
     C                   ENDDO                                                  Z->Y
     C*
      ***** BEG ADD ***** 10/16/03 ***********************************
     C                   ENDIF                                                  XXSELN = 'Y'
      *
      ***** END ADD ***** 10/16/03 ***********************************
     C*
     C*** Begin dlt *** 09/25/03 **************************************
     C***************    ENDDO
     C*** End dlt ***** 09/25/03 **************************************
     C*
     C     TERM          TAG
     C                   ENDSR
      ***  End   Add  **** 11/22/02  *********************************
     C*
     C/EJECT
     C*===============================================================
     C*  CHECK THE INFORMATION AGAINST THE SELECTION CRITERIA
     C*===============================================================
     C*
     C     CHKINF        BEGSR
     C*
     C*** Begin add *** 10/04/04 *************************************
     C                   MOVE      *ON           VALID
     C*** End add ***** 10/04/04 *************************************
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              50
     C*
     C     *IN50         IFEQ      *OFF
     C*
     C                   MOVE      *ON           VALID
     C*
     C     EMMZIP        IFNE      *ZERO
     C                   MOVEL     EMMZIP        X2ZIP
     C                   ELSE
     C                   MOVEL     EMPZIP        X2ZIP
     C                   ENDIF                                                  XXMZIP<>*ZERO
     C*
     C     EMMST         IFNE      *BLANKS
     C     EMMST         CAT(P)    EMCNTY        XXKEY
     C                   ELSE
     C     EMPST         CAT(P)    EMCNTY        XXKEY
     C                   ENDIF                                                  EMMST<>*BLACKS
     C*
     C*  CHAIN TO AGENT HISTORY FILE TO GET ACTIVE AGENT
     C*
     C*** Begin chg *** 03/27/01 **************************************
     C********** AHKY      SETGTPAH01
     C     AHKY1         SETGT     PAH01
     C*** End chg ***** 03/27/01 **************************************
     C     AHKY          READPE    PAH01                                  61
     C*
     C     *IN61         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     AHAGT#        X2AGT#
      ***** BEG ADD ***** 02/20/03 *************************************
     C                   MOVEL     AHPROD        X2PROD
      ***** END ADD ***** 02/20/03 *************************************
     C                   ELSE
     C                   MOVE      *ZEROS        X2AGT#
      ***** BEG ADD ***** 02/20/03 *************************************
     C                   MOVEL     *ZEROS        X2PROD
      ***** END ADD ***** 02/20/03 *************************************
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C***** BEG ADD ***** 10/16/03 **************************************
      *
     C                   ELSE                                                   ELSE *IN50 = *ON
      *
     C* If EMPs with no claims flag on, use emp detail fields
     C                   IF        XXSELN = 'Y'
      *
     C     EMKY1         CHAIN     PEM01                              50
     C*
     C     *IN50         IFEQ      *OFF
     C*
     C                   MOVE      *ON           VALID
     C*
     C     EMMZIP        IFNE      *ZERO
     C                   MOVEL     EMMZIP        X2ZIP
     C                   ELSE
     C                   MOVEL     EMPZIP        X2ZIP
     C                   ENDIF                                                  XXMZIP<>*ZERO
     C*
     C     EMMST         IFNE      *BLANKS
     C     EMMST         CAT(P)    EMCNTY        XXKEY
     C                   ELSE
     C     EMPST         CAT(P)    EMCNTY        XXKEY
     C                   ENDIF                                                  EMMST<>*BLACKS
      *
     C* If EMPs with no claims flag on, use emp detail fields for agt info
      *
     C     AHKY4         SETGT     PAH01
     C     AHKY3         READPE    PAH01                                  61
     C*
     C     *IN61         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY3         READPE    PAH01                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     AHAGT#        X2AGT#
     C                   MOVEL     AHPROD        X2PROD
     C                   ELSE
     C                   MOVE      *ZEROS        X2AGT#
     C                   MOVEL     *ZEROS        X2PROD
     C                   ENDIF                                                  *IN61=*OFF
      *
     C                   ENDIF                                                  *in50 = *OFF
      *
     C                   ENDIF                                                  XXSELN = Y
     C*** END ADD ***** 10/16/03 **************************************
     C*
     C                   ENDIF                                                  *IN50=*OFF
      *
     C*** Begin add *** 11/18/03 *************************************
      * If not Include Emp#s w/no claims and had poicy eff date range, then
      * Check if policy effect date is in the input policy date range
      *
     C                   IF        XXSELN = 'N'
     C     ELKY          CHAIN     PEL01                              60
     C                   IF        *IN60
     C                   EVAL      VALID = *OFF
     C                   ELSE
     C                   IF        ELPFDT < X7FPDT OR ELPFDT > X7TPDT
     C                   EVAL      VALID = *OFF
     C                   ENDIF                                                  ELPFDT < X7FPDT
     C                   ENDIF                                                  *IN60 = *ON
      *
     C                   IF        CMACDT < X7FRDT OR CMACDT > X7TODT
     C                   EVAL      VALID = *OFF
     C                   ENDIF                                                  ELPFDT > X7TPDT
      *
     C                   ENDIF                                                  XXSELN = 'N'
     C*** End add ***** 11/18/03 *************************************
     C*_______________________________________________________________
     C*
     C                   SELECT
     C*
     C*  EMPLOYER DOESN'T EXIST
     C*
     C     *IN50         WHENEQ    *ON
     C                   MOVE      *OFF          VALID
      *
     C*** Begin dlt *** 11/18/03 *************************************
      ***** BEG ADD ***** 10/16/03 ***********************************
      * If not Include Emp#s w/no claims and had poicy eff date range, then
      * Check if policy effect date is in the input policy date range
      *
     C*****              WHEN      XXSELN = 'N'
     C*****              IF        X7FPDT > *ZEROS
     C*****ELKY          CHAIN     PEL01                              60
     C*****              IF        *IN60
     C*****              EVAL      VALID = *OFF
     C*****              ELSE
     C*****              IF        ELPFDT < X7FPDT OR ELPFDT > X7TPDT
     C*****              EVAL      VALID = *OFF
     C*****              ENDIF                                                  ELPFDT < X7FPDT
     C*****              ENDIF                                                  *IN60 = *ON
     C*****              ENDIF                                                  X7FPDT > *ZEROS
      *****
     C*****              IF        CMACDT < X7FRDT OR CMACDT > X7TODT
     C*****              EVAL      VALID = *OFF
     C*****              ENDIF                                                  ELPFDT > X7TPDT
      ***** END ADD ***** 10/16/03 ***********************************
     C*** End dlt ***** 11/18/03 *************************************
     C*
     C*  ONLY INCLUDE THOSE CLAIMS WITHIN SELECTED ZIP CODE
     C*
     C     XXZIP         WHENNE    *ZERO
     C     XXZIP         ANDNE     X2ZIP
     C                   MOVE      *OFF          VALID
     C*
     C*  ONLY INCLUDE THOSE CLAIMS WITHIN SELECTED REGION
     C*
     C     XXRGN#        WHENNE    *ZERO
     C     XXRGN#        ANDNE     EMRGN#
     C                   MOVE      *OFF          VALID
     C*
     C*  ONLY INCLUDE THOSE CLAIMS WITHIN SELECTED COUNTY
     C*
     C     @@CNTY        WHENNE    *BLANK
     C     @@CNTY        ANDNE     '*S'
     C     @@CNTY        ANDNE     EMCNTY
     C                   MOVE      *OFF          VALID
     C*
     C*  ONLY INCLUDE THOSE CLAIMS WITH SELECTED AGENT
     C*
     C     XXAGT#        WHENNE    *ZERO
     C     XXAGT#        ANDNE     X2AGT#
     C                   MOVE      *OFF          VALID
     C*
     C*** Begin dlt *** 11/18/03 *************************************
     C*  ONLY INCLUDE THOSE CLAIMS THAT ARE FOR EMPLOYERS IN
     C*  SELECTED COUNTY(S)
     C*****
     C*****@@CNTY        WHENEQ    '*S'
     C*****
     C*****SLKY          CHAIN     PCN02                              61
     C*****
     C*****              MOVE      *OFF          VALID
     C*****
     C******IN61         DOWEQ     *OFF
     C*****VALID         ANDNE     *ON
     C*****
     C*****CNCNTY        IFEQ      EMCNTY
     C*****              MOVE      *ON           VALID
     C*****              ENDIF                                                  CNCNTY=EMCNTY
     C*****
     C*****SLKY          READE     PCN02                                  61
     C*****
     C*****              ENDDO                                                  *IN61=*OFF
     C*** End dlt ***** 11/18/03 *************************************
     C*
      **** BEGIN ADD **** 05/13/99 ************************************
     C     CMIDT         WHENGT    X7RNDT
     C                   MOVE      *OFF          VALID
      **** END   ADD **** 05/13/99 ************************************
     C*
     C*  VALID SELECTION
     C*
     C                   OTHER
     C*** Begin dlt *** 10/04/04 *************************************
     C****************   MOVE      *ON           VALID
     C*** End dlt ***** 10/04/04 *************************************
     C*
     C                   ENDSL
     C*
     C*** Begin add *** 11/18/03 *************************************
     C*  ONLY INCLUDE THOSE CLAIMS THAT ARE FOR EMPLOYERS IN
     C*  SELECTED COUNTY(S)
     C*
     C     @@CNTY        IFEQ      '*S'
     C*
     C     SLKY          CHAIN     PCN02                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C     CNCNTY        IFEQ      EMCNTY
     C                   LEAVE
     C                   ENDIF                                                  CNCNTY=EMCNTY
     C*
     C     SLKY          READE     PCN02                                  61
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C   61              MOVE      *OFF          VALID
     C*
     C                   ENDIF
     C*** End add ***** 11/18/03 *************************************
     C*
     C                   ENDSR                                                  CHKINF
     C*
     C/EJECT
     C*===============================================================
     C*  READ THRU ACCIDENT DATE RANGE DATA FOR SPECIFIC EMPLOYER
     C*  OR GROUP.  THIS IS DONE TO DETERMINE WHICH YEAR(S) TO
     C*  PROCESS DURING THE MAIN LOOP PROCESSING.
     C*===============================================================
     C*
     C     GETYRS        BEGSR
     C*
     C                   Z-ADD     0             Z
     C*
     C                   SELECT
     C     XXEMP#        WHENEQ    *ZERO
     C     CMKYS1        SETLL     PCM01                                        ALL EMPLRS
     C     CMKYS2        READE     PCM01                                  60
     C     @@DIV         WHENEQ    *BLANK
     C     CMK111        SETLL     PCM11                                        ALL DIVISION
     C     CMK112        READE     PCM11                                  60
     C                   OTHER
     C     CMKYC1        SETLL     PCM02                                        SEL EMPLR
     C     CMKYC2        READE     PCM02                                  60
     C                   ENDSL
     C***  END   CHG  ***  11/24/99  *********************************
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*  IF BEYOND END OF DATE RANGE, STOP READING DATA
     C*
     C     CMACDT        IFGT      X7TODT
     C                   LEAVE
     C                   ENDIF
     C*
     C*  SEE IF YEAR HAS BEEN PREVIOUSLY SAVED TO ARRAY
     C*
     C                   Z-ADD     1             X
     C     CMFYR         LOOKUP    YR(X)                                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     1             X
     C     *HIVAL        LOOKUP    YR(X)                                  99
     C                   ADD       1             Z                 3 0
     C                   Z-ADD     CMFYR         YR(X)
     C                   ENDIF
     C*
     C                   SELECT
     C     XXEMP#        WHENEQ    *ZERO
     C     CMKYS2        READE     PCM01                                  60
     C     @@DIV         WHENEQ    *BLANK
     C     CMK112        READE     PCM11                                  60
     C                   OTHER
     C     CMKYC2        READE     PCM02                                  60
     C                   ENDSL
     C*
     C                   ENDDO
     C*
     C                   ENDSR                                                  GETYRS
     C*
      ***** BEG ADD ***** 10/16/03 ***********************************
     C/EJECT
     C*===============================================================
     C*  LOAD THE FIELDS FOR WFE2P IF NO CLAIMS FOR EMPLOYER DETAIL
     C*===============================================================
     C*
     C     LOADEL        BEGSR
     C*
     C                   CLEAR                   PE201
     C*
     C*  INITAILLY WRITE ALL RECORDS TO WFE2P (FOR CALC. PURPOSES),
     C*  USE E2PRT TO DISTINGUISH WHICH RECORDS SHOULD BE PRINTED
     C*
     C                   Z-ADD     ELCO#         E2CO#
     C                   Z-ADD     ELFND         E2FND
     C                   Z-ADD     ELEMP#        E2EMP#
     C                   Z-ADD     ELDIV         E2DIV
     C                   Z-ADD     ELFYR         E2FYR
     C                   Z-ADD     X2AGT#        E2AGT#
     C                   Z-ADD     X2PROD        E2PROD
     C                   EVAL      E2PRT  = 'Y'
     C*
     C                   Z-ADD     DSDATE        E2RNDT
     C                   Z-ADD     X8RNDT        E2ASOF
     C                   Z-ADD     X8FRDT        E2FRDT
     C                   Z-ADD     X8TODT        E2TODT
     C                   Z-ADD     XXFPRM        E2FPRM
     C                   Z-ADD     XXTPRM        E2TPRM
     C*
     C                   Z-ADD     EMRGN#        E2RGN#
     C                   MOVEL(P)  EMCNTY        E2CNTY
     C                   MOVEL     EMMZIP        E2ZIP
     C                   MOVEL(P)  EMNAME        E2EMNM
     C*
     C     EMMAD1        IFEQ      *BLANK
     C     EMPAD1        ANDNE     *BLANK
     C                   MOVEL(P)  EMPAD1        E2EAD1
     C                   MOVEL(P)  EMPAD2        E2EAD2
     C                   MOVEL(P)  EMPCTY        E2ECTY
     C                   MOVEL(P)  EMPST         E2EST
     C                   Z-ADD     EMPZIP        E2EZIP
     C                   ELSE
     C                   MOVEL(P)  EMMAD1        E2EAD1
     C                   MOVEL(P)  EMMAD2        E2EAD2
     C                   MOVEL(P)  EMMCTY        E2ECTY
     C                   MOVEL(P)  EMMST         E2EST
     C                   Z-ADD     EMMZIP        E2EZIP
     C                   ENDIF                                                  EMMAD1=*BLANKS
     C*
     C*
     C*  DETERMINE SORT FIELD
     C*
     C                   SELECT
     C     @@SEQ         WHENEQ    'A'
     C                   MOVEL(P)  E2EMNM        E2SORT
     C     @@SEQ         WHENEQ    'N'
     C                   MOVEL(P)  E2EMP#        E2SORT
     C     @@SEQ         WHENEQ    'R'
     C                   MOVEL(P)  E2RGN#        E2SORT
     C     @@SEQ         WHENEQ    'C'
     C                   MOVEL(P)  E2CNTY        E2SORT
     C     @@SEQ         WHENEQ    'G'
     C                   MOVEL(P)  E2AGT#        E2SORT
     C     @@SEQ         WHENEQ    'Z'
     C                   MOVEL(P)  E2ZIP         E2SORT
     C     @@SEQ         WHENEQ    '1'
     C                   MOVEL(P)  XXSORT        E2SORT
     C                   ENDSL
      *
      *  Chapter Code / Description
      *
      *
     C                   MOVEL(P)  EMCHAP        E2CHAP                         Chap Code
      *
     C     CPKY          CHAIN     PCP01                              62
     C  N62              MOVEL(P)  CPDESC        E2CHDS
     C   62              MOVEL(P)  *BLANKS       E2CHDS
      *
      *
     C                   eval      e2exmd = elexm1
      *
     C                   CLEAR                   XXPREM
     C                   eval      XXAAPR = ELAAPR - ELPRTD
     C                   eval      XXANPR = ELANPR - ELPRTD
     C
     C                   if        elfaf  = 'A'
     C                   eval      e2wprm = elaapr
     C                   eval      XXPREM = XXAAPR
     C                   else
     C                   eval      e2wprm = elanpr
     C                   eval      XXPREM = XXANPR
     C                   endif
     C                   Z-ADD     XXPREM        E2PREM
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         E2PFDT
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         E2PTDT
      *
      * Rating state
      *
     C     RSKY          CHAIN     PRS02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        E2RST
     C                   MOVE      RSJRST        E2JRST
      *
     C                   EXSR      GETCLS                                       Get Governing Class
      *
     C                   ELSE
     C                   MOVE      *BLANK        E2RST
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C*  Effective date/Cancel date
     C                   Z-ADD     1             I1                3 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C* Determine New/Renewal Code
     C*** BEGIN ADD *** 03/09/09 *******************************************
     C                   IF        ELNWRW=*BLANKS
     C*** END   ADD *** 03/09/09 *******************************************
     C                   IF        EF(I1) < ELPFDT
     C                   EVAL      E2NWRW = 'R'
     C                   ELSE
     C                   EVAL      E2NWRW = 'N'
     C                   ENDIF
     C*** BEGIN ADD *** 03/09/09 *******************************************
     C                   ELSE
     C                   EVAL      E2NWRW=ELNWRW
     C                   ENDIF
     C*** END   ADD *** 03/09/09 *******************************************
     C* Cancel date
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         E2CXDT
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C* GET EARNED PREMIUM
     C                   MOVE      ELCO#         SRCO#
     C                   MOVE      ELFND         SRFND
     C                   MOVE      ELFYR         SRFYR
     C                   MOVE      ELEMP#        SREMP#
     C                   MOVE      ELDIV         SRDIV
     C*
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#             3
     C                   PARM                    SRFND             3
     C                   PARM                    SRFYR             3
     C                   PARM                    SREMP#            9
     C                   PARM                    SRDIV             5
     C                   PARM      @@RNDT        SRRNDT            8
     C                   PARM      *BLANK        SRAMT            11
     C                   PARM      *BLANK        SRAMT2           11
     C                   PARM      '1'           SRTERM            1
     C*
     C                   MOVE      SRAMT         E2EARN
      *
      * Load Prompt Screen Selections in output fields
     C                   EVAL      E2FPDT = XXFPDT
     C                   EVAL      E2TPDT = XXTPDT
     C                   EVAL      E2IEMP = XXSELN
     C*
     C*  Agent federal id#
     C     AAKY          CHAIN     PAA01                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     AAFEIN        E2AGFE
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C*** Begin add *** 06/23/04 *************************************
     C*  Load SIC code and description.
     C*
     C                   eval      e2sic = emsic
     C*
     C                   movel(p)  'SIC'         xxtbid
     C                   movel(p)  emsic         xxkey
     C     gtky          chain     pgt01                              99
     C  N99              movel     gtdesc        e2sicd
     C*
     C*  Load Underwriter's staff code and name
     C*
     C                   eval      e2uwtr = emuwtr
     C     emuwtr        chain     pup01                              99
     C                   movel(p)  upusnm        e2uwnm
     C*** End add ***** 06/23/04 *************************************
     C*** beg add ***** 05/12/09 ************************************
     c                   eval      E2CAR# = svcar#
     C*** end add ***** 05/12/09 ************************************
     C*
     C                   WRITE     PE201
     C*
     C                   ENDSR                                                  LOADEL
     C*
      ***** END ADD ***** 10/16/03 ***********************************
     C/EJECT
     C*===============================================================
     C*  LOAD THE FIELDS FOR WFE2P
     C*===============================================================
     C*
     C     LOADE2        BEGSR
     C*
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKY          SETGT     PCH01
     C     CHKY2         READPE    PCH01                                  61
     C*
     C     *IN61         IFEQ      *ON
     C                   MOVE      CMSTAT        CHOCST
     C                   MOVE      CMIDT         CHEFDT
     C                   ENDIF                                                  *IN61=*ON
     C*
     C*  IF CLAIM IS CLOSED, OUTPUT 'C' TO WORK FILE
     C*  IF CLAIM IS OPEN, OUTPUT 'BLANK' TO WORK FILE
     C*
     C     CHOCST        IFEQ      'C'
     C*
     C                   MOVE      'C'           E2OPCL
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         E2CLDT
     C*
     C                   ELSE
     C*
     C*** Begin chg *** 08/13/03 ***************************************
     C***************    MOVE      *BLANK        E2OPCL
     C                   MOVE      'O'           E2OPCL
     C*** End chg ***** 08/13/03 ***************************************
     C                   Z-ADD     *ZERO         E2CLDT                         CHOCST=C
     C*
     C                   ENDIF
     C*
     C*** Begin add *** 09/18/03 **************************************
     C*  Retrieve the lost time value as of the run date.
     C*
     C     CHKY          SETGT     PTH01
     C     CHKY2         READPE    PTH01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVE      THTO          E2LSTM
     C                   ELSE
     C                   MOVE      CMLTFL        E2LSTM
     C                   ENDIF                                                  *IN99=*ON
     C*** End add ***** 09/18/03 **************************************
     C*
     C*  INITAILLY WRITE ALL RECORDS TO WFE2P (FOR CALC. PURPOSES),
     C*  USE E2PRT TO DISTINGUISH WHICH RECORDS SHOULD BE PRINTED
     C*
     C                   Z-ADD     CMCO#         E2CO#
     C                   Z-ADD     CMFND         E2FND
     C                   Z-ADD     CMEMP#        E2EMP#
     C                   Z-ADD     CMDIV         E2DIV
     C                   Z-ADD     X2AGT#        E2AGT#
      ***** BEG ADD ***** 02/20/03 *************************************
     C                   Z-ADD     X2PROD        E2PROD
      ***** END ADD ***** 02/20/03 *************************************
     C                   MOVE      CMSS#         E2SS#
      ***** BEG DEL ***** 01/26/07 ***********************************
      ******BEG ADD ***** 01/11/07 ***********************************
     C*********          IF        @@PRSS <> 'Y'
     C*********          MOVE      *BLANKS       E2SS#
     C*********          ENDIF
      ******END CHG ***** 01/11/07 ***********************************
      ***** END DEL ***** 01/26/07 ***********************************
      *
     C                   Z-ADD     CMFYR         E2FYR
     C                   Z-ADD     CMCLAS        E2CLAS
      ***** BEG ADD  **** 04/20/09 ************************************
     C                   MOVE      CMSPEC        E2SPEC
     C                   MOVEL     CMEXT         E2EXT
      ***** END ADD  **** 04/20/09 ************************************
     C                   Z-ADD     CMCASE        E2CASE
     C*** Begin dlt *** 09/18/03 **************************************
     C****************** MOVE      CMLTFL        E2LSTM
     C*** End dlt ***** 09/18/03 **************************************
     C                   MOVEL(P)  CMACDS        E2ACDS
     C*
     C*** BEG ADD****** 12/01/10 **************************************
     C                   MOVEL(P)  CMACD2        E2ACD2
     C*
     C*** END ADD****** 12/01/10 **************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         E2ACDT
     C*
     C                   MOVEL(P)  CMCTG1        E2CTG1
     C                   Z-ADD     DSDATE        E2RNDT
     C                   Z-ADD     X8RNDT        E2ASOF
     C                   Z-ADD     X8FRDT        E2FRDT
     C                   Z-ADD     X8TODT        E2TODT
     C                   Z-ADD     XXFPRM        E2FPRM
     C                   Z-ADD     XXTPRM        E2TPRM
     C*
     C*  GET CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          CHAIN     PCD01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL(P)  CDLSN         E2CLMN
     C                   CAT       ',':0         E2CLMN
     C                   CAT       CDFSN:1       E2CLMN
     C                   CAT       CDMID:1       E2CLMN
      ***** BEG ADD ******* 02/01/99 **********************************
     C                   MOVEL(P)  CDSEX         E2SEX
      *
      *  Age
      *
     C     CDDTOB        IFNE      *ZERO
      *
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DOB
      *
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##MDYY
      *
     C                   CALL      'SRDDIF8'
     C                   PARM                    ##DOB             8 0
     C                   PARM                    ##MDYY            8 0
     C                   PARM      *ZERO         ##DAYS            5 0
     C                   PARM      *BLANK        ##ERR             1
      *
     C     ##DAYS        DIV       365.25        E2AGE
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         E2AGE
      *
     C                   ENDIF                                                  CDDTOB
      *
      ***** END ADD ******* 02/01/99 **********************************
     C                   ELSE
      *
     C                   MOVEL     *BLANK        E2CLMN
     C*** Begin add *** 09/25/03 **************************************
     C                   MOVEL(P)  CMLSN         E2CLMN
     C                   CAT       ',':0         E2CLMN
     C                   CAT       CMFSN:1       E2CLMN
     C                   CAT       CMMID:1       E2CLMN
     C*** End add ***** 09/25/03 **************************************
      ***** BEG ADD ******* 02/01/99 **********************************
     C                   MOVEL(P)  *BLANKS       E2SEX
     C                   Z-ADD     *ZERO         E2AGE
      ***** END ADD ******* 02/01/99 **********************************
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   Z-ADD     EMRGN#        E2RGN#
     C                   MOVEL(P)  EMCNTY        E2CNTY
     C                   MOVEL     EMMZIP        E2ZIP
     C                   MOVEL(P)  EMNAME        E2EMNM
     C*
     C     EMMAD1        IFEQ      *BLANK
     C     EMPAD1        ANDNE     *BLANK
     C                   MOVEL(P)  EMPAD1        E2EAD1
     C                   MOVEL(P)  EMPAD2        E2EAD2
     C                   MOVEL(P)  EMPCTY        E2ECTY
     C                   MOVEL(P)  EMPST         E2EST
     C                   Z-ADD     EMPZIP        E2EZIP
     C                   ELSE
     C                   MOVEL(P)  EMMAD1        E2EAD1
     C                   MOVEL(P)  EMMAD2        E2EAD2
     C                   MOVEL(P)  EMMCTY        E2ECTY
     C                   MOVEL(P)  EMMST         E2EST
     C                   Z-ADD     EMMZIP        E2EZIP
     C                   ENDIF                                                  EMMAD1=*BLANKS
     C*
     C*  GET MOST CURRENT PAYROLL CLASS RECORD FOR DESCRIPTION
     C*
     C                   Z-ADD     CMCLAS        XXCLAS
     C                   Z-ADD     X7RNDT        XXEFDT
     C*
      ***** BEG ADD  **** 04/20/09 ************************************
     C     CMEXT         IFNE      ' '
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL(P)  PXDESC        E2CLDS
     C                   ENDIF                                                  *IN61=*OFF
     C                   ELSE
      ***** END ADD  **** 04/20/09 ************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  61
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL(P)  PRDESC        E2CLDS
     C                   ELSE
     C                   MOVE      *BLANK        E2CLDS
     C                   ENDIF                                                  *IN61=*OFF
      ***** BEG ADD  **** 04/20/09 ************************************
     C                   ENDIF                                                  CMEXT<>' '
      ***** END ADD  **** 04/20/09 ************************************
     C*
     C*  GET CLAIMS PAYMENT FILE FOR ACCUMULATING PAYMENTS THIS PERIOD
     C*
     C     PAKY2         CHAIN     PPA02                              61
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C                   SELECT
     C     PATYPE        WHENEQ    'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   Z-ADD     PAAMT         XXAMT
     C                   OTHER
     C                   Z-SUB     PAAMT         XXAMT
     C                   ENDSL
     C*
     C     PATRDT        IFLE      X7RNDT
     C*
     C                   SELECT
     C     PARETP        WHENEQ    'C'                                          COMP
     C                   ADD       XXAMT         E2PCMP
     C     PARETP        WHENEQ    'M'                                          MEDICAL
     C                   ADD       XXAMT         E2PMED
     C     PARETP        WHENEQ    'L'                                          LEGAL
     C                   ADD       XXAMT         E2PLEG
     C                   OTHER
     C                   ADD       XXAMT         E2POTH                         OTHER
     C                   ENDSL
     C*
     C                   ENDIF                                                  PATRDT<=X7RNDT
     C*
     C                   MOVEL     PATRDT        PACYM             5 0
     C*
     C     PACYM         IFEQ      XXCYM
     C*
     C                   SELECT
     C     PARETP        WHENEQ    'C'                                          COMP
     C                   ADD       XXAMT         E2ACMP
     C     PARETP        WHENEQ    'M'                                          MEDICAL
     C                   ADD       XXAMT         E2AMED
     C     PARETP        WHENEQ    'L'                                          LEGAL
     C                   ADD       XXAMT         E2ALEG
     C                   OTHER                                                  OTHER
     C                   ADD       XXAMT         E2AOTH
     C                   ENDSL
     C*
     C                   ENDIF                                                  PACYM=XXCYM
     C*
     C     PAKY2         READE     PPA02                                  61
     C*
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C*  GET RESERVE HISTORY FILE - ACCUMULATE RESERVE CHGS THIS PERIOD
     C*
     C     RHKY          CHAIN     PRH01                              61
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C*** BEGIN CHG *** 05/05/00 **************************************
     C********** RHDT      IFLE X7RNDT
     C     RHEFDT        IFLE      X7RNDT
     C*** END CHG ***** 05/05/00 **************************************
     C*
     C     1             DO        10            X                 2 0
     C*
     C                   MOVEL(P)  RHR(X)        XXKEY
     C*
     C*  GET RESERVE TYPE CODE FILE
     C*
     C     TBKY          CHAIN     PRT01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   SELECT
     C     RTTYPE        WHENEQ    'C'                                          COMP
     C                   ADD       RHS(X)        E2RCMP
     C     RTTYPE        WHENEQ    'M'                                          MEDICAL
     C                   ADD       RHS(X)        E2RMED
     C     RTTYPE        WHENEQ    'L'
     C                   ADD       RHS(X)        E2RLEG                         LEGAL
     C                   OTHER
     C                   ADD       RHS(X)        E2ROTH                         OTHER
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN61=*OFF
      **** BEGIN ADD **** 02/24/99 ************************************
      *
      *  Reserve changes this period
      *
     C*** BEGIN CHG *** 05/05/00 **************************************
     C******************** MOVELRHDT      RHCYM   50
     C                   MOVEL     RHEFDT        RHCYM             5 0
     C*** END CHG ***** 05/05/00 **************************************
      *
     C     RHCYM         IFEQ      XXCYM
      *
     C                   SELECT
     C     RTTYPE        WHENEQ    'C'                                          COMP
     C                   ADD       RHS(X)        E2CRSV
     C     RTTYPE        WHENEQ    'M'                                          MEDICAL
     C                   ADD       RHS(X)        E2MRSV
     C     RTTYPE        WHENEQ    'L'
     C                   ADD       RHS(X)        E2LRSV                         LEGAL
     C                   OTHER
     C                   ADD       RHS(X)        E2ORSV                         OTHER
     C                   ENDSL
      *
     C                   ENDIF                                                  RHCYM = XXCYM
      **** END   ADD **** 02/24/99 ************************************
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   ENDIF                                                  RHDT<=X7RNDT
     C*
     C     RHKY          READE     PRH01                                  61
     C*
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C*  DETERMINE SORT FIELD
     C*
     C                   SELECT
     C     @@SEQ         WHENEQ    'A'
     C                   MOVEL(P)  E2EMNM        E2SORT
     C     @@SEQ         WHENEQ    'N'
     C                   MOVEL(P)  E2EMP#        E2SORT
     C     @@SEQ         WHENEQ    'R'
     C                   MOVEL(P)  E2RGN#        E2SORT
     C     @@SEQ         WHENEQ    'C'
     C                   MOVEL(P)  E2CNTY        E2SORT
     C     @@SEQ         WHENEQ    'G'
     C                   MOVEL(P)  E2AGT#        E2SORT
     C     @@SEQ         WHENEQ    'Z'
     C                   MOVEL(P)  E2ZIP         E2SORT
     C     @@SEQ         WHENEQ    '1'
     C                   MOVEL(P)  XXSORT        E2SORT
     C                   ENDSL
     C*
     C*  CALL PGM TO CALCULATE DAYS SINCE CLAIM WAS CLOSED
     C*
     C     E2CLDT        IFNE      *ZERO
     C*
     C                   Z-ADD     E2CLDT        XXMDYY
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM      XXMDYY        ##DAT1
     C                   PARM      X8RNDT        ##DAT2
     C                   PARM      *ZEROS        ##DAYS            5 0
     C                   PARM      ' '           ##ERR             1
     C                   PARM      '1'           ##TERM            1
     C*
     C     ##DAYS        DIV(H)    30            E2MNTH            5 2
     C*
     C                   ENDIF                                                  E2CLDT<>*ZERO
     C*
     C*  EXCLUDE CLAIMS THAT FALL OUTSIDE ACCIDENT DATE RANGE
     C*
     C     CMACDT        IFLT      X7FRDT
     C     CMACDT        ORGT      X7TODT
     C                   MOVE      'N'           E2PRT
     C                   ELSE
     C                   MOVE      'Y'           E2PRT
     C                   ENDIF                                                  X7TODT<E2ACDT<X7FRDT
     C*
     C*  EXCLUDE CLAIMS THAT HAVE BEEN CLOSED MORE THAN
     C*  NUMBER OF MONTHS SPECIFIED ON PROMPT
     C*
     C     E2CLDT        IFNE      *ZERO
     C     XXMNTH        ANDNE     *ZERO
     C     E2MNTH        ANDGT     XXMNTH
     C     @@SELO        OREQ      'N'
     C*** Begin chg *** 08/13/03 ***************************************
     C**** E2OPCL        ANDEQ     *BLANK
     C     E2OPCL        ANDEQ     'O'
     C*** End chg ***** 08/13/03 ***************************************
     C     @@SELC        OREQ      'N'
     C     E2OPCL        ANDEQ     'C'
     C                   MOVE      'N'           E2PRT
     C                   ENDIF
      ***** BEG ADD ******* 02/01/99 **********************************
      *
     C                   MOVEL(P)  CMCTG2        E2CTG2                         Category 2
      *
      *  Department Code / Description
      *
     C                   MOVEL(P)  CMDEPT        E2DEPT                         Dept Code
      *
     C     DPKY          CHAIN     PDP01                              62
     C  N62              MOVEL(P)  DPDESC        E2DPDS
     C   62              MOVEL(P)  *BLANKS       E2DPDS
      *
      *  Chapter Code / Description
      *
     C     EMKY          CHAIN     PEM01                              62
     C     *IN62         IFEQ      *OFF
      *
     C                   MOVEL(P)  EMCHAP        E2CHAP                         Chap Code
      *
     C     CPKY          CHAIN     PCP01                              62
     C  N62              MOVEL(P)  CPDESC        E2CHDS
     C   62              MOVEL(P)  *BLANKS       E2CHDS
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       E2CHAP                         Chap Code
     C                   MOVEL(P)  *BLANKS       E2CHDS                         Chap Desc
     C                   ENDIF                                                  *IN62
      *
      ***** END ADD ******* 02/01/99 **********************************
     C***  BEGIN ADD  ***  05/03/00  *********************************
     C*  ACCUMULATE PREMIUM ACROSS DIVISIONS
     C*
     C                   CLEAR                   XXPREM
     C*
      ***** BEG CHG ***** 10/16/03 ***********************************
     C*****ELKY1         CHAIN     PEL01                              60
     C     ELKY1         CHAIN     PEL02                              60
      ***** END CHG ***** 10/16/03 ***********************************
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELAAPR        SUB       ELPRTD        XXAAPR
     C     ELANPR        SUB       ELPRTD        XXANPR
     C*
     C     ELFAF         IFEQ      'A'
     C                   ADD       XXAAPR        XXPREM
     C                   ELSE
     C                   ADD       XXANPR        XXPREM
     C                   ENDIF                                                  ELFAF=A
     C*
      ***** BEG CHG ***** 10/16/03 ***********************************
     C*****ELKY1         READE     PEL01                                  60
     C     ELKY1         READE     PEL02                                  60
      ***** END CHG ***** 10/16/03 ***********************************
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C***  END   ADD  ***  05/03/00  *********************************
      ***** BEG ADD ******* 05/29/01 **********************************
      *
      * New / Renewal
      *
     C*** BEGIN ADD *** 03/09/09 *******************************************
     C                   IF        ELNWRW=*BLANKS
     C*** END   ADD *** 03/09/09 *******************************************
     C     CMFYR         SUB       1             KKFYR
     C     ELKY2         CHAIN     PEL01                              60
     C  N60              MOVE      'R'           E2NWRW
     C   60              MOVE      'N'           E2NWRW
     C*** BEGIN ADD *** 03/09/09 *******************************************
     C                   ELSE
     C                   EVAL      E2NWRW=ELNWRW
     C                   ENDIF
     C*** END   ADD *** 03/09/09 *******************************************
      *
      ***** END ADD ******* 05/29/01 **********************************
      **** BEGIN ADD **** 02/24/99 ************************************
      *
      *  Policy from and to dates
      *
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
      *
      ***** BEG ADD ***** 10/16/03 ***********************************
      * Load new fields from WDELL
     C                   eval      e2exmd = elexm1
      *
     C                   if        elfaf  = 'A'
     C                   eval      e2wprm = elaapr
     C                   else
     C                   eval      e2wprm = elanpr
     C                   endif
      ***** END ADD ***** 10/16/03 ***********************************
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         E2PFDT
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         E2PTDT
     C                   ELSE
     C                   Z-ADD     *ZEROS        E2PFDT
     C                   Z-ADD     *ZEROS        E2PTDT
     C                   ENDIF                                                  *IN60 = *OFF
      *
      * Rating state
      *
     C     RSKY          CHAIN     PRS02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        E2RST
      *
      ***** BEG ADD ***** 10/16/03 ***********************************
     C                   EXSR      GETCLS                                       Get Governing Class
      ***** END ADD ***** 10/16/03 ***********************************
      *
     C                   ELSE
     C                   MOVE      *BLANK        E2RST
     C                   ENDIF                                                  *IN60 = *OFF
      *
      *  Jurisdiction state
      *
     C                   MOVE      CMJRST        E2JRST
      **** END   ADD **** 02/24/99 ************************************
     C**** BEGIN ADD **** 12/08/99 ************************************
     C*  Cancel date
     C                   Z-ADD     1             I1                3 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         E2CXDT
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C*  Agent federal id#
     C     AAKY          CHAIN     PAA01                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     AAFEIN        E2AGFE
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C*  Annual premium
     C***  BEGIN CHG  ***  05/03/00  *********************************
     C********** ELFAF     IFEQ 'A'
     C********** ELAAPR    SUB  ELPRTD    E2PREM
     C**********           ELSE
     C********** ELANPR    SUB  ELPRTD    E2PREM
     C**********           ENDIF
     C                   Z-ADD     XXPREM        E2PREM
     C***  END   CHG  ***  05/03/00  *********************************
     C*** Begin add *** 08/06/03 *************************************
     C*
     C                   MOVE      CMCO#         SRCO#
     C                   MOVE      CMFND         SRFND
     C                   MOVE      CMFYR         SRFYR
     C                   MOVE      CMEMP#        SREMP#
     C                   MOVE      CMDIV         SRDIV
     C*
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#             3
     C                   PARM                    SRFND             3
     C                   PARM                    SRFYR             3
     C                   PARM                    SREMP#            9
     C                   PARM                    SRDIV             5
     C                   PARM      @@RNDT        SRRNDT            8
     C                   PARM      *BLANK        SRAMT            11
     C                   PARM      *BLANK        SRAMT2           11
     C                   PARM      '1'           SRTERM            1
     C*
     C                   MOVE      SRAMT         E2EARN
     C*** End add ***** 08/06/03 *************************************
     C*
     C*  # of comp days lost
     C                   Z-ADD     *ZERO         XXDAYS            5 0
     C                   Z-ADD     *ZERO         XXWKS             5 0
     C                   Z-ADD     *ZERO         XXDYS             5 0
     C*
     C     PAKY2         SETLL     PPA02
     C     PAKY2         CHAIN     PPA02                              61
     C*
     C     *IN61         DOWEQ     *OFF
     C     PABR2         IFEQ      'TT'
     C     PABR2         OREQ      'TP'
     C     PABR2         OREQ      'PT'
     C     PATRDT        ANDLE     DSDATE
     C*
     C*** BEG DEL *** 07/17/03 **************************************
     C**** PATYPE        IFNE      'V'
     C**** PATYPE        ANDNE     'S'
     C**** PATYPE        ANDNE     'R'
     C****               ADD       PAWKS         XXWKS
     C****               ADD       PADAYS        XXDYS
     C****               ELSE
     C****               SUB       PAWKS         XXWKS
     C****               SUB       PADAYS        XXDYS
     C****               ENDIF
     C*** END DEL *** 07/17/03 **************************************
     C*** BEG ADD *** 07/17/03 **************************************
     C     PATYPE        IFEQ      'A'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'P'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C                   ADD       PAWKS         XXWKS
     C                   ADD       PADAYS        XXDYS
     C                   ELSE
     C                   SUB       PAWKS         XXWKS
     C                   SUB       PADAYS        XXDYS
     C                   ENDIF
     C*** END ADD *** 07/17/03 **************************************
     C*
     C                   ENDIF
     C     PAKY2         READE     PPA02                                  61
     C                   ENDDO
     C*
     C     XXWKS         MULT      7             XXDAYS
     C                   ADD       XXDYS         XXDAYS
     C*
     C                   Z-ADD     XXDAYS        E2CMP
     C*
      **** END   ADD **** 12/08/99 ************************************
      *
      ***** BEG ADD ***** 10/16/03 ***********************************
      * Load Prompt Screen Selections in output fields
     C                   EVAL      E2FPDT = XXFPDT
     C                   EVAL      E2TPDT = XXTPDT
     C                   EVAL      E2IEMP = XXSELN
      ***** END ADD ***** 10/16/03 ***********************************
     C*
     C*** Begin add *** 06/23/04 *************************************
     C*  Load SIC code and description.
     C*
     C                   eval      e2sic = emsic
     C*
     C                   movel(p)  'SIC'         xxtbid
     C                   movel(p)  emsic         xxkey
     C     gtky          chain     pgt01                              99
     C  N99              movel     gtdesc        e2sicd
     C*
     C*  Load Underwriter's staff code and name
     C*
     C                   eval      e2uwtr = emuwtr
     C     emuwtr        chain     pup01                              99
     C                   movel(p)  upusnm        e2uwnm
     C*** End add ***** 06/23/04 *************************************
     C*** beg add ***** 05/12/09 ************************************
     c                   eval      E2CAR# = svcar#
     C*** end add ***** 05/12/09 ************************************
     C*
     C                   ENDSR                                                  LOADE2
     C/EJECT
      *
      ***** BEG ADD ***** 10/16/03 ***********************************
      ** ========================================================== **
      *  GETCLS - Get Governing Class Code
      ** ========================================================== **
     C     GETCLS        BEGSR
      *
     C                   CLEAR                   XPCLS
     C                   CLEAR                   ZPCLS
     C                   CLEAR                   XPAMT            13 2
     C                   CLEAR                   ZPAMT            13 2
     C                   CLEAR                   E2GCLS
     C                   CLEAR                   E2GCLD
      *
     C     EPKY          CHAIN     PEP01                              60
      *
     C                   DOW       NOT *IN60
      * If one of these class codes capture largest payroll amt & class
     C                   IF        EPCLAS = 7380 OR EPCLAS = 8742 OR
     C                             EPCLAS = 8748 OR EPCLAS = 8810 OR
     C                             EPCLAS = 8871
     C                   IF        ELFAF = 'A' AND EPAAPR > ZPAMT
     C                   EVAL      ZPCLS = EPCLAS
     C                   EVAL      ZPAMT = EPAAPR
     C                   ENDIF                                                  ELFAF = A
     C                   IF        ELFAF <> 'A' AND EPEAPR > ZPAMT
     C                   EVAL      ZPCLS = EPCLAS
     C                   EVAL      ZPAMT = EPEAPR
     C                   ENDIF                                                  ELFAF <> A
      *
      * Else capture largest payroll amt and class of ones not in above classes
     C                   ELSE
      *
     C                   IF        ELFAF = 'A' AND EPAAPR > XPAMT
     C                   EVAL      XPCLS = EPCLAS
     C                   EVAL      XPAMT = EPAAPR
     C                   ENDIF                                                  ELFAF = A
     C                   IF        ELFAF <> 'A' AND EPEAPR > XPAMT
     C                   EVAL      XPCLS = EPCLAS
     C                   EVAL      XPAMT = EPEAPR
     C                   ENDIF                                                  ELFAF <> A
     C                   ENDIF                                                  EPCLAS <> 7380
      *
     C     EPKY          READE     PEP01                                  60
     C                   ENDDO                                                  do NOT *IN60
      *
      * Get The Governing Class code and then get description from WMPRL1
     C                   IF        XPAMT > *ZEROS
     C                   EVAL      E2GCLS = XPCLS
     C                   ELSE
     C                   EVAL      E2GCLS = ZPCLS
     C                   ENDIF
      *
      ***** BEG ADD  **** 04/20/09 ************************************
     C     CMEXT         IFNE      ' '
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL(P)  PXDESC        E2GCLD
     C                   ENDIF                                                  *IN61=*OFF
     C                   ELSE
      ***** END ADD  **** 04/20/09 ************************************
     C     PRKY2         SETGT     PPR02
     C     PRKY3         READPE    PPR02                                  60
      *
     C                   IF        NOT *IN60
     C                   MOVEL(P)  PRDESC        E2GCLD
     C                   ELSE
     C                   MOVE      *BLANKS       E2GCLD
     C                   ENDIF
      ***** BEG ADD  **** 04/20/09 ************************************
     C                   ENDIF                                                  CMEXT<>' '
      ***** END ADD  **** 04/20/09 ************************************
      *
     C                   ENDSR                                                  GETCLS
      ***** END ADD ***** 10/16/03 ***********************************
