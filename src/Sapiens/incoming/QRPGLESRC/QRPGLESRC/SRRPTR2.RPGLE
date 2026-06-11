     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *****************************************************************
      *****************************************************************
      *TITLE SRRPTR - EMPLOYER REPORT WORKFILE BUILD
      *****************************************************************
      ** PROGRAM DESCRIPTION:                                         *
      **                                                              *
      **     This program creates a workfile of Employer information  *
      ** based on selection criteria sent in the ENTRY parameters.    *
      ** These parameters are as follows:                             *
      **   Company                                                    *
      **   Fund                                                       *
      **   Start Date                                                 *
      **   End Date                                                   *
      **   Run Date                                                   *
      **   Run Date Status (A = Active Only, C = Cancelled Only,      *
      **                    B = Both)                                 *
      **   Employer Type (E = Estimating, S = Self Reporting,         *
      **                  B = Both)                                   *
      **   Account Status (A = Active Only, C = Cancelled Only,       *
      **                   B = Both, E = Pol Eff Falls Within From/To,*
      **                   R = Sel Pol with one  A/R billing trans    *
      **                       posted within From/To but <canx dt,    *
      **                   X = Pol Exp date, ELPTDT, falls within     *
      **                       From/To date range)                    *
      **                   I = Issue date, falls within     *
      **                       From/To date range)                    *
      **   Primary Sort (EN = Employer/Division                       *
      **                 EA = Employer Alpha                          *
      **                 ER = Region                                  *
      **                 EC = Chapter                                 *
      **                 EY = County                                  *
      **                 EZ = Employer Zip                            *
      **                 AN = Agent #                                 *
      **                 AA = Agent Alpha                             *
      **                 AZ = Agent Zip                               *
      **                 PR = Premium Range)                          *
      **                 SN = Secondary Agent #
      **                 SA = Secondary Agent Alpha
      **                 UW = Underwriter
      **   Secondary Sort (Codes are the same as the Primary Sort)    *
      **                '  ' is also valid                            *
      **   Employer Mailing/Physical Address Flag (M = Mailing,       *
      **                                           P = Physical,      *
      **                                           E = EMMPFL,        *
      **                                           A = WMA3P 3rd add) *
      **   Agent Mailing/Physical Address Flag (M = Mailing,          *
      **                                        P = Physical)         *
      **   Premium Type (RX = Earned Prem (excl Exp Const)            *
      **                 RI = Earned Prem (incl Exp Const)            *
      **                 EX = Est Annual Prem (excl Exp Const)        *
      **                 EI = Est Annual Prem (incl Exp Const)        *
      **                 PX = Prorata Prem (excl Exp Const)           *
      **                 PI = Prorata Prem (incl Exp Const)           *
      **                 AX = Audit Prem (excl Exp Const)             *
      **                 AI = Audit Prem (incl Exp Const))            *
      **   Employer #                                                 *
      **   Division                                                   *
      **   Fund year                                                  *
      **   Range 1 Type (EN = Employer/Division                       *
      **                 EA = Employer Alpha                          *
      **                 ER = Region                                  *
      **                 EC = Chapter                                 *
      **                 EY = County                                  *
      **                 EZ = Employer Zip                            *
      **                 AN = Agent #                                 *
      **                 AA = Agent Alpha                             *
      **                 AZ = Agent Zip                               *
      **                 PR = Premium Range)                          *
      **   Range 1 From Value                                         *
      **           ** NOTE ** (Alpha values MUST be LEFT-Justified    *
      **                & Numeric values MUST be Right-Justified      *
      **                Also if numeric, pass only whole numbers!!!   *
      **   Range 1 To Value  see note above                           *
      **   Range 2 Type   (Codes are the same as the Primary Sort)    *
      **   Range 2 From Value see note above                          *
      **   Range 2 To Value  see note above                           *
      **                                                              *
      ** Employers will be selected as follows:                       *
      **                                                              *
      ** If the Employer # parameter is not zeroes, then the program  *
      ** load the workfile with only the one record specified by the  *
      ** Employer #, Division and Fund Year which were passed in the  *
      ** parameters.  If the Employer # is not zeroes and the Fund    *
      ** Year is zeroes, then the program will process all detail     *
      ** records for that Employer, according to the ranges and       *
      ** restrictions specified by the other parameters (see next     *
      ** paragraph).  Otherwise, the program will process as follows: *
      **                                                              *
      ** The Employer Detail File will be read in Policy From Date    *
      ** order for the selected Company and Fund.  The program will   *
      ** start reading the file at a date 1 year prior to the         *
      ** selected Start Date, unless a Maximum Extended Year Policy   *
      ** value has been entered in Fund Master Maintenance.  In that  *
      ** case, the program will start reading the Employer Detail     *
      ** File at a date computed by subtracting the number of months  *
      ** specified by the Maximum Extended Year Policy (FNMXEX) from  *
      ** the selected Start Date.  The file will be read until a      *
      ** record with a Policy Effective Date greater than the         *
      ** selected End Date is encountered.  For each record read, the *
      ** program will compare the Billing Schedule Code type to the   *
      ** selected Type (Estimating, Self-Reporting or Both) to see if *
      ** the record should be included in the workfile (WXEBP).  If   *
      ** it is a valid record, it will then check the Account Status. *
      ** If Status "A" (Active Only) was chosen, then only those      *
      ** Policies that do not have a Cancel Date that falls within    *
      ** the selected Date Range will be included.  If Status "C"     *
      ** (Cancelled Only) was chosen, then only those Policies that   *
      ** have a Cancel Date that falls within the selected Date Range *
      ** will be included.  If Status "B" (Both) was chosen, then any *
      ** Policy that was active during ANY PORTION of the Date Range  *
      ** will be included. If Status "E" (Pol Eff Dt Range) was chosen*
      ** any policy with its Effective Date being greater than or     *
      ** equal to the From Date, and less than or equal to the To Date*
      ** will be included. If Status "X" (Pol Exp Dt Range) was chosen*
      ** only policies whose expiration date falls within the selected*
      ** range will be included. If Status "R" then only policies     *
      ** which have at least one A/R billing transaction (B,A,H,D,F)  *
      ** posted within the selected range (and the post date is less  *
      ** than the canx date) will be included.                        *
      **                   If a Run Date is specified, the Account    *
      ** Status checked against the Run Date Status.  If Run Date     *
      ** Status is "A", the Account must be active on the Run Date,   *
      ** "C" the Account must be Cancelled as of that date, "B" will  *
      ** allow for Both Active & Cancelled. There are two sets of     *
      ** Range fields.  They will allow the workfile build program    *
      ** to further whittle down the extracted data.  Enter the       *
      ** appropriate Selection code, and a low and high range value.  *
      **                                                              *
      ** *** NOTE *** Alpha range data must be left justified, and    *
      **              numeric data right justified.  Also numeric     *
      **              data must be passed as a WHOLE number.          *
      **                                                              *
      ** The other parameters control the output.  The Primary and    *
      ** Secondary sort codes are used to determine how to load the   *
      ** sort fields (40 Alpha) in the output file (the codes are     *
      ** described above).  These sort fields are used in the logical *
      ** to control the order in which the records are accessed.  The *
      ** last two paramters are the Employer and Agent Mailing/       *
      ** Physical Address Flags.  These Flags are used to specify     *
      ** from which address the zip will be taken to load the output  *
      ** file. If Flag "M" then use the mailing address. If Flag "P"  *
      ** then use the physical address. The employer flag can also    *
      ** have a value of "A" use the employer third address file for  *
      ** the zip.                                                     *
      **                                                              *
      ** NOTE:  The CL which calls this program creates a duplicate   *
      ** object of WXEBP and WXEBL in QTEMP and overrides to that     *
      ** copy.                                                        *
      **                                                              *
      ** CALLED FROM:                                                 *
      **    SRRPTC                                                    *
      **                                                              *
      ** CALLS TO:                                                    *
      **    (NONE)                                                    *
      **                                                              *
      **                                                              *
      ** PROGRAMMER: C:35                                             *
      ** CREATED:    06/25/96         PROJECT: CM019                  *
      **                                                              *
      *****************************************************************
      * REVISIONS:                                                    *
     H*                                                               *
     H* 01/10/18 C:33  Support 63651                                  *
     H*                Clear agt# before WA002R call                  *
      *                                                               *
      * 07/20/16  C:310  Project: WC2443                              *
      *                  - Changed the parameter list                 *
      *                  - Changed the logic to create report on the  *
      *                    basis of groups that were selected on      *
      *                    group select screen.                       *
      * 11/01/04  C:132  Project: WA129  Seq: 44290                   *
      *                  Change to allow policy start/end dates out   *
      *                  of date range when Issue date is requested.  *
      *                                                               *
      * 10/27/03  C:33  Project: WC1132                               *
      *                 Allow for single employer, multiple divisions *
      *                                                               *
      * 01/08/03  C:142 Project WC1029                                *
      *                Added changed from SRRPTR on 11/20/03          *
      *                                                               *
      * 01/24/01  C:33  SEQ#: WA033/13606                             *
      *                -IF INTIAL SORT IS AGENT ALPHA AND NO SECONDARY*
      *                 SORT IS SPECIFIED, FORCE SECONDARY SORT TO BE *
      *                 AGENT #.                                      *
      *                -IF INTIAL SORT IS AGENT ALPHA AND SECONDARY   *
      *                 SORT IS NOT AGENT #, FORCE THE AGT# INTO THE  *
      *                 LAST POSITIONS OF EBSRT1.                     *
      *                                                               *
      * 10/13/99  C:65  PROJECT WC942                                 *
      *                 INCLUDE FULL EXPENSE CONSTANT IN NORMAL PREM  *
      *                                                               *
      * 05/18/99 C:61 PROJECT WC577                                   *
      *               Change array index to allow sort by             *
      *               Premium Range (PR).                             *
      *                                                               *
      * 04/07/99 C:90 PROJECT: 96WAD                                  *
      *               DETERMINE IF POLICY IS ACCOUNT CURRENT PLAN.    *
      *                                                               *
      * 12/10/98 C:61 SUPPORT 14595                                   *
      *               Only process active (WMA3P/A3STAT = 'A')        *
      *               Employer 3rd Address records.                   *
      *                                                               *
      * 02/16/98 C:35 PROJECT: WC577                                  *
      *               CODE WAS OVERRIDING VALUE PASSED IN QQEMPF      *
      *               (EMPLOYER MAILING/PHYSICAL ADDRESS FLAG).       *
      *               CHANGED CODE SO THAT IF 'E' IS PASSED, THE      *
      *               PROGRAM WILL USE THE VALUE IN EMPLOYER MASTER   *
      *               (EMMPFL).                                       *
      *                                                               *
      * 12/11/97 C:87 PROJECT CM033                                   *
      *               IF QQSTAT = 'R', PROGRAM WAS ONLY READING ACCT/ *
      *                RCVBL FILE (WMEAP) FOR POST DATE = STRT DATE.  *
      *                ADDED 'KEY' AND CHGD READS TO FILE TO INCLUDE  *
      *                POST DATES BETWEEN START AND END DATES.        *
      *                                                               *
      * 12/01/97 C:87 PROJECT CM031, CM032                            *
      *               IF PARM QQSTAT (ACCOUNT STATUS) = 'R' :         *
      *                PROVIDE SELECTION BY ACCOUNTS RECEIVABLE/ POST *
      *                -ING DATES. WRITE TO WXEBP IF POLICY HAS A     *
      *                'B','A','D','H', OR 'F' WITHIN DATE RANGE, BUT *
      *                IGNORING TRANS POSTED AFTER THE CANX DATE.     *
      *                ADDED EBPTDT TO BE WRITTEN TO WXEBP.           *
      *               IF PARM QQSTAT = 'X':                           *
      *                ADD CODE SO THAT ONLY THOSE ACCOUNTS WHOSE     *
      *                POLICY EXPIRATION DATE FALLS WITHIN SELECTED   *
      *                RANGE ARE INCLUDED.                            *
      *               IF THE EMPLOYER'S THIRD ADDRESS IS NOT BLANK,   *
      *                USE IT IN PLACE OF THE EMPLOYER ADD IN WMEMP   *
      *                                                               *
      *****************************************************************
      * INDICATORS USED:                                              *
      *                                                               *
      * 60 - CHAIN INDICATOR FOR FUND MASTER FILE                     *
      * 61 - READE INDICATOR FOR EMPLOYER DETAIL FILE                 *
      * 62 - EMPLOYER TYPE MATCHES SELECTED CRITERIA                  *
      * 63 - CHAIN INDICATOR FOR BILLING SCHEDULE CODE TABLE FILE     *
      * 64 - EMPLOYER HAS CANCEL DATE WITHIN SELECTED DATE RANGE      *
      * 65 - CHAIN INDICATOR FOR EMPLOYER MASTER FILE                 *
      * 66 - EFFECTIVE/CANCEL DATE LOOK-UP INDICATOR                  *
      * 67 - EMPLOYER STATUS MATCHES SELECTED CRITERIA FOR PERIOD     *
      * 68 - REDPE INDICATOR FOR AGENT HISTORY FILE                   *
      * 69 - CHAIN INDICATOR FOR AGENT ASSIGNMENT FILE                *
      * 70 - CHAIN INDICATOR FOR AGENT MASTER FILE                    *
      * 71 - INDICATOR FOR SORT TYPE LOOK-UP                          *
      * 72 - EMPLOYER STATUS MATCHES SELECTED CRITERIA FOR RUN DATE   *
      * 73 - RANGE 1 SELECTION WAS OKAY                               *
      * 74 - RANGE 2 SELECTION WAS OKAY                               *
      * 75 - TESTN INDICATOR                                          *
      * 76 - CHAIN INDICATOR FOR EMPLOYER THIRD ADDRESS FILE          *
      * 77 - CHAIN INDICATOR FOR EMPLOYER ACCOUNTS RECEIVABLE FILE    *
      * 78 - CHAIN INDICATOR FOR EMPLOYER WORKFILE                    *
      * 79 - IF XXSTAT = 'X' THEN *ON ELSE *OFF                       *
      * 80 - IF XXSTAT = 'R' THEN *ON ELSE *OFF                       *
      *                                                               *
      *****************************************************************
      ** FILE USAGE:                                                  *
      **                                                              *
      ** DISPLAY FILES:                                               *
      **    (NONE)                                                    *
      **                                                              *
      ** DATA FILES:                                                  *
      **    WDELL  (PEL00): CO#/FND/EMP#/DIV/FYR                      *
      **    WDELL1 (PEL01): CO#/FND/FYR/EMP#/DIV                      *
      **    WDELL3 (PEL03): CO#/FND/PFDT                              *
      **    WDELL4 (PEL04): CO#/FND/PTDT                              *
      **    WMEML  (PEM01): CO#/FND/EMP#/DIV                          *
      **    WMFNL  (PFN01): CO#/FND                                   *
      **    WMAHL  (PAH01): CO#/FND/EMP#/DIV/FYR                      *
      **    WMAAL1 (PAA01): CO#/FND/AGT#                              *
      **    WMAGL  (PAG01): CO#/FEIN/MOD                              *
      **    WXEBP  (PEB01): ------------                              *
      **    WXEBL1 (PEB02): CO#/FND/FYR/EMP/DIV                       *
      **    WTABL  (PBS01): CO#/KEY                                   *
      **    WMA3L  (PA301): CO#/FND/EMP#/DIV                          *
      **    WMEAL11(PEA01): CO#/FND/PODT                              *
      **                                                              *
      *****************************************************************
      *
     F***  Begin Chg  ***  10/27/03  **************************************
     F**********WDELL3    IF   E           K DISK
     F**********WDELL4    IF   E           K DISK
     F***********                          RENAME(PEL01:PEL03)
     F**********WDELL     IF   E           K DISK
     F**********                           RENAME(PEL01:PEL02)
     FWDELL     IF   E           K DISK    RENAME(PEL01:PEL00)
     FWDELL1    IF   E           K DISK    RENAME(PEL01:PEL01)
     FWDELL3    IF   E           K DISK    RENAME(PEL01:PEL03)
     FWDELL4    IF   E           K DISK    RENAME(PEL01:PEL04)
     F***  End   Chg  ***  10/27/03  **************************************
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWXEBP     O  A E           K DISK
      *
     FWXEBL1    IF   E           K DISK
     F                                     RENAME(PEB01:PEB02)
     FWMEAL11   IF   E           K DISK
     FWMA3L     IF   E           K DISK
      *
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWMRSL     IF   E           K DISK
      *
     FWDEHL12   IF   E           K DISK
     F***** BEG-ADD ***** 07/20/16 *****************************************
     FWXGSL     IF   E           K DISK
     F***** END-ADD ***** 07/20/16 *****************************************
      *
     D SK              S              2    DIM(16) CTDATA PERRCD(16)            SORT KEY TABLE
     D SR              S             40    DIM(16)                              SORT TABLE
      *
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
      *
     D                 DS
     D  SRTS1                  1    160
     D  EBEMP#                 1      9  0
     D  EBDIV                 10     14  0
     D  XXEMP                  1     14  0
     D  EBENAM                41     80
     D  EBRGN#                81     83  0
     D  EBCHAP               121    123  0
      *
     D                 DS
     D  SRTS2                  1    200
     D  EBJRST                 1      2
     D  EBCNTY                 3      4
     D  EBEZIP                41     45  0
     D  EBAGT#                81     85  0
     D  EBANAM               121    160
     D  EBAZIP               161    165  0
     D                 DS
     D  SRTS3                  1     40
     D  XXPREM                 1     11  2
      *
     D                 DS
     D  SRT16A                 1     40
     D  Isdate16a              1      7
     D  EMP#16a               32     40  0
      *
     D                 DS
     D  SRT16B                 1     40
     D  Isdate16b              1      7
      *
     D                 DS
     D  SRT15A                 1     49
     D  XXSNMA                 1     40
     D  EMP#7                 41     49  0
      *
     D                 DS
     D  SRT15B                 1     40
     D  XXSNMB                 1     40
      *
     D                 DS
     D  SRT14A                 1     40
     D  XXSAGA                 1      5
     D  EMP#8                  6     14  0
      *
     D                 DS
     D  SRT14B                 1     40
     D  XXSAGB                 1      5
      *
      * FOR NEW SORTS EX,UW, AND ST, USE DS WITH PREFIX "A" IN PRIMARY
      * SORT IF THERE IS NO SECONDARY SORT. IF THERE IS A SCONDARY SORT
      * USE PREFIX "B" IN PRIMARY SORT AND PREFIX "A" IN SECONDARY SORT
      * THE PURPOSE OF THIS METHOD IS TO HAVE EMPLOYER NUMBER AS THE
      * MINOR SORT IN EACH CASE.
      *
     D                 DS
     D  SRT13A                 1     40
     D  JRST                   1      2
     D  EMP#4                  3     11  0
      *
     D                 DS
     D  SRT13B                 1     40
     D  JRST2                  1      2
      *
     D                 DS
     D  SRT12A                 1     40
     D  EMUWTR                 1      3
     D  EMP#5                  4     12  0
      *
     D                 DS
     D  SRT12B                 1     40
     D  EMUWT2                 1      3
      *
     D                 DS
     D  SRT11A                 1     40
     D  ELPTD1                 1      7  0
     D  EMP#6                  8     16  0
      *
     D                 DS
     D  SRT11B                 1     40
     D  ELPTD2                 1      7  0
      *
     D                 DS
     D  XXDATE                 1      7  0
     D  XXC                    1      1  0
     D  XXYY                   2      3  0
     D  XXMM                   4      5  0
      *
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP5                 1      5  0
     D  XXZIP4                 6      9  0
      *
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
      *
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXFRGA                 1     15
     D  XXFRGN                 1     15  0
     D  XXFEMP                 2     15  0
     D                 DS
     D  XXTRGA                 1     15
     D  XXTRGN                 1     15  0
     D  XXTEMP                 2     15  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D***** BEG-ADD ***** 07/20/16 ************************************
     D charGrpSlct     S              1A
     D***** END-ADD ***** 07/20/16 ************************************
      *****************************************************************
      *
      *  KEY LISTS FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPFDT
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPTDT
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     yrNoDivELKY   KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     yrELKY        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     noDivELKY     KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C***  End   Add  ***  10/27/03  **************************************
      *****************************************************************
      *
      *  KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *****************************************************************
      *
      *  KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    STRFND
     C***** BEG-ADD ***** 07/20/16 *****************************************
      *  KEY LIST FOR FUND MASTER FILE
      *****************************************************************
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
      *****************************************************************
      *  KEY LIST FOR FUND WORK FILE
      *****************************************************************
     C     XGKY          KLIST
     C                   KFLD                    GsCO#
     C                   KFLD                    GsFnd
     C***** END-ADD ***** 07/20/16 *****************************************
      *****************************************************************
      *
      *  KEY LIST FOR THE AGENT HISTORY FILE
      *
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *****************************************************************
      *
      *  KEY LIST FOR THE AGENT ASSIGNMENT FILE
      *
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
      *
     C     AAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSAG#
      *****************************************************************
      *
      *  KEY LIST FOR THE AGENT MASTER FILE
      *
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *****************************************************************
      *
      *  KEY LIST FOR THE BILLING SCHEDULE CODE TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *****************************************************************
      *
      *  KEY LIST FOR THE RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
      *
      *  KEY LIST FOR THE EMPLOYER ACCOUNTS RECEIVABLE FILE
      *
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPODT
      *
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *****************************************************************
      *
      *  KEY LIST FOR THE WORKFILE
      *
     C     EBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *****************************************************************
      *
      *  KEY LIST FOR THE THIRD ADDRESS FILE
      *
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *****************************************************************
      *
      *  KEY LIST FOR THE EMPLOYER DETAIL HISTORY FILE
      *
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      /EJECT
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    ELPFDT        XXTRDT
     C     *LIKE         DEFINE    ELPFDT        XXSTDT
     C     *LIKE         DEFINE    ELPFDT        XXENDT
     C     *LIKE         DEFINE    ELPFDT        XXRNDT
     C     *LIKE         DEFINE    ELPFDT        XXSTDX
     C     *LIKE         DEFINE    ELPFDT        XXASOF
     C     *LIKE         DEFINE    ELPFDT        ISDATE
     C     *LIKE         DEFINE    AGFEIN        XXFEIN
     C     *LIKE         DEFINE    AGMOD         XXMOD
     C     *LIKE         DEFINE    AGNAME        SECNAM
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    AHSAGT        XXSAG#
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    ELPTDT        XXPTDT
     C     *LIKE         DEFINE    EAPODT        XXPODT
      *
      ****** BEG ADD ***** 11/01/04 ***************************************
     C     *LIKE         DEFINE    ELPFDT        XXEIDT
      ****** END ADD ***** 11/01/04 ***************************************
      *
     C                   MOVE      *BLANKS       XXSRT1            2
     C                   MOVE      *BLANKS       XXSRT2            2
     C                   MOVE      *BLANK        XXSTAT            1
     C                   MOVE      *BLANK        XXTYPE            1
     C                   MOVE      *BLANK        XXEMPF            1
     C                   MOVE      *BLANK        XXAMPF            1
      *
     C                   Z-ADD     0             X1STDT            8 0
     C                   Z-ADD     0             XXSTDX
     C                   Z-ADD     0             X1ENDT            8 0
     C                   Z-ADD     0             XXYRS             2 0
     C                   Z-ADD     0             XXMTHS            2 0
     C                   Z-ADD     0             Y                 2 0
     C                   Z-ADD     0             X1RNDT            8 0
     C                   Z-ADD     0             XXASOF
      *
     C                   MOVE      *BLANK        XXRDST            1
     C                   MOVE      *BLANK        XXPRTY            2
     C                   MOVE      *BLANK        XXSEL             2
     C                   MOVEL     *HIVAL        HIVAL            15
      *
      *****************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND1            3
     C                   PARM                    QQFND2            3
     C                   PARM                    QQSTDT            8
     C                   PARM                    QQENDT            8
     C                   PARM                    QQRNDT            8            RUN DATE
     C                   PARM                    QQRDST            1            RUN DT STS
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQSTAT            1
     C                   PARM                    QQSRT1            2
     C                   PARM                    QQSRT2            2
     C                   PARM                    QQEMPF            1
     C                   PARM                    QQAMPF            1
     C                   PARM                    QQPRTY            2            PREM TYPE
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQSEL1            2            SEL 1
     C                   PARM                    QQS1FR           15            LOW VAL
     C                   PARM                    QQS1TO           15            HI VAL
     C                   PARM                    QQSEL2            2            SEL2
     C                   PARM                    QQS2FR           15            LOW VAL
     C                   PARM                    QQS2TO           15            HI VAL
     C***** BEG-ADD ***** 07/20/16 *****************************************
     C                   PARM                    QQGRPSLCT         1            HI VAL
     C***** END-ADD ***** 07/20/16 *****************************************
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
      *
      *  DEFINE PLIST FOR SRPRT1 & SRPRT2
      *
     C     PLEARN        PLIST
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    SRDATE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
      *
     C***** BEG-ADD ***** 07/20/16 *****************************************
     C                   If        %Parms > 23
     C                   MOVE      QQGRPSLCT     charGrpSlct
     C                   ENDIF
     C***** END-ADD ***** 07/20/16 *****************************************
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSTDT        X1STDT
      *
     C                   Z-ADD     X1STDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSTDT
     C                   MOVE      QQENDT        X1ENDT
     C                   Z-ADD     X1ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
      *
     C                   MOVE      QQTYPE        XXTYPE
     C                   MOVE      QQSTAT        XXSTAT
     C                   MOVE      QQSRT1        XXSRT1
     C                   MOVE      QQSRT2        XXSRT2
     C                   MOVE      QQEMPF        XXEMPF
     C                   MOVE      QQAMPF        XXAMPF
      *
      ****** BEG ADD ***** 11/01/04 ***************************************
      * If Issue date, change Ending Date to be one year later
     C                   MOVEL     SRDTO         XXEIDT
     C                   IF        xxstat  = 'I'
     C                   eval      XXENDT  = XXENDT + 10000
     C                   ENDIF
      ****** END ADD ***** 11/01/04 ***************************************
      *
     C     QQEMP#        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        QQEMP#
     C                   ENDIF                                                  QQEMP# IFEQ
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C*****QQDIV         IFEQ      *BLANKS
     C**********         MOVE      *ZEROS        QQDIV
     C**********         ENDIF                                                  QQDIV IFEQ
     C***  End   Del  ***  10/27/03  **************************************
      *
     C     QQFYR         IFEQ      *BLANKS
     C                   MOVE      *ZEROS        QQFYR
     C                   ENDIF                                                  QQFYR IFEQ
      *
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
      *
     C                   MOVEL     QQRNDT        X1RNDT
      *
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      *
     C                   MOVEL     QQRDST        XXRDST
     C                   MOVEL     QQPRTY        XXPRTY
      /EJECT
      *****************************************************************
      * MAIN PROCESS
      *****************************************************************
     C**** BEG-ADD ***** 07/20/16 *************************************
     C                   IF        charGrpSlct <> 'Y'
     C**** END-ADD ***** 07/20/16 *************************************
      *
     C     QQFND1        IFEQ      *BLANKS
     C     QQFND1        OREQ      '000'
     C                   MOVE      *ZERO         STRFND            3 0
     C                   ELSE
     C                   MOVE      QQFND1        STRFND
     C                   ENDIF
      *
     C     QQFND2        IFEQ      *BLANKS
     C     QQFND2        OREQ      '000'
     C                   MOVE      *ZERO         ENDFND            3 0
     C                   ELSE
     C                   MOVE      QQFND2        ENDFND
     C                   ENDIF
      *
     C     STRFND        IFEQ      *ZERO
     C     ENDFND        ANDEQ     *ZERO
     C                   MOVE      1             STRFND
     C                   MOVE      999           ENDFND
     C                   ENDIF
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C                   if        strFnd > endFnd
     C                   eval      endFnd = strFnd
     C                   endif
     C***  End   Add  ***  10/27/03  **************************************
      *
      * READ FUND MASTER FILE
     C     FNKY          SETLL     PFN01
      *
     C     ENDFND        DOUEQ     FNFND
     C                   READ      PFN01
      *
     C                   IF        %Eof
     C                   Leave
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVE      FNFND         XXFND
     C                   EXSR      PRFUND
      *
     C                   ENDDO                                                  *IN60 IFEQ
     C***** BEG-ADD ***** 07/20/16 *****************************************
     C                   ELSE
     C     FNKY1         SETLL     PFN01
     C                   READ      PFN01
     C                   DOW       NOT %EOF
     C                   Eval      GsCo# = FnCo#
     C                   Eval      GsFnd = Fnfnd
     C     XGKY          chain     PGS01
     C                   If        %Found and GsIEFl ='I'
     C                   MOVE      GsFnd         XXFND
     C                   EXSR      PRFUND
     C                   ENDIF
     C                   READ      PFN01
     C                   ENDDO                                                  *IN60 IFEQ
     C                   EVAL      charGrpSlct ='N'
     C                   ENDIF
     C***** END-ADD ***** 07/20/16 *****************************************
      *
      *  END THE PROGRAM
     C                   MOVE      *ON           *INLR
      /EJECT
      ********************************************************************
      *  PROCESS FUND
      ********************************************************************
      *
     C     PRFUND        BEGSR
      *
      * DETERMINE START DATE BASED ON MAX EXTENDED YEAR POLICY VALUE
     C                   EXSR      STDATE
      *
     C                   MOVE      *OFF          *IN61
     C                   MOVE      *OFF          *IN77
     C                   MOVE      *OFF          *IN79
     C                   MOVE      *OFF          *IN80
      *
      * BEGIN READING EMPLOYER DETAIL FILE
      *
     C                   EXSR      STFILE
      *
     C     *IN61         DOWEQ     *OFF
     C     ELPFDT        ANDLE     XXENDT
     C     *IN79         ANDEQ     *OFF
     C     *IN80         ANDEQ     *OFF
      *
     C     *IN61         OREQ      *OFF
     C     ELPTDT        ANDLE     XXENDT
     C     *IN79         ANDEQ     *ON
     C     *IN80         ANDEQ     *OFF
      *
     C     *IN77         OREQ      *OFF
     C     EAPODT        ANDLE     XXENDT
     C     *IN79         ANDEQ     *OFF
     C     *IN80         ANDEQ     *ON
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     *in61         oreq      *off
     C     QQEMP#        andne     *zeros
     C***  End   Add  ***  10/27/03  **************************************
      *
     C     *IN80         IFEQ      *ON
     C***  Begin Add  ***  10/27/03  **************************************
     C     QQEMP#        andeq     *zeros
     C***  End   Add  ***  10/27/03  **************************************
     C                   MOVE      EAEMP#        XXEMP#
     C                   MOVE      EADIV         XXDIV
     C                   MOVE      EAFYR         XXFYR
     C***  Begin Chg  ***  10/27/03  **************************************
     C*****ELKY2         CHAIN     PEL02                              61
     C     ELKY2         CHAIN     PEL00                              61
     C***  End   Chg  ***  10/27/03  **************************************
     C                   ENDIF                                                  *IN80=*ON
      * PROCESS ACTIVE
     C                   EXSR      PRACTV
      * READ NEXT DETAIL
     C                   EXSR      RDNXDT
      *
     C                   ENDDO                                                  *IN61 DOWEQ
      *
     C                   ENDSR
      *
      /EJECT
      ********************************************************************
      *  SRRANG - DATA RANGE VALIDATION SUBROUTINE
      ********************************************************************
      *
     C     SRRANG        BEGSR
      *
     C                   MOVE      *OFF          XXRANG            1
     C                   MOVE      *BLANK        XXVALA           15
     C                   MOVE      *ZERO         XXVALN           15 0
      *
     C                   SELECT
     C     XXSEL         WHENEQ    'EN'
     C                   Z-ADD     XXEMP         XXVALN
     C     XXSEL         WHENEQ    'EA'
     C                   MOVEL(P)  EBENAM        XXVALA
     C     XXSEL         WHENEQ    'ER'
     C                   Z-ADD     EBRGN#        XXVALN
     C     XXSEL         WHENEQ    'EC'
     C                   Z-ADD     EBCHAP        XXVALN
     C     XXSEL         WHENEQ    'EY'
     C     EBJRST        CAT       EBCNTY:0      XXVALA
     C     XXSEL         WHENEQ    'EZ'
     C                   Z-ADD     EBEZIP        XXVALN
     C     XXSEL         WHENEQ    'AN'
     C                   Z-ADD     EBAGT#        XXVALN
     C     XXSEL         WHENEQ    'AA'
     C                   MOVEL(P)  EBANAM        XXVALA
     C     XXSEL         WHENEQ    'AZ'
     C                   Z-ADD     EBAZIP        XXVALN
     C     XXSEL         WHENEQ    'PR'
     C                   Z-ADD     XXPREM        XXVALN
     C                   ENDSL
      *
     C     XXSEL         IFEQ      'EA'
     C     XXSEL         OREQ      'AA'
      *
     C     LO:UP         XLATE     XXVALA        XXVALA
     C     LO:UP         XLATE     XXFRGA        XXFRGA
     C     LO:UP         XLATE     XXTRGA        XXTRGA
     C                   CAT       HIVAL:0       XXTRGA
      *
     C                   ENDIF                                                  XXSEL IFEQ 'EA'
      *
     C                   SELECT
     C     XXSEL         WHENEQ    'EA'
     C     XXSEL         OREQ      'EY'
     C     XXSEL         OREQ      'AA'
      *
     C     XXVALA        IFGE      XXFRGA
     C     XXVALA        ANDLE     XXTRGA
     C                   MOVE      *ON           XXRANG
     C                   ENDIF
      *
     C     XXSEL         WHENEQ    'EN'
     C     XXSEL         OREQ      'ER'
     C     XXSEL         OREQ      'EC'
     C     XXSEL         OREQ      'EZ'
     C     XXSEL         OREQ      'AN'
     C     XXSEL         OREQ      'AZ'
     C     XXSEL         OREQ      'PR'
     C                   TESTN                   XXFRGA               757575
      *
     C     *IN75         IFEQ      *OFF
     C                   Z-ADD     *ZERO         XXFRGN
     C                   ENDIF
      *
     C                   TESTN                   XXTRGA               757575
      *
     C     *IN75         IFEQ      *OFF
     C                   MOVE      *ALL'9'       XXTRGN
     C                   ENDIF
      *
     C     XXVALN        IFGE      XXFRGN
     C     XXVALN        ANDLE     XXTRGN
     C                   MOVE      *ON           XXRANG
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR
      /EJECT
      *****************************************************************
      * ACCTCR - DETERMINE IF POLICY IS ACCOUNT CURRENT PLAN SUBROUTINE
      *****************************************************************
      *                                                               *
     C     ACCTCR        BEGSR
      *                                                               *
     C                   MOVEL     QQRNDT        @@DATE
      *
     C                   CALL      'WA002R'
      *
     C                   PARM      EBCO#         @@CO#             3 0
     C                   PARM      EBFND         @@FND             3 0
     C                   PARM      EBEMP#        @@EMP#            9 0
     C                   PARM      EBDIV         @@DIV             5 0
     C                   PARM      EBFYR         @@FYR             3 0
     C***  Begin Add  ***  01/10/18  **************************************
     C**********         PARM                    @@AGT#            5 0
     C**********         PARM                    @@ANAM           60
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C***  End   Add  ***  01/10/18  **************************************
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM      '*'           @@PLAN            1
     C                   PARM                    @@DATE            8 0
      *
     C                   SELECT
      *
     C     @@PLAN        WHENEQ    'A'
     C                   MOVE      'Y'           EBACUR
      *
     C                   OTHER
     C                   MOVE      *BLANKS       EBACUR
      *
     C                   ENDSL                                                  @@PLAN = 'A '
      *
     C                   ENDSR                                                  ACCTCR
      /EJECT
      *****************************************************************
      * DETERMINE START DATE BASED ON MAX EXTENDED YEAR POLICY VALUE
      *****************************************************************
      *
     C     STDATE        BEGSR
      *
     C     XXSTDT        IFNE      0
      *
     C     FNMXEX        IFEQ      0
     C                   Z-ADD     12            FNMXEX
     C                   ENDIF                                                  FNMXEX IFEQ
      *
     C     FNMXEX        DIV       12            XXYRS
     C                   MVR                     XXMTHS
     C                   MOVE      XXSTDT        XXDATE
      *
     C     XXYRS         IFGT      XXYY
     C                   SUB       1             XXC
     C                   SUB       XXYY          XXYRS
     C     100           SUB       XXYRS         XXYY
     C                   ELSE
     C                   SUB       XXYRS         XXYY
     C                   ENDIF                                                  XXYRS IFGT
      *
     C     XXMM          IFGT      XXMTHS
     C                   SUB       XXMTHS        XXMM
     C                   ELSE
      *
     C     XXYY          IFEQ      0
     C                   Z-ADD     99            XXYY
     C                   SUB       1             XXC
     C                   ELSE
     C                   SUB       1             XXYY
     C                   ENDIF                                                  XXYY IFEQ
      *
     C                   SUB       XXMM          XXMTHS
     C     12            SUB       XXMTHS        XXMM
     C                   ENDIF                                                  XXMM IFGT
      *
     C                   MOVE      XXDATE        XXSTDX
     C                   ENDIF                                                  XXSTDT IFNE
      *
     C                   ENDSR
      *                                                               *
      /EJECT
      *****************************************************************
      * START READING EMPLOYER DETAIL FILE
      *****************************************************************
      *                                                               *
     C     STFILE        BEGSR
      *
     C***  Begin Del  ***  10/27/03  **************************************
     C**********         Eval      Xxemp# = *Zero
     C***  End   Del  ***  10/27/03  **************************************
     C                   MOVE      *loval        XXPFDT
      *
      *  IF PROCESSING FOR POL EFF DATE RANGE, NO NEED TO START FILE
      *  EXTENDED YEAR POLICY MONTHS PRIOR, JUST USE FROM DATE
      *
     C                   SELECT
     C     XXSTAT        WHENEQ    'X'
     C                   MOVE      XXSTDT        XXPTDT
     C                   MOVE      *ON           *IN79
     C     XXSTAT        WHENEQ    'R'
     C                   MOVE      XXSTDT        XXPODT
     C                   MOVE      *ON           *IN80
     C     XXSTAT        WHENEQ    'E'
     C                   MOVE      XXSTDT        XXPFDT
     C     XXSTAT        WHENEQ    'I'
     C                   MOVE      *loval        XXPFDT
     C                   OTHER
     C                   MOVE      XXSTDX        XXPFDT
     C                   ENDSL
      *
     C***  Begin Chg  ***  10/27/03  **************************************
     C**********         If        *In79 = *Off and
     C**********                   *In80 = *Off
     C*****ELKY*         SETLL     PEL01
     C*****ELKY1         READE     PEL01                                  61
     C**********         Endif
     C**********
     C**********         If        *In79 = *On and
     C**********                   *In80 = *Off
     C*****ELKY4         SETLL     PEL03
     C*****ELKY1         READE     PEL03                                  61
     C**********         Endif
     C**********
     C**********         If        *In80 = *On
     C*****EAKY*         SETLL     PEA01
     C*****EAKY1         READE     PEA01                                  77
     C**********         Endif
     C                   select
     C                   when      QQEMP# = *zero
     C                              and not *in79 and not *in80
     C     ELKY          SETLL     PEL03
     C     ELKY1         READE     PEL03                                  61
     C                   when      QQEMP# = *zero
     C                              and *in79 and not *in80
     C     ELKY4         SETLL     PEL04
     C     ELKY1         READE     PEL04                                  61
     C                   when      QQEMP# = *zero
     C                              and *in80
     C     EAKY          SETLL     PEA01
     C     EAKY1         READE     PEA01                                  77
     C                   when      QQFYR <> *zero
     C                              and QQDIV = *blank
     C     yrNoDivELKY   chain     PEL01                              61
     C                   when      QQFYR <> *zero
     C     yrELKY        chain     PEL01                              61
     C                   when      QQDIV = *blank
     C     noDivELKY     chain     PEL00                              61
     C                   when      QQFYR = *zero
     C     ELKY3         chain     PEL00                              61
     C                   other
     C                   eval      *in61 = *on
     C                   endsl
     C*
     C                   if        QQEMP# <> *zeros
     C*****              eval      *in80 = *off
     C                   eval      *in77 = *on
     C                   endif
     C***  End   Chg  ***  10/27/03  **************************************
      *
     C                   ENDSR
      /EJECT
      *****************************************************************
      *  GET ISSUE DATE
      *****************************************************************
      *
     C     ISSUE         BEGSR
      *
     C                   CLEAR                   ISDATE
     C     EHKY          SETLL     WDEHL12
      *
     C     *IN60         DOUEQ     *ON
     C     EHKY          READE     WDEHL12                                60
      *
     C     *IN60         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
      *
     C     EHFAF         IFEQ      'E'
     C     EHFAF         OREQ      'R'
     C                   MOVE      EHTRDT        ISDATE
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDDO
      *
     C     ISDATE        IFEQ      *ZERO
     C     EHKY          SETLL     WDEHL12
      *
     C     *IN60         DOUEQ     *ON
     C     EHKY          READE     WDEHL12                                60
      *
     C     *IN60         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
      *
     C     EHFAF         IFEQ      'R'
     C                   MOVE      EHTRDT        ISDATE
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDIF
      *
     C     ISDATE        IFGE      XXSTDT
      ****** BEG CHG ***** 11/01/04 ***************************************
     C*****ISDATE        ANDLE     XXENDT
     C     ISDATE        ANDLE     XXEIDT
      ****** END CHG ***** 11/01/04 ***************************************
      *
     C                   MOVE      *ON           *IN67
     C                   ENDIF
      *
     C                   ENDSR
      /EJECT
      *****************************************************************
      *  VALIDATE STATUS AS OF RUN DATE IF IT WAS PASSED
      *****************************************************************
      *
     C     VLRUND        BEGSR
      *
     C     XXRNDT        IFEQ      *ZERO                                        RUN DT NOT USED
     C     XXRDST        OREQ      'B'                                          RUN STS = BOTH
     C                   MOVE      *ON           *IN72
     C                   ELSE
      *
     C                   Z-ADD     1             Y
     C     *ZERO         LOOKUP    ED(Y)                                  66
      *
     C     *IN66         IFEQ      *OFF
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   ENDIF                                                  *IN66 IFEQ
      *
     C     Y             DOWNE     0
      *
     C     XXRNDT        IFGE      ED(Y)
     C                   LEAVE
     C                   ENDIF
      *
     C                   SUB       1             Y
     C                   ENDDO                                                  Y DOWNE
      *
     C     Y             IFNE      0
     C     CD(Y)         ANDNE     0
     C     CD(Y)         ANDLE     XXRNDT
     C                   MOVE      *ON           *IN64                          CANCEL AS OF RNDT
     C                   ENDIF                                                  Y IFNE
      *
     C     *IN64         IFEQ      *ON
     C     XXRDST        ANDEQ     'C'
     C     *IN64         OREQ      *OFF
     C     XXRDST        ANDEQ     'A'
     C                   MOVE      *ON           *IN72
     C                   ENDIF                                                  *IN64 IFEQ
      *
     C                   ENDIF                                                  XXRDST IFEQ
      *
     C                   ENDSR
      *                                                               *
      /EJECT
      *****************************************************************
      *  COMPUTE XXPREM IF XXPRTY WAS PASSED
      *****************************************************************
      *                                                               *
     C     CALCPR        BEGSR
      *
     C     XXPRTY        IFEQ      *BLANKS
     C                   Z-ADD     *ZERO         XXPREM
     C                   ELSE
      *
     C                   SELECT
     C     XXPRTY        WHENEQ    'RX'                                         EARN NO EXP CONST
     C     XXPRTY        OREQ      'RI'                                         EARN W/ EXP CONST
      *
      *  LOAD PARMS FOR PLIST PLEARN
      *
     C                   MOVEL     ELCO#         SRCO#             3
     C                   MOVEL     ELFND         SRFND             3
     C                   MOVEL     ELFYR         SRFYR             3
     C                   MOVEL     ELEMP#        SREMP#            9
     C                   MOVEL     ELDIV         SRDIV             5
      *
     C     XXRNDT        IFGT      XXENDT
     C                   MOVE      QQRNDT        SRDATE            8
     C                   ELSE
     C                   MOVE      QQENDT        SRDATE
     C                   ENDIF
      *
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
      *
      *     CALL PGM TO CALCULATE FINANCIAL EARNED PREMIUM
      *
     C     XXPRTY        IFEQ      'RX'
     C                   CALL      'SRPRT2'      PLEARN                         EARN NO EXP CONST
     C                   ELSE
      *
     C                   CALL      'SRPRT1'      PLEARN                         EARN W/ EXP CONST
     C                   ENDIF                                                  XXPRTY IFEQ
      *
     C                   MOVE      SRAMT         XXPREM
      *
     C     XXPRTY        WHENEQ    'EX'                                         EST ANNL NO EXP CONS
     C                   Z-ADD     ELENMP        XXPREM
      *
     C     XXPRTY        WHENEQ    'EI'                                         EST ANNL W/ EXP CONS
      *
     C                   Z-ADD     ELPRTD        XXEXCO           11 0
      *
     C     ELPROR        IFNE      *ZERO
     C     ELPRTD        DIV(H)    ELPROR        XXEXCO
     C                   ENDIF                                                  ELPROR IFNE
     C                   MOVE      RSJRST        XXST
      *
     C     ELENMP        ADD       XXEXCO        XXPREM
      *
     C     XXPRTY        WHENEQ    'PX'                                         PRORATA NO EXP CONST
     C     ELANPR        SUB       ELPRTD        XXPREM
     C     XXPRTY        WHENEQ    'PI'                                         PRORATA W/ EXP CONST
     C                   Z-ADD     ELANPR        XXPREM
     C     XXPRTY        WHENEQ    'AX'                                         AUD NO EXP CONST
     C     ELAAPR        SUB       ELPRTD        XXPREM
     C     XXPRTY        WHENEQ    'AI'                                         AUD W/ EXP CONST
     C                   Z-ADD     ELAAPR        XXPREM
     C                   OTHER
     C                   Z-ADD     *ZERO         XXPREM
     C                   ENDSL
      *
     C                   ENDIF                                                  XXPRTY IFEQ *BLANKS
      *
     C     XXPREM        IFLT      *ZERO
     C                   MOVE      *ZERO         XXPREM
     C                   ENDIF
      *
     C                   ENDSR
      *
      /EJECT
      *****************************************************************
      *  INITIALIZE SORT FIELDS BASED ON SELECTION
      *****************************************************************
      *
     C     VLSORT        BEGSR
      *
      *  INITIALIZE PRIMARY SORT FIELD BASED ON SELECTION
      *
     C     QQEMP#        IFEQ      *ZEROS
     C                   Z-ADD     1             Y
     C     XXSRT1        LOOKUP    SK(Y)                                  71
      *
     C     *IN71         IFEQ      *OFF
     C                   Z-ADD     1             Y
     C                   ENDIF                                                  *IN71 IFEQ
      *
     C                   MOVEA     SRTS1         SR(1)
     C                   MOVEA     SRTS2         SR(5)
     C                   MOVEA     SRTS3         SR(10)
     C                   MOVE      SR(Y)         EBSRT1
     C     LO:UP         XLATE     EBSRT1        EBSRT1
      *
     C                   SELECT
     C     XXSRT1        WHENEQ    'AA'
     C     XXSRT2        ANDEQ     *BLANKS
     C                   MOVE      'AN'          XXSRT2
     C     XXSRT1        WHENEQ    'AA'
     C     XXSRT2        ANDNE     'AN'
     C                   MOVE      EBAGT#        EBSRT1
     C                   ENDSL
      *
      *  INITIALIZE SECONDARY SORT FIELD BASED ON SELECTION
      *
     C     XXSRT2        IFNE      *BLANKS
      *
     C                   Z-ADD     1             Y
     C     XXSRT2        LOOKUP    SK(Y)                                  71
      *
     C     *IN71         IFEQ      *OFF
     C                   Z-ADD     1             Y
     C                   ENDIF                                                  *IN71 IFEQ
      *
     C                   MOVEA     SRTS1         SR(1)
     C                   MOVEA     SRTS2         SR(5)
     C                   MOVEA     SRTS3         SR(10)
     C                   MOVE      SR(Y)         EBSRT2
     C     LO:UP         XLATE     EBSRT2        EBSRT2
      *
     C                   ELSE
     C                   MOVE      *BLANKS       EBSRT2
      *
     C                   ENDIF                                                  XXSRT2 IFNE
     C                   ENDIF                                                  QQEMP# IFEQ
      ***************
      *
     C     XXSRT1        IFEQ      'EX'
     C     XXSRT2        IFEQ      *BLANK
     C                   MOVE      ELEMP#        EMP#6
     C                   MOVE      ELPTDT        ELPTD1
     C                   MOVE      SRT11A        EBSRT1
     C                   ELSE
     C                   MOVE      ELPTDT        ELPTD2
     C                   MOVE      SRT11B        EBSRT1
     C                   ENDIF
     C                   ENDIF
      *
     C     XXSRT1        IFEQ      'UW'
     C     XXSRT2        IFEQ      *BLANK
     C                   MOVE      ELEMP#        EMP#5
     C                   MOVE      SRT12A        EBSRT1
     C                   ELSE
      ****** BEGIN ADD  01/08/03 **************************************
     C                   MOVE      EMUWTR        EMUWT2
      ****** END   ADD  01/08/03 **************************************
     C                   MOVE      SRT12B        EBSRT1
     C                   ENDIF
     C                   ENDIF
      *
     C     XXSRT1        IFEQ      'ST'
     C     XXSRT2        IFEQ      *BLANK
     C                   MOVE      ELEMP#        EMP#4
     C                   MOVE      EBJRST        JRST
     C                   MOVE      SRT13A        EBSRT1
     C                   ELSE
     C                   MOVE      EBJRST        JRST2
     C                   MOVE      SRT13B        EBSRT1
     C                   ENDIF
     C                   ENDIF
      *
     C     XXSRT2        IFEQ      'EX'
     C                   MOVE      ELEMP#        EMP#6
     C                   MOVE      ELPTDT        ELPTD1
     C                   MOVE      SRT11A        EBSRT2
     C                   ENDIF
      *
     C     XXSRT2        IFEQ      'UW'
      ****** BEGIN ADD  01/08/03 **************************************
     C                   MOVE      ELEMP#        EMP#5
      ****** END   ADD  01/08/03 **************************************
     C                   MOVE      SRT12A        EBSRT2
     C                   ENDIF
      *
     C     XXSRT2        IFEQ      'ST'
      ****** BEGIN ADD  01/08/03 **************************************
     C                   MOVE      ELEMP#        EMP#4
      ****** END   ADD  01/08/03 **************************************
     C                   MOVE      EBJRST        JRST
     C                   MOVE      SRT13A        EBSRT2
     C                   ENDIF
      *
     C     XXSRT1        IFEQ      'SN'
     C     XXSRT2        IFEQ      *BLANK
     C                   MOVE      EBEMP#        EMP#8
     C                   MOVE      SRT14A        EBSRT1
     C                   ELSE
     C                   MOVE      SRT14B        EBSRT1
     C                   ENDIF
     C                   ENDIF
      *
     C     XXSRT2        IFEQ      'SN'
     C                   MOVE      EBEMP#        EMP#8
     C                   MOVE      SRT14A        EBSRT2
     C                   ENDIF
      *
     C     XXSRT1        IFEQ      'SA'
     C     XXSRT2        IFEQ      *BLANK
     C                   MOVE      EBEMP#        EMP#7
     C                   MOVE      SRT15A        EBSRT1
     C                   ELSE
     C                   MOVE      SRT15B        EBSRT1
     C                   ENDIF
     C                   ENDIF
      *
     C     XXSRT2        IFEQ      'SA'
     C                   MOVE      EBEMP#        EMP#7
     C                   MOVE      SRT15A        EBSRT2
     C                   ENDIF
      *
     C                   IF        Xxstat = 'I'
     C     XXSRT1        IFEQ      'DT'
     C     XXSRT2        IFEQ      *BLANK
     C                   MOVE      Isdate        Isdate16a
     C                   MOVE      EBEMP#        EMP#16a
     C                   MOVE      SRT16A        EBSRT1
     C                   ELSE
     C                   MOVE      Isdate        Isdate16b
     C                   MOVE      SRT16B        EBSRT1
     C                   ENDIF
     C                   ENDIF
      *
     C     XXSRT2        IFEQ      'DT'
     C                   MOVE      Isdate        Isdate16a
     C                   MOVE      EBEMP#        EMP#16a
     C                   MOVE      SRT16A        EBSRT2
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
      *                                                               *
      /EJECT
      *****************************************************************
      * WRITE WORKFILE RECORD
      *****************************************************************
      *                                                               *
     C     WRITE         BEGSR
      *
     C     *IN67         IFEQ      *ON
     C     *IN72         ANDEQ     *ON                                          XXRDST MATCH
      *
      * INITIALIZE WORKFILE FIELDS
      *
     C                   MOVE      *BLANKS       SRTS1
     C                   MOVE      *BLANKS       SRTS2
     C                   MOVE      *BLANKS       SRTS3
     C                   MOVE      ELCO#         EBCO#
     C                   MOVE      ELFND         EBFND
     C                   MOVE      ELFYR         EBFYR
     C                   MOVE      ELEMP#        EBEMP#
     C                   MOVE      ELDIV         EBDIV
     C                   MOVE      EMNAME        EBENAM
     C                   MOVE      EMRGN#        EBRGN#
     C                   MOVE      EMCHAP        EBCHAP
     C                   MOVE      EMCNTY        EBCNTY
     C                   MOVE      BSSEFL        EBSEFL
     C                   MOVE      ELPTDT        EBPTDT
      *
     C     QQEMPF        IFEQ      'E'
     C                   MOVE      EMMPFL        XXEMPF
     C                   ENDIF                                                  XXEMPF IFEQ 'E'
      *
     C                   MOVE      *BLANKS       A3AD21
      *
     C     XXEMPF        IFEQ      'A'
      *
     C     A3KY          CHAIN     PA301                              76
      *
     C     *IN76         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C                   TESTN                   A3ZP21               76
     C                   ELSE
     C                   SETON                                        76
     C                   ENDIF                                                  *IN76=*OFF
      *
     C     *IN76         IFEQ      *ON
      *
     C                   TESTN                   A3ZP22               76
      *
     C     *IN76         IFEQ      *OFF
     C                   MOVE      *ZEROS        A3ZP22
     C                   ENDIF                                                  *IN76=*OFF
      *
     C                   ELSE
     C                   MOVE      *BLANKS       A3AD21
     C                   ENDIF                                                  *IN76=*ON
     C                   ENDIF                                                  XXEMPF='A'
      *
     C     A3AD21        IFNE      *BLANKS
     C                   MOVE      A3ZP21        XXZIP5
     C                   MOVE      A3ZP22        XXZIP4
      *
     C                   ELSE
      *
     C     XXEMPF        IFEQ      'P'
     C                   MOVE      EMPZIP        XXZIP
     C                   ELSE
     C                   MOVE      EMMZIP        XXZIP
     C                   ENDIF                                                  XXEMPF IFEQ 'P'
      *
     C                   ENDIF                                                  A3AD21 NE BLANK
      *
     C                   MOVE      XXZIP5        EBEZIP
      *
      * GET MOST RECENT ACTIVE AGENT
      *
     C                   EXSR      PRIAGT
      *
     C                   MOVE      ELRTCD        XXRTCD
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        EBJRST
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
      *  COMPUTE XXPREM IF XXPRTY WAS PASSED
      *
     C                   EXSR      CALCPR
      *
      *  INITIALIZE PRIMARY SORT FIELD BASED ON SELECTION
      *
     C                   EXSR      VLSORT
      *
      * INITIALIZE MAINTENANCE FIELDS
      *
     C                   MOVE      Q1USER        EBUSER
     C                   MOVE      Q1PGM         EBPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EBDT
      *
     C                   Z-ADD     DSTIME        EBTIME
      *
      *  LOAD ORIGINAL INCEPTION / POLICY EFFECTIVE / CANCEL DATES
      *
     C                   MOVE      ED(1)         EBINDT
     C                   MOVE      *ZEROS        EBPFDT
     C                   MOVE      *ZEROS        EBCNDT
      *
     C     XXRNDT        IFGT      XXENDT
     C                   MOVE      XXRNDT        XXASOF
     C                   ELSE
     C                   MOVE      XXENDT        XXASOF
     C                   ENDIF                                                  XXRNDT IFGT
      *
     C                   Z-ADD     5             IX                3 0
     C     IX            DOWGT     *ZERO
      *
     C     ED(IX)        IFNE      *ZERO
     C     ED(IX)        ANDLE     XXASOF
     C                   LEAVE
     C                   ENDIF                                                  ED,IX IFNE
      *
     C                   SUB       1             IX
     C                   ENDDO                                                  IX > 0
      *
     C     IX            IFGT      *ZERO
     C     CD(IX)        IFLE      XXASOF
     C                   MOVE      CD(IX)        EBCNDT
     C                   ENDIF                                                  CD,IX IFLE
     C                   ENDIF                                                  IX = 0
      *
     C                   MOVE      ELPFDT        EBPFDT
      *
      *  PROCESS RANGE SELECTIONS
      *
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *OFF          *IN74
      *
     C     QQSEL1        IFEQ      *BLANK
     C                   MOVE      *ON           *IN73
     C                   ELSE
     C                   MOVEL     QQSEL1        XXSEL
     C                   MOVEL     QQS1FR        XXFRGA
     C                   MOVEL     QQS1TO        XXTRGA
     C                   EXSR      SRRANG
     C                   MOVE      XXRANG        *IN73
     C                   ENDIF
      *
     C     QQSEL2        IFEQ      *BLANK
     C                   MOVE      *ON           *IN74
     C                   ELSE
     C                   MOVEL     QQSEL2        XXSEL
     C                   MOVEL     QQS2FR        XXFRGA
     C                   MOVEL     QQS2TO        XXTRGA
     C                   EXSR      SRRANG
     C                   MOVE      XXRANG        *IN74
     C                   ENDIF
      *
     C                   EXSR      ACCTCR
      *
     C     *IN73         IFEQ      *ON
     C     *IN74         ANDEQ     *ON
      *
      * WRITE WORKFILE RECORD
      *
     C                   WRITE     PEB01
      *
     C                   ENDIF                                                  *IN73 IFEQ
     C                   ENDIF                                                  *IN67 IFEQ
      *
     C                   ENDSR
      *                                                               *
      /EJECT
      *****************************************************************
      * GET PRIMARY AGENT
      *****************************************************************
      *                                                               *
     C     PRIAGT        BEGSR
      *                                                               *
     C                   MOVE      *BLANKS       EBANAM
     C                   Z-ADD     0             EBAZIP
     C                   Z-ADD     0             EBAGT#
     C                   MOVE      ELFYR         XXFYR
      *
     C     AHKY          SETGT     PAH01
     C     AHKY          READPE    PAH01                                  68
      *
     C     AHSTAT        DOWEQ     'I'
     C     *IN68         ANDEQ     *OFF
     C     AHKY          READPE    PAH01                                  68
     C                   ENDDO                                                  AHSTAT DOWEQ I
      *
     C     *IN68         IFEQ      *OFF
     C                   MOVE      AHAGT#        XXAGT#
     C                   MOVE      AHAGT#        EBAGT#
     C                   MOVE      AHSAGT        XXSAG#
     C                   EXSR      SECAGT
      *
     C     AAKY          CHAIN     PAA01                              69
      *
     C     *IN69         IFEQ      *OFF
     C                   MOVE      AAFEIN        XXFEIN
     C                   MOVE      AAMOD         XXMOD
      *
     C     AGKY          CHAIN     PAG01                              70
      *
     C     *IN70         IFEQ      *OFF
     C                   MOVE      AGNAME        EBANAM
      *
     C     XXAMPF        IFEQ      'P'
     C                   MOVE      AGPZIP        XXZIP
     C                   ELSE
      *
     C                   MOVE      AGMZIP        XXZIP
     C                   ENDIF                                                  XXAMPF IFEQ
      *
     C                   MOVE      XXZIP5        EBAZIP
     C                   ENDIF                                                  *IN70 IFEQ
      *
     C                   ENDIF                                                  *IN69 IFEQ
     C                   ENDIF                                                  *IN68 IFEQ
      *
     C                   ENDSR
      *                                                               *
      /EJECT
      *****************************************************************
      * GET SECONDARY AGENT
      *****************************************************************
      *                                                               *
     C     SECAGT        BEGSR
      *
     C                   MOVE      AHSAGT        XXSAG#
      *
     C     AAKY2         CHAIN     PAA01                              69
      *
     C     *IN69         IFEQ      *OFF
     C                   MOVE      AAFEIN        XXFEIN
     C                   MOVE      AAMOD         XXMOD
      *
     C     AGKY          CHAIN     PAG01                              70
      *
     C     *IN70         IFEQ      *OFF
     C                   MOVE      AGNAME        SECNAM
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
      *                                                               *
      /EJECT
      *****************************************************************
      * SELECT STATUS
      *****************************************************************
      *                                                               *
     C     SLSTAT        BEGSR
      *                                                               *
     C                   MOVE      *OFF          *IN64
     C                   MOVE      *OFF          *IN67
     C                   MOVE      *OFF          *IN72
     C                   Z-ADD     1             Y
     C     *ZERO         LOOKUP    ED(Y)                                  66
      *
     C     *IN66         IFEQ      *OFF
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   ENDIF                                                  *IN66 IFEQ
      *
     C     Y             DOWNE     0
     C     ELPTDT        COMP      ED(Y)                              66  66
     C  N66              SUB       1             Y
     C  N66              ENDDO                                                  Y DOWNE
      *
     C                   SELECT
      *
     C     XXSTAT        WHENEQ    'X'
     C                   MOVE      *ON           *IN67
      *
     C     XXSTAT        WHENEQ    'I'
     C                   EXSR      ISSUE
      *
     C     XXSTAT        WHENEQ    'R'
      *
     C     QQEMP#        IFNE      *ZEROS
     C                   MOVE      *ON           *IN67
      *
     C                   ELSE
      *
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'H'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
      *
     C                   MOVE      *ON           *IN67
      *
      *   MAKE SURE POLICY HAS NOT BEEN WRITTEN TO WXEBP ALREADY
      *
     C     EBKY          CHAIN     PEB02                              78
      *
     C     *IN78         IFEQ      *OFF
     C                   MOVE      *OFF          *IN67
     C                   ENDIF                                                  *IN78IFEQ*ON
      *
     C                   ENDIF                                                  EAARTY='B'
     C                   ENDIF                                                  QQEMP# NE *ZEROS
      *
     C     XXSTAT        WHENEQ    'E'
     C     ELPFDT        ANDGE     XXSTDT
     C     ELPFDT        ANDLE     XXENDT
     C                   MOVE      *ON           *IN67
      *
     C     XXSTAT        WHENEQ    'B'
      *
     C     Y             IFEQ      0
     C                   MOVE      *ON           *IN67
     C                   ELSE
      *
     C     CD(Y)         IFEQ      0
     C     CD(Y)         ORGE      XXSTDT
     C                   MOVE      *ON           *IN67
     C                   ENDIF                                                  CD,Y IFEQ
      *
     C                   ENDIF                                                  Y IFEQ
      *
     C                   OTHER
      *
     C     Y             IFNE      0
     C     CD(Y)         ANDNE     0
     C     CD(Y)         ANDGE     ELPFDT
     C     CD(Y)         ANDGE     XXSTDT
     C     CD(Y)         ANDLE     XXENDT
      *
     C                   MOVE      *ON           *IN64
      *
     C                   ENDIF                                                  Y IFNE
      *
     C     *IN64         IFEQ      *ON
     C     XXSTAT        ANDEQ     'C'
     C     *IN64         OREQ      *OFF
     C     XXSTAT        ANDEQ     'A'
      *
     C                   MOVE      *ON           *IN67
      *
     C                   ENDIF                                                  *IN64 IFEQ
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      /EJECT
      *****************************************************************
      * PROCESS ACTIVE
      *****************************************************************
      *
     C     PRACTV        BEGSR
      *
     C     ELPTDT        IFGT      XXSTDT
     C     *IN79         OREQ      *ON
     C     *IN80         OREQ      *ON
      *
      * CHECK BILLING SCHED TYPE AGAINST SELECTED EMPLOYER TYPE
      *
     C                   MOVE      *OFF          *IN62
     C                   MOVEL(P)  ELBSCD        XXKEY
      *
     C     TBKY          CHAIN     PBS01                              63
      *
     C     *IN63         IFEQ      *OFF
      *
     C     XXTYPE        IFEQ      BSSEFL
     C     XXTYPE        OREQ      'B'
     C                   MOVE      *ON           *IN62
     C                   ENDIF                                                  XXTYPE IFEQ
     C                   ENDIF                                                  *IN63 IFEQ
      *
     C     *IN62         IFEQ      *ON
      *
      * CHECK EMPLOYER STATUS AGAINST SELECTED STATUS
      *
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR
      *
     C     EMKY          CHAIN     PEM01                              65
      *
     C     *IN65         IFEQ      *OFF
      *
      * SELECT STATUS
     C                   EXSR      SLSTAT
      *
      * VALIDATE STATUS AS OF RUN DATE IF IT WAS PASSED
     C                   EXSR      VLRUND
      *
      * WRITE WORKFILE RECORD
     C                   EXSR      WRITE
      *
     C                   ENDIF                                                  *IN65 IFEQ
     C                   ENDIF                                                  *IN62 IFEQ
     C                   ENDIF                                                  ELPTDT IFGT
      *
     C                   ENDSR
      /EJECT
      *****************************************************************
      * READ NEXT DETAIL
      *****************************************************************
      *                                                               *
     C     RDNXDT        BEGSR
      *                                                               *
     C***  Begin Chg  ***  10/27/03  **************************************
      **********
     C*****QQEMP#        IFEQ      *ZEROS
      **********
     C**N79*****
     C*NN80ELKY1         READE     PEL01                                  61
     C***79*****
     C*NN80ELKY1         READE     PEL03                                  61
     C***80EAKY1         READE     PEA01                                  77
      **********
     C**********         ELSE
      **********
     C*****QQFYR         IFEQ      '000'
     C*****ELKY3         READE     PEL02                                  61
      **********
     C**********         ELSE
      **********
     C**********         MOVE      *ON           *IN61
     C**********         ENDIF
     C**********         ENDIF
     C                   select
     C                   when      QQEMP# = *zero
     C                              and not *in79 and not *in80
     C     ELKY1         reade     PEL03                                  61
     C                   when      QQEMP# = *zero
     C                              and *in79 and not *in80
     C     ELKY1         reade     PEL04                                  61
     C                   when      QQEMP# = *zero
     C                              and *in80
     C     EAKY1         reade     PEA01                                  77
     C                   when      QQFYR <> *zero
     C                              and QQDIV = *blank
     C     yrNoDivELKY   reade     PEL01                                  61
     C                   when      QQFYR <> *zero
     C     yrELKY        reade     PEL01                                  61
     C                   when      QQDIV = *blank
     C     noDivELKY     reade     PEL00                                  61
     C                   when      QQFYR = *zero
     C     ELKY3         reade     PEL00                                  61
     C                   other
     C                   eval      *in61 = *on
     C                   endsl
     C***  End   Chg  ***  10/27/03  **************************************
      *
     C                   ENDSR
      *                                                               *
      /EJECT
**  TABLE SK - SORT KEY TABLE
ENEAERECEYEZANAAAZPREXUWSTSASNDT
