     Hcopyright('(c) 2022 Sapiens, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W8311R1 - DCI Data Export Processor                      *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: DCI Data Export Processor                *
     H*                                                               *
     H* CALLED BY: W8311C                                             *
     H*                                                               *
     H* CALLS TO: NONE                                                *
     H*                                                               *
     H* CREATED: 05/21/09  C:191 PROJECT WC1792                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                                                    *
     H* 03/15/22 C:062 Ticket 99430(133)                                                   *
     H*                no longer report new claims for non-TX states                       *
     H*                                                                                    *
     H* 04/22/20 C:062 Project WC2743                                                      *
     H*                Add Catastrophe processing                                          *
     H*
     H* 03/20/20 C:295  Support: 87487(133)     (+05/13/20)
     H*                 LUBA  Impairment rating issue
     H*
     H* 12/13/19 C:053  Project: 133WRN
     H*                 Correct percentage being reported.
     H*
     H* 09/07/16 C:062  Support: 39361(133)                           *
     H*                 Initialize TX amount fields                   *
     H*                                                               *
     H* 08/02/16 C:062  Support: 42629(133)                           *
     H*                 Don't pick up items that have never been sent.*
     H*                                                               *
     H* 04/05/16 C:062  Support: 40252(133)                           *
     H*                 Skip claims if they aren't being processed    *
     H*                                                               *
     H* 01/27/16 C:062  Support: 39361(133)                           *
     H*                 Get first Comp payment not first payment.     *
     H*                                                               *
     H* 09/11/15 C:062  Support: 37177(133)                           *
     H*                 Check for payments on closed claims.          *
     H*                                                               *
     H* 08/25/14 C:283  Support: WC133 85974                          *
     H*                 Change the Status DIstat to 'C' if the Claim  *
     H*                 is Closed, so that it is not selected for     *
     H*                 reporting the next time.                      *
     H*                                                               *
     H* 05/01/14 C:279  Project: WC2174                               *
     H*                 Exclude Non-compensable and fraudulent claims *
     H*                 from reporting.                               *
     H*                                                               *
     H* 09/02/13 C:283  Support: WC133 83354                          *
     H*                 The DOU loop is incorrectly ending after      *
     H*                 processing an extra record.                   *
     H*                                                               *
     H* 07/17/13 C:283  Support: WC133 81896                          *
     H*                 Correctly populate the Temporary Total Injury *
     H*                 code '05' if no payments were found.          *
     H*                                                               *
     H* 11/19/12 C:283  Support: WC114 81086                          *
     H*                 Changed the Output on file WXDIP1.            *
     H*                 Policy Number is now loaded as                *
     H*                    'gggeeeeeeeeeyyy'.                         *
     H*                 Claim Number is now loaded as 'ccccccc'       *
     H*                                                               *
     H* 09/12/12 C:283  Support: WC133 80615                          *
     H*                 Code Commented for Support SEQ 79264 is       *
     H*                 allowing any claim to be reported at all      *
     H*                 report levels but it was actually supposed    *
     H*                 to only allow Death Claims and Permanent      *
     H*                 Disability Claims.                            *
     H*                 Re-activating the code in different           *
     H*                 subroutine to work as expected.               *
     H*                                                               *
     H* 03/01/12 C:255  Support: WA133 79142                          *
     H*                 Subroutine srPRCtrailerB is not being called  *
     H*                 due to Select/When Condition. Corrected recor-*
     H*                 d count in srPRCtrailerB routine.             *
     H*                                                               *
     H* 02/27/12 C:279  Support: WC133 79264                          *
     H*                 Modified to report Death Claims and Permanent *
     H*                 disability claims at all report levels.       *
     H*                                                               *
     H* 12/12/11 C:062  Support: WA133 75837                          *
     H*                 Medical Extinguishment Flag                   *
     H*                                                               *
     H* 11/15/11 C:191  Support: WA133 78498                          *
     H*                 Fix impairment %                              *
     H*                                                               *
     H* 11/02/11 C:191  Support: WA133 78361                          *
     H*                 Claim status as of run date to be reported    *
     H*                 Incurred amounts as of run date to be reported*
     H*                                                               *
     H* 10/26/11 C:191  Support: WA133 78270                          *
     H*                 Only payments till rundate should be included *
     H*                 Current processing picks payments till system *
     H*                 date.                                         *
     H*                                                               *
     H* 10/18/11 C:255  Support: WA133 78254                          *
     H*          C:191  Weekly payment amount to be based on Compensa-*
     H*                 tion rate, instead of average per week        *
     H*                                                               *
     H* 08/15/11 C:191  Support: 76807 WC114                          *
     H*                 Change part, cause and nature codes to picked *
     H*                 from EDI codes                                *
     H*                                                               *
     H* 07/07/11 C:255  Support: 77284 WA133                          *
     H*                 Amounts rounded to nearest $                  *
     H*                  - Benefit amount paid                        *
     H*                  - Claimant Legal amount paid                 *
     H*                  - Employer Legal amount paid                 *
     H*                  - Incurred indemnity amount total            *
     H*                  - Incurred medical   amount total            *
     H*                  - Lump sum settlement amount paid            *
     H*                  - Preinjury/AWW Amount                       *
     H*                  - Recovery reimbursement amount              *
     H*                  - Total paid medical amount                  *
     H*                  - Voc rehab education exp amount             *
     H*                  - Voc rehab evaluation exp amount            *
     H*                  - Voc rehab maintenance benefit amount       *
     H*                  - Voc rehab other paid                       *
     H*                  - Weekly benefit amount                      *
     H*                  - TX: Hospital costs amount paid             *
     H*                  - TX: Total payment to physicians            *
     H*                                                               *
     H* 05/19/11 C:191  Support: 76917 WA133                          *
     H*                 Fix DCI test issues-type of loss condition    *
     H*                                                               *
     H* 05/09/11 C:191  Support: 76799 WA133                          *
     H*                 Fix DCI test issues                           *
     H*                                                               *
     H* 03/23/11 C:191  PROJECT: WC1987  Priss #24                    *
     H*                 Fix invalid date                              *
     H*                                                               *
     H* 03/08/11 C:220  PROJECT: WC1987                               *
     H*          C:191                                                *
     H*                 - Add functionality for the new file format.  *
     H*                 (09/2009).                                    *
     H*                                                               *
     H*****************************************************************
    F*  Primary logical file definition
     Fwddil     uf   e           k disk
     Fwdf2l     if   e           k disk
     Fwmcml1    if   e           k disk
     Fwdell     if   e           k disk
     Fststl1    if   e           k disk
     Fwtgtl     if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmchl     if   e           k disk
     Fwtrtl1    if   e           k disk
     Fwdpal     if   e           k disk
     F*** Begin And *** 09/11/15 ***************************************
     Fwdpal5    if   e           k disk    Rename(ppa01:ppa05)
     F*** End   And *** 09/11/15 ***************************************
     Fwmlll     if   e           k disk
     Fwtabl     if   e           k disk
     F                                     ignore(PAT01)
     F                                     ignore(PSP01)
     F                                     ignore(PNC01)
     F*** BEGIN CHG ****** 08/15/11 ************************************
     F*************************            ignore(PNA01)
     F*************************            ignore(PPA01)
     F*************************            ignore(PCA01)
     F                                     rename(PPA01:PPA0T)
     F*** END   CHG ****** 08/15/11 ************************************
     F                                     ignore(PRC01)
     F                                     ignore(PRT01)
     F                                     ignore(PPC01)
     F*************************************ignore(PB201)
     F                                     ignore(PB701)
     F                                     ignore(PBS01)
     F                                     ignore(PTT01)
     F                                     ignore(PCC01)
     F                                     ignore(PBC01)
     F                                     ignore(PCS01)
     F                                     ignore(PCN01)
     F                                     ignore(PRS01)
     F                                     ignore(PLR01)
     F                                     ignore(PVS01)
     F                                     ignore(PTR01)
     F                                     ignore(PHY01)
     F                                     ignore(PSY01)
     F                                     ignore(PBA01)
     F                                     ignore(PID01)
     F                                     ignore(PIS01)
     Fwxdip     o    e             disk    rename(pdi01:pdx01)
     F***** Beg Add ***** 03/08/11 *****************************************
     Fwxdip1    o    e             disk    rename(pdi01:pdx02)
     Fwmeml     if   e           k disk
     Fwxf1l     if   e           k disk
     F***** End Add ***** 03/08/11 *****************************************
     F*** BEGIN ADD ****** 11/02/11 ************************************
     Fwmrhl     if   e           k disk
     F*** END   ADD ****** 11/02/11 ************************************
     F*** Begin add ****** 12/13/19 ************************************
     Fwmbyl     if   e           k disk
     F*** End   add ****** 12/13/19 ************************************
     F*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
      *
     D @@co#           S              3
     D @@prd           S              4
     D @@trcd          S              1
     D*
     D xxco#           S              3  0
     D xxfnd           S              3  0
     D xxst            S              2
     D xxprdt          S              6  0
     D xxprd           S              4  0
     D xxpdci          S              5  0
     D xxopni          S              5  0
     D xxcnpt          S              5  0
     D xxcpnt          S              5  0
     D xxsamp          S              5  0
     D xxkey           S             10
     D xxDiff          S              5  0
     D xxcnt1          S              7  0
     D xxcnt2          S              7  0
     D xxsubt          S              7  0
     D xxFirst         S               N
     D isValid         S               N
     D xxDetCnt        S              7  0
     D XXINDC          S              5  0
     D XXDCIT          S              5  0
     D xxrplv          S              2  0
     D***** Beg Add ***** 03/08/11 *****************************************
     D xxrplv1         S              3  0
     D xxcntH          S              7  0
     D xxpgmc          S             10
     D***** End Add ***** 03/08/11 *****************************************
     D xxtbid          S              4
     D xxaww           S              5  0
     D xxaww9          S              9  0
     D*** BEGIN ADD ****** 11/15/11 ************************************
     D xxipct          S              3  0
     D*** END   ADD ****** 11/15/11 ************************************
     D xxTIIA          S              9  2
     D xxTIMA          S              9  2
     D xxTPMA          S              9  2
     D XXVEEA          S              9  2
     D XXVMBA          S              9  2
     D XXVEDA          S              9  2
     D XXVOTA          S              9  2
     D XXCLAP          S              9  2
     D XXELAP          S              9  2
     D xxIndex         S              3  0
     D***** Beg Chg ***** 07/07/11 *****************************************
     D************* xxwbn           S              6  2
     D xxwbn           S              6  0
     D***** End Chg ***** 07/07/11 *****************************************
     D xxmein          S              1
     D***** Beg Add ***** 03/08/11 *****************************************
     D XXMVEA          S              9  2
     D XXCVEA          S              9  2
     D XXRRAM          S              9  2
     D XXHSCA          S              9  2
     D XXTPPH          S              9  2
     D xxNewFormat     S               n
     D xxfil91         C                   Const('99999999999999999999999999999-
     D                                     999999')
     D***** End Add ***** 03/08/11 *****************************************
     D*** Begin And *** 09/11/15 ***************************************
     D kkDt            S                   Like(DIdt)
     D reportLvl       S              2  0
     D reportLvlIn     S              2  0
     D reportMth       S              3  0
     D lastReportDate  s               d   datfmt(*iso)
     D*** End   And *** 09/11/15 ***************************************
     D*
     D date1           s               d   datfmt(*iso)
     D date2           s               d   datfmt(*iso)
     D*
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D*** BEG   ADD ****** 07/07/11 ************************************
     D xxBAP           s              9  0
     D xxAMT90         s              9  0
     D*** END   ADD ****** 07/07/11 ************************************
     D*
     D arrBTC          s              2    dim(5)
     D arrBAP          s              9  2 dim(5)
     C*** Beg   Del ****** 10/18/11 ************************************
     D******** arrWKS          s              3  0 dim(5)
     C*** End   Del ****** 10/18/11 ************************************
     D arrLSC          s              2    dim(6)
     D arrLSA          s              9  2 dim(6)
     D*
     D***** Beg Add ***** 03/08/11 *****************************************
     D  X1RTCD         s              1                                         Record Type Code
     D  X1CAR#         s              5                                         Carrier Code
     D  X1CAR1         s              5                                         Carrier Group Code
     D  X1POL#         s             18                                         Policy Number
     D  X1PFDT         s              8                                         Policy Eff Date
     D  X1RPCD         s              1                                         Replacement Type
     D  X1CLM#         s             12                                         Claim Number
     D  X1JRST         s              2                                         Jurisdiction State
     D  X1AST          s              2                                         Accident State Code
     D  X1ACDT         s              8                                         Accident/Injury Date
     D  X1DTRP         s              8                                         Reported Insurer Dt
     D  X1CLAS         s              4                                         Classification Code
     D  X1LSCV         s              2                                         Type of Loss Code
     D  X1LSRV         s              2                                         Type of Recovery
     D  X1COVT         s              2                                         Type of Claim Code
     D  X1SEX          s              1                                         Claimant Gender Code
     D  X1DOBY         s              4                                         Birth Year
     D  X1HDTY         s              4                                         Hire Year
     D  X1AWW          s              5                                         Average Weekly Wage
     D  X1WMTH         s              1                                         Method of AWW
     D  X1PART         s              2                                         Part of Body Code
     D  X1NATR         s              2                                         Nature of Injury
     D  X1CAUS         s              2                                         Cause of Injury Code
     D  X1STAT         s              1                                         Claim Status Code
     D  X1CLDT         s              8                                         Closing Date
     D  X1TIIA         s              9                                         Tot Incurred Indemni
     D  X1BTC1         s              2                                         Benefit Type Code 1
     D  X1BAP1         s              9                                         Benefit Amt Paid  1
     D  X1WBN1         s              6                                         Weekly Benefit Amt1
     D  X1BTC2         s              2                                         Benefit Type Code 2
     D  X1BAP2         s              9                                         Benefit Amt Paid  2
     D  X1WBN2         s              6                                         Weekly Benefit Amt2
     D  X1BTC3         s              2                                         Benefit Type Code 3
     D  X1BAP3         s              9                                         Benefit Amt Paid  3
     D  X1WBN3         s              6                                         Weekly Benefit Amt3
     D  X1BTC4         s              2                                         Benefit Type Code 4
     D  X1BAP4         s              9                                         Benefit Amt Paid  4
     D  X1WBN4         s              6                                         Weekly Benefit Amt4
     D  X1BTC5         s              2                                         Benefit Type Code 5
     D  X1BAP5         s              9                                         Benefit Amt Paid  5
     D  X1WBN5         s              6                                         Weekly Benefit Amt5
     D  X1VEEA         s              9                                         VR Evaluation EA
     D  X1VMBA         s              9                                         VR Maint Benefit Amt
     D  X1VEDA         s              9                                         VR Education EA
     D  X1VOTA         s              9                                         VR Other Amount Paid
     D  X1TIMA         s              9                                         Total Incurred Med
     D  X1TPMA         s              9                                         Total Paid Medical
     D  X1PIWW         s              9                                         Post-Injury WW
     D  X1IPCT         s              3                                         Impairment %
     D  X1IPBC         s              1                                         Impairment % Basis
     D  X1MMID         s              8                                         Max Medical Imp Date
     D  X1PLAA         s              1                                         Attorney Indicator
     D  X1CVFL         s              1                                         Controverted Indicat
     D  X1CLAP         s              9                                         Claimant Legal Amt
     D  X1ELAP         s              9                                         Employer Legal Amt
     D  X1LSC1         s              2                                         Lumpsum Sttlment Cd1
     D  X1LSA1         s              9                                         Lumpsum Sttlment Am1
     D  X1LSC2         s              2                                         Lumpsum Sttlment Cd2
     D  X1LSA2         s              9                                         Lumpsum Sttlment Am2
     D  X1LSC3         s              2                                         Lumpsum Sttlment Cd3
     D  X1LSA3         s              9                                         Lumpsum Sttlment Am3
     D  X1LSC4         s              2                                         Lumpsum Sttlment Cd4
     D  X1LSA4         s              9                                         Lumpsum Sttlment Am4
     D  X1LSC5         s              2                                         Lumpsum Sttlment Cd5
     D  X1LSA5         s              9                                         Lumpsum Sttlment Am5
     D  X1LSC6         s              2                                         Lumpsum Sttlment Cd6
     D  X1LSA6         s              9                                         Lumpsum Sttlment Am6
     D  X1MEIN         s              1                                         Medical Ext Indicato
     D  X1RTWD         s              8                                         Return to Work Date
     D  X1RTWS         s              1                                         Return to Work Stat
     D  X1ELEI         s              1                                         Extraordinary Loss
     D  X1PCAR         s              5                                         Previous Carrier
     D  X1PCR1         s              5                                         Previous Carrier Grp
     D  X1PPOL         s             18                                         Previous Policy
     D  X1PPFD         s              8                                         Previous Policy Eff
     D  X1PRPD         s              8                                         Previous Report Date
     D  X1PCLM         s             12                                         Previous Policy Clm
     D  X1RRAM         s              9                                         Recovery Reimburse
     D  X1ESSN         s              9                                         Employee SSN
     D  X1EFEN         s              9                                         Employer TAX ID
     D  X1ZPIS         s              5                                         Zip code for Inj sit
     D  X1DTFP         s              8                                         Date of first paymen
     D  X1HSCA         s              9                                         Hospital costs paid
     D  X1TPPH         s              9                                         Tot payment to Phy
     D***** End Add ***** 03/08/11 *****************************************
     D*
     D arrLvl          s              2  0 dim(11) ctdata perrcd(1)
     D arrMths         s              3  0 dim(11) alt(arrLvl) ascend
     D*
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D                 ds
     D xxdt7                          7  0
     D  xxcc                          1  0 overlay(xxdt7:1)
     D  xxyy                          2  0 overlay(xxdt7:2)
     D  xxmm                          2  0 overlay(xxdt7:4)
     D  xxdd                          2  0 overlay(xxdt7:6)
     D                 ds
     D xxdt6                          6  0
     D  xxmm6                         2  0 overlay(xxdt6:1)
     D  xxdd6                         2  0 overlay(xxdt6:3)
     D  xxyy6                         2  0 overlay(xxdt6:5)
     D                 ds
     D xxmy                           4  0
     D  xxmm1                         2  0 overlay(xxmy)
     D  xxyy1                         2  0 overlay(xxmy:3)
    D*
     D xxflags         ds             6
     D  distat                        1    overlay(xxflags:1)
     D  dipdci                        1    overlay(xxflags:2)
     D  diopni                        1    overlay(xxflags:3)
     D  dicnpt                        1    overlay(xxflags:4)
     D  dicpnt                        1    overlay(xxflags:5)
     D  disamp                        1    overlay(xxflags:6)
     D*
     D* Payment reserves type code
     D                 ds
     D arrRT                   1     20    dim(10)
     D*** BEGIN CHG ****** 11/02/11 ************************************
     D  rhrt01                 1      2
     D  rhrt02                 3      4
     D  rhrt03                 5      6
     D  rhrt04                 7      8
     D  rhrt05                 9     10
     D  rhrt06                11     12
     D  rhrt07                13     14
     D  rhrt08                15     16
     D  rhrt09                17     18
     D  rhrt10                19     20
     D***
     D**cmrt01                 1      2
     D**cmrt02                 3      4
     D**cmrt03                 5      6
     D**cmrt04                 7      8
     D**cmrt05                 9     10
     D**cmrt06                11     12
     D**cmrt07                13     14
     D**cmrt08                15     16
     D**cmrt09                17     18
     D**cmrt10                19     20
     D*
     D* reserve change
     D                 ds
     D arrSU                   1     90  2 dim(10)
     D  rhsu01                 1      9  2
     D  rhsu02                10     18  2
     D  rhsu03                19     27  2
     D  rhsu04                28     36  2
     D  rhsu05                37     45  2
     D  rhsu06                46     54  2
     D  rhsu07                55     63  2
     D  rhsu08                64     72  2
     D  rhsu09                73     81  2
     D  rhsu10                82     90  2
     D*** END   CHG ****** 11/02/11 ************************************
     D*** BEGIN DEL ****** 11/02/11 ************************************
     D* Payment remaining reserve
     D*************    ds
     D*arrRR******             1     90  2 dim(10)
     D****
     D**cmrr01                 1      9  2
     D**cmrr02                10     18  2
     D**cmrr03                19     27  2
     D**cmrr04                28     36  2
     D**cmrr05                37     45  2
     D**cmrr06                46     54  2
     D**cmrr07                55     63  2
     D**cmrr08                64     72  2
     D**cmrr09                73     81  2
     D**cmrr10                82     90  2
     D*
     D* Payment against reserve
     D***************  ds
     D*arrAR*********          1     90  2 dim(10)
     D**cmar01                 1      9  2
     D**cmar02                10     18  2
     D**cmar03                19     27  2
     D**cmar04                28     36  2
     D**cmar05                37     45  2
     D**cmar06                46     54  2
     D**cmar07                55     63  2
     D**cmar08                64     72  2
     D**cmar09                73     81  2
     D**cmar10                82     90  2
     D*** END   DEL ****** 11/02/11 ************************************
     D*
     D* Record Type 1 Definition - detail
     D DSTYP1          DS           500
     D***** Beg Chg ***** 03/08/11 *****************************************
     D* field names are renamed from X1**** to XA**** to maintain consistent naming in
     D* two similar data structures. This was intentionally done as hard change (no commenting)
     D*
     D  XARTCD                        1    OVERLAY(DSTYP1:1)                    Record Type Code
     D  XACAR#                        5    OVERLAY(DSTYP1:2)                    Carrier Code
     D  XACAR1                        5    OVERLAY(DSTYP1:7)                    Carrier Group Code
     D  XAPOL#                       18    OVERLAY(DSTYP1:12)                   Policy Number
     D  XAPFDT                        8    OVERLAY(DSTYP1:30)                   Policy Eff Date
     D  XARPLV                        2    OVERLAY(DSTYP1:38)                   Report Level Code
     D  XARPCD                        1    OVERLAY(DSTYP1:40)                   Replacement Type
     D  XACLM#                       12    OVERLAY(DSTYP1:41)                   Claim Number
     D  XAFUT1                       18    OVERLAY(DSTYP1:53)                   Reserved for Future
     D  XAJRST                        2    OVERLAY(DSTYP1:71)                   Jurisdiction State
     D  XAAST                         2    OVERLAY(DSTYP1:73)                   Accident State Code
     D  XAACDT                        8    OVERLAY(DSTYP1:75)                   Accident/Injury Date
     D  XADTRP                        8    OVERLAY(DSTYP1:83)                   Reported Insurer Dt
     D  XACLAS                        4    OVERLAY(DSTYP1:91)                   Classification Code
     D  XALSCV                        2    OVERLAY(DSTYP1:95)                   Type of Loss Code
     D  XALSRV                        2    OVERLAY(DSTYP1:97)                   Type of Recovery
     D  XACOVT                        2    OVERLAY(DSTYP1:99)                   Type of Claim Code
     D  XASEX                         1    OVERLAY(DSTYP1:101)                  Claimant Gender Code
     D  XADOBY                        4    OVERLAY(DSTYP1:102)                  Birth Year
     D  XAHDTY                        4    OVERLAY(DSTYP1:106)                  Hire Year
     D  XAFUT2                        4    OVERLAY(DSTYP1:110)                  Reserved for Future
     D  XAAWW                         5    OVERLAY(DSTYP1:114)                  Average Weekly Wage
     D  XAWMTH                        1    OVERLAY(DSTYP1:119)                  Method of AWW
     D  XAPART                        2    OVERLAY(DSTYP1:120)                  Part of Body Code
     D  XANATR                        2    OVERLAY(DSTYP1:122)                  Nature of Injury
     D  XACAUS                        2    OVERLAY(DSTYP1:124)                  Cause of Injury Code
     D  XASTAT                        1    OVERLAY(DSTYP1:126)                  Claim Status Code
     D  XACLDT                        8    OVERLAY(DSTYP1:127)                  Closing Date
     D  XATIIA                        9    OVERLAY(DSTYP1:135)                  Tot Incurred Indemni
     D  XABTC1                        2    OVERLAY(DSTYP1:144)                  Benefit Type Code 1
     D  XABAP1                        9    OVERLAY(DSTYP1:146)                  Benefit Amt Paid  1
     D  XAWBN1                        6    OVERLAY(DSTYP1:155)                  Weekly Benefit Amt1
     D  XABTC2                        2    OVERLAY(DSTYP1:161)                  Benefit Type Code 2
     D  XABAP2                        9    OVERLAY(DSTYP1:163)                  Benefit Amt Paid  2
     D  XAWBN2                        6    OVERLAY(DSTYP1:172)                  Weekly Benefit Amt2
     D  XABTC3                        2    OVERLAY(DSTYP1:178)                  Benefit Type Code 3
     D  XABAP3                        9    OVERLAY(DSTYP1:180)                  Benefit Amt Paid  3
     D  XAWBN3                        6    OVERLAY(DSTYP1:189)                  Weekly Benefit Amt3
     D  XABTC4                        2    OVERLAY(DSTYP1:195)                  Benefit Type Code 4
     D  XABAP4                        9    OVERLAY(DSTYP1:197)                  Benefit Amt Paid  4
     D  XAWBN4                        6    OVERLAY(DSTYP1:206)                  Weekly Benefit Amt4
     D  XABTC5                        2    OVERLAY(DSTYP1:212)                  Benefit Type Code 5
     D  XABAP5                        9    OVERLAY(DSTYP1:214)                  Benefit Amt Paid  5
     D  XAWBN5                        6    OVERLAY(DSTYP1:223)                  Weekly Benefit Amt5
     D  XAFUT3                       17    OVERLAY(DSTYP1:229)                  Reserved for Future
     D  XAVEEA                        9    OVERLAY(DSTYP1:246)                  VR Evaluation EA
     D  XAVMBA                        9    OVERLAY(DSTYP1:255)                  VR Maint Benefit Amt
     D  XAVEDA                        9    OVERLAY(DSTYP1:264)                  VR Education EA
     D  XAVOTA                        9    OVERLAY(DSTYP1:273)                  VR Other Amount Paid
     D  XATIMA                        9    OVERLAY(DSTYP1:282)                  Total Incurred Med
     D  XATPMA                        9    OVERLAY(DSTYP1:291)                  Total Paid Medical
     D  XAPIWW                        9    OVERLAY(DSTYP1:300)                  Post-Injury WW
     D  XAIPCT                        3    OVERLAY(DSTYP1:309)                  Impairment %
     D  XAIPBC                        1    OVERLAY(DSTYP1:312)                  Impairment % Basis
     D  XAMMID                        8    OVERLAY(DSTYP1:313)                  Max Medical Imp Date
     D  XAPLAA                        1    OVERLAY(DSTYP1:321)                  Attorney Indicator
     D  XACVFL                        1    OVERLAY(DSTYP1:322)                  Controverted Indicat
     D  XACLAP                        9    OVERLAY(DSTYP1:323)                  Claimant Legal Amt
     D  XAELAP                        9    OVERLAY(DSTYP1:332)                  Employer Legal Amt
     D  XALSC1                        2    OVERLAY(DSTYP1:341)                  Lumpsum Sttlment Cd1
     D  XALSA1                        9    OVERLAY(DSTYP1:343)                  Lumpsum Sttlment Am1
     D  XALSC2                        2    OVERLAY(DSTYP1:352)                  Lumpsum Sttlment Cd2
     D  XALSA2                        9    OVERLAY(DSTYP1:354)                  Lumpsum Sttlment Am2
     D  XALSC3                        2    OVERLAY(DSTYP1:363)                  Lumpsum Sttlment Cd3
     D  XALSA3                        9    OVERLAY(DSTYP1:365)                  Lumpsum Sttlment Am3
     D  XALSC4                        2    OVERLAY(DSTYP1:374)                  Lumpsum Sttlment Cd4
     D  XALSA4                        9    OVERLAY(DSTYP1:376)                  Lumpsum Sttlment Am4
     D  XALSC5                        2    OVERLAY(DSTYP1:385)                  Lumpsum Sttlment Cd5
     D  XALSA5                        9    OVERLAY(DSTYP1:387)                  Lumpsum Sttlment Am5
     D  XALSC6                        2    OVERLAY(DSTYP1:396)                  Lumpsum Sttlment Cd6
     D  XALSA6                        9    OVERLAY(DSTYP1:398)                  Lumpsum Sttlment Am6
     D  XAMEIN                        1    OVERLAY(DSTYP1:407)                  Medical Ext Indicato
     D  XARTWD                        8    OVERLAY(DSTYP1:408)                  Return to Work Date
     D  XARTWS                        1    OVERLAY(DSTYP1:416)                  Return to Work Stat
     D  XAELEI                        1    OVERLAY(DSTYP1:417)                  Extraordinary Loss
     D  XAFUT4                        8    OVERLAY(DSTYP1:418)                  Reserved for Future
     D  XAPCAR                        5    OVERLAY(DSTYP1:426)                  Previous Carrier
     D  XAPCR1                        5    OVERLAY(DSTYP1:431)                  Previous Carrier Grp
     D  XAPPOL                       18    OVERLAY(DSTYP1:436)                  Previous Policy
     D  XAPPFD                        8    OVERLAY(DSTYP1:454)                  Previous Policy Eff
     D  XAPRPD                        8    OVERLAY(DSTYP1:462)                  Previous Report Date
     D  XAPCLM                       12    OVERLAY(DSTYP1:470)                  Previous Policy Clm
     D  XAFUT5                       19    OVERLAY(DSTYP1:482)                  Reserved for Future
     D***** End Chg ***** 03/08/11 *****************************************
     D*
     D***** Beg Add ***** 03/08/11 *****************************************
     D* Record Type 1 Definition - new file detail
     D DSTYP11         DS           600
     D  XBRTCD                        1    OVERLAY(DSTYP11:1)                   Record Type Code
     D  XBCAR#                        5    OVERLAY(DSTYP11:2)                   Carrier Code
     D  XBCAR1                        5    OVERLAY(DSTYP11:7)                   Carrier Group Code
     D  XBPOL#                       18    OVERLAY(DSTYP11:12)                  Policy Number
     D  XBPFDT                        8    OVERLAY(DSTYP11:30)                  Policy Eff Date
     D  XBRPLV                        3    OVERLAY(DSTYP11:38)                  Valuation Level Code
     D  XBRPCD                        1    OVERLAY(DSTYP11:41)                  Replacement Type
     D  XBCLM#                       12    OVERLAY(DSTYP11:42)                  Claim Number
     D  XBFUT1                       18    OVERLAY(DSTYP11:54)                  Reserved for Future
     D  XBJRST                        2    OVERLAY(DSTYP11:71)                  Jurisdiction State
     D  XBAST                         2    OVERLAY(DSTYP11:73)                  Accident State Code
     D  XBACDT                        8    OVERLAY(DSTYP11:75)                  Accident/Injury Date
     D  XBDTRP                        8    OVERLAY(DSTYP11:83)                  Reported Insurer Dt
     D  XBCLAS                        4    OVERLAY(DSTYP11:91)                  Classification Code
     D  XBLSCV                        2    OVERLAY(DSTYP11:95)                  Type of Loss Code
     D  XBLSRV                        2    OVERLAY(DSTYP11:97)                  Type of Recovery
     D  XBCOVT                        2    OVERLAY(DSTYP11:99)                  Type of Claim Code
     D  XBSEX                         1    OVERLAY(DSTYP11:101)                 Claimant Gender Code
     D  XBDOBY                        4    OVERLAY(DSTYP11:102)                 Birth Year
     D  XBHDTY                        4    OVERLAY(DSTYP11:106)                 Hire Year
     D  XBFUT2                        4    OVERLAY(DSTYP11:110)                 Reserved for Future
     D  XBAWW                         5    OVERLAY(DSTYP11:114)                 Average Weekly Wage
     D  XBWMTH                        1    OVERLAY(DSTYP11:119)                 Method of AWW
     D  XBPART                        2    OVERLAY(DSTYP11:120)                 Part of Body Code
     D  XBNATR                        2    OVERLAY(DSTYP11:122)                 Nature of Injury
     D  XBCAUS                        2    OVERLAY(DSTYP11:124)                 Cause of Injury Code
     D  XBSTAT                        1    OVERLAY(DSTYP11:126)                 Claim Status Code
     D  XBCLDT                        8    OVERLAY(DSTYP11:127)                 Closing Date
     D  XBTIIA                        9    OVERLAY(DSTYP11:135)                 Tot Incurred Indemni
     D  XBBTC1                        2    OVERLAY(DSTYP11:144)                 Benefit Type Code 1
     D  XBBAP1                        9    OVERLAY(DSTYP11:146)                 Benefit Amt Paid  1
     D  XBWBN1                        6    OVERLAY(DSTYP11:155)                 Weekly Benefit Amt1
     D  XBBTC2                        2    OVERLAY(DSTYP11:161)                 Benefit Type Code 2
     D  XBBAP2                        9    OVERLAY(DSTYP11:163)                 Benefit Amt Paid  2
     D  XBWBN2                        6    OVERLAY(DSTYP11:172)                 Weekly Benefit Amt2
     D  XBBTC3                        2    OVERLAY(DSTYP11:178)                 Benefit Type Code 3
     D  XBBAP3                        9    OVERLAY(DSTYP11:180)                 Benefit Amt Paid  3
     D  XBWBN3                        6    OVERLAY(DSTYP11:189)                 Weekly Benefit Amt3
     D  XBBTC4                        2    OVERLAY(DSTYP11:195)                 Benefit Type Code 4
     D  XBBAP4                        9    OVERLAY(DSTYP11:197)                 Benefit Amt Paid  4
     D  XBWBN4                        6    OVERLAY(DSTYP11:206)                 Weekly Benefit Amt4
     D  XBBTC5                        2    OVERLAY(DSTYP11:212)                 Benefit Type Code 5
     D  XBBAP5                        9    OVERLAY(DSTYP11:214)                 Benefit Amt Paid  5
     D  XBWBN5                        6    OVERLAY(DSTYP11:223)                 Weekly Benefit Amt5
     D  XBFUT3                       17    OVERLAY(DSTYP11:229)                 Reserved for Future
     D  XBVEEA                        9    OVERLAY(DSTYP11:246)                 VR Evaluation EA
     D  XBVMBA                        9    OVERLAY(DSTYP11:255)                 VR Maint Benefit Amt
     D  XBVEDA                        9    OVERLAY(DSTYP11:264)                 VR Education EA
     D  XBVOTA                        9    OVERLAY(DSTYP11:273)                 VR Other Amount Paid
     D  XBTIMA                        9    OVERLAY(DSTYP11:282)                 Total Incurred Med
     D  XBTPMA                        9    OVERLAY(DSTYP11:291)                 Total Paid Medical
     D  XBPIWW                        9    OVERLAY(DSTYP11:300)                 Post-Injury WW
     D  XBIPCT                        3    OVERLAY(DSTYP11:309)                 Impairment %
     D  XBIPBC                        1    OVERLAY(DSTYP11:312)                 Impairment % Basis
     D  XBMMID                        8    OVERLAY(DSTYP11:313)                 Max Medical Imp Date
     D  XBPLAA                        1    OVERLAY(DSTYP11:321)                 Attorney Indicator
     D  XBCVFL                        1    OVERLAY(DSTYP11:322)                 Controverted Indicat
     D  XBCLAP                        9    OVERLAY(DSTYP11:323)                 Claimant Legal Amt
     D  XBELAP                        9    OVERLAY(DSTYP11:332)                 Employer Legal Amt
     D  XBLSC1                        2    OVERLAY(DSTYP11:341)                 Lumpsum Sttlment Cd1
     D  XBLSA1                        9    OVERLAY(DSTYP11:343)                 Lumpsum Sttlment Am1
     D  XBLSC2                        2    OVERLAY(DSTYP11:352)                 Lumpsum Sttlment Cd2
     D  XBLSA2                        9    OVERLAY(DSTYP11:354)                 Lumpsum Sttlment Am2
     D  XBLSC3                        2    OVERLAY(DSTYP11:363)                 Lumpsum Sttlment Cd3
     D  XBLSA3                        9    OVERLAY(DSTYP11:365)                 Lumpsum Sttlment Am3
     D  XBLSC4                        2    OVERLAY(DSTYP11:374)                 Lumpsum Sttlment Cd4
     D  XBLSA4                        9    OVERLAY(DSTYP11:376)                 Lumpsum Sttlment Am4
     D  XBLSC5                        2    OVERLAY(DSTYP11:385)                 Lumpsum Sttlment Cd5
     D  XBLSA5                        9    OVERLAY(DSTYP11:387)                 Lumpsum Sttlment Am5
     D  XBLSC6                        2    OVERLAY(DSTYP11:396)                 Lumpsum Sttlment Cd6
     D  XBLSA6                        9    OVERLAY(DSTYP11:398)                 Lumpsum Sttlment Am6
     D  XBMEIN                        1    OVERLAY(DSTYP11:407)                 Medical Ext Indicato
     D  XBRTWD                        8    OVERLAY(DSTYP11:408)                 Return to Work Date
     D  XBRTWS                        1    OVERLAY(DSTYP11:416)                 Return to Work Stat
     D  XBELEI                        1    OVERLAY(DSTYP11:417)                 Extraordinary Loss
     D  XBFUT4                        8    OVERLAY(DSTYP11:418)                 Reserved for Future
     D  XBPCAR                        5    OVERLAY(DSTYP11:426)                 Previous Carrier
     D  XBPCR1                        5    OVERLAY(DSTYP11:431)                 Previous Carrier Grp
     D  XBPPOL                       18    OVERLAY(DSTYP11:436)                 Previous Policy
     D  XBPPFD                        8    OVERLAY(DSTYP11:454)                 Previous Policy Eff
     D  XBPRPD                        8    OVERLAY(DSTYP11:462)                 Previous Report Date
     D  XBPCLM                       12    OVERLAY(DSTYP11:470)                 Previous Policy Clm
     D  XBRRAM                        9    OVERLAY(DSTYP11:482)                 Recovery Reimburseme
     D  XBFUT5                       10    OVERLAY(DSTYP11:491)                 Reserved for Future
     D*** Following fields are for TX only
     D  XBESSN                        9    OVERLAY(DSTYP11:501)                 Employee SSN
     D  XBEFEN                        9    OVERLAY(DSTYP11:510)                 Employer TAX ID
     D  XBFUT6                        8    OVERLAY(DSTYP11:519)                 Reserved for Future
     D  XBZPIS                        5    OVERLAY(DSTYP11:527)                 Zip code for Inj sit
     D  XBDTFP                        8    OVERLAY(DSTYP11:532)                 Date of first paymen
     D  XBHSCA                        9    OVERLAY(DSTYP11:540)                 Hospital costs paid
     D  XBTPPH                        9    OVERLAY(DSTYP11:549)                 Tot payment to Phy
     D  XBFUT7                        9    OVERLAY(DSTYP11:558)                 Reserved for Future
     D***** End Add ***** 03/08/11 *****************************************
     D*
     D* Record Type 2 Definition - Header
     D DSTYP2          DS           500
     D  X2RTCD                        1    OVERLAY(DSTYP2:1)                    Record Type Code
     D  X2CAR#                        5    OVERLAY(DSTYP2:2)                    Carrier codeCode
     D  X2TRCD                        1    OVERLAY(DSTYP2:7)                    Transaction code
     D  X2RPDT                        4    OVERLAY(DSTYP2:8)                    Report date Code
     D  X2JRST                        2    OVERLAY(DSTYP2:12)                   Jurisdiction state
     D  X2INDC                        5    OVERLAY(DSTYP2:14)                   Indemnity claims
     D  X2PDCI                        5    OVERLAY(DSTYP2:19)                   Potential DCI claims
     D  X2OPNI                        5    OVERLAY(DSTYP2:24)                   Open indemnity clm
     D  X2CPNT                        5    OVERLAY(DSTYP2:29)                   Closed Permanent
     D  X2CNPT                        5    OVERLAY(DSTYP2:34)                   Closed Non-Perma
     D  X2SAMP                        5    OVERLAY(DSTYP2:39)                   Sample of field 10
     D  X2DCIT                        5    OVERLAY(DSTYP2:44)                   DCI Claims Total
     D  X2FUT1                      452    OVERLAY(DSTYP2:49)                   Future Use
     D*
      ***** beg add ******* 05/01/14 ***************************************
      * Record Type 3 Definition
     d dsTyp3          ds           500
     d  x3RtCd                        1    Overlay(dsTyp3:1)                    Record Type Code
     d  x3PvCr                        5    Overlay(dsTyp3:2)                    Prev Carrier Code
     d  x3PvPl                       18    Overlay(dsTyp3:7)                    Prev Policy Number
     d  x3PvCl                       18    Overlay(dsTyp3:25)                   Prev Claim Number
     d  x3PvRp                        6    Overlay(dsTyp3:43)                   Prev Reprt to Ins Dt
     d  x3PvJS                        2    Overlay(dsTyp3:49)                   Prev Jrd St Code
     d  x3Fut1                        2    Overlay(dsTyp3:51)                   Future Use
     d  x3RvCr                        5    Overlay(dsTyp3:53)                   Rev Carrier Code
     d  x3RvPl                       18    Overlay(dsTyp3:58)                   Rev Policy Number
     d  x3RvCl                       18    Overlay(dsTyp3:76)                   Rev Claim Number
     d  x3RvRp                        6    Overlay(dsTyp3:94)                   Rev Reprt to Ins Dt
     d  x3RvJS                        2    Overlay(dsTyp3:100)                  Rev Jrd St Code
     d  x3DltC                        2    Overlay(dsTyp3:102)                  Delete Res Code
     d  x3CICE                        2    Overlay(dsTyp3:104)                  CIC fatal Err Code
     d  x3Fut2                      395    Overlay(dsTyp3:106)                  Future Use
      ***** end add ******* 05/01/14 ***************************************

     D* Record Type 9 Definition - Trailer
     D DSTYP9          DS           500
     D  X9RTCD                        1    OVERLAY(DSTYP9:1)                    Record Type Code
     D  X9RTT1                        7    OVERLAY(DSTYP9:2)                    Record type 1 total
     D  X9RTT2                        7    OVERLAY(DSTYP9:9)                    Record type 2 total
     D  X9RTT3                        7    OVERLAY(DSTYP9:16)                   Record type 3 total
     D  X9FUT1                       35    OVERLAY(DSTYP9:23)                   Reserved for future
     D  X9SUBT                        7    OVERLAY(DSTYP9:58)                   Submission record
     D  X9FUT2                       35    OVERLAY(DSTYP9:65)                   Reserved for future
     D  X9RTTA                        7    OVERLAY(DSTYP9:100)                  Record type A total
     D  X9RTTB                        7    OVERLAY(DSTYP9:107)                  Record type B total
     D  X9FUT3                      367    OVERLAY(DSTYP9:114)                  Reserved for future
     D***** Beg Add ***** 03/08/11 *****************************************
     D* Record Type 9 Definition - Trailer for new file
     D DSTYP91         DS           600
     D  X9RTC1                        1    OVERLAY(DSTYP91:1)                   Record Type Code
     D  X9CGRC                        5    OVERLAY(DSTYP91:2)                   Carrier Group Code
     D  X9FIL1                       35    OVERLAY(DSTYP91:7)                   Filler  9's
     D  X9RT1T                        8    OVERLAY(DSTYP91:42)                  Record type 1 total
     D  X9FU11                      201    OVERLAY(DSTYP91:50)                  Reserved for future
     D***** End Add ***** 03/08/11 *****************************************
     D*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@prd             4
     C                   parm                    @@trcd            1
     C*
     C     dtyymd        PLIST
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcmdp        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *blanks       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM      *BLANK        SRERR
     C*
     C*** BEGIN ADD ****** 10/26/11 ************************************
     C     dtcymd        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM      *BLANK        SRERR
     C*** END   ADD ****** 10/26/11 ************************************
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C* Key list for Fund Aux file
     C     f2ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C*
     C* Key list for Claim File
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    difyr
     C                   kfld                    dicase
     C*
     C* Key list for Employer Detail file
     C     elky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmfyr
     C*
     C* Key list for generic table file
     C     gtky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxtbid
     C                   kfld                    xxkey
     C*
     C* Key list for Payment history file
     C     paky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*** Begin And *** 09/11/15 ***************************************
     C     keyWDPAL5A    klist
     C                   kfld                    DIco#
     C                   kfld                    DIfnd
     C                   kfld                    DIfyr
     C                   kfld                    DIcase
     C                   kfld                    kkDt
     C*
     C     keyWDPAL5B    klist
     C                   kfld                    DIco#
     C                   kfld                    DIfnd
     C                   kfld                    DIfyr
     C                   kfld                    DIcase
     C*** End   And *** 09/11/15 ***************************************
     C*
     C* Key list for PV1 table file
     C     b2ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxkey
     C*
     C***** Beg Add ***** 03/08/11 *****************************************
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C*
     C     diky          klist
     C                   kfld                    xxco#
     C                   kfld                    f1fnd
     C                   kfld                    f1st
     C***** End Add ***** 03/08/11 *****************************************
     C*** BEGIN ADD ****** 11/02/11 ************************************
     C* Key list for Claim status history file
     C     chky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    difyr
     C                   kfld                    dicase
     C                   kfld                    xxendt
     C*** END   ADD ****** 11/02/11 ************************************
     C*
     C*
    C*================================================================
    C*  MAIN PROGRAM
    C*================================================================
     C                   move      @@co#         xxco#             3 0
     C                   move      @@prd         xxprd             4 0
     C                   move      @@trcd        xxtrcd            1 0
     C*
     C*** BEGIN ADD ****** 10/26/11 ************************************
     C* Calculate run period end date
     C                   exsr      srEndDate
     C*** END   ADD ****** 10/26/11 ************************************
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C* Read records from DCI extract file
     C     xxco#         setll     pf101
     C                   dou       %eof(wxf1l)
     C     xxco#         reade     pf101
     C                   if        not %eof(wxf1l) and f1flag = 'I'
     C                   exsr      srMain
     C                   endif                                                  not %eof & f1flag
     C                   enddo
     C*
     C* done process return
     C                   seton                                        lr
     C*
     C*** Begin And *** 09/11/15 ***************************************
    C*================================================================
    C*  GetReportDate - Get the last date to find payment
    C*================================================================
     C     GetReportDate BegSr
     C*
     C                   Z-Add     0             reportMth
     C*
     C     1             Do        13            reportLvl
     C                   If        reportLvl > reportLvlIn
     C                   Leave
     C                   EndIf
     C*
     C                   Select
     C                   When      reportLvl = 1
     C                   Eval      reportMth = reportMth + 18
     C                   Other
     C                   Eval      reportMth = reportMth + 12
     C                   EndSl
     C*
     C                   EndDo
     C*
     C     *CYMD         Move      DIdtRp        lastReportDate
     C                   Eval      lastReportDate = lastReportDate +
     C                                              %Months(reportMth)
     C     *CYMD         Move      lastReportDatekkDt
     C*
     C                   EndSr
    C*================================================================
    C*  GetReportLvl   - Get Reporting Level
    C*================================================================
     C     GetReportLvl  BegSr
     C*
     C                   move      didtrp        xxdd2             2 0
     C                   eval      xxmy = xxprd
     C                   eval      xxyy6 = xxyy1
     C                   eval      xxmm6 = xxmm1
     C* Bring back old logic as new one may result in invalid date e.g. 02/30!!
     C                   eval      xxdd6 = 01
     C                   eval      xxprdt = xxdt6
     C* get date diff in months
     C     *CYMD         move      didtrp        date1
     C     *MDY          move      xxprdt        date2
     C     date2         subdur    date1         xxDiff:*months
     C*
     C* Since day 1 of period is used, Add 1 to get difference as of month-end
     C                   if        xxdd2 > 1
     C                   eval      xxdiff = xxdiff + 1
     C                   endif
     C* Search number of months in array to determine and validate reporting level
     C                   eval      xxIndex = %lookup(xxdiff:arrMths)
     C                   if        xxIndex = 0
     C* Did not match any level
     C* For TX claims - first report is at 6 months
     C                   if        xxNewFormat and dist = 'TX' and xxdiff = 6
     C                   eval      xxrplv = 1
     C                   eval      xxrplv1 = 6
     C                   else
     C                   eval      xxrplv = 0
     C                   endif
     C                   else
     C* Matching level found - store for later use
     C                   eval      xxrplv = arrLvl(xxIndex)
     C                   eval      xxrplv1= arrMths(xxIndex)
     C*** Begin Add *** 03/15/22 ******************************************
     C* No longer reporting new non-TX state claims
     C                   if        xxNewFormat and dist <> 'TX' And
     C                             xxrpLv = 1
     C                   eval      xxrplv = 0
     C                   eval      xxrplv1= 0
     C                   EndIf
     C*** End   Add *** 03/15/22 ******************************************
     C                   if        xxNewFormat and dist = 'TX'
     C                   eval      xxrplv = xxrplv + 1
     C                   endif
     C                   endif                                                  xxIndex = 0
     C*
     C                   EndSr
     C*** End   And *** 09/11/15 ***************************************
    C*================================================================
    C*  srMain - Main process routine
    C*================================================================
     C     srMain        begsr
     C*** END   ADD ****** 03/08/11 ************************************
     C* Initialize Trailer Vars
     C                   eval      xxcnt1 = 0
     C                   eval      xxcnt2 = 0
     C* Initialize Header Vars
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C                   eval      xxcntH = 0
     C                   eval      xxDetCnt = 0
     C*** END   ADD ****** 03/08/11 ************************************
     C                   eval      xxFirst = *on
     C                   exsr      srInitHdrVar
     C* Read thru DCI Data Extract File
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C**** xxco#         setll     pdi01
     C     diky          setll     pdi01
     C*** END   CHG ****** 03/08/11 ************************************
     C                   dou       %eof(wddil)
     C                   eval      difnd = 0
     C                   eval      dist = ' '
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C**** xxco#         reade     pdi01
     C     diky          reade     pdi01
     C*** END   CHG ****** 03/08/11 ************************************
     C* First time process
     C                   if        xxFirst
     C                   eval      xxfnd = difnd
     C                   eval      xxst = dist
     C                   eval      xxFirst = *off
     C                   endif                                                  xxFirst
     C*
     C* Process header on Fund/State break
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C**************     if        xxfnd <> difnd or xxst <> dist
     C* Only if Detail records processed
     C**************     if        xxDetCnt > 0
     C                   if        xxDetCnt > 0 and %eof(wddil)
     C*** END   CHG ****** 03/08/11 ************************************
     C                   exsr      srPrcHeader
     C                   endif                                                  xxDetCnt > 0
     C*
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C* Re-initialize Header Vars
     C***************    exsr      srInitHdrVar
     C***************    eval      xxfnd = difnd
     C***************    eval      xxst = dist
     C***************    endif                                                  xxfnd <> difnd
     C*** END   DEL ****** 03/08/11 ************************************
     C*
     C                   if        not %eof(wddil)
     C***** Beg Add ***** 03/08/11 *****************************************
     C* Determine flag for New or Old processing
     C                   if        didtrp >= 1090901
     C                   eval      xxNewFormat = '1'
     C                   else
     C                   eval      xxNewFormat = '0'
     C                   endif
     C***** End Add ***** 03/08/11 *****************************************
     C* Process Detail record, if not eof
     C***** Beg Chg ***** 03/08/11 *****************************************
     C* Process appropriate Detail record
     C                   exsr      srPrcDetail
     C*
     C                   if        xxrplv > 0 and isValid
     C*
     C                   select
     C                   when      not xxNewFormat
     C                   exsr      srPrcDtlA
     C                   when      xxNewFormat
     C                   exsr      srPrcDtlB
     C                   endsl
     C*** Begin And *** 09/11/15 ***************************************
     C                   EndIf
     C*** End   And *** 09/11/15 ***************************************
     C*
     C* Update DCI extract record
     C*****************  if        xxrplv > 0
     C***** End Chg ***** 03/08/11 *****************************************
     C*** Begin And *** 09/11/15 ***************************************
     C                   If        xxrplv > 0
     C*** End   And *** 09/11/15 ***************************************
     C                   eval      dirplv = xxrplv
     C*** Begin And *** 09/11/15 ***************************************
     C                   EndIf
     C*** End   And *** 09/11/15 ***************************************
     C*
     C* Set maintenance fields
     C                   eval      diuser = q1user
     C                   eval      dipgm  = q1pgm
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         didt
     C                   eval      ditime = dstime
     C*
     C                   update    pdi01
     C*** Begin Del *** 09/11/15 ***************************************
     C**********         endif                                                  xxrplv > 0
     C*** End   Del *** 09/11/15 ***************************************
     C*
     C                   else
     C* If eof and detail records processed - Process Trailer record
     C***** Beg Chg ***** 03/01/12 *****************************************
     C***** Beg Chg ***** 03/08/11 *****************************************
     C*************      if        xxcnt1 > 0
     C*************      exsr      srPrcTrailer
     C*************      endif                                                  xxcnt1 > 0
     C*************      select
     C*************      when      xxcnt1 > 0
     C*************      exsr      srPrcTrailerA
     C*************      when      xxcnt2 > 0
     C*************      exsr      srPrcTrailerB
     C*************      endsl
     C***** End Chg ***** 03/08/11 *****************************************
     C                   if        xxcnt1 > 0
     C                   exsr      srPrcTrailerA
     C                   endif                                                  xxcnt1 > 0
     C*
     C                   if        xxcnt2 > 0
     C                   exsr      srPrcTrailerB
     C                   endif                                                  xxcnt1 > 0
     C***** End Chg ***** 03/01/12 *****************************************
     C*
     C                   endif                                                  not %eof(wddil)
     C*
     C                   enddo                                                  %eof(wddil)
     C*
     C***** Beg Chg ***** 03/08/11 *****************************************
     C***************    seton                                        lr
     C                   endsr
     C***** End Chg ***** 03/08/11 *****************************************
    C*================================================================
    C*  srPrcDetail - Process/Write Detail record
    C*================================================================
     C     srPrcDetail   begsr
     C*
     C* Run Sampling processes (Primary and Stratified)
     C                   move      xxco#         ##co#
     C                   move      xxfnd         ##fnd
     C                   move      difyr         ##fyr
     C                   move      dicase        ##case
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C* Run old or new sampling process
     C                   select
     C                   when      not xxNewFormat
     C                   eval      xxpgmc = 'W8311R2'
     C                   when      xxNewFormat
     C                   eval      xxpgmc = 'W8311R3'
     C                   endsl
     C*** END   ADD ****** 03/08/11 ************************************
     C*** Begin Add *** 04/05/16 ************************************
     C*
     C* Check reporting level, if there is no hit then don't process
     C                   ExSr      GetReportLvl
     C*** Begin Chg *** 08/02/16 ***************************************
     C**********         If        xxrplv <> 0 Or DIrplv <> 0
     C                   If        (DIstat <> ' ' And xxRpLv <> 0) Or
     C                             (DIstat = ' ' And xxRpLv = 1) Or
     C                             (DIstat = ' ' And xxRpLv > 1 And
     C                             (CMncci = '01' Or CMncci = '02'))
     C*** End   Chg *** 08/02/16 ***************************************
     C*** End   Add *** 04/05/16 ************************************
     C*
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C***********        call      'W8311R2'
     C                   call      XXPGMC
     C*** END   CHG ****** 03/08/11 ************************************
     C                   parm                    ##co#             3
     C                   parm                    ##fnd             3
     C                   parm                    ##fyr             3
     C                   parm                    ##case            7
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C**************     parm      *blanks       ##rnd             2
     C                   parm      *blanks       ##rnd             3
     C*** END   CHG ****** 03/08/11 ************************************
     C                   parm      *blanks       ##flags           6
     C*
     C                   move      ##rnd         dirnbr
     C                   move      ##flags       xxflags
     C*** Begin Add *** 04/05/16 ************************************
     C                   EndIf
     C*** End   Add *** 04/05/16 ************************************
     C*
      /free
       //****** beg add ******** 05/01/14 ******************************
       // process Delete Detail record if DIstat = 'D'.
       If DIstat = 'D';
          Exsr srPrcDelete;
          leaveSr;
       Endif;
       //****** end add ******** 05/01/14 ******************************
      /end-free

     C* Validate for include in extract
     C                   exsr      srValidDet
     C                   if        not isValid
     C                   leavesr
     C                   endif                                                  not isValid
     C*
     C* Valid record - Load and write Type 1 (detail) record
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C*************      clear                   dstyp1
     C*** END   DEL ****** 03/08/11 ************************************
     C                   eval      X1RTCD = '1'
     C*
     C* Get carrier code from group aux. file
     C     f2ky          chain     pf201
     C                   if        %found
     C                   move      f2car#        X1CAR#
     C                   move      f2car#        X1CAR1
     C                   endif                                                  %found(pf201)
     C*
     C* Get claims master record
     C     cmky          chain     pcm01
     C                   if        not %found
     C                   leavesr
     C***** Beg Add ***** 09/12/12 *****************************************
     C                   else
     C                   if        not ((xxrplv = dirplv and dirplv > 0)
     C                             or xxrplv = dirplv + 1
     C                             or xxrplv = dirplv - 1)
     C                   if        CMNCCI <> '01' and CMNCCI <> '02'
     C                   eval      isValid = *Off
     C                   leavesr
     C                   endif
     C                   endif
     C***** End Add ***** 09/12/12 *****************************************
     C                   endif                                                  not %found(pcm01)
     C*
     C* Policy # ID - FND + EMP# + DIV
     C                   move      cmfnd         xcfnd             3
     C                   move      cmemp#        xcemp#            9
     C                   move      cmdiv         xcdiv             5
     C                   move      cmcase        xccase            7
     C*
     C*** BEGIN CHG ****** 11/19/12 ************************************
     C*********          eval      X1POL# = xcfnd + xcemp# + xcdiv
     C*********          eval      X1CLM# = xcfnd + xccase
     C                   move      cmfyr         xcfyr             3
     C                   eval      X1POL# = xcfnd + xcemp# + xcfyr
     C                   eval      X1CLM# = xccase
     C*** END   CHG ****** 11/19/12 ************************************
     C*
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C                   eval      X1ESSN = CMSS#
     C                   movel     CMACZ1        X1ZPIS
     C*** END   ADD ****** 03/08/11 ************************************
     C*
     C* Get employer detail record
     C     elky          chain     pel01
     C                   if        not %found
     C                   leavesr
     C                   endif                                                  not %found(pel01)
     C*
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         x1pfdt
     C*
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C***********        move      xxrplv        x1rplv
     C***********
     C***********        if        xxtrcd = 1
     C***********        eval      X1RPCD = '0'
     C***********        else                                                   xxtrcd = 1
     C***********        eval      X1RPCD = '1'
     C***********        endif                                                  xxtrcd = 1
     C*** END   DEL ****** 03/08/11 ************************************
     C*
     C*** END   DEL ****** 03/08/11 ************************************
     C***********        eval      X1FUT1 = *blanks
     C*** END   DEL ****** 03/08/11 ************************************
     C* get NAICS state code
     C                   movel(p)  cmjrst        xxkey
     C     xxkey         chain     pst01
     C                   if        %found
     C                   eval      X1JRST = stnst
     C                   endif                                                  %found(pst01)
     C*
     C                   movel(p)  cmast         xxkey
     C     xxkey         chain     pst01
     C                   if        %found
     C                   eval      X1AST = stnst
     C                   endif                                                  %found(pst01)
     C*
     C                   eval      srdti = cmacdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         x1acdt
     C*
     C                   eval      srdti = cmdtrp
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         x1dtrp
     C*
     C                   move      cmclas        X1CLAS
     C*
     C*** BEGIN ADD ****** 05/09/11 ************************************
     C                   eval      X1LSCV ='00'
     C                   eval      X1LSRV ='00'
     C                   eval      X1COVT ='00'
     C*** END   ADD ****** 05/09/11 ************************************
     C* get codes from generic table file
     C*** BEGIN CHG ****** 05/19/11 ************************************
     C*************      eval      xxtbid = 'CALC'
     C* Try state specific table first and then try generic
     C                   eval      xxtbid = *blanks
     C                   move      'LC'          xxtbid
     C                   movel     cmjrst        xxtbid
     C*** END   CHG ****** 05/19/11 ************************************
     C                   movel(p)  cmlscv        xxkey
     C     gtky          chain     pgt01
     C                   if        %found
     C                   eval      X1LSCV = %subst(gtcf01:3:2)
     C*** BEGIN CHG ****** 05/19/11 ************************************
     C                   else
     C                   movel(p)  '  LC'        xxtbid
     C     gtky          chain     pgt01
     C                   if        %found
     C                   eval      X1LSCV = %subst(gtcf01:3:2)
     C                   endif                                                  %found(pgt01)
     C*** END   CHG ****** 05/19/11 ************************************
     C                   endif                                                  %found(pgt01)
     C*
     C*** BEGIN CHG ****** 05/19/11 ************************************
     C*************      eval      xxtbid = 'LSRV'
     C*************      movel(p)  cmlsrv        xxkey
     C*****gtky****      chain     pgt01
     C*************      if        %found
     C*************      eval      X1LSRV = %subst(gtcf01:3:2)
     C*************      endif                                                  %found(pgt01)
     C*
     C                   movel(p)  cmlsrv        X1LSRV
     C*
     C*************      eval      xxtbid = 'CACT'
     C*************      movel(p)  cmcovt        xxkey
     C*****gtky****      chain     pgt01
     C*************      if        %found
     C*************      eval      X1COVT = %subst(gtcf01:3:2)
     C*************      endif                                                  %found(pgt01)
     C*
     C                   movel(p)  cmcovt        X1COVT
     C*** END   CHG ****** 05/19/11 ************************************
     C* get demographic file record
     C     cmss#         chain     pcd01
     C                   if        %found
     C                   if        cdsex = 'M'
     C                   eval      X1SEX  = '1'
     C                   else
     C                   eval      X1SEX  = '2'
     C                   endif                                                  cdsex = 'M'
     C*
     C                   if        cddtob<>0
     C                   eval      xxdt7 = cddtob
     C                   move      xxyy          x1doby
     C                   if        xxcc = 1
     C                   movel     '20'          x1doby
     C                   else
     C                   movel     '19'          x1doby
     C                   endif                                                  xxcc = 1
     C                   endif                                                  cddtob<>0
     C*
     C                   endif                                                  %found(pcd01)
     C*
     C*** BEGIN ADD ****** 05/09/11 ************************************
     C                   move      '0000'        x1hdty
     C*** END   ADD ****** 05/09/11 ************************************
     C                   if        cmhidt<>0
     C                   eval      xxdt7 = cmhidt
     C                   move      xxyy          x1hdty
     C*** BEGIN CHG ****** 05/09/11 ************************************
     C*************      if        xxcc = 1
     C*************      movel     '20'          x1hdty
     C*************      else
     C*************      movel     '19'          x1hdty
     C*************      endif                                                  xxcc = 1
     C* convert century to year
     C                   z-add     0             xxtmp2            2 0
     C                   eval      xxtmp2 = xxcc + 19
     C                   movel     xxtmp2        x1hdty
     C*** END   CHG ****** 05/09/11 ************************************
     C                   endif                                                  cmhidt<>0
     C*
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C*************      eval      X1FUT2 = *blanks
     C*** END   DEL ****** 03/08/11 ************************************
     C                   eval      xxaww = %int(cmaww)
     C***** Beg Chg ***** 07/07/11 *****************************************
     C**********         move      xxaww         X1AWW
     C                   eval      xxAMT90 = %inth(xxaww)
     C                   move      xxAMT90       X1AWW
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C                   movel     cmwmth        X1WMTH
     C*** BEGIN ADD ****** 05/09/11 ************************************
     C                   if        X1WMTH = *blanks
     C                   eval      X1WMTH = '0'
     C                   endif
     C*** END   ADD ****** 05/09/11 ************************************
     C*** BEGIN CHG ****** 08/15/11 ************************************
     C*************      eval      X1PART = cmpart
     C*************      eval      X1NATR = cmnatr
     C*************      eval      X1CAUS = cmcaus
     C*
     C* Load EDI codes instead of direct values from claims master
     C                   eval      X1PART = '00'
     C                   eval      X1NATR = '00'
     C                   eval      X1CAUS = '00'
     C*
     C                   movel(p)  cmpart        xxkey
     C     b2ky          chain     ppa0t
     C                   if        %found
     C                   eval      X1PART = PAEDIP
     C                   endif
     C*
     C                   movel(p)  cmnatr        xxkey
     C     b2ky          chain     pna01
     C                   if        %found
     C                   eval      X1NATR = NAEDIN
     C                   endif
     C*
     C                   movel(p)  cmcaus        xxkey
     C     b2ky          chain     pca01
     C                   if        %found
     C                   eval      X1CAUS = CAEDIC
     C                   endif
     C*** END   CHG ****** 08/15/11 ************************************
     C*** BEGIN CHG ****** 05/09/11 ************************************
     C************       eval      X1STAT = cmstat
     C*** BEGIN DEL ****** 11/02/11 ************************************
     C* Claim status will be as off run period end date
     C************       if        cmstat='C'
     C************       eval      X1STAT = '1'
     C************       else
     C************       eval      X1STAT = '0'
     C************       endif
     C*** END   DEL ****** 11/02/11 ************************************
     C*** END   CHG ****** 05/09/11 ************************************
     C* Closing date
     C*** BEGIN CHG ****** 11/02/11 ************************************
     C*************      if        cmstat = 'C'
     C*****cmky****      setgt     pch01
     C*****cmky****      readpe    pch01
     C*
     C     chky          setgt     pch01
     C     cmky          readpe    pch01
     C                   if        not %eof
     C                   if        chocst = 'C'
     C                   eval      X1STAT = '1'
      *** BEG   ADD ****** 08/25/14 ************************************
      /Free
       DIstat = 'C';
      /End-Free
      *** END   ADD ****** 08/25/14 ************************************
     C                   eval      srdti = chefdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         x1cldt
     C                   else
     C                   eval      X1STAT = '0'
     C                   move      '00000000'    x1cldt
     C                   endif                                                  chocst='C'
     C                   endif                                                  not %eof
     C*** END   CHG ****** 11/02/11 ************************************
     C*
     C* Sum all payments against reserves
     C                   eval      xxTIIA = 0
     C                   eval      xxTIMA = 0
     C*** BEGIN ADD ****** 11/02/11 ************************************
     C                   clear                   arrRT
     C                   clear                   arrSU
     C* Get incurred amounts from reserve history file
     C     chky          setgt     prh01
     C     cmky          readpe    prh01
     C                   dow       not %eof(wmrhl)
     C*** END   ADD ****** 11/02/11 ************************************
     C*
     C                   for       xxIndex = 1 to 10
     C                   if        arrRT(xxIndex) <> *blanks
     C* Get reserve type
     C                   movel(p)  arrRT(xxIndex)xxkey
     C     b2ky          chain     prt01
     C                   if        %found
     C                   select
     C* Indemnity
     C                   when      rttype = 'C'
     C*** BEGIN CHG ****** 11/02/11 ************************************
     C***************    eval      xxTIIA = xxTIIA + arrAR(xxIndex)
     C***************                              + arrRR(xxIndex)
     C*
     C                   eval      xxTIIA = xxTIIA + arrSU(xxIndex)
     C*** END   CHG ****** 11/02/11 ************************************
     C* Medical
     C                   when      rttype = 'M'
     C*** BEGIN CHG ****** 11/02/11 ************************************
     C***************    eval      xxTIMA = xxTIMA + arrAR(xxIndex)
     C***************                              + arrRR(xxIndex)
     C*
     C                   eval      xxTIMA = xxTIMA + arrSU(xxIndex)
     C*** END   CHG ****** 11/02/11 ************************************
     C                   endsl
     C*
     C                   endif                                                  %found(prt01)
     C                   endif                                                  arrRT(xxIndex)<>' '
     C                   endfor
     C*
     C*** BEGIN ADD ****** 11/02/11 ************************************
     C     cmky          readpe    prh01                                        not %eof(wmrhl)
     C                   enddo
     C*** END   ADD ****** 11/02/11 ************************************
     C*
     C***** Beg Chg ***** 07/07/11 *****************************************
     C************       move      xxTIIA        X1TIIA
     C                   eval      xxAMT90 = %inth(xxTIIA)
     C                   move      xxAMT90       X1TIIA
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C                   eval      x1dtfp = *blanks
     C*** END   ADD ****** 03/08/11 ************************************
     C* Process Payment file for required data
     C                   exsr      srPrcPayFil
     C*
     C* Load from arrays
     C                   eval      X1BTC1 = arrBTC(1)
     C*** BEGIN CHG ****** 07/07/11 ************************************
     C*************      move      arrBAP(1)     x1bap1
     C                   eval      xxBAP = %inth(arrBAP(1))
     C                   move      xxBAP         x1bap1
     C*** END   CHG ****** 07/07/11 ************************************
     C                   eval      xxwbn = 0
     C*** BEGIN DEL ****** 10/18/11 ************************************
     C* Weekly payment will be loaded from compensation rate
     C************       if        arrWKS(1) <> 0
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      eval      xxwbn  = arrBAP(1)/arrWKS(1)
     C************       eval(h)   xxwbn  = %inth(arrBAP(1)/arrWKS(1))
     C*** END   CHG ****** 07/07/11 ************************************
     C*************      endif                                                  arrWKS
     C*** END   DEL ****** 10/18/11 ************************************
     C*
     C*** BEGIN ADD ****** 10/18/11 ************************************
     C                   eval      xxwbn = %inth(cmcrt)
     C                   if        x1btc1 <> '00'
     C                   move      xxwbn         x1wbn1
     C                   else
     C                   move      '000000'      x1wbn1
     C                   endif
     C*** END   ADD ****** 10/18/11 ************************************
     C                   eval      X1BTC2 = arrBTC(2)
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      move      arrBAP(2)     x1bap2
     C                   eval      xxBAP = %inth(arrBAP(2))
     C                   move      xxBAP         x1bap2
     C*** END   CHG ****** 07/07/11 ************************************
     C*** BEGIN DEL ****** 10/18/11 ************************************
     C*************      eval      xxwbn = 0
     C*************      if        arrWKS(2) <> 0
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      eval      xxwbn  = arrBAP(2)/arrWKS(2)
     C*************      eval      xxwbn  = %inth(arrBAP(2)/arrWKS(2))
     C*** END   CHG ****** 07/07/11 ************************************
     C*************      endif                                                  arrWKS
     C*** END   DEL ****** 10/18/11 ************************************
     C*** BEGIN ADD ****** 10/18/11 ************************************
     C                   if        x1btc2 <> '00'
     C                   move      xxwbn         x1wbn2
     C                   else
     C                   move      '000000'      x1wbn2
     C                   endif
     C*** END   ADD ****** 10/18/11 ************************************
     C*
     C                   eval      X1BTC3 = arrBTC(3)
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      move      arrBAP(3)     x1bap3
     C                   eval      xxBAP = %inth(arrBAP(3))
     C                   move      xxBAP         x1bap3
     C*** END   CHG ****** 07/07/11 ************************************
     C*** BEGIN DEL ****** 10/18/11 ************************************
     C*************      eval      xxwbn = 0
     C*************      if        arrWKS(3) <> 0
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      eval      xxwbn  = arrBAP(3)/arrWKS(3)
     C*************      eval      xxwbn  = %inth(arrBAP(3)/arrWKS(3))
     C*** END   CHG ****** 07/07/11 ************************************
     C*************      endif                                                  arrWKS
     C*** END   DEL ****** 10/18/11 ************************************
     C*** BEGIN ADD ****** 10/18/11 ************************************
     C                   if        x1btc3 <> '00'
     C                   move      xxwbn         x1wbn3
     C                   else
     C                   move      '000000'      x1wbn3
     C                   endif
     C*** END   ADD ****** 10/18/11 ************************************
     C*
     C                   eval      X1BTC4 = arrBTC(4)
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      move      arrBAP(4)     x1bap4
     C                   eval      xxBAP = %inth(arrBAP(4))
     C                   move      xxBAP         x1bap4
     C*** END   CHG ****** 07/07/11 ************************************
     C*** BEGIN DEL ****** 10/18/11 ************************************
     C*************      eval      xxwbn = 0
     C*************      if        arrWKS(4) <> 0
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      eval      xxwbn  = arrBAP(4)/arrWKS(4)
     C*************      eval      xxwbn  = %inth(arrBAP(4)/arrWKS(4))
     C*** END   CHG ****** 07/07/11 ************************************
     C*************      endif                                                  arrWKS
     C*** END   DEL ****** 10/18/11 ************************************
     C*** BEGIN ADD ****** 10/18/11 ************************************
     C                   if        x1btc4 <> '00'
     C                   move      xxwbn         x1wbn4
     C                   else
     C                   move      '000000'      x1wbn4
     C                   endif
     C*** END   ADD ****** 10/18/11 ************************************
     C*
     C                   eval      X1BTC5 = arrBTC(5)
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      move      arrBAP(5)     x1bap5
     C                   eval      xxBAP = %inth(arrBAP(5))
     C                   move      xxBAP         x1bap5
     C*** END   CHG ****** 07/07/11 ************************************
     C*** BEGIN DEL ****** 10/18/11 ************************************
     C*************      eval      xxwbn = 0
     C*************      if        arrWKS(5) <> 0
     C*** BEG   CHG ****** 07/07/11 ************************************
     C*************      eval      xxwbn  = arrBAP(5)/arrWKS(5)
     C*************      eval      xxwbn  = %inth(arrBAP(5)/arrWKS(5))
     C*** END   CHG ****** 07/07/11 ************************************
     C*************      endif                                                  arrWKS
     C*** END   DEL ****** 10/18/11 ************************************
     C*** BEGIN ADD ****** 10/18/11 ************************************
     C                   if        x1btc5 <> '00'
     C                   move      xxwbn         x1wbn5
     C                   else
     C                   move      '000000'      x1wbn5
     C                   endif
     C*** END   ADD ****** 10/18/11 ************************************
     C*
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C************       eval      X1FUT3 = *blanks
     C*** END   DEL ****** 03/08/11 ************************************
     C*
     C***** Beg Chg ***** 07/07/11 *****************************************
     C***********        move      XXVEEA        X1VEEA
     C***********        move      XXVMBA        X1VMBA
     C***********        move      XXVEDA        X1VEDA
     C***********        move      XXVOTA        X1VOTA
     C*
     C                   eval      xxAMT90 = %inth(XXVEEA)
     C                   move      xxAMT90       X1VEEA
     C*
     C                   eval      xxAMT90 = %inth(XXVMBA)
     C                   move      xxAMT90       X1VMBA
     C*
     C                   eval      xxAMT90 = %inth(XXVEDA)
     C                   move      xxAMT90       X1VEDA
     C*
     C                   eval      xxAMT90 = %inth(XXVOTA)
     C                   move      xxAMT90       X1VOTA
     C***** End Chg ***** 07/07/11 *****************************************
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C***** Beg Chg ***** 07/07/11 *****************************************
     C************       move      XXRRAM        X1RRAM
     C************       move      XXHSCA        X1HSCA
     C************       move      XXTPPH        X1TPPH
     C*
     C                   eval      xxAMT90 = %inth(XXRRAM)
     C                   move      xxAMT90       X1RRAM
     C*
     C                   eval      xxAMT90 = %inth(XXHSCA)
     C                   move      xxAMT90       X1HSCA
     C*
     C                   eval      xxAMT90 = %inth(XXTPPH)
     C                   move      xxAMT90       X1TPPH
     C***** End Chg ***** 07/07/11 *****************************************
     C*** END   ADD ****** 03/08/11 ************************************
     C*
     C***** Beg Chg ***** 07/07/11 *****************************************
     C************       move      xxtima        X1TIMA
     C************       move      xxtpma        X1TPMA
     C                   eval      xxAMT90 = %inth(xxtima)
     C                   move      xxAMT90       X1TIMA
     C                   eval      xxAMT90 = %inth(xxtpma)
     C                   move      xxAMT90       X1TPMA
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C***** Beg Chg ***** 07/07/11 *****************************************
     C***********        eval      xxaww9 = cmaww
     C                   eval      xxaww9 = %inth(cmaww)
     C***** End Chg ***** 07/07/11 *****************************************
     C                   move      xxaww9        X1PIWW
     C***** Beg Chg ***** 11/15/11 *****************************************
     C*************      move      cmipct        X1IPCT
     C***** Beg Del *** 12/13/19 *****************************************
     C*** Moved to qualify
     C*************      eval      xxipct = %inth(cmipct)
     C*************      move      xxipct        x1ipct
     C***** End Del *** 12/13/19 *****************************************
     C***** End Chg ***** 11/15/11 *****************************************
     C***** Beg Add *** 12/13/19 *****************************************
     C                   move      *zeros        x1ipct
     C***** Beg Add *** 05/13/20 *****************************************
     C                   eval      X1IPBC = ' '
     C***** End Add *** 05/13/20 *****************************************
     C* if main WB% use
     C                   if        cmipct <> 0
     C                   eval      xxipct = %inth(cmipct)
     C                   move      xxipct        x1ipct
     C***** Beg Add *** 05/13/20 *****************************************
     C                   eval      x1ipbc = '1'
     C***** End Add *** 05/13/20 *****************************************
     C* try 1st entry
     C                   else
     C     paky          chain     pby01
     C                   if        %found
     C                   eval      xxipct = %inth(bypct)
     C                   move      xxipct        x1ipct
     C***** Beg Add *** 05/13/20 *****************************************
     C                   eval      x1ipbc = '2'
     C***** End Add *** 05/13/20 *****************************************
     C                   endif                                                  cmipct <> 0
     C* if WB code found, override
     C                   dou       %eof(wmbyl)
     C     paky          reade     pby01
     C                   if        not %eof(wmbyl) and bykey = '99        '
     C                   eval      xxipct = %inth(bypct)
     C                   move      xxipct        x1ipct
     C***** Beg Add *** 05/13/20 *****************************************
     C                   eval      x1ipbc = '1'
     C***** End Add *** 05/13/20 *****************************************
     C                   endif                                                  not %eof & f1flag
     C                   enddo
     C*
     C                   endif                                                  cmipct <> 0
     C***** End Add *** 12/13/19 *****************************************
     C*
     C***** Beg Del *** 05/13/20 *****************************************
     C*****              eval      X1IPBC = ' '
     C***** End Del *** 05/13/20 *****************************************
     C***** Beg Del *** 03/20/20 *****************************************
     C*****              if        cmipct <> 0
     C*****              if        cmpart = '99'
     C*****              eval      X1IPBC = '1'
     C*****              else
     C*****              eval      X1IPBC = '2'
     C*****              endif                                                  cmpart = '99'
     C*****              endif                                                  cmipct <> 0
     C***** End Del *** 03/20/20 *****************************************
     C*
     C                   if        cmmmi <> 0
     C                   eval      srdti = cmmmi
     C                   else
     C                   eval      srdti = cmammi
     C                   endif                                                  cmmmi <> 0
     C*
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         x1mmid
     C                   if        x1mmid = *blanks
     C                   eval      x1mmid = '00000000'
     C                   endif
     C*
     C     cmky          chain     pll01
     C                   if        %found and llplaa <> 0
     C                   eval      X1PLAA = 'Y'
     C                   else
     C                   eval      X1PLAA = 'N'
     C                   endif                                                  %found & llplaa <> 0
     C*
     C                   if        cmcvfl = 'Y'
     C                   eval      X1CVFL = 'Y'
     C                   else
     C                   eval      X1CVFL = 'N'
     C                   endif                                                  cmcvfl = 'Y'
     C*
     C***** Beg Chg ***** 07/07/11 *****************************************
     C************       move      xxclap        x1clap
     C************       move      xxelap        x1elap
     C*
     C                   eval      xxAMT90 = %inth(xxclap)
     C                   move      xxAMT90       x1clap
     C*
     C                   eval      xxAMT90 = %inth(xxelap)
     C                   move      xxAMT90       x1elap
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C* Load from arrays
     C                   eval      X1LSC1 = arrLSC(1)
     C***** Beg Chg ***** 07/07/11 *****************************************
     C*********          move      arrLSA(1)     x1lsa1
     C                   eval      xxAMT90 = %inth(arrLSA(1))
     C                   move      xxAMT90       x1lsa1
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C                   eval      X1LSC2 = arrLSC(2)
     C***** Beg Chg ***** 07/07/11 *****************************************
     C***********        move      arrLSA(2)     x1lsa2
     C                   eval      xxAMT90 = %inth(arrLSA(2))
     C                   move      xxAMT90       x1lsa2
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C                   eval      X1LSC3 = arrLSC(3)
     C***** Beg Chg ***** 07/07/11 *****************************************
     C**********         move      arrLSA(3)     x1lsa3
     C                   eval      xxAMT90 = %inth(arrLSA(3))
     C                   move      xxAMT90       x1lsa3
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C                   eval      X1LSC4 = arrLSC(4)
     C***** Beg Chg ***** 07/07/11 *****************************************
     C*********          move      arrLSA(4)     x1lsa4
     C                   eval      xxAMT90 = %inth(arrLSA(4))
     C                   move      xxAMT90       x1lsa4
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C                   eval      X1LSC5 = arrLSC(5)
     C***** Beg Chg ***** 07/07/11 *****************************************
     C*********          move      arrLSA(5)     x1lsa5
     C                   eval      xxAMT90 = %inth(arrLSA(5))
     C                   move      xxAMT90       x1lsa5
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C                   eval      X1LSC6 = arrLSC(6)
     C***** Beg Chg ***** 07/07/11 *****************************************
     C**********         move      arrLSA(6)     x1lsa6
     C                   eval      xxAMT90 = %inth(arrLSA(6))
     C                   move      xxAMT90       x1lsa6
     C***** End Chg ***** 07/07/11 *****************************************
     C*
     C                   eval      X1MEIN = xxmein
     C*
     C                   eval      srdti = cmrtw
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         x1rtwd
     C                   if        x1rtwd = *blanks
     C                   eval      x1rtwd = '00000000'
     C                   endif
     C*
     C                   select
     C                   when      cmrtws = '1'
     C                   eval      X1RTWS = 'Y'
     C                   when      cmrtws = '2'
     C                   eval      X1RTWS = 'N'
     C                   other
     C                   eval      X1RTWS = ' '
     C                   endsl
     C*** BEGIN ADD ****** 05/09/11 ************************************
     C                   if        cmrtw <> 0 and X1RTWS = ' '
     C                   eval      X1RTWS = 'N'
     C                   endif
     C*** END   ADD ****** 05/09/11 ************************************
     C*
     C*** Begin Chg *** 04/22/20 **************************************
     C**********         if        cmcata <> 0
     C                   if        (CMcata <> 0) Or
     C                             (CMnatr = '83' And CMcaus = '83' And
     C                              CMacDt > 1191201)
     C*** End   Chg *** 04/22/20 **************************************
     C                   eval      X1ELEI = 'Y'
     C                   else
     C*** BEGIN CHG ****** 05/09/11 ************************************
     C*************      eval      X1ELEI = ' '
     C                   eval      X1ELEI = 'N'
     C*** END   CHG ****** 05/09/11 ************************************
     C                   endif                                                  cmcata <> 0
     C*
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C************       eval      X1FUT4 = *blanks
     C*** END   DEL ****** 03/08/11 ************************************
     C                   eval      X1PCAR = '00000'                             do not capture
     C                   eval      X1PCR1 = '00000'                             do not capture
     C                   eval      X1PPOL = *blanks                             do not capture
     C                   eval      X1PPFD = '00000000'                          do not capture
     C                   eval      X1PRPD = '00000000'                          do not capture
     C                   eval      X1PCLM = *blanks                             do not capture
     C*
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C***********        eval      X1FUT5 = *blanks
     C***********
     C* Write to flat file
     C***********        clear                   pdx01
     C***********        move      dstyp1        didata
     C***********        write     pdx01
     C***********
     C* Detail count for trailer
     C***********        eval      xxcnt1 = xxcnt1 + 1
     C* Detail count for header, initialized at fnd/st break
     C***********        eval      xxDetCnt = xxDetCnt + 1
     C***********
     C* Flag Count for header record
     C***********        if        diindc = 'Y'
     C***********        eval      XXINDC = XXINDC + 1
     C***********        endif                                                  diindc = 'Y'
     C***********        if        dipdci = 'Y'
     C***********        eval      XXPDCI = XXPDCI + 1
     C***********        endif                                                  dipdci = 'Y'
     C***********        if        diopni = 'Y'
     C***********        eval      XXOPNI = XXOPNI + 1
     C***********        endif                                                  diopni = 'Y'
     C***********        if        dicpnt = 'Y'
     C***********        eval      XXCPNT = XXCPNT + 1
     C***********        endif                                                  dicpnt = 'Y'
     C***********        if        dicnpt = 'Y'
     C***********        eval      XXCNPT = XXCNPT + 1
     C***********        endif                                                  dicnpt = 'Y'
     C***********        if        disamp = 'Y'
     C***********        eval      XXSAMP = XXSAMP + 1
     C***********        endif                                                  disamp = 'Y'
     C*** END   DEL ****** 03/08/11 ************************************
     C*
     C                   endsr
    C*================================================================
    C*  srPrcDtlA - Process/Write Detail record
    C*================================================================
     C     srPrcDtlA     begsr
     C*
     C* Load and write Type 1 (detail) record - Old format
     C                   clear                   dstyp1
     C*
     C                   eval      XARTCD = X1RTCD
     C                   move      x1car#        XACAR#
     C                   move      x1car#        XACAR1
     C*
     C                   eval      XAPOL# = X1POL#
     C                   eval      XAPFDT = X1PFDT
     C*
     C                   move      xxrplv        xarplv
     C                   if        xxtrcd = 1
     C                   eval      XARPCD = '0'
     C                   else                                                   xxtrcd = 1
     C                   eval      XARPCD = '1'
     C                   endif                                                  xxtrcd = 1
     C*
     C                   eval      XACLM# = X1CLM#
     C                   eval      XAFUT1 = *blanks
     C*
     C                   eval      XAJRST = X1JRST
     C                   eval      XAAST = X1AST
     C                   eval      XAACDT = X1ACDT
     C                   eval      XADTRP = X1DTRP
     C                   move      X1CLAS        XACLAS
     C                   eval      XALSCV = X1LSCV
     C                   eval      XALSRV = X1LSRV
     C                   eval      XACOVT = X1COVT
     C                   eval      XASEX  = X1SEX
     C                   eval      XADOBY = X1DOBY
     C                   eval      XAHDTY = X1HDTY
     C                   eval      XAFUT2 = *blanks
     C*
     C                   eval      XAAWW = X1AWW
     C                   eval      XAWMTH = X1WMTH
     C                   eval      XAPART = X1PART
     C                   eval      XANATR = X1NATR
     C                   eval      XACAUS = X1CAUS
     C                   eval      XASTAT = X1STAT
     C                   eval      XACLDT = X1CLDT
     C                   eval      XATIIA = X1TIIA
     C                   eval      XABTC1 = X1BTC1
     C                   eval      XABAP1 = X1BAP1
     C                   eval      XAWBN1 = X1WBN1
     C                   eval      XABTC2 = X1BTC2
     C                   eval      XABAP2 = X1BAP2
     C                   eval      XAWBN2 = X1WBN2
     C                   eval      XABTC3 = X1BTC3
     C                   eval      XABAP3 = X1BAP3
     C                   eval      XAWBN3 = X1WBN3
     C                   eval      XABTC4 = X1BTC4
     C                   eval      XABAP4 = X1BAP4
     C                   eval      XAWBN4 = X1WBN4
     C                   eval      XABTC5 = X1BTC5
     C                   eval      XABAP5 = X1BAP5
     C                   eval      XAWBN5 = X1WBN5
     C                   eval      XAFUT3 = *blanks
     C*
     C                   eval      XAVEEA = X1VEEA
     C                   eval      XAVMBA = X1VMBA
     C                   eval      XAVEDA = X1VEDA
     C                   eval      XAVOTA = X1VOTA
     C                   eval      XATIMA = X1TIMA
     C                   eval      XATPMA = X1TPMA
     C                   eval      XAPIWW = X1PIWW
     C                   eval      XAIPCT = X1IPCT
     C                   eval      XAIPBC = X1IPBC
     C                   eval      XAMMID = X1MMID
     C                   eval      XAPLAA = X1PLAA
     C                   eval      XACVFL = X1CVFL
     C                   eval      XACLAP = X1CLAP
     C                   eval      XAELAP = X1ELAP
     C***** Beg Chg ***** 07/07/11 *****************************************
     C********           eval      XALSC1 = X1LSA1
     C********           eval      XALSC2 = X1LSA2
     C********           eval      XALSC3 = X1LSA3
     C********           eval      XALSC4 = X1LSA4
     C********           eval      XALSC5 = X1LSA5
     C********           eval      XALSC6 = X1LSA6
     C*
     C                   eval      XALSC1 = X1LSC1
     C                   eval      XALSC2 = X1LSC2
     C                   eval      XALSC3 = X1LSC3
     C                   eval      XALSC4 = X1LSC4
     C                   eval      XALSC5 = X1LSC5
     C                   eval      XALSC6 = X1LSC6
     C***** End Chg ***** 07/07/11 *****************************************
     C                   eval      XALSA1 = X1LSA1
     C                   eval      XALSA2 = X1LSA2
     C                   eval      XALSA3 = X1LSA3
     C                   eval      XALSA4 = X1LSA4
     C                   eval      XALSA5 = X1LSA5
     C                   eval      XALSA6 = X1LSA6
     C                   eval      XAMEIN = X1MEIN
     C                   eval      XARTWD = X1RTWD
     C                   eval      XARTWS = X1RTWS
     C                   eval      XAELEI = X1ELEI
     C                   eval      XAFUT4 = *blanks
     C*
     C                   eval      XAPCAR = '00000'                             do not capture
     C                   eval      XAPCR1 = '00000'                             do not capture
     C                   eval      XAPPOL = *blanks                             do not capture
     C                   eval      XAPPFD = '00000000'                          do not capture
     C                   eval      XAPRPD = '00000000'                          do not capture
     C                   eval      XAPCLM = *blanks                             do not capture
     C                   eval      XAFUT5 = *blanks
     C*
     C* Write to flat file
     C                   clear                   pdx01
     C                   move      dstyp1        didata
     C                   write     pdx01
     C*
     C* Detail count for trailer
     C                   eval      xxcnt1 = xxcnt1 + 1
     C* Detail count for header, initialized at fnd/st break
     C                   eval      xxDetCnt = xxDetCnt + 1
     C*
     C* Flag Count for header record
     C                   if        diindc = 'Y'
     C                   eval      XXINDC = XXINDC + 1
     C                   endif                                                  diindc = 'Y'
     C                   if        dipdci = 'Y'
     C                   eval      XXPDCI = XXPDCI + 1
     C                   endif                                                  dipdci = 'Y'
     C                   if        diopni = 'Y'
     C                   eval      XXOPNI = XXOPNI + 1
     C                   endif                                                  diopni = 'Y'
     C                   if        dicpnt = 'Y'
     C                   eval      XXCPNT = XXCPNT + 1
     C                   endif                                                  dicpnt = 'Y'
     C                   if        dicnpt = 'Y'
     C                   eval      XXCNPT = XXCNPT + 1
     C                   endif                                                  dicnpt = 'Y'
     C                   if        disamp = 'Y'
     C                   eval      XXSAMP = XXSAMP + 1
     C                   endif                                                  disamp = 'Y'
     C*
     C                   endsr
    C*================================================================
    C*  srPrcHeader - Process/Write Header record
    C*================================================================
     C     srPrcHeader   begsr
     C*
     C                   clear                   dstyp2
     C*
     C                   eval      X2RTCD = '2'
     C                   move      f2car#        X2CAR#
     C                   move      xxtrcd        X2TRCD
     C                   eval      xxmy = xxprd
     C                   movel     xxyy1         x2rpdt
     C                   move      xxmm1         x2rpdt
     C                   eval      X2JRST = xxst
     C                   move      XXINDC        X2INDC
     C                   move      XXPDCI        X2PDCI
     C                   move      XXOPNI        X2OPNI
     C                   move      XXCPNT        X2CPNT
     C                   move      XXCNPT        X2CNPT
     C                   move      XXSAMP        X2SAMP
     C                   eval      XXDCIT = XXOPNI + XXCPNT + XXSAMP
     C                   move      XXDCIT        X2DCIT
     C                   eval      X2FUT1 = *blanks
     C*
     C* Write to flat file
     C                   clear                   pdx01
     C                   move      dstyp2        didata
     C                   write     pdx01
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C*************      eval      xxcnt = xxcnt + 1
     C                   eval      xxcntH = xxcntH + 1
     C*** END   CHG ****** 03/08/11 ************************************
     C*
     C                   endsr
    C*================================================================
    C*  srPrcTrailerA - Process/Write Trailer record
    C*================================================================
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C*****srPrcTrailer  begsr
     C     srPrcTrailerA begsr
     C*** END   CHG ****** 03/08/11 ************************************
     C*
     C                   clear                   dstyp9
     C                   eval      X9RTCD = '9'
     C                   move      xxcnt1        X9RTT1
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C***********        move      xxcnt2        X9RTT2
     C                   move      xxcntH        X9RTT2
     C*** END   CHG ****** 03/08/11 ************************************
     C                   move      '0000000'     X9RTT3
     C                   eval      X9FUT1 = *blanks
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C***********        eval      xxsubt = xxcnt1 + xxcnt2
     C                   eval      xxsubt = xxcnt1 + xxcntH
     C*** END   CHG ****** 03/08/11 ************************************
     C                   move      XXSUBT        X9SUBT
     C                   eval      X9FUT2 = *blanks
     C                   move      '0000000'     X9RTTA
     C                   move      '0000000'     X9RTTB
     C                   eval      X9FUT3 = *blanks
     C*
     C* Write to flat file
     C                   clear                   pdx01
     C                   move      dstyp9        didata
     C                   write     pdx01
     C*
     C                   endsr
     C***** Beg Add ***** 03/08/11 *****************************************
    C*================================================================
    C*  srPrcDtlB - Process/Write Detail for new format record
    C*================================================================
     C     srPrcDtlB     begsr
     C*
     C                   clear                   dstyp11
     C*
     C                   eval      XBRTCD = X1RTCD
     C                   move      x1car#        XBCAR#
     C                   move      *blanks       XBCAR1
     C*
     C                   eval      XBPOL# = X1POL#
     C                   eval      XBPFDT = X1PFDT
     C*
     C                   move      xxrplv1       xbrplv
     C                   if        xxtrcd = 1
     C                   eval      XBRPCD = *blank
     C                   else                                                   xxtrcd = 1
     C                   eval      XBRPCD = 'R'
     C                   endif                                                  xxtrcd = 1
     C*
     C                   eval      XBCLM# = X1CLM#
     C                   eval      XBFUT1 = *blanks
     C*
     C                   eval      XBJRST = X1JRST
     C                   eval      XBAST = X1AST
     C                   eval      XBACDT = X1ACDT
     C                   eval      XBDTRP = X1DTRP
     C                   move      X1CLAS        XBCLAS
     C                   eval      XBLSCV = X1LSCV
     C                   eval      XBLSRV = X1LSRV
     C                   eval      XBCOVT = X1COVT
     C                   eval      XBSEX  = X1SEX
     C                   eval      XBDOBY = X1DOBY
     C                   eval      XBHDTY = X1HDTY
     C                   eval      XBFUT2 = *blanks
     C*
     C                   eval      XBAWW = X1AWW
     C                   eval      XBWMTH = X1WMTH
     C                   eval      XBPART = X1PART
     C                   eval      XBNATR = X1NATR
     C                   eval      XBCAUS = X1CAUS
     C                   eval      XBSTAT = X1STAT
     C                   eval      XBCLDT = X1CLDT
     C                   eval      XBTIIA = X1TIIA
     C                   eval      XBBTC1 = X1BTC1
     C                   eval      XBBAP1 = X1BAP1
     C                   eval      XBWBN1 = X1WBN1
     C                   eval      XBBTC2 = X1BTC2
     C                   eval      XBBAP2 = X1BAP2
     C                   eval      XBWBN2 = X1WBN2
     C                   eval      XBBTC3 = X1BTC3
     C                   eval      XBBAP3 = X1BAP3
     C                   eval      XBWBN3 = X1WBN3
     C                   eval      XBBTC4 = X1BTC4
     C                   eval      XBBAP4 = X1BAP4
     C                   eval      XBWBN4 = X1WBN4
     C                   eval      XBBTC5 = X1BTC5
     C                   eval      XBBAP5 = X1BAP5
     C                   eval      XBWBN5 = X1WBN5
     C                   eval      XBFUT3 = *blanks
     C*
     C                   eval      XBVEEA = X1VEEA
     C                   eval      XBVMBA = X1VMBA
     C                   eval      XBVEDA = X1VEDA
     C                   eval      XBVOTA = X1VOTA
     C                   eval      XBTIMA = X1TIMA
     C                   eval      XBTPMA = X1TPMA
     C                   eval      XBPIWW = X1PIWW
     C                   eval      XBIPCT = X1IPCT
     C                   eval      XBIPBC = X1IPBC
     C                   eval      XBMMID = X1MMID
     C                   eval      XBPLAA = X1PLAA
     C                   eval      XBCVFL = X1CVFL
     C                   eval      XBCLAP = X1CLAP
     C                   eval      XBELAP = X1ELAP
     C                   eval      XBLSC1 = X1LSC1
     C                   eval      XBLSC2 = X1LSC2
     C                   eval      XBLSC3 = X1LSC3
     C                   eval      XBLSC4 = X1LSC4
     C                   eval      XBLSC5 = X1LSC5
     C                   eval      XBLSC6 = X1LSC6
     C                   eval      XBLSA1 = X1LSA1
     C                   eval      XBLSA2 = X1LSA2
     C                   eval      XBLSA3 = X1LSA3
     C                   eval      XBLSA4 = X1LSA4
     C                   eval      XBLSA5 = X1LSA5
     C                   eval      XBLSA6 = X1LSA6
     C                   eval      XBMEIN = X1MEIN
     C                   eval      XBRTWD = X1RTWD
     C                   eval      XBRTWS = X1RTWS
     C                   eval      XBELEI = X1ELEI
     C                   eval      XBFUT4 = *blanks
     C*
     C                   eval      XBPCAR = '00000'                             do not capture
     C                   eval      XBPCR1 = '00000'                             do not capture
     C                   eval      XBPPOL = *blanks                             do not capture
     C                   eval      XBPPFD = '00000000'                          do not capture
     C                   eval      XBPRPD = '00000000'                          do not capture
     C                   eval      XBPCLM = *blanks                             do not capture
     C                   eval      XBRRAM = X1RRAM
     C                   eval      XBFUT5 = *blanks
     C* TX only fields --
     C                   if        DIST  = 'TX'
     C                   eval      XBESSN = X1ESSN
     C                   eval      XBEFEN = '000000000'
     C     emky          chain     pem01
     C                   if        %found
     C                   movel     EMFEIN        XBEFEN
     C                   endif
     C*
     C                   eval      XBZPIS = X1ZPIS
     C                   eval      XBFUT6 = *blanks
     C*
     C                   eval      XBDTFP = X1DTFP
     C                   eval      XBHSCA = X1HSCA
     C                   eval      XBTPPH = X1TPPH
     C                   eval      XBFUT7 = *blanks
     C                   endif                                                  DIST = 'TX'
     C* Process state exceptions
     C                   exsr      srPrcExcptn
     C*
     C* Write to new flat file
     C                   clear                   pdx02
     C                   move      dstyp11       didta1
     C                   write     pdx02
     C*
     C* Detail count for trailer
     C                   eval      xxcnt2 = xxcnt2 + 1
     C*
     C                   endsr
    C*================================================================
    C*  srPrcExcptn - Process state exceptions
    C*================================================================
     C     srPrcExcptn   begsr
     C*
     C                   select
     C* FL state exceptions
     C                   when      cmjrst = 'FL'
     C*  1) Tot Incurred Indemnity amt should not include Voc Rehab education exp (indemnity)
     C                   eval      xxtiia = xxtiia - xxcvea
     C***** Beg Chg ***** 07/07/11 *****************************************
     C*******            move      xxtiia        xbtiia
     C                   eval      xxamt90 = %inth(xxtiia)
     C                   move      xxamt90       xbtiia
     C***** End Chg ***** 07/07/11 *****************************************
     C*  2) Total Paid Medical Amount already includes Voc Rehab education exp (Medical)
     C                   eval      xxtpma = xxtpma - xxmvea
     C***** Beg Chg ***** 07/07/11 *****************************************
     C*******            move      xxtpma        xbtpma
     C                   eval      xxamt90 = %inth(xxtpma)
     C                   move      xxamt90       xbtpma
     C***** End Chg ***** 07/07/11 *****************************************
     C*  3) Report first $2500 for reemployment services, above 2500 is not reported
     C                   eval      xxveda = xxcvea + xxmvea
     C                   if        xxveda > 2500
     C                   eval      xxveda =  2500
     C                   endif
     C***** Beg Chg ***** 07/07/11 *****************************************
     C***********        move      xxveda        xbveda
     C                   eval      xxamt90 = %inth(xxveda)
     C                   move      xxamt90       xbveda
     C***** End Chg ***** 07/07/11 *****************************************
     C*  4) Voc rehab evaluation exp amount - no changes required in processing
     C*  5) Voc rehab maintenance benefit amt - Report zero
     C                   eval      xxvmba =  0
     C                   move      xxvmba        xbvmba
     C* MI state exceptions
     C                   when      cmjrst = 'MI'
     C*  1) Tot Incurred Indemnity amt should not include Voc Rehab education exp (indemnity)
     C                   eval      xxtiia = xxtiia - xxcvea
     C***** Beg Chg ***** 07/07/11 *****************************************
     C************       move      xxtiia        xbtiia
     C                   eval      xxamt90 = %inth(xxtiia)
     C                   move      xxamt90       xbtiia
     C***** End Chg ***** 07/07/11 *****************************************
     C*  2) Total Paid Medical Amount already includes Voc Rehab education exp (Medical)
     C                   eval      xxtpma = xxtpma - xxmvea
     C***** Beg Chg ***** 07/07/11 *****************************************
     C*********          move      xxtpma        xbtpma
     C                   eval      xxamt90 = %inth(xxtpma)
     C                   move      xxamt90       xbtpma
     C***** End Chg ***** 07/07/11 *****************************************
     C*  3) Voc Rehab Education Expence Amount - Report zero
     C                   eval      xxveda = 0
     C                   move      xxveda        xbveda
     C*  4) Voc rehab evaluation exp amount - Report zero
     C                   eval      xxveea = 0
     C                   move      xxveea        xbveea
     C*  5) Voc rehab maintenance benefit amt - Report zero
     C                   eval      xxvmba =  0
     C                   move      xxvmba        xbvmba
     C*  6) Voc rehab others paid - Report zero
     C                   eval      xxvota =  0
     C                   move      xxvota        xbvota
     C* PA state exceptions
     C                   when      cmjrst = 'PA'
     C*  1) Classification code - Report PA 3 digit class code as four digit by inserting
     C*                           leading zero
     C                   move      cmclas        xtclas            3 0
     C                   move      xtclas        xbclas
     C* TX state exceptions
     C                   when      cmjrst = 'TX'
     C*  1) Benefits covered by Lump Sum Settlement code - NCCI code '04' - No changes req
     C*  2) Impairment % basis code - Whole body - No changes req
     C*  3) Medical Extinguishment Indicator  - Blanks
     C                   eval      xbmein = ' '
     C*  4) Method for determining preinjury/AWW code - No changes req
     C*
     C                   endsl
     C                   endsr
    C*================================================================
    C*  srPrcTrailerB -Process/Write Trailer record
    C*================================================================
     C     srPrcTrailerB begsr
     C*
     C                   clear                   dstyp91
     C                   eval      X9RTC1 = '9'
     C                   move      f2car#        X9CGRC
     C                   move      xxfil91       X9FIL1
     C*
     C***** Beg Chg ***** 03/01/12 *****************************************
     C*********          eval      xxsubt = xxcnt1
     C                   eval      xxsubt = xxcnt2
     C***** End Chg ***** 03/01/12 *****************************************
     C                   move      XXSUBT        X9RT1T
     C                   move      0             XXSUBT
     C                   eval      X9FU11 = *blanks
     C*
     C* Write to new flat file
     C                   clear                   pdx02
     C                   move      dstyp91       didta1
     C                   write     pdx02
     C*
     C                   endsr
     C***** End Add ***** 03/08/11 *****************************************
    C*================================================================
    C*  srValidDet  - Validate Detail Record for Inclusion
    C*================================================================
     C     srValidDet    begsr
     C* Default to not valid
     C                   eval      isValid = *off
     C                   eval      xxrplv = 0
     C*
     C* Status should be 'S' - Selected
     C                   if        distat <> 'S'
     C*** Begin And *** 09/11/15 ***************************************
     C                   If        DIstat = 'C'
     C* Check to see if there has been any payments made
     C* since it was last processed.
     C                   Eval      reportLvlIn = DIrpLv
     C                   ExSr      GetReportDate
     C                   ExSr      GetReportLvl
     C     keyWDPAL5A    SetGT     ppa05
     C     keyWDPAL5B    ReadE     ppa05
     C                   If        %Eof(WDPAL5)
     C                   Leavesr
     C                   Else
     C* Should be in one of reporting intervals
     C                   If        xxRpLv = 0
     C                   LeaveSr
     C                   Else
     C                   Eval      reportLvlIn = xxRpLv
     C                   ExSr      GetReportDate
     C                   If        PAtrdt > kkDt
     C                   LeaveSr
     C                   EndIf
     C                   EndIf
     C                   EndIf
     C                   Else
     C*** End   And *** 09/11/15 ***************************************
     C                   leavesr
     C*** Begin And *** 09/11/15 ***************************************
     C                   EndIf
     C*** End   And *** 09/11/15 ***************************************
     C                   endif                                                  distat <> 'S'
     C*
     C* Should be in one of reporting intervals
     C****---- Get number of months since reported to Insurer xxPRD - DIDTRP
     C*******-----Build 6,0 (*mdy) date from run period
     C*** Begin Chg *** 09/11/15 ***************************************
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C**********         move      didtrp        xxdd2             2 0
     C*** END   ADD ****** 03/08/11 ************************************
     C**********         eval      xxmy = xxprd
     C**********         eval      xxyy6 = xxyy1
     C**********         eval      xxmm6 = xxmm1
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C*****************  eval      xxdd6 = 01
     C*** BEGIN CHG ****** 03/23/11 ************************************
     C*****************  eval      xxdd6 = xxdd2
     C* Bring back old logic as new one may result in invalid date e.g. 02/30!!
     C**********         eval      xxdd6 = 01
     C*** END   CHG ****** 03/23/11 ************************************
     C*** END   CHG ****** 03/08/11 ************************************
     C**********         eval      xxprdt = xxdt6
     C* get date diff in months
     C******CYMD         move      didtrp        date1
     C******MDY*         move      xxprdt        date2
     C*****date2         subdur    date1         xxDiff:*months
     C*
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C* Since day 1 of period is used, Add 1 to get difference as of month-end
     C***************    eval      xxdiff = xxdiff + 1
     C*** END   DEL ****** 03/08/11 ************************************
     C*** BEGIN ADD ****** 03/23/11 ************************************
     C* Since day 1 of period is used, Add 1 to get difference as of month-end
     C**********         if        xxdd2 > 1
     C**********         eval      xxdiff = xxdiff + 1
     C**********         endif
     C*** END   ADD ****** 03/23/11 ************************************
     C* Search number of months in array to determine and validate reporting level
     C**********         eval      xxIndex = %lookup(xxdiff:arrMths)
     C**********         if        xxIndex = 0
     C* Did not match any level
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C* For TX claims - first report is at 6 months
     C**********         if        xxNewFormat and dist = 'TX' and xxdiff = 6
     C**********         eval      xxrplv = 1
     C**********         eval      xxrplv1 = 6
     C**********         else
     C*** END   ADD ****** 03/08/11 ************************************
     C**********         leavesr
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C**********         endif
     C*** END   ADD ****** 03/08/11 ************************************
     C**********         else
     C* Matching level found - store for later use
     C**********         eval      xxrplv = arrLvl(xxIndex)
     C***** Beg Add ***** 03/08/11 *****************************************
     C**********         eval      xxrplv1= arrMths(xxIndex)
     C**********         if        xxNewFormat and dist = 'TX'
     C**********         eval      xxrplv = xxrplv + 1
     C**********         endif
     C***** End Add ***** 03/08/11 *****************************************
     C**********         endif                                                  xxIndex = 0
     C* Should be in one of reporting intervals
     C                   ExSr      GetReportLvl
     C                   If        xxRpLv = 0
     C                   LeaveSr
     C                   EndIf
     C*** End   Chg *** 09/11/15 ***************************************
     C***** Beg Del ***** 02/27/12 *****************************************
     C***** Beg Add ***** 03/08/11 *****************************************
     C* Check last reported level, exclude if not in sequence
     C******             if        not ((xxrplv = dirplv and dirplv > 0)
     C******                       or xxrplv = dirplv + 1)
     C******             leavesr
     C******             endif
     C***** End Add ***** 03/08/11 *****************************************
     C***** End Del ***** 02/27/12 *****************************************
     C*
     C* Reached here, must be valid record!
     C                   eval      isValid = *on
     C                   endsr
    C*================================================================
    C*  srPrcPayFil - Process Payment File
    C*================================================================
     C     srPrcPayFil   begsr
     C*
     C* Initialize holding vars
     C                   clear                   arrBTC
     C                   clear                   arrBAP
     C*** BEGIN DEL ****** 10/18/11 ************************************
     C*************      clear                   arrWKS
     C*** END   DEL ****** 10/18/11 ************************************
     C                   clear                   arrLSC
     C                   clear                   arrLSA
     C*** BEGIN ADD ****** 05/09/11 ************************************
     C                   move      *ALL'00'      arrBTC
     C                   move      *ALL'00'      arrLSC
     C*** END   ADD ****** 05/09/11 ************************************
     C                   eval      xxveea = 0
     C                   eval      xxvmba = 0
     C                   eval      xxveda = 0
     C                   eval      xxvota = 0
     C                   eval      xxrram = 0
     C                   eval      xxtpma = 0
     C                   eval      xxclap = 0
     C                   eval      xxelap = 0
     C                   eval      xxmein = 'N'
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C                   eval      xxmvea = 0
     C                   eval      xxcvea = 0
     C*** END   ADD ****** 03/08/11 ************************************
     C*** Begin Add *** 09/07/16 ***************************************
     C                   eval      xxHSCA = 0
     C                   eval      xxTPPH = 0
     C*** End   Add *** 09/07/16 ***************************************
     C*
     C* Get Benefit type data
     C     paky          setll     ppa01
     C                   dou       %eof(wdpal)
     C     paky          reade     ppa01
     C*** BEGIN CHG ****** 07/17/13 ************************************
     C*************      if        %eof(wdpal)
     C*** BEGIN CHG ****** 09/02/13 ************************************
     C*************      if        %eof(wdpal) and x1dtfp = *blanks
     C                   if        %eof(wdpal)
     C*** END   CHG ****** 09/02/13 ************************************
     C*** END   CHG ****** 07/17/13 ************************************
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C* If no payments have been made then report benefit type code for anticipated payment
     C* This applicable for both new and old format
     C* xxtiia already holds Total Indemnity reserve setup for the claim
     C*** BEGIN CHG ****** 09/02/13 ************************************
     C*************      if        xxtiia > 0
     C                   if        xxtiia > 0 and x1dtfp = *blanks
     C*** END   CHG ****** 09/02/13 ************************************
     C* Anticipated payment is temporary total ('05')
     C                   eval      arrBTC(1) = '05'
     C                   endif
     C*** END   ADD ****** 03/08/11 ************************************
     C                   leave
     C                   endif                                                  %eof(wdpal)
     C*
     C*** BEGIN ADD ****** 10/26/11 ************************************
     C                   if        papodt > xxendt
     C                   iter
     C                   endif
     C*** END   ADD ****** 10/26/11 ************************************
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C* First payment on claim
     C*** Begin Chg *** 01/27/16 ***************************************
     C**********         if        x1dtfp = *blanks
     C                   if        x1dtfp = *blanks And PAreTp = 'C'
     C*** End   Chg *** 01/27/16 ***************************************
     C                   eval      srdti = patrdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         x1dtfp
     C                   endif
     C*** END   ADD ****** 03/08/11 ************************************
     C                   if        patype = 'V' or patype = 'S' or patype = 'R'
     C                   eval      paamt = paamt * -1
     C                   eval      pawks = pawks * -1
     C                   endif                                                  patype
     C*
     C* Get benefit type from table file
     C                   movel(p)  pabr2         xxkey
     C     b2ky          chain     pb201
     C* Process for benefit type
     C                   if        not %found
     C                   iter
     C                   endif                                                  not %found(wtb2p)
     C*
     C                   if        b2bnft <> *blanks
     C*
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C* Lump sum settlement amount paid is excluded from benfit amount paid
     C* This for both new and old format reporting
     C                   if        b2plcd <> 'L'
     C*** END   ADD ****** 03/08/11 ************************************
     C* Benefit type code process
     C                   eval      xxIndex = %lookup(b2bnft:arrBTC)
     C*
     C                   if        xxIndex = 0
     C*** BEGIN CHG ****** 05/09/11 ************************************
     C**************     eval      xxIndex = %lookup('  ':arrBTC)
     C                   eval      xxIndex = %lookup('00':arrBTC)
     C*** END   CHG ****** 05/09/11 ************************************
     C                   eval      arrBTC(xxIndex) = b2bnft
     C                   endif                                                  xxIndex = 0
     C*
     C                   if        xxIndex > 0
     C                   eval      arrBAP(xxIndex) = arrBAP(xxIndex) + paamt
     C*** Beg   Del ****** 10/18/11 ************************************
     C*********          eval      arrWKS(xxIndex) = arrWKS(xxIndex) + pawks
     C*** End   Del ****** 10/18/11 ************************************
     C                   endif                                                  xxIndex > 0
     C*
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C* Lump sum settlement amount paid reported in separate fields
     C****************** if        b2plcd = 'L'
     C                   else
     C*** END   CHG ****** 03/08/11 ************************************
     C*** Begin Add *** 12/12/11 *******************************************
     C                   if        paretp='M'
     C*** End   Add *** 12/12/11 *******************************************
     C                   eval      xxmein = 'Y'
     C*** Begin Add *** 12/12/11 *******************************************
     C                   endif
     C*** End   Add *** 12/12/11 *******************************************
     C*
     C* Benefits Covered by Lump Sum Settlement Code
     C                   eval      xxIndex = %lookup(b2bnft:arrLSC)
     C*
     C                   if        xxIndex = 0
     C*** BEGIN CHG ****** 05/09/11 ************************************
     C*************      eval      xxIndex = %lookup('  ':arrLSC)
     C                   eval      xxIndex = %lookup('00':arrLSC)
     C*** END   CHg ****** 05/09/11 ************************************
     C                   eval      arrLSC(xxIndex) = b2bnft
     C                   endif                                                  xxIndex = 0
     C*
     C                   if        xxIndex > 0
     C                   eval      arrLSA(xxIndex) = arrLSA(xxIndex) + paamt
     C                   endif                                                  xxIndex > 0
     C*
     C                   endif                                                  b2plcd = 'L'
     C*
     C                   endif                                                  b2bnft <> *blanks
     C*
     C* Process for Vocational Rehab fields
     C                   select
     C                   when      b2minr = '062'                               X1VEEA
     C                   eval      xxveea = xxveea + paamt
     C                   when      b2minr = '201'                               X1VMBA
     C                   eval      xxvmba = xxvmba + paamt
     C                   when      b2minr = '202'                               X1VEDA
     C                   eval      xxveda = xxveda + paamt
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C                   if        paretp = 'C'
     C                   eval      xxcvea = xxcvea + paamt
     C                   endif
     C*** END   ADD ****** 03/08/11 ************************************
     C                   when      b2minr = '007'                               X1VOTA
     C                   eval      xxvota = xxvota + paamt
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C* Recovery Reimbursement amount
     C                   when      patype='R' and (b2minr = '081' or            X1RRAM
     C                             b2minr = '083' or b2minr = '085')
     C                   eval      xxrram = xxrram + paamt
     C* Hospital costs paid
     C                   when      b2minr = '027' or b2minr = '033'             X1HSCA
     C                   eval      xxhsca = xxhsca + paamt
     C* Total payments to physicians
     C                   when      b2minr = '026'                               X1TPPH
     C                   eval      xxtpph = xxtpph + paamt
     C*** END   ADD ****** 03/08/11 ************************************
     C                   endsl
     C*
     C* Total Medical Paid
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C*************      if        b2plcd <> 'L'
     C                   if        b2plcd <> 'L' and paretp = 'M'
     C*** END   CHG ****** 03/08/11 ************************************
     C                   eval      xxtpma = xxtpma + paamt
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C                   if        b2minr = '202'
     C                   eval      xxmvea = xxmvea + paamt
     C                   endif
     C*** END   ADD ****** 03/08/11 ************************************
     C                   endif                                                  b2plcd <> 'L'
     C*
     C* Legal Amount Paid
     C                   select
     C                   when      b2plcd = 'M'
     C                   eval      xxclap = xxclap + paamt
     C                   when      b2plcd = 'N'
     C                   eval      xxelap = xxelap + paamt
     C                   endsl                                                  b2plcd
     C*
     C                   enddo                                                  %eof(wdpal)
     C*
     C                   endsr
    C*================================================================
    C*  srPrcRsrv - Process Reserves when no payments
    C*================================================================
     C     srPrcRsrv     begsr
     C                   endsr
    C*================================================================
    C*  srInitHdrVar- Initialize Header record variables
    C*================================================================
     C     srInitHdrVar  begsr
     C*
     C                   eval      xxDetCnt = 0
     C                   eval      XXINDC = 0
     C                   eval      XXPDCI = 0
     C                   eval      XXOPNI = 0
     C                   eval      XXCPNT = 0
     C                   eval      XXCNPT = 0
     C                   eval      XXSAMP = 0
     C                   eval      XXDCIT = 0
     C*
     C                   endsr
     C*** BEGIN ADD ****** 10/26/11 ************************************
    C*================================================================
    C*  srEndDate - Calculate period end date
    C*================================================================
     C     srEndDate     begsr
     C*
     C                   eval      xxmy = xxprd
     C                   eval      xxyy6 = xxyy1
     C                   eval      xxmm6 = xxmm1
     C                   eval      xxdd6 = 01
     C* Add one month
     C                   eval      date1 = %date(xxdt6:*mdy) + %months(1)
     C* Subtract 1 day to get month end date
     C                   eval      date1 = date1 - %days(1)
     C*
     C                   movel(p)  date1         srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         xxendt            7 0
     C*
     C                   endsr
     C*** END   ADD ****** 10/26/11 ************************************
      /free
       //***** beg add ****** 05/01/14 *********************************
      //==============================================================
      // srPrcDelete - Process Delete Reason record
      //==============================================================
       Begsr srPrcDelete;

       // Load and write Type '3' (detail) record
       x3RtCd = '3';

       // Get carrier code from group aux. file
       Chain f2Ky pf201;
       If %found;
          x3PvCr = %Editc(F2car#:'X');
       Endif;

       // Get claims master record
       Chain cmky pcm01;
       If not %found;
          leaveSr;
       Endif;

       x3PvPl = %Editc(CMfnd:'X') + %Editc(CMemp#:'X') + %Editc(CMfyr:'X');
       x3PvCl = %Editc(Cmfnd:'X') + %Editc(CMfyr:'X') + %Editc(CMcase:'X');

       // get Jurisdiction state code
       xxKey = CMjrst;
       Chain xxkey pst01;
       If %found;
          x3PvJS = STnst;
       Endif;

       x3Fut1 = *blanks;
       x3Fut2 = *blanks;
       x3RvCr = '00000';
       x3RvPl = *blanks;
       x3RvCl = *blanks;
       x3RvRp = '000000';
       x3RvJS = '00';
       x3DltC = '01';
       x3CICE = '00';
      /end-free
     C                   eval      srdti = CMdtrp
     C                   call      'SRCVTDAT'    dtyymd
      /free
       If srDto <> *Blanks;
          x3PvRp = %Subst(srDto:3:6);
       Endif;

       // Write to flat file
       Clear pdx02;
       DIdta1 = dsTyp3;
       Write pdx02;

       Endsr;
       //****** end add ***** 05/01/14 ***************************************
      /end-free

** Reporting Level and Months
01018
02030
03042
04054
05066
06078
07090
08102
09114
10126
11138
