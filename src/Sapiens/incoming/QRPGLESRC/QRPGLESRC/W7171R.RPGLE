     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_OVR dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7171R - Claims Listing by Selection Criteria
     H*****************************************************************
     H* REVISIONS                                                     *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     F* 09/17/18 C:079 Support: 65/73354                              *
     F*                Fix Route to user name                         *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 03/02/13  C:283  SUPPORT: WC065 81710                         *
     H*                  SS# is getting printed with one extra space  *
     H*                  xxx- xx-xxxx. Corrected the position to      *
     H*                  print as xxx-xx-xxxx.                        *
     H*                                                               *
     H* 01/22/13  C:287 Seq#: SY266_OVR                               *
     H*                 Added DFTACTGRP and ACTGRP compile options    *
     H*                                                               *
     H* 01/05/11  C:189  SUPPORT: WC090 75361                         *
     H*                  SS# not printing correctly                   *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/20/09 C:190 PROJECT: WC1473                               *
     H*                 Add Spec and extension field in the report.   *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*
     H*  02/18/05 C:132 PROJECT: WC1221                               *
     H*                - Add new fields for Recoveries.               *
     H*                - Calculate new recovery fields and totals     *
     H*                - Reformat totals on report                    &
     H*                                                               &
     H*  06/21/04 C:53 SUPPORT: 42169
     H*                CHANGE UNIT MANAGER TO CLAIM MANAGER
     H*                                                               *
     H*  08/16/02 C:15  Project: WC1092                               *
     H*                -Changed the report sort output to reflect the *
     H*                 changes made to the prompt screen.            *
     H*                -Added code to sort by jurisdiction state, if  *
     H*                 selected on the primary sort.                 *
     H*                                                               *
     H*  12/18/01 C:15  Support: WC114/29935                          *
     H*                 Added code to make this pgm more efficient    *
     H*                 when ranges are selected.                     *
     H*                                                               *
     H*  05/09/01 C:15  Support: WC033/27392                          *
     H*                 Added code to clear XXSTAF to prevent invalid *
     H*                 page breaking when the staff code sort is not *
     H*                 selected.                                     *
     H*                                                               *
     H*  06/26/00 C:61  PROJECT WC577                                 *
     H*                 Add policy FROM and TO dates to output file.  *
     H*                                                               *
     H*  04/12/00 C:118 PROJECT: WC577
     H*                 Add fields to output workfile WFCLP.          *
     H*                                                               *
     H*
     H*  03/20/00 C:118 PROJECT: WC577                                *
     H*                 Add output for file option.                   *
     H*                                                               *
     H*  10/19/99 C:118 PROJECT: WC577                                *
     H*                 Reformat output listing for readability.      *
     H*                                                               *
     H*  06/02/99 C:61 PROJECT: 101AT                                 *
     H*                Restrict access to claims upon a combination   *
     H*                of user parm flags and Group/Employer          *
     H*                restrictions.                                  *
     H*                                                               *
     H*  03/12/99 C:15 Support: WC033/16897                           *
     H*                Changed the total incurred parms to match the  *
     H*                length of the screen fields.                   *
     H*                                                               *
     H*  02/26/99 C:112 Support Item: 16653                           *
     H*                 Changed code to resolve report error where    *
     H*                 some records are not selected when they should*
     H*                 be.  The Date Entered field (CMENTD) is used  *
     H*                 as one of the record seletcor field range     *
     H*                 check.  This date field is updated only if    *
     H*                 Lost Time Flag (CMLTFL) is 'I' or 'M', and the*
     H*                 field (CMENTD) is zero.  Changing compare     *
     H*                 variable in this program to CMIDT (initial)   *
     H*                 because the CMENTD field, if zero, and used   *
     H*                 in compares within a date range (with the     *
     H*                 lower range limit > 0) and the Lost Time Flag *
     H*                 (CMLTFL) is 'R', the record is excluded from  *
     H*                 report.
     H*                                                               *
     H*  12/02/98 C:112 SUPPORT ITEM 13725                            *
     H*                 Modify name range check to convert 1st letter *
     H*                 of name to capital all the time to ensure     *
     H*                 a valid check                                 *
     H*                                                               *
     H*  06/16/98 C:90 PROJECT: WC828                                 *
     H*                ADD SELECTION OF TOTAL INCURRED RANGE.         *
     H*                                                               *
     H*  03/27/98 C:85 PROJECT: WC577                                 *
     H*                CHANGED ROUTING PAGE SELECTION                 *
     H*                FROM TOTAL INCURRED TO O/S RESERVE.            *
     H*                                                               *
     H*  11/07/97 C:85 PROJECT: WC756                                 *
     H*                ADDED INCURRED AMOUNT TO DETAIL,               *
     H*                STAFF TOTAL, AND GRAND TOTAL.                  *
     H*                ADDED PRINT FOR STAFF TOTAL ON LAST RECORD.    *
     H*                CENTERED COMPANY NAME.                         *
     H*                                                               *
     H*  05/06/97 C:62 PROJECT: WC665                                 *
     H*                1) MODIFY TO RECOGNIZE CMLTFL = 'I' AS LOST    *
     H*                   TIME, OTHERWISE MED ONLY                    *
     H*                2) PROVIDE OPTION TO RUN THE REPORT IN CLAIMANT*
     H*                   NAME ORDER.  ALSO, ADD CLAIM HISTORY DATE TO*
     H*                   THE REPORT.                                 *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     F*
     FWMCML5    IF   E           K DISK
     FWXLIP     O    E             DISK
     FWXLIL     IF   E           K DISK
     F                                     RENAME(PLI01:PLI02)
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWDRBL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWTABL     IF   E           K DISK
      ***** BEG ADD ***** 02/18/05 ************************************
     F**********  PB201                             KIGNORE
     F*********   PCA01                             KIGNORE
     F*********   PPA01                             KIGNORE
     F*********   PRT01                             KIGNORE
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PVS01)
      ***** END ADD ***** 02/18/05 ************************************
     FWMUPL     IF   E           K DISK
     F***** BEGIN ADD **** 03/20/00 ***********************************
     FWFCLP     O    E             DISK
     F***** END   ADD **** 03/20/00 ***********************************
     F***** BEGIN ADD **** 04/12/00 ***********************************
     FWMFNL     IF   E           K DISK
     FWMPRL     IF   E           K DISK
     FWMREL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMALL     IF   E           K DISK
     FWMDPL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     F***** END   ADD **** 04/12/00 ***********************************
      ***** BEG ADD ******* 06/26/00 **********************************
     FWDELL     IF   E           K DISK
      ***** END ADD ******* 06/26/00 **********************************
     F*
      ***** BEG ADD ***** 02/18/05 ************************************
     F*  PAYMENT HISTORY - CO#/FND/FYR/CASE/SEQ#
     F*
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
      ***** END ADD ***** 02/18/05 ************************************
     F*
     F***** BEGIN CHG **** 10/19/99 ***********************************
     F**********QSYSPRT O   F     196     OF     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F***** END   CHG **** 10/19/99 ***********************************
     F*

     D SRT1            S             12    DIM(2) CTDATA PERRCD(1)
     D*** Begin chg *** 08/16/02 **************************************
     D******************* SRT2    1   5 13               SORT OPTION 2
     D SRT2            S             13    DIM(6) CTDATA PERRCD(1)
     D*** End chg ***** 08/16/02 **************************************
     D FSN             S              1    DIM(15)
     D LSN             S              1    DIM(31)
     D NAM             S              1    DIM(47)
     D*
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)
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
     D                                     DIM(10)
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
     D                                     DIM(10)
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
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  DATA                   1    241
     D  FFND                   1      3  0
     D  TFND                   4      6  0
     D  FCAS                   7     13  0
     D  TCAS                  14     20  0
     D  FYR                   21     23  0
     D  TYR                   24     26  0
     D  FCLA                  27     31  0
     D  TCLA                  32     36  0
     D  FRGN                  37     39  0
     D  TRGN                  40     42  0
     D  FAWW                  43     49  2
     D  TAWW                  50     56  2
     D  FEMP                  57     65  0
     D  TEMP                  66     74  0
     D  FRAT                  75     81  2
     D  TRAT                  82     88  2
     D  FDIV                  89     93  0
     D  TDIV                  94     98  0
     D  FACC                  99    106  0
     D  TACC                 107    114  0
     D  FLT                  115    115
     D  TLT                  116    116
     D  FREC                 117    124  0
     D  TREC                 125    132  0
     D  FNCC                 133    134
     D  TNCC                 135    136
     D  FENT                 137    144  0
     D  TENT                 145    152  0
     D  FNAT                 153    154
     D  TNAT                 155    156
     D  FCLO                 157    164  0
     D  TCLO                 165    172  0
     D  FPAR                 173    174
     D  TPAR                 175    176
     D  FPAY                 177    187  2
     D  TPAY                 188    198  2
     D  FCAU                 199    200
     D  TCAU                 201    202
     D  FRES                 203    213  2
     D  TRES                 214    224  2
     D  FSTAT                225    225
     D  FRNM                 226    226
     D  TONM                 227    227
     D  SUPR                 228    230
     D  UMAN                 231    233
     D  CLEX                 234    236
     D  FEAD                 237    239
     D  SCSORT               240    240
     D  SCSRT2               241    241
     D*  DATA STRUCTURE TO LOAD SORT FIELD
     D                 DS
     D  DSSORT                 1     60
     D*  STAFF AND NAME ORDER
     D  DSFLD1                 1      3
     D  DSFLD2                 4     50
     D*  NAME ORDER ONLY
     D  DSFLD3                 1     47
     D*  STAFF AND CLAIM NUMBER ORDER
     D  DSFLD4                 1      3
     D  DSFLD5                 4      6
     D  DSFLD6                 7     13
     D*  CLAIM NUMBER ORDER ONLY
     D  DSFLD7                 1      3
     D  DSFLD8                 4     10
     D*
     D*** BEGIN DLT *** 03/12/99 **************************************
      **** BEGIN ADD **** 06/16/98 ************************************
     D*  DATA STRUCTURE TO LOAD FINC FIELD
     D**********  DS
     D**********                              1   92FINC
     D*  DATA STRUCTURE TO LOAD TINC FIELD
     D**********  DS
     D**********                              1   92TINC
      **** END   ADD **** 06/16/98 ************************************
     D*** END DLT ***** 03/12/99 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* Program specific constants
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')
     D XXAT15          C                   CONST('*NO AUTHORITY* ')
      *** END ADD ****** 05/25/07 **********************************

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C*                                                    ¦
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*                                                    ¦
     C     CMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C*                                                    ¦
     C     CMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCASE
     C*                                                    ¦
     C*** Begin add *** 12/18/01 **************************************
     C     CMKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXEMP#
     C*
     C     CMKY6         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*** End add ***** 12/18/01 **************************************
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKY4         KLIST
     C                   KFLD                    LICO#
     C                   KFLD                    LIFND
     C                   KFLD                    LIFYR
     C                   KFLD                    LICASE
     C*
     C*   KEY LIST FOR THE REIMBURSEMENT FILE
     C*
     C     RBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    XXACDT
     C*
     C*   KEY LIST FOR THE REIMBURSEMENT FILE
     C*
     C     RBKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDATE
     C*
     C     CHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C***** BEGIN ADD **** 04/12/00 ***********************************
     C*
     C*  KEY LIST FOR THE FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE REGION FILE
     C*
     C     REKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMRGN#
     C*
     C*  KEY LIST FOR THE PAYROLL FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMCLAS
     C                   KFLD                    CMACDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMCLAS
     C*
     C*  KEY LIST FOR USER PARAMETER FILE FOR WORK FILE DESCRIPTIONS
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     C*
     C*  KEY LIST FOR ALTERNATE LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    CMCTG1
     C*
     C*  KEY LIST FOR DEPARTMENT FILE
     C*
     C     DPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    CMDEPT
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE CHAPTER FILE
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CLCHAP
     C*
     C***** END   ADD **** 04/12/00 ***********************************
      ***** BEG ADD ******* 06/26/00 **********************************
      *
      *  EMPLOYER DETAIL
      *
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
      ***** END ADD ******* 06/26/00 **********************************
      *
      ***** BEG ADD ***** 02/18/05 ************************************
      *
      *  PAYMENT HISTORY
      *
     C     PAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    CMACDT        XXDATE
     C     *LIKE         DEFINE    CMACDT        FACC1
     C     *LIKE         DEFINE    CMACDT        TACC1
     C     *LIKE         DEFINE    CMACDT        FREC1
     C     *LIKE         DEFINE    CMACDT        TREC1
     C     *LIKE         DEFINE    CMACDT        FENT1
     C     *LIKE         DEFINE    CMACDT        TENT1
     C     *LIKE         DEFINE    CMACDT        FCLO1
     C     *LIKE         DEFINE    CMACDT        TCLO1
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C*** BEG ADD ****** 05/25/07 *******************************************
     C     *LIKE         DEFINE    CMSS#         X1SS#
     C*** END ADD ****** 05/25/07 *******************************************
     C     *LIKE         DEFINE    B2KEY         XXKEY
     C***** BEGIN ADD **** 04/12/00 ***********************************
     C     *LIKE         DEFINE    CPDESC        CLCPDS
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    CONAME        XXCONM
     C     *LIKE         DEFINE    CMACDT        XXCLDT
     C***** END   ADD **** 04/12/00 ***********************************
     C*** Beg Add *** 09/17/18 ****************************************
     C     *LIKE         DEFINE    UPUSNM        strUsNm
     C*** End Add *** 09/17/18 ****************************************
     C*
     C*  DEFINE DETAIL TOTALS FOR INCURRED AMOUNTS
     C*
     C                   Z-ADD     0             ICOMP            10 2
     C                   Z-ADD     0             IMED             10 2
     C                   Z-ADD     0             IOTHR            10 2
     C                   Z-ADD     0             TOTALI           10 2
      ***** BEG ADD ***** 02/18/05 ************************************
     C                   Z-ADD     0             GCOMP            10 2
     C                   Z-ADD     0             GMED             10 2
     C                   Z-ADD     0             GOTHR            10 2
     C                   Z-ADD     0             GTOTAL           10 2
     C                   Z-ADD     0             VCOMP            10 2
     C                   Z-ADD     0             VMED             10 2
     C                   Z-ADD     0             VOTHR            10 2
     C                   Z-ADD     0             VTOTAL           10 2
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C*  DEFINE STAFF TOTALS FOR INCURRED AMOUNTS
     C*
     C                   Z-ADD     0             STICMP           12 2
     C                   Z-ADD     0             STIMED           12 2
     C                   Z-ADD     0             STIOTH           12 2
     C                   Z-ADD     0             STITOT           12 2
      ***** BEG ADD ***** 02/18/05 ************************************
     C                   Z-ADD     0             SGRCMP           12 2
     C                   Z-ADD     0             SGRMED           12 2
     C                   Z-ADD     0             SGROTH           12 2
     C                   Z-ADD     0             SGRTOT           12 2
     C                   Z-ADD     0             SRVCMP           12 2
     C                   Z-ADD     0             SRVMED           12 2
     C                   Z-ADD     0             SRVOTH           12 2
     C                   Z-ADD     0             SRVTOT           12 2
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C*  DEFINE FINAL TOTALS FOR INCURRED AMOUNTS
     C*
     C                   Z-ADD     0             LRICMP           12 2
     C                   Z-ADD     0             LRIMED           12 2
     C                   Z-ADD     0             LRIOTH           12 2
     C                   Z-ADD     0             LRITOT           12 2
      ***** BEG ADD ***** 02/18/05 ************************************
     C                   Z-ADD     0             LRGCMP           12 2
     C                   Z-ADD     0             LRGMED           12 2
     C                   Z-ADD     0             LRGOTH           12 2
     C                   Z-ADD     0             LRGTOT           12 2
     C                   Z-ADD     0             LRVCMP           12 2
     C                   Z-ADD     0             LRVMED           12 2
     C                   Z-ADD     0             LRVOTH           12 2
     C                   Z-ADD     0             LRVTOT           12 2
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATA          241
     C                   PARM                    QQQUE            10
     C                   PARM                    QQCO#             3
     C*** BEGIN DLT *** 03/12/99 **************************************
      **** BEGIN ADD **** 06/16/98 ************************************
     C*******************  PARM           QQFINC  9
     C*******************  PARM           QQTINC  9
      **** END   ADD **** 06/16/98 ************************************
     C*** END DLT ***** 03/12/99 **************************************
     C*** BEGIN ADD *** 03/12/99 **************************************
     C                   PARM                    QQFINC           11
     C                   PARM                    QQTINC           11
     C*** END ADD ***** 03/12/99 **************************************
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   PARM                    QQTYPO            1
     C***** END   ADD **** 03/20/00 ***********************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      ***** BEG ADD ******* 06/26/00 **********************************
      *
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ***** END ADD ******* 06/26/00 **********************************
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C                   MOVEL     QQDATA        DATA
     C*** BEGIN DLT *** 03/12/99 **************************************
      **** BEGIN ADD **** 06/16/98 ************************************
     C******************** MOVE QQFINC    FINC    92
     C******************** MOVE QQTINC    TINC    92
      **** END   ADD **** 06/16/98 ************************************
     C*** END DLT ***** 03/12/99 **************************************
     C*** BEGIN ADD *** 03/12/99 **************************************
     C                   MOVE      QQFINC        FINC             11 2
     C                   MOVE      QQTINC        TINC             11 2
     C*** END ADD ***** 03/12/99 **************************************
     C*
     C* Before flipping date-save dates in work fields in order to
     C* tp print selection criteria on routing page.
     C*
     C     FACC          IFNE      *ZEROS
     C*
     C                   Z-ADD     FACC          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFACC            6 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXFACC
     C*
     C                   ENDIF
     C*
     C     TACC          IFNE      99999999
     C*
     C                   Z-ADD     TACC          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTACC            6 0
     C                   ELSE
     C                   Z-ADD     999999        XXTACC
     C                   ENDIF
     C*
     C     FREC          IFNE      *ZEROS
     C*
     C                   Z-ADD     FREC          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFREC            6 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXFREC
     C*
     C                   ENDIF
     C*
     C     TREC          IFNE      99999999
     C                   Z-ADD     TREC          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTREC            6 0
     C                   ELSE
     C                   Z-ADD     999999        XXTREC
     C                   ENDIF
     C*
     C     FENT          IFNE      *ZEROS
     C*
     C                   Z-ADD     FENT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFENT            6 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXFENT
     C*
     C                   ENDIF
     C*
     C     TENT          IFNE      99999999
     C                   Z-ADD     TENT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTENT            6 0
     C                   ELSE
     C                   Z-ADD     999999        XXTENT
     C                   ENDIF
     C*
     C     FCLO          IFNE      *ZEROS
     C*
     C                   Z-ADD     FCLO          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFCLO            6 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXFCLO
     C*
     C                   ENDIF
     C*
     C     TCLO          IFNE      99999999
     C                   Z-ADD     TCLO          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTCLO            6 0
     C                   ELSE
     C                   Z-ADD     999999        XXTCLO
     C                   ENDIF
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C                   Z-ADD     FFND          XXFND
     C                   Z-ADD     FEMP          XXEMP#
     C                   Z-ADD     FDIV          XXDIV
     C                   Z-ADD     FYR           XXYR
     C                   Z-ADD     FCAS          XXCASE
     C*
     C* Indicators 30-32 are used to determine which key list to use
     C* when reading the claim file                        LJQ 11-01-88
     C*                                                    ¦
     C                   MOVEA     '000'         *IN(30)
     C*
     C* NEED TO FLIP DATES
     C*
     C     FACC          IFNE      *ZEROS
     C*
     C                   Z-ADD     FACC          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FACC1
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             FACC1
     C*
     C                   ENDIF
     C*
     C     TACC          IFNE      99999999
     C                   Z-ADD     TACC          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TACC1
     C                   ELSE
     C                   Z-ADD     9999999       TACC1
     C                   ENDIF
     C*
     C     FREC          IFNE      *ZEROS
     C*
     C                   Z-ADD     FREC          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FREC1
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             FREC1
     C*
     C                   ENDIF
     C*
     C     TREC          IFNE      99999999
     C                   Z-ADD     TREC          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TREC1
     C                   ELSE
     C                   Z-ADD     9999999       TREC1
     C                   ENDIF
     C*
     C     FENT          IFNE      *ZEROS
     C*
     C                   Z-ADD     FENT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FENT1
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             FENT1
     C*
     C                   ENDIF
     C*
     C     TENT          IFNE      99999999
     C                   Z-ADD     TENT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TENT1
     C                   ELSE
     C                   Z-ADD     9999999       TENT1
     C                   ENDIF
     C*
     C     FCLO          IFNE      *ZEROS
     C*
     C                   Z-ADD     FCLO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FCLO1
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             FCLO1
     C*
     C                   ENDIF
     C*
     C     TCLO          IFNE      99999999
     C                   Z-ADD     TCLO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TCLO1
     C                   ELSE
     C                   Z-ADD     9999999       TCLO1
     C                   ENDIF
     C*
     C*****************************************************************
     C*
     C*  READ CLAIM FILE
     C*
     C* Determine if we can use partial keys to get requested data
     C*
     C     CMKY3         SETLL     PCM01
     C*
     C* If from & to fund years are equal use CMKEY2
     C*
     C     FYR           IFEQ      TYR
     C     FFND          ANDEQ     TFND
     C                   SETOFF                                       3031
     C                   SETON                                        32
     C     CMKY2         READE     PCM01                                  99
     C                   ELSE
     C*
     C* If from & to fund are equal use CMKEY1
     C*
     C     FFND          IFEQ      TFND
     C                   SETOFF                                       3032
     C                   SETON                                        31
     C     CMKY1         READE     PCM01                                  99
     C                   ELSE
     C*
     C* If no specifics requested read entire claim file
     C*
     C                   SETOFF                                       3132
     C                   SETON                                        30
     C     CMKY          READE     PCM01                                  99
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C*** Begin add *** 12/18/01 **************************************
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMFYR         XXYR
     C                   MOVE      CMCASE        XXCASE
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C*** End add ***** 12/18/01 **************************************
     C**
     C**   DETERMINE IF FUND IS WITHIN RANGE SELECTED
     C**
     C     CMFND         IFLT      FFND
     C     CMFND         ORGT      TFND
     C*** Begin add *** 12/18/01 **************************************
     C     CMFND         IFGT      TFND
     C     CMKY1         SETGT     PCM01
     C                   END
     C*** End add ***** 12/18/01 **************************************
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF YEAR IS WITHIN RANGE SELECTED
     C     CMFYR         IFLT      FYR
     C     CMFYR         ORGT      TYR
     C*** Begin add *** 12/18/01 **************************************
     C     CMFYR         IFGT      TYR
     C     CMKY2         SETGT     PCM01
     C                   END
     C*** End add ***** 12/18/01 **************************************
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF EMPLOYER IS WITHIN RANGE SELECTED
     C     CMEMP#        IFLT      FEMP
     C     CMEMP#        ORGT      TEMP
     C*** Begin add *** 12/18/01 **************************************
     C     CMEMP#        IFGT      TEMP
     C     CMKY5         SETGT     PCM01
     C                   END
     C*** End add ***** 12/18/01 **************************************
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF DIVISION IS WITHIN RANGE SELECTED
     C     CMDIV         IFLT      FDIV
     C     CMDIV         ORGT      TDIV
     C*** Begin add *** 12/18/01 **************************************
     C     CMDIV         IFGT      TDIV
     C     CMKY6         SETGT     PCM01
     C                   END
     C*** End add ***** 12/18/01 **************************************
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF CASE IS WITHIN RANGE SELECTED
     C     CMCASE        IFLT      FCAS
     C     CMCASE        ORGT      TCAS
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF CLASS IS WITHIN RANGE SELECTED
     C     CMCLAS        IFLT      FCLA
     C     CMCLAS        ORGT      TCLA
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF NCC CODE IS WITHIN RANGE SELECTED
     C     CMNCCI        IFLT      FNCC
     C     CMNCCI        ORGT      TNCC
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF NATURE IS WITHIN RANGE SELECTED
     C     CMNATR        IFLT      FNAT
     C     CMNATR        ORGT      TNAT
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF PART IS WITHIN RANGE SELECTED
     C     CMPART        IFLT      FPAR
     C     CMPART        ORGT      TPAR
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF CAUSE IS WITHIN RANGE SELECTED
     C     CMCAUS        IFLT      FCAU
     C     CMCAUS        ORGT      TCAU
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF LOST TIME FLAG IS WITHIN RANGE SELECTED
     C**
     C     CMLTFL        IFEQ      'I'
     C                   MOVE      'L'           LFLAG             1
     C                   ELSE
     C                   MOVE      'N'           LFLAG
     C                   ENDIF
     C*
     C     LFLAG         IFLT      FLT
     C     LFLAG         ORGT      TLT
     C                   GOTO      TAG04
     C                   ENDIF
     C**
     C**   DETERMINE IF AVERAGE WEEKLY WAGE IS WITHIN RANGE SELECTED
     C     CMAWW         IFLT      FAWW
     C     CMAWW         ORGT      TAWW
     C                   GOTO      TAG04
     C                   END
     C*
     C****************************************************************
     C**
     C**   DETERMINE IF RATE IS WITHIN RANGE SELECTED
     C**
     C                   Z-ADD     CMCRT         RATE              7 2
     C*
     C                   MOVEL     CMJRST        XXJRST
     C                   Z-ADD     CMACDT        XXACDT
     C*
     C     RBKY          SETGT     PRB01
     C     RBKY1         READPE    PRB01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     RATE          IFGT      RBPACP
     C                   Z-ADD     RBPACP        RATE
     C                   END
     C*
     C                   ENDIF
     C*
     C     RATE          IFLT      FRAT
     C     RATE          ORGT      TRAT
     C                   GOTO      TAG04
     C                   END
     C**
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ACCDTE            6 0
     C**
     C**   DETERMINE IF ACCIDENT DATE IS WITHIN RANGE SELECTED
     C     CMACDT        IFLT      FACC1
     C     CMACDT        ORGT      TACC1
     C                   GOTO      TAG04
     C                   END
     C**
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDTE            6 0
     C**
     C**   DETERMINE IF RECEIVED DATE IS WITHIN RANGE SELECTED
     C     CMRCVD        IFLT      FREC1
     C     CMRCVD        ORGT      TREC1
     C                   GOTO      TAG04
     C                   END
     C**
      *** BEGIN CHANGE *** 02/26/99 ***********************************
     C******************   Z-ADDCMENTD    SRDTI
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ENTDTE            6 0
     C**
     C**   DETERMINE IF ENTERED DATE IS WITHIN RANGE SELECTED
     C********** CMENTD    IFLT FENT1                            "
     C********** CMENTD    ORGT TENT1                            "
     C     CMIDT         IFLT      FENT1
     C     CMIDT         ORGT      TENT1
      *** END CHANGE *** 02/26/99 *************************************
     C                   GOTO      TAG04
     C                   END
     C*
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMFYR         XXYR
     C                   MOVE      CMCASE        XXCASE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKY          SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         CLODTE
     C                   GOTO      ARND05
     C                   END
     C     ARND06        TAG
     C**
     C*  If claim is closed, effective date is closed date
     C*  If claim is open, -no closed date-
     C     CHOCST        IFEQ      'C'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         CLODTE            6 0
     C                   ELSE
     C                   Z-ADD     *ZERO         CLODTE
     C                   END
     C     ARND05        TAG
     C**
     C**   DETERMINE IF CLOSED DATE IS WITHIN RANGE SELECTED
     C     CHEFDT        IFLT      FCLO1
     C     CHEFDT        ORGT      TCLO1
     C                   GOTO      TAG04
     C                   END
     C**
     C                   Z-ADD     *ZERO         PCOMP
     C                   Z-ADD     *ZERO         PMED
     C                   Z-ADD     *ZERO         POTHR
     C                   Z-ADD     *ZERO         RCOMP
     C                   Z-ADD     *ZERO         RMED
     C                   Z-ADD     *ZERO         ROTHR
     C                   Z-ADD     *ZERO         TOTPMT
     C                   Z-ADD     *ZERO         TOTRES
      **** BEGIN ADD **** 06/16/98 ************************************
     C                   Z-ADD     *ZERO         ICOMP
     C                   Z-ADD     *ZERO         IMED
     C                   Z-ADD     *ZERO         IOTHR
     C                   Z-ADD     *ZERO         TOTINC
      **** END   ADD **** 06/16/98 ************************************
     C**
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C                   MOVEL     CMT(R)        XXKEY
     C**
     C*  GET RESERVE TYPE CODE FILE
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        IFEQ      'C'
     C                   MOVE      '1'           *IN20
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   MOVE      '1'           *IN21
     C                   END
     C                   END
     C**
     C**  ACCUMULATE PAYMENT AMOUNTS
     C*** BEGIN DLT *** 03/12/99 **************************************
     C******20********     ADD  CMA,R     PCOMP   92
     C******21********     ADD  CMA,R     PMED    92
     C**N20N21********     ADD  CMA,R     POTHR   92
     C*** END DLT ***** 03/12/99 **************************************
     C*** BEGIN ADD *** 03/12/99 **************************************
     C   20              ADD       CMA(R)        PCOMP            11 2
     C   21              ADD       CMA(R)        PMED             11 2
     C  N20
     CANN21              ADD       CMA(R)        POTHR            11 2
     C*** END ADD ***** 03/12/99 **************************************
     C**
     C**  ACCUMULATE RESERVE AMOUNTS, PAYMENT AMOUNTS
     C*** BEGIN DLT *** 03/12/99 **************************************
     C******20********     ADD  CMR,R     RCOMP   92
     C******21********     ADD  CMR,R     RMED    92
     C**N20N21********     ADD  CMR,R     ROTHR   92
     C*** END DLT ***** 03/12/99 **************************************
     C*** BEGIN ADD *** 03/12/99 **************************************
     C   20              ADD       CMR(R)        RCOMP            11 2
     C   21              ADD       CMR(R)        RMED             11 2
     C  N20
     CANN21              ADD       CMR(R)        ROTHR            11 2
     C*** END ADD ***** 03/12/99 **************************************
     C**
     C                   ENDDO
     C**
     C*** BEGIN CHG *** 03/12/99 **************************************
     C********** PCOMP     ADD  PMED      TOTPMT  92
     C     PCOMP         ADD       PMED          TOTPMT           11 2
     C*** END CHG ***** 03/12/99 **************************************
     C                   ADD       POTHR         TOTPMT
     C*** BEGIN CHG *** 03/12/99 **************************************
     C********** RCOMP     ADD  RMED      TOTRES  92
     C     RCOMP         ADD       RMED          TOTRES           11 2
     C*** END CHG ***** 03/12/99 **************************************
     C                   ADD       ROTHR         TOTRES
     C*** BEGIN DLT *** 03/12/99 **************************************
      **** BEGIN ADD **** 06/16/98 ************************************
     C********** TOTPMT    ADD  TOTRES    TOTINC  92
      **** END   ADD **** 06/16/98 ************************************
     C*** END DLT ***** 03/12/99 **************************************
     C*** BEGIN ADD *** 03/12/99 **************************************
     C     TOTPMT        ADD       TOTRES        TOTINC           11 2
     C*** END ADD ***** 03/12/99 **************************************
     C**
     C**   DETERMINE IF PAYMENT AMOUNT IS WITHIN RANGE SELECTED
     C     TOTPMT        IFLT      FPAY
     C     TOTPMT        ORGT      TPAY
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF RESERVE AMOUNT IS WITHIN RANGE SELECTED
     C     TOTRES        IFLT      FRES
     C     TOTRES        ORGT      TRES
     C                   GOTO      TAG04
     C                   END
     C*
      **** BEGIN ADD **** 06/16/98 ************************************
     C**
     C**   DETERMINE IF INCURRED AMOUNT IS WITHIN RANGE SELECTED
     C     TOTINC        IFLT      FINC
     C     TOTINC        ORGT      TINC
     C                   GOTO      TAG04
     C                   END
      **** END   ADD **** 06/16/98 ************************************
     C****************************************************************
     C*
     C*  CHAIN TO EMPLOYER FILE TO CHECK REGION
     C*
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C                   GOTO      TAG04
     C                   END
     C**
     C**   DETERMINE IF REGION IS WITHIN RANGE SELECTED
     C     EMRGN#        IFLT      FRGN
     C     EMRGN#        ORGT      TRGN
     C                   GOTO      TAG04
     C                   END
     C*
     C* DETERMINE IF CLAIM STATUS IS EQUAL TO SELECTED STATUS
     C*                                                    LJQ 04-26-88
     C     FSTAT         IFNE      *BLANK
     C     CMSTAT        IFNE      FSTAT
     C                   GOTO      TAG04
     C                   END
     C                   END
     C**
     C** CHECK TO SEE IF THE CLAIMS SUPERVISOR IS EQUAL TO THE
     C**  SELECTED SUPERVISOR
     C**
     C     SUPR          IFNE      *BLANKS
     C     CMSUPR        ANDNE     SUPR
     C                   GOTO      TAG04
     C                   END
     C**
     C** CHECK TO SEE IF THE UNIT MANAGER IS EQUAL TO THE
     C**  SELECTED UNIT MANAGER
     C**
     C     UMAN          IFNE      *BLANKS
     C     CMUMAN        ANDNE     UMAN
     C                   GOTO      TAG04
     C                   END
     C**
     C** CHECK TO SEE IF THE ADJUSTER (CLAIMS EXAMINER) IS EQUAL
     C**  TO THE SELECTED ADJUSTER
     C**
     C     CLEX          IFNE      *BLANKS
     C     CMCLEX        ANDNE     CLEX
     C                   GOTO      TAG04
     C                   END
     C**
     C** CHECK TO SEE IF THE FIELD ADJUSTER  IS EQUAL
     C**  TO THE SELECTED FIELD ADJUSTER
     C**
     C     FEAD          IFNE      *BLANKS
     C     CMFEAD        ANDNE     FEAD
     C                   GOTO      TAG04
     C                   END
     C**
     C** IF A SORT OPTION FOR STAFF CODE WAS SELECTED THEN LOAD
     C**  THE APPROPRIATE STAFF CODE TO THE SORT FIELD
     C**
     C     SCSRT2        IFNE      'N'
     C*
     C                   MOVE      *BLANKS       XXSTAF
     C*
     C   43              MOVE      CMSUPR        XXSTAF
     C   44              MOVE      CMUMAN        XXSTAF
     C   45              MOVE      CMCLEX        XXSTAF
     C   46              MOVE      CMFEAD        XXSTAF
     C*** Begin add *** 08/16/02 **************************************
     C   48              MOVEL(P)  CMJRST        XXSTAF
     C*** End add ***** 08/16/02 **************************************
     C*
     C                   END
     C**
     C**  NOW GET CLAIMANTS NAME TO SEE IF THE FIRST INITAL OF THE
     C**   LAST NAME FALLS WITHIN THE SELECTED CRITERIA.
     C**
     C                   MOVE      *BLANK        XXWK1             1
     C*
     C                   MOVEL     CMSS#         XXSS#
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         XXWK1
     C                   EXSR      SRNAME
     C                   END
     C**
     C     XXWK1         IFNE      *BLANK
     C**** BEGIN ADD *** 12/02/98 ************************************
     C* This will force convert the first letter of the last name to
     C* a capital letter to ensure a valid comparison
     C     LO:UP         XLATE     XXWK1         XXWK1
     C**** END ADD ***** 12/02/98 ************************************
     C     XXWK1         IFLT      FRNM
     C     XXWK1         ORGT      TONM
     C                   GOTO      TAG04
     C                   END
     C                   ELSE
     C                   GOTO      TAG04
     C                   END
     C**
     C** NOW THAT THE RECORD HAS PASSED ALL THE SELECTION CRITERIAS,
     C** WRITE THE RECORD TO THE WORK FILE
     C**
     C                   MOVE      CMCO#         LICO#
     C                   MOVE      CMFND         LIFND
     C                   MOVE      CMFYR         LIFYR
     C                   MOVE      CMCASE        LICASE
     C*
     C                   MOVE      *BLANKS       DSSORT
     C*
     C     *IN41         IFEQ      '1'
     C     *IN42         ANDEQ     '0'
     C                   MOVE      XXSTAF        DSFLD1
     C                   MOVE      XXWK47        DSFLD2
     C                   END
     C*
     C     *IN41         IFEQ      '1'
     C     *IN42         ANDEQ     '1'
     C                   MOVE      XXWK47        DSFLD3
     C                   END
     C*
     C     *IN40         IFEQ      '1'
     C     *IN42         ANDEQ     '0'
     C                   MOVE      XXSTAF        DSFLD4
     C                   MOVE      CMFYR         DSFLD5
     C                   MOVE      CMCASE        DSFLD6
     C                   END
     C*
     C     *IN40         IFEQ      '1'
     C     *IN42         ANDEQ     '1'
     C                   MOVE      CMFYR         DSFLD7
     C                   MOVE      CMCASE        DSFLD8
     C                   END
     C*
     C                   MOVE      DSSORT        LISRT
     C*
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * Check for Fund Restriction
      *
     C                   SETOFF                                       72
      *
     C     ##RCDS        IFEQ      *ON
     C     UPXFND        ANDEQ     'Y'
     C                   CALL      'SA008R'
     C                   PARM                    UPCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C     *IN72         PARM      *OFF          ##RTCD            1
     C                   PARM      *BLANK        ##AUTH            1
     C                   PARM                    CMFND
     C                   ENDIF
      *
      * Check for Employer Restriction
      *
     C  N72##RCDS        IFEQ      *ON
     C     UPXEMP        ANDEQ     'Y'
     C                   CALL      'SA008R'
     C                   PARM                    UPCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C     *IN72         PARM      *OFF          ##RTCD            1
     C                   PARM      *BLANK        ##AUTH            1
     C                   PARM                    CMFND
     C                   PARM                    CMEMP#
     C                   ENDIF
      *
     C     *IN72         IFEQ      *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C                   WRITE     PLI01
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   ENDIF
      ***** END ADD ******* 06/02/99 **********************************
     C**
     C     TAG04         TAG
     C*                                                    ¦
     C   30CMKY          READE     PCM01                                  99
     C   31CMKY1         READE     PCM01                                  99
     C   32CMKY2         READE     PCM01                                  99
     C*                                                    ¦
     C                   ENDDO
     C**
     C** NOW EXECUTED SUBROUTINE SRPRT TO READ THE WORK FILE AND
     C**  PRINT THE INFORMATION.
     C**
     C                   EXSR      SRPRT
     C**
     C     END           TAG
     C*
     C*
     C                   SETON                                        LR
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*
     C*  CHECK OUTPUT TYPE BEFORE PRINTING
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C***** END   ADD **** 03/20/00 ***********************************
     C     *INLR         IFEQ      *ON
     C     *IN22         IFEQ      *ON
     C*
     C*
     C                   EXCEPT    HEAD
     C                   ENDIF
     C*
     C** PRINT LAST STAFF TOTAL
     C*
     C     *IN43         IFEQ      *ON
     C     *IN44         OREQ      *ON
     C     *IN45         OREQ      *ON
     C     *IN46         OREQ      *ON
     C*** Begin add *** 08/16/02 **************************************
     C     *IN48         OREQ      *ON
     C*** End add ***** 08/16/02 **************************************
     C*
      ***** BEG ADD ***** 02/18/05 ************************************
     C* Calculate Gross Amounts for Staff Total Lines
     C     SRVCMP        ADD       SRVMED        SRVTOT
     C                   ADD       SRVOTH        SRVTOT
     C     STPCMP        ADD       SRVCMP        SGRCMP
     C     STPMED        ADD       SRVMED        SGRMED
     C     STPOTH        ADD       SRVOTH        SGROTH
     C     STPTOT        ADD       SRVTOT        SGRTOT
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C                   EXCEPT    STAFTL
     C                   ENDIF
     C*
      ***** BEG ADD ***** 02/18/05 ************************************
     C* Calculate Gross Amounts for Staff Total Lines
     C     LRVCMP        ADD       LRVMED        LRVTOT
     C                   ADD       LRVOTH        LRVTOT
     C     LRPCMP        ADD       LRVCMP        LRGCMP
     C     LRPMED        ADD       LRVMED        LRGMED
     C     LRPOTH        ADD       LRVOTH        LRGOTH
     C     LRPTOT        ADD       LRVTOT        LRGTOT
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C                   EXCEPT    TOTALS
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   ENDIF
     C***** END   ADD **** 03/20/00 ***********************************
     C*
      /Free
       // ***** Beg Add ***** 08/26/14 ********************************
       If qqTypo = 'P' or qqTypo = 'B';
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ********************************
      /End-free
     C                   ENDIF
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C*                                                    MLS 1/15/88
     C*   SET-UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXSTAF
     C*
     C     SCSORT        COMP      'C'                                    40
     C     SCSORT        COMP      'N'                                    41
     C*
     C     SCSRT2        COMP      'N'                                    42
     C*
     C     SCSRT2        COMP      'S'                                    43
     C*** BEG CHG *** 06/21/04 *****************************
     C********** SCSRT2    COMP 'U'                      44
     C     SCSRT2        COMP      'C'                                    44
     C*** END CHG *** 06/21/04 *****************************
     C     SCSRT2        COMP      'A'                                    45
     C     SCSRT2        COMP      'F'                                    46
     C*** Begin add *** 08/16/02 **************************************
     C     SCSRT2        COMP      'J'                                    48
     C*** End add ***** 08/16/02 **************************************
     C*
     C     SUPR          IFEQ      *BLANKS
     C                   MOVE      '*ALL'        RSSUPR            4
     C                   ELSE
     C                   MOVE      SUPR          RSSUPR
     C                   END
     C     UMAN          IFEQ      *BLANKS
     C                   MOVE      '*ALL'        RSUMAN            4
     C                   ELSE
     C                   MOVE      UMAN          RSUMAN
     C                   END
     C     CLEX          IFEQ      *BLANKS
     C                   MOVE      '*ALL'        RSCLEX            4
     C                   ELSE
     C                   MOVE      CLEX          RSCLEX
     C                   END
     C     FEAD          IFEQ      *BLANKS
     C                   MOVE      '*ALL'        RSFEAD            4
     C                   ELSE
     C                   MOVE      FEAD          RSFEAD
     C                   END
     C*
     C   40              MOVE      SRT1(1)       RSSORT           12
     C   41              MOVE      SRT1(2)       RSSORT
     C*
     C   42              MOVE      SRT2(1)       RSSRT2           13
     C   43              MOVE      SRT2(2)       RSSRT2
     C   44              MOVE      SRT2(3)       RSSRT2
     C   45              MOVE      SRT2(4)       RSSRT2
     C   46              MOVE      SRT2(5)       RSSRT2
     C*** Begin add *** 08/16/02 **************************************
     C   48              MOVE      SRT2(6)       RSSRT2
     C*** End add ***** 08/16/02 **************************************
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   MOVEL     QQTYPO        RSTYPO            1
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C     *IN42         IFEQ      '0'
     C                   SETON                                        47
     C                   END
     C*
     C*
     C     UPKY          CHAIN     PUP01                              60
      /Free
                         strUsNm = UPusNm;
      /End-Free
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C     UPXFND        IFEQ      'Y'
     C     UPXEMP        OREQ      'Y'
      *
      * Check for existence of Fund/Emplr restriction records for this
      *   user.  SMAZP
      *
     C                   CALL      'SA008R'
     C                   PARM                    UPCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C                   PARM      *BLANK        ##AUTH            1
      *
     C                   SETON                                        71
      *
     C                   ENDIF
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      UPPRDT        XXDATE
     C*
     C     COKY          CHAIN     SMCOL                              60
     C*
     C*  CENTER COMPANY NAME
     C*
     C***** BEGIN ADD **** 04/12/00 ***********************************
     C                   MOVEL(P)  CONAME        XXCONM
     C***** END   ADD **** 04/12/00 ***********************************
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* * * OPEN PRINT FILE
     C                   OPEN      QSYSPRT
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * Left Justify Title in Field
      *
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF
     C                   ENDIF
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*
     C*   CHECK OUTPUT TYPE
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C***** END   ADD **** 03/20/00 ***********************************
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   ENDIF
     C***** END   ADD **** 03/20/00 ***********************************
     C                   END
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C                   MOVE      '1'           *IN11
     C                   MOVE      '1'           *IN22
     C*
     C                   ENDSR
     C*****************************************************************
     C**  SRPRT - SUBROUTINE TO READ WORK FILE AND PRINT INFORMATION
     C*****************************************************************
     C**
     C     SRPRT         BEGSR
     C*
     C                   MOVE      *BLANKS       SVSTAF            3
     C                   MOVE      *BLANKS       XXSTAF            3
     C*
     C                   READ      PLI02                                  50
     C                   MOVE      LISRT         DSSORT
     C     *IN42         IFEQ      '0'
     C                   MOVE      DSFLD1        SVSTAF
     C                   END
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*  SEPARATE THE SORT FIELD INTO ITS APPROPRIATE FIELDS
     C*
     C                   MOVE      LISRT         DSSORT
     C*
     C* GO TO CLAIM FILE AND GET CLAIM INFORMATION
     C*
     C     CMKY4         CHAIN     PCM02                              60
     C     *IN60         IFEQ      '0'
     C*
      ***** BEG ADD ***** 02/18/05 ************************************
     C* Get the Recovered amounts for this claim.
     C                   EXSR      SRRCVR
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C*  LOAD THE APPROPRIATE STAFF CODE IF THE STAFF CODE SORT
     C*   FLAG IS SELECTED
     C*
     C*** Begin add *** 05/09/01 **************************************
     C                   MOVE      *BLANKS       XXSTAF
     C*** End add ***** 05/09/01 **************************************
     C     *IN42         IFEQ      '0'
     C   43              MOVE      CMSUPR        XXSTAF
     C   44              MOVE      CMUMAN        XXSTAF
     C   45              MOVE      CMCLEX        XXSTAF
     C   46              MOVE      CMFEAD        XXSTAF
     C*** Begin add *** 08/16/02 **************************************
     C   48              MOVEL     CMJRST        XXSTAF
     C*** End add ***** 08/16/02 **************************************
     C                   END
     C*
     C*  CHECK TO SEE IF THE STAFF CODE HAS CHANGED IF THE SORT
     C*   CRITERIA WAS SELECTED
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*
     C*  CHECK OUTPUT TYPE BEFORE PRINTING
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C     XXSTAF        IFNE      SVSTAF
     C     *INOF         IFEQ      '1'
     C*
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
      ***** BEG ADD ***** 02/18/05 ************************************
     C* Calculate Gross Amounts for Staff Total Lines
     C     SRVCMP        ADD       SRVMED        SRVTOT
     C                   ADD       SRVOTH        SRVTOT
     C     STPCMP        ADD       SRVCMP        SGRCMP
     C     STPMED        ADD       SRVMED        SGRMED
     C     STPOTH        ADD       SRVOTH        SGROTH
     C     STPTOT        ADD       SRVTOT        SGRTOT
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C                   EXCEPT    STAFTL
     C*
     C                   Z-ADD     0             STRCMP
     C                   Z-ADD     0             STRMED
     C                   Z-ADD     0             STROTH
     C                   Z-ADD     0             STRTOT
     C                   Z-ADD     0             STPCMP
     C                   Z-ADD     0             STPMED
     C                   Z-ADD     0             STPOTH
     C                   Z-ADD     0             STPTOT
     C                   Z-ADD     0             STCASE
      ***** BEG ADD ***** 02/18/05 ************************************
     C                   Z-ADD     0             SGRCMP
     C                   Z-ADD     0             SGRMED
     C                   Z-ADD     0             SGROTH
     C                   Z-ADD     0             SGRTOT
     C                   Z-ADD     0             SRVCMP
     C                   Z-ADD     0             SRVMED
     C                   Z-ADD     0             SRVOTH
     C                   Z-ADD     0             SRVTOT
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C                   MOVE      DSFLD1        SVSTAF
     C                   MOVE      '1'           *IN22
     C                   END
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   ENDIF
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C*    DETERMINE IF RATE IS WITHIN RANGE SELECTED
     C*
     C                   Z-ADD     CMCRT         RATE
     C*
     C                   MOVEL     CMJRST        XXJRST
     C                   Z-ADD     CMACDT        XXACDT
     C*
     C     RBKY          SETGT     PRB01
     C     RBKY1         READPE    PRB01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     RATE          IFGT      RBPACP
     C                   Z-ADD     RBPACP        RATE
     C                   END
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ACCDTE
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDTE
     C*
      *** BEGIN CHANGE *** 02/26/99 ***********************************
     C******************** Z-ADDCMENTD    SRDTI
     C                   Z-ADD     CMIDT         SRDTI
      *** END CHANGE ***** 02/26/99 ***********************************
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ENTDTE
     C*
     C                   MOVE      CMCO#         XXCO#
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMFYR         XXYR
     C                   MOVE      CMCASE        XXCASE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C**
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKY          SETGT     PCH01
     C     CHKY1         READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHOCST        ANDEQ     'C'
     C*
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         CLODTE            6 0
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         CLODTE
     C                   END
     C*
     C                   Z-ADD     *ZERO         PCOMP
     C                   Z-ADD     *ZERO         PMED
     C                   Z-ADD     *ZERO         POTHR
     C                   Z-ADD     *ZERO         TOTALP
     C                   Z-ADD     *ZERO         RCOMP
     C                   Z-ADD     *ZERO         RMED
     C                   Z-ADD     *ZERO         ROTHR
     C                   Z-ADD     *ZERO         TOTALR
     C*
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C                   MOVEL     CMT(R)        XXKEY
     C**
     C*  GET RESERVE TYPE CODE FILE
     C*
     C                   SETOFF                                       2021
     C*
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        IFEQ      'C'
     C                   SETON                                        20
     C                   ENDIF
     C     RTTYPE        IFEQ      'M'
     C                   SETON                                        21
     C                   ENDIF
     C                   ENDIF
     C*
     C*   ACCUMULATE PAYMENT AMOUNTS
     C*
     C   20              ADD       CMA(R)        PCOMP
     C   21              ADD       CMA(R)        PMED
     C  N20
     CANN21              ADD       CMA(R)        POTHR
     C*
     C*   ACCUMULATE RESERVE AMOUNTS
     C*
     C   20              ADD       CMR(R)        RCOMP
     C   21              ADD       CMR(R)        RMED
     C  N20
     CANN21              ADD       CMR(R)        ROTHR
     C*
     C                   END
     C*
     C*   DETERMINE TOTAL AMOUNTS
     C**
     C*** BEGIN CHG *** 03/12/99 **************************************
     C********** PCOMP     ADD  PMED      TOTALP  92
     C     PCOMP         ADD       PMED          TOTALP           11 2
     C*** END CHG ***** 03/12/99 **************************************
     C                   ADD       POTHR         TOTALP
     C*** BEGIN CHG *** 03/12/99 **************************************
     C********** RCOMP     ADD  RMED      TOTALR  92
     C     RCOMP         ADD       RMED          TOTALR           11 2
     C*** END CHG ***** 03/12/99 **************************************
     C                   ADD       ROTHR         TOTALR
     C*** BEGIN DLT *** 03/12/99 **************************************
      **** BEGIN ADD **** 06/16/98 ************************************
     C********** ICOMP     ADD  IMED      TOTALI 102
     C**********           ADD  IOTHR     TOTALI
      **** END   ADD **** 06/16/98 ************************************
     C*** END DLT ***** 03/12/99 **************************************
     C*
     C*  GET REGION FROM EMPLOYER FILE
     C*
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  LOAD CLAIMANT'S NAME
     C*
     C     CMSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      SRNAME
     C                   MOVE      XXNAME        CNAME            15
     C                   END
     C*
     C*  SPLIT CLAIMANT'S SOCIAL SECURITY NUMBER OUT
     C*
     C*** BEG CHG ****** 05/25/07 **********************************************
     C*** X1SS# setup in SRNAME will have value after authority processing
     C*****                MOVE CMSS#     XXSS#
     C*****                MOVELXXSS#     SS4     4
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'X1SS#'       QQNAME           25
     C     X1SS#         PARM      X1SS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL     X1SS#         SS4               4
     C                   MOVE      '-'           SS4
     C*****                MOVE XXSS#     SS6     6
     C                   MOVE      X1SS#         SS6               6
     C*** END CHG ****** 05/25/07 **********************************************
     C                   MOVEL     SS6           SS2               2
     C                   MOVE      SS6           SS5               5
     C                   MOVEL     '-'           SS5
     C*
     C*  ACCUMULATE DETAIL TOTALS FOR INCURRED AMOUNTS
     C*
     C     PCOMP         ADD       RCOMP         ICOMP
     C     PMED          ADD       RMED          IMED
     C     POTHR         ADD       ROTHR         IOTHR
     C     TOTALP        ADD       TOTALR        TOTALI
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*
     C*  CHECK TYPE OF OUTPUT BEFORE PROCESSING TOTALS
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C*  ACCUMULATE STAFF CODE TOTALS AND INCREMENT STAFF COUNTER
     C*
     C                   ADD       RCOMP         STRCMP           11 2
     C                   ADD       RMED          STRMED           11 2
     C                   ADD       ROTHR         STROTH           11 2
     C                   ADD       TOTALR        STRTOT           11 2
     C*
     C                   ADD       PCOMP         STPCMP           11 2
     C                   ADD       PMED          STPMED           11 2
     C                   ADD       POTHR         STPOTH           11 2
     C                   ADD       TOTALP        STPTOT           11 2
     C*
     C*  ACCUMULATE STAFF TOTALS FOR INCURRED AMOUNTS
     C*
     C     STRCMP        ADD       STPCMP        STICMP
     C     STRMED        ADD       STPMED        STIMED
     C     STROTH        ADD       STPOTH        STIOTH
     C     STRTOT        ADD       STPTOT        STITOT
     C*
     C                   ADD       1             STCASE            5 0
     C*
     C*
     C*  ACCUMULATE FINAL TOTALS AND INCREMENT FINAL COUNTER
     C*
     C                   ADD       RCOMP         LRRCMP           11 2
     C                   ADD       RMED          LRRMED           11 2
     C                   ADD       ROTHR         LRROTH           11 2
     C                   ADD       TOTALR        LRRTOT           11 2
     C*
     C                   ADD       PCOMP         LRPCMP           11 2
     C                   ADD       PMED          LRPMED           11 2
     C                   ADD       POTHR         LRPOTH           11 2
     C                   ADD       TOTALP        LRPTOT           11 2
     C*
     C*  ACCUMULATE FINAL TOTALS FOR INCURRED AMOUNTS
     C*
     C     LRRCMP        ADD       LRPCMP        LRICMP
     C     LRRMED        ADD       LRPMED        LRIMED
     C     LRROTH        ADD       LRPOTH        LRIOTH
     C     LRRTOT        ADD       LRPTOT        LRITOT
     C*
     C                   ADD       1             LRCASE            5 0
     C   22              EXCEPT    HEAD
     C                   SETOFF                                       22
     C*
     C***** BEGIN ADD **** 04/12/00 ***********************************
     C                   ENDIF
     C***** END   ADD **** 04/12/00 ***********************************
     C*
      ***** BEG ADD ***** 02/18/05 ************************************
     C* Calculate the Detail Lines Gross Amounts
     C     VCOMP         ADD       VMED          VTOTAL
     C                   ADD       VOTHR         VTOTAL
     C     PCOMP         ADD       VCOMP         GCOMP
     C     PMED          ADD       VMED          GMED
     C     POTHR         ADD       VOTHR         GOTHR
     C     TOTALP        ADD       VTOTAL        GTOTAL
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*
     C*  WRITE RECORDS TO WORK FILE
     C*
     C     QQTYPO        IFEQ      'F'
     C     QQTYPO        OREQ      'B'
     C                   EXSR      WRITE
     C                   ENDIF
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C***** BEGIN ADD **** 04/12/00 ***********************************
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C***** END   ADD **** 04/12/00 ***********************************
     C*
     C                   EXCEPT    DETAIL
     C*
      ***** BEG ADD ***** 02/18/05 ************************************
     C                   Z-ADD     *ZERO         GCOMP
     C                   Z-ADD     *ZERO         GMED
     C                   Z-ADD     *ZERO         GOTHR
     C                   Z-ADD     *ZERO         GTOTAL
     C                   Z-ADD     *ZERO         VCOMP
     C                   Z-ADD     *ZERO         VMED
     C                   Z-ADD     *ZERO         VOTHR
     C                   Z-ADD     *ZERO         VTOTAL
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C                   ENDIF
     C***** END   ADD **** 03/20/00 ***********************************
     C*
     C     *INOF         IFEQ      '1'
     C                   SETON                                        22
     C                   END
     C*
     C                   ENDIF
     C                   READ      PLI02                                  50
     C                   ENDDO
     C*
     C                   ENDSR
     C****************************************************************
     C*    SRNAME -SUBROUTINE TO COMBINE NAME
     C****************************************************************
     C*
     C     SRNAME        BEGSR
     C*
     C                   MOVE      *BLANKS       FSN
     C                   MOVE      *BLANKS       LSN
     C                   MOVE      *BLANKS       NAM
     C                   MOVE      *BLANKS       XXWK47
     C                   MOVE      *BLANKS       XXNAME
     C*
     C                   MOVEA     CDFSN         FSN
     C                   MOVEA     CDLSN         LSN
     C                   Z-ADD     31            X                 3 0
     C                   Z-ADD     1             Y                 3 0
     C*
     C     X             DOWGT     0
     C     LSN(X)        ANDEQ     *BLANK
     C                   SUB       1             X
     C                   END
     C                   ADD       1             X
     C                   MOVE      ','           LSN(X)
     C                   MOVEA     LSN           NAM
     C                   ADD       2             X
     C     Y             DOWLT     16
     C                   MOVE      FSN(Y)        NAM(X)
     C                   ADD       1             X
     C                   ADD       1             Y
     C                   END
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          XXAUTH            1
     C                   MOVE      *BLANKS       XXWK47
     C                   MOVEL     XXATTX        XXWK47           47
     C                   MOVE      *BLANKS       XXNAME
     C                   MOVEL     XXAT15        XXNAME
     C                   MOVE      *BLANKS       X1SS#
     C                   ELSE
     C                   MOVE      *ON           XXAUTH
     C                   MOVEA     NAM           XXWK47           47
     C                   MOVEL     XXWK47        XXNAME           15
     C                   MOVE      CMSS#         X1SS#
     C                   ENDIF
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   ENDSR
     C*
      ***** BEG ADD ***** 02/18/05 ************************************
     C*****************************************************************
     C**  SRRCVR - SUBROUTINE TO GET RECOVERIES FOR CLAIM
     C*****************************************************************
     C**
     C     SRRCVR        BEGSR
     C*
     C* Read the all the Payments for for this claim
     C     PAKY          SETLL     PPA02
     C     PAKY          READE     PPA02                                  60
     C*
     C*
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  PABR2         XXKEY
     C*
     C* Check payment value 1 table for Recoveries Flay
     C* If flag = Y, then determine which category to report amt
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *OFF
     C     B2RCVR        ANDEQ     'Y'
     C*
     C* Compensation recovery
     C     PARETP        IFEQ      'C'
     C                   SELECT
     C     PATYPE        WHENEQ    'R'
     C                   ADD       PAAMT         VCOMP
     C                   ADD       PAAMT         SRVCMP
     C                   ADD       PAAMT         LRVCMP
     C     PATYPE        WHENEQ    'S'
     C                   ADD       PAAMT         VCOMP
     C                   ADD       PAAMT         SRVCMP
     C                   ADD       PAAMT         LRVCMP
     C     PATYPE        WHENEQ    'V'
     C                   ADD       PAAMT         VCOMP
     C                   ADD       PAAMT         SRVCMP
     C                   ADD       PAAMT         LRVCMP
     C     PATYPE        WHENEQ    'A'
     C     VCOMP         SUB       PAAMT         VCOMP
     C     SRVCMP        SUB       PAAMT         SRVCMP
     C     LRVCMP        SUB       PAAMT         LRVCMP
     C     PATYPE        WHENEQ    'M'
     C     VCOMP         SUB       PAAMT         VCOMP
     C     SRVCMP        SUB       PAAMT         SRVCMP
     C     LRVCMP        SUB       PAAMT         LRVCMP
     C     PATYPE        WHENEQ    'P'
     C     VCOMP         SUB       PAAMT         VCOMP
     C     SRVCMP        SUB       PAAMT         SRVCMP
     C     LRVCMP        SUB       PAAMT         LRVCMP
     C     PATYPE        WHENEQ    'Z'
     C     VCOMP         SUB       PAAMT         VCOMP
     C     SRVCMP        SUB       PAAMT         SRVCMP
     C     LRVCMP        SUB       PAAMT         LRVCMP
     C                   OTHER
     C                   ENDSL
     C                   ENDIF
     C*
     C* Medical recovery
     C     PARETP        IFEQ      'M'
     C                   SELECT
     C     PATYPE        WHENEQ    'R'
     C                   ADD       PAAMT         VMED
     C                   ADD       PAAMT         SRVMED
     C                   ADD       PAAMT         LRVMED
     C     PATYPE        WHENEQ    'S'
     C                   ADD       PAAMT         VMED
     C                   ADD       PAAMT         SRVMED
     C                   ADD       PAAMT         LRVMED
     C     PATYPE        WHENEQ    'V'
     C                   ADD       PAAMT         VMED
     C                   ADD       PAAMT         SRVMED
     C                   ADD       PAAMT         LRVMED
     C     PATYPE        WHENEQ    'A'
     C     VMED          SUB       PAAMT         VMED
     C     SRVMED        SUB       PAAMT         SRVMED
     C     LRVMED        SUB       PAAMT         LRVMED
     C     PATYPE        WHENEQ    'M'
     C     VMED          SUB       PAAMT         VMED
     C     SRVMED        SUB       PAAMT         SRVMED
     C     LRVMED        SUB       PAAMT         LRVMED
     C     PATYPE        WHENEQ    'P'
     C     VMED          SUB       PAAMT         VMED
     C     SRVMED        SUB       PAAMT         SRVMED
     C     LRVMED        SUB       PAAMT         LRVMED
     C     PATYPE        WHENEQ    'Z'
     C     VMED          SUB       PAAMT         VMED
     C     SRVMED        SUB       PAAMT         SRVMED
     C     LRVMED        SUB       PAAMT         LRVMED
     C                   OTHER
     C                   ENDSL
     C                   ENDIF
     C*
     C* Other recovery - Legal or Expense
     C     PARETP        IFEQ      'L'
     C     PARETP        OREQ      'E'
     C                   SELECT
     C     PATYPE        WHENEQ    'R'
     C                   ADD       PAAMT         VOTHR
     C                   ADD       PAAMT         SRVOTH
     C                   ADD       PAAMT         LRVOTH
     C     PATYPE        WHENEQ    'S'
     C                   ADD       PAAMT         VOTHR
     C                   ADD       PAAMT         SRVOTH
     C                   ADD       PAAMT         LRVOTH
     C     PATYPE        WHENEQ    'V'
     C                   ADD       PAAMT         VOTHR
     C                   ADD       PAAMT         SRVOTH
     C                   ADD       PAAMT         LRVOTH
     C     PATYPE        WHENEQ    'A'
     C     VOTHR         SUB       PAAMT         VOTHR
     C     SRVOTH        SUB       PAAMT         SRVOTH
     C     LRVOTH        SUB       PAAMT         LRVOTH
     C     PATYPE        WHENEQ    'M'
     C     VOTHR         SUB       PAAMT         VOTHR
     C     SRVOTH        SUB       PAAMT         SRVOTH
     C     LRVOTH        SUB       PAAMT         LRVOTH
     C     PATYPE        WHENEQ    'P'
     C     VOTHR         SUB       PAAMT         VOTHR
     C     SRVOTH        SUB       PAAMT         SRVOTH
     C     LRVOTH        SUB       PAAMT         LRVOTH
     C     PATYPE        WHENEQ    'Z'
     C     VOTHR         SUB       PAAMT         VOTHR
     C     SRVOTH        SUB       PAAMT         SRVOTH
     C     LRVOTH        SUB       PAAMT         LRVOTH
     C                   OTHER
     C                   ENDSL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     PAKY          READE     PPA02                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C***** BEGIN ADD **** 03/20/00 ***********************************
     C*================================================================
     C*    WRITE - SUBROUTINE TO WRITE DETAIL TO WORK FILE WFCLP
     C*================================================================
     C*
     C     WRITE         BEGSR
     C*
     C                   CLEAR                   PCL01
     C*
     C                   Z-ADD     CMCO#         CLCO#
     C                   Z-ADD     CMFND         CLFND
     C                   Z-ADD     CMFYR         CLFYR
     C                   Z-ADD     CMEMP#        CLEMP#
     C                   Z-ADD     CMDIV         CLDIV
     C                   Z-ADD     EMRGN#        CLRGN#
     C                   MOVEL(P)  CNAME         CLCLMT
     C*** BEG CHG ****** 05/25/07 *********************************************
     C*** Move blanks to SS# if no authority
     C     XXAUTH        IFEQ      *ON
     C                   MOVEL(P)  CMSS#         CLSS#
     C                   ELSE
     C                   MOVE(P)   *BLANKS       CLSS#
     C                   ENDIF
     C*** END CHG ****** 05/25/07 *********************************************
     C                   Z-ADD     CMCASE        CLCASE
     C                   Z-ADD     CMCLAS        CLCLAS
     C******BEG ADD******04/20/09**************************************
     C                   MOVE      CMSPEC        CLSPEC
     C                   MOVEL     CMEXT         CLEXT
     C******END ADD******04/20/09**************************************
     C                   Z-ADD     CMAWW         CLAWW
     C                   Z-ADD     RATE          CLRATE
     C***** BEGIN CHG **** 04/12/00 ***********************************
     C*
     C*  CONVERT DATE TO 00/00/0000
     C*
     C**********           Z-ADDCMACDT    CLACDT
     C**********           Z-ADDCMRCVD    CLRCDT
     C**********           Z-ADDCMIDT     CLENDT
     C*
     C**********           Z-ADDCLODTE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     CLCLDT
     C*
     C**********           MOVELCMNCCI    CLNCCI
     C**********           MOVELCMNATR    CLNATR
     C**********           MOVELCMPART    CLPART
     C**********           MOVELCMCAUS    CLCAUS
     C*
     C                   MOVEL(P)  CMNCCI        CLNCCI
     C                   MOVEL(P)  CMNATR        CLNATR
     C                   MOVEL(P)  CMPART        CLPART
     C                   MOVEL(P)  CMCAUS        CLCAUS
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLACDT
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLRCDT
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLENDT
     C*
     C                   Z-ADD     CLODTE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXCLDT
     C*
     C                   Z-ADD     XXCLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLCLDT
     C***** END   CHG **** 04/12/00 ***********************************
     C                   Z-ADD     RCOMP         CLRCMP
     C                   Z-ADD     RMED          CLRMED
     C                   Z-ADD     ROTHR         CLROTH
     C                   Z-ADD     TOTALR        CLRTOT
     C                   Z-ADD     PCOMP         CLPCMP
     C                   Z-ADD     PMED          CLPMED
     C                   Z-ADD     POTHR         CLPOTH
     C                   Z-ADD     TOTALP        CLPTOT
     C                   Z-ADD     ICOMP         CLICMP
     C                   Z-ADD     IMED          CLIMED
     C                   Z-ADD     IOTHR         CLIOTH
     C                   Z-ADD     TOTALI        CLITOT
     C                   MOVEL(P)  CMSUPR        CLSUPR
     C                   MOVEL(P)  CMCLEX        CLCLEX
     C                   MOVEL(P)  CMUMAN        CLUMAN
     C                   MOVEL(P)  CMFEAD        CLFEAD
     C                   MOVEL(P)  CMSTAT        CLSTAT
     C***** BEGIN ADD **** 04/12/00 ***********************************
     C*
      ***** BEG ADD ***** 02/18/05 ************************************
     C* Add to new fields
     C                   Z-ADD     GCOMP         CLGCMP
     C                   Z-ADD     GMED          CLGMED
     C                   Z-ADD     GOTHR         CLGOTH
     C                   Z-ADD     GTOTAL        CLGTOT
     C                   Z-ADD     VCOMP         CLRCCO
     C                   Z-ADD     VMED          CLRCME
     C                   Z-ADD     VOTHR         CLRCOT
     C                   Z-ADD     VTOTAL        CLRCTL
      ***** END ADD ***** 02/18/05 ************************************
     C*
     C*  LOAD DESCRIPTIONS AND NEW FIELDS
     C*     Company name, employer name and lost time flag
     C*
     C                   MOVEL(P)  XXCONM        CLCONM
     C                   MOVEL(P)  EMNAME        CLEMNM
     C                   MOVEL(P)  CMLTFL        CLLTFL
     C*
     C*     Group name
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        CLFNNM
     C                   ENDIF
     C*
     C*     Region description
     C*
     C     REKY          CHAIN     PRE01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  REDESC        CLRDES
     C                   ENDIF
     C*
     C*     Payroll class description
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDEQ     'A'
     C                   MOVEL(P)  PRDESC        CLCLDS
     C                   ENDIF
     C*                                                    PRSTAT=A
     C*     NCCI code description
     C*
     C                   MOVEL(P)  CMNCCI        XXKEY
     C                   MOVE(P)   'NC'          XXTBID
     C*
     C*     Check state table first
     C*
     C                   MOVEL     CMJRST        XXTBID
     C     GTKY          CHAIN     PGT01                              61
     C*
     C*     If not found try generic NCCI table
     C*
     C     *IN61         IFEQ      *ON
     C     CMJRST        ANDNE     *BLANK
     C                   MOVEL     '  NC'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        CLNCDS
     C                   ENDIF
     C                   ENDIF
     C*                                                    CMJRST<>BLK
     C     *IN61         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        CLNCDS
     C                   ENDIF
     C*
     C*     Nature code description
     C*
     C                   MOVEL(P)  CMNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        CLNADS
     C                   ENDIF
     C*
     C*     Part code description
     C*
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        CLPADS
     C                   ENDIF
     C*
     C*     Cause code description
     C*
     C                   MOVEL     *BLANKS       CLCADS
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CADESC        CLCADS
     C                   ENDIF
     C*
     C*     Staff names
     C*
     C                   MOVEL(P)  CMSUPR        XXSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  UPUSNM        CLSUNM
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMCLEX        XXSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  UPUSNM        CLEXNM
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMUMAN        XXSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  UPUSNM        CLUMNM
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMFEAD        XXSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  UPUSNM        CLFANM
     C                   ENDIF
     C*
     C*     Activity status code and description
     C*
     C                   MOVEL(P)  CMACST        CLACST
     C                   MOVEL(P)  'ACST'        XXTBID
     C                   MOVEL(P)  CMACST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        CLASDS
     C                   ENDIF
     C*
     C*     Jurisdiction state and various flags
     C*
     C                   MOVEL(P)  CMJRST        CLJRST
     C                   MOVEL(P)  CMDNY         CLDNY
     C                   MOVEL(P)  CMCVFL        CLCVFL
     C                   MOVEL(P)  CMSUBR        CLSUBR
     C                   MOVEL(P)  CMSDF         CLSDF
     C                   MOVEL(P)  CMMCO         CLMCO
     C                   MOVEL(P)  CMDTH         CLDTH
     C*
     C*     Dates: death, disability, return to work, date reported
     C*
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLDOD
     C*
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLDBDT
     C*
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLRTW
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         CLDTRP
     C*
     C*     Accident description, location and location name
     C*
     C                   MOVEL(P)  CMACDS        CLACDS
     C                   MOVEL(P)  CMACD2        CLACD2
     C*
     C                   MOVEL(P)  CMCTG1        CLLOC
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  ALNAME        CLLCNM
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMCTG2        CLCTG2
     C*
     C*     Office, dept, employment status, shift and chapter
     C*
     C                   MOVEL(P)  CMOFF#        CLOFF#
     C*
     C                   MOVEL(P)  CMDEPT        CLDEPT
     C     DPKY          CHAIN     PDP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DPDESC        CLDPDS
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMESTS        CLESTS
     C                   MOVEL(P)  'EMST'        XXTBID
     C                   MOVEL(P)  CMESTS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        CLESDS
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMSHFT        CLSHFT
     C                   MOVEL(P)  'SHFT'        XXTBID
     C                   MOVEL(P)  CMSHFT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        CLSHDS
     C                   ENDIF
     C*
     C                   MOVEL(P)  EMCHAP        CLCHAP
     C     CPKY          CHAIN     PCP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CPDESC        CLCHDS
     C                   ENDIF
     C*
     C***** END   ADD **** 04/12/00 ***********************************
     C*
     C                   MOVEL(P)  Q1USER        CLUSER
     C                   MOVEL(P)  Q1PGM         CLPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CLDT
     C*
     C                   MOVEL(P)  DSTIME        CLTIME
     C*
      ***** BEG ADD ******* 06/26/00 **********************************
      * ZZZ
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CLPFDT
      *
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         CLPTDT
      *
     C                   ENDIF
      *
      ***** END ADD ******* 06/26/00 **********************************
     C*
     C                   WRITE     PCL01
     C*
     C                   ENDSR
     C***** END   ADD **** 03/20/00 ***********************************
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*  MLS 1/15/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/15/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O               08      ORDIS1              60
     O*** Beg Chg *** 09/17/18 *********************************
     O*************     09   UPUSNM              67
     O                  09   strUsNm             67
     O*** End Chg *** 09/17/18 *********************************
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      OROUTQ              30
     O*  MLS 1/15/88 ADD INDICATORS AND OUT QUEUE
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************
     O*
     O*  Selection information added to report  6-14-88  LJQ
     O*
     O          E            ROUTE          2 35
     O                                           49 'SELECTION CRITERIA:'
     O          E            ROUTE          1
     O                                           37 'FROM         TO'
     O                                           71 'FROM'
     O                                           87 'TO'
     O          E            ROUTE          1
     O                                           17 'GROUP . . . . . :'
     O                       FFND          3     26
     O                       TFND          3     37
     O                                           55 'CASE  . . . . :'
     O                       FCAS          3     71
     O                       TCAS          3     87
     O          E            ROUTE          1
     O                                           17 'YEAR  . . . . . :'
     O                       FYR           3     26
     O                       TYR           3     37
     O                                           55 'CLASS . . . . :'
     O                       FCLA          3     71
     O                       TCLA          3     87
     O          E            ROUTE          1
     O                                           17 'REGION  . . . . :'
     O                       FRGN          3     26
     O                       TRGN          3     37
     O                                           55 'AWW . . . . . :'
     O                       FAWW          J     72
     O                       TAWW          J     88
     O          E            ROUTE          1
     O                                           17 'EMPLOYER  . . . :'
     O                       FEMP          3     26
     O                       TEMP          3     37
     O                                           55 'RATE  . . . . :'
     O                       FRAT          J     72
     O                       TRAT          J     88
     O          E            ROUTE          1
     O                                           17 'DIVISION  . . . :'
     O                       FDIV          3     26
     O                       TDIV          3     37
     O                                           55 'ACCIDENT DATE :'
     O                       XXFACC        Y     71
     O                       XXTACC        Y     87
     O          E            ROUTE          1
     O                                           17 'LOST TIME (L/N) :'
     O                       FLT                 26
     O                       TLT                 37
     O                                           55 'DATE RECEIVED :'
     O                       XXFREC        Y     71
     O                       XXTREC        Y     87
     O          E            ROUTE          1
     O                                           17 'NCCI CODE . . . :'
     O                       FNCC                26
     O                       TNCC                37
     O                                           55 'DATE ENTERED  :'
     O                       XXFENT        Y     71
     O                       XXTENT        Y     87
     O          E            ROUTE          1
     O                                           17 'NATURE CODE . . :'
     O                       FNAT                26
     O                       TNAT                37
     O                                           55 'DATE CLOSED   :'
     O                       XXFCLO        Y     71
     O                       XXTCLO        Y     87
     O          E            ROUTE          1
     O                                           17 'PART CODE . . . :'
     O                       FPAR                26
     O                       TPAR                37
     O                                           55 'PAYMENT AMOUNT:'
     O                       FPAY          J     72
     O                       TPAY          J     88
     O          E            ROUTE          1
     O                                           17 'CAUSE CODE  . . :'
     O                       FCAU                26
     O                       TCAU                37
     O*
     O*** BEGIN CHG *** 03/27/98 **************************************
     O**********                         55 'TOTAL INCURRED:'
     O                                           55 'O/S RESERVE   :'
     O*** END   CHG *** 03/27/98 **************************************
     O*
     O                       FRES          J     72
     O                       TRES          J     88
     O          E            ROUTE          1
     O                                           17 'SUPERVISOR  . . :'
     O                       RSSUPR              37
      **** BEGIN CHG **** 06/16/98 ************************************
     O************************           55 'UNIT MANAGER  :'
     O************************ RSUMAN    71
     O                                           55 'TOTAL INCURRED:'
     O                       FINC          J     72
     O                       TINC          J     88
      **** END   CHG **** 06/16/98 ************************************
     O          E            ROUTE          1
     O                                           17 'ADJUSTER  . . . :'
     O                       RSCLEX              37
      **** BEGIN CHG **** 06/16/98 ************************************
     O************************           55 'FIELD ADJUSTER:'
     O************************ RSFEAD    71        4
     O*** BEG CHG *** 06/21/04 *****************************
     O***********                        55 'UNIT MANAGER  :'
     O                                           55 'CLAIM MANAGER :'
     O*** END CHG *** 06/21/04 *****************************
     O                       RSUMAN              71
      **** END   CHG **** 06/16/98 ************************************
     O          E            ROUTE          1
     O                                           17 'CLAIM NAME RANGE:'
     O                       FRNM                26
     O                       TONM                37
      **** BEGIN CHG **** 06/16/98 ************************************
     O************************           55 'CLAIM STATUS  :'
     O************************ FSTAT     72
     O                                           55 'FIELD ADJUSTER:'
     O                       RSFEAD              71
      **** END   CHG **** 06/16/98 ************************************
     O          E            ROUTE          1
     O                                           17 'SORT OPTION #1  :'
     O*** Begin chg *** 08/16/02 **************************************
     O***********************  RSSORT    37
     O                       RSSRT2              38
     O*** End chg ***** 08/16/02 **************************************
      **** BEGIN ADD **** 06/16/98 ************************************
     O                                           55 'CLAIM STATUS  :'
     O                       FSTAT               72
      **** END   ADD **** 06/16/98 ************************************
     O          E            ROUTE          1
     O                                           17 'SORT OPTION #2  :'
     O*** Begin chg *** 08/16/02 **************************************
     O************************ RSSRT2    38
     O                       RSSORT              37
     O*** End chg ***** 08/16/02 **************************************
      ***** BEG ADD ******* 06/02/99 **********************************
     O***** BEGIN ADD **** 03/20/00 ***********************************
     O          E            ROUTE          1
     O                                           17 'OUTPUT TYPE     :'
     O                       RSTYPO              38
     O***** END   ADD **** 03/20/00 ***********************************
      *
     O          E    71      ROUTE       3  1
     O                                           23 '******* NOTE *******   '
      *
     O          E    71      ROUTE          1
     O                                           23 'LISTING MAY REFLECT USE'
     O                                         +000 'R AUTHORIZATION TO GROU'
     O                                         +000 'P AND EMPLOYER RECORDS.'
      *
      ***** END ADD ******* 06/02/99 **********************************
     O*
      ***** BEG CHG ***** 02/18/05 ************************************
      ***** END CHG ***** 02/18/05 ************************************
     O**************                     23 '(C) COPYRIGHT PROGRESSI'
     O**************                     47 'VE DATA SOLUTIONS, INC.,'
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT FISERV IN'
     O***                                47 'SURANCE SOLUTIONS, INC.,'
     O***                      *YEAR     52
      ****** END DLT ***** 01/28/10 **********************************
      ***** END CHG ***** 02/18/05 ************************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O***** BEGIN CHG **** 10/19/99 ***********************************
     O**********               CONAME    87
     O                       CONAME             120
     O**********               DSDATEY  132
     O                       DSDATE        Y    198
     O***** END   CHG **** 10/19/99 ***********************************
     O          E            HEAD           2
     O                       Q1PGM               10
     O***** BEGIN CHG **** 10/19/99 ***********************************
     O**********               ORTITL    96
     O**********                        126 'PAGE:'
     O**********               PAGE  Z  132
     O                       ORTITL             130
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O***** END   CHG **** 10/19/99 ***********************************
     O          E    47      HEAD           2
     O*** Begin chg *** 08/16/02 **************************************
     O********************************   25 'CLAIMS FOR STAFF'
     O********************************   31 'CODE:'
     O*** End chg ***** 08/16/02 **************************************
     O*** Begin chg *** 08/16/02 **************************************
     O              N48                          25 'CLAIMS FOR STAFF'
     O              N48                          31 'CODE:'
     O               48                          17 'CLAIMS FOR JURIS'
     O               48                          31 'DICTION STATE:'
     O*** End chg ***** 08/16/02 **************************************
     O                       SVSTAF              35
     O***** BEGIN CHG **** 10/19/99 ***********************************
     O          E            HEAD           1
     O                                            3 'GRP'
     O                                            8 'YR'
     O                                           16 'CASE'
     O                                           27 'EMPLOYER'
     O                                           34 'DIV'
     O                                           39 'RGN'
     O                                           52 'CLAIMANT'
     O******BEG CHG******04/20/09**************************************
     O************                       84 'SOC SEC #'
     O************                       94 'CLASS'
     O                                           80 'SOC SEC #'
     O                                           90 'CLASS'
     O******END CHG******04/20/09**************************************
     O******BEG ADD******04/20/09**************************************
     O                                           95 'SPEC'
     O                                          100 'EXT'
     O******END ADD******04/20/09**************************************
     O******BEG CHG******04/20/09**************************************
     O*************                     109 'AWW'
     O                                          111 'AWW'
     O******END CHG******04/20/09**************************************
     O                                          122 'RATE'
     O                                          135 'ACC DATE'
     O                                          150 'RECVD DATE'
     O                                          165 'CLOSE DATE'
     O***** BEGIN CHG **** 04/12/00 ***********************************
     O**********                        187 'NC   NT   PT   CS'
     O**********                        194 'STAT'
     O                                          190 'LT   NC   NT   PT   CS'
     O                                          197 'STAT'
     O***** END   CHG **** 04/12/00 ***********************************
     O          E            DETAIL      1  1
     O                       CMFND         3      3
     O                       CMFYR         3      8
     O                                            9 '-'
     O                       CMCASE              16
     O                       CMEMP#        Z     27
     O                       CMDIV         Z     34
     O                       EMRGN#        Z     39
     O                       CNAME               59
     O******BEG CHG******04/20/09**************************************
     O*************            SS4       77
     O*************            SS2       79
     O*************            SS5       84
     O*************            CMCLASZ   94
     O******BEG CHG******03/02/13**************************************
     O**********             SS4                 73
     O                       SS4                 74
     O******END CHG******03/02/13**************************************
     O                       SS2                 76
     O******BEG CHG******01/05/11**************************************
     O************             SS5       80
     O                       SS5                 81
     O******END CHG******01/05/11**************************************
     O                       CMCLAS        Z     90
     O******END CHG******04/20/09**************************************
     O******BEG ADD******04/20/09**************************************
     O                       CMSPEC              92
     O                       CMEXT              100
     O******END ADD******04/20/09**************************************
     O******BEG CHG******04/20/09**************************************
     O*************            CMAWW J  110
     O                       CMAWW         J    112
     O******END CHG******04/20/09**************************************
     O                       RATE          J    123
     O                       ACCDTE             135 '  /  /  '
     O                       RECDTE             150 '  /  /  '
     O                       CLODTE             165 '  /  /  '
     O***** BEGIN CHG **** 04/12/00 ***********************************
     O**********               CMNCCI   173
     O**********               CMNATR   177
     O**********               CMPART   182
     O**********               CMCAUS   187
     O**********               CMSTAT   192
     O                       CMLTFL             170
     O                       CMNCCI             175
     O                       CMNATR             180
     O                       CMPART             185
     O                       CMCAUS             190
     O                       CMSTAT             196
     O***** END   CHG **** 04/12/00 ***********************************
     O          E            DETAIL         1
     O*** BEG CHG *** 06/21/04 *****************************
     O***********                        15 'UNIT MANAGER  :'
     O                                           15 'CLAIM MANAGER :'
     O*** END CHG *** 06/21/04 *****************************
     O                       CMUMAN              19
     O                                          137 'COMPENSATION'
     O                                          156 'MEDICAL'
     O                                          175 'OTHER'
     O                                          194 'TOTAL'
     O          E            DETAIL         1
     O                                           15 'SUPERVISOR    :'
     O                       CMSUPR              19
      ***** BEG ADD ***** 02/18/05 ************************************
     O                                          118 'GROSS PAID:'
     O                       GCOMP         KB   138
     O                       GMED          KB   157
     O                       GOTHR         KB   176
     O                       GTOTAL        JB   195
     O          E            DETAIL         1
     O                                           15 'ADJUSTER      :'
     O                       CMCLEX              19
     O                                          118 'RECOVERED:'
     O                       VCOMP         KB   138
     O                       VMED          KB   157
     O                       VOTHR         KB   176
     O                       VTOTAL        JB   195
     O          E            DETAIL         1
     O                                           15 'FIELD ADJUSTER:'
     O                       CMFEAD              19
     O                                          118 'NET PAID:'
     O                       PCOMP         KB   138
     O                       PMED          KB   157
     O                       POTHR         KB   176
     O                       TOTALP        JB   195
     O          E            DETAIL         1
      ***** END ADD ***** 02/18/05 ************************************
     O                                          118 'RESERVES:'
     O                       RCOMP         KB   138
     O                       RMED          KB   157
     O                       ROTHR         KB   176
     O                       TOTALR        JB   195
      ****************
     O********E**1****         DETAIL
     O****************                   15 'ADJUSTER      :'
     O****************         CMCLEX    19
     O****************                  118 'PAYMENTS:'
     O****************         PCOMP KB 138
     O****************         PMED  KB 157
     O****************         POTHR KB 176
     O****************         TOTALPJB 195
     O********E**2****         DETAIL
     O****************                   15 'FIELD ADJUSTER:'
     O****************         CMFEAD    19
     O          E            DETAIL         2
     O                                          118 'INCURRED:'
     O                       ICOMP         KB   138
     O                       IMED          KB   157
     O                       IOTHR         KB   176
     O                       TOTALI        JB   195
     O***** END   CHG **** 10/19/99 ***********************************
     O          E            STAFTL      2  1
     O*** Begin chg *** 08/16/02 **************************************
     O********************************   21 'CLAIMS FOR STAFF'
     O********************************   27 'CODE:'
     O*** End chg ***** 08/16/02 **************************************
     O*** Begin chg *** 08/16/02 **************************************
     O              N48                          21 'CLAIMS FOR STAFF'
     O              N48                          27 'CODE:'
     O               48                          16 'CLAIMS FOR JURIS'
     O               48                          27 'DICTION ST:'
     O*** End chg ***** 08/16/02 **************************************
     O                       SVSTAF              31
     O                                           38 'COUNT:'
     O                       STCASE        1     44
     O***** BEGIN CHG **** 10/19/99 ***********************************
      ***** BEG ADD ***** 02/18/05 ************************************
     O                                          118 'TOTAL GROSS PAID:'
     O                       SGRCMP        J    138
     O                       SGRMED        J    157
     O                       SGROTH        J    176
     O                       SGRTOT        J    195
     O          E            STAFTL         1
     O                                          118 'TOTAL RECOVERED:'
     O                       SRVCMP        J    138
     O                       SRVMED        J    157
     O                       SRVOTH        J    176
     O                       SRVTOT        J    195
     O          E            STAFTL         1
     O                                          118 'TOTAL NET PAID:'
     O                       STPCMP        J    138
     O                       STPMED        J    157
     O                       STPOTH        J    176
     O                       STPTOT        J    195
     O          E            STAFTL         1
      ***** END ADD ***** 02/18/05 ************************************
     O                                          118 'TOTAL RESERVES:'
     O                       STRCMP        J    138
     O                       STRMED        J    157
     O                       STROTH        J    176
     O                       STRTOT        J    195
      ***** BEG DEL ***** 02/18/05 ************************************
      *******************
     O********E**1*******      STAFTL
     O*******************               118 'TOTAL PAYMENTS:'
     O*******************      STPCMPJ  138
     O*******************      STPMEDJ  157
     O*******************      STPOTHJ  176
     O*******************      STPTOTJ  195
      ***** END DEL ***** 02/18/05 ************************************
     O          E            STAFTL         2
     O                                          118 'TOTAL INCURRED:'
     O                       STICMP        J    138
     O                       STIMED        J    157
     O                       STIOTH        J    176
     O                       STITOT        J    195
     O          E    LR      TOTALS      2  1
     O                                           18 'FINAL TOTALS:'
     O                                           38 'CLAIM COUNT:'
     O                       LRCASE        1     44
      ***** BEG ADD ***** 02/18/05 ************************************
     O                                          118 'TOTAL GROSS PAID:'
     O                       LRGCMP        JB   138
     O                       LRGMED        JB   157
     O                       LRGOTH        JB   176
     O                       LRGTOT        JB   195
     O          E    LR      TOTALS         1
     O                                          118 'TOTAL RECOVERED:'
     O                       LRVCMP        JB   138
     O                       LRVMED        JB   157
     O                       LRVOTH        JB   176
     O                       LRVTOT        JB   195
     O          E    LR      TOTALS         1
     O                                          118 'TOTAL NET PAID:'
     O                       LRPCMP        JB   138
     O                       LRPMED        JB   157
     O                       LRPOTH        JB   176
     O                       LRPTOT        JB   195
     O          E    LR      TOTALS         1
      ***** END ADD ***** 02/18/05 ************************************
     O                                          118 'TOTAL RESERVES:'
     O                       LRRCMP        JB   138
     O                       LRRMED        JB   157
     O                       LRROTH        JB   176
     O                       LRRTOT        JB   195
      ***** BEG DEL ***** 02/18/05 ************************************
     O********E**1**   LR      TOTALS
     O**************                    118 'TOTAL PAYMENTS:'
     O**************           LRPCMPJB 138
     O**************           LRPMEDJB 157
     O**************           LRPOTHJB 176
     O**************           LRPTOTJB 195
      ***** END DEL ***** 02/18/05 ************************************
     O          E    LR      TOTALS         1
     O                                          118 'TOTAL INCURRED:'
     O                       LRICMP        JB   138
     O                       LRIMED        JB   157
     O                       LRIOTH        JB   176
     O                       LRITOT        JB   195
     O***** END   CHG **** 10/19/99 ***********************************
**     SRT1
CLAIM# SORT
CLAIM NAME
**     SRT2
NO STAFF SORT
BY SUPERVISOR
BY CLM MANGR
BY ADJUSTER
BY FIELD ADJR
BY JURIS ST
