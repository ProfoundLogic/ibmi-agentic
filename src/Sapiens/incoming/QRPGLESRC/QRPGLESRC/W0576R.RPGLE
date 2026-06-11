     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0576R - Work With Payment Detail
     H*****************************************************************
     H*  DESCRIPTION:
     H*    This program will display a list of Payment Detail recs by *
     H*    Transaction Date (Descending), EOMB#, or Check # sequence. *
     H*                                                               *
     H*    Mode 'U' to process Duplicate Payment Exclusion.           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 05/29/97  C:30  Project:  WC660                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask/Unmask SSN
     H*
     H* 06/17/11 C:257Support: WC133/77141                            *
     H*               Change the screen label from 'Work with Payment *
     H*               Detail' to 'Work with Processed Payment Search' *
     H*                                                               *
     H* 09/24/10 C:180 SUPPORT 74306
     H*                Fix weeks paid calc to fully include
     H*                adjustments and recoveries.
     H*
     H* 02/21/08 C:15 Support: WC136/62431                            *
     H*               Added code to initialize the FRTO array         *
     H*               correctly.                                      *
     H*                                                               *
     H* 07/26/07 C:191 SUPPORT: 57741                                 *
     H*               Fix weeks/days calculation to account for       *
     H*               overlapping "dates of services"                 *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Load SCPODT with PARQDT if not zero.           *
     H*                                                               *
     H* 08/28/06 C:53 Project 114WMF - Apply 114MFA  12/26/02         *
     H*               Add parm and process to exclude hospital bills  *
     H*                                                               *
     H* 04/06/04 C:53 Support: WC044/41791                            *
     H*               Tighten search process for WDRPP                *
     H*                                                               *
     H* 12/10/03 C:53 Support: WC082/40304                            *
     H*               Change read of file.                            *
     H*                                                               *
     H* 08/12/03 C:53 Support: WC102/38307                            *
     H*               Add additionl link to WDRPP.                    *
     H*                                                               *
     H* 07/17/03 C:15 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*                                                               *
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H* 12/03/02 C:53 Support: WC045/34853                            *
     H*               Correct issues w/paging.                        *
     H*                                                               *
     H* 10/10/02 C:62 Project WC1078                                  *
     H*               -Populate SCXFYR/SCXCAS/SCADST                  *
     H*               -Pass SCXFYR/SCXCAS if not Adjusted and         *
     H*                SCXFYR/SCXCAS <> 0.  Opt 15,16,80
     H*                                                               *
     H* 07/03/02 C:15 Support: WC025/33106                            *
     H*               Added group to the WDCRL key list.              *
     H*                                                               *
     H* 06/04/02 C:15 Support: WC091/32779                            *
     H*               Chgd code to prevent incorrect case # from being*
     H*               passed to other pgms.                           *
     H*                                                               *
     H* 04/17/02 C:15 Support: WC065/31973                            *
     H*               Correct the positioning routines.               *
     H*                                                               *
     H* 03/07/02 C:53 SUPPORT: WC065/31359                            *
     H*               CORRECTED SUBSET ISSUE WHICH IGNORED MODIFIER   *
     H*                                                               *
     H* 02/27/02 C:15 Project: WC835                                  *
     H*               Added code to check for additional WDRPP record *
     H*               to get the appropriate payee name.              *
     H*                                                               *
     H* 01/02/02 C:15  Support: WC065/30095                           *
     H*                Chgd code to process opt20 correctly.          *
     H*                                                               *
     H* 05/01/01 C:15  Project: WC1020                                *
     H*                Added code to use the bank code when retrieving*
     H*                the bank info record.                          *
     H*                                                               *
     H* 01/19/00 C:53  Support: WC025/25829                           *
     H*                Added consideration for modifier in search.    *
     H*                                                               *
     H* 06/12/00 C:62  PROJECT: MC230                                 *
     H*                SWAP OUT FA015R USAGE WITH FA031R, FA032R,     *
     H*                FA033R AND FA034R.                             *
     H*                                                               *
     H* 05/19/00 C:15  Support: WC034/20089                           *
     H*                Added the TRDT to key list for WDRPP.          *
     H*                                                               *
     H* 09/07/99 C:112 Project: WC928                                 *
     H*               Add option '20=Reverse deductible recovery'     *
     H*               which provide ability to reverse a deductible   *
     H*               recovery transaction on a claim. This is useful *
     H*               when a third party recovery has been received   *
     H*               but can not be posted because the claim's       *
     H*               payment amount is zero due to all deductibles   *
     H*               having been recovered.                          *
     H*                                                               *
     H* 07/07/99 C:15 Support: WC048/18728                            *
     H*               Added code to load the correct payee name based *
     H*               on the View Check file (WDRPP).  If an addl     *
     H*               address record was used, the name on the screen *
     H*               did not match the name on the check.            *
     H*                                                               *
     H* 05/27/99 C:33 SEQ#: WC095/18133                               *
     H*               RESET THE FOLD/TRUNCATE INDICATOR UPON ENTRY.   *
     H*                                                               *
     H* 04/02/99 C:33 SEQ#: WC095/13001                               *
     H*               LOAD HIVAL TO SEQ# FOR 1ST VIEW.                *
     H*                                                               *
     H* 03/22/99 C:94 PROJECT: SUP17239
     H*               ADD ERROR MESSAGE IF NO ASSOCIATED EOMB
     H*
     H* 03/15/99 C:94 PROJECT: 25MKL
     H*               ADD RECON#
     H*
     H* 03/11/99 C:94 PROJECT: MC025 SUP17048
     H*                        CHANGE PARMS FOR OPT15 & 16
     H*
     H* 02/23/99 C:94 PROJECT: 25MKL                                  *
     H*               SWITCH OPT15 AND 16                             *
     H*                                                               *
     H* 01/23/99 C:94 PROJECT: 25MKL                                  *
     H*               IF PROVIDER BILLS IS INSTALLED THEN CALL F0340R *
     H*               OTHERWISE STILL CALL F7200C1                    *
     H*                                                               *
     H*  01/06/99 C:15 Support: WC072/13647                           *
     H*                Changed the condition for displaying opt80 and *
     H*                the pgm which is called by opt80.              *
     H*                                                               *
     H*  11/16/98 C:33 SEQ#: WC100/14209                              *
     H*                CHANGE CALC FOR TOTAL WEEKS & DAYS.            *
     H*                                                               *
     H*  07/16/98 C:15 Project: WC577                                 *
     H*                Added code to not allow option 4 (delete).     *
     H*                                                               *
     H*  04/06/98 C:15 SEQ#: WC060/10546                              *
     H*                IF NOT IN 'FOLD' MODE BE SURE TO LOAD A FULL   *
     H*                SCREEN OF DATA, NOT JUST 3.                    *
     H*                                                               *
     H*  01/07/98 C:15 Project: WC577                                 *
     H*               -Chgd pgm to use API's to retreive WCMC data    *
     H*                instead of referencing WCMC files in the pgm.  *
     H*               -Do not display option 15, 16 or 80 if WCMC is  *
     H*                not installed.                                 *
     H*                                                               *
     H*  11/18/97 c:30 Support: #032                                  *
     H*           F7=Financial Summary is displaying zeroes.          *
     H*           WMCMP RRN is not passed to WA009R.                  *
     H*                                                               *
     H*  11/18/97 c:30 Support:                                       *
     H*           Reverse Image goes away after Display return.       *
     H*           *IN89 change to *IN44.                              *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  20 - Number Order Search                                     *
     H* N20 - Alphabetic Order Search                                 *
     H*  30 - Called as a search                                      *
     H*  31 - Called as a search w/YR                                 *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  93 - Used to display CPF6A7C; Option invalid                 *
     H*  94 - Used to display CPF5204; Function Key invalid           *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0576F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *__________________________________________________________
      *  Table File logical
     FWTABL     IF   E           K DISK
      *__________________________________________________________
      *  Check Reconciliation logical
     FWDCRL     IF   E           K DISK
      *__________________________________________________________
      *  User Parameter logical
     FWMUPL     IF   E           K DISK
      *__________________________________________________________
      *  Payment Detail Physical (for refreshing subfile record)
     FWDPAP     IF   E             DISK
     F                                     RENAME(PPA01:PFRCDF)
      *__________________________________________________________
      *  Payment Detail Logical  (CO#/FND/FYR/CASE/TRDT)
     FWDPAL6    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPA01:DBRCDF)
      *__________________________________________________________
      *  Payment Detail Logical  (CO#/FND/FYR/CASE/EOB#)
     FWDPALA    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPA01:DBRCDF2)
      *__________________________________________________________
      *  Payment Detail Logical  (CO#/FND/FYR/CASE/CHK#)
     FWDPALB    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PPA01:DBRCDF3)
     F*** BEG ADD *** 08/12/03 **************************************
      *  Payment Detail Logical  (CO#/FND/CHK#)
     FWDPAL4    IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PPA01:DBRCDF4)
     F*** END ADD *** 08/12/03 **************************************
      *__________________________________________________________
      *  Payee Demographics
     FWMPDL     IF   E           K DISK
      *__________________________________________________________
     F*** BEGIN DLT *** 01/07/98 ***********************************************
     F*  WCMC Group Master file
     F*****FMFNL1  IF  E           K        DISK
     F************PFN01                             KRENAMEPFN02
     F*  WCMC EOB Master file
     F*****FAEOL4  IF  E           K        DISK
     F*** END DLT ***** 01/07/98 ***********************************************
      *__________________________________________________________
      *  Bank Info  file      Key= CO#/APPL/GRP/ACCT#/EFDT
     F*** Begin chg *** 05/01/01 **************************************
     F*********WMBIL1  IF  E           K        DISK
     FWMBIL2    IF   E           K DISK
     F*** End chg ***** 05/01/01 **************************************
      *__________________________________________________________
      *** BEGIN ADD *** 11/18/97 ********************************
      *  Claim Master file    Key= CO#/GRP/FYR/CASE
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBCLM)
     F*** BEGIN ADD *** 07/07/99 **************************************
     F*** BEG DEL *** 08/12/03 **************************************
     F***View Check file    Key= CO#/GRP#/FYR/CASE/CHK#
     F***WDRPL   IF  E           K        DISK
     F*** END DEL *** 08/12/03 **************************************
     F*** END ADD ***** 07/07/99 **************************************
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** End add ***** 05/22/03 **************************************
     F*** BEGIN ADD *** 09/07/99 **************************************
     F*  Fund Master File
     FWMFNL     IF   E           K DISK
     F*** END ADD ***** 09/07/99 **************************************
      *__________________________________________________________
      *** END   ADD *** 11/18/97 ********************************
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(6) CTDATA PERRCD(1)              Column headings ABOV
     D HDA             S             78    DIM(1) CTDATA PERRCD(1)              Column headings BELO
     D***  BEGIN CHG  ***  04/06/98  *********************************
     D**********          OF      1  18  1   OFD    35   Options, Function
     D*** BEGIN CHANGE *** 09/07/99 ***********************************
     D******************* OF      1  19  1   OFD    35   Options, Function
     D OF              S              1    DIM(20) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(20) ALT(OF)
     D*** END CHANGE ***** 09/07/99 ***********************************
     D***  END   CHG  ***  04/06/98  *********************************
      *
     D I6              S              6    DIM(63)                              Indicator Values
      *** BEG ADD ****** 07/26/07 ****************************************
      * NEW ARRAYS ADDED TO HANDLE SERVICE PERIOD OVERLAPPING
     D FRDT            S              7  0 DIM(9999) ASCEND                     DATE ARRAY
     D FRTO            S             14    DIM(9999) ASCEND                     DATE ARRAY
      *** BEGIN ADD *** 11/18/97 ********************************
      *
     D DBCLM           DS
     D  RRNCLM               397    400B 0
      *
      *** END   ADD *** 11/18/97 ********************************
      *
      *  Payment Codes
      *
     D                 DS
     D  PC                     1     10
     D                                     DIM(5)                               Payment Codes
     D  SCSPC1                 1      2
     D  SCSPC2                 3      4
     D  SCSPC3                 5      6
     D  SCSPC4                 7      8
     D  SCSPC5                 9     10
      *
      *  Transaction Type
      *
     D                 DS
     D  TT                     1      5
     D                                     DIM(5)                               Transaction Type
     D  SCSTT1                 1      1
     D  SCSTT2                 2      2
     D  SCSTT3                 3      3
     D  SCSTT4                 4      4
     D  SCSTT5                 5      5
      *
      *  Payment Value 1
      *
     D                 DS
     D  PV1                    1     10
     D                                     DIM(5)                               Payment Value 1
     D  SCSPV1                 1      2
     D  SCSPV2                 3      4
     D  SCSPV3                 5      6
     D  SCSPV4                 7      8
     D  SCSPV5                 9     10
      *
      *  Reserve Codes
      *
     D                 DS
     D  RC                     1     10
     D                                     DIM(5)                               Reserve Codes
     D  SCSRC1                 1      2
     D  SCSRC2                 3      4
     D  SCSRC3                 5      6
     D  SCSRC4                 7      8
     D  SCSRC5                 9     10
      *
      *  SCDATA - Fields defined for the 2 INFO Displays
      *
     D                 DS
     D  SLIN1                  1     74
     D  SNAME                  1     25
     D  SFEIN#                 1      9
     D  SMOD                  12     14
     D  SEOB#                 17     25
     D*** BEG ADD *** 03/15/99 **************************************
     D  SRCN#                 27     29
     D*** END ADD *** 03/15/99 **************************************
     D  SCLRDT                29     40
     D  SDESC                 34     73
     D  SBKCD                 50     74
      *
      *  SCDAT2 - Fields defined for the 2 INFO Displays
      *
     D                 DS
     D  SLIN2                  1     21
     D  SDT1                   1     10
     D  SDT2                  12     21
      *
      *  SCCS02 - Field Definitions for Claim Heading Line 2
      *
     D                 DS
     D  SCCS02                 1     79
     D  S2SS1                 26     28
     D  S2SS2                 30     31
     D  S2SS3                 33     36
     D  S2NAME                39     62
      *
      *  SCCS03 - Field Definitions for Payment Totals
      *
     D                 DS
     D  SCCS03                 1     79
     D  S3HD1                  1     15
     D  S3WKS                 17     19
     D  S3HD2                 23     27
     D  S3DYS                 29     29
     D  S3HD3                 33     37
     D  S3AMT                 39     55
      *
      *  Claimant Social Security Number
      *
     D                 DS
     D*** BEGIN CHG *** 02/20/98 ***********************************************
     D*************************************** 1   90SSN
     D  SSN                    1      9
     D*** END CHG ***** 02/20/98 ***********************************************
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D                 DS
     D  FIELD                  1     10
     D  FLDNAM                 1      5
     D  FLDNUM                 6      6  0
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D***  BEGIN CHG  ***  06/17/11  *********************************
     D******        'Payment Detail      'C         OPTNM
     D OPTNM           C                   CONST('Processed Payments S-
     D                                     earch')
     D***  END   CHG  ***  06/17/11  *********************************
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0576R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(3)
     D***  BEGIN CHG  ***  04/06/98  *********************************
     D**********    3                     C         SF1PAG
     D SF1PAG          C                   CONST(9)
     D***  END   CHG  ***  04/06/98  *********************************
     D DELETD          C                   CONST('DELETED')
     D HEAD1           C                   CONST('Totals - Weeks:')
     D HEAD2           C                   CONST('Days:')
     D HEAD3           C                   CONST('Paid:')
     D*** Begin chg *** 02/21/08 ************************************
     D XXHIVA          C                   CONST('99999999999999')
      *** END ADD ****** 07/26/07 ****************************************
      *
     I***PY SCOPYWW01E
      *

     I*** BEG ADD *** 08/12/03 **************************************
     IDBRCDF4
     I              PACO#                       P4CO#
     I              PAFND                       P4FND
     I              PAFYR                       P4FYR
     I              PACASE                      P4CASE
     I              PASEQ#                      P4SEQ#
     I              PACHK#                      P4CHK#
     I              PABASQ                      P4BASQ
     I              PATRDT                      P4TRDT
     I              PAPODT                      P4PODT
     I              PAFEIN                      P4FEIN
     I              PAMOD                       P4MOD
     I              PAAMT                       P4AMT
     I              PABR2                       P4BR2
     I              PABR7                       P4BR7
     I              PAFRDT                      P4FRDT
     I              PATODT                      P4TODT
     I              PAWKS                       P4WKS
     I              PADAYS                      P4DAYS
     I              PATRCD                      P4TRCD
     I              PATYPE                      P4TYPE
     I              PARECD                      P4RECD
     I              PARETP                      P4RETP
     I              PADESC                      P4DESC
     I              PAADST                      P4ADST
     I              PASTAT                      P4STAT
     I              PAUSER                      P4USER
     I              PAPGM                       P4PGM
     I              PATIME                      P4TIME
     I              PADT                        P4DT
     I              PABUL1                      P4BUL1
     I              PAPGTP                      P4PGTP
     I              PAEOB#                      P4EOB#
     I              PACKTP                      P4CKTP
     I              PAACCT                      P4ACCT
     I              PAOFF#                      P4OFF#
     I              PARCVD                      P4RCVD
     I              PADUE                       P4DUE
     I              PALTCD                      P4LTCD
     I              PADPCD                      P4DPCD
     I              PADPCU                      P4DPCU
     I              PARQST                      P4RQST
     I              PAFNAL                      P4FNAL
     I              PARCDT                      P4RCDT
     I              PARADJ                      P4RADJ
     I              PAEOBS                      P4EOBS
     I              PAXFYR                      P4XFYR
     I              PAXCAS                      P4XCAS
     I*** BEG ADD *** 08/12/03 **************************************
      *
     I***PY SCOPYWW01I
     I*** End chg ***** 02/21/08 ************************************
      *================================================================
      *  PLIST definitions
      *================================================================
      *

     C*          DATCHK    PLIST
     C*                    PARM           SRDTI   80
     C*                    PARM *BLANKS   SRFFMT  8
     C*                    PARM *BLANKS   SRTFMT  8
     C*                    PARM           SRDTO  10
     C*                    PARM           SRERR   1
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@SPS            10            Payment Status
     C                   PARM                    @@SFDT            8            Service "From" DT
     C                   PARM                    @@STDT            8            Service "To" DT
     C                   PARM                    @@SPC            10            Payment Codes
     C                   PARM                    @@STT             5            Transaction Type
     C                   PARM                    @@SPV1           10            Payment Value 1
     C                   PARM                    @@SRC            10            Reserve Codes
     C                   PARM                    @@SFEI            9            Federal ID #
     C                   PARM                    @@SCHK            7            Check #
     C                   PARM                    @@SEOB            9            EOMB #
     C                   PARM                    @@SIRQ           10            Initial Requestor
     C                   PARM                    @@SGRP            1            Goup vs. Non-Group
     C*** BEG ADD ***** 01/19/01 ***********************************************
     C                   PARM                    @@SMOD            3            modifier
     C*** END ADD ***** 01/19/01 ***********************************************
     C*** BEGIN ADD *** 12/26/02 **************************************
     C                   PARM                    @@EXHS            1            exclude hospital
     C*** END   ADD *** 12/26/02 **************************************
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #ACO#             3
     C                   PARM                    #AFND             3
     C                   PARM                    #AFYR             3
     C                   PARM                    #ACASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  WCMC FUND file
      *
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
     C*** BEGIN DLT *** 01/07/98 ***********************************************
     C*  KEY LIST FOR THE EOMB FILE
     C***********
     C***********EOKY      KLIST
     C***********          KFLD           KKCO#
     C***********          KFLD           XXSUB#
     C***********          KFLD           XXGRP#
     C***********          KFLD           SCFYR
     C***********          KFLD           SCCASE
     C***********          KFLD           SCEOB#
     C*** END DLT ***** 01/07/98 ***********************************************
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  Table File Key List
      *
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
      *
      *  Payment Detail Key
      *
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
      *  Payment Detail Key  (Transaction Date)
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKSEQ#
      *
      *  Payment Detail Key  (EOMB #)
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKSEQ#
      *
      *  Payment Detail Key  (Check #)
      *
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCHK#
     C                   KFLD                    KKSEQ#
      *
      *  Check Reconciliation
      *
     C     CRKEY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXKY             15
     C*** Begin add *** 07/03/02 **************************************
     C                   KFLD                    KKFND
     C*** End add ***** 07/03/02 **************************************
      *
      *  Payee Demographics
      *
     C     PDKY          KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
      *
      *  Bank Info file
      *
     C     BIKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    SCFND
     C*** Begin chg *** 05/01/01 **************************************
     C******************** KFLD           SCACCT
     C                   KFLD                    SCCKTP
     C*** End chg ***** 05/01/01 **************************************
     C                   KFLD                    SCYMD
      *
     C     BIKY2         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    SCFND
     C*** Begin chg *** 05/01/01 **************************************
     C******************** KFLD           SCACCT
     C                   KFLD                    SCCKTP
     C*** End chg ***** 05/01/01 **************************************
      *
     C*** BEGIN ADD *** 07/07/99 **************************************
     C*** BEG DEL *** 08/12/03 **************************************
     C***View Check file
     C*******
     C*******    RPKY      KLIST
     C*******              KFLD           PACO#
     C*******              KFLD           PAFND
     C*******              KFLD           PAFYR
     C*******              KFLD           PACASE
     C*******              KFLD           PACHK#
     C****BEGIN ADD *** 05/19/00 **************************************
     C****Begin chg *** 02/27/02 **************************************
     C*******************  KFLD           PATRDT
     C*******              KFLD           XXTRDT  70
     C****End chg ***** 02/27/02 **************************************
     C*** END DEL *** 08/12/03 **************************************
     C*** END ADD ***** 05/19/00 **************************************
     C*** END ADD ***** 07/07/99 **************************************
     C*** Begin add *** 05/22/03 **************************************
     C*
     C     R2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*** Begin chg *** 08/12/03 **************************************
     C***********          KFLD           PASEQ#
     C                   KFLD                    XXSEQ#
     C*** End chg ***** 08/12/03 **************************************
     C*** End add ***** 05/22/03 **************************************
     C*** Begin add *** 08/12/03 **************************************
     C*
     C     PAKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PACHK#
     C*** End add ***** 08/12/03 **************************************
     C*
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*  Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*** END ADD ***** 09/07/99 **************************************
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C*** BEGIN DLT *** 01/07/98 ***********************************************
     C********** *LIKE     DEFN FNSUB#    XXSUB#
     C********** *LIKE     DEFN FNFND     XXGRP#
     C*** END DLT ***** 01/07/98 ***********************************************
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
     C     *LIKE         DEFINE    PATRDT        KKTRDT
     C     *LIKE         DEFINE    PAEOB#        KKEOB#
     C     *LIKE         DEFINE    PACHK#        KKCHK#
     C     *LIKE         DEFINE    PASEQ#        KKSEQ#
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#                         ADD 08/12/03
     C     *LIKE         DEFINE    PASEQ#        XBSEQ#                         ADD 08/12/03
      *
     C/COPY SCOPYWW01C
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
      *
     C                   DO        5             X
     C     PC(X)         IFNE      *BLANK
     C     TT(X)         ORNE      *BLANK
     C     PV1(X)        ORNE      *BLANK
     C     RC(X)         ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C                   ENDDO
      *
     C     SCSFEI        IFNE      *ZERO
     C     SCSCHK        ORNE      *ZERO
     C     SCSEOB        ORNE      *ZERO
     C     SCSFDT        ORNE      *ZERO
     C     SCSTDT        ORNE      *ZERO
     C     SCSIRQ        ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
      *  Call sub COUNT to process Claim Payment Detail records to
      *  obtain field S3AMT.
      *
     C                   MOVE      *IN95         SV95              1            SAVE *IN95
     C                   EXSR      COUNT
     C                   MOVE      SV95          *IN95                          RESTORE *IN95
      *
     C                   ENDSR
      *
      *===============================================================
      * COUNT - Subroutine to process Claim Payment Detail records
      *===============================================================
      *
     C     COUNT         BEGSR
      *
     C                   Z-ADD     *ZERO         XXWKS
     C                   Z-ADD     *ZERO         XXDYS
     C                   Z-ADD     *ZERO         XXTOTL
      *** BEG ADD ****** 07/26/07 **********************************************
     C                   Z-ADD     0             T                 4 0
     C                   CLEAR                   FRDT
     C                   CLEAR                   FRTO
     C                   MOVEA     *HIVAL        FRDT
     C*** Begin chg *** 02/21/08 ************************************
     C****************     MOVEA*HIVAL    FRTO
     C                   MOVE      XXHIVA        FRTO
     C*** End chg ***** 02/21/08 ************************************
      *** END ADD ****** 07/26/07 **********************************************
      *
     C                   EXSR      KEYINZ
     C                   EXSR      SETLL
     C                   EXSR      READ
      *
B01  C     *IN95         DOWEQ     *OFF
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
      *
B02  C     MEETSS        IFEQ      *ON
     C                   EXSR      CUMM
E02  C                   ENDIF
      *
     C                   EXSR      READ
E01  C                   ENDDO
      *
      *  Assign Screen Fields
      *
     C                   Z-ADD     XXWKS         SCTWKS
     C                   Z-ADD     XXDYS         SCTDYS
     C                   Z-ADD     XXTOTL        SCTAMT
      *
     C                   ENDSR
      *
      *===============================================================
      * CUMM - Subroutine to Accumulate Total Payment Amount and
      *        Total Weeks and Days based on "Subset" selections.
      *===============================================================
      *
     C     CUMM          BEGSR
      *
      *  PAYMENTS CALCULATION
      *
     C     PATYPE        IFEQ      'A'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   ADD       PAAMT         XXTOTL           10 2
      *
     C                   ELSE
     C                   SUB       PAAMT         XXTOTL
     C                   ENDIF
      *
      *  WEEKS & DAYS CALCULATION
      *
      *** BEG ADD ****** 07/26/07 ****************************************
      ** Ignore records with PAADST <> *BLANKS, VOIDS AND STOPS
     C     PAADST        IFEQ      *BLANKS
     C     PATYPE        ANDNE     'V'
     C     PATYPE        ANDNE     'S'
      *** END ADD ****** 07/26/07 ****************************************
      *** BEG ADD **** 09/24/10 ***************************************
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'R'
      *** END ADD **** 09/24/10 ***************************************
     C     PAWKS         IFGT      0
     C     PADAYS        ORGT      0
      *** BEG ADD ****** 07/26/07 ****************************************
      * Determine if this record to be considered for calculation
      * If service dates of current record are within existing service dates
      * then it is overlapping record, ignore
     C                   Z-ADD     0             XXDYFT            3 0
      **
     ***** BEGIN ADD **** 09/24/10 ***********************************
     C     PATYPE        IFNE      'A'
     C     PATYPE        ANDNE     'R'
     ***** END   ADD **** 09/24/10 ***********************************
     C                   SORTA     FRDT
     C                   SORTA     FRTO
      **
     C                   Z-ADD     1             R                 4 0
     C                   Z-ADD     1             K                 4 0
     C     PAFRDT        LOOKUP    FRDT(R)                              8080
     C     PATODT        LOOKUP    FRDT(K)                              8181
      ** Get TO date for period R and K
     C     *IN80         IFEQ      *ON
     C                   MOVE      FRTO(R)       XXTOR             7 0
     C                   ENDIF
     C     *IN81         IFEQ      *ON
     C                   MOVE      FRTO(K)       XXTOK             7 0
     C                   ENDIF
      **
     C     *IN80         IFEQ      *OFF
     C     *IN81         ANDEQ     *OFF
      ** CASE 1:
      ** No array record found, not an overlapping period.
      ** Save period into array for further processing
     C                   MOVE      'N'           XXIGNR            1
     C                   ADD       1             T
     C                   Z-ADD     PAFRDT        FRDT(T)
     C                   MOVEL     PAFRDT        FRTO(T)
     C                   MOVE      PATODT        FRTO(T)
     C                   ENDIF                                                  *IN80/81 = *OFF
      **
     C     *IN80         IFEQ      *OFF
     C     *IN81         ANDEQ     *ON
     C                   MOVE      'N'           XXIGNR
      ** CASE 2: PAFRDT is not found, PATODT is found in K period
      **    Get Days diff between PAFRDT and FRDT(1)
     C                   Z-ADD     PAFRDT        XXFDT             7 0
     C                   Z-ADD     FRDT(1)       XXTDT             7 0
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   Z-ADD     PAFRDT        FRDT(1)
     C                   MOVEL     PAFRDT        FRTO(1)
      **  There may be more than 1 period between 2 and K, get days diff between
      **  all periods, avoiding overlaps :get days between this period TODT and
      **  and next period FRDT then close gap by setting FRDT nxt = TODT cur
     C     2             DO        K             X
      ** Cal difference between TODT and FRDT of next period
     C     X             SUB       1             Y
      ** Set period dates to no diff
     C                   MOVE      FRTO(Y)       XXFDT                          moving TODT
     C                   Z-ADD     FRDT(X)       XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   MOVE      FRDT(X)       FRTO(Y)                        moving TODT
     C                   ENDDO                                                  2 to K
      **
     C     PATODT        IFGT      XXTOK
      ** CASE 3: PATODT is beyond K period to date
      ** Get Days diff between TODT(K) and PATODT
     C                   Z-ADD     XXTOK         XXFDT
     C                   Z-ADD     PATODT        XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   MOVE      PATODT        FRTO(K)
     C                   ENDIF                                                  CASE 2B
      **
     C                   ENDIF                                                  *IN80=*OFF, 81=*ON
      **
      ** Found array from date and To Date
     C     *IN80         IFEQ      *ON
     C     *IN81         ANDEQ     *ON
      **
      ** PAFRDT and PATODT found in same period
     C     R             IFEQ      K
      ** CASE 4:
      ** Period starts within existing period and TO date is also within
      ** existing period
     C     PAFRDT        IFLE      XXTOR
     C     PATODT        IFLE      XXTOR
      ** This is pure overlap record, IGNORE completely
     C                   MOVE      'Y'           XXIGNR
      **
     C                   ELSE                                                   PATODT<=XXTOR
      ** CASE 5:
      ** PATODT is greater than XXTOR
      ** Get Days diff between XXTOR and PATODT
     C                   MOVE      'N'           XXIGNR
     C                   Z-ADD     XXTOR         XXFDT
     C                   Z-ADD     PATODT        XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   MOVE      PATODT        FRTO(R)
     C                   ENDIF                                                  PATODT<=XXTOR
      **
     C                   ELSE                                                   PAFRDT<=XXTOR
      ** CASE 6:
      ** PAFRDT is greater than XXTOR (FR and TO both hit same period)
      ** This is a new period in between add entry
      ** Save period into array for further processing
     C                   MOVE      'N'           XXIGNR
     C                   ADD       1             T
     C                   Z-ADD     PAFRDT        FRDT(T)
     C                   MOVEL     PAFRDT        FRTO(T)
     C                   MOVE      PATODT        FRTO(T)
     C                   ENDIF                                                  PAFRDT<=XXTOR
      **
     C                   ELSE                                                   R = K
      **
      ** FRDT and TODT hit different periods
      **  From date beyond arry To date: This period starts after array period
      **
     C     PAFRDT        IFGT      XXTOR
      ** CASE 7:
      **  TO date lies in K period, calculate days between these periods
      ** Get Days diff between PAFRDT and FRDT(R+1)
     C                   Z-ADD     PAFRDT        XXFDT
     C                   ELSE                                                   PAFRDT>XXTOR
      ** CASE 8:
      **  TO date lies in K period, calculate days between these periods
      ** Get Days diff between TODT and FRDT,R+1
     C                   MOVE      FRTO(R)       XXFDT
     C                   ENDIF                                                  PAFRDT>XXTOR
      **
     C                   MOVE      'N'           XXIGNR
      **
      ** Common process for case 7 and 8
     C     R             ADD       1             Z                 4 0
     C                   Z-ADD     FRDT(Z)       XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
      **  There may be more than 1 period between Z and K, get days diff between
      **  all periods, avoiding overlaps :get days between this period TODT and
      **  and next period FRDT then close gap by setting FRDT nxt = TODT cur
     C     Z             DO        K             X
      ** Cal difference between TODT and FRDT of next period
     C     X             ADD       1             Y
      ** Set period dates to no diff
     C                   MOVE      FRTO(X)       XXFDT
     C                   Z-ADD     FRDT(Y)       XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
     C                   MOVE      FRTO(X)       FRDT(Y)
     C                   MOVEL     FRDT(Y)       FRTO(Y)
     C                   ENDDO                                                  R to K
      ** CASE 9:
      **  TO date lies outside K period, calculate days outside K period and
      **  and add to days calculated in CASE 3D
     C     PATODT        IFGT      XXTOK
     C                   Z-ADD     XXTOK         XXFDT
     C                   Z-ADD     PATODT        XXTDT
     C                   EXSR      SRDTDF
     C                   ADD       QQDAYS        XXDYFT
      ** Set period dates to no diff
     C                   MOVE      PATODT        FRTO(K)
     C                   ENDIF                                                  PATODT > XXTOK
      **
     C                   ENDIF                                                  R=K
     C                   ENDIF                                                  80/81 = *ON
      **
     ***** BEGIN ADD **** 09/24/10 ***********************************
     C                   ENDIF
     ***** END   ADD **** 09/24/10 ***********************************
      ** Override ignore flag if adjustment/reversal/void
     C     PATYPE        IFEQ      'A'
     ***** BEGIN DEL **** 09/24/10 ***********************************
     C***        PAAMT     ANDLT0
     ***** END   DEL **** 09/24/10 ***********************************
     C     PATYPE        OREQ      'R'
     ***** BEGIN DEL **** 09/24/10 ***********************************
     C***        PAAMT     ANDGT0
     ***** END   DEL **** 09/24/10 ***********************************
     C****       PATYPE    OREQ 'S'
     C****       PAAMT     ANDGT0
     C                   MOVE      'N'           XXIGNR
     C                   ENDIF
     C*
      **
      ** If XXIGNR flag set to Y then ignore this record for days calculation
     C     XXIGNR        IFEQ      'N'
      *** END ADD ****** 07/26/07 ****************************************
     C                   MOVE      'N'           XXADD             1
      *
     C*** Begin dlt *** 07/17/03 **************************************
     C********** PATYPE    IFEQ 'P'
     C********** PATYPE    OREQ 'Z'
     C**********           MOVE 'Y'       XXADD
     C**********           ENDIF
      *
     C********** PATYPE    IFEQ 'A'
     C********** PAADST    ANDEQ*BLANKS
     C**********           MOVE 'Y'       XXADD
     C**********           ENDIF
      *
     C********** PATYPE    IFEQ 'M'
     C********** PAAMT     ANDGE0
     C**********           MOVE 'Y'       XXADD
     C**********           ENDIF
      *
     C********** PATYPE    IFEQ 'A'
     C********** PAADST    ANDEQ'A'
     C********** PAAMT     ANDGE0
     C**********           MOVE 'Y'       XXADD
     C**********           ENDIF
     C*** End dlt ***** 07/17/03 **************************************
      *
     C*** Begin add *** 07/17/03 **************************************
     C     PATYPE        IFEQ      'P'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C                   MOVE      'Y'           XXADD
     C                   ENDIF
     C*** End add ***** 07/17/03 **************************************
      *
     C     XXADD         IFEQ      'Y'
      *** BEG ADD ****** 07/26/07 *********************************************
     C     XXDYFT        IFEQ      0
      *** END ADD ****** 07/26/07 *********************************************
     C                   ADD       PAWKS         XXWKS             3 0
     C                   ADD       PADAYS        XXDYS             3 0
      *** BEG ADD ****** 07/26/07 *********************************************
     C                   ELSE
     C     XXDYFT        DIV       7             XXTWKS            3 0
     C                   MVR                     XXDYFT
     C                   ADD       XXTWKS        XXWKS
     C                   ADD       XXDYFT        XXDYS
     C                   ENDIF                                                  XXDYFT = 0
      *** END ADD ****** 07/26/07 *********************************************
      *
     C     XXDYS         IFGE      7
     C                   ADD       1             XXWKS
     C     XXDYS         SUB       7             XXDYS
     C                   ENDIF
     C                   ENDIF
      *
     C     XXADD         IFEQ      'N'
      *** BEG ADD ****** 07/26/07 *********************************************
     C     XXDYFT        IFEQ      0
      *** END ADD ****** 07/26/07 *********************************************
     C                   SUB       PAWKS         XXWKS
     C     XXDYS         SUB       PADAYS        XXDYS
      *** BEG ADD ****** 07/26/07 *********************************************
     C                   ELSE
     C     XXDYFT        DIV       7             XXTWKS
     C                   MVR                     XXDYFT
     C                   SUB       XXTWKS        XXWKS
     C                   SUB       XXDYFT        XXDYS
     C                   ENDIF
      *** END ADD ****** 07/26/07 *********************************************
     C***  BEGIN CHG  ***  11/16/98  *********************************
     C********** XXDYS     IFLT 0
     C                   SELECT
     C     XXDYS         WHENLT    0
     C     XXWKS         ANDGT     0
     C     XXDYS         ORLE      -7
     C***  END   CHG  ***  11/16/98  *********************************
     C                   SUB       1             XXWKS
     C     7             ADD       XXDYS         XXDYS
     C***  BEGIN CHG  ***  11/16/98  *********************************
     C**********           ENDIF
     C     XXWKS         WHENLT    0
     C     XXDYS         ANDGT     0
     C                   ADD       1             XXWKS
     C                   SUB       7             XXDYS
     C                   ENDSL
     C***  END   CHG  ***  11/16/98  *********************************
     C                   ENDIF
      *** BEG ADD ****** 07/26/07 **********************************************
     C                   ENDIF                                                  XXIGNR = 'N'
      *** END ADD ****** 07/26/07 **********************************************
     C                   ENDIF                                                  PAWKS GT 0
      *** BEG ADD ****** 07/26/07 **********************************************
     C                   ENDIF                                                  PAADST = ' '
      *** END ADD ****** 07/26/07 **********************************************
      *
     C*                    MOVE XXWKS     S3WKS
     C*                    MOVE XXDYS     S3DYS
     C*                    MOVELXXTOTL    XXNUM     P
     C*                    MOVEL'X,XXX.XX'EDTMSK    P
     C*                    CALL 'WA003R'
     C*                    PARM           XXNUM  80
     C*                    PARM           EDTMSK 10
     C*                    MOVELXXNUM     S3AMT
      *
     C                   ENDSR
      *
      *===============================================================
      * CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
      *===============================================================
      *
     C     CVTDT         BEGSR
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C                   ENDSR                                                  CVTDT
      *
      *================================================================
      * ERRCKC - Edit errors on Position To fields
      *================================================================
     C     ERRCKC        BEGSR
      *
     C     SCPST1        IFGT      *BLANK
     C     SCPST1        ANDNE     *ZERO
     C                   MOVEL     SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
      *
     C***        *IN90     IFEQ *OFF
     C***        SCPST1    IFEQ *ZERO
     C***        SCPST1    OREQ *BLANK
     C***                  MOVE '99999999'SCPST1
     C***                  ENDIF
     C***                  ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
     C*
     C                   SELECT
     C*          ______________________________________________________
     C*** BEGIN ADD *** 07/16/98 **************************************
     C*  Don't allow option 4 (delete)
     C*
     C     SCSEL         WHENEQ    '4'
     C                   SETON                                        9085      INVALID OPTION
     C*** END ADD ***** 07/16/98 **************************************
     C*          ______________________________________________________
     C     SCSEL         WHENEQ    '6'
     C     SCEOB#        IFGT      *ZERO
     C     SCPGTP        OREQ      'G'
     C                   SETON                                        9085      INVALID OPTION
     C                   ENDIF
      *          ______________________________________________________
     C     SCSEL         WHENEQ    '15'
     C     SCSEL         OREQ      '16'
     C     SCEOB#        IFLE      *ZERO
     C                   SETON                                        9084      NO EOMB#
     C                   ENDIF
      *          ______________________________________________________
      *
      *** BEGIN ADD *** 09/07/99 **************************************
      *
     C     SCSEL         WHENEQ    '20'
      *
      * This option can only be used on recovery transactions
      * (PATYPE='R') and the recovery must be a deduct recovery
      * (PABR2 must match one of the following codes in the Fund Master
      * File (WMFNP):
      *              FNMEP1 - MED NET DEDUCT PMT VAL 1
      *              FNCOP1 - COMP NET DEDUCT PMT VAL 1
      *              FNEXP1 - EXP NET DEDUCT PMT VAL 1
      *              FNLEP1 - LEGAL NET DEDUCT PMT VAL 1
      *
     C     SCDBRR        CHAIN     PFRCDF                             99
     C     PATYPE        IFEQ      'R'
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     FNMEP1        IFNE      PABR2
     C     FNCOP1        ANDNE     PABR2
     C     FNEXP1        ANDNE     PABR2
     C     FNLEP1        ANDNE     PABR2
     C                   SETON                                        9082
     C                   ENDIF
      *
     C                   ELSE
     C                   SETON                                        908283
     C                   ENDIF                                                  Chain PFN01
      *
     C                   ELSE
     C                   SETON                                        9083      PATYPE/='R'
     C                   ENDIF
      *
     C*** Begin dlt *** 01/02/02 **************************************
      *  Display a window to allow entry of the reversal amount.
      **********
     C********** *IN90     IFEQ *OFF
      **********           MOVE PAAMT     SCRAMT
     C**********
     C********** RDSPW1    TAG
     C**********           EXFMTWDW01
     C********** *INKL     IFEQ *ON
     C**********           SETON                     90
     C**********           ELSE
      **********
     C********** SCRAMT    IFLE 0
     C**********           SETON                     71
     C**********           GOTO RDSPW1
     C**********           ENDIF
     C********** SCRAMT    IFGT PAAMT
     C**********           SETON                     72
     C**********           GOTO RDSPW1
     C**********           ENDIF
      **********
     C**********           ENDIF
      **********
     C**********           ENDIF
     C*** End dlt ***** 01/02/02 **************************************
      *** END ADD ***** 09/07/99 **************************************
      *          ______________________________________________________
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                           F10 Search
     C                   ENDCS
      *
     C     *IN90         IFEQ      *OFF
      *
      *  Edit Service Dates
      *
     C     SCSFDT        IFGT      *ZERO
     C     SCSTDT        ORGT      *ZERO
      *
      *  Edit "From" Service Date
      *
     C                   Z-ADD     SCSFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7790
     C                   ENDIF
      *
      *  Edit "TO" Date
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7890
     C                   END
      *
      *  "FROM" Date may not be greater than "TO" Date
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCSFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM            7 0
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO              7 0
      *
     C     XXFROM        IFGT      XXTO
     C                   SETON                                        7990
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  Edit Service DT
     C                   ENDIF
      *
      *  Edit 'Payment Code' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X                 3 0
     C                   Z-ADD     5             STR               3 0
     C                   Z-ADD     *ZERO         Y                 3 0
     C                   DO        5             X
     C     PC(X)         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PC(X)         XXKEY
     C     TBKEY         CHAIN     PPC01                              60
     C     *IN60         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Edit 'Transaction Type' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     10            STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     TT(X)         IFNE      *BLANK
     C     TT(X)         ANDNE     'M'
     C     TT(X)         ANDNE     'A'
     C     TT(X)         ANDNE     'P'
     C     TT(X)         ANDNE     'R'
     C     TT(X)         ANDNE     'Z'
     C     TT(X)         ANDNE     'V'
     C     TT(X)         ANDNE     'S'
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Edit 'Payment Value 1' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     15            STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     PV1(X)        IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PV1(X)        XXKEY
     C     TBKEY         CHAIN     PB201                              60
     C     *IN60         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Edit 'Reserve Code' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X
     C                   Z-ADD     20            STR
     C                   Z-ADD     *ZERO         Y
     C                   DO        5             X
     C     RC(X)         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     RC(X)         XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      *ON
     C     X             ADD       STR           Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      *  Group vs. Non-Group flag
      *
     C     *IN90         IFEQ      *OFF
     C     SCSGRP        IFNE      'G'
     C     SCSGRP        ANDNE     *BLANK
     C                   SETON                                        8190
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   ENDSR
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
      *
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      *HIVAL        KKTRDT
     C                   MOVE      *ZERO         KKEOB#
     C                   MOVE      *ZERO         KKCHK#
     C                   MOVE      *ZERO         KKSEQ#
     C*
     C***  BEGIN ADD  ***  04/02/99  *********************************
     C     VIEW#         IFEQ      1
     C                   MOVE      *HIVAL        KKSEQ#
     C                   ENDIF                                                  VIEW#=1
     C***  END   ADD  ***  04/02/99  *********************************
      *
     C                   ENDSR
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C     *IN21         IFEQ      *ON
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTRDT
     C***  BEGIN ADD  ***  04/02/99  *********************************
     C                   MOVE      *HIVAL        KKSEQ#
     C***  END   ADD  ***  04/02/99  *********************************
     C                   ENDIF
      *
     C***21***             MOVE SCPST1    KKTRDT
     C   22              MOVE      SCPST2        KKEOB#
     C   23              MOVE      SCPST3        KKCHK#
     C*** Begin add *** 04/17/02 *************************************
     C*** BEG CHG *** 12/03/02 *************************************
     C******************   MOVE *ZERO     KKSEQ#
     C  N21              MOVE      *ZERO         KKSEQ#
     C*** END CHG *** 12/03/02 *************************************
     C*** End add ***** 04/17/02 *************************************
      *
     C                   ENDSR
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   MOVE      SCCO#         KKCO#
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCFYR         KKFYR
     C                   MOVE      SCCASE        KKCASE
     C                   MOVE      SCEOB#        KKEOB#
     C                   MOVE      SCYMD         KKTRDT
     C                   MOVE      SCCHK#        KKCHK#
     C                   MOVE      SCSEQ#        KKSEQ#
      *
     C                   ENDSR
      *
      *================================================================
      *  KHDO - Process F8 "Payment Summary" request.                 *
      *================================================================
     C     KHDO          BEGSR
      *
     C*** Begin dlt *** 06/04/02 **************************************
     C*******************  MOVE SCCO#     QQCO#
     C*******************  MOVE SCFND     QQFND
     C*******************  MOVE SCFYR     QQFYR
     C*******************  MOVE SCCASE    QQCASE
     C*** End dlt ***** 06/04/02 **************************************
     C*** Begin add *** 06/04/02 **************************************
     C                   MOVE      @@CO#         QQCO#
     C                   MOVE      @@FND         QQFND
     C                   MOVE      @@FYR         QQFYR
     C                   MOVE      @@CASE        QQCASE
     C*** End add ***** 06/04/02 **************************************
      *
     C                   CALL      'W0576R5'
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      *BLANK        SCTRCD            2
     C                   PARM      *BLANK        SCTYPE            1
     C                   PARM      *BLANK        SCRECD            2
     C                   PARM      *BLANK        SCRETP            1
      *
     C                   SETON                                        90
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ELSE
     C                   WRITE     SCR01
     C                   ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      *  KJDO - Process F10 "Table Search" requests.                  *
      *================================================================
     C     KJDO          BEGSR
      *
     C                   MOVE      HFLD          FIELD
      *
      *  Assign Index references for ERROR Message or Position Cursor
      *  based on field requesting SEARCH.
      *
     C                   SELECT
      *
     C     FLDNAM        WHENEQ    'SCSPS'
     C                   MOVE      FLDNUM        KJNUM             3 0
     C                   Z-ADD     *ZERO         KJERR             3 0
     C                   Z-ADD     32            KJPC              3 0
      *
     C     FLDNAM        WHENEQ    'SCSPC'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     5             KJERR
     C                   Z-ADD     37            KJPC
      *
     C     FLDNAM        WHENEQ    'SCSTT'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     10            KJERR
     C                   Z-ADD     42            KJPC
      *
     C     FLDNAM        WHENEQ    'SCSPV'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     15            KJERR
     C                   Z-ADD     47            KJPC
      *
     C     FLDNAM        WHENEQ    'SCSRC'
     C                   MOVE      FLDNUM        KJNUM
     C                   Z-ADD     20            KJERR
     C                   Z-ADD     52            KJPC
      *
     C     HFLD          WHENEQ    'SCSTYP'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     1             KJPC
      *
     C     HFLD          WHENEQ    'SCSFDT'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     2             KJPC
      *
     C     HFLD          WHENEQ    'SCSTDT'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     3             KJPC
      *
     C     HFLD          WHENEQ    'SCSGRP'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     4             KJPC
      *
     C     HFLD          WHENEQ    'SCSFEI'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     5             KJPC
      *
     C     HFLD          WHENEQ    'SCSMOD'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     6             KJPC
      *
     C     HFLD          WHENEQ    'SCSCHK'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     7             KJPC
      *
     C     HFLD          WHENEQ    'SCSEOB'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     8             KJPC
      *
     C     HFLD          WHENEQ    'SCSIRQ'
     C                   Z-ADD     99            KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     9             KJPC
      *
     C                   OTHER
     C                   Z-ADD     *ZERO         KJNUM
     C                   Z-ADD     *ZERO         KJERR
     C                   Z-ADD     *ZERO         KJPC
     C                   ENDSL
      *
      *  Select the field requesting SEARCH or return with Position Cursor.
      *
     C                   SELECT
      *
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCSPC'
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         PC(X)                          Payment Status
      *
     C     X             IFEQ      5
     C                   MOVEA     I6(58)        CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
      *
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCSPV'
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         PV1(X)                         Payment Status
      *
     C     X             IFEQ      5
     C                   MOVEA     I6(53)        CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
      *
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCSTT'
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
      *_________________________________________________________________
      *
     C     FLDNAM        WHENEQ    'SCSRC'
     C                   CALL      'W2136R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C                   SETON                                        90
     C                   Z-ADD     FLDNUM        X
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         RC(X)                          Payment Status
      *
     C     X             IFEQ      5
     C                   MOVEA     '0100'        *IN(86)                        Position Cursor
     C                   ELSE
     C     KJPC          ADD       X             Y                 3 0
     C                   ADD       1             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
      *
     C                   ELSE                                                   ##KEY Blank
     C     KJPC          ADD       X             Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)                        Position Cursor
     C                   ENDIF
     C                   ENDIF
      *_________________________________________________________________
      *
     C     HFLD          WHENEQ    'SCSFEI'
     C     HFLD          OREQ      'SCSMOD'                                     Mod Search
     C                   MOVE      SCSFEI        QQFEIN
     C                   MOVE      SCSMOD        QQMOD
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTC             1
      *
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *OFF          *INKL
      *
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
      *
     C     QQRTC         IFEQ      'B'
     C                   SETON                                        90
     C                   MOVEA     '0101'        *IN(86)
     C                   ENDIF
      *
     C     QQRTC         IFEQ      '*'
     C                   MOVE      QQFEIN        SCSFEI
     C                   MOVE      QQMOD         SCSMOD
     C                   SETON                                        90
     C                   MOVEA     '1010'        *IN(86)
     C                   ENDIF
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCSFDT'
     C                   SETON                                        90
     C                   MOVEA     '1011'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCSTDT'
     C                   SETON                                        90
     C                   MOVEA     '0011'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCSGRP'
     C                   SETON                                        90
     C                   MOVEA     '1100'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCSCHK'
     C                   SETON                                        90
     C                   MOVEA     '0111'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCSEOB'
     C                   SETON                                        90
     C                   MOVEA     '1000'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCSIRQ'
     C                   CALL      'SC530R'
     C                   PARM                    SCSIRQ
     C                   PARM                    QQRTCD            1
     C                   SETON                                        90
     C***                  MOVEA'1001'    *IN,86
      *_________________________________________________________________
      *
      *  NO SEARCH AVAILABLE
      *
     C                   OTHER
     C                   SETON                                        4990
     C     KJNUM         IFEQ      99
     C                   MOVE      I6(KJPC)      CH4               4
     C                   MOVEA     CH4           *IN(86)
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
      *================================================================
      *  KGDO - Process F7  "Financial Summary" request.              *
      *================================================================
     C     KGDO          BEGSR
      *
      *  F7=Claimant Financial Info Window
      *
      *** BEGIN ADD *** 11/18/97 **************************************
      *
      *  Access Claim Master to obtain RRN
      *
     C     KEY           CHAIN     PCM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      *** END   ADD *** 11/18/97 **************************************
      *
     C                   CALL      'WA009R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
      *** BEGIN CHG *** 11/18/97 **************************************
     C*********************PARM SCDBRR    ##RRN
     C                   PARM      RRNCLM        ##RRN
      *
     C                   ENDIF
      *** END   CHG *** 11/18/97 **************************************
      *
      * If F3=Exit is hit, Return to Menu.
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      *BLANK        SLIN1
     C                   MOVE      *BLANK        SLIN2
     C                   MOVE      PACO#         SCCO#
     C                   MOVE      PAFND         SCFND
     C                   MOVE      PAFYR         SCFYR
     C                   MOVE      PACASE        SCCASE
     C                   MOVE      PASEQ#        SCSEQ#
     C                   MOVE      PATRDT        SCYMD
     C                   MOVE      PAEOB#        SCEOB#
     C*** BEGIN ADD *** 06/12/00 **************************************
     C                   Z-ADD     PAEOBS        SCRCN
     C*** END   ADD *** 06/12/00 **************************************
     C                   MOVE      PACHK#        SCCHK#
     C                   MOVE      PAPGTP        SCPGTP
     C                   MOVE      PAACCT        SCACCT
     C*** Begin add *** 05/01/01 **************************************
     C                   MOVE      PACKTP        SCCKTP
     C*** End add ***** 05/01/01 **************************************
     C*** BEGIN ADD *** 10/10/02 **************************************
     C                   Z-ADD     PAXFYR        SCXFYR
     C                   Z-ADD     PAXCAS        SCXCAS
     C                   MOVEL(P)  PAADST        SCADST
     C*** END   ADD *** 10/10/02 **************************************
      *
      *  Build Subfile Record based on INFO# (F9=Info Display)
      *
     C                   SELECT
      *          _______________________________________________________
B01  C     INFO#         WHENEQ    1
      *
     C                   MOVEL(P)  PADESC        SDESC                          Payment Description
      *
     C*** BEGIN ADD *** 07/07/99 **************************************
     C*  Use the View Check file to display to payee name exactly as
     C*  it appeared on the check.  If that record can not be found
     C*  (possible due to converted pmts), then use the claimant or
     C*  payee demographics file.
     C*
     C                   MOVE      *BLANKS       SNAME
     C*** Begin del *** 08/12/03 **************************************
     C******     *IN99     IFEQ *ON
     C******               Z-ADDPABASQ    XXSEQ#
     C******     R2KY      CHAINPRP02                99
     C******               ENDIF
     C******
     C******     *IN99     IFEQ *ON
     C****End add ***** 05/22/03 **************************************
     C****Begin add *** 02/27/02 **************************************
     C******               Z-ADDPATRDT    XXTRDT
     C****End add ***** 02/27/02 **************************************
     C******     RPKY      CHAINPRP01                99
     C****Begin add *** 02/27/02 **************************************
     C******     *IN99     IFEQ *ON
     C******               Z-ADDPAPODT    XXTRDT
     C******     RPKY      CHAINPRP01                99
     C******               ENDIF
     C****End add ***** 02/27/02 **************************************
     C****Begin add *** 05/22/03 **************************************
     C******               ENDIF
     C****End add ***** 05/22/03 **************************************
     C*** End del ***** 08/12/03 **************************************
     C*** Begin add *** 08/12/03 **************************************
     C*** Begin add *** 08/12/03 **************************************
     C                   Z-ADD     PASEQ#        XXSEQ#
     C*** End add ***** 08/12/03 **************************************
     C*** Begin add *** 05/22/03 **************************************
     C     R2KY          CHAIN     PRP02                              99
     C*** BEG ADD *** 04/06/04 ***************************
     C  N99RPCHK#        COMP      PACHK#                             9999
     C*
     C     *IN99         IFEQ      *ON
     C     PABASQ        ANDNE     *ZEROS
     C*** END ADD *** 04/06/04 ***************************
     C                   Z-ADD     PABASQ        XXSEQ#
MJF  C********   *IN99     IFEQ *ON
     C     R2KY          CHAIN     PRP02                              99
     C*** BEG ADD *** 04/06/04 ***************************
     C  N99RPCHK#        COMP      PACHK#                             9999
     C*** END ADD *** 04/06/04 ***************************
     C                   ENDIF
     C*** BEG ADD *** 04/06/04 ***************************
MJF  C********             ENDIF
     C*** END ADD *** 04/06/04 ***************************
      *
     C     *IN99         IFEQ      *ON
     C*
     C*** BEG ADD *** 12/10/03 ***************************
     C     PACHK#        IFNE      *ZEROS
     C*** BEG ADD *** 04/06/04 ***************************
     C     PABASQ        ANDNE     *ZEROS
     C*** END ADD *** 04/06/04 ***************************
     C*** END ADD *** 12/10/03 ***************************
     C*
     C*** Setup the sequence # to match
     C                   Z-ADD     PABASQ        XBSEQ#
     C*
     C*** Read payments for FND/CHK#
     C     PAKY          CHAIN     DBRCDF4                            98
     C     *IN98         DOWEQ     *OFF
     C*
     C*** Match found
     C     XBSEQ#        IFEQ      P4SEQ#
     C                   Z-ADD     P4BASQ        XXSEQ#
     C*
     C*** Try for RP rcd.
     C     R2KY          CHAIN     PRP02                              99
     C*** BEG ADD *** 04/06/04 ***************************
     C  N99RPCHK#        COMP      PACHK#                             9999
     C*** END ADD *** 04/06/04 ***************************
     C*
     C*** Not found, reset and look for the next base seq#
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     P4BASQ        XBSEQ#
     C*
     C** If this is the original rcd and still not found,
     C** leave to prevent endless loop.
     C     XBSEQ#        IFEQ      *ZEROS
     C                   LEAVE
     C                   ENDIF
     C*
     C*** BEG CHG *** 12/10/03 ***************************
     C*********  PAKY      CHAINDBRCDF4              98
     C     PAKY          READE     DBRCDF4                                98
     C*** END CHG *** 12/10/03 ***************************
     C                   ITER
     C*** Found RCD, leave
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     PAKY          READE     DBRCDF4                                98
     C                   ENDDO
     C*** BEG ADD *** 12/10/03 ***************************
     C                   ENDIF
     C*** END ADD *** 12/10/03 ***************************
     C                   ENDIF
      *
     C*** End add ***** 08/12/03 **************************************
     C     *IN99         IFEQ      *OFF
      *
     C                   MOVEL     RPPNAM        SNAME
      *
     C                   ELSE
     C*** END ADD ***** 07/07/99 **************************************
      *
      *  Assign Name as either Claimant or Payee
      *
     C                   MOVE      S2SS1         SS1
     C                   MOVE      S2SS2         SS2
     C                   MOVE      S2SS3         SS3
     C*** BEGIN ADD *** 02/20/98 ***********************************************
     C                   MOVE(P)   PAFEIN        XXFEIN            9
     C*** END ADD ***** 02/20/98 ***********************************************
      *
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE1          10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     SSN           PARM      SSN           QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     PAFEIN        IFNE      *ZERO
     C*** BEGIN CHG *** 02/20/98 ***********************************************
     C********** PAFEIN    ANDNESSN
     C     XXFEIN        ANDNE     SSN
     C*** END CHG ***** 02/20/98 ***********************************************
      *
      *  Assign Payee Name
      *
     C     PDKY          CHAIN     PPD01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *BLANKS       SNAME
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL     PDNAME        SNAME
     C                   ELSE
     C                   MOVEL(P)  PDNAME        QQLAST           56
     C                   MOVEL(P)  PDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SNAME
     C                   ENDIF
     C                   ENDIF
      *
     C                   ELSE
      *
      *  Assign Claimant Name
      *
     C                   MOVEL     S2NAME        SNAME
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/07/99 **************************************
     C                   ENDIF
     C*** END ADD ***** 07/07/99 **************************************
      *
      *          _______________________________________________________
X01  C     INFO#         WHENEQ    2
      *
     C                   MOVEL(P)  PACKTP        SBKCD                          Bank Acct# Code
      *
     C     PAFEIN        IFNE      *ZERO
     C                   MOVE      PAFEIN        SFEIN#                         Federal ID #
     C                   MOVE      PAMOD         SMOD                           Mod
     C                   ENDIF
      *
     C     PAEOB#        IFNE      *ZERO
     C                   MOVE      PAEOB#        SEOB#                          EOMB #
     C                   ENDIF
     C*** BEG ADD *** 03/15/99 ***************************************
     C     PAEOBS        IFNE      *ZERO
     C                   MOVE      PAEOBS        SRCN#
     C                   ENDIF
     C*** END ADD *** 03/15/99 ***************************************
      *
      *  Clear Date
      *
     C                   MOVEL     PACASE        XXCSSQ           12
     C                   MOVE      PASEQ#        XXCSSQ
     C                   MOVE      XXCSSQ        XXKY
     C                   MOVEL     PAFYR         XXKY
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      *OFF
     C     CRACTP        ANDEQ     'X'
     C                   Z-ADD     CRACDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL     XXNUM         SCLRDT
     C                   ENDIF
      *
E01  C                   ENDSL
      *
      *  Service "From" Date
      *
     C                   Z-ADD     PAFRDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL     XXNUM         SDT1
      *
      *  Service "To" Date
      *
     C                   Z-ADD     PATODT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM
     C                   PARM                    EDTMSK
     C                   MOVEL     XXNUM         SDT2
      *
      *  Assign Remaining Subfile fields
      *
     C                   MOVE      SLIN1         SCDATA
     C                   MOVE      SLIN2         SCDAT2
      *
      *  Transaction Date
      *
     C                   Z-ADD     PATRDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCTRDT
      *
      *  Posting Date
      *
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        SRDTI
     C                   ELSE
     C                   Z-ADD     PAPODT        SRDTI
     C                   ENDIF
     C**************       Z-ADDPAPODT    SRDTI
      ***** END CHG ***** 12/15/06 ***************************************
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCPODT
      *
     C     PATYPE        IFEQ      'S'
     C     PATYPE        OREQ      'V'
     C     PATYPE        OREQ      'R'
     C     PAAMT         MULT      -1            SCAMT                          Amount
     C                   ELSE
     C                   Z-ADD     PAAMT         SCAMT                          Amount
     C                   ENDIF
      *
     C                   MOVE      PABR2         SCBR2                          Payment Value 1
     C                   Z-ADD     PAWKS         SCWKS                          # Weeks
     C                   Z-ADD     PADAYS        SCDYS                          # Days
      *
      *** BEGIN CHG *** 11/18/97 *************************************
     C*********************MOVE *OFF      *IN89
     C                   MOVE      *OFF          *IN44
     C     PAPGTP        IFEQ      'G'
     C*********************MOVE *ON       *IN89
     C                   MOVE      *ON           *IN44
     C                   ENDIF
      *** END   CHG *** 11/18/97 *************************************
      *
     C                   ENDSR
      *
      *===============================================================
      * OPT6 - Subroutine for Option 6 (Reprint File Copy)
      *===============================================================
     C     OPT6          BEGSR
      *
      *  Access Bank Info file to obtain current Account record
      *
     C                   MOVE      'WC'          XXAPPL            2            Application
      *
     C     BIKY1         SETLL     PBI01
     C     BIKY2         READE     PBI01                                  60
     C*** Begin add *** 05/01/01 **************************************
     C     *IN60         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKY2         READE     PBI01                                  60
     C                   ENDDO
     C*** End add ***** 05/01/01 **************************************
      *
     C                   MOVEL(P)  '*STD'        ##FTYP           10            Formtype
     C                   MOVEL(P)  'QPRINT'      ##CHKQ           10            Filecopy OUTQ
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  BIFTYP        ##FTYP                         Formtype
     C                   MOVEL(P)  BICPYQ        ##CHKQ                         Filecopy OUTQ
     C                   ENDIF
      *
      *  Call FILECOPY Print Program
      *
     C                   MOVE      SCCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      SCYMD         ##TRDT            7
     C                   MOVE      SCSEQ#        ##SEQ#            5
     C*
     C                   CALL      'W7019C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    ##TRDT
     C                   PARM                    ##SEQ#
     C                   PARM                    ##FTYP                         Formtype
     C                   PARM                    ##CHKQ                         Check/Voucher OUTQ
      *
     C                   ENDSR
      *
      *===============================================================
      * OPTEOB - Subroutine for Option 15 (Reprint EOB)
      *          and Option 16 (Display EOB)
      *===============================================================
     C     OPTEOB        BEGSR
      *
     C*** BEGIN DLT *** 01/07/98 ***********************************************
     C*  CHAIN TO THE WCMC FUND X-REF FILE A GET THE SUB# AND GRP#
     C**********
     C********** CXKY      CHAINPFN02                60
     C**********           Z-ADDFNSUB#    XXSUB#
     C**********           Z-ADDFNFND     XXGRP#
     C**********
     C*  CHAIN TO THE EOMB FILE TO VALIDATE THE EOMB.
     C**********
     C********** EOKY      CHAINPEO01                60
     C********** *IN60     IFEQ '1'
     C*   EOMB# not found
     C**********           ENDIF
     C*** END DLT ***** 01/07/98 ***********************************************
     C*** BEGIN ADD *** 01/07/98 ***********************************************
     C*
     C*  Verify that the pmt has a valid EOMB attached.
     C*
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           MOVE KKCO#     ##CO#
     C**********           MOVE KKFND     ##FND
     C**********           MOVE SCFYR     ##FYR
     C**********           MOVE SCCASE    ##CASE
     C**********           MOVE SCEOB#    ##EOB#
     C**********           CALL 'FA015R'
     C**********           PARM           ##CO#   30
     C**********           PARM           ##FND   30
     C**********           PARM           ##FYR   30
     C**********           PARM           ##CASE  70
     C**********           PARM           ##EOB#  90
     C**********           PARM           ##RRN   90
     C**********           PARM           ##SUB#  50
     C**********           PARM           ##GRP#  30
     C**********           PARM *BLANK    ##RTCD  1
     C*
     C********** ##RRN     IFEQ 0
     C**********           SETON                     9084  NO EOMB#
     C**********           END
     C*** BEGIN ADD *** 10/10/02 **************************************
     C     SCADST        IFNE      *BLANKS
     C     SCXFYR        ANDNE     *ZEROS
     C     SCXCAS        ANDNE     *ZEROS
     C                   Z-ADD     SCXFYR        ##FYR
     C                   Z-ADD     SCXCAS        ##CASE
     C                   MOVE      SCXFYR        QQFYR
     C                   MOVE      SCXCAS        QQCASE
     C                   ELSE
     C                   Z-ADD     SCFYR         ##FYR
     C                   Z-ADD     SCCASE        ##CASE
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   ENDIF
     C*** END   ADD *** 10/10/02 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA031R'
1    C                   PARM      KKCO#         ##CO#             3 0          Company #
2    C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C*** BEGIN CHG *** 10/10/02 **************************************
3    C**********           PARM SCFYR     ##FYR   30       Policy Year
4    C**********           PARM SCCASE    ##CASE  70       Claim #
3    C                   PARM                    ##FYR             3 0          Policy Year
4    C                   PARM                    ##CASE            7 0          Claim #
     C*** END   CHG *** 10/10/02 **************************************
5    C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
6    C                   PARM      SCRCN         ##RCN             3 0          RECON #
7    C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ELSE
     C*
     C                   CALL      'FA032R'
     C                   PARM      KKCO#         ##CO#             3 0          Company #
     C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C*** BEGIN CHG *** 10/10/02 **************************************
     C**********           PARM SCFYR     ##FYR   30       Policy Year
     C**********           PARM SCCASE    ##CASE  70       Claim #
     C                   PARM                    ##FYR             3 0          Policy Year
     C                   PARM                    ##CASE            7 0          Claim #
     C*** END   CHG *** 10/10/02 **************************************
     C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
     C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ENDIF
     C*
     C     ##RC          IFEQ      '1'
     C     ##RC          OREQ      '2'
     C     ##RC          OREQ      '5'
     C*
     C                   SETON                                        8490
     C*
     C                   ENDIF
     C*
     C                   MOVE      KKCO#         #@CO#
     C                   MOVE      KKFND         #@WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@WCF#            3
     C                   PARM      *BLANKS       ##SUB#            5
     C                   PARM      *BLANKS       ##GRP#            3
     C*** END   CHG *** 06/12/00 **************************************
     C*** END ADD ***** 01/07/98 ***********************************************
      *
      *  CALL PROGRAM TO DISPLAY/REPRINT THE EOMB.
      *
     C                   MOVE      KKCO#         QQCO#
     C*** BEGIN DLT *** 01/07/98 ***********************************************
     C******************** MOVE XXSUB#    QQSUB#
     C******************** MOVE XXGRP#    QQGRP#
     C*** END DLT ***** 01/07/98 ***********************************************
     C*** BEGIN ADD *** 01/07/98 ***********************************************
     C                   MOVE      ##SUB#        QQSUB#
     C                   MOVE      ##GRP#        QQGRP#
     C*** END ADD ***** 01/07/98 ***********************************************
     C                   MOVE      SCEOB#        QQEOB#
     C*** BEGIN DEL *** 10/10/02 **************************************
     C**********           MOVE SCFYR     QQFYR
     C**********           MOVE SCCASE    QQCASE
     C*** END   DEL *** 10/10/02 **************************************
      *
     C     SCSEL         IFEQ      '16'
     C                   MOVE      '1'           QQMODE
     C                   ELSE
     C                   MOVE      'D'           QQMODE
     C                   END
      *
     C*** BEG CHG *** 01/23/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C*** BEG CHG *** 02/23/99 ***************************************
     C     SCSEL         IFEQ      '15'
     C********** SCSEL     IFEQ '16'
     C*** END CHG *** 02/23/99 ***************************************
     C*** BEG CHG *** 03/11/99 ***************************************
     C                   MOVE      'P'           #1MODE
     C                   MOVEL     UPOUTQ        #1QUE
     C**********           MOVE ' '       #1MODE
     C*** END CHG *** 03/11/99 ***************************************
     C                   ELSE
     C                   MOVE      'D'           #1MODE
     C*** BEG ADD *** 03/11/99 ****************************************
     C                   MOVEL     *BLANKS       #1QUE            10
     C*** END ADD *** 03/11/99 ****************************************
     C                   ENDIF
     C*
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           MOVE ##GRP#    ##WCMS
     C**********           CALL 'FA015R'
     C**********           PARM           ##CO#            Company #
     C**********           PARM           ##WCMS  30       WCMS Fund
     C**********           PARM           ##FYR            Policy Year
     C**********           PARM           ##CASE           Claim #
     C**********           PARM           ##EOB#           EOMB #
     C**********           PARM           ##RRN   90       EOMB RRN#
     C**********           PARM           ##SUB#           Subscriber #
     C**********           PARM           ##GRP#           WCMC Group #
     C**********           PARM           ##RTCD           Return Code
     C**********           PARM           ##AST   2
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA033R'
1    C                   PARM      KKCO#         ##CO#             3 0          Company #
2    C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C*** BEGIN CHG *** 10/10/02 **************************************
3    C**********           PARM SCFYR     ##FYR   30       Policy Year
4    C**********           PARM SCCASE    ##CASE  70       Claim #
3    C                   PARM                    ##FYR             3 0          Policy Year
4    C                   PARM                    ##CASE            7 0          Claim #
     C*** END   CHG *** 10/10/02 **************************************
5    C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
6    C                   PARM      SCRCN         ##RCN             3 0          RECON #
7    C                   PARM      *ZEROS        ##FEIN            9 0          FEIN
8    C                   PARM      *ZEROS        ##MOD             3 0          MOD
9    C                   PARM      *ZEROS        ##CHK#            9 0          CHECK NUMBER
10   C                   PARM      *ZEROS        ##OTHR            9 2          EOMB billing amt
11   C                   PARM      *BLANKS       ##AST             2            ADJUDICATION ST
12   C                   PARM      *ZEROS        ##BINV            9 0          BRS INV#
13   C                   PARM      *ZEROS        ##NINV            9 0          NWS INV#
14   C                   PARM      *ZEROS        #@SUB#            5 0          SUBSCRIBER
15   C                   PARM      *ZEROS        #@FND             3 0          GROUP
16   C                   PARM      *ZEROS        ##RRN             9 0          RRN OF FAEOP
17   C                   PARM      *BLANKS       ##RTCD            1            Return Code
     C*
     C                   ELSE
     C*
     C                   CALL      'FA034R'
01   C                   PARM      KKCO#         ##CO#             3 0          Company #
02   C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C*** BEGIN CHG *** 10/10/02 **************************************
03   C**********           PARM SCFYR     ##FYR   30       Policy Year
04   C**********           PARM SCCASE    ##CASE  70       Claim #
     C                   PARM                    ##FYR             3 0          Policy Year
     C                   PARM                    ##CASE            7 0          Claim #
     C*** END   CHG *** 10/10/02 **************************************
05   C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
06   C                   PARM      *ZEROS        ##FEIN            9 0          FEIN
07   C                   PARM      *ZEROS        ##MOD             3 0          MOD
08   C                   PARM      *ZEROS        ##CHK#            9 0          CHECK NUMBER
09   C                   PARM      *ZEROS        ##OTHR            9 2          EOMB billing amt
10   C                   PARM      *BLANKS       ##AST             2            Adjudication St
11   C                   PARM      *ZEROS        ##BINV            9 0          BRS INV#
12   C                   PARM      *ZEROS        ##NINV            9 0          NWS INV#
13   C                   PARM      *ZEROS        #@SUB#            5 0          Subscriber #
14   C                   PARM      *ZEROS        #@FND             3 0          WCMC Group #
15   C                   PARM      *ZEROS        ##RRN             9 0          RRN of FAEOP
16   C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ENDIF
     C*** END   CHG *** 06/12/00 **************************************
     C*
     C*** BEG ADD *** 03/22/99 ***************************************
     C     ##RTCD        IFEQ      *BLANK
     C*** END ADD *** 03/22/99 ***************************************
     C*
     C                   MOVE      ##RRN         #1RRN
     C*
     C                   CALL      'F0340C'
     C                   PARM                    #1RRN             9
     C                   PARM      ##AST         #1AST             2
     C                   PARM                    #1MODE            1
     C*** BEG ADD *** 03/11/99 ***************************************
     C                   PARM                    #1QUE
     C*** END ADD *** 03/11/99 ***************************************
     C*** BEG ADD *** 03/22/99 ***************************************
     C                   ELSE
     C                   SETON                                        9084      #EOMB
     C                   ENDIF
     C*** END ADD *** 03/22/99 ***************************************
     C*
     C                   ELSE
     C*
     C                   CALL      'F7200C1'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQGRP#            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
     C                   PARM                    UPOUTQ
     C                   PARM                    QQMODE            1
     C*
     C                   ENDIF
     C*** END CHG *** 01/23/99 ***************************************
      *
     C                   ENDSR
      *
      *** BEGIN ADD *** 09/07/99 **************************************
      *===============================================================
      * OPT20 - Subroutine for Option 20 (Reverse deductible recovery)
      *===============================================================
     C     OPT20         BEGSR
     C*
     C*** Begin add *** 01/02/02 **************************************
     C*  Display a window to allow entry of the reversal amount.
     C*
     C     SCAMT         MULT      -1            SCRAMT
     C     SCAMT         MULT      -1            SVAMT             9 2
     C*
     C     RDSPW1        TAG
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ELSE
     C*
     C     SCRAMT        IFLE      0
     C                   SETON                                        71
     C                   GOTO      RDSPW1
     C                   ENDIF
     C     SCRAMT        IFGT      SVAMT
     C                   SETON                                        72
     C                   GOTO      RDSPW1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*** End add ***** 01/02/02 **************************************
      *
      * Call the Deductible recovery reversal pgm (W8025R).
      *
     C                   MOVE      SCCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCSEQ#        ##SEQ#
     C                   MOVE      SCRAMT        ##RAMT
      *
     C                   CALL      'W8025R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    ##SEQ#
     C                   PARM                    ##RAMT            9
     C                   PARM      *BLANK        ##RTCD
     C*
     C*** Begin add *** 01/02/02 **************************************
     C                   ENDIF
     C*** End add ***** 01/02/02 **************************************
      *
     C                   ENDSR
      *** END ADD ***** 09/07/99 **************************************
      *
      *===============================================================
      * OPT80 - Subroutine for Option 80 (Time & Materials)
      *===============================================================
     C     OPT80         BEGSR
     C*
     C*** BEGIN ADD *** 01/07/98 ***********************************************
     C*  Retieve the WCMC Sub# and Grp#.
     C*
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           MOVE KKCO#     ##CO#
     C**********           MOVE KKFND     ##FND
     C**********           MOVE SCFYR     ##FYR
     C**********           MOVE SCCASE    ##CASE
     C**********           MOVE SCEOB#    ##EOB#
     C**********           CALL 'FA015R'
     C**********           PARM           ##CO#
     C**********           PARM           ##FND
     C**********           PARM           ##FYR
     C**********           PARM           ##CASE
     C**********           PARM           ##EOB#
     C**********           PARM           ##RRN2
     C**********           PARM           ##SUB#
     C**********           PARM           ##GRP#
     C**********           PARM *BLANK    ##RTCD
     C*** BEGIN ADD *** 10/10/02 **************************************
     C     SCADST        IFNE      *BLANKS
     C     SCXFYR        ANDNE     *ZEROS
     C     SCXCAS        ANDNE     *ZEROS
     C                   Z-ADD     SCXFYR        ##FYR
     C                   Z-ADD     SCXCAS        ##CASE
     C                   MOVE      SCXFYR        QQFYR
     C                   MOVE      SCXCAS        QQCASE
     C                   ELSE
     C                   Z-ADD     SCFYR         ##FYR
     C                   Z-ADD     SCCASE        ##CASE
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   ENDIF
     C*** END   ADD *** 10/10/02 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA033R'
1    C                   PARM      KKCO#         ##CO#             3 0          Company #
2    C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C*** BEGIN CHG *** 10/10/02 **************************************
3    C**********           PARM SCFYR     ##FYR   30       Policy Year
4    C**********           PARM SCCASE    ##CASE  70       Claim #
3    C                   PARM                    ##FYR             3 0          Policy Year
4    C                   PARM                    ##CASE            7 0          Claim #
     C*** END   CHG *** 10/10/02 **************************************
5    C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
6    C                   PARM      SCRCN         ##RCN             3 0          RECON #
7    C                   PARM      *ZEROS        ##FEIN            9 0          FEIN
8    C                   PARM      *ZEROS        ##MOD             3 0          MOD
9    C                   PARM      *ZEROS        ##CHK#            9 0          CHECK NUMBER
10   C                   PARM      *ZEROS        ##OTHR            9 2          EOMB billing amt
11   C                   PARM      *BLANKS       ##AST             2            ADJUDICATION ST
12   C                   PARM      *ZEROS        ##BINV            9 0          BRS INV#
13   C                   PARM      *ZEROS        ##NINV            9 0          NWS INV#
14   C                   PARM      *ZEROS        #@SUB#            5 0          SUBSCRIBER
15   C                   PARM      *ZEROS        #@FND             3 0          GROUP
16   C                   PARM      *ZEROS        ##RRN2            9 0          RRN OF FAEOP
17   C                   PARM      *BLANKS       ##RTCD            1            Return Code
     C*
     C                   ELSE
     C*
     C                   CALL      'FA034R'
01   C                   PARM      KKCO#         ##CO#             3 0          Company #
02   C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C*** BEGIN CHG *** 10/10/02 **************************************
03   C**********           PARM SCFYR     ##FYR   30       Policy Year
04   C**********           PARM SCCASE    ##CASE  70       Claim #
03   C                   PARM                    ##FYR             3 0          Policy Year
04   C                   PARM                    ##CASE            7 0          Claim #
     C*** END   CHG *** 10/10/02 **************************************
05   C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
06   C                   PARM      *ZEROS        ##FEIN            9 0          FEIN
07   C                   PARM      *ZEROS        ##MOD             3 0          MOD
08   C                   PARM      *ZEROS        ##CHK#            9 0          CHECK NUMBER
09   C                   PARM      *ZEROS        ##OTHR            9 2          EOMB billing amt
10   C                   PARM      *BLANKS       ##AST             2            Adjudication St
11   C                   PARM      *ZEROS        ##BINV            9 0          BRS INV#
12   C                   PARM      *ZEROS        ##NINV            9 0          NWS INV#
13   C                   PARM      *ZEROS        #@SUB#            5 0          Subscriber #
14   C                   PARM      *ZEROS        #@FND             3 0          WCMC Group #
15   C                   PARM      *ZEROS        ##RRN2            9 0          RRN of FAEOP
16   C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ENDIF
     C*** END   CHG *** 06/12/00 **************************************
     C*** END ADD ***** 01/07/98 ***********************************************
     C*
     C*** BEGIN ADD *** 01/06/99 **************************************
     C*  Call the Time & Materials program.
     C*
     C                   CALL      'F3040R'
     C                   PARM      '1'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C*** BEGIN ADD *** 06/12/00 **************************************
     C                   PARM      *BLANKS       ##EOB             1
     C                   PARM      SCRCN         ##RCN             3 0
     C*** END   ADD *** 06/12/00 **************************************
     C*** END ADD ***** 01/06/99 **************************************
      *
     C*** BEGIN DLT *** 01/06/99 **************************************
     C**********           MOVELOPTNM     QQPRDS 40
     C**********           MOVE KKCO#     QQCO#
     C*** BEGIN DLT *** 01/07/98 ***********************************************
     C******************** MOVE *BLANKS   QQSUB#
     C******************** MOVE XXGRP#    QQGRP#
     C*** END DLT ***** 01/07/98 ***********************************************
     C*** BEGIN ADD *** 01/07/98 ***********************************************
     C**********           MOVE ##SUB#    QQSUB#
     C**********           MOVE ##GRP#    QQGRP#
     C*** END ADD ***** 01/07/98 ***********************************************
     C**********           MOVE SCFYR     QQFYR
     C**********           MOVE SCCASE    QQCASE
     C**********           MOVE SCEOB#    QQEOB#
     C**********           MOVE '2'       QQRTCD  1
     C**********
     C**********           CALL 'N0540R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCO#
     C**********           PARM           QQSUB#
     C**********           PARM           QQGRP#
     C**********           PARM           QQFYR
     C**********           PARM           QQCASE
     C**********           PARM           QQEOB#
     C**********           PARM           QQRTCD
     C**********
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           ENDIF
     C*** END DLT ***** 01/06/99 **************************************
      *
     C                   ENDSR
      *
      *===============================================================
      * OPT99 - Subroutine for Option 99 (Duplicate Payment Exclusion)
      *===============================================================
     C     OPT99         BEGSR
      *
     C                   MOVE      'U'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C     *INKH         CASEQ     *ON           KHDO                           F8  Paymt Summary
     C     *INKG         CASEQ     *ON           KGDO                           F7  Financ.Summary
     C                   CAS                     ENTRDO
     C                   ENDCS
      *
     C                   ENDSR
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '15'          OPTEOB
     C     SCSEL         CASEQ     '16'          OPTEOB
      *** BEGIN ADD *** 09/07/99 **************************************
     C     SCSEL         CASEQ     '20'          OPT20
      *** END ADD ***** 09/07/99 **************************************
     C     SCSEL         CASEQ     '80'          OPT80
     C     SCSEL         CASEQ     '99'          OPT99
     C                   ENDCS
      *
     C                   ENDSR
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
     C*
     C***  BEGIN ADD  ***  05/27/99  *********************************
     C                   MOVE      *OFF          *IN12
     C***  END   ADD  ***  05/27/99  *********************************
      *
     C/COPY SINDIC01
      *
     C                   MOVE      @@SFDT        SCSFDT                         Service FROM Date
     C                   MOVE      @@STDT        SCSTDT                         Service TO Date
     C                   MOVEA     @@SPC         PC                             Payment Codes
     C                   MOVEA     @@STT         TT                             Transaction Type
     C                   MOVEA     @@SPV1        PV1                            Payment Value 1
     C                   MOVEA     @@SRC         RC                             Reserve Codes
     C                   MOVE      @@SFEI        SCSFEI                         Federal ID#
     C*** BEG ADD ***** 01/19/01 ***********************************************
     C     Q1PRMS        IFGE      20
     C                   MOVE      @@SMOD        SCSMOD                         Modifier
     C                   ENDIF
     C*** END ADD ***** 01/19/01 ***********************************************
     C                   MOVE      @@SCHK        SCSCHK                         Check #
     C                   MOVE      @@SEOB        SCSEOB                         EOMB #
     C                   MOVE      @@SIRQ        SCSIRQ                         Initial Requestor
     C                   MOVE      @@SGRP        SCSGRP                         Group vs. Non-Group
      *
     C                   Z-ADD     SCSFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSFDT
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
      *
     C                   Z-ADD     SCSFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM            7 0
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO              7 0
      *
      *  Access User Parameters
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF
      *
     C                   MOVE      *ON           *IN20
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      @@CO#         #ACO#
     C                   MOVE      @@FND         #AFND
     C                   MOVE      @@FYR         #AFYR
     C                   MOVE      @@CASE        #ACASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   RETURN
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@FND         ##FND
     C                   MOVE      @@FYR         ##FYR             3 0
     C                   MOVE      @@CASE        ##CASE            7 0
      *
     C                   CALL      'WA006R'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    SCCS01           79
     C                   PARM                    SCCS02
      *
      *  Assign Headings/Flag for "Totals" line in Claim Header
      *
     C                   MOVE      *BLANK        SCCS03
     C                   MOVE      HEAD1         S3HD1
     C                   MOVE      HEAD2         S3HD2
     C                   MOVE      HEAD3         S3HD3
     C                   MOVE      *ON           BLDTOT            1            Build Totals
      *
     C                   MOVE      HDA(1)        SCHDLA
      *
     C*** BEGIN ADD *** 01/07/98 ***********************************************
     C*  Determine if WCMC is installed.
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C*** BEGIN ADD *** 01/06/99 ***********************************************
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C*** END ADD ***** 01/06/99 ***********************************************
     C*
     C*  If WCMC is not installed or not interfaced with WCMS, do not
     C*    display options 15, 16, or 80.
     C*
     C     ##MCIF        IFNE      'Y'
     C     ##WCIN        ORNE      'Y'
     C*
     C                   Z-ADD     1             X
     C     OF(X)         DOWEQ     '1'
     C*
     C                   MOVEL     OFD(X)        XXWK2A            2
     C     XXWK2A        IFEQ      '15'
     C     XXWK2A        OREQ      '16'
     C     XXWK2A        OREQ      '80'
     C                   MOVE      *BLANKS       OFD(X)
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF
     C*** END ADD ***** 01/07/98 ***********************************************
     C*
     C*** BEGIN ADD *** 01/06/99 ***********************************************
     C*  If WCMC medical management is not installed, do not display
     C*    option 80.
     C*
     C     ##MED         IFNE      'Y'
     C*
     C                   Z-ADD     1             X
     C     OF(X)         DOWEQ     '1'
     C*
     C                   MOVEL     OFD(X)        XXWK2A
     C     XXWK2A        IFEQ      '80'
     C                   MOVE      *BLANKS       OFD(X)
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF
     C*** END ADD ***** 01/06/99 ***********************************************
     C*** BEGIN ADD *** 12/26/02 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** END   ADD *** 12/26/02 **************************************
      *
     C                   ENDSR
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
      *
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCDF2                                95
     C   23KEY           READE     DBRCDF3                                95
      *
     C                   ENDSR
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
      *
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C   23KEY           READPE    DBRCDF3                                95
      *
     C                   ENDSR
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
      *
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
      *
     C                   ENDSR
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
      *
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
      *
     C                   ENDSR
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
      *
      * Service Date Range
      *
     C     SCSFDT        IFNE      *ZERO
     C                   MOVE      *OFF          MEETSS
      *
     C     XXFROM        IFGE      PAFRDT
     C     XXFROM        ANDLE     PATODT
      *
     C     XXTO          ORGE      PAFRDT
     C     XXTO          ANDLE     PATODT
      *
     C     XXFROM        ORLE      PAFRDT
     C     XXTO          ANDGE     PATODT
      *
     C                   MOVE      *ON           MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Payment Code
      *
     C                   MOVEA     PC            CH10             10
     C     CH10          IFNE      *BLANK
     C     PATRCD        LOOKUP    PC                                     40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Transaction Type
      *
     C                   MOVEA     TT            CH5               5
     C     CH5           IFNE      *BLANK
     C     PATYPE        LOOKUP    TT                                     40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Payment Value 1
      *
     C                   MOVEA     PV1           CH10             10
     C     CH10          IFNE      *BLANK
     C     PABR2         LOOKUP    PV1                                    40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Reserve Code
      *
     C                   MOVEA     RC            CH10             10
     C     CH10          IFNE      *BLANK
     C     PARECD        LOOKUP    RC                                     40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Group vs. Non-Group
      *
     C     SCSGRP        IFNE      *BLANK
     C     PAPGTP        ANDNE     SCSGRP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Federal ID #
      *
     C     SCSFEI        IFNE      *ZERO
     C     PAFEIN        ANDNE     SCSFEI
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C*** BEG ADD ***** 01/19/01 ***********************************************
     C*** BEG DEL *** 03/07/02 **************************************  *********
     C*********  Q1PRMS    IFGE 20
     C*** END DEL *** 03/07/02 **************************************  *********
      *
     C*** BEG ADD *** 03/07/02 **************************************  *********
     C                   MOVE      SCSMOD        XXSMOD            3 0
     C*** END ADD *** 03/07/02 **************************************  *********
     C     SCSFEI        IFNE      *ZERO
     C*** BEG CHG *** 03/07/02 **************************************  *********
     C*********  SCSMOD    IFNE *ZERO
     C*********  PAMOD     ANDNESCSMOD
     C     SCSMOD        IFNE      *BLANKS
     C     PAMOD         ANDNE     XXSMOD
     C*** END CHG *** 03/07/02 **************************************  *********
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*** BEG DEL *** 03/07/02 **************************************  *********
     C****************     ENDIF
     C*** END DEL *** 03/07/02 **************************************  *********
     C*** END ADD ***** 01/19/01 ***********************************************
      *
      *
      *
      * Check #
      *
     C     SCSCHK        IFNE      *ZERO
     C     PACHK#        ANDNE     SCSCHK
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * EOMB #
      *
     C     SCSEOB        IFNE      *ZERO
     C     PAEOB#        ANDNE     SCSEOB
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SCSIRQ        IFNE      *BLANK
     C     PARQST        ANDNE     SCSIRQ
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEGIN ADD *** 12/26/02 **************************************
     C*
     C* Exclude if Hospital Bill
     C*
     C     CLTID         IFEQ      '114'
     C*
     C     Q1PRMS        IFGT      20
     C     @@EXHS        IFEQ      'Y'
     C     PAEOB#        ANDNE     *ZEROS
     C*
     C                   CALL      'FA033R'
1    C                   PARM      PACO#         ##CO#             3 0          Company #
2    C                   PARM      PAFND         ##WCF#            3 0          WCMS Fund
3    C                   PARM      PAFYR         ##FYR             3 0          Policy Year
4    C                   PARM      PACASE        ##CASE            7 0          Claim #
5    C                   PARM      PAEOB#        ##EOB#            9 0          EOMB #
6    C                   PARM      PAEOBS        ##RCN             3 0          RECON #
7    C                   PARM      *ZEROS        ##FEIN            9 0          FEIN
8    C                   PARM      *ZEROS        ##MOD             3 0          MOD
9    C                   PARM      *ZEROS        ##CHK#            9 0          CHECK NUMBER
10   C                   PARM      *ZEROS        ##OTHR            9 2          EOMB billing amt
11   C                   PARM      *BLANKS       ##AST             2            ADJUDICATION ST
12   C                   PARM      *ZEROS        ##BINV            9 0          BRS INV#
13   C                   PARM      *ZEROS        ##NINV            9 0          NWS INV#
14   C                   PARM      *ZEROS        #@SUB#            5 0          SUBSCRIBER
15   C                   PARM      *ZEROS        #@FND             3 0          GROUP
16   C                   PARM      *ZEROS        ##RRN             9 0          RRN OF FAEOP
17   C                   PARM      *BLANKS       ##RTCD            1            Return Code
18   C                   PARM      *BLANKS       ##ACCT           30            Pat Account#
19   C                   PARM      *BLANKS       ##BTYP            1            Bill type
     C*
     C     ##BTYP        IFEQ      'H'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 12/26/02 **************************************
      *
     C                   ENDSR
     C*** BEG ADD ****** 07/26/07 ******************************************
     C     SRDTDF        BEGSR
     C                   Z-ADD     XXFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQFROM            8 0
      *
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQTO              8 0
      *
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
      *
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   ENDSR
     C*** END ADD ****** 07/26/07 ******************************************
** Column headings   TOP HEADING
    Payee Name                       Description
    Payee Name                       Description
    Payee Name                       Description
    FEIN       MOD      EOMB# RCN#   Clear Dt   Bank Acct Code
    FEIN       MOD      EOMB# RCN#   Clear Dt   Bank Acct Code
    FEIN       MOD      EOMB# RCN#   Clear Dt   Bank Acct Code
** Column headings   BOTTOM HEADING
Opt PV1  WK/D --- Service Dates ---  Post Date Trans Date  Check#       Amount
** Description--------------MISN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Reprint file copy      YYY
11515=Reprint EOB           YYY
11616=Display EOB           YYY
12020=Reverse ded recovery  YNN
18080=Time & materials      YYY
19999=Dup paymt exclusion   YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Financial summary     YYN
208F8=Paymt history summary YYN
209F9=Change info           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
