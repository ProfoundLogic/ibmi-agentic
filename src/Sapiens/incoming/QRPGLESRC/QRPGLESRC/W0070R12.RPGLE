     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     h  bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
      /TITLE W0070R12 - RESERVE / PAYMENT SUMMARY
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY RESERVE/PAYMENT SUMMARY
      *                                                               *
      *  CALLED FROM:                                                 *
      *    W0070R - CLAIM MASTER SEARCH                               *
      *                                                               *
      *  CALLS TO:  FA003R - API to update MC claim master FMCMP      *
      *             SA001R - API to determine if MC installed         *
      *                                                               *
      *  CREATED:  2/12/97  PROGRAMMER: 47    PROJECT: 60ANC          *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 01/16/19 C:062 Ticket: 76289(114)
     H*                Expand arrays used by WA576R5
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*
     H* 06/23/11 C:235 Project: 133WLN                                *
     H*                Claim Status Pop up Scrn-add status change     *
     H*
     H* 03/23/11 C: 58 Project: 133WKY                                *
     H*                Closing LT Claim, Window: check for pmts-stop  *
     H*
     H* 03/23/11  C: 58 Project: 133WKY                               *
     F*                Closing LT Claim, Window: check for pmts-stop  *
     H*
     H* 12/02/10  C:235 Project: 133WKI                               *
     H*                 Clm Reserve-Add Tbl bkup Open/Close Descrption*
     H*
     H* 11/09/10  C:227 Project: 133WJX                               *
     H*                 CMS edit:Users Chk/ORM and/or TPOC date       *
     H*                                                               *
     H* 06/07/10  C:247 Project: 25WQO                                *
     H*                 Fix code for LARGERSV diary (PRISS 45)        *
     H*
     H* 05/27/10  C:220 Project: 25WQO
     H*                 Made additinal changes
     H*
     H* 05/12/10  C:58  Project: WC1588 Condition the UPDATR
     H*                 subroutine for last screen only if
     H*                 claim JRST is GA.
     H*
     H* 03/29/10  C:190 Project: 25WQD
     H*                 Display information window if record is in    *
     H*                 pending status (For clt# 25 only)             *
     H*
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*
     H* 03/25/10  C:139 Project: 139WNF                               *
     H*                 EDI SCREEN DESIGN                             *s
     H*                 Only Compiled the pgm                         *s
     H*
     H* 03/23/10  C:061 Project: 133WIR
     H*                 Allow closure on indemnity claim if no         s
     H*                 compensation payments - after user reply
     H*                 to info window.
     H*
     H* 03/10/10  C:242 Project 25WRK
     H*                 Retrofit 25WJW
     H*                   Added code to delete the claim index suspense
     H*                   record when a claim is closed and create a
     H*                   record when a claim is reopened.
     H*
     H* 03/03/10  C:232 Project:  139WNH                              *
     H*                 Update all of the initial maintenance fields. *
     H*                 Populating RHIDT and RHDT with the current    *
     H*                 system date.                                  *
     H*
     H* 09/11/09  C:062 Project: 133WGP
     H*                 Don't allow closure on indemnity claim if no   s
     H*                 compensation payments.
     H*
     H* 08/28/09  C:189 Project: 133WHE
     H*                    Do not execute the call to the state programs
     H*                    if the client is LUBA and add the calls to
     H*                    W0190R and SC010R.
     H*
     F* 10/26/07  C:190 Project: WC1588
     F*                 Three entry fields are added to the
     F*                 second screen. It will only be displayed
     F*                 if the claim's jurisdiction state
     F*                 (CMJRST) = 'GA'.
     F*                 The entry fields are as follows:
     F*                 1.SITF effective date.
     F*                 2.SITF indemnity.
     F*                 3.SITF medical.
     F*
     H*  03/01/06 C:132 Support: WC071 51160
     H*                Change Code to load DSDTTM and use DSDATE
     H*                instead of *DATE anywhere in the program.
     H*
     H*  06/07/05 C: 53 Support: WC065 47167
     H*                Consider screen changes for reserves when the
     H*                edit is for cumulative reserves.
     H*
     H*   02/23/05  C:58     P: WC1128   Auto-DARTS
     H*                Add call to WA700R if change in claim status.
     H*                Add call to WA700R if change to reserve is
     H*                  => specified large amount.
     H*
     H* 03/23/04 C:15  Project:  WC1170                               *
     H*                Added code to load the reserve history buckets *
     H*                according to the order in the claim file.      *
     H*
     H*  04/01/03 C: 53 PROJECT: WC1122
     H*                 Add changes to reserve authority limits
     H*
     H*  05/23/02 C: 53 Support: WC101 # 32673
     H*                 Do not lock WDELP.
     H*
     H*  12/12/01 C: 53 PROJECT: WC1060
     H*                 ADD EXCLUDE MEDICAL PROCESSING.
     H*
     H*  08/25/00 C:062 PROJECT: MC232
     H*                 ADD JURISDICTION STATE TO PARM LIST OF FA003R.
     H*
     H*  06/23/00 C:15  Project: WC577
     H*                 Chgd pgm to prevent claim initial entry fields
     H*                 from being changed.
     H*
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
      * 04/14/00 C:15  Support: WC090/22236                           *
      *                Chgd pgm to load valid value in QQCALL when    *
      *                calling W0575R.                                *
      *                                                               *
      * 04/03/00 C:118 PROJECT: WC854                                 *
      *                Add parm to call of FA003R to pass WMCMP/      *
      *                CMIMP.                                         *
      *                                                               *
      * 10/26/99 C:15  Support: WC044/20043                           *
      *                Fixed reserve authority edit.                  *
      *                                                               *
      * 09/02/99 C:112 Support: 19256 (MC048)                         *
      *                Change QMSCL# from 10A to 25A                  *
      *                                                               *
      * 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
      *                              DR048/17294                      *
      *                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
      *                immediately.                                   *
      *                                                               *
      *  06/22/99 C:61 PROJECT 48WQJ                                  *
      *                Add previous fund number to header info.       *
      *                                                               *
      * 06/25/99 C:15 Support: WC034/17662                            *
      *               Added FEOD after the update to WMCMP to force   *
      *               the reserves to be updated to prevent negative  *
      *               reserves if a pmt is released.                  *
      *                                                               *
      * 11/16/98 C:15 Project: WC577                                  *
      *               Changed the bar code process.  A bar code API   *
      *               is now called to return certain parameters.     *
      *                                                               *
      * 08/31/98 C:15 Support: WC100/13042                            *
      *               If IXFROI and CMJRST are blank, do not call the *
      *               state specific screen because XXPGM = 'W0070R'  *
      *               and a recursive call error will result.         *
      *                                                               *
      * 08/10/98 C:61 Project: 48WLC                                  *
      *               Display old Emp#/Div from Employer Cross        *
      *               Reference (WZEXP).                              *
      *                                                               *
      * 08/11/98 C:62 Project: MC155                                  *
      *               ADDED PARM FOR CMFEAD TO FA003R                 *
      *                                                               *
      * 08/04/98 C:15 Project: WC577                                  *
      *               Fixed the claim record lockout routine.         *
      *                                                               *
      * 07/13/98 C:15 Support: WC051/12451                            *
      *               Removed a parm from ACDPL3 parm list.           *
      *                                                               *
      * 06/24/98 C:15 Support: WC051/12238                            *
      *               Fixed edit which verifies if reserve cushion is *
      *               adequate to close a claim with released pmts.   *
      *                                                               *
      * 04/24/98 C:15 Project: WC577  seq# 11096                      *
      *               Removed code which loads various claim fields   *
      *               in ADD mode.                                    *
      *                                                               *
      * 04/22/98 C:15 Project: WC577  seq# 10944                      *
      *               Removed code which loads the default Medical    *
      *               reserves.  This will be done in W0070R09.       *
      *                                                               *
      * 04/07/98 C:62 Project: MC155                                  *
      *               Added additional fields to FA003R parm list.    *
      *                                                               *
      * 02/23/98 C:85 Project: WC577                                  *
      *               CHANGED THE FIELD MOVED TO CMSMCD               *
      *               FROM *BLANKS TO CTSMCD.                         *
      *                                                               *
      * 02/17/98 C:15 Project: WC577                                  *
      *               Added code to pgm to call addl claim pgms if    *
      *               the State Report pgm was called but did not     *
      *               exist.                                          *
      *                                                               *
      * 02/03/98 C:15 Project: WC577                                  *
      *               Chgd pgm to prevent loop in add mode.           *
      *                                                               *
      * 01/16/98 C:47 Project: 60AND                                  *
      *               IF WRITING A REOPEN DIARY REMARK, FIRST CHECK   *
      *               TO SEE IF A REOPEN DIARY REMARK CODE HAS        *
      *               BEEN DEFINED (WMCXP:CXROPN).                    *
      *                                                               *
      * 11/05/97 C:15 Project: WC577                                  *
      *               Changed the format of date fields used in the   *
      *               parm lists for pgms FA003R from MDCY to CYMD.   *
      *                                                               *
      * ***************************************************************
      *                                                               *
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  07 - EDI INSTALLED                                           *
      *  09 - INQUIRY                                                 *
      * N09 - UPDATE                                                  *
      *  16 - INVALID CURSOR POSITION FOR SEARCH                      *
      *  40 - ADD MODE   ***********************  CHANGED TO 98       *
      * N40 - UPDATE MODE   ********************                      *
      *  63 - Old Emp#/Div displayed in header                        *
      *  64 - CLAIM WAS CONVERTED FROM ANOTHER SYSTEM                 *
      *  66 - COMMERCIAL APPLICATION INSTALLED                        *
      *  67 - SECURITY LEVEL VALID                                    *
      * N67 - SECURITY LEVEL INVALID                                  *
      *  69 - EXCESS REINSURANCE                                      *
      *  70 - BAR CODE INSTALLED                                      *
      *  98 - ADD MODE                                                *
      * N98 - UPDATE MODE                                             *
      *                                                               *
      * ***************************************************************
      *
     F*** BEG   CHG *** 03/29/10 **************************************
     FW0070F12  CF   E             WORKSTN USROPN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*****W0070F12CF  E                    WORKSTN
     F*** END   CHG *** 03/29/10 **************************************
     F                                     INFDS(FIDS01)
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
      *
     FWMEML     IF   E           K DISK
      *
     FWMFNL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *
     FWMCDL     IF   E           K DISK
      *
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*** BEG   ADD *** 03/29/10 **************************************
     FWMRWL     IF   E           K DISK
     F*** END   ADD *** 03/29/10 **************************************
      *
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PID01)
      *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *
      *  DARTS
     F*** BEGIN CHG *** 04/26/00 **************************************
     F*****SDCMLA  IF  E           K        DISK                      A
     FSDCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM04)
     F********SDCMLI  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM03
     F*** END CHG *** 04/26/00 ****************************************
      *
      *  CLAIM/DARTS CONTROL FILE
     FWMCXL     IF   E           K DISK
      *
      *  CLAIM/EDI CONTROL FILE
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************
      *
      *  REQUEST FOR PAYMENT FILE
     FWARPL     IF   E           K DISK
      *
      *  EMPLOYER DETAIL FILE
     FWDELL     UF   E           K DISK
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
      ***** END ADD ******* 08/10/98 **********************************
     F***** BEG ADD ******* 02/23/05 **********************************
     F*  Auto DARTS Control File
     F*
     FWMADL     IF   E           K DISK
     F***** END ADD ******* 02/23/05 **********************************
      *
      *  DARTS DIARY REMARK FILE
     FWMTHL     O    E             DISK
      *
      *  REMARK CODE FILE
     FSTCTL1    IF   E           K DISK
     F                                     RENAME(PCT01:PCT03)
      *
      *  RESERVE HISTORY FILE
     FWMRHL     O    E             DISK
      *
      *  CLAIM HISTORY
     FWMCHL     O    E           K DISK
      *
      *  RESERVE CUSHION DEF FILE
     FWDRCL     IF   E           K DISK
     F*** BEGIN ADD *** 03/10/10 **************************************
     FWAIDL1    UF A E           K DISK
     F*** END ADD ***** 03/10/10 **************************************
     F*** BEG ADD ***** 05/27/10 **************************************
     F*                                              KINFDS DBFDS
     FWDF1L     IF   E           K DISK
     F*
     F*** END ADD ***** 05/27/10 **************************************
     F*** BEG ADD ***** 12/02/10 **************************************
     FWTGTL     IF   E           K DISK
     F*** END ADD ***** 12/02/10 **************************************
      *
      ** *******  CLAIM DEPT LETTERS WORKFILE
     F** *******WDCLL3  O   E           K        DISK
     F** *******            PCL01                             KRENAMEPCL02
     F** *******WDCLL   UF  E           K        DISK                      A
     F** *******           PCL01                             KRENAMEPCL03
      *
      /COPY SCOPYMT01D
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D SS#             S              9    DIM(7)
     D EX              S              1    DIM(7)
      *
     D WK              S              2    DIM(10)
     D PT              S              2    DIM(10)                              WARPP RESV CD
     D PA              S              9  2 DIM(10)                              WARPP PMT AMT
     D MSG             S             70    DIM(4) CTDATA PERRCD(1)
     D RMK             S             49    DIM(4) CTDATA PERRCD(1)
     D*** BEGIN ADD *** 01/26/98 ***********************************************
     D*** END ADD ***** 01/26/98 ***********************************************
     D*** Begin add *** 03/23/04 **************************************
     D*** End add ***** 03/23/04 **************************************
     D*** Begin add *** 02/23/05 **************************************
     D LA              S              9  2 DIM(10)                              Large rsv amt
     D*** End add ***** 02/23/05 **************************************
     D*** BEG ADD ***** 05/27/10 **************************************
     D*** Begin Chg *** 01/16/19 **************************************
     D*WK2             S              3  0 DIM(9999)
     D*DA2             S              1  0 DIM(9999)
     D*AM2             S             11  2 DIM(9999)
     D*CDP             S              2    DIM(9999)
     D WK2             S              3  0 DIM(99999)
     D DA2             S              1  0 DIM(99999)
     D AM2             S             11  2 DIM(99999)
     D CDP             S              2    DIM(99999)
     D*** End   Chg *** 01/16/19 **************************************
      *
     D DBFDS           DS
     D  FLSTCM           *STATUS
     D  DBRRN                397    400B 0
     D*
     D*   Parameters for MC Claim Master API        QMxxxx
     D*
     D PARMCL          DS                  INZ
     D  QMRTCD                 1      1
     D  QMSS#                  2     10
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D*************************************  11  18 QMACDT
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMACDT                11     17
     D  FILL1                 18     18
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QMCO#                 19     21
     D  QMSUB#                22     26
     D  QMFND                 27     29
     D  QMFYR                 30     32
     D  QMCASE                33     39
     D  QMEMP#                40     48
     D  QMDIV                 49     53
     D  QMACTM                54     59
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D*************************************  60  67 QMDTRP
     D*************************************  68  75 QMDTLW
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMDTRP                60     66
     D  FILL2                 67     67
     D  QMDTLW                68     74
     D  FILL3                 75     75
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QMACDS                76    135
     D  QMMCON               136    136
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D************************************  137 144 QMENTD
     D************************************  145 152 QMRCVD
     D************************************  153 160 QMMMI
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMENTD               137    143
     D  FILL4                144    144
     D  QMRCVD               145    151
     D  FILL5                152    152
     D  QMMMI                153    159
     D  FILL6                160    160
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QM1A1                161    161
     D  QM10A1               162    171
     D  QMLTFL               172    172
     D  QMSCL#               173    197
     D  QMMCTM               198    201
     D  QMSTWK               202    203
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D************************************* 189 196 QMDEDT
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMDEDT               204    210
     D  FILL7                211    211
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QMDEEM               212    213
     D  QMCMNU               214    216
     D  QMCMSU               217    219
     D  QMCLAD               220    222
     D  QMRRN                223    231  0
     D  QMLSN                232    261
     D  QMFSN                262    276
     D  QMMID                277    277
     D*** BEGIN DLT *** 11/05/97 ***********************************************
     D************************************* 263 270 QMDTRE
     D*** END DLT ***** 11/05/97 ***********************************************
     D*** BEGIN ADD *** 11/05/97 ***********************************************
     D  QMDTRE               278    284
     D  FILL8                282    282
     D*** END ADD ***** 11/05/97 ***********************************************
     D  QMCLSU               285    287
     D  QM#DEP               288    290
      *
     D                 DS                  INZ
     D  AR                     1     90  2
     D                                     DIM(10)                              PMT AGAINST RR
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
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *  DARTS
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS                  INZ
     D  RT                     1     20
     D                                     DIM(10)
     D  SCX01                  1      2
     D  SCX02                  3      4
     D  SCX03                  5      6
     D  SCX04                  7      8
     D  SCX05                  9     10
     D  SCX06                 11     12
     D  SCX07                 13     14
     D  SCX08                 15     16
     D  SCX09                 17     18
     D  SCX10                 19     20
     D                 DS                  INZ
     D  TP                     1     10
     D                                     DIM(10)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D  SCTP07                 7      7
     D  SCTP08                 8      8
     D  SCTP09                 9      9
     D  SCTP10                10     10
     D                 DS                  INZ
     D  RR                     1     90  2
     D                                     DIM(10)
     D  SCRR01                 1      9  2
     D  SCRR02                10     18  2
     D  SCRR03                19     27  2
     D  SCRR04                28     36  2
     D  SCRR05                37     45  2
     D  SCRR06                46     54  2
     D  SCRR07                55     63  2
     D  SCRR08                64     72  2
     D  SCRR09                73     81  2
     D  SCRR10                82     90  2
     D                 DS                  INZ
     D  IR                     1     20
     D                                     DIM(10)
     D  SCRT01                 1      2
     D  SCRT02                 3      4
     D  SCRT03                 5      6
     D  SCRT04                 7      8
     D  SCRT05                 9     10
     D  SCRT06                11     12
     D  SCRT07                13     14
     D  SCRT08                15     16
     D  SCRT09                17     18
     D  SCRT10                19     20
     D                 DS                  INZ
     D  IS                     1     90  2
     D                                     DIM(10)
     D  SCSU01                 1      9  2
     D  SCSU02                10     18  2
     D  SCSU03                19     27  2
     D  SCSU04                28     36  2
     D  SCSU05                37     45  2
     D  SCSU06                46     54  2
     D  SCSU07                55     63  2
     D  SCSU08                64     72  2
     D  SCSU09                73     81  2
     D  SCSU10                82     90  2
      *
     D                 DS
     D  FRS                    1     20
     D                                     DIM(10)
     D  FNRS01                 1      2
     D  FNRS02                 3      4
     D  FNRS03                 5      6
     D  FNRS04                 7      8
     D  FNRS05                 9     10
     D  FNRS06                11     12
     D  FNRS07                13     14
     D  FNRS08                15     16
     D  FNRS09                17     18
     D  FNRS10                19     20
     D*
     D*** BEGIN ADD *** 01/26/98 ***********************************************
     D                 DS                  INZ
     D  RC                     1     90  2
     D                                     DIM(10)
     D  WKRC01                 1      9  2
     D  WKRC02                10     18  2
     D  WKRC03                19     27  2
     D  WKRC04                28     36  2
     D  WKRC05                37     45  2
     D  WKRC06                46     54  2
     D  WKRC07                55     63  2
     D  WKRC08                64     72  2
     D  WKRC09                73     81  2
     D  WKRC10                82     90  2
     D                 DS                  INZ
     D  TI                     1     90  2
     D                                     DIM(10)
     D  SCTI01                 1      9  2
     D  SCTI02                10     18  2
     D  SCTI03                19     27  2
     D  SCTI04                28     36  2
     D  SCTI05                37     45  2
     D  SCTI06                46     54  2
     D  SCTI07                55     63  2
     D  SCTI08                64     72  2
     D  SCTI09                73     81  2
     D  SCTI10                82     90  2
     D*** END ADD ***** 01/26/98 ***********************************************
     D*
     D*** Begin add *** 03/23/04 **************************************
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)                              RH rsv types
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
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)                              RH rsv amt
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
     D*** End add ***** 03/23/04 **************************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D OPTNM           C                   CONST('Claims, Reserve / -
     D                                     Payment Summary')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D*** BEGIN DLT *** 11/16/98 **************************************
     D************* 'FILE STATUS IS OUT, -C         XCBC01
     D************* 'USER - XXXXXXXXXX'
     D************* 'FILE STATUS IS IN'   C         XCBC02
     D*** END DLT ***** 11/16/98 **************************************
     D*** BEGIN ADD *** 11/16/98 **************************************
     D XCBC01          C                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D XCBC02          C                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D*** END ADD ***** 11/16/98 **************************************
     D*** BEG DLT ***** 10/26/07 **************************************
     D*****         1                     C         #OFSCR
     D*** END DLT ***** 10/26/07 **************************************
     D COMAPI          C                   CONST('WA001R')

     I*** BEG ADD ***** 05/27/10 **************************************
      *
     IDBRCDF
     I              CMSTAT                      C2STAT
     I              CMUSER                      C2USER
     I              CMIDT                       C2IDT
     I              CMITM                       C2ITM
     I*** BEGIN ADD *** 06/23/00 **************************************
     I              CMIUSR                      C2IUSR
     I              CMIPGM                      C2IPGM
     I*** END ADD ***** 06/23/00 **************************************
      *
     I***PY SCOPYMT01I
      * ================================================================
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *

     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
      *  CLAIM MASTER RECORD FORMAT PCM02
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *   KEY LIST FOR THE CLAIM/DARTS CONTROL FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  REMARK CODE TABLE FILE
      *
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD            5
      *
      *   CLAIMS/DARTS CONTROL KEY LIST
      *
     C     CXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
      *
      *  DARTS FILE
      *
     C     C4KY          KLIST
     C                   KFLD                    QCKEY
      *
      *  DIARY MASTER FILE
      *
     C     DDKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      ***** END ADD ******* 08/10/98 **********************************
      *
      *   KEY LIST FOR THE GROUP
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  CLAIM/EDI CONTROL FILE
      *
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XX148             3
      *
      *  MEDICAL ONLY DARTS DIARY SUSPENSE FILE
      *
     C     MDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C***** BEG ADD ******* 02/23/05 **********************************
     C*   KEY LIST - Auto DARTS Control File
     C*
     C     ADKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXKY
     C***** END ADD ******* 02/23/05 **********************************
      *
      *   KEY LIST FOR THE RESERVE CUSHION MASTER FILE
      *
     C     RCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRVCD
     C                   KFLD                    XXEFDT
      *
     C     RCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRVCD
      *
      *  REQUEST PAYMENT FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
     C*** BEG   ADD *** 03/29/10 **************************************
     C     RWKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*** END   ADD *** 03/29/10 **************************************
     C*** BEG   ADD *** 05/27/10 **************************************
     C*
     C     FNKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C*
     C*** END   ADD *** 05/27/10 **************************************
     C*** BEG   ADD *** 12/02/10 **************************************
      *  GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQTBID
     C                   KFLD                    QQKEY
     C*
     C*** END   ADD *** 12/02/10 **************************************
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMFYR         SCYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    CMCASE        SCCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXDT                           SYSTEM DT
     C     *LIKE         DEFINE    CMACDT        SVDATE                         CURRENT DATE
     C     *LIKE         DEFINE    CMACTM        SVTIME                         CURRENT TIME
     C     *LIKE         DEFINE    CMACDT        XXEFDT                         EFFECTIVE DT
     C     *LIKE         DEFINE    CMACDT        XXACDT                         ACC DT
     C     *LIKE         DEFINE    CMDTLW        XXHDLW
     C     *LIKE         DEFINE    CMRCVD        XXCDAT
     C     *LIKE         DEFINE    CMLTFL        XXLTFL
     C     *LIKE         DEFINE    UPUSNM        QCUSER
     C     *LIKE         DEFINE    UPRAL         XXRAL
     C     *LIKE         DEFINE    UPFYR         XUFYR
     C     *LIKE         DEFINE    CMAWW         SVAWW
     C     *LIKE         DEFINE    CMLTFL        BFLTFL
     C     *LIKE         DEFINE    C2STAT        BFSTAT
     C     *LIKE         DEFINE    RCRVCD        XXRVCD
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    CMEMP#        QQEMP#
     C     *LIKE         DEFINE    CMDIV         QQDIV
     C     *LIKE         DEFINE    CMSS#         QQSS#
     C     *LIKE         DEFINE    CMCO#         Q1CO#
     C     *LIKE         DEFINE    CMFND         Q1FND
     C     *LIKE         DEFINE    CMFYR         Q1FYR
     C     *LIKE         DEFINE    CMCASE        Q1CASE
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C     *LIKE         DEFINE    *IN60         XXAPIF                         CLAIM API FL
     C     *LIKE         DEFINE    *IN60         XXMANC                         MC INSTALLED
     C*** BEG ADD ***** 02/23/05 *************************************
     C     *LIKE         DEFINE    C2STAT        SVSTAT
     C*** BEG ADD ***** 06/23/11 *************************************
     C     *LIKE         DEFINE    C2STAT        SVOCST
     C*** END ADD ***** 06/23/11**************************************
     C     *LIKE         DEFINE    ADKEY         XXKY
     C*** END ADD ***** 02/23/05 *************************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CONT            1
      *
     C     ACDPL3        PLIST
     C                   PARM                    QQCALL            1
     C                   PARM                    QACO#             3
     C                   PARM                    QAFND             3
     C                   PARM                    QAFYR             3
     C                   PARM                    QACASE            7
      *** BEGIN DLT *** 07/13/98 **************************************
      *******************  PARM           QQAZIP  90
      *** END DLT ***** 07/13/98 **************************************
     C                   PARM                    QQRTCD            1
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJRST
     C                   PARM                    XX148             3
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
      *
     C/COPY SCOPYMT01C
      *
      *===============================================================
      * CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
      *===============================================================
      *
     C     CALLR         BEGSR
      *
     C     SCRN#         IFEQ      #OFSCR
      *
      * THIS IS TO MAKE SURE SCMODE EQUALS ADD IN ADD MODE
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDEQ     'A'
     C                   MOVE      'A'           @@MODE
     C                   END
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVEL     'A'           QQCALL
     C     @@MODE        WHENEQ    'C'
     C                   MOVEL     'Y'           QQCALL
     C     @@MODE        WHENEQ    'D'
     C                   MOVEL     'Z'           QQCALL
     C                   ENDSL
     C                   END
      *
     C     @@MODE        IFEQ      'A'
      *** BEG ADD *** 08/28/09 **************************************
     C     CLTID         ANDNE     '133'
      *** END ADD *** 08/28/09 **************************************
      *
     C                   SELECT
     C     CXSTSP        WHENEQ    'Y'
      *
     C                   MOVE      CMCO#         QACO#
     C                   MOVE      CMFND         QAFND
     C                   MOVE      CMFYR         QAFYR
     C                   MOVE      CMCASE        QACASE
      *** BEGIN DLT *** 07/13/98 **************************************
      ******************** MOVE CDZIP     QQAZIP
      *** END DLT ***** 07/13/98 **************************************
     C                   MOVE      *BLANK        QQRTCD
      *
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C***        *IN60     IFEQ *OFF
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C*** END CHG ****** 03/24/10 *************************************
     C     'W0070R'      CAT       IXFROI:0      XXPGM
     C                   ELSE
     C     'W0070R'      CAT       CMJRST:0      XXPGM
     C                   END
      *
     C*** BEGIN ADD *** 08/31/98 **************************************
     C     2             SUBST(P)  XXPGM:7       XXST              2
     C     XXST          IFNE      *BLANKS
     C*** END ADD ***** 08/31/98 **************************************
      *
     C                   CALL      XXPGM         ACDPL3                 99
      *
     C*** BEGIN ADD *** 08/31/98 **************************************
     C                   ELSE
     C                   MOVE      '1'           *IN99
     C                   ENDIF
     C*** END ADD ***** 08/31/98 **************************************
      *** BEGIN ADD *** 02/17/98 ***********************************************
     C     *IN99         IFEQ      '1'
     C     CXDBEN        CABEQ     'Y'           TDBEN
     C     CXDART        CABEQ     'Y'           TDART
     C                   END
      *** END ADD ***** 02/17/98 ***********************************************
      *** BEGIN ADD *** 02/03/98 ***********************************************
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *** END ADD ***** 02/03/98 ***********************************************
      *
     C     CXDBEN        WHENEQ    'Y'
      *** BEGIN ADD *** 02/17/98 ***********************************************
     C     TDBEN         TAG
      *** END ADD ***** 02/17/98 ***********************************************
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
      *
     C                   CALL      'W0190R'                             99
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACDT            8
     C                   PARM      '1'           QQRTCD
      *** BEGIN ADD *** 02/03/98 ***********************************************
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *** END ADD ***** 02/03/98 ***********************************************
      *
     C     CXDART        WHENEQ    'Y'
      *** BEGIN ADD *** 02/17/98 ***********************************************
     C     TDART         TAG
      *** END ADD ***** 02/17/98 ***********************************************
     C                   Z-ADD     CMCO#         QCCO#
     C                   Z-ADD     CMFND         QCFND
     C                   Z-ADD     CMFYR         QCFYR
     C                   Z-ADD     CMCASE        QCCASE
     C                   CALL      'SC010R'                             99
     C                   PARM      QCKEY         QAKEY            40
     C                   PARM      'W0070R'      QQPGM            10
     C                   ENDSL
     C                   END                                                    @@MODE=A
      ** **
     C     @@MODE        IFNE      'A'
     C     Q1PRMS        ANDGE     5
      *** BEG ADD *** 08/28/09 **************************************
     C     CLTID         ANDNE     '133'
      *** END ADD *** 08/28/09 **************************************
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QAFYR
     C                   MOVE      CMCASE        QACASE
      *** BEGIN DLT *** 07/13/98 **************************************
      ******************** MOVE CDZIP     QQAZIP
      *** END DLT ***** 07/13/98 **************************************
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     '148'         XX148
      *
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C***        *IN60     IFEQ *OFF
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C*** END CHG ****** 03/24/10 *************************************
     C     'W0070R'      CAT       IXFROI:0      XXPGM
     C                   ELSE
     C     'W0070R'      CAT       CMJRST:0      XXPGM
     C                   END
      *
     C*** BEGIN ADD *** 08/31/98 **************************************
     C     2             SUBST(P)  XXPGM:7       XXST              2
     C     XXST          IFNE      *BLANKS
     C*** END ADD ***** 08/31/98 **************************************
     C                   CALL      XXPGM                                99
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QAFYR
     C                   PARM                    QACASE
      *** BEGIN DLT *** 07/13/98 **************************************
      *******************  PARM           QQAZIP  90
      *** END DLT ***** 07/13/98 **************************************
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C*** BEGIN ADD *** 08/31/98 **************************************
     C                   ELSE
     C                   MOVE      '1'           *IN99
     C                   ENDIF
     C*** END ADD ***** 08/31/98 **************************************
      *
      *** BEGIN ADD *** 02/17/98 ***********************************************
     C     *IN99         IFEQ      '1'
      *
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
     C                   CALL      'W0190R'                             99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQACDT
     C                   PARM      '1'           QQRTCD
      *
     C                   END
      *** END ADD ***** 02/17/98 ***********************************************
      *
     C                   END
      *
      *** BEG ADD ***** 08/28/09 ***********************************************
     C     CLTID         IFEQ      '133'
      *
     C     @@MODE        IFEQ      'A'
      *
     C                   SELECT
     C     CXDBEN        WHENEQ    'Y'
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
      *
     C                   CALL      'W0190R'                             99
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACDT            8
     C                   PARM      '1'           QQRTCD
      *
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
      *
     C     CXDART        WHENEQ    'Y'
     C                   Z-ADD     CMCO#         QCCO#
     C                   Z-ADD     CMFND         QCFND
     C                   Z-ADD     CMFYR         QCFYR
     C                   Z-ADD     CMCASE        QCCASE
     C                   CALL      'SC010R'                             99
     C                   PARM      QCKEY         QAKEY            40
     C                   PARM      'W0070R'      QQPGM            10
     C                   ENDSL
     C                   ENDIF                                                  @MODE = A
      *
     C     @@MODE        IFNE      'A'
     C     Q1PRMS        ANDGE     5
      *
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
     C                   CALL      'W0190R'                             99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQACDT
     C                   PARM      '1'           QQRTCD
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
      *
     C                   ENDIF                                                  @MODE <> A
      *
     C                   ENDIF                                                  CLTID = 133
      *** END ADD ***** 08/28/09 ***********************************************
      *
      *
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'C'           @@MODE
     C                   END
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
      *
     C                   MOVE      *ON           *IN90
     C                   END
     C                   END
     C                   ENDSR                                                  CALLR
      *
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     C     DSPLYR        BEGSR
     C     1             SUBST(P)  Q2RECF:5      SCRN#A            1
     C                   MOVE      SCRN#A        SCRN#
      *
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
      *
     C                   ENDSR                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *          (Re-written on 1/26/98)
      *===============================================================
     C     ERRCK1        BEGSR
      *
      *  INDICATORS 71 - 91 ARE RESET IN COPYBOOK SCOPTMT01C.
      *
     C                   MOVEA     '00'          *IN(17)                        *IN17 - 18
     C                   MOVEA     '00000000'    *IN(20)                        *IN20 - 27
     C                   MOVEA     '00'          *IN(29)                        *IN29 - 30
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '0'           *IN(47)                        *IN47
     C                   MOVEA     '00'          *IN(53)                        *IN53 - 54
      *
     C                   CLEAR                   RC
     C                   CLEAR                   WK
      *** BEG ADD ***** 06/23/11 ***********************************************
      *
     C     CLTID         IFEQ      '133'
     C     SVOCST        IFNE      SCOCST
     C                   SETON                                        90
      *
     C     SCOCST        IFNE      'O'
     C     SCOCST        ANDNE     'C'
     C                   SETON                                        9042
     C                   GOTO      ENDEC1
     C                   ENDIF
     C*
     C                   CALL      'W0575R'                             99
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      XXCO#         Q1CO#
     C                   PARM      XXFND         Q1FND
     C                   PARM      XXEMP#        QQEMP#
     C                   PARM      XXDIV         QQDIV
     C                   PARM      CMSS#         QQSS#
     C                   PARM      XXFYR         Q1FYR
     C                   PARM      XXCASE        Q1CASE
     C                   PARM      *ZEROS        QQEFDT
     C                   PARM      'J'           QQRTCD
      *
     C                   MOVE      SCOCST        SVOCST
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN62
      *
     C                   GOTO      ENDCK1
      *
     C                   ENDIF
     C                   ENDIF
      *
      *** END ADD ***** 06/23/11 ***********************************************
      *
      *  RESERVES CANNOT BE CHANGED IF CLOSING CLAIM OR CLAIM ALREADY CLOSED.
      *
     C     SCOCST        IFEQ      'C'
      *
     C                   DO        10            X
      *
     C     IR(X)         IFNE      *BLANKS
     C                   SETON                                        9044
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDIF
      *
      *   RELOAD THE RESERVE, PAYMENT, RESERVE CODE, AND TOTAL
      *   INCURRED ARRAYS IN THE CASE THAT THERE HAS BEEN A CHANGE DUE
      *   TO A PMT OR RESERVE CHG BY ANOTHER USER.
      *
     C     CMKY          CHAIN     PCM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CMRR01        SCRR01                         REMAINING
     C                   MOVE      CMRR02        SCRR02                          RESERVES
     C                   MOVE      CMRR03        SCRR03
     C                   MOVE      CMRR04        SCRR04
     C                   MOVE      CMRR05        SCRR05
     C                   MOVE      CMRR06        SCRR06
     C                   MOVE      CMRR07        SCRR07
     C                   MOVE      CMRR08        SCRR08
     C                   MOVE      CMRR09        SCRR09
     C                   MOVE      CMRR10        SCRR10
     C                   XFOOT     RR            SCRRTO
     C                   MOVE      CMRT01        SCX01                          RESERVE TYPES
     C                   MOVE      CMRT02        SCX02
     C                   MOVE      CMRT03        SCX03
     C                   MOVE      CMRT04        SCX04
     C                   MOVE      CMRT05        SCX05
     C                   MOVE      CMRT06        SCX06
     C                   MOVE      CMRT07        SCX07
     C                   MOVE      CMRT08        SCX08
     C                   MOVE      CMRT09        SCX09
     C                   MOVE      CMRT10        SCX10
     C                   MOVE      CMAR01        SCAR01                         PAYMENTS
     C                   MOVE      CMAR02        SCAR02
     C                   MOVE      CMAR03        SCAR03
     C                   MOVE      CMAR04        SCAR04
     C                   MOVE      CMAR05        SCAR05
     C                   MOVE      CMAR06        SCAR06
     C                   MOVE      CMAR07        SCAR07
     C                   MOVE      CMAR08        SCAR08
     C                   MOVE      CMAR09        SCAR09
     C                   MOVE      CMAR10        SCAR10
     C     CMAR01        ADD       CMAR02        SCARTO
     C                   ADD       CMAR03        SCARTO
     C                   ADD       CMAR04        SCARTO
     C                   ADD       CMAR05        SCARTO
     C                   ADD       CMAR06        SCARTO
     C                   ADD       CMAR07        SCARTO
     C                   ADD       CMAR08        SCARTO
     C                   ADD       CMAR09        SCARTO
     C                   ADD       CMAR10        SCARTO
     C     CMRR01        ADD       CMAR01        SCTI01
     C     CMRR02        ADD       CMAR02        SCTI02
     C     CMRR03        ADD       CMAR03        SCTI03
     C     CMRR04        ADD       CMAR04        SCTI04
     C     CMRR05        ADD       CMAR05        SCTI05
     C     CMRR06        ADD       CMAR06        SCTI06
     C     CMRR07        ADD       CMAR07        SCTI07
     C     CMRR08        ADD       CMAR08        SCTI08
     C     CMRR09        ADD       CMAR09        SCTI09
     C     CMRR10        ADD       CMAR10        SCTI10
     C     SCTI01        ADD       SCTI02        SCTITO
     C                   ADD       SCTI03        SCTITO
     C                   ADD       SCTI04        SCTITO
     C                   ADD       SCTI05        SCTITO
     C                   ADD       SCTI06        SCTITO
     C                   ADD       SCTI07        SCTITO
     C                   ADD       SCTI08        SCTITO
     C                   ADD       SCTI09        SCTITO
     C                   ADD       SCTI10        SCTITO
      *
     C                   DO        10            I                 3 0
     C                   MOVEL(P)  RT(I)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C  N60              MOVEL     RTTYPE        TP(I)
     C   60              MOVE      *BLANK        TP(I)
     C                   ENDDO
      *
      *  IF ANOTHER USER CHANGED THE STATUS SINCE THIS SCREEN WAS
      *  ACCESSED, SEND AN ERROR MESSAGE.
      *
     C     C2STAT        IFNE      BFSTAT
     C                   SETON                                        9045
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  Edit reserve changes
      *
     C                   CLEAR                   WK
     C                   DO        10            I
      *
     C     30            ADD       I             X1                2 0
     C                   MOVE      *ON           *IN(X1)
      *
     C     IR(I)         IFNE      *BLANKS
     C     IS(I)         ORNE      *ZERO
      *
      *  Reserve code and amount are mutually inclusive.  If the 'Claim
      *    Reserve Change' control flag indicates that amount entered
      *    is the total incurred, the amount CAN be zero.
      *
      *
     C     IR(I)         IFNE      *BLANKS
     C     IS(I)         ANDEQ     *ZERO
     C     CXRCHG        ANDNE     'Y'
     C     IR(I)         OREQ      *BLANKS
     C     IS(I)         ANDNE     *ZERO
     C                   SETON                                        9021
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
      *  A reserve code cannot be used more than once.
      *
     C     IR(I)         LOOKUP    WK                                     26
     C     *IN26         IFEQ      *ON
     C                   SETON                                        90
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
      *  Validate reserve code
      *
     C                   MOVEL(P)  IR(I)         XXKEY
     C     TBKY          CHAIN     PRT01                              20
     C  N20RTSTAT        COMP      'I'                                    20
     C     *IN20         IFEQ      *ON
     C                   SETON                                        90
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
      *  Reserve code must be valid for Group
      *
     C     IR(I)         LOOKUP    FRS                                    61
     C     *IN61         IFEQ      *OFF
     C                   SETON                                        9029
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
      *  COMP RESERVES CAN ONLY BE ENTERED ON AN INDEMNITY CLAIM.
      *
     C     RTTYPE        IFEQ      'C'
     C     CMLTFL        ANDNE     'I'
     C                   SETON                                        9054
     C                   GOTO      ENDEC1
     C                   ENDIF
     C*** BEG ADD *** 12/12/01 *****************************
     C*** BEG CHG *** 05/23/02 *****************************
     C********** ELKY      CHAINPEL01                60
     C     ELKY          CHAIN(N)  PEL01                              60
     C*** END CHG *** 05/23/02 *****************************
      *
     C     ELEXMF        IFEQ      'Y'
     C     RTTYPE        ANDEQ     'M'
     C                   SETON                                        9056
     C                   GOTO      ENDEC1
     C                   ENDIF
     C*** END ADD *** 12/12/01 *****************************
      *
      *  If the entered reserve code does not already exist in the
      *    claim, verify that there are not already 10 reserve codes
      *    setup in the claim.
      *
     C                   Z-ADD     1             X                 3 0
     C     IR(I)         LOOKUP    RT(X)                                  60
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RT(X)                                  60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9023
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CXRCHG        IFEQ      'Y'
     C     IS(I)         SUB       TI(X)         RC(I)
     C                   ELSE
     C                   Z-ADD     IS(I)         RC(I)
     C                   ENDIF
     C     RR(X)         ADD       RC(I)         TEST              9 2
      *
      *  REMAINING RESERVES CANNOT BE LESS THAN 0
      *    (Array RC equals the change in remaining reserves.)
      *    (TEST equals the resulting remaining reserves.)
      *
     C     TEST          IFLT      *ZERO
     C                   SETON                                        9022
     C                   GOTO      ENDEC1
     C                   END
      *
      *  IF RSV CHANGE IS NEGATIVE MAKE SURE REMAINING RSV
      *  WILL BE ENOUGH TO COVER ANY REQUESTS FOR PAYMENTS
      *
     C     RC(I)         IFLT      *ZERO
      *
     C                   Z-ADD     0             XXRQPY            9 2
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
     C     RPSTAT        IFEQ      *BLANK
     C     RPRECD        ANDEQ     IR(I)
     C                   ADD       RPAMT         XXRQPY
     C                   END
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO
      *
     C     XXRQPY        IFGT      TEST
     C                   SETON                                        9025
     C                   GOTO      ENDEC1
     C                   END
      *
     C                   ENDIF                                                  RC,I  IFLT  0
      *
      *  Do not allow the user to exceed their reserve authority
      *    limit.
      *
     C     RC(I)         IFGT      *ZERO
      *
     C                   Z-ADD     *ZERO         QQLMT
     C                   CALL      'SRRSVL'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    SCYR
     C                   PARM                    SCCASE
     C                   PARM                    IR(I)
     C                   PARM                    QQLMT            11 2
      *
     C*** BEG ADD *** 06/07/05 ***************************
     C*
     C*** For cumulative edits consider previous screen amounts in
     C*** in the total.
     C*
     C     UPRAL         IFNE      *ZEROS
     C                   XFOOT     RC            CHKRC            11 2
     C     QQLMT         SUB       CHKRC         QQLMT
     C                   ADD       RC(I)         QQLMT
     C                   ENDIF
     C*
     C*** END ADD *** 06/07/05 ***************************
      *
     C     *IN52         IFEQ      '1'
     C     I             ANDEQ     1
     C     RC(I)         SUB       FNDFRA        XXWK11           11 2
     C                   ELSE
     C                   Z-ADD     RC(I)         XXWK11
     C                   ENDIF
      *
     C     XXWK11        IFGT      QQLMT
     C                   SETON                                        9024
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  If the 'Claim Reserve Change' control flag indicates that amount
      *    entered is the total incurred and the amount is zero, display
      *    a warning window indicating that the total incurred will be
      *    reduced to zero.
      *
      *
     C     CXRCHG        IFEQ      'Y'
     C     IR(I)         ANDNE     *BLANKS
     C     IS(I)         ANDEQ     *ZERO
      *
     C                   MOVE      IR(I)         SCRECD
     C                   EXFMT     WDW02
     C     *INKL         IFEQ      *ON
     C                   SETON                                        9028
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C                   ENDIF
      *
      *
     C                   ENDIF                                                  IR,I <> *BLANK OR
      *
      *  Reset control indicator.
      *
     C                   MOVE      *OFF          *IN(X1)
     C                   MOVE      IR(I)         WK(I)
      *                                                      IS,I <> *ZERO
      *  Get the next reserve change.
      *
     C                   ENDDO                                                  DO  10    I
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  Total reserve change may not exceed the user's reserve
      *    authority limit.
      *
     C*** BEGIN CHG *** 10/26/99 **************************************
     C******************** XFOOTIS        TEST
     C                   XFOOT     RC            TEST
     C*** END CHG ***** 10/26/99 **************************************
     C     TEST          IFGT      XXRAL
     C     XXRAL         ANDNE     0                                            ADD 04/01/03
     C                   MOVE      '1'           *IN24
     C                   MOVEA     '1111111'     *IN(31)
     C                   MOVEA     '111'         *IN(38)
     C                   SETON                                        90
     C                   GOTO      ENDEC1
     C                   END
      *
      *  If the reserves are changed, an explanation is required.
      *
     C     WKRC01        IFNE      0
     C     WKRC02        ORNE      0
     C     WKRC03        ORNE      0
     C     WKRC04        ORNE      0
     C     WKRC05        ORNE      0
     C     WKRC06        ORNE      0
     C     WKRC07        ORNE      0
     C     WKRC08        ORNE      0
     C     WKRC09        ORNE      0
     C     WKRC10        ORNE      0
      *
     C     SCEXRS        IFEQ      *BLANKS
     C                   SETON                                        9030
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Validate the claim status.
      *
     C     SCOCST        IFNE      'O'
     C     SCOCST        ANDNE     'C'
     C                   SETON                                        9042
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C*** BEG  ADD***** 12/02/10 **************************************
     C     SCEXCD        IFNE      *BLANKS
     C                   MOVE      'OCST'        QQTBID
     C                   MOVEL     SCEXCD        QQKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9068
     C                   ENDIF
     C                   ENDIF
      *
      *  If the claim status changes, an explanation is required.
      *
     C     CXOPCL        IFEQ      'Y'
      *
     C     SCOCST        IFNE      BFSTAT
     C     SCEXCD        IFEQ      *BLANKS
     C                   SETON                                        9068
     C                   GOTO      ENDEC1
     C                   ENDIF
     C     GTDESC        IFEQ      *BLANKS
     C                   SETON                                        9046
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C*** END  ADD***** 12/02/10 **************************************
      *  If the claim status changes, an explanation is required.
      *
     C     SCOCST        IFNE      BFSTAT
      *
     C     SCEXPL        IFEQ      *BLANKS
     C                   SETON                                        9043
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  If the claim is being closed, process various edits.
      *
B1   C     SCOCST        IFEQ      'C'
     C     BFSTAT        ANDEQ     'O'
      *
      *  Read the Request Payment file.
      *
     C                   CLEAR                   PT
     C                   CLEAR                   PA
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
      *
      *  Send an error msg if any pended pmts exist and the 'Allow claim
      *    closing for pended pmts' flag (WMCXP/CXACCP) = N.
      *
     C     RPSTAT        IFEQ      'P'
     C     CXACCP        ANDNE     'Y'
     C                   MOVE      '1'           *IN47
     C                   SETON                                        90
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
      *  Accumulate released pmts by reserve code.
      *
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    PT(X)                                  61
     C     *IN61         IFEQ      '1'
     C                   ADD       RPAMT         PA(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    PT(X)                                  61
     C     *IN61         IFEQ      '1'
     C                   MOVE      RPRECD        PT(X)
     C                   Z-ADD     RPAMT         PA(X)
     C                   END                                                    LOKUP BLANKS
     C                   END                                                    LOKUP RPRECD
     C                   END
      *
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO
      *
      *  The claim cannot be closed if the amt of pmt requests
      *    exceed the reserve cushion setup.
      *
     C                   DO        10            X
      *
     C     PT(X)         IFNE      *BLANKS
      *
     C                   MOVE      PT(X)         XXRVCD
     C     RCKY          SETGT     PRC01
     C     RCKY1         READPE    PRC01                                  60
      *** BEGIN DLT *** 06/24/98 **************************************
      ********** *IN60     IFEQ '0'
      *** END DLT ***** 06/24/98 **************************************
      *
     C     CMLTFL        IFEQ      'I'
     C     PA(X)         ANDGT     RCLAMT                                       LOST TIME AMT
     C     CMLTFL        OREQ      'M'
     C     PA(X)         ANDGT     RCMAMT                                       MED ONLY AMT
      *** BEGIN ADD *** 06/24/98 **************************************
     C     *IN60         OREQ      '1'
      *** END ADD ***** 06/24/98 **************************************
     C                   MOVE      '1'           *IN17
     C                   SETON                                        90
     C                   GOTO      ENDEC1
     C                   ENDIF
      *
      *** BEGIN DLT *** 06/24/98 **************************************
      ******************** ENDIF                           *IN60
      *** END DLT ***** 06/24/98 **************************************
      *
     C                   ENDIF                                                  PT,X  NE  *blanks
      *
     C                   ENDDO                                                  DO X
     C*** Begin Add *** 09/11/09 **************************************
     C*
     C* Do not allow an indemnity claim to be closed if no comp paymnts
     C*
     C     CLTID         IFEQ      '133'
     C*
     C     CMLTFL        IFEQ      'I'
     C     *IN90         ANDEQ     *OFF
      ***** BEG ADD ******* 03/23/10 **********************************
     C     SCOCST        ANDEQ     'C'
      ***** END ADD ******* 03/23/10 **********************************
     C***** BEG ADD ******* 03/23/11 **********************************
     C*
     C*  If closing LT claim, reminder to stop payment.
     C                   DO
     C                   EXFMT     WDW05
     C*
     C     *INKG         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF                                                  e- INKG = ON
     C*
     C                   SETON                                        90
     C*
     C                   ENDDO                                                  e- DO
     C***** END ADD ******* 03/23/11 **********************************
     C*
     C                   CALL      'WA723R'
     C                   PARM      CMCO#         $$CO#             3 0
     C                   PARM      CMFND         $$FND             3 0
     C                   PARM      CMFYR         $$FYR             3 0
     C                   PARM      CMCASE        $$CASE            7 0
     C                   PARM      *BLANKS       $$RTCD            1
      *
     C     $$RTCD        IFEQ      'N'
      ***** BEG DEL ******* 03/23/10 **********************************
     C*********            SETON                     7190
     C*********            GOTO ENDEC1
      ***** END DEL ******* 03/23/10 **********************************
      ***** BEG ADD ******* 03/23/10 **********************************
     C                   MOVE      ' '           SCYORN
      *
     C     SCYORN        DOUEQ     'Y'
     C     SCYORN        OREQ      'N'
     C     *INKL         OREQ      *ON
     C                   EXFMT     WDW03
     C                   ENDDO
      *
     C                   SELECT
      *
     C     SCYORN        WHENEQ    'N'
     C                   SETON                                          90
     C                   GOTO      ENDEC1
      *
     C     SCYORN        WHENEQ    'Y'
     C                   SETOFF                                         90
      *
     C     *INKL         WHENEQ    *ON
     C                   MOVE      *OFF          *INKL
     C                   SETON                                          90
     C                   GOTO      ENDEC1
      *
     C                   ENDSL
      *
      ***** END ADD ******* 03/23/10 **********************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** End   Add *** 09/11/09 **************************************
     C*** BEG ADD *** 11/09/10 ****************************************
     C                   MOVE      *BLANKS       XXMDBN
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN(N)  PCD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CDMDBN        XXMDBN            1
     C                   ENDIF
     C     CLTID         IFEQ      '133'
     C     XXMDBN        ANDEQ     'Y'
     C                   CALL      'WA117R'
     C                   PARM      'R'           @@CALL            1
     C                   PARM                    @@KL              1
     C                   PARM                    @@YES             1
     C     @@YES         IFEQ      ' '
     C     @@KL          OREQ      '1'
     C                   SETON                                        9055
     C                   GOTO      ENDEC1
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 11/09/10 ****************************************
      *
      *  If active DARTS Diaries exist, display an informational
      *    window to inform the user and allow them to continue
      *    procesing the claim closing.
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
      *
     C     C4KY          CHAIN     PCM04                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     CMDCDT        IFEQ      0
     C     CMDIFL        ANDEQ     'Y'
      *
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *ON
     C                   SETON                                        9055
     C                   GOTO      ENDEC1
     C                   END
      *
     C                   LEAVE
     C                   ENDIF
      *
     C     C4KY          READE     PCM04                                  60
     C                   ENDDO
      *
      *
E1   C                   ENDIF                                                  SCOCST = 'C' and
      *                                                      BFSTAT = 'O'
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *** BEGIN ADD *** 08/04/98 **************************************
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *** END ADD ***** 08/04/98 **************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C     ENDEC1        TAG
      *
     C                   Z-ADD     1             SCRN#             1 0
      *** BEG ADD ***** 06/23/11 **************************************
     C     ENDCK1        TAG
      *** END ADD ***** 06/23/11 **************************************
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
     C***** BEG ADD ******* 10/26/07 ***********************************
      *
     C     SMEFDT        IFNE      *ZEROS
     C                   Z-ADD     SMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL(P)  SRDTO         SMEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN12
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDEC2
     C                   ENDIF
     C                   ENDIF
      *
     C     SMEFDT        IFEQ      *ZEROS
     C     SCCSTF        ANDNE     *ZEROS
     C                   SETON                                        9048
     C                   SETON                                        9049
     C                   GOTO      ENDEC2
     C                   ENDIF
      *
     C     SMEFDT        IFEQ      *ZEROS
     C     SCMSTF        ANDNE     *ZEROS
     C                   SETON                                        9048
     C                   SETON                                        9049
     C                   GOTO      ENDEC2
     C                   ENDIF
     C***** END ADD ******* 10/26/07 **********************************
      *
      *** BEGIN ADD *** 08/04/98 **************************************
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *** END ADD ***** 08/04/98 **************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C***** BEG ADD ******* 10/26/07 **********************************
     C     ENDEC2        TAG
     C*** END CHG ***** 10/26/07 **************************************
      *
     C                   Z-ADD     2             SCRN#
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     C     ERRCK3        BEGSR
      *
      *** BEGIN ADD *** 08/04/98 **************************************
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C  N99SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *** END ADD ***** 08/04/98 **************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     3             SCRN#
     C                   ENDSR                                                  ERRCK3
      *
      *===============================================================
      * KGDO - SUBROUTINE FOR F7
      *===============================================================
      *
      *  F7=CLAIMANT FINANCIAL INFO WINDOW
      *
     C     KGDO          BEGSR
      *
     C                   CALL      'WA009R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   END
      *
     C                   ENDSR
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=TABLE SEARCH
      *
     C     KJDO          BEGSR
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQCALL            1
      *
      *  RESERVE CODES
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCRT01'
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT01
      *
     C                   MOVE      *ON           *IN31
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      ** **
     C     HFLD          WHENEQ    'SCRT02'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT02
      *
     C                   MOVE      *ON           *IN32
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      ** **
     C     HFLD          WHENEQ    'SCRT03'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT03
      *
     C                   MOVE      *ON           *IN33
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      ** **
     C     HFLD          WHENEQ    'SCRT04'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT04
      *
     C                   MOVE      *ON           *IN34
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      ** **
     C     HFLD          WHENEQ    'SCRT05'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT05
      *
     C                   MOVE      *ON           *IN35
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      *****
     C     HFLD          WHENEQ    'SCRT06'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT06
      *
     C                   MOVE      *ON           *IN36
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      *****
     C     HFLD          WHENEQ    'SCRT07'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT07
      *
     C                   MOVE      *ON           *IN37
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      *****
     C     HFLD          WHENEQ    'SCRT08'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT08
      *
     C                   MOVE      *ON           *IN38
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      *****
     C     HFLD          WHENEQ    'SCRT09'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT09
      *
     C                   MOVE      *ON           *IN39
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      ** **
     C     HFLD          WHENEQ    'SCRT10'
     C                   MOVE      *BLANK        SCKEY
     C                   CALL      'W2136R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCRT10
      *
     C                   MOVE      *ON           *IN40
      *
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN27
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28
      *
     C                   ENDIF
      *
     C*** BEG  ADD***** 12/02/10 **************************************
     C     HFLD          WHENEQ    'SCEXCD'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      *BLANKS       QQCALL
     C                   PARM      'OCST'        QQTBID            4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM      *BLANK        QQRTCD            1
      *
     C     *INKL         IFEQ      *ON
     C     SCEXCD        OREQ      *BLANKS
      *
     C                   MOVE      *ON           *IN62
     C                   ENDIF
      *
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL(P)  QQKEY         SCEXCD
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL     GTDESC        SCEXDS
     C                   MOVEL     SCEXDS        SCEXPL
     C                   ENDIF                                                  QQKEY
      *
     C*** END  ADD***** 12/02/10 **************************************
     C                   OTHER
     C                   MOVE      *ON           *IN16
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      *===============================================================
      * KNDO - SUBROUTINE FOR F14 = DARTS
      *===============================================================
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      *
      *===============================================================
      * KVDO - SUBROUTINE FOR F21 = RESERVE HISTORY SEARCH
      *===============================================================
      *
     C     KVDO          BEGSR
      *
     C                   CALL      'W0574R1'                            99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      XXCO#         Q1CO#
     C                   PARM      XXFND         Q1FND
     C                   PARM      CMSS#         QQSS#
     C                   PARM      XXFYR         Q1FYR
     C                   PARM      XXCASE        Q1CASE
     C                   PARM      *ZEROS        QQEFDT            8 0
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  QQRTCD
      *
     C                   ENDSR                                                  KVDO
      *
      *===============================================================
      * KWDO - SUBROUTINE FOR F22 = STATUS HISTORY SEARCH
      *===============================================================
      *
     C     KWDO          BEGSR
      *
     C                   CALL      'W0575R'                             99
     C                   PARM                    QQPRDS
     C*** BEGIN CHG *** 04/14/00 **************************************
     C******************** PARM           QQCALL
     C                   PARM      '2'           QQCALL
     C*** END CHG ***** 04/14/00 **************************************
     C                   PARM      XXCO#         Q1CO#
     C                   PARM      XXFND         Q1FND
     C                   PARM      XXEMP#        QQEMP#
     C                   PARM      XXDIV         QQDIV
     C                   PARM      CMSS#         QQSS#
     C                   PARM      XXFYR         Q1FYR
     C                   PARM      XXCASE        Q1CASE
     C                   PARM      *ZEROS        QQEFDT
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  QQRTCD
      *
     C                   ENDSR                                                  KWDO
      *
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
     C     *IN98         IFEQ      *ON
     C                   MOVEL     MODEA         SCMODE                         ADD MODE
     C                   END
      *
      *  LOAD KEYS
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C                   Z-ADD     CMDIV         SVDIV
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        XXCASE
     C                   Z-ADD     CMCASE        SVCASE
     C                   Z-ADD     CMTIME        SVTIME
     C                   Z-ADD     CMDT          SVDATE
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     '148'         XX148
     C***** BEG ADD ******* 10/26/07 **********************************
     C                   Z-ADD     CMCSTF        SCCSTF
     C                   Z-ADD     CMMSTF        SCMSTF
     C                   Z-ADD     CMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SMEFDT
     C***** END ADD ******* 10/26/07 **********************************
     C*
     C***** BEG ADD ******* 02/23/05 **********************************
     C*    Auto DARTS
     C*   Load Reserve Large Amounts to array LA by reserve code.
     C***** BEG DEL ******* 06/07/10 **********************************
     C*****                MOVEL'LARGERSV'XXKY
     C*****      ADKY      CHAINPAD01                60
     C*****      *IN60     DOWEQ*OFF
     C*****                MOVE *OFF      *IN93
     C*****                Z-ADD1         R       30
     C*****      ADRECD    LOKUPRT,R                     93
     C*****      *IN93     IFEQ *OFF
     C*****                Z-ADDADAMT     LA,R
     C*****                ENDIF
     C*****      ADKY      READEPAD01                    60
     C*****                ENDDO
     C*****
     C***** END DEL ******* 06/07/10 **********************************
     C***** END ADD ******* 02/23/05 **********************************
     C***** BEG ADD ******* 05/27/10 **********************************
     C*    Read payments.
     C*
     C                   EXSR      SUMCLP
     C                   EXSR      FUNRET
     C*
     C*    Auto DARTS
     C*    Based on % of Specific Retention of total incurred on the
     C*    claim
     C*    Get 'CLMRETEN' control record(s).
     C*
     C                   MOVEL(P)  'CLMRETEN'    XXKY
     C     ADKY          CHAIN     PAD01                              51
     C*
     C                   MOVE      *ZEROS        WK#RET            5 2             -RSV+RMK
     C*
     C     *IN51         DOWEQ     *OFF
     C     ADSTAT        IFNE      'I'
     C                   MOVE      ADGRET        WK#RET                            -RSV+RMK
     C     RETPR#        IFGE      WK#RET
     C                   MOVEL(P)  'CLMRETEN'    XXKY
     C*
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      XXKY          @@DKEY           10
     C                   PARM      *BLANKS       @@COMM          256
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     ADKY          READE     PAD01                                  51     Next record
     C*
     C                   ENDDO                                                  e 51 DOW=OFF
     C*
     C***** END ADD ******* 05/27/10 **********************************
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C                   MOVE      *BLANK        SCMSG1
     C                   MOVE      *OFF          *IN69
     C     CMEXCS        IFNE      'Y'
     C     @@RRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    @@RRN                          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
      *
      *  GET THE DARTS CLAIM CONTROL RECORD
      *
     C     CXKY          CHAIN     PCX01                              60
      *
      * GET SYSTEM CONTROL FILE
      *
     C                   MOVEL(P)  'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
      *
      * BAR CODE INSTALLED
      *
     C     SYBCIF        IFEQ      'Y'
     C*** BEGIN DLT *** 11/16/98 **************************************
     C******************** CALL 'WA027R'
     C******************** PARM           @@RRN   90
     C*** END DLT ***** 11/16/98 **************************************
     C*** BEGIN ADD *** 11/16/98 **************************************
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'BA001R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C*** END ADD ***** 11/16/98 **************************************
     C                   PARM                    QQBCFL            1
     C                   PARM                    QQTRTP            1
     C                   PARM                    QQTUSR           10
     C                   PARM                    QQRTCD            1
      *
     C     QQBCFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN70
     C     QQTRTP        IFEQ      'I'
     C                   MOVEL(P)  XCBC02        SCMSG2
     C                   ELSE
     C                   MOVEL(P)  XCBC01        SCMSG2
     C*** BEGIN CHG *** 11/16/98 **************************************
     C******************** MOVE QQTUSR    SCMSG2
     C                   CAT       QQTUSR:1      SCMSG2
     C*** END CHG ***** 11/16/98 **************************************
     C                   END
     C                   END
      *
     C                   END                                                        SYBCIF = Y
      *
     C                   END                                                        *IN60
      *
     C                   Z-ADD     CMFND         SCFND
     C                   MOVE      *BLANK        SCFNAM
      *
      *  GET FUND NAME & IF FUND IS EXPIRED, THEN DISPLAY IN INQUIRY MODE
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCFNAM
     C     FNEXDT        IFGT      *ZERO
     C     FNEXDT        IFLE      XXDT
     C                   MOVE      *ON           *IN09
     C                   MOVE      'D'           @@MODE
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
     C                   END
     C                   END
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   END                                                    WMEMP
      *
      *  CLAIMANT SS#
      *
     C     CMSS#         IFNE      *BLANK
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYP            25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYP            25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
      *  CLAIMANT NAME
      *
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
     C                   END                                                    *IN60
     C                   END                                                    CMSS# NE BLANK
      *  ACCIDENT DATE
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   END
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
      *  FYR-CASE
     C                   MOVE      CMFYR         SCYR
     C                   MOVE      CMCASE        SCCASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   END
      *
      *  AGENGY CLAIM# (OR, SATE CLAIM#)
      *
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SCREEN 1
      *
      *  BLANK RSV TYPE AND CHANGES
      *
     C                   MOVE      *BLANKS       SCRT01                         RSV CODE
     C                   MOVE      *BLANKS       SCRT02
     C                   MOVE      *BLANKS       SCRT03
     C                   MOVE      *BLANKS       SCRT04
     C                   MOVE      *BLANKS       SCRT05
     C                   MOVE      *BLANKS       SCRT06
     C                   MOVE      *BLANKS       SCRT07
     C                   MOVE      *BLANKS       SCRT08
     C                   MOVE      *BLANKS       SCRT09
     C                   MOVE      *BLANKS       SCRT10
     C                   Z-ADD     *ZERO         SCSU01                         RSV AMOUNT
     C                   Z-ADD     *ZERO         SCSU02
     C                   Z-ADD     *ZERO         SCSU03
     C                   Z-ADD     *ZERO         SCSU04
     C                   Z-ADD     *ZERO         SCSU05
     C                   Z-ADD     *ZERO         SCSU06
     C                   Z-ADD     *ZERO         SCSU07
     C                   Z-ADD     *ZERO         SCSU08
     C                   Z-ADD     *ZERO         SCSU09
     C                   Z-ADD     *ZERO         SCSU10
      *
      *  LOAD THE RESERVE, PAYMENT, RESERVE CODE, AND TOTAL INCURRED
      *
     C                   MOVE      CMRR01        SCRR01                         REMAINING
     C                   MOVE      CMRR02        SCRR02                          RESERVES
     C                   MOVE      CMRR03        SCRR03
     C                   MOVE      CMRR04        SCRR04
     C                   MOVE      CMRR05        SCRR05
     C                   MOVE      CMRR06        SCRR06
     C                   MOVE      CMRR07        SCRR07
     C                   MOVE      CMRR08        SCRR08
     C                   MOVE      CMRR09        SCRR09
     C                   MOVE      CMRR10        SCRR10
     C                   XFOOT     RR            SCRRTO                         TOTAL
      *
     C                   MOVE      CMRT01        SCX01                          RESERVE TYPE
     C                   MOVE      CMRT02        SCX02
     C                   MOVE      CMRT03        SCX03
     C                   MOVE      CMRT04        SCX04
     C                   MOVE      CMRT05        SCX05
     C                   MOVE      CMRT06        SCX06
     C                   MOVE      CMRT07        SCX07
     C                   MOVE      CMRT08        SCX08
     C                   MOVE      CMRT09        SCX09
     C                   MOVE      CMRT10        SCX10
      *
     C                   MOVE      CMAR01        SCAR01                         PAYMENTS
     C                   MOVE      CMAR02        SCAR02
     C                   MOVE      CMAR03        SCAR03
     C                   MOVE      CMAR04        SCAR04
     C                   MOVE      CMAR05        SCAR05
     C                   MOVE      CMAR06        SCAR06
     C                   MOVE      CMAR07        SCAR07
     C                   MOVE      CMAR08        SCAR08
     C                   MOVE      CMAR09        SCAR09
     C                   MOVE      CMAR10        SCAR10
     C     CMAR01        ADD       CMAR02        SCARTO
     C                   ADD       CMAR03        SCARTO
     C                   ADD       CMAR04        SCARTO
     C                   ADD       CMAR05        SCARTO
     C                   ADD       CMAR06        SCARTO
     C                   ADD       CMAR07        SCARTO
     C                   ADD       CMAR08        SCARTO
     C                   ADD       CMAR09        SCARTO
     C                   ADD       CMAR10        SCARTO                         TOTAL PMTS
      *
     C     CMRR01        ADD       CMAR01        SCTI01                         TOTAL INCURRED
     C     CMRR02        ADD       CMAR02        SCTI02
     C     CMRR03        ADD       CMAR03        SCTI03
     C     CMRR04        ADD       CMAR04        SCTI04
     C     CMRR05        ADD       CMAR05        SCTI05
     C     CMRR06        ADD       CMAR06        SCTI06
     C     CMRR07        ADD       CMAR07        SCTI07
     C     CMRR08        ADD       CMAR08        SCTI08
     C     CMRR09        ADD       CMAR09        SCTI09
     C     CMRR10        ADD       CMAR10        SCTI10
     C     SCTI01        ADD       SCTI02        SCTITO
     C                   ADD       SCTI03        SCTITO
     C                   ADD       SCTI04        SCTITO
     C                   ADD       SCTI05        SCTITO
     C                   ADD       SCTI06        SCTITO
     C                   ADD       SCTI07        SCTITO
     C                   ADD       SCTI08        SCTITO
     C                   ADD       SCTI09        SCTITO
     C                   ADD       SCTI10        SCTITO
      *
     C                   MOVE      'N'           XXCMP             1
     C                   Z-ADD     1             I
      *
     C     I             DOUGT     10
     C                   MOVEL(P)  RT(I)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C  N60              MOVEL     RTTYPE        TP(I)
     C   60              MOVE      *BLANK        TP(I)
     C     TP(I)         IFEQ      'C'
     C     AR(I)         ANDGT     0
     C     TP(I)         OREQ      'C'
     C     RR(I)         ANDGT     0
     C                   MOVE      'Y'           XXCMP
     C                   END
     C                   ADD       1             I
     C                   ENDDO
      *
     C     C2STAT        IFEQ      *BLANK                                       NEW
     C                   MOVEL     'O'           SCOCST
     C                   MOVE      *BLANK        BFSTAT
     C                   MOVEL     MSG(2)        SCEXPL                         ORIG OPEN
     C                   MOVE      *BLANK        SCEXRS
     C                   MOVE      *BLANK        BFLTFL
     C                   ELSE                                                   EXISTING
     C                   MOVEL     C2STAT        SCOCST
     C                   MOVEL     C2STAT        BFSTAT
     C*** BEG ADD ***** 06/23/11 *************************************
     C                   MOVEL     C2STAT        SVOCST
     C*** END ADD ***** 06/23/11 *************************************
     C                   MOVE      *BLANK        SCEXPL
     C*** BEG ADD ***** 12/02/10 *************************************
     C                   MOVE      *BLANK        SCEXDS
     C                   MOVE      *BLANK        SCEXCD
     C*** END ADD ***** 12/02/10 *************************************
     C                   MOVE      *BLANK        SCEXRS
     C                   MOVEL     CMLTFL        BFLTFL
     C                   END
     C*** BEG ADD ***** 02/23/05 *************************************
     C*    Save OC status to see if updated.
     C                   MOVEL     C2STAT        SVSTAT
     C*** END ADD ***** 02/23/05 *************************************
      *
      *  SETON INDICATOR SO RESERVES CANNOT BE ENTERED IF THE CLAIM IS
      *  'RECORD ONLY' TYPE.
      *
     C                   MOVE      *OFF          *IN59
     C     CMLTFL        IFEQ      'R'
     C                   MOVE      *ON           *IN59
     C                   END
      *
      *  IF THE CLAIM IS NOT 'RECORD ONLY' AND NO RESERVES HAVE BEEN
      *  ENTERED, LOAD 'ORIGINAL RESERVES' TO EXPLANATION.
      *
      *
     C     SCX01         IFEQ      *BLANKS
     C     CMLTFL        ANDNE     'R'
     C                   MOVEL     MSG(1)        SCEXRS
     C                   END
      *
      *** BEGIN DLT *** 04/22/98 ***********************************************
      *  IF THE CLAIM IS A 'MED ONLY' TYPE AND NO RESERVES HAVE BEEN
      *  ENTERED, LOAD THE MEDICAL RESERVE DEFAULT CODE AND AMOUNT.
      **********
      **********           MOVE '0'       *IN52
      ********** SCX01     IFEQ *BLANKS
      ********** CMLTFL    ANDEQ'M'
      **********           MOVE FNDFRC    IR,1
      **********           MOVE FNDFRA    IS,1
      ********** FNDFRA    IFNE 0
      **********           SETON                     2732
      **********           MOVE *ON       *IN52
      **********           END
      **********           END
      *** END DLT ***** 04/22/98 ***********************************************
      *
      *
     C                   MOVE      *ON           *IN19                          DSPATR(PC)
     C***** BEG ADD ******* 10/26/07 **********************************
     C     CMJRST        IFNE      'GA'
     C                   Z-ADD     1             #OFSCR            1 0
     C                   ELSE
     C                   Z-ADD     2             #OFSCR
     C                   ENDIF
     C***** END ADD ******* 10/26/07 **********************************
     C                   ENDSR                                                  MVDBSC
      *
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
      *
      *    Set flag to update MC if SCL# has changed.
      *
     C                   MOVE      *OFF          XXAPIF
     C     XXMANC        IFEQ      *ON
     C     SCSCL#        IFNE      CMACL#
     C                   MOVE      *ON           XXAPIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL     SCSCL#        CMACL#
      *
      *  If the claim is being closed, reduce remaining reserves to 0.
      *
     C     SCOCST        IFEQ      'C'
     C     BFSTAT        ANDEQ     'O'
      *
     C                   MOVEL     MSG(3)        SCEXRS
     C                   DO        10            X
     C     RT(X)         IFNE      *BLANKS
     C                   MOVE      RT(X)         IR(X)
     C                   Z-SUB     RR(X)         RC(X)
     C                   END
     C                   ENDDO
      *
      *  ADD THE AMOUNT OF ANY RELEASED PAYMENTS BACK INTO THE REMAINING
      *  RESERVES SO THE PAYMENTS CAN BE PROCESSED.
      *
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     RPSTAT        IFEQ      ' '
      *
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    IR(X)                                  61
     C     *IN61         IFEQ      *ON
     C                   ADD       RPAMT         RC(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    IR(X)                                  61
     C   60              MOVE      RPRECD        IR(X)
     C   60              Z-ADD     RPAMT         RC(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                   SCOCST = 'C' and
      *                                                       BFSTAT = 'O'
      *
      *   If in update mode, reload the remaining reserve array in
      *     case there has been a change due to a pmt or reserve chg
      *     by another user.
      *
     C     *IN98         IFEQ      '0'
     C                   MOVE      CMRR01        SCRR01
     C                   MOVE      CMRR02        SCRR02
     C                   MOVE      CMRR03        SCRR03
     C                   MOVE      CMRR04        SCRR04
     C                   MOVE      CMRR05        SCRR05
     C                   MOVE      CMRR06        SCRR06
     C                   MOVE      CMRR07        SCRR07
     C                   MOVE      CMRR08        SCRR08
     C                   MOVE      CMRR09        SCRR09
     C                   MOVE      CMRR10        SCRR10
     C                   END
      *
      *  LOOP THRU CODES
      *
     C                   DO        10            I
     C     IR(I)         IFNE      *BLANKS
      *
     C                   Z-ADD     1             X
     C     IR(I)         LOOKUP    RT(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   ADD       RC(I)         RR(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RT(X)                                  60
     C   60              MOVE      IR(I)         RT(X)
     C   60              ADD       RC(I)         RR(X)
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDDO
      *
     C                   MOVE      SCOCST        C2STAT
     C                   MOVE      Q1USER        C2USER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CMDT
     C                   Z-ADD     DSTIME        CMTIME
      *** BEGIN DLT *** 04/24/98 ***********************************************
      ********** *IN98     IFEQ '1'
      **********           Z-ADD0         CMAR01
      **********           Z-ADD0         CMAR02
      **********           Z-ADD0         CMAR03
      **********           Z-ADD0         CMAR04
      **********           Z-ADD0         CMAR05
      **********           Z-ADD0         CMAR06
      **********           Z-ADD0         CMAR07
      **********           Z-ADD0         CMAR08
      **********           Z-ADD0         CMAR09
      **********           Z-ADD0         CMAR10
      **********           Z-ADD0         CMBUL1
      **********           Z-ADD0         CMPCTR
      **********           Z-ADDCMDT      CMENTD
      **********           MOVE Q1USER    CMIUSR
      **********           Z-ADDCMDT      C2IDT
      **********           Z-ADDDSTIME    C2ITM
      **********           ENDIF
      *** END DLT ***** 04/24/98 ***********************************************
     C                   MOVE      SCRR01        CMRR01
     C                   MOVE      SCRR02        CMRR02
     C                   MOVE      SCRR03        CMRR03
     C                   MOVE      SCRR04        CMRR04
     C                   MOVE      SCRR05        CMRR05
     C                   MOVE      SCRR06        CMRR06
     C                   MOVE      SCRR07        CMRR07
     C                   MOVE      SCRR08        CMRR08
     C                   MOVE      SCRR09        CMRR09
     C                   MOVE      SCRR10        CMRR10
     C                   MOVE      SCX01         CMRT01
     C                   MOVE      SCX02         CMRT02
     C                   MOVE      SCX03         CMRT03
     C                   MOVE      SCX04         CMRT04
     C                   MOVE      SCX05         CMRT05
     C                   MOVE      SCX06         CMRT06
     C                   MOVE      SCX07         CMRT07
     C                   MOVE      SCX08         CMRT08
     C                   MOVE      SCX09         CMRT09
     C                   MOVE      SCX10         CMRT10
     C***** BEG ADD ******* 10/26/07 **********************************
     C                   MOVE      SCCSTF        CMCSTF
     C                   MOVE      SCMSTF        CMMSTF
     C                   Z-ADD     SMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMEFDT
     C***** END ADD ******* 10/26/07 **********************************
      *
     C                   ENDSR                                                  MVSCDB
      *
      *===============================================================
      * OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
      *===============================================================
      *
     C     OTHERR        BEGSR
      *
     C                   ENDSR                                                   OTHERR
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     C     OTHFKS        BEGSR
     C                   MOVE      *OFF          *IN16                          INVALID SEARCH
     C                   MOVE      *OFF          *IN19                          RESET DSPATR(PC)
     C                   MOVEA     '00'          *IN(17)                        *IN17 - 18
     C                   MOVEA     '00000000'    *IN(20)                        *IN20 - 27
     C                   MOVEA     '00'          *IN(29)                        *IN29 - 30
     C                   MOVEA     '0000'        *IN(42)                        *IN42 - 45
     C                   MOVEA     '0'           *IN(47)                        *IN47
     C                   MOVEA     '00'          *IN(53)                        *IN53 - 54
      *
     C     *INKG         CASEQ     *ON           KGDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C     *INKV         CASEQ     *ON           KVDO                     90
     C     *INKW         CASEQ     *ON           KWDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *===============================================================
      * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
      *===============================================================
      *
     C     PGMEND        BEGSR
      *
      *  DELETE THE CLAIM RECORD IF THE USER PRESSED F3 IN ADD MODE
      *
      *******    *IN40     IFEQ *ON
     C     *IN98         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   END
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
      * *******            CLOSEBATRL
     C*** BEG   ADD *** 03/29/10 **************************************
     C                   CLOSE     W0070F12
     C*** END   ADD *** 03/29/10 **************************************
     C                   RETURN
     C                   ENDSR
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
      *
      ***** BEG ADD ******* 03/29/10 **********************************
     C                   OPEN      W0070F12
     C     CLTID         IFEQ      '025'
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     RWKY          SETGT     PRW01
     C     RWKY          READPE    PRW01                                  60
     C     *IN60         DOWEQ     *OFF
     C     RWSTS         IFEQ      'P'
     C                   EXFMT     WDW04
     C                   MOVE      'D'           @@MODE
     C                   LEAVE
     C                   ENDIF
     C     RWKY          READPE    PRW01                                  60
     C                   ENDDO
     C                   ENDIF                                                  CLTID = 25
      ***** END ADD ******* 03/29/10 **********************************
      *  CHECK THE MODE.  A=ADD, C=CHANGE, D=DISPLAY
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
      ********   @@MODE    COMP 'A'                      40ADD
     C     @@MODE        COMP      'A'                                    98    ADD
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           @@MODE
     C                   MOVEL(P)  MODEA         SCMODE
     C                   END
      *
     C     *IN09         IFEQ      *ON
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
      *
     C                   MOVE      *ON           DSPTYP
      *
      * GET WORKERS COMP CONTROL FILE
      *
     C                   MOVEL(P)  'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
      *
      * COMMERCIAL APPLICATION INSTALLED
      *
     C                   MOVE      *OFF          *IN66
     C     WCFL07        IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   END
      *
     C                   END
      *
      *     WCMC Installed determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   MOVE      *ON           XXMANC
     C                   ELSE
     C                   MOVE      *OFF          XXMANC
     C                   ENDIF
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  GET SECURITY LEVEL OF THE USER
      *
     C                   MOVE      *BLANK        XXAUTH            1
     C                   MOVE      *OFF          *IN67
     C     UPLVL         IFGE      WCSL03
     C                   MOVE      *ON           *IN67
     C                   MOVE      'Y'           XXAUTH                         AUTHORIZED
     C                   END
      *
     C                   MOVEL     UPUSNM        QCUSER
     C                   Z-ADD     UPRAL         XXRAL
     C                   Z-ADD     UPFYR         XUFYR
     C                   END
      *
      *  GET CURRENT DATE, CONVERT TO 7,0  TO CHAIN TO FILES
      *
      ***** BEG CHG ***** 03/01/06 ************************************
     C**************       Z-ADD*DATE     SRDTI
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      ***** END CHG ***** 03/01/06 ************************************
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   Z-ADD     XXDT          XXEFDT                         YMD
      ***** BEG CHG ***** 03/01/06 ************************************
     C****************     Z-ADD*DATE     SCEFDT  80       MDY
     C                   Z-ADD     DSDATE        SCEFDT            8 0          MDY
      ***** END CHG ***** 03/01/06 ************************************
      *
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     EXKY          CHAIN     PEX01                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      *
     C*** BEG  ADD***** 12/02/10 **************************************
      *INTIALIZE THE INDICATORS
     C                   MOVE      *OFF          *IN65
     C*
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C     CXOPCL        IFEQ      'Y'
     C                   MOVE      *ON           *IN65
     C                   ELSE
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C                   ENDIF
     C*** END  ADD***** 12/02/10 **************************************
      ***** END ADD ******* 08/10/98 **********************************
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
     C                   ENDSR
      *
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             9997
      *
     C     *IN97         IFEQ      *ON
     C     FLSTCM        ANDEQ     01218
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN99                          SKIP TO BOTTOM
     C                   END
      *
B01  C     *IN99         IFEQ      *OFF
      *
      *** BEGIN DLT *** 08/04/98 **************************************
      ********** SVDATE    IFNE CMDT
      ********** SVTIME    ORNE CMTIME
      **********           CALL 'WA017R'
      **********           PARM           CMUSER
      **********           PARM           QQRTCD
      **********
      **********           ELSE
      *** END DLT ***** 08/04/98 **************************************
      *
     C                   EXSR      MVSCDB
      *
     C*** BEGIN ADD *** 06/23/00 **************************************
     C                   UPDATE    DBRCDF
     C                   FEOD      WMCMP
     C*** END ADD ***** 06/23/00 **************************************
     C*** BEG ADD ***** 02/23/05 *************************************
     C*  Auto-DARTS
     C*    Check to see if status updated; if not ADD, call WA700R.
     C     SVSTAT        IFNE      C2STAT
     C     *IN98         ANDEQ     *OFF
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      'CLMCHG04'    @@DKEY           10
     C                   PARM      *BLANKS       @@COMM          256
     C                   ENDIF
     C*** END ADD ***** 02/23/05 *************************************
      *
     C*** BEG ADD ***** 06/07/10 *************************************
     C                   MOVEL     'LARGERSV'    XXKY
     C                   Z-ADD     *ZEROS        #RSVAM           11 2
     C     ADKY          CHAIN     PAD01                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      *OFF          *IN93
     C                   Z-ADD     1             R                 3 0
     C     ADSTAT        IFNE      'I'
     C     ADRECD        LOOKUP    IR(R)                                  93
     C     *IN93         IFEQ      *ON
     C                   Z-ADD     ADAMT         #RSVAM
     C                   ENDIF
     C                   ENDIF
     C     ADKY          READE     PAD01                                  60
     C                   ENDDO
     C*** END ADD ***** 06/07/10 *************************************
      *  MOVE FIELDS FOR RESERVE HISTORY
      *
     C                   MOVE      *BLANKS       XXRH              1
     C     WKRC01        IFNE      *ZERO
     C     WKRC02        ORNE      *ZERO
     C     WKRC03        ORNE      *ZERO
     C     WKRC04        ORNE      *ZERO
     C     WKRC05        ORNE      *ZERO
     C     WKRC06        ORNE      *ZERO
     C     WKRC07        ORNE      *ZERO
     C     WKRC08        ORNE      *ZERO
     C     WKRC09        ORNE      *ZERO
     C     WKRC10        ORNE      *ZERO
      *
     C                   MOVE      '1'           XXRH
     C                   MOVE      XXCO#         RHCO#
     C                   MOVE      XXFND         RHFND
     C                   Z-ADD     SCYR          RHFYR
     C                   MOVE      SCCASE        RHCASE
     C*** Begin add *** 03/23/04 **************************************
     C                   CLEAR                   RRT
     C                   CLEAR                   RSU
     C*
     C                   DO        10            X                 3 0
     C*
     C     RC(X)         IFNE      *ZERO
     C                   Z-ADD     1             Y                 3 0
     C     IR(X)         LOOKUP    RT(Y)                                  99
     C                   MOVE      IR(X)         RRT(Y)
     C                   Z-ADD     RC(X)         RSU(Y)
     C*
     C*** BEG ADD ***** 02/23/05 *************************************
     C*  Auto-DARTS: Now check large reserve change amount
     C*
     C*** BEG ADD ***** 06/07/10 *************************************
     C     #RSVAM        IFNE      *ZERO
     C     RC(X)         ANDGE     #RSVAM
     C                   EXSR      AUDRT
     C                   ENDIF
      *
     C*** BEG DEL ***** 06/07/10 *************************************
     C***        LA,X      IFNE *ZERO
     C***        RC,X      ANDGELA,X
     C***                  EXSR AUDRT
     C***                  ENDIF
     C*** END DEL ***** 06/07/10 *************************************
     C*** END ADD ***** 02/23/05 *************************************
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*** End add ***** 03/23/04 **************************************
     C*** Begin dlt *** 03/23/04 **************************************
     C**********           MOVE SCRT01    RHRT01
     C**********           MOVE SCRT02    RHRT02
     C**********           MOVE SCRT03    RHRT03
     C**********           MOVE SCRT04    RHRT04
     C**********           MOVE SCRT05    RHRT05
     C**********           MOVE SCRT06    RHRT06
     C**********           MOVE SCRT07    RHRT07
     C**********           MOVE SCRT08    RHRT08
     C**********           MOVE SCRT09    RHRT09
     C**********           MOVE SCRT10    RHRT10
     C**********           MOVE WKRC01    RHSU01
     C**********           MOVE WKRC02    RHSU02
     C**********           MOVE WKRC03    RHSU03
     C**********           MOVE WKRC04    RHSU04
     C**********           MOVE WKRC05    RHSU05
     C**********           MOVE WKRC06    RHSU06
     C**********           MOVE WKRC07    RHSU07
     C**********           MOVE WKRC08    RHSU08
     C**********           MOVE WKRC09    RHSU09
     C**********           MOVE WKRC10    RHSU10
     C*** End dlt ***** 03/23/04 **************************************
      *
     C                   Z-ADD     XXEFDT        RHEFDT
     C                   MOVE      SCEXRS        RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      *BLANKS       RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   TIME                    DSDTTM
     C*** BEG DEL ***** 03/03/10 **************************************
     C*****                Z-ADDXXDT      RHDT
     C*** END DEL ***** 03/03/10 **************************************
     C                   Z-ADD     DSTIME        RHTIME
      *
     C*** BEG ADD ***** 03/03/10 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
      *
     C                   MOVEL     SRDTO         RHIDT
      *
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      *
     C                   Z-ADD     DSTIME        RHITIM
     C*** END ADD ***** 03/03/10 **************************************
      *
     C                   END
      *
      *  MOVE FIELDS FOR TRANSFER LOST TIME - MED ONLY CHANGE HISTORY
      *
     C                   MOVE      *BLANKS       XXTH              1
     C     BFLTFL        IFNE      CMLTFL
     C                   MOVE      '1'           XXTH
     C                   MOVE      XXCO#         THCO#
     C                   MOVE      XXFND         THFND
     C                   Z-ADD     SCYR          THFYR
     C                   MOVE      SCCASE        THCASE
      *
     C                   MOVE      BFLTFL        THFROM
     C                   MOVE      CMLTFL        THTO
     C                   Z-ADD     XXEFDT        THTRDT
     C                   MOVE      *BLANK        THSTAT
     C                   MOVE      Q1USER        THUSER
     C                   MOVE      Q1PGM         THPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          THDT
     C                   Z-ADD     DSTIME        THTIME
     C                   END
      *
      *  MOVE FIELDS FOR STATUS CHANGE HISTORY
      *
     C                   MOVE      *BLANKS       XXCH              1
     C     SCOCST        IFNE      BFSTAT
     C                   MOVE      '1'           XXCH
     C                   MOVE      XXCO#         CHCO#
     C                   MOVE      XXFND         CHFND
     C                   Z-ADD     XXFYR         CHFYR
     C                   MOVE      XXCASE        CHCASE
      *
     C                   Z-ADD     XXEFDT        CHEFDT
     C                   MOVE      SCEXPL        CHEXPL
     C                   MOVE      SCOCST        CHOCST
     C                   MOVE      *BLANK        CHSTAT
     C                   MOVE      Q1USER        CHUSER
     C                   MOVE      Q1PGM         CHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CHDT
     C                   Z-ADD     DSTIME        CHTIME
     C                   END
      *
      *  Update the Oustanding Reserve bucket in the Emp Detail file.
      *
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C                   Z-ADD     SCYR          XXFYR
     C     ELKY          CHAIN     PEL01                              42
     C                   XFOOT     RC            XXCHG             9 2
     C                   ADD       XXCHG         ELOUTS
      *
      *  IF CLAIM IS REOPENED, WRITE A DARTS DIARY REMARK.
      *
     C     BFSTAT        IFEQ      'C'
     C     SCOCST        ANDEQ     'O'
      *** BEG ADD *** 01/16/98 *************************************
     C     CXROPN        ANDNE     *BLANK
      *** END ADD *** 01/16/98 *************************************
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     XXFYR         QCFYR
     C                   Z-ADD     XXCASE        QCCASE
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             MOVELQCKEY     CMKEY
      ********
      ***INITIALIZE FIELDS
      ********
     C********             MOVE *BLANKS   CMCOMM
     C********             MOVE *BLANKS   CMDCID
     C********             MOVE *BLANKS   CMPNAM
     C********             MOVE *BLANKS   CMUSER
     C********             MOVE *BLANKS   CMPGM
     C********             MOVE *BLANKS   CMIUSR
     C********             MOVE *BLANKS   CMFOLD
     C********             MOVE *BLANKS   CMIPGM
     C********             MOVE *BLANKS   CMRMCD
     C********             MOVE *BLANKS   CMCODE
      ********
      ***Send*diary to the Adjuster.
      ********
     C********   CMCLEX    CHAINPUP02                60
     C********   *IN60     IFEQ '0'
     C********             MOVELUPUSR     CMUSER
      ********
      ***CALCULATE DIARY DATE
      ********
     C********             TIME           DSDTTM
     C********             Z-ADDCXRODY    SRDAYS
     C********             Z-ADD*DATE     SRDATE
     C********             MOVE *BLANKS   SRERR
     C********             CALL 'SRFUTR8'
     C********             PARM           SRDATE  80
     C********             PARM           SRDAYS  50
     C********             PARM           SRERR
     C********             MOVE SRDATE    XXMDAT  80
     C********             Z-ADDXXMDAT    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
      ********
     C********             MOVE CXROPN    XXRMCD
     C********   CTKY      CHAINPCT03                60
     C********   *IN60     IFEQ '0'
     C********             MOVELRMK,1     CMCOMM
     C********             MOVE 'Y'       CMDIFL
     C********             MOVE CXROPN    CMRMCD
     C********             MOVE *BLANKS   CMCSID
     C********             MOVELCTDEPG    CMDCID
     C********             MOVEL'W0070R'  CMPNAM    P
     C********             Z-ADDXXDT      CMTRDT
     C********             Z-ADDDSTIME    CMTRTM
     C********             MOVE *ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE 'A'       CMSTAT
     C********             MOVELQ1PGM     CMPGM
     C********             Z-ADD1         CMTIME
     C********             Z-ADDXXDT      CMDT
     C********             MOVE Q1USER    CMIUSR
     C********   CMDCID    IFNE *BLANKS
     C********             MOVELQCKEY     CMFOLD
     C********             ELSE
     C********             MOVE *BLANKS   CMFOLD
     C********             END
     C********             MOVE *ZEROS    CMDCDT
     C********             MOVE *BLANKS   CMAPID
     C********             MOVE *BLANKS   CMIDOC
     C********             MOVE *BLANKS   CMIQUE
     C********             MOVE *BLANKS   CMIMGF
     C********             MOVE *BLANKS   CMIPTH
     C********             MOVE *ZEROS    CMPIDX
     C********             MOVE *BLANKS   CMQPTY
     C********             MOVE *BLANKS   CMDCTP
     C********             MOVELQ1PGM     CMIPGM
      ********
      ****BEGIN CHG *** 02/23/98 ***********************************************
      **********           MOVE *BLANKS   CMSMCD
     C********             MOVE CTSMCD    CMSMCD
      ****END*  CHG *** 02/23/98 ***********************************************
      ********
     C********   CMDCID    IFNE *BLANKS
     C********             MOVE 'Y'       CMFL01
     C********             ELSE
     C********             MOVE *BLANKS   CMFL01
     C********             END
     C********   CTDPRG    IFGT *ZERO
     C********             MOVE 'Y'       CMFL02
     C********             ELSE
     C********             MOVE *BLANKS   CMFL02
     C********             END
     C********             MOVE CTFL02    CMFL03
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********             Z-ADDXXDT      CMIDT
      ********
      ***FIND*NEXT DARTS SEQUENCE NUMBER
      ********
     C********   C4KY      SETGTPCM03
     C********   C4KY      REDPEPCM03                    60
     C********   *IN60     IFEQ '0'
     C********             ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     C********             END
      ********
     C********             WRITEPCM04
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCMLA
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVE      *BLANKS       Q#COMM
     C                   MOVE      *BLANKS       Q#DCID
     C                   MOVE      *BLANKS       Q#PNAM
     C                   MOVE      *BLANKS       Q#USER
     C                   MOVE      *BLANKS       Q#PGM
     C                   MOVE      *BLANKS       Q#FOLD
     C                   MOVE      *BLANKS       Q#IPGM
     C                   MOVE      *BLANKS       Q#RMCD
     C                   MOVE(P)   *BLANKS       Q#CODE            2
     C     CMCLEX        CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  UPUSR         Q#USER           10
     C                   TIME                    DSDTTM
     C                   Z-ADD     CXRODY        SRDAYS
      *
      ***** BEG CHG ***** 03/01/06 ************************************
     C*************        Z-ADD*DATE     SRDATE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDATE
      ***** END CHG ***** 03/01/06 ************************************
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE            8 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM                    SRERR
     C                   MOVE(P)   SRDATE        Q#CDAT            8
     C                   MOVE      CXROPN        XXRMCD
     C     CTKY          CHAIN     PCT03                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  RMK(1)        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL(P)  'Y'           Q#DIFL            1
     C                   MOVE(P)   CXROPN        Q#RMCD            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  QCKEY         Q#FOLD           80
     C                   ELSE                                                   DCID = blanks
     C                   MOVE      *BLANKS       Q#FOLD
     C                   ENDIF                                                  DCID = blanks
     C                   MOVE(P)   *BLANKS       Q#APID            2
     C                   MOVE(P)   *BLANKS       Q#IDOC           12
     C                   MOVE(P)   *BLANKS       Q#IQUE           10
     C                   MOVE(P)   *BLANK        Q#IMGF            1
     C                   MOVE(P)   *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVE(P)   *BLANK        Q#QPTY            1
     C                   MOVE(P)   *BLANKS       Q#DCTP            5
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE      'Y'           Q#FL01            1
     C                   ELSE                                                   DCID = blanks
     C                   MOVE      *BLANKS       Q#FL01
     C                   ENDIF                                                  DCID = blanks
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           Q#FL02            1
     C                   ELSE                                                   DPRG > 0
     C                   MOVE      *BLANKS       Q#FL02
     C                   ENDIF                                                  DPRG > 0
     C                   MOVE      '00000'       Q#SEQ#            5
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
     C*** END ADD *** 04/26/00 ****************************************
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  DDKEY
      *
     C                   ENDIF                                                  SCOCST = 'O' and
      *                                                      BFSTAT = 'C'
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
      *
      *  WRITE HISTORY RESERVE
      *
     C     XXRH          IFEQ      '1'
     C                   WRITE     PRH01
      *** BEGIN ADD *** 05/12/10 **************************************
     C                   CLEAR                   RRT
     C                   CLEAR                   RSU
     C                   CLEAR                   RC
     C                   CLEAR                   IS
      *** END ADD ***** 05/12/10 **************************************
     C                   END
      *
      *  WRITE TRANSFER HISTORY
      *
     C     XXTH          IFEQ      '1'
     C                   WRITE     PTH01
     C                   END
      *
      *  WRITE STATUS CHANGE HISTORY
      *
     C     XXCH          IFEQ      '1'
     C                   WRITE     PCH01
     C                   END
      *
      *  UPDATE EMPLOYER DETAIL FILE
      *
     C     *IN42         IFEQ      '0'
     C                   UPDATE    PEL01
     C                   END
      *
     C*** BEGIN DLT *** 06/23/00 **************************************
     C******************** UPDATDBRCDF
     C*** BEGIN ADD *** 06/25/99 **************************************
     C******************** FEOD WMCMP
     C*** END ADD ***** 06/25/99 **************************************
     C*** END DLT ***** 06/23/00 **************************************
      *
     C*** BEGIN ADD *** 03/10/10 **************************************
     C     CLTID         IFEQ      '025'
     C*  When a claim is closed, delete the claim index suspense record
     C*    if it exists.
     C*
     C     BFSTAT        IFEQ      'O'
     C     SCOCST        ANDEQ     'C'
     C*
     C     CMKY          CHAIN     PID01                              60
     C  N60              DELETE    PID01
     C*
     C                   ENDIF
     C*
     C*  Wnen a claim is reopened, create a claim index suspense record
     C*    if the claim index DARTS remark code is not blank, and the
     C*    suspense record does not already exist.
     C*
     C     BFSTAT        IFEQ      'C'
     C     SCOCST        ANDEQ     'O'
     C*
     C     CMKY          CHAIN(N)  PID01                              60
     C     *IN60         IFEQ      *ON
     C     CXAIC         ANDNE     *BLANKS
     C*
     C                   Z-ADD     CMCO#         IDCO#
     C                   Z-ADD     CMFND         IDFND
     C                   Z-ADD     CMFYR         IDFYR
     C                   Z-ADD     CMCASE        IDCASE
     C                   MOVE      *BLANKS       IDCLEX
     C*
     C                   MOVE      DSDATE        SRDATE
     C                   Z-ADD     CXAICD        SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
      *
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IDASDT
      *
     C                   MOVEL     Q1PGM         IDPGM
     C                   MOVEL     Q1USER        IDUSER
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IDDT
     C                   Z-ADD     DSTIME        IDTIME
     C                   MOVEL     Q1USER        IDIUSR
     C                   Z-ADD     IDDT          IDIDT
      *
     C                   WRITE     PID01
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 03/10/10 **************************************
      *
      *    Call API to update Managed Care Claim Master - FMCMP
      *
     C                   CLEAR                   PARMCL
      *
     C     XXAPIF        CASEQ     *ON           MCAPIC
     C                   ENDCS                                                              DATE
      *
      * CALL NEXT PROGRAM IF F6, OPT 2, OR 5 WAS SELECTED FROM CLM MASTER
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   EXSR      CALLR
     C                   END
      *
      *** BEGIN DLT *** 08/04/98 **************************************
E02   ******************** ENDIF                           SVDATE NE CMDATE
      *** END DLT ***** 08/04/98 **************************************
E02  C                   ENDIF                                                  *IN99=*OFF
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  UPDATR
      *
     C*** BEG ADD ***** 02/23/05 *************************************
     C*===============================================================
     C* AUDRT  - WRITE LARGE RESERVE CHANGE AUTO-DARTS
     C*===============================================================
     C     AUDRT         BEGSR
     C*
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      'LARGERSV'    @@DKEY           10
     C                   PARM      *BLANKS       @@COMM          256
     C*
     C                   ENDSR                                                  AUDRT
     C*
     C*** END ADD ***** 02/23/05 *************************************
      *===============================================================
      * WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
      *===============================================================
      *
     C     WRITER        BEGSR
      *
     C                   ENDSR                                                  WRITER
      *===============================================================
      * MCAPIC - DATA INTERFACE: MC CLAIM MASTER - FMCMP
      *===============================================================
      *
     C     MCAPIC        BEGSR
      *
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ******************** Z-ADDCMACDT    SRDTI
      ******************** CALL 'SRCVTDAT'DTMDYY
      ******************** MOVELSRDTO     QMACDT
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE      CMACDT        QMACDT
      *** END ADD ***** 11/05/97 ***********************************************
      *
     C                   MOVE      CMEMP#        QMEMP#
     C                   MOVE      CMDIV         QMDIV
      *
     C                   MOVE      XXCO#         QMCO#
     C                   MOVE      *BLANKS       QMSUB#
     C                   MOVE      XXFND         QMFND
     C                   MOVE      XXFYR         QMFYR
     C                   MOVE      XXCASE        QMCASE
      *
     C                   MOVE      CMACTM        QMACTM                         Accidnt time
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ****************     Z-ADDCMDTRP    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMDTRP    P      Reported
      ****************     Z-ADDCMDTLW    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMDTLW    P      Last worked
      ****************     Z-ADDCMENTD    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMENTD    P      ENTERED
      ****************     Z-ADDCMRCVD    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMRCVD    P      Received
      ****************     Z-ADDCMMMI     SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMMMI     P      Max med impr
      ****************     Z-ADDCMDOD     SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMDEDT    P      Death date
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE(P)   CMDTRP        QMDTRP                         Reported
     C                   MOVE(P)   CMDTLW        QMDTLW                         Last worked
     C                   MOVE(P)   CMENTD        QMENTD                         ENTERED
     C                   MOVE(P)   CMRCVD        QMRCVD                         Received
     C                   MOVE(P)   CMMMI         QMMMI                          Max med impr
     C                   MOVE(P)   CMDOD         QMDEDT                         Death date
      *** END ADD ***** 11/05/97 ***********************************************
     C     CMDTH         IFEQ      'Y'                                          Death flag
     C                   MOVEL     'DI'          QMDEEM
     C                   ELSE
     C                   MOVEL     *BLANKS       QMDEEM
     C                   ENDIF
      *
     C                   Z-ADD     DBRRN         QMRRN
      *
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ****************     Z-ADDCMDTRE    SRDTI              Date
      ****************     CALL 'SRCVTDAT'DTMDYY           YMD --> MDY
      ****************     MOVELSRDTO     QMDTRE    P      Rehabilitatn
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   MOVE(P)   CMDTRE        QMDTRE                         Rehabilitation
      *** END ADD ***** 11/05/97 ***********************************************
     C                   MOVE      CM#DEP        QM#DEP                         # dependents
      *** BEGIN ADD *** 04/07/98 **************************************
     C                   MOVEL(P)  CMSHFT        QQWKPT                         Shift
      *** END   ADD *** 04/07/98 **************************************
      *
      *=====================================================================
     C                   CALL      'FA003R'
      *
     C                   PARM      *BLANK        QMRTCD            1
     C                   PARM      CMSS#         QMSS#             9
      *** BEGIN CHG *** 11/05/97 ***********************************************
      ******************** PARM           QMACDT  8
     C                   PARM                    QMACDT            7
      *** END CHG ***** 11/05/97 ***********************************************
      *
     C                   PARM                    QMCO#             3
     C                   PARM      *BLANK        QMSUB#            5
     C                   PARM                    QMFND             3
     C                   PARM                    QMFYR             3
     C                   PARM                    QMCASE            7
      *
     C                   PARM                    QMEMP#            9
     C                   PARM                    QMDIV             5
     C                   PARM                    QMACTM            6
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ******************** PARM           QMDTRP  8
      ******************** PARM           QMDTLW  8
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   PARM                    QMDTRP            7
     C                   PARM                    QMDTLW            7
      *** END ADD ***** 11/05/97 ***********************************************
     C                   PARM      CMACDS        QMACDS           60            Acc description
     C                   PARM      CMMCON        QMMCON            1            Method contact
      *** BEGIN DLT *** 11/05/97 ***********************************************
      ******************** PARM           QMENTD  8
      ******************** PARM           QMRCVD  8
      ******************** PARM           QMMMI   8
      *** END DLT ***** 11/05/97 ***********************************************
      *** BEGIN ADD *** 11/05/97 ***********************************************
     C                   PARM                    QMENTD            7
     C                   PARM                    QMRCVD            7
     C                   PARM                    QMMMI             7
      *** END ADD ***** 11/05/97 ***********************************************
     C                   PARM      *BLANKS       QM1A1             1            Unavailable
     C                   PARM      *BLANKS       QM10A1           10            Unavailable
     C                   PARM      CMLTFL        QMLTFL            1            Lost time flag
      *** BEGIN CHANGE *** 09/02/99 ***********************************
     C******************** PARM CMACL#    QMSCL# 10        State claim numbr
     C                   PARM      CMACL#        QMSCL#           25            State claim numbr
      *** END CHANGE ***** 09/02/99 ***********************************
     C                   PARM      *BLANKS       QMMCTM            4            Unavailable
     C                   PARM      *BLANKS       QMSTWK            2            Unavailable
      *** BEGIN CHG *** 11/05/97 ***********************************************
      ******************** PARM           QMDEDT  8
     C                   PARM                    QMDEDT            7
      *** END CHG ***** 11/05/97 ***********************************************
     C                   PARM                    QMDEEM            2            Death claim flag
     C                   PARM      CMNRSE        QMCMNU            3            Nurse
     C                   PARM      *BLANKS       QMCMSU            3            Unavailable
     C                   PARM      CMCLEX        QMCLAD            3            Claim examiner
     C                   PARM                    QMRRN             9 0
     C                   PARM      CMLSN         QMLSN            30            Last name
     C                   PARM      CMFSN         QMFSN            15            First name
     C                   PARM      CMMID         QMMID             1            Middle initial
      *** BEGIN CHG *** 11/05/97 ***********************************************
      ******************** PARM           QMDTRE  8
     C                   PARM                    QMDTRE            7
      *** END CHG ***** 11/05/97 ***********************************************
     C                   PARM      CMSUPR        QMCLSU            3            Supervisor
     C                   PARM                    QM#DEP            3
      *** BEGIN ADD *** 04/07/98 **************************************
     C                   PARM      CMOCDS        QQOCDS           40            OCDS
     C                   PARM      CMHIDT        QQHIDT            7 0          HIDT
     C                   PARM      CMESTS        QQESTS            2            ESTS
     C                   PARM                    QQWKPT            2            SHFT
     C                   PARM      CMPREX        QQPREX            1            PREX
     C                   PARM      CMACD2        QQACD2           60            ACD2
     C                   PARM      CMCVFL        QQCVFL            1            CVFL
     C                   PARM      CMMCO         QQMCA             1            MCO
     C                   PARM      CMIPCT        QQMMIP            5 2          IPCT
     C                   PARM      CMODIS        QQODIS            1            ODIS
     C                   PARM      CMYRLX        QQYRLX            4 0          YRLX
     C                   PARM      CMDBDT        QQDBDT            7 0          DBDT
     C                   PARM      CMHRSD        QQHRDY            3 0          HRSD
     C                   PARM      CMDAYS        QQDYWK            1 0          DAYS
      *** END   ADD *** 04/07/98 **************************************
      *** BEGIN ADD *** 08/11/98 **************************************
     C                   PARM      CMFEAD        QQCLFA            3            FEAD
      *** END   ADD *** 08/11/98 **************************************
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C                   PARM      CMDNY         QQDNY             1            DENIED
     C                   PARM      CMIMP         QQIMP             1            IMPAIRMENT
     C***** END   ADD **** 04/03/00 ***********************************
     C*** BEGIN ADD *** 08/25/00 **************************************
     C                   PARM      CMJRST        QQJRST            2            JUR STATE
     C*** END   ADD *** 08/25/00 **************************************
      *
     C                   ENDSR
      *===============================================================
     C*** BEGIN ADD *** 05/27/10 **************************************
     C*================================================================
     C* SPECIFIC RETENTION CALCULATION ( QQTRSV )
     C*================================================================
     C     SUMCLP        BEGSR
     C                   MOVEL     CMCO#         QQCO#
     C                   MOVEL     CMFND         QQFND
     C                   MOVEL     CMFYR         QQFYR
     C                   MOVEL     CMCASE        QQCASE
     C                   MOVEA     *BLANKS       CDP
     C                   MOVEA     *ZEROS        WK2
     C                   MOVEA     *ZEROS        DA2
     C                   MOVEA     *ZEROS        AM2
     C                   CALL      'WA576R5'
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      *BLANKS       QQTRCD            2
     C                   PARM      *BLANKS       QQTYPE            1
     C                   PARM      *BLANKS       QQRECD            2
     C                   PARM      *BLANKS       QQRETP            1
     C                   PARM                    CDP
     C                   PARM                    WK2
     C                   PARM                    DA2
     C                   PARM      *ZEROS        QQTAMT           11 2
     C                   PARM      *ZEROS        QQTRSV           11 2
     C                   PARM                    AM2
     C                   ENDSR
     C*===============================================================
     C* TO CALCULATE THE FUND RETENTION PERCENTAGE FROM WDF1P
     C*===============================================================
     C     FUNRET        BEGSR
     C     FNKY1         CHAIN     PF101                              75
     C     *IN75         IFEQ      *OFF
     C                   Z-ADD     F1EMRT        FNRET#           11 2
     C     FNRET#        IFNE      *ZERO
     C     QQTRSV        DIV       FNRET#        RETPR#            5 2
     C     RETPR#        MULT      100           RETPR#
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*** END   ADD *** 05/27/10 **************************************
      *
**  ARRAY MSG
ORIGINAL RESERVES
ORIGINAL OPEN OF CLAIM
CLAIM CLOSED
PAYMENTS EXIST & HISTORY = 0. PRESS CMD 23 TO CONFIRM DELETE
**  ARRAY RMK - DARTS DIARY REMARK
FILE REOPENED
X
X
RESCIND - CONTROVERT FLAG CHANGED FROM "Y" TO "N"
