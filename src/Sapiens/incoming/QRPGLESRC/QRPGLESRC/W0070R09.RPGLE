     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     h  bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0070R09 - CLAIMS MANAGEMENT INFORMATION
     H* ***************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY MANAGEMNT INFORMATION*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0070R - CLAIM MASTER SEARCH                               *
     H*                                                               *
     H*  CALLS TO:  FA003R - API to update MC claim master FMCMP      *
     H*             SA001R - API to determine if MC installed         *
     H*                                                               *
     H*  CREATED: 01/30/97  PROGRAMMER: 47    PROJECT: 60ANC          *
     H* ***************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 06/13/12 C:282 Project: 133WNA Priss 24                       *
     H*                Added the edits when claim denial flag changes *
     H*                from 'N' to 'Y'.                               *
     H*                                                               *
     H* 05/15/12 C:282 Project: 133WMZ                                *
     H*                Add functionality to Clm DARTS control when    *
     H*                flags change y to blank                        *
     H*                                                               *
     H* 05/09/12 C:282 Project: 133WNA                                *
     H*                Edits based on change of Claim Denial Flag     *
     H*                - Full denial effective flag                   *
     H*                - Full Denial Rescission Date                  *
     H*                - Full Denial Reason Code                      *
     H*                - Denial Reason Narrative fields               *
     H*                                                               *
     H* 03/16/12 C:282 Project: 133WML                                *
     H*                Added the pop wiwdow to re-confirm the CMS     *
     H*                submission flag value.                         *
     H*                                                               *
     H* 01/05/12 C: 53 PROJECT: 114WDR PRISS 22
     H*                CORRECT INDICATOR                                                           *
     H*                                                               *
     H* 09/07/10 C:235 Project: 114WWK/PRISS-20                       *
     H*                Made the changes so that user can enter value  *
     H*                'R' in Subrogation field and not take them to  *
     H*                the Subro screen                               *
     H*                                                               *
     F* 08/05/10 C:190 Seq#: 73439                                    *
     F*                Revert changes done under #73439 on 07/02/10.  *
     F*                                                               *
     H* 08/04/10 C:235 PROJECT: 139WNF Priss#33                       *
     H*                If  Claim Denied is "Yes" for CMJRST<>CA       *
     H*                unable to navigate to back to W/W Claim screen *
     H*                                                               *
     H* 07/29/10 C:190 PROJECT: 139WNF Priss#25                       *
     H*                Added filed - 'Partial Denial Field'           *
     H*                on SCR02.                                      *
     H*                                                               *
     H* 07/13/10 C:235 Project: 114WWK                                *
     H*                Add Recovery Option to Subro Drop Down         *
     H*                                                               *
     H* 07/02/10 C:190 Seq#: 73439                                    *
     H*                Seasonal emp must be 'Y',                      *
     H*                if Seasonal data is saved.                     *
     H*                                                               *
     H* 07/02/10 C:190 Project: 25WQC(P#38)                           *
     H*                Lifetime Benefit Flag must be 'Y',             *
     H*                if Lifetime Benefit record is associated       *
     H*                to the claim.                                  *
     H*                                                               *
     H* 05/17/10 C:235 Project: 139WNF Additional Changes             *
     H*                All New field and function keys will be        *
     H*                qualified on the WCMS EDI install flag='Y'     *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 03/23/10 C:235 Project: 139WNF                                *
     H*                EDI SCREEN DESIGN                              *
     H*                                                               *
     H* 03/10/10 C:242 Project: 25WRK                                 *
     H*                Claims Diary Retrofit - 25WJW, 25WKA, 25WKD    *
     H*                                                               *
     H* 02/22/10 C:247 Project: 25WQC                                 *
     H*                Lifetime income benefits                       *
     H*                                                               *
     H* 02/03/10 C:242 Project: 25WPU                                 *
     H*                Benefit Calculator-Seasonal Indicator          *
     H*
     H* 09/30/09 C:53  PROJECT: 114WVH
     H*                Added carrier code field for group 460.        *
     H*                                                               *
     H* 05/20/09 C:33  Project: WC1770                                *
     H*                Medicare Beneficiary Submission                *
     H*                                                               *
     H* 03/19/09  C:227  SEQ#67704                                    *
     H*                  Program checked out for compilation          *
     H*                                                               *
     H* 02/25/09  C:190  SEQ#66069                                    *
     H*                  Made Denied flag mandatory for Clt#065       *
     H*                  only.                                        *
     H*                                                               *
     H* 08/28/08 C:136 SUPPORT: DR139 64774                           *
     H*                CHANING THE DECISION DATE SHOULD WRITE         *
     H*                ONLY ONE DIARY REMARK.                         *
     H*                                                               *
     H* 04/17/08  C:190  PROJECT WC1636(PRISS#6)                      *
     H*                  Commented repeated code for SUBRO flag.
     H*
     H* 03/18/08  C:190  PROJECT WC1636                               *
     H*                  Condition the display of SCR02 on the        *
     H*                  jurisdiction state of the claim being        *
     H*                  "CA"
     H*
     H* 03/17/08  C:180  PROJECT WC1589 - PRISS # 14
     H*                  Display Claim-Denied as required.
      *                  Changes to DSPF only. No pgm changes.
      *
     H* 02/27/08  C:180  Project: 65WGI PRISS # 9
     H*                  Protect 2nd occurrence of SCDNY.
     H*                  No pgm changes - DSPF changes only.
     H*
     H*
     H* 02/08/08  C:33   Project: 45WFD  WA133/61727                  *
     H*                  Update indicators for flag messages.  *IN92  *
     H*                   was already being used.                     *
     H*
     H* 01/10/08 C:33 Project: WC1128    WA133/61801                  *
     H*               Only call the Auto Diary pgm once for NEWCLAIM. *
     H*                                                               *
     H* 09/12/07 C:15 Support: 60116                                  *
     H*               Added code to correct issue with project 71WEJ/ *
     H*               71WHI.  Staff codes should only be assigned if  *
     H*               they are blank.                                 *
     H*                                                               *
     H* 08/20/07 C:53 SUPPORT: 59363                                  *
     H*               CHANGES FOR 114WAS ONLY FOR 114                 *
     H*                                                               *
     H* 08/20/07 C:193 PROJECT: 65WGI                                 *
     H*                EDI Rel 3.0 Impl/Setup/Training                *
     H*                                                               *
     H* 06/07/07  C:132  P: 45WFR                                     *
     H*                  ADD Subro Info Screen for "P" Types for back *
     H*                  to base for client 45, now for all clients.  *
     H*                                                               *
     H* 04/23/07 C:62  Support: 58084                                 *
     H*                Correct user authority issue by not editing if *
     H*                code comes in invalid                          *
     H*                                                               *
     H* 02/09/07 C:53  Support: 54901
     H*                Correct user authority issue
     H*
     H* 11/01/06 C:53  PROJECT: 114WMF   114WBY 08/24/01
     H*                ADDED CARRIER CODE FIELD.                      *
     H*
     H* 11/01/06 C:53  PROJECT: 114WMF   114WAS 01/26/01
     H*              - Make Case Nurse required entry.                *
     H*              - Diary item, "MCIRF", on initial entry.         *
     H*
     H*  09/25/06 C: 58 PROJECT: 71WHI                                *
     H*                 USE SPECIFIC SUPERVISORS AND ADJUSTORS IF     *
     H*                 EXISTING IN EXCPT CODE FILE (WMSEP)
     H*                                                               *
     H* 09/06/06 C:15  Project: WC1128  SEQ# 54582                    *
     H*                Added code to call WA700R to create New Claim  *
     H*                auto diaries.                                  *
     H*                                                               *
     H* 07/05/06  C:53   P: 45WFD
     H*                  Allow "P" for SDF & SUBRO flags for client 45
     H*
     H* 01/24/06  C:168  PROJECT: 132WBU
     H*                - Added code to display screen 02, moved the
     H*                  "Claim denied" flag from screen 1 to 2, added
     H*                  code to calculate the decision date if delayed
     H*                  and write a Diary remark for adjuster.  To
     H*                  change the Decision Date user most have DARTS
     H*                  authority
     H*              - Change the code to get the *date to use the    *
     H*                *time instead to use correct date if they don't*
     H*                log out at night                               *
     H*
     H* 08/17/05  C:132  P: 132WBO
     H*                  Change for F12 on Return from W0070R17
     H*
     H* 03/23/05  C:132  P: 132WBO
     H*                  ADD Call to W0070R17 if Subrogation = "Y"
     H*
     H*   02/23/05   C:58     P: WC1128   Auto-DARTS
     H*                Add call to WA700R if change to any code of:
     H*                  Deny, Controverted, Subrogated, Excess,
     H*                  Acquired, OSHA reportable, SDA, or USL&H.
     H*
     H*  12/08/03 C: 53 Support: 40199
     H*                 Initialize exess rein/specific retention fields
     H*                                                               *
     H*  08/25/00 C:062 PROJECT: MC232                                *
     H*                 ADD JURISDICTION STATE TO PARM LIST OF FA003R.*
     H*
     H* 07/10/00 C:53  PROJECT WC999
     H*                ADD USL&H EXPOSURE FIELD
     H*
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 04/03/00 C:118 PROJECT: WC854                                 *
     H*                Add parm to call of FA003R to pass WMCMP/      *
     H*                CMIMP.                                         *
     H*                                                               *
     H* 09/02/99 C:112 Support: 19256 (MC048)                         *
     H*                Change QMSCL# from 10A to 25A                  *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*
     H*  05/10/99 C:112 Support# 17145
     H*           The SCCEC (Claim Entry Clerk) field is showing
     H*           invalid values.  The field is initialized via the
     H*           CMIUSR (Initial User) field. Made change to
     H*           initialize SCCEC to blank before re-initializing
     H*           with CMIUSR.
     H*
     H*  05/05/99 C:33  SEQ#: WC033/17985
     H*                 ADD PARM TO CALL OF W0321R
     H*
     H*  01/11/99 C:31  Project: MC152
     H*                 V4: Bill Entry re-write
     H*                 1. Add parm to FA003R call.
     H*
     H* 12/16/98 C:15 Support: WC060/15577
     H*               Removed code which adds the initial medical
     H*               reserves to a claim.  This is being done in
     H*               W0070R08.
     H*
     H* 11/16/98 C:15 Project: WC577
     H*               Changed the bar code process.  A bar code API
     H*               is now called to return certain parameters.
     H*
     H* 08/31/98 C:15 Support: WC100/13042
     H*               If IXFROI and CMJRST are blank, do not call the
     H*               state specific screen because XXPGM = 'W0070R'
     H*               and a recursive call error will result.
     H*
     H* 08/11/98 C:62 Project: MC155
     H*               ADDED PARM FOR CMFEAD TO FA003R
     H*
     H* 08/10/98 C:61 Project: 48WLC
     H*               Display old Emp#/Div from Employer Cross
     H*               Reference (WZEXP).
     H*
     H* 08/04/98 C:15 Support: WC021/13035
     H*               Fixed the claim record lockout routine.
     H*
     H* 07/13/98 C:15 Support: WC051/12451
     H*               Removed a parm from parm list for the state
     H*               specific pgms.
     H*
     H* 06/23/98 C:15 Support: WC095/11923
     H*               Write a reserve history record when the initial
     H*               medical reserves are added to a claim.
     H*
     H* 05/18/98 C:15 Support: WC060/10942
     H*               Removed code which creates the comp and state
     H*               benefit diaries.
     H*
     H* 04/22/98 C:15 Project: WC577  seq# 10944
     H*               Added code to load the default medical reserves
     H*               for medical claims in add mode.
     H*
     H* 04/13/98 C:15 Project: WC468
     H*               Added call to the EDI Log API pgm.
     H*
     H* 04/07/98 C:62 Project: MC155
     H*               Added additional fields to FA003R parm list.
     H*
     H* 03/30/98 C:85 Project: WC577
     H*               CORRECTIONS AND REVISIONS TO 02/23/98 PROJECT.
     H*
     H* 02/23/98 C:85 Project: WC577
     H*               CHANGED THE FIELD MOVED TO CMSMCD
     H*               FROM *BLANKS TO CTSMCD.
     H*
     H* 02/11/98 C:15 Project: WC577
     H*               Chgd pgm to create the 'Auto' claim diaries in
     H*               add mode.
     H*
     H* 02/03/98 C:47 Project: WC577
     H*               Chgd pgm to prevent loop in add mode.
     H*
     H* 11/05/97 C:15 Project: WC577
     H*               Changed the format of date fields used in the
     H*               parm lists for pgms FA003R from MDCY to CYMD.
     H*
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.
     H*    -------------------------------------------------------
     H* ***************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  07 - EDI INSTALLED                                           *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  20 - INVALID CURSOR POSITION FOR SEARCH                      *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  63 - CLAIM WAS CONVERTED FROM ANOTHER SYSTEM                 *
     H*  66 - COMMERCIAL APPLICATION INSTALLED                        *
     H*  67 - SECURITY LEVEL VALID                                    *
     H* N67 - SECURITY LEVEL INVALID                                  *
     H*  69 - EXCESS REINSURANCE                                      *
     H*  70 - BAR CODE INSTALLED                                      *
     H*                                                               *
     H* ***************************************************************
      *
      *
     FW0070F09  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FWMCDL     IF   E           K DISK
      *  MULTIPLE CLAIM FILE
     FWAMCL     IF   E           K DISK
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
      *  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
      *  CLAIM/DARTS CONTROL FILE
     FWMCXL     IF   E           K DISK
      *  CLAIM/EDI CONTROL FILE
      *  EMPLOYER DETAIL FILE
     FWDELL     IF   E           K DISK
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
      *  DIARY TRANSFER FILE
     FWADTL     O    E           K DISK
     F* STAFF CODE EXCEPTION FILE
     FWMSEL     IF   E           K DISK
     FWMDDL     IF A E           K DISK
     FSTCTL1    IF   E           K DISK
     FWAIDL1    IF A E           K DISK
     FWMLBL     IF   E           K DISK
      *
      *  DARTS ARRAY
      *
     D DRT             S             21    DIM(4) CTDATA PERRCD(1)
     D*   SAVED CODES AND UPDATED CODES                                *********
      ***  Beg Chg  ***  05/15/12  **************************************
     D***** SC              S              1    DIM(8)
     d SC              S              1    DIM(9)
      ***  End Chg  ***  05/15/12  **************************************
      ***  Beg Add  ***  05/09/12  **************************************
     d svDNY           s              1
      ***  End Add  ***  05/09/12  **************************************
      *
      *
      ***** Beg Del ***** 03/16/12 ***************************************
     D*****/COPY SCOPYMT01I
      ***** End Del ***** 03/16/12 ***************************************
      *
     D DBFDS           DS
     D  FLSTCM           *STATUS
     D  DBRRN                397    400B 0
      *
     D DATEDS          DS                  INZ
     D  CYMD                   1      8  0
     D  CC1                    1      2  0
     D  CY1                    1      4
     D  YY1                    3      4  0
     D  YMD                    3      8  0
     D  MMDD                   5      8  0
     D  MM                     5      6
     D  DD                     7      8
     D  CC2                    9     10  0
     D  CY2                    9     12
     D  YY2                   11     12  0
     D  MDCY                   5     12  0
      *
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D                 DS                  INZ
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D                 DS
     D  XXDNYN                 1    150
     D  SCDNN1                 1     50
     D  SCDNN2                51    100
     D  SCDNN3               101    150
      ***  Beg Add  ***  05/09/12  **************************************
     d                 DS
     d  svDNYN                 1    150
     d  svDNN1                 1     50
     d  svDNN2                51    100
     d  svDNN3               101    150
      ***  End Add  ***  05/09/12  **************************************
     D*     Selection decision fields for Auto-DARTS API
     D                 DS                  INZ
      ***  Beg Chg  ***  05/15/12  **************************************
     D*****  DSCODZ                 1      8
     D*****  UC                     1      8
     D*****                                DIM(8)
     D DSCODZ                  1      9
     D UC                      1      9
     D                                     DIM(9)
      ***  End Chg  ***  05/15/12  **************************************
     D  CMDNY                  1      1
     D  CMCVFL                 2      2
     D  CMSUBR                 3      3
     D  CMEXCS                 4      4
     D  CMACQR                 5      5
     D  CMOSHA                 6      6
     D  CMSDF                  7      7
     D  CMUSLH                 8      8
      ***  Beg Add  ***  05/15/12  **************************************
     D  CMDTH                  9      9
      ***  End Add  ***  05/15/12  **************************************
     D*
     D*
     D*   Parameters for MC Claim Master API        QMxxxx
     D*
     D PARMCL          DS                  INZ
     D  QMRTCD                 1      1
     D  QMSS#                  2     10
     D  QMACDT                11     17
     D  FILL1                 18     18
     D  QMCO#                 19     21
     D  QMSUB#                22     26
     D  QMFND                 27     29
     D  QMFYR                 30     32
     D  QMCASE                33     39
     D  QMEMP#                40     48
     D  QMDIV                 49     53
     D  QMACTM                54     59
     D  QMDTRP                60     66
     D  FILL2                 67     67
     D  QMDTLW                68     74
     D  FILL3                 75     75
     D  QMACDS                76    135
     D  QMMCON               136    136
     D  QMENTD               137    143
     D  FILL4                144    144
     D  QMRCVD               145    151
     D  FILL5                152    152
     D  QMMMI                153    159
     D  FILL6                160    160
     D  QM1A1                161    161
     D  QM10A1               162    171
     D  QMLTFL               172    172
     D  QMSCL#               173    197
     D  QMMCTM               198    201
     D  QMSTWK               202    203
     D  QMDEDT               204    210
     D  FILL7                211    211
     D  QMDEEM               212    213
     D  QMCMNU               214    216
     D  QMCMSU               217    219
     D  QMCLAD               220    222
     D  QMRRN                223    231  0
     D  QMLSN                232    261
     D  QMFSN                262    276
     D  QMMID                277    277
     D  QMDTRE               278    284
     D  FILL8                282    282
     D  QMCLSU               285    287
     D  QM#DEP               288    290
     D*
      *  DARTS
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS                  INZ
     D  QCCOM1                 1    195
     D  QCFEAD                 1      3
     D  QCUSNM                 5     35
     D  QCDRT1                36     56
     D  QCCNAM                58     87
     D  QCCTY                 89    113
     D  QCDRT2               116    129
     D  QCMO                 131    132
     D  QCSLS1               133    133
     D  QCDY                 134    135
     D  QCSLS2               136    136
     D  QCYR                 137    138
     D                 DS                  INZ
     D  QCCOM2                 1     61
     D  QCDRT3                 1     10
     D  QCUSER                12     61
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D OPTNM           C                   CONST('Claims Management -
     D                                     Information')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D XCBC01          C                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D XCBC02          C                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D #OFSCR          C                   CONST(2)
     D COMAPI          C                   CONST('WA001R')
     D XCMCL           C                   CONST('Multiple claim#:')
     D***** DOTS            C                   CONST(' . . . . . . . . . .')
     D XXADJ           C                   CONST('Adjuster')
      *** BEG ADD ***** 03/16/12 **************************************
     d strMBS          s              1
      *
     d strComm         s            256
     d strCdat         s              8
     d strDcid         s             12
     d strPnam         s             10
     d strSecl         s              3
     d strUser         s             10
     d strDesc         s              3
     d strPgm          s             10
     d strFold         s             80
     d strDifl         s              1
     d strIpgm         s             10
     d strFl01         s              1
     d strFl02         s              1
     d strRmcd         s              5
     d strSeq          s              5
     d strSbyl         s              1
     d strCom2         s             19
     d strApid         s              2
     d strIdoc         s             12
     d strIque         s             10
     d strImgf         s              1
     d strIpth         s             10
     d strPidx         s              5
     d strQpty         s              1
     d strDctp         s              5
     d strCsid         s              2
      *
     D                 DS                  INZ
     D  strKey                 1     40
     D  intCo#                 1      3  0
     D  intFnd                 4      6  0
     D  intFyr                 7      9  0
     D  intCase               10     16  0
     *---------------------------------------------------------------------
     * Prototype Declaration
     *---------------------------------------------------------------------
     d WriteDarts      pr
     d strKey                        40
     d strComm                      256
     d strCdat                        8
     d strDcid                       12
     d strPnam                       10
     d strSecl                        3
     d strUser                       10
     d strPgm                        10
     d strFold                       80
     d strDifl                        1
     d strIpgm                       10
     d strFl01                        1
     d strFl02                        1
     d strRmcd                        5
     d strSeq                         5
     d strSbyl                        1
     d strCom2                       19
     d strApid                        2
     d strIdoc                       12
     d strIque                       10
     d strImgf                        1
     d strIpth                       10
     d strPidx                        5
     d strQpty                        1
     d strDctp                        5
     d strCsid                        2
      *
     d GetDiaryUr      pr            10
     d strDesc                        3
      *** BEG ADD ***** 03/16/12 **************************************
      *** BEG ADD ***** 05/09/12 **************************************
     d intCO##         s              3  0
     d strJRST         s              2
     d strTCD          s              3
     d intFnd#         s              3  0
     d strRtncd        s              1
     d strEdif         s              3
     d strElt          s              1
     d intEltd         s              3  0
     d strFroi         s              2
     d intSdue         s              3  0
     d strRls          s              1
     d intCsdt         s              7  0
     d strEadc         s              5
     d strHcpr         s              5
     d strFnrc         s              5
     d strHced         s              1
      *
     *---------------------------------------------------------------------
     * Prototype Declaration
     *---------------------------------------------------------------------
     d RtvEdiRls       pr                  ExtPgm('WA076R')
     d intCO##                        3  0
     d strJRST                        2
     d strTCD                         3
     d intFnd#                        3  0
     d strRtncd                       1
     d strEdif                        3
     d strElt                         1
     d intEltd                        3  0
     d strFroi                        2
     d intSdue                        3  0
     d strRls                         1
     d intCsdt                        7  0
     d strEadc                        5
     d strHcpr                        5
     d strFnrc                        5
     d strHced                        1
      *** End ADD ***** 05/09/12 **************************************
      *** BEG ADD ***** 05/15/12 **************************************
     d @@Co#           s              3  0
     d @@Fnd           s              3  0
     d @@Fyr           s              3  0
     d @@Case          s              7  0
     d @@Dkey          s             10
     d @@Comm          s            256
     d @@Codz          s             10
     d svCodz          s             10
     *---------------------------------------------------------------------
     * Prototype Declaration
     *---------------------------------------------------------------------
     d WrtDrtRemarks   pr                  ExtPgm('WA700R')
     d @@Co#                          3  0
     d @@Fnd                          3  0
     d @@Fyr                          3  0
     d @@Case                         7  0
     d @@Dkey                        10
     d @@Comm                       256
     d @@Codz                        10
      *** End ADD ***** 05/15/12 **************************************
      * ================================================================
      ***** Beg Add ***** 03/16/12 ***************************************
     C/COPY SCOPYMT05C
      ***** End Add ***** 03/16/12 ***************************************
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE CLAIM/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     CXKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
      *  CLAIM/EDI CONTROL FILE
      *
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XX148             3
     C*
     C*   Key List for the Multiple Claim file
     C*
     C     MCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE           14
     C*
     C*   KEY LIST FOR THE STATE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXCLEX
     C*
     C     UPKY2         KLIST
     C                   KFLD                    XXUSR
     C*
     C*  Key List for the Diary Date file
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*  KEY LIST FOR THE EXCEPTION STAFF CODE FILE
     C*
     C     SEKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXDT                           SYSTEM DT
     C     *LIKE         DEFINE    CMACDT        SVDATE                         CURRENT DATE
     C     *LIKE         DEFINE    CMACTM        SVTIME                         CURRENT TIME
     C     *LIKE         DEFINE    CMACDT        XXACDT                         ACC DT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    CMCLEX        SVCLEX
     C     *LIKE         DEFINE    CMSUPR        SVSUPR
     C     *LIKE         DEFINE    CMAMAN        SVAMAN
     C     *LIKE         DEFINE    CMUMAN        SVUMAN
     C     *LIKE         DEFINE    CMNRSE        SVNRSE
     C     *LIKE         DEFINE    CMFEAD        SVFEAD
     C     *LIKE         DEFINE    CMSDFA        SVSDFA
     C     *LIKE         DEFINE    CMESUP        SVESUP
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    UPUSR         XXUSR
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C     *LIKE         DEFINE    *IN60         XXMANC
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    DDCDAT        XXCDAT
     C     *LIKE         DEFINE    DDMDAT        XXMDAT
     C     *LIKE         DEFINE    DDSDAT        XXSDAT
     C     *LIKE         DEFINE    DDBDAT        XXBDAT
     C     *LIKE         DEFINE    DDUDAT        XXUDAT
     C     *LIKE         DEFINE    DDCDSQ        XXCDSQ
     C     *LIKE         DEFINE    DDMDSQ        XXMDSQ
     C     *LIKE         DEFINE    DDSDSQ        XXSDSQ
     C     *LIKE         DEFINE    DDBDSQ        XXBDSQ
     C     *LIKE         DEFINE    DDUDSQ        XXUDSQ
     C     *LIKE         DEFINE    CMDT          XXCDT
     C     *LIKE         DEFINE    CXRADS        XXRMCD
     C     *LIKE         DEFINE    CMCLEX        XXSTAF
     C     *LIKE         DEFINE    DSDATE        XXRCVD
     C     *LIKE         DEFINE    DDUDAT        XXADAT
     C     *LIKE         DEFINE    DDUDSQ        XXADSQ
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CONT            1
     C*
     C     ACDPL3        PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   PARM                    @@CONT
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
      *
     C     KQPLST        PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    CMCO#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    CMEMP#
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJRST
     C                   PARM                    XX148
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
      *
     C*
      *******Beg Del********03/16/12**********************************
     C*****/COPY SCOPYMT01C
      *******End Del********03/16/12**********************************
     C*
     C*===============================================================
     C* CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
     C*===============================================================
     C*
     C     CALLR         BEGSR
     C*
     C     SCRN#         IFEQ      #OFSCR
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
     C*
     C     @@MODE        IFEQ      'A'
     C                   SELECT
     C     CXMEDI        WHENEQ    'Y'
     C                   CALL      'W0070R10'    ACDPL3                 99
      *
     C     CXBENI        WHENEQ    'Y'
     C                   CALL      'W0070R11'    ACDPL3                 99
      *
     C     CXRPS         WHENEQ    'Y'
     C                   CALL      'W0070R12'    ACDPL3                 99
      *
     C     CXSTSP        WHENEQ    'Y'
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     '148'         XX148
      *
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C     'W0070R'      CAT       IXFROI:0      XXPGM
     C                   ELSE
     C     'W0070R'      CAT       CMJRST:0      XXPGM
     C                   END
      *
     C     2             SUBST(P)  XXPGM:7       XXST              2
     C     XXST          IFNE      *BLANKS
      *
     C                   CALL      XXPGM                                99
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      *BLANK        QQRTCD            1
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *
     C                   ELSE
     C                   MOVE      *ON           *IN99
     C                   ENDIF
      *
     C     *IN99         IFEQ      *ON
     C     CXDBEN        CABEQ     'Y'           TDBEN
     C     CXDART        CABEQ     'Y'           TDART
     C                   ENDIF
      *
     C     CXDBEN        WHENEQ    'Y'
     C     TDBEN         TAG
     C                   MOVE      CMCO#         QQCO#
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
     C*
     C                   CALL      'W0190R'                             99
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACDT            8
     C                   PARM      '1'           QQRTCD
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   END
      *
     C     CXDART        WHENEQ    'Y'
     C     TDART         TAG
     C                   Z-ADD     CMCO#         QCCO#
     C                   Z-ADD     CMFND         QCFND
     C                   Z-ADD     CMFYR         QCFYR
     C                   Z-ADD     CMCASE        QCCASE
     C                   CALL      'SC010R'                             99
     C                   PARM      QCKEY         QQKEY            40
     C                   PARM      'W0070R'      QQPGM            10
     C                   ENDSL
     C                   END                                                    @@MODE = A
      ** **
     C     @@MODE        IFNE      'A'
     C     Q1PRMS        ANDGE     5
     C                   CALL      'W0070R10'    ACDPL3                 99
     C                   END
      *
     C     *INKL         IFEQ      *ON
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
     C*
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
     C*
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C     1             SUBST(P)  Q2RECF:5      SCRN#A            1
     C                   MOVE      SCRN#A        SCRN#
      *
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
      *
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 91 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
     C                   MOVE      '0'           *IN26
     C                   MOVE      '0'           *IN45                          *IN45
     C                   MOVEA     '0000000'     *IN(50)                        *IN50 - 56
     C                   MOVE      '0'           *IN64                          *IN64
     C                   MOVEA     '0'           *IN(95)                           95
      *
     C*  CLAIMS EXAMINER STAFF CODE MUST BE ON FILE
     C*
     C     SCCLEX        IFEQ      *BLANKS
     C                   SETON                                        9076
     C                   LEAVE
     C                   ENDIF
      *
     C     SCCLEX        CHAIN     PUP02                              51
     C     SCCLEX        IFNE      SVCLEX
     C  N51UPSTAT        COMP      'I'                                    51
     C                   ENDIF
     C     *IN51         IFEQ      *ON
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C*
     C*  SUPERVISOR STAFF CODE MUST BE ON FILE
     C*
     C     SCSUPR        IFEQ      *BLANKS
     C                   SETON                                        9071
     C                   LEAVE
     C                   ENDIF
     C*
     C     SCSUPR        CHAIN     PUP02                              52
     C     SCSUPR        IFNE      SVSUPR
     C  N52UPSTAT        COMP      'I'                                    52
     C                   ENDIF
     C     *IN52         IFEQ      *ON
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C*
     C*  ACCOUNT MANAGER STAFF CODE MUST BE ON FILE
     C*
     C     SCAMAN        IFNE      *BLANKS
     C     SCAMAN        CHAIN     PUP02                              73
     C     SCAMAN        IFNE      SVAMAN
     C  N73UPSTAT        COMP      'I'                                    73
     C                   ENDIF
     C     *IN73         IFEQ      *ON
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  CLAIM MANAGER STAFF CODE MUST BE ON FILE
     C*
     C     SCUMAN        IFNE      *BLANKS
     C     SCUMAN        CHAIN     PUP02                              74
     C     SCUMAN        IFNE      SVUMAN
     C  N74UPSTAT        COMP      'I'                                    74
     C                   ENDIF
     C     *IN74         IFEQ      *ON
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C                   END
      *
      *
     C     CLTID         IFEQ      '114'
     C     SCNRSE        ANDEQ     *BLANKS
     C                   SETON                                        39  90
     C                   LEAVE
     C                   ENDIF
      *
     C*
     C*  CASE NURSE STAFF CODE MUST BE ON FILE
     C*
     C     SCNRSE        IFNE      *BLANK
     C     SCNRSE        CHAIN     PUP02                              79
     C     SCNRSE        IFNE      SVNRSE
     C  N79UPSTAT        COMP      'I'                                    79
     C                   ENDIF
     C     *IN79         IFEQ      *ON
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  OUTSIDE ADJUSTER STAFF CODE MUST BE ON FILE
     C*
     C     SCFEAD        IFNE      *BLANK
     C     SCFEAD        CHAIN     PUP02                              45
     C     SCFEAD        IFNE      SVFEAD
     C  N45UPSTAT        COMP      'I'                                    45
     C                   ENDIF
     C     *IN45         IFEQ      *ON
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  SDF ADJUSTER STAFF CODE MUST BE ON FILE
     C*
     C     SCSDFA        IFNE      *BLANK
     C     SCSDFA        CHAIN     PUP02                              77
     C     SCSDFA        IFNE      SVSDFA
     C  N77UPSTAT        COMP      'I'                                    77
     C                   ENDIF
     C     *IN77         IFEQ      *ON
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  EXCESS ADJUSTER STAFF CODE MUST BE ON FILE
     C*
     C     SCESUP        IFNE      *BLANK
     C     SCESUP        CHAIN     PUP02                              78
     C     SCESUP        IFNE      SVESUP
     C  N78UPSTAT        COMP      'I'                                    78
     C                   ENDIF
     C     *IN78         IFEQ      *ON
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  CLAIM ENTRY CLERK STAFF CODE MUST BE ON FILE
     C*
      *
      *  ACTIVITY STATUS & DESC
      *
     C     SCACST        IFNE      *BLANK
     C                   MOVEL(P)  'ACST'        XXTBID
     C                   MOVEL(P)  SCACST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9083
     C                   LEAVE
     C                   ENDIF
     C                   END
      *
      *  MCO FLAG & TYPE
      *
     C     SCMCO         IFNE      'Y'
     C     SCMCO         ANDNE     'N'
     C     SCMCO         ANDNE     ' '
     C                   MOVE      *ON           *IN80
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * IF SCMCO = Y, MUST HAVE A MCO CODE
      *
     C     SCMCO         IFEQ      'Y'
     C                   MOVEL(P)  'MCO '        XXTBID
     C                   MOVEL(P)  SCMCTP        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9084
     C                   LEAVE
     C                   ENDIF
     C                   END
      *
      * IF YOU HAVE A DEFAULTED MCO TYPE (LIKE IN ADD MODE),
      * YOU MUST ALSO ENTER THE MCO FLAG.
      *
     C     SCMCTP        IFNE      *BLANK
     C     SCMCO         ANDEQ     *BLANK
     C                   MOVE      *ON           *IN75
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  CLAIM DENIED
      *
     C     SCDNY         IFNE      'Y'
     C     SCDNY         ANDNE     'N'
     C     SCDNY         ANDNE     ' '
     C                   MOVE      *ON           *IN82
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
      *
     C     CLTID         IFEQ      '065'
     C     SCDNY         ANDNE     'Y'
     C     SCDNY         ANDNE     'N'
     C                   MOVE      *ON           *IN82
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  CONTROVERT FLAG MUST BE YES OR NO
      *
     C     SCCVFL        IFNE      'Y'
     C     SCCVFL        ANDNE     'N'
     C     SCCVFL        ANDNE     ' '
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  SUBROGATION  FLAG MUST BE YES OR NO
      *
     C     SCSUBR        IFNE      'Y'
     C     SCSUBR        ANDNE     'R'
     C     SCSUBR        ANDNE     'N'
     C     SCSUBR        ANDNE     ' '
     C     SCSUBR        ANDNE     'P'
     C                   MOVE      *ON           *IN93
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
      *
      *
      *  EXCESS REINSURANCE FLAG MUST BE YES OR NO
      *
     C     SCEXCS        IFNE      'Y'
     C     SCEXCS        ANDNE     'N'
     C     SCEXCS        ANDNE     ' '
     C                   MOVE      *ON           *IN86
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  SPECIAL DISABILITY FUND FLAG MUST BE YES OR NO
      *
     C     CLTID         IFEQ      '045'
     C     SCSDF         IFNE      'Y'
     C     SCSDF         ANDNE     'N'
     C     SCSDF         ANDNE     ' '
     C     SCSDF         ANDNE     'P'
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C     SCSDF         IFNE      'Y'
     C     SCSDF         ANDNE     'N'
     C     SCSDF         ANDNE     ' '
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   ENDIF
     C*
     C*
     C* IF GROUP 440, THEN CARRIER NUMBER MUST BE '093' OR '094'
     C*
     C     CLTID         IFEQ      '114'
     C     *IN16         ANDEQ     *ON
     C     SCCMPS        ANDNE     '093'
     C     SCCMPS        ANDNE     '094'
     C*** BEG CHG *** 01/05/12 **************************************
     C****************   SETON                                        9390
     C                   SETON                                        0590
     C*** END CHG *** 01/05/12 **************************************
     C                   ENDIF
     C*
     C* IF GROUP 460, THEN CARRIER NUMBER MUST BE '100' OR '101'
     C*
     C     CLTID         IFEQ      '114'
     C     *IN17         ANDEQ     *ON
     C     SCCMPS        ANDNE     '100'
     C     SCCMPS        ANDNE     '101'
     C                   SETON                                        9490
     C                   ENDIF
      *
      *  ACQUIRED FLAG MUST BE YES OR NO
      *
     C     SCACQR        IFNE      'Y'
     C     SCACQR        ANDNE     'N'
     C     SCACQR        ANDNE     ' '
     C                   MOVE      *ON           *IN88
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  OHSA RECORDABLE FLAG MUST BE YES OR NO
      *
     C     SCOSHA        IFNE      'Y'
     C     SCOSHA        ANDNE     'N'
     C     SCOSHA        ANDNE     ' '
     C                   MOVE      *ON           *IN89
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C*
     C*  Medical Benificiary Submission Flag
     C*
     C     EDTMBS        IFEQ      *ON
     C     SCMBS         IFNE      'Y'
     C* * *      SCMBS     ANDNE'N'
     C     SCMBS         ANDNE     ' '
     C                   SETON                                        26  90
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C***  Beg Add  ***  03/16/12  **************************************
      /free

       If (strMBS <> scMBS and scMBS = *blanks);
          Exfmt wdw01;
          If (*inKG = *on);
             If (CXrcsc <> ' ');
                If (CXrcdu <> ' ' or CXrcsu <> ' ');
                   Exsr WriteDiary;
                Endif;                                    // (CXrcdu <> ' ' or CXrcsu <> ' ');
             Endif;                                       // (CXrcsc <> ' ')
             *inKG = *off;
          Else;
             *in90 = *on;
             Leave;
          Endif;                                          // (*in07 = *on)
       Endif;                                          // (strMBS <> scMBS and scMBS = *blanks);

      /end-free
     C***  End Add  ***  03/16/12  **************************************
      *
      *
      *  USL&H EXPOSURE FLAG MUST BE YES, NO OR BLANK
      *
     C     SCUSLH        IFNE      'Y'
     C     SCUSLH        ANDNE     'N'
     C     SCUSLH        ANDNE     ' '
     C                   MOVE      *ON           *IN91
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   ENDDO
      *
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
      *
     C     SCSUBR        IFEQ      'Y'
     C     *INKL         ANDEQ     *OFF
     C     *IN90         ANDEQ     *OFF
     C     SCSUBR        OREQ      'P'
     C     *INKL         ANDEQ     *OFF
     C     *IN90         ANDEQ     *OFF
     C                   CALL      'W0070R17'                           99
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      @@MODE        ##MODE            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
      *
      * the move of the flags happens here, to allow the user time to
      * change the flags and see the new system control info
     C                   MOVEL     SCDNY         ##DNY
     C                   MOVEL     SCDLY         ##DLY
      *
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *INKL
     C                   ENDIF
     C     *INKC         IFEQ      *ON
     C                   EXSR      PGMEND
     C                   ENDIF
     C*
      *
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      XXCO#         QQCO#
      *
     C                   ENDIF
      *
     C     SCSEAE        IFNE      'Y'
     C     SCSEAE        ANDNE     ' '
     C                   MOVE      *ON           *IN54
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C     CLTID         IFEQ      '025'
     C     SCLBEN        IFNE      'Y'
     C     SCLBEN        ANDNE     ' '
     C                   MOVE      *ON           *IN96
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C     SCLBEN        IFEQ      'Y'
     C     DDKY          CHAIN     PLB01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN98
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCLBEN        IFEQ      ' '
     C     DDKY          CHAIN     PLB01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN98
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     1             SCRN#             1 0
     C                   ENDSR                                                  ERRCK1
      *
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
      *
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
      *
      ***  Beg Add  ***  05/09/12  **************************************
      /free

       intCo## = xxCO#;
       strJrst = xxJRST;
       strTcd  = *blank;

        RtvEdiRls(intCo##:strJrst:strTcd:intFnd#:strRtncd:
                   strEdif:strElt:intEltd:strFroi:intSdue:
                   strRls:intCsdt:strEadc:strHcpr:strFnrc:
                   strHced);

       // Full Denial Effective date is mandatory if Claim denied flag is
       // changed from ' ' or 'N' to 'Y'.

       If CLTID = '133' or strRls = '3';

          If *IN90 = *OFF AND @@mode = 'C';
             If (scDNY = 'Y' AND svDNY = ' ')
       // ***  Beg Add  ***  06/13/12  **************************************
             Or (scDNY = 'Y' AND svDNY = 'N');
       // ***  End Add  ***  06/13/12  **************************************
                If scDYEF = *zero;
                   *IN41 = *ON;
                   *IN90 = *ON;
                Else;
                   *IN41 = *OFF;
                   *IN90 = *OFF;
                Endif;
             Endif;
          EndIf;

       EndIf;
      /end-free
      ***  End Add  ***  05/09/12  **************************************
     C*
     C*FULL DENIAL EFFECTIVE DATE
     C  N90SCDYEF        IFNE      *ZEROS
     C                   Z-ADD     SCDYEF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDYEF                                     D
     C     SRERR         IFEQ      '1'
     C                   SETON                                        2290
     C                   ENDIF
     C                   ENDIF
     C*
      ***  Beg Add  ***  05/09/12  **************************************
      /free

       // Full Denial Rescission Date is mandatory if Claim denied flag is
       // changed from 'Y' to 'N' or ' '.

       If CLTID = '133' or strRls = '3';

          If *IN90 = *OFF AND @@mode = 'C';
             If (scDNY = 'N' AND svDNY = 'Y') Or
                (scDNY = ' ' AND svDNY = 'Y');
                If scDRES = *zero;
                   *IN42 = *ON;
                   *IN90 = *ON;
                Else;
                   *IN42 = *OFF;
                   *IN90 = *OFF;
                Endif;
             EndIf;
          EndIf;

       EndIf;
      /end-free
      ***  End Add  ***  05/09/12  **************************************
     C*FULL DENIAL RESCISSION DATE
     C  N90SCDRES        IFNE      *ZEROS
     C                   Z-ADD     SCDRES        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDRES                                     D
     C     SRERR         IFEQ      '1'
     C                   SETON                                        2390
     C                   ENDIF
     C                   ENDIF
     C*
      ***  Beg Add  ***  05/09/12  **************************************
      /free

       // At least one Full Denial Reason should be selected if Claim denied flag is
       // changed from ' ' or 'N' to 'Y'.

       If CLTID = '133' or strRls = '3';

          If *IN90 = *OFF AND @@mode = 'C';
             If (scDNY = 'Y' AND svDNY = ' ')
       // ***  Beg Add  ***  06/13/12  **************************************
             Or (scDNY = 'Y' AND svDNY = 'N');
       // ***  End Add  ***  06/13/12  **************************************
                If (scDNY1 = *blanks and scDNY2 = *blanks and
                    scDNY3 = *blanks and scDNY4 = *blanks and
                    scDNY5 = *blanks);
                   *IN44 = *ON;
                   *IN90 = *ON;
                Else;
                   *IN44 = *OFF;
                   *IN90 = *OFF;
                Endif;
             Endif;
          EndIf;

       EndIf;
      /end-free
      ***  End Add  ***  05/09/12  **************************************
     C*DENIAL CODE 1
     C*
     C  N90SCDNY1        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY1        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        2790
     C                   ENDIF
     C                   ENDIF
     C*
     C*DENIAL CODE 2
     C*
     C  N90SCDNY2        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY2        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        2990
     C                   ENDIF
     C                   ENDIF
     C*
     C*DENIAL CODE 2
     C*
     C  N90SCDNY3        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY3        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        3290
     C                   ENDIF
     C                   ENDIF
     C*
     C*DENIAL CODE 4
     C*
     C  N90SCDNY4        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY4        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        3490
     C                   ENDIF
     C                   ENDIF
     C*
     C*DENIAL CODE 5
     C*
     C  N90SCDNY5        IFNE      *BLANKS
     C                   MOVEL(P)  'DENY'        XXTBID
     C                   MOVEL(P)  SCDNY5        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        3790
     C                   ENDIF
     C                   ENDIF
     C* Partial denial reason code
     C*
     C  N90SCPDNY        IFNE      *BLANKS
     C                   MOVEL(P)  'PDCD'        XXTBID
     C                   MOVEL(P)  SCPDNY        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
      *
      ***  Beg Add  ***  05/09/12  **************************************
      /free

       // Something must be placed or changed in the Denial Reason Narrative if
       // Claim Denied flag changed from ' ' or 'N' to 'Y' or
       // Claim Denied flag changed from 'Y' to 'N or ' '.

       If CLTID = '133' or strRls = '3';

          If *IN90 = *OFF AND @@mode = 'C';
             If (scDNY = 'Y' AND svDNY = ' ') Or
       // ***  Beg Add  ***  06/13/12  **************************************
                (scDNY = 'Y' AND svDNY = 'N') Or
       // ***  End Add  ***  06/13/12  **************************************
                (scDNY = 'N' AND svDNY = 'Y') Or
                (scDNY = ' ' AND svDNY = 'Y');
                If (xxDNYN= *blanks OR xxDNYN = svDNYN);
                   *IN46 = *ON;
                   *IN90 = *ON;
                Else;
                   *IN46 = *OFF;
                   *IN90 = *OFF;
                Endif;
             EndIf;
          EndIf;

       EndIf;
      /end-free
      ***  End Add  ***  05/09/12  **************************************
      *  Claim Denied
      *
     C     SCDNY         IFNE      'Y'
     C     SCDNY         ANDNE     'N'
     C     SCDNY         ANDNE     ' '
     C                   MOVE      *ON           *IN82
     C                   MOVE      *ON           *IN90
     C                   END
      *
      * If Denial Flag = 'Y', then requires a Denial Date, otherwise
      * clear the denial date
      *
     C     SCDNY         IFEQ      'Y'
     C     SCDNDT        IFEQ      *ZEROS
     C     *IN68         ANDEQ     *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCDNDT
     C                   ENDIF
      *
      * Validates the "Denial date"
     C     SCDNDT        IFNE      *ZEROS
     C                   Z-ADD     SCDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                          Date check
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN90                          GEN ERROR IND
     C                   MOVE      *ON           *IN83
     C                   ENDIF
     C                   ENDIF
      *
      *  Claim Delayed
      *
     C     SCDLY         IFNE      'Y'
     C     SCDLY         ANDNE     'N'
     C     SCDLY         ANDNE     ' '
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN90
     C                   END
      *
      * If "Delayed" and the delayed date has not previously been filled
      * calculate the date with "Date Reported to Employer" + 90 days
      *
     C     SCDLY         IFEQ      'Y'
     C     SCDCSN        ANDEQ     *ZEROS
     C                   MOVEL     ##DTRP        SRDATE
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE            8 0
     C                   PARM      90            SRDAYS            5 0
     C                   PARM      *BLANKS       SRERR             1
     C                   Z-ADD     SRDATE        SCDCSN
     C                   ENDIF
      *
      * Validates the "Delayed date"
      *
     C     SCDCSN        IFNE      *ZEROS
     C                   Z-ADD     SCDCSN        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                          Date check
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN90                          GEN ERROR IND
     C                   MOVE      *ON           *IN84
     C                   ENDIF
     C                   ENDIF
      *
      *  S&W/132A Flag
      *
     C     SCSW          IFNE      'Y'
     C     SCSW          ANDNE     'N'
     C     SCSW          ANDNE     ' '
     C                   MOVE      *ON           *IN88
     C                   MOVE      *ON           *IN90
     C                   END
      *
      *  Life Pension
      *
     C     SCLFPN        IFNE      'Y'
     C     SCLFPN        ANDNE     'N'
     C     SCLFPN        ANDNE     ' '
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   END
      *
      *  DWC1 Flag
      *
     C     SCDWC1        IFNE      'Y'
     C     SCDWC1        ANDNE     'N'
     C     SCDWC1        ANDNE     ' '
     C                   MOVE      *ON           *IN86
     C                   MOVE      *ON           *IN90
     C                   END
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     2             SCRN#
      *
      *
      * the move of the flags happens here, to allow the user time to
      * change the flags and see the new system control info
      *
     C*
      *
      *
      *
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
      *
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
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   Z-ADD     3             SCRN#
     C                   ENDSR                                                  ERRCK3
     C*
      *** Beg Add ***** 03/16/12 **************************************
      ****************************************************************
      *  ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04                *
      ****************************************************************
      /Free
       BegSr  Errck4;
       Endsr;
      /End-Free
      ****************************************************************
      *  ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR03                *
      ****************************************************************
      /Free
       BegSr  Errck5;
       Endsr;
      /End-Free
      *** End Add ***** 03/16/12 **************************************
     C*===============================================================
     C* KGDO - SUBROUTINE FOR F7 = FINANCIAL SUMMMARY
     C*===============================================================
     C*
     C*  F7=CLAIMANT FINANCIAL INFO WINDOW
     C*
     C     KGDO          BEGSR
     C*
     C                   CALL      'WA009R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   END
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* KHDO - SUBROUTINE FOR F8 = MULTIPLE CLAIM SEARCH
     C*===============================================================
     C*
     C     KHDO          BEGSR
     C*
     C                   CALL      'W0321R'                             99
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      *BLANK        @@CALL            1
     C                   PARM                    CMCO#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM      MCCLM#        @@CLM#            7 0
     C*
     C     @@KC          IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   END
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
     C                   MOVEA     '0'           *IN(20)
     C                   MOVEA     '0000'        *IN(41)                           41 - 44
     C                   MOVEA     '0000000'     *IN(46)                           46 - 52
     C                   MOVEA     '0000000'     *IN(53)                           53 - 59
     C                   MOVEA     '0'           *IN(64)
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQCALL            1
     C*
     C*   STAFF CODE SEARCH - ADJUSTER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCLEX'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCCLEX
     C                   MOVE      *OFF          *IN64
     C                   MOVE      *ON           *IN55
     C                   ELSE
     C                   MOVE      *ON           *IN64
     C                   MOVE      *OFF          *IN55
     C                   END
     C*
     C*   STAFF CODE SEARCH - SUPERVISOR
     C*
     C     HFLD          WHENEQ    'SCSUPR'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCSUPR
     C                   MOVE      *OFF          *IN55
     C                   MOVE      *ON           *IN53
     C                   ELSE
     C                   MOVE      *ON           *IN55
     C                   MOVE      *OFF          *IN53
     C                   END
     C*
     C*   STAFF CODE SEARCH - ACCOUNT MANAGER
     C*
     C     HFLD          WHENEQ    'SCAMAN'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCAMAN
     C                   MOVE      *OFF          *IN53
     C                   MOVE      *ON           *IN46
     C                   ELSE
     C                   MOVE      *ON           *IN53
     C                   MOVE      *OFF          *IN46
     C                   END
     C*
     C*   STAFF CODE SEARCH - CLAIM MANAGER
     C*
     C     HFLD          WHENEQ    'SCUMAN'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCUMAN
     C                   MOVE      *OFF          *IN46
     C                   MOVE      *ON           *IN47
     C                   ELSE
     C                   MOVE      *ON           *IN46
     C                   MOVE      *OFF          *IN47
     C                   END
     C*
     C*   STAFF CODE SEARCH - CASE NURSE
     C*
     C     HFLD          WHENEQ    'SCNRSE'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCNRSE
     C                   MOVE      *OFF          *IN47
     C                   MOVE      *ON           *IN50
     C                   ELSE
     C                   MOVE      *ON           *IN47
     C                   MOVE      *OFF          *IN50
     C                   END
     C*
     C*   STAFF CODE SEARCH - OUTSIDE ADJUSTER
     C*
     C     HFLD          WHENEQ    'SCFEAD'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCFEAD
     C                   MOVE      *OFF          *IN50
     C                   MOVE      *ON           *IN48
     C                   ELSE
     C                   MOVE      *ON           *IN50
     C                   MOVE      *OFF          *IN48
     C                   END
     C*
     C*   STAFF CODE SEARCH - SDF ADJUSTER
     C*
     C     HFLD          WHENEQ    'SCSDFA'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCSDFA
     C                   MOVE      *OFF          *IN48
     C                   MOVE      *ON           *IN49
     C                   ELSE
     C                   MOVE      *ON           *IN48
     C                   MOVE      *OFF          *IN49
     C                   END
     C*
     C*   STAFF CODE SEARCH - EXCESS ADJUSTER
     C*
     C     HFLD          WHENEQ    'SCESUP'
     C*
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SCESUP
     C                   MOVE      *OFF          *IN49
     C                   MOVE      *ON           *IN41
     C                   ELSE
     C                   MOVE      *ON           *IN49
     C                   MOVE      *OFF          *IN41
     C                   END
     C*
     C     HFLD          WHENEQ    'SCDNY1'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY1
     C                   MOVE      *ON           *IN28
     C                   ELSE
     C                   MOVE      *ON           *IN24
     C                   END
     C*
     C     HFLD          WHENEQ    'SCDNY2'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY2
     C                   MOVE      *ON           *IN31
     C                   ELSE
     C                   MOVE      *ON           *IN28
     C                   END
     C*
     C     HFLD          WHENEQ    'SCDNY3'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY3
     C                   MOVE      *ON           *IN33
     C                   ELSE
     C                   MOVE      *ON           *IN31
     C                   END
     C*
     C     HFLD          WHENEQ    'SCDNY4'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY4
     C                   MOVE      *ON           *IN36
     C                   ELSE
     C                   MOVE      *ON           *IN33
     C                   END
     C*
     C     HFLD          WHENEQ    'SCDNY5'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'DENY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCDNY5
     C                   MOVE      *ON           *IN55
     C                   ELSE
     C                   MOVE      *ON           *IN36
     C                   END
     C*
     C     HFLD          WHENEQ    'SCPDNY'
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM      '2'           QQCALL
     C                   PARM      'PDCD'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCPDNY
     C                   MOVE      *ON           *IN38
     C                   ELSE
     C                   MOVE      *ON           *IN55
     C                   ENDIF
      *
      *  ACTIVTY STATUS
      *
     C     HFLD          WHENEQ    'SCACST'
     C                   MOVEL(P)  'ACST'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN41                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCACST
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCACDS
     C                   MOVE      *ON           *IN44                          POS CUSR
     C                   MOVE      *OFF          *IN41
     C                   END
     C                   END
      *
      *  MCO TYPE
      *
     C     HFLD          WHENEQ    'SCMCTP'
     C                   MOVEL(P)  'MCO '        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN42                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCMCTP
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCMCOD
     C                   MOVE      *ON           *IN43                          POS CUSR
     C                   MOVE      *OFF          *IN42
     C                   END
     C                   END
     C*
     C                   OTHER
     C                   MOVE      *ON           *IN20
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14 = DARTS
     C*===============================================================
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*===============================================================
     C* KQDO - SUBROUTINE FOR F16 = Seasonal Adjustments
     C*===============================================================
      *
     C     KQDO          BEGSR
      *
     C     SCSEAE        IFNE      'Y'
     C                   MOVE      *ON           *IN57
     C                   ELSE
     C*                    CALL 'W0325R'  KQPLST       99
     C                   CALL      'W0325R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   ENDIF
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   END
      *
     C                   ENDSR                                                  KQDO
     C*
      *
     C*===============================================================
     C* KRDO - SUBROUTINE FOR F17= Lifetime Benefits
     C*===============================================================
      *
     C     KRDO          BEGSR
      *
     C     CMLTFL        IFEQ      'I'
     C     SCLBEN        ANDEQ     'Y'
     C                   CALL      'W4344R'
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      '1'           QQRTCD
     C                   ELSE
     C                   SETON                                        97
     C                   ENDIF
      *
     C                   ENDSR
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
      *
     C     *IN40         IFEQ      *ON
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
     C                   MOVEL     CMDCSN        SVDCSN            7 0
      *** BEG ADD ***** 03/16/12 *****************************************
      /free
       strMBS = CMmbs;
      /end-free
      *** END ADD ***** 03/16/12 *****************************************
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C                   Z-ADD     *ZEROS        XXFNDR
     C                   MOVE      *BLANKS       XXEXRE
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
      *  GET THE DARTS SETUP CONTROL RECORD
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
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'BA001R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQBCFL            1
     C                   PARM                    QQTRTP            1
     C                   PARM                    QQTUSR           10
     C                   PARM                    QQRTCD            1
      *
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *BLANK        SCMSG2
     C     QQBCFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN70
     C     QQTRTP        IFEQ      'I'
     C                   MOVEL(P)  XCBC02        SCMSG2
     C                   ELSE
     C                   MOVEL(P)  XCBC01        SCMSG2
     C                   CAT       QQTUSR:1      SCMSG2
     C                   END
     C                   END
     C                   END
      *
     C                   END                                                    *IN60  SSY01
      *
     C                   Z-ADD     CMFND         SCFND
     C                   MOVE(P)   *BLANK        SCFNAM
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
     C*
     C*  Get the EDI control record to determine if the jurisdiction state
     C*    for this claim uses EDI.
     C*
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '148'         XX148
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C  N60              MOVE      'Y'           XXEDI             1
     C   60              MOVE      'N'           XXEDI
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
     C                   PARM      'SSN'         QQTYPE           25
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
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
      *  CLAIMANT NAME
      *
     C                   MOVE      *OFF          *IN81
     C                   CLEAR                   CDMDBN
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN81
     C                   END
     C                   MOVEL     CDMDBN        SCMDBN
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
     C                   END
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
      *
     C                   Z-ADD     CMDYEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDYEF
      *
     C                   Z-ADD     CMDRES        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDRES
      *
     C                   MOVE      CMDNY1        SCDNY1
     C                   MOVE      CMDNY2        SCDNY2
     C                   MOVE      CMDNY3        SCDNY3
     C                   MOVE      CMDNY4        SCDNY4
     C                   MOVE      CMDNY5        SCDNY5
     C                   MOVE      CMDNYN        XXDNYN
     C                   MOVE      CMPDNY        SCPDNY
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN63
     C                   END
      *  FYR-CASE
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
      *
      * LOAD FLEXIBLE FIELD FROM CLAIM/DARTS CONTROL FILE
      *
     C*****              MOVE(P)   DOTS          SCADJ
     C                   MOVE      *BLANKS       SCADJ
     C     CXADJ         IFNE      *BLANK
     C     CXADJ         CAT       ' ':0         SCADJ
     C                   ELSE
     C     XXADJ         CAT       ' ':0         SCADJ
     C                   ENDIF
      *
      * SCREEN DISPLAY RULE: YOU CAN'T HAVE ONLY ONE DOT IN PROMPT TEXT
      *
     C     ' '           CHECKR    CXADJ         XXLEN             3 0
     C     XXLEN         IFEQ      15
     C     XXLEN         OREQ      14
     C                   MOVEL(P)  CXADJ         SCADJ
     C                   END
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DARTS
     C                   MOVEL(P)  CMCLEX        SVCLEX
     C                   MOVEL(P)  CMSUPR        SVSUPR
     C                   MOVEL(P)  CMAMAN        SVAMAN
     C                   MOVEL(P)  CMUMAN        SVUMAN
     C                   MOVEL(P)  CMNRSE        SVNRSE
     C                   MOVEL(P)  CMFEAD        SVFEAD
     C                   MOVEL(P)  CMSDFA        SVSDFA
     C                   MOVEL(P)  CMESUP        SVESUP
     C                   MOVEL(P)  CMCLEX        SCCLEX
     C                   MOVEL(P)  CMSUPR        SCSUPR
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     SEKY1         CHAIN(N)  PSE01                              65
     C     *IN65         IFEQ      *OFF
     C     CMSUPR        IFEQ      *BLANKS
     C                   MOVEL     SELTSP        SCSUPR
     C                   ENDIF
     C     CMCLEX        IFEQ      *BLANKS
     C                   MOVEL     SELTAD        SCCLEX
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMAMAN        SCAMAN
      *
      *** initialize values prior to chain to preclude invalid entries
      *** from previous chains
     C                   MOVE      *BLANKS       SCCEC
     C                   MOVE      *BLANKS       UPSTAF
     C                   MOVEL(P)  CMIUSR        XXUSR
     C     UPKY2         CHAIN     PUP01                              60
     C                   MOVEL(P)  UPSTAF        SCCEC
      *
     C                   MOVEL(P)  CMUMAN        SCUMAN
     C                   MOVEL(P)  CMFEAD        SCFEAD
     C                   MOVEL(P)  CMSDFA        SCSDFA
     C                   MOVEL(P)  CMESUP        SCESUP
     C                   MOVEL(P)  CMNRSE        SCNRSE
      *
      *  ACTIVITY STATUS & DESC
      *
     C                   MOVEL     CMACST        SCACST
     C                   MOVE(P)   *BLANK        SCACDS
     C                   MOVEL(P)  'ACST'        XXTBID
     C                   MOVEL(P)  CMACST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCACDS
     C                   ENDIF
      *
      * MCO FLAG, TYPE, & DESC
      *
     C                   MOVEL     CMMCO         SCMCO
     C                   MOVEL     CMMCTP        SCMCTP
      *
     C     *IN40         IFEQ      *ON
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     ELMCCD        ANDNE     *BLANK
     C                   MOVEL(P)  ELMCCD        SCMCTP
     C                   MOVEL(P)  'Y'           SCMCO
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      *BLANK        SCMCOD
     C                   MOVEL(P)  'MCO '        XXTBID
     C                   MOVEL(P)  SCMCTP        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCMCOD
     C                   ENDIF
      *
     C                   MOVEL     CMDNY         SCDNY
      *
     C                   MOVEL     CMDNY         ##DNY             1
     C                   MOVEL     CMDLY         SCDLY
     C                   MOVEL     CMDLY         ##DLY             1
     C                   MOVEL     CMSW          SCSW
     C                   MOVEL     CMLFPN        SCLFPN
     C                   MOVEL     CMDWC1        SCDWC1
      * Decision date
      *
      * Date reported to employer
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                          CYMD to MDCY
     C                   MOVEL     SRDTO         ##DTRP            8 0
      *
      * Denial date
     C                   Z-ADD     CMDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                          CYMD to MDCY
     C                   MOVEL     SRDTO         SCDNDT
      *
      * If there is already a decision date, then check if user has
      * authority to change it
      *
     C                   SETON                                        92        PROTECTED
     C     ##AUTH        IFEQ      'Y'
     C                   SETOFF                                       92        ALLOW EDIT
     C                   ENDIF
      *
      *
     C                   MOVEL     CMCVFL        SCCVFL
     C                   MOVEL     CMSUBR        SCSUBR
     C*
     C*
     C                   MOVEL     CMEXCS        SCEXCS
     C                   MOVEL     CMSDF         SCSDF
     C                   MOVEL(P)  CMCMPS        SCCMPS
     C                   MOVEL     CMACQR        SCACQR
     C                   MOVEL     CMOSHA        SCOSHA
     C                   MOVEL     CMMBS         SCMBS
     C                   MOVEL     CMUSLH        SCUSLH                         ADD 07/10/00
     C                   Z-ADD     CMCATA        SCCATA
     C                   MOVEL     CMSEAE        SCSEAE
     C*
     C*    Save codes to see if updated.
      ***  Beg Chg  ***  05/15/12  **************************************
     C*****              MOVEL     DSCODZ        SVCODZ            8
      /free
       svCODZ = dsCODZ;
      /end-free
      ***  Beg Chg  ***  05/15/12  **************************************
     C                   MOVEA     DSCODZ        SC
     C*
     C*  LOAD THE SPECIFIC RETENTION FIELD
     C*
     C                   Z-ADD     *ZERO         SCSPRE
     C                   Z-ADD     XXFNDR        SCSPRE
     C*
     C     SCSPRE        IFNE      *ZERO
     C                   MOVE      *ON           *IN58                          PROTECT DISPLAY
     C                   END
     C*
     C*  REINSURER CLAIM #
     C*
     C                   MOVE      *BLANK        SCRECL
     C                   MOVEL     CMRECL        SCRECL
     C*
     C*  MULTIPLE CLAIM #
     C*
     C                   MOVE      *BLANK        SCMCL
     C                   MOVE      *ZERO         X@CASE
     C                   MOVE      XXCASE        X@CASE
     C     MCKY          CHAIN     PMC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     XCMCL         SCMCL
     C                   MOVE      MCCLM#        SCMCL
     C*
     C                   MOVE      *OFF          *IN59
     C     MCCLM#        IFNE      *ZERO
     C                   MOVE      *ON           *IN59                          DISPLAY
     C                   END
     C                   END
     C*
     C                   MOVE      *OFF          *IN68
     C     CMJRST        IFEQ      'CA'
     C                   MOVE      *ON           *IN68
     C                   ENDIF
     C     CMLBEN        IFEQ      *ZEROS
     C                   MOVE      *BLANKS       CMLBEN
     C                   ENDIF
     C     CLTID         IFEQ      '025'
     C                   MOVEL     CMLBEN        SCLBEN
     C                   ENDIF
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
      *
      *  CLAIM MASTER FILE
     C                   MOVEL     SCSCL#        CMACL#
     C                   MOVEL     SCCLEX        CMCLEX
     C                   MOVEL     SCSUPR        CMSUPR
     C                   MOVEL     SCAMAN        CMAMAN
     C                   MOVEL     SCCEC         CMCEC
     C                   MOVEL     SCUMAN        CMUMAN
     C                   MOVEL     SCFEAD        CMFEAD
     C                   MOVEL     SCSDFA        CMSDFA
     C                   MOVEL     SCESUP        CMESUP
     C                   MOVEL     SCNRSE        CMNRSE
      *
     C                   MOVEL     SCACST        CMACST
     C                   MOVEL     SCMCO         CMMCO
     C                   MOVEL     SCMCTP        CMMCTP
      *
     C                   MOVEL     SCDNY         CMDNY
     C                   MOVEL     SCCVFL        CMCVFL
     C                   MOVEL     SCSUBR        CMSUBR
     C                   MOVEL     SCEXCS        CMEXCS
     C                   MOVEL     SCSDF         CMSDF
     C                   MOVEL(P)  SCCMPS        CMCMPS
     C                   MOVEL     SCACQR        CMACQR
     C                   MOVEL     SCOSHA        CMOSHA
     C                   MOVEL     SCMBS         CMMBS
     C                   MOVEL     SCUSLH        CMUSLH                         ADD 07/10/00
     C                   Z-ADD     SCCATA        CMCATA
     C                   MOVEL     SCSEAE        CMSEAE
      *
     C                   MOVEL     SCRECL        CMRECL
      *
      *
     C                   MOVEL(P)  SCDLY         CMDLY
     C                   MOVEL(P)  SCSW          CMSW
     C                   MOVEL(P)  SCLFPN        CMLFPN
     C                   MOVEL(P)  SCDWC1        CMDWC1
      *
     C                   Z-ADD     SCDNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMDNDT
      *
     C                   Z-ADD     SCDCSN        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMDCSN
      *
     C                   Z-ADD     SCDYEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMDYEF
      *
     C                   Z-ADD     SCDRES        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMDRES
      *
     C                   MOVE      SCDNY1        CMDNY1
     C                   MOVE      SCDNY2        CMDNY2
     C                   MOVE      SCDNY3        CMDNY3
     C                   MOVE      SCDNY4        CMDNY4
     C                   MOVE      SCDNY5        CMDNY5
     C                   MOVE      XXDNYN        CMDNYN
     C                   MOVE      SCPDNY        CMPDNY
      *
     C                   MOVEL     *BLANKS       CMLBEN
     C     CLTID         IFEQ      '025'
     C                   MOVEL     SCLBEN        CMLBEN
     C                   ENDIF
     C*  If in add mode and this is a Med only claim, load the default med
     C*    reserves into the claim and write a reserve history record.
      *
     C                   ENDSR                                                  MVSCDB
     C*
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C*
     C     OTHERR        BEGSR
     C*
     C                   ENDSR                                                   OTHERR
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
      *
     C                   MOVEA     '0'           *IN(20)
     C                   MOVE      '0'           *IN45                          *IN45
     C                   MOVEA     '0000000'     *IN(50)                        *IN50 - 56
     C                   MOVE      '0'           *IN64                          *IN64
     C                   MOVE      '0'           *IN57                          *IN57
      *
     C     *INKG         CASEQ     *ON           KGDO                     90
     C     *INKH         CASEQ     *ON           KHDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C     *INKQ         CASEQ     *ON           KQDO                     90
      *
     C     *INKR         CASEQ     *ON           KRDO                     90
     C                   ENDCS
     C*
      *
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      XXCO#         QQCO#
      *
      *
      *
      *
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
      *
      *  DELETE THE CLAIM RECORD IF THE USER PRESSED F3 IN ADD MODE
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   END
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
     C                   RETURN
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
      *
      *  CHECK THE MODE.  A=ADD, C=CHANGE, D=DISPLAY
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           @@MODE
     C                   MOVEL(P)  MODEA         SCMODE
     C                   END
     C*
     C     *IN09         IFEQ      *ON
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
     C*
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
     C*
     C*     WCMC Installed determination
     C*
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
     C*
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   MOVE      *ON           XXMANC
     C                   ELSE
     C                   MOVE      *OFF          XXMANC
     C                   ENDIF
      *
     C                   MOVEL     UPUSNM        QCUSER
     C                   END
      *
      *  GET CURRENT DATE, CONVERT TO 7,0  TO CHAIN TO FILES
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
     C                   MOVE      '0'           *IN45                          *IN45
     C                   MOVEA     '0000000'     *IN(50)                        *IN50 - 56
     C                   MOVE      *ON           *IN64                          POS CUSR
      *
      *  Get Employer Cross Reference info for header
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     EXKY          CHAIN     PEX01                              62
     C  N62              Z-ADD     EXOFND        SCOFND
     C  N62              Z-ADD     EXOEMP        SCOEMP
     C  N62              Z-ADD     EXODIV        SCODIV
      *
      *
      * Check if the user has authority to change the decision date
      *
      *
     C                   MOVE      '1'           SRRES                          Authorized to
     C     CXKY2         CHAIN     PCX01                              99
      *
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ##LVL         SRULVL
     C                   MOVE      CXDSLV        SRCLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXDS01        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM                    SRRES             1
     C                   PARM      CXDS01        SRCTP2            1
     C                   PARM      CXDS02        SRCTP3            1
     C                   PARM      CXDS03        SRCTP4            1
     C                   PARM      CXDS04        SRCTP5            1
     C                   PARM      CXDS05        SRCTP6            1
      *
     C                   ENDIF                                                  *IN99
      *
      *         SRRES = '0' if "Passed" Security Check
      *                 '1' if "Did Not Pass" Security Check
      *
     C                   MOVEL     'N'           ##AUTH            1
     C     SRRES         IFEQ      '0'
     C                   MOVEL     'Y'           ##AUTH
     C                   ENDIF
     C*
     C*
     C* Check security for MBS
     C                   MOVE      ##LVL         SRULVL
     C                   MOVE      CXMBS         SRCLVL
     C*
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXMBS1        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM      *ON           SRRES             1
     C                   PARM      CXMBS2        SRCTP2            1
     C                   PARM      CXMBS3        SRCTP3            1
     C                   PARM      CXMBS4        SRCTP4            1
     C                   PARM      CXMBS5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
     C*
     C                   MOVE      *IN09         *IN25
     C                   MOVE      *ON           EDTMBS            1
     C                   SELECT
     C     SRRES         WHENEQ    *ON
     C                   MOVE      *OFF          EDTMBS
     C                   MOVE      *ON           *IN25                          Do not allow edit
     C                   ENDSL
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C**SET*GROUP 440 INDICATOR
     C*
     C* SET GROUP 440 INDICATOR 16 & 460 INDICATOR 17
     C*
     C                   SETOFF                                       1617
     C     CLTID         IFEQ      '114'
     C*
     C                   SELECT
     C     CMFND         WHENEQ    440
     C                   MOVE      *ON           *IN16
     C     CMFND         WHENEQ    460
     C                   MOVE      *ON           *IN17
     C                   ENDSL
     C*
     C                   ENDIF
      *
      *  GET CLIENT ID
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C* USE *IN12 FOR DISPLAY
     C*
     C     CLTID         COMP      '045'                                  12
     C*
     C     CLTID         COMP      '065'                                  61
     C     CLTID         COMP      '025'                                  30
     C     CMLTFL        IFEQ      'I'
     C     CLTID         ANDEQ     '025'
     C                   SETON                                        35
     C                   ENDIF
      *
      *
     C                   CALL      'S6000R'
     C                   PARM                    @@WCMC            1            WCMC INSTALLED
     C                   PARM                    @@CHK             1            CHECK REC
     C                   PARM                    @@BAR             1            INTRFACD W/WCMS
     C                   PARM                    @@DED             1
     C                   PARM                    @@ADMN            1
     C                   PARM                    @@CLM             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@MICR            1
     C     @@EDI         IFEQ      'Y'
     C                   MOVE      *ON           *IN19
     C                   ELSE
     C                   MOVE      *OFF          *IN19
     C                   ENDIF
      ***  Beg Add  ***  05/09/12  **************************************
      /free
       // Loading the value of Claim Denied Flag and Denial reason narrative
       // in temp varibale.
           svDNY  = cmDNY;
           svDNYN = cmDNYN;
      /end-free
      ***  End Add  ***  05/09/12  **************************************
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             9997
     C*
     C     *IN97         IFEQ      *ON
     C     FLSTCM        ANDEQ     01218
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN99                          SKIP TO BOTTOM
     C                   END
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C*  WRITE DARTS RECORD IF FIELD ADJUSTER WAS CHANGED
     C*
     C                   EXSR      SRRITE
     C*
     C*    Check codes to see if updated, call Auto-DARTS.
     C     SVCODZ        IFNE      DSCODZ
     C     *IN40         ANDEQ     *OFF                                           NOT ADD
     C*
      ***  Beg Chg  ***  05/15/12  **************************************
     C*****              DO        8             C                 3 0
     C                   DO        9             C                 3 0
     C*
     C*****UC(C)         IFEQ      'Y'                                             IS NOW Y
     C*****UC(C)         OREQ      'R'
     C*****SC(C)         ANDNE     'Y'                                             WASN'T
     C*****SC(C)         ANDNE     'R'                                             WASN'T
     C*****              CALL      'WA700R'
     C*****              PARM      CMCO#         @@CO#             3 0
     C*****              PARM      CMFND         @@FND             3 0
     C*****              PARM      CMFYR         @@FYR             3 0
     C*****              PARM      CMCASE        @@CASE            7 0
     C*****              PARM      'CLMCHG03'    @@DKEY           10
     C*****              PARM      *BLANKS       @@COMM          256
      /free
       if UC(C) <> SC(C);
          @@Co#  = CMco#;
          @@Fnd  = CMfnd;
          @@Fyr  = CMfyr;
          @@Case = CMcase;
          @@Dkey = 'CLMCHG03';
          @@Comm = *blanks;
          @@Codz = svCODZ;

          WrtDrtRemarks(@@Co#:@@Fnd:@@Fyr:@@Case:@@Dkey:
                        @@Comm:@@Codz);

      /end-free
      ***  Beg Chg  ***  05/15/12  **************************************
     C                   LEAVE
     C                   ENDIF                                                   UC,C = Y...
     C                   ENDDO                                                   DO 8
     C                   ENDIF                                                   CODZ<>CODZ
     C*  In 'ADD' mode, create Auto claim diaries and Diary Date record.
     C*
     C     *IN40         IFEQ      *ON
      *
     C*
     C*  If a Diary Date record exists, then the Auto Diaries have already
     C*    been created.
     C*
     C     DDKY          CHAIN     PDD01                              60
     C     *IN60         IFEQ      '1'
     C*
     C                   EXSR      SRADRT
     C**** This SR call checks for different conditions not checked
     C**** in another subroutine to see if a diary write is necessary
     C     CLTID         IFEQ      '025'
     C                   EXSR      SRADR2
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         DDCO#
     C                   MOVE      CMFND         DDFND
     C                   MOVE      CMFYR         DDFYR
     C                   MOVE      CMCASE        DDCASE
     C                   Z-ADD     XXCDAT        DDCDAT
     C                   Z-ADD     XXMDAT        DDMDAT
     C                   Z-ADD     XXSDAT        DDSDAT
     C                   Z-ADD     XXBDAT        DDBDAT
     C                   Z-ADD     XXUDAT        DDUDAT
     C                   Z-ADD     XXCDSQ        DDCDSQ
     C                   Z-ADD     XXMDSQ        DDMDSQ
     C                   Z-ADD     XXSDSQ        DDSDSQ
     C                   Z-ADD     XXBDSQ        DDBDSQ
     C                   Z-ADD     XXUDSQ        DDUDSQ
     C                   MOVE      CMCLEX        DDCLEX
     C                   MOVE      *BLANKS       DDSTAT
     C                   MOVE      Q1USER        DDUSER
     C                   MOVE      Q1PGM         DDPGM
     C                   Z-ADD     XXDT          DDDT
     C                   TIME                    DDTIME
     C                   WRITE     PDD01
     C*
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#
     C                   PARM      CMFND         @@FND
     C                   PARM      CMFYR         @@FYR
     C                   PARM      CMCASE        @@CASE
     C                   PARM      'NEWCLAIM'    @@DKEY
     C                   PARM      *BLANKS       @@COMM
     C*
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ENDIF                                                  *IN40 = *ON
     C*  Create the claim index suspense record if in ADD mode, the
     C*    claim index DARTS remark code is not blank, and the suspense
     C*    record does not already exist.
     C*
     C     CLTID         IFEQ      '025'
     C     DDKY          CHAIN     PID01                              60
     C     *IN60         IFEQ      *ON
     C     *IN40         ANDEQ     *ON
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
     C                   ENDIF
     C*
     C*    Call API to update Managed Care Claim Master - FMCMP
     C*
     C     XXMANC        IFEQ      *ON
     C                   CLEAR                   PARMCL
     C                   EXSR      MCAPIC
     C                   ENDIF                                                  MANC ON     DATE
     C*
     C*  Decision date was changed, Create the Adjuster Comp diary
      *
     C     CMDCSN        IFNE      SVDCSN
     C                   MOVE      XXDT          XXCDAT
     C                   MOVE      XXCDAT        XXCDT
     C                   MOVE      CMCLEX        XXSTAF
     C                   MOVE      CXDSRC        XXRMCD
     C                   EXSR      SRDDRY
     C                   Z-ADD     CMSEQ#        XXCDSQ
     C                   ENDIF
     C*
     C*
     C*  WRITE TO DIARY TRANSFER FILE IF A STAFF CODE WAS CHANGED
     C*
     C     *IN40         IFEQ      *OFF
     C                   EXSR      SRDTRN
     C                   END
     C*
     C     CLTID         IFEQ      '114'
      *
      * Write Diary remark for initial entry of Case Nurse staff code
      *
     C     *IN40         IFEQ      *ON
     C     SVNRSE        ANDEQ     *BLANKS
     C     SCNRSE        ANDNE     *BLANKS
      *
     C                   MOVEL(P)  QCKEY         #1KEY            40
     C                   MOVE      DSDATE        #1CDAT
     C                   MOVEL(P)  'F0269C1'     #1DCID           12
     C                   MOVEL(P)  QCKEY         #1FOLD           80
      *
     C     SCNRSE        CHAIN     PUP02                              60
      *
     C                   CALL      'DA700R'
     C                   PARM                    #1KEY            40
     C                   PARM      *BLANKS       #1COMM          256
     C                   PARM                    #1CDAT            8
     C                   PARM                    #1DCID           12
     C                   PARM      'W0070R'      #1PNAM           10
     C                   PARM      *BLANKS       #1SECL            3
     C                   PARM      UPUSR         #1USER           10
     C                   PARM      Q1PGM         #1PGM            10
     C                   PARM                    #1FOLD           80
     C                   PARM      'Y'           #1DIFL            1
     C                   PARM      Q1PGM         #1IPGM           10
     C                   PARM      'Y'           #1FL01            1
     C                   PARM      'N'           #1FL02            1
     C                   PARM      'MCIRF'       #1RMCD            5
     C                   PARM      *BLANKS       #1SEQ#            5
      *
     C                   ENDIF                                                  SVNRSE
     C                   ENDIF
      *
     C*
     C*  If this pgm was called using option 9 and EDI is active, call
     C*    the EDI Log API.
     C*
     C     WCFL09        IFEQ      'Y'
     C     XXEDI         ANDEQ     'Y'
     C     SCRN#         ANDEQ     #OFSCR
     C*
     C                   MOVE      XXCO#         QACO#
     C                   MOVE      CMFND         QAFND
     C                   MOVE      CMFYR         QAFYR
     C                   MOVE      CMCASE        QACASE
     C                   CALL      'W0070R6'
     C                   PARM                    QACO#             3
     C                   PARM                    QAFND             3
     C                   PARM                    QAFYR             3
     C                   PARM                    QACASE            7
     C                   PARM      *BLANK        QQACTF            1
     C                   PARM      *BLANKS       QQMGID            7
     C                   PARM                    QQRTCD            1
     C*
     C                   END
      *
      * CALL NEXT PROGRAM IF F6, OPT 2, OR 5 WAS SELECTED FROM CLM MASTER
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   EXSR      CALLR
     C                   END
     C*
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   ENDSR                                                  WRITER
      *
     C* ***************************************************************
     C*  SRDTRN - IF THE SUPERVISOR, CLAIMS EXAMINER, FIELD ADJUSTOR,
     C*    OR UNIT MANAGER IS CHANGED, THIS SUBROUTINE WILL WRITE A
     C*    RECORD TO THE DIARY TRANSFER FILE.  THIS FILE IS USED TO
     C*    DRIVE A BATCH PROGRAM WHICH TRANSFERS DARTS DIARIES FROM THE
     C*    OLD USER TO THE NEW USER.
     C* ***************************************************************
     C     SRDTRN        BEGSR
     C*
     C                   Z-ADD     XXCO#         DTCO#
     C                   Z-ADD     XXFND         DTFND
     C                   Z-ADD     CMFYR         DTFYR
     C                   Z-ADD     CMCASE        DTCASE
     C                   MOVEL     Q1USER        DTUSER
     C                   MOVEL     Q1PGM         DTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DTTIME
     C                   Z-ADD     XXDT          DTDT
     C* ADJUSTER
     C     SVCLEX        IFNE      SCCLEX
     C     SVCLEX        ANDNE     *BLANKS
     C                   MOVE      'C'           DTCTGY
     C                   MOVE      *BLANKS       DTOSTF
     C                   MOVE      *BLANKS       DTNSTF
     C                   MOVEL     SVCLEX        DTOSTF
     C                   MOVEL     SCCLEX        DTNSTF
     C                   WRITE     PDT01
     C                   END
     C* SUPERVISOR
     C     SVSUPR        IFNE      SCSUPR
     C     SVSUPR        ANDNE     *BLANKS
     C                   MOVE      'S'           DTCTGY
     C                   MOVE      *BLANKS       DTOSTF
     C                   MOVE      *BLANKS       DTNSTF
     C                   MOVEL     SVSUPR        DTOSTF
     C                   MOVEL     SCSUPR        DTNSTF
     C                   WRITE     PDT01
     C                   END
     C* ACCOUNT MANAGER
     C     SVAMAN        IFNE      SCAMAN
     C     SVAMAN        ANDNE     *BLANKS
     C                   MOVE      'A'           DTCTGY
     C                   MOVE      *BLANKS       DTOSTF
     C                   MOVE      *BLANKS       DTNSTF
     C                   MOVEL     SVAMAN        DTOSTF
     C                   MOVEL     SCAMAN        DTNSTF
     C                   WRITE     PDT01
     C                   END
     C* CLAIM (UNIT) MANAGER
     C     SVUMAN        IFNE      SCUMAN
     C     SVUMAN        ANDNE     *BLANKS
     C                   MOVE      'U'           DTCTGY
     C                   MOVE      *BLANKS       DTOSTF
     C                   MOVE      *BLANKS       DTNSTF
     C                   MOVEL     SVUMAN        DTOSTF
     C                   MOVEL     SCUMAN        DTNSTF
     C                   WRITE     PDT01
     C                   END
     C* NURSE
     C     SVNRSE        IFNE      SCNRSE
     C     SVNRSE        ANDNE     *BLANKS
     C                   MOVE      'N'           DTCTGY
     C                   MOVE      *BLANKS       DTOSTF
     C                   MOVE      *BLANKS       DTNSTF
     C                   MOVEL     SVNRSE        DTOSTF
     C                   MOVEL     SCNRSE        DTNSTF
     C                   WRITE     PDT01
     C                   END
     C* OUTSIDE (FIELD) ADJUSTER
     C     SVFEAD        IFNE      SCFEAD
     C     SVFEAD        ANDNE     *BLANKS
     C                   MOVE      'F'           DTCTGY
     C                   MOVE      *BLANKS       DTOSTF
     C                   MOVE      *BLANKS       DTNSTF
     C                   MOVEL     SVFEAD        DTOSTF
     C                   MOVEL     SCFEAD        DTNSTF
     C                   WRITE     PDT01
     C                   END
     C* SDF ADJUSTER
     C     SVSDFA        IFNE      SCSDFA
     C     SVSDFA        ANDNE     *BLANKS
     C                   MOVE      'D'           DTCTGY
     C                   MOVE      *BLANKS       DTOSTF
     C                   MOVE      *BLANKS       DTNSTF
     C                   MOVEL     SVSDFA        DTOSTF
     C                   MOVEL     SCSDFA        DTNSTF
     C                   WRITE     PDT01
     C                   END
     C* EXCESS ADJUSTER (OR SUPERVISOR)
     C     SVESUP        IFNE      SCESUP
     C     SVESUP        ANDNE     *BLANKS
     C                   MOVE      'E'           DTCTGY
     C                   MOVE      *BLANKS       DTOSTF
     C                   MOVE      *BLANKS       DTNSTF
     C                   MOVEL     SVESUP        DTOSTF
     C                   MOVEL     SCESUP        DTNSTF
     C                   WRITE     PDT01
     C                   END
     C*
     C                   ENDSR
      *
     C*
     C****************************************************************
     C*  SRADRT - This subroutine creates the Claim Auto Diaries.
     C****************************************************************
     C     SRADRT        BEGSR
     C*
     C*  Initialize work fields.
     C*
     C                   Z-ADD     0             XXCDAT
     C                   Z-ADD     0             XXMDAT
     C                   Z-ADD     0             XXSDAT
     C                   Z-ADD     0             XXBDAT
     C                   Z-ADD     0             XXUDAT
     C                   Z-ADD     0             XXCDSQ
     C                   Z-ADD     0             XXMDSQ
     C                   Z-ADD     0             XXSDSQ
     C                   Z-ADD     0             XXBDSQ
     C                   Z-ADD     0             XXUDSQ
     C*
     C*  Convert dates needed by the auto diaries.
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                          CYMD to MDCY
     C                   MOVEL     SRDTO         XXRCVD
     C*
     C*  Create the Adjuster Comp diary.
     C*  Create the State Report diary.
     C*
     C*  If this is a medical only claim, create the Adj Medical diary.
     C*
     C     CXRADS        IFNE      *BLANKS
     C     CMLTFL        ANDEQ     'M'
     C*
     C                   MOVE      DSDATE        SRDATE            8 0
     C                   Z-ADD     CXBADJ        SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXMDAT
     C*
     C                   MOVE      XXMDAT        XXCDT
     C                   MOVE      CMCLEX        XXSTAF
     C                   MOVE      CXRADS        XXRMCD
     C                   EXSR      SRDDRY
     C   21              Z-ADD     CMSEQ#        XXMDSQ
     C                   ENDIF
     C*
     C*  If this is an indemnity claim, create the Supervisor and
     C*    Manager diaries.
     C*
     C     CMLTFL        IFEQ      'I'
      ** Removal of this check allows diary write for all claims.
     C     CLTID         OREQ      '025'
     C***
     C     CXRSSD        IFNE      *BLANKS
     C*
     C     XXRCVD        IFEQ      0
      **** Use only system date.
     C     CLTID         OREQ      '025'
     C                   MOVE      DSDATE        SRDATE
     C                   ELSE
     C                   MOVE      XXRCVD        SRDATE
     C                   ENDIF
     C                   Z-ADD     CXBSPV        SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDAT
     C*
     C                   MOVE      XXSDAT        XXCDT
     C                   MOVE      CMSUPR        XXSTAF
     C                   MOVE      CXRSSD        XXRMCD
     C                   EXSR      SRDDRY
     C   21              Z-ADD     CMSEQ#        XXSDSQ
     C                   ENDIF
     C***
     C     CXRUMD        IFNE      *BLANKS
     C*
     C     XXRCVD        IFEQ      0
      **** Use only system date.
     C     CLTID         OREQ      '025'
     C                   MOVE      DSDATE        SRDATE
     C                   ELSE
     C                   MOVE      XXRCVD        SRDATE
     C                   ENDIF
     C                   Z-ADD     CXBMGR        SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXUDAT
     C*
     C                   MOVE      XXUDAT        XXCDT
     C                   MOVE      CMUMAN        XXSTAF
     C                   MOVE      CXRUMD        XXRMCD
     C                   EXSR      SRDDRY
     C   21              Z-ADD     CMSEQ#        XXUDSQ
     C                   ENDIF
     C*
     C                   ENDIF
     C* Check parameter to write Adjuster diary entry
     C***
     C     CXRAD2        IFNE      *BLANKS
     C     CLTID         ANDEQ     '025'
     C                   MOVE      DSDATE        SRDATE
     C                   Z-ADD     CXBAD2        SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXADAT
     C*
     C                   MOVE      XXADAT        XXCDT
     C                   MOVE      CMCLEX        XXSTAF
     C                   MOVE      CXRAD2        XXRMCD
     C                   EXSR      SRDDRY
     C   21              Z-ADD     CMSEQ#        XXADSQ
     C                   ENDIF
     C*
     C                   ENDSR
     C****************************************************************
     C*  SRADR2 - This subroutine creates the Claim Auto Diaries.
     C*           It writes auto diary remarks for Adjuster AND
     C*           Supervisor in ADD MODE and other conditions apply
     C****************************************************************
     C     SRADR2        BEGSR
     C*
     C*
     C*  Convert Date Reported (CMDTRP) fot auto diaries
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                          CYMD to MDCY
     C                   MOVEL     SRDTO         XXRCVD
     C*
     C*** Check parameters to write Supervisory diary
     C*
     C     CXAFA         IFNE      *BLANKS
     C     CMLTFL        ANDNE     'R'
     C                   MOVE      XXRCVD        SRDATE
     C                   Z-ADD     CXAFAD        SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDAT
     C                   MOVE      XXSDAT        XXCDT
     C                   MOVE      CMSUPR        XXSTAF
     C                   MOVE      CXAFA         XXRMCD
     C                   EXSR      SRDDRY
     C   21              Z-ADD     CMSEQ#        XXSDSQ
     C                   ENDIF
     C***
     C* Check parameter to write Adjuster diary entry
     C***
     C     CXAFA         IFNE      *BLANKS
     C     CMLTFL        ANDNE     'R'
     C                   MOVE      XXRCVD        SRDATE
     C                   Z-ADD     CXAFAD        SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXADAT
     C*
     C                   MOVE      XXADAT        XXCDT
     C                   MOVE      CMCLEX        XXSTAF
     C                   MOVE      CXAFA         XXRMCD
     C                   EXSR      SRDDRY
     C   21              Z-ADD     CMSEQ#        XXADSQ
     C                   ENDIF
     C                   ENDSR
     C****************************************************************
     C*  SRDDRY - This subroutine writes DARTS Diary records.
     C****************************************************************
     C     SRDDRY        BEGSR
     C*
     C                   MOVE      *OFF          *IN21
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     XXFYR         QCFYR
     C                   Z-ADD     XXCASE        QCCASE
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C     XXSTAF        CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C     XXSTAF        ANDNE     *BLANKS
     C     XXRMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  CTDESC        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     XXCDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  UPUSR         Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  QCKEY         Q#FOLD           80
     C                   ELSE                                                   DCID <> blanks
     C                   MOVE      *BLANKS       Q#FOLD
     C                   ENDIF                                                  DCID <> blanks
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE(P)   'Y'           Q#FL01            1
     C                   ELSE                                                   DCID <> blanks
     C                   MOVE      *BLANKS       Q#FL01
     C                   ENDIF                                                  DCID <> blanks
     C     CTDPRG        IFGT      0
     C                   MOVE(P)   'Y'           Q#FL02            1
     C                   ELSE                                                   DPRG > 0
     C                   MOVE      *BLANKS       Q#FL02
     C                   ENDIF                                                  DPRG > 0
     C                   MOVEL(P)  XXRMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
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
     C                   MOVE      Q#SEQ#        CMSEQ#            5 0
     C*
     C                   MOVE      *ON           *IN21
     C                   ENDIF                                                  *IN60  PCT01
     C                   ENDIF                                                  *IN60  PUP02
     C*
     C                   ENDSR
     C* **************************************************************
     C*  SRRITE - SUBROUTINE TO WRITE DARTS
     C* **************************************************************
     C     SRRITE        BEGSR
     C*
     C     SCCLEX        IFNE      SVCLEX
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     XXFYR         QCFYR
     C                   Z-ADD     XXCASE        QCCASE
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY
     C                   MOVEL     *BLANKS       Q#CSID
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         Q#CDAT
     C                   MOVEL     *BLANKS       Q#DCID
     C                   MOVEL(P)  'W0070R'      Q#PNAM
     C                   MOVE      *ZEROS        Q#SECL
     C                   MOVEL(P)  'N'           Q#SBYL
     C                   MOVEL(P)  Q1USER        Q#USER
     C                   MOVEL(P)  Q1PGM         Q#PGM
     C                   MOVE      *BLANKS       Q#FOLD
     C                   MOVEL     *BLANKS       Q#APID
     C                   MOVEL     *BLANKS       Q#IDOC
     C                   MOVEL     *BLANKS       Q#IQUE
     C                   MOVEL     *BLANK        Q#IMGF
     C                   MOVEL     *BLANKS       Q#IPTH
     C                   MOVE      *ZEROS        Q#PIDX
     C                   MOVEL     *BLANK        Q#QPTY
     C                   MOVEL     *BLANKS       Q#DCTP
     C                   MOVEL(P)  Q1PGM         Q#IPGM
     C                   MOVE      *BLANKS       QCCOM1
     C                   MOVEL     DRT(1)        QCDRT1
     C                   MOVE      *BLANKS       QCCNAM           30
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        QCCNAM
     C                   MOVEL     CDCTY         QCCTY
     C                   MOVEL     DRT(2)        QCDRT2
     C                   MOVE      '/'           QCSLS1
     C                   MOVE      '/'           QCSLS2
     C                   CLEAR                   DATEDS
     C                   Z-ADD     CMACDT        CYMD
     C                   MOVE      CY1           QCYR
     C                   MOVE      MM            QCMO
     C                   MOVE      DD            QCDY
     C                   MOVEL     DRT(3)        QCDRT3
     C     SVCLEX        IFNE      *BLANKS
     C     CXRFFA        ANDNE     *BLANKS
     C                   MOVE      SVCLEX        XXCLEX            3
     C     UPKY1         CHAIN     PUP02                              60
     C                   MOVE      XXCLEX        QCFEAD
     C  N60              MOVEL     UPUSNM        QCUSNM
     C                   MOVEL(P)  CXRFFA        Q#RMCD
     C                   MOVEL(P)  QCCOM1        Q#COMM
     C                   MOVE      QCCOM2        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2
     C     CXRFFA        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTFL01        Q#DIFL
     C                   MOVEL(P)  CTDEPG        Q#DCID
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE(P)   'Y'           Q#FL01
     C                   ELSE                                                   DCID <> blanks
     C                   MOVE(P)   *BLANKS       Q#FL01
     C                   ENDIF                                                  DCID <> blanks
     C     CTDPRG        IFGT      0
     C                   MOVE(P)   'Y'           Q#FL02
     C                   ELSE                                                   DPRG > 0
     C                   MOVE(P)   *BLANKS       Q#FL02
     C                   ENDIF                                                  DPRG > 0
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD
     C                   ENDIF                                                  DCID <> blanks
     C                   ENDIF                                                  *IN60 = Off
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL     *BLANKS       Q#CODE            2
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
     C                   MOVE      Q#SEQ#        CMSEQ#            5 0
     C                   END
     C*
     C     SCCLEX        IFNE      *BLANKS
     C     CXRTFA        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       QCFEAD
     C                   MOVE      *BLANKS       QCUSNM
     C                   MOVE      SCCLEX        XXCLEX
     C     UPKY1         CHAIN     PUP02                              60
     C                   MOVE      XXCLEX        QCFEAD
     C  N60              MOVEL     UPUSNM        QCUSNM
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  CXRTFA        Q#RMCD
     C                   MOVEL(P)  QCCOM1        Q#COMM
     C                   MOVE      QCCOM2        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2
     C     CXRTFA        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTFL01        Q#DIFL
     C                   MOVEL(P)  CTDEPG        Q#DCID
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE      'Y'           Q#FL01
     C                   ELSE                                                   DCID <> blanks
     C                   MOVE      *BLANKS       Q#FL01
     C                   ENDIF                                                  DCID <> blanks
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           Q#FL02
     C                   ELSE                                                   DPRG > 0
     C                   MOVE      *BLANKS       Q#FL02
     C                   ENDIF                                                  DPRG > 0
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD
     C                   ENDIF                                                  DCID <> blanks
     C                   ENDIF                                                  *IN60 = *Off
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
     C                   MOVE      Q#SEQ#        CMSEQ#            5 0
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*===============================================================
     C* MCAPIC - DATA INTERFACE: MC CLAIM MASTER - FMCMP
     C*===============================================================
     C*
     C     MCAPIC        BEGSR
     C*
     C                   MOVE      CMACDT        QMACDT
     C*
     C                   MOVE      CMEMP#        QMEMP#
     C                   MOVE      CMDIV         QMDIV
     C*
     C                   MOVE      XXCO#         QMCO#
     C                   MOVE      *BLANKS       QMSUB#
     C                   MOVE      XXFND         QMFND
     C                   MOVE      XXFYR         QMFYR
     C                   MOVE      XXCASE        QMCASE
     C*
     C                   MOVE      CMACTM        QMACTM                         Accidnt time
     C                   MOVE(P)   CMDTRP        QMDTRP                         Reported
     C                   MOVE(P)   CMDTLW        QMDTLW                         Last worked
     C                   MOVE(P)   CMENTD        QMENTD                         ENTERED
     C                   MOVE(P)   CMRCVD        QMRCVD                         Received
     C                   MOVE(P)   CMMMI         QMMMI                          Max med impr
     C                   MOVE(P)   CMDOD         QMDEDT                         Death date
     C     CMDTH         IFEQ      'Y'                                          Death flag
     C                   MOVEL     'DI'          QMDEEM
     C                   ELSE
     C                   MOVEL     *BLANKS       QMDEEM
     C                   ENDIF
     C*
     C                   Z-ADD     DBRRN         QMRRN
     C*
     C                   MOVE(P)   CMDTRE        QMDTRE                         Rehabilitation
     C                   MOVE      CM#DEP        QM#DEP                         # dependents
     C                   MOVEL(P)  CMSHFT        QQWKPT                         # dependents
     C*
     C*=====================================================================
     C                   CALL      'FA003R'
     C*
     C                   PARM      *BLANK        QMRTCD            1
     C                   PARM      CMSS#         QMSS#             9
     C                   PARM                    QMACDT            7
     C*
     C                   PARM                    QMCO#             3
     C                   PARM      *BLANK        QMSUB#            5
     C                   PARM                    QMFND             3
     C                   PARM                    QMFYR             3
     C                   PARM                    QMCASE            7
     C*
     C                   PARM                    QMEMP#            9
     C                   PARM                    QMDIV             5
     C                   PARM                    QMACTM            6
     C                   PARM                    QMDTRP            7
     C                   PARM                    QMDTLW            7
     C                   PARM      CMACDS        QMACDS           60            Acc description
     C                   PARM      CMMCON        QMMCON            1            Method contact
     C                   PARM                    QMENTD            7
     C                   PARM                    QMRCVD            7
     C                   PARM                    QMMMI             7
     C                   PARM      *BLANKS       QM1A1             1            Unavailable
     C                   PARM      *BLANKS       QM10A1           10            Unavailable
     C                   PARM      CMLTFL        QMLTFL            1            Lost time flag
     C                   PARM      CMACL#        QMSCL#           25            State claim numbr
     C                   PARM      *BLANKS       QMMCTM            4            Unavailable
     C                   PARM      *BLANKS       QMSTWK            2            Unavailable
     C                   PARM                    QMDEDT            7
     C                   PARM                    QMDEEM            2            Death claim flag
     C                   PARM      CMNRSE        QMCMNU            3            Nurse
     C                   PARM      *BLANKS       QMCMSU            3            Unavailable
     C                   PARM      CMCLEX        QMCLAD            3            Claim examiner
     C                   PARM                    QMRRN             9 0
     C                   PARM      CMLSN         QMLSN            30            Last name
     C                   PARM      CMFSN         QMFSN            15            First name
     C                   PARM      CMMID         QMMID             1            Middle initial
     C                   PARM                    QMDTRE            7
     C                   PARM      CMSUPR        QMCLSU            3            Supervisor
     C                   PARM                    QM#DEP            3
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
     C                   PARM      CMFEAD        QQCLFA            3            FEAD
     C                   PARM      CMDNY         QQDNY             1            DNY
     C                   PARM      CMIMP         QQIMP             1            IMPAIRMENT
     C                   PARM      CMJRST        QQJRST            2            JUR STATE
     C*
     C*
     C                   ENDSR
     C*===============================================================
      *** Beg Add *** 03/16/12 **************************************
      /free

        Begsr WriteDiary;

        strKey = *blanks;

        intCo#  = %dec(qqco#:3:0);
        intFnd  = %dec(qqfnd:3:0);
        intFyr  = %dec(qqfyr:3:0);
        intCase = %dec(qqcase:7:0);

        strCdat = %editc(dsDate:'X');

        If (CXrcdu <> ' ');

           Select;
           When CXrcdu = '01';
              strUser = GetDiaryUr(CMclex);

           When CXrcdu = '02';
              strUser = GetDiaryUr(CMsupr);

           When CXrcdu = '03';
              strUser = GetDiaryUr(CMaman);

           When CXrcdu = '04';
              strUser = GetDiaryUr(CMaman);

           when CXrcdu = '05';
              strUser = GetDiaryUr(CMnrse);

           When CXrcdu = '06';
              strUser = GetDiaryUr(CMfead);

           When CXrcdu = '07';
              strUser = GetDiaryUr(CMsdfa);

           When CXrcdu = '08';
              strUser = GetDiaryUr(CMesup);

           Endsl;

        Else;

           If (CXrcsu <> ' ');

              Chain CXrcsu pup02;
              If %found;
                 strUser = Upusr;
              Endif;

           Endif;

        Endif;

        Chain CXrcsc pct01;
        If %found;

           strComm = CTdesc;
           strDCID = CTdepg;
           strDIFL = CTfl01;

           If (strDCID <> ' ');
              strFold = strKey;
              strFl01 = 'Y';
           Else;
              strFold = *blanks;
              strFl01 = *blank;
           Endif;

           If (Ctdprg <> 0);
              strFl02 = 'Y';
           Else;
              strFl02 = *blank;
           Endif;

        Endif;

        strRmcd = CXrcsc;
        strPnam = 'W0070R';
        strSecl = *blanks;
        strPgm  = q1Pgm;
        strIpgm = q1Pgm;
        strSeq  = '00000';
        strSbyl = 'N';
        strCom2 = *blanks;
        strApid = *blanks;
        strIdoc = *blanks;
        strIque = *blanks;
        strImgf = *blanks;
        strIpth = *blanks;
        strPidx = *blanks;
        strQpty = *blanks;
        strDctp = *blanks;
        strCsid = *blanks;

        // WrtDart(strKey:strComm:strCdat:strDcid:strPnam:
        WriteDarts(strKey:strComm:strCdat:strDcid:strPnam:
                   strSecl:strUser:strPgm:strFold:strDifl:
                   strIpgm:strFl01:strFl02:strRmcd:strSeq:
                   strSbyl:strCom2:strApid:strIdoc:strIque:
                   strImgf:strIpth:strPidx:strQpty:strDctp:
                   strCsid);

        Endsr;

      /end-free

     P GetDiaryUr      B

     d GetDiaryUr      PI            10
     d  strDesc                       3
      *
     d strUr           s             10
      /free

       Chain strDesc pup02;

       If %found (WMUPL1);
          strUr = Upusr;
       Else;
          strUr = *blanks;
       Endif;

       return strUr;

      /end-free

     P GetDiaryUr      E
      *** End Add *** 03/16/12 **************************************
      *
**  TABLE DRT
CLAIMANT NAME & CITY:
ACCIDENT DATE:
USER NAME:
OLD CLAIMANT NAME:
