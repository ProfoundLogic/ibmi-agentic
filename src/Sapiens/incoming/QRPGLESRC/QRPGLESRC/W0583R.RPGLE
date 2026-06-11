     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0583R - Work With Requested Payments
     H*****************************************************************
     H*  DESCRIPTION:
     H*    This program will display a list of Requested Payments by  *
     H*    FEIN# or FEIN Name for a selected Claim.                   *
     H*        Mode 'N' used to Pend Request                          *
     H*        Mode 'R' used to Release Request                       *
     H*        Mode 'G' used to toggle "Group" flag RPPGTP            *
     H*        Mode 'E' used to Change EFT flag                       *
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
     H* 11/01/16 C:136 Support: 43408                                 *
     H*                Change Opt24 to 'Chg EFT to Non-EFT'           *
     H*                                                               *
     H* 01/15/13 C:65  Project: WC2116/Redmine# 2101                  *
     H*                Added handler to screen file.                  *
     H*                                                               *
     H* 04/17/12 C:191 Project: WC2044                                *
     H*                new added EFT flag and process accordlingly.   *
     H*                                                               *
     H* 10/06/10 C:062 SUPPORT 74420
     H*                DON'T CHANGE GROUP TYPE IF AMOUNT IS NEGATIVE.
     H*                                                               *
     H* 09/04/09 C:234 SUPPORT: WC1453 PRISS#12                       *
     H*                Throw error message if option 24 is selected   *
     H*                on a claim that does not have EFT set up, or   *
     H*                if the change is un-successful.                *
     H*                                                               *
     H* 07/10/08 C:136 SUPPORT: WA114 63406                           *
     H*                CORRECT CODING FOR OPTION 22 EDIT. THE         *
     H*                USER AND LEVEL CONTROL FLAGS WERE REVERSED.    *
     H*                                                               *
     H* 04/01/08 C:191  Project: 139MIV                               *
     H*                 Handle new edit for negative total paid on    *
     H*                 the reserve. Change in DSPF only              *
     H*                                                               *
     H* 08/09/07 C:191 Project: WC1453                                *
     H*                Add option 24=Change EFT Status Option not     *
     H*                allowed if the EFT control Flag is not a 'Y'   *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 01/25/07 C:183 PROJECT: 114WLY
     H*                Added a PARM ##RTCD to accept the exact dupl-  *
     H*                cate indicator. If this returns as '1' from    *
     H*                WA019R then call SRCCSC to check whether the   *
     H*                user has got authority to release the payment. *
     H*                                                               *
     H* 03/07/02 C:53 SUPPORT: WC065/31359                            *
     H*               CORRECTED SUBSET ISSUE WHICH IGNORED MODIFIER   *
     H*                                                               *
     H* 12/12/01 C: 53 PROJECT: WC1060
     H*                ADD EXCLUDE MEDICAL PROCESSING.
     H*                                                               *
     H* 01/19/01 C:53  Support: WC025/25829                           *
     H*                Added consideration for modifier in search.    *
     H*                                                               *
     H* 01/02/01 C:15  Project: WC577                                 *
     H*                Added code to prevent the dup pmt edit from    *
     H*                being skipped.                                 *
     H*                                                               *
     H* 06/12/00 C:62  PROJECT: MC230                                 *
     H*                SWAP OUT FA015R USAGE WITH FA031R, FA032R,     *
     H*                FA033R AND FA034R. CHANGED TO F3040R.          *
     H*                                                               *
     H* 05/31/00 C:53  Project: WC461                                 *
     H*                Add Daily batch lockout process                *
     H*                                                               *
     H* 05/30/00 C:53  Project: WC991                                 *
     H*                Add parms for WA013R to facilitate edit # 12   *
     H*                                                               *
     H* 02/09/00 C:118 SUPPORT: MC048/21651
     H*                CLEAR SCSEOB BEFORE SUBSETTING.
     H*
     H* 05/27/99 C:15 Support: WC025/18265                            *
     H*               Added the duplicate pmt edit.  This will be     *
     H*               invoked when a pmt is released.                 *
     H*                                                               *
     H* 03/26/99 C:15 Support: WC051/16635                            *
     H*               Fix pgm to prevent pmts from being released     *
     H*               when the claim reserves are less than the pmt.  *
     H*                                                               *
     H* 03/22/99 C:94 PROJECT: SUP17239
     H*               IF NO EOMB SEND MESSAGE
     H*
     H* 03/15/99 C:94 PROJECT: 25MKL
     H*               ADD RECON NUMBER TO INFORMATION FIELDS
     H*
     H* 03/11/99 C:94 PROJECT: MC025 SUP 17048
     H*               ADD AND CHANGED PARMS
     H*
     H* 01/23/99 C:94 PROJECT: 25MKL                                  *
     H*               IF PROVIDER BILLS IS INSTALLED THEN CALL F0340R *
     H*               OTHERWISE STILL CALL F7200C1                    *
     H*                                                               *
     H* 04/30/98 C:15 Project: WC577                                  *
     H*               Added 5 addl parms to SRCCSC.  These parms are  *
     H*               addl security types.                            *
     H*                                                               *
     H*  01/07/98 C:15 Project: WC577                                 *
     H*               -Chgd pgm to use API's to retreive WCMC data    *
     H*                instead of referencing WCMC files in the pgm.  *
     H*               -Do not display option 15, 16 or 80 if WCMC is  *
     H*                not installed.                                 *
     H*                                                               *
     H*  11/18/97 c:30 Support:                                       *
     H*           Reverse Image goes away after Display return.       *
     H*           *IN89 change to *IN44.                              *
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
     FW0583F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *__________________________________________________________
      *  Request for Payment Physical (for refreshing subfile record)
     FWARPP     IF   E             DISK
     F                                     RENAME(PRP01:PFRCDF)
      *__________________________________________________________
      *  Request for Payment Logical  (for displaying items)
     FWARPLD    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PRP01:DBRCDF)
      *__________________________________________________________
      *  Table File logical
     FWTABL     IF   E           K DISK
      *__________________________________________________________
      *  User Parameter logical
     FWMUPL     IF   E           K DISK
      *__________________________________________________________
      *  Payee Demographics
     FWMPDL     IF   E           K DISK
      *__________________________________________________________
      *  "Group" Provider file
     FWMNRL     IF   E           K DISK
      *__________________________________________________________
      *  Claims/DARTS Control file
     FWMCXL     IF   E           K DISK
      *__________________________________________________________
     F*  WCMC Group Master file
     F*  WCMC EOB Master file
      *__________________________________________________________
      *  Payment Status Table file
     FWTPSL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *__________________________________________________________
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
      *
     D I6              S              6    DIM(63)                              Indicator Values
     D WK              S              1    DIM(80)                              Indicator Values
      *
      *
      *
     D                 DS
     D  FIELD                  1     10
     D  FLDNAM                 1      5
     D  FLDNUM                 6      6  0
      *
      *  Payment Status
      *
     D                 DS
     D  PS                     1     10
     D                                     DIM(5)                               Payment Status
     D  SCSPS1                 1      2
     D  SCSPS2                 3      4
     D  SCSPS3                 5      6
     D  SCSPS4                 7      8
     D  SCSPS5                 9     10
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
     D  SNAME                  1     18
     D  SFEIN#                 1      9
     D  SMOD                  12     14
     D  SEOB#                 20     28
     D  SRCN#                 30     32
     D  SEFT                  38     38
     D  SFDT                  20     29
     D  STDT                  31     40
     D  SAMT                  43     55
     D  SDT                   57     66
     D  SRC                   69     70
     D  SSTAT                 73     74
      *
      *  SCCS02 - Field Definitions for Claim Heading Line 2
      *
     D                 DS
     D  SCCS02                 1     79
     D  S2SS1                 26     28
     D  S2SS2                 30     31
     D  S2SS3                 33     36
     D  S2NAME                39     60
      *
      *  Claimant Social Security Number
      *
     D                 DS
     C***  Begin Chg  ***  06/09/20  *******************************
     D* SSN                    1      9  0
     D  SSN                    1      9
     C***  End hg  ***  06/09/20  *******************************
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Requests for Payment')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0583R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
     D DELETD          C                   CONST('DELETED')
      *================================================================
      *  PLIST definitions
      *================================================================
      *

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
     C                   PARM                    @@SFDT            8
     C                   PARM                    @@STDT            8
     C                   PARM                    @@SPC            10            Payment Codes
     C                   PARM                    @@STT             5            Transaction Type
     C                   PARM                    @@SPV1           10            Payment Value 1
     C                   PARM                    @@SRC            10            Reserve Codes
     C                   PARM                    @@SFEI            9            Federal ID #
     C                   PARM                    @@SCHK            7            Check #
     C                   PARM                    @@SEOB            9            EOMB #
     C                   PARM                    @@SIRQ           10            Initial Requestor
     C                   PARM                    @@SGRP            1            Group vs. Non/Group
     C                   PARM                    @@SMOD            3            modifier
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
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #ACO#             3
     C                   PARM                    #AFND             3
     C                   PARM                    #AFYR             3
     C                   PARM                    #ACASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  Group Provider Key
      *
     C     NRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
      *
      *  Requested Payment Key
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSORT
     C                   KFLD                    KKRCVD
     C                   KFLD                    KKSEQ#
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
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
      *  Payee Demographics
      *
     C     PDKY          KLIST
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
      *
      *  Claims/DARTS Control file
      *
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     CMKY          KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*  KEY LIST FOR THE EOMB FILE
     C*  KEY LIST FOR THE SYSTEM CONTROL FILES
     C*
     C     SYKY          KLIST
     C                   KFLD                    XSKEY
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    RPCO#         KKCO#
     C     *LIKE         DEFINE    RPFND         KKFND
     C     *LIKE         DEFINE    RPFYR         KKFYR
     C     *LIKE         DEFINE    RPCASE        KKCASE
     C     *LIKE         DEFINE    RPSORT        KKSORT
     C     *LIKE         DEFINE    RPRCVD        KKRCVD
     C     *LIKE         DEFINE    RPSEQ#        KKSEQ#
     C     *LIKE         DEFINE    WCKEY         XSKEY
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
     C     PS(X)         ORNE      *BLANK
     C     TT(X)         ORNE      *BLANK
     C     PV1(X)        ORNE      *BLANK
     C     RC(X)         ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C                   ENDDO
      *
     C     SCSFEI        IFNE      *ZERO
     C     SCSEOB        ORNE      *ZERO
     C     SCSFDT        ORNE      *ZERO
     C     SCSTDT        ORNE      *ZERO
     C     SCSIRQ        ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF
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
      * EDTRES - Payment/Reserve Edits
      *================================================================
     C     EDTRES        BEGSR
      *
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
      *
     C                   CALL      'WA011R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM      *BLANK        ##RC              2
      *
     C     ##RC          IFEQ      *BLANK
     C                   CALL      'WA012R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCTRCD
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    ##RC
     C                   PARM                    ##RECD            2            Reserve Code
     C                   PARM                    ##RETP            1            Reserve Type
     C                   PARM                    ##TYPE            1            Transaction Type
     C                   ENDIF
      *
     C     ##RC          IFEQ      *BLANK
     C                   CALL      'WA013R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCEOB#
     C                   PARM                    SCSEQ#
     C                   PARM                    SCAMT
     C                   PARM                    SCRECD
     C                   PARM                    SCRETP
     C                   PARM                    ##RC
     C                   PARM                    SCAMT
     C                   PARM                    XXEOBS
     C                   PARM                    SCTRCD
     C                   ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      * ERRCKC - Errors on Position To field
      *================================================================
     C     ERRCKC        BEGSR
     C                   ENDSR
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
     C*
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C     SCSEL         OREQ      '21'
     C     SCSEL         OREQ      '22'
     C     SCSEL         OREQ      '23'
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD            1
     C     ##RTCD        IFEQ      'F'
     C                   MOVE      *ON           *IN90
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF
      *
     C                   SELECT
      *          ______________________________________________________
     C     SCSEL         WHENEQ    '4'
      *
      *  1)  Invalid Option
     C     SCSTAT        IFEQ      'D'
     C                   SETON                                        9080      "4"  INVALID
     C                   ENDIF
      *
      *  Access Claims/DARTS Control file
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9081      Not Found
      *
     C                   ELSE
      *
      *  2)  User security level and type DELETE check
      *
     C                   MOVE      UPTYPE        SRUTYP            1            User Type
     C                   MOVE      UPLVL         SRULVL            3            User Level
     C                   MOVE      CXDPPT        SRCTYP            1            Del Payment type
     C                   MOVE      CXDLTD        SRCLVL            3            Del Payment Level
     C                   MOVE      '1'           SREXEC            1            Execution Type
     C                   MOVE      *ON           SRRES             1            Result Flag
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM                    SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM                    SREXEC
     C                   PARM                    SRRES
     C                   PARM      CXDPP2        SRCTP2            1
     C                   PARM      CXDPP3        SRCTP3            1
     C                   PARM      CXDPP4        SRCTP4            1
     C                   PARM      CXDPP5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *
     C     SRRES         IFEQ      *ON
     C                   SETON                                        9082      No Delete Security
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCEOB#        ANDGT     *ZERO
      *
      *  3)  User security level and type DELETE EOB check
      *
     C                   MOVE      UPTYPE        SRUTYP            1            User Type
     C                   MOVE      UPLVL         SRULVL            3            User Level
     C                   MOVE      CXCKDT        SRCTYP            1            Del EOB pay type
     C                   MOVE      CXCKDL        SRCLVL            3            Del EOB pay Level
     C                   MOVE      '1'           SREXEC            1            Execution Type
     C                   MOVE      *ON           SRRES             1            Result Flag
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM                    SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM                    SREXEC
     C                   PARM                    SRRES
     C                   PARM      CXCKD2        SRCTP2
     C                   PARM      CXCKD3        SRCTP3
     C                   PARM      CXCKD4        SRCTP4
     C                   PARM      CXCKD5        SRCTP5
     C                   PARM      ' '           SRCTP6
      *
     C     SRRES         IFEQ      *ON
     C                   SETON                                        9083      No EOB Delete Secur
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
      *          ______________________________________________________
     C     SCSEL         WHENEQ    '15'
     C     SCSEL         OREQ      '16'
     C     SCSEL         OREQ      '80'
      *
     C     SCEOB#        IFLE      *ZERO
     C                   SETON                                        9084      NO EOMB#
     C                   ENDIF
      *          ______________________________________________________
     C     SCSEL         WHENEQ    '21'
      *
     C     SCSTAT        IFNE      *BLANK
     C     SCSTAT        IFEQ      'D'
     C                   SETON                                        9086      "21" INV-DELETED
     C                   ELSE
     C                   SETON                                        9077      "21" INVALID
     C                   ENDIF
     C                   ENDIF
      *          ______________________________________________________
     C     SCSEL         WHENEQ    '22'
      *
     C     SCSTAT        IFNE      'P'
     C     SCSTAT        IFEQ      'D'
     C                   SETON                                        9085      "21" INV-DELETED
     C                   ELSE
     C                   SETON                                        9078      "22" INVALID
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN97
     C     SCRETP        IFEQ      'M'
     C                   MOVEL(P)  SSTAT         XXKEY
     C*  CHAIN TO GET THE EXCLUDE MEDICAL FLAG.
     C     CMKY          CHAIN     PCM01                              60
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     ELEXMF        IFEQ      'Y'
     C                   SETON                                        90  97
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF
      *
     C                   EXSR      EDTRES
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ENDIF
     C*
     C*  Check for dup or possible dup pmts.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDUPF        ANDNE     'Y'
     C*
     C                   MOVEA(P)  SCCS01        WK
     C                   MOVEA(P)  WK(12)        QQDFND
     C                   MOVEA(P)  SCCS02        WK
     C                   MOVEA(P)  WK(39)        XX24A            24
     C                   MOVEL(P)  XX24A         QQDCNM
     C                   MOVEL(P)  SCPNAM        QQDPNM
     C*
     C                   CALL      'WA019R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCSEQ#
     C                   PARM                    SCFRDT
     C                   PARM                    SCTODT
     C                   PARM                    SCAMT
     C                   PARM                    SCRECD
     C                   PARM                    SCFEIN
     C                   PARM                    QQDFND           51            Fund Name
     C                   PARM                    QQDCNM           36            Claimant Name
     C                   PARM                    QQDPNM           36            Provider Name
     C     *IN90         PARM      *ZERO         I90               1
     C                   PARM                    SCRETP
     C                   PARM                    SCMOD
     C                   PARM                    ##RTCD            1
     C*
      *  Access Claims/DARTS Control file
     C     CXKY          CHAIN     PCX01                              60
      *
     C     ##RTCD        IFEQ      '1'
     C     *IN60         ANDEQ     '0'
     C*
     C*  Security level and Type check
      *
     C                   MOVE      UPTYPE        SRUTYP                         User Type
     C                   MOVE      UPLVL         SRULVL                         User Level
     C                   MOVE      CXDPT1        SRCTYP                         Del Payment type
     C                   MOVE      CXDPLV        SRCLVL                         Del Payment Level
     C                   MOVE      '1'           SREXEC                         Execution Type
     C                   MOVE      *ON           SRRES                          Result Flag
      *
     C*
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM                    SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM                    SREXEC
     C                   PARM                    SRRES
     C                   PARM      CXDPT2        SRCTP2
     C                   PARM      CXDPT3        SRCTP3
     C                   PARM      CXDPT4        SRCTP4
     C                   PARM      CXDPT5        SRCTP5
     C                   PARM      ' '           SRCTP6
     C*
     C     SRRES         IFEQ      '1'
     C                   SETON                                        9087
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *ON
     C                   MOVE      'Y'           SCDUPF
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *          ______________________________________________________
     C     SCSEL         WHENEQ    '23'
      *
     C     SCPGTP        IFNE      'G'
     C     SCSTAT        ANDNE     'D'
     C     NRKY          CHAIN     PNR01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9079      "23" INVALID
     C                   ENDIF
     C                   ENDIF
     C*
     C* DON'T ALLOW CHANGE OF GROUP TYPE IF AMOUNT IS NEGATIVE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCAMT         ANDLT     0
     C                   SETON                                        8990
     C                   ENDIF
      *
     C                   ENDSL
      *
     C     ENDCK1        ENDSR
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                           F10 Search
     C                   ENDCS
      *
      *  Edit 'Payment Status' if entered
      *
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     1             X                 3 0
     C                   Z-ADD     *ZERO         STR               3 0
     C                   Z-ADD     *ZERO         Y                 3 0
     C                   DO        5             X
     C     PS(X)         IFNE      *BLANK
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PS(X)         XXKEY
     C     TBKEY         CHAIN     PPS01                              60
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
     C*
     C*
     C                   MOVE      *BLANKS       PS                             Payment Status
     C                   MOVE      *BLANKS       SCSFDT                         Service FROM Date
     C                   MOVE      *BLANKS       SCSTDT                         Service TO Date
     C                   MOVE      *BLANKS       PC                             Payment Codes
     C                   MOVE      *BLANKS       TT                             Transaction Type
     C                   MOVE      *BLANKS       PV1                            Payment Value 1
     C                   MOVE      *BLANKS       RC                             Reserve Codes
     C                   MOVE      *BLANKS       SCSFEI                         Federal ID#
     C                   MOVE      *BLANKS       SCSEOB                         EOMB #
     C                   MOVE      *BLANKS       SCSIRQ                         Initial Requestor
     C                   MOVE      *BLANKS       SCSGRP                         Group/Non Group
     C*
     C     Q1PRMS        IFGT      7
     C                   MOVEA     @@SPS         PS                             Payment Status
     C                   MOVE      @@SFDT        SCSFDT                         Service FROM Date
     C                   MOVE      @@STDT        SCSTDT                         Service TO Date
     C                   MOVEA     @@SPC         PC                             Payment Codes
     C                   MOVEA     @@STT         TT                             Transaction Type
     C                   MOVEA     @@SPV1        PV1                            Payment Value 1
     C                   MOVEA     @@SRC         RC                             Reserve Codes
     C                   MOVE      @@SFEI        SCSFEI                         Federal ID#
     C                   MOVE      @@SEOB        SCSEOB                         EOMB #
     C                   MOVE      @@SIRQ        SCSIRQ                         Initial Requestor
     C                   MOVE      @@SGRP        SCSGRP                         Group/Non Group
     C     Q1PRMS        IFGE      20
     C                   MOVE      @@SMOD        SCSMOD                         Modifier
     C                   ENDIF
     C*
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
     C*
     C                   ENDIF                                                  Q1PRMS>7
     C*
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
     C                   MOVE      *BLANK        KKSORT
     C                   MOVE      *ZERO         KKRCVD
     C                   MOVE      *ZERO         KKSEQ#
      *
     C                   ENDSR
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
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
     C                   MOVE      SCSORT        KKSORT
     C                   MOVE      SCRCVD        KKRCVD
     C                   MOVE      SCSEQ#        KKSEQ#
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
     C     FLDNAM        WHENEQ    'SCSPS'
     C                   CALL      'W2142R'
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
     C                   MOVEL     ##KEY         PS(X)                          Payment Status
      *
     C     X             IFEQ      5
     C                   MOVEA     '0010'        *IN(86)                        Position Cursor
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
     C     HFLD          OREQ      'SCMOD'                                      Mod Search
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
     C     HFLD          WHENEQ    'SCSEOB'
     C                   SETON                                        90
     C                   MOVEA     '1000'        *IN(86)
      *_________________________________________________________________
     C     HFLD          WHENEQ    'SCSIRQ'
     C                   CALL      'SC530R'
     C                   PARM                    SCSIRQ
     C                   PARM                    QQRTCD            1
     C                   SETON                                        90
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
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      *BLANK        SLIN1
     C                   MOVE      RPCO#         SCCO#
     C                   MOVE      RPFND         SCFND
     C                   MOVE      RPFYR         SCFYR
     C                   MOVE      RPCASE        SCCASE
     C                   MOVE      RPSEQ#        SCSEQ#
     C                   MOVE      RPRCVD        SCRCVD
     C                   MOVE      RPSORT        SCSORT
     C                   MOVE      RPSTAT        SCSTAT
     C                   MOVE      RPPGTP        SCPGTP
     C                   MOVE      RPFEIN        SCFEIN
     C                   MOVE      RPMOD         SCMOD
     C                   MOVE      RPEOB#        SCEOB#
     C                   Z-ADD     RPEOBS        SCRCN
     C*** BEG CHG *** 04/17/12 ****************************************
     C********RPCKTP        IFEQ      'EFT'
     C                   IF        RPEFTC = 'E'
     C*** END CHG *** 04/17/12 ****************************************
     C                   MOVE      'Y'           SCEFT
     C                   ELSE
     C                   MOVE      ' '           SCEFT
     C                   ENDIF                                                  RPCKTP = EFT'
     C                   MOVE      RPEOBS        XXEOBS            3 0          ADD 05/30/00
     C                   MOVE      RPTRCD        SCTRCD
     C                   MOVE      RPRECD        SCRECD
     C                   MOVE      RPRETP        SCRETP
     C                   MOVE      RPAMT         SCAMT
     C                   MOVE      RPPNAM        SCPNAM
     C                   Z-ADD     RPFRDT        SCFRDT
     C                   Z-ADD     RPTODT        SCTODT
     C                   MOVE      *BLANK        SCDUPF
      *
      *  Build Subfile Record based on INFO# (F9=Info Display)
      *
     C                   SELECT
      *          _______________________________________________________
B01  C     INFO#         WHENEQ    1
      *
     C     RPPNAM        IFNE      *BLANKS
     C                   MOVEL     RPPNAM        SNAME
      *
     C                   ELSE
      *
      *  Assign Name as either Claimant or Payee
      *
     C                   MOVE      S2SS1         SS1
     C                   MOVE      S2SS2         SS2
     C                   MOVE      S2SS3         SS3
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE1          10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     SSN           PARM      SSN           QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
     C***  Begin Chg  ***  06/09/20  *******************************
     C*    RPFEIN        IFNE      *ZERO
     C*    RPFEIN        ANDNE     SSN
     C                   IF        RPFEIN <> *ZERO AND %CHAR(RPFEIN) <>SSN
     C***  End Chg  ***  06/09/20  *******************************
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
     C                   ENDIF                                                  RPPNAM NE *BLANK
      *
      *  Assign Service "From" and "To" Dates
      *
     C                   Z-ADD     RPFRDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL     XXNUM         SFDT
      *
     C                   Z-ADD     RPTODT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL     XXNUM         STDT
      *
      *  Assign Receive Date
      *
     C                   Z-ADD     RPRCVD        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL     XXNUM         SDT
      *
     C                   MOVE      RPRECD        SRC
      *          _______________________________________________________
X01  C     INFO#         WHENEQ    2
      *
     C     RPFEIN        IFGT      *ZERO
     C                   MOVE      RPFEIN        SFEIN#
     C                   MOVE      RPMOD         SMOD
     C                   ELSE
     C                   MOVEL     RPPNAM        SNAME
     C                   ENDIF
      *
     C     RPEOB#        IFGT      *ZERO
     C                   MOVE      RPEOB#        SEOB#
     C                   ENDIF
     C     RPEOBS        IFGT      *ZERO
     C                   MOVE      RPEOBS        SRCN#
     C                   ENDIF
     C                   MOVE      SCEFT         SEFT
      *
      *  Assign Posting Date
      *
     C                   Z-ADD     RPTRDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL     XXNUM         SDT
      *
     C                   MOVE      RPBR2         SRC
      *
     C                   ENDSL
      *          _______________________________________________________
      *
     C                   MOVEL(P)  RPAMT         XXNUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL     XXNUM         SAMT
      *
     C                   MOVE      *OFF          *IN44
     C     RPPGTP        IFEQ      'G'
     C                   MOVE      *ON           *IN44
     C                   ENDIF
      *
     C                   MOVE      RPSTS         SSTAT
     C                   MOVE      SLIN1         SCDATA
      *
     C                   ENDSR
      *
      *===============================================================
      * OPTEOB - Subroutine for Option 15 (Reprint EOB)
      *          and Option 16 (Display EOB)
      *===============================================================
     C     OPTEOB        BEGSR
      *
     C*  CHAIN TO THE WCMC FUND X-REF FILE A GET THE SUB# AND GRP#
     C*  CHAIN TO THE EOMB FILE TO VALIDATE THE EOMB.
     C*   EOMB# not found
     C*
     C*  Verify that the pmt has a valid EOMB attached.
     C*
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA031R'
1    C                   PARM      KKCO#         ##CO#             3 0          Company #
2    C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
3    C                   PARM      SCFYR         ##FYR             3 0          Policy Year
4    C                   PARM      SCCASE        ##CASE            7 0          Claim #
5    C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
6    C                   PARM      SCRCN         ##RCN             3 0          RECON #
7    C                   PARM      *BLANKS       ##RTCD            1            Return Code
     C*
     C                   ELSE
     C*
     C                   CALL      'FA032R'
     C                   PARM      KKCO#         ##CO#             3 0          Company #
     C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C                   PARM      SCFYR         ##FYR             3 0          Policy Year
     C                   PARM      SCCASE        ##CASE            7 0          Claim #
     C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
     C                   PARM      *BLANKS       ##RTCD            1            Return Code
     C*
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      '1'
     C     ##RTCD        OREQ      '2'
     C     ##RTCD        OREQ      '5'
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
      *
      *  CALL PROGRAM TO DISPLAY/REPRINT THE EOMB.
      *
     C                   MOVE      KKCO#         QQCO#
     C                   MOVE      SCEOB#        QQEOB#
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      ##SUB#        QQSUB#
     C                   MOVE      ##GRP#        QQGRP#
      *
     C     SCSEL         IFEQ      '16'
     C                   MOVE      '1'           QQMODE
     C                   ELSE
     C                   MOVE      'D'           QQMODE
     C                   END
      *
     C     ##PRVB        IFEQ      'Y'
     C     SCSEL         IFEQ      '15'
     C                   MOVE      'P'           #1MODE
     C                   MOVEL     UPOUTQ        #1QUE
     C                   ELSE
     C                   MOVE      'D'           #1MODE
     C                   MOVE      *BLANKS       #1QUE
     C                   ENDIF
     C*
     C*
     C                   CALL      'FA033R'
1    C                   PARM      KKCO#         ##CO#             3 0          Company #
2    C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
3    C                   PARM      SCFYR         ##FYR             3 0          Policy Year
4    C                   PARM      SCCASE        ##CASE            7 0          Claim #
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
     C                   MOVE      ##RRN         #1RRN
     C*
     C     ##RTCD        IFEQ      *BLANK
     C                   CALL      'F0340C'
     C                   PARM                    #1RRN             9
     C                   PARM      ##AST         #1AST             2
     C                   PARM                    #1MODE            1
     C                   PARM                    #1QUE            10
     C                   ELSE
     C                   SETON                                        9084      NO EOMB#
     C                   ENDIF
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
      *
     C                   ENDSR
      *
      *===============================================================
      * OPT21 - Subroutine for Option 21 (Pend Request)
      *===============================================================
     C     OPT21         BEGSR
      *
     C                   MOVE      'N'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR
      *
      *===============================================================
      * OPT22 - Subroutine for Option 22 (Release Request)
      *===============================================================
     C     OPT22         BEGSR
      *
     C                   EXSR      EDTRES
     C*
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   ELSE
      *
     C                   MOVE      'R'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      *===============================================================
      * OPT23 - Subroutine for Option 23 (Change Group Type)
      *===============================================================
     C     OPT23         BEGSR
      *
     C                   MOVE      'G'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR
      *===============================================================
      * OPT24 - Subroutine for Option 24 (Change EFT Status)
      *===============================================================
     C     OPT24         BEGSR
      *
     C                   MOVE      'F'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C     ##MODE        IFEQ      'X'
     C                   SETON                                        9088      "24" INVALID
     C                   MOVE      'F'           ##MODE
     C                   ENDIF
     C                   ENDSR
     C*
      *
      *===============================================================
      * OPT80 - Subroutine for Option 80 (Time & Materials)
      *===============================================================
     C     OPT80         BEGSR
     C*
     C*  Retieve the WCMC Sub# and Grp#.
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA033R'
1    C                   PARM      KKCO#         ##CO#             3 0          Company #
2    C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
3    C                   PARM      SCFYR         ##FYR             3 0          Policy Year
4    C                   PARM      SCCASE        ##CASE            7 0          Claim #
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
03   C                   PARM      SCFYR         ##FYR             3 0          Policy Year
04   C                   PARM      SCCASE        ##CASE            7 0          Claim #
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
16   C                   PARM      *BLANKS       ##RTCD            1            Return Code
     C*
     C                   ENDIF
      *
     C                   MOVEL     OPTNM         QQPRDS           40
     C                   MOVE      KKCO#         QQCO#
     C                   MOVE      ##SUB#        QQSUB#
     C                   MOVE      ##GRP#        QQGRP#
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCEOB#        QQEOB#
     C                   MOVE      '2'           QQRTCD            1
      *
      *
     C*
     C                   CALL      'F3040R'
     C                   PARM      '1'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C                   PARM      *BLANKS       ##EOB             1
     C                   PARM      SCRCN         ##RCN             3 0
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
     C     SCSEL         CASEQ     '15'          OPTEOB
     C     SCSEL         CASEQ     '16'          OPTEOB
     C     SCSEL         CASEQ     '21'          OPT21
     C     SCSEL         CASEQ     '22'          OPT22
     C     SCSEL         CASEQ     '23'          OPT23
     C     SCSEL         CASEQ     '24'          OPT24
     C     SCSEL         CASEQ     '80'          OPT80
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
      *
     C/COPY SINDIC01
      *
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
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
     C*  Determine if WCMC is installed.
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
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
     C* Disable option 24, if EFT Install Flag is not 'Y'
     C*
     C                   MOVEL(P)  'WC'          XSKEY
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL35        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '24'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
      *
     C   21KEY2          READE     DBRCDF                                 95
      *
     C                   ENDSR
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
      *
     C   21KEY2          READPE    DBRCDF                                 95
      *
     C                   ENDSR
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
      *
     C   21KEY1          SETGT     DBRCDF                             96
      *
     C                   ENDSR
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
      *
     C   21KEY1          SETLL     DBRCDF                             96
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
      * Payment Status
      *
     C                   MOVEA     PS            CH10             10
     C     CH10          IFNE      *BLANK
     C     RPSTS         LOOKUP    PS                                     40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Service Date Range
      *
     C     SCSFDT        IFNE      *ZERO
     C                   MOVE      *OFF          MEETSS
      *
     C     XXFROM        IFGE      RPFRDT
     C     XXFROM        ANDLE     RPTODT
      *
     C     XXTO          ORGE      RPFRDT
     C     XXTO          ANDLE     RPTODT
      *
     C     XXFROM        ORLE      RPFRDT
     C     XXTO          ANDGE     RPTODT
      *
     C                   MOVE      *ON           MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Payment Code
      *
     C                   MOVEA     PC            CH10             10
     C     CH10          IFNE      *BLANK
     C     RPTRCD        LOOKUP    PC                                     40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Transaction Type
      *
     C                   MOVEA     TT            CH5               5
     C     CH5           IFNE      *BLANK
     C     RPTYPE        LOOKUP    TT                                     40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Payment Value 1
      *
     C                   MOVEA     PV1           CH10             10
     C     CH10          IFNE      *BLANK
     C     RPBR2         LOOKUP    PV1                                    40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Reserve Code
      *
     C                   MOVEA     RC            CH10             10
     C     CH10          IFNE      *BLANK
     C     RPRECD        LOOKUP    RC                                     40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * Group vs. Non-Group
      *
     C     SCSGRP        IFNE      *BLANK
     C     RPPGTP        ANDNE     SCSGRP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Federal ID #
      *
     C     SCSFEI        IFNE      *ZERO
     C     RPFEIN        ANDNE     SCSFEI
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *
     C                   MOVE      SCSMOD        XXSMOD            3 0
     C     SCSFEI        IFNE      *ZERO
     C     SCSMOD        IFNE      *BLANKS
     C     RPMOD         ANDNE     XXSMOD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
      *
      * EOMB #
      *
     C     SCSEOB        IFNE      *ZERO
     C     RPEOB#        ANDNE     SCSEOB
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     SCSIRQ        IFNE      *BLANK
     C     RPRQST        ANDNE     SCSIRQ
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR
** Column headings
Opt Payee              --- Service Dates ---        Amount   Dt Rcvd   Rsv Sts
Opt FEIN       Mod     EOB#      RCN#   EFT         Amount   Post Dt   PV1 Sts
** Description--------------MISN
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
11515=Reprint EOB           YYN
11616=Display EOB           YYY
12121=Pend payment          YNN
12222=Release payment       YNN
12323=Change group type     YNN
12424=Chg EFT to non-EFT    YNN
18080=Time & materials      YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
209F9=Change info           YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
