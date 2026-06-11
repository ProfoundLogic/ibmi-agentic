     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0576R2 - Work With Payment Detail
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
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Load SCPODT with PARQDT if not zero.           *
     H*                                                               *
     H* 05/05/05 C:53 Support: WC045/45746                            *
     H*               Correct file read issue.                        *
     H*                                                               *
     H* 03/08/05 C:53 Support: WC063/45820                            *
     H*               Correct paging issue                            *
     H*                                                               *
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H* 07/03/02 C:15 Support: WC025/33106                            *
     H*               Added group to the WDCRL key list.              *
     H*                                                               *
     H* 02/27/02 C:15 Project: WC835                                  *
     H*               Added code to check for additional WDRPP record *
     H*               to get the appropriate payee name.              *
     H*                                                               *
     H* 05/21/01 C:62  PROJECT: MC114/27498                           *
     H*                ADD PROCESSING FOR F0340R IF PBILLS INSTALLED. *
     H*                                                               *
     H* 05/01/01 C:15  Project: WC1020                                *
     H*                Added code to use the bank code when retrieving*
     H*                the bank info record for opt 6 reprint.        *
     H*                                                               *
     H* 04/30/01 C:53  PROJECT: WC094/27230                           *
     H*                ADD OPT 6 PROCESSING TO PROGRAM                *
     H*                                                               *
     H* 06/12/00 C:62  PROJECT: MC230                                 *
     H*                SWAP OUT FA015R USAGE WITH FA031R, FA032R,     *
     H*                FA033R AND FA034R. CHANGED TO F3040R.          *
     H*                                                               *
     H*  05/19/00 C:15  Support: WC034/20089                          *
     H*                 Added the TRDT to key list for WDRPP.         *
     H*                                                               *
     H*  04/17/00 C:118 Support: WC044/22486                          *
     H*                 Add SEQ# to KEY1 to correct paging.           *
     H*                                                               *
     H*  04/07/99 C:15 Support: WC090/17156                           *
     H*                Removed PATRDT from key list used to retrieve  *
     H*                records from WDRPP due to incorrect date being *
     H*                loaded into RPTRDT.                            *
     H*                                                               *
     H*  01/16/98 C:15 Project: WC577                                 *
     H*                Added the Group # and name to the heading on   *
     H*                SCR01.                                         *
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
     FW0576F2   CF   E             WORKSTN
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
      *  Payment Detail Logical  (CO#/FND/CHK#/EOB#)
     FWDPAL8    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPA01:DBRCDF)
      *__________________________________________________________
      *  Payment Detail Logical  (CO#/FND/TRDT/CHK#/SEQ#)
     FWDPAL7    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPA01:DBRCDF2)
     F*** BEG ADD *** 04/30/01 ***************************************
      *  Payment Detail Logical  (CO#/FND/FEIN/MOD/TRDT/CHK#)
     FWDPALF    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PPA01:DBRCDF3)
     F*** END ADD *** 04/30/01 ***************************************
      *__________________________________________________________
      *  Payment Detail Detail Logical  (CO#/FND/FYR/CASE/CHK#/TRDT)
     FWDRPL     IF   E           K DISK
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** End add ***** 05/22/03 **************************************
      *__________________________________________________________
      *  Claim Master                   (CO#/FND/FYR/CASE)
     FWMCML1    IF   E           K DISK
      *__________________________________________________________
      *  Claimant Demographics          (SSN#)
     FWMCDL     IF   E           K DISK
      *__________________________________________________________
      *  Payee Demographics
     FWMPDL     IF   E           K DISK
     F*** BEGIN ADD *** 01/16/98 ***********************************************
      *__________________________________________________________
      *  Fund Master
     FWMFNL     IF   E           K DISK
     F*** END ADD ***** 01/16/98 ***********************************************
     F*** BEG ADD *** 04/30/01 *************************************************
     F*** Begin chg *** 05/01/01 **************************************
     F*********WMBIL1  IF  E           K        DISK
     FWMBIL2    IF   E           K DISK
     F*** End chg ***** 05/01/01 **************************************
     F*** END ADD *** 04/30/01 *************************************************
      *__________________________________________________________
     F*** BEGIN DLT *** 01/07/98 ***********************************************
     F*  WCMC Group Master file
     F*****FMFNL1  IF  E           K        DISK
     F************PFN01                             KRENAMEPFN02
     F*  WCMC EOB Master file
     F*****FAEOL4  IF  E           K        DISK
     F*** END DLT ***** 01/07/98 ***********************************************
      *__________________________________________________________
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(9) CTDATA PERRCD(1)              Column headings ABOV
     D HDA             S             78    DIM(1) CTDATA PERRCD(1)              Column headings BELO
     D OF              S              1    DIM(18) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(18) ALT(OF)
      *
     D I6              S              6    DIM(63)                              Indicator Values
      *
     D***PY SCOPYWW01E
      *
     D***PY SCOPYWW01I
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
      *  SCDATA - Fields defined for the 3 INFO Displays
      *
     D                 DS
     D  SLIN1                  1     74
     D  SNAME                  1     25
     D  SFEIN#                 1      9
     D  SMOD                  12     14
     D  SEOB#                 17     25
     D  SCLRDT                29     40
     D  SDESC                 34     73
     D  SBKCD                 50     74
     D  SYR                    2      3
     D  SCASE                  6     12
      *
      *  SCDAT2 - Fields defined for the 3 INFO Displays
      *
     D                 DS
     D  SLIN2                  1     21
     D  SDT1                   1     10
     D  SDT2                  12     21
      *
     D                 DS
     D  FIELD                  1     10
     D  FLDNAM                 1      5
     D  FLDNUM                 6      6  0
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Payment Detail      ')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0576R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(3)
     D*** BEG CHG *** 04/30/01 ***************************************
     D***********   2                     C         VIEWMX
     D VIEWMX          C                   CONST(3)
     D*** END CHG *** 04/30/01 ***************************************
     D*** BEG CHG *** 03/08/05 ***************************************
     D***********   9                     C         SF1PAG
     D SF1PAG          C                   CONST(3)
     D*** END CHG *** 03/08/05 ***************************************
     D DELETD          C                   CONST('DELETED')
     D HEAD1           C                   CONST('Totals - Weeks:')
     D HEAD2           C                   CONST('Days:')
     D HEAD3           C                   CONST('Paid:')
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
     C                   KFLD                    KKCO#
     C                   KFLD                    XXKEY            10
      *
      *  Payment Detail Key
      *
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      *  Payment Detail Key  (Check #)
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCHK#
     C                   KFLD                    KKEOB#
     C***** BEGIN ADD **** 04/17/00 ***********************************
     C                   KFLD                    KKSEQ#
     C***** END   ADD **** 04/17/00 ***********************************
      *
      *  Payment Detail Key  (Transaction Date)
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKCHK#
     C                   KFLD                    KKSEQ#
     C*** BEG ADD *** 04/30/01 ***************************************
      *
      *  Payment Detail Key  (FEIN/MOD)
      *
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*** BEG ADD *** 05/05/05 ***************************************
     C                   KFLD                    KKTRDT
     C                   KFLD                    KKCHK#
     C*** END ADD *** 05/05/05 ***************************************
     C*** END ADD *** 04/30/01 ***************************************
      *
      *  Payment Detail Detail Key
      *
     C     RPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C*** Begin chg *** 02/27/02 **************************************
     C*******************  KFLD           PATRDT
     C                   KFLD                    XXTRDT            7 0
     C*** End chg ***** 02/27/02 **************************************
     C*** END ADD ***** 05/19/00 **************************************
     C*** BEGIN DLT *** 04/07/99 ***********************************************
     C******************** KFLD           PATRDT
     C*** END DLT ***** 04/07/99 ***********************************************
     C*** Begin add *** 05/22/03 **************************************
     C*
     C     R2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PASEQ#
     C*** End add ***** 05/22/03 **************************************
      *
      *  Claim Master
      *
     C     CMKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
      *
      *  Claimant Demographics
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
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
     C*** BEG ADD *** 04/30/01 *************************************************
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
     C*** END ADD *** 04/30/01 *************************************************
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
     C*** BEG ADD *** 04/30/01 *************************************************
     C     *LIKE         DEFINE    PAFEIN        KKFEIN
     C     *LIKE         DEFINE    PAMOD         KKMOD
     C*** END ADD *** 04/30/01 *************************************************
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
     C     SCPST2        IFGT      *BLANK
     C     SCPST2        ANDNE     *ZERO
     C                   MOVEL     SCPST2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
      *
     C                   SELECT
      *          ______________________________________________________
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
     C     Q1PRMS        IFGT      3
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   ELSE
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      ##FND         KKFND
     C                   Z-ADD     *ZERO         KKFYR
     C                   Z-ADD     *ZERO         KKCASE
     C                   ENDIF
      *
     C                   MOVE      *ZERO         KKTRDT
     C                   MOVE      *ZERO         KKEOB#
     C                   MOVE      *ZERO         KKCHK#
     C                   MOVE      *ZERO         KKSEQ#
     C*** BEG ADD *** 05/05/05 *****************************
     C                   MOVE      *ZERO         KKFEIN
     C                   MOVE      *ZERO         KKMOD
     C*** END ADD *** 05/05/05 *****************************
      *
     C                   ENDSR
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C   21              MOVE      SCPST1        KKCHK#
      *
     C     *IN22         IFEQ      *ON
     C                   MOVE      SCPST2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTRDT
     C                   ENDIF
      *
     C*** BEG ADD *** 04/30/01 ***************************************
      *
     C     *IN23         IFEQ      *ON
     C                   MOVE      SCPST3        KKFEIN
     C                   MOVE      SCPS3A        KKMOD
     C                   ENDIF
      *
     C*** END ADD *** 04/30/01 ***************************************
     C***22***             MOVE SCPST2    KKTRDT
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
     C*** BEG ADD *** 05/05/05 *****************************
     C                   MOVE      SCFEIN        KKFEIN
     C                   MOVE      SCMOD         KKMOD
     C*** END ADD *** 05/05/05 *****************************
      *
     C                   ENDSR
      *
      *================================================================
      *  KHDO - Process F8 "Payment Summary" request.                 *
      *================================================================
     C     KHDO          BEGSR
      *
     C                   MOVE      SCCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
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
      *  KPDO - Process F15 "Financial Summary" request.              *
      *================================================================
     C     KPDO          BEGSR
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
     C*** BEG ADD *** 04/30/00 **************************************
     C                   MOVE      PAACCT        SCACCT
     C*** END ADD *** 04/30/00 **************************************
     C*** Begin add *** 05/01/01 **************************************
     C                   MOVE      PACKTP        SCCKTP
     C*** End add ***** 05/01/01 **************************************
     C*** BEG ADD *** 05/05/05 **************************************
     C                   MOVE      PAFEIN        SCFEIN
     C                   MOVE      PAMOD         SCMOD
     C*** END ADD *** 05/05/05 **************************************
      *
      *  Build Subfile Record based on INFO# (F9=Info Display)
      *
     C                   SELECT
      *          _______________________________________________________
B01  C     INFO#         WHENEQ    1
      *
     C                   MOVEL(P)  PADESC        SDESC                          Payment Description
      *
      *  Execute Assign Name subroutine
      *
     C                   EXSR      USER01
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
      *          _______________________________________________________
     C     INFO#         WHENEQ    3
      *
     C                   MOVEL(P)  PACKTP        SBKCD                          Bank Acct# Code
      *
     C                   MOVE      PAFYR         SYR
     C                   MOVE      PACASE        SCASE
      *
     C     PAEOB#        IFNE      *ZERO
     C                   MOVE      PAEOB#        SEOB#                          EOMB #
     C                   ENDIF
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
     C*** BEG ADD *** 04/30/01 *************************************************
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
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BIFTYP        ##FTYP           10            Formtype
     C                   MOVEL     BICPYQ        ##CHKQ           10            Filecopy OUTQ
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
     C*** END ADD *** 04/30/01 *************************************************
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
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA031R'
1    C                   PARM      KKCO#         ##CO#             3 0          Company #
2    C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
3    C                   PARM      SCFYR         ##FYR             3 0          Policy Year
4    C                   PARM      SCCASE        ##CASE            7 0          Claim #
5    C                   PARM      SCEOB#        ##EOB#            9 0          EOMB #
6    C                   PARM      SCRCN         ##RCN             3 0          RECON #
7    C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ELSE
     C*
     C                   CALL      'FA032R'
     C                   PARM      KKCO#         ##CO#             3 0          Company #
     C                   PARM      KKFND         ##WCF#            3 0          WCMS Fund
     C                   PARM      SCFYR         ##FYR             3 0          Policy Year
     C                   PARM      SCCASE        ##CASE            7 0          Claim #
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
     C*** BEGIN ADD *** 05/21/01 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     ##RC          IFEQ      *BLANKS
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
     C     SCSEL         IFEQ      '15'
     C                   MOVE      'P'           #1MODE
     C                   MOVEL     UPOUTQ        #1QUE
     C                   ELSE
     C                   MOVE      'D'           #1MODE
     C                   MOVEL     *BLANKS       #1QUE
     C                   ENDIF
     C*
     C                   CALL      'F0340C'
     C                   PARM                    #1RRN             9
     C                   PARM      ##AST         #1AST             2
     C                   PARM                    #1MODE            1
     C                   PARM                    #1QUE            10
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END   ADD *** 05/21/01 **************************************
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
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
      *
     C     SCSEL         IFEQ      '16'
     C                   MOVE      '1'           QQMODE
     C                   ELSE
     C                   MOVE      'D'           QQMODE
     C                   END
      *
     C                   CALL      'F7200C1'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQGRP#            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
     C                   PARM                    UPOUTQ
     C                   PARM                    QQMODE            1
     C*** BEGIN ADD *** 05/21/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/21/01 **************************************
      *
     C                   ENDSR
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
     C**********           PARM           ##RRN
     C**********           PARM           ##SUB#
     C**********           PARM           ##GRP#
     C**********           PARM *BLANK    ##RTCD
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
16   C                   PARM      *BLANKS       ##RC              1            Return Code
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
     C                   MOVEL     OPTNM         QQPRDS           40
     C                   MOVE      KKCO#         QQCO#
     C*** BEGIN DLT *** 01/07/98 ***********************************************
     C******************** MOVE *BLANKS   QQSUB#
     C******************** MOVE XXGRP#    QQGRP#
     C*** END DLT ***** 01/07/98 ***********************************************
     C*** BEGIN ADD *** 01/07/98 ***********************************************
     C                   MOVE      ##SUB#        QQSUB#
     C                   MOVE      ##GRP#        QQGRP#
     C*** END ADD ***** 01/07/98 ***********************************************
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCEOB#        QQEOB#
     C                   MOVE      '2'           QQRTCD            1
      *
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           CALL 'N0540R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCO#
     C**********           PARM           QQSUB#
     C**********           PARM           QQGRP#
     C**********           PARM           QQFYR
     C**********           PARM           QQCASE
     C**********           PARM           QQEOB#
     C**********           PARM           QQRTCD
      *
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           ENDIF
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
     C*** END   CHG *** 06/12/00 **************************************
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
     C     *INKP         CASEQ     *ON           KPDO                           F15 Financ.Summary
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
     C*** BEGIN ADD *** 06/12/00 ***********************************************
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
     C*** END   ADD *** 06/12/00 ***********************************************
      *
     C/COPY SINDIC01
      *
     C     Q1PRMS        IFGT      3
     C                   MOVE      @@SFDT        SCSFDT                         Service FROM Date
     C                   MOVE      @@STDT        SCSTDT                         Service TO Date
     C                   MOVEA     @@SPC         PC                             Payment Codes
     C                   MOVEA     @@STT         TT                             Transaction Type
     C                   MOVEA     @@SPV1        PV1                            Payment Value 1
     C                   MOVEA     @@SRC         RC                             Reserve Codes
     C                   MOVE      @@SFEI        SCSFEI                         Federal ID#
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
     C                   ENDIF
      *
      *  Access User Parameters
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF
      *
     C*** BEGIN ADD *** 01/16/98 ***********************************************
     C                   MOVE      *BLANKS       FNNAME
     C     KEY           CHAIN     PFN01                              60
     C                   MOVE      KKFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*** END ADD ***** 01/16/98 ***********************************************
      *
     C                   MOVE      *ON           *IN20
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
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
     C   23KEY           READE     DBRCDF3                                95    ADD 04/30/01
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
     C   23KEY           READPE    DBRCDF3                                95    ADD 04/30/01
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
     C   23KEY3          SETGT     DBRCDF3                            96        ADD 04/30/01
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
     C   23KEY3          SETLL     DBRCDF3                            96        ADD 04/30/01
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
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      PACO#         #ACO#
     C                   MOVE      PAFND         #AFND
     C                   MOVE      PAFYR         #AFYR
     C                   MOVE      PACASE        #ACASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
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
     C*
     C*** BEG ADD *** 04/30/00 **************************************
     C     SCSFEI        IFNE      *ZERO
     C     MEETSS        ANDNE     *OFF
     C*
     C     SCSMOD        IFNE      *ZERO
     C     PAMOD         ANDNE     SCSMOD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 04/30/00 **************************************
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
      *
     C                   ENDSR
      *
      *================================================================
      * USER01 - Assign Payee Name from WDRPP, WMPDP, or WMCDP
      *================================================================
     C     USER01        BEGSR
      *
      *  1) Access Payment Detail Detail file
      *
     C*** Begin add *** 05/22/03 **************************************
     C     R2KY          CHAIN     PRP02                              60
     C     *IN60         IFEQ      *ON
     C*** End add ***** 05/22/03 **************************************
     C*** Begin add *** 02/27/02 **************************************
     C                   Z-ADD     PATRDT        XXTRDT
     C*** End add ***** 02/27/02 **************************************
     C     RPKY          CHAIN     PRP01                              60
     C*** Begin add *** 02/27/02 **************************************
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     PAPODT        XXTRDT
     C     RPKY          CHAIN     PRP01                              60
     C                   ENDIF
     C*** End add ***** 02/27/02 **************************************
     C*** Begin add *** 05/22/03 **************************************
     C                   ENDIF
     C*** End add ***** 05/22/03 **************************************
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     RPPNAM        SNAME                          assign name
      *
     C                   ELSE
      *
      *  Access Claim Master
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         CMSS#
     C                   ENDIF                                                  *IN60 WMCMP
      *
     C                   MOVE      PAFEIN        CFEIN             9
      *
      *  2) If FEIN# > 0 & FEIN# NE CMSS#, access Payee Demographics
      *
     C     PAFEIN        IFNE      *ZERO
     C     CFEIN         ANDNE     CMSS#
      *
      *  Access Payee Demographics
      *
     C     PDKY          CHAIN     PPD01                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *BLANKS       SNAME                          assign name
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL     PDNAME        SNAME                          assign name
     C                   ELSE
     C                   MOVEL(P)  PDNAME        QQLAST           56
     C                   MOVEL(P)  PDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SNAME                          assign name
     C                   ENDIF                                                  PDFSN
     C                   ENDIF                                                  *IN61 WMPDP
      *
     C                   ELSE
      *
      *  Access Claimant Demographics
      *
     C     CDKY          CHAIN     PCD01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        SNAME
     C                   ELSE
     C     CDFSN         IFEQ      *BLANKS
     C                   MOVEL     CDLSN         SNAME                          assign name
     C                   ELSE
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     CDFSN         QQFRST
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SNAME                          assign name
     C                   ENDIF                                                  CDFSN = ' '
     C                   ENDIF                                                  *IN60 WMCDP
     C                   ENDIF                                                  PAFEIN > 0
     C                   ENDIF                                                  *IN60 WDRPP
      *
     C                   ENDSR
      *
** Column headings   TOP HEADING
    Payee Name                       Description
    Payee Name                       Description
    Payee Name                       Description
    FEIN       MOD      EOMB#        Clear Dt   Bank Acct Code
    FEIN       MOD      EOMB#        Clear Dt   Bank Acct Code
    FEIN       MOD      EOMB#        Clear Dt   Bank Acct Code
    FYR     Case        EOMB#        Clear Dt   Bank Acct Code
    FYR     Case        EOMB#        Clear Dt   Bank Acct Code
    FYR     Case        EOMB#        Clear Dt   Bank Acct Code
** Column headings   BOTTOM HEADING
Opt PV1  WK/D --- Service Dates ---  Post Date Trans Date  Check#       Amount
** Description--------------MISN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Reprint file copy      YYY
11515=Reprint EOB           YYY
11616=Display EOB           YYY
18080=Time & materials      YYY
19999=Dup paymt exclusion   YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Financial summary     NNN
208F8=Paymt history summary NNN
209F9=Change info           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
