     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0576R02 - Payment Detail Maintenance program
     H*****************************************************************
     H*  DESCRIPTION:
     H*    This program is used to View Check, Update record for      *
     H*    Duplicate Payment Exclusion.                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 05/29/97  C:30  Project:  WC660                     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     h*                                                               *
     h* 06/09/20 C:318 Project: SY420
     h*                Mask/Unmask SSN
     h*
     h* 04/10/19 C:062 Project: 152WAB                                *
     h*                Apply CSI mods for S1 Medical.                 *
     H*                                                               *
     H* 08/29/11 C:277 SUPPORT 77320                                  *
     H*                Populate WDPAP initial fields                  *
     H*                                                               *
     H* 03/17/11 C:58  Project: 114WYF
     H*                Make MAILING DATE heading a 15A field, allows
     H*                using PROCESSED DATE if CLT = 114.
     H*
     H* 01/05/11 C:257 PROJECT: 114WXM                                *
     H*                Process Invoice # Field on Payment detail      *
     H*                screen.                                        *
     H*                                                               *
     H* 08/03/10 C:53  Support: 73286
     H*                CALCULATE MAIL DATE IF NOT FOUND.
     H*
     H* 06/08/10 C:190 PROJECT: 139WOM
     H*                For CLTID = 139 change the program to
     H*                correctly access the check schedule
     H*                maintenance record by subtracting 1 day         .
     H*                                                               *
     H* 05/01/09 C:220 PROJECT: WC1766                                *
     H*                Create Holiday Blackout Date entry screen      *
     H*                for mailing Claims checks                      *
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Load SCPODT with PARQDT if not zero.           *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*                                                               *
     H* 04/06/04 C:53 Support: WC044/41791                            *
     H*               Tighten search process for WDRPP                *
     H*                                                               *
     H* 08/12/03 C:53 Support: WC102/38307                            *
     H*               Add additionl link to WDRPP.                    *
     H*                                                               *
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 02/27/02 C:15 Project: WC835                                  *
     H*               Added code to retrieve WDRPP record for V/S/Z/R *
     H*               so the reason for the V/S/Z/R will be displayed.*
     H*                                                               *
     H* 02/18/02 C:15 Support: WC114/30809                            *
     H*               Added Pmt Val 2 code and desc to screen.        *
     H*                                                               *
     H* 06/12/00 C:62 Project: MC230                                  *
     H*               Replace FA020R with new API's.                  *
     H*                                                               *
     H* 05/19/00 C:15 Support: WC034/20089                            *
     H*               Added the TRDT to key list for WDRPP.           *
     H*                                                               *
     H* 05/04/00 C:62 Project: WC048 22709                            *
     H*               ADD ADDITIONAL PARM FOR CHECK# AND USE THAT TO  *
     H*               RECIEVE INVOICE NUMBERS.                        *
     H*                                                               *
     H* 04/06/00 C:62  Support: WC048 22467                           *
     H*                GET RCN FROM PAEOBS AND PASS THAT TO FA020R    *
     H*                TO ACCESS THE CORRECT FAEAP RECORD.            *
     H*                                                               *
     H* 03/03/00 C:62  Support: MCMNT                                 *
     H*                PASS INTO FA020R PAFEIN/MOD.                   *
     H*                                                               *
     H* 11/04/99 C:15  Support: WC048/20507                           *
     H*                Added code to clear the remark fields.         *
     H*                                                               *
     H* 10/13/99 C:53  Project: MC212                                 *
     H*                EXPANDED INVOICE # TO 9 POS FOR CALL TO FA020R *
     H*                AND IN THE DISPLAY FILE                        *
     H*                                                               *
     H* 08/03/99 C:112 Project: WC659                                 *
     H*                Modify Duplictae Payment Exclusion selection to*
     H*                allow user to Toggle the exclusion field from  *
     H*                'Y' to 'N' and 'N' to 'Y'                      *
     H* 07/06/99 C:112 Project: WC856                                 *
     H*                Add display field to indicate the status of    *
     H*                the Duplicate Payment Exclusion selection,     *
     H*                i.e. if the 'Dup PMT Clear Date' field (PADPCD)*
     H*                is > zero then display a 'Y'                   *
     H*                                                               *
     H* 04/02/99 C:90  PROJ: WC889                                    *
     H*                IF DBA FLAG = 'Y' LOAD PDADR2, ELSE LOAD PDADR1*
     H*                                                               *
     H* 03/03/99 C:94  PROJ: MC152                                    *
     H*                ADDED RECON # TO SCREEN IF PROVIDER BILLS IS   *
     H*                INSTALLED                                      *
     H*                                                               *
     H* 01/21/99 C:94  PROJ: MC152                                    *
     H*                CHANGED PROGRAM TO SHOW BRS INV# AND NWS INV#  *
     H*                                                               *
     H* 10/18/98 C:112 PROJ: WC577                                    *
     H*                Display 'Initial Requester' on Payment Detail  *
     H*                screen.                                        *
     H*                                                               *
     H* 03/27/98 C:15 Support: WC091/#10666                           *
     H*               Removed KKTRDT from key list used to retrieve   *
     H*               records from WDRPP due to incorrect date being  *
     H*               loaded into RPTRDT.                             *
     H*                                                               *
     H* 02/20/98 C:15 Project: WC659                                  *
     H*              -Chgd SSN to alphanumeric since the claimant SS# *
     H*               is alphanumeric.                                *
     H*                                                               *
     H*  10/31/97 c:30 Project: WC660 correction                      *
     H*           #028:                                               *
     H*           Group payment detail rec abends during Display.     *
     H*           #029:                                               *
     H*           Recovery, Stop, etc. display correction.            *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *  Display File
     FW0576F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *________________________________________________________________
      *  Payment Detail file (for maintaining record)
     FWDPAP     UF A E             DISK
     F                                     RENAME(PPA01:DBRCDF)
     F*** BEG ADD *** 08/12/03 **************************************
      *  Payment Detail Logical  (CO#/FND/CHK#)
     FWDPAL4    IF   E           K DISK
     F                                     RENAME(PPA01:DBRCDF4)
     F*** END ADD *** 08/12/03 **************************************
      *________________________________________________________________
      *  User Parameter file
     FWMUPL     IF   E           K DISK
      *________________________________________________________________
     F*** BEG DEL *** 08/12/03 **************************************
      ****  Payment Detail Detail file
     F***WDRPL   IF  E           K        DISK
     F*** END DEL *** 08/12/03 **************************************
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** End add ***** 05/22/03 **************************************
      *________________________________________________________________
      *  Payee Demographics
     FWMPDL     IF   E           K DISK
      *________________________________________________________________
      *  Claimant Demographics
     FWMCDL     IF   E           K DISK
      *________________________________________________________________
      *  Table Files
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F*** BEG ADD *** 10/20/03 **********************************
     FWTGTL     IF   E           K DISK
     F*** END ADD *** 10/20/03 **********************************
     F*** BEG ADD *** 05/01/09 **********************************
     FWACPL     IF   E           K DISK
     F*** END ADD *** 05/01/09 **********************************
     F*** BEG ADD *** 08/03/10 ************************************
     FWMCXL     IF   E           K DISK
     F*** END ADD *** 08/03/10 ************************************
      *________________________________________________________________
      /COPY SCOPYMT01D
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*** BEG ADD *** 08/12/03 **************************************
     D*** BEG ADD *** 10/20/03 *******************************************  ****
     D NF              C                   CONST('NOT FOUND')
     D*** END ADD *** 10/20/03 *******************************************  ****
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
     D*** BEGIN CHG *** 02/20/98 ***********************************************
     D**************************************  1   90SSN
     D  SSN                    1      9
     D*** END CHG ***** 02/20/98 ***********************************************
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D***PY SCOPYMT01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Payment Detail')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D*** BEGIN ADD *** 03/17/11 **************************************
     D HEAD01          C                   CONST('  Mailing Date ')
     D HEAD02          C                   CONST('Processed Date ')

     I*** Begin dlt *** 02/27/02 **************************************
     I******************* CD      1   6  1  A            OTHER TYPE CODE
     I******************* DS      1   6 15               OTHER TYPES
     I*** End dlt ***** 02/27/02 **************************************
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
     I*** END ADD ***** 03/17/11 **************************************
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *================================================================
      *  KLIST definitions
      *================================================================
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CCSSN
      *
      *  TABLE FILE KEY
      *
     C     TBKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
      *
      *  PAYEE DEMOGRAPHICS FILE
      *
     C     PDKEY         KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
      *
      *  USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  Payment Detail Detial file
      *
     C     RPKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCHK#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   KFLD                    KKTRDT
     C*** END ADD ***** 05/19/00 **************************************
     C*** BEGIN DLT *** 03/27/98 ***********************************************
     C******************** KFLD           KKTRDT
     C*** END DLT ***** 03/27/98 ***********************************************
     C*** Begin add *** 05/22/03 **************************************
     C*
     C     R2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*** Begin add *** 08/12/03 **************************************
     C                   KFLD                    XXSEQ#
     C*** End add ***** 08/12/03 **************************************
     C*
     C     PAKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PACHK#
     C*** Begin chg *** 08/12/03 **************************************
     C***********          KFLD           PASEQ#
     C                   KFLD                    XXSEQ#
     C*** End chg ***** 08/12/03 **************************************
     C*** End add ***** 05/22/03 **************************************
     C*** Begin add *** 08/12/03 **************************************
     C*
     C     P2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PACHK#
     C*** End add ***** 08/12/03 **************************************
     C*** BEG ADD *** 10/20/03 **********************************
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
     C*** END ADD *** 10/20/03 **********************************
     C*** BEG ADD *** 05/01/09 **********************************
     C     PCPKY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXDAT             7 0
     C                   KFLD                    PAFND
     C*** END ADD *** 05/01/09 **********************************
     C*** BEG ADD *** 08/03/10 ************************************
     C     CXKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C*** END ADD *** 08/03/10 ************************************
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
     C     *LIKE         DEFINE    PACHK#        KKCHK#
     C     *LIKE         DEFINE    PATRDT        KKTRDT
     C*** BEG ADD *** 10/20/03 **********************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END ADD *** 10/20/03 **********************************
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#                         ADD 08/12/03
     C     *LIKE         DEFINE    PASEQ#        XBSEQ#                         ADD 08/12/03
      *
      *  Access User Parameters
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   ENDIF
      *
     C                   MOVE      UPCO#         ##CO#
     C                   MOVE      UPFND         ##FND
      *
     C/COPY SCOPYMT01C
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C*** BEGIN ADD *** 11/04/99 **************************************
      *  Clear all screen fields
      *
     C                   MOVE      *BLANKS       SCCKNM
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCSZ
     C*** BEG ADD *** 10/20/03 **********************************
     C                   MOVE      *BLANKS       SCADR4
     C                   MOVE      *BLANKS       XXCTRY            3
     C                   MOVE      *BLANKS       XXCTRD           15
     C                   MOVE      *OFF          *IN05
     C*** END ADD *** 10/20/03 **********************************
     C                   MOVE      *ZEROS        SCCHK#
     C                   MOVE      *ZEROS        SCAMT
     C                   MOVE      *ZEROS        SCINV#
     C                   MOVE      *ZEROS        SCEOB#
     C                   MOVE      *BLANKS       SCDES
     C                   MOVE      *BLANKS       SCDES2
     C                   MOVE      *BLANKS       SCRMDS
     C                   MOVE      *BLANKS       SCBR2
     C                   MOVE      *BLANKS       SCRMK1
     C                   MOVE      *BLANKS       SCRMK2
     C                   MOVE      *BLANKS       SCRMK3
     C                   MOVE      *BLANKS       SCCKCN
     C                   MOVE      *ZEROS        SCFEIN
     C                   MOVE      *ZEROS        SCMOD
     C                   MOVE      *BLANKS       SCPYNM
     C                   MOVE      *BLANKS       SCTRCD
     C                   MOVE      *BLANK        SCTYPE
     C                   MOVE      *BLANKS       SCRECD
     C                   MOVE      *BLANK        SCRETP
     C                   MOVE      *ZEROS        SCPODT
     C                   MOVE      *ZEROS        SCTRDT
     C                   MOVE      *ZEROS        SCSVFR
     C                   MOVE      *ZEROS        SCSVTO
     C                   MOVE      *ZEROS        SCWKS
     C                   MOVE      *ZEROS        SCDAY
     C                   MOVE      *BLANKS       SCRQST
     C                   MOVE      *BLANK        SCDUPM
     C                   MOVE      *ZEROS        SCNINV
     C                   MOVE      *ZEROS        SCRCN
     C*** END ADD ***** 11/04/99 **************************************
     C*** Begin add *** 02/18/02 **************************************
     C                   MOVE      *BLANKS       SCBR7
     C                   MOVE      *BLANKS       SCDES7
     C*** End add ***** 02/18/02 **************************************
     C**** BEG ADD *** 06/08/10 ***************************************
     C                   MOVE      *ZEROS        SCSMDT
     C**** END ADD *** 06/08/10 ***************************************
      *
      *  Build Claim Header
      *
     C                   MOVE      PACO#         ##CO#
     C                   MOVE      PAFND         ##FND
     C                   MOVE      PAFYR         ##FYR             3 0
     C                   MOVE      PACASE        ##CASE            7 0
      *
     C                   CALL      'WA006R'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
      *  Assign Name as either Claimant or Payee
      *
     C                   MOVE      S2SS1         SS1
     C                   MOVE      S2SS2         SS2
     C                   MOVE      S2SS3         SS3
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYP            25
     C                   PARM      'CMSS#'       QQNAME           25
     C     SSN           PARM      SSN           QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVE      SSN           CCSSN             9
     C*** BEG ADD *** 03/03/99 ***************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C*** END ADD *** 03/03/99 ***************************************
      *
     C************         Z-ADDPAFYR     SCYR
     C                   MOVE      PACHK#        SCCHK#
     C************         MOVE PACASE    SCCASE
      *
      *** BEGIN DEL *** 10/31/97 ***********************************************
     C***********PAPGTP    IFNE 'G'
      *** END   DEL *** 10/31/97 ***********************************************
      *
     C                   MOVE      PAAMT         SCAMT
      **** BEGIN ADD *** 10/18/98 *************************************
     C                   Z-ADD     PAEOB#        SCEOB#
      **** END ADD ***** 10/18/98 *************************************
      *
     C*
     C*** BEGIN DEL *** 06/12/00 **************************************
     C*** BEG ADD *** 01/21/99 ***************************************
     C**********           MOVE PACO#     @#CO#
     C**********           MOVE PAFND     @#WCF#
     C**********           MOVE PAFYR     @#FYR
     C**********           MOVE PACASE    @#CASE
     C**********           MOVE PAEOB#    @#EOB#
     C*** BEGIN ADD *** 04/06/00 **************************************
     C**********           MOVE PAEOBS    @#RCN
     C**********           Z-ADDPAEOBS    SCRCN
     C*** END   ADD *** 04/06/00 **************************************
     C*** END   DEL *** 06/12/00 **************************************
     C*** BEG ADD *** 03/03/99 ***************************************
     C*** BEG ADD *** 03/17/99 ***************************************
     C     ##MCIF        IFEQ      'Y'
     C     PAEOB#        ANDNE     0
     C*** END ADD *** 03/17/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           CALL 'FA020R'
     C**********           PARM           @#CALL  1        Call code
     C**********           PARM           @#CO#   3        Company #
     C**********           PARM           @#WCF#  3        WCMS Fund
     C**********           PARM           @#FYR   3        Policy Year
     C**********           PARM           @#CASE  7        Claim #
     C**********           PARM           @#EOB#  9        EOMB #
     C**********           PARM *BLANKS   @#OTHR  9        EOMB billing amt
     C**********           PARM *BLANKS   @#RC    1        Return Code
     C*** BEG CHG *** 10/13/99 *********************************
     C**********           PARM *BLANKS   @#INV#  9        INVOICE #
     C**********           PARM *BLANKS   @#NINV  9        NWS INV#
     C*** END CHG *** 10/13/99 *********************************
     C*** BEGIN CHG *** 04/06/00 **************************************
     C**********           PARM *BLANKS   @#RCN   3        RECON #
     C**********           PARM           @#RCN   3        RECON #
     C*** END   CHG *** 04/06/00 **************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
     C**********           PARM PAFEIN    @#FEIN  90       RECON #
     C**********           PARM PAMOD     @#MOD   30       RECON #
     C*** END   ADD *** 03/03/00 **************************************
     C*** BEGIN ADD *** 05/04/00 **************************************
     C**********           PARM PACHK#    @#CHK#  90       CHECK #
     C*** END   ADD *** 05/04/00 **************************************
     C                   CALL      'FA033R'
     C                   PARM      PACO#         @#CO#             3 0          Company #
     C                   PARM      PAFND         @#WCF#            3 0          WCMS Fund
     C                   PARM      PAFYR         @#FYR             3 0          Policy Year
     C                   PARM      PACASE        @#CASE            7 0          Claim #
     C                   PARM      PAEOB#        @#EOB#            9 0          EOMB #
     C                   PARM      PAEOBS        @#RCN             3 0          RECON #
     C                   PARM      PAFEIN        @#FEIN            9 0          RECON #
     C                   PARM      PAMOD         @#MOD             3 0          RECON #
     C                   PARM      PACHK#        @#CHK#            9 0          CHECK #
     C                   PARM      *ZEROS        @#OTHR            9 2          EOMB billing amt
     C                   PARM      *BLANKS       @#AST             2            ADJUDICATION ST
     C                   PARM      *ZEROS        @#INV#            9 0          INVOICE #
     C                   PARM      *ZEROS        @#NINV            9 0          NWS INV#
     C                   PARM      *ZEROS        @#SUB#            5 0          SUBSCRIBER
     C                   PARM      *ZEROS        @#FND             3 0          GROUP
     C                   PARM      *ZEROS        @#RRN             9 0          RRN OF FAEOP
     C                   PARM      *BLANKS       @#RC              1            Return Code
     C*** END   CHG *** 06/12/00 **************************************
     C*
     C                   ELSE
      *
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           CALL 'FA020R'
     C**********           PARM           @#CALL  1        Call code
     C**********           PARM           @#CO#   3        Company #
     C**********           PARM           @#WCF#  3        WCMS Fund
     C**********           PARM           @#FYR   3        Policy Year
     C**********           PARM           @#CASE  7        Claim #
     C**********           PARM           @#EOB#  9        EOMB #
     C**********           PARM *BLANKS   @#OTHR  9        EOMB billing amt
     C**********           PARM *BLANKS   @#RC    1        Return Code
     C**********           PARM *BLANKS   @#INV#           INVOICE #
     C**********           PARM *BLANKS   @#NINV           NWS INV#
     C                   CALL      'FA034R'
     C                   PARM      PACO#         @#CO#             3 0          Company #
     C                   PARM      PAFND         @#WCF#            3 0          WCMS Fund
     C                   PARM      PAFYR         @#FYR             3 0          Policy Year
     C                   PARM      PACASE        @#CASE            7 0          Claim #
     C                   PARM      PAEOB#        @#EOB#            9 0          EOMB #
     C                   PARM      PAFEIN        @#FEIN            9 0          RECON #
     C                   PARM      PAMOD         @#MOD             3 0          RECON #
     C                   PARM      PACHK#        @#CHK#            9 0          CHECK #
     C                   PARM      *ZEROS        @#OTHR            9 2          EOMB billing amt
     C                   PARM      *BLANKS       @#AST             2            ADJUDICATION ST
     C                   PARM      *ZEROS        @#INV#            9 0          INVOICE #
     C                   PARM      *ZEROS        @#NINV            9 0          NWS INV#
     C                   PARM      *ZEROS        @#SUB#            5 0          SUBSCRIBER
     C                   PARM      *ZEROS        @#FND             3 0          GROUP
     C                   PARM      *ZEROS        @#RRN             9 0          RRN OF FAEOP
     C                   PARM      *BLANKS       @#RC              1            Return Code
     C*** END   CHG *** 06/12/00 **************************************
     C                   ENDIF
     C*** END ADD *** 03/03/99 ***************************************
     C*
     C                   MOVE      @#INV#        SCINV#
     C                   MOVE      @#NINV        SCNINV
     C*
     C*** END ADD *** 01/21/99 ***************************************
     C*** BEG ADD *** 03/03/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C*** BEGIN DEL *** 04/06/00 **************************************
     C**********           MOVE @#RCN     SCRCN
     C*** END   DEL *** 04/06/00 **************************************
     C                   SETON                                        30
     C                   ELSE
     C                   SETOFF                                       30
     C                   ENDIF
     C*** END ADD *** 03/03/99 ***************************************
     C*** BEG ADD *** 03/17/99 ****************************************
     C                   ENDIF
     C*** END ADD *** 03/17/99 ****************************************
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSVFR
      *
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSVTO
      *
     C                   MOVE      PATRCD        SCTRCD
     C                   MOVE      PATYPE        SCTYPE
     C                   MOVEL     PARECD        SCRECD
     C                   MOVE      PARETP        SCRETP
     C                   MOVE      PAWKS         SCWKS
     C                   MOVE      PADAYS        SCDAY
     C                   MOVEL     PADESC        SCDES
     C**** BEGIN ADD *** 10/18/98 ************************************
     C                   MOVE      PARQST        SCRQST
     C**** END ADD ***** 10/18/98 ************************************
      *
     C**** BEG ADD ***** 05/01/09 ************************************
     C*
     C************ Populate the new mailing date field ***************
     C*
     C     CLTID         IFEQ      '139'
     C                   EXSR      SRPAST
     C                   ELSE
     C                   MOVEL     PATRDT        XXDAT
     C                   ENDIF
     C     PCPKY         CHAIN     PCP01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CPCMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSMDT
     C                   ENDIF
     C**** END ADD ***** 05/01/09 ************************************
     C*** BEG ADD *** 08/03/10 ************************************
     C     SCSMDT        IFEQ      *ZEROS
     C* Get EDI mailing date increment #
     C                   Z-ADD     *ZEROS        XXEDIM            1 0
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CXEDIM        XXEDIM
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   Z-ADD     XXEDIM        SRDAYS            5 0
     C                   Z-ADD     XXDAT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C* Calculate EDI mailing date
     C*** BEG ADD ****** 05/01/09 **********************************************
     C*
     C* Call API to find out the next working day date
     C*
     C                   MOVE      ##CO#         @@CO#
     C                   MOVE      SRDATE        @@DTI
     C                   MOVE      SRDAYS        @@SRD
     C                   MOVE      '2'           @@MOD
     C                   MOVE      *BLANKS       @@DTO
     C                   MOVE      *BLANK        @@RTCD
     C*
     C                   CALL      'WA072R'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DTI             8
     C                   PARM                    @@SRD             5
     C                   PARM                    @@MOD             1
     C                   PARM                    @@DTO             8
     C                   PARM                    @@RTCD            1
     C*
     C                   MOVEL     @@DTO         SCSMDT
     C                   ENDIF
     C*** END ADD *** 08/03/10 ************************************
      *
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        SRDTI
     C                   ELSE
     C                   Z-ADD     PAPODT        SRDTI
     C                   ENDIF
     C**************       Z-ADDPAPODT    SRDTI
      ***** END CHG ***** 12/15/06 ***************************************
      *
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPODT
      *
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
      *
     C                   MOVEL     PABR2         XXKEY
     C     TBKEY         CHAIN     PB201                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     B2DESC        SCDES2
     C                   END
     C                   MOVEL     PABR2         XXBR2             4
     C                   MOVE      ' -'          XXBR2
     C                   MOVE      XXBR2         SCBR2
     C*
     C*** Begin add *** 02/18/02 **************************************
     C*  Load Pmt Val 2 and description
     C*
     C                   MOVEL(P)  PABR7         XXKEY
     C     TBKEY         CHAIN     PB701                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     B7DESC        SCDES7
     C                   END
     C                   MOVEL(P)  PABR7         SCBR7
     C                   MOVE      ' -'          SCBR7
     C*** End add ***** 02/18/02 **************************************
      *
      *** BEGIN DEL *** 10/31/97 ***********************************************
     C*********************ENDIF                           PAPGTP<>'G'
      *** END   DEL *** 10/31/97 ***********************************************
      *
      *   LOAD THE CLAIMANT NAME
      *
     C     CDKEY         CHAIN     PCD01                              61
     C     *IN61         IFEQ      '0'
     C*                    MOVELCDLSN     QQLAST 56
     C*                    MOVELCDFSN     QQFRST 15
     C*                    MOVE '1'       QQTERM  1
     C*                    CALL 'SRFRLS'
     C*                    PARM           QQLAST
     C*                    PARM           QQFRST
     C*                    PARM           QQTERM
     C*                    MOVELQQLAST    SCCNAM
     C*                    ELSE
     C*                    MOVE *BLANK    SCCNAM
     C                   END
      *
      *** BEGIN DEL *** 10/31/97 ***********************************************
     C***********PAPGTP    IFNE 'G'
      *** END   DEL *** 10/31/97 ***********************************************
      *
     C                   MOVE      PAFEIN        SCFEIN
     C                   MOVE      PAMOD         SCMOD
      *
     C     PDKEY         CHAIN     PPD01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     PDNAME        SCPYNM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCPYNM
     C                   END
      *
     C*** Begin dlt *** 02/27/02 **************************************
     C********** PATYPE    IFEQ 'P'
     C********** PATYPE    OREQ 'M'
     C*** End dlt ***** 02/27/02 **************************************
      *
     C                   Z-ADD     PACO#         KKCO#
     C                   Z-ADD     PAFND         KKFND
     C                   Z-ADD     PAFYR         KKFYR
     C                   Z-ADD     PACASE        KKCASE
     C                   Z-ADD     PACHK#        KKCHK#
     C                   Z-ADD     PATRDT        KKTRDT
      *
     C*** Begin add *** 05/22/03 **************************************
     C*** Begin add *** 08/12/03 **************************************
     C*** Begin del *** 08/12/03 **************************************
     C******
     C******     *IN61     IFEQ *ON
     C******               Z-ADDPABASQ    XXSEQ#
     C******     R2KY      CHAINPRP02                61
     C******               ENDIF
     C****End add ***** 08/12/03 **************************************
     C******     *IN61     IFEQ *ON
     C****End add ***** 05/22/03 **************************************
     C******     RPKEY     CHAINPRP01                61
     C****Begin add *** 02/27/02 **************************************
     C***If*a record was not found for the transaction and it is a
     C***void, stop, reversal, or adjustment, get the View Check
     C***record for the original transaction.
     C******
     C***61*     PATYPE    IFEQ 'A'
     C******     PATYPE    OREQ 'S'
     C******     PATYPE    OREQ 'V'
     C******     PATYPE    OREQ 'Z'
     C******
     C******               Z-ADDPAPODT    KKTRDT
     C******     RPKEY     CHAINPRP01                61
     C******               ENDIF
     C****End add ***** 02/27/02 **************************************
     C****Begin add *** 05/22/03 **************************************
     C******               ENDIF
     C*** end del *** 08/12/03 **************************************
     C*** Begin add *** 08/12/03 **************************************
     C                   Z-ADD     PASEQ#        XXSEQ#
     C*** End add ***** 08/12/03 **************************************
     C     R2KY          CHAIN     PRP02                              61
     C****End add ***** 05/22/03 **************************************
     C*** BEG ADD *** 04/06/04 ***************************
     C  N61RPCHK#        COMP      PACHK#                             6161
     C*
     C     *IN61         IFEQ      *ON
     C     PABASQ        ANDNE     *ZEROS
     C*** END ADD *** 04/06/04 ***************************
     C*** Begin add *** 08/12/03 **************************************
     C                   Z-ADD     PABASQ        XXSEQ#
MJF  C*********  *IN61     IFEQ *ON
     C     R2KY          CHAIN     PRP02                              61
     C*** BEG ADD *** 04/06/04 ***************************
     C  N61RPCHK#        COMP      PACHK#                             6161
     C*** END ADD *** 04/06/04 ***************************
     C                   ENDIF
     C*** BEG ADD *** 04/06/04 ***************************
MJF  C**********           ENDIF
     C*** END ADD *** 04/06/04 ***************************
      *
     C     *IN61         IFEQ      *ON
     C*
     C*** BEG ADD *** 04/06/04 ***************************
     C     PACHK#        IFNE      *ZEROS
     C     PABASQ        ANDNE     *ZEROS
     C*** END ADD *** 04/06/04 ***************************
     C*
     C*** Setup the sequence # to match
     C                   Z-ADD     PABASQ        XBSEQ#
     C*
     C*** Read payments for FND/CHK#
     C     P2KY          CHAIN     DBRCDF4                            98
     C     *IN98         DOWEQ     *OFF
     C*
     C*** Match found
     C     XBSEQ#        IFEQ      P4SEQ#
     C                   Z-ADD     P4BASQ        XXSEQ#
     C*
     C*** Try for RP rcd.
     C     R2KY          CHAIN     PRP02                              61
     C*** BEG ADD *** 04/06/04 ***************************
     C  N61RPCHK#        COMP      PACHK#                             6161
     C*** END ADD *** 04/06/04 ***************************
     C*
     C*** Not found, reset and look for the next base seq#
     C     *IN61         IFEQ      *ON
     C                   Z-ADD     P4BASQ        XBSEQ#
     C*
     C** If this is the original rcd and still not found,
     C** leave to prevent endless loop.
     C     XBSEQ#        IFEQ      *ZEROS
     C                   LEAVE
     C                   ENDIF
     C*
     C     P2KY          CHAIN     DBRCDF4                            98
     C                   ITER
     C*** Found RCD, leave
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     P2KY          READE     DBRCDF4                                98
     C                   ENDDO
     C                   ENDIF
     C*** BEG ADD *** 04/06/04 ***************************
     C                   ENDIF
     C*** END ADD *** 04/06/04 ***************************
      *
     C*** End add ***** 08/12/03 **************************************
     C     *IN61         IFEQ      '0'
     C                   MOVEL     RPPNAM        SCCKNM
     C                   MOVEL     RPADR1        SCADR1
     C                   MOVEL     RPADR2        SCADR2
     C                   MOVE      RPZIP         XXZIP
      *
      *  LOAD CITY, STATE AND ZIP
      *
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     RPST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
      *
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     RPCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCCSZ
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C     RPCTRY        IFNE      *BLANK
     C     RPCTRY        ANDNE     'USA'
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  RPCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            XXCTRD
     C                   END
     C     RPADR2        IFEQ      *BLANKS
     C                   MOVEL(P)  RPADR3        SCADR2
     C                   MOVEL(P)  XXCTRD        SCCSZ
     C                   MOVE      *BLANKS       SCADR4
     C                   ELSE
     C                   MOVEL(P)  RPADR3        SCCSZ
     C                   MOVEL(P)  XXCTRD        SCADR4
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*** END ADD *** 10/20/03 *******************************************  ****
     C*** BEG CHG *** 10/20/03 *******************************************  ****
     C*********  RPADR2    IFEQ *BLANKS
     C  N05RPADR2        IFEQ      *BLANKS
     C*** END CHG *** 10/20/03 *******************************************  ****
     C                   MOVEL     SCCSZ         SCADR2
     C                   MOVEL     *BLANKS       SCCSZ
     C                   END
     C                   MOVEL     RPRMDS        SCRMDS
      *
     C     RPRMK1        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPRMK1        XXKEY
     C     TBKEY         CHAIN     PRC01                              61
     C                   MOVEL     RCDESC        SCRMK1
     C                   END
      *
     C     RPRMK2        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPRMK2        XXKEY
     C     TBKEY         CHAIN     PRC01                              61
     C                   MOVEL     RCDESC        SCRMK2
     C                   END
      *
     C     RPRMK3        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPRMK3        XXKEY
     C     TBKEY         CHAIN     PRC01                              61
     C                   MOVEL     RCDESC        SCRMK3
     C                   END
      *
     C     RPCKCN        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     RPCKCN        XXKEY
     C     TBKEY         CHAIN     PRC01                              61
     C                   MOVEL     RCDESC        SCCKCN
     C                   END
      *
      *  LOAD THE EOMB NUMBER AND INVOICE NUMBER
      *** BEGIN DELETE *** 10/18/98 ***********************************
     C******************** Z-ADDPAEOB#    SCEOB#
      *** END DELETE ***** 10/18/98 ***********************************
     C                   ELSE
     C*
     C*** BEGIN CHG *** 02/20/98 ***********************************************
     C                   MOVE(P)   PAFEIN        XXFEIN            9
     C*** END CHG ***** 02/20/98 ***********************************************
     C*
     C     PAFEIN        IFEQ      *ZERO
     C*** BEGIN CHG *** 02/20/98 ***********************************************
     C********** PAFEIN    OREQ SSN
     C     XXFEIN        OREQ      SSN
     C*** END CHG ***** 02/20/98 ***********************************************
     C     PARETP        OREQ      'C'
      *
     C******************   MOVELSCCNAM    SCCKNM
     C                   MOVEL     S2NAME        SCCKNM
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C**********           MOVELCDADR3    SCADR3    P
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  CDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            XXCTRD
     C                   END
     C     CDADR2        IFEQ      *BLANKS
     C                   MOVEL(P)  CDADR3        SCADR2
     C     CDCTRY        CAT(P)    XXCTRD:2      SCCSZ
     C                   MOVE      *BLANKS       SCADR4
     C                   ELSE
     C                   MOVEL(P)  CDADR3        SCCSZ
     C     CDCTRY        CAT(P)    XXCTRD:2      SCADR4
     C                   ENDIF
     C*********            MOVELCDCTRY    SCCTRY    P
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVE      CDZIP         XXZIP
      *
      *  LOAD CITY, STATE AND ZIP
      *
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     CDST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
      *
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCCSZ
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C*********  CDADR2    IFEQ *BLANKS
     C  N05CDADR2        IFEQ      *BLANKS
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   MOVEL     SCCSZ         SCADR2
     C                   MOVEL     *BLANKS       SCCSZ
     C                   END
      *
     C                   ELSE
      *
     C     PDKEY         CHAIN     PPD01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     PDNAME        SCCKNM
      **** BEGIN ADD **** 04/02/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C*** Begin add *** 02/27/02 **************************************
     C                   MOVEL     PDADR1        SCCKNM
     C*** End add ***** 02/27/02 **************************************
     C                   MOVEL     PDADR2        SCADR1
     C                   ELSE
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVEL     PDADR1        SCADR1
      **** BEGIN ADD **** 04/02/99 ************************************
     C*** Begin add *** 02/27/02 **************************************
     C                   MOVEL     PDADR2        SCADR2
     C*** End add ***** 02/27/02 **************************************
     C                   ENDIF
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVE      PDZIP         XXZIP
      *
      *  LOAD CITY, STATE AND ZIP
      *
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     PDST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
      *
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C*** Begin dlt *** 02/27/02 **************************************
     C******************** MOVELXXLAST    SCADR2
     C*** End dlt ***** 02/27/02 **************************************
     C*** Begin add *** 02/27/02 **************************************
     C     SCADR2        IFEQ      *BLANKS
     C                   MOVEL     SCCSZ         SCADR2
     C                   MOVEL     *BLANKS       SCCSZ
     C                   ELSE
     C                   MOVEL     XXLAST        SCCSZ
     C                   END
     C*** End add ***** 02/27/02 **************************************
     C*** Begin add *** 03/23/05 **************************************
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  PDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            XXCTRD
     C                   END
     C     PDADR2        IFEQ      *BLANKS
     C                   MOVEL(P)  PDADR3        SCADR2
     C     PDCTRY        CAT(P)    XXCTRD:2      SCCSZ
     C                   MOVE      *BLANKS       SCADR4
     C                   ELSE
     C                   MOVEL(P)  PDADR3        SCCSZ
     C     PDCTRY        CAT(P)    XXCTRD:2      SCADR4
     C                   ENDIF
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*** End add ***** 03/23/05 **************************************
      *
     C                   END
     C                   END
      *
     C                   END                                                    CHAIN WDRPP
      *
     C*** Begin dlt *** 02/27/02 **************************************
     C**********           ELSE
      **********
      *  IF TRANSACTION IS A NON-PAYMENT TYPE SHOW THE DESCRIPTION
      *  (VOID, ADJUSTMENT, STOP PAYMENT, RECOVERY, REVERSAL, GROUP)
      **********
     C**********           Z-ADD1         X       30
     C********** PATYPE    LOKUPCD,X                     61
     C********** *IN61     IFEQ '1'
      *** BEGIN CHG *** 10/31/97 *********************************************
     C*********************MOVELDS,X      SCADR1
     C**********           MOVE *BLANK    SCCKNM
     C**********           MOVELDS,X      SCADR1    P
     C**********           MOVE *BLANK    SCADR2
     C**********           MOVE *BLANK    SCCSZ
      *** END   CHG *** 10/31/97 *********************************************
     C**********           END
      **********
     C**********           END                             PATRCD P/M
     C*** End dlt ***** 02/27/02 **************************************
      *
     C*********************WRITESCR99
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
      *
     C     SCWKS         IFEQ      *ZEROS
     C     SCDAY         ANDEQ     *ZEROS
     C                   MOVE      '1'           *IN51
     C                   ELSE
     C                   MOVE      '0'           *IN51
     C                   END
      *
     C     SCFEIN        IFEQ      *ZEROS
     C     SCMOD         ANDEQ     *ZEROS
     C                   MOVE      '1'           *IN52
     C                   ELSE
     C                   MOVE      '0'           *IN52
     C                   END
      *
     C     SCRMK1        IFEQ      *BLANKS
     C     SCRMK2        ANDEQ     *BLANKS
     C     SCRMK3        ANDEQ     *BLANKS
     C     SCCKCN        ANDEQ     *BLANKS
     C*** BEGIN ADD *** 11/04/99 **************************************
     C     SCRMDS        ANDEQ     *BLANKS
     C*** END ADD ***** 11/04/99 **************************************
     C                   MOVE      '1'           *IN53
     C                   ELSE
     C                   MOVE      '0'           *IN53
     C                   END
      *
      *
      *** BEGIN DEL *** 10/31/97 ***********************************************
     C*********************ELSE
      ***IF CHECK IS A GROUP CHECK DISPLAY GROUP
     C*****                MOVELDS,2      SCADR1
     C*****                ENDIF                           PAPGTP<>'G'
      *** END   DEL *** 10/31/97 ***********************************************
      *
      *** BEGIN ADD *** 07/06/99 **************************************
     C     PADPCD        IFEQ      *ZERO
     C                   MOVE      'N'           SCDUPM
     C                   ELSE
     C                   MOVE      'Y'           SCDUPM
     C                   ENDIF
      *** END ADD ***** 07/06/99 **************************************
     C*** BEG ADD **** 01/05/11 ***************************************
     C* New invoice field will display only for client 114, when payment
     C* reserve type is expence,legal or vocational rehab.
     C*** Begin Chg *** 04/10/19 **************************************
     C*****CLTID         IFEQ      '114'
     C*****PARETP        ANDEQ     'E'
     C**********
     C*****CLTID         OREQ      '114'
     C*****PARETP        ANDEQ     'L'
     C**********
     C*****CLTID         OREQ      '114'
     C*****PARETP        ANDEQ     'M'
     C*****PARECD        ANDEQ     'RE'
     C     CLTID         IFEQ      '114'
     C     CLTID         OREQ      '152'

     C     PARETP        IFEQ      'E'
     C*
     C     PARETP        OREQ      'L'
     C*
     C     PARETP        OREQ      'M'
     C     PARECD        ANDEQ     'RE'
     C*** End   Chg *** 04/10/19 **************************************
     C*
     C                   MOVEL     PAINV#        SCINVO
     C                   SETON                                        89
     C                   ENDIF                                                  CLTID = '114'
     C*** Begin Add *** 04/10/19 **************************************
     C                   ENDIF                                                  CLTID = '114'
     C*** End   Add *** 04/10/19 **************************************
     C*** END ADD ***** 01/05/11 **************************************
      *
     C                   ENDSR
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
     C                   ENDSR
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
      *
      *
     C*** BEG ADD *** 03/03/99 ***************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C*** END ADD *** 03/03/99 ***************************************
     C*** BEGIN ADD *** 03/17/11 ****************************************
     C                   MOVEL(P)  HEAD01        SCCAP1                         Mailing date
     C     CLTID         IFEQ      '114'
     C                   MOVEL(P)  HEAD02        SCCAP1                         Processed date
     C                   ENDIF                                                  e CLTID = 114
     C*** END ADD ***** 03/17/11 ****************************************
     C                   ENDSR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
      *  Duplicate Payment exclusion
      *
     C     @@MODE        IFEQ      'U'
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   Z-ADD     DSTIME        PATIME
      *
      *** BEGIN CHANGE *** 08/03/99 ***********************************
     C******************** Z-ADDPADT      PADPCD
     C     PADPCD        IFEQ      *ZERO
     C                   Z-ADD     PADT          PADPCD
     C                   ELSE
     C                   MOVE      *ZERO         PADPCD
     C                   ENDIF
      *** END CHANGE ***** 08/03/99 ***********************************
     C                   MOVE      Q1USER        PADPCU
      *
     C                   UPDATE    DBRCDF
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*** BEGIN ADD *** 08/29/11 *****************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** END ADD *** 08/29/11 *******************************************
     C                   WRITE     DBRCDF
     C                   FEOD      WDPAP
     C                   ENDSR
     C*** BEG ADD *** 06/08/10 **************************************
      *==============================================================
      *   SRPAST - SUBROUTINE TO CALCULATE A DATE IN THE PAST    *
      *==============================================================
     C     SRPAST        BEGSR
     **
     C                   Z-ADD     1             SRDAYS
      *
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTO              8 0
     C                   MOVE      XXTO          SRDTE             8 0
      *
      * Calculate a date in the past
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C                   MOVE(P)   SRDTE         X8DAT             8 0
      *
     C                   MOVE(P)   X8DAT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDCY --> CYMD
     C                   MOVEL(P)  SRDTO         XXDAT
     C*
     C                   ENDSR
     C*** END ADD *** 06/08/10 **************************************
      *** Begin dlt *** 02/27/02 **************************************
      **  NON-PAYMENT CODES
      ****A
      ****G
      ****R
      ****S
      ****V
      ****Z
      **  NON-PAYMENT DESCRIPTIONS
      ****ADJUSTMENT
      ****GROUP CHECK
      ****RECOVERY
      ****STOP PAYMENT
      ****VOID
      ****REVERSAL
      *** End dlt ***** 02/27/02 **************************************
