     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W8070RA1   IA-1 (FIRST REPORT OF INJURY)
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM PRINTS THE IA-1 (FIRST     *
     H*               REPORT OF INJURY) FORM, THEN WRITES A DARTS     *
     H*               REMARK WHICH WILL BE ATTACHED TO THE IA-1       *
     H*               RESOLVED OUTPUT.                                *
     H*                                                               *
     H* CALLED FROM:  W8070CA1                                        *
     H* CALLS TO:     W8070C1                                         *
     H* PROJECT:      25WDE         C:15           01/15/96           *
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 06/09/20 C:33  Project: SY420
     H*                Mask SSN
     H*
     H* 07/19/17 C:53 PROJECT WC2499
     H*               Remove Scriptura processing.
     H*
     H* 12/09/15 C:62 PROJECT SY371                                   *
     H*               Add Scriptura processing.                       *
     H*                                                               *
     H* 07/27/10 C:235 PROJECT: 25WRG/PRISS-41                        *
     H*                Company Name should print for all client based *
     H*                on field XXCONM and for TASB based on field    *
     H*                F3INAM                                         *
     H*                                                               *
     H* 04/19/10 C:235 PROJECT: 25WRG                                 *
     H*                CLAIMS GENERAL RETROFIT                        *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 04/07/05       PROJECT: 126WDK                                *
     H*          C:15  PROJECT: 126WBC                                *
     H*                ADDED PARMS FOR GRP/FYR/CASE TO BE ABLE TO     *
     H*                PRINT THIS FORM FROM WW CLAIMS.                *
     H*                                                               *
     H*                                                               *
     H* 03/08/05 C:15  Support: WC126/45864                           *
     H*                Chgd pgm to print the same policy # that is    *
     H*                reported in WCSTAT.                            *
     H*                                                               *
     H* 10/20/03 C:53 Project: WC909                                  *
     H*               Added country info.                             *
     H*                                                               *
     H* 04/03/03 C:136 PROJECT: WC1084                                *
     H*                Get number of dependents from WMCMP and pull   *
     H*                state of hire from State NOI file.             *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get Employer Contact info from WMECP.          *
     H*                                                               *
     H* 06/13/00 C:15  Support: WC048/23117                           *
     H*                If the initial treatment code was blank, the   *
     H*                pgm defaulted to '5'.  Fixed code.             *
     H*                                                               *
     H* 05/18/00 C:53  PROJECT: WC962
     H*                Added jurisdiction state to WMNIP chain.
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 10/01/99 C:112 SUPPORT: 19857                                 *
     H*                Added code to clear display fileds for Hospital*
     H*                information                                    *
     H*
     H* 08/11/99 C:112 SUPPORT: 19190, 19225                          *
     H*                Added code to correct print problem with       *
     H*                Physician Name and Address.  Fields were not   *
     H*                being cleared on an unsuccesful read.          *
     H*                Hospital name and address information loaded   *
     H*                first from Payee Demographics if there is a    *
     H*                FEIN and MOD in the Claim Master for the claim *
     H*                otherwise load information from the Claim      *
     H*                Master Hospital name and address fileds.       *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 06/25/99 C:112 Support: Seq # 17915, 18393                    *
     H*               Implements separate condition check for         *
     H*               'Full Pay for Day of Injury' form field.        *
     H*               Implements new check field to populate Physician*
     H*               Name and Address and Hospital location and name.*
     H*                                                               *
     H*                                                               *
     H* 04/02/99 C:90 PROJECT: WC889                                  *
     H*               IF DBA FLAG = 'Y' LOAD PDADR2, ELSE LOAD PDADR1.*
     H*                                                               *
     H* 11/22/98 C:58 PROJECT: SUPP WC081 #14631
     H*                Use claim accident description if ancillary
     H*                fields are blank.
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                ADDED FIELD CMSMCD, LOADED FROM CTSMCD.        *
     H*                                                               *
     H* 01/03/97 C:58 PROJECT: 81WBP
     H*               Correct Provider Key PDKY set up to get Hospital
     H*                                                               *
     H* 08/26/96 C:58 PROJECT: 81WBP
     H*               EDI - GENERAL CHANGES, CONFORM TO BASE RELEASE.
     H*               ADD WMNIL  TO GET ACCIDENT L0CATION DATA.
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  20  -  EMPLOYER ADDRESS 2 = BLANKS                           *
     H*  21  -  EMP LOCATION ADDRESS 2 = BLANKS                       *
     H*  22  -  COMPANY ADDRESS 2 = BLANKS                            *
     H*  23  -  CLAIMANT ADDRESS 2 = BLANKS                           *
     H*  26  -  PRINTS 'X' IN ACC TIME (AM) BOX                       *
     H*  27  -  PRINTS 'X' IN ACC TIME (PM) BOX                       *
     H*  29  -  COMPANY AUXILIARY ADDRESS 2 = BLANKS
     H*  60  -  GENERAL USAGE (CHAIN, READ, etc...)                   *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMLGL1    IF   E           K DISK
     FWMLGL     UF   E           K DISK
     F                                     RENAME(PLG01:PLG02)
     FWMFNL     IF   E           K DISK
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************
     FSTCTL1    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWACML     IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK
      *** BEG DEL ***** 04/03/03 **************************************
     F*************  WDACL   IF  E           K        DISK
      *** END DEL ***** 04/03/03 **************************************
     FWMEML     IF   E           K DISK
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
     FWDELL     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMNIL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F*
     FWTGTL     IF   E           K DISK
     F*** BEGIN DEL *** 04/26/00 **************************************
     F********SDCML   O   E           K        DISK
     F********    PCM01                             KRENAMEPCM03
     F********SDCMLA  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM04
     F*** END DEL *** 04/26/00 ****************************************
     F*** BEGIN ADD *** 06/25/99 **************************************
     FWMTPL1    IF   E           K DISK
     F*** END ADD ***** 06/25/99 **************************************
     F*** Begin add *** 03/08/05 **************************************
     FWMRPL     IF   E           K DISK
     F*** End add ***** 03/08/05 **************************************
     F*** BEG  ADD  *** 04/19/10 **************************************
     FWDF3L     IF   E           K DISK
     F*** END  ADD  *** 04/19/10 **************************************
     f*** Begin Del *** 07/19/17 ***************************************
     f*** Begin Add *** 12/09/15 ***************************************
     F**SXFRP     uf a e             disk    Rename(pfr01:pfr0p)
     F**XMLPREL1  if   e           k disk    Rename(xp100:pxp01)
     f*** End   Add *** 12/09/15 ***************************************
     f*** End   Del *** 07/19/17 ***************************************
     FQSYSPRT   O    F  127        PRINTER OFLIND(*INOF)
     F                                     USROPN

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D PR              S              1    DIM(40)
     D*
     D*** BEGIN DEL *** 04/26/00 **************************************
     D********PCM04
     D********      CMKEY                           C4KEY
     D********      CMCSID                          C4CSID
     D********      CMCOMM                          C4COMM
     D********      CMCDAT                          C4CDAT
     D********      CMCODE                          C4CODE
     D********      CMDCID                          C4DCID
     D********      CMPNAM                          C4PNAM
     D********      CMTRDT                          C4TRDT
     D********      CMTRTM                          C4TRTM
     D********      CMSEQ#                          C4SEQ#
     D********      CMSECL                          C4SECL
     D********      CMSBYL                          C4SBYL
     D********      CMSTAT                          C4STAT
     D********      CMUSER                          C4USER
     D********      CMPGM                           C4PGM
     D********      CMTIME                          C4TIME
     D********      CMDT                            C4DT
     D********      CMIUSR                          C4IUSR
     D********      CMFOLD                          C4FOLD
     D********      CMDCDT                          C4DCDT
     D********      CMDIFL                          C4DIFL
     D********      CMAPID                          C4APID
     D********      CMIDOC                          C4IDOC
     D********      CMIQUE                          C4IQUE
     D********      CMIMGF                          C4IMGF
     D********      CMIPTH                          C4IPTH
     D********      CMPIDX                          C4PIDX
     D********      CMQPTY                          C4QPTY
     D********      CMDCTP                          C4DCTP
     D********      CMIPGM                          C4IPGM
     D********      CMSMCD                          C4SMCD
     D********      CMFL01                          C4FL01
     D********      CMFL02                          C4FL02
     D********      CMFL03                          C4FL03
     D********      CMFL04                          C4FL04
     D********      CMFL05                          C4FL05
     D********      CMRMCD                          C4RMCD
     D********      CMIDT                           C4IDT
     D*** END DEL *** 04/26/00 ****************************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXACTM                 1      6  0
     D  XXTIME                 1      4  0
     D                 DS
     D  XXTIM2                 1      6  0
     D  X1TIM2                 1      4  0
     D                 DS
     D*** Begin chg *** 03/08/05 **************************************
     D***********************                 1  15 XXPOL#
     D  XXPOL1                 1     15
     D*** End chg ***** 03/08/05 **************************************
     D  CMFND                  1      3  0
     D  CMEMP#                 4     12  0
     D  CMFYR                 13     15  0
     D                 DS
     D  XXFNMD                 1     12
     D  XXFEIN                 1      9  0
     D  XXMOD                 10     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** BEG ADD ******* 01/31/02 **********************************
     d*** Begin Del *** 07/19/17 ***************************************
     d*** Begin Add *** 12/09/15 *********************************************
     D* isScriptUsed    s              1A   inz('N')
     D* isUseSpoolOut   s              1n   inz
     D* isUseXML        S              1n   inz
     D* outCall         s             11
     D* InXML           s             60
     D* InReturn        s            250
     D* OutMode         s              1
     D* OutRrn          s              9
     D* OutPrefix       s              2
     D* OutString       s             10
     D* OutCo#          s              3
     D* OutFnd          s              3
     D* isTrCd          s               n
     D* chrKey          s                   Like(PCkey)
     D* procType        s            100
     d* massFlag        s              1    Inz('Y')
     d* xmlLink         s             60    Inz(' ')
     D*
     D* Get Generic Table Value
     D* GetTblDesc      pr            40
     D*   tableID                      4    Const
     D*   tableKey                    10    Const
     D*
     D* Get Cause Code Description
     D* GetCauseDs      pr            40
     D*   causeCd                      2    Const
     D*
     D* Scripture Processing Program
     D* StLetter        pr                  ExtPgm('STLETTER')
     D*   outCall_                    11
     D*   InXML_                      60
     D*   InReturn_                  250
     D*   OutMode_                     1
     D*   OutRrn_                      9
     D*   OutPrefix_                   2
     D*   OutString_                  10
     D*   OutCo#_                      3
     D*   OutFnd_                      3
     d*   mass_                        1    const options(*nopass)
     d*   link_                       60    const options(*nopass)
     D*/copy sPrModule
     D*/copy sPrXML
     C***  Begin Add  ***  06/09/20  *******************************
     D/copy sPrSecure
     C***  End Add  ***  06/09/20  *******************************
     d*** End   Add *** 12/09/15 *********************************************
     d*** End   Del *** 07/19/17 ***************************************
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      *
      *  Employer Contact
      *

     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXHCPY
     C                   KFLD                    XXJRST
     C                   KFLD                    XXDATE
     C*
     C     LGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    LGIDT
     C                   KFLD                    LGITM
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE EDI CLAIM CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGJST
     C                   KFLD                    LGTSET
     C*
     C     F3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    LGJST
     C*
     C*  KEY LIST FOR THE DARTS REMARK CODE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    IXHCPR
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE ADDITIONAL ADDRESS FILE
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    XXFYR
     C*
     C*  KEY LIST FOR THE ADDITIONAL LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCTG1
     C*
     C*  KEY LIST FOR THE AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*** BEGIN DELETE *** 06/25/99 ***********************************
     C******************** KFLD           CMACDT
     C*
     C********** AHKY1     KLIST
     C******************** KFLD           XXCO#
     C******************** KFLD           XXFND
     C******************** KFLD           CMEMP#
     C******************** KFLD           CMDIV
     C******************** KFLD           CMFYR
     C*** END DELETE ***** 06/25/99 ***********************************
     C*
     C*  KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AHAGT#
     C*
     C*  KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  KEY LIST FOR THE STATE NOI INFORMATION FILE
     C*
     C     NIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X2ST                           ADD 05/18/00
     C*
     C*  KEY LIST FOR THE PROVIDER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********
     C***KEY*LIST FOR THE DARTS FILE
     C********
     C********   C4KY      KLIST
     C********             KFLD           QCKEY
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 06/25/99 **************************************
     C*
     C*  KEY LIST FOR THE TREATING PHYSICIAN HISTORY FILE
     C*
     C     TPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** END ADD ***** 06/25/99 **************************************
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFROI            2
     C                   PARM                    QQJRST            2
     C                   PARM                    QQDATE            8
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C***  END   ADD  ***  04/07/05 ***********************************
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
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    LGJST
     C                   PARM                    LGTSET
     C                   PARM                    XXFND
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
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    LGHCPY        XXHCPY
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXJRST    XXJRST
     C     *LIKE         DEFINE    LGJST         XXJRST
     C*** END CHG ****** 03/24/10 *************************************
     C     *LIKE         DEFINE    CMDT          XXDATE
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    CDCTY         XXCITY
     C     *LIKE         DEFINE    CDST          XXST
     C     *LIKE         DEFINE    CDZIP         XXZIP
     C     *LIKE         DEFINE    FNSFIN        XXSFIN
     C     *LIKE         DEFINE    LGTSET        SVTSET
     C*** BEGIN CHG *** 04/26/00 **************************************
     C********   *LIKE     DEFN CMDCID    QQDCID
     C     *LIKE         DEFINE    Q#DCID        QQDCID
     C********   *LIKE     DEFN CMFOLD    QQFLDR
     C     *LIKE         DEFINE    Q#FOLD        QQFLDR
     C*** END CHG *** 04/26/00 ****************************************
      *** BEGIN ADD *** 08/11/99 **************************************
     C     *LIKE         DEFINE    TPPHYN        XXPHYN
     C     *LIKE         DEFINE    TPPHYA        XXPHYA
      *** END ADD ***** 08/11/99 **************************************
     C     *LIKE         DEFINE    NIST          X2ST                           ADD 05/18/00
     C                   Z-ADD     0             XXDTE             8 0
      *** BEGIN ADD *** 04/19/10 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *** END   ADD *** 04/19/10 **************************************
     C*** Begin Del *** 07/19/17 ***************************************
     C*** Begin Add *** 12/09/15 *********************************************
      **/Free
      **
      ** // Determine if this module get spool file and/or XML output
      ** procType = 'First Report of Injury';
      ** isUseSpoolOut = UseSpoolOut(procType);
      ** isUseXML = UseScriptura(procType) Or
      **            Use3rdXml(procType);
      ** if isUseXML;
      **   isScriptUsed = 'Y';
      ** else;
      **   isScriptUsed = 'N';
      ** Endif;
      **
      */End-free
     C*** End   Add *** 12/09/15 ******************************************************
     C*** End   Del *** 07/19/17 ***************************************
     C*
     C                   MOVE      *BLANKS       XXAREA            3
     C                   MOVE      *BLANKS       XEAREA            5
     C                   MOVE      *BLANKS       XCAREA            5
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVEL     QQFROI        XXHCPY
     C                   MOVE      QQJRST        XXJRST
     C                   MOVE      QQJRST        X2ST                           ADD 05/18/00
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C***  END   ADD  ***  04/07/05 ***********************************
     C                   MOVE      QQDATE        XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   TIME                    DSDTTM
     C*
     C*  LOAD THE COMPANY INFORMATION.
     C*
     C                   MOVE      *BLANKS       XXCONM           40
     C                   MOVE      *BLANKS       XCOCSZ           40
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM
     C                   MOVEL     COCTY         XXCITY
     C                   MOVEL     COST          XXST
     C                   Z-ADD     COZIP         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XCOCSZ
     C     COADR2        COMP      *BLANKS                                22
     C                   END
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C     @@CASE        COMP      *BLANKS                            3131
     C***  END   ADD  ***  04/07/05 ***********************************
     C*
     C***
     C*
     C*  READ ALL RECORDS IN THE LOG HISTORY FILE.
     C*
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C     *IN31         IFEQ      *OFF
     C***  END   ADD  ***  04/07/05 ***********************************
     C     LGKY          CHAIN     PLG01                              60
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   ELSE
     C     CMKY          CHAIN     PCM01                              60
     C                   ENDIF
     C***  END   ADD  ***  04/07/05 ***********************************
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C     *IN31         IFEQ      *OFF
     C***  END   ADD  ***  04/07/05 ***********************************
     C     LGPRSQ        IFNE      0
     C     LGTSET        ORNE      '148'
     C                   GOTO      SKIP
     C                   END
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   ENDIF
     C***  END   ADD  ***  04/07/05 ***********************************
     C*
     C                   EXSR      SRINIT
     C*
     C*  RETRIEVE THE CLAIM MASTER INFORMATION.
     C*
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C     *IN31         IFEQ      *OFF
     C***  END   ADD  ***  04/07/05 ***********************************
     C     CMKY          CHAIN     PCM01                              60
     C   60CMKY          CHAIN     PCM02                              60
     C   60              GOTO      SKIP
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   ENDIF
     C***  END   ADD  ***  04/07/05 ***********************************
     C*
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C                   EVAL      XXSS#=DISPLAYSS#(XXSS#)
     C***  End Add  ***  06/09/20  *******************************
     C
     C                   MOVEL(P)  CMOCDS        XXOCDS           30
     C*
     C*  RETRIEVE THE CLAIMANT NAME AND ADDRESS.
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         XXCNAM
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       CDFSN:1       XXCNAM
     C     XXCNAM        CAT       CDMID:1       XXCNAM
     C                   MOVEL     CDADR1        XXCAD1
     C                   MOVEL     CDADR2        XXCAD2
     C*
     C                   MOVE      CDCTY         XXCITY
     C                   MOVE      CDST          XXST
     C                   Z-ADD     CDZIP         XXZIP
     C                   EXSR      SRADDR
     C*
     C*** BEG ADD *** 10/20/03 *****************************
     C     CDCTRY        IFNE      *BLANKS
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        XXCSZ
     C                   END
     C*** END ADD *** 10/20/03 *****************************
     C                   MOVEL     XXCSZ         XXCCSZ
     C*** BEG ADD *** 10/20/03 *****************************
     C                   MOVE      CDCTRY        XXCCSZ
     C*** END ADD *** 10/20/03 *****************************
     C*
     C     CDADR2        COMP      *BLANKS                                23
     C*
     C                   END
     C*
     C*
     C*  GET THE EMPLOYER MASTER AND DETAIL INFORMATION
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Get Employer Contact Name
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         EMCONT
      *
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVEL     EMCONT        XXCONT
     C                   MOVEL     EMNAME        XXENAM
     C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL     EMPAD1        XXEAD1
     C                   MOVEL     EMPAD2        XXEAD2
     C                   MOVEL     EMPCTY        XXECTY
     C                   MOVE      EMPST         XXEST
     C                   Z-ADD     EMPZIP        XXZIP
     C                   ELSE
     C                   MOVEL     EMMAD1        XXEAD1
     C                   MOVEL     EMMAD2        XXEAD2
     C                   MOVEL     EMMCTY        XXECTY
     C                   MOVE      EMMST         XXEST
     C                   Z-ADD     EMMZIP        XXZIP
     C                   END
     C*
     C                   MOVE      *BLANKS       XXEZIP
     C                   MOVEL     XXZIP         XXZP1
     C                   MOVE      XXZIP         XXZP2
     C                   MOVEL     XXZP1         XXEZIP
     C     XXZP2         IFEQ      '0000'
     C     XXEZIP        CAT       '-':0         XXEZIP
     C     XXEZIP        CAT       XXZP2:0       XXEZIP
     C                   END
     C*
     C                   MOVE      *BLANKS       XEAREA
     C                   MOVE      EMAREA        XXAREA
     C     EMAREA        IFNE      0
     C                   MOVEL     '('           XEAREA
     C     XEAREA        CAT       XXAREA:0      XEAREA
     C     XEAREA        CAT       ')':0         XEAREA
     C                   END
     C*
     C                   END
     C*
     C*  GET THE LOCATION INFORMATION, IF ANY.
     C*
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         IFEQ      '0'
     C     ALPAD1        IFNE      *BLANKS
     C                   MOVEL     ALPAD1        XXLAD1
     C                   MOVEL     ALPAD2        XXLAD2
     C                   MOVEL     ALPCTY        XXCITY
     C                   MOVEL     ALPST         XXST
     C                   MOVEL     ALPZIP        XXZIP
     C                   ELSE
     C                   MOVEL     ALMAD1        XXLAD1
     C                   MOVEL     ALMAD2        XXLAD2
     C                   MOVEL     ALMCTY        XXCITY
     C                   MOVEL     ALMST         XXST
     C                   Z-ADD     ALMZIP        XXZIP
     C                   END
     C*
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXLCSZ
     C     XXLAD2        COMP      *BLANKS                                21
     C                   END
     C*
     C*** Begin add *** 03/08/05 **************************************
     C*  Use the policy # from the Carrier Info file, if it exists.
     C*
     C     ELKY          CHAIN     PRP01                              60
     C   60              MOVE      *BLANKS       RPCAP#
     C     RPCAP#        IFNE      *BLANKS
     C                   MOVEL(P)  RPCAP#        XXPOL#           18
     C                   ELSE
     C                   MOVEL(P)  XXPOL1        XXPOL#
     C                   ENDIF
     C*** End add ***** 03/08/05 **************************************
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C*** BEGIN CHANGE *** 06/25/99 ***********************************
     C     AHKY          SETGT     PAH01
     C********** AHKY1     REDPEPAH01                    60
     C     AHKY          READPE    PAH01                                  60
     C*** END CHANGE ***** 06/25/99 ***********************************
     C     *IN60         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  60
     C                   ENDDO
     C   60              Z-ADD     0             AHAGT#
     C                   MOVE      *BLANKS       AGNAME
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      *ZEROS        AAFEIN
     C                   MOVE      *ZEROS        AAMOD
     C     AAKY          CHAIN     PAA01                              60
     C     AGKY          CHAIN     PAG01                              60
     C                   END
     C*
     C                   MOVE      *BLANKS       XCAREA
     C                   MOVE      CDAREA        XXAREA
     C     CDAREA        IFNE      0
     C                   MOVEL     '('           XCAREA
     C     XCAREA        CAT       XXAREA:0      XCAREA
     C     XCAREA        CAT       ')':0         XCAREA
     C                   ENDIF
     C*
     C*  GET THE EMPLOYMENT STATUS DESCRIPTION
     C*
     C                   MOVEL     'EMST'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMESTS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXESTS
     C*
     C*  CALCULATE THE # OF DEPENDENTS
     C*
      *** BEG CHG ***** 04/03/03 *******************************************
     C**********           Z-ADD0         XXDPND
     C********** ACKY      CHAINPAC01                60
     C********** *IN60     DOWEQ'0'
     C********** ACREL     IFNE *BLANK
     C**********           ADD  1         XXDPND
     C**********           END
     C********** ACKY      READEPAC01                    60
     C**********           END
     C                   Z-ADD     CM#DEP        XXDPND
      *** END CHG ***** 04/03/03 *******************************************
     C*
     C*  GET THE TYPE OF INJURY DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              60
     C  N60              MOVEL     NADESC        XXNADS
     C*
     C*  GET THE PART OF BODY DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C  N60              MOVEL     PADESC        XXPADS
     C*
     C*  GET THE STATE NOI INFORMATION
     C*
     C     NIKY          CHAIN     PNI01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       NI1A1
     C                   MOVE      *BLANKS       NI1A2
     C                   MOVE      *BLANKS       NI20A1
     C                   MOVE      *BLANKS       NI20A2
     C                   MOVE      *BLANKS       NI40A1
     C                   MOVE      *BLANKS       NI40A2
     C                   MOVE      *BLANKS       NI40A3
     C                   MOVE      *BLANKS       NI40A4
     C                   MOVE      *BLANKS       NI40A5
     C                   MOVE      *BLANKS       NI40A6
     C                   MOVE      *BLANKS       NI40A7
     C                   MOVE      *BLANKS       NI40A8
     C                   MOVE      *BLANKS       NI60A1
     C                   MOVE      *BLANKS       NI60A2
     C                   MOVE      *BLANKS       NI60A3
     C                   MOVE      *BLANKS       NI60A4
     C                   MOVE      *BLANKS       NI60A5
     C                   MOVE      *BLANKS       NI60A6
     C                   MOVE      *BLANKS       NI80A1
     C                   MOVE      *BLANKS       NI80A2
     C                   MOVE      *BLANKS       NI80A3
     C                   MOVEL     *BLANKS       NI40A9
     C                   MOVEL     *BLANKS       NI40A0
     C                   MOVEL     *BLANKS       NI30A1
     C                   MOVEL     *BLANKS       NI10A1
     C                   Z-ADD     0             NIDT5
     C                   Z-ADD     0             NI9N1
     C                   Z-ADD     0             NIPREP
     C                   MOVEL     *BLANKS       NIPRNM
     C                   MOVEL     *BLANKS       NIPRPH
     C                   MOVEL     *BLANKS       NICONT
     C                   MOVEL     *BLANKS       NI40A1
     C                   MOVEL     *BLANKS       NI40A2
     C                   MOVEL     *BLANKS       NI25A1
     C                   MOVEL     *BLANKS       NI2A1
     C                   Z-ADD     *ZEROS        NI9N2
     C                   END
     C*
     C     XXLAD1        IFEQ      *BLANKS
     C                   MOVEL     NI40A1        XXLAD1
     C                   MOVEL     NI40A2        XXLAD2
     C     XXLAD2        COMP      *BLANKS                                21
     C                   CAT       NI40A2:2      NI40A1
     C     NI25A1        CAT(P)    ',':0         XXLCSZ
     C                   CAT       NI2A1:1       XXLCSZ
     C                   MOVEL     NI9N2         XXLZP1            5
     C                   MOVE      NI9N2         XXLZP2            4
     C                   CAT       XXLZP1:1      XXLCSZ
     C                   CAT       '-':0         XXLCSZ
     C                   CAT       XXLZP2:0      XXLCSZ
     C                   ENDIF
     C*
     C                   MOVE      *OFF          BL                1
     C                   MOVEA     NI40A7        PR
     C     1             DO        40            X                 3 0
     C     PR(X)         IFEQ      ','
     C     BL            OREQ      *ON
     C                   MOVE      ' '           PR(X)
     C                   MOVE      *ON           BL
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     PR            NI40A7
     C*
     C                   SELECT
     C     NI20A9        WHENEQ    'Day'
     C                   MOVEL     'X'           XXWGP1
     C     NI20A9        WHENEQ    'Week'
     C                   MOVEL     'X'           XXWGP3
     C     NI20A9        WHENEQ    'Month'
     C                   MOVEL     'X'           XXWGP2
     C                   OTHER
     C                   MOVEL     'X'           XXWGP4
     C                   ENDSL
     C*
     C                   MOVEL     NICONT        XXCONT
     C*
     C                   Z-ADD     NIPREP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         XXDTE
     C                   ENDIF
     C*
     C                   MOVEA     NIPRNM        PR
     C     1             DO        40            X                 3 0
     C     PR(X)         IFEQ      ','
     C                   MOVE      ' '           PR(X)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVEA     PR            NIPRNM
     C                   MOVEL     NIPRNM        XXPRNM           30
     C*
     C                   MOVEL     NIPRPH        XXTEN            10
     C                   MOVEL     XXTEN         XXPRPH           10 0
     C*
     C                   MOVEL(P)  NI80A1        XX30A1
     C                   MOVE(P)   NI80A1        XX50A7
     C                   MOVEL(P)  NI80A2        XX30A2
     C                   MOVEL(P)  XX50A7        NI80A2
     C                   MOVE      XX30A2        NI80A2
     C                   MOVEL     NI60A1        XX50A1
     C                   MOVEL     NI60A2        XX50A2
     C                   MOVEL     NI60A3        XX50A3
     C                   MOVEL     NI60A4        XX50A4
     C                   MOVEL     NI60A5        XX50A5
     C                   MOVEL     NI60A6        XX50A6
     C*** BEG ADD ***** 04/03/03 **************************************
     C                   MOVEL     NI10A2        XXSTOH            2
     C*** END ADD ***** 04/03/03 **************************************
     C*
     C*** BEGIN ADD *** 11/22/98 **************************************
     C     XX50A3        IFEQ      *BLANK
     C     XX50A4        ANDEQ     *BLANK
     C     CMACDS        CAT       CMACD2:0      XXWK3           100
     C                   MOVEL     XXWK3         XX50A3
     C                   MOVE      XXWK3         XX50A4
     C                   ENDIF                                                  . 50A3,4 Blk
     C*
     C*** END ADD ***** 11/22/98 **************************************
     C*
     C*
     C*  LOAD THE PHYSICIAN AND HOSPITAL NAME AND ADDRESS
     C*
     C*** BEGIN DELETE *** 06/25/99 ***********************************
     C********** NI20A1    IFNE *BLANKS                    PHYS FEIN/MOD
     C**********           MOVELNI20A1    XXFNMD
     C********** PDKY      CHAINPPD01                60
     C********** *IN60     IFEQ *OFF
     C********** PDNAME    CAT  ',':0     NI40A1
     C********** NI40A1    CAT  PDFSN:1   NI40A1
      **** BEGIN ADD **** 04/02/99 ************************************
     C********** PDDBA1    IFEQ 'Y'
     C**********           MOVELPDADR2    NI40A2
     C**********           ELSE
      **** END   ADD **** 04/02/99 ************************************
     C**************       MOVELPDADR1    NI40A2
      **** BEGIN ADD **** 04/02/99 ************************************
     C*************        ENDIF
      **** END   ADD **** 04/02/99 ************************************
     C***************      MOVELPDCTY     XXCITY
     C***************      MOVELPDST      XXST
     C***************      Z-ADDPDZIP     XXZIP
     C***************      EXSR SRADDR
     C***************      MOVELXXCSZ     NI40A3
     C***************      END
     C******************** END
     C*
     C********** NI20A2    IFNE *BLANKS                    HOSP FEIN/MOD
     C**********           MOVELNI20A2    XXFNMD
     C*** END DELETE ***** 06/25/99 ***********************************
     C*
     C*** BEGIN ADD *** 06/25/99 **************************************
     C*
     C*** chain to the TREATING PHYSICIAN HISTORY FILE (WMTPP)
     C*** to obtain Physician Name and Address information
     C     TPKY          CHAIN     PTP01                              60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD *** 08/11/99 **************************************
     C     TPPHYN        IFNE      *BLANKS
     C*** END ADD ***** 08/11/99 **************************************
     C                   MOVEL     TPPHYC        XXCITY
     C                   MOVEL     TPPHYS        XXST
     C                   Z-ADD     TPPHYZ        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXTCSZ
     C*** BEGIN ADD *** 08/11/99 **************************************
     C                   MOVEL     TPPHYN        XXPHYN
     C                   MOVEL     TPPHYA        XXPHYA
     C                   ENDIF
     C*
     C*** END ADD ***** 08/11/99 **************************************
     C                   ENDIF
     C*
     C*
     C*** populate PDKY key fields with Hospital FEIN and MOD codes
     C*** from the Claim Master file WMCMP
     C                   MOVE      CMHOSF        XXFEIN
     C                   MOVE      CMHOSM        XXMOD
     C*** END ADD ***** 06/25/99 **************************************
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN CHANGE *** 06/25/99 ***********************************
     C********** PDNAME    CAT  ',':0     NI40A5
     C     PDNAME        CAT       ',':0         XXPDNM           40
     C********** NI40A5    CAT  PDFSN:1   NI40A5
     C     XXPDNM        CAT       PDFSN:1       XXPDNM
     C*** END CHANGE ***** 06/25/99 ***********************************
      **** BEGIN ADD **** 04/02/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C*** BEGIN CHANGE *** 06/25/99 ***********************************
     C******************** MOVELPDADR2    NI40A6
     C                   MOVEL     PDADR2        XXPDAD           40
     C                   ELSE
      **** END   ADD **** 04/02/99 ************************************
     C******************** MOVELPDADR1    NI40A6
     C                   MOVEL     PDADR1        XXPDAD
     C*** END CHANGE ***** 06/25/99 ***********************************
      **** BEGIN ADD **** 04/02/99 ************************************
     C                   ENDIF
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVEL     PDCTY         XXCITY
     C                   MOVEL     PDST          XXST
     C                   Z-ADD     PDZIP         XXZIP
     C                   EXSR      SRADDR
     C*** BEGIN CHANGE *** 06/25/99 ***********************************
     C******************** MOVELXXCSZ     NI40A7
     C                   MOVEL     XXCSZ         XXPCSZ           40
     C*** END CHANGE ***** 06/25/99 ***********************************
      *** BEGIN ADD *** 08/11/99 **************************************
     C                   ELSE
     C     CMHOSN        IFNE      *BLANK
     C                   MOVEL     CMHOSN        XXPDNM
     C                   MOVEL     CMHOSA        XXPDAD
     C                   MOVEL     CMHOSC        XXCITY
     C                   MOVEL     CMHOSS        XXST
     C                   Z-ADD     CMHOSZ        XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXPCSZ           40
     C                   ENDIF
      *** END ADD ***** 08/11/99 **************************************
     C                   ENDIF
     C*
     C*  LOAD THE PLACE OF ACCIDENT
     C                   MOVEL     NI30A1        XXCITY
     C                   MOVEL     NI10A1        XXST
     C                   Z-ADD     NI9N1         XXZIP
     C                   EXSR      SRADDR
     C                   MOVEL     XXCSZ         XXACSZ
     C*
     C*  LOAD AN 'X' TO THE FIELDS WHICH CORRESPOND TO THE APPROPRIATE
     C*  BOXES ON THE REPORT.
     C*
     C*** BEGIN ADD *** 06/13/00 **************************************
     C     CDSEX         IFNE      *BLANKS
     C*** END ADD ***** 06/13/00 **************************************
     C     CDSEX         IFEQ      'M'                                          CLAIMANT'S SEX
     C                   MOVE      'X'           XXSEX1
     C                   ELSE
     C     CDSEX         IFEQ      'F'
     C                   MOVE      'X'           XXSEX2
     C                   ELSE
     C                   MOVE      'X'           XXSEX3
     C                   END
     C                   END
     C*** BEGIN ADD *** 06/13/00 **************************************
     C                   END
     C*** END ADD ***** 06/13/00 **************************************
     C*
     C*** BEGIN ADD *** 06/13/00 **************************************
     C     CDMSTS        IFNE      *BLANKS
     C*** END ADD ***** 06/13/00 **************************************
     C     CDMSTS        IFEQ      'U'                                          MARITAL STATUS
     C                   MOVE      'X'           XXMST1
     C                   ELSE
     C     CDMSTS        IFEQ      'M'
     C                   MOVE      'X'           XXMST2
     C                   ELSE
     C     CDMSTS        IFEQ      'S'
     C                   MOVE      'X'           XXMST3
     C                   ELSE
     C                   MOVE      'X'           XXMST4
     C                   END
     C                   END
     C                   END
     C*** BEGIN ADD *** 06/13/00 **************************************
     C                   END
     C*** END ADD ***** 06/13/00 **************************************
     C*
     C*** BEGIN ADD *** 06/13/00 **************************************
     C     CMWGPD        IFNE      *BLANKS
     C*** END ADD ***** 06/13/00 **************************************
     C     CMWGPD        IFEQ      '6'                                          WAGE PERIOD
     C                   MOVE      'X'           XXWGP1
     C                   ELSE
     C     CMWGPD        IFEQ      '4'
     C                   MOVE      'X'           XXWGP2
     C                   ELSE
     C     CMWGPD        IFEQ      '1'
     C                   MOVE      'X'           XXWGP3
     C                   ELSE
     C                   MOVE      'X'           XXWGP4
     C                   END
     C                   END
     C                   END
     C*** BEGIN ADD *** 06/13/00 **************************************
     C                   END
     C*** END ADD ***** 06/13/00 **************************************
     C*
     C*** BEGIN ADD *** 06/25/99 **************************************
     C     CMPOI         IFEQ      'Y'                                          FULL SALARY
     C                   MOVE      'X'           XXPOI1
     C                   ELSE
     C                   MOVE      'X'           XXPOI2
     C                   END
     C*** END ADD ***** 06/25/99 **************************************
     C*
     C     CMFSAL        IFEQ      'Y'                                          FULL SALARY
     C                   MOVE      'X'           XXFSL1
     C                   ELSE
     C                   MOVE      'X'           XXFSL2
     C                   END
     C*
     C     CMEADR        IFEQ      'Y'                                          EMP PREMISES
     C                   MOVE      'X'           XXEPR1
     C                   ELSE
     C                   MOVE      'X'           XXEPR2
     C                   END
     C*
     C*** BEGIN ADD *** 06/13/00 **************************************
     C     CMITRT        IFNE      *BLANKS
     C*** END ADD ***** 06/13/00 **************************************
     C     CMITRT        IFEQ      '0'                                          INITIAL TREATMENT
     C                   MOVE      'X'           XXITR0
     C                   ELSE
     C     CMITRT        IFEQ      '1'
     C                   MOVE      'X'           XXITR1
     C                   ELSE
     C     CMITRT        IFEQ      '2'
     C                   MOVE      'X'           XXITR2
     C                   ELSE
     C     CMITRT        IFEQ      '3'
     C                   MOVE      'X'           XXITR3
     C                   ELSE
     C     CMITRT        IFEQ      '4'
     C                   MOVE      'X'           XXITR4
     C                   ELSE
     C                   MOVE      'X'           XXITR5
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*** BEGIN ADD *** 06/13/00 **************************************
     C                   END
     C*** END ADD ***** 06/13/00 **************************************
     C*
     C     NI1A1         IFNE      *BLANKS                                      SAFETY EQUIP PROVIDE
     C     NI1A1         IFEQ      'Y'
     C                   MOVE      'X'           XXSFY1
     C                   ELSE
     C                   MOVE      'X'           XXSFY2
     C                   END
     C                   END
     C*
     C     NI1A2         IFNE      *BLANKS                                      SAFETY EQUIP USED
     C     NI1A2         IFEQ      'Y'
     C                   MOVE      'X'           XXSFY3
     C                   ELSE
     C                   MOVE      'X'           XXSFY4
     C                   END
     C                   END
     C*
     C*  CONVERT ALL DATES WHICH ARE PRINTED.
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTOB
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOH
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTLW
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTRP
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDBDT
     C                   Z-ADD     CMRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRTW
     C                   Z-ADD     CMDOD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOD
     C                   Z-ADD     NIDT5         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCVD
     C     XXRCVD        IFEQ      *ZEROS
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCVD
     C                   ENDIF
     C*
     C*  CONVERT THE BEGIN TIME
     C*
     C                   CLEAR                   XXTIM2
     C     NITIM2        IFGE      130000
     C     NITIM2        SUB       120000        XXTIM2
     C                   ELSE
     C     NITIM2        IFLT      10000
     C                   MOVEL     NITIM2        XXWK4A            4
     C                   MOVEL     '12'          XXWK4A
     C                   MOVEL     XXWK4A        XXTIM2
     C                   ELSE
     C                   Z-ADD     NITIM2        XXTIM2
     C                   END
     C                   END
     C     NITIM2        IFGE      120000
     C     NITIM2        ANDNE     240000
     C                   MOVE      '1'           *IN28
     C                   ELSE
     C                   MOVE      '0'           *IN28
     C                   END
     C*
     C*  CONVERT THE ACCIDENT TIME
     C*
     C     CMACTM        IFGE      130000
     C     CMACTM        SUB       120000        XXACTM
     C                   ELSE
     C     CMACTM        IFLT      10000
     C                   MOVEL     CMACTM        XXWK4A            4
     C                   MOVEL     '12'          XXWK4A
     C                   MOVEL     XXWK4A        XXACTM
     C                   ELSE
     C                   Z-ADD     CMACTM        XXACTM
     C                   END
     C                   END
     C     CMACTM        IFGE      120000
     C     CMACTM        ANDNE     240000
     C                   MOVE      '1'           *IN27
     C                   ELSE
     C                   MOVE      '1'           *IN26
     C                   END
     C*
     C*  OPEN THE SPOOLFILE AND PRINT THE REPORT.  THEN CLOSE THE
     C*  SPOOLFILE, CREATE THE DARTS REMARK AND RESOLVE THE OUTPUT.
     C*
     C*** Begin Del *** 07/19/17 ***************************************
     C*** Beg Add *** 12/09/15 ***************************************
     C*******            If        isUseSpoolOut
     C*** End Add *** 12/09/15 ***************************************
     C*** End   Del *** 07/19/17 ***************************************
     C                   OPEN      QSYSPRT
     C                   EXCEPT    DET
     C                   CLOSE     QSYSPRT
     C                   EXSR      SRDART
     C*** Begin Del *** 07/19/17 ***************************************
     C*** Beg Add *** 12/09/15 ***************************************
     C****               EndIf
     C*** End Add *** 12/09/15 ***************************************
     C*** Beg Add *** 12/09/15 ***************************************
     C***
     C*** Write to Scriptura Payment History - Report Header
     C***                If        isScriptUsed = 'Y'
     C***                ExSr      GetXmlFileNm
     C***                Exsr      wrtSXFRP
     C***                Endif
     C*** End Add *** 12/09/15 ***************************************
     C*** End   Del *** 07/19/17 ***************************************
     C*
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C     *IN31         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C***  END   ADD  ***  04/07/05 ***********************************
     C*
     C*  RETRIEVE THE NEXT RECORD
     C*
     C     SKIP          TAG
     C     LGKY          READE     PLG01                                  60
     C                   END
     C*
     C*  END OF PROGRAM
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Name
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   ENDIF                                                  ECKY
      *
     C                   ENDSR                                                  SRCONT
      *
      *
      ***** END ADD ******* 01/31/02 **********************************
     C*****************************************************************
     C* SRINIT SUBROUTINE              ** Subroutine **   S R I N I T *
     C* THIS SUBROUTINE INITIALIZES ALL WORK FIELDS                   *
     C*****************************************************************
     C     SRINIT        BEGSR
     C*
     C                   MOVEA     '0000000'     *IN(21)
     C                   MOVE      *BLANKS       XXCNAM           35
     C                   MOVE      *BLANKS       XXCAD1           35
     C                   MOVE      *BLANKS       XXCAD2           35
     C                   MOVE      *BLANKS       XXCCSZ           35
     C                   MOVE      *BLANKS       XXCONT           33
     C                   MOVE      *BLANKS       XXENAM           40
     C                   MOVE      *BLANKS       XXEAD1           40
     C                   MOVE      *BLANKS       XXEAD2           40
     C                   MOVE      *BLANKS       XXECTY           25
     C                   MOVE      *BLANKS       XXEST             2
     C                   MOVE      *BLANKS       XXEZIP            9
     C                   MOVE      *BLANKS       XXLAD1           40
     C                   MOVE      *BLANKS       XXLAD2           40
     C                   MOVE      *BLANKS       XXLCSZ           40
     C*** BEGIN ADD *** 06/25/99 **************************************
     C                   MOVE      *BLANKS       XXTCSZ           40
     C*** END ADD ***** 06/25/99 **************************************
     C*** BEGIN ADD *** 10/01/99 **************************************
     C                   MOVE      *BLANKS       XXPCSZ           40
     C*** END ADD ***** 10/01/99 **************************************
     C                   MOVE      *BLANKS       XXACSZ           40
     C                   MOVE      *BLANKS       XXESTS           30
     C                   MOVE      *BLANKS       XXNADS           25
     C                   MOVE      *BLANKS       XXPADS           25
     C                   MOVE      *BLANKS       XX30A1           30
     C                   MOVE      *BLANKS       XX30A2           30
     C                   MOVE      *BLANKS       XX50A1           50
     C                   MOVE      *BLANKS       XX50A2           50
     C                   MOVE      *BLANKS       XX50A3           50
     C                   MOVE      *BLANKS       XX50A4           50
     C                   MOVE      *BLANKS       XX50A5           50
     C                   MOVE      *BLANKS       XX50A6           50
     C                   MOVE      *BLANKS       XX50A7           50
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                  Z-ADD     0             XXSS#             9 0
     C                   MOVE      *BLANK        XXSS#            11
     C***  End Chg  ***  06/09/20  *******************************
     C                   Z-ADD     0             XXDPND            5 0
     C                   Z-ADD     0             XXPFDT            8 0
     C                   Z-ADD     0             XXPTDT            8 0
     C                   Z-ADD     0             XXDTOB            8 0
     C                   Z-ADD     0             XXDOH             8 0
     C                   Z-ADD     0             XXACDT            8 0
     C                   Z-ADD     0             XXACTM            6 0
     C                   Z-ADD     0             XXDTLW            8 0
     C                   Z-ADD     0             XXDTRP            8 0
     C                   Z-ADD     0             XXDBDT            8 0
     C                   Z-ADD     0             XXRTW             8 0
     C                   Z-ADD     0             XXDOD             8 0
     C                   Z-ADD     0             XXRCVD            8 0
     C                   MOVE      *BLANK        XXSEX1            1
     C                   MOVE      *BLANK        XXSEX2            1
     C                   MOVE      *BLANK        XXSEX3            1
     C                   MOVE      *BLANK        XXMST1            1
     C                   MOVE      *BLANK        XXMST2            1
     C                   MOVE      *BLANK        XXMST3            1
     C                   MOVE      *BLANK        XXMST4            1
     C                   MOVE      *BLANK        XXWGP1            1
     C                   MOVE      *BLANK        XXWGP2            1
     C                   MOVE      *BLANK        XXWGP3            1
     C                   MOVE      *BLANK        XXWGP4            1
     C                   MOVE      *BLANK        XXFSL1            1
     C                   MOVE      *BLANK        XXFSL2            1
     C                   MOVE      *BLANK        XXEPR1            1
     C                   MOVE      *BLANK        XXEPR2            1
     C                   MOVE      *BLANK        XXITR0            1
     C                   MOVE      *BLANK        XXITR1            1
     C                   MOVE      *BLANK        XXITR2            1
     C                   MOVE      *BLANK        XXITR3            1
     C                   MOVE      *BLANK        XXITR4            1
     C                   MOVE      *BLANK        XXITR5            1
     C                   MOVE      *BLANK        XXSFY1            1
     C                   MOVE      *BLANK        XXSFY2            1
     C                   MOVE      *BLANK        XXSFY3            1
     C                   MOVE      *BLANK        XXSFY4            1
     C*** BEGIN ADD *** 06/25/99 **************************************
     C                   MOVE      *BLANK        XXPOI1            1
     C                   MOVE      *BLANK        XXPOI2            1
     C*** END ADD ***** 06/25/99 **************************************
      *** BEGIN ADD *** 08/11/99 **************************************
     C                   MOVE      *BLANK        XXPHYN
     C                   MOVE      *BLANK        XXPHYA
      *** END ADD ***** 08/11/99 **************************************
      *** BEGIN ADD *** 10/01/99 **************************************
     C                   MOVE      *BLANK        XXPDNM
     C                   MOVE      *BLANK        XXPDAD
      *** END ADD ***** 10/01/99 **************************************
     C*
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C     *IN31         IFEQ      *OFF
     C***  END   ADD  ***  04/07/05 ***********************************
     C                   Z-ADD     LGFND         XXFND
     C                   Z-ADD     LGFYR         XXFYR
     C                   Z-ADD     LGCASE        XXCASE
     C                   MOVE      LGJST         X2ST                           ADD 05/18/00
     C***  BEGIN ADD  ***  04/07/05 ***********************************
     C                   ELSE
     C                   MOVE      CMJRST        X2ST
     C                   ENDIF
     C***  END   ADD  ***  04/07/05 ***********************************
     C*
     C*  RETRIEVE THE NEW FUND INFORMATION, IF THE FUND CHANGED.
     C*
     C     XXFND         IFNE      SVFND
     C*
     C     FNKY          CHAIN     PFN01                              60
     C   60              MOVEL     *BLANKS       FNSFIN
     C                   MOVEL     *BLANKS       XXSFIN
     C     FNSFIN        IFEQ      'Y'
     C                   MOVE      'X'           XXSFIN
     C                   END
     C*
     C                   Z-ADD     XXFND         SVFND
     C                   END
     C*
     C*
     C*  RETRIEVE THE EDI CLAIMS CONTROL INFO, IF THE TRANS ID CHANGED.
     C*    ALSO, RETRIEVE THE DARTS REMARK CODE INFO.
     C*
     C     LGTSET        IFNE      SVTSET
     C*
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C*** END CHG ****** 03/24/10 *************************************
     C   60              MOVE      *BLANKS       IXHCPR
     C     CTKY          CHAIN     PCT01                              60
     C   60              MOVE      *BLANKS       CTTYPE
     C   60              MOVE      *BLANKS       CTDESC
     C*
     C                   MOVEL     LGTSET        SVTSET
     C                   END
     C*
     C******BEG ADD******04/19/10**************************************
     C     CLTID         IFEQ      '025'
     C     F3KY          CHAIN     PF301                              60
     C******BEG ADD******07/27/10**************************************
     C                   MOVE      '1'           *IN35
     C                   ELSE
     C                   MOVE      '0'           *IN35
     C******END ADD******07/27/10**************************************
     C                   ENDIF
     C******END ADD******04/19/10**************************************
     C                   ENDSR
     C*****************************************************************
     C* SRADDR SUBROUTINE              ** Subroutine **   S R A D D R *
     C* THIS SUBROUTINE PUTS TOGETHER AN ADDRESS INTO ONE STRING      *
     C* CONTAINING THE CITY, STATE AND ZIP                            *
     C*****************************************************************
     C     SRADDR        BEGSR
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVE      *BLANKS       XXSZ             13
     C*
     C                   MOVEA     XXST          SZ
     C                   MOVEL     XXZIP         XXZP1
     C                   MOVE      XXZIP         XXZP2
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZP2         IFNE      '0000'
     C                   MOVE      '-'           SZ(9)
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST           56
     C                   PARM                    XXFRST           15
     C                   PARM                    XXTERM            1
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRDART SUBROUTINE              ** Subroutine **   S R D A R T *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH REPORT WHICH   *
     C* IS PROCESSED.                                                 *
     C*****************************************************************
     C     SRDART        BEGSR
     C*
     C*  DON'T CREATE THE DARTS REMARK IF THE HARDCOPY DARTS CODE IS
     C*  BLANK OR INVALID.
     C*
     C     IXHCPR        IFEQ      *BLANKS
     C     CTTYPE        OREQ      *BLANKS
     C                   GOTO      SKIP1
     C                   END
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      XXCO#         QCCO#
     C                   MOVE      XXFND         QCFND
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      XXCASE        QCCASE
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********
     C***ESTABLISH RECORD
     C********
     C********             CLEARPCM03
     C********             MOVELQCKEY     CMKEY
     C********             Z-ADDXXDATE    CMCDAT
     C********             MOVELIXHCPR    CMRMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   CMRMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    CMSMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVELCTDESC    CMCOMM
     C********
     C***GET*FOLDER NAME
     C********
     C********             MOVE *BLANKS   QQFND   8
     C********             MOVE *BLANKS   QQSER   8
     C********             MOVE *BLANKS   QQSUB   8
     C********             MOVE *BLANKS   QQFOLD  8
     C********             MOVE *BLANKS   QQFLDX  7
     C********             MOVE *BLANKS   QQFPTH 40
     C********             MOVE *BLANKS   QQTERM  1
     C********
     C********             MOVELXXFND     QQFND            for Fund
     C********             MOVEL'CS'      QQSER            for Area of Service
     C********             MOVELCTTYPE    QQSUB            for Sub-Area
     C********
     C********             MOVEL'C'       QQFOLD           means "Member"
     C********             MOVELCMCASE    QQFLDX           CASE for Folder name
     C********             MOVE QQFLDX    QQFOLD
     C********
     C********             CALL 'SRFBLD'
     C********             PARM           QQFND
     C********             PARM           QQSER
     C********             PARM           QQSUB
     C********             PARM           QQFOLD
     C********             PARM           QQFPTH           Built, returned
     C********             PARM           QQTERM
     C********
     C********             MOVELQQFPTH    CMFOLD
     C********
     C***GET*DOCUMENT ID
     C********
     C********             MOVE *BLANKS   QQRT    2
     C********             MOVE *BLANKS   QQDOC  12
     C********             MOVE *BLANKS   QQTERM  1
     C********
     C********             MOVELCTTYPE    QQRT             Remark Type
     C********             CALL 'SRDBLD'
     C********             PARM           QQFPTH           From SRFBLD routine
     C********             PARM           QQRT             Rmrk Type for Doc ID
     C********             PARM           QQDOC            Built, returned
     C********             PARM           QQTERM
     C********
     C********             MOVELQQDOC     CMDCID
     C********             MOVEL'W0070R'  CMPNAM
     C********             Z-ADDXXDATE    CMTRDT
     C********             Z-ADDDSTIME    CMTRTM
     C********             MOVE 'N'       CMSBYL
     C********             MOVELLGIUSR    CMUSER
     C********             MOVELQ1PGM     CMPGM
     C********             Z-ADDDSTIME    CMTIME
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMDT
     C********             MOVELLGIUSR    CMIUSR
     C********             MOVE 'WC'      CMAPID
     C********             MOVELQ1PGM     CMIPGM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMIDT
     C********
     C***GET*LAST SEQ# USED
     C********
     C********   C4KY      SETGTPCM04
     C********   C4KY      REDPEPCM04                    60
     C********   *IN60     IFEQ '0'
     C********   C4SEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     C********             END
     C********
     C********             WRITEPCM03
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  *BLANKS       Q#COMM          256
     C                   MOVEL(P)  IXHCPR        Q#RMCD            5
     C     Q#RMCD        CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  LGIUSR        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVE      *BLANKS       QQFND             8
     C                   MOVE      *BLANKS       QQSER             8
     C                   MOVE      *BLANKS       QQSUB             8
     C                   MOVE      *BLANKS       QQFOLD            8
     C                   MOVE      *BLANKS       QQFLDX            7
     C                   MOVE      *BLANKS       QQFPTH           40
     C                   MOVE      *BLANKS       QQTERM            1
     C                   MOVEL     XXFND         QQFND                          for Fund
     C                   MOVEL     'CS'          QQSER                          for Area of Service
     C                   MOVEL     CTTYPE        QQSUB                          for Sub-Area
     C                   MOVEL     'C'           QQFOLD                         means "Member"
     C                   MOVEL     CMCASE        QQFLDX                         CASE for Folder name
     C                   MOVE      QQFLDX        QQFOLD
     C                   CALL      'SRFBLD'
     C                   PARM                    QQFND
     C                   PARM                    QQSER
     C                   PARM                    QQSUB
     C                   PARM                    QQFOLD
     C                   PARM                    QQFPTH                         Built, returned
     C                   PARM                    QQTERM
     C                   MOVEL(P)  QQFPTH        Q#FOLD           80
     C                   MOVE      *BLANKS       QQRT              2
     C                   MOVE      *BLANKS       QQDOC            12
     C                   MOVE      *BLANKS       QQTERM            1
     C                   MOVEL     CTTYPE        QQRT                           Remark Type
     C                   CALL      'SRDBLD'
     C                   PARM                    QQFPTH                         From SRFBLD routine
     C                   PARM                    QQRT                           Rmrk Type for Doc ID
     C                   PARM                    QQDOC                          Built, returned
     C                   PARM                    QQTERM
     C                   MOVEL(P)  QQDOC         Q#DCID           12
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  'WC'          Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *BLANKS       Q#PIDX            5
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
     C*** END ADD *** 04/26/00 ****************************************
     C*
     C*  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
     C*
     C     LGKY1         CHAIN     PLG02                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN CHG *** 04/26/00 **************************************
     C********             Z-ADDCMSEQ#    LGPRSQ
     C                   MOVE      Q#SEQ#        LGPRSQ
     C*** END CHG *** 04/26/00 ****************************************
     C                   UPDATE    PLG02
     C                   END
     C*
     C*  CALL W8070C1 TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
     C*
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             MOVE CMDCID    QQDCID
     C                   MOVE      Q#DCID        QQDCID
     C********             MOVE CMFOLD    QQFLDR
     C                   MOVE      Q#FOLD        QQFLDR
     C*** END DEL *** 04/26/00 ****************************************
     C                   CALL      'W8070C1'
     C                   PARM                    Q1JOB
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOBN
     C                   PARM                    QQDCID
     C                   PARM                    QQFLDR
     C*
     C     SKIP1         TAG
     C                   ENDSR
     C*** Begin Del *** 07/19/17 ***************************************
     C*** Beg Add *** 12/09/15 *****************************************
      **/Free
      ** //-------------------------------------------------------------
      ** // GetXMLFileNm - Get the XML File Name
      ** //--------------------------------------------------------------
      **     Begsr GetXMLFileNm;
      **
      **        outCall = 'XMLREQUEST';
      **        OutPrefix = '  ';
      **        OutString = 'A1Froi';
      **        InReturn  = procType;
      **        massFlag = 'N';
      **
      **        StLetter(OutCall:InXML:InReturn:outMode:OutRrn:
      **         OutPrefix:OutString:OutCo#:OutFnd:massFlag:xmlLink);
      **
      **       Chain inXML pxp01;
      **       If %Found(xmlprel1);
      **        inXML = XPxmlT;
      **       EndIf;
      **
      **    Endsr;
      **//-------------------------------------------------------------
      **// WrtSXFRP - Write to Scriptura First Report if Injury File
      **//--------------------------------------------------------------
      **    Begsr WrtSXFRP;
      **
      **       Chain (CMco#:CMfnd:CMfYr:CMcase:'A1') pni01;
      **
      **       Clear pfr0p;
      **
      **       FRxml    = inXML;
      **       FReNam   = EMname;
      **       FRco#    = CMco#;
      **       FRfnd    = CMfnd;
      **       FRfName  = FNname;
      **       FRfYr    = CMfyr;
      **      FRcase   = CMcase;
      **      FReMad1  = EMmAd1;
      **      FReMad2  = EMmAd2;
      **      FReMcty  = EMmCty;
      **      FReMst   = EMmSt;
      **      FReMzip  = %EditC(EMmZip:'X');
      **      FRePad1  = EMpAd1;
      **      FRePad2  = EMpAd2;
      **      FRePcty  = EMpCty;
      **      FRePst   = EMpSt;
      **      FRePzip  = %EditC(EMpZip:'X');
      **      FRjrSt   = CMjrSt;
      **      FRacl#   = CMacl#;
      **      FRlPad1  = ALpAd1;
      **      FRlPad2  = ALpAd2;
      **      FRlPcty  = ALpCty;
      **      FRlPst   = ALpSt;
      **      FRlPzip  = ALpZip;
      **      FRlMad1  = ALmAd1;
      **      FRlMad2  = ALmAd2;
      **      FRlMcty  = ALmCty;
      **      FRlMst   = ALmSt;
      **      FRlMzip  = ALmZip;
      **      FRsic    = CMsic;
      **
      **      chrKey   = CMsic;
      **      FRsicDs  = GetTblDesc('SIC ':chrKey);
      **
      **      FReFei   = EMfein;
      **      FReArea  = EMarea;
      **      FRePhn#  = EMphn#;
      **      FRcoNm   = COname;
      **
      **      Monitor;
      **         FRpfDt = %dec(%date(ELpfDt:*cymd):*usa);
      **      On-Error;
      **         FRpfDt = 0;
      **      EndMon;
      **
      **      Monitor;
      **         FRptDt = %dec(%date(ELptDt:*cymd):*usa);
      **      On-Error;
      **         FRptDt = 0;
      **      EndMon;
      **
      **      FRcoAd1  = COadr1;
      **      FRcoAd2  = COadr2;
      **      FRcoCty  = COcty;
      **      FRcoSt   = COst;
      **      FRcoZip  = COzip;
      **
      **      If FNsFin = 'Y';
      **         FRsFin   = 'X';
      **      EndIf;
      **
      **      If *in35;
      **         FRcoFe = F3Ifei;
      **      Else;
      **         FRcoFe = COfein;
      **      EndIf;
      **
      **      If RPcap# <> ' ';
      **         FRPol# = RPcap#;
      **      Else;
      **         FRPol# = %EditC(CMfnd:'X') + %EditC(CMemp#:'X') +
      **                  %EditC(CMfYr:'X');
      **      EndIf;
      **
      **      FRagNm   = AGname;
      **      FRcFsn   = CDfsn;
      **      FRcLsn   = CDlsn;
      **      FRcMid   = CDmid;
      **
      **      Monitor;
      **         FRdtob = %dec(%date(CDdtob:*cymd):*usa);
      **      On-Error;
      **         FRdtob = 0;
      **      EndMon;
      **
      **      FRss#    = CDss#;
      **      FRstoh   = NI10A2;
      **      FRsex    = CDsex;
      **      FRcAd1   = CDadr1;
      **      FRcAd2   = CDadr2;
      **      FRcCty   = CDcty;
      **      FRcSt    = CDst;
      **      FRcZip   = CDzip;
      **      FRcArea  = CDarea;
      **      FRcPhn#  = CDphn#;
      **      FRmSt    = CDmSts;
      **      FRocDs   = CMocDs;
      **
      **      chrKey   = CMeSts;
      **      FRestD   = %Subst(GetTblDesc('EMST':chrKey):1:30);
      **
      **      FRdPnd   = CM#dep;
      **      FRclas   = CMclas;
      **      FRwgp    = NI20A9;
      **      FRpoi    = CMpoi;
      **      FRaww    = CMaww;
      **      FRdays   = CMdays;
      **      FRfSal   = CMfSal;
      **      FRbgTm   = NItim2;
      **
      **      Monitor;
      **         FRacDt = %dec(%date(CMacdt:*cymd):*usa);
      **      On-Error;
      **         FRacDt = 0;
      **      EndMon;
      **
      **      FRacTm   = CMacTm;
      **
      **      Monitor;
      **         FRdtLw = %dec(%date(CMdtLw:*cymd):*usa);
      **      On-Error;
      **         FRdtLw = 0;
      **      EndMon;
      **
      **      Monitor;
      **         FRdtRp = %dec(%date(CMdtRp:*cymd):*usa);
      **      On-Error;
      **         FRdtRp = 0;
      **      EndMon;
      **
      **      Monitor;
      **         FRdbDt = %dec(%date(CMdbDt:*cymd):*usa);
      **      On-Error;
      **         FRdbDt = 0;
      **      EndMon;
      **
      **      FRcont   = EMcont;
      **      FRnads   = xxNaDs;
      **      FRpads   = xxPaDs;
      **      FRePrm   = CMeAdr;
      **      FRnatr   = CMnatr;
      **      FRnatrDs = xxNaDs;
      **      FRpart   = CMpart;
      **      FRpartDs = xxPaDs;
      **      FRaccL1  = NI40A7;
      **      FRaccL2  = NI40A0;
      **      FRaCty   = NI30A1;
      **      FRaSt    = NI10A1;
      **      FRaZip   = NI9N1;
      **      FReUse1  = NI60A1;
      **      FReUse2  = NI60A2;
      **      FRempA1  = NI60A3;
      **      FRempA2  = NI60A4;
      **      FRwPrc1  = NI60A5;
      **      FRwPrc2  = NI60A6;
      **      FRhowi1  = NI80A1;
      **      FRhowi2  = NI80A2;
      **      FRcaus   = CMcaus;
      **      FRcausDs = GetCauseDs(CMcaus);
      **      FRsafeP  = NI1A1;
      **      FRsafeU  = NI1A2;
      **
      **      Monitor;
      **         FRrtw  = %dec(%date(CMrtw:*cymd):*usa);
      **      On-Error;
      **         FRrtw  = 0;
      **      EndMon;
      **
      **      Monitor;
      **         FRdod  = %dec(%date(CMdod:*cymd):*usa);
      **      On-Error;
      **         FRdod  = 0;
      **      EndMon;
      **
      **      FRinit   = CMiTrt;
      **
      **      chrKey   = CMiTrt;
      **      FRinitDs = GetTblDesc('INIT':chrKey);
      **
      **      FRphyN   = TPphyN;
      **      FRphyA   = TPphyA;
      **      FRtCty   = TPphyC;
      **      FRtSt    = TPphyS;
      **      FRtZip   = TPphyZ;
      **      FRhosN   = CMhosN;
      **      FRhosA   = CMhosA;
      **      FRhosC   = CMhosC;
      **      FRhosS   = CMhosS;
      **      FRhosZ   = CMhosZ;
      **      FRphyE   = NI40A4;
      **      FRhosE   = NI40A8;
      **      FRprPhA  = NI80A3;
      **
      **      Monitor;
      **         FRdTan = %dec(%date(NIdt5:*cymd):*usa);
      **      On-Error;
      **         FRdod  = 0;
      **      EndMon;
      **
      **      FRdte    = %dec(%date():*usa);
      **      FRprNm   = NIprNm;
      **
      **      If NIprPh <> ' ' And NIprPh <> 'nullnull';
      **         FRprPh   = %Dec(%Trim(NIprPh):10:0);
      **      EndIf;
      **
      **      FRiDt =  %dec(%date():*cymd);
      **
      **      Write pfr0p;
      **
      **   Endsr;
      **
      */End-free
     C*** End Add *** 12/09/15 *****************************************
     C*** End   Del *** 07/19/17 ***************************************
     C* ************************************************************************
     OQSYSPRT   E            DET              05
     O                       XXENAM              47
     O                       XXCO#               53
     O                                           54 '-'
     O                       XXFND               57
     O                                           58 '-'
     O                       XXFYR               61
     O                                           62 '-'
     O                       XXCASE              69
     O          E            DET              07
     O                       XXEAD1              47
     O                       CMJRST              52
     O                       CMACL#             100
     O*** BEGIN CHANGE *** 06/25/99 ***********************************
     O******* E   08           DET
     O          E            DET              09
     O                       XXEAD2              47
     O******* E   09           DET
     O          E            DET              10
     O*** END CHANGE ***** 06/25/99 ***********************************
     O                       XXECTY              32
     O          E            DET              12
     O                       XXEST               09
     O                       XXEZIP              40
     O                       CMCTG1             103
     O          E            DET              13
     O                       XXLAD1              90
     O          E            DET              14
     O              N21      XXLAD2              90
     O               21      XXLCSZ              90
     O          E            DET              15
     O              N21      XXLCSZ              90
     O          E            DET              16
     O                       CMSIC               14
     O                       EMFEIN              36 '   -  -    '
     O                       XEAREA              98
     O                       EMPHN#             108 '   -    '
     O          E            DET              19
     C*** BEGIN CHG *** 04/19/10 *************************************
     O********                 XXCONM    47
     C*** BEGIN CHG *** 07/27/10 *************************************
     O*******                  F3INAM    47
     C*** END CHANGE ** 04/19/10 *************************************
     O              N35      XXCONM              47
     O               35      F3INAM              47
     C*** END CHANGE*** 07/27/10 *************************************
     O                       XXPFDT              61 '0 /  /    '
     O                       XXCONM             109
     O          E            DET              20
     O                       COADR1              47
     O                       COADR1             109
     O          E            DET              21
     O              N22      COADR2              47
     O               22      XCOCSZ              47
     O                       XXPTDT              61 '0 /  /    '
     O              N22      COADR2             109
     O               22      XCOCSZ             109
     O          E            DET              22
     O              N22      XCOCSZ              47
     O              N22      XCOCSZ             109
     O          E            DET              25
     O                       XXSFIN              51
     O          E            DET              27
     C*** BEGIN CHG *** 04/19/10 *************************************
     O********                 COFEIN    18 '   -  -    '
     C*** BEGIN CHG *** 07/27/10 *************************************
     O*********                F3IFEI    18 '   -  -    '
     O              N35      COFEIN              18 '   -  -    '
     O               35      F3IFEI              18 '   -  -    '
     C*** END CHANGE ** 04/19/10 *************************************
     C*** END CHANGE*** 07/27/10 *************************************
     O                       XXPOL#              49
     O                       COFEIN             100 '   -  -    '
     O          E            DET              30
     O                       AGNAME              47
     O                       AHAGT#        4     55
     O          E            DET              33
     O                       XXCNAM              42
     O                       XXDTOB              54 '0 /  /    '
     O*** BEGIN CHANGE *** 06/25/99 **********************************
     O************************ XXSS#     71 '0  -  -    '
     C***  Begin Chg  ***  06/09/20  *******************************
     O*                      XXSS#               71 '0   -  -    '
     O                       XXSS#               71
     C***  End Chg  ***  06/09/20  *******************************
     O*** END CHANGE ***** 06/25/99 ***********************************
     O                       XXDOH               90 '0 /  /    '
      *** BEG ADD ***** 04/03/03 **************************************
     O                       XXSTOH             104
      *** END ADD ***** 04/03/03 **************************************
     O          E            DET              35
     O                       XXSEX1              43
     O          E            DET              36
     O                       XXCAD1              42
     O                       XXMST1              60
     O                       XXOCDS             106
     O          E            DET              37
     O              N23      XXCAD2              42
     O               23      XXCCSZ              42
     O                       XXSEX2              43
     O          E            DET              38
     O              N23      XXCCSZ              42
     O                       XXSEX3              43
     O                       XXMST2              60
     O                       XXESTS             106
     O          E            DET              39
     O                       XXMST3              60
     O          E            DET              41
     O                       XCAREA              12
     O                       CDPHN#              22 '   -    '
     O                       XXDPND              48
     O                       XXMST4              60
     O                       CMCLAS              83
     O          E            DET              43
     O                       XXWGP1              30
     O                       XXWGP2              42
     O*** BEGIN CHANGE *** 06/25/99 ***********************************
     O************************ XXFSL1    98
     O************************ XXFSL2   106
     O                       XXPOI1              98
     O                       XXPOI2             106
     O*** END CHANGE ***** 06/25/99 ***********************************
     O          E            DET              44
     O                       CMAWW         2     22
     O                       XXWGP3              30
     O                       XXWGP4              42
     O                       CMDAYS        4     63
     O                       XXFSL1              98
     O                       XXFSL2             106
     O          E            DET              45
     O              N28                          19 'X'
     O               26                          56 'X'
     O          E            DET              47
     O                       X1TIM2              11 '  :  '
     O               28                          19 'X'
     O                       XXACDT              35 '0 /  /    '
     O                       XXTIME              50 '  :  '
     O               27                          56 'X'
     O                       XXDTLW              72 '0 /  /    '
     O                       XXDTRP              87 '0 /  /    '
     O                       XXDBDT             105 '0 /  /    '
     O          E            DET              49
     O                       XXCONT              40
     O                       XEAREA              46
     O                       EMPHN#              56 '   -    '
     O                       XXNADS              84
     O                       XXPADS             112
     O          E            DET              51
     O                       XXEPR1              20
     O                       XXEPR2              29
     O                       CMNATR              60
     O                       CMPART              95
     O*
     O          E            DET              54
     O                       NI40A7              47
     O          E            DET              55
     O                       NI40A0              47
     O                       XX50A1             109
     O*
     O          E            DET              56
     O                       XXACSZ              47
     O                       XX50A2             109
     O          E            DET              59
     O                       XX50A3              57
     O                       XX50A5             109
     O          E            DET              60
     O                       XX50A4              57
     O                       XX50A6             109
     O          E            DET              62
     O                       XX30A1              86
     O          E            DET              63
     O                       NI80A2              87
     O                       CMCAUS             100
     O          E            DET              65
     O                       XXSFY1              98
     O                       XXSFY2             105
     O          E            DET              66
     O                       XXRTW               20 '0 /  /    '
     O                       XXDOD               45 '0 /  /    '
     O                       XXSFY3              98
     O                       XXSFY4             105
     O          E            DET              68
     O                       XXITR0              93
     O          E            DET              69
     O*** BEGIN CHANGE *** 06/25/99 ***********************************
     O************************ NI40A1    47
     O*** BEGIN CHANGE *** 08/11/99 ***********************************
     O************************ TPPHYN    47
     O                       XXPHYN              47
     O*** END CHANGE ***** 08/11/99 ***********************************
     O************************ NI40A5    89
     O                       XXPDNM              89
     O*** END CHANGE ***** 06/25/99 ***********************************
     O          E            DET              70
     O*** BEGIN CHANGE *** 06/25/99 ***********************************
     O************************ NI40A2    47
     O*** BEGIN CHANGE *** 08/11/99 ***********************************
     O************************ TPPHYA    47
     O                       XXPHYA              47
     O*** END CHANGE ***** 08/11/99 ***********************************
     O************************ NI40A6    89
     O                       XXPDAD              89
     O*** END CHANGE ***** 06/25/99 ***********************************
     O                       XXITR1              93
     O          E            DET              71
     O*** BEGIN CHANGE *** 06/25/99 ***********************************
     O************************ NI40A3    47
     O                       XXTCSZ              47
     O************************ NI40A7    89
     O                       XXPCSZ              89
     O*** END CHANGE ***** 06/25/99 ***********************************
     O                       XXITR2              93
     O          E            DET              72
     O                       NI40A4              47
     O                       NI40A8              89
     O                       XXITR3              93
     O          E            DET              73
     O                       XXITR4              93
     O          E            DET              74
     O                       XXITR5              93
     O          E            DET              77
     O                       NI80A3              87
     O          E            DET              79
     O                       XXRCVD              20 '0 /  /    '
     O                       XXDTE               40 '0 /  /    '
     O                       XXPRNM              77
     O                       XXPRPH             104 '   -   -    '
     O*
