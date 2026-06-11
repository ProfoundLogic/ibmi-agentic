     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR ')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0583R02 - Request Payment Update Program
     H*****************************************************************
     H*  DESCRIPTION:
     H*    This program is used to View Request for Payment, update   *
     H*    by "Pending" or "Releasing" Request for Payment, reprint or*
     H*    display EOB, delete Request for Payment, change Group Type,*
     H*    and Time and Materials.                                    *
     H*    For Closed Claim, Reserve Cushion logic applies.           *
     H*    Payment Edits using 4 API's apply.                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0583R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*    SRCENTER - Center Screen Headings                          *
     H*    SRCVTDAT - Date Conversion/Edit                            *
     H*    SRPRCC - Allocate/De-allocate Reserves                     *
     H*    WA006R - Claim Heading                                     *
     H*    WA010R - Re-insurance logic                                *
     H*    WA011R - Payment Edit I                                    *
     H*    WA012R - Payment Edit II                                   *
     H*    WA013R - Payment Edit III                                  *
     H*    WA014R - Payment Edit IV                                   *
     H*    WA018R - Complete DARTS Diary Remark                       *
     H*    W2142R - Payment Status Search                             *
     H*    W2146R - Payment Value 1 Search                            *
     H*    W2151R - Payment Value 2 Search                            *
     H*    W2171R - Remark Code Search                                *
     H*    W2221R - Late Payment Code Search                          *
     H*                                                               *
     H*  CREATED: 05/29/97  C:30  Project:  WC660                     *
     H*****************************************************************
     H* Revisions:                                                    *
     h*                                                               *
     h* 06/09/20 C:318 Project: SY420
     h*                Mask/Unmask SSN
     h*
     h* 04/10/19 C:062 Project: 152WAB                                *
     h*                Apply CSI mods for S1 Medical.                 *
     H*                                                               *
     H* 11/01/16 C:136 Support: 43408                                 *
     H*                Rework ChgEft (Opt 24 - Chg Eft to non-EFT)    *
     H*                to use EFT flag (RPEFTC). Changes not          *
     H*                commented in CHGEFT subroutine.                *
     H*                                                               *
     H* 05/08/13 C:279 Redmine: 2756                                  *
     H*                Compiled for display file changes.             *
     H*                                                               *
     H* 01/05/11 C:257 PROJECT: 114WXM                                *
     H*                Process Invoice # Field on Claim Payment       *
     H*                request screen                                 *
     H*                                                               *
     H* 04/26/11 C:136 SUPPORT: WA025 76707                           *
     H*                CORRECT EFT EDIT FOR ADDITIONAL ADDRESSES      *
     H*                                                               *
     H* 12/06/10 C:136 PROJECT: WC1806 PRISS 78                       *
     H*                CHECK FOR EFT STATUS IF PV1 IS FLAGGED FOR EFT *
     H*                                                               *
     H* 09/04/09 C:234 SUPPORT: WC1453 PRISS#12                       *
     H*                Introduce Bank Code edit for opt 24.           *
     H*                                                               *
     H* 10/09/08 C:62  Support: 65325                                 *
     H*                If payment status code changes hit edit        *
     H*                                                               *
     H* 04/02/08 C:15  Support: WC091/62273                           *
     H*                Added code to prevent negative reserves when a *
     H*                'D' status payment is released.                *
     H*                                                               *
     H* 04/01/08 C:191  Project: 139MIV                               *
     H*                 Handle new edit for negative total paid on    *
     H*                 the reserve. Change in DSPF only              *
     H*                                                               *
     H*  08/09/07 C:191 PROJECT: WC1453                               *
     H*                 Update DATA DS with new fields                *
     H*                 Added new mode 'E' to handle Change EFT Status*
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 02/23/07 C:185 Project: 114WMF                                *
     H*                Back to Base:114WBY dated 06/11/01 & 05/29/01, *
     H*                             114WER dated 07/02/03 & 06/12/03. *
     H*                                                               *
     H* 01/25/07 C:183 PROJECT: 114WLY  *** REMOVED ***
     H*                Added a PARM ##RTCD to accept the exact dupl-  *
     H*                cate indicator. If this returns as '1' from    *
     H*                WA019R then call SRCCSC to check whether the   *
     H*                user has got authority to release the payment. *
     H*                                                               *
     H* 01/11/06 C:15  Support: WC129/50722                           *
     H*                Added code to clear the out of country address *
     H*                screen fields to prevent invalid data from     *
     H*                being loaded into other WARPP records.         *
     H*                                                               *
     H* 08/30/04 C:53  Support: 43225                                 *
     H*                Correct record lock issue                      *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 10/10/02 C:53  Project: WC577                                 *
     H*                Added check for medical payment type           *
     H*                                                               *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 10/10/02 C:53  Project: WC577                                 *
     H*                Added check for medical payment type           *
     H*                                                               *
     H* 06/27/02 C:15  Support: WC111/32069                           *
     H*                Fixed code added on 4/12/02 to prevent the     *
     H*                mode from being changed when *IN09 is *ON.     *
     H*                                                               *
     H* 04/12/02 C:15  Support: WC111/32069                           *
     H*                Added code to prevent the request from being   *
     H*                changed if the check was created but the batch *
     H*                did not complete.                              *
     H*                                                               *
     H* 02/13/02 C:15  Support: WC114/31265                           *
     H*                Added code to prevent RPSTS from being changed *
     H*                erroneously.                                   *
     H*                                                               *
     H* 12/12/01 C: 53 PROJECT: WC1060
     H*                ADD EXCLUDE MEDICAL PROCESSING.
     H*                                                               *
     H* 04/25/01 C:15  Project: WC1020                                *
     H*                Added code to retrieve the bank code for mult  *
     H*                bank account processing.                       *
     H*                                                               *
     H* 04/20/01 C:15  Support: WC113/27189                           *
     H*                Chgd pgm to use logical WARPLN instead of      *
     H*                WARPLA so that the FYR/CASE are part of the    *
     H*                key.                                           *
     H*                                                               *
     H* 01/30/01 C:53  Project: MC090 # 26124
     H*                Reinstate code for *IN52
     H*                                                                *
     H* 11/28/00 C:53  Project: MC071 # 24471
     H*                Protect certain fields when EOMB# <> 0, IN DSPF
     H*                                                               *
     H* 07/25/00 C:15  Support: WC048/23656                           *
     H*                Added recon# to screen and to parm list for    *
     H*                WA014R to prevent invalid edit.                *
     H*                                                               *
     H* 06/23/00 C:15  Project: WC577                                 *
     H*                Added code to verify that a payment is pended  *
     H*                before trying to release it.                   *
     H*                                                               *
     H* 05/30/00 C:53  Project: WC991                                 *
     H*                Add parms for WA013R to facilitate edit # 12   *
     H*                                                               *
     H* 05/15/00 C:53  Support: WC044/22882                           *
     H*                Omit edit for valid EOMB # on this screen due to
     H*                  - eomb # cannot be changed on this screen    *
     H*                  - new edit for rcn# in FA015R triggers a     *
     H*                    false invalid eomb edit.                   *
     H*                                                               *
     H* 04/12/00 C:15  Project: WC577                                 *
     H*                Added code to prevent negative reserves.       *
     H*                                                               *
     H* 02/25/00 C:15  Project: WC577                                 *
     H*                When a pmt is released, load the release maint *
     H*                fields in WARPP.                               *
     H*                                                               *
     H* 01/12/00 C:15  Support: WC048/21324                           *
     H*                Chgd condition which determines whether RPSTS  *
     H*                is updated.                                    *
     H*                                                               *
     H* 08/02/99 C:58 Project: WC925                                  *
     H*               Use return code "D" for API WA018R, specifying
     H*               remark deletion (setting status to 'I')
     H*                                                               *
     H*  07/01/99 C:118                                               *
     H*                Modified RPG so Payment Status file would be   *
     H*                read from the beginning to locate deletes.     *
     H*                                                               *
     H*  03/17/99 C:33 SEQ#: WC025/17140                              *
     H*                CHANGE SSN FROM NUMERIC TO ALPHA.              *
     H*                                                               *
     H*  03/02/99 C:33 SEQ#: WC095/13271                              *
     H*                PASS THE RESERVE TYPE TO WA019R.               *
     H*                                                               *
     H* 09/08/98 C:15 Support: WC082/12790                            *
     H*               Changed pgm to update claim reserves correctly  *
     H*               if the pmt amt changes.                         *
     H*                                                               *
     H* 05/19/98 C:15 Support: WC095/11264                            *
     H*               Fixed pgm to prevent reserve cushion processing *
     H*               on open claims.                                 *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *  Display File
     FW0583F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *________________________________________________________________
      *  Request for Payment file (for maintaining record)
     FWARPP     UF A E             DISK
     F                                     RENAME(PRP01:DBRCDF)
      *________________________________________________________________
      *  Request for Payment file (for Deletion)
     F*** Begin chg *** 04/20/01 **************************************
     F******WARPLA  UF  E           K        DISK
     FWARPLN    UF   E           K DISK
     F*** End chg ***** 04/20/01 **************************************
     F                                     RENAME(PRP01:PRP02)
      *________________________________________________________________
      *  Network Review ID file (for Deletion)
     FWMNRL     IF   E           K DISK
      *________________________________________________________________
      *  User Parameter file
     FWMUPL     IF   E           K DISK
      *________________________________________________________________
      *  Table Files
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
      *________________________________________________________________
      *  Claim Master
     F*** BEGIN CHG *** 05/19/98 **************************************
     F*****WMCML1  UF  E           K        DISK
     FWMCML1    IF   E           K DISK
     F*** END CHG ***** 05/19/98 **************************************
      *________________________________________________________________
      *  Additional Address
     FWDACL     IF   E           K DISK
      *________________________________________________________________
      *  Fund Detail
     FWDF1L     IF   E           K DISK
      *________________________________________________________________
      *  Request for Payment file -       KEY = CO#/FND/FYR/CASE/SEQ#
     FWARPL     IF   E           K DISK
      *________________________________________________________________
      *  Claim/DARTS Control file -       KEY = CO#/FND
     FWMCXL     IF   E           K DISK
      *________________________________________________________________
      *  Payment Status file      -       KEY = CO#/Key
     FWTPSL     IF   E           K DISK
      *________________________________________________________________
     F*** BEGIN DLT *** 05/19/98 **************************************
     F** Reserve History file
     F*****WMRHL   O   E           K        DISK
     F*** END DLT ***** 05/19/98 **************************************
      *** BEG ADD *** 12/12/01 **********************************
     FWDELL     IF   E           K DISK
      *** END ADD *** 12/12/01 **********************************
     F*** BEG ADD *** 10/20/03 **********************************
     FWTGTL     IF   E           K DISK
     F*** END ADD *** 10/20/03 **********************************
     F*** Beg Add *** 08/09/07 ********************************
     FWMEFL2    UF   E           K DISK
     F                                     RENAME(PEF01:PEF02)
     FWMEFL3    UF   E           K DISK
     F                                     RENAME(PEF01:PEF03)
      /COPY SCOPYMT01D
     D*** End Add *** 08/09/07 ********************************
      *** beg add ***** 11/01/16 ******************************
      /copy sprGenPop
      *** end add ***** 11/01/16 ******************************
      *________________________________________________________________
     D I6              S              6    DIM(63)                              Indicator Values
      *** beg add ***** 11/01/16 ******************************
     d svEftc          s              1
      *** end add ***** 11/01/16 ******************************
      *
     D*** BEG ADD *** 10/20/03 *******************************************  ****
     D NF              C                   CONST('NOT FOUND')
     D*** END ADD *** 10/20/03 *******************************************  ****
     D                 DS
     D  RT                     1     20
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
     D  RR                     1     90  2
     D                                     DIM(10)                              Remaining Reserve
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
     D***  BEGIN CHG  ***  03/17/99  *********************************
     D**********                              1   90SSN
     D  SSN                    1      9
     D***  END   CHG  ***  03/17/99  *********************************
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
      *  DDS Field Name
     D                 DS
     D  FIELD                  1     10
     D  FLDNAM                 1      5
     D  FLDNUM                 6      6
      *  DARTS Key Field structure
     D                 DS
     D  CMKEY                  1     40
     D  CCO#                   1      3
     D  CFND                   4      6
     D  CFYR                   7      9
     D  CCASE                 10     16
     D*
     D*** Begin add *** 04/25/01 **************************************
     D                 DS
     D*** Beg Chg *** 08/09/07 **************************************
     D***************                         1  50 DATA
     D  DATA                   1     75
     D*** End Chg *** 08/09/07 ****************************************
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D*** End add ***** 04/25/01 **************************************
     D*** Begin add *** 02/23/07 **************************************
     D  QXSEQ#                30     34  0
     D*** End add ***** 02/23/07 **************************************
     D*** Begin add *** 02/23/07 **************************************
     D  QXEOB#                35     43
     D*** End add ***** 02/23/07 **************************************
     D*** Beg Add ***** 08/09/07 **************************************
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
     D*** End Add ***** 08/09/07 **************************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D***PY SCOPYMT01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Request for Payment')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
     D CRH             C                   CONST('Request for Payment -
     D                                     Deleted')
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
      *  CLAIMANT DEMOGRAPHICS FILE
      *
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
     C     NRKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
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
     C                   KFLD                    KKTRDT
      *
      *   REQUEST PAYMENT FILE KEY LIST (WARPL)
      *
     C     RPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ
      *
     C     RPKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
     C     RPKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*** Begin add *** 04/20/01 **************************************
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** End add ***** 04/20/01 **************************************
     C                   KFLD                    SVEOB#
      *
      *   TABLE FILE KEY LIST
      *
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXKEY
     C*
     C***** BEGIN ADD **** 07/01/99 ***********************************
     C*
     C     TBKY1         KLIST
     C                   KFLD                    KKCO#
     C*
     C***** END   ADD **** 07/01/99 ***********************************
      *
      *  PAYMENT DETAIL FILE - PARTIAL
      *
     C     PAKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
     C     PAKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKSEQ#
      *
      *  REQUEST PAYMENT FILE - PARTIAL
      *
     C     RPKEY2        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
      *   FUND DETAIL FILE KEY LIST
      *
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
      *
      *   CLAIM MASTER FILE KEY LIST
      *
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
      *
      *   CLAIMANT DEMOGRAPHICS FILE KEY LIST
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *   Claim/DARTS Control key
      *
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
     C*** BEG ADD ***** 12/12/01 **************************************
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*** END ADD ***** 12/12/01 **************************************
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
     C*** Beg Add ****** 08/09/07 *******************************
     C     EFKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN            9 0
     C                   KFLD                    KKMOD             3 0
     C*****
     C     EFKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSQ#            5 0
     C*** End Add ****** 08/09/07 *******************************
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    RPCO#         KKCO#
     C     *LIKE         DEFINE    RPFND         KKFND
     C     *LIKE         DEFINE    RPFYR         KKFYR
     C     *LIKE         DEFINE    RPCASE        KKCASE
     C     *LIKE         DEFINE    RPSEQ#        KKSEQ#
     C     *LIKE         DEFINE    RPSEQ#        KKSEQ
     C     *LIKE         DEFINE    RPCHK#        KKCHK#
     C     *LIKE         DEFINE    RPTRDT        KKTRDT
     C     *LIKE         DEFINE    SCAMT         SVAMT
     C     *LIKE         DEFINE    SCFRDT        SVFRDT
     C     *LIKE         DEFINE    SCFRDT        XXFROM
     C     *LIKE         DEFINE    SCFRDT        XXTO
     C     *LIKE         DEFINE    SCTODT        SVTODT
     C     *LIKE         DEFINE    RPFEIN        SVFEIN
     C     *LIKE         DEFINE    RPMOD         SVMOD
     C     *LIKE         DEFINE    RPEOB#        SVEOB#
     C     *LIKE         DEFINE    RPRECD        SVRECD
     C***  BEGIN ADD  ***  03/02/99  *********************************
     C     *LIKE         DEFINE    RPRETP        SVRETP
     C***  END   ADD  ***  03/02/99  *********************************
     C     *LIKE         DEFINE    RPSTAT        SVSTAT
     C     *LIKE         DEFINE    RPAMT         XXAMT
     C     *LIKE         DEFINE    RPAMT         XXREM
     C     *LIKE         DEFINE    RPAMT         XXDIF
     C     *LIKE         DEFINE    RPSORT        SVSORT
     C     *LIKE         DEFINE    RPSTS         SVSTS
     C     *LIKE         DEFINE    RPSTS         DDSTS                          DELETE Paymnt Status
     C     *LIKE         DEFINE    PSSRT         DDSORT                         DELETE Paymnt SORT
     C     *LIKE         DEFINE    RPSTAT        OLSTAT                         "OLD" Status
     C     *LIKE         DEFINE    RPSTAT        NWSTAT                         "NEW" Status
     C     *LIKE         DEFINE    RPSTS         NWSTS                          "NEW" Paymnt Status
     C     *LIKE         DEFINE    PSSRT         NWSRT                          "NEW" Paymnt SORT
     C*** BEGIN ADD *** 09/08/98 **************************************
     C     *LIKE         DEFINE    RPAMT         SVAMT1
     C*** END ADD ***** 09/08/98 **************************************
     C*** BEGIN ADD *** 01/12/00 **************************************
     C     *LIKE         DEFINE    RPSTS         OLSTS                          "OLD" Paymnt Status
     C*** END ADD ***** 01/12/00 **************************************
     C*** BEG ADD *** 10/20/03 **********************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END ADD *** 10/20/03 **********************************
     C***  BEGIN ADD  ***  02/23/07  **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  END   ADD  ***  02/23/07  **********************************
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
     C*** BEGIN ADD *** 10/09/08 **************************************
     C                   MOVE      *OFF          RELEAS
     C*** END   ADD *** 10/09/08 **************************************
      *
     C/COPY SCOPYMT01C
      *
      *================================================================
      * CHGGRP - Subroutine to change the group type flag.
      *================================================================
     C     CHGGRP        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
      *
     C*** BEGIN ADD *** 04/12/02 **************************************
     C*  If RPSTAT = 'Y', do not process this change.
     C*
     C     RPSTAT        ANDNE     'Y'
     C*** END ADD ***** 04/12/02 **************************************
      *
     C     RPPGTP        IFEQ      'G'
     C                   MOVE      *BLANK        RPPGTP
     C                   ELSE
     C                   MOVE      'G'           RPPGTP
     C                   ENDIF
      *
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPTIME
     C                   UPDATE    DBRCDF
      *
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR
      *
      *================================================================
      * CHGSTS - Subroutine to "Pend" or "Release" Request for Payment
      *================================================================
     C     CHGSTS        BEGSR
      *
      *  Access Request for Payment record
      *
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
      *
     C*** BEGIN ADD *** 04/12/02 **************************************
     C*  If RPSTAT = 'Y', do not process this change.
     C*
     C     RPSTAT        ANDNE     'Y'
     C*** END ADD ***** 04/12/02 **************************************
      *
     C                   Z-ADD     RPCO#         KKCO#
     C                   Z-ADD     RPFND         KKFND
     C                   Z-ADD     RPFYR         KKFYR
     C                   Z-ADD     RPCASE        KKCASE
      *
      *  Access System Control File to obtain default "Pend" & "Release" codes.
      *
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     @@MODE        IFEQ      'N'                                          Pend status
      *
     C*** BEGIN ADD *** 06/23/00 **************************************
     C     RPSTAT        IFNE      ' '
     C*** BEG ADD *** 08/30/04 ****************************************
     C                   UPDATE    DBRCDF
     C*** END ADD *** 08/30/04 ****************************************
     C                   GOTO      ENDCHG
     C                   ENDIF
     C*** END ADD ***** 06/23/00 **************************************
      *
     C                   MOVEL(P)  CXPMTS        XXKEY
     C                   MOVEL     CXPMTS        RPSTS
     C                   MOVE      'P'           RPSTAT
     C                   MOVE      *BLANK        RPSENT
     C                   Z-ADD     *ZERO         RPDSEQ
      *
     C                   ELSE                                                   Release status
      *
     C*** BEGIN ADD *** 06/23/00 **************************************
     C     RPSTAT        IFNE      'P'
     C*** BEG ADD *** 08/30/04 ****************************************
     C                   UPDATE    DBRCDF
     C*** END ADD *** 08/30/04 ****************************************
     C                   GOTO      ENDCHG
     C                   ENDIF
     C*** END ADD ***** 06/23/00 **************************************
      *
     C                   MOVEL(P)  CXPSC2        XXKEY
     C                   MOVEL     CXPSC2        RPSTS
     C                   MOVE      *BLANK        RPSTAT
     C*** BEGIN ADD *** 02/25/00 **************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPRDT
     C                   MOVEL     DSTIME        RPRTM
     C                   MOVEL     Q1USER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C*** END ADD ***** 02/25/00 **************************************
      *  Execute DARTS Complete Diary logic
     C                   EXSR      DARTS1
      *
     C                   ENDIF
      *
      *  Access Payment Status File to obtain SORT Sequence value
      *
     C     TBKY          CHAIN     PPS01                              60
     C  N60PSSTAT        COMP      'I'                                    60
     C  N60              MOVE      PSSRT         RPSORT                         Payment Status SORT
      *
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPTIME
      *
     C                   UPDATE    DBRCDF
      *
      *  Get Claim Master record to determine if CLOSED
      *
     C*** BEGIN ADD *** 04/12/00 **************************************
     C     CMKY          CHAIN     PCM01                              62
     C*** END ADD ***** 04/12/00 **************************************
     C     *IN62         IFEQ      '0'
     C     CMSTAT        ANDEQ     'C'
      *
      *  For "Pend", Subtract Amount from Claim Remaining Reserves & History
      *  For "Release", Allocate Amount to Claim Remaining Reserves & History
      *
     C     @@MODE        IFEQ      'N'
     C                   MOVE      'D'           QQTYPE                         "Decrease" Res
     C                   ELSE
     C                   MOVE      'R'           QQTYPE                         "Allocate" Res
     C                   ENDIF                                                  @@MODE 'N'
      *
     C                   CALL      'SRPRCC'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
     C*
     C                   ENDIF                                                  CMSTAT 'C'
     C*
     C*** BEGIN ADD *** 05/19/98 **************************************
     C*
     C*  If releasing a payment and
     C*    an EOMB is attached and the request being released is not
     C*    the bill review or medical review payment, then release all
     C*    requests with the corresponding EOB#.
     C*
     C     @@MODE        IFEQ      'R'
     C     RPEOB#        ANDGT     *ZERO
     C                   Z-ADD     RPEOB#        SVEOB#
     C*
     C     NRKEY         CHAIN     PNR01                              60
     C     *IN60         IFEQ      *ON
     C     NRGRTY        OREQ      'O'
     C*
     C*  Read all requested payments with the corresponding EOB#.
     C*
     C     RPKY2         CHAIN     PRP02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RPSTAT        IFEQ      'P'
     C                   MOVE      RPSTAT        SVSTAT
     C*
     C*  Update Request for Payment record with "Released" status
     C*
     C                   MOVE      *BLANK        RPSTAT
     C                   MOVEL     CXPSC2        RPSTS
     C  N60              MOVE      PSSRT         RPSORT
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPTIME
     C*** BEGIN ADD *** 02/25/00 **************************************
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     Q1USER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C*** END ADD ***** 02/25/00 **************************************
     C*
     C                   UPDATE    PRP02
     C*
     C                   EXSR      DARTS1                                       Complete DARTS Diary
     C*
     C*  If the claim is closed, increase reserves in claim master and
     C*    reserve history.
     C*
     C*** BEGIN ADD *** 04/12/00 **************************************
     C     CMKY          CHAIN     PCM01                              62
     C*** END ADD ***** 04/12/00 **************************************
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'R'           QQTYPE                         "Allocate" Res
     C                   CALL      'SRPRCC'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
     C                   ENDIF                                                  CMSTAT = 'C'
     C*
     C                   ENDIF
     C*
     C     RPKY2         READE     PRP02                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  RPEOB# > 0 and
     C*                                                     @@MODE = 'R'
     C*** END ADD ***** 05/19/98 **************************************
      *
     C                   ENDIF                                                  *IN60 (PCX01)
      *
     C                   ENDIF                                                  *IN99 (DBRCDF)
      *
     C     ENDCHG        ENDSR
     C*** Beg Add ******* 08/09/07 ***************************************
      *================================================================
      * CHGEFT - Subroutine to "Change EFT Status to non-EFT"
      *================================================================
     C     CHGEFT        BEGSR

      /free
       // Called from W0581R and W0583R
       // Just blank out RPeftc (EFT flag)
       // Don't update maint info unless the
       // value actually changes
       Chain @@rrn dbrcdf;
       If %found;
          svEftc = RPeftc;
          If svEftc = 'E';
             RPeftc = *blank;
             RPuser = Q1user;
             RPpgm  = Q1pgm;
             RPdt   = %dec(%date():*cymd);
             RPtime = %dec(%char(%time():*hms0):6:0);
             PopOneBtn ('Transaction changed from EFT to non-EFT. -
              Press OK to continue.' : 'Confirmation':'OK');
          Endif;
          Update dbrcdf;
       Endif;
       // Tell W0583R that the transaction wasn't
       //  EFT in the first place so it can
       //  invoke its edit. W0581R has no edit but
       //  'X' is returned anyway like prv versions
       If svEftc <> 'E';
          @@mode = 'X';
       Endif;
      /end-free
     C                   ENDSR
     C*** End Add ******* 08/09/07 ***************************************
      *
      *================================================================
      * DARTS1 - Complete DARTS Diary Item
      *================================================================
     C     DARTS1        BEGSR
      *
     C                   MOVE      *BLANK        CMKEY
     C                   MOVE      RPCO#         CCO#
     C                   MOVE      RPFND         CFND
     C                   MOVE      RPFYR         CFYR
     C                   MOVE      RPCASE        CCASE
     C*** BEGIN ADD *** 08/02/99 **************************************
     C                   MOVEL     'D'           ##RTCD
     C*** END ADD ***** 08/02/99 **************************************
      *
     C                   CALL      'WA018R'
     C                   PARM                    CMKEY
     C                   PARM                    RPDSEQ
     C                   PARM                    ##RTCD            1
      *
     C                   ENDSR
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C*** BEGIN ADD *** 04/12/02 **************************************
     C*  If RPSTAT = 'Y', do not process this change.
     C*
     C     RPSTAT        ANDNE     'Y'
     C*** END ADD ***** 04/12/02 **************************************
      *
     C                   Z-ADD     RPCO#         KKCO#
     C                   Z-ADD     RPFND         KKFND
     C                   Z-ADD     RPFYR         KKFYR
     C                   Z-ADD     RPCASE        KKCASE
     C                   MOVE      'DE'          DDSTS                          Payment Status
     C                   MOVE      'X'           DDSORT                         Payment Status SORT
     C                   MOVE      RPSTAT        SVSTAT                         Record Status SAVE
      *
      *  Find "Deleted" Payment Status code
      *
     C***** BEGIN DEL **** 07/01/99 ***********************************
     C**********           READ PPS01                    60
     C***** END   DEL **** 07/01/99 ***********************************
     C*
     C***** BEGIN ADD **** 07/01/99 ***********************************
     C     TBKY1         CHAIN     PPS01                              60
     C***** END   ADD **** 07/01/99 ***********************************
      *
     C     *IN60         DOWEQ     *OFF
     C     PSTYPE        IFEQ      'D'
     C     PSSTAT        ANDNE     'I'
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   READ      PPS01                                  60
     C                   ENDIF
     C                   ENDDO
      *
     C     PSTYPE        IFEQ      'D'
     C                   MOVEL     PSKEY         DDSTS                          Payment Status
     C                   MOVE      PSSRT         DDSORT                         Payment Status SORT
     C                   ENDIF
      *
      *  Update Request for Payment record with "Deleted" status
      *
     C                   MOVE      DDSTS         RPSTS                          Payment Status
     C                   MOVE      DDSORT        RPSORT                         Payment Status SORT
     C                   MOVE      RPSTAT        SVSTAT
     C                   MOVE      'D'           RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPTIME
      *
     C                   UPDATE    DBRCDF
      *
     C                   EXSR      DLCONT
      *
     C                   ENDIF                                                  *IN99
      *
      *  DO NOT allow normal Delete Processing to occur.
      *
     C                   RETURN
      *
     C                   ENDSR
      *
      *================================================================
      *  DLCONT - Subroutine to Continue "Delete" process.
      *================================================================
     C     DLCONT        BEGSR
      *
      *  Complete DARTS Remark
      *
     C                   EXSR      DARTS1                                       Complete DARTS Diary
      *
      *  If Claim Closed, Decrease Reserves in Claim & Reserve History
      *
     C*** BEGIN ADD *** 04/12/00 **************************************
     C     CMKY          CHAIN     PCM01                              62
     C*** END ADD ***** 04/12/00 **************************************
     C     SVSTAT        IFEQ      ' '
     C     CMSTAT        ANDEQ     'C'
     C                   MOVE      'D'           QQTYPE                         "Decrease" Res
      *
     C                   CALL      'SRPRCC'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
     C                   ENDIF                                                  SVSTAT
     C*
     C*  If an EOMB is attached and the request being deleted is not
     C*    the bill review or medical review payment, then delete all
     C*    requests with the corresponding EOB#.
     C*
     C     RPEOB#        IFGT      *ZERO
     C                   Z-ADD     RPEOB#        SVEOB#
     C*
     C     NRKEY         CHAIN     PNR01                              60
     C     *IN60         IFEQ      *ON
     C     NRGRTY        OREQ      'O'
     C*
     C*  Read all requested payments with the corresponding EOB#.
     C*
     C     RPKY2         CHAIN     PRP02                              60
     C     *IN60         DOWEQ     *OFF
     C     RPSTAT        IFNE      'D'
     C                   MOVE      RPSTAT        SVSTAT
     C*
     C*  Update Request for Payment record with "Deleted" status
     C*
     C                   MOVE      DDSORT        RPSORT
     C                   MOVE      DDSTS         RPSTS
     C                   MOVE      'D'           RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPTIME
      *
     C                   UPDATE    PRP02
      *
     C                   EXSR      DARTS1                                       Complete DARTS Diary
     C*
     C*  If the claim is closed and the payment is released, decrease
     C*    the reserves in claim master and reserve history.
     C*
     C*** BEGIN ADD *** 04/12/00 **************************************
     C     CMKY          CHAIN     PCM01                              62
     C*** END ADD ***** 04/12/00 **************************************
     C     SVSTAT        IFEQ      ' '
     C     CMSTAT        ANDEQ     'C'
     C                   MOVE      'D'           QQTYPE                         "Decrease" Reserves
     C                   CALL      'SRPRCC'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
     C                   ENDIF                                                  SVSTAT & CMSTAT
      *
     C                   ENDIF
      *
     C     RPKY2         READE     PRP02                                  60
     C                   ENDDO
      *
     C                   ENDIF
     C*
     C                   ENDIF                                                  RPEOB# > 0
      *
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      *
      *================================================================
      * EDTRES - Payment/Reserve Edits
      *================================================================
     C     EDTRES        BEGSR
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
     C                   PARM                    RPTRCD
     C                   PARM                    RPFEIN
     C                   PARM                    RPMOD
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
     C                   PARM                    RPSEQ#
     C                   PARM                    SCAMT
     C                   PARM                    RPRECD
     C                   PARM                    RPRETP
     C                   PARM                    ##RC
     C*** BEG ADD *** 05/30/00 **************************************
     C                   PARM                    SCAMT
     C                   PARM                    XXEOBS
     C                   PARM                    SCTRCD
     C*** END ADD *** 05/30/00 **************************************
     C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
     C*********  *IN52     CABEQ'1'       ENDCK1           DEL 01/30/01
     C     *IN09         CABEQ     '1'           ENDCK1
     C*** BEG ADD *** 12/12/01 **************************************
     C*  CHAIN TO GET THE EXCLUDE MEDICAL FLAG.
     C     RPRETP        IFEQ      'M'                                          ADD 10/10/02
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVEL(P)  SCSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              61
     C     ELEXMF        IFEQ      'Y'
     C     PSTYPE        ANDEQ     'R'
     C                   SETON                                        90  96
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF                                                  ADD 10/10/02
     C*** END ADD *** 12/12/01 **************************************
      *
      *  Determine if Request Payment Status changed
      *  Then Verify Payment Status code is valid
      *  If so, assign Sort & Record Status to "New" Payment Status code
      *
     C     SCSTS         IFNE      RPSTS
      *  Payment Status change
     C                   MOVE      RPSTAT        OLSTAT                         "Old"
     C*** BEGIN ADD *** 01/12/00 **************************************
     C                   MOVE      RPSTS         OLSTS                          "Old"
     C*** END ADD ***** 01/12/00 **************************************
     C                   MOVE      SCSTS         NWSTS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSTS         XXKEY
     C     TBKY          CHAIN     PPS01                              61
     C     *IN61         IFEQ      *OFF
     C     PSSTAT        ANDEQ     'A'
     C                   MOVE      PSSRT         NWSRT
     C     PSTYPE        IFEQ      'R'
     C                   MOVE      *BLANK        NWSTAT                         "New"
     C                   ELSE
     C                   MOVE      PSTYPE        NWSTAT                         "New"
     C                   ENDIF
      *
     C                   ELSE
      *  Invalid Payment Status code entered
     C                   Z-ADD     36            Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF
      *
     C                   ELSE
      *  NO Payment Status change
     C                   MOVE      RPSTAT        OLSTAT                         "Old"
     C                   MOVE      RPSTAT        NWSTAT                         "New"
     C*** Begin add *** 02/13/02 **************************************
     C                   MOVE      RPSTS         OLSTS                          "Old"
     C                   MOVE      RPSTS         NWSTS                          "New"
     C*** End add ***** 02/13/02 **************************************
     C                   ENDIF
      *
      *   If Payment Status is "Delete", skip edits.
      *
     C     NWSTAT        IFEQ      'D'
     C                   GOTO      ENDCK1
     C                   ENDIF
      *
      *   Execute Edit Field routine - Non-Blank Return Code indicates Error
      *
     C     RPPGTP        IFEQ      *BLANK
     C                   MOVE      'N'           SCGRP             1
     C                   ELSE
     C                   MOVE      'Y'           SCGRP
     C                   ENDIF
      *
     C*** BEG ADD *** 12/06/10 **************************************  *********
      *
      * BUILD ##EKEY AND PASS IT TO WA014 TO CHECK TO ENSURE THAT PAYEE
      * EFT IS SET UP FOR EFT PAYMENT. ##EKEY = PAYEE TYPE (C,D,P) CAT
      * KEY INFO FOR WMEL17
      *
     C                   MOVE      *BLANKS       ##EKEY
     C     RPFEIN        IFEQ      0
     C                   MOVEL(P)  RPFYR         XXWK10           10
     C                   MOVE      RPCASE        XXWK10
     C     RPDSQ#        IFEQ      0
     C                   MOVEL     'C'           ##EKEY
     C                   CAT(P)    XXWK10:0      ##EKEY
     C                   ELSE
     C                   MOVEL     'D'           ##EKEY
      *** BEG CHG ***** 04/26/11 ************************************
     C***********          MOVELXXWK10    XXWK30 30 P
     C***********          MOVE RPDSQ#    XXWK30
     C***********          CAT  XXWK30:0  ##EKEY    P
     C                   MOVEL     XXWK10        XXWK15           15
     C                   MOVE      RPDSQ#        XXWK15
     C                   CAT(P)    XXWK15:0      ##EKEY
      *** END CHG ***** 04/26/11 ************************************
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'P'           ##EKEY
     C                   MOVEL(P)  RPFEIN        XXWK12           12
     C                   MOVE      RPMOD         XXWK12
     C                   CAT(P)    XXWK12:0      ##EKEY
     C                   ENDIF                                                  SCFEIN=0
     C*** END ADD *** 12/06/10 *************************************************
     C                   CALL      'WA014R'
     C                   PARM                    RPCO#                          Company
     C                   PARM                    RPFND                          Group
     C                   PARM                    RPFYR                          Fund Year
     C                   PARM                    RPCASE                         Claim
     C                   PARM                    SCGRP                          Group Type
     C                   PARM                    SCPODT                         Posting Date
     C                   PARM                    SCRCVD                         Received Date
     C                   PARM                    SCFRDT                         From Date
     C                   PARM                    SCTODT                         To Date
     C                   PARM                    RPRETP                         Reserve Type
     C                   PARM                    RPRECD                         Reserve Code
     C                   PARM                    SCBR2                          Payment Value 1
     C                   PARM                    SCBR7                          Payment Value 2
     C                   PARM                    SCSTS                          Payment Status
     C                   PARM                    SCLTCD                         Paid on Time
     C                   PARM                    SCDYWK                         Number of Days
     C                   PARM                    ##RC                           Return Code
     C                   PARM                    SCEOB#                         EOMB#
     C                   PARM                    SCAMT                          Payment Amount
     C*** BEGIN ADD *** 07/25/00 **************************************
     C                   PARM                    SCRCN
     C*** END ADD ***** 07/25/00 **************************************
     C*** BEG ADD ***** 12/06/10 **************************************
     C                   PARM                    ##EKEY           31
     C*** END ADD ***** 12/06/10 **************************************
      *
     C*** Begin add *** 02/23/07 **************************************
     C     CLTID         IFEQ      '114'
     C     RPFND         IFEQ      440
     C     ##RC          ANDEQ     '33'
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 02/23/07 **************************************
      *
     C     ##RC          IFNE      *BLANK
     C     ##RC          ANDNE     '40'                                         ADD 05/15/00
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF
     C*** BEG ADD ***** 01/05/11 **************************************
     C     DUPLI         IFEQ      *ON
     C                   MOVE      '1'           *IN89
     C*
     C*  CALL WA081R TO CHECK FOR DUPLICATE PAYMENT
     C*
     C     RPINV#        IFNE      SCINV#
     C                   CALL      'WA081R'
     C                   PARM                    RPCO#
     C                   PARM                    RPFND
     C                   PARM                    RPFYR
     C                   PARM                    RPCASE
     C                   PARM                    RPSEQ#
     C                   PARM                    RPFEIN
     C                   PARM                    SCINV#
     C                   PARM                    XXFKEY
     C     *IN90         PARM      *ZERO         I90               1
     C*
     C* SCREEN SCR02 WILL BE RE-DISPLAYED *IN90 WILL BE ON IF DUPICATE PAYMENT
     C* FOUND
     C*
     C     *IN90         IFEQ      *ON
     C                   GOTO      ENDCK1
     C                   ENDIF                                                  SCINV# <> RPINV#
     C                   ENDIF                                                  *IN90
     C*
     C                   ENDIF                                                  DUPLI
     C*
     C*** END ADD ***** 01/05/11 **************************************
      *
     C*** Begin add *** 02/23/07 **************************************
     C     SKIP          TAG
     C*** End add ***** 02/23/07 **************************************
      *
      *  Payment Transaction Description can not be Blank.
      *
     C     SCTRDS        IFEQ      *BLANK
     C                   SETON                                        9077
     C                   GOTO      ENDCK1
     C                   ENDIF
      *
      *  Final Comp Payment flag must be 'Y' or 'N'.
      *
     C     *IN12         IFEQ      *OFF
     C     SCFNAL        IFNE      'Y'
     C     SCFNAL        ANDNE     'N'
     C                   SETON                                        9078
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF
      *
      *  EDIT REMARK CODE 1
      *
     C     SCRMK1        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK1        XXKEY
     C     TBKEY         CHAIN     PRC01                              79
     C  N79RCSTAT        COMP      'I'                                    79
     C     *IN79         IFEQ      '1'
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF
      *
      *  EDIT REMARK CODE 2
      *
     C     SCRMK2        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK2        XXKEY
     C     TBKEY         CHAIN     PRC01                              80
     C  N80RCSTAT        COMP      'I'                                    80
     C     *IN80         IFEQ      '1'
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF
      *
      *  EDIT REMARK CODE 3
      *
     C     SCRMK3        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK3        XXKEY
     C     TBKEY         CHAIN     PRC01                              81
     C  N81RCSTAT        COMP      'I'                                    81
     C     *IN81         IFEQ      '1'
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF
      *
      *   Only Check Remaining Reserves If Payment is "Released".
      *
     C     NWSTAT        IFEQ      ' '
      *
     C                   EXSR      EDTRES
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y                 2 0
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      ENDCK1
     C                   ENDIF
     C                   ENDIF
      *
      *  Check for Duplicate or Possible Duplicate Payment
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NUM7              7 0
     C                   Z-ADD     NUM7          XXFROM
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NUM7
     C                   Z-ADD     NUM7          XXTO
      *
     C*** BEGIN CHG *** 10/09/08 **************************************
     C********** SVAMT     IFNE SCAMT
     C********** SVFRDT    ORNE XXFROM
     C********** SVTODT    ORNE XXTO
     C     SVSTS         IFNE      SCSTS
     C     SCSTS         ANDEQ     'PY'
     C     RELEAS        ANDEQ     *OFF
     C*
     C                   MOVE      *ON           RELEAS            1
     C*** END   ADD *** 10/09/08 **************************************
      *
     C                   Z-ADD     SCAMT         SVAMT
     C                   Z-ADD     XXFROM        SVFRDT
     C                   Z-ADD     XXTO          SVTODT
      *
     C     51            SUBST     SCCS01:12     QQDFND                         Fund Name
     C                   MOVEL     S2NAME        QQDCNM                         Claimant Name
     C                   MOVEL     SCPNAM        QQDPNM                         Provider Name
      *
     C                   CALL      'WA019R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKSEQ
     C                   PARM                    XXFROM
     C                   PARM                    XXTO
     C                   PARM                    SCAMT
     C                   PARM                    SVRECD
     C                   PARM                    SVFEIN
     C                   PARM                    QQDFND           51            Fund Name
     C                   PARM                    QQDCNM           36            Claimant Name
     C                   PARM                    QQDPNM           36            Provider Name
     C     *IN90         PARM      *ZERO         I90               1
     C***  BEGIN ADD  ***  03/02/99  *********************************
     C                   PARM                    SVRETP
     C***  END   ADD  ***  03/02/99  *********************************
     C*** BEGIN ADD *** 09/05/00 **************************************
     C                   PARM      RPMOD         @RPMOD            3 0
     C*** END   ADD *** 09/05/00 **************************************
      *
      *  Re-position/read original Request for Payment record
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C                   ENDIF                                                  SVSTAT = ' '
      *
      *   ALL edits have been passed.
      *
     C     ENDCK1        TAG
      *
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
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
     C     KJDO          BEGSR
      *
     C                   MOVE      HFLD          FIELD
      *
     C                   SELECT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCBR2'
     C*** BEG ADD *** 02/23/07 **************************************
     C                   MOVE      ##FND         #2FND             3
     C*** END ADD *** 02/23/07 *************************************
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
     C*** BEG ADD *** 02/23/07 **************************************
     C                   PARM      *BLANKS       ##RECD
     C                   PARM                    #2FND
     C*** END ADD *** 02/23/07 *************************************
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C     ##KEY         ANDNE     *BLANK
     C                   MOVEL     ##KEY         SCBR2
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C                   MOVE      *ON           *IN41
     C                   MOVE      *BLANK        SCDBR2
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B2DESC        SCDBR2
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN40
     C                   ENDIF
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCBR7'
     C*** BEG ADD *** 02/23/07 **************************************
     C                   MOVE      KKFND         #2FND
     C*** END ADD *** 02/23/07 **************************************
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2151R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
     C*** BEG ADD *** 02/23/07 **************************************
     C                   PARM                    #2FND
     C*** END ADD *** 02/23/07 *************************************
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C     ##KEY         ANDNE     *BLANK
     C                   MOVEL     ##KEY         SCBR7
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR7         XXKEY
     C                   MOVE      *ON           *IN51
     C                   MOVE      *BLANK        SCDBR7
     C     TBKY          CHAIN     PB701                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B7DESC        SCDBR7
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN41
     C                   ENDIF
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRMK1'
     C     HFLD          OREQ      'SCRMK2'
     C     HFLD          OREQ      'SCRMK3'
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2171R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C     ##KEY         ANDNE     *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ##KEY         XXKEY
     C                   SETOFF                                       444547
      *
     C                   SELECT                                                 Cursor Next Field
     C     FLDNUM        WHENEQ    '1'
     C                   MOVEL     ##KEY         SCRMK1
     C                   SETON                                        44
     C     FLDNUM        WHENEQ    '2'
     C                   MOVEL     ##KEY         SCRMK2
     C                   SETON                                        45
     C     FLDNUM        WHENEQ    '3'
     C                   MOVEL     ##KEY         SCRMK3
     C                   SETON                                        47
     C                   ENDSL
      *
     C     TBKY          CHAIN     PRC01                              61
     C     *IN61         IFEQ      *OFF
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   SELECT                                                 Default Cursor
     C     FLDNUM        WHENEQ    '1'
     C                   SETON                                        46
     C     FLDNUM        WHENEQ    '2'
     C                   SETON                                        44
     C     FLDNUM        WHENEQ    '3'
     C                   SETON                                        45
     C                   ENDSL
      *
     C                   ENDIF
      *
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCSTS'
     C                   MOVE      *BLANK        ##KEY
      *
     C                   CALL      'W2142R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C     ##KEY         ANDNE     *BLANK
     C                   MOVEL     ##KEY         SCSTS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSTS         XXKEY
     C                   MOVE      *ON           *IN40
     C                   MOVE      *BLANK        SCSDES
     C     TBKY          CHAIN     PPS01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     PSDESC        SCSDES
     C                   ENDIF
     C                   ELSE
     C                   MOVEA     '1101'        *IN(82)
     C                   ENDIF
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCLTCD'
     C                   MOVE      *BLANK        ##KEY
      *
     C                   CALL      'W2221R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C     ##KEY         ANDNE     *BLANK
     C                   MOVEL     ##KEY         SCLTCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCLTCD        XXKEY
     C                   MOVEA     '1011'        *IN(82)                        Position Cursor
     C                   MOVE      *BLANK        SCDLTC
     C     TBKY          CHAIN     PLR01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     LRDESC        SCDLTC           30
     C                   ENDIF
     C                   ELSE
     C                   MOVEA     '1100'        *IN(82)
     C                   ENDIF
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCPODT'
     C                   MOVEA     '0001'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCAMT'
     C                   MOVEA     '0010'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCFRDT'
     C                   MOVEA     '0011'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCTODT'
     C                   MOVEA     '0100'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCWKS'
     C                   MOVEA     '0101'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCDYWK'
     C                   MOVEA     '0110'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRCVD'
     C                   MOVEA     '0111'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCTRDS'
     C                   MOVEA     '1000'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCFNAL'
     C                   MOVEA     '1001'        *IN(82)
     C                   SETON                                        90
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRMDS'
     C                   MOVEA     '1010'        *IN(82)
     C                   SETON                                        9047
      *________________________________________________________________
     C                   OTHER
      *
      *  No Search available for field in cursor selection
     C                   SETON                                        4990
      *
     C                   ENDSL
     C*** BEG ADD *** 01/05/11 **************************************
     C     DUPLI         IFEQ      '1'
     C                   MOVE      '1'           *IN89
     C                   MOVE      '0'           *IN64
     C                   ENDIF
     C*** END ADD *** 01/05/11 **************************************
      *
     C                   ENDSR
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   SETOFF                                       212223
     C                   SETOFF                                       2425
      *
      *  Initialize Output Fields
      *
     C                   MOVE      *BLANK        SCDBR2
     C                   MOVE      *BLANK        SCDBR7
      *
      *  Assign Key Fields
      *
     C                   Z-ADD     RPCO#         KKCO#
     C                   Z-ADD     RPFND         KKFND
     C                   Z-ADD     RPFYR         KKFYR
     C                   Z-ADD     RPCASE        KKCASE
     C                   Z-ADD     RPCHK#        KKCHK#
     C                   Z-ADD     RPTRDT        KKTRDT
     C                   Z-ADD     RPSEQ#        KKSEQ
      *
      *  Build Claim Header
      *
     C                   MOVE      RPCO#         ##CO#
     C                   MOVE      RPFND         ##FND
     C                   MOVE      RPFYR         ##FYR             3 0
     C                   MOVE      RPCASE        ##CASE            7 0
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
      *
      *  Execute Re-Insurance logic for Claim
      *
     C                   CALL      'WA010R'
     C                   PARM                    @@RRN             9 0          FOR WMCMP
     C                   PARM      *ZERO         XXFNDR           13 2
     C                   PARM      *BLANK        XXEXRE            1
      *  Excess
     C     XXEXRE        IFEQ      'Y'
     C                   SETON                                        21
     C                   ENDIF
      *
     C     F1KY          CHAIN     PF101                              60
      *
      *   CLAIM MASTER Record accessed in PGMSTR routine
      *
     C     *IN62         IFEQ      '0'
      *
     C                   MOVE      CMJRST        SCJRST
     C                   MOVE      CMNCCI        SCNCCI
     C                   MOVE      CMLTFL        SCLTFL
      *
      *  Controvert
     C     CMCVFL        IFEQ      'Y'
     C                   SETON                                        22
     C                   ENDIF
      *  Denied
     C     CMDNY         IFEQ      'Y'
     C                   SETON                                        23
     C                   ENDIF
      *  Group
     C     RPPGTP        IFEQ      'G'
     C                   SETON                                        24
     C                   ENDIF
      *  Additional Address
     C     CMKY          CHAIN     PAC01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        25
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      RPEOB#        SCEOB#
     C                   MOVE      RPEOBS        XXEOBS            3 0          ADD 05/30/00
     C*** BEGIN ADD *** 07/25/00 **************************************
     C                   MOVE      RPEOBS        SCRCN
     C*** END ADD ***** 07/25/00 **************************************
     C*** BEG ADD ***** 01/30/01 **************************************
     C     RPEOB#        IFNE      *ZERO
     C                   MOVE      *ON           *IN52
     C                   ELSE
     C                   MOVE      *OFF          *IN52
     C                   ENDIF
     C*** EBD ADD ***** 01/30/01 **************************************
      *
     C                   Z-ADD     RPRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCVD
      *
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPODT
      *
     C                   Z-ADD     RPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
      *
     C                   Z-ADD     RPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
      *
     C                   Z-ADD     RPWKS         SCWKS
     C                   Z-ADD     RPDAYS        SCDYWK
     C                   Z-ADD     RPAMT         SCAMT
     C                   Z-ADD     RPAMT         XXAMT
      *
      *  Payment Value 1
      *
     C                   MOVE      RPBR2         SCBR2
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B2DESC        SCDBR2
     C                   ENDIF
      *
      *  Payment Value 2
      *
     C                   MOVE      RPBR7         SCBR7
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR7         XXKEY
     C     TBKY          CHAIN     PB701                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B7DESC        SCDBR7
     C                   ENDIF
      *
      *  Payment Status
      *
     C                   MOVE      RPSTS         SVSTS
     C                   MOVE      RPSTS         SCSTS
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSTS         XXKEY
     C                   MOVE      *BLANK        SCSDES
     C     TBKY          CHAIN     PPS01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     PSDESC        SCSDES
     C                   ENDIF
      *
     C                   MOVE      RPRMDS        SCRMDS
      *
     C                   MOVE      RPRMK1        SCRMK1
     C                   MOVE      RPRMK2        SCRMK2
     C                   MOVE      RPRMK3        SCRMK3
     C                   MOVE      RPTRCD        SCTRCD
     C                   MOVE      RPDESC        SCTRDS
     C                   MOVE      RPPNAM        SCPNAM
     C                   MOVE      RPADR1        SCADR1
     C                   MOVE      RPADR2        SCADR2
     C                   MOVE      RPCTY         SCCTY
     C                   MOVE      RPST          SCST
     C                   MOVEL     RPZIP         SCZIP1
     C                   MOVE      RPZIP         SCZIP2
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C     RPCTRY        IFNE      *BLANK
     C     RPCTRY        ANDNE     'USA'
     C                   MOVEL(P)  RPADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  RPCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  RPCTRY        SCCTRY
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C*
     C*** Begin add *** 01/11/06 *************************************
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRY
     C*** End add ***** 01/11/06 *************************************
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*** END ADD *** 10/20/03 *******************************************  ****
     C*
     C                   MOVE      RPFNAL        SCFNAL
     C                   MOVE      RPLTCD        SCLTCD
     C                   MOVE      RPSORT        SCSORT            1            Payment SORT code
     C                   MOVE      RPSTAT        SCSTAT            1            Record Status
      *
     C                   MOVE      *OFF          *IN55
     C     RPFEIN        IFGT      *ZERO
     C                   MOVE      *ON           *IN55
     C                   ENDIF
      *
     C                   MOVE      RPRECD        SVRECD
     C***  BEGIN ADD  ***  03/02/99  *********************************
     C                   MOVE      RPRETP        SVRETP
     C***  END   ADD  ***  03/02/99  *********************************
     C                   MOVE      RPSTAT        SVSTAT
     C                   Z-ADD     RPFEIN        SVFEIN
     C                   Z-ADD     RPEOB#        SVEOB#
     C                   Z-ADD     RPAMT         SVAMT
     C                   Z-ADD     RPFRDT        SVFRDT
     C                   Z-ADD     RPTODT        SVTODT
     C*** BEGIN ADD *** 09/08/98 **************************************
     C                   Z-ADD     RPAMT         SVAMT1
     C*** END ADD ***** 09/08/98 **************************************
     C*** BEG ADD ***** 01/05/11 **************************************
     C                   MOVEL     RPINV#        SCINV#
     C*** END ADD ***** 01/05/11 **************************************
      *
     C                   MOVE      *OFF          *IN12
     C     RPRETP        IFNE      'C'
     C                   MOVE      '1'           *IN12
     C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   MOVE      SCEOB#        RPEOB#
     C*** BEGIN ADD *** 07/25/00 **************************************
     C                   MOVE      SCRCN         RPEOBS
     C*** END ADD ***** 07/25/00 **************************************
      *
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPRCVD
      *
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTRDT
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTODT
      *
     C                   Z-ADD     SCWKS         RPWKS
     C                   Z-ADD     SCDYWK        RPDAYS
     C                   Z-ADD     SCAMT         RPAMT
     C                   MOVE      SCBR2         RPBR2
     C                   MOVE      SCBR7         RPBR7
     C                   MOVE      CMNCCI        RPNCCI
     C                   MOVE      SCRMDS        RPRMDS
     C                   MOVE      SCRMK1        RPRMK1
     C                   MOVE      SCRMK2        RPRMK2
     C                   MOVE      SCRMK3        RPRMK3
     C                   MOVE      SCTRDS        RPDESC
     C                   MOVE      SCPNAM        RPPNAM
     C                   MOVE      SCADR1        RPADR1
     C                   MOVE      SCADR2        RPADR2
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C                   MOVE      SCADR3        RPADR3
     C                   MOVE      SCCTRY        RPCTRY
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   MOVE      SCCTY         RPCTY
     C                   MOVE      SCST          RPST
     C                   MOVEL     SCZIP1        RPZIP
     C                   MOVE      SCZIP2        RPZIP
     C                   MOVE      SCLTCD        RPLTCD
     C                   MOVE      SCFNAL        RPFNAL
     C                   MOVEL     SCTRDS        RPDESC
      *
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   MOVEL     DSTIME        RPTIME
      *
      *  If Payment Status changed, assign RP fields with "New" info
      *
     C*** BEGIN ADD *** 01/12/00 **************************************
     C     OLSTS         IFNE      NWSTS
     C                   MOVE      NWSTS         RPSTS
     C                   MOVE      NWSRT         RPSORT
     C                   ENDIF
     C*** END ADD ***** 01/12/00 **************************************
      *
     C     OLSTAT        IFNE      NWSTAT
     C*** BEGIN DLT *** 01/12/00 **************************************
     C*******************  MOVE NWSTS     RPSTS
     C*******************  MOVE NWSRT     RPSORT
     C*** END DLT ***** 01/12/00 **************************************
     C                   MOVE      NWSTAT        RPSTAT
     C     RPSTAT        IFEQ      'P'
      *  Assign DARTS Diary Sent flag and Sequence #
     C                   MOVE      *BLANK        RPSENT                         Diary Sent flag
     C                   Z-ADD     *ZERO         RPDSEQ                         Diary Sequence #
     C                   ENDIF
     C*** BEGIN ADD *** 02/25/00 **************************************
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     RPDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     Q1USER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*** END ADD ***** 02/25/00 **************************************
     C                   ENDIF
      *
     C*** Begin add *** 04/25/01 **************************************
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      RPCO#         #1CO#
     C                   MOVE      RPFND         #1FND
     C                   MOVE      RPFYR         #1FYR
     C                   MOVE      RPCASE        #1CASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C*** Begin add *** 02/23/07 **************************************
     C                   MOVE      RPCMSQ        QXSEQ#
     C*** End add ***** 02/23/07 **************************************
     C*** Begin add *** 02/23/07 **************************************
     C                   MOVE      RPEOB#        QXEOB#
     C*** End add ***** 02/23/07 **************************************
     C*** Beg Add *** 08/09/07 **************************************
     C                   MOVE      RPFEIN        QXFEIN
     C                   MOVE      RPMOD         QXMOD
     C                   MOVE      RPDSQ#        QXDSQ#
     C*** End Add ***** 08/09/07 **************************************
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    #1CO#             3            Company
     C                   PARM                    #1FND             3            Group #
     C                   PARM                    #1FYR             3            Year
     C                   PARM                    #1CASE            7            Case
     C                   PARM      'WC'          ##APPL            2            Applic Code
     C*** Beg Chg *** 08/09/07 **************************************
     C****************     PARM DATA      ##DTA  50        Selection criteria
     C                   PARM      DATA          ##DTA            75            Selection criteria
     C*** End Chg *** 08/09/07 **************************************
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD                         Return Code
      *
     C*** Begin add *** 02/23/07 **************************************
     C     CLTID         IFNE      '144'
     C*** End add ***** 02/23/07 **************************************
     C                   MOVEL     ##CKTP        RPCKTP
     C*** Begin add *** 02/23/07 **************************************
     C                   ELSE
     C     RPCKTP        IFNE      'CPAMED'
     C     RPCKTP        ANDNE     'CPARX'
     C                   MOVEL     ##CKTP        RPCKTP
     C                   ENDIF
     C                   ENDIF
     C*** End add ***** 02/23/07 **************************************
     C*** End add ***** 04/25/01 **************************************
     C*** BEG ADD ***** 01/05/11 **************************************
     C                   MOVEL     SCINV#        RPINV#
     C*** END ADD ***** 01/05/11 **************************************
      *
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
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
      *
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
     C*
     C*** BEGIN ADD *** 05/19/98 **************************************
     C*  Get the CO#/FND/FYR/CASE from the pmt request record so the
     C*    proper claim record will be retrieved.
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C   99              RETURN
     C                   Z-ADD     RPCO#         KKCO#
     C                   Z-ADD     RPFND         KKFND
     C                   Z-ADD     RPFYR         KKFYR
     C                   Z-ADD     RPCASE        KKCASE
     C*** END ADD ***** 05/19/98 **************************************
     C*
     C*** BEGIN ADD *** 04/12/02 **************************************
     C*  If RPSTAT = 'Y', protect the request from being changed.
     C*
     C*** Begin add *** 06/27/02 **************************************
     C     *IN09         IFEQ      *OFF
     C*** End add ***** 06/27/02 **************************************
     C     RPSTAT        COMP      'Y'                                    09
     C*** END ADD ***** 04/12/02 **************************************
     C*** Begin add *** 06/27/02 **************************************
     C                   ENDIF
     C*** End add ***** 06/27/02 **************************************
      *
      *  Get Claim Master record
      *
     C     CMKY          CHAIN(N)  PCM01                              62
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         #ACO#
     C                   MOVE      CMFND         #AFND
     C                   MOVE      CMFYR         #AFYR
     C                   MOVE      CMCASE        #ACASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   RETURN
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*** BEG ADD ***** 01/05/11 ***********************************************
     C                   MOVE      *BLANKS       XXFKEY            2
     C* New invoice field will display only for client 114, when payment
     C* reserve type is expence,legal or vocational rehab.
     C*** Begin Chg *** 04/10/19 ***************************************
     C*****CLTID         IFEQ      '114'
     C*****RPRETP        ANDEQ     'E'
     C**********
     C*****CLTID         OREQ      '114'
     C*****RPRETP        ANDEQ     'L'
     C**********
     C*****CLTID         OREQ      '114'
     C*****RPRETP        ANDEQ     'M'
     C*****RPRECD        ANDEQ     'RE'
     C**********
     C     CLTID         IFEQ      '114'
     C     CLTID         OREQ      '152'

     C     RPRETP        IFEQ      'E'
     C
     C     RPRETP        OREQ      'L'
     C
     C     RPRETP        OREQ      'M'
     C     RPRECD        ANDEQ     'RE'
     C*** End   Chg *** 04/10/19 ***************************************
     C*
     C                   SETON                                        89
     C                   SETON                                        64
     C                   MOVE      '1'           DUPLI             1
     C                   ENDIF                                                  CLTID = '114'
     C*** Begin Add *** 04/10/19 ***************************************
     C                   ENDIF                                                  CLTID = '114'
     C*** End   Add *** 04/10/19 ***************************************
     C*** END ADD **** 01/05/11 ***************************************
      *
     C/COPY SINDIC01
      *
     C                   ENDSR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
      *          __________________________________________
     C     @@MODE        WHENEQ    'N'
     C     @@MODE        OREQ      'R'
      *
     C                   EXSR      CHGSTS
     C                   RETURN
      *          __________________________________________
     C     @@MODE        WHENEQ    'G'
      *
     C                   EXSR      CHGGRP
     C                   RETURN
     C*** Beg Chg *** 08/09/07 **************************************
     C     @@MODE        WHENEQ    'F'
      *
     C                   EXSR      CHGEFT
     C                   RETURN
     C*** End Chg *** 08/09/07 **************************************
      *
     C                   ENDSL
      *
     C                   ENDSR
      *
     C*** BEGIN DLT *** 05/19/98 **************************************
      *================================================================
      * RESERV - Update Claim Master and Reserve History with RPAMT
      *================================================================
     C********** RESERV    BEGSR
      **********
      *  Assign Key Fields
      **********
     C**********           Z-ADDRPCO#     KKCO#
     C**********           Z-ADDRPFND     KKFND
     C**********           Z-ADDRPFYR     KKFYR
     C**********           Z-ADDRPCASE    KKCASE
      **********
      *  Get Claim Master record to determine if CLOSED
      **********
     C********** CMKY      CHAINPCM01                60
      **********
     C********** *IN60     IFEQ '0'
     C********** CMSTAT    ANDEQ'C'
      **********
      *  Claim CLOSED.  Adjust Reserves by "Requested" Amount
      **********
     C**********           Z-ADD*ZERO     TEST    92
     C**********           Z-ADD1         X       30
     C********** RPRECD    LOKUPRT,X                     60
     C********** *IN60     IFEQ *ON
     C********** RR,X      SUB  RPAMT     RR,X
     C********** RR,X      IFLT *ZERO
     C********** RPAMT     ADD  RR,X      XXAMT
     C**********           Z-ADD*ZERO     RR,X
     C**********           ELSE
     C**********           Z-ADDRPAMT     XXAMT
     C**********           ENDIF
      **********
      *  Update Claim with adjusted Remaining Reserves
      **********
     C**********           MOVE Q1USER    CMUSER
     C**********           MOVE Q1PGM     CMPGM
     C**********           Z-ADDRPDT      CMDT
     C**********           Z-ADDRPTIME    CMTIME
      **********
      *  Adjust Claim Reserve History
      **********
     C**********           MOVE RPRECD    RHRT01
     C********** XXAMT     MULT -1        RHSU01
     C**********           Z-ADDRPDT      RHEFDT
     C**********           MOVE CRH       RHEXPL
     C**********           MOVE *BLANKS   RHSTAT
     C**********           MOVE CMSTAT    RHCLST
     C**********           MOVE Q1USER    RHUSER
     C**********           MOVE Q1PGM     RHPGM
     C**********           Z-ADDRPDT      RHDT
     C**********           Z-ADDRPTIME    RHTIME
      **********
     C**********           WRITEPRH01
      **********
     C**********           ENDIF
     C**********           ENDIF
      **********
      *  Update to release record lock
      **********
     C**********           UPDATPCM01
      **********
     C**********           ENDSR
     C*** END DLT ***** 05/19/98 **************************************
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
      *
      *  If Payment Status changed, determine specific change and process
      *  accordingly.
      *
     C*** BEGIN ADD *** 04/12/00 **************************************
     C     CMKY          CHAIN     PCM01                              62
     C*** END ADD ***** 04/12/00 **************************************
      *
     C     OLSTAT        IFNE      NWSTAT
     C                   SELECT
      *
     C     NWSTAT        WHENEQ    'D'                                          "Deleting"
     C                   MOVE      OLSTAT        SVSTAT
     C                   EXSR      DLCONT
      *
     C     NWSTAT        WHENEQ    'P'                                          "Pending" a previous
     C     OLSTAT        ANDEQ     ' '                                          "Released" payment
     C     CMSTAT        ANDEQ     'C'                                          "Closed" Claim
     C                   MOVE      'D'           QQTYPE                         "Decrease" Reserves
      *
     C                   CALL      'SRPRCC'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C*** BEGIN CHG *** 09/08/98 **************************************
     C******************** PARM           RPAMT
     C* Only decrease the reserves by the old pmt amount.
     C                   PARM                    SVAMT1
     C*** END CHG ***** 09/08/98 **************************************
      *
     C     NWSTAT        WHENEQ    ' '                                          "Releasing" payment
     C*** BEGIN ADD *** 09/08/98 **************************************
     C     OLSTAT        ANDEQ     'P'
     C*** END ADD ***** 09/08/98 **************************************
     C     CMSTAT        ANDEQ     'C'                                          "Closed" Claim
     C*** Begin add *** 04/02/08 **************************************
     C     NWSTAT        OREQ      ' '
     C     OLSTAT        ANDEQ     'D'
     C     CMSTAT        ANDEQ     'C'
     C*** End add ***** 04/02/08 **************************************
     C                   MOVE      'R'           QQTYPE                         "Increase" Reserves
      *
     C                   CALL      'SRPRCC'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
      *
     C     OLSTAT        IFEQ      'P'                                          Previously "Pended"
     C                   EXSR      DARTS1
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  Paymnt STS Chg
     C*
     C*** BEGIN ADD *** 09/08/98 **************************************
     C*  When changing the amount of a released pmt on a closed claim,
     C*    create a reserve history record for the difference.
     C*
     C     SCAMT         IFNE      SVAMT1
     C     NWSTAT        ANDEQ     ' '
     C     OLSTAT        ANDEQ     ' '
     C     CMSTAT        ANDEQ     'C'
     C                   MOVE      'R'           QQTYPE
     C     RPAMT         SUB       SVAMT1        XXDIF
      *
     C                   CALL      'SRPRCC'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE
     C                   PARM                    XXDIF
     C                   ENDIF                                                  Paymnt STS Chg
     C*** END ADD ***** 09/08/98 **************************************
      *
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WARPP
      *
     C                   ENDSR
