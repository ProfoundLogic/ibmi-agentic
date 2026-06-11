     H* (c) 2013 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H/TITLE W7000R - UPDATE CHECK TRANSACTIONS
      *** Begin Change 06/21/19 ***************************************
     H**dftactgrp(*no) actgrp(*caller)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
      *** End   Change 06/21/19 ***************************************
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/28/19 C:180 Project: WC2604                                *
     H* 06/21/19       Add interface to IDC module.                   *
     H*                                                               *
     H* 03/29/13 C:136 SUPPORT: WA090 81891                           *
     H*                POPULATE DEP SEQ# IN WDPAP SO THAT             *
     H*                EFT WILL WORK WHEN PRIMARY CLAIM HAS NO EFT    *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY. Added actgrp directives    *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Load RPEFTC to PAEFTC.                         *
     H*                                                               *
     H* 08/29/11 C:277 SUPPORT 77320                                  *
     H*                Populate WDPAP initial fields                  *
     H*                                                               *
     H* 01/05/11 C:257 PROJECT: 114WXM                                *
     H*                Write new field RPINV# to PAINV#.              *
     H*                                                               *
     H* 05/13/10 C:61  PROJECT: 136WFD                                *
     H*                Update Maintenance fields.                     *
     H*                                                               *
     H* 07/27/09 C:136 PROJECT: 114WUK                                *
     H*                ALLOW FOR PAYMENTS TO BE PROCESSED BY XML      *
     H*                                                               *
     H* 08/21/07 C:062 PROJECT:  MC044 58291                          *
     H*                Check for status of WMNRP.                     *
     H*                                                               *
     H* 06/15/07 C:062 PROJECT:  139WGE                               *
     H*                Correct how managed care is updated            *
     H*                                                               *
     H* 02/26/07 C:185 Project:114WMF Back to Base.                   *
     H*                114WBY Dated 06/07/01.                         *
     H*                                                               *
     H* 01/30/07 C:133 PROJECT:  139WGE                               *
     H*               Revision after test by client                   *
     H*                                                               *
     H* 12/15/06 C:133 PROJECT:  139WGE                               *
     H*             - Load reqwest payment trans date into new field  *
     H*               WDPAP/PARQDT if client 139.                     *
     H*             - Load system date into WDPAP/PATRDT and into     *
     H*               WDRPP/PRTRDT if client 139.                     *
     H*             - Load system date into WDPAP/PAPODT if clt 139   *
     H*               WDRPP/PRTRDT if client 139.                     *
     H*                                                               *
     H* 12/11/03 C:53 Support: 40175                                  *
     H*               Moved code to only write to WDRPP if we are     *
     H*               writing a WDPAP rcd.  This will prevent the     *
     H*               overwriting of fields in WARPP in case of       *
     H*               a processing error with BUL#'S                  *
     H*                                                               *
     H* 05/15/03 C:15 Project: WC577                                  *
     H*               Added code to load the View Check File seq#.    *
     H*                                                               *
     H* 05/09/01 C:15 Project: WC1020                                 *
     H*               Added code to read the check schedule records   *
     H*               by CO#/PRDT/ACCT/CKTP/FND.  And read the rqst   *
     H*               pmt records by CO#/CKTP/FND.                    *
     H*                                                               *
     H* 05/01/00 C:15 Support: WC034/20089                            *
     H*               Added code to load the correct trans date which *
     H*               is loaded into WDRPP.                           *
     H*                                                               *
     H* 03/02/00 C:62 Project: MCMNT                                  *
     H*               ADD PARM TO CALL F7304R                         *
     H*                                                               *
     H* 11/11/99 C:15 Project: WC577                                  *
     H*              -Added code to prevent losing WARPP records if   *
     H*               a user enters the check schedule record after   *
     H*               the check printing pgm runs or if a user enters *
     H*               a check request during the batch.               *
     H*              -Added code to help analyze neg reserve problem. *
     H*                                                               *
     H* 07/27/99 C:62 PROJECT: MC201
     H*               ADD CALL TO F7304R AND CONDITION IF PROVIDER
     H*               BILLS INSTALLED.
     H*                                                               *
     H*  06/10/99  C:79  PROJECT: SUP17939                            *
     H*                  ADD RECON NUMBER TO CALL OF F7304R SO THAT   *
     H*                  CHECK DATES/NUMBERS ARE WRITTEN CORRECTLY    *
     H*                  TO FAEAP.                                    *
     H*
     H* 03/15/99 C:94 PROJECT: 25MKL
     H*               MOVE RPEOBS TO PAEOBS
     H*
     H* 04/22/98 C:15 Project:  seq# 10666                            *
     H*               Chgd the pgm to load XXDT into RPDT so it will  *
     H*               match the date loaded into PATRDT.              *
     H*                                                               *
     H* 02/14/91 KWK  PROJECT WC034                                   *
     H*               WHEN CREATING PAYMENT DETAIL RECORD FROM        *
     H*               REQUEST PAYMENT RECORD, UPDATE PAYMENT DETAIL   *
     H*               RECORD WITH BATCH UPDATE LOG NUMBER FROM THE    *
     H*               REQUEST PAYMENT RECORD                          *
     H*                                                               *
     H* 10/13/93 C:15 PROJECT WC258                                   *
     H*               ONLY PROCESS REQUESTS WHICH HAVE A TRANSACTION  *
     H*               DATE LESS THAN OR EQUAL TO THE BATCH DATE.      *
     H*                                                               *
     H*  02/16/95 C:15 PROJECT: WC357                                 *
     H*                BECAUSE THE GROUP CHECK PROCESS WAS ADDED TO   *
     H*                BASE, CODE WAS ADDED TO INITIALIZE NEW FIELDS. *
     H*                                                               *
     H* 04/26/95 C:26 PROJECT WC423                                   *
     H*               ADDED CODE TO WRITE TO WDRPP FOR EACH CHECK.    *
     H*                                                               *
     H* 08/21/96 C:56 PROJECT WC371                                   *
     H*               REMOVE UPDATE TO CLAIMS MASTER NCCI CODE        *
     H*****************************************************************
     F*** Begin chg *** 05/09/01 **************************************
     F*******WARPL1  UF  E           K        DISK
     FWARPLS    UF   E           K DISK
     F*** End chg ***** 05/09/01 **************************************
     FWDELL     UF   E           K DISK
     FWMCML1    UF   E           K DISK
      *** Begin Change *** 06/21/19 ***********************************
     F**WDPAL1    IF A E           K DISK
     FWDPAL1    IF A E           K DISK    InfDS(dbfPPAds)
      *** End   Change *** 06/21/19 ***********************************
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     F*** Begin chg *** 05/09/01 **************************************
     F******WACPL   UF  E           K        DISK
     FWACPL4    UF   E           K DISK
     F*** End chg ***** 05/09/01 **************************************
     F*** Begin chg *** 12/11/03 **************************************
     F***WDRPL1  IF  E           K        DISK                      A
     FWDRPL1    O    E           K DISK
     F*** End chg ***** 12/11/03 **************************************
     F                                     RENAME(PRP01:PRP02)
     F*** BEGIN ADD *** 07/27/99 **************************************
     F*  Network Review ID file
     FWMNRL     IF   E           K DISK
     F*** END   ADD *** 07/27/99 **************************************
     F*** Begin add *** 02/26/07 **************************************
     F*** BEGIN CHG *** 06/15/07 **************************************
     F**********WMCRL   UF  E           K        DISK
     FWMCRL     UF   E           K DISK    USROPN
     F*** END   CHG *** 06/15/07 **************************************
     F*** BEG ADD ***** 07/27/09 **************************************
     FWDF2L     IF   E           K DISK
     F*** END ADD ***** 07/27/09 **************************************
     F*** End add ***** 02/26/07 **************************************
     D*** End add ***** 12/11/03 **************************************
     D*** Begin dlt *** 12/11/03 **************************************
     D*** BEGIN ADD *** 05/01/00 **************************************
     D**PRP02
     D*****         RPTRDT                          R2TRDT
     D*** Begin add *** 05/15/03 **************************************
     D*****         RPSEQ#                          R2SEQ#
     D*** End add ***** 05/15/03 **************************************
     D*** END ADD ***** 05/01/00 **************************************
     D*** End dlt ***** 12/11/03 **************************************
      *** Begin Add *** 06/21/19 **************************************
     D dbfPPAds        DS
     D  dbPPArrn             397    400B 0
      *** End   Add *** 06/21/19 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CD                     1     20
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
     D  PA                     1     90  2
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
     D  RR                     1     90  2
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
      *** Begin Add *** 06/21/19 **************************************
     D/Copy sprmodule
     D/Copy wpridc
     D intRRN          S              9  0 inz
      *** End   Add *** 06/21/19 **************************************
     I*** BEGIN ADD *** 11/11/99 **************************************
     IPRP01
     I              RPPGM                       R1PGM
     I              RPDT                        R1DT
     I              RPTIME                      R1TIME
     I*** END ADD ***** 11/11/99 **************************************
     I*** Begin add *** 12/11/03 **************************************
     I              RPTRDT                      R1TRDT
     I              RPSEQ#                      R1SEQ#
     C*
     C     *LIKE         DEFINE    RPDT          XXDT
     C*** BEGIN ADD *** 07/27/99 **************************************
     C     *LIKE         DEFINE    RPCO#         ##CO#
     C     *LIKE         DEFINE    RPFND         ##FND
     C     *LIKE         DEFINE    RPFYR         ##FYR
     C     *LIKE         DEFINE    RPCASE        ##CASE
     C     *LIKE         DEFINE    RPEOB#        ##EOB#
     C     *LIKE         DEFINE    RPCHK#        ##CHK#
     C     *LIKE         DEFINE    RPDT          ##CHDT
     C     *LIKE         DEFINE    RPAMT         ##CHAT
     C*** END   ADD *** 07/27/99 **************************************
      *** BEG ADD *** 06/10/99 ****************************************
     C     *LIKE         DEFINE    RPEOBS        ##RCN
      *** END ADD *** 06/10/99 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCO#             3
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
     C*
     C***  Begin Add  ***  02/26/07  **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  End   Add  ***  02/26/07  **********************************
     C*
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C*** Begin add *** 05/09/01 **************************************
     C                   KFLD                    CPCKTP
     C*** End add ***** 05/09/01 **************************************
     C                   KFLD                    CPFND
     C*
     C*  PAYMENT DETAIL KEY LIST
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPBUL1
     C*** BEGIN ADD *** 07/27/99 **************************************
     C*
     C*  NETWORK REVIEW ID FILE
     C*
     C     NRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C*** END   ADD *** 07/27/99 **************************************
     C*
     C     PAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    RPFYR
     C*
     C*  CHECK PROCESSING FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C*
     C*  REQUEST PAYMENT DETAIL FILE
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPBUL1
     C*
     C*** Begin add *** 02/26/07 **************************************
     C*  Key list for the Recurring Comp Info File.
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPCMSQ
     C*
     C*** End add ***** 02/26/07 **************************************
     C*** BEG ADD ***** 07/27/09 **************************************
     C     F2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C*** END ADD ***** 07/27/09 **************************************
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     CPKEY         CHAIN     PCP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*** BEGIN ADD *** 11/11/99 **************************************
     C*  Only process groups if checks were printed.
     C*
     C     CPSTA0        IFEQ      'Y'
     C*** END ADD ***** 11/11/99 **************************************
     C*
     C*  THE CHECK PROCESSING FLAG MUST BE BLANK
     C*
     C     CPSTA1        IFEQ      ' '
     C*
     C*  START READING ALL PAYMENT RECORDS WHICH HAVE NOT BEEN UPDATED
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*** Begin chg *** 12/11/03 **************************************
     C*********  RPTRDT    IFLE XXDT
     C     R1TRDT        IFLE      XXDT
     C*** End chg ***** 12/11/03 **************************************
     C*** BEGIN ADD *** 11/11/99 **************************************
     C     RPBUL1        ANDNE     0
     C*** END ADD ***** 11/11/99 **************************************
     C*
     C*  WE NEED TO WRITE THE PAYMENT DETAIL RECORD FROM THE
     C*  TRANSACTION
     C*  WE ARE GOING WITH THE LOG NUMBER AS KEY
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '1'
     C*
     C*  WE NEED THE LAST SEQUENCE NUMBER FROM THE FILE FOR THIS CLAIM
     C*
     C     PAKEY1        SETGT     PPA02
     C                   READP     PPA02                                  60
     C     PACO#         IFNE      RPCO#
     C     PAFND         ORNE      RPFND
     C     PAFYR         ORNE      RPFYR
     C     PACASE        ORNE      RPCASE
     C*
     C                   Z-ADD     1             XXSEQ#            5 0
     C*
     C                   ELSE
     C*
     C     PASEQ#        ADD       1             XXSEQ#
     C*
     C                   END
     C*
     C*  MOVE THE TRANSACTION RECORD TO THE PAYMENT DETAIL RECORD
     C*
     C                   MOVE      RPCO#         PACO#
     C                   MOVE      RPFND         PAFND
     C                   MOVE      RPFYR         PAFYR
     C                   MOVE      RPCASE        PACASE
     C                   MOVE      XXSEQ#        PASEQ#
     C                   MOVE      RPCHK#        PACHK#
     C                   MOVE      *ZERO         PABASQ
     C                   MOVE      XXDT          PATRDT
     C                   MOVE      XXDT          PAPODT
     C                   MOVE      RPFEIN        PAFEIN
     C                   MOVE      RPMOD         PAMOD
     C                   MOVE      RPAMT         PAAMT
     C                   MOVE      RPBR2         PABR2
     C                   MOVE      RPBR7         PABR7
     C                   MOVE      RPFRDT        PAFRDT
     C                   MOVE      RPTODT        PATODT
     C                   MOVE      RPWKS         PAWKS
     C                   MOVE      RPDAYS        PADAYS
     C                   MOVE      RPTRCD        PATRCD
     C                   MOVE      RPTYPE        PATYPE
     C                   MOVE      RPRECD        PARECD
     C                   MOVE      RPRETP        PARETP
     C                   MOVE      RPDESC        PADESC
     C                   MOVE      *BLANK        PAADST
     C                   MOVE      RPBUL1        PABUL1
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      RPUSER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
      ***** BEG ADD ***** 12/15/06 ***************************************
      * Retrieve the Client ID
     C                   MOVEL     SRDTO         ZZSYDT            7 0
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVE      *ZEROS        PARQDT
     C     CLTID         IFEQ      '139'
     C                   MOVE      R1TRDT        PARQDT
      ***** BEG DEL ***** 01/30/07 ***************************************
     C**************       MOVELSRDTO     PAPODT
      ***** END DEL ***** 01/30/07 ***************************************
     C                   MOVEL     SRDTO         PATRDT
     C                   ENDIF
      ***** END ADD ***** 12/15/06 ***************************************
     C*
     C                   Z-ADD     DSTIME        PATIME
     C                   MOVE      RPPGTP        PAPGTP
     C                   Z-ADD     RPEOB#        PAEOB#
      *
     C                   MOVEL     RPCKTP        PACKTP
      *******Beg Add********04/17/12**********************************
      /Free
       PAeftc = RPeftc;
      /End-Free
      *******End Add********04/17/12**********************************
     C                   MOVEL     RPACCT        PAACCT
     C                   MOVEL     RPOFF#        PAOFF#
     C                   Z-ADD     RPRCVD        PARCVD
     C                   Z-ADD     RPDUE         PADUE
     C                   MOVEL     RPLTCD        PALTCD
     C                   Z-ADD     *ZERO         PADPCD
     C                   MOVE      *BLANK        PADPCU
     C                   MOVEL     RPRQST        PARQST
     C                   MOVEL     RPFNAL        PAFNAL
     C                   Z-ADD     *ZERO         PARCDT
     C                   MOVE      *BLANK        PARADJ
     C*** BEG ADD *** 03/15/99 ***************************************
     C                   MOVE      RPEOBS        PAEOBS
     C*** END ADD *** 03/15/99 ***************************************
     C*** BEG ADD ***** 03/29/13 *************************************
     C                   Z-ADD     RPDSQ#        PADSQ#
     C*** END ADD ***** 03/29/13 *************************************
      *
     C*** BEG ADD ***** 07/27/09 **************************************
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C     F2CTYP        IFEQ      'X'
     C                   MOVE      'X'           PASTAT
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 07/27/09 **************************************
     C*** BEG ADD ***** 01/05/11 **************************************
     C                   MOVEL     RPINV#        PAINV#
     C*** END ADD ***** 01/05/11 **************************************
     C*** BEGIN ADD *** 08/28/19 *****************************************
     C                   MOVE      RPLMPS        PALMPS
     C*** END   ADD *** 08/28/19 *****************************************
      *
     C*** BEGIN ADD *** 08/29/11 *****************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** END ADD *** 08/29/11 *******************************************
     C                   WRITE     PPA01
      /Free
       //*** Begin Add *** 06/21/19 ******************************************
        If isModEnabled('IDC') and PAretp = 'C';    //Write Idc Trans Log Rec
           intRRN = dbPPArrn;
           WriteTxLog(intRRN);
        Endif;
       //*** End   Add *** 06/21/19 ******************************************
      /End-Free
     C*** BEG ADD *** 12/11/03 *****************************
     C*
     C*  Load the View Check detail file (WDRPP)
     C*
     C********   RPKY1     CHAINPRP02                60
     C********   *IN60     IFEQ '1'
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      XXDT          RPDT
     C                   MOVE      XXDT          RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
      *
      ***** BEG ADD ***** 12/15/06 ***************************************
      * If Client 139, load the system date into WDRPP/RPTRDT
     C     CLTID         IFEQ      '139'
     C                   Z-ADD     ZZSYDT        RPTRDT
     C                   ENDIF
      ***** END ADD ***** 12/15/06 ***************************************
     C*
     C                   Z-ADD     DSTIME        RPTIME
     C                   WRITE     PRP02
     C********             END
     C*
     C*** END ADD *** 12/11/03 *****************************
     C                   END
     C*** Begin add *** 02/26/07 **************************************
     C*  Update the 'last pmt thru date' in the Recurring Comp file.
     C*
     C     CLTID         IFEQ      '114'
     C*** BEGIN ADD *** 06/15/07 **************************************
     C     XOPCRL        IFNE      '1'
     C                   OPEN      WMCRL
     C                   MOVE      '1'           XOPCRL            1
     C                   ENDIF
     C*** END   ADD *** 06/15/07 **************************************
     C     CRKY          CHAIN     PCR01                              99
     C     *IN99         IFEQ      *OFF
     C     RPTODT        ANDGT     CRLPDT
     C*
     C                   Z-ADD     RPTODT        CRLPDT
     C     CRENDT        IFNE      0
     C     CRENDT        ANDEQ     CRLPDT
     C                   MOVE      'I'           CRSTAT
     C                   ENDIF
     C*
     C                   UPDATE    PCR01
     C*
     C                   ENDIF
     C                   ENDIF
     C*** End add *** 02/26/07 ****************************************
     C*
     C*  GET THE CLAIM MASTER FILE, WE NEED TO UPDATE THE RESERVES
     C*
     C     CMKEY         CHAIN     PCM01                              61
     C     *IN61         IFEQ      '0'
     C*
     C*  CHECK TO MAKE SURE THE BATCH NUMBER IS NOT THE SAME
     C*
     C     CMBUL1        IFNE      RPBUL1
     C*
     C*  WE NEED TO TAKE RESERVE CODE FROM THE REQUEST PAYMENT
     C*  AND FIND THE APPLICABLE RESERVE CODE IN THE CLAIM FILE.
     C*  APPLY THE PAYMENT.
     C*
     C                   Z-ADD     1             X1                5 0
     C     RPRECD        LOOKUP    CD(X1)                                 60
     C     *IN60         IFEQ      '1'
     C*
     C*  ADD TO THE CLAIMS PAYMENTS
     C*
     C     PA(X1)        ADD       RPAMT         PA(X1)
     C*
     C*  REDUCE THE REMAINING RESERVE
     C*
     C     RR(X1)        SUB       RPAMT         RR(X1)
     C*
     C*
     C*  INCREASE THE PAYMENT COUNTER IN THE CLAIM
     C*
     C                   ADD       1             CMPCTR
     C*
     C*  UPDATE THE LOGGING INFORMATION
     C*
     C                   MOVE      RPUSER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   Z-ADD     DSTIME        CMTIME
     C                   MOVE      RPBUL1        CMBUL1
     C*
     C                   UPDATE    PCM01
     C*
     C                   END
     C                   END
     C                   END
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE TOTAL PAYMENTS
     C*  BUILD THE CORRECT KEY
     C*
     C     *IN61         IFEQ      '0'
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C     ELBUL1        IFNE      RPBUL1
     C*
     C*  ADD TO THE TOTAL CLAIMS PAID
     C*
     C                   ADD       RPAMT         ELTCLP
     C*
     C*  REDUCE THE OUTSTANDING RESERVES
     C*
     C                   SUB       RPAMT         ELOUTS
     C*  DO THE UPDATE INFO
     C*
     C                   MOVE      RPUSER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   Z-ADD     DSTIME        ELTIME
     C                   MOVE      RPBUL1        ELBUL1
     C*
     C                   UPDATE    PEL01
     C*
     C                   END
     C                   END
     C                   END
     C*** BEG DEL *** 12/11/03 *****************************
     C**********
     C***Load*the View Check detail file (WDRPP)
     C**********
     C********** RPKY1     CHAINPRP02                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE Q1PGM     RPPGM
     C**********           TIME           DSDTTM
     C**********
     C****BEGIN*DLT *** 04/22/98 ***********************************************
     C******************** Z-ADDDSDATE    SRDTI
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     RPDT
     C****END*DLT ***** 04/22/98 ***********************************************
     C****BEGIN*ADD *** 04/22/98 ***********************************************
     C**********           MOVE XXDT      RPDT
     C****END*ADD ***** 04/22/98 ***********************************************
     C****BEGIN*ADD *** 05/01/00 **************************************
     C**********           MOVE XXDT      R2TRDT
     C****END*ADD ***** 05/01/00 **************************************
     C****Begin*add *** 05/15/03 **************************************
     C**********           Z-ADDPASEQ#    R2SEQ#
     C****End*add ***** 05/15/03 **************************************
     C**********
     C**********           Z-ADDDSTIME    RPTIME
     C**********           WRITEPRP02
     C**********           END
     C*** END DEL *** 12/11/03 *****************************
     C*
     C*  UPDATE THE TRANSACTION AS PROCESSED
     C*
     C                   MOVE      'Y'           RPSTAT
      ***** BEG ADD ******* 05/13/10 **********************************
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*
     C                   MOVE      DSTIME        RPTIME
      ***** END ADD ******* 05/13/10 **********************************
     C                   UPDATE    PRP01
     C*** BEGIN ADD *** 12/03/98 *************************************
     C*
     C*  CALL SA001R TO SEE IF PROVIDER BILLS IS INSTALLED
     C*
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1
02   C                   PARM      *BLANKS       ##MCIF            1
03   C                   PARM      *BLANKS       ##WCIN            1
04   C                   PARM      *BLANKS       ##AR              1
05   C                   PARM      *BLANKS       ##NTWK            1
06   C                   PARM      *BLANKS       ##UR              1
07   C                   PARM      *BLANKS       ##MED             1
08   C                   PARM      *BLANKS       ##EDI             1
09   C                   PARM      *BLANKS       ##WCDK            1
10   C                   PARM      *BLANKS       ##RHAB            1
11   C                   PARM      *BLANKS       ##MSS             1
12   C                   PARM      *BLANKS       ##FSCH            1
13   C                   PARM      *BLANKS       ##PRVB            1
     C*
     C*  CALL THE SUBROUTINE THAT WILL FIND WHAT TYPE OF CHECK
     C*  IS BEING WRITTEN, AND CALL THE UPDATE PROGRAMS
     C*
     C     RPEOB#        IFNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C*
     C                   EXSR      CHKTYP
     C*
     C                   ENDIF
     C*** END   ADD *** 12/03/98 *************************************
     C*
     C                   END
     C     RPKEY         READE     PRP01                                  60
     C                   END
     C*
     C                   END
     C*
     C*  UPDATE THIS RECORD AS PROCESSED
     C*
     C                   MOVE      'Y'           CPSTA1
     C                   UPDATE    PCP01
     C*** BEGIN ADD *** 11/11/99 **************************************
     C                   ENDIF
     C*** END ADD ***** 11/11/99 **************************************
     C*
     C     CPKEY         READE     PCP01                                  60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD *** 07/27/99 *************************************
     C*****************************************************************
     C*  SUBROUTINE CHKTYP WILL FIND OUT WHAT TYPE OF CHECK IS BEING
     C*  WRITTEN AND THEN CALL F7304R TO UPDATE FAEOP AND FAE1P.
     C*****************************************************************
     C     CHKTYP        BEGSR
     C*
     C                   MOVE      'P'           ##TYPE
     C*
     C     NRKY          CHAIN     PNR01                              99
     C*
     C     *IN99         IFEQ      *OFF
     c*** begin add *** 08/21/07 **************************************
     C     NRSTAT        ANDNE     'I'
     c*** end   add *** 08/21/07 **************************************
     C*
     C     NRGRTY        IFEQ      'O'
     C     NRGRTY        OREQ      ' '
     C*
     C                   MOVE      'P'           ##TYPE            1
     C*
     C                   ELSE
     C*
     C                   MOVE      NRGRTY        ##TYPE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 06/15/07 **************************************
     C     CLTID         IFEQ      '139'
     C                   Z-ADD     PATRDT        ##CHDT
     C                   ELSE
     C                   Z-ADD     XXDT          ##CHDT
     C                   ENDIF
     C*** END   ADD *** 06/15/07 **************************************
     C*
     C                   CALL      'F7304R'
     C                   PARM      RPCO#         ##CO#
     C                   PARM      RPFND         ##FND
     C                   PARM      RPFYR         ##FYR
     C                   PARM      RPCASE        ##CASE
     C                   PARM      RPEOB#        ##EOB#
     C                   PARM      RPCHK#        ##CHK#
     C*** BEGIN CHG *** 06/15/07 **************************************
     C**********           PARM XXDT      ##CHDT
     C                   PARM                    ##CHDT
     C*** END   CHG *** 06/15/07 **************************************
     C                   PARM      RPAMT         ##CHAT
     C                   PARM                    ##TYPE
      *** BEG ADD *** 06/10/99 ****************************************
     C                   PARM      RPEOBS        ##RCN
      *** END ADD *** 06/10/99 ****************************************
     C*** BEGIN ADD *** 03/02/00 **************************************
     C                   PARM      RPNTWK        ##NTWK            1
     C*** END   ADD *** 03/02/00 **************************************
     C*
     C                   ENDSR
     C*** END ADD *** 12/03/98 ***************************************
