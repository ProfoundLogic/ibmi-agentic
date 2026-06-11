     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8070R3 - LOG API
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 07/20/23 C:053 Support 111709
     H*                Suppress the PY for TN on the 1st med pmt for CLT146
     H*
     H* 10/03/23 C:053 SUPPORT PHW113810
     H*                No IP reporting for NJ.
     H*
     H* 02/20/20 C:180 Support 87315
     H*                Update comp rate based on AWW grid.
     H*
     H* 04/24/19 C:180 Project WC2669
     H*                Add A49 start date override
     H*
     H* 11/01/18 C:079 Support: 90/74340
     H*                Use %Found when checking WMIXP Chain results
     H*
     H* 08/11/15 C:180 SUPPORT PHW35741
     H*                Fix IP reporting incorrect BTCs.
     H*
     H* 06/30/14 C:180 SUPPORT WC146 86012
     H*                Check for deactivated A49
     H*
     H* 06/03/14 C:180 SUPPORT WC146 85708
     H*                Supress denials for NC
     H*
     H* 05/02/13 C:180 SUPPORT WC146 82252
     H*                Implement EDI start date for A49.
     H*
     H* 01/04/11 C:180 SUPPORT 75348
     H*                FIX AP LOGIC.
     H*
     H* 04/23/10 C:180 SUPPORT 72297
     H*                FIX PY CREATION CRITERIA FOR STATE OF TN.
     H*                                                               *
     H* 04/05/10 C:189 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                  PROJECT RE-JAD , RE-DESIGNED                 *
     H*                                                               *
     H*                                                               *
     H* 03/24/10 C:232 PROJECT: 139WNN                                *
     H*                MODIFIED FOR EDI EXTRACT AND FTP AUTOMATION.   *
     H*                                                               *
     H* 04/06/09 C:180 PRISS:  WC1589
     H*                EDI log record to be written only when
     H*                MTC is valid.
     H*
     H* 03/27/09 C:180 SUPPORT: 67753
     H*                Change code dated 01/31/08 to omit writing of
     H*                EDI log record when payment code is 500.
     H*
     H* 10/15/08 C:180 SUPPORT: 65486
     H*                Fix error with payment issue date.
     H*                                                               *
     H* 10/08/08 C:180 Support 64328
     H*                Prevent writing dup PY rec for state of TN. Only
     H*                one PY rec should exist.
     H*
     H* 09/29/08 C:180 Support: 64714
     H*                Create A49 IP record when payment is for
     H*                claimant only. Applicable to rls 1 only.
     H*
     H* 07/02/08 C:183 Project: 135WLG                                *
     H*                If a RAT record exist for the CMJRST being     *
     H*                processed, bypass the processing for the       *
     H*                particular payment and read the next payment.  *
     H*                                                               *
     H* 06/30/08 C:180 Support: 60207
     H*                Place A49 trans on HOLD when state is TN       *
     H*                and 1st med payment is processed.
     H*
     H* 01/31/08 C:53  Project: WC1584                                *
     H*                Accomdate additional MTC Codes                 *
     H*                                                               *
     H* 10/19/07 C:191 Project: WC1584                                *
     H*                EDI Release 3:Update LGMTCD for specific state *
     H*                                                               *
     H* 10/17/07 C:15  Support: 60207                                 *
     H*                Removed the requirement of CMGT1 = 'Y' for the *
     H*                TN A49-PY.                                     *
     H*                                                               *
     H* 05/01/07 C:191 Support: 57704                                 *
     H*                Correct code to update the WMLGP with status   *
     H*                'EC'                                           *
     H*                                                               *
     H* 02/26/07 C:53 Support: 56484                                  *
     H*               Correct code to write the "PY" log record and   *
     H*               suppress the issueance of a duplicate.          *
     H*                                                               *
     H* 03/15/05 C:15  Project: 114WJE                                *
     H*                Added code to check the EDI flag at the group  *
     H*                level to determine if EDI should be processed. *
     H*                                                               *
     H* 12/14/04 C:15 Project: WC468                                  *
     H*               Added code to create a 'PY' log record for the  *
     H*               first medical pmt, if CMJRST = 'TN'.            *
     H*                                                               *
     H* 08/19/03 C:15 Project: WC1137                                 *
     H*               Added code to load the check mailing date into  *
     H*               LGMTCD for IP transactions.                     *
     H*                                                               *
     H* 02/17/03 C:53  Support: WC082/35923                           *
     H*                Initialize LGBLFL so data from a previous      *
     H*                chain.                                         *
     H*                                                               *
     H* 08/28/02 C:15  Support: WC025/31966                           *
     H*                Do not create a record if an 'IP' already      *
     H*                exists.                                        *
     H*                                                               *
     H* 05/03/02 C:53  Support: WC025/31966                           *
     H*                Check for "SN" records to prevent the write    *
     H*                of duplicate "IP" records.                     *
     H*                                                               *
     H* 03/22/02 C:15  Support: WC025/30451                           *
     H*                Per Rick Honeycutt, load the pmt transaction   *
     H*                date into the MTC date.                        *
     H*                                                               *
     H* 01/07/02 C:15  Support: WC065/30451                           *
     H*               -Ignore pmts that do not have an PV1 EDI pmt    *
     H*                code.                                          *
     H*               -If the claim is acquired, load 'AP' to the     *
     H*                MTC code.                                      *
     H*                                                               *
     H* 10/12/00 C:15  Project: WC468                                 *
     H*                Added code to prevent A49 log records from     *
     H*                being created if the A49 control record does   *
     H*                not exist.                                     *
     H*                                                               *
     H* 09/22/00 C:15  Project: WC468                                 *
     H*                Added code to load a unique seq# into LGDSQH.  *
     H*                                                               *
     H* 07/20/00 C:15 Project: WC468                                  *
     H*               Added code to load the correct due date.        *
     H*                                                               *
     H*  04/26/00 C:61  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 04/28/99 C:15 Project: WC468                                  *
     H*               Changed pgm to process claims with an initial   *
     H*               entry date prior to the EDI start date.  The    *
     H*               pgm will assume that the FROI has already been  *
     H*               sent.                                           *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H* 11/04/97  C:33  PROJECT: WC577                                *
     H*                 SRDAY8 FIRST PARM MUST BE NUMERIC             *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*****************************************************************
     H*
     FWDPAL9    IF   E           K DISK
     F                                     RENAME(PPA01:PPA09)
     FWDPAL5    IF   E           K DISK
     F                                     RENAME(PPA01:PPA05)
     FWMCML1    IF   E           K DISK
     FWMIXL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FWMLGL     UF A E           K DISK
      *** BEGIN ADD **** 10/08/08 *************************************
     FWMLGL4    IF   E           K DISK
     F                                     RENAME(PLG01:PLG04)
      *** END ADD ****** 10/08/08 *************************************
     FSTCTL1    IF   E           K DISK
     F                                     RENAME(PCT01:PCT03)
     F*** BEGIN DEL *** 04/26/00 **************************************
     F*******SDCML   O   E           K        DISK
     F*******            PCM01                             KRENAMEPCM02
     F*******SDCMLA  IF  E           K        DISK
     F*******            PCM01                             KRENAMEPCM03
     F*** END DEL *** 04/26/00 ****************************************
     F*** Begin add *** 01/07/02 **************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F*** End add ***** 01/07/02 **************************************
     F*** Begin add *** 08/19/03 **************************************
     FWACPL     IF   E           K DISK
     F*** End add ***** 08/19/03 **************************************
     F*** Begin add *** 03/15/05 **************************************
     FWMFNL     IF   E           K DISK
     F*** End add ***** 03/15/05 **************************************
     F*** BEG ADD ****** 10/19/07 **********************************************
      *** BEGIN DELETE **** 10/15/08 **********************************
     F****WMNIL   IF  E           K        DISK
      *** END DELETE ****** 10/15/08 **********************************
     F*** END ADD ****** 10/19/07 **********************************************
     F*
     F*** BEGIN ADD *** 04/06/09 **************************************
     FWTGIL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
      *** Beg Add *** 02/20/20 ***********************************
     FWMAWL3    UF   E           K DISK
      *** End Add *** 02/20/20 ***********************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QCCOM2                 1     80
     D  QCDRT3                 1     10
     D  QCUSER                12     61
     D***** BEG ADD ***** 04/05/10 **************************************
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     F*** END   ADD *** 04/06/09 **************************************

     IPCT03
     I              CTDESC                      C3DESC
     I*
     I*** BEGIN DEL *** 04/26/00 **************************************
     I**********PCM03
     I**********    CMKEY                           C3KEY
     I**********    CMCSID                          C3CSID
     I**********    CMCOMM                          C3COMM
     I**********    CMCDAT                          C3CDAT
     I**********    CMCODE                          C3CODE
     I**********    CMDCID                          C3DCID
     I**********    CMPNAM                          C3PNAM
     I**********    CMTRDT                          C3TRDT
     I**********    CMTRTM                          C3TRTM
     I**********    CMSEQ#                          C3SEQ#
     I**********    CMSECL                          C3SECL
     I**********    CMSBYL                          C3SBYL
     I**********    CMSTAT                          C3STAT
     I**********    CMUSER                          C3USER
     I**********    CMPGM                           C3PGM
     I**********    CMTIME                          C3TIME
     I**********    CMDT                            C3DT
     I**********    CMIUSR                          C3IUSR
     I**********    CMFOLD                          C3FOLD
     I**********    CMDCDT                          C3DCDT
     I**********    CMDIFL                          C3DIFL
     I**********    CMAPID                          C3APID
     I**********    CMIDOC                          C3IDOC
     I**********    CMIQUE                          C3IQUE
     I**********    CMIMGF                          C3IMGF
     I**********    CMIPTH                          C3IPTH
     I**********    CMPIDX                          C3PIDX
     I**********    CMQPTY                          C3QPTY
     I**********    CMDCTP                          C3DCTP
     I**********    CMIPGM                          C3IPGM
     I**********    CMSMCD                          C3SMCD
     I**********    CMFL01                          C3FL01
     I**********    CMFL02                          C3FL02
     I**********    CMFL03                          C3FL03
     I**********    CMFL04                          C3FL04
     I**********    CMFL05                          C3FL05
     I**********    CMRMCD                          C3RMCD
     I**********    CMIDT                           C3IDT
     I*** END DEL *** 04/26/00 ****************************************
     IPPA05
     I              PACO#                       P5CO#
     I              PAFND                       P5FND
     I              PAFYR                       P5FYR
     I              PACASE                      P5CASE
     I              PASEQ#                      P5SEQ#
     I              PACHK#                      P5CHK#
     I              PABASQ                      P5BASQ
     I              PATRDT                      P5TRDT
     I              PAPODT                      P5PODT
     I              PAFEIN                      P5FEIN
     I              PAMOD                       P5MOD
     I              PAAMT                       P5AMT
     I              PABR2                       P5BR2
     I              PABR7                       P5BR7
     I              PAFRDT                      P5FRDT
     I              PATODT                      P5TODT
     I              PAWKS                       P5WKS
     I              PADAYS                      P5DAYS
     I              PATRCD                      P5TRCD
     I              PATYPE                      P5TYPE
     I              PARECD                      P5RECD
     I              PARETP                      P5RETP
     I              PADESC                      P5DESC
     I              PAADST                      P5ADST
     I              PASTAT                      P5STAT
     I              PAUSER                      P5USER
     I              PAPGM                       P5PGM
     I              PATIME                      P5TIME
     I              PADT                        P5DT
     I              PABUL1                      P5BUL1
     I              PAPGTP                      P5PGTP
     I              PAEOB#                      P5EOB#
     I***** END ADD ***** 04/05/10 **************************************

     C*** Beg add *** 10/03/23 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** End add *** 10/03/23 **************************************
     C*
     C*** Read the Payments for the current batch date and select those
     C*** payments that are the 1st comp pmt on a claim.
     C*
     C     PAKY9         CHAIN     PPA09                              11
     C     *IN11         DOWEQ     *OFF
     C*
     C*** Begin add *** 03/15/05 **************************************
     C     PAFND         IFNE      SVFND
     C     FNKY          CHAIN     PFN01                              99
     C                   Z-ADD     FNFND         SVFND
     C*** BEG ADD ****** 03/24/10 *************************************
     C                   MOVEL     FNEDIF        XXEDIF
     C*** END ADD ****** 03/24/10 *************************************
     C                   ENDIF
     C     FNEDI         CABEQ     'Y'           NEXT
     C*** End add ***** 03/15/05 **************************************
     C*
     C*** Begin add *** 01/07/02 **************************************
     C*  Only process comp pmts.
     C*
     C*** Begin dlt *** 12/14/04 **************************************
     C********** PARETP    CABNE'C'       NEXT
     C*** End dlt ***** 12/14/04 **************************************
     C*
     C*** Begin add *** 12/14/04 **************************************
     C     PARETP        IFNE      'C'
     C     PARETP        ANDNE     'M'
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C*  If a med pmt, determine if a 'PY' should be created.
     C*
     C     PARETP        IFEQ      'M'
     C*** Begin add *** 07/20/23 **************************************
     C     CLTID         ANDNE     '146'
     C*** End   add *** 07/20/23 **************************************
     C*
     C                   MOVEL(P)  PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     B2PYCD        ANDNE     *BLANKS
     C                   EXSR      MEDPMT
     C                   ENDIF
     C*
     C                   ELSE
     C*** End add ***** 12/14/04 **************************************
     C*
     C*
     C*  Ignore pmts that will not be included in the A49 extract.
     C*
     C                   MOVEL(P)  PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     B2PYCD        ANDEQ     *BLANKS
     C                   GOTO      NEXT
     C                   ENDIF
     C*** End add ***** 01/07/02 **************************************
     C*
     C***        Select if 1st comp pmt on claim.
     C*
     C                   MOVE      *OFF          XXSEL
     C                   MOVE      PAFND         XXFND
     C                   MOVE      PAFYR         XXFYR
     C                   MOVE      PACASE        XXCASE
     C     PAKY5         CHAIN     PPA05                              12
     C     *IN12         DOWEQ     *OFF
     C*
     C*** Begin add *** 01/07/02 **************************************
     C*  Ignore pmts made by other TPAs.
     C*
     C                   MOVEL(P)  P5BR2         XXKEY
     C     TBKY          CHAIN     PB201                              99
     C   99              MOVE      *BLANKS       B2PYCD
     C*** End add ***** 01/07/02 **************************************
     C*
     C     P5RETP        IFEQ      'C'
     C*** Begin add *** 01/07/02 **************************************
     C     B2PYCD        ANDNE     *BLANKS
     C*** End add ***** 01/07/02 **************************************
     C     PASEQ#        IFEQ      P5SEQ#
     C                   MOVE      *ON           XXSEL
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C*
     C     PAKY5         READE     PPA05                                  12
     C                   ENDDO
     C*
     C     XXSEL         IFEQ      *ON
     C*
     C***        Retrieve the Claim Master.
     C*
     C                   EXSR      SRCM
     C*
     C***        Check for 148 log record.
     C*
     C                   EXSR      SRRLOG
     C*
     C***        If the 148 log record is not found, run the API to
     C***        write it.
     C*
     C     XXFROI        IFEQ      *OFF
     C                   EXSR      SRW148
     C                   ENDIF
     C*
     C***        Determine the status of the A49 Log Record.
     C*
     C                   EXSR      SRSTS
     C*
     C***        If the status is not 'COMPLETE', write diary remarks.
     C*
     C     XXSTS         IFNE      'EC'
     C                   EXSR      SRDDRY                                       Diary Items
     C                   ENDIF
     C*
     C***        Write the A49 log record.
     C*
     C     XXERR         IFEQ      *OFF
     C                   EXSR      SRUPDT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** Begin add *** 12/14/04 **************************************
     C                   ENDIF
     C*** End add ***** 12/14/04 **************************************
     C*** BEGIN ADD *** 04/28/99 **************************************
     C     NEXT          TAG
     C*** END ADD ***** 04/28/99 **************************************
     C     PAKY9         READE     PPA09                                  11
     C                   ENDDO
     C*
     C*** End the program.
     C*
     C                   MOVE      '1'           *INLR
     C/EJECT
     C*****************************************************************
     C*  *INZSR - Initialization Routine.
     C*****************************************************************
     C*
     C     *INZSR        BEGSR
     C*
     C*** Define Parm Lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3            Company
     C                   PARM                    QQTRDT            8            Trans Dt
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
     C*** Define Key Lists
     C*
     C***        WDPAL9 - Payments by Transaction Date
     C*
     C     PAKY9         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTRDT            7 0
     C*
     C***        WDPAL5 - Payments by Case
     C*
     C     PAKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C***        WMCML - Claim Master
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C***        WMLGL - EDI Log File
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      ****** BEGIN ADD ********** 10/08/08 ****************************
     C     LGKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTSET
     C                   KFLD                    XXMTC
      ****** END ADD   ********** 10/08/08 ****************************
     C*
     C***        WMIXL - EDI Claims Control Maintenance File
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTSET
     C*** BEG ADD ****** 03/24/10 *************************************
     C                   KFLD                    XXEDIF
     C*** END ADD ****** 03/24/10 *************************************
     C*
     C*** Begin add *** 01/07/02 **************************************
     C*          WTABL - WCMS Table File
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*** End add ***** 01/07/02 **************************************
     C*
     C*** Begin add *** 08/19/03 **************************************
     C*  WACPL - Check Schedule Maintenance File
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTRDT
     C                   KFLD                    XXFND
     C*** End add ***** 08/19/03 **************************************
     C*** Begin add *** 03/15/05 **************************************
     C*  WMFNL - Group Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C*** End add ***** 03/15/05 **************************************
     C*** BEG ADD ****** 10/19/07 **********************************************
      *** BEGIN DELETE **** 10/15/08 **********************************
     C****       NIKY      KLIST
     C****                 KFLD           XXCO#
     C****                 KFLD           XXFND
     C****                 KFLD           XXFYR
     C****                 KFLD           XXCASE
      *** END DELETE ****** 10/15/08 **********************************
     C*** END ADD ****** 10/19/07 **********************************************
     C*** BEGIN ADD **** 04/06/09 **********************************************
     C     GIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
      *
     C     XTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXTKEY
     C*** END ADD ****** 04/06/09 **********************************************
     C*** BEG ADD ****** 04/05/10 **********************************************
     C*
     C     IXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGJST
     C                   KFLD                    LGTSET
     C*** END ADD ****** 04/05/10 **********************************************
     C*
     C*** Define Work Fields
     C*
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PAFND         XXFND
     C     *LIKE         DEFINE    PAFYR         XXFYR
     C     *LIKE         DEFINE    PACASE        XXCASE
     C     *LIKE         DEFINE    *IN11         XXFROI
     C     *LIKE         DEFINE    *IN11         XXSEL
     C     *LIKE         DEFINE    *IN11         XXDNY
     C     *LIKE         DEFINE    *IN11         XXERR
     C     *LIKE         DEFINE    LGMTC         XXMTC
     C     *LIKE         DEFINE    LGMTC         SVMTC
     C     *LIKE         DEFINE    LGSTS         SVSTS
     C     *LIKE         DEFINE    LGDSQG        XXDSQG
     C     *LIKE         DEFINE    LGDSQH        XXDSQH
     C     *LIKE         DEFINE    LGDSQI        XXDSQI
     C     *LIKE         DEFINE    LGTSET        XXTSET
      *** BEG DEL ***** 04/26/00 **************************************
     C********   *LIKE     DEFN CMKEY     XCKEY
      *** END DEL ***** 04/26/00 **************************************
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C*** BEGIN ADD *** 04/28/99 **************************************
     C     *LIKE         DEFINE    IXCSDT        SVCSDT
     C*** END ADD ***** 04/28/99 **************************************
     C*** Begin add *** 01/07/02 **************************************
     C     *LIKE         DEFINE    B2KEY         XXKEY
     C*** End add ***** 01/07/02 **************************************
     C*** Begin add *** 03/15/05 **************************************
     C     *LIKE         DEFINE    FNFND         SVFND
     C*** End add ***** 03/15/05 **************************************
     C*** BEG ADD ****** 03/24/10 *************************************
     C     *LIKE         DEFINE    IXEDIF        XXEDIF
     C*** END ADD ****** 03/24/10 *************************************
     C*** BEGIN ADD *** 04/24/19 **************************************
     C     *LIKE         DEFINE    IXA49D        SVA49D
     C*** END ADD ***** 04/24/19 **************************************
     C*
     C                   MOVE      *BLANKS       XCCASE            7
     C                   MOVE      *BLANKS       XCFND             3
     C                   MOVE      *BLANKS       XCFYR             3
     C                   MOVE      *BLANKS       XXSTS             2
     C                   MOVE      *ZEROS        XXTRDT
     C                   MOVE      *ZEROS        XXDAT8            8 0
     C                   MOVE      *ZEROS        XI                5 0
     C                   MOVE      *OFF          XXFROI
     C                   MOVE      *OFF          XXSEL
     C                   MOVE      *OFF          XXDNY
     C                   MOVE      *OFF          XXERR
     C                   MOVE      *OFF          XXMTC
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQTRDT        XXDAT8
     C                   Z-ADD     XXDAT8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C                   TIME                    DSDTTM
     C*** BEGIN ADD *** 07/20/00 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*** END ADD ***** 07/20/00 **************************************
     C***** BEG ADD ****** 04/06/09 ***********************************
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXTKEY
     C***** END ADD ****** 04/06/09 ***********************************
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************
     C*  SRCM - Process Claims Master and EDI Auxillary.
     C*****************************************************************
     C*
     C     SRCM          BEGSR
     C*
     C*** Retrieve Claim Master
     C*
     C     CMKY          CHAIN     PCM01                              13
     C*
     C                   MOVE      *OFF          XXERR
     C     *IN13         IFEQ      *ON
     C                   MOVE      *ON           XXERR
     C                   MOVE      *BLANKS       CMDNY
     C                   MOVE      *BLANKS       CMJRST
     C                   ENDIF
     C*
     C                   MOVE      *OFF          XXDNY
      /Free
       // *** Begin Change *** 06/03/14 *******************************
       // C**     CMDNY         IFEQ      'Y'                                          Denial Flag
       // C**                   MOVE      *ON           XXDNY
       // C**                   ENDIF
          If CMDNY = 'Y' and CMJRST <> 'NC';
             XXDNY = *ON;
          Endif;
       // *** End   Change *** 06/03/14 *******************************
      /End-Free
     C*
     C*** Retrieve the EDI Claim Control Maintenance Record.
     C*
     C                   MOVE      *BLANKS       XXJRST
     C                   MOVEL     CMJRST        XXJRST
     C*** BEG ADD ***** 07/02/08 **************************************
     C                   MOVE      'RAT'         XXTSET
     C     IXKY          CHAIN     PIX01                              60
     C  N60              GOTO      NEXT
     C*** END ADD ***** 07/02/08 **************************************
      *
     C*** BEGIN ADD *** 04/28/99 **************************************
     C                   MOVE      *BLANKS       XXTSET
     C     IXKY          CHAIN     PIX01                              60
     C   60              GOTO      NEXT
     C                   MOVE      IXCSDT        SVCSDT
     C*** END ADD ***** 04/28/99 **************************************
     C*** BEGIN DELETE ****** 04/23/10 ********************************
     C*** BEGIN ADD *** 09/29/08 **************************************
     C***        IXRLS     IFEQ '1'
     C***                  MOVE PAFEIN    FEIN9A  9
     C***        FEIN9A    IFEQ CMSS#
     C***        PAFEIN    OREQ *ZEROS
     C***                  ELSE
     C***                  GOTO NEXT
     C***                  ENDIF
     C***                  ENDIF
     C*** END ADD   *** 09/29/08 **************************************
     C*** END DELETE   ****** 04/23/10 ********************************
     C*** BEGIN DELETE   ****** 06/30/14 ********************************
     C***                   MOVE      'A49'         XXTSET
     C***     IXKY          CHAIN     PIX01                              60
     C***     *IN60         IFEQ      *ON
     C***                   CLEAR                   PIX01
     C*** BEGIN ADD *** 10/12/00 **************************************
     C***   60              GOTO      NEXT
     C*** END ADD ***** 10/12/00 **************************************
     C***                   ENDIF
     C*** END DELETE   ****** 06/30/14 ********************************
     C***
     C************** Begin Add 06/30/14 *******************************
      /Free
       XXTSET = 'A49';
       Chain IXKY PIX01;
       //*** Beg Chg *** 11/01/18 *************************************
       //**** If *in60 or (Not *in60 and IXSTAT = 'I');
       If not %Found(wmixl) or (%Found(wmixl) and ixStat = 'I');
       //*** End Chg *** 11/01/18 *************************************
         CLEAR PIX01;
         *IN60 = *ON;
       Endif;
      //*** BEGIN ADD *** 04/24/19 **************************************
       If %Found(wmixl);
          SVA49D = IXA49D;
       Endif;
      //*** END   ADD *** 04/24/19 **************************************
      *
      /End-Free
     C   60              GOTO      NEXT
     C************** End Add   06/30/14 *******************************
     C*** Beg add *** 10/03/23 **************************************
     C     CLTID         IFEQ      '114'
     C     XXJRST        IFEQ      'NJ'
     C                   GOTO      NEXT
     C                   ENDIF
     C                   ENDIF
     C*** End add *** 10/03/23 **************************************
     C                   ENDSR
     C/EJECT
     C*****************************************************************
     C*  SRRLOG - Retrieve the 148 Log Record.
     C*****************************************************************
     C*
     C     SRRLOG        BEGSR
     C*
     C*** Determine if the 148 log record exists.                      .
     C*
     C                   MOVE      *OFF          XXFROI
     C     LGKY          CHAIN(N)  PLG01                              13
     C     *IN13         DOWEQ     *OFF
     C*
     C     LGTSET        IFEQ      '148'
     C                   MOVE      *ON           XXFROI
     C                   LEAVE
     C                   ENDIF
     C*
     C     LGKY          READE(N)  PLG01                                  13
     C                   ENDDO
     C*


     C*** BEGIN ADD *** 04/24/19 **************************************
     C     SVA49D        IFGT      *ZERO
     C     CMIDT         IFLT      SVA49D
     C                   MOVE      *ON           XXFROI
     C                   ENDIF

     C                   ELSE
     C*** END ADD ***** 04/24/19 **************************************

     C*** BEGIN ADD *** 04/28/99 **************************************
     C     CMIDT         IFLT      SVCSDT
     C                   MOVE      *ON           XXFROI
     C                   ENDIF
     C*** END ADD ***** 04/28/99 **************************************
     C*
     C*** BEGIN ADD *** 04/24/19 **************************************
     C                   ENDIF
     C*** END ADD ***** 04/24/19 **************************************
     C                   ENDSR
     C/EJECT
     C*****************************************************************
     C*  SRW148 - Call the 148 Log API.
     C*****************************************************************
     C*
     C     SRW148        BEGSR
     C*
     C*** Initialize the parms and call.                               .
     C*
     C                   MOVE      PACO#         QQCO#             3
     C                   MOVE      PAFND         QQFND             3
     C                   MOVE      PAFYR         QQFYR             3
     C                   MOVE      PACASE        QQCASE            7
     C                   MOVE      'B'           QQACTF            1
     C                   MOVE      *BLANKS       QQMGID            7
     C                   MOVE      *BLANKS       QQRTCD            1
     C*
     C                   CALL      'W0070R6'
     C                   PARM                    QQCO#             3            COMPANY
     C                   PARM                    QQFND             3            FUND ID
     C                   PARM                    QQFYR             3            FUND YR
     C                   PARM                    QQCASE            7            CASE/CLAIM
     C                   PARM                    QQACTF            1            ACTION FLAG
     C                   PARM                    QQMGID            7            MSG ID
     C                   PARM                    QQRTCD            1
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*****************************************************************
     C*  SRSTS - Check all mandatory fields to determine the status
     C*          of the log record.
     C*****************************************************************
     C*
     C     SRSTS         BEGSR
     C*
     C                   MOVE      'EC'          XXSTS
     C*
     C*** BEGIN ADD *** 04/24/19 **************************************
     C     SVA49D        IFGT      *ZERO
     C     CMIDT         IFLT      SVA49D
     C                   MOVE      *ON           XXERR
     C                   ENDIF
     C                   ELSE
     C*** END ADD ***** 04/24/19 **************************************
      *
     C*** BEGIN ADD *** 05/02/13 **************************************
     C     CMIDT         IFLT      SVCSDT
     C                   MOVE      *ON           XXERR
     C                   ENDIF
     C*** END ADD ***** 05/02/13 **************************************

     C*** BEGIN ADD *** 04/24/19 **************************************
     C                   ENDIF
     C*** END ADD ***** 04/24/19 **************************************
     C                   ENDSR
     C*
     C/EJECT
     C*****************************************************************
     C*  SRDDRY - ADD OR UPDATE DIARY RECORDS.
     C*****************************************************************
     C*
     C     SRDDRY        BEGSR
     C*
     C                   Z-ADD     0             XXDSQG
     C                   Z-ADD     0             XXDSQH
     C                   Z-ADD     0             XXDSQI
     C*
      *** BEG CHG *** 04/26/00 **************************************
     C***********          MOVE *BLANKS   XCKEY
     C                   MOVE(P)   *BLANKS       XCKEY            40
      *** END CHG *** 04/26/00 **************************************
     C                   MOVEL     XXCO#         XCKEY
     C                   MOVE      XXFND         XCFND
     C                   MOVE      XXFYR         XCFYR
     C                   MOVE      XXCASE        XCCASE
     C     XCKEY         CAT       XCFND:0       XCKEY
     C     XCKEY         CAT       XCFYR:0       XCKEY
     C     XCKEY         CAT       XCCASE:0      XCKEY
     C*
      *** BEG DEL *** 04/26/00 **************************************
     C********   1         DO   3         XI
     C********
     C********             MOVE *BLANKS   XCKEY
     C********             MOVELXXCO#     XCKEY
     C********             MOVE XXFND     XCFND
     C********             MOVE XXFYR     XCFYR
     C********             MOVE XXCASE    XCCASE
     C********   XCKEY     CAT  XCFND:0   XCKEY
     C********   XCKEY     CAT  XCFYR:0   XCKEY
     C********   XCKEY     CAT  XCCASE:0  XCKEY
     C********             MOVELXCKEY     CMKEY
     C********             MOVE *BLANKS   CMCSID
     C********             TIME           CMTIME
     C********             MOVE *BLANKS   CMCOMM
     C********             Z-ADDUDATE     SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********             MOVE *BLANKS   CMCODE
     C********             MOVE *BLANKS   CMDCID
     C********                                                         *****
     C****BEGIN CHG *** 02/23/98 *******************************************
     C*SR*******           MOVEL'W8070R3' CMPNAM
     C********             MOVEL'W0070R'  CMPNAM
     C****END*  CHG *** 02/23/98 *******************************************
     C********                                                         *****
     C********             Z-ADDUDATE     SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMTRDT
     C********             TIME           CMTRTM
     C********             MOVE *ZEROS    CMSECL
     C********             MOVE 'N'       CMSBYL
     C********             MOVE 'A'       CMSTAT
     C********             MOVELQ1PGM     CMPGM
     C********             Z-ADD1         CMTIME
     C********             Z-ADDUDATE     SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMDT
     C********             MOVE Q1USER    CMIUSR
     C********             MOVE *BLANKS   CMFOLD
     C********             MOVE *ZEROS    CMDCDT
     C********             MOVE 'Y'       CMDIFL
     C********             MOVE *BLANKS   CMAPID
     C********             MOVE *BLANKS   CMIDOC
     C********             MOVE *BLANKS   CMIQUE
     C********             MOVE *BLANKS   CMIMGF
     C********             MOVE *BLANKS   CMIPTH
     C********             MOVE *ZEROS    CMPIDX
     C********             MOVE *BLANKS   CMQPTY
     C********             MOVE *BLANKS   CMDCTP
     C********             MOVELQ1PGM     CMIPGM
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C*SR*******           MOVE *BLANKS   CMSMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C********             MOVE *BLANKS   CMFL01
     C********             MOVE *BLANKS   CMFL02
     C********             MOVE *BLANKS   CMFL03
     C********             MOVE *BLANKS   CMFL04
     C********             MOVE *BLANKS   CMFL05
     C********             Z-ADDUDATE     SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMIDT
     C********
     C********   Calculate Diary Date
     C********
     C********   XI        IFEQ 1
     C********             Z-ADDIXADRC    SRDAYS
     C********             MOVELIXADDR    CMRMCD
     C********             MOVE *BLANKS   CMUSER
     C********   CMCLEX    CHAINPUP01                14
     C********             MOVELUPUSR     CMUSER
     C********             ENDIF
     C********
     C********   XI        IFEQ 2
     C********             Z-ADDIXSDIM    SRDAYS
     C********             MOVELIXSDDR    CMRMCD
     C********             MOVE *BLANKS   CMUSER
     C********   CMSUPR    CHAINPUP01                14
     C********             MOVELUPUSR     CMUSER
     C********             ENDIF
     C********
     C********   XI        IFEQ 3
     C********             Z-ADDIXLRIM    SRDAYS
     C********             MOVELIXDDRC    CMRMCD
     C********             MOVE *BLANKS   CMUSER
     C********   IXLRSC    CHAINPUP01                14
     C********             MOVELUPUSR     CMUSER
     C********             ENDIF
     C********
     C********             MOVE QQTRDT    SRDATE
     C********             MOVE XXTRDT    CMCDAT
     C********             MOVE *BLANKS   SRERR   1
     C********   SRDAYS    IFGT *ZEROS
     C********             CALL 'SRFUTR8'
     C********             PARM           SRDATE  80
     C********             PARM           SRDAYS  50
     C********             PARM           SRERR   1
     C********             Z-ADDSRDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     CMCDAT
     C********             ENDIF
     C********
     C******Don't write diary entry if remark code OR USER is blank.
     C********
     C********   CMRMCD    IFNE *BLANKS
     C********   CMUSER    ANDNE*BLANKS
     C********
     C********   Retrieve the remark table entry
     C********
     C********   CMRMCD    CHAINPCT03                14
     C********   *IN14     IFEQ *OFF
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********             MOVE CTSMCD    CMSMCD
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********             MOVELC3DESC    CMCOMM
     C********             MOVELCTDEPG    CMDCID
     C********   CMDCID    IFNE *BLANKS
     C********             MOVELCMKEY     CMFOLD
     C********             MOVE 'Y'       CMFL01
     C********             ENDIF
     C********   CTDPRG    IFGT *ZEROS
     C********             MOVE 'Y'       CMFL02
     C********             ENDIF
     C********             MOVE CTFL01    CMDIFL
     C********             MOVE CTFL02    CMFL03
     C********             ENDIF
     C********
     C********   Get last sequence # used
     C********
     C********   XCKEY     SETGTPCM03
     C********             READPPCM03                    15
     C********
     C********   *IN15     IFEQ *OFF
     C********   C3SEQ#    ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     C********             ENDIF
     C********
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C********
     C********   XI        IFEQ 1
     C********             MOVE CMSEQ#    XXDSQG
     C********             ENDIF
     C********   XI        IFEQ 2
     C********             MOVE CMSEQ#    XXDSQH
     C********             ENDIF
     C********   XI        IFEQ 3
     C********             MOVE CMSEQ#    XXDSQI
     C********             ENDIF
     C********
     C********             ENDIF                           IF USER EQ BLANKS
      *** END DEL *** 04/26/00 **************************************
      *** BEG ADD *** 04/26/00 **************************************
      *
      *          Call API to write a DARTS Remark
      *          ------------------------------------------------------
      *
     C     1             DO        3             XI
      *
     C                   MOVEL(P)  'Y'           Q#DIFL            1
     C                   MOVE      *BLANKS       Q#CODE
     C                   MOVE      *BLANKS       Q#DCID
     C                   MOVE      *BLANKS       Q#FOLD
      *                                                                    *****
      *          Calculate Diary Date
      *
     C     XI            IFEQ      1
     C                   Z-ADD     IXADRC        SRDAYS
     C                   MOVEL(P)  IXADDR        Q#RMCD
     C     CMCLEX        CHAIN     PUP01                              14
     C                   MOVEL(P)  UPUSR         Q#USER
     C                   ENDIF
      *
     C     XI            IFEQ      2
     C                   Z-ADD     IXSDIM        SRDAYS
     C                   MOVEL(P)  IXSDDR        Q#RMCD
     C     CMSUPR        CHAIN     PUP01                              14
     C                   MOVEL(P)  UPUSR         Q#USER
     C                   ENDIF
      *
     C     XI            IFEQ      3
     C                   Z-ADD     IXLRIM        SRDAYS
     C                   MOVEL(P)  IXDDRC        Q#RMCD            5
     C     IXLRSC        CHAIN     PUP01                              14
     C                   MOVEL(P)  UPUSR         Q#USER
     C                   ENDIF
      *
     C                   MOVE      QQTRDT        SRDATE
      *
     C     SRDAYS        IFGT      *ZEROS
      *
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE            8 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM      *BLANK        SRERR             1
      *
     C                   MOVE      SRDATE        Q#CDAT            8
     C                   ENDIF
      *
      *   Don't write diary entry if remark code OR USER is blank.
      *
     C     Q#RMCD        IFNE      *BLANKS
     C     Q#USER        ANDNE     *BLANKS
      *
      *      Retrieve the remark table entry
      *
     C     Q#RMCD        CHAIN     PCT03                              14
      *
     C     *IN14         IFEQ      *OFF
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
      *
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  XCKEY         Q#FOLD           80
     C                   MOVE      'Y'           Q#FL01            1
     C                   ENDIF
      *
     C     CTDPRG        IFGT      *ZEROS
     C                   MOVE      'Y'           Q#FL02            1
     C                   ENDIF
      *
     C                   MOVE      CTFL01        Q#DIFL            1
     C                   ENDIF
      ********
      *
     C                   MOVEL(P)  XCKEY         Q#KEY            40
     C                   MOVEL(P)  *BLANKS       Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      '000'         Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
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
      *
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
      ********
      *
     C     XI            IFEQ      1
     C                   MOVE      Q#SEQ#        XXDSQG
     C                   ENDIF
      *
     C     XI            IFEQ      2
     C                   MOVE      Q#SEQ#        XXDSQH
     C                   ENDIF
      *
     C     XI            IFEQ      3
     C                   MOVE      Q#SEQ#        XXDSQI
     C                   ENDIF
      *
     C                   ENDIF                                                  IF USER EQ BLANKS
      *
      *** END ADD *** 04/26/00 **************************************
     C*
     C                   ENDDO                                                  DO 3
     C*
     C                   ENDSR
     C/EJECT
     C*****************************************************************
     C*  SRUPDT - Add LOG RECORD.
     C*****************************************************************
     C*
     C     SRUPDT        BEGSR
     C*
     C*** Determine if an add or update is being processed.
     C*
     C                   MOVE      *BLANKS       SVMTC
     C                   MOVE      *BLANKS       SVSTS
     C     CMKY          CHAIN     PLG01                              13
     C     *IN13         DOWEQ     *OFF
     C*
     C*** BEG ADD ***** 02/26/07 **************************************
     C     PARETP        IFNE      'M'
     C*** END ADD ***** 02/26/07 **************************************
     C     LGTSET        IFEQ      'A49'
     C     LGMTC         ANDEQ     'IP'
     C*
     C                   EXSR      SRSAV                                        Save log fields
     C                   MOVE      LGMTC         SVMTC
     C                   MOVE      LGSTS         SVSTS
     C*
     C*** Begin dlt *** 08/28/02 **************************************
     C********** LGSTS     IFEQ 'EI'                       OPEN LOG RECORD
     C********** LGSTS     OREQ 'EC'
     C********** LGSTS     OREQ 'SN'                       ADD 05/03/02
     C*** End dlt ***** 08/28/02 **************************************
     C                   LEAVE
     C*** Begin dlt *** 08/28/02 **************************************
     C**********           ENDIF
     C*** End dlt ***** 08/28/02 **************************************
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 01/04/11 **************************************
     C     LGTSET        IFEQ      'A49'
     C     LGMTC         ANDEQ     'AP'
     C                   EXSR      SRSAV                                                    lds
     C                   MOVE      LGMTC         SVMTC
     C                   MOVE      LGSTS         SVSTS
     C                   LEAVE
     C                   ENDIF
     C*** END ADD   *** 01/04/11 **************************************
     C*
     C*** BEG ADD ***** 02/26/07 **************************************
     C                   ELSE
     C     LGTSET        IFEQ      'A49'
     C     LGMTC         ANDEQ     'PY'
     C*
     C                   EXSR      SRSAV                                        Save log fields
     C                   MOVE      LGMTC         SVMTC
     C                   MOVE      LGSTS         SVSTS
     C*
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 02/26/07 **************************************
     C     CMKY          READE     PLG01                                  13
     C                   ENDDO
     C*
     C*** Load the Maint Type Code
     C*
     C     XXDNY         IFEQ      *ON                                          Denial/Controvrt
     C                   MOVEL     '04'          XXMTC
     C                   ELSE
     C*** Begin add *** 01/07/02 **************************************
     C     CMACQR        IFEQ      'Y'                                          Acquired
     C                   MOVEL     'AP'          XXMTC
     C                   ELSE
     C*** End add ***** 01/07/02 **************************************
     C                   MOVEL     'IP'          XXMTC
     C*** Begin add *** 01/07/02 **************************************
     C                   ENDIF
     C*** End add ***** 01/07/02 **************************************
     C                   ENDIF
     C*** Begin add *** 12/14/04 **************************************
     C*** BEG ADD *** 01/31/08 ****************************************
     C*
     C                   MOVEL(P)  PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              62
     C     *IN62         IFEQ      '0'
     C     B2PYCD        ANDNE     *BLANKS
     C*
     C                   SELECT
     C     B2PYCD        WHENEQ    '500'
      ***** BEG CHG ****** 03/27/09 ***********************************
     C****                 MOVEL'PY'      XXMTC
     C                   GOTO      SKIP
      ***** END CHG ****** 03/27/09 ***********************************
     C                   ENDSL
     C*
     C                   ENDIF
     C*** END ADD *** 01/31/08 ****************************************
     C     PARETP        IFEQ      'M'
     C                   MOVEL     'PY'          XXMTC
     C*** BEG ADD ***** 02/26/07 **************************************
     C                   MOVE      *ON           *IN13
     C*** END ADD ***** 02/26/07 **************************************
     C                   END
     C*** End add ***** 12/14/04 **************************************
     C                   MOVE      XXMTC         LGMTC
     C*** BEG ADD ***** 04/05/10 **************************************
     C                   MOVE      LGSTS         SVSTS#            2
     C*** END ADD ***** 04/05/10 **************************************
     C                   MOVE      XXSTS         LGSTS
     C*
     C*** Load the Submission Due Date (aka. Maint Type Code Date)
     C*
     C                   Z-ADD     0             LGDUE
     C                   Z-ADD     CMACDT        SRDTI
     C*
     C*** Begin add *** 12/14/04 **************************************
     C     PARETP        IFNE      'M'
     C*** End add ***** 12/14/04 **************************************
     C*
     C*** BEGIN ADD *** 07/20/00 **************************************
     C*     Select basis date to compute the submission due date.
     C*
     C                   SELECT
     C*
     C     IXBASE        WHENEQ    'A'                                          Accident date
     C     CMACDT        ANDGT     *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C*
     C     IXBASE        WHENEQ    'R'
     C     CMRCVD        ANDGT     *ZERO                                        Date receive
     C                   Z-ADD     CMRCVD        SRDTI
     C*
     C     IXBASE        WHENEQ    'D'
     C     CMDBDT        ANDGT     *ZERO
     C                   Z-ADD     CMDBDT        SRDTI                          Disability
     C*
     C                   OTHER                                                  Default
     C                   Z-ADD     XXDATE        SRDTI                          system date
     C                   ENDSL
     C*** END ADD ***** 07/20/00 **************************************
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C                   Z-ADD     IXSDUE        SRDAYS
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C     SRERR         IFNE      '1'
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDUE
     C                   ENDIF
     C*
     C*  IF THE DUE DATE IS ON A SAT OR SUN, CHANGE IT TO THE PREVIOUS FRI.
     C*
     C                   Z-ADD     LGDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C                   MOVE      SRDATE        SRDTE
     C                   MOVE      *BLANKS       SRSDAY
     C                   MOVE      *BLANKS       SRLDAY
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRDAY8'
     C***  BEGIN CHG  ***  11/04/97  *********************************
     C**********           PARM           SRDTE   8
     C                   PARM                    SRDTE             8 0
     C***  END   CHG  ***  11/04/97  *********************************
     C                   PARM                    SRSDAY            4
     C                   PARM                    SRLDAY            9
     C                   PARM                    SRERR
     C*
     C                   Z-ADD     0             SRDAYS
     C     SRSDAY        IFEQ      'SAT.'
     C                   Z-ADD     1             SRDAYS
     C                   ENDIF
     C     SRSDAY        IFEQ      'SUN.'
     C                   Z-ADD     2             SRDAYS
     C                   ENDIF
     C*
     C     SRDAYS        IFNE      0
     C                   Z-ADD     LGDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C                   MOVE      SRDATE        SRDTE
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C     SRERR         IFNE      '1'
     C                   MOVE      SRDTE         SRDATE
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDUE
     C                   ENDIF
     C                   ENDIF
     C*
     C*** Begin add *** 12/14/04 **************************************
     C                   ELSE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDUE
     C*
     C                   ENDIF
     C*** End add ***** 12/14/04 **************************************
     C*
     C*** Load the Diary sequence numbers
     C*
     C                   MOVE      XXDSQG        LGDSQG
     C*** BEGIN DLT *** 09/22/00 **************************************
     C*******************  MOVE XXDSQH    LGDSQH
     C*** END DLT ***** 09/22/00 **************************************
     C                   MOVE      XXDSQI        LGDSQI
     C*
     C***** BEG ADD ****** 04/06/09 ***********************************
     C                   EXSR      CHKMTC
     C     XXVMTC        IFEQ      'N'
     C                   GOTO      SKIP
     C                   ENDIF
     C***** END ADD ****** 04/06/09 ***********************************
      *
     C     *IN13         IFEQ      *OFF                                         Update
     C*
     C*** Beg Add ****** 05/01/07 **********************************************
     C* Do not update existing 'IP' maintenance code records.
     C     SVMTC         IFEQ      'IP'
     C                   GOTO      SKIP
     C                   ENDIF
     C*** End Add ****** 05/01/07 **********************************************
     C*** Beg Add ****** 01/04/11 **********************************************
     C* Do not update existing 'AP' maintenance code records.
     C     SVMTC         IFEQ      'AP'
     C                   GOTO      SKIP
     C                   ENDIF
     C*** End Add ****** 01/04/11 **********************************************
     C*
     C****** BEGIN ADD ***************** 06/30/08 *********************
     C     TNMP1         IFEQ      *ON
     C                   MOVE      'H'           LGSTAT
     C                   ENDIF
     C****** END ADD ******************* 06/30/08 *********************
     C*
     C                   MOVEL     Q1USER        LGUSER
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   TIME                    LGTIME
     C                   MOVEL     Q1PGM         LGPGM
     C*** BEG ADD ***** 04/05/10 **************************************
     C     LGSTS         IFEQ      'EC'
     C     SVSTS#        ANDNE     'EC'
     C                   EXSR      DARTS
     C                   ENDIF
     C*** END ADD ***** 04/05/10 **************************************
     C                   UPDATE    PLG01
     C*
      /Free
       //*** Begin Add *** 02/20/20 **********************************
       If XXMTC = 'IP';
          Setgt (XXco#:XXfnd:XXfyr:XXcase:PAbr2:PAtrdt) PAW01;
          Readpe (XXco#:XXfnd:XXfyr:XXcase:PAbr2)PAW01;
           If not %eof(WMAWL3);
              If AWmtc = *blanks;
                 AWmtc = XXMTC;
                 Update PAW01;
              Endif;
           Endif;
       Endif;
       //*** End   Add *** 02/20/20 **********************************
      /End-Free
     C                   ELSE
     C     SVMTC         IFEQ      'IP'
     C     XXMTC         ANDEQ     'IP'
     C     SVSTS         IFEQ      'EC'
     C     SVSTS         OREQ      'EI'
     C*
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*** BEG ADD ***** 02/26/07 **************************************
     C     SVMTC         IFEQ      'PY'
     C     XXMTC         ANDEQ     'PY'
     C     SVSTS         IFEQ      'EC'
     C     SVSTS         OREQ      'EI'
     C*
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 02/26/07 **************************************
     C*
     C*** BEG ADD ***** 01/04/11 **************************************
     C     SVMTC         IFEQ      'AP'
     C     XXMTC         ANDEQ     'AP'
     C     SVSTS         IFEQ      'EC'
     C     SVSTS         OREQ      'EI'
     C                   GOTO      SKIP
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 01/04/11 **************************************
     C                   MOVE      XXCO#         LGCO#
     C                   MOVE      XXFND         LGFND
     C                   MOVE      XXFYR         LGFYR
     C                   MOVE      XXCASE        LGCASE
     C                   MOVE      XXJRST        LGJST
     C                   MOVEL     'A49'         LGTSET
     C                   MOVE      *ZEROS        LGEXDT
     C                   MOVE      *ZEROS        LGMTCD
     C*** Begin add *** 03/22/02 **************************************
     C                   MOVE      PATRDT        LGMTCD
     C*** End add ***** 03/22/02 **************************************
     C*** Begin add *** 08/19/03 **************************************
     C*** BEG ADD ****** 10/19/07 **********************************************
      *** BEGIN DELETE **** 10/15/08 **********************************
     C* If LGJST is FL, and PATYPE = P:
     C*   Retrieve NIDT4 from WMNIP, If not zero
     C*   Load to LGMTCD
     C****       LGJST     IFEQ 'FL'
     C****       PATYPE    ANDEQ'P'
     C****       NIKY      CHAINPNI01                99
     C****       *IN99     IFEQ *OFF
     C****       NIDT4     ANDNE0
     C****                 MOVE NIDT4     LGMTCD
     C****                 ENDIF                           *IN99-*OFF
     C****                 ELSE                            LGJST = 'FL'
      *** END DELETE ****** 10/15/08 **********************************
     C*** END ADD ****** 10/19/07 **********************************************
     C     CPKY          CHAIN     PCP01                              99
     C     *IN99         IFEQ      *OFF
     C     CPCMDT        ANDNE     0
     C     PATYPE        ANDEQ     'P'
     C                   MOVE      CPCMDT        LGMTCD
     C                   ENDIF
     C*** BEG ADD ****** 10/19/07 **********************************************
      *** BEGIN DELETE **** 10/15/08 **********************************
     C****                 ENDIF                           LGJST = 'FL'
      *** END DELETE ****** 10/15/08 **********************************
     C*** END ADD ****** 10/19/07 **********************************************
     C*** End add ***** 08/19/03 **************************************
     C                   MOVE      *ZEROS        LGSENT
     C                   MOVE      *ZEROS        LGARCV
     C                   MOVE      *ZEROS        LGAENT
     C                   MOVEL     IXFROI        LGHCPY
     C                   MOVE      *ZEROS        LGSCHP
     C                   MOVE      *ZEROS        LGPRSQ
     C                   MOVE      *BLANK        LGBLFL                         ADD 02/17/03
     C                   MOVEL     Q1USER        LGIUSR
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGIDT
     C                   TIME                    LGITM
     C                   MOVEL     Q1PGM         LGIPGM
     C                   MOVE      Q1USER        LGUSER
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   TIME                    LGTIME
     C                   MOVEL     Q1PGM         LGPGM
     C*
     C*** BEGIN ADD *** 09/22/00 **************************************
     C*  Call api to retrieve the next available log seq#.
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C                   MOVE      *ZEROS        ##SEQ#
     C                   CALL      'WA045R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##SEQ#            5
     C                   PARM                    ##RTCD            1
     C*
     C                   MOVE      ##SEQ#        LGDSQH
     C*** END ADD ***** 09/22/00 **************************************
     C*
     C****** BEGIN ADD ***************** 06/30/08 *********************
     C     TNMP1         IFEQ      *ON
     C                   MOVE      'H'           LGSTAT
     C                   ENDIF
     C****** END ADD ******************* 06/30/08 *********************
     C*** BEG ADD ***** 04/05/10 **************************************
     C     LGSTS         IFEQ      'EC'
     C                   EXSR      DARTS
     C                   ENDIF
     C*** END ADD ***** 04/05/10 **************************************
      *** Begin Add *** 08/11/15 **************************************
      /Free
         LGDSQM = *Zeros;
      /End-Free
      *** End   Add *** 08/11/15 **************************************
     C                   WRITE     PLG01
      /Free
       //*** Begin Add *** 02/20/20 **********************************
       If XXMTC = 'IP';
          Setgt (XXco#:XXfnd:XXfyr:XXcase:PAbr2:PAtrdt) PAW01;
          Readpe (XXco#:XXfnd:XXfyr:XXcase:PAbr2)PAW01;
           If not %eof(WMAWL3);
              If AWmtc = *blanks;
                 AWMTC = XXMTC;
                 Update PAW01;
               Endif;
           Endif;
       Endif;
       //*** End   Add *** 02/20/20 **********************************
      /End-Free
     C*
     C     SKIP          TAG
     C*
     C                   ENDIF
     C*
     C****** BEGIN ADD ***************** 06/30/08 *********************
     C                   MOVE      *OFF          TNMP1
     C****** END ADD ******************* 06/30/08 *********************
     C                   ENDSR
     C*****************************************************************
     C*  SRSAV - SAVE PREVIOUS LOG RECORD VALUES.
     C*****************************************************************
     C*
     C     SRSAV         BEGSR
     C*
     C     XXDSQG        IFEQ      *ZEROS
     C                   MOVE      LGDSQG        XXDSQG
     C                   ENDIF
     C*** BEGIN DLT *** 09/22/00 **************************************
     C********** XXDSQH    IFEQ *ZEROS
     C**********           MOVE LGDSQH    XXDSQH
     C**********           ENDIF
     C*** END DLT ***** 09/22/00 **************************************
     C     XXDSQI        IFEQ      *ZEROS
     C                   MOVE      LGDSQI        XXDSQI
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*** Begin add *** 12/14/04 **************************************
     C*****************************************************************
     C*  MEDPMT - Create a 'PY' A49 Log Record if this is the first
     C*           Medical pmt and the jurisdiction state is 'TN'.
     C*****************************************************************
     C*
     C     MEDPMT        BEGSR
     C*
     C***** BEGIN ADD ********** 06/30/08 *****************************
     C                   MOVE      *OFF          TNMP1             1
     C***** END ADD ************ 06/30/08 *****************************
      *
     C                   MOVE      PAFND         XXFND
     C                   MOVE      PAFYR         XXFYR
     C                   MOVE      PACASE        XXCASE
     C     CMKY          CHAIN     PCM01                              13
     C*
     C     CMJRST        IFEQ      'TN'
     C*** Begin dlt *** 10/17/07 **************************************
     C********** CMGT1     ANDEQ'Y'
     C*** End dlt ***** 10/17/07 **************************************
     C*
     C*  Determine if this is the first medical payment.
     C*
     C                   MOVE      *OFF          XXSEL
      *** BEGIN ADD **** 10/08/08 *************************************
     C                   MOVE      *OFF          XXDUP             1
      *** END ADD ****** 10/08/08 *************************************
     C     PAKY5         CHAIN     PPA05                              12
     C     *IN12         DOWEQ     *OFF
     C*
     C*  Ignore pmts made by other TPAs.
     C*
     C                   MOVEL(P)  P5BR2         XXKEY
     C     TBKY          CHAIN     PB201                              99
     C   99              MOVE      *BLANKS       B2PYCD
     C*
     C     P5RETP        IFEQ      'M'
     C     B2PYCD        ANDNE     *BLANKS
     C     PASEQ#        IFEQ      P5SEQ#
     C                   MOVE      *ON           XXSEL
     C***** BEGIN ADD ********** 06/30/08 *****************************
     C                   MOVE      *ON           TNMP1
     C***** END ADD ************ 06/30/08 *****************************
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C*
     C     PAKY5         READE     PPA05                                  12
     C                   ENDDO
     C*
      *** BEGIN ADD **** 10/08/08 *************************************
     C                   EXSR      CHKPY
      *** END ADD ****** 10/08/08 *************************************
     C     XXSEL         IFEQ      *ON
     C*
     C*    Retrieve the Claim Master.
     C*
     C                   EXSR      SRCM
     C*
     C     XXERR         IFEQ      *OFF
     C*
     C*    Check for 148 log record.
     C*
     C                   EXSR      SRRLOG
     C*
     C*    If the 148 log record is not found, run the API to
     C*    write it.
     C*
     C     XXFROI        IFEQ      *OFF
     C                   EXSR      SRW148
     C                   ENDIF
     C*
     C*    Determine the status of the A49 Log Record.
     C*
     C                   EXSR      SRSTS
     C*
     C*    Write the A49 log record.
     C*
     C                   EXSR      SRUPDT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** End add ***** 12/14/04 **************************************
      *** BEGIN ADD **** 10/08/08 *************************************
     C*----------------------------------------------------------------
     C* CHECK FOR EXISTANCE OF PY MTC
     C*----------------------------------------------------------------
     C     CHKPY         BEGSR
     C                   MOVE      'A49'         XXTSET
     C                   MOVE      'PY'          XXMTC
     C     LGKY4         SETLL     WMLGL4                                 60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          XXSEL
     C                   ENDIF
     C                   ENDSR
      *** END ADD ****** 10/08/08 *************************************
      *
      *** BEGIN ADD **** 04/06/09 *************************************
     C*----------------------------------------------------------------
     C* CHECK FOR VALID MTC
     C*----------------------------------------------------------------
     C     CHKMTC        BEGSR
      *
      *   Maintenance Type Code must be valid
      *
     C     CMJRST        CAT(P)    'E2':0        XXTBID
     C     GIKY          CHAIN     PGI01                              99
     C   99              MOVEL     'SMTC'        XXTBID
      *
     C                   MOVEL(P)  XXMTC         XXTKEY
     C     XTKY          CHAIN     PGT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           XXVMTC            1
     C                   ELSE
     C                   MOVE      'N'           XXVMTC
     C                   ENDIF
      *
     C                   ENDSR
     C***** END ADD ****** 04/06/09 ***********************************
     C*** BEG ADD ***** 04/05/10 **************************************
     C     DARTS         BEGSR
     C*
     C     LGTSET        IFEQ      '148'
     C     LGTSET        OREQ      'A49'
     C*
     C     IXKY1         CHAIN     PIX01                              60
     C*
     C                   SELECT
     C*
     C*
     C     XXSTS         WHENEQ    'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE      IXCECR        Q#RMCD            5
     C*
     C                   OTHER
     C                   GOTO      END
     C                   ENDSL
     C*
     C                   MOVE      LGCO#         QCCO#
     C                   MOVE      LGFND         QCFND
     C                   MOVE      LGFYR         QCFYR
     C                   MOVE      LGCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY
     C*
     C     Q#RMCD        CHAIN     PCT03                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  C3DESC        Q#COMM
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C*
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C*
     C     CMCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   UPUSR         Q#USER           10
     C                   ENDIF
     C     Q#USER        IFEQ      *BLANKS
     C                   GOTO      END
     C                   ENDIF
     C     LGSTS         IFEQ      'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD           80
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  'Y'           Q#FL01            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   ENDIF
     C*
     C     CTDPRG        IFGT      0
     C                   MOVEL(P)  'Y'           Q#FL02            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   ENDIF
     C*
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
     C                   MOVEL     *BLANKS       Q#COMM
     C     LGMTC         CAT       '-':0         Q#COMM
     C                   CAT       C3DESC:0      Q#COMM
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#COMM          256
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
     C                   ENDIF
     C     END           ENDSR
     C*** END ADD ***** 04/05/10 **************************************
