     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7019R1- PRINT FILECOPY
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM WILL PRINT A FILECOPY.                         *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W0576R -  Payment Detail Search                             *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   SRAMNT  - ASSIGN PAYMENT VERBAGE                            *
     H*   SRNAME  - FORMAT FIRST & LAST NAME INTO ONE FIELD           *
     H*                                                               *
     H* CREATED:  08/29/97  C:30  Project:  WC659                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/17/20 C:062 Ticket: 93545(146)
     H*                Call to RTVCLTID must be in uppercase.         *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN, corrected existing compilation error *
     H*
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                ALLOW FOR XML TYPE TRANSACTIONS                *
     H*                                                               *
     H* 08/09/07  C:191 PROJECT:WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*
     H* 10/20/03 C:53 Project: WC909                                  *
     H*               Added country info.                             *
     H*                                                               *
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H* 04/25/01 C:15 Project: WC1020                                 *
     H*               Added bank code (PACKTP) to key list when       *
     H*               retrieving the bank info record.                *
     H*                                                               *
     H* 05/19/00 C:15 Support: WC034/20089                            *
     H*               Added the TRDT to key list for WDRPP.           *
     H*                                                               *
     H* 02/23/99 C:15 Support: WC065/16782                            *
     H*               Fixed code which retrieves the Bank Info data.  *
     H*                                                               *
     H* 12/23/98 C:15 Support: WC021/#15639                           *
     H*               Removed XXTRDT from key list used to retrieve   *
     H*               records from WDRPP due to incorrect date being  *
     H*               loaded into RPTRDT.                             *
     H*                                                               *
     H* 10/05/98 C:15 Support                                         *
     H*               Load the claimant SS# into the payee box on     *
     H*               the check if RPFEIN is blank.                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       51      GENERAL USAGE (CHAIN)                           *
     H*       61      ADDRESS 2 IS BLANK                              *
     H*****************************************************************
     H* FILE USAGE:                                                   *
     H*                                                               *
     H*   WMFNL  (WMFNL) : CO/FND                                     *
     H*   WDPAL5 (WDPAL5): CO/FND/FYR/CASE/DATE/SEQ#                  *
     H*   WMCML1 (WMCML1): CO/FND/FYR/CASE                            *
     H*   WMCDL  (WMCDL) : CO/SSN                                     *
     H*   WTABL  (PRC01) : CO/KEY                                     *
     H*          (PB201) : CO/KEY                                     *
     H*   WMCDL  (PCD01) : SS#                                        *
     H*   WMEML  (PEM01) : CO/FND/EMP#/DIV                            *
     H****WMPDL  (PPD01) : FEIN/MOD                                   *
     H*   WMBIL4 (PBI01) : CO/ACCT/FND/EFDT                           *
     H*   CHECKS         : VOUCHER                                    *
     H*****************************************************************
     FWMCXL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F*** BEG ADD *** 10/20/03 *****************************
     FWTGTL     IF   E           K DISK
     F*** END ADD *** 10/20/03 *****************************
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F***WMPDL   IF  E           K        DISK
      *
      *  Bank Information file
     F*** Begin chg *** 04/25/01 **************************************
     F*****WMBIL4  IF  E           K        DISK
     FWMBIL6    IF   E           K DISK
     F*** End chg ***** 04/25/01 **************************************
     F*** Begin dlt *** 04/25/01 **************************************
     F*** BEGIN ADD *** 02/23/99 **************************************
     F*******WMBIL7  IF  E           K        DISK
     F*******     PBI01                             KRENAMEPBI02
     F*** END ADD ***** 02/23/99 **************************************
     F*** End dlt ***** 04/25/01 **************************************
      *
     FFILECOPY  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
      *
      *  Bank Account  file    - CO#/ACCT
     FWSCKL     IF   E           K DISK
      *
      *  Payment Detail Detail file
     FWDRPL     IF   E           K DISK
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** End add ***** 05/22/03 **************************************
      *
     F*

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D WCA             S             35    DIM(2) CTDATA PERRCD(1)              CONSTANTS     XC
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *  WORK AREA FOR FUND, FUND YEAR, AND CASE FOR VOUCHER FIELD
      *
     D                 DS
     D  XXCLM#                 1     13  0
     D  PAFND                  1      3  0
     D  PAFYR                  4      6  0
     D  PACASE                 7     13  0
      *
      *  WORK AREA FOR CLAIM AND CHECK FOR VOUCHER FIELD
      *
     D                 DS
     D  XXCNBR                 1     10
     D  XXSLSH                 1      1
     D  PACHK#                 2      8  0
     D  XXSLS2                 9      9
     D  XXBLK1                10     10
      *
      *  WORK AREA FOR ZIP CODE
      *
     D***         DS
     D***                                     1   90PDZIP
     D***                                     1   50XXZIP1
     D***                                     6   90XXZIP2
     D                 DS
     D  RPZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
     D*** BEGIN ADD *** 10/05/98 **************************************
     D                 DS
     D  XXSS#                  1      9
     D  SSPRE                  1      3
     D  SSMID                  4      5
     D  SSSUF                  6      9
     D*** END ADD ***** 10/05/98 **************************************
      *
     D                 DS
     D  XXTRAN                 1     11
     D  XXAMPR                 1      1
     D  BITRNS                 2      5
     D  BIBNK#                 6      9
     D  BICKDG                10     10
     D  XXAMP2                11     11
     D*
     D*** Begin add *** 04/25/01 **************************************
     D                 DS
     D*** BEG CHG ***** 08/09/07 **************************************
     D**********                              1  50 DATA
     D  DATA                   1     75
     D*** END CHG ***** 08/09/07 **************************************
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D*** BEG ADD ***** 08/09/07 **************************************
     D  QXSEQ#                30     34
     D  QXEOB#                35     43
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
     D*** END ADD ***** 08/09/07 **************************************
      *
      *  Constant
      *
     D*** BEG CHG ***** 07/27/09 **************************************
     D***********   'VOUCHER'             C         VOUCHR
     D VCHR1           C                   CONST('VOUCHER')
     D VCHR2           C                   CONST('XML    ')
     D*** END CHG ***** 07/27/09 **************************************
     D*** BEG ADD *** 06/09/20 **************************************
     D @@PAY1          C                   CONST('PAY TO THE')
     D @@PAY2          C                   CONST('ORDER OF')
     D*** END ADD *** 06/09/20 **************************************
     D XXADJ           C                   CONST('ADJUSTER:')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D PIDS01        E DS                  EXTNAME(SZQ4P)
      *
      *   CLAIMS/DARTS CONTROL KEY LIST
      *

     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    XXCLEX
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *  CLAIM FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  CLAIM CHECK PAYMENT
      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTRDT            7 0
     C                   KFLD                    XXSEQ#
      *
      *  Payment Detail Detail file
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    PACHK#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   KFLD                    PATRDT
     C*** END ADD ***** 05/19/00 **************************************
     C*** BEGIN DLT *** 12/23/98 **************************************
     C******************** KFLD           XXTRDT
     C*** END DLT ***** 12/23/98 **************************************
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
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
      *  FUND MASTER
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
      *
      *  BANK INFORMAITON FILE
      *
     C*** Begin dlt *** 04/25/01 **************************************
     C********** BIKEY     KLIST
     C**********           KFLD           PACO#
     C**********           KFLD           XXAPPL  2
     C**********           KFLD           PAACCT
     C**********           KFLD           PAFND
     C**********           KFLD           XXEFDT
      **********
     C********** BIKEY1    KLIST
     C**********           KFLD           PACO#
     C**********           KFLD           XXAPPL
     C**********           KFLD           PAACCT
     C**********           KFLD           PAFND
     C*** End dlt ***** 04/25/01 **************************************
     C*** Begin add *** 04/25/01 **************************************
     C     BIKEY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    PAFND
     C                   KFLD                    PACKTP
     C                   KFLD                    PAACCT
     C                   KFLD                    XXEFDT
      *
     C     BIKEY1        KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    PAFND
     C                   KFLD                    PACKTP
     C                   KFLD                    PAACCT
     C*** End add ***** 04/25/01 **************************************
     C*
     C*** BEGIN ADD *** 02/23/99 **************************************
     C     B2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    PAFND
     C*** END ADD ***** 02/23/99 **************************************
      *
      *  Bank Account Master File
      *
     C     CKKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAACCT
      *
     C*** BEG ADD *** 10/20/03 *****************************
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
     C*** END ADD *** 10/20/03 *****************************
      *
     C     *LIKE         DEFINE    CXADJ         X1ADJ
     C     *LIKE         DEFINE    PAMOD         @@MOD
     C     *LIKE         DEFINE    PACO#         @@CO#
     C     *LIKE         DEFINE    PAFND         @@FND
     C     *LIKE         DEFINE    PAFYR         @@FYR
     C     *LIKE         DEFINE    PACASE        @@CASE
     C     *LIKE         DEFINE    PABR2         @@BR2
     C     *LIKE         DEFINE    PARECD        @@RECD
     C     *LIKE         DEFINE    PAWKS         @@WKS
     C     *LIKE         DEFINE    PADAYS        @@DAYS
     C     *LIKE         DEFINE    PACHK#        @@CHK#
     C     *LIKE         DEFINE    X1NAME        @@NAME
     C     *LIKE         DEFINE    PAFEIN        @@FEIN
     C********   *LIKE     DEFN CMACDT    @@ACDT
     C     *LIKE         DEFINE    PAAMT         @@AMT
     C     *LIKE         DEFINE    QQAMT1        @@AMT1
     C     *LIKE         DEFINE    QQAMT2        @@AMT2
     C     *LIKE         DEFINE    X1NAME        @@PNAM
     C     *LIKE         DEFINE    RPADR1        @@ADR1
     C     *LIKE         DEFINE    RPADR2        @@ADR2
     C     *LIKE         DEFINE    PAACCT        @@ACCT
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
     C*** BEG ADD *** 10/20/03 *****************************
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C*** END ADD *** 10/20/03 *****************************
      *
      *****************************************************************
      *  ENTRY PARM LIST
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQTRDT            7
     C                   PARM                    QQSEQ#            5
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
     C     DTMDY6        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQTRDT        XXTRDT
     C                   MOVE      QQSEQ#        XXSEQ#            5 0
     C                   MOVE      '/'           XXSLSH            1
     C                   MOVE      '/'           XXSLS2            1
     C                   MOVE      '&'           XXAMPR            1
     C                   MOVE      '&'           XXAMP2            1
     C                   MOVE      *BLANKS       XXBLK2            2
     C                   MOVE      *BLANKS       XXBLK1            1
     C*** Begin add *** 04/25/01 **************************************
     C                   TIME                    DSDTTM
     C*** End add ***** 04/25/01 **************************************
     C                   SETOFF                                           17
     C*
     C                   Z-ADD     XXTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         @@DATE            6 0
      *
     C                   MOVEL     'VOUCHER'     XXVCH             7
     C*
     C****** PERFORM CVTHEX TO GET ATRN VALUE TO PASS TO THE PRINTER **
     C*****                                                          **
     C                   MOVEL     '03011B'      CHR             256
     C                   MOVE      *BLANKS       HEX             128
     C                   MOVE      *BLANK        RTCODE            1
     C                   CALL      'CVTHEX'
     C                   PARM                    CHR
     C                   PARM                    HEX
     C                   PARM                    RTCODE
     C                   MOVEL     HEX           ESC               3
     C*** beg add *** 06/09/20 ******************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    Cltid             3
      /Free
       If CltId = '129';
          *In71 = *On;
       Else;
          *In71 = *Off;
       EndIf;
          *In62 = *Off;
       If CltId = '034' and XXFND=015;
          *In62 = *On;
       EndIf;
      /End-Free
     C*** End add *** 06/09/20 ******************************
      *** BEGIN ADD *** 05/15/97 **************************************
      *
      *  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE.
      *
     C     CXKY          CHAIN     PCX01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     CXADJ         ANDNE     *BLANKS
      *
     C                   MOVEL     CXADJ         X1ADJ
     C                   ELSE
      *
     C                   MOVEL     XXADJ         X1ADJ
      *
     C                   ENDIF
      *** END   ADD *** 05/15/97 **************************************
      *
      *  RETRIEVE FUND MASTER RECORD
      *
     C     FNKY          CHAIN     WMFNL                              51
     C     *IN51         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
      *
     C                   MOVE      *BLANKS       XXFN             60
     C                   MOVE      *BLANKS       XXFAD1           40
     C                   MOVE      *BLANKS       XXFAD2           40
     C                   MOVE      *BLANKS       XXFCT            25
     C                   MOVE      *BLANKS       XXFST             2
     C                   MOVE      *BLANKS       XXFZIP            5 0
     C                   MOVEL     FNNAME        XXFN
     C                   MOVE      FNMAD1        XXFAD1
     C                   MOVE      FNMAD2        XXFAD2
     C                   MOVE      FNMCTY        XXFCT
     C                   MOVE      FNMST         XXFST
     C                   MOVEL     FNMZIP        XXFZIP
     C     XXFAD2        COMP      *BLANKS                                65
      *
      *  RETRIEVE CLAIM CHECK PAYMENT RECORD
      *
     C     PAKY          CHAIN     WDPAL5                             51
     C     *IN51         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
      *
      *  Retrieve Payment Detail Detail record
      *
     C*** Begin add *** 05/22/03 **************************************
     C     R2KY          CHAIN     PRP02                              51
     C     *IN51         IFEQ      *ON
     C*** End add ***** 05/22/03 **************************************
     C     RPKY          CHAIN     PRP01                              51
     C*** Begin add *** 05/22/03 **************************************
     C                   ENDIF
     C*** End add ***** 05/22/03 **************************************
      *
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     *ZERO         RPZIP
     C                   ELSE
     C*
     C*  RETRIEVE REMARKS
     C*
     C                   MOVEL     RPRMK1        XXKEY
     C     TBKY          CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM1           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM1
     C                   END
     C*
     C                   MOVEL     RPRMK2        XXKEY
     C     TBKY          CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM2           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM2
     C                   END
     C*
     C                   MOVEL     RPRMK3        XXKEY
     C     TBKY          CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM3           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM3
     C                   END
     C*
     C                   MOVEL     RPCKCN        XXKEY                          JJJ 1-6-89
     C     TBKY          CHAIN     PRC01                              51        |  ADDED
     C     *IN51         IFEQ      '0'                                          |
     C                   MOVE      RCDESC        XXREM4           40            |
     C                   ELSE                                                   |
     C                   MOVE      *BLANKS       XXREM4                         |
     C                   END                                                    |
     C                   ENDIF
      *
      *  LOAD DOLLAR AMOUNT SPELLED OUT
      *
     C                   MOVE      PAAMT         QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
      *
      *  RETRIEVE CLAIM  RECORD
      *
     C     CMKY          CHAIN     WMCML1                             51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *ZEROS        CMSS#
     C                   MOVE      *BLANKS       CMACDT
     C                   MOVE      *BLANKS       CMCLEX
     C                   END
      *
      * GET CLAIMS EXAMINER TITLE
     C                   MOVE      CMCLEX        XXCLEX            3
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     *BLANKS       UPTITL
     C                   MOVEL     *BLANKS       UPUSNM
     C                   ELSE
     C                   MOVEL     UPUSNM        XXUSNM           40
     C                   ENDIF
      *
      *** GET EMPLOYER NAME
      *
     C     EMKEY         CHAIN     PEM01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     EMNAME        XXENAM           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXENAM
     C                   END
      *
      *  RETRIEVE CLAIMANT DEMOGRAPHICS RECORD
      *
     C     CDKY          CHAIN     WMCDL                              51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       XXNAME           47
     C                   ELSE
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        X1NAME           40
     C                   END
      *
      *  RETRIEVE PAYEE DEMOGRAPHICS RECORD
      *
     C***        PDKY      CHAINWMPDL                51
     C***        *IN51     IFEQ '1'
     C***                  MOVELX1NAME    X2NAME
     C***                  MOVELCDADR1    PDADR1
     C***                  MOVELCDCTY     PDCTY
     C***                  MOVELCDST      PDST
     C***                  MOVE CDZIP     PDZIP
     C***                  MOVE CDSS#     PAFEIN
     C***                  ELSE
     C***                  MOVE *BLANKS   XXLAST
     C***                  MOVE *BLANKS   XXFRST
     C***                  MOVELPDNAME    XXLAST
     C***                  MOVELPDFSN     XXFRST
     C***                  MOVE '1'       XXTERM
     C***                  CALL 'SRNAME'
     C***                  PARM           XXLAST
     C***                  PARM           XXFRST
     C***                  PARM           XXTERM
     C***                  MOVELXXLAST    X2NAME 40
     C***                  END
      *
      * GET PAYMENT VALUE ONE DESCRIPTION
      *
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       B2DESC
     C                   END
     C                   MOVEL     B2KEY         XXB2D2            3
     C                   MOVEL(P)  B2DESC        XXB2DS           30
      *
      *  RETRIEVE RESERVE CODE DESCRIPTION
      *
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     PARECD        XXKEY
     C     TBKY          CHAIN     PRT01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     RTDESC        XXRTDS           15
     C                   MOVEL     RTDESC        XXRTD2           30
     C                   ELSE
     C                   MOVE      *BLANKS       XXRTDS
     C                   END
     C                   MOVEL     *BLANKS       XXKEY
      *
      *  REVERSE DATES TO PRINT
      *
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         XXFRDT            6 0
      *
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         XXTODT            6 0
      *
      *  LOAD CITY, STATE and ZIP from Payment Detail Detail file
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
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C*** BEG ADD *** 10/20/03 *****************************
     C     RPCTRY        IFNE      *BLANKS
     C     RPCTRY        ANDNE     'USA'
     C                   MOVE      QQCO#         XXCO#
     C                   MOVEL(P)  RPADR3        XXCSZ
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  RPCTRY        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        XXADR4           40
     C                   ELSE
     C                   MOVE      'N/F'         XXADR4
     C                   ENDIF
     C*
     C*********            MOVELRPADR3    XXADR4    P
     C                   END
     C*** END ADD *** 10/20/03 *****************************
      *
     C                   MOVE      *OFF          *IN75                          Regular Check
      *
      *  Check for a Blank Second Address Line
      *
     C                   MOVE      '0'           *IN61
     C     RPADR2        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN61
     C                   ENDIF
      *
      *  Access Bank Account Master to obtain Voucher Flag (Y/N)
      *
     C     CKKEY         CHAIN     PCK01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     CKVCHR        ANDEQ     'Y'
      *** BEG ADD ***** 07/27/09 **********************************
     C     *IN60         OREQ      *OFF
     C     CKVCHR        ANDEQ     'X'
     C                   SELECT
     C     CKVCHR        WHENEQ    'Y'
     C                   MOVEL     VCHR1         VOUCHR            7
     C     CKVCHR        WHENEQ    'X'
     C                   MOVEL     VCHR2         VOUCHR            7
     C                   ENDSL
      *** END ADD ***** 07/27/09 **********************************
     C                   MOVE      *ON           *IN75                          Voucher NO MICR
     C                   ELSE
     C                   MOVE      *OFF          *IN75                          Check   MICR
     C                   ENDIF
      *
      *    GET BANK INFORMATION
      *
     C                   MOVE      'WC'          XXAPPL
     C                   Z-ADD     XXTRDT        XXEFDT
     C*** Begin dlt *** 04/25/01 **************************************
     C*** BEGIN ADD *** 02/23/99 **************************************
     C********** PAACCT    IFNE *BLANKS
     C*** END ADD ***** 02/23/99 **************************************
     C*** End dlt ***** 04/25/01 **************************************
     C*
     C*** Begin add *** 04/25/01 **************************************
     C     PAACCT        IFEQ      *BLANKS
     C*
     C                   Z-ADD     PATRDT        QXTRDT
     C                   Z-ADD     PAFRDT        QXFRDT
     C                   Z-ADD     PATODT        QXTODT
     C                   MOVEL     PATRCD        QXTRCD
     C                   MOVEL     PARECD        QXRECD
     C                   MOVEL     PABR2         QXPV1
     C                   MOVEL     PABR7         QXPV2
     C*** BEG ADD ***** 08/09/07 **************************************
     C                   MOVE      *BLANKS       QXEOB#
     C                   MOVE      *ZEROS        QXFEIN
     C                   MOVE      *ZEROS        QXMOD
     C                   MOVE      *BLANKS       QXSEQ#
     C                   MOVE      *ZEROS        QXDSQ#
     C*** END ADD ***** 08/09/07 **************************************
     C*
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    QQCO#                          Company
     C                   PARM                    QQFND                          Group #
     C                   PARM                    QQFYR                          Year
     C                   PARM                    QQCASE                         Case
     C                   PARM                    XXAPPL                         Applic Code
      ***** BEG CHG ***** 08/09/07 ************************************
     C*******              PARM DATA      ##DATA 50        Selection criteria
     C                   PARM      DATA          ##DATA           75            Selection criteria
      ***** END CHG ***** 08/09/07 ************************************
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD            1            Return Code
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##SRDT                         MMDDCCYY
     C*
     C                   CALL      'WA023R'
     C                   PARM                    QQCO#                          Company
     C                   PARM                    QQFND                          Group #
     C                   PARM                    XXAPPL                         Applic Code
     C                   PARM                    ##SRDT            8            MMDDCCYY
     C                   PARM                    ##ACCT           20            Bank Acct #
     C                   PARM      'C'           ##CALL            1            (V)oucher/(C)heck
     C                   PARM      *BLANK        ##HDCY            1            Hardcopy Y/N
     C                   PARM      *BLANK        ##CHKQ           10            Check/Voucher OUTQ
     C                   PARM      *BLANK        ##FTYP           10            Form Type
     C                   PARM      *BLANK        ##RTCD            1            Return Code
     C                   PARM      *ZERO         ##EFDT            7            WMBIP Eff. DT CYYMMD
     C                   PARM      *BLANKS       ##CPYQ           10
     C                   PARM                    ##CKTP
     C*
     C                   MOVEL     ##CKTP        PACKTP
     C                   MOVEL     ##ACCT        PAACCT
     C*
     C                   ENDIF
     C*** End add ***** 04/25/01 **************************************
      *
     C     BIKEY         SETLL     PBI01
     C     BIKEY1        READE     PBI01                                  51
     C*** BEGIN CHG *** 02/23/99 **************************************
     C********** *IN60     DOWEQ*OFF
     C     *IN51         DOWEQ     *OFF
     C*** END CHG ***** 02/23/99 **************************************
     C     BISTAT        ANDEQ     'I'
     C     BIKEY1        READE     PBI01                                  51
     C                   ENDDO
      *
     C*** Begin dlt *** 04/25/01 **************************************
     C*** BEGIN ADD *** 02/23/99 **************************************
     C**********           ELSE
     C**********
     C********** B2KY      SETGTPBI02
     C********** B2KY      REDPEPBI02                    51
     C**********
     C**********           ENDIF
     C*** END ADD ***** 02/23/99 **************************************
     C*** End dlt ***** 04/25/01 **************************************
      *
     C     *IN51         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
      *
      *  Assign MACRO to PCL5 control characters with Bank Information Macro ID
      *
     C                   MOVE      BIMACR        TEMP3             3
     C     '&f'          CAT       TEMP3         TEMP5             5
     C     TEMP5         CAT       'y3X'         MACRO             8
      *
     C     BIAXOU        IFEQ      *BLANK
     C                   MOVE      *BLANKS       XXAXOU
     C                   MOVE      XXCNBR        XXAXOU           20
     C                   ELSE
     C                   MOVE      BIAXOU        XXAXOU
     C                   END
      *
     C     BIBNK#        CAT(P)    '/'           XXBT#5            5
     C     XXBT#5        CAT(P)    BITRNS        XXBT#             9
      *
      * CHECK FOR AN AMOUNT IN BI$LMT
      *
     C                   MOVE      *OFF          *IN85
     C                   MOVE      *OFF          *IN83
     C     BI$LMT        IFGT      0
     C     PAAMT         ANDGT     BI$LMT
     C                   MOVE      '1'           *IN85
     C                   MOVE      *ON           *IN83
     C                   END
      *
      * CHECK FOR A SIGNATURE MACRO ID
      *
     C     BI10A1        IFNE      *BLANK
     C                   MOVEL     BI10A1        CH1               7
     C                   MOVE      '1'           *IN86
     C                   END
      *
      * CHECK FOR SECOND SIGNATURE MACRO ID
      *
     C     BI10A2        IFNE      *BLANK
     C                   MOVEL     BI10A2        CH2               7
     C                   MOVE      '1'           *IN87
     C                   MOVE      *ON           *IN83
     C                   END
      *
      *  PRINT FUND SHORT NAME IF NO LOGO
      *
     C     BILOGO        IFEQ      'N'
     C                   MOVE      '1'           *IN73
     C                   END
      *
      *  PRINT TRUST NAME ON VOUCHER BASED ON ARRAY
      *
     C     BITRST        IFEQ      'Y'
     C                   MOVEL     WCA(2)        XXWCA            35
     C                   ELSE
     C                   MOVEL     WCA(1)        XXWCA            35
     C                   END
      *
     C     PAFEIN        IFGT      *ZERO
     C                   Z-ADD     PAFEIN        @@FEIN
     C                   MOVEL     PAMOD         @@MOD
     C                   MOVE      *ON           *IN44
     C                   ELSE
     C*** BEGIN ADD *** 10/05/98 **************************************
     C******************** MOVE CDSS#     @@FEIN
     C*** END ADD ***** 10/05/98 **************************************
     C                   MOVE      *ZERO         @@MOD
     C                   MOVE      *OFF          *IN44
     C                   ENDIF
      *
     C                   Z-ADD     PACASE        @@CASE
     C                   Z-ADD     PACHK#        @@CHK#
     C                   MOVEL     X1NAME        @@NAME
     C                   Z-ADD     PAAMT         @@AMT
     C                   MOVEL     QQAMT1        @@AMT1
     C                   MOVEL     QQAMT2        @@AMT2
     C                   MOVEL     RPPNAM        @@PNAM
     C                   MOVEL     RPADR1        @@ADR1
     C                   MOVEL     RPADR2        @@ADR2
     C                   MOVEL     PAACCT        @@ACCT
     C                   MOVEL     PACO#         @@CO#
     C                   MOVEL     PAFND         @@FND
     C                   MOVEL     PAFYR         @@FYR
     C                   MOVEL     PABR2         @@BR2
     C                   MOVEL     PARECD        @@RECD
     C                   MOVEL     PAWKS         @@WKS
     C                   MOVEL     PADAYS        @@DAYS
      *
     C*** BEGIN ADD *** 10/05/98 **************************************
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL(P)  SSPRE         @@SS#            11
     C                   CAT       '-':0         @@SS#
     C                   CAT       SSMID:0       @@SS#
     C                   CAT       '-':0         @@SS#
     C                   CAT       SSSUF:0       @@SS#
     C*** END ADD ***** 10/05/98 **************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         @@ACDT            6 0
      *
     C                   MOVE      *OFF          *IN66
     C     PARETP        IFEQ      'M'
     C                   MOVE      *ON           *IN66
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN07                          NOT Group Type
     C                   MOVE      *ON           *IN77                          First Time
      *
      *  OPEN VOUCHER SPOOL FILE, PRINT VOUCHER AND STUB, THEN CLOSE FILE
      *
     C                   OPEN      FILECOPY
     C                   EXCEPT    CHECK
     C                   EXCEPT    DET2
     C                   CLOSE     FILECOPY
      *
     C     TERM          TAG
     C                   SETON                                        LR
      *** BEG ADD ****** 08/09/07 ***************************************
      * WILL NEVER EXECUTE... DUMMY EXCPT TO AVOID COMPILE ERROR
     C     *INLR         IFEQ      *OFF
     C                   EXCEPT    FRAUD
     C                   MOVE      ' '           XXTXT1            4
     C                   ENDIF
      *** END ADD ****** 08/09/07 ***************************************
      *********************************************************************
      *
     O/COPY OFILCHECK
      *_________________________________________________________________
      *
      *  Copybook for file CHECKS except name DET2
     O/COPY OFILDET2
      *_________________________________________________________________
**  CONSTANTS
WORKERS' COMPENSATION ACCOUNT
ACORDIA WORKERS' COMPENSATION ACCT.
