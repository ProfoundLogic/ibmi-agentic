     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE SC900R - DARTS RECORDS TRANSFER PROGRAM
     H*****************************************************************
     H*                                                              **
     H* PROGRAM:     SC900R                                          **
     H* DESCRIPTION: DARTS GENERIC Records Transfer Program          **
     H* CALLED FROM:                                                 **
     H*                                                              **
     H* DUPLICATE FROM SC900R01 BY:  MIKE FLOWER  DATE:  09/02/98    **
     H*                                                              **
     H* ALL CHANGES PRIOR TO THIS DATE IN SC900R01 ARE IN THIS PGM   **
     H* ALSO.                                                        **
     H*                                                              **
     H*   =================== HISTORY ==========================     **
     H*                                                              **
     H* SC900R WAS RENAMED TO SC900R01 WHICH USES A SPECIFIC KEY.    **
     H* THIS PROGRAM USES THE KEY UP TO THE CASE AND PERFORMS A TEST **
     H* TO SEE IF THERE IS MORE TO THE KEY SUCH AS PRE-CERT # OR     **
     H* MED SERV CODE.  IF SO IT WILL CALL THE ORIGINAL PGM (NOW     **
     H* SC900R01) TO TRANSFER AND RESEQUENCE BASED ON THE ENTIRE KEY **
     H*                                                              **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/24/07 C:062 PROJECT DR214
     H*                 ADD PROCESSING FOR SMCLP
     H*
     H*  03/30/07 C:062 PROJECT 56338
     H*                 CALL SC917R
     H*
     H*  11/29/04 C:062 PROJECT 129WCS
     H*                 DELETE SDAIP
     H*
     H*  09/05/03 C:062 PROJECT: DRMNT
     H*                 Replace the write to SDCMP with the call of
     H*                 DA700R.
     H*
     H*  QMFSEQ03 C:062 PROJECT: WC129 38674
     H*                 DARTS transfer issue
     H*
     H*  07/30/03 C:128
     H*                 DARTS transfer issue - Changed to make sure
     H*                 the sequence number in the key was correct.
     H*                 Sequence number was always at *Highval
     H*
     H*
     H*  12/03/02 C:128 Support: 34696  WC090
     H*                 DARTS transfer issue - Check for error when
     H*                 updating, increment seq number until clears
     H*
     H*  11/26/02 C:15  Support: WC128/35009
     H*                 Changed pgm to use SDCMLI (which includes
     H*                 inactive records) to prevent duplicate keys.
     H*
     H*  01/08/01 C:128 Project: DR131
     H*                 Visual Darts issue
     H*                 1. The image flag in SDCMP had a new type added
     H*                    to it. A 'W' = a PC document. New logic was
     H*                    was added to this program to include this
     H*                    type.
     H*
     H*  02/29/00 C:31  Support: 21781
     H*                 DARTs transfer issue
     H*                 1. Added Folder and Image transfer logic to
     H*                    the claim not found section
     H*
     H*  01/04/00 C:113 PROJECT: DR092
     H*                 Added logic to transfer the VDARTS multiple
     H*                 object file records, if any.
     H*
     H*  12/06/99 C:31  Support: 20731
     H*                 DARTs transfer issue
     H*                 1. Added reposition logic: required if FEOD
     H*                    is used.
     H*
     H* 07/05/99 C:062 SUPPORT: DR095/13079,DR048/13846,DR087/16339
     H*                              DR048/17294
     H*                 CONDITIONED FEOD STATEMENTS.
     H*
     H* 07/05/99 C:062 SUPPORT: MC098/16692
     H*                ADD AN ADDITIONAL TAG NAME TO KEEP FROM CROSS-
     H*                ING OVER TO WRONG TRANSFER LOOP.
     H*
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339
     H*                              DR048/17294
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates
     H*                immediately.
     H*
     H*  09/02/98 C:53 PROJECT:
     H*                USE GENERIC KEY FOR DARTS CASE TRANSFER.
     H*                CALL TO SPECIFIC DARTS KEY TRANSFER ON RCDS
     H*                WITH ADDITIONAL KEY INFO IE.. PRE-CERT, MSCD
     H*
     H*** 11/26/98 C:02
     H*      1. RE-APPLY EXTERNAL IMAGE FILE PROCESSING BASED ON IMGF
     H*
     H*
     H*****************************************************************
     H* INDICATOR USAGE:                                             **
     H*****************************************************************
     H* FILES:                                                       **
     H* SDCMLA:  REMARK DETAIL FILE (KEY/SEQ#/STAT NE 'I')           **
     H* SMARL:   ADDITIONAL REMARKS FILE (KEY/SEQ#/ASQ#/STAT NE I)   **
     H* SMHDL:   CUSTOM GROUP ID DATA FILE RCD FMT A (KEY/CSID/RFMT) **
     H* SMH1L:   CUSTOM GROUP ID DATA FILE RCD FMT B (KEY/CSID/RFMT) **
     H* SMMDL:   MULTI-OBJECT FILE (KEY/SEQ)                         **
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F*** Begin chg *** 11/26/02 **************************************
     F******SDCMLA  IF  E           K        DISK
     F*** BEGIN DEL *** 09/05/03 **************************************
     F**********SDCMLI  IF  E           K        DISK
     F*** End chg ***** 11/26/02 **************************************
     F*** BEG ADD *** 09/02/98 *************************************
     F**********  PCM01                             KRENAMEPCM03
     F*** END   DEL *** 09/05/03 **************************************
     F*** BEGIN DEL *** 12/06/99 **************************************
     F******SDCMLN  UF  E           K        DISK                A    UC
     F******SDCMLO  UF  E           K        DISK                A    UC
     F********    PCM01                             KRENAMEPCM02
     F*** END DEL *** 12/06/99 ****************************************
     F*** END ADD *** 09/02/98 *************************************
     F*** BEGIN ADD *** 12/06/99 **************************************
     FSDCMLN    UF   E           K DISK    USROPN
     FSDCMLO    UF   E           K DISK    USROPN
     F                                     RENAME(PCM01:PCM02)
     F*** BEGIN DEL *** 09/05/03 **************************************
     F*****SDCMP   O   E                    DISK                      A
     F**********  PCM01                             KRENAMEPCM04
     F*** END   DEL *** 09/05/03 **************************************
     F*** END ADD *** 12/06/99 ****************************************
     FSMARL     UF A E           K DISK
     FSMHDL     UF   E           K DISK
     FSMH1L     UF   E           K DISK
     FSMDLL     UF A E           K DISK
     F*** BEGIN ADD *** 11/29/04 **************************************
     FSDAIL     UF   E           K DISK
     F*** END   ADD *** 11/29/04 **************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*** BEGIN ADD *** 12/06/99 **************************************
     D*
     D* -----------------------------------------------------
     D* RENAME PMCMLA
     D* -----------------------------------------------------
     D*** BEGIN DEL *** 09/05/03 **************************************
     D**********PCM03
     D**********    CMKEY                           C2KEY
     D**********    CMSEQ#                          C2SEQ#
     D*** END   DEL *** 09/05/03 **************************************
     D*** Begin dlt *** 11/26/02 **************************************
     D**********    CMCSID                          C2CSID
     D**********    CMCOMM                          C2COMM
     D**********    CMCDAT                          C2CDAT
     D**********    CMCODE                          C2CODE
     D**********    CMDCID                          C2DCID
     D**********    CMPNAM                          C2PNAM
     D**********    CMTRDT                          C2TRDT
     D**********    CMTRTM                          C2TRTM
     D**********    CMSECL                          C2SECL
     D**********    CMSBYL                          C2SBYL
     D**********    CMSTAT                          C2STAT
     D**********    CMUSER                          C2USER
     D**********    CMPGM                           C2PGM
     D**********    CMTIME                          C2TIME
     D**********    CMDT                            C2DT
     D**********    CMIUSR                          C2IUSR
     D**********    CMFOLD                          C2FOLD
     D**********    CMDCDT                          C2DCDT
     D**********    CMDIFL                          C2DIFL
     D**********    CMAPID                          C2APID
     D**********    CMIDOC                          C2IDOC
     D**********    CMIQUE                          C2IQUE
     D**********    CMIMGF                          C2IMGF
     D**********    CMIPTH                          C2IPTH
     D**********    CMPIDX                          C2PIDX
     D**********    CMQPTY                          C2QPTY
     D**********    CMDCTP                          C2DCTP
     D**********    CMIPGM                          C2IPGM
     D**********    CMSMCD                          C2SMCD
     D**********    CMFL01                          C2FL01
     D**********    CMFL02                          C2FL02
     D**********    CMFL03                          C2FL03
     D**********    CMFL04                          C2FL04
     D**********    CMFL05                          C2FL05
     D**********    CMRMCD                          C2RMCD
     D**********    CMIDT                           C2IDT
     D*** End dlt ***** 11/26/02 **************************************
     D*** END ADD *** 12/06/99 ****************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEG ADD *** 09/02/98 *************************************
     D*
     D*** KEY TYPE AND ADDL KEYS FOR WC/MC
     D*
     D                 DS
     D  XXTST                  1     40
     D  XXKY                  17     21
     D  XXWC                  17     40
     D  XXMC                  22     40
     D*** END ADD *** 09/02/98 *************************************
     D*****************************************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *=======================================================================*
      * KLIST Definitions                                                     *
      *=======================================================================*

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*================================================================
     C*** BEG DEL *** 09/02/98 *************************************
     C********
     C***REMARK DETAIL FILE - From key
     C********
     C********   CMKYFR    KLIST
     C********             KFLD           QQFKEY
     C********
     C*  REMARK DETAIL FILE - To key
     C*
     C     CMKYTO        KLIST
     C                   KFLD                    QQTKEY
     C*
     C     CMKYT1        KLIST
     C                   KFLD                    QQTKEY
     C                   KFLD                    XHSEQ#
     C*** END DEL *** 09/02/98 *************************************
     C*** BEG ADD *** 09/02/98 *************************************
     C*
     C*  REMARK DETAIL FILE - From key
     C*
     C     CMKMFR        KLIST
     C                   KFLD                    QMFKEY           21
     C*
     C     CMKWFR        KLIST
     C                   KFLD                    QWFKEY           16
     C*
     C*  REMARK DETAIL FILE - To key
     C*
     C     CMKMTO        KLIST
     C                   KFLD                    QMTKEY           21
     C*
     C     CMKMT1        KLIST
     C                   KFLD                    QMTKEY
     C                   KFLD                    XHSEQ#
     C*
     C     CMKWTO        KLIST
     C                   KFLD                    QWTKEY           16
     C*
     C     CMKWT1        KLIST
     C                   KFLD                    QWTKEY
     C                   KFLD                    XHSEQ#
     C*** END ADD *** 09/02/98 *************************************
     C*** BEGIN ADD *** 12/06/99 **************************************
     C*
     C     CMKY1         KLIST
     C                   KFLD                    QMFKEY
     C                   KFLD                    QMFSEQ
     C*
     C     CMKY2         KLIST
     C                   KFLD                    QWFKEY
     C                   KFLD                    QMFSEQ
     C     *LIKE         DEFINE    CMSEQ#        QMFSEQ
     C*** END ADD *** 12/06/99 ****************************************
     C*** BEGIN ADD *** 04/24/07 **************************************
     C     *LIKE         DEFINE    CMKEY         ##FKEY
     C     *LIKE         DEFINE    CMSEQ#        ##FSEQ
     C     *LIKE         DEFINE    CMKEY         ##TKEY
     C     *LIKE         DEFINE    CMSEQ#        ##TSEQ
     C     *LIKE         DEFINE    CMSTAT        ##CALL
     C*** END   ADD *** 04/24/07 **************************************
     C*** BEGIN ADD *** 11/29/04 **************************************
     C*
     C* ADDITIONAL INFO FILE - FROM KEY
     C*
     C     AIKYFR        KLIST
     C                   KFLD                    QQFKEY
     C                   KFLD                    QMFSEQ
     C*** END   ADD *** 11/29/04 **************************************
     C*
     C*  ADDITIONAL REMARKS FILE - FROM KEY
     C*
     C     ARKYFR        KLIST
     C                   KFLD                    QQFKEY
     C*** BEGIN CHG *** 12/06/99 **************************************
     C********             KFLD           CMSEQ#
     C                   KFLD                    QMFSEQ
     C*** END CHG *** 12/06/99 ****************************************
     C*
     C*  ADDITIONAL REMARKS FILE - TO KEY
     C*
     C     ARKYTO        KLIST
     C                   KFLD                    QQTKEY
     C                   KFLD                    XHSEQ#
     C*
     C*  CUSTOM GROUP ID - FROM KEY
     C*
     C     HDKYFR        KLIST
     C                   KFLD                    QQFKEY
     C*
     C*  CUSTOM GROUP ID - TO KEY
     C*
     C     HDKYTO        KLIST
     C                   KFLD                    QQTKEY
     C*
     C*==ADDITIONAL DETAIL FROM KEY====================================
     C     DLKYFR        KLIST
     C                   KFLD                    QQFKEY
     C*** BEGIN CHG *** 12/06/99 **************************************
     C********             KFLD           CMSEQ#
     C                   KFLD                    QMFSEQ
     C*** END CHG *** 12/06/99 ****************************************
      *=======================================================================*
      * PLIST Definitions                                                     *
      *=======================================================================*
      *
      ** Date check/conversion parameters
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ** Entry parameters
     C     *ENTRY        PLIST
     C                   PARM                    QQFKEY
     C                   PARM                    QQTKEY
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQRTCD            1
      *=======================================================================*
      * *LIKE DEFN Definitions                                                *
      *=======================================================================*
     C     *LIKE         DEFINE    CMKEY         QQFKEY
     C     *LIKE         DEFINE    CMKEY         QQTKEY
     C     *LIKE         DEFINE    CMSEQ#        XHSEQ#
      *=======================================================================*
      * Main                                                                  *
      *=======================================================================*
     C*** BEGIN ADD *** 11/29/04 **************************************
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C*** END   ADD *** 11/29/04 **************************************
     C     QQFKEY        IFEQ      QQTKEY
     C                   GOTO      TERM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQRTCD        XXKTYP            1
     C                   MOVE      QQMODE        XXMODE            1
     C                   Z-ADD     *ZEROS        RCNT              3 0
     C                   Z-ADD     *ZEROS        ACNT              3 0
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ************************************
     C********           MOVE      '0'           XXPRT
     C********     ORKEY         CHAIN     POR01                              60
     C********     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C********           EXCEPT    ROUTE
     C********           END
      ***** End Del ***** 08/26/14 ************************************
     C*
     C*   PRINT PAGE HEADING
     C                   EXCEPT    HEAD
     C*
     C*** BEG ADD *** 09/02/98 *************************************
     C*   BASED ON DECISION FROM FA703R, DETERMINE WC/MC KEY TYPE
     C*   TO USE.
     C*
     C                   SELECT
     C     XXKTYP        WHENEQ    'M'
     C                   MOVE      *ON           *IN10
     C                   MOVEL(P)  QQFKEY        QMFKEY
     C                   MOVEL(P)  QQTKEY        QMTKEY
     C                   OPEN      SDCMLN
     C     XXKTYP        WHENEQ    'W'
     C                   MOVE      *OFF          *IN10
     C                   MOVEL(P)  QQFKEY        QWFKEY
     C                   MOVEL(P)  QQTKEY        QWTKEY
     C                   OPEN      SDCMLO
     C                   OTHER
     C                   MOVEL     QQFKEY        XXTST
     C     XXKY          IFEQ      *BLANKS
     C                   MOVE      *OFF          *IN10
     C                   MOVEL(P)  QQFKEY        QWFKEY
     C                   MOVEL(P)  QQTKEY        QWTKEY
     C                   OPEN      SDCMLO
     C                   ELSE
     C                   MOVE      *ON           *IN10
     C                   MOVEL(P)  QQFKEY        QMFKEY
     C                   MOVEL(P)  QQTKEY        QMTKEY
     C                   OPEN      SDCMLN
     C                   ENDIF
     C                   ENDSL
     C*** END ADD *** 09/02/98 *************************************
     C*
     C*   IF 'TO' MAJOR REMARK EXISTS, WRITE ALL 'FROM' MAJOR REMARKS
     C*     TO 'TO' MAJOR REMARKS STARTING WITH THE NEXT AVAILABLE
     C*     SEQUENCE NUMBER
     C*
     C*** BEG CHG *** 09/02/98 *************************************
     C   10CMKMTO        CHAIN(N)  PCM01                              60
     C  N10CMKWTO        CHAIN(N)  PCM02                              60
     C*** END CHG *** 09/02/98 *************************************
     C     *IN60         IFEQ      '0'
     C*
     C*******************  EXCPTRCM01                      DEL 09/02/98
     C*
     C*    GET NEXT available seq# for TO KEY (store seq# in variable)
     C*
     C*** BEGIN DEL *** 12/06/99 **************************************
     C********             Z-ADD*HIVAL    XHSEQ#
     C********
     C****BEG*CHG *** 09/02/98 *************************************
     C********   CMKYT1    SETGTPCM03
     C********   CMKYTO    REDPEPCM03               N    60
     C********   *IN60     IFEQ '0'
     C********** QQTKEY    ANDEQCMKEY
     C**********           Z-ADD*ZEROS    XHSEQ#
     C********   CMSEQ#    ADD  1         XHSEQ#
     C**********           EXCPTRCM01
     C****END*CHG *** 09/02/98 *************************************
     C********             ELSE
     C********             Z-ADD*ZEROS    XHSEQ#
     C********             END
     C*** END DEL *** 12/06/99 ****************************************
     C*
     C* Transfer all major remarks
     C*
     C*** BEG CHG *** 09/02/98 *************************************
     C   10CMKMFR        CHAIN     PCM01                              60
     C  N10CMKWFR        CHAIN     PCM02                              60
     C*** END CHG *** 09/02/98 *************************************
     C     *IN60         DOWEQ     '0'
     C*** Begin Chg *** 12/03/02 **************************************
     C*** BEGIN ADD *** 12/06/99 **************************************
     C***                  Z-ADDCMSEQ#    QMFSEQ
     C***                  Z-ADD*HIVAL    XHSEQ#
     C***        CMKYT1    SETGTPCM03
     C***        CMKYTO    REDPEPCM03               N    60
     C***        *IN60     IFEQ '0'
     C***        C2SEQ#    ADD  1         XHSEQ#
     C***                  ELSE
     C***                  Z-ADD*ZEROS    XHSEQ#
     C***                  END
     C*** END ADD *** 12/06/99 ****************************************
     C*** End   Chg *** 12/03/02 **************************************
     C                   Z-ADD     CMSEQ#        QMFSEQ
     C*** BEGIN DEL *** 09/05/03 **************************************
     C**********           Z-ADD*HIVAL    XHSEQ#
     C********** CMKYT1    SETGTPCM03
     C********** CMKYTO    REDPEPCM03               N    60
     C********** *IN60     IFEQ '0'
      *** BEGIN ADD 08/21/03 ******************************************
     C**********           MOVE C2SEQ#    XHSEQ#
      *** END   ADD 08/21/03 ******************************************
     C********** *IN99     DOUEQ*ON
      *** BEGIN DEL 08/21/03 ******************************************
      *** Begin Add 07/30/03 ******************************************
     C**N99*****           MOVE C2SEQ#    XHSEQ#
      *** End   Add 07/30/03 ******************************************
      *** END   DEL 08/21/03 ******************************************
     C********** CMKYT1    CHAINPCM03                99
     C**N99***** C2SEQ#    ADD  1         XHSEQ#
     C**********           ENDDO
     C**********           END
     C*** END   DEL *** 09/05/03 **************************************
     C*** BEG ADD *** 09/02/98 *************************************
     C                   EXSR      TEST
     C     *IN11         CABEQ     *ON           NXTREC
     C*** END ADD *** 09/02/98 *************************************
     C*** BEGIN ADD *** 09/05/03 **************************************
     C*
     C* CALL DA700R TO WRITE SDCMP AND GET THE SEQ#
     C*
     C                   MOVEL(P)  CMCOMM        QQCOMM
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQCDAT
     C*
     C     CMKEY         IFEQ      CMFOLD
     C                   MOVEL(P)  QQTKEY        QQFOLD
     C                   ELSE
     C                   MOVEL(P)  CMFOLD        QQFOLD
     C                   ENDIF
     C*
     C                   MOVE      CMSECL        QQSECL
     C                   MOVE(P)   CMCOMM        QQCOM2
     C                   MOVE      CMPIDX        QQPIDX
     C                   MOVE      CMTRDT        QQTRDT
     C                   MOVE      CMTRTM        QQTRTM
     C                   MOVE      CMDCDT        QQDCDT
     C                   MOVE      CMIDT         QQIDT
     C*** BEGIN ADD *** 11/29/04 **************************************
     C     ##DEIF        IFEQ      'Y'
     C     AIKYFR        CHAIN(N)  PAI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     AIVCFN        QQVCFN
     C                   MOVEL     AIDPGM        QQDPGM
     C     AIDPGM        IFNE      *BLANKS
     C                   MOVEL     QQTKEY        QQDKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       QQVCFN
     C                   MOVEL     '*NO'         QQDPGM
     C                   MOVEL     *BLANKS       QQDKEY
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       QQVCFN
     C                   MOVEL     '*NO'         QQDPGM
     C                   MOVEL     *BLANKS       QQDKEY
     C                   ENDIF
     C*** END   ADD *** 11/29/04 **************************************
     C*
     C* CALL API TO WRITE DARTS RECORD
     C*
     C                   CALL      'DA700R'
     C                   PARM      QQTKEY        ##KEY            40
     C                   PARM                    QQCOMM          256
     C                   PARM                    QQCDAT            8
     C                   PARM      CMDCID        QQDCID           12
     C                   PARM      CMPNAM        QQPNAM           10
     C                   PARM                    QQSECL            3
     C                   PARM      CMUSER        QQUSER           10
     C                   PARM      Q1USER        QQPGM            10
     C                   PARM                    QQFOLD           80
     C                   PARM      CMDIFL        QQDIFL            1
     C                   PARM      Q1PGM         QQIPGM           10
     C                   PARM      CMFL01        QQFL01            1
     C                   PARM      CMFL02        QQFL02            1
     C                   PARM      CMRMCD        QQRMCD            5
     C                   PARM      *BLANKS       QQSEQ#            5
     C                   PARM      CMSBYL        QQSBYL            1            16
     C                   PARM                    QQCOM2           19            17
     C                   PARM      CMAPID        QQAPID            2            18
     C                   PARM      CMIDOC        QQIDOC           12            19
     C                   PARM      CMIQUE        QQIQUE           10            20
     C                   PARM      CMIMGF        QQIMGF            1            21
     C                   PARM      CMIPTH        QQIPTH           10            22
     C                   PARM                    QQPIDX            5            23
     C                   PARM      CMQPTY        QQQPTY            1            24
     C                   PARM      CMDCTP        QQDCTP            5            25
     C                   PARM      CMCSID        QQCSID            2            26
     C                   PARM      CMCODE        QQCODE            2            27
     C                   PARM                    QQTRDT            7            28
     C                   PARM                    QQTRTM            6            29
     C                   PARM      CMSTAT        QQSTAT            1            30
     C                   PARM      CMIUSR        QQIUSR           10            31
     C                   PARM                    QQDCDT            7            32
     C                   PARM      CMFL03        QQFL03            1            33
     C                   PARM      CMFL04        QQFL04            1            34
     C                   PARM      CMFL05        QQFL05            1            35
     C                   PARM                    QQIDT             7            36
     C*** BEGIN ADD *** 11/29/04 **************************************
     C                   PARM                    QQVCFN            1            37
     C                   PARM                    QQDPGM           10            38
     C                   PARM                    QQDKEY           40            39
     C*** END   ADD *** 11/29/04 **************************************
     C*
     C                   MOVE      QQSEQ#        XHSEQ#
     C*
     C*** END   ADD *** 09/05/03 **************************************
     C*
     C*   Transfer all DARTS additional remarks
     C*
     C     ARKYFR        CHAIN     PAR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     QQTKEY        ARKEY
     C                   MOVEL     XHSEQ#        ARSEQ#
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   ADD       1             ACNT
     C                   WRITE     PAR01
     C*
     C     ARKYFR        READE     PAR01                                  60
     C                   END
     C*
     C*==TRANSFER ALL 'FROM' ADDITIONAL REMARKS========================
     C*
     C     CMFL05        IFEQ      'O'
     C     DLKYFR        CHAIN     PDL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     QQTKEY        DLKEY
     C                   MOVEL     XHSEQ#        DLSEQ#
     C                   MOVEL     Q1PGM         DLPGM
     C                   MOVE      DSTIME        DLTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLDT
     C*
     C                   WRITE     PDL01
     C*
     C     DLKYFR        READE     PDL01                                  60
     C                   END                                                     END DOW
     C                   END                                                     END IF
     C*** BEGIN ADD *** 04/24/07 **************************************
     C*
     C* TRANSFER SMCLP
     C*
     C                   CALL      'DA703R'
     C                   PARM      QQFKEY        ##FKEY
     C                   PARM      QMFSEQ        ##FSEQ
     C                   PARM      QQTKEY        ##TKEY
     C                   PARM      XHSEQ#        ##TSEQ
     C                   PARM      *BLANKS       ##CALL
     C*** END   ADD *** 04/24/07 **************************************
     C*
     C*** BEGIN ADD *** 08/26/98 ***********************************************
     C     CMKEY         IFEQ      CMFOLD
     C                   MOVEL     QQTKEY        CMFOLD
     C                   ENDIF                                                  CMKEY EQ CMFOLD
     C*** END   ADD *** 08/26/98 ***********************************************
     C*** ADD 11/26/99 ***** CALL TO PROCESS EXTERNAL IMAGE FILES ***
     C*
      ***  NO ERROR IF PGM DOESNT EXIST ***
     C     CMIMGF        IFNE      ' '
     C*** BEGIN ADD *** 01/08/01 ***********************************************
     C     CMIMGF        ANDNE     'W'
     C*** END   ADD *** 01/08/01 ***********************************************
     C                   CALL      'SC900C1'                            88
     C                   PARM                    CMIDOC
     C                   PARM                    CMFOLD
     C                   PARM                    QQFKEY
     C                   PARM                    CMSEQ#
     C                   PARM                    QQTKEY
     C                   PARM                    XHSEQ#
     C                   PARM                    XXRTNC            1
     C                   ENDIF
     C*
     C*** END ADD 11/26/99 ***** CALL TO PROCESS EXTERNAL IMAGE FILES *
     C*** BEGIN ADD *** 01/04/00 ***********************************************
     C*
     C*  Process Multiple Objects
     C*
     C     CMQPTY        IFEQ      'Y'
     C*
     C                   MOVE      XHSEQ#        QQTSEQ            5
     C                   MOVE      CMSEQ#        QQFSEQ            5
     C                   MOVE      'T'           QXMODE            1
     C*
     C                   CALL      'SC917R'
     C                   PARM                    QQFKEY
     C                   PARM                    QQFSEQ
     C                   PARM                    QQTKEY
     C                   PARM                    QQTSEQ
     C                   PARM                    QXMODE
     C*
     C                   ENDIF
     C*** END   ADD *** 01/04/00 ***********************************************
     C*
     C*** BEGIN DEL *** 09/05/03 **************************************
     C**********           MOVELQQTKEY    CMKEY
     C**********           MOVELXHSEQ#    CMSEQ#
     C**********           MOVELQ1PGM     CMPGM
     C**********           MOVE DSTIME    CMTIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     CMDT
     C*** END   DEL *** 09/05/03 **************************************
     C*
     C                   ADD       1             RCNT
     C*** BEGIN DEL *** 12/06/99 **************************************
     C****BEG*CHG *** 09/02/98 *************************************
     C***10***             WRITEPCM01
     C******BEGIN ADD **** 07/01/99 ***********************************
     C****BEGIN CHG *** 07/05/99 **************************************
     C**********           FEOD SDCMLN
     C***10***             FEOD SDCMLN
     C****END*  CHG *** 07/05/99 **************************************
     C******END   ADD **** 07/01/99 ***********************************
     C**N10***             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C*** BEGIN CHG *** 07/05/99 **************************************
     C**********           FEOD SDCMLO
     C**N10***             FEOD SDCMLO
     C****END*  CHG *** 07/05/99 **************************************
     C******END   ADD **** 07/01/99 ***********************************
     C****END*CHG *** 09/02/98 *************************************
     C********
     C********             ADD  1         XHSEQ#
     C*** END DEL *** 12/06/99 ****************************************
     C*** BEGIN ADD *** 12/06/99 **************************************
     C*** BEGIN DEL *** 09/05/03 **************************************
     C**********           WRITEPCM04
     C**********           FEOD SDCMP
     C*** END   DEL *** 09/05/03 **************************************
     C*** END ADD *** 12/06/99 ****************************************
     C     NXTREC        TAG                                                    ADD 09/02/98
     C*** BEG CHG *** 09/02/98 *************************************
     C   10CMKMFR        READE     PCM01                                  60
     C  N10CMKWFR        READE     PCM02                                  60
     C*** END CHG *** 09/02/98 *************************************
     C                   END                                                    END DOW
     C*
     C*    Delete all FROM remarks
     C*
     C*** BEG CHG *** 09/02/98 *************************************
     C   10CMKMFR        CHAIN     PCM01                              60
     C  N10CMKWFR        CHAIN     PCM02                              60
     C*** END CHG *** 09/02/98 *************************************
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 12/06/99 **************************************
     C                   Z-ADD     CMSEQ#        QMFSEQ
     C*** END ADD *** 12/06/99 ****************************************
     C*
     C*    Delete all FROM addt'l remarks
     C*
     C     ARKYFR        CHAIN     PAR01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAR01
     C     ARKYFR        READE     PAR01                                  60
     C                   END
     C*
     C*==DELETE ALL 'FROM' ADDITIONAL REMARKS==========================
     C*
     C     CMFL05        IFEQ      'O'
     C     DLKYFR        CHAIN     PDL01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDL01
     C     DLKYFR        READE     PDL01                                  60
     C                   END                                                     END DOW
     C                   END                                                     END IF
     C*** BEGIN ADD *** 04/24/07 **************************************
     C*
     C* DELETE SMCLP
     C*
     C                   CALL      'DA703R'
     C                   PARM      QQFKEY        ##FKEY
     C                   PARM      QMFSEQ        ##FSEQ
     C                   PARM      *BLANKS       ##TKEY
     C                   PARM      *ZEROS        ##TSEQ
     C                   PARM      'D'           ##CALL
     C*** END   ADD *** 04/24/07 **************************************
     C*** BEGIN ADD *** 11/29/04 **************************************
     C*
     C* DELETE THE ADDITIONAL INFO
     C*
     C     ##DEIF        IFEQ      'Y'
     C     AIKYFR        CHAIN     PAI01                              50
     C     *IN50         DOWEQ     '0'
     C                   DELETE    PAI01
     C     AIKYFR        READE     PAI01                                  50
     C                   ENDDO                                                   END *IN50 DOWEQ '0'
     C                   ENDIF
     C*** END   ADD *** 11/29/04 **************************************
     C*
     C*** BEG CHG *** 09/02/98 *************************************
     C   10              DELETE    PCM01
     C  N10              DELETE    PCM02
     C*
     C   10CMKMFR        READE     PCM01                                  60
     C  N10CMKWFR        READE     PCM02                                  60
     C*** END CHG *** 09/02/98 *************************************
     C                   END                                                    END DOW
     C*
     C*   Else 'TO' Major Remark does not exist so all 'FROM'
     C*      remarks and additional remarks are updated with the
     C*      'TO' value.
     C*
     C                   ELSE
     C*** BEG CHG *** 09/02/98 *************************************
     C   10CMKMFR        CHAIN     PCM01                              60
     C  N10CMKWFR        CHAIN     PCM02                              60
     C*** END CHG *** 09/02/98 *************************************
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 12/06/99 **************************************
     C                   Z-ADD     CMSEQ#        QMFSEQ
     C*** END ADD *** 12/06/99 ****************************************
     C*
     C*** BEG ADD *** 09/02/98 *************************************
     C                   EXSR      TEST
     C*** BEGIN CHG *** 07/05/99 **************************************
     C*********  *IN11     CABEQ*ON       NXTREC
     C     *IN11         CABEQ     *ON           NXTRC2
     C*** END   CHG *** 07/05/99 **************************************
     C*** END ADD *** 09/02/98 *************************************
     C*   Transfer all DARTS additional remarks
     C*
     C     ARKYFR        CHAIN     PAR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     QQTKEY        ARKEY
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   ADD       1             ACNT
     C                   UPDATE    PAR01
     C*
     C     ARKYFR        READE     PAR01                                  60
     C                   END
     C*
     C*==TRANSFER ALL 'FROM' ADDITIONAL REMARKS========================
     C*
     C     CMFL05        IFEQ      'O'
     C     DLKYFR        CHAIN     PDL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     QQTKEY        DLKEY
     C                   MOVEL     Q1PGM         DLPGM
     C                   MOVE      DSTIME        DLTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DLDT
     C*
     C                   UPDATE    PDL01
     C*
     C     DLKYFR        READE     PDL01                                  60
     C                   END                                                        END DOW
     C                   END                                                     END IF
     C*** BEGIN ADD *** 04/24/07 **************************************
     C*
     C* TRANSFER SMCLP
     C*
     C                   CALL      'DA703R'
     C                   PARM      QQFKEY        ##FKEY
     C                   PARM      QMFSEQ        ##FSEQ
     C                   PARM      QQTKEY        ##TKEY
     C                   PARM      QMFSEQ        ##TSEQ
     C                   PARM      *BLANKS       ##CALL
     C*** END   ADD *** 04/24/07 **************************************
     C*** BEGIN ADD *** 11/29/04 **************************************
     C*
     C* UPDATE THE ADDITIONAL INFO
     C*
     C     ##DEIF        IFEQ      'Y'
     C     AIKYFR        CHAIN     PAI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     QQTKEY        AIKEY
     C     AIDPGM        IFNE      *BLANKS
     C                   MOVEL     QQTKEY        AIDKEY
     C                   ENDIF
     C                   MOVEL     Q1PGM         AIPGM
     C                   MOVE      DSTIME        AITIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C                   UPDATE    PAI01
     C                   ENDIF                                                   END *IN60 IFEQ '0'
     C                   ENDIF
     C*** END   ADD *** 11/29/04 **************************************
     C*** BEGIN ADD *** 02/29/00 **************************************
     C     CMKEY         IFEQ      CMFOLD
     C                   MOVEL     QQTKEY        CMFOLD
     C                   ENDIF                                                  CMKEY EQ CMFOLD
     C*
      ***  NO ERROR IF PGM DOESNT EXIST ***
     C     CMIMGF        IFNE      ' '
     C*** BEGIN ADD *** 01/08/01 ***********************************************
     C     CMIMGF        ANDNE     'W'
     C*** END   ADD *** 01/08/01 ***********************************************
     C                   CALL      'SC900C1'                            88
     C                   PARM                    CMIDOC
     C                   PARM                    CMFOLD
     C                   PARM                    QQFKEY
     C                   PARM                    CMSEQ#
     C                   PARM                    QQTKEY
     C                   PARM                    CMSEQ#
     C                   PARM                    XXRTNC            1
     C                   ENDIF
     C*
     C*** END ADD *** 02/29/00 ****************************************
     C*** BEGIN ADD *** 03/30/07 ***********************************************
     C*
     C*  Process Multiple Objects
     C*
     C     CMQPTY        IFEQ      'Y'
     C*
     C                   MOVE      CMSEQ#        QQTSEQ
     C                   MOVE      CMSEQ#        QQFSEQ
     C                   MOVE      'T'           QXMODE
     C*
     C                   CALL      'SC917R'
     C                   PARM                    QQFKEY
     C                   PARM                    QQFSEQ
     C                   PARM                    QQTKEY
     C                   PARM                    QQTSEQ
     C                   PARM                    QXMODE
     C*
     C                   ENDIF
     C*** END   ADD *** 03/30/07 ***********************************************
     C*
     C                   MOVEL     QQTKEY        CMKEY
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVE      DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   ADD       1             RCNT
     C*** BEG CHG *** 09/02/98 *************************************
     C   10              UPDATE    PCM01
     C  N10              UPDATE    PCM02
     C*** BEGIN ADD *** 07/05/99 **************************************
     C     NXTRC2        TAG
     C*** END   ADD *** 07/05/99 **************************************
     C*
     C   10CMKMFR        READE     PCM01                                  60
     C  N10CMKWFR        READE     PCM02                                  60
     C*** END CHG *** 09/02/98 *************************************
     C                   END                                                    END PCM01
     C                   END                                                    END IF
     C*
     C*  Output FROM and TO values with remark and addt'l remark totals
     C*
     C                   EXCEPT    RMK
     C*
     C*  Transfer all Custom Group ID Data Records - Format A
     C*    If both 'TO' and 'FROM' key exist, print error message
     C*      and don't transfer data.
     C*
     C     HDKYTO        CHAIN     PHD01                              60
     C     *IN60         IFEQ      '0'
     C     HDKYFR        CHAIN     PHD01                              50
     C     *IN50         IFEQ      '0'
     C                   EXCEPT    ERRMSG
     C                   EXCEPT    FROM
     C     *IN50         DOWEQ     '0'
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   Z-ADD     HDUD01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXUD01            6 0
     C*
     C                   Z-ADD     HDUD02        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXUD02            6 0
     C*
     C                   Z-ADD     HDUD03        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXUD03            6 0
     C*
     C                   Z-ADD     HDUD04        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXUD04            6 0
     C*
     C                   Z-ADD     HDUD05        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXUD05            6 0
     C*
     C                   EXCEPT    CGIDA
     C     HDKYFR        READE     PHD01                                  50
     C                   END
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    TO
     C*
     C     HDKYTO        CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    CGIDA
     C     HDKYTO        READE     PHD01                                  60
     C                   END
     C*
     C                   END
     C*
     C*  Else if 'TO' key does not exist, update 'FROM' key value
     C*      with 'TO' key value.
     C*
     C                   ELSE
     C     HDKYFR        CHAIN     PHD01                              50
     C  N50              EXCEPT    MSG
     C     *IN50         DOWEQ     '0'
     C                   MOVEL     QQTKEY        HDKEY
     C                   MOVEL     Q1PGM         HDPGM
     C                   MOVE      DSTIME        HDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HDDT
     C*
     C                   UPDATE    PHD01
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    CGIDA
     C*
     C     HDKYFR        READE     PHD01                                  50
     C                   END
     C                   END
     C*
     C*  Transfer all Custom Group ID Data Records - Format B
     C*    If both 'TO' and 'FROM' key exist, print error message
     C*      and don't transfer data.
     C*
     C     HDKYTO        CHAIN     PH101                              60
     C     *IN60         IFEQ      '0'
     C     HDKYFR        CHAIN     PH101                              50
     C     *IN50         IFEQ      '0'
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    ERRMSG
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    FROM
     C     *IN50         DOWEQ     '0'
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    CGIDB
     C     HDKYFR        READE     PH101                                  50
     C                   END
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    TO
     C*
     C     HDKYTO        CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    CGIDB
     C     HDKYTO        READE     PH101                                  60
     C                   END
     C*
     C                   END
     C*
     C*  Else if 'TO' key does not exist, update 'FROM' key value
     C*      with 'TO' key value.
     C*
     C                   ELSE
     C     HDKYFR        CHAIN     PH101                              50
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C  N50              EXCEPT    MSG
     C     *IN50         DOWEQ     '0'
     C                   MOVEL     QQTKEY        H1KEY
     C                   MOVEL     Q1PGM         H1PGM
     C                   MOVE      DSTIME        H1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         H1DT
     C*
     C                   UPDATE    PH101
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    CGIDB
     C*
     C     HDKYFR        READE     PH101                                  50
     C                   END
     C                   END
     C*
     C*** END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C     TEST          BEGSR
     C*
     C                   MOVE      *OFF          *IN11
     C                   MOVEL     CMKEY         XXTST
     C*
     C*** PROCESS ADDTL INFO FOR MANG CARE KEY
     C*
     C     *IN10         IFEQ      *ON
     C     XXMC          IFNE      *BLANKS
     C                   MOVE      *BLANKS       Q2TKEY           40
     C     QQTKEY        CAT       XXMC:0        Q2TKEY
     C                   EXCEPT    RCM01
     C                   CALL      'SC900R01'
     C                   PARM      CMKEY         Q2FKEY           40
     C                   PARM                    Q2TKEY
     C                   PARM                    QQQUE
     C                   PARM                    QQMODE
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
     C*** PROCESS ADDTL INFO FOR WORK COMP KEY
     C*
     C                   ELSE
     C     XXWC          IFNE      *BLANKS
     C                   MOVE      *BLANKS       Q2TKEY
     C     QQTKEY        CAT       XXWC:0        Q2TKEY
     C                   EXCEPT    RCM02
     C                   CALL      'SC900R01'
     C                   PARM      CMKEY         Q2FKEY
     C                   PARM                    Q2TKEY
     C                   PARM                    QQQUE
     C                   PARM                    QQMODE
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     O*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS, INC.'
     O****                               58 '1993 - 2000'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    120
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          114 'PAGE:'
     O                       PAGE          Z    120
     O          E            RMK            1
     O                                           72 '# OF REMARKS'
     O                                           91 '# OF ADDT''L'
     O          E            RMK            1
     O                                           72 'TRANSFERRED'
     O                                           89 'REMARKS'
     O          E            RMK            1
     O                                            5 'FROM:'
     O                       QQFKEY              48
     O                       RCNT          Z     67
     O                       ACNT          Z     87
     O*
     O          E            RMK            2
     O                                            5 '  TO:'
     O                       QQTKEY              48
     O*
     O          E            FROM           1
     O                                           10 'FROM:'
     O*
     O          E            TO             1
     O                                           10 'TO:  '
     O*
     O          E            CGIDA          1
     O                                           12 'CGID'
     O                                           21 'UA01'
     O                                           33 'UA02'
     O                                           45 'UA03'
     O                                           57 'UA04'
     O                                           69 'UA05'
     O                                           81 'UA06'
     O                                           93 'UA07'
     O                                          105 'UA08'
     O                                          117 'UA09'
     O                                          129 'UA10'
     O*
     O          E            CGIDA          2
     O                       HDCSID              11
     O                       HDUA01              24
     O                       HDUA02              36
     O                       HDUA03              48
     O                       HDUA04              60
     O                       HDUA05              72
     O                       HDUA06              84
     O                       HDUA07              96
     O                       HDUA08             108
     O                       HDUA09             120
     O                       HDUA10             132
     O*
     O          E            CGIDA          1
     O                                           21 'UD01'
     O                                           33 'UD02'
     O                                           45 'UD03'
     O                                           57 'UD04'
     O                                           69 'UD05'
     O                                           81 'UN01'
     O                                           93 'UN02'
     O                                          105 'UN03'
     O                                          117 'UN04'
     O                                          129 'UN05'
     O*
     O          E            CGIDA          2
     O                       XXUD01              23 '  /  /  '
     O                       XXUD02              35 '  /  /  '
     O                       XXUD03              47 '  /  /  '
     O                       XXUD04              59 '  /  /  '
     O                       XXUD05              71 '  /  /  '
     O                       HDUN01        J     83
     O                       HDUN02        J     95
     O                       HDUN03        J    107
     O                       HDUN04        J    119
     O                       HDUN05        J    131
     O*
     O          E            CGIDB          1
     O                                           12 'CGID'
     O                                           34 'UB01'
     O                                           75 'UB02'
     O                                          116 'UB03'
     O*
     O          E            CGIDB          2
     O                       H1CSID              11
     O                       H1UB01              50
     O                       H1UB02              91
     O                       H1UB03             132
     O*
     O          E            CGIDB          1
     O                                           34 'UB04'
     O                                           75 'UB05'
     O                                          116 'UB06'
     O*
     O          E            CGIDB          2
     O                       H1UB04              50
     O                       H1UB05              91
     O                       H1UB06             132
     O*
     O**** Message, If Custom Data was successfully transferred ****   *
     O*
     O          E            MSG            1
     O                                           32 '** THE FOLLOWING CUSTOM '
     O                                           56 'DATA WAS TRANSFERRED SUC'
     O                                           66 'CESSFULLY:'
     O*
     O**** Error Message, If FROM and TO Custom Data both exist   ***  *
     O*
     O          E            ERRMSG         1
     O                                           32 '** THE FOLLOWING CUSTOM '
     O                                           56 'DATA WAS NOT TRANSFERRED'
     O                                           57 ':'
     O*** BEG CHG *** 09/02/98 *************************************
     OPCM01     E            RCM01
     OPCM02     E            RCM02
     O*** END CHG *** 09/02/98 *************************************
