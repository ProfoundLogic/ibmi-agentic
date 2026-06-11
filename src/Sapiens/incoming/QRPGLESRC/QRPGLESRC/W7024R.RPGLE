     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7024R - GROUP CHECK - STOP/VOID/REVERSAL PROCESSING
     H* ***************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/29/11 C:277 SUPPORT 77320                                  *
     H*                Populate WDPAP initial fields                  *
     H*                                                               *
     H* 11/16/10  C:227 SUPPORT: 71687  WC025                         *
     H*                 FIX GROUP CHECK VOID ISSUE                    *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 08/09/07  C:191 PROJECT:WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*
     H* 11/21/03 C:15  Support: WC114/40021                           *
     H*                Added code to load RPCKTP.                     *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H* 09/21/01 C:53  Support: MC048/28910                           *
     H*                Add process date to parms and use it for *TRDT *
     H*                                                               *
     H* 06/27/01 C:62  Support: MC044/27991                           *
     H*                -CHAIN OUT TO WARPPM TO GET THE NETWORK FLAG.  *
     H*                -CLEAR OUT WC CHECK INFO IN FAEAP              *
     H*                                                               *
     H* 05/03/00 C:15  Support: WC034/20089                           *
     H*                Added the TRDT back to key list for WDRPP.     *
     H*                Added code to update the employer detail file  *
     H*                correctly.                                     *
     H*                                                               *
     H* 02/25/00 C:15  Project: WC577                                 *
     H*                Added code to load new maint fields added to   *
     H*                WARPP.                                         *
     H*                                                               *
     H*  11/23/99 C:15 Support: WC065/20751                           *
     H*                Chgd pgm to load the requester's user id into  *
     H*                the appropriate fields since this pgm is run   *
     H*                from the batch process and Q1USER is always    *
     H*                PDSPGMR.                                       *
     H*                                                               *
     H*  04/07/99 C:15 Support: WC090/17156                           *
     H*                Removed PATRDT from key list used to retrieve  *
     H*                records from WDRPP due to incorrect date being *
     H*                loaded into RPTRDT.                            *
     H*                                                               *
     H*  02/19/99 C:15 Support: WC090/16694                           *
     H*                Changed the spacing on the output to prevent   *
     H*                the emp# from overlaying the case#.            *
     H*                                                               *
     H*  01/25/99 C:58 PROJECT
     H*           Correct original transaction date to V3 Standards.
     H*
     H*  05/04/98 C:78 PROJECT                                        *
     H*                V3 STANDARDS.                                  *
     H*                                                               *
     H*  04/16/97 C:47 PROJECT                                        *
     H*                INSTALL RESERVE CUSHION FOR VOID/STOP/REVERSE  *
     H*                PROCESSING.                                    *
     H*                                                               *
     H*  09/03/96 C:47 PROJ: 68ANE                                    *
     H*                THIS MOD WILL VOID/STOP ALL TRANSACTIONS WITHIN*
     H*                A PARTICULAR GROUP CHECK AND THEN RECREATE WARPP
     H*                TRANSACTIONS ON PEND STATUS.                   *
     H*                                                               *
     H*  05/02/96 C:15 PROJ: 68AEF                                    *
     H*                MOVED A SECTION OF CODE WHICH UPDATES THE CLM  *
     H*                RESERVES TO BE PROCESSED AFTER THE VOID RECORD *
     H*                HAS BEEN WRITTEN.  THIS WILL PREVENT THE CLAIM *
     H*                FILE FROM BECOMIMG OUT OF BALANCE WITH THE     *
     H*                PAYMENT DETAIL FILE.                           *
     H*                                                               *
     H*  01/11/96 C:36 PROJECT: 68AEF                                 *
     H*                FIXED ROUTING INFORMATION TO ONLY PRINT IF     *
     H*                THERE IS STOP/VOID/REVERSAL ACTIVITY           *
     H*                                                               *
     H*  09/19/95 C:28 PROJECT: 68AEB                                 *
     H*                ADDED:
     H*                       'PAACCT' = BANK ACCT #                  *
     H*                                                               *
     H* ***************************************************************
     FWAGCL     UF   E           K DISK
     FWDPAL9    IF   E           K DISK
     FWDPAL1    IF A E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWDPAL     UF   E           K DISK
     F                                     RENAME(PPA01:PPA03)
     FSMCOL     IF   E           K DISK
     FWMCML1    UF   E           K DISK
     FSMORL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     FWDRPL     IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP04)
     F*** End add ***** 05/22/03 **************************************
     FWARPL     IF A E           K DISK
     FWDELL     UF   E           K DISK
     F**** WMHRL   IF  E           K        DISK
     FWTPSL     IF   E           K DISK
     F**** WMFNL   IF  E           K        DISK
     F**** WMEML   IF  E           K        DISK
     F**** WMMCL   IF  E           K        DISK
     F*** BEGIN ADD *** 06/27/01 **************************************
     FWARPPML   IF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
     FWMNRL     IF   E           K DISK
     F*** END   ADD *** 06/27/01 **************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *

     D*** END ADD *** 10/20/03 *******************************************  ****
     D*
     D*** Begin add *** 11/21/03 **************************************
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
     D*** End add ***** 11/21/03 **************************************
      *
     D                 DS
     D  CD                     1     60
     D                                     DIM(30)
     D  HRCD01                 1      2
     D  HRCD02                 3      4
     D  HRCD03                 5      6
     D  HRCD04                 7      8
     D  HRCD05                 9     10
     D  HRCD06                11     12
     D  HRCD07                13     14
     D  HRCD08                15     16
     D  HRCD09                17     18
     D  HRCD10                19     20
     D  HRCD11                21     22
     D  HRCD12                23     24
     D  HRCD13                25     26
     D  HRCD14                27     28
     D  HRCD15                29     30
     D  HRCD16                31     32
     D  HRCD17                33     34
     D  HRCD18                35     36
     D  HRCD19                37     38
     D  HRCD20                39     40
     D  HRCD21                41     42
     D  HRCD22                43     44
     D  HRCD23                45     46
     D  HRCD24                47     48
     D  HRCD25                49     50
     D  HRCD26                51     52
     D  HRCD27                53     54
     D  HRCD28                55     56
     D  HRCD29                57     58
     D  HRCD30                59     60
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
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
     D                 DS
     D  RT                     1     20
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
     D  AR                     1     90  2
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
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D GRPTTL          C                   CONST('GROUP CHECK - STOP/V-
     D                                     OID/REVERSAL LISTING')

     I*
     IPPA02
     I              PACO#                       P2CO#
     I              PAFND                       P2FND
     I              PAFYR                       P2FYR
     I              PACASE                      P2CASE
     I              PASEQ#                      P2SEQ#
     I              PACHK#                      P2CHK#
     I              PABASQ                      P2BASQ
     I              PATRDT                      P2TRDT
     I              PAPODT                      P2PODT
     I              PAFEIN                      P2FEIN
     I              PAMOD                       P2MOD
     I              PAAMT                       P2AMT
     I              PABR2                       P2BR2
     I              PABR7                       P2BR7
     I              PAFRDT                      P2FRDT
     I              PATODT                      P2TODT
     I              PAWKS                       P2WKS
     I              PADAYS                      P2DAYS
     I              PATRCD                      P2TRCD
     I              PATYPE                      P2TYPE
     I              PARECD                      P2RECD
     I              PARETP                      P2RETP
     I              PADESC                      P2DESC
     I              PAADST                      P2ADST
     I              PASTAT                      P2STAT
     I              PAUSER                      P2USER
     I              PAPGM                       P2PGM
     I              PATIME                      P2TIME
     I              PADT                        P2DT
     I              PABUL1                      P2BUL1
     I              PARCVD                      P2RCVD
     I              PADUE                       P2DUE
     I              PAPGTP                      P2PGTP
     I              PAEOB#                      P2EOB#
     I              PALTCD                      P2LTCD
     I              PAACCT                      P2ACCT
     I              PAOFF#                      P2OFF#
     I              PACKTP                      P2CKTP
     I              PARQST                      P2RQST
     I              PADPCD                      P2DPCD
     I              PADPCU                      P2DPCU
     I              PAFNAL                      P2FNAL
     I              PARCDT                      P2RCDT
     I              PARADJ                      P2RADJ
     I*** BEG ADD *** 11/16/10 ****************************************
     I              PAEOBS                      P2EOBS
     I*** END ADD *** 11/16/10 ****************************************
     I*** BEG ADD *** 08/29/11 ****************************************
     I              PAIUSR                      P2IUSR
     I              PAIPGM                      P2IPGM
     I              PAIDT                       P2IDT
     I              PAITIM                      P2ITIM
     I*** END ADD *** 08/29/11 ****************************************
     IPPA03
     I              PACO#                       P3CO#
     I              PAFND                       P3FND
     I              PAFYR                       P3FYR
     I              PACASE                      P3CASE
     I              PASEQ#                      P3SEQ#
     I              PACHK#                      P3CHK#
     I              PABASQ                      P3BASQ
     I              PATRDT                      P3TRDT
     I              PAPODT                      P3PODT
     I              PAFEIN                      P3FEIN
     I              PAMOD                       P3MOD
     I              PAAMT                       P3AMT
     I              PABR2                       P3BR2
     I              PABR7                       P3BR7
     I              PAFRDT                      P3FRDT
     I              PATODT                      P3TODT
     I              PAWKS                       P3WKS
     I              PADAYS                      P3DAYS
     I              PATRCD                      P3TRCD
     I              PATYPE                      P3TYPE
     I              PARECD                      P3RECD
     I              PARETP                      P3RETP
     I              PADESC                      P3DESC
     I              PAADST                      P3ADST
     I              PASTAT                      P3STAT
     I              PAUSER                      P3USER
     I              PAPGM                       P3PGM
     I              PATIME                      P3TIME
     I              PADT                        P3DT
     I              PABUL1                      P3BUL1
     I              PARCVD                      P3RCVD
     I              PADUE                       P3DUE
     I              PAPGTP                      P3PGTP
     I              PAEOB#                      P3EOB#
     I              PALTCD                      P3LTCD
     I              PAACCT                      P3ACCT
     I              PAOFF#                      P3OFF#
     I              PACKTP                      P3CKTP
     I              PARQST                      P3RQST
     I              PADPCD                      P3DPCD
     I              PADPCU                      P3DPCU
     I              PAFNAL                      P3FNAL
     I              PARCDT                      P3RCDT
     I              PARADJ                      P3RADJ
     I*** BEG ADD *** 11/16/10 ****************************************
     I              PAEOBS                      P3EOBS
     I*** END ADD *** 11/16/10 ****************************************
     I*
     IPRP02
     I              RPRMDS                      R2RMDS
     I              RPRMK1                      R2RMK1
     I              RPRMK2                      R2RMK2
     I              RPRMK3                      R2RMK3
     I              RPCKCN                      R2CKCN
     I              RPPNAM                      R2PNAM
     I              RPADR1                      R2ADR1
     I              RPADR2                      R2ADR2
     I              RPCTY                       R2CTY
     I              RPST                        R2ST
     I              RPZIP                       R2ZIP
     I*** BEG ADD *** 10/20/03 *******************************************  ****
     I              RPCTRY                      R2CTRY
     I              RPADR3                      R2ADR3
     I*** END ADD *** 10/20/03 *******************************************  ****
     I*
     I*** Begin add *** 05/22/03 **************************************
     IPRP04
     I              RPRMDS                      R2RMDS
     I              RPRMK1                      R2RMK1
     I              RPRMK2                      R2RMK2
     I              RPRMK3                      R2RMK3
     I              RPCKCN                      R2CKCN
     I              RPPNAM                      R2PNAM
     I              RPADR1                      R2ADR1
     I              RPADR2                      R2ADR2
     I              RPCTY                       R2CTY
     I              RPST                        R2ST
     I              RPZIP                       R2ZIP
     I*** BEG ADD *** 10/20/03 *******************************************  ****
     I              RPCTRY                      R2CTRY
     I              RPADR3                      R2ADR3
     I*** END ADD *** 10/20/03 *******************************************  ****
     I*** End add ***** 05/22/03 **************************************
     I*
     I*** BEGIN ADD *** 06/27/01 **************************************
     IPRP03
     I              RPCO#                       R3CO#
     I              RPFND                       R3FND
     I              RPFYR                       R3FYR
     I              RPCASE                      R3CASE
     I              RPSEQ#                      R3SEQ#
     I              RPTRDT                      R3TRDT
     I              RPFEIN                      R3FEIN
     I              RPMOD                       R3MOD
     I              RPAMT                       R3AMT
     I              RPBR2                       R3BR2
     I              RPBR7                       R3BR7
     I              RPNCCI                      R3NCCI
     I              RPFRDT                      R3FRDT
     I              RPTODT                      R3TODT
     I              RPWKS                       R3WKS
     I              RPDAYS                      R3DAYS
     I              RPTRCD                      R3TRCD
     I              RPTYPE                      R3TYPE
     I              RPRETP                      R3RETP
     I              RPRECD                      R3RECD
     I              RPRMDS                      R3RMDS
     I              RPRMK1                      R3RMK1
     I              RPRMK2                      R3RMK2
     I              RPRMK3                      R3RMK3
     I              RPDESC                      R3DESC
     I              RPCKCN                      R3CKCN
     I              RPPNAM                      R3PNAM
     I              RPADR1                      R3ADR1
     I              RPADR2                      R3ADR2
     I              RPCTY                       R3CTY
     I              RPST                        R3ST
     I              RPZIP                       R3ZIP
     I              RPSTAT                      R3STAT
     I              RPUSER                      R3USER
     I              RPPGM                       R3PGM
     I              RPTIME                      R3TIME
     I              RPDT                        R3DT
     I              RPBUL1                      R3BUL1
     I              RPCHK#                      R3CHK#
     I              RPEOB#                      R3EOB#
     I              RPPGTP                      R3PGTP
     I              RPOFF#                      R3OFF#
     I              RPCKTP                      R3CKTP
     I              RPACCT                      R3ACCT
     I              RPRCVD                      R3RCVD
     I              RPDUE                       R3DUE
     I              RPSTS                       R3STS
     I              RPSORT                      R3SORT
     I              RPSENT                      R3SENT
     I              RPDSEQ                      R3DSEQ
     I              RPRQST                      R3RQST
     I              RPLTCD                      R3LTCD
     I              RPNTWK                      R3NTWK
     I              RPEOBS                      R3EOBS
     I              RPFNAL                      R3FNAL
     I              RPIDT                       R3IDT
     I              RPITM                       R3ITM
     I              RPIPGM                      R3IPGM
     I              RPIUSR                      R3IUSR
     I              RPRDT                       R3RDT
     I              RPRTM                       R3RTM
     I              RPRPGM                      R3RPGM
     I              RPRUSR                      R3RUSR
     I*** END   ADD *** 06/27/01 **************************************
     I*** BEG ADD *** 10/20/03 *******************************************  ****
     I              RPCTRY                      R2CTRY
     I              RPADR3                      R2ADR3

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*** BEG ADD *** 09/21/01 **************************************
     C                   PARM                    QQDT              8
     C*** END ADD *** 09/21/01 **************************************
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
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    PAFYR
     C*
     C*  GROUP CHECK PENDING FILE
     C*
     C     GCKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  CLAIM CHECK PAYMENT DETAIL FILE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    GCCO#
     C                   KFLD                    GCTRDT
     C                   KFLD                    GCCHK#
     C*
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVBUL1
     C*
     C     PAKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVCASE
     C                   KFLD                    XXSEQ#
     C*
     C     PAKY3B        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVCASE
     C*
     C     PCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  PAYMENT STATUS CODE TABLE FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  REQUEST PAYMENT FILE - WARPP
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    X1SEQ#
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C*  PAYMENT DETAIL FILE - WDRPP
     C*
     C     R2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C*** BEGIN ADD *** 04/07/99 ***********************************************
     C                   KFLD                    PATRDT
     C*** END ADD ***** 04/07/99 ***********************************************
     C*** BEGIN DLT *** 04/07/99 ***********************************************
     C******************** KFLD           PATRDT
     C*** END DLT ***** 04/07/99 ***********************************************
     C*** Begin add *** 05/22/03 **************************************
     C*
     C     R4KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PASEQ#
     C*** End add ***** 05/22/03 **************************************
     C*
     C*** BEGIN ADD *** 06/27/01 **************************************
     C*
     C*  PROCESSED CHECK REQUEST FILE - WARPPM
     C*
     C     R3KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PAEOB#
     C*
     C*  NETWORK REVIEW ID FILE
     C*
     C     NRKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*** END   ADD *** 06/27/01 **************************************
     C*
     C*  KEY LIST FOR THE MCO PMT VALUE 1 DEFINITION FILE
     C*
     C**         HRKY      KLIST
     C**                   KFLD           XXCO#
     C**                   KFLD           PAFND
     C**                   KFLD           CMACDT
     C**
     C**         HRKY1     KLIST
     C**                   KFLD           XXCO#
     C**                   KFLD           PAFND
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C*          EMKY      KLIST
     C*                    KFLD           XXCO#
     C*                    KFLD           PAFND
     C*                    KFLD           XXEMP#
     C*                    KFLD           XXDIV
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C*          FNKY      KLIST
     C*                    KFLD           XXCO#
     C*                    KFLD           PAFND
     C*
     C*   KEY LIST FOR THE MCO COUNTY DEFN FILE
     C*
     C**         MCKY      KLIST
     C**                   KFLD           XXCO#
     C**                   KFLD           XXKEY
     C*
     C     *LIKE         DEFINE    PASEQ#        SVSEQ#
     C     *LIKE         DEFINE    PABUL1        SVBUL1
     C     *LIKE         DEFINE    PAFND         SVFND
     C     *LIKE         DEFINE    PAFYR         SVFYR
     C     *LIKE         DEFINE    PACASE        SVCASE
     C     *LIKE         DEFINE    PAPODT        SVPODT
     C     *LIKE         DEFINE    PAFEIN        SVFEIN
     C     *LIKE         DEFINE    PAMOD         SVMOD
     C     *LIKE         DEFINE    PAAMT         SVAMT
     C     *LIKE         DEFINE    PABR2         SVBR2
     C     *LIKE         DEFINE    PABR7         SVBR7
     C     *LIKE         DEFINE    PAFRDT        SVFRDT
     C     *LIKE         DEFINE    PATODT        SVTODT
     C     *LIKE         DEFINE    PAWKS         SVWKS
     C     *LIKE         DEFINE    PADAYS        SVDAYS
     C     *LIKE         DEFINE    PARECD        SVRECD
     C     *LIKE         DEFINE    PARETP        SVRETP
     C     *LIKE         DEFINE    PADESC        SVDESC
     C     *LIKE         DEFINE    PARCVD        SVRCVD
     C     *LIKE         DEFINE    PADUE         SVDUE
     C     *LIKE         DEFINE    PAPGTP        SVPGTP
     C     *LIKE         DEFINE    PAEOB#        SVEOB#
     C     *LIKE         DEFINE    PALTCD        SVLTCD
     C     *LIKE         DEFINE    PAACCT        SVACCT
     C     *LIKE         DEFINE    PAOFF#        SVOFF#
     C     *LIKE         DEFINE    PACKTP        SVCKTP
     C     *LIKE         DEFINE    PARQST        SVRQST
     C     *LIKE         DEFINE    PADPCD        SVDPCD
     C     *LIKE         DEFINE    PADPCU        SVDPCU
     C     *LIKE         DEFINE    PAFNAL        SVFNAL
     C     *LIKE         DEFINE    PARCDT        SVRCDT
     C     *LIKE         DEFINE    PARADJ        SVRADJ
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    PASEQ#        X1SEQ#
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMACDT        XXDT
     C***** BEGIN DLT ***** 01/25/99 **********************************
     C********** *LIKE     DEFN CMACDT    XXDATE
     C***** END DELETE **** 01/25/99 **********************************
     C     *LIKE         DEFINE    PSKEY         XXKEY
     C*** BEGIN ADD *** 06/27/01 **************************************
     C     *LIKE         DEFINE    PACO#         ##CO#
     C     *LIKE         DEFINE    PAFND         ##FND
     C     *LIKE         DEFINE    PAFYR         ##FYR
     C     *LIKE         DEFINE    PACASE        ##CASE
     C     *LIKE         DEFINE    PAEOB#        ##EOB#
     C     *LIKE         DEFINE    PACHK#        ##CHK#
     C     *LIKE         DEFINE    PADT          ##CHDT
     C     *LIKE         DEFINE    PAAMT         ##CHAT
     C     *LIKE         DEFINE    PAEOBS        ##RCN
     C*** BEG ADD *** 09/21/01 **************************************
     C     *LIKE         DEFINE    CMACDT        XXPRDT                         PROCESS DT
     C*** END ADD *** 09/21/01 **************************************
     C*
     C*  CALL SA001R TO SEE IF PROVIDER BILLS IS INSTALLED
     C*
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
     C*** END   ADD *** 06/27/01 **************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C*** BEG ADD *** 09/21/01 **************************************
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT
     C*** END ADD *** 09/21/01 **************************************
     C*
     C     *IN20         IFEQ      *OFF
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   MOVE      '1'           *IN20
     C                   END
     C*
     C*  READ THE GROUP CHECK PENDING FILE
     C*
     C     GCKY1         READE     PGC01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*  PRINT THE DISTRIBUTION INFORMATION
     C*
     C     *IN21         IFEQ      *OFF
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      GRPTTL        XXTITL           40
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *INOA
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C                   END
     C*
     C*  PRINT HEADINGS
     C*
     C                   EXCEPT    HEAD
     C*
     C*  READ ALL PAYMENT RECORDS FOR EACH PENDING GROUP
     C*  CHECK RECORD
     C*
     C     PAKY1         SETLL     PPA01
     C     PAKY1         READE     PPA01                                  60
     C     *IN60         DOWEQ     '0'
     C     PAFND         IFEQ      GCFND
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PAADST        IFEQ      ' '
     C     PAADST        OREQ      'V'
     C     PAADST        OREQ      'S'
     C                   EXSR      SVORVL
     C*
      *  CHECK PAYMENT FILE USING THE BUL# TO SEE IF THE
     C*  STOP/VOID/REVERSAL RECORD ALREADY EXISTS. IF NOT, THEN
     C*  GET THE NEXT VALID SEQUENCE NUMBER AND WRITE OUT THE
     C*  STOP/VOID/REVERSAL RECORD
     C*
     C     PAKY2         CHAIN     PPA02                              60
     C     PAKY2         READE     PPA02                                  60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *HIVAL        XXSEQ#
     C*
     C     PAKY3         SETGT     PPA03
     C     PAKY3B        READPE(N) PPA03                                  60
     C     *IN60         IFEQ      *OFF
     C     P3SEQ#        ADD       1             P2SEQ#
     C                   ELSE
     C                   Z-ADD     1             P2SEQ#
     C                   END
      *
     C                   Z-ADD     XXCO#         P2CO#
     C                   Z-ADD     SVFND         P2FND
     C                   Z-ADD     SVFYR         P2FYR
     C                   Z-ADD     SVCASE        P2CASE
     C                   Z-ADD     SVSEQ#        P2BASQ
     C                   Z-ADD     GCCHK#        P2CHK#
     C*** BEG CHG *** 09/21/01 **************************************
     C**********           Z-ADDXXDT      P2TRDT
     C                   Z-ADD     XXPRDT        P2TRDT
     C*** END CHG *** 09/21/01 **************************************
     C                   Z-ADD     SVPODT        P2PODT
     C                   Z-ADD     SVFEIN        P2FEIN
     C                   Z-ADD     SVMOD         P2MOD
     C                   Z-ADD     SVAMT         P2AMT
     C                   MOVE      SVBR2         P2BR2
     C                   MOVE      SVBR7         P2BR7
     C                   Z-ADD     SVFRDT        P2FRDT
     C                   Z-ADD     SVTODT        P2TODT
     C                   Z-ADD     SVWKS         P2WKS
     C                   Z-ADD     SVDAYS        P2DAYS
     C                   MOVE      GCTRCD        P2TRCD
     C                   MOVE      GCTRTY        P2TYPE
     C                   MOVE      SVRECD        P2RECD
     C                   MOVE      SVRETP        P2RETP
     C*
     C*  GET TRAN CODE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       P2DESC
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     P2TRCD        XXKEY
     C     PCKY1         CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PCDESC        P2DESC
     C                   END
     C                   MOVE      *BLANK        P2ADST
     C     GCTRTY        IFEQ      'Z'
     C                   MOVE      'Z'           P2ADST
     C                   END
     C                   MOVE      ' '           P2STAT
     C*** BEGIN CHG *** 11/23/99 **************************************
     C******************** MOVELQ1USER    P2USER
     C                   MOVEL     GCUSER        P2USER
     C*** END CHG ***** 11/23/99 **************************************
     C                   MOVEL     Q1PGM         P2PGM
     C                   Z-ADD     DSTIME        P2TIME
     C                   Z-ADD     XXDT          P2DT
     C                   Z-ADD     SVBUL1        P2BUL1
     C                   Z-ADD     SVRCVD        P2RCVD
     C                   Z-ADD     SVDUE         P2DUE
     C                   MOVE      SVPGTP        P2PGTP
     C                   Z-ADD     SVEOB#        P2EOB#
     C                   MOVE      SVLTCD        P2LTCD
     C                   MOVEL     SVACCT        P2ACCT
     C*** BEGIN CHG *** 11/23/99 **************************************
     C******************** MOVELQ1USER    P2RQST
     C                   MOVEL     GCUSER        P2RQST
     C*** END CHG ***** 11/23/99 **************************************
     C*****************    MOVELSVRQST    P2RQST
     C                   MOVEL     SVOFF#        P2OFF#
     C                   MOVEL     SVCKTP        P2CKTP
     C                   Z-ADD     SVDPCD        P2DPCD
     C                   MOVEL     SVDPCU        P2DPCU
     C                   MOVE      SVFNAL        P2FNAL
     C                   Z-ADD     SVRCDT        P2RCDT
     C                   MOVEL     SVRADJ        P2RADJ
      *
     C     GCRCR         IFEQ      'Y'
     C                   EXSR      SRRCR
     C                   END
     C*** BEGIN ADD *** 06/27/01 **************************************
     C     PAEOB#        IFNE      *ZEROS
     C     ##PRVB        ANDEQ     'Y'
     C                   EXSR      SRCLEA
     C                   ENDIF
     C*** END   ADD *** 06/27/01 **************************************
      *
     C*** BEGIN ADD *** 08/29/11 *****************************************
     C                   MOVEL     GCUSER        P2IUSR
     C                   MOVE      Q1PGM         P2IPGM
     C                   Z-ADD     XXDT          P2IDT
     C                   Z-ADD     DSTIME        P2ITIM
     C*** END ADD *** 08/29/11 *******************************************
     C                   WRITE     PPA02
     C*
     C*  UPDATE ORIGINAL PAYMENT RECORD WITH THE PROPER
     C*  ADJUSTED STATUS
     C*
     C                   Z-ADD     SVSEQ#        XXSEQ#
     C     PAKY3         CHAIN     PPA03                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      GCTRTY        P3ADST
     C*
     C     GCTRTY        IFEQ      'Z'
     C                   MOVE      *BLANK        P3ADST
     C                   END
     C*
     C*** BEGIN CHG *** 11/23/99 **************************************
     C******************** MOVELQ1USER    P3USER
     C                   MOVEL     GCUSER        P3USER
     C*** END CHG ***** 11/23/99 **************************************
     C                   MOVEL     Q1PGM         P3PGM
     C                   Z-ADD     DSTIME        P3TIME
     C                   Z-ADD     XXDT          P3DT
     C                   UPDATE    PPA03
     C                   END
     C*
     C*  IF THE CLAIM IS CLOSED, CALL SRPRCC TO WRITE THE RESERVE
     C*  HISTORY RECORD.
     C*
     C                   MOVE      'N'           XXRVCH            1
     C     CMKEY         CHAIN(N)  PCM01                              60
     C     *IN60         IFEQ      *OFF
     C     CMSTAT        ANDEQ     'C'
     C*
     C                   MOVEL     GCTRTY        QQTYPE
     C*
     C                   CALL      'SRPRCC'
     C                   PARM                    XXCO#
     C                   PARM                    SVFND
     C                   PARM                    SVFYR
     C                   PARM                    SVCASE
     C                   PARM                    SVRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    SVAMT
     C                   MOVE      'Y'           XXRVCH
     C                   ENDIF
      *
      * IF RESERVE CUSHION WAS NOT USED,
      * UPDATE THE EMP DETAIL FILE: OUTSTANDING RESERVES & TOT CLAIMS PAID
      *
     C*** BEGIN DLT *** 05/03/00 **************************************
     C********** XXRVCH    IFEQ 'N'
     C*** END DLT ***** 05/03/00 **************************************
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
      *
     C*** BEGIN CHG *** 05/03/00 **************************************
     C********** GCTRTY    IFEQ 'R'
     C     GCTRTY        IFEQ      'Z'
     C*** END CHG ***** 05/03/00 **************************************
     C                   ADD       PAAMT         ELTCLP                         TOT CLM PAID
     C*** BEGIN ADD *** 05/03/00 **************************************
     C     CMSTAT        IFEQ      'O'
     C*** END ADD ***** 05/03/00 **************************************
     C                   SUB       PAAMT         ELOUTS                         OUTSTAND RSV
     C*** BEGIN ADD *** 05/03/00 **************************************
     C                   END
     C*** END ADD ***** 05/03/00 **************************************
     C                   END
      *
     C     GCTRTY        IFEQ      'V'
     C     GCTRTY        OREQ      'S'
     C                   SUB       PAAMT         ELTCLP
     C*** BEGIN ADD *** 05/03/00 **************************************
     C     CMSTAT        IFEQ      'O'
     C*** END ADD ***** 05/03/00 **************************************
     C                   ADD       PAAMT         ELOUTS
     C*** BEGIN ADD *** 05/03/00 **************************************
     C                   END
     C*** END ADD ***** 05/03/00 **************************************
     C                   END
      *
     C                   UPDATE    PEL01
     C                   END
     C*** BEGIN DLT *** 05/03/00 **************************************
     C*******************  END                             XXRVCH = N
     C*** END DLT ***** 05/03/00 **************************************
     C*
     C*  GET CLAIM RECORD AND UPDATE BUL#, RESERVES, PAYMENTS,
     C*  AND PAYMENT COUNTER
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C     CMBUL1        ANDNE     PABUL1
     C                   Z-ADD     PABUL1        CMBUL1
     C*
     C*  UPDATE THE RESERVE & PAYMENT AMOUNTS IN THE CLAIM MASTER.
     C*
     C                   Z-ADD     1             X                 2 0
     C     PARECD        LOOKUP    RT(X)                                  60
     C  N60              Z-ADD     10            X
     C     GCTRTY        IFEQ      'Z'
     C                   SUB       PAAMT         RR(X)
     C                   ADD       PAAMT         AR(X)
     C                   ADD       1             CMPCTR
     C                   ELSE
     C                   ADD       PAAMT         RR(X)
     C                   SUB       PAAMT         AR(X)
     C                   SUB       1             CMPCTR
     C                   END
     C*** BEGIN CHG *** 11/23/99 **************************************
     C******************** MOVELQ1USER    CMUSER
     C                   MOVEL     GCUSER        CMUSER
     C*** END CHG ***** 11/23/99 **************************************
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     XXDT          CMDT
     C                   UPDATE    PCM01
     C                   ELSE
     C***************      EXCPTPCM01E
     C                   UNLOCK    WMCML1
     C                   END
     C*
     C*   PRINT DETAIL LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   Z-ADD     GCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C***** BEGIN CHG ***** 01/25/99 **********************************
     C                   MOVEL     SRDTO         XXDATE            8 0
     C**********           MOVELSRDTO     XXDATE
     C***** END CHANGE **** 01/25/99 **********************************
     C                   EXCEPT    DET
     C                   END                                                    *IN60 = 1
     C                   END
     C                   END
     C                   END
     C     PAKY1         READE     PPA01                                  60
     C                   ENDDO
     C*
     C*   PRINT TOTAL LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    CHKTT
     C*
     C*   UPDATE STATUS OF RECORD IN PENDING FILE
     C*
     C                   MOVE      'D'           GCSTAT
     C                   UPDATE    PGC01
     C*
     C     GCKY1         READE     PGC01                                  60
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       if *in21 = *off;
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*** BEGIN ADD *** 06/27/01 **************************************
     C*************************************************************
     C*  THIS SUBROUTINE WILL CLEAR FAEAP'S WCMS CHECK INFO
     C*************************************************************
     C     SRCLEA        BEGSR
     C*
     C                   MOVE      'P'           ##TYPE
     C*
     C     NRKY          CHAIN     PNR01                              99
     C*
     C     *IN99         IFEQ      *OFF
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
     C*
     C     R3KY          CHAIN     PRP03                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     PAEOBS        IFEQ      R3EOBS
     C     PACHK#        ANDEQ     R3CHK#
     C     PAFEIN        ANDEQ     R3FEIN
     C     PAMOD         ANDEQ     R3MOD
     C     PAAMT         ANDEQ     R3AMT
     C*
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     R3KY          READE     PRP03                                  99
     C                   ENDDO
     C*
     C                   CALL      'F7304R'
     C                   PARM      PACO#         ##CO#
     C                   PARM      PAFND         ##FND
     C                   PARM      PAFYR         ##FYR
     C                   PARM      PACASE        ##CASE
     C                   PARM      PAEOB#        ##EOB#
     C                   PARM      *ZEROS        ##CHK#
     C                   PARM      *ZEROS        ##CHDT
     C                   PARM      PAAMT         ##CHAT
     C                   PARM                    ##TYPE
     C                   PARM      PAEOBS        ##RCN
     C                   PARM      R3NTWK        ##NTWK            1
     C*
     C                   ENDSR
     C*** END   ADD *** 06/27/01 **************************************
     C* ***********************************************************
     C*  THIS SUBROUTINE WILL SAVE THE ORIGINAL VALUES FROM THE
     C*  PAYMENT FILE ON THE FIRST READ
     C* ***********************************************************
     C     SVORVL        BEGSR
     C                   Z-ADD     PASEQ#        SVSEQ#
     C                   Z-ADD     PAFND         SVFND
     C                   Z-ADD     PAFYR         SVFYR
     C                   Z-ADD     PACASE        SVCASE
     C                   Z-ADD     PAPODT        SVPODT
     C                   Z-ADD     PAFEIN        SVFEIN
     C                   Z-ADD     PAMOD         SVMOD
     C                   Z-ADD     PAAMT         SVAMT
     C                   MOVE      PABR2         SVBR2
     C                   MOVE      PABR7         SVBR7
     C                   Z-ADD     PAFRDT        SVFRDT
     C                   Z-ADD     PATODT        SVTODT
     C                   Z-ADD     PAWKS         SVWKS
     C                   Z-ADD     PADAYS        SVDAYS
     C                   MOVE      PARECD        SVRECD
     C                   MOVE      PARETP        SVRETP
     C                   MOVE      PADESC        SVDESC
     C                   Z-ADD     PABUL1        SVBUL1
     C                   Z-ADD     PARCVD        SVRCVD
     C                   Z-ADD     PADUE         SVDUE
     C                   MOVE      PAPGTP        SVPGTP
     C                   Z-ADD     PAEOB#        SVEOB#
     C                   MOVE      PALTCD        SVLTCD
     C                   MOVEL     PAOFF#        SVOFF#
     C                   MOVE      PAACCT        SVACCT
     C                   MOVEL     PACKTP        SVCKTP
     C** **************    MOVELPARQST    SVRQST
     C                   Z-ADD     PADPCD        SVDPCD
     C                   MOVEL     PADPCU        SVDPCU
     C                   MOVE      PAFNAL        SVFNAL
     C                   Z-ADD     PARCDT        SVRCDT
     C                   MOVEL     PARADJ        SVRADJ
     C                   ENDSR
      *
      * ***********************************************************
      *  THIS SUBROUTINE WILL RECREATE THE WARPP RECORD.
      * ***********************************************************
     C     SRRCR         BEGSR
      *
      *  WDRPP
     C*** Begin add *** 05/22/03 **************************************
     C     R4KY          CHAIN     PRP04                              60
     C     *IN60         IFEQ      *ON
     C*** End add ***** 05/22/03 **************************************
     C     R2KY          CHAIN     PRP02                              60
     C*** Begin add *** 05/22/03 **************************************
     C                   ENDIF
     C*** End add ***** 05/22/03 **************************************
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        R2RMDS
     C                   MOVE      *BLANK        R2RMK1
     C                   MOVE      *BLANK        R2RMK2
     C                   MOVE      *BLANK        R2RMK3
     C                   MOVE      *BLANK        R2CKCN
     C                   MOVE      *BLANK        R2PNAM
     C                   MOVE      *BLANK        R2ADR1
     C                   MOVE      *BLANK        R2ADR2
     C                   MOVE      *BLANK        R2CTY
     C                   MOVE      *BLANK        R2ST
     C                   Z-ADD     *ZERO         R2ZIP
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C                   MOVE      *BLANK        R2CTRY
     C                   MOVE      *BLANK        R2ADR3
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   END
      *
      *  WMCMP
     C     CMKEY         CHAIN(N)  PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CMNCCI
     C                   END
      *
      *  WTPSP
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     GCSTS         XXKEY
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       PSTYPE
     C                   MOVE      *BLANKS       PSSRT
     C                   END
      *
      *  GET THE NEXT AVAILABLE SEQ#
      *
     C                   Z-ADD     *HIVAL        X1SEQ#
     C     RPKY          SETGT     PRP01
     C     RPKY1         READPE    PRP01                                  60
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             RPSEQ#
     C                   ELSE
     C                   Z-ADD     1             RPSEQ#
     C                   END
      *
      *  LOAD THE REQUEST PAYMENT RECORD
      *
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C*** BEG CHG *** 09/21/01 **************************************
     C**********           Z-ADDXXDT      RPTRDT
     C                   Z-ADD     XXPRDT        RPTRDT
     C*** END CHG *** 09/21/01 **************************************
     C                   Z-ADD     PAFEIN        RPFEIN
     C                   Z-ADD     PAMOD         RPMOD
     C                   Z-ADD     PAAMT         RPAMT
     C                   MOVEL     PABR2         RPBR2
     C                   MOVEL     PABR7         RPBR7
     C                   MOVEL     CMNCCI        RPNCCI
     C                   Z-ADD     PAFRDT        RPFRDT
     C                   Z-ADD     PATODT        RPTODT
     C                   Z-ADD     PAWKS         RPWKS
     C                   Z-ADD     PADAYS        RPDAYS
     C                   MOVEL     PATRCD        RPTRCD
     C                   MOVEL     PATYPE        RPTYPE
     C                   MOVEL     PARETP        RPRETP
     C                   MOVEL     PARECD        RPRECD
     C                   MOVEL     PADESC        RPDESC
     C                   MOVEL     R2RMDS        RPRMDS
     C                   MOVEL     R2RMK1        RPRMK1
     C                   MOVEL     R2RMK2        RPRMK2
     C                   MOVEL     R2RMK3        RPRMK3
     C                   MOVEL     R2CKCN        RPCKCN
     C                   MOVEL     R2PNAM        RPPNAM
     C                   MOVEL     R2ADR1        RPADR1
     C                   MOVEL     R2ADR2        RPADR2
     C                   MOVEL     R2CTY         RPCTY
     C                   MOVEL     R2ST          RPST
     C                   Z-ADD     R2ZIP         RPZIP
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C                   MOVE(P)   R2CTRY        RPCTRY
     C                   MOVEL(P)  R2ADR3        RPADR3
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   MOVEL     PSTYPE        RPSTAT
     C*** BEGIN CHG *** 11/23/99 **************************************
     C******************** MOVELQ1USER    RPUSER
     C                   MOVEL     GCUSER        RPUSER
     C*** END CHG ***** 11/23/99 **************************************
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     XXDT          RPDT
     C                   Z-ADD     *ZERO         RPBUL1
     C                   Z-ADD     *ZERO         RPCHK#
     C                   Z-ADD     PAEOB#        RPEOB#
     C                   Z-ADD     PARCVD        RPRCVD
     C                   Z-ADD     PADUE         RPDUE
     C                   MOVEL     GCSTS         RPSTS
     C                   MOVEL     PSSRT         RPSORT
     C                   MOVEL     'N'           RPSENT
     C                   MOVEL     *ZERO         RPDSEQ
     C                   MOVEL     PAPGTP        RPPGTP
     C*** BEGIN CHG *** 11/23/99 **************************************
     C******************** MOVELQ1USER    RPRQST
     C                   MOVEL     GCUSER        RPRQST
     C*** END CHG ***** 11/23/99 **************************************
     C                   MOVEL     PALTCD        RPLTCD
     C*** BEGIN ADD *** 02/25/00 **************************************
     C                   Z-ADD     XXDT          RPIDT
     C                   Z-ADD     DSTIME        RPITM
     C                   MOVEL     GCUSER        RPIUSR
     C                   MOVEL     Q1PGM         RPIPGM
     C                   Z-ADD     0             RPRDT
     C                   Z-ADD     0             RPRTM
     C                   MOVE      *BLANKS       RPRUSR
     C                   MOVE      *BLANKS       RPRPGM
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     XXDT          RPRDT
     C                   Z-ADD     DSTIME        RPRTM
     C                   MOVEL     GCUSER        RPRUSR
     C                   MOVEL     Q1PGM         RPRPGM
     C                   ENDIF
     C*** BEGIN ADD *** 06/27/01 **************************************
     C     R3KY          CHAIN     PRP03                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     PAEOBS        IFEQ      R3EOBS
     C     PACHK#        ANDEQ     R3CHK#
     C     PAFEIN        ANDEQ     R3FEIN
     C     PAMOD         ANDEQ     R3MOD
     C     PAAMT         ANDEQ     R3AMT
     C*
     C                   MOVEL(P)  R3NTWK        RPNTWK
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     R3KY          READE     PRP03                                  99
     C                   ENDDO
     C*
     C                   Z-ADD     PAEOBS        RPEOBS
     C*** END   ADD *** 06/27/01 **************************************
     C*** END ADD ***** 02/25/00 **************************************
      *
     C*** Begin add *** 11/21/03 **************************************
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      PACO#         QQCO#
     C                   MOVE      PAFND         QQFND
     C                   MOVE      PAFYR         QQFYR
     C                   MOVE      PACASE        QQCASE
     C                   Z-ADD     RPTRDT        QXTRDT
     C                   Z-ADD     RPFRDT        QXFRDT
     C                   Z-ADD     RPTODT        QXTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     C*** BEG ADD ***** 08/09/07 **************************************
     C                   MOVE      *BLANKS       QXSEQ#
     C                   MOVE      *ZEROS        QXDSQ#
     C                   MOVE      *BLANKS       QXEOB#
     C                   MOVE      *ZEROS        QXFEIN
     C                   MOVE      *ZEROS        QXMOD
     C*** END ADD ***** 08/09/07 **************************************
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           ##MODE            1            Mode
     C                   PARM                    QQCO#                          Company
     C                   PARM                    QQFND             3            Group #
     C                   PARM                    QQFYR             3            Year
     C                   PARM                    QQCASE            7            Case
     C                   PARM      'WC'          ##APPL            2            Applic Code
      ***** BEG CHG ***** 08/09/07 ************************************
     C*******              PARM DATA      ##DATA 50        Selection criteria
     C                   PARM      DATA          ##DATA           75            Selection criteria
      ***** END CHG ***** 08/09/07 ************************************
     C                   PARM                    ##CKTP           10            Bank Acct Code
     C                   PARM                    ##RTCD            1            Return Code
     C*
     C                   MOVE      ##CKTP        RPCKTP                         Bank Acct# Code
     C*** End add ***** 11/21/03 **************************************
      *
     C                   WRITE     PRP01
     C                   ENDSR
      * =================================================================
     ****  OPCM01   E                PCM01E
      *                                                                     ****
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
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                52 '.1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      * ----------------------------------------------------------------
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              80
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL              80
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           95 'ORG'
     O          E            HEAD           2
     O***** BEGIN CHG ***** 05/04/98 **********************************
     O**************                      3 'FND'
     O**************                      9 'FYR'
     O**************                     18 'CASE#'
     O                                            3 'GRP'
     O*** BEGIN DLT *** 02/19/99 **************************************
     O*********************************   9 'PO YR'
     O*********************************  17 'CASE#'
     O*** END DLT ***** 02/19/99 **************************************
     O***** END   CHG ***** 05/04/98 **********************************
     O*** BEGIN DLT *** 02/19/99 **************************************
     O*********************************  25 'EMP#'
     O*** END DLT ***** 02/19/99 **************************************
     O*** BEGIN ADD *** 02/19/99 **************************************
     O                                            7 'YR'
     O                                           16 'CASE#'
     O                                           26 'EMP#'
     O*** END ADD ***** 02/19/99 **************************************
     O***** BEGIN CHG ***** 05/04/98 **********************************
     O**************                     30 'DIV'
     O**************                     41 'FEIN'
     O                                           32 'DIV'
     O                                           42 'FEIN'
     O***** END   CHG ***** 05/04/98 **********************************
     O                                           46 'MOD'
     O                                           66 'PAYEE'
     O                                           98 'TRN DATE'
     O                                          107 'CHECK#'
     O                                          122 'AMOUNT'
     O                                          126 'TC'
     O                                          130 'TP'
     O          E            DET            1
     O                       GCFND         Z      3
     O*** BEGIN DLT *** 02/19/99 **************************************
     O**********************   PAFYR Z    9
     O****** BEGIN CHG ****** 05/04/98 ********************************
     O**********               PACASEZ   18
     O**********************   PACASEZ   17
     O****** END   CHG ****** 05/04/98 ********************************
     O**********************   CMEMP#Z   25
     O*** END DLT ***** 02/19/99 **************************************
     O*** BEGIN ADD *** 02/19/99 **************************************
     O                       PAFYR         Z      7
     O                       PACASE        Z     16
     O                       CMEMP#        Z     26
     O*** END ADD ***** 02/19/99 **************************************
     O****** BEGIN CHG ****** 05/04/98 ********************************
     O**********               CMDIV Z   30
     O**********               GCFEINZ   41
     O                       CMDIV         Z     32
     O                       GCFEIN        Z     42
     O****** END   CHG ****** 05/04/98 ********************************
     O                       GCMOD         Z     46
     O                       GCPNAM              88
     O****** BEGIN CHG ****** 05/04/98 ********************************
     O*******************      XXDATEY   98
     O                       XXDATE              99 '  /  /    '
     O****** END   CHG ****** 05/04/98 ********************************
     O                       GCCHK#        Z    107
     O                       PAAMT         J    122
     O                       GCTRCD             126
     O                       GCTRTY             130
     O          E            CHKTT       1  2
     O                                           88 'TOTALS FOR CHECK #:'
     O                       GCCHK#        Z     96
     O                                          109 'AMOUNT:'
     O                       GCAMT         J    122
