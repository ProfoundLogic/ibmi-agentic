     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: W8101R                                            *
     H*                                                               *
     H*    DESCRIPTION: CREATE WORKFILE                               *
     H*                                                               *
     H*    CREATED BY:  C:33   03/28/96   PROJECT# 65WAY              *
     H*    REWRITE V3:  C:33   03/26/98   PROJECT# 65WCG              *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      W8101C - OVERRIDE                                        *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/26/01 C:15 Project: 65WCG                                 *
     H*                Pgm rewrite:  Load a reserve record for all    *
     H*                claims with a PTD total.                       *
     H*                                                               *
     H*  04/12/01 C:15 Support: WC065/26981                           *
     H*                Pgm rewrite:  reserve records did not reflect  *
     H*                as of the "To" date.                           *
     H*                                                               *
     H*  03/14/01 C:15 Project: 65WCG                                 *
     H*               -Chgd code to correct the pmt types.            *
     H*               -Chgd code to load neg amts with a leading '-'. *
     H*                                                               *
     H*  01/19/01 C:15 Support: WC065/25435                           *
     H*                Added code to send a reserve record on any     *
     H*                claims with activity during the date range.    *
     H*                                                               *
     H*  09/18/00 C:15 Support: WC065/24271                           *
     H*                Added code to prevent data from being reported *
     H*                on claims entered after the date range.        *
     H*                                                               *
     H*  03/22/00 C:33 PROJECT: 65WCG    WC065/22000                  *
     H*                LOAD THE POLICY SYMBOL & POLICY # FROM THE     *
     H*                CARRIER POLICY #.  CALCULATE THE SYMBOL IF THE *
     H*                CARRIER POLICY # DOES NOT HAVE IT.             *
     H*                                                               *
     H*  02/25/00 C:33 PROJECT: 65WCG    WC065/22000                  *
     H*                DO NOT LOAD ANY POLICY # IF THE CARRIER POLICY *
     H*                # DOES NOT EXIST.                              *
     H*                                                               *
     H*  04/02/99 C:33 PROJECT: WC889                                 *
     H*                IF DBA FLAG = 'Y' LOAD PDADR2, ELSE LOAD PDADR1*
     H*                                                               *
     H*  06/24/98  C:33  SEQ#: WC065/12354                            *
     H*                  IF THE ACCIDENT STATE IS BLANK AND THE EMP   *
     H*                  PREMISES FLAG IS 'Y', LOAD THE EMPLOYER STATE*
     H*                                                               *
     H*  06/02/98  C:33  SEQ#: WC065/12015                            *
     H*                  RECOMPLILED DUE TO FILE CHANGE.              *
     H*                                                               *
     H*  12/12/97  C:33  PROJECT: SEQ# 9620                           *
     H*                  DO NOT CLEAR CLAIMANT CITY/ST WHEN USING     *
     H*                  PROVIDER CITY/ST                             *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  60-65  -  GENERAL CHAINING                                   *
     H*  93     -  CLAIM CLOSED BEFORE DATE RANGE                     *
     H*  94     -  CLAIM CLOSED W/IN DATE RANGE                       *
     H*******************************************************************
     FWXG1P     O  A E           K DISK
     FWXG2P     O  A E           K DISK
     FWXG3P     O  A E           K DISK
     FWXG4P     O  A E           K DISK
     FWMCML     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     F***  BEGIN ADD  ***  02/25/00  *********************************
     FWMRPL2    IF   E           K DISK
     F***  END   ADD  ***  02/25/00  *********************************
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA03)
     FWMRHL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FXLPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FXLNAL     IF   E           K DISK
     FXLCAL     IF   E           K DISK
     FXLB2L     IF   E           K DISK
     FXLPCL     IF   E           K DISK
     FXLPCL1    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FSMHDL     IF   E           K DISK
     F*

     D*** Begin add *** 04/12/01 **************************************
     D RC              S              2    DIM(10)
     D RR              S              9  2 DIM(10)
     D CHG             S              9  2 DIM(10)
     D AR              S              9  2 DIM(10)
     D*
     D*
     D RCHG            S              9  2 DIM(10)
     D*** End add ***** 04/12/01 **************************************
     D*
     D RSVC            S              2  0 DIM(10)
     D RSV             S             11  2 DIM(10)
     D PTD             S             11  2 DIM(10)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  CMNCCI                 1      2
     D  XXNCCI                 1      1
     D  YYNCCI                 2      2
     D                 DS
     D  CLM#                   1     20
     D  CMFND                  8     10  0
     D  CMFYR                 11     13  0
     D  CMCASE                14     20  0
     D                 DS
     D  XXHDKY                 1     40
     D  CMCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D                 DS
     D  TM                     1      8
     D  XXHH                   1      2  0
     D  COL1                   3      3
     D  XXMM                   4      5  0
     D  COL2                   6      6
     D  XXSS                   7      8  0
     D                 DS
     D  TM2                    1      6  0
     D  ZZHH                   1      2  0
     D  ZZMM                   3      4  0
     D  ZZSS                   5      6  0
     D                 DS
     D  PLSY                   1      3
     D  WC                     1      2
     D  XXYR                   3      3  0
     D***  BEGIN ADD  ***  03/22/00  *********************************
     D                 DS
     D  RPCAP#                 1     25
     D  XXWC                   1      2
     D  XXPLSY                 1      3
     D  XXPOL#                 5     12
     D  ZZPOL#                 1      7
     D***  END   ADD  ***  03/22/00  *********************************
     D***  BEGIN DEL  ***  02/25/00  *********************************
     D**********  DS
     D**********                              1   7 POL#
     D**********                              1   50XYEMP#
     D**********                              6   70XYDIV
     D***  END   DEL  ***  02/25/00  *********************************
     D                 DS
     D  ZIP                    1     10
     D  CMACZ1                 1      5  0
     D  DASH                   6      6
     D  CMACZ2                 7     10  0
     D                 DS
     D  PATRDT                 1      7  0
     D  YY                     2      3  0
     D  MM                     4      5  0
     D                 DS
     D  X7TDT                  1      7  0
     D  YYR                    2      3  0
     D  MMR                    4      5  0
     D*** Begin add *** 04/12/01 **************************************
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  SU                     1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*** End add ***** 04/12/01 **************************************
     D*
     D* External Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
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
     I              PAPGTP                      P3PGTP
     I              PAEOB#                      P3EOB#

     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CROSS REFERENCE FILES
     C*
     C     XLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXKEY
     C*
     C*  CLAIMS HISTORY FILE
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C*
     C*  CUSTOM GROUP ID FILE
     C*
     C     HDKY          KLIST
     C                   KFLD                    XXHDKY
     C                   KFLD                    XXCSID
     C                   KFLD                    XXRFMT
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C***  BEGIN ADD  ***  02/25/00  *********************************
     C*
     C*  EMPLOYER CARRIER INFO
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C***  END   ADD  ***  02/25/00  *********************************
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  RESERVE HISTORY FILE
     C*
     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** Begin add *** 04/12/01 **************************************
     C     RHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C*** End add ***** 04/12/01 **************************************
     C*
     C*  PAYMENT FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** Begin add *** 04/12/01 **************************************
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEFDT
     C*** End add ***** 04/12/01 **************************************
     C*
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    PABASQ
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*_______________________________________________________________
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
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMDT          X7FDT
     C     *LIKE         DEFINE    PAFEIN        XXFEIN
     C     *LIKE         DEFINE    HDCSID        XXCSID
     C     *LIKE         DEFINE    HDRFMT        XXRFMT
     C***  BEGIN ADD  ***  02/25/00  *********************************
     C     *LIKE         DEFINE    G1POL#        POL#
     C***  END   ADD  ***  02/25/00  *********************************
     C*** Begin add *** 04/12/01 **************************************
     C     *LIKE         DEFINE    CMDT          XXEFDT
     C*** End add ***** 04/12/01 **************************************
     C*
     C* Set-up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFDT         X8FDT             8 0          MM/DD/CCYY
     C                   MOVE      QQTDT         X8TDT             8 0          MM/DD/CCYY
     C*
     C                   Z-ADD     X8FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FDT
     C*
     C                   Z-ADD     X8TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TDT
     C*
     C                   MOVEL     *ZEROS        CLM#
     C                   MOVEL     ':'           COL1
     C                   MOVEL     ':'           COL2
     C                   MOVEL     'WC'          WC
     C                   MOVEL     '-'           DASH
     C                   MOVEL     'V1'          XXCSID
     C                   MOVEL     'A'           XXRFMT
     C*
     C*** Begin add *** 04/12/01 **************************************
     C                   Z-ADD     X7FDT         XXEFDT
     C*** End add ***** 04/12/01 **************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET THE GROUP MASTER RECORD
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C*  READ THE CLAIM RECORDS
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     CMCASE        XXCASE
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMEMP#        XXEMP#
     C***  BEGIN DEL  ***  02/25/00  *********************************
     C**********           Z-ADDCMEMP#    XYEMP#
     C***  END   DEL  ***  02/25/00  *********************************
     C                   Z-ADD     CMDIV         XXDIV
     C***  BEGIN DEL  ***  02/25/00  *********************************
     C**********           Z-ADDCMDIV     XYDIV
     C***  END   DEL  ***  02/25/00  *********************************
     C                   MOVEL     CMSS#         XXSS#
     C                   Z-ADD     *ZEROS        DTL#              4 0
     C*** BEGIN ADD *** 09/18/00 **************************************
     C                   SETOFF                                           21
     C*** END ADD ***** 09/18/00 **************************************
     C*** BEGIN ADD *** 01/19/01 *************************************
     C                   SETOFF                                           22
     C*** END ADD ***** 01/19/01 *************************************
     C*
     C*  GET CLAIMANT DEMOGRAPHICS
     C*
     C     CDKY          CHAIN     PCD01                              61
     C*
     C                   MOVEL(P)  CDCTY         XCCTY            27
     C                   MOVEL(P)  XCCTY         QQLAST
     C                   MOVEL(P)  CDST          QQFRST
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST           56
     C                   PARM                    QQFRST           15
     C                   PARM      '1'           QQEOJ             1
     C*
     C                   MOVEL(P)  QQLAST        XCCTST           30
     C*
     C     CDFSN         CAT(P)    CDMID:1       XXFSN            17
     C                   MOVEL(P)  CDLSN         XXLSN            56
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    XXLSN
     C                   PARM                    XXFSN
     C                   PARM      '1'           XXRTCD            1
     C*
     C                   MOVEL(P)  XXLSN         XXNAME           30
     C*
     C*  GET CUSTOM GROUP ID
     C*
     C     HDKY          CHAIN     PHD01                              61
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              61
     C***  BEGIN ADD  ***  02/25/00  *********************************
     C*
     C*  GET EMPLOYER CARRIER INFO
     C*
     C                   CLEAR                   RPCAP#
     C     RPKY          CHAIN     PRP01                              61
     C*
     C***  BEGIN CHG  ***  03/22/00  *********************************
     C**********           MOVELRPCAP#    POL#      P
     C     XXWC          IFEQ      WC
     C                   MOVEL(P)  XXPOL#        POL#
     C                   ELSE
     C                   MOVEL(P)  ZZPOL#        POL#
     C                   ENDIF                                                  XXWC=WC
     C***  END   CHG  ***  03/22/00  *********************************
     C***  END   ADD  ***  02/25/00  *********************************
     C*
     C*  GET EMPLOYERS YEAR OF COVERAGE AT TIME OF CLAIM
     C*
     C     ELKY          CHAIN     PEL01                              61
     C*
     C     CMFYR         SUB       ELFYR         XXYR
     C                   ADD       1             XXYR
     C*
     C***  BEGIN ADD  ***  03/22/00  *********************************
     C     XXWC          IFEQ      WC
     C                   MOVEL(P)  XXPLSY        PLSY
     C                   ENDIF                                                  XXWC=WC
     C***  END   ADD  ***  03/22/00  *********************************
     C*
     C*  IF THE CLAIM HAS BEEN UPDATED FOR ANY REASON WRITE A WXG1P &
     C*  WXG2P RECORD FOR THE CLAIM.
     C*
     C     CMDT          IFGE      X7FDT
     C*** BEGIN CHG *** 09/18/00 **************************************
     C********** CMDT      ANDLEX7FDT
     C     CMDT          ANDLE     X7TDT
     C*** END CHG ***** 09/18/00 **************************************
     C*
     C*** Begin dlt *** 04/12/01 **************************************
     C*******************  EXSR CLR
     C*******************  EXSR G1SR
     C*******************  EXSR G2SR
     C*** End dlt ***** 04/12/01 **************************************
     C*** Begin add *** 04/12/01 **************************************
     C                   SETON                                            21
     C*** End add ***** 04/12/01 **************************************
     C*
     C                   ENDIF                                                  CMDT>=X7FDT
     C*
     C*  READ CLAIM HISTORY FILE TO DETERMINE IF THIS CLAIM WAS NEW,
     C*  REOPENED OR CLOSED DURING THE SELECTED DATE RANGE AND WRITE
     C*  A WXG1P & WXG2P RECORD FOR THAT CLAIM.
     C*
     C                   MOVE      *OFF          *IN94
     C*
     C     CHKY          CHAIN     PCH01                              65
     C     *IN65         DOWEQ     *OFF
     C*
     C     CHEFDT        IFGE      X7FDT
     C     CHEFDT        ANDLE     X7TDT
     C*
     C*** Begin dlt *** 04/12/01 **************************************
     C******************** EXSR CLR
     C******************** EXSR G1SR
     C******************** EXSR G2SR
     C*** End dlt ***** 04/12/01 **************************************
     C*** Begin add *** 04/12/01 **************************************
     C                   SETON                                            21
     C*** End add ***** 04/12/01 **************************************
     C*
     C     CHOCST        COMP      'C'                                    94
     C*
     C                   ENDIF                                                  CHEFDT>=X7FDT
     C*
     C     CHEFDT        IFLT      X7FDT
     C     CHOCST        ANDEQ     'C'
     C                   MOVE      *ON           *IN93
     C                   ELSE
     C                   MOVE      *OFF          *IN93
     C                   ENDIF                                                  CHEFDT<X7FDT
     C*
     C     CHKY          READE     PCH01                                  65
     C                   ENDDO                                                  *IN65=*OFF
     C*
     C*** Begin add *** 04/12/01 **************************************
     C*  Determine if any payments were made within the date range.
     C*  If so, load records.
     C*
     C     PAKY1         SETLL     PPA01
     C     PAKY          READE     PPA01                                  99
     C     *IN99         IFEQ      *OFF
     C     PATRDT        ANDLE     X7TDT
     C                   SETON                                            21
     C                   ENDIF
     C*
     C*  Determine if any reserves were changed within the date range.
     C*  If so, load records.
     C*
     C     RHKY1         SETLL     PRH01
     C     RHKY          READE     PRH01                                  99
     C     *IN99         IFEQ      *OFF
     C     RHEFDT        ANDLE     X7TDT
     C                   SETON                                            21
     C                   ENDIF
     C*** End add ***** 04/12/01 **************************************
     C*
     C*** BEGIN ADD *** 09/18/00 **************************************
     C     *IN21         IFEQ      *ON
     C*** END ADD ***** 09/18/00 **************************************
     C*
     C*** Begin add *** 04/12/01 **************************************
     C*  Load the G1 and G2 records.
     C*
     C                   EXSR      CLR
     C                   EXSR      G1SR
     C                   EXSR      G2SR
     C*
     C*** Begin add *** 10/26/01 **************************************
     C                   ENDIF
     C*** End add ***** 10/26/01 **************************************
     C*
     C*  Clear amount arrays.
     C*
     C                   CLEAR                   RT
     C                   CLEAR                   RR
     C                   CLEAR                   CHG
     C                   CLEAR                   AR
     C*** End add ***** 04/12/01 **************************************
     C*
     C*  Read all payment records.
     C*
     C     PAKY          CHAIN     PPA01                              65
     C     *IN65         DOWEQ     *OFF
     C*
     C*  If a payment record was created within the date range,
     C*  create a WXG4P record.
     C*
     C     PATRDT        IFGE      X7FDT
     C     PATRDT        ANDLE     X7TDT
     C                   EXSR      CLR
     C                   EXSR      G4SR
     C                   ENDIF                                                  PATRDT>=X7FDT
     C*
     C*** Begin add *** 04/12/01 **************************************
     C*  Accumulate the total paid as of the end of the period.
     C*
     C     PATRDT        IFLE      X7TDT
     C                   EXSR      SRPTD
     C                   ENDIF                                                  PATRDT>=X7FDT
     C*** end add ***** 04/12/01 **************************************
     C*
     C     PAKY          READE     PPA01                                  65
     C                   ENDDO                                                  *IN65=*OFF
     C*
     C*** Begin rewrite *** 04/12/01 **********************************
     C*  WRITE A WXG3P RECORD FOR EACH RESERVE TYPE WITH REMAINING
     C*  RESERVES FOR EACH CLAIM
     C*
     C     RHKY          CHAIN     PRH01                              65
     C     *IN65         DOWEQ     *OFF
     C*
     C     RHEFDT        IFLE      X7TDT
     C*
     C                   DO        10            X                 2 0
     C*
     C     RT(X)         IFNE      *BLANKS
     C*
     C                   Z-ADD     1             Y                 2 0
     C     RT(X)         LOOKUP    RC(Y)                                  90
     C     *IN90         IFEQ      *ON
     C                   ADD       SU(X)         RR(Y)
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             Y
     C     *BLANKS       LOOKUP    RC(Y)                                  90
     C                   MOVE      RT(X)         RC(Y)
     C                   ADD       SU(X)         RR(Y)
     C                   ENDIF                                                  *IN90=*ON
     C*
     C     RHEFDT        IFGE      X7FDT
     C                   ADD       SU(X)         CHG(Y)
     C                   ENDIF
     C*
     C                   ENDIF                                                  RT<>*BLANKS
     C*
     C                   ENDDO                                                  X -> 10
     C*
     C                   ENDIF                                                  RHEFDT <= TDT
     C*
     C     RHKY          READE     PRH01                                  65
     C                   ENDDO
     C*
     C                   EXSR      G3SR2
     C                   EXSR      CLR
     C                   EXSR      G3SR
     C*** End rewrite *** 04/12/01 ************************************
     C*
     C*** Begin dlt *** 10/26/01 **************************************
     C*** BEGIN ADD *** 09/18/00 **************************************
     C******************** ENDIF
     C*** END ADD ***** 09/18/00 **************************************
     C*** End dlt ***** 10/26/01 **************************************
     C*
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*===============================================================
     C*  G1SR - ASSIGN VALUES TO WXG1P FIELDS
     C*===============================================================
     C*
     C     G1SR          BEGSR
     C*
     C*** BEGIN ADD *** 09/18/00 **************************************
     C                   SETON                                            21
     C*** END ADD ***** 09/18/00 **************************************
     C*
     C                   MOVEL     CLM#          G1CLM
     C                   MOVEL     '070'         G1SOFC
     C                   MOVEL     *BLANKS       G1EXAM
     C                   Z-ADD     CMACDT        G1ACDT
     C*
     C     CMACTM        IFEQ      *ZERO
     C                   Z-ADD     12            XXHH
     C                   Z-ADD     *ZEROS        XXMM
     C                   Z-ADD     *ZEROS        XXSS
     C                   ELSE
     C                   Z-ADD     CMACTM        TM2
     C                   Z-ADD     ZZHH          XXHH
     C                   Z-ADD     ZZMM          XXMM
     C                   Z-ADD     ZZSS          XXSS
     C                   ENDIF                                                  CMACTM=*ZEROS
     C*
     C                   MOVEL     TM            G1ACTM
     C                   Z-ADD     CMDTRP        G1RPDT
     C*
     C                   MOVEL     HDUA01        XXUA01            2
     C     XXUA01        IFEQ      '0 '
     C     HDUA01        OREQ      *BLANKS
     C                   Z-ADD     12            XXHH
     C                   Z-ADD     *ZEROS        XXMM
     C                   Z-ADD     *ZEROS        XXSS
     C                   ELSE
     C                   Z-ADD     CMITM         TM2
     C                   Z-ADD     ZZHH          XXHH
     C                   Z-ADD     ZZMM          XXMM
     C                   Z-ADD     ZZSS          XXSS
     C                   ENDIF                                                  XXUA01=*ZEROS
     C*
     C                   MOVEL     TM            G1RPTM
     C                   Z-ADD     *ZEROS        G1MDDT
     C                   MOVEL     *BLANKS       G1RPNM
     C                   MOVEL     CMACDS        G1ACDS
     C*
     C     CMLTFL        IFEQ      'I'
     C                   MOVEL     '06'          G1LTFL
     C                   ELSE
     C                   MOVEL     '07'          G1LTFL
     C                   ENDIF                                                  CMLTFL='I'
     C*
     C                   MOVEL     'WC'          G1TYLS
     C** *******           MOVELHDUA02    G1ACST
     C*
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C     CMAST         IFEQ      *BLANK
     C     CMEADR        ANDEQ     'Y'
     C*
     C     EMPST         IFNE      *BLANK
     C                   MOVEL     EMPST         CMAST
     C                   ELSE
     C                   MOVEL     EMMST         CMAST
     C                   ENDIF                                                  EMPST<>*BLANK
     C*
     C                   ENDIF                                                  CMAST=*BLANK
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C                   MOVEL(P)  CMAST         G1ACST
     C                   MOVEL     *BLANKS       G1CAST
     C                   MOVEL     CMSTAT        G1STAT
     C                   MOVEL     PLSY          G1PLSY
     C                   MOVEL     POL#          G1POL#
     C                   MOVEL     *ZEROS        G1MODL
     C                   MOVEL     *BLANKS       G1COV#
     C                   MOVEL     *BLANKS       G1TPAB
     C                   MOVEL     *BLANKS       G1LOCD
     C                   MOVEL     *BLANKS       G1DRIV
     C                   MOVEL     *BLANKS       G1WTNS
     C                   MOVEL     *BLANKS       G1FIL1
     C*
     C                   WRITE     PG101
     C*
     C                   ENDSR                                                  G1SR
     C*
     C*===============================================================
     C*  G2SR - ASSIGN VALUES TO WXG2P FIELDS
     C*===============================================================
     C*
     C     G2SR          BEGSR
     C*
     C                   MOVEL     CLM#          G2CLM
     C                   Z-ADD     1             G2SEQ#
     C                   Z-ADD     *ZEROS        G2SUIT
     C*
     C                   MOVEL     CMPART        XXKEY            10
     C*
     C     XLKY          CHAIN     PPA02                              61
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     PAXPRT        G2INJ1
     C                   ELSE
     C                   MOVEL     *BLANKS       G2INJ1
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   MOVEL     CMNATR        XXKEY
     C*
     C     XLKY          CHAIN     PNA01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     NAXNTR        G2INJ2
     C                   ELSE
     C                   MOVEL     *BLANKS       G2INJ2
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   MOVEL     CMCAUS        XXKEY
     C*
     C     XLKY          CHAIN     PCA01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     CAXNTR        G2INJ3
     C                   ELSE
     C                   MOVEL     *BLANKS       G2INJ3
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C                   MOVEL     HDUA03        G2MART
     C                   Z-ADD     *ZEROS        G2DPDT
     C                   Z-ADD     CDDTOB        G2DOB
     C*
     C** *******           Z-ADDHDUD01    G2DOD
     C                   Z-ADD     CMDOD         G2DOD
     C                   MOVEL     CMSS#         G2SSN
     C                   MOVEL     *BLANKS       G2OCC
     C                   Z-ADD     CMDTLW        G2DTLW
     C*
     C** ******* WHWKST    IFEQ 'R'
     C** *******           Z-ADDWHDATE    G2RTW
     C** *******           ELSE
     C** *******           Z-ADD*ZEROS    G2RTW
     C** *******           ENDIF                           WHWKST='R'
     C                   Z-ADD     CMRTW         G2RTW
     C*
     C                   MOVEL     *BLANKS       G2LGFL
     C                   MOVEL     *BLANKS       G2LGNM
     C                   Z-ADD     CMAWW         G2AWW
     C                   Z-ADD     *ZEROS        G2AJWG
     C                   Z-ADD     CMENTD        G2ENTD
     C*
     C** *******           MOVELHDUA05    XXCVGC  2
     C** ******* XXCVGC    IFGE '11'
     C** ******* XXCVGC    ANDLE'19'
     C** *******           MOVELHDUA05    G2CVGC
     C     CMLSCV        IFGE      '11'
     C     CMLSCV        ANDLE     '19'
     C                   MOVEL     CMLSCV        G2CVGC
     C                   ELSE
     C                   MOVEL     '11'          G2CVGC
     C                   ENDIF                                                  '11'<=CMLSCV<='19'
     C*
     C     XXNCCI        IFNE      *ZERO
     C                   MOVE(P)   CMNCCI        G2NCCI
     C                   ELSE
     C                   MOVEL(P)  YYNCCI        G2NCCI
     C                   ENDIF                                                  XXNCCI<>*ZERO
     C*
     C** *******           MOVELHDUA06    G2JRST
     C** *******           MOVELHDUA07    G2IJZP
     C                   MOVEL(P)  CMJRST        G2JRST
     C                   MOVEL(P)  ZIP           G2IJZP
     C                   Z-ADD     *ZEROS        G2DOH
     C                   MOVEL     *BLANKS       G2EMPS
     C*
     C                   MOVEL     CMCLAS        XXCLAS            4
     C                   MOVEL     XXCLAS        G2CLAS
     C*
     C                   MOVEL     EMFEIN        G2EFID
     C                   MOVEL     EMSIC         G2SIC
     C                   MOVEL     *BLANKS       G2PYIN
     C                   MOVEL     *BLANKS       G2WAGM
     C                   Z-ADD     *ZEROS        G2OWPY
     C                   Z-ADD     CMDTRP        G2DTRP
     C                   MOVEL     *BLANKS       G2SURG
     C                   Z-ADD     *ZEROS        G2PWAG
     C                   Z-ADD     *ZEROS        G2IMP
     C                   Z-ADD     *ZEROS        G2DTDD
     C                   MOVEL     *BLANKS       G2CVER
     C                   MOVEL     *BLANKS       G2BSS
     C                   MOVEL     *BLANKS       G2BUNE
     C                   MOVEL     *BLANKS       G2BPEN
     C                   MOVEL     *BLANKS       G2BSFN
     C                   MOVEL     *BLANKS       G2BOTH
     C                   MOVEL     CDFSN         G2FSN
     C                   MOVEL     CDLSN         G2LSN
     C                   MOVEL     CDMID         G2MID
     C                   MOVEL     CDADR1        G2ADR1
     C                   MOVEL     CDADR2        G2ADR2
     C                   MOVEL     CDCTY         G2CTY
     C                   MOVEL     CDST          G2ST
     C                   MOVEL     CDZIP         G2ZIP
     C                   MOVEL     CDSEX         G2SEX
     C                   MOVEL     *BLANKS       G2LOCI
     C                   MOVEL     *BLANKS       G2OCCU
     C                   MOVEL     *ZEROS        G2STDT
     C                   MOVEL     *BLANKS       G2BILL
     C                   MOVEL     *BLANKS       G2SUBR
     C                   MOVEL     *BLANKS       G2VID#
     C                   MOVEL     *BLANKS       G2MAKE
     C                   MOVEL     *BLANKS       G2MODL
     C                   MOVEL     *ZEROS        G2YEAR
     C                   MOVEL     *BLANKS       G2FIL1
     C*
     C                   WRITE     PG201
     C*
     C                   ENDSR                                                  G2SR
     C*
     C*===============================================================
     C*  G3SR - ASSIGN VALUES TO WXG3P FIELDS
     C*===============================================================
     C     G3SR          BEGSR
     C*
     C                   MOVEL     CLM#          G3CLM
     C                   Z-ADD     1             G3SEQ#
     C                   Z-ADD     *ZEROS        G3TRN#
     C   94              MOVEL     'CL'          G3STAT
     C  N94              MOVEL     'SS'          G3STAT
     C                   Z-ADD     *ZEROS        G3RSCH
     C                   Z-ADD     X7TDT         G3DT
     C*
     C                   Z-ADD     12            XXHH
     C                   Z-ADD     *ZEROS        XXMM
     C                   Z-ADD     *ZEROS        XXSS
     C                   MOVEL     TM            G3TIME
     C*
     C                   MOVEL     MMR           G3PER
     C                   MOVE      YYR           G3PER
     C*
     C                   MOVEL     *BLANKS       G3FIL1
     C                   MOVEL     *BLANKS       G3ADJ
     C                   MOVEL     *BLANKS       G3OFS
     C                   MOVEL     '032'         G3PRL
     C                   MOVEL     *BLANKS       G3FIL2
     C*
     C                   DO        10            X
     C*
     C     RSV(X)        IFNE      *ZERO
     C*** Begin add *** 10/26/01 **************************************
     C     *IN21         ANDEQ     *ON
     C*** End add ***** 10/26/01 **************************************
     C     PTD(X)        ORNE      *ZERO
     C*** BEGIN ADD *** 01/19/01 *************************************
     C     RSVC(X)       ORNE      *ZERO
     C     *IN22         ANDEQ     *ON
     C*** END ADD ***** 01/19/01 *************************************
     C*
     C                   ADD       1             DTL#
     C                   Z-ADD     DTL#          G3DTL#
     C                   Z-ADD     RSVC(X)       G3RSVC
     C*******************  Z-ADDRSV,X     G3RSV
     C                   Z-ADD     PTD(X)        G3PTD
     C*
     C*** Begin add *** 04/12/01 *************************************
     C     RSV(X)        IFGE      0
     C                   Z-ADD     RSV(X)        G3RSV
     C                   ELSE
     C                   MULT      -1            RSV(X)
     C                   MOVE      RSV(X)        G3RSV
     C                   MOVEL     '-'           G3RSV
     C                   ENDIF
     C*
     C     RCHG(X)       IFGE      0
     C                   Z-ADD     RCHG(X)       G3RSCH
     C                   ELSE
     C                   MULT      -1            RCHG(X)
     C                   MOVE      RCHG(X)       G3RSCH
     C                   MOVEL     '-'           G3RSCH
     C                   ENDIF
     C*** End add ***** 04/12/01 *************************************
     C*
     C                   WRITE     PG301
     C*
     C                   ENDIF                                                  RSV,X<>*ZERO<>PTD,X
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   ENDSR                                                  G3SR
     C*
     C*===============================================================
     C*  G3SR2 - COMBINE LIKE RESERVE TYPES
     C*===============================================================
     C     G3SR2         BEGSR
     C*
     C                   CLEAR                   RSVC
     C                   CLEAR                   RSV
     C                   CLEAR                   PTD
     C*
     C                   DO        10            X
     C*
     C     RC(X)         IFNE      *BLANKS
     C*
     C                   MOVEL     RC(X)         XXKEY
     C     XLKY          CHAIN     PPC02                              61
     C  N61              MOVEL     PCXRCT        XXRSVC            2 0
     C   61              MOVEL     *ZERO         XXRSVC
     C*
     C                   Z-ADD     1             Y
     C     XXRSVC        LOOKUP    RSVC(Y)                                90
     C     *IN90         IFEQ      *ON
     C*
     C                   ADD       RR(X)         RSV(Y)
     C                   ADD       AR(X)         PTD(Y)
     C                   ADD       CHG(X)        RCHG(Y)
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             Y
     C     *ZEROS        LOOKUP    RSVC(Y)                                90
     C                   Z-ADD     XXRSVC        RSVC(Y)
     C                   Z-ADD     RR(X)         RSV(Y)
     C                   Z-ADD     AR(X)         PTD(Y)
     C                   Z-ADD     CHG(X)        RCHG(Y)
     C*
     C                   ENDIF                                                  *IN90=*ON
     C*
     C                   ENDIF                                                  RC,X NE BLANKS
     C*
     C                   ENDDO
     C*
     C*  Subtract the PTD from the RSV total for each category, to
     C*  reflect the actual remaining reserves as of the "to" date.
     C*
     C                   DO        10            X
     C                   SUB       PTD(X)        RSV(X)
     C                   ENDDO
     C*
     C                   ENDSR                                                  G3SR2
     C*===============================================================
     C*  G4SR - ASSIGN VALUES TO WXG4P FIELDS
     C*===============================================================
     C*
     C     G4SR          BEGSR
     C*
     C                   MOVEL     PLSY          G4PLSY
     C                   MOVEL     POL#          G4POL#
     C                   MOVEL     *ZEROS        G4MODL
     C                   MOVEL     '05'          G4MCO#
     C                   MOVEL     *BLANKS       G4AGT#
     C                   MOVEL     CLM#          G4CLM
     C                   Z-ADD     1             G4SEQ#
     C*
     C                   Z-ADD     DTL#          G4DTL#
     C*
     C                   Z-ADD     PATRDT        G4TRDT
     C                   MOVEL     MM            G4PER
     C                   MOVE      YY            G4PER
     C*
     C*** BEGIN DLT *** 03/14/01 **************************************
     C********** PATYPE    IFEQ 'P'
     C********** PATYPE    OREQ 'M'
     C********** PATYPE    OREQ 'Z'
     C********** PATYPE    OREQ 'A'
     C********** PATYPE    OREQ 'R'                        LEGION REQUIREMENTS
     C**********           Z-ADDPAAMT     G4AMT
     C**********           ELSE
     C**********           Z-SUBPAAMT     G4AMT
     C**********           ENDIF                           PATYPE=P/M/Z/A/R
     C*** END DLT ***** 03/14/01 **************************************
     C*
     C                   MOVEL     PATRCD        XXKEY
     C*
     C                   SELECT
     C*** BEGIN CHG *** 03/14/01 **************************************
     C********** PATYPE    WHEQ 'Z'
     C     PATYPE        WHENEQ    'S'
     C*** END CHG ***** 03/14/01 **************************************
     C     PATYPE        OREQ      'V'
     C                   MOVEL     'RV'          G4TRCD
     C     PATYPE        WHENEQ    'R'
     C*** BEGIN ADD *** 03/14/01 **************************************
     C     PAAMT         ANDGE     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDLT     0
     C*** END ADD ***** 03/14/01 **************************************
     C                   MOVEL     'RC'          G4TRCD
     C                   OTHER
     C                   MOVEL     'PP'          G4TRCD
     C                   ENDSL
     C*
     C*** BEGIN ADD *** 03/14/01 **************************************
     C     PATYPE        IFEQ      'S'
     C     PATYPE        OREQ      'V'
     C                   MULT      -1            PAAMT
     C                   ENDIF
     C*
     C     PATYPE        IFEQ      'A'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C                   MULT      -1            PAAMT
     C                   ENDIF
     C*
     C                   MOVE      *ZEROS        G4AMT
     C     PAAMT         IFGE      0
     C                   MOVE      PAAMT         G4AMT
     C                   ELSE
     C                   MULT      -1            PAAMT
     C                   MOVE      PAAMT         G4AMT
     C                   MOVEL     '-'           G4AMT
     C                   ENDIF
     C*** END ADD ***** 03/14/01 **************************************
     C*
     C                   MOVE      PAFEIN        XYFEIN            9
     C*
     C     PAFEIN        IFEQ      *ZERO
     C     XYFEIN        OREQ      CMSS#
     C*
     C                   MOVEL     XXNAME        G4PNAM
     C                   MOVEL     CDADR1        G4ADR1
     C                   MOVEL     CDADR2        G4ADR2
     C                   MOVEL     XCCTST        G4CTST
     C                   MOVEL     CDZIP         G4ZIP
     C                   MOVEL     'N'           G41099
     C                   MOVEL     CMSS#         G4FEIN
     C*
     C                   ELSE
     C*
     C                   Z-ADD     PAFEIN        XXFEIN
     C     PDKY          CHAIN     PPD01                              61
     C*
     C                   MOVEL     PDNAME        G4PNAM
      **** BEGIN ADD **** 04/02/99 ************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR2        G4ADR1
     C                   ELSE
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVEL     PDADR1        G4ADR1
      **** BEGIN ADD **** 04/02/99 ************************************
     C                   ENDIF
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVEL     *BLANKS       G4ADR2
     C*
     C                   MOVEL(P)  PDCTY         XPCTY            27
     C                   MOVEL     XPCTY         QQLAST
     C                   MOVEL     PDST          QQFRST
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM      '1'           QQEOJ             1
     C                   MOVEL     QQLAST        G4CTST
     C*
     C                   MOVEL     PDZIP         G4ZIP
     C*
     C     PD1099        IFEQ      'N'
     C                   MOVEL     'N'           G41099
     C                   ELSE
     C                   MOVEL     'Y'           G41099
     C                   ENDIF                                                  PD1099='N'
     C*
     C                   MOVEL     PDFEIN        G4FEIN
     C*
     C                   ENDIF                                                  CMSS#=*ZEROS
     C*
     C                   MOVEL     *BLANKS       G4FIL1
     C*
     C     PATYPE        IFEQ      'R'
     C*
     C*** BEGIN DLT *** 03/14/01 **************************************
     C******************** MOVEL'RC'      G4TRCD
     C*** END DLT ***** 03/14/01 **************************************
     C*
     C     PABR2         IFEQ      'SB'
     C                   MOVE      '1'           G4REC
     C                   ELSE
     C*
     C     PABR2         IFEQ      FNMEP1
     C     PABR2         OREQ      FNCOP1
     C     PABR2         OREQ      FNEXP1
     C     PABR2         OREQ      FNLEP1
     C                   MOVEL     '3'           G4REC
     C                   ELSE
     C                   MOVEL     '5'           G4REC
     C                   ENDIF                                                  PABR2=FNMEP1
     C*
     C                   ENDIF                                                  PABR2='SB'
     C*
     C                   ENDIF                                                  PATYPE='R'
     C*
     C                   MOVEL     PABR2         XXKEY
     C*
     C     XLKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B2XPCD        G4BR2
     C                   ELSE
     C                   MOVEL     *BLANKS       G4BR2
     C                   ENDIF                                                   *IN61=*OFF
     C*
     C                   MOVE      PACHK#        G4CHK#
     C                   MOVEL     '032'         G4PRL
     C*
     C     PATYPE        IFEQ      'V'
     C*** BEGIN ADD *** 03/14/01 **************************************
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'Z'
     C*** END ADD ***** 03/14/01 **************************************
     C     PAKY2         CHAIN     PPA03                              61
     C  N61              MOVEL(P)  P3TRCD        XXKEY
     C   61              MOVEL(P)  PATRCD        XXKEY
     C                   ELSE
     C                   MOVEL(P)  PATRCD        XXKEY
     C                   ENDIF                                                  PATYPE=V
     C*
     C     XLKY          CHAIN     PPC01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     PCREST        G4RSV
     C                   ELSE
     C                   MOVEL     *BLANKS       G4RSV
     C                   ENDIF                                                   *IN61=*OFF
     C*
     C                   MOVEL     *BLANKS       G4FIL2
     C*
     C                   WRITE     PG401
     C*
     C*** BEGIN ADD *** 01/19/01 *************************************
     C                   SETON                                            22
     C*** END ADD ***** 01/19/01 *************************************
     C*
     C                   ENDSR                                                  G4SR
     C*
     C*===============================================================
     C*  CLR - CLR VALUES IN ALL WORKFILE FIELDS
     C*===============================================================
     C*
     C     CLR           BEGSR
     C*
     C*  WXG1P FIELDS
     C*
     C                   MOVEL     *BLANKS       G1CLM
     C                   MOVEL     *BLANKS       G1SOFC
     C                   MOVEL     *BLANKS       G1EXAM
     C                   Z-ADD     *ZEROS        G1ACDT
     C                   MOVEL     *BLANKS       G1ACTM
     C                   Z-ADD     *ZEROS        G1RPDT
     C                   MOVEL     *BLANKS       G1RPTM
     C                   Z-ADD     *ZEROS        G1MDDT
     C                   MOVEL     *BLANKS       G1RPNM
     C                   MOVEL     *BLANKS       G1ACDS
     C                   MOVEL     *BLANKS       G1LTFL
     C                   MOVEL     *BLANKS       G1TYLS
     C                   MOVEL     *BLANKS       G1ACST
     C                   MOVEL     *BLANKS       G1CAST
     C                   MOVEL     *BLANKS       G1STAT
     C                   MOVEL     *BLANKS       G1PLSY
     C                   MOVEL     *BLANKS       G1POL#
     C                   MOVEL     *ZEROS        G1MODL
     C                   MOVEL     *BLANKS       G1COV#
     C                   MOVEL     *BLANKS       G1TPAB
     C                   MOVEL     *BLANKS       G1LOCD
     C                   MOVEL     *BLANKS       G1DRIV
     C                   MOVEL     *BLANKS       G1WTNS
     C                   MOVEL     *BLANKS       G1FIL1
     C*
     C*  WXG2P FIELDS
     C*
     C                   MOVEL     *BLANKS       G2CLM
     C                   Z-ADD     *ZEROS        G2SEQ#
     C                   Z-ADD     *ZEROS        G2SUIT
     C                   MOVEL     *BLANKS       G2INJ1
     C                   MOVEL     *BLANKS       G2INJ2
     C                   MOVEL     *BLANKS       G2INJ3
     C                   MOVEL     *BLANKS       G2MART
     C                   Z-ADD     *ZEROS        G2DPDT
     C                   Z-ADD     *ZEROS        G2DOB
     C                   Z-ADD     *ZEROS        G2DOD
     C                   MOVEL     *BLANKS       G2SSN
     C                   MOVEL     *BLANKS       G2OCC
     C                   Z-ADD     *ZEROS        G2DTLW
     C                   Z-ADD     *ZEROS        G2RTW
     C                   MOVEL     *BLANKS       G2LGFL
     C                   MOVEL     *BLANKS       G2LGNM
     C                   Z-ADD     *ZEROS        G2AWW
     C                   Z-ADD     *ZEROS        G2AJWG
     C                   Z-ADD     *ZEROS        G2ENTD
     C                   MOVEL     *BLANKS       G2CVGC
     C                   MOVEL     *BLANKS       G2NCCI
     C                   MOVEL     *BLANKS       G2JRST
     C                   MOVEL     *BLANKS       G2IJZP
     C                   Z-ADD     *ZEROS        G2DOH
     C                   MOVEL     *BLANKS       G2EMPS
     C                   MOVEL     *BLANKS       G2CLAS
     C                   MOVEL     *BLANKS       G2EFID
     C                   MOVEL     *BLANKS       G2SIC
     C                   MOVEL     *BLANKS       G2PYIN
     C                   MOVEL     *BLANKS       G2WAGM
     C                   Z-ADD     *ZEROS        G2OWPY
     C                   Z-ADD     *ZEROS        G2DTRP
     C                   MOVEL     *BLANKS       G2SURG
     C                   Z-ADD     *ZEROS        G2PWAG
     C                   Z-ADD     *ZEROS        G2IMP
     C                   Z-ADD     *ZEROS        G2DTDD
     C                   MOVEL     *BLANKS       G2CVER
     C                   MOVEL     *BLANKS       G2BSS
     C                   MOVEL     *BLANKS       G2BUNE
     C                   MOVEL     *BLANKS       G2BPEN
     C                   MOVEL     *BLANKS       G2BSFN
     C                   MOVEL     *BLANKS       G2BOTH
     C                   MOVEL     *BLANKS       G2FSN
     C                   MOVEL     *BLANKS       G2LSN
     C                   MOVEL     *BLANKS       G2MID
     C                   MOVEL     *BLANKS       G2ADR1
     C                   MOVEL     *BLANKS       G2ADR2
     C                   MOVEL     *BLANKS       G2CTY
     C                   MOVEL     *BLANKS       G2ST
     C                   MOVEL     *BLANKS       G2ZIP
     C                   MOVEL     *BLANKS       G2SEX
     C                   MOVEL     *BLANKS       G2LOCI
     C                   MOVEL     *BLANKS       G2OCCU
     C                   MOVEL     *ZEROS        G2STDT
     C                   MOVEL     *BLANKS       G2BILL
     C                   MOVEL     *BLANKS       G2SUBR
     C                   MOVEL     *BLANKS       G2VID#
     C                   MOVEL     *BLANKS       G2MAKE
     C                   MOVEL     *BLANKS       G2MODL
     C                   MOVEL     *ZEROS        G2YEAR
     C                   MOVEL     *BLANKS       G2FIL1
     C*
     C*  WXG3P FIELDS
     C*
     C                   MOVEL     *BLANKS       G3CLM
     C                   Z-ADD     *ZEROS        G3SEQ#
     C                   Z-ADD     *ZEROS        G3DTL#
     C                   MOVEL     *BLANKS       G3RSVC
     C                   MOVEL     *BLANKS       G3TRN#
     C                   MOVEL     *BLANKS       G3STAT
     C                   Z-ADD     *ZEROS        G3RSCH
     C                   Z-ADD     *ZEROS        G3RSV
     C                   Z-ADD     *ZEROS        G3PTD
     C                   Z-ADD     *ZEROS        G3DT
     C                   MOVEL     *BLANKS       G3TIME
     C                   Z-ADD     *ZEROS        G3PER
     C                   MOVEL     *BLANKS       G3FIL1
     C                   MOVEL     *BLANKS       G3ADJ
     C                   MOVEL     *BLANKS       G3OFS
     C                   MOVEL     *BLANKS       G3PRL
     C                   MOVEL     *BLANKS       G3FIL2
     C*
     C*  WXG4P FIELDS
     C*
     C                   MOVEL     *BLANKS       G4PLSY
     C                   MOVEL     *BLANKS       G4POL#
     C                   MOVEL     *ZEROS        G4MODL
     C                   MOVEL     *BLANKS       G4MCO#
     C                   MOVEL     *BLANKS       G4AGT#
     C                   MOVEL     *BLANKS       G4CLM
     C                   Z-ADD     *ZEROS        G4SEQ#
     C                   Z-ADD     *ZEROS        G4DTL#
     C                   Z-ADD     *ZEROS        G4TRDT
     C                   MOVEL     *BLANKS       G4PER
     C*** BEGIN CHG *** 03/14/01 **************************************
     C*******************  Z-ADD*ZEROS    G4AMT
     C                   MOVE      *ZEROS        G4AMT
     C*** END CHG ***** 03/14/01 **************************************
     C                   Z-ADD     *ZEROS        G3RSCH
     C                   MOVEL     *BLANKS       G4TRCD
     C                   MOVEL     *BLANKS       G4PNAM
     C                   MOVEL     *BLANKS       G4ADR1
     C                   MOVEL     *BLANKS       G4ADR2
     C                   MOVEL     *BLANKS       G4CTST
     C                   MOVEL     *BLANKS       G4ZIP
     C                   MOVEL     *BLANKS       G41099
     C                   MOVEL     *BLANKS       G4FEIN
     C                   MOVEL     *BLANKS       G4FIL1
     C                   MOVEL     *BLANKS       G4REC
     C                   MOVEL     *BLANKS       G4BR2
     C                   MOVEL     *BLANKS       G4PRL
     C                   MOVEL     *BLANKS       G4RSV
     C                   MOVEL     *BLANKS       G4FIL2
     C*
     C                   ENDSR                                                  CLR
     C*
     C*** Begin add *** 04/12/01 **************************************
     C*================================================================
     C*  SRPTD - This routine loads the paid to date totals.
     C*===============================================================
     C     SRPTD         BEGSR
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   MULT      -1            PAAMT
     C                   ENDIF
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RC(X)                                  90
     C     *IN90         IFEQ      *ON
     C                   ADD       PAAMT         AR(X)
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RC(X)                                  90
     C                   MOVE      PARECD        RC(X)
     C                   ADD       PAAMT         AR(X)
     C*
     C                   ENDIF                                                  *IN90=*ON
     C*
     C                   ENDSR
     C*** End add ***** 04/12/01 **************************************
