     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W9209R - JDE CLAIM PAYMENT DOWNLOAD
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*   This program reads Payment Detail records and accumulates   *
     H*   sums for each transaction code.  Stop Loss Reimbursement    *
     H*   transactions will be pulled from the Reinsurance Recovery   *
     H*   file.  Only records with a transaction date equal to the    *
     H*   batch date are included.  The totals are written to the     *
     H*   T57901P file.  This file is cleared at the beginning of the *
     H*   process (in W9209C).                                        *
     H*                                                               *
     H*  CALLED FROM: W9209C                                          *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER: C:35                                             *
     H*  CREATED:    09/07/95         PROJECT: 25WAW                  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/23/10 C:65  PROJECT 25WQI/SUPP#73846                      *
     H*                 CHANGE THE FYR PORTION OF Q1MCU/MCU2 TO BE    *
     H*                 BASED ON 9/1 OF EACH YEAR.                    *
     H*                                                               *
     H*  07/22/10 C:191 SUPPORT: 73264 WA025                          **
     H*                 FUND YEAR NOT RETRIEVED CORRECTLY             *
     H*                                                               *
     H*  03/03/10 C:191 PROJECT 25WQI PRISS #28                       **
     H*                 DATA ERROR
     H*                                                               *
     H*  11/17/09 C:191 PROJECT 25WQI                                 **
     H*                 RE-WRITE BASED ON EXISTING PGM
     H*                 Write output to new record format F5503B11    *
     H*                                                               *
     H* 04/21/04 C:09 PROJECT 25WNU                                   *
     H*               Rick requested the following new offset acct    *
     H*               value be added: 1212.                           *
     H*                                                               *
     H* 03/30/04 C:09 PROJECT 25WNU                                   *
     H*               Check for 2 new offset acct values: 2184, 1222  *
     H*               per Rick H.                                     *
     H*                                                               *
     H* 11/06/98 C:61 PROJECT 25WJE                                   *
     H*               Convert for V3.                                 *
     H*                                                               *
     H* 11/05/97  C:86  PROJECT:25WIU                                 *
     H*       1.  CHANGED Q1CO TO HAVE BLANKS INSTEAD OF 005,         *
     H*       2.  CHANGED Q1PO TO BE 4 IF OFFSET ACCT = 1230 OR 1201, *
     H*       3.  CHANGED Q1MCU TO BE 4 IF OBJECT ACCT = 1230 OR 1201,*
     H*           TO CHANGE G/L ACCOUNT DEFINITIONS.                  *
     H*                                                               *
     H* 03/13/96  C:35  PROJECT:25WAW                                 *
     H*                 LOAD Q1PO WITH COST CENTER FOR OFFSET ACCOUNT *
     H*                 USING SAME METHOD AS OBJECT ACCOUNT.          *
     H*                                                               *
     H* 03/12/96  C:35  PROJECT:25WAW                                 *
     H*                 ONLY CONCATENATE FYR ONTO COST CENTER IF      *
     H*                 OBJECT ACCOUNT >= 4000 AND <= 8999.           *
     H*                                                               *
     H* 02/22/96  C:35  PROJECT:25WAW                                 *
     H*                 CHANGED TO DETERMINE FUND YEAR BASED ON THE   *
     H*                 BATCH DATE.                                   *
     H*                                                               *
     H* 11/28/95  C:35  PROJECT:25WAW                                 *
     H*                 1. CHANGED CODE TO USE FIELD Q1AG AS 15 BYTE  *
     H*                    NUMERIC WITH AN IMPLIED DECIMAL POINT.     *
     H*                 2. CORRECTED SPELLING OF FIELDS.              *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 60 - GENERAL READ/CHAIN INDICATOR                             *
     H* 61 - LOOP EXIT INDICATOR                                      *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*  DISPLAY FILES:                                               *
     H*     (NONE)                                                    *
     H*                                                               *
     H*  DATA FILES:                                                  *
     H*     T57901P (P57901P): ------------                           *
     H*     WTABL   (PPC01):   CO#/KEY                                *
     H*     WDPAL9  (PPA01):   CO#/TRDT                               *
     H*     WDRVL   (PRV01):   CO#                                    *
     H*                                                               *
     H*****************************************************************
      *
      *
     FF5503B11  O    E             DISK
     FWMJEL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     FWDPALH    IF   E           K DISK
     FWDRVL     IF   E           K DISK
     F*** BEGIN DEL ****** 08/23/10 ************************************
     F*****WDF1L1  IF  E           K        DISK
     F*** END DEL ******** 08/23/10 ************************************
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XNFYR                  1      3  0
     D  XCFYR                  1      3
     D                 DS
     D  XXOBJ                  1      4
     D  XNOBJ                  1      4  0
     D                 DS
     D  XXGLC                  1      4
     D  XNGLC                  1      4  0
      *
      *  ORDER ID
      *
     D                 DS
     D  XXBORI                 1     50
     D  Q1DCT                  1      2
     D  Q1DOC                  3     10  0
     D  Q1SFX                 11     13
      *
      *  DATE FIELD WITH SEPARATORS
      *
     D                 DS
     D  XXDT10                 1     10
     D  XXDTMM                 1      2
     D  XXSEP1                 3      3
     D  XXDTDD                 4      5
     D  XXSEP2                 6      6
     D  XXDTYY                 7     10
      *
      *  DATE FIELD
      *
     D                 DS
     D  XXDT8                  1      8
     D  XXDTM8                 1      2
     D  XXDTD8                 3      4
     D  XXDTY8                 5      8
     D*** BEGIN ADD ***  08/23/10  *************************************
     D                 DS
     D  X1DATE                 1      7  0
     D  XXDTY                  1      3  0
     D  XXDTM                  4      5  0
     D  XXDTD                  6      7  0
     D*** END ADD *****  08/23/10  *************************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D XXDESC          C                   CONST('STOP LOSS REIMBURSEM-
     D                                     ENT')
      *

     C     *LIKE         DEFINE    PACO#         XXCO#
     C                   Z-ADD     0             XXRV$            15 2
      *
      *  KEY LIST FOR THE TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
      *  KEY LIST FOR THE PAYMENT DETAIL FILE
      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTRDT            7 0
      *
      *  KEY LIST FOR THE REINSURANCE RECOVERY FILE
      *
     C     RVKY          KLIST
     C                   KFLD                    XXCO#
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#            9 0
     C                   KFLD                    XXDIV             5 0
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C*** BEGIN DEL ****** 08/23/10 ************************************
     C*********  F1KY      KLIST
     C*********            KFLD           XXCO#
     C*********            KFLD           XXFND
      *
     C*********  F1KY1     KLIST
     C*********            KFLD           XXCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXTRDT
     C*** END DEL ******** 08/23/10 ************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C                   MOVE      *BLANKS       XCFYR             3
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     0             XXRV$            15 2
     C*** BEGIN ADD ****** 08/23/10 ************************************
     C                   TIME                    DSDTTM
     C*** END ADD ******** 08/23/10 ************************************
      *
      * MAIN PROCESS
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        XXTMP1            8 0
     C                   Z-ADD     XXTMP1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT            7 0
      *
     C                   MOVE      ' '           ISRV              1
      *
      * CHAIN TO JDE CONTROL FILE
     C     XXCO#         CHAIN     PJE01                              50
      *
     C*** BEGIN DEL ****** 07/06/10 ************************************
      * MOVE CODE TO INSIDE LOOP ON PAYMENT FILE AS XXFND DOESN'T EXIST YET
      * DETERMINE CURRENT FUND YEAR
      *
     C**********           Z-ADD0         XNFYR
     C********** F1KY1     SETGTPF101
     C********** F1KY      REDPEPF101                    60
     C********** *IN60     IFEQ '0'
     C**********           Z-ADDF1FYR     XNFYR
     C**********           ENDIF                           *IN60='0'
     C*** END   DEL ****** 07/06/10 ************************************
      *
      * READ THROUGH PAYMENT DETAIL FILE
      *
     C     PAKY          CHAIN     PPA01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
      * VALIDATE IF RECORD SHOULD BE INCLUDED
     C                   EXSR      SRVLD
     C     XXTRAN        IFEQ      'Y'
     C                   EXSR      SRINZ
     C                   Z-ADD     PAFND         XXFND             3 0
     C                   Z-ADD     PAFYR         XXFYR
     C                   Z-ADD     PACASE        XXCASE
     C                   Z-ADD     PAAMT         XXAMT             9 2
     C*** BEGIN ADD ****** 07/22/10 ************************************
      *
      * DETERMINE CURRENT FUND YEAR
     C*** BEGIN CHG ****** 08/23/10 ************************************
     C*********            Z-ADD0         XNFYR
     C*********  F1KY1     SETGTPF101
     C*********  F1KY      REDPEPF101                    60
     C*********  *IN60     IFEQ '0'
     C*********            Z-ADDF1FYR     XNFYR
     C*********            ENDIF                           *IN60='0'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
     C     XXDTM         IFGE      9
     C     XXDTM         ANDLE     12
     C                   Z-ADD     XXDTY         XNFYR
     C                   ELSE
     C     XXDTY         SUB       1             XNFYR
     C                   ENDIF
     C*** END CHG ******** 08/23/10 ************************************
     C*** END   ADD ****** 07/22/10 ************************************
      *
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRD8            8 0
      *
     C                   Z-ADD     PAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C*
     C                   EXSR      SRWRIT
     C                   ENDIF
      *
      * READ NEXT PAYMENT DETAIL RECORD
      *
     C     PAKY          READE     PPA01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
      * READ THROUGH REINSURANCE RECOVERY FILE
      *
     C     RVKY          CHAIN     PRV01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
      * PROCESS RECORD ONLY IF TRANSACTION DATE EQUALS BATCH DATE
      *
     C     RVTRDT        IFEQ      XXTRDT
     C     RVAMT         ANDNE     0
      * SKIP TEST FUND
     C                   Z-ADD     RVFND         XXFND             3 0
     C     FNKY          CHAIN     WMFNL                              65
     C     *IN65         IFEQ      '0'
     C     FNTEST        ANDNE     'Y'
     C                   ADD       RVAMT         XXRV$
     C                   ENDIF                                                  TEST FUND
     C                   ENDIF                                                  RVTRDT IFEQ
     C*** BEGIN ADD ****** 07/22/10 ************************************
      *
      * DETERMINE CURRENT FUND YEAR
     C*** BEGIN CHG ****** 08/23/10 ************************************
     C*********            Z-ADD0         XNFYR
     C*********  F1KY1     SETGTPF101
     C*********  F1KY      REDPEPF101                    60
     C*********  *IN60     IFEQ '0'
     C*********            Z-ADDF1FYR     XNFYR
     C*********            ENDIF                           *IN60='0'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
     C     XXDTM         IFGE      9
     C     XXDTM         ANDLE     12
     C                   Z-ADD     XXDTY         XNFYR
     C                   ELSE
     C     XXDTY         SUB       1             XNFYR
     C                   ENDIF
     C*** END CHG ******** 08/23/10 ************************************
     C*** END   ADD ****** 07/22/10 ************************************
      * READ NEXT REINSURANCE RECOVERY RECORD
      *
     C     RVKY          READE     PRV01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     XXRV$         IFNE      0
      * INITIALIZE FIELDS FOR STOP LOSS REIMBURSEMENT DOWNLOAD RECORD
      *
     C                   MOVE      'JW'          Q1DCT
     C                   MOVE      '1382'        Q1OBJ
     C                   MOVE      *BLANKS       Q1EXA
     C                   MOVE      *BLANKS       Q1EXR
     C                   MOVEL     XXDESC        Q1EXA
     C                   MOVEL     XXDESC        Q1EXR
     C                   MOVE      XXRV$         Q1AG
     C                   MOVE      XXRV$         Q1AA
     C                   MOVE      '1210'        Q1GLC
      *  WRITE RECORD TO INTERFACE FILE
      *
     C                   WRITE     I5503B
     C                   ENDIF                                                  XXRV$ <> 0
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      ************************************************************************
      *  SRVLD - VALIDATE RECORD FOR INCLUSION                               *
      ************************************************************************
      *
     C     SRVLD         BEGSR
      *
     C                   MOVE      ' '           XXTRAN            1
      * PROCESS RECORD ONLY IF IT IS NOT AN ADJUSTMENT
      *
     C     PATYPE        IFNE      'A'
     C                   MOVE      'Y'           XXTRAN
     C                   ENDIF                                                  PATYPE IFNE
      *
      * SKIP TEST FUND
     C                   Z-ADD     PAFND         XXFND             3 0
     C     FNKY          CHAIN     WMFNL                              65
     C     *IN65         IFEQ      '0'
     C     FNTEST        ANDEQ     'Y'
     C                   MOVE      ' '           XXTRAN
     C                   ENDIF
      *
     C                   ENDSR
      *
      ************************************************************************
      *  SRINZ - INITIALIZE INTERFACE FILE FIELDS                            *
      ************************************************************************
      *
     C     SRINZ         BEGSR
      *
     C                   MOVE      *BLANKS       Q1BSTS
     C                   MOVE      *BLANKS       Q1BINT
     C*** BEGIN CHG ****** 03/03/10 ************************************
     C**********           MOVE *ZEROS    Q1BPTY
     C                   MOVE      *BLANKS       Q1BPTY
     C*** END   CHG ****** 03/03/10 ************************************
     C                   MOVE      *ZEROS        Q1AN8
     C                   MOVE      *ZEROS        Q1BORI
     C                   Z-ADD     1             Q1LIN
     C                   MOVE      *BLANKS       Q1DCT
     C                   MOVE      *ZEROS        Q1DOC
     C                   MOVE      '001'         Q1SFX
     C                   MOVE      *BLANKS       Q1CO
     C                   MOVE      *BLANKS       Q1MCU
     C                   MOVE      *BLANKS       Q1MCU2
     C                   MOVE      *BLANKS       Q1OBJ
     C                   MOVE      *BLANKS       Q1SUB
     C                   MOVE      *BLANKS       Q1SBL
     C                   MOVE      *BLANKS       Q1SBLT
     C                   MOVE      *ZEROS        Q1AA
     C                   MOVE      *ZEROS        Q1AG
     C                   MOVE      *BLANKS       Q1EXA
     C                   MOVE      *BLANKS       Q1EXR
     C                   MOVE      *BLANKS       Q1PTC
     C                   MOVE      *ZEROS        Q1GLDT
     C                   MOVE      *ZEROS        Q1DTEN
     C                   MOVE      *ZEROS        Q1CKAM
     C*** BEGIN CHG ****** 03/03/10 ************************************
     C*********            MOVE *ZEROS    Q1DMTJ
     C                   MOVE      *BLANKS       Q1DMTJ
     C*** END   CHG ****** 03/03/10 ************************************
     C                   MOVE      *BLANKS       Q1CKNU
     C                   MOVE      *ZEROS        Q1PAAP
     C                   MOVE      *ZEROS        Q1AAP
     C                   MOVE      *BLANKS       Q1GLBA
     C                   MOVE      *BLANKS       Q1GLC
     C                   MOVE      *ZEROS        Q1DI
     C                   MOVE      *ZEROS        Q1DDJ
     C*** BEGIN ADD ****** 03/03/10 ************************************
     C**********           MOVE 'GL'      Q1JDMD
     C                   MOVEL     'GL'          Q1JDMD
     C*** END   ADD ****** 03/03/10 ************************************
     C                   MOVE      *BLANKS       Q1LNTY
     C                   MOVE      *ZEROS        Q1UORG
     C                   MOVE      *BLANKS       Q1LITM
     C                   MOVE      *BLANKS       Q1DSC1
     C                   MOVE      *BLANKS       Q1DSC2
     C                   MOVE      *BLANKS       Q1PMCT
     C* DATE SEPARATOR
     C                   MOVE      '/'           XXSEP1
     C                   MOVE      '/'           XXSEP2
      *
     C                   MOVEL     JESTCD        Q1BSTS
     C                   MOVEL     JEITYP        Q1BINT
      *
     C                   ENDSR
      *
      ************************************************************************
      *  SRWRIT- WRITE RECORD TO INTERFACE FILE                              *
      ************************************************************************
      *
     C     SRWRIT        BEGSR
      *
      * CHAIN TO CLAIM RECORD
     C     CMKY          CHAIN     PCM01                              50
     C     *IN50         IFEQ      *OFF
      *
      *  CHAIN TO EMPLOYER MASTER FILE TO GET DISTRICT
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C     EMKY          CHAIN     PEM01                              50
     C                   Z-ADD     0             Q1AN8
     C     *IN50         IFEQ      '0'
     C     6             SUBST     EMSEMP:2      XX6A              6
     C                   MOVE      XX6A          Q1AN8
     C                   ENDIF
      *
     C     ELKY          CHAIN     PEL01                              50
     C*
     C                   SELECT
     C     FNPFFL        WHENEQ    'C'
     C                   MOVEL     'WCASO '      Q1BPTY
     C     ELAMAG        WHENNE    0
     C                   MOVEL     'WCAD  '      Q1BPTY
     C                   OTHER
     C                   MOVEL     'WCPOOL'      Q1BPTY
     C                   ENDSL
     C                   ENDIF                                                  CLAIM RECORD
      *
     C* BUILD ORDERID - CONCATENATION OF EAFND, EAEMP#, EADIV, EAFYR, EAARCD
     C*
     C* LOAD GL DATE WITH SEPARATOR
      *
     C                   MOVE      XXTRD8        XXDT8
     C                   EXSR      SRLDDT
     C                   MOVE      XXDT10        Q1GLDT
     C                   MOVE      Q1GLDT        Q1DTEN
     C*
     C                   MOVE      XXPODT        XXDT8
     C                   EXSR      SRLDDT
     C                   MOVE      XXDT10        Q1DI
     C                   MOVE      Q1DI          Q1DDJ
      *
      * CHAIN TO TRANSACTION CODE TABLE FILE
      *
     C                   MOVEL(P)  PATRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
      *
     C     *IN60         IFEQ      *OFF
      * INITIALIZE FIELDS IN DOWNLOAD RECORD
      *
     C                   MOVE      PCGLDT        Q1DCT
     C                   MOVE      PCGLOA        Q1OBJ
     C                   MOVEL     PCDESC        Q1EXA
     C                   MOVEL     PCDESC        Q1EXR
     C                   MOVE      PCGLOF        Q1GLC
     C                   ELSE
     C                   MOVE      *BLANKS       Q1DCT
     C                   MOVE      *BLANKS       Q1OBJ
     C                   MOVE      *BLANKS       Q1EXA
     C                   MOVE      *BLANKS       Q1EXR
     C                   MOVE      *BLANKS       Q1GLC
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   MOVEL     XXBORI        Q1BORI
      *
     C                   MOVE      *BLANKS       Q1MCU
     C                   MOVEL     '5'           Q1MCU
      *
     C     Q1OBJ         IFNE      *BLANKS
      *
     C                   MOVE      Q1OBJ         XXOBJ
      *
     C     XNOBJ         IFGE      4000
     C     XNOBJ         ANDLE     8999
     C                   CAT       XCFYR:0       Q1MCU
     C                   ENDIF                                                  XNOBJ IFGE
      *
     C     XNOBJ         IFEQ      1230
     C     XNOBJ         OREQ      1201
     C     XNOBJ         OREQ      2184
     C     XNOBJ         OREQ      1222
     C     XNOBJ         OREQ      1212
     C                   MOVEL(P)  '4'           Q1MCU
     C                   ENDIF                                                  XNOBJ IFEQ
     C                   ENDIF                                                  Q1OBJ IFNE
      *
     C                   MOVE      *BLANKS       Q1MCU2
     C                   MOVEL     '5'           Q1MCU2
     C     Q1GLC         IFNE      *BLANKS
      *
     C                   MOVE      Q1GLC         XXGLC
      *
     C     XNGLC         IFGE      4000
     C     XNGLC         ANDLE     8999
     C                   CAT       XCFYR:0       Q1MCU2
     C                   ENDIF                                                  XNGLC IFGE
      *
     C     XNGLC         IFEQ      1230
     C     XNGLC         OREQ      1201
     C     XNGLC         OREQ      2184
     C     XNGLC         OREQ      1222
     C     XNOBJ         OREQ      1212
     C                   MOVEL(P)  '4'           Q1MCU2
     C                   ENDIF                                                  XNGLC IFNE
      *
     C                   ENDIF                                                  Q1GLC IFNE
      *
     C                   MOVE      XXAMT         Q1AG
     C                   MOVE      XXAMT         Q1AA
      *
      *  WRITE RECORD TO INTERFACE FILE
      *
     C                   WRITE     I5503B
      *
     C                   ENDSR
      *
      ************************************************************************
      *  SRLDDT- LOAD 8,0 DATE TO 10A DATE WITH SEPARATOR                    *
      ************************************************************************
     C     SRLDDT        BEGSR
     C                   MOVE      XXDTM8        XXDTMM
     C                   MOVE      XXDTD8        XXDTDD
     C                   MOVE      XXDTY8        XXDTYY
     C                   ENDSR
