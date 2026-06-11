     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7038R - CLT121 - CLARENDON BANK DOWNLOAD
     H*****************************************************************
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*      W7038C - OVERRIDE                                        *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*      NONE                                                     *
     H*                                                               *
     H* Created  10/29/03  C:132 PROJECT: 121WAR                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/06 C:15   Project: 121WAR  SEQ: 53104                   *
     H*                 Added code to download checks from the new    *
     H*                 return premium file WAR4P.                    *
     H*                                                               *
     H* 08/31/04 C: 53  PROJECT: 121WBB
     H*                 CREATE IN CLT129
     H*                                                               *
     H* 01/07/04 C:132  PROJECT: 121WAR  SEQ: 40619                   *
     H*                 Chg code to put in correct type for Group    *
     H*                 checks into XXTYPE field.                     *
     H*                                                               *
     H* 01/02/04 C:062  PROJECT: 121WAR  SEQ: 40619                   *
     H*                 DEFAULT XXTYPE = '1' FOR GROUP CHECKS         *
     H*                                                               *
     H* 11/19/03 C:132  PROJECT: 121WAR  SEQ: 40046                   *
     H*                 CHANGE TEXT MSG IN OUTPUT FILE RECORD         *
     H*                                                               *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * WCMS Payment Detail File  (OPNQRYF from W7036C - CO#/ACCT/CHK#)
     FWDPALI    IF   E           K DISK
      *
      * Check Reconcilation File
     FBKCRP     IF   E             DISK
      *
      * WCMS Payment Detail File  (OPNQRYF from W7036C - CO#/ACCT/CHK#)
     FAPBIL1    IF   E           K DISK
      *
     F*** Begin add *** 06/09/06 **************************************
      * Return Premium Check File
     FWAR4L7    IF   E           K DISK
     F*** End add ***** 06/09/06 **************************************
      *
      * Clarendon Bank Download
     FWXBR1P    O    E             DISK
      *

     D BANKID          C                   CONST('6301538124509')
      *
     D TESTXT          C                   CONST('TEST CLARENDON BANK-
     D                                      DOWNLOAD')
      *
     D LIVMSG          C                   CONST('CLARENDON BANK DOWN-
     D                                     LOAD')
      * ZZZ
     D                 DS
     D  XAMT                   1     11
     D  XAMTDD                 1      8  0
     D  XAMTPP                 9      9
     D  XAMTCC                10     11  0
      *
     D                 DS
     D  XXAMTX                 1      9
     D  XXAMDD                 1      7  0
     D  XXAMCC                 8      9  0
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
      *
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *
      *
      *==================================================================
      * KLISTS
      *==================================================================
      *
      *  Payment Detail
      *

     C     PAKY1         KLIST
     C                   KFLD                    COCO#
     C                   KFLD                    @@ACCT
      *
     C     PAKY2         KLIST
     C                   KFLD                    COCO#
      *
      *  Check Reconcilation file
      *
     C     BIKY          KLIST
     C                   KFLD                    BIFND
     C                   KFLD                    BIAPBK
      *
     C     BIKY1         KLIST
     C                   KFLD                    CRCO#
     C                   KFLD                    CRBNK#
     C                   KFLD                    ZZCKDT
      *
      *=================================================================
      * *LIKE DEFN
      *=================================================================
     C     *LIKE         DEFINE    PAACCT        XXACCT
     C     *LIKE         DEFINE    PATRDT        XXFRDT
     C     *LIKE         DEFINE    PATRDT        XXTODT
     C     *LIKE         DEFINE    PACHK#        SVCHK#
     C     *LIKE         DEFINE    PACHK#        XXCHK#
     C     *LIKE         DEFINE    BRCHKD        XXCHDT
     C     *LIKE         DEFINE    CRCDT         ZZCKDT
     C     *LIKE         DEFINE    CRBNK#        SVBNK#
     C     *LIKE         DEFINE    CRCO#         SVCO#
      *
      *=================================================================
      *
      *=================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C                   PARM                    @@ACCT           20
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *===============================================================
      *
      * Initialize
      *
     C                   MOVE      @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
      *
     C                   MOVE      @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
      *
     C                   Z-ADD     *ZERO         XXTAMT           15 2
     C                   Z-ADD     *ZERO         XXTCNT            7 0
      *
      * For each company
      *
     C                   READ      PCO01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
      * Process WCMS payments
      *
     C     PAKY1         CHAIN     PPA01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     PAPGTP        IFEQ      'G'
     C                   EXSR      GRPCK1
      *
     C                   ELSE
      *
     C     PATRDT        IFGE      XXFRDT
     C     PATRDT        ANDLE     XXTODT
     C     PAACCT        ANDEQ     @@ACCT
     C                   MOVEL(P)  PAACCT        XXACCT
     C                   Z-ADD     PACHK#        XXCHK#
     C                   Z-ADD     PAAMT         XXAMT             9 2
      *
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCHDT
      *
     C     PATYPE        IFEQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   MOVE      '1'           XXTYPE            1
     C                   ELSE
     C                   MOVE      '2'           XXTYPE
     C                   ENDIF                                                  PATYPE = M, P, Z
      *
     C                   EXSR      WRTBRP
      *
     C                   ENDIF                                                  TRDT
      *
     C     PAKY1         READE     PPA01                                  60
     C                   ENDIF                                                  PGTP = G
      *
     C                   ENDDO
      *
      * Process Premium Refund Check
      *
     C                   READ      PCR01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CRCLR         IFEQ      'V'
     C                   Z-ADD     CRCDT         ZZCKDT
     C                   MOVE      '2'           XXTYPE
     C                   ELSE
     C                   Z-ADD     CRCHKD        ZZCKDT
     C                   MOVE      '1'           XXTYPE
     C                   ENDIF                                                  CRCLR = 'V'
     C     ZZCKDT        IFGE      XXFRDT
     C     ZZCKDT        ANDLE     XXTODT
     C*
      * If the bank number changes, then get Bank Account number from
      * APBIL1 file to write out in output file.
      *
     C     CRBNK#        IFNE      SVBNK#
     C     CRCO#         ORNE      SVCO#
     C                   MOVE      *BLANKS       XXACCT
     C*
     C********   BIKY1     SETGTPBI01
     C                   Z-ADD     CRCO#         BIFND
     C                   Z-ADD     CRBNK#        BIAPBK
      *
     C     BIKY          CHAIN     PBI01                              61
     C*
     C     *IN61         DOUEQ     *ON
     C     BIEFDT        IFLT      ZZCKDT
     C     BISTAT        ANDNE     'I'                                          INACTIVE ?
     C                   MOVEL(P)  BIACCT        XXACCT
     C                   Z-ADD     CRBNK#        SVBNK#
     C                   Z-ADD     CRCO#         SVCO#
     C                   ENDIF
     C     BIKY          READE     PBI01                                  61
     C                   ENDDO
      *
     C                   ENDIF                                                  CRBNK# <> SVBNK#
      *
     C     XXACCT        IFEQ      @@ACCT
      *
     C                   Z-ADD     CRCHK#        XXCHK#
     C                   Z-ADD     CRAMT         XXAMT
      *
     C                   Z-ADD     ZZCKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCHDT
      *
     C                   EXSR      WRTBRP
     C                   ENDIF                                                  XXACCT = @@ACCT
      *
     C                   ENDIF                                                  ZZCKDT
      *
     C                   READ      PCR01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *
     C*** Begin add *** 06/09/06 **************************************
      * Process WCMS return premium payments.
      *
     C     PAKY1         CHAIN     PR401                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     R4CKDT        IFGE      XXFRDT
     C     R4CKDT        ANDLE     XXTODT
     C     R4ACCT        ANDEQ     @@ACCT
      *
     C                   MOVEL(P)  R4ACCT        XXACCT
     C                   Z-ADD     R4CHK#        XXCHK#
     C                   Z-ADD     R4AMT         XXAMT             9 2
      *
     C                   Z-ADD     R4CKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCHDT
      *
     C     R4VDSP        IFEQ      'V'
     C     R4VDSP        OREQ      'S'
     C                   MOVE      '2'           XXTYPE
     C                   ELSE
     C                   MOVE      '1'           XXTYPE
     C                   ENDIF
      *
     C                   EXSR      WRTBRP
      *
     C                   ENDIF                                                  TRDT
      *
     C     PAKY1         READE     PR401                                  60
     C                   ENDDO
     C*** End add ***** 06/09/06 **************************************
      *
     C                   READ      PCO01                                  60
     C                   ENDDO                                                  SMCOL
      *
      ***** TERM PROGRAM ****
     C     TERM          TAG
      *
     C                   MOVE      *ON           *INLR
      *
      *=============================================================
      *  GRPCK1 - Process Group Check
      *=============================================================
     C     GRPCK1        BEGSR
     C                   Z-ADD     *ZERO         XXAMT
     C                   Z-ADD     PACHK#        SVCHK#
     C                   MOVE      'Y'           XX1ST             1
     C*** BEGIN ADD *** 01/02/04 **************************************
      ***** BEG DEL ***** 01/07/04 ************************************
     C****************     MOVE '1'       XXTYPE    P
      ***** END DEL ***** 01/07/04 ************************************
     C*** END   ADD *** 01/02/04 **************************************
      *
      ***** BEG ADD ***** 01/07/04 ************************************
     C     PATYPE        IFEQ      'M'
     C     PATYPE        OREQ      'P'
     C     PATYPE        OREQ      'Z'
     C                   MOVE(P)   '1'           XXTYPE
     C                   ELSE
     C                   MOVE      '2'           XXTYPE
     C                   ENDIF                                                  PATYPE = M, P, Z
      ***** END ADD ***** 01/07/04 ************************************
      *
     C     *IN60         DOWEQ     *OFF
     C     PACHK#        ANDEQ     SVCHK#
      *
     C     PATRDT        IFGE      XXFRDT
     C     PATRDT        ANDLE     XXTODT
     C     PAACCT        ANDEQ     @@ACCT
      *
     C     XX1ST         IFEQ      'Y'
     C                   MOVE      'N'           XX1ST
     C                   MOVEL(P)  PAACCT        XXACCT
     C                   Z-ADD     PACHK#        XXCHK#
      *
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCHDT
      *
     C                   ENDIF                                                  XX1ST = 'Y'
      *
     C                   ADD       PAAMT         XXAMT
     C                   ENDIF                                                  TRDT
      *
     C     PAKY1         READE     PPA01                                  60
     C                   ENDDO
      *
     C     XXAMT         IFNE      *ZERO
     C                   EXSR      WRTBRP
     C                   ENDIF
      *
     C                   ENDSR                                                  GRPCK1
      *
      *=============================================================
      *  WRTBTP - Write record to WXBTP
      *=============================================================
     C     WRTBRP        BEGSR
      *
     C                   CLEAR                   PBR01
      *
     C                   Z-ADD     *ZEROS        XAMTCC
     C                   Z-ADD     *ZEROS        XAMTDD
     C                   MOVE      XXAMT         XXAMTX
     C                   Z-ADD     XXAMCC        XAMTCC
     C                   Z-ADD     XXAMDD        XAMTDD
     C                   MOVE      '.'           XAMTPP
      *
     C                   MOVEL     XXTYPE        BRTYPE
     C                   MOVEL(P)  XXACCT        BRACCT
     C*********            MOVELBANKID    BRACCT    P
     C                   MOVE      *ZERO         BRCHK#
     C                   MOVE      XXCHK#        BRCHK#
     C                   MOVE      XAMT          BRAMT
     C                   MOVE      XXCHDT        BRCHKD
     C*********            MOVE *BLANKS   BRFILL
     C*********            MOVELTESTXT    BRFILL    P      TEST MESSAGE
     C                   MOVEL(P)  LIVMSG        BRFILL                         LIVE MESSAGE
      *
     C                   ADD       XXAMT         XXTAMT
     C                   ADD       1             XXTCNT
      *
     C                   WRITE     PBR01
      *
     C                   ENDSR                                                  WRTBRP
      *
      *=============================================================
