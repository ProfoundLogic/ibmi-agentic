     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N95DCR - TYPE DC DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program caculates the network discount amount by   **
     H**     taking a percentage off the State Fee Schedule Reim-    **
     H**     bursement Amount and comparing that amount to the bill- **
     H**     ed charges and pay whichever is less.                   **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     N9500R - Process Detail Charges to Correct Discount     **
     H**   CALLS TO:                                                 **
     H**     QCMDEXC - IBM Command                                   **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   10/21/97      PROJECT: 48MAY                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*  06/03/99 C:31  Support: 18284
     H*                 Negative network discount
     H*                 1. Replace with CAMRAA with CATOTP + CADIFF
     H*
     H*  02/19/99 C:31  Project: 95MAR
     H*                 DC discount changes
     H*                 1. Add type 4 provider support
     H*                 2. Add No-Fee support
     H*                 3. Add CAMNAA support
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      General Usage                                  **
     H**      61      Case Detail File not found                     **
     H**      80      Update Case Detail record                      **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FDCALN (PCA01): Case Detail File (CO#/SUB#/FND/FYR/    **
     H**                      CASE/FEIN/MOD/FDAT/DSEQ/FLAG <> 'P')   **
     H**      FTABL         : Logical over table files (CO#/KEY)     **
     H**             (PCS01): All States Claim Status Code Table     **
     H**      NMBDLD (PBD01): Billing Rate/Discount Master           **
     H**                      (CODE/EFDT/RCDT = 'D')                 **
     H*****************************************************************
     H*
     FFDCALN    UF   E           K DISK
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PRC01)
     FNMBDLD    IF   E           K DISK

     C*
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C*
     C*          FDCALN (PCA01): Case Detail File
     C     CAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXFDAT            7 0
     C                   KFLD                    XXDSEQ
     C*
     C*         NMBDLD (PBD01): Billing Rate/Discount Master
     C     BDKY          KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    XXFDAT
     C*
     C*          FTABL  (PCS01): All States Claim Status Code Table
     C     CSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C* ---------------------------------------------------------------
     C*        PARM LISTS
     C* ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFUND            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFDAT            8
     C                   PARM                    QQDSEQ            5
     C                   PARM                    QQCODE            3
     C                   PARM                    QQCEDT            8
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFL1             1
     C                   PARM                    QQFL2             1
     C                   PARM                    QQFL3             1
     C                   PARM                    QQFL4             1
     C                   PARM                    QQFL5             1
     C                   PARM                    QQDS1            10
     C                   PARM                    QQDS2            10
     C                   PARM                    QQDS3            10
     C                   PARM                    QQDS4            10
     C                   PARM                    QQDS5            10
     C                   PARM                    QQRTCD            1
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
     C* ---------------------------------------------------------------
     C*        DEFINES
     C* ---------------------------------------------------------------
     C     *LIKE         DEFINE    BDPC01        XXPC00
     C     *LIKE         DEFINE    CAAST         XXAST
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C     *LIKE         DEFINE    CATOTA        XXTOTA
     C     *LIKE         DEFINE    CADIFF        XXDIFF
     C     *LIKE         DEFINE    CAUNTS        X1UNTS
     C     *LIKE         DEFINE    CAMRAA        XXALOW
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Move parms to keys
     C*          ------------------------------------------------------
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFUND            3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCODE        XXCODE            3
     C*
     C*          Get case detail record
     C*          ------------------------------------------------------
     C     CAKY          CHAIN     PCA01                              61
     C     *IN61         IFEQ      '0'
     C*** BEGIN CHG *** 02/19/99 **************************************
     C     CATOTP        ANDNE     0
     C*** END CHG *** 02/19/99 ****************************************
     C*
     C*             Set up adjudication state
     C*             ---------------------------------------------------
     C     CAAST         IFEQ      *BLANKS
     C                   MOVE      'OK'          XXAST
     C                   ELSE                                                   AST = Blanks
     C                   MOVE      CAAST         XXAST
     C                   ENDIF                                                  AST = Blanks
     C*
     C*             Get claim status code - do not process if 'C' (CAP)
     C*             ---------------------------------------------------
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     CSKY          CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSDISC        ANDNE     'C'
     C*
     C*                Get Discount Master for date of service
     C*                ------------------------------------------------
     C                   Z-ADD     0             XXPC00
     C                   Z-ADD     0             XXNDIS
     C                   Z-ADD     0             XXALOW
     C     BDKY          SETGT     PBD01
     C                   READP     PBD01                                  60
     C     *IN60         DOWEQ     *OFF
     C     BDCODE        ANDEQ     QQCODE
     C     BDSTAT        IFNE      'I'
     C                   LEAVE
     C                   ENDIF                                                  STAT = 'I'
     C                   READP     PBD01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C*
     C*                Determine % off fee schedule
     C*                ------------------------------------------------
     C*** BEGIN CHG *** 06/03/99 **************************************
     C********             Z-ADDCAMRAA    XXALOW
     C     CATOTP        ADD       CADIFF        XXALOW
     C*** END CHG *** 06/03/99 ****************************************
     C*** BEGIN CHG *** 02/19/99 **************************************
     C********   CAMRAA    MULT XXPC01    XXNDIS    H
     C                   Z-ADD     0             XXNDIS
     C     CAICD9        IFEQ      'NO-FEE'
     C                   Z-ADD     BDPC03        XXPC00
     C                   ELSE                                                   ICD9 = 'NO-FEE'
     C     CAIPOP        IFEQ      '1'
     C                   Z-ADD     BDPC01        XXPC00
     C                   ELSE                                                   IPOP = '1'
     C                   Z-ADD     BDPC02        XXPC00
     C                   ENDIF                                                  IPOP = '1'
     C                   ENDIF                                                  ICD9 = 'NO-FEE'
     C*** END CHG *** 02/19/99 ****************************************
     C*** BEGIN CHG *** 06/03/99 **************************************
     C********   CAMRAA    MULT XXPC00    XXNDIS    H
     C     XXALOW        MULT(H)   XXPC00        XXNDIS
     C********   CAMRAA    SUB  XXNDIS    XXTOTA
     C     XXALOW        SUB       XXNDIS        XXTOTA
     C*** END CHG *** 06/03/99 ****************************************
     C*
     C*                Update Paid, Allowed, Discount, and EOMB code
     C*                ------------------------------------------------
     C                   SETOFF                                       80
     C     XXTOTA        IFLT      CATOTC
     C     XXTOTA        ANDNE     0
     C     CATOTP        ANDNE     0
     C     CATOTP        SUB       XXTOTA        CANDIS
     C     CATOTP        SUB       CANDIS        CATOTP
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   MOVE      '398'         CAEOMB
     C                   SETON                                        80
     C                   ENDIF                                                  TOTA < TOTC
     C*
     C*                If state = 'OK' then
     C*                   CADIFF = MRA$ - (MRA$ * NTWK%) - Charges
     C*                ------------------------------------------------
     C     XXAST         IFEQ      'OK'
     C     XXALOW        MULT(H)   XXPC00        XXDIFF
     C     XXALOW        SUB       XXDIFF        XXDIFF
     C     XXDIFF        SUB       CATOTC        CADIFF
     C*** BEGIN CHG *** 02/19/99 **************************************
     C*** BEGIN CHG *** 06/03/99 **************************************
     C********   CAMRAA    SUB  XXNDIS    CAMNAA    H
     C     XXALOW        SUB(H)    XXNDIS        CAMNAA
     C*** END CHG *** 06/03/99 ****************************************
     C     CADIFF        IFLE      0
     C                   Z-ADD     0             CADIFF
     C                   ENDIF                                                  DIFF <= 0
     C                   SETON                                        80
     C*** END CHG *** 02/19/99 ****************************************
     C                   ENDIF                                                  AST = 'OK'
     C*
     C*                If discount applies: update Case Detail
     C*                ------------------------------------------------
     C   80              UPDATE    PCA01
     C*
     C                   ENDIF                                                   *IN60 = Off
     C                   ENDIF                                                   *IN61 = Off
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
