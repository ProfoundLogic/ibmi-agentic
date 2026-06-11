     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N95DER - TYPE DE DISCOUNT CODE CALCULATIONS
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
     H**   CREATED:   01/15/98      PROJECT: 60MBS                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
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
     H**      FMS4L1 (PS401): Provider Network (FEIN/MOD/NWCD)       **
     H**      FTABL         : Logical over table files (CO#/KEY)     **
     H**             (PCS01): All States Claim Status Code Table     **
     H**      NDCOL1 (PCO01): Network Carve-out (CODE/CPT4/EFDT)     **
     H*****************************************************************
     H*
     FFDCALN    UF   E           K DISK
     FFMS4L1    IF   E           K DISK
     FFTABL     IF   E           K DISK
     FNDCOL1    IF   E           K DISK

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
     C                   KFLD                    XXFDAT
     C                   KFLD                    XXDSEQ
     C*
     C*          FMS4L1 (PS401): Provider Network
     C     S4KY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXNWCD
     C*
     C*          FTABL  (PCS01): All States Claim Status Code Table
     C     CSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C*          NDCOL1 (PCO01): Network Carve-out
     C     COKY          KLIST
     C                   KFLD                    QQCODE
     C                   KFLD                    CACPT4
     C                   KFLD                    CAFDAT
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
     C* ---------------------------------------------------------------
     C*        DEFINES
     C* ---------------------------------------------------------------
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C     *LIKE         DEFINE    CATOTA        XXTOTA
     C     *LIKE         DEFINE    CADIFF        XXDIFF
     C     *LIKE         DEFINE    CATOTA        XXALOW
     C     *LIKE         DEFINE    CAUNTS        X1UNTS
     C     *LIKE         DEFINE    S4NWCD        XXNWCD
     C     *LIKE         DEFINE    S4DISP        XXDISP
     C     *LIKE         DEFINE    COAAMT        XXAAMT
     C     *LIKE         DEFINE    S4BGDT        XXFDAT
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
     C*
     C                   MOVE      QQFDAT        X1FDAT            8 0
     C*
     C                   Z-ADD     X1FDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCODE        XXCODE            3
     C                   MOVEL(P)  'ANT'         XXNWCD
     C                   Z-ADD     0             XXDISP
     C                   Z-ADD     0             XXAAMT
     C*
     C*          Get case detail record
     C*          ------------------------------------------------------
     C     CAKY          CHAIN     PCA01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C*             Get claim status code - do not process if 'C' (CAP)
     C*             ---------------------------------------------------
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     CSKY          CHAIN     PCS01                              60
     C     *IN60         IFEQ      *OFF
     C     CSDISC        ANDNE     'C'
     C*
     C*                Get Carve Out record
     C*                ------------------------------------------------
     C     COKY          SETGT     PCO01
     C                   READP     PCO01                                  60
     C     *IN60         DOWEQ     *OFF
     C     COCODE        ANDEQ     'DE1'
     C     S4STAT        IFNE      'I'
     C                   Z-ADD     COAAMT        XXAAMT
     C                   LEAVE
     C                   ENDIF                                                  STAT = 'I'
     C                   READP     PCO01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C*
     C*                Get Provider Network Contract record
     C*                ------------------------------------------------
     C     S4KY          SETGT     PS401
     C                   READP     PS401                                  60
     C     *IN60         DOWEQ     *OFF
     C     S4NWCD        ANDEQ     'ANT'
     C     S4STAT        IFNE      'I'
     C                   Z-ADD     S4DISP        XXDISP
     C                   LEAVE
     C                   ENDIF                                                  STAT = 'I'
     C                   READP     PS401                                  60
     C                   ENDDO                                                  *IN60 = Off
     C*
     C*                Determine % off fee schedule
     C*                1/ Network Payment = Carve out Amt * discount %
     C*                ------------------------------------------------
     C     XXDISP        IFNE      0
     C     XXAAMT        ANDNE     0
     C                   MULT(H)   XXDISP        XXAAMT
     C*
     C*                2/ If Network Payment < State Fee schedule
     C*                   . Difference goes in CANDIS
     C*                   . Difference subtracted from CAPAMT
     C*                   . EOMB code set to 398
     C*                   . Update FDCAP
     C*                ------------------------------------------------
     C     XXAAMT        IFLE      CATOTP
     C     CATOTP        SUB       XXAAMT        CANDIS
     C     CATOTP        SUB       CANDIS        CATOTP
     C     CATOTA        SUB       CANDIS        CATOTA
     C                   MOVE      '398'         CAEOMB
     C                   UPDATE    PCA01
     C                   ENDIF                                                  AAMT < TOTP
     C                   ENDIF                                                  DISP <> 0
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDIF                                                  *IN61 = Off
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
