     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N95DFR - TYPE DF DISCOUNT CODE CALCULATIONS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program calculates the CorVel Network discount     **
     H**     amount as follows:                                      **
     H**     . If Outpatient the lessor of                           **
     H**          CATOTA * BDPC01 (Discount % off Fee Schedule)      **
     H**          CATOTC * BDPC02 (Billed Amt Discount %)            **
     H**     . If Inpatient the lessor of                            **
     H**          CATOTA * BDPC03 (Inpatient Discount %)             **
     H**          CATOTC * BDPC04 (Inpatient Billed Amt Discount %)  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     N9500R - Process Detail Charges to Correct Discount     **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED: 05/21/98         PROJECT: 51MCT                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*  03/30/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Correct HCPOS test.
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      Case Detail File not found                     **
     H**      61      Billing Rate/Discount Master not found         **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FDCALN (PCA01): Case Detail File (CO#/SUB#/FND/FYR/    **
     H**                      CASE/FEIN/MOD/FDAT/DSEQ/FLAG <> 'P')   **
     H**      FDHCL  (PHC01): Detail Charge Auxiliary                **
     H**                      (CO#/SUB/FND/FYR/CASE/DSEQ)            **
     H**      NMBDLA (PBD01): Billing Rate/Discount Master           **
     H**                      (CODE/EFDT/STAT <> 'I')                **
     H*****************************************************************
     H*
     FFDCALN    UF   E           K DISK
     FFDHCL     IF   E           K DISK
     FNMBDLA    IF   E           K DISK

     C*
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C*          PCA01 - Case Detail File
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
     C     *LIKE         DEFINE    CATOTA        XXTOTA
     C     *LIKE         DEFINE    CATOTC        XXTOTC
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C*
     C*          PBD01 - Billing Rate/Discount Master
     C     BDKY          KLIST
     C                   KFLD                    QQCODE            3
     C                   KFLD                    CAFDAT
     C*
     C*          PHC01 - Detail Charge Auxiliary
     C     HCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFUND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDSEQ
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
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Initialize FDCAP key
     C*          ------------------------------------------------------
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFUND            3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQFDAT        XXFDT8            8 0
     C                   Z-ADD     XXFDT8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C                   MOVE      QQDSEQ        XXDSEQ            5 0
     C                   MOVE      QQCEDT        XXEDAT            8 0
     C*
     C*          Get Case Detail and Detail Auxilary File records
     C*          ------------------------------------------------------
     C     CAKY          CHAIN     PCA01                              60
     C  N60HCKY          CHAIN     PHC01                              60
     C*
     C*             Get most current Discount Master record
     C*             ---------------------------------------------------
     C     *IN60         IFEQ      *OFF
     C     BDKY          SETGT     PBD01
     C                   READP     PBD01                                  61
     C     *IN61         IFEQ      *OFF
     C     BDCODE        ANDEQ     QQCODE
     C*
     C*                Most current Discount Master record found
     C*                If Inpatient the lessor of
     C*                  CATOTA * BDPC03 (Inpatient Discount %)
     C*                  CATOTC * BDPC04 (Inpatient Bill Amt Discnt %)
     C*                ------------------------------------------------
     C*** BEGIN CHG *** 03/30/99 **************************************
     C                   MOVE      HCPOS         XXPOS             2
     C********   HCPOS     IFEQ '21'
     C     XXPOS         IFEQ      '21'
     C********   HCPOS     OREQ '51'
     C     XXPOS         OREQ      '51'
     C********   HCPOS     OREQ '61'
     C     XXPOS         OREQ      '61'
     C*** END CHG *** 03/30/99 ****************************************
     C     CATOTA        MULT      BDPC03        XXNDIS
     C     CATOTA        SUB       XXNDIS        XXTOTA
     C     CATOTC        MULT      BDPC04        XXNDIS
     C     CATOTC        SUB       XXNDIS        XXTOTC
     C                   ELSE                                                   POS = '21'
     C*
     C*                If Outpatient the lessor of
     C*                  CATOTA * BDPC01 (Discount % off Fee Schedule)
     C*                  CATOTC * BDPC02 (Billed Amt Discount %)
     C*                ------------------------------------------------
     C     CATOTA        MULT      BDPC01        XXNDIS
     C     CATOTA        SUB       XXNDIS        XXTOTA
     C     CATOTC        MULT      BDPC02        XXNDIS
     C     CATOTC        SUB       XXNDIS        XXTOTC
     C                   ENDIF                                                  POS = '21'
     C*
     C*                Update FDCAL if there is a network discount
     C*                ------------------------------------------------
     C     XXTOTA        IFLT      XXTOTC
     C     CATOTA        SUB       XXTOTA        CANDIS
     C                   ELSE                                                   TOTA < TOTC
     C     CATOTP        SUB       XXTOTC        CANDIS
     C                   ENDIF                                                  TOTA < TOTC
     C     CANDIS        IFGT      0
     C     CATOTA        SUB       CANDIS        CATOTA
     C     CATOTP        SUB       CANDIS        CATOTP
     C                   MOVE      '398'         CAEOMB
     C                   UPDATE    PCA01
     C                   ENDIF                                                  NDIS > 0
     C*
     C                   ENDIF                                                  *IN61 = Off
     C                   ENDIF                                                  *IN60 = Off
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
