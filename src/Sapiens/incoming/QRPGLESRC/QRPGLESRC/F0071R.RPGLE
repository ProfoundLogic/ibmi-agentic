     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0071R - CROSSFOOT BILL
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program crossfoots the detail line items for the   **
     H**     a bill after exit from Bill Entry.                      **
     H**   CALLED FROM:                                              **
     H**     F0070R - FS Entry                                       **
     H**   CALLS TO:                                                 **
     H**     F0086R - Request Payment/EOMB Print                     **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   03/19/99      SUPPORT: 17129                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H**
     H** 04/08/99 C:56 SUPPORT: MC095 #17568
     H**         1.  Add entry PARMS QQCO#/QQSUB#/QQFND
     H**         2.  Replace UPCO#/UPFND/UPFYR with QQCO#/QQSUB#/QQFND
     H**             in all KLISTs.
     H**         3.  Add QQCO#/QQSUB#/QQFND to call of F0086R
     H**
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      50      General usage                                  **
     H**      51      General usage                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FDCAL    (PCA01): Detail Charge File                   **
     H**                        (CO#/SUB#/FND/FYR/CASE/FEIN/MOD/DCS#)**
     H**      SMUPL    (PUP01): User Parameters File (USR)           **
     H*****************************************************************
     H*
     FF0071F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*** BEG DEL *** 04/08/99 ****************************************
     F*****SMUPL   IF  E           K        DISK
     F*** END DEL *** 04/08/99 ****************************************
     FFDCALD    IF   E           K DISK

     D*
     D* ---------------------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* ---------------------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D*
     D* ---------------------------------------------------------------
     D* SYSTEM DATE AND TIME
     D* ---------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C* ---------------------------------------------------------------
     C*        PARM LISTS
     C* ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQPMCD            2
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQSFDT            8 0
     C                   PARM                    QQSTDT            8 0
     C                   PARM                    QQTOTP            9 2
     C                   PARM                    QQTOTC            9 2
     C                   PARM                    QQMRAR            9 2
     C                   PARM                    QQOTHR            9 2
     C                   PARM                    QQNDIS            9 2
     C                   PARM                    QQBR2             2
     C                   PARM                    QQSYS             1
     C                   PARM                    QQDCS#            5
     C                   PARM                    QQPRTY            1
     C                   PARM                    QQAST             3
     C                   PARM                    QQAUDF            3
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRTCD            1
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C     *LIKE         DEFINE    CAFYR         QQFYR
     C     *LIKE         DEFINE    CACASE        QQCASE
     C     *LIKE         DEFINE    CAFEIN        QQFEIN
     C     *LIKE         DEFINE    CAMOD         QQMOD
     C     *LIKE         DEFINE    CADCS#        XXDCS#
     C*
     C*  CASE DETAIL FILE
     C*
     C*
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C     CAKY          KLIST
     C*** BEG CHG *** 04/08/99 ****************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           UPFND
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C*** END CHG *** 04/08/99 ****************************************
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQFEIN
     C                   KFLD                    QQMOD
     C                   KFLD                    XXDCS#
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          GET TIME AND USER INFORMATION
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C*** BEG DEL *** 04/08/99 ****************************************
     C********** Q1USER    CHAINPUP01                60
     C********** *IN60     IFEQ *ON
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPSUB#
     C**********           MOVE *ZERO     UPFND
     C**********           END                             *IN60 = *ON
     C*** END DEL *** 04/08/99 ****************************************
     C*
     C                   MOVE      QQDCS#        XXDCS#
     C*
     C*          Read all records for this bill from FDCAP
     C*          ------------------------------------------------------
     C     CAKY          CHAIN     PCA01                              50
     C     *IN50         DOWEQ     *OFF
     C     CATOTC        SUB       CATOTD        XXTOTP            9 2
     C                   SUB       CAOTHR        XXTOTP
     C                   SUB       CANDIS        XXTOTP
     C     CATOTP        IFNE      XXTOTP
     C                   ADD       1             SCCNT
     C     CATOTP        SUB       XXTOTP        XXWRK             9 2
     C                   ADD       XXWRK         SCAMT
     C                   ENDIF                                                  TOTP <> TOTP
     C*
     C*             Read next record
     C*             ---------------------------------------------------
     C     CAKY          READE     PCA01                                  50
     C                   ENDDO                                                  *IN50 = Off
     C*
     C*          Check if imbalance
     C*          ------------------------------------------------------
     C     SCCNT         IFEQ      0
     C                   CALL      'F0086R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQPMCD
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQSFDT
     C                   PARM                    QQSTDT
     C                   PARM                    QQTOTP
     C                   PARM                    QQTOTC
     C                   PARM                    QQMRAR
     C                   PARM                    QQOTHR
     C                   PARM                    QQNDIS
     C                   PARM                    QQBR2
     C                   PARM                    QQSYS
     C                   PARM                    QQDCS#
     C                   PARM                    QQPRTY
     C                   PARM                    QQAST
     C                   PARM                    QQAUDF
     C                   PARM                    QQEOB#
     C                   PARM                    QQRTCD
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C*** END ADD *** 04/08/99 ****************************************
     C                   ELSE                                                   SCCNT = 0
     C     SCCNT         COMP      1                                  81
     C                   EXFMT     WDW01
     C                   MOVE      '*'           QQRTCD
     C                   ENDIF                                                  SCCNT = 0
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
