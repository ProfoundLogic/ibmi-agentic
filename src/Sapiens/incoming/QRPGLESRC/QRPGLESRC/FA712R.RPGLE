     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FA712R - RETRIEVE RESTRICTED FLAG
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program retrieves the restricted flag setting for  **
     H**     the requested claim.                                    **
     H**   CALLED FROM:                                              **
     H**     VARIOUS                                                 **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   08/13/98      PROJECT: 48MBR                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      General usage                                  **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FAPPL5 (PPP01): Claim Phys/Prov/Spec Trans File        **
     H**                      (CO#/SUB#/FND/FYR/CASE/FEIN/MOD)       **
     H*****************************************************************
     H*
     FFAPPL5    IF   E           K DISK
     F*

     D*
     D* ---------------------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* ---------------------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C* ---------------------------------------------------------------
     C*        ENTRY LIST
     C* ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRSTR            1
     C*
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C*          FAPPL5 (PPP01): Claim Phys/Prov/Spec Trans File
     C     PPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C     *LIKE         DEFINE    PPCO#         XXCO#
     C     *LIKE         DEFINE    PPSUB#        XXSUB#
     C     *LIKE         DEFINE    PPFND         XXFND
     C     *LIKE         DEFINE    PPFYR         XXFYR
     C     *LIKE         DEFINE    PPCASE        XXCASE
     C     *LIKE         DEFINE    PPFEIN        XXFEIN
     C     *LIKE         DEFINE    PPMOD         XXMOD
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Initialize key list
     C*          ------------------------------------------------------
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C*
     C*          Get restricted flag
     C*          ------------------------------------------------------
     C     PPKY          CHAIN     PPP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PPRSTR        QQRSTR
     C                   ELSE                                                   *IN60 = Off
     C                   MOVE      'N'           QQRSTR
     C                   ENDIF                                                  *IN60 = Off
     C*
     C*          End of program
     C*          ------------------------------------------------------
     C                   SETON                                        LR
