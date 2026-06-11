     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W8104R2 - PURGE FUND MASTER/TABLE FILES  2ND Program
     H****************************************************************
     H*
     H*  PROGRAM DESCRIPTION:
     H*       THIS PROGRAM WILL PURGE ADDITIONAL RECORDS FROM FUND
     H*       MASTER AND FUND TABLE FILES FOR THE SELECTED FUND
     H*
     H*  CALLED FROM:
     H*    W8104R - FUND MASTER PURGE PROGRAM
     H*
     H*  CALLS TO:
     H*
     H****************************************************************
     H*  CREATED:    04/15/04  C:58      PROJECT: WC1178
     H****************************************************************
     H*
     H* REVISIONS:
     H*
     H****************************************************************
     H*
     H* INDICATOR USAGE:
     H*
     H****************************************************************
     F*
     F****************************************************************
     F*     F I L E S
     F****************************************************************
     F*
     FWMAXL     UF   E           K DISK
     FWMCFL     UF   E           K DISK
     FWMDXL     UF   E           K DISK
     FWMEXL     UF   E           K DISK
   5 FWMGCL     UF   E           K DISK
     FWMMGL     UF   E           K DISK
   7 FWMPFL     UF   E           K DISK

     D*
     D*     Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     C                   PARM                    XXFILE            8
     C                   PARM                    XX#REC            6 0
     C*
     C*
     C*   Set-up and Initialize
     C                   MOVE      X@CO#         XXCO#             3 0
     C                   MOVE      X@FND         XXFND             3 0
     C*
     C*   KEY LIST FOR ALL FILES
     C     XXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C                   Z-ADD     *ZERO         XX#REC            6 0
     C*
     C* =============================================================
     C* READ/DELETE ALL RECORDS IN SELECTED FILE FOR DESIGNATED FUND
     C*
     C* =============================================================
     C                   SELECT
     C*
 01  C*    WMAXL - Audit Tracking Control
     C     XXFILE        WHENEQ    'WMAXL   '
     C     XXKEY         CHAIN     PAX01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAX01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PAX01                                  60                R
     C                   ENDDO
     C*
 02  C*    WMCFL - Cancellation Confirmation Control
     C     XXFILE        WHENEQ    'WMCFL   '
     C     XXKEY         CHAIN     PCF01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCF01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PCF01                                  60                R
     C                   ENDDO
     C*
 03  C*    WMDXL - Endorsement DARTS Control
     C     XXFILE        WHENEQ    'WMDXL   '
     C     XXKEY         CHAIN     PDX01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDX01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PDX01                                  60                R
     C                   ENDDO
     C*
 04  C*    WMEXL - Admin/DARTS Control/Defn
     C     XXFILE        WHENEQ    'WMEXL   '
     C     XXKEY         CHAIN     PEX01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEX01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PEX01                                  60                R
     C                   ENDDO
     C* -------------------------------------------------------------
     C* -------------------------------------------------------------
     C*
 05  C*    WMGCL - Group Carrier Information
     C     XXFILE        WHENEQ    'WMGCL   '
     C     XXKEY         CHAIN     PGC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PGC01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PGC01                                  60                R
     C                   ENDDO
     C*
 06  C*    WMMGL - Employer Increased Limits File
     C     XXFILE        WHENEQ    'WMMGL   '
     C     XXKEY         CHAIN     PMG01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PMG01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PMG01                                  60                R
     C                   ENDDO
     C*
 07  C*    WMPFL - Premium Finance File
     C     XXFILE        WHENEQ    'WMPFL   '
     C     XXKEY         CHAIN     PPF01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPF01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PPF01                                  60                R
     C                   ENDDO
     C*
     C* -------------------------------------------------------------
     C*    Last Call
     C     XXFILE        WHENEQ    '*       '
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSL
     C* =============================================================
     C*
     C*    Back to calling program.
     C                   RETURN
     C* =============================================================
