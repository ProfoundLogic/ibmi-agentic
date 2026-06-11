     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8107R - PURGE POLICY ADMIN FILES
     H****************************************************************
     H*
     H*  PROGRAM DESCRIPTION:
     H*       THIS PROGRAM WILL PURGE ADDITIONAL RECORDS FOR THE POLICY
     H*       ADMIN RELATED FILES FOR THE SELECTED FUND
     H*
     H*  CALLED FROM:
     H*    W8107R - POLICY ADMIN PURGE PROGRAM
     H*
     H*  CALLS TO:
     H*
     H*  CREATED:    04/15/04  C:58      PROJECT: WC1178
     H****************************************************************
     H* REVISIONS:
     H*
     H*  04/07/09 C:220 SUPPORT: 66684                               *
     H*                MAKE CPYF CMD COMP WITH FILE WMEAP            *
     H*                - JUST COMPILED                               *
     H*                                                              *
     H*  04/12/05 C:61 PROJECT WC1283 135WBW
     H*                Add processing for WMEEP.
     H*
     H*  11/12/04 C:62 PROJECT WA099 44375
     H*                Add 2 more file for processing.
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
     FQFEUP     UF   E             DISK
     FQFE2P     UF   E             DISK
     FWAC6P     UF   E             DISK
     FWAR4L1    UF   E           K DISK
   5 FWDABL2    UF   E           K DISK
     F*
     FWDILL     UF   E           K DISK
     FWDLCL     UF   E           K DISK
     FWDLDL     UF   E           K DISK
     FWDLSL     UF   E           K DISK
  10 FWDL2L     UF   E           K DISK
     F*
     FWDPFL     UF   E           K DISK
     FWDRVL     UF   E           K DISK
     FWDVHL     UF   E           K DISK
     FWDXSL     UF   E           K DISK
  15 FWHESL     UF   E           K DISK
     F*
     FWHXAP     UF   E           K DISK
     FWMANL     UF   E           K DISK
     FWMASL     UF   E           K DISK
     FWMAVL     UF   E           K DISK
  20 FWMA2L     UF   E           K DISK
     F*
     FWMCAP     UF   E             DISK
     FWMDBL     UF   E           K DISK
     FWMDHL     UF   E           K DISK
     FWMECL     UF   E           K DISK
  25 FWMEWL     UF   E           K DISK
     F*
     FWMIFL     UF   E           K DISK
     FWMRIL     UF   E           K DISK
     FWMRQL     UF   E           K DISK
     FWMWVL     UF   E           K DISK
  30 FWMXAL     UF   E           K DISK
     F                                     RENAME(PXA01:PXA02)
     F*
  31 FWXNAL11   UF   E           K DISK
     F*** BEGIN ADD *** 11/12/04 **************************************
  32 FWDRTL     UF   E           K DISK
  33 FWMNBL     UF   E           K DISK
     F*** END   ADD *** 11/12/04 **************************************
     F*** BEGIN ADD *** 04/12/05 **************************************
  33 FWMEEL     UF   E           K DISK
     F*** END   ADD *** 04/12/05 **************************************

     D*
     D*     Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     C                   PARM                    XXFILE            8
     C                   PARM                    XX#REC            6 0
      *
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
 01  C*    QFEUP - Calendar earned/unearned
     C     XXFILE        WHENEQ    'QFEUP   '
     C                   READ      PEU01                                  60
     C     *IN60         DOWEQ     '0'
     C     EUCO#         IFEQ      XXCO#
     C     EUFND         ANDEQ     XXFND
     C                   DELETE    PEU01
     C                   ADD       1             XX#REC
     C                   ENDIF
     C                   READ      PEU01                                  60                R
     C                   ENDDO
     C*
 02  C*    QFE2P - Member Experience
     C     XXFILE        WHENEQ    'QFE2P   '
     C                   READ      PE201                                  60
     C     *IN60         DOWEQ     '0'
     C     E2CO#         IFEQ      XXCO#
     C     E2FND         ANDEQ     XXFND
     C                   DELETE    PE201
     C                   ADD       1             XX#REC
     C                   ENDIF
     C                   READ      PEU01                                  60                R
     C                   ENDDO
     C*
 03  C*    WAC6P - Return Premium Check Schedule
     C     XXFILE        WHENEQ    'WAC6P   '
     C                   READ      PC601                                  60
     C     *IN60         DOWEQ     '0'
     C     C6CO#         IFEQ      XXCO#
     C     C6FND         ANDEQ     XXFND
     C                   DELETE    PC601
     C                   ADD       1             XX#REC
     C                   ENDIF
     C                   READ      PC601                                  60                R
     C                   ENDDO
     C*
 04  C*    WAR4L1 - Return Premium Check Request File
     C     XXFILE        WHENEQ    'WAR4P   '
     C     XXKEY         CHAIN     PR401                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PR401
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PR401                                  60                R
     C                   ENDDO
     C* -------------------------------------------------------------
     C* -------------------------------------------------------------
     C*
 05  C*    WDABL2 - Agency Billing Detail File
     C     XXFILE        WHENEQ    'WDABP   '
     C     XXKEY         CHAIN     PAB01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAB01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PAB01                                  60                R
     C                   ENDDO
     C*
 06  C*    WDILL - Employer Increased Limits File
     C     XXFILE        WHENEQ    'WDILP   '
     C     XXKEY         CHAIN     PIL01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PIL01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PIL01                                  60                R
     C                   ENDDO
     C*
 07  C*    WDLCL - Labor Contract Exclusion File
     C     XXFILE        WHENEQ    'WDLCP   '
     C     XXKEY         CHAIN     PLC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLC01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PLC01                                  60                R
     C                   ENDDO
     C*
 08  C*    WDLDL - Additional Etity / Location Detail
     C     XXFILE        WHENEQ    'WDLDP   '
     C     XXKEY         CHAIN     PLD01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLD01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PLD01                                  60                R
     C                   ENDDO
     C*
 09  C*    WDLSL - Reinsurance Loss Report Detail
     C     XXFILE        WHENEQ    'WDLSP   '
     C     XXKEY         CHAIN     PLS01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLS01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PLS01                                  60                R
     C                   ENDDO
     C* -------------------------------------------------------------
     C* -------------------------------------------------------------
     C*
 10  C*    WDL2L - Labor Contractors End File
     C     XXFILE        WHENEQ    'WDL2P   '
     C     XXKEY         CHAIN     PL201                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PL201
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PL201                                  60                R
     C                   ENDDO
     C*
 11  C*    WDPFL - Reinsurance Partial Filing
     C     XXFILE        WHENEQ    'WDPFP   '
     C     XXKEY         CHAIN     PPF01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPF01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PPF01                                  60                R
     C                   ENDDO
     C*
 12  C*    WDRVL - Reinsurance recovery File
     C     XXFILE        WHENEQ    'WDRVP   '
     C     XXKEY         CHAIN     PRV01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRV01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PRV01                                  60                R
     C                   ENDDO
     C*
 13  C*    WDVHL - Vendor History
     C     XXFILE        WHENEQ    'WDVHP   '
     C     XXKEY         CHAIN     PVH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PVH01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PVH01                                  60                R
     C                   ENDDO
     C*
 14  C*    WDXSL - Excluded States Detail
     C     XXFILE        WHENEQ    'WDXSP   '
     C     XXKEY         CHAIN     PXS01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PXS01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PXS01                                  60                R
     C                   ENDDO
     C* -------------------------------------------------------------
     C* -------------------------------------------------------------
     C*
 15  C*    WHESL - Employer Status History
     C     XXFILE        WHENEQ    'WHESP   '
     C     XXKEY         CHAIN     PES01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PES01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PES01                                  60                R
     C                   ENDDO
     C*
 16  C*    WHXAP - Data Warehouse - Extract Control Agent Commission
     C     XXFILE        WHENEQ    'WHXAP   '
     C     XXKEY         CHAIN     PXA01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PXA01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PXA01                                  60                R
     C                   ENDDO
     C*
 17  C*    WMANL - Additional Named Insureds File
     C     XXFILE        WHENEQ    'WMANP   '
     C     XXKEY         CHAIN     PAN01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAN01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PAN01                                  60                R
     C                   ENDDO
     C*
 18  C*    WMASL - Data Warehouse - Extract Control Agent Commission
     C     XXFILE        WHENEQ    'WMASP   '
     C     XXKEY         CHAIN     PAS01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAS01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PAS01                                  60                R
     C                   ENDDO
     C*
 19  C*    WMAVL - Data Warehouse - Extract Control Agent Commission
     C     XXFILE        WHENEQ    'WMAVP   '
     C     XXKEY         CHAIN     PAV01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAV01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PAV01                                  60                R
     C                   ENDDO
     C* -------------------------------------------------------------
     C* -------------------------------------------------------------
     C*
 20  C*    WMA2L - Data Warehouse - Extract Control Agent Commission
     C     XXFILE        WHENEQ    'WMA2P   '
     C     XXKEY         CHAIN     PA201                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PA201
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PA201                                  60                R
     C                   ENDDO
     C*
 21  C*    WMCAP - Carrier Commission File
     C     XXFILE        WHENEQ    'WMCAP   '
     C                   READ      PCA01                                  60
     C     *IN60         DOWEQ     '0'
     C     CACO#         IFEQ      XXCO#
     C     CAFND         ANDEQ     XXFND
     C                   DELETE    PCA01
     C                   ADD       1             XX#REC
     C                   ENDIF
     C                   READ      PCA01                                  60                R
     C                   ENDDO
     C*
 22  C*    WMDBL - Direct Debit Employer file
     C     XXFILE        WHENEQ    'WMDBP   '
     C     XXKEY         CHAIN     PDB01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDB01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PDB01                                  60                R
     C                   ENDDO
     C*
 23  C*    WMDHL - Direct Debit History File
     C     XXFILE        WHENEQ    'WMDHP   '
     C     XXKEY         CHAIN     PDH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PDH01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PDH01                                  60                R
     C                   ENDDO
     C*
 24  C*    WMECL - Employer Contacts File
     C     XXFILE        WHENEQ    'WMECP   '
     C     XXKEY         CHAIN     PEC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEC01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PEC01                                  60                R
     C                   ENDDO
     C*
     C* -------------------------------------------------------------
     C* -------------------------------------------------------------
     C*
 25  C*    WMEWL - Waiver Information File
     C     XXFILE        WHENEQ    'WMEWP   '
     C     XXKEY         CHAIN     PEW01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEW01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PEW01                                  60                R
     C                   ENDDO
     C*
 26  C*    WMIFL - Installment Fee Information
     C     XXFILE        WHENEQ    'WMIFP   '
     C     XXKEY         CHAIN     PIF01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PIF01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PIF01                                  60                R
     C                   ENDDO
     C*
 27  C*    WMRIL - Reinsurance Policy Master
     C     XXFILE        WHENEQ    'WMRIP   '
     C     XXKEY         CHAIN     PRI01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRI01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PRI01                                  60                R
     C                   ENDDO
     C*
 28  C*    WMRQL - Reinsurance Recovery Request
     C     XXFILE        WHENEQ    'WMRQP   '
     C     XXKEY         CHAIN     PRQ01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRQ01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PRQ01                                  60                R
     C                   ENDDO
     C*
 29  C*    WMWVL - Waiver Information
     C     XXFILE        WHENEQ    'WMWVP   '
     C     XXKEY         CHAIN     PWV01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PWV01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PWV01                                  60                R
     C                   ENDDO
     C*
     C* -------------------------------------------------------------
     C* -------------------------------------------------------------
     C*
 30  C*    WMXAL - Cancelled Employers A/R
     C     XXFILE        WHENEQ    'WMXAP   '
     C     XXKEY         CHAIN     PXA02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PXA02
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PXA02                                  60                R
     C                   ENDDO
     C*
 31  C*    WXNALL1 - Waiver Information
     C     XXFILE        WHENEQ    'WXNAP1  '
     C     XXKEY         CHAIN     PNA01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PNA01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PNA01                                  60                R
     C                   ENDDO
     C*** BEGIN ADD *** 11/12/04 **************************************
     C*
 32  C*    WDRTL - WCMS Renewal Tracking
     C     XXFILE        WHENEQ    'WDRTP   '
     C     XXKEY         CHAIN     PRT01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRT01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PRT01                                  60                R
     C                   ENDDO
     C*
 33  C*    WMNBL - WCMS New Business
     C     XXFILE        WHENEQ    'WMNBP   '
     C     XXKEY         CHAIN     PNB01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PNB01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PNB01                                  60                R
     C                   ENDDO
     C*** END   ADD *** 11/12/04 **************************************
     F*** BEGIN ADD *** 04/12/05 **************************************
      *
 33   *    WMEEL - WCMS Employees
      *
     C     XXFILE        WHENEQ    'WMEEP   '
     C     XXKEY         CHAIN     PEE01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEE01
     C                   ADD       1             XX#REC
     C     XXKEY         READE     PEE01                                  60                R
     C                   ENDDO
      *
     F*** END   ADD *** 04/12/05 **************************************
     C* -------------------------------------------------------------
 34  C*    Last Call
     C     XXFILE        WHENEQ    '*       '
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSL
     C* =============================================================
     C*
     C*    Back to calling program.
     C                   RETURN
     C* =============================================================
