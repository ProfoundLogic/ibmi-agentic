     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0031R - INTERACTIVE PREMIUM VARIFICATION EDIT
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program is the Interactive Premium Varification   **
     H**     Edit                                                   **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     ______ - ___________________________                   **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H**                                                            **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      __       ___________________________                  **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   ______ (_____): ___________________ (___/___)            **
     H****************************************************************
     F*
     F* Company File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter File
     FCMUPL     IF   E           K DISK
     F*
     F* Policy Master File
     FCMPML     IF   E           K DISK
     F*
     F* Quote Log Master File
     FCMQLL     IF   E           K DISK
     F*
     F* Boiler Machinery File
     FCMBML     IF   E           K DISK
     F*
     F* Auto File
     FCMA1L     IF   E           K DISK
     F*
     F* Burglary and Theft File
     FCMBTL     IF   E           K DISK
     F*
     F* Fidelity and Forgery File
     FCMFIL     IF   E           K DISK
     F*
     F* General Liability File
     FCMGLL     IF   E           K DISK
     F*
     F* Inland Marine File
     FCMIML     IF   E           K DISK
     F*
     F* Fire File
     FCMFAL     IF   E           K DISK
     F*
     F* Farm and Ranch File
     FCMFRL     IF   E           K DISK
     F*
     F* Glass File
     FCMGAL     IF   E           K DISK
     F*
     F* Business Owners File
     FCMBOL     IF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE POLICY SECTIONS
     C*
     C     PSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXPOL#
     C*
     C*   KEY LIST FOR THE QUOTE LOG MASTER FILE
     C*
     C     QLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PMQUT#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQPOL#           13
     C                   PARM                    QQPRMQ            8
     C                   PARM                    QQPRMP            8
     C                   PARM                    QQERR             1
     C*
     C     *LIKE         DEFINE    PMPOL#        XXPOL#
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQPOL#        XXPOL#
     C*
     C*    CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   CHAIN TO THE POLICY MASTER FILE
     C*
     C     PSKEY         CHAIN     PPM01                              60
     C*
     C*   CHAIN OUT TO QUOTE LOG MASTER FILE
     C*
     C     QLKEY         CHAIN     PQL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   ZERO OUT TOTALS BUCKET
     C*
     C                   Z-ADD     *ZERO         XXTOTL            9 0
     C*
     C*   CHAIN OUT TO BOILER MACHINERY FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PBM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     BMSTAT        IFNE      'I'
     C*
     C                   ADD       BMPREM        XXTOTL
     C*
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PBM01                                  60
     C                   END
     C*
     C*   CHAIN OUT TO AUTO FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PA101                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     A1STAT        IFNE      'I'
     C                   ADD       A1SLBA        XXTOTL
     C                   ADD       A1MEDA        XXTOTL
     C                   ADD       A1UNMA        XXTOTL
     C                   ADD       A1PRDA        XXTOTL
     C                   ADD       A1PIPA        XXTOTL
     C                   ADD       A1ADLA        XXTOTL
     C                   ADD       A1COLA        XXTOTL
     C                   ADD       A1NCLA        XXTOTL
     C                   ADD       A1TOWA        XXTOTL
     C*
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PA101                                  60
     C                   END
     C*
     C*   CHAIN OUT TO BURGLARY AND THEFT FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PBT01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     BTSTAT        IFNE      'I'
     C                   ADD       BTPREM        XXTOTL
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PBT01                                  60
     C                   END
     C*
     C*   CHAIN OUT TO FIDELITY AND FORGERY FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PFI01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     FISTAT        IFNE      'I'
     C                   ADD       FIPREM        XXTOTL
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PFI01                                  60
     C                   END
     C*
     C*   CHAIN OUT TO GENERAL LIABILITY FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PGL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     GLSTAT        IFNE      'I'
     C                   ADD       GLPREM        XXTOTL
     C                   ADD       GLPDPM        XXTOTL
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PGL01                                  60
     C                   END
     C*
     C*   CHAIN OUT TO INLAND MARINE FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PIM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     IMSTAT        IFNE      'I'
     C                   ADD       IMPREM        XXTOTL
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PIM01                                  60
     C                   END
     C*
     C*   CHAIN OUT TO COMMERCIAL FIRE FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PFA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     FASTAT        IFNE      'I'
     C                   ADD       FAPRM1        XXTOTL
     C                   ADD       FAPRM2        XXTOTL
     C                   ADD       FAPRM3        XXTOTL
     C                   ADD       FAPRM4        XXTOTL
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PFA01                                  60
     C                   END
     C*
     C*   CHAIN OUT TO FARM & RANCH FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PFR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     FRSTAT        IFNE      'I'
     C                   ADD       FRPREM        XXTOTL
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PFR01                                  60
     C                   END
     C*
     C*   CHAIN OUT TO GLASS FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PGA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     GASTAT        IFNE      'I'
     C                   ADD       GAPREM        XXTOTL
     C                   ADD       GAVBPR        XXTOTL
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PGA01                                  60
     C                   END
     C*
     C*   CHAIN OUT TO BUSINESSOWNERS FILE FOR PREMIUM
     C*
     C     PSKEY         CHAIN     PBO01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check for inactive ('I') status - skip inactive status records
     C*
     C     BOSTAT        IFNE      'I'
     C                   ADD       BOPREM        XXTOTL
     C                   END
     C*
     C*   GET NEXT RECORD
     C*
     C     PSKEY         READE     PBO01                                  60
     C                   END
     C*
     C*   CHECK TOTAL BUCKET AGAINST QUOTED PREMIUM AND SET RETURN FIELDS
     C*
     C     QLPRMQ        IFNE      XXTOTL
     C                   MOVE      '1'           QQERR
     C                   MOVE      QLPRMQ        QQPRMQ
     C                   MOVE      XXTOTL        QQPRMP
     C                   ELSE
     C                   MOVE      '0'           QQERR
     C                   MOVE      *ZERO         QQPRMQ
     C                   MOVE      *ZERO         QQPRMP
     C                   END
     C*
     C                   END                                                    Pol Mstr End
     C*
     C* End The Program
     C*
     C                   MOVE      '1'           *INLR
     C*
