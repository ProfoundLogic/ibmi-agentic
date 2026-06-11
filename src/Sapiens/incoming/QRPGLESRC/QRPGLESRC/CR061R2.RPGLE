     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM SUBROUTINE CR061R2- GET CLAIMANT NAMES FOR SIS SYSTEM
     F*** DATA IN SISFIL ***
     H*****************************************************************
     H*****************************************************************
     H*  PROGRAMMER:  MG
     H*  DATE:        10/28/2009
     H*  CLIENT:      002
     H*  PROJECT:
     H*****************************************************************
     H*  REVISIONS:                                                   *
     F**************************************************************************
     H*****************************************************************
     H*
     FCMNAME01  IF   E           K DISK
     F                                     RENAME(CMNAMER:RNAME)

     D*
     D* DATA STRUCTURE FOR KEY FOR
     D                 DS
     D  XZKEY                  1     79
     D  XZCKEY                 1     27
     D  XZCO#                  1      2
     D  XZPRE                  3      6
     D  XZPOL                  7     15
     D  XZBRCH                16     17
     D  XZCLM                 18     27
     D*
     D                 DS
     D  X3KEY                  1     79
     D  X3CKEY                 1     27
     D  X3CO#                  1      2
     D  X3PRE                  3      6
     D  X3POL                  7     15
     D  X3BRCH                16     17
     D  X3CLM                 18     27
     D*
     D* DATA STRUCTURE FOR KEY FOR FEE SCHEDULE
      *
     D DATEDS          DS
     D  CYMD                   1      8  0
     D  CC1                    1      2  0
     D  CY1                    1      4
     D  YY1                    3      4  0
     D  YMD                    3      8  0
     D  MMDD                   5      8  0
     D  MM                     5      6
     D  DD                     7      8
     D  CC2                    9     10  0
     D  CY2                    9     12
     D  YY2                   11     12  0
     D  MDCY                   5     12  0
     D*
    *D**** TEMP HERE ***************************
     D STUFF           C                   CONST('TESTING THE ABILTY O-
     D                                     F IDARTS TO DEAL WIT-
     D                                     H TEXT ENTRIES 2 AND-
     D                                     THREE. TEST TEST TES')
     D**
     D CINS            C                   CONST('Insured: ')
     D CCLMNT          C                   CONST('Claimant:')
     D*
     D*
    *D**** END TEMP HERE ***************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*
     IRNAME
     I              CNNMCTL                     ##CTL
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************
      *

     C*
     C     NMK           KLIST
     C                   KFLD                    QQBRCH
     C                   KFLD                    QQCLM
     **
     C*
     C*
     C* ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQBRCH            2            BRANCH
     C                   PARM                    QQCLM            10            CLAIM
     C                   PARM                    QQMULT            1            MORE THN 1 CLAIM?
     C                   PARM                    QQMORE            1            MORE THN 4 CLAIM?
     C                   PARM                    Q1DTA1           79            DTA 1
     C                   PARM                    Q1DTA2           79            DTA 2
     C*
     C*
     C*
     C*
     C*
     C                   MOVE      *BLANKS       NAME1            30
     C                   MOVE      *BLANKS       NAME2            30
     C                   MOVE      *BLANKS       NAME3            30
     C                   MOVE      *BLANKS       NAME4            30
     ** *
     C     NMK           CHAIN     RNAME                              60
     C     *IN60         IFEQ      *OFF
     C*** NAMES ARE 30 CHARACTERS SO FOR EACH HIT SAVE IT.
     **
     **
     **
     C                   MOVEL     CNSRCH        NAME1
     C**
     C*NAME 2
     C     NMK           READE     RNAME                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '1'           QQMULT
     C                   MOVEL     CNSRCH        NAME2
     ****
     ** NAME 3
     C     NMK           READE     RNAME                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNSRCH        NAME3
     ****
     ** NAME 4
     C     NMK           READE     RNAME                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CNSRCH        NAME4
     C*
     C     NMK           READE     RNAME                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     '1'           QQMORE
     C                   ELSE
     C                   MOVE      *BLANKS       QQMORE
     C                   ENDIF
     ******
     C                   ENDIF                                                      HIT ON CLAIMANT
     C                   ENDIF                                                      HIT ON CLAIMANT
     C                   ENDIF                                                      HIT ON CLAIMANT
     C                   ENDIF                                                      HIT ON CLAIMANT
     ** *
     **** BUILD THE 2 RETURN PARMS BASED ON THE
     ** *
     C     NAME1         IFEQ      *BLANKS
     C                   GOTO      ENDIT
     C                   ENDIF
     **
     **
     C     NAME2         IFNE      *BLANKS
     C     NAME1         CAT(P)    '       '     XX37             37
     C     XX37          CAT(P)    NAME2         Q1DTA1
     C                   ELSE
     C                   MOVEL     *BLANKS       Q1DTA1
     C                   MOVEL     NAME1         Q1DTA1
     C                   GOTO      ENDIT
     C                   ENDIF
     ** *
    ***8
     **
     C     NAME3         IFEQ      *BLANKS
     C                   GOTO      ENDIT
     C                   ENDIF
     **
     C     NAME4         IFNE      *BLANKS
     C     NAME3         CAT(P)    '       '     XX37             37
     C     XX37          CAT(P)    NAME4         Q1DTA2
     C     QQMORE        IFNE      *BLANKS
     C                   CAT       '+':2         Q1DTA2
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       Q1DTA2
     C                   MOVEL     NAME3         Q1DTA2
     C                   GOTO      ENDIT
     C                   ENDIF
     ** *
     C     ENDIT         TAG
     C                   MOVE      *ON           *INLR
