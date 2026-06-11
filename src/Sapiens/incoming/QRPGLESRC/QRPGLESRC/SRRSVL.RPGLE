     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRRSVL - CALCULATE REMAINING RESERVE LIMIT FOR USER
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*       THIS PROGRAM CALCULATES THE REMAINING RESERVE LIMIT     *
     H*       FOR A USER PROFILE BASED ON THE LIMIT SPECIFIED IN      *
     H*       THE USER MASTER AND RESERVES ALREADY ALLOCATED.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0070R - CLAIM MAINTENANCE                                 *
     H*    W0080R - REQUEST PAYMENT                                   *
     H*    W0120R - MANUAL PAYMENTS/PAYMENT CORRECTIONS               *
     H*    W0583R2- CHANGE PAYMENT REQUEST                            *
     H*    W0581R - PAYMENT INQUIRY                                   *
     H*    W0581R1- PENDED PAYMENT SEARCH                             *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:46                                             *
     H*  CREATED:    01/09/96    PROJECT: WC411                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/03/15  C:180 SUPPORT: 31933                                *
     H*                 Calculate rsv limit based on rsv type         *
     H*                 for non-cumulative set up.                    *
     H*
     H* 05/13/04  C:53  PROJECT: WC1186                               *
     H*                 ADD TOTAL INCURRED PROCESSING FOR RESERVE     *
     H*                 AMOUNTS                                       *
     H*                                                               *
     H* 04/01/03  C:53  PROJECT: WC1122                               *
     H*                 PROCESS THE RESERVE AUTHORITY LIMIT EDIT BASED*
     H*                 ON EITHER A 'PER RESERVE TYPE' AMOUNT OR A    *
     H*                 'CUMULATIVE ACROSS ALL RESERVES' AMOUNT       *
     H*                                                               *
     H* 08/06/99  C:33  SEQ#: WC048/18813                             *
     H*                 EXPAND THE SIZE OF XXAMT                      *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*    50 - READE INDICATOR FOR PRH01 - CONTROLS LOOP             *
     H*    60 - UTILITY INDICATOR                                     *
     H*    61 - LOKUP INDICATOR                                       *
     H*                                                               *
     H*****************************************************************
     FWMUPL     IF   E           K DISK
     FWMRHL1    IF   E           K DISK
     F*** BEG ADD *** 05/13/04 **************************************
     FWMCML1    IF   E           K DISK
     F*** END ADD *** 05/13/04 **************************************
     F*** BEG ADD *** 04/01/03 ****************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F*** END ADD *** 04/01/03 ****************************

     D*** BEG ADD *** 05/13/04 **************************************
     D*** END ADD *** 05/13/04 **************************************
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              RESERVE TYPE
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*** BEG ADD *** 05/13/04 **************************************
     D                 DS                  INZ
     D  CR                     1     90  2
     D                                     DIM(10)                              CLM RSV AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS                  INZ
     D  AR                     1     90  2
     D                                     DIM(10)                              CLM RSV PMYTS
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS                  INZ
     D  CT                     1     20
     D                                     DIM(10)                              CLM RSV TYPE
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D*** END ADD *** 05/13/04 **************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEG ADD *** 04/03/15 **************************************
     D i               S              3  0 inz
     D*** END ADD *** 04/03/15 **************************************
     C****************************************************************
     C*   PROGRAM FILE KEY LISTINGS
     C****************************************************************
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  RESERVE HISTORY MASTER FILE
     C*
     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** BEG ADD *** 04/01/03 ****************************
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*** END ADD *** 04/01/03 ****************************
     C****************************************************************
     C*   PROGRAM *LIKE DEFINES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    RHCO#         XXCO#
     C     *LIKE         DEFINE    RHFND         XXFND
     C     *LIKE         DEFINE    RHRT01        XXRVCD
     C     *LIKE         DEFINE    RHFYR         XXFYR
     C     *LIKE         DEFINE    RHCASE        XXCASE
     C***  BEGIN DEL  ***  08/06/99  *********************************
     C********** *LIKE     DEFN UPRAL     XXAMT
     C***  END   DEL  ***  08/06/99  *********************************
     C     *LIKE         DEFINE    RHCO#         QQCO#
     C     *LIKE         DEFINE    RHFND         QQFND
     C     *LIKE         DEFINE    RHRT01        QQRVCD
     C     *LIKE         DEFINE    RHFYR         QQFYR
     C     *LIKE         DEFINE    RHCASE        QQCASE
     C     *LIKE         DEFINE    UPRAL         QQRAL
     C     *LIKE         DEFINE    RTTYPE        XXTYCD                         ADD 04/01/03
     C     *LIKE         DEFINE    XXAMT         WKRR                           ADD 04/01/03
     C     *LIKE         DEFINE    XXAMT         XXTINC                         ADD 05/13/04
     C*
     C****************************************************************
     C*   PROGRAM ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRVCD
     C                   PARM                    QQRAL
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQRVCD        XXRVCD
     C***  BEGIN CHG  ***  08/06/99  *********************************
     C**********           Z-ADD*ZERO     XXAMT
     C                   Z-ADD     *ZERO         XXAMT            13 2
     C***  END   CHG  ***  08/06/99  *********************************
     C*
     C*** BEG ADD *** 04/01/03 ****************************
     C*
     C*  GET RESERVE CODE TYPE
     C*
     C                   MOVEL     XXRVCD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RTTYPE        XXTYCD
     C                   ENDIF
     C*
     C*** END ADD *** 04/01/03 ****************************
     C*
     C*  CHAIN TO THE USER PARAMETER FILE TO GET THE RESERVE AUTHORITY
     C*  LIMIT
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C     UPRAL         IFNE      *ZEROS                                       ADD 04/01/03
     C                   Z-ADD     UPRAL         XXAMT
     C*
     C*** BEG ADD *** 04/01/03 ****************************
     C*
     C*** IF CUMM AMT USED, MUST BE A MATCHING INCOMING TYPE
     C*
     C     XXTYCD        IFNE      UPRSV1
     C     XXTYCD        ANDNE     UPRSV2
     C     XXTYCD        ANDNE     UPRSV3
     C     XXTYCD        ANDNE     UPRSV4
     C                   Z-ADD     *ZEROS        XXAMT
     C                   GOTO      END
     C                   ENDIF
     C*
     C*** BEG ADD *** 05/13/04 **************************************
     C*
     C* UPTINC = ' ': USE AMOUNTS WITH USER RESERVE HISTORY
     C*
     C     UPTINC        IFNE      'Y'
     C*
     C*** END ADD *** 05/13/04 **************************************
     C*
     C*  SEARCH THE RESERVE HISTORY FILE FOR RESERVES PREVIOUSLY
     C*  ALLOCATED BY THE USER.
     C*
     C     RHKY          SETLL     PRH01
     C     RHKY          READE     PRH01                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C     RHUSER        IFEQ      Q1USER
     C                   XFOOT     RR            WKRR
     C                   SUB       WKRR          XXAMT
     C                   ENDIF                                                  RHUSER = QIUSER
     C*
     C     RHKY          READE     PRH01                                  50
     C                   ENDDO                                                  *IN50 DOW *OFF
     C*
     C     XXAMT         IFGT      UPRAL
     C                   Z-ADD     UPRAL         XXAMT
     C                   ENDIF                                                  XXAMT>UPRAL
     C*
     C*
     C*** BEG ADD *** 05/13/04 **************************************
     C                   ELSE
     C                   EXSR      TOTINC
     C                   ENDIF
     C*** END ADD *** 05/13/04 **************************************
     C*
     C* UPRAL = 0'S, USE PER RESERVE TYPE FIELDS
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     XXTYCD        WHENEQ    'M'
     C                   Z-ADD     UPMRAL        XXAMT
     C     XXTYCD        WHENEQ    'C'
     C                   Z-ADD     UPCRAL        XXAMT
     C     XXTYCD        WHENEQ    'E'
     C                   Z-ADD     UPERAL        XXAMT
     C     XXTYCD        WHENEQ    'L'
     C                   Z-ADD     UPLRAL        XXAMT
     C                   ENDSL
     C*
     C*** END ADD *** 04/01/03 ****************************
     C*
     C*** BEG ADD *** 05/13/04 **************************************
     C*
     C* UPTINC = ' ': USE AMOUNTS WITH USER RESERVE HISTORY
     C*
     C     UPTINC        IFNE      'Y'
     C*
     C*** END ADD *** 05/13/04 **************************************
     C*
     C*  SEARCH THE RESERVE HISTORY FILE FOR RESERVES PREVIOUSLY
     C*  ALLOCATED.
     C*
     C     RHKY          SETLL     PRH01
     C     RHKY          READE     PRH01                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C     RHUSER        IFEQ      Q1USER
     C                   Z-ADD     1             X                 3 0
      /Free
       // Begin Add **** 04/03/15 ************************************
       For i = 1 to 10;
          XXKEY = RT(i);
          chain (XXCO#:XXKEY) PRT01;
          If %found and RTTYPE = XXTYCD;
              XXAMT = XXAMT - RR(i);
          Endif;
       Endfor;
       // End   Add **** 04/03/15 ************************************
      /End-free
      *** Begin Delete 04/03/15  ************************************
     C***     XXRVCD        LOOKUP    RT(X)                                  61
     C***
     C***     *IN61         IFEQ      *ON
     C***                   SUB       RR(X)         XXAMT
     C***                   ENDIF                                                  *IN61 = *OFF
      *** End Delete 04/03/15  ************************************
     C*
     C                   ENDIF                                                  RHUSER = QIUSER
     C*
     C     RHKY          READE     PRH01                                  50
     C                   ENDDO                                                  *IN50 DOW *OFF
     C*
     C*** BEG ADD *** 04/01/03 ****************************
     C*
     C                   SELECT
     C     XXTYCD        WHENEQ    'M'
     C     XXAMT         ANDGT     UPMRAL
     C                   Z-ADD     UPMRAL        XXAMT
     C*
     C     XXTYCD        WHENEQ    'C'
     C     XXAMT         ANDGT     UPCRAL
     C                   Z-ADD     UPCRAL        XXAMT
     C*
     C     XXTYCD        WHENEQ    'E'
     C     XXAMT         ANDGT     UPERAL
     C                   Z-ADD     UPERAL        XXAMT
     C*
     C     XXTYCD        WHENEQ    'L'
     C     XXAMT         ANDGT     UPLRAL
     C                   Z-ADD     UPLRAL        XXAMT
     C                   ENDSL
     C*
     C*** BEG ADD *** 05/13/04 **************************************
     C                   ELSE
     C                   EXSR      TOTINC
     C                   ENDIF
     C*** END ADD *** 05/13/04 **************************************
     C*
     C                   ENDIF
     C*** END ADD *** 04/01/03 ****************************
     C*
     C                   ENDIF                                                  UPKY
     C*
     C*** BEG DEL *** 04/01/03 ****************************
     C***  BEGIN ADD  ***  08/06/99  *********************************
     C*****      XXAMT     IFGT UPRAL
     C*****                Z-ADDUPRAL     XXAMT
     C*****                ENDIF                           XXAMT>UPRAL
     C***  END   ADD  ***  08/06/99  *********************************
     C*** END DEL *** 04/01/03 ****************************
     C*
     C     END           TAG                                                    ADD 04/01/03
     C*
     C                   Z-ADD     XXAMT         QQRAL
     C                   MOVE      *ON           *INLR
     C*
     C*** BEG ADD *** 05/13/04 **************************************
     C*
     C     TOTINC        BEGSR
     C*
     C* UPTINC = 'Y': USE AMOUNTS WITH TOTAL INCURRED
     C*
     C     RHKY          CHAIN     PCM01                              99
     C*
     C                   DO        10            X                 3 0
     C     CT(X)         IFNE      *BLANK
     C                   MOVEL     CT(X)         XXKEY
     C     TBKY          CHAIN     PRT01                              60         Get reserve
     C     *IN60         IFEQ      *OFF                                           type
     C*
     C     UPRAL         IFNE      *ZEROS
     C     CR(X)         ADD       AR(X)         XXTINC
     C                   SUB       XXTINC        XXAMT
     C                   ELSE
     C     RTTYPE        IFEQ      XXTYCD
     C     CR(X)         ADD       AR(X)         XXTINC
     C                   SUB       XXTINC        XXAMT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  e 60 = OFF
     C                   ENDIF                                                  e RT NE blan
     C*
     C                   ENDDO                                                  e DO 10
     C                   ENDSR
     C*
     C*** END ADD *** 05/13/04 **************************************
