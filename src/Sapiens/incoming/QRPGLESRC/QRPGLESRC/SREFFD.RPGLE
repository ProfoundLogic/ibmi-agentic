     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SREFFD - RETURN ARD/POLICY EFF DATE FOR DISCOUNTS
     H*****************************************************************
     H*    PROGRAM DESCRIPTION:                                       *
     H*                                                               *
     H*       THIS PROGRAM WILL RETURN THE APPROPRIATE RATING DATE    *
     H*       TO BE USED ON CHAINING TO A GROUP FILE FOR A DISCOUNT.  *
     H*       THIS ONLY APPLIES TO DISCOUNTS WHICH USE ARD RATING.    *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   VARIOUS                                                     *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    04/10/06            PROJECT: SUPP#51293           *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H* 61  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/04/25 C:65  Project: Phw#119668 (Sunz)                     *
     H*                Remove CLTID call to help clean up job logs.   *
     H*                                                               *
     H* 09/10/20 C:65  Project: Phw#91280 (Sunz)                      *
     H*                Remove PA exception for pulling rates - always *
     H*                use state reporting division policy eff date.  *
     H*                                                               *
     H* 11/30/12 C:136 SUPPORT: WA146 81173                           *
     H*                RETURN RATE DATE AND POLICY DATE ARRAY IF      *
     H*                REQUESTED                                      *
     H*                                                               *
     H* 10/18/10 C:251 Project: WC1939                                *
     H*                09/01/2010 OK Rate split                       *
     H*                If a policy with OK coverage is unexpired as   *
     H*                of 09/01/10 then the policy will need to be    *
     H*                split for the rate decrease                    *
     H*                                                               *
     H* 09/10/10 C:180 SUPPORT 70749 - Per MD's instructions, this
     H*                change should be applicable to client 093 only,
     H*                except for 'MOVE ELRTCD    XXRTCD' change on
     H*                01/13/10 which is applicable to all clients.
     H*
     H* 05/08/10 C:136 PROJECT: WC1861 PRISS 5                        *
     H*                ADD DATE RANGE TO AZ RATE SPLIT LOGIC          *
     H*                                                               *
     H* 01/13/10 C:180 SUPPORT 70749 - Incorrect rate - Effective date
     H*                for rating should be from state reporting div.
     H*
     H*                PA child div to get rate eff date from the
     H*                state reporting div regardless of what rating
     H*                plan the state reporting div has.
     H*
     H*
     H* 12/29/09 C:136 PROJECT: WC1861                                *
     H*                ADD AZ RATE SPLIT - 01/01/2010                 *
     H*                                                               *
     H* 11/23/09 C:136 PROJECT: 139WNR                                *
     H*                FL RATE SPLIT FOR 07/01/09 - CLIENT SPECIFIC   *
     H*                                                               *
     H* 06/17/09 C:136 PROJECT: WC1834                                *
     H*                FL RATE CHG FOR 07/01/09                       *
     H*                                                               *
     H* 12/16/08 C:136 PROJECT: WC1768                                *
     H*                DE RATE SPLIT FOR 10/01/08                     *
     H*                                                               *
     H* 09/09/08 C:136 SUPPORT: WA132 65141                           *
     H*                IF QQCLAS IS NOT PASSED IN DON'T TRY TO        *
     H*                USE 3-TIER LOGIC                               *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Added a new parameter for Payroll Class.       *
     H*                Payroll processing will now be altered to      *
     H*                pull rates using a three tier system when      *
     H*                rates cannot be found using the normal system. *
     H*                                                               *
     H* 10/09/07 C:136 SUPPORT: WA093 60489                           *
     H*                ADDED PROCESSING TO GET PAYROLL RATING DATE    *
     H*                IF REQUESTED AS A PAYROLL RATING TYPE          *
     H*                (QQTYPE = 'P')                                 *
     H*                                                               *
     H* 04/05/07  C:65  PROJECT: 114WMF                               *
     H*                 IF A NJ POLICY IS BEING CHECKED ONLY PULL     *
     H*                 DISCOUNTS BY THE POLICY EFF DATE.             *
     H*                                                               *
     H*****************************************************************
     FWDELL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F*** BEG ADD ***** 04/01/08 *************************************
     FWMPRL1    IF   E           K DISK
     F*** END ADD ***** 04/01/08 *************************************
     F*** BEGIN ADD ****** 10/18/10 ************************************
     FWMRTL     IF   E           K DISK
     F                                     RENAME(PRT01:PRT0X)
     F*** END   ADD ****** 10/18/10 ************************************

     D*
     D RDAT            S              7  0 DIM(8) ASCEND                        RATING DATES
      *** BEG ADD ***** 10/09/07 *************************************
     D PDAT            S              7  0 DIM(8) ASCEND                        POLICY DATES
      *** END ADD ***** 10/09/07 *************************************
      *** BEG ADD ***** 11/30/12 *************************************
     D rdc             S              7    DIM(8)                               rate dates char
     D pdc             S              7    DIM(8)                               policy dates char
      *** END ADD ***** 11/30/12 *************************************
     D*
      *** BEG ADD ***** 10/09/07 *************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** END ADD ***** 10/09/07 *************************************
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0

     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*** BEG ADD ***** 04/01/08 *************************************
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXDATE
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C*** END ADD ***** 04/01/08 *************************************
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQDATE            7
      *** BEG ADD ***** 10/09/07 *****************************************
     C                   PARM                    QQTYPE            1
      *** END ADD ***** 10/09/07 *****************************************
     C*** BEG ADD ***** 04/01/08 *************************************
     C                   PARM                    QQCLAS            5
     C*** END ADD ***** 04/01/08 *************************************
     C*** BEG ADD ***** 11/30/12 *************************************
     C                   PARM                    QQRDAT           56
     C                   PARM                    QQPDAT           56
     C*** END ADD ***** 11/30/12 *************************************
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDIV         X1DIV
     C     *LIKE         DEFINE    ELDIV         SV1DIV
     C     *LIKE         DEFINE    ELPFDT        X1PFDT
     C     *LIKE         DEFINE    ELPFDT        X1ANDT
     C     *LIKE         DEFINE    ELPFDT        XXSTDT
     C     *LIKE         DEFINE    ELPFDT        XXDATE
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C*
     C*    SET UP AND INITALIZE
     C*
      *** BEG ADD ***** 11/23/09 **********************************
     C*** begin chg ***  09/04/25  ***********************************
     C*****              CALL      'RTVCLTID'
     C*****              PARM                    CLTID             3
     C                   Move      *blanks       CLTID             3
     C*** end chg *****  09/04/25  ***********************************
      *** END ADD ***** 11/23/09 **********************************
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQDATE        XXDATE
     C                   Z-ADD     9999999       RDAT
      *** BEG ADD ***** 10/09/07 **********************************
     C                   Z-ADD     9999999       PDAT
      *
     C     Q1PRMS        IFGT      6
     C                   MOVE      QQTYPE        XXTYPE            1
     C                   ENDIF
      *** END ADD ***** 10/09/07 **********************************
     C*** BEG ADD ***** 04/01/08 **********************************
      *
     C     Q1PRMS        IFGT      7
     C                   MOVE      QQCLAS        XXCLAS            5 0
     C                   ENDIF                                                  Q1PRMS IFGT 7
     C*** END ADD ***** 04/01/08 **********************************
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C  N60ELKEY         CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C                   Z-ADD     XXDIV         X1DIV
     C                   Z-ADD     0             X1PFDT
     C*
     C*** begin del ***  09/10/20  *************************************
     C*****XXST          IFEQ      'PA'
     C*****              Z-ADD     ELPFDT        X1PFDT
     C*****              ENDIF
     C*** end del *****  09/10/20  *************************************
     C*
     C                   Z-ADD     0             X1ANDT
     C                   MOVE      *OFF          *IN61
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         SV1DIV
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C     EMSTRP        IFEQ      'Y'
     C     EMDIV         ANDNE     X1DIV
     C*** begin del ***  09/10/20  *************************************
     C*****XXST          OREQ      'PA'
     C*****EMDIV         ANDNE     X1DIV
     C*
     C*****XXST          IFEQ      'PA'
     C*
     C*****              Z-ADD     EMDIV         XXDIV
     C*****ELKEY         CHAIN     PEL01                              60
     C******IN60         IFEQ      *OFF
     C*****              MOVE      ELRTCD        XXRTCD
     C*****RSKY          CHAIN     PRS01                              60
     C*
      **** BEG DELETE ***** 01/13/10 ********************************
     C***        *IN60     IFEQ *OFF
     C***        RSJRST    ANDEQ'PA'
     C***        ELPFDT    ANDLTX1PFDT
     C***                  Z-ADDELPFDT    X1PFDT
     C***                  ENDIF                           *IN60 IFEQ
      **** END DELETE ***** 01/13/10 ********************************
     C** BEGIN ADD **** 09/10/10 **************************************
     C*****              SELECT
     C*****CLTID         WHENEQ    '093'
     C** END   ADD **** 09/10/10 **************************************
      **** BEG ADD ******** 01/13/10 ********************************
     C******IN60         IFEQ      *OFF
     C*****EMSTRP        ANDNE     'Y'
     C*****RSJRST        ANDEQ     'PA'
     C*****ELPFDT        ANDLT     X1PFDT
     C*****              Z-ADD     ELPFDT        X1PFDT
     C*****              ENDIF                                                  *IN60 IFEQ
      *
     C******IN60         IFEQ      *OFF
     C*****EMSTRP        ANDEQ     'Y'
     C*****ELPFDT        ANDLT     X1PFDT
     C*****              Z-ADD     ELPFDT        X1PFDT
     C*****              ENDIF                                                  *IN60 IFEQ
      **** END ADD ******** 01/13/10 ********************************
     C*
     C** BEGIN ADD **** 09/10/10 **************************************
     C*****              OTHER
     C******IN60         IFEQ      *OFF
     C*****RSJRST        ANDEQ     'PA'
     C*****ELPFDT        ANDLT     X1PFDT
     C*****              Z-ADD     ELPFDT        X1PFDT
     C*****              ENDIF                                                  *IN60 IFEQ
     C*****              ENDSL
     C** END   ADD **** 09/10/10 **************************************
     C*****              ENDIF                                                  *IN60 IFEQ
     C*
     C*****              ELSE                                                   XXST IFEQ
     C*** end del *****  09/10/20  *************************************
     C*
     C                   Z-ADD     EMDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN61
     C                   Z-ADD     ELPFDT        X1PFDT
     C                   Z-ADD     ELANDT        X1ANDT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*** begin del ***  09/10/20  *************************************
     C*****              ENDIF                                                  XXST IFEQ
     C*** end del *****  09/10/20  *************************************
     C*
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C  N61EMKY          READE     PEM01                                  60
     C   61              MOVE      *ON           *IN60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* RESET EMPLOYER INFORMATION
     C*
     C                   Z-ADD     X1DIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
     C     ELKEY         CHAIN     PEL01                              60
      *
     C***** BEGIN ADD ***** 01/13/10 **********************************
     C                   MOVE      ELRTCD        XXRTCD
     C***** END ADD ******* 01/13/10 **********************************
     C*
     C* DETERMINE IF STATE REPORTING DIV INFO MUST BE USED FOR RATES
     C*
     C                   Z-ADD     0             XXSTDT
     C*
     C     *IN61         IFEQ      *ON
     C     ELANDT        ANDEQ     *ZERO
     C*
     C     X1ANDT        IFNE      *ZERO
     C                   ADD       10000         X1ANDT
     C     X1ANDT        IFLE      ELPFDT
     C                   Z-ADD     X1ANDT        XXSTDT
     C                   ELSE
     C     X1ANDT        SUB       10000         XXSTDT
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXSTDT        IFEQ      *ZERO
     C     X1PFDT        IFLT      ELPFDT
     C                   Z-ADD     X1PFDT        XXSTDT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C* BUILD RATING DATE ARRAY.
     C*
     C                   Z-ADD     ELPFDT        RDAT(1)
     C*** BEG ADD ***** 10/09/07 **************************************
     C                   Z-ADD     ELPFDT        PDAT(1)
     C*** END ADD ***** 10/09/07 **************************************
     C*
     C     XXSTDT        IFNE      *ZERO
     C                   Z-ADD     XXSTDT        RDAT(1)
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXANDT
     C                   Z-ADD     2             X                 2 0
     C*
     C     ELANDT        IFNE      0
     C     ELANDT        ANDNE     ELPFDT
     C*
     C                   MOVE      ELANDT        XXANDT
     C                   ADD       1             XXANYR
     C*
     C     XXANDT        IFGT      ELPFDT
     C                   MOVE      ELANDT        RDAT(1)
     C                   ENDIF
     C*
     C     XXANDT        IFGE      ELPTDT
     C     XXANDT        ORLE      ELPFDT
     C                   MOVE      *ON           *IN78
     C                   ELSE
     C                   MOVE      *OFF          *IN78
     C                   ENDIF
     C*
     C     *IN78         IFEQ      '0'
      *** BEG ADD ***** 10/09/07 **********************************
     C                   Z-ADD     XXANDT        PDAT(X)
      *** END ADD ***** 10/09/07 **********************************
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ADD       1             X
     C                   ENDIF                                                    *IN78='0'
     C*
     C                   ENDIF                                                  ELANDT IFNE
     C*
      *** BEG ADD ***** 10/09/07 **********************************
     C                   Z-ADD     ELPTDT        PDAT(X)
     C*
     C* INCLUDE FLORIDA RATE SPLIT DATES FOR PAYROLL PROCESSING.
     C*
     C     XXTYPE        IFEQ      'P'
     C*
     C     XXST          IFEQ      'FL'
     C*
     C                   DO        6             W                 2 0
     C     W             ADD       1             Z                 2 0
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFLT      1031001
     C     PDAT(W)       ANDLT     1031001
     C     PDAT(Z)       ANDGT     1031001
     C*
     C                   ADD       1             X
     C                   Z-ADD     1031001       PDAT(X)
     C                   Z-ADD     1031001       RDAT(X)
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 6 W
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* SORT DATE ARRAYS INTO SEQUENCE.
     C*
     C                   SORTA     PDAT
     C                   SORTA     RDAT
     C                   ADD       1             X
     C*
     C* CHECK IF ANY POLICY PERIODS NEED TO BE SPLIT FOR 4/1/3 RATES.
     C*
     C     XXST          IFEQ      'FL'
     C*
     C                   DO        6             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFGE      1030101
     C     RDAT(W)       ANDLE     1030331
     C     PDAT(W)       ANDLT     1030401
     C     PDAT(Z)       ANDGT     1030401
     C*
     C                   Z-ADD     1030401       PDAT(X)
     C                   Z-ADD     1030401       RDAT(X)
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 8 X
     C*
      *** BEG ADD ***** 11/23/09 *************************************
     C*
     C* FL RATE SPLIT FOR 07/01/09 FOR GIC
     C*
     C*** begin chg ***  09/04/25  ***********************************
     C* GIC no longer client, client ID not set in pgm.
     C*** end chg *****  09/04/25  ***********************************
     C     CLTID         IFEQ      '139'
     C*
     C                   DO        6             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFGE      1090401
     C     RDAT(W)       ANDLT     1090701
     C     PDAT(W)       ANDLT     1090701
     C     PDAT(Z)       ANDGT     1090701
     C*
     C                   Z-ADD     1090701       PDAT(X)
     C                   Z-ADD     1090701       RDAT(X)
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 8 X
     C*
     C                   ENDIF                                                  CLTID = '139'
      *** END ADD ***** 11/23/09 *************************************
      *** BEG ADD ***** 06/17/09 *************************************
     C     ELANDT        IFGE      1090401
     C     ELANDT        ANDLT     1090701
     C     ELPFDT        ANDGE     1090701
     C                   Z-ADD     1090701       RDAT(1)
     C                   ENDIF
      *** END ADD ***** 06/17/09 *************************************
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
      *** BEG ADD ***** 12/16/08 *************************************
     C     XXST          IFEQ      'DE'
     C*
     C                   DO        6             W                 2 0
     C     W             ADD       1             Z                 2 0
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFLT      1081001
     C     PDAT(W)       ANDLT     1081001
     C     PDAT(Z)       ANDGT     1081001
     C*
     C                   Z-ADD     1081001       PDAT(X)
     C                   Z-ADD     1081001       RDAT(X)
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 6 W
     C*
     C                   ENDIF                                                  XXST IFEQ
      *** END ADD ***** 12/16/08 *************************************
      *** BEG ADD ***** 12/29/09 *************************************
     C     XXST          IFEQ      'AZ'
     C*
     C                   DO        6             W                 2 0
     C     W             ADD       1             Z                 2 0
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
      *** BEG CHG ***** 05/08/10 *************************************
     C***********RDAT,W    IFLT 1100101
     C***********PDAT,W    ANDLT1100101
     C***********PDAT,Z    ANDGT1100101
     C     RDAT(W)       IFGE      1091001
     C     RDAT(W)       ANDLE     1091231
     C     PDAT(W)       ANDLT     1100101
     C     PDAT(Z)       ANDGT     1100101
      *** END CHG ***** 05/08/10 *************************************
     C*
     C                   Z-ADD     1100101       PDAT(X)
     C                   Z-ADD     1100101       RDAT(X)
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 6 W
      *** BEG ADD ***** 05/08/10 *************************************
     C     ELANDT        IFGE      1091001
     C     ELANDT        ANDLE     1091231
     C     ELPFDT        ANDGE     1100101
     C                   Z-ADD     1100101       RDAT(1)
     C                   ENDIF
      *** END ADD ***** 05/08/10 *************************************
     C*
     C                   ENDIF                                                  XXST IFEQ
      *** END ADD ***** 12/29/09 *************************************
      *** BEG ADD ***** 10/18/10 *************************************
     C     XXST          IFEQ      'OK'
     C*
     C* Check opt out flag for OK 1100901 rate change
     C                   Z-ADD     1100901       XXSPDT            7 0
     C                   EXSR      SRAPRT
     C     XXOPTF        IFEQ      'N'
     C*
     C                   DO        6             W                 2 0
     C     W             ADD       1             Z                 2 0
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       ANDLT     XXSPDT
     C     PDAT(W)       ANDLT     XXSPDT
     C     PDAT(Z)       ANDGT     XXSPDT
     C*
     C                   Z-ADD     XXSPDT        PDAT(X)
     C                   Z-ADD     XXSPDT        RDAT(X)
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDDO                                                  DO 6 W
     C*
     C                   ENDIF                                                  XXOPTF FLAG
     C*
     C                   ENDIF                                                  XXST IFEQ
      *** END ADD ***** 10/18/10 *************************************
     C*
     C* SORT DATE ARRAYS INTO SEQUENCE.
     C*
     C                   SORTA     PDAT
     C                   SORTA     RDAT
     C*
     C                   ENDIF                                                  XXTYPE IFEQ
      *** END ADD ***** 10/09/07 **********************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* SEARCH RATING DATE ARRAY FOR PROPER DATE TO PULL DISCOUNT.
     C*
     C                   Z-ADD     1             X
     C     XXDATE        LOOKUP    RDAT(X)                            60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   Z-ADD     RDAT(X)       XXDATE
     C                   ELSE
     C                   Z-ADD     1             X
     C                   Z-ADD     ELPFDT        XXDATE
     C                   END
     C*** BEG ADD ***** 04/01/08 **********************************
     C     XXTYPE        IFEQ      'P'
     C*** BEG ADD ***** 09/09/08 **********************************
     C     XXCLAS        ANDNE     *ZERO
     C*** END ADD ***** 09/09/08 **********************************
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  61
     C*
     C     *IN61         IFEQ      '1'
     C                   MOVE(P)   ELPFDT        XXDATE
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  61
     C*
     C     *IN61         IFEQ      '1'
     C                   MOVE(P)   QQDATE        XXDATE
     C                   ENDIF                                                  *IN61 IFEQ '1'
     C*
     C                   ENDIF                                                  *IN61 IFEQ '1'
     C*
     C                   ENDIF                                                  XXTYPE IFEQ 'P'
     C*** END ADD ***** 04/01/08 **********************************
     C*
     C*** BEGIN ADD ***  04/05/07  ****************************************
     C     XXST          IFEQ      'NJ'
      *** BEG ADD ***** 10/09/07 **********************************
     C     XXTYPE        ANDNE     'P'
      *** END ADD ***** 10/09/07 **********************************
     C                   Z-ADD     ELPFDT        XXDATE
     C     X1PFDT        IFLT      XXDATE
     C     X1PFDT        ANDNE     *ZERO
     C                   Z-ADD     X1PFDT        XXDATE
     C                   ENDIF
     C                   ENDIF                                                  XXST IFEQ
     C*** END ADD *****  04/05/07  ****************************************
     C                   MOVE      XXDATE        QQDATE
     C*** BEG ADD ***** 11/30/12 **********************************
     C     Q1PRMS        IFGT      8
     C                   DO        8             IX                1 0
     C                   MOVE      RDAT(IX)      RDC(IX)
     C                   ENDDO
     C                   MOVEA     RDC           QQRDAT
     C                   ENDIF
     C     Q1PRMS        IFGT      9
     C                   DO        8             IX                1 0
     C                   MOVE      PDAT(IX)      PDC(IX)
     C                   ENDDO
     C                   MOVEA     PDC           QQPDAT
     C                   ENDIF
     C*** END ADD ***** 11/30/12 **********************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*** BEGIN ADD ****** 10/18/10 ************************************
      *=============================================================
      * ApplyRateSplit - Check option flag to apply rate split
      *=============================================================
     C     SRAPRT        BEGSR
     C*
     C     RTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXSPDT            7 0
     C*
     C                   MOVE      'N'           XXOPTF            1
     C     RTKY          CHAIN     PRT0X                              60
     C     *IN60         IFEQ      *OFF
     C     RTOPT         ANDEQ     'Y'
     C                   MOVE      'Y'           XXOPTF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD ****** 10/18/10 ************************************
