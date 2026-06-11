     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F9900R - DETAIL CHARGE LIST BY FND/SS#/ACDT - PRT PGM
     F*
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  08    Batch Mode                                             *
     H*  09    Demand Mode                                            *
     H*  10    Print Service Code   XXCMSV                            *
     H* N10    Print CPT4 Code      CACPT4                            *
     H*  41    ALL CHARGES REQUESTED                                  *
     H*  42    CURRENT CHARGES REQUESTED                              *
     H*  43    PENDED CHARGES REQUESTED                               *
     H*  44    HISTORY CHARGES REQUESTED                              *
     H*  60    General Chain/Usage                                    *
     H***99****CACPT4 = 'FSMRA': don't print Total Charge - NOT USED  *
     H*****************************************************************
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  12/02/97 C:85 PROJECT: MC145                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                CHANGED FUND TO GROUP, CENTERED CONAME.        *
     H*                REARRANGED REVISIONS SECTION.                  *
     H*                                                               *
     H*  10/16/96 C:32  REQ/PROJ: OK005                               *
     H*                 1. CHG CRITERIA FOR PROCESSING 'FSMRA'        *
     H*                    NO SPECIAL PROCESSING FOR 'FSMRA'          *
     H*                                                               *
     H*  10/09/96 C:62   PROJ: MC146                                  *
     H*                     MODIFY OUTPUT SPECS TO INCLUDE ICD9 CODE  *
     H*                                                               *
     H*  05/25/94 C:20   PROJ: MCMNT                                  *
     H*                     1. ADD BILL SUB-TYPE TO REPORT            *
     H*                                                               *
     H*  04/16/93 C:20   PROJ: FS089                                  *
     H*                     1. ADD PROV ZIP CODE AND P1FL01 TO REPORT *
     H*                                                               *
     H*  03/24/93 C:20   PROJ: 99AAE                                  *
     H*                     1. CHANGED 'TOTAL DISALLOWED' TO 'MRA     *
     H*                        REDUCTIONS' MOVE TOTAL CHARGES COLUMN  *
     H*                        TO BEFORE MRA REDUCTIONS COLUMN        *
     H*                                                               *
     H*  03/16/93 C:20   PROJ: 99AAE                                  *
     H*                     1. CHANGED USE OF P1ST TO CAAST           *
     H*                        IF CHARGE WAS ADJUDICATED AGAINST STATE*
     H*                        DIFFERENT THAN PROV STATE REPORT SHOWED*
     H*                        PROVIDER ADJUDICATION SSTATE.          *
     H*                                                               *
     H*  10/30/92 C:20   PROJ: 99AAE                                  *
     H*                     1. ADDED PROCESSING FOR SELECT TYPE OF    *
     H*                        CHARGES.                               *
     H*                                                               *
     H*  10/19/92 C:05   PROJ: FS063                                  *
     H*                  If CACPT4 = 'FSMRA':                         *
     H*                     1.  Turn on *IN99                         *
     H*                     1.  Don't display as Charge field (CATOTC)*
     H*                     2.  Don't add into Total Charge field (TOTCHR)
     H*                                                               *
     H*  05/07/92 KRR PROJ:                                           *
     H*              1. CHANGED REPORT TO PRINT COMMON SERVICE CODE   *
     H*                 (11A) IF CAPRTY IS EQUAL TO '4' ELSE PRINT    *
     H*                 CACPT4 NUMBER.                                *
     H*                                                               *
     H*  03-17-92 KRR PROJ#: P0102                                    *
     H*              1. ADDED ADJ. STATE COLUMN TO REPORT             *
     H*                                                               *
     H*  02/18/92 RV PROJ #: P0102                                    *
     H*              1. ADDED LOGICAL WMPDL FOR XREF                  *
     H*              2. ADDED KLIST PDKY WITH KFLD CAFEIN AND CAMOD   *
     H*              3. ADDED CHAIN USING RECORD FORMAT PPD01       *
     H*              4. ADDED SPECIALTY CODE COLUMN TO REPORT         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     F*** BEGIN ADD *** 02/18/92 **************************************
     FFMPDL     IF   E           K DISK
     F*** END ADD *** 02/18/92 ****************************************
     F*
     F*** BEGIN ADD *** 03-16-92 **************************************
     FFMP1L     IF   E           K DISK
     F*** END ADD *** 03-16-92 ****************************************
     F*** BEGIN ADD *** 03/24/93 **************************************
     FSSYLF     IF   E           K DISK
     F*** END ADD *** 03/24/93 ****************************************
     F*** BEGIN ADD *** 04/23/93 **************************************
     FFDHCL     IF   E           K DISK
     F*** END ADD *** 04/23/93 ****************************************
     F*** BEGIN ADD *** 05/25/94 **************************************
     FFADCL     IF   E           K DISK
     F*** END ADD *** 05/25/94 ****************************************
     F*** BEGIN ADD *** 10/09/96 **************************************
     FFAEOL4    IF   E           K DISK
     F*** END   ADD *** 10/09/96 **************************************
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** BEGIN ADD *** 05/07/92 **************************************
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D*** END ADD *** 05/07/92 ****************************************
     D*** BEGIN ADD *** 04/23/93 **************************************
     D                 DS
     D  XX10A1                 1     10
     D  X110A1                 1      3
     D  X210A1                 4      4
     D*** END ADD *** 04/23/93 ****************************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   Company Master File
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   Detail Charge File
     C     CAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDSEQ
     C*
     C**** BEGIN ADD *** 02/19/92 *************************************
     C*   PROVIDER MASTER FILE
     C     PDKY          KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C**** END ADD *** 02/19/92 ***************************************
     C**** BEGIN ADD *** 04/16/93 *************************************
     C*   PROVIDER AUX FILE
     C     P1KY          KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C**** END ADD *** 04/16/93 ***************************************
     C*
     C*** BEGIN ADD *** 03-16-92 **************************************
     C     STKY          KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C*** END ADD *** 03-16-92 ****************************************
     C*
     C*   Output Routing File
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   User Parameter File
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   Claimant Demographics File
     C     CDKY1         KLIST
     C                   KFLD                    XXSS#
     C*** BEGIN ADD *** 03/24/93 **************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*** END ADD *** 03/24/93 ****************************************
     C*** BEGIN ADD *** 04/23/93 *************************************
     C*  HOSPITAL DETAIL CHARGE FILE
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*** END ADD ***** 04/23/93 *************************************
     C*** BEGIN ADD *** 04/23/93 *************************************
     C*  TEMP DIAGNOSIS CODE FILE
     C     DCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CADCS#
     C*** END ADD ***** 04/23/93 *************************************
     C*
     C*** BEGIN ADD *** 10/09/96 **************************************
     C     EOKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*** END   ADD *** 10/09/96 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8
     C*** BEGIN ADD *** 09/11/92 **************************************
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C*** END ADD *** 09/11/92 ****************************************
     C*** BEGIN ADD *** 10/30/92 **************************************
     C                   PARM                    QQTYPE            1
     C*** END ADD *** 10/30/92 ****************************************
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    CACO#         XXCO#
     C     *LIKE         DEFINE    CASUB#        XXSUB#
     C     *LIKE         DEFINE    CAFND         XXFND
     C     *LIKE         DEFINE    CAFYR         XXFYR
     C     *LIKE         DEFINE    CACASE        XXCASE
     C     *LIKE         DEFINE    CASS#         XXSS#
     C*** BEGIN ADD *** 09/11/92 **************************************
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C*** END ADD *** 09/11/92 ****************************************
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    HCNDC         XXNDC
     C*
     C                   MOVE      *BLANKS       QQLAST           56
     C                   MOVE      *BLANKS       QQFRST           15
     C                   MOVE      *BLANKS       QQTERM            1
     C                   MOVE      *BLANKS       XXNAME           56
     C*** BEGIN ADD *** 10/30/92 **************************************
     C                   MOVE      *BLANKS       XXTYPE            1
     C*** END ADD *** 10/03/92 ****************************************
     C                   Z-ADD     *ZEROS        TOTCNT            7 0
     C*** BEGIN ADD *** 09/11/92 **************************************
     C                   Z-ADD     *ZEROS        TOTCHR           11 2
     C                   Z-ADD     *ZEROS        TOTDIS           11 2
     C                   Z-ADD     *ZEROS        TOTPAD           11 2
     C*** END ADD *** 09/11/92 ****************************************
     C*** BEGIN ADD *** 03/24/93 **************************************
     C                   MOVE      '0'           *IN98
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* CHECK TO SEE IF NETWORK PPO IS INSTALLED FOR ALLOWING F15
     C*
     C     FBFL06        IFEQ      'Y'
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END                                                    END FBFL06
     C                   END                                                    END *IN60
     C*** END ADD **8 03/24/93 ****************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQSS#         XXSS#
     C                   MOVE      QQACDT        XXACDT            8 0
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1ACDT            8 0
     C*** BEGIN ADD *** 09/11/92 **************************************
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C*** END ADD *** 09/11/92 ****************************************
     C*** BEGIN ADD *** 10/30/92 **************************************
     C                   MOVE      QQTYPE        XXTYPE
     C*** END ADD *** 10/03/92 ****************************************
     C                   MOVE      *LOVAL        XXDSEQ            5 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*   SET-UP AND INITIALIZE
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*** BEGIN ADD *** 09/11/92 **************************************
     C                   MOVE      '0'           *IN40
     C     XXFEIN        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN40
     C                   END
     C*** END ADD *** 09/11/92 ****************************************
     C*** BEGIN ADD *** 10/30/92 **************************************
     C*
     C* CHECK TO SEE WHAT TYPE OF CHARGES ARE REQUESTED FOR THIS
     C* LISTING
     C*
     C                   MOVE      '0'           *IN41
     C                   MOVE      '0'           *IN42
     C                   MOVE      '0'           *IN43
     C                   MOVE      '0'           *IN44
     C     XXTYPE        IFEQ      'A'                                          A = ALL
     C                   MOVE      '1'           *IN41
     C                   ELSE                                                   ELSE NE 'A'
     C     XXTYPE        IFEQ      'C'                                          C = CURRENT
     C                   MOVE      '1'           *IN42
     C                   ELSE                                                   ELSE NE 'C'
     C     XXTYPE        IFEQ      'P'                                          P = PENDED
     C                   MOVE      '1'           *IN43
     C                   ELSE                                                   ELSE NE 'P'
     C     XXTYPE        IFEQ      'H'                                          H = HISTORY
     C                   MOVE      '1'           *IN44
     C                   END                                                    END EQ 'H'
     C                   END                                                    END EQ 'P'
     C                   END                                                    END EQ 'C'
     C                   END                                                    END EQ 'A'
     C*** END ADD *** 10/30/92 ****************************************
     C*
     C*   GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*** BEGIN ADD *** 12/02/97 **************************************
     C*
     C*   CENTER COMPANY NAME
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
     C*
     C*** END   ADD *** 12/02/97 **************************************
     C*
     C*   GET CLAIMANT NAME FROM THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY1         CHAIN     PCD01                              60
     C*
     C*  Load Claimant Name for Header
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     CDFSN         QQFRST
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME
     C*
     C*  Get the distribution information
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*  Print the distribution information
      ***** Beg Del ***** 08/26/14 *********************************************
     C*******  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C*       Print headings
     C                   EXCEPT    HEAD
     C*
     C*  Get first record
     C     CAKY1         SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C*
     C*  If no match, print "error message"
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      END
     C                   END                                                    END 1 *IN60
     C*
     C*  Start loop
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 09/11/92 **************************************
     C     XXFEIN        IFNE      *ZEROS
     C     CAFEIN        IFNE      XXFEIN
     C     CAMOD         ORNE      XXMOD
     C                   GOTO      NEXT
     C                   END
     C                   END
     C*** END ADD *** 09/11/92 ****************************************
     C*** BEGIN ADD *** 10/30/92 **************************************
     C*
     C* SELECT OUT THOSE DETAIL CHARGES THAT ARE OF THE REQUESTED TYPE
     C*
     C     *IN41         IFEQ      '0'
     C*
     C     *IN42         IFEQ      '1'
     C     CAEOB#        IFNE      *ZEROS
     C     CAFLAG        ORNE      *BLANKS
     C                   GOTO      NEXT
     C                   END                                                    END CAEOMB
     C                   END                                                    END *IN42
     C*
     C     *IN43         IFEQ      '1'
     C     CAFLAG        IFNE      'P'
     C                   GOTO      NEXT
     C                   END                                                    END CAFLAG
     C                   END                                                    END *IN43
     C*
     C     *IN44         IFEQ      '1'
     C     CAEOB#        IFEQ      *ZEROS
     C                   GOTO      NEXT
     C                   END                                                    END CAEOMB
     C                   END                                                    END *IN44
     C*
     C                   ELSE                                                   ELSE *IN41
     C                   MOVE      '0'           *IN42
     C                   MOVE      '0'           *IN43
     C                   MOVE      '0'           *IN44
     C*
     C                   END                                                    END *IN41
     C*** END ADD *** 10/30/92 ****************************************
     C*
     C*  Print the detail
     C*** BEGIN ADD *** 05/07/92 **************************************
     C                   Z-ADD     CASVCD        XXRVCD            3 0
     C                   MOVE      CACPT4        XXHCPC            5
     C                   Z-ADD     CAUNTS        XXUNTS            3 0
     C*** END ADD *** 05/07/92 ****************************************
     C                   MOVE      '0'           *IN10
     C     CAPRTY        IFEQ      '4'                                          CHG 05/07/92
     C                   MOVE      '1'           *IN10
     C                   END
     C                   Z-ADD     CARDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRDAT            6 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFDAT            6 0
     C*** BEGIN ADD *** 10/19/92 **************************************
     C*** BEGIN DEL *** 10/16/96 **************************************
     C********** CACPT4    IFEQ 'FSMRA'
     C**********           MOVE '1'       *IN99
     C**********           ELSE                            ELSE EQ 'FSMRA'
     C**********           MOVE '0'       *IN99
     C**********           END                             END EQ 'FSMRA'
     C*** END DEL ***** 10/16/96 **************************************
     C*** END ADD ***** 10/19/92 **************************************
     C*** BEGIN ADD *** 10/30/92 **************************************
     C*
     C* IF ALL CHARGES ARE REQUESTED SET INDICATORS ON DEPENDING ON
     C* WHAT TYPE OF CHARGE WE ARE GOING TO PRINT
     C*
     C     *IN41         IFEQ      '1'
     C*
     C     CAFLAG        IFEQ      'P'
     C                   MOVE      '1'           *IN43
     C                   ELSE                                                   ELSE CAFLAG
     C*
     C     CAEOMB        IFNE      *ZEROS
     C                   MOVE      '1'           *IN44
     C                   END                                                    END CAEOMB
     C                   END                                                    END CAFLAG
     C*
     C                   END                                                    END *IN41
     C*** END ADD *** 10/03/92 ****************************************
     C*
     C*** BEGIN ADD *** 03-16-92 **************************************
     C*   GET THE ADJ. STATE
     C     STKY          CHAIN     PP101                              60
     C*** END ADD *** 03-16-92 ****************************************
     C**** BEGIN ADD *** 02/19/92 *************************************
     C*   CHAIN TO FILE WMPDP IN PROVIDER MASTER FILE
     C*
     C     PDKY          CHAIN     PPD01                              60
     C     P1KY          CHAIN     PP101                              60
     C                   MOVE      '0'           *IN25
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     HC10A1        XX10A1
     C                   MOVE      '0'           *IN25
     C     CARCFL        IFEQ      'X'
     C                   MOVE      '1'           *IN25
     C                   MOVEL     HCNDC         XXNDC
     C                   Z-ADD     HCRXMQ        XXUNTS
     C                   ELSE
     C                   Z-ADD     CAUNTS        XXUNTS
     C                   END
     C                   ELSE
     C                   MOVE      *BLANKS       XX10A1
     C                   END
     C**** END ADD *** 02/19/92 ***************************************
     C*** BEGIN ADD *** 05/25/94 ***********************************************
     C     DCKY          CHAIN     PDC01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     6             X                 5 0
     C     3             SUBST     DC10A1:X      XCTYPE            3    65
     C                   ELSE
     C                   MOVE      *BLANKS       XCTYPE
     C                   END                                                    END *IN60
     C*** END ADD *** 05/25/94 *************************************************
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*** BEGIN ADD *** 10/09/96 **************************************
     C     EOKEY         CHAIN     PEO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XICD9             5
     C                   SELECT
     C     CADXR         WHENEQ    1
     C                   MOVE      EODIA1        XICD9
     C     CADXR         WHENEQ    2
     C                   MOVE      EODIA2        XICD9
     C     CADXR         WHENEQ    3
     C                   MOVE      EODIA3        XICD9
     C     CADXR         WHENEQ    4
     C                   MOVE      EODIA4        XICD9
     C                   ENDSL
     C                   ELSE
     C                   MOVE      *BLANKS       XICD9             5
     C                   ENDIF
     C*** END   ADD *** 10/09/96 **************************************
     C                   EXCEPT    DET
     C                   ADD       1             TOTCNT
     C*** BEGIN ADD *** 09/11/92 **************************************
     C*** BEGIN CHG *** 10/19/92 **************************************
     C*** BEGIN DEL *** 10/16/96 **************************************
     C********** CACPT4    IFNE 'FSMRA'
     C                   ADD       CATOTC        TOTCHR
     C**********           END                             END NE 'FSMRA'
     C*** END   DEL *** 10/16/96 **************************************
     C*** END CHG ***** 10/19/92 **************************************
     C                   ADD       CATOTD        TOTDIS
     C                   ADD       CATOTP        TOTPAD
     C*** END ADD *** 09/11/92 ****************************************
     C*
     C     NEXT          TAG
     C     CAKY          READE     PCA01                                  60
     C                   END
     C*
     C*  End of program
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*** BEGIN ADD *** 10/30/92 **************************************
     C     TOTCNT        IFEQ      *ZEROS
     C                   EXCEPT    ERRMSG
     C                   END
     C*** END ADD *** 10/30/92 ****************************************
     C                   EXCEPT    TOT
     C*
     C     END           TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'COMPANY        :'
     O                       XXCO#               38
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER     :'
     O                       XXSUB#              38
     O          E            ROUTE          2
     O*** BEGIN DEL *** 12/02/97 **************************************
     O                                           26 'FUND           :'
     O*** END   DEL *** 12/02/97 **************************************
     O*** BEGIN CHG *** 12/02/97 **************************************
     O                                           26 'GROUP          :'
     O*** END   CHG *** 12/02/97 **************************************
     O                       XXFND               38
     O          E            ROUTE          2
     O*** BEGIN DEL *** 12/02/97 **************************************
     O                                           26 'FUND YEAR      :'
     O*** END   DEL *** 12/02/97 **************************************
     O*** BEGIN CHG *** 12/02/97 **************************************
     O                                           26 'POLICY YEAR    :'
     O*** END   CHG *** 12/02/97 **************************************
     O                       XXFYR         Z     38
     O          E            ROUTE          2
     O                                           26 'CASE NUMBER    :'
     O                       XXCASE        Z     38
     O          E            ROUTE          2
     O                                           26 'SS NUMBER      :'
     O                       XXSS#               38
     O          E            ROUTE          2
     O                                           26 'ACCIDENT DATE  :'
     O                       X1ACDT              38 '  /  /    '
     O*** END ADD *** 09/11/92 ****************************************
     O          E            ROUTE          2
     O                                           26 'PROVIDER/MOD   :'
     O              N40      XXFEIN        Z     38
     O              N40      XXMOD         Z     42
     O               40                          38 'ALL'
     O*** END ADD *** 09/11/92 ****************************************
     O*** BEG ADD *** 10/30/92 ****************************************
     O          E            ROUTE          2
     O                                           26 'CHARGE TYPE    :'
     O               41                          40 'ALL CHARGES'
     O               42                          44 'CURRENT CHARGES'
     O               43                          43 'PENDED CHARGES'
     O               44                          44 'HISTORY CHARGES'
     O*** END ADD *** 10/30/92 ****************************************
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O                       DSDATE              38 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                63 'INC. 1992-1996       '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O*** BEGIN DEL *** 12/02/97 **************************************
     O                       DSDATE        Y    155
     O*** END   DEL *** 12/02/97 **************************************
     O*** BEGIN CHG *** 12/02/97 **************************************
     O                       DSDATE        Y    197
     O*** END   CHG *** 12/02/97 **************************************
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O*** BEGIN DEL *** 12/02/97 **************************************
     O**********                        149 'PAGE:'
     O**********               PAGE  Z  155
     O*** END   DEL *** 12/02/97 **************************************
     O*** BEGIN CHG *** 12/02/97 **************************************
     O                                          191 'PAGE:'
     O                       PAGE          Z    197
     O*** END   CHG *** 12/02/97 **************************************
     O          E            HEAD           1
     O*** BEGIN DEL *** 12/02/97 **************************************
     O**********                         27 'FUND'
     O*** END   DEL *** 12/02/97 **************************************
     O                                           53 'ACCIDENT'
     O          E            HEAD           1
     O                                           04 'CO#'
     O                                           11 'SUB#'
     O*** BEGIN DEL *** 12/02/97 **************************************
     O**********                         18 'FUND'
     O*** END   DEL *** 12/02/97 **************************************
     O*** BEGIN CHG *** 12/02/97 **************************************
     O                                           18 'GROUP'
     O*** END   CHG *** 12/02/97 **************************************
     O                                           27 'YEAR'
     O                                           33 'CASE'
     O                                           45 'SOC.SEC.#'
     O                                           51 'DATE'
     O                                           82 'CLAIMANT NAME'
     O          E            HEAD           2
     O                       XXCO#               04
     O                       XXSUB#              11
     O                       XXFND               18
     O                       XXFYR         Z     26
     O                       XXCASE        Z     35
     O                       XXSS#               45
     O                       X1ACDT              56 '  /  /    '
     O                       XXNAME             128
     O          E            HEAD           1
     O**********                         12 'DETAIL'
     O                                           06 'DETAIL'
     O**********                         36 'TRANS'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         26 'TRANS'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           25 'TRANS'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         61 'PROV'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         49 'PROV'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           47 'PROV'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         68 'GEO'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         55 'GEO'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           52 'GEO'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         73 'SPEC'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         61 'SPEC'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           57 'SPEC'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         80 'ADJ'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         67 'ADJ'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           62 'ADJ'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         85 'CLM'
     O*** BEGIN ADD *** 10/09/96 **************************************
     O**********                         72 'CLM'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           66 'CLM'
     O                                           72 'ICD-9'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         93 'SERV'
     O                                           80 'SERV'
     O**********                        102 'PLACE'
     O                                           89 'PLACE'
     O                                           97 'BILL'
     O                                          120 'PROC'
     O                                          131 'ANES'
     O                                          147 'PART'
     O                                          158 'TOTAL'
     O                                          171 'MRA'
     O               98                         184 '*'
     O                                          190 'TOTAL'
     O                                          197 'EOMB'
     O          E            HEAD           1
     O**********                         10 'SEQ#'
     O                                           04 'SEQ#'
     O**********                         27 'EOMB#/LTR#'
     O                                           18 'EOMB#/LTR#'
     O**********                         35 'DATE'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         25 'DATE'
     O*** ADD   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           24 'DATE'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         50 'PROVIDER'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         39 'PROVIDER'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           37 'PROVIDER'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         56 'MOD'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         44 'MOD'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           41 'MOD'
     O*** BEGIN ADD *** 10/09/96 **************************************
     O**********                         60 'ZIP'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         48 'ZIP'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           46 'ZIP'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         68 'ZIP'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         55 'ZIP'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           52 'ZIP'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         73 'CODE'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         61 'CODE'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           57 'CODE'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         80 'ST'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********                         66 'ST'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           61 'ST'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         85 'STS'
     O*** BEGIN DEL *** 10/09/96 **************************************
     O                                           72 'STS'
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                                           66 'STS'
     O                                           71 'CODE'
     O*** END   ADD *** 10/09/96 **************************************
     O**********                         93 'DATE'
     O                                           80 'DATE'
     O**********                        102 'OF SRV'
     O                                           89 'OF SRV'
     O                                           99 'SUB-TYPE'
     O                                          109 'CODE'
     O                                          120 'MOD'
     O                                          130 'MIN.'
     O                                          141 'UNITS'
     O                                          147 'CODE'
     O                                          158 'CHARGED'
     O                                          175 'REDUCTIONS'
     O                                          190 'PAID'
     O                                          197 'CODE'
     O          E            DET            1
     O**********               CADSEQZ   11
     O                       CADSEQ        Z     05
     O**********       44      CAEOB#Z   25
     O               44      CAEOB#        Z     16
     O**********       43      CALTR#Z   25
     O               43      CALTR#        Z     16
     O**********       43                27 'L'
     O               43                          18 'L'
     O**********               XXRDAT    38 '  /  /  '
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********               XXRDAT    28 '  /  /  '
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                       XXRDAT              27 '  /  /  '
     O*** END   ADD *** 10/09/96 **************************************
     O**********               CAFEIN    50
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********               CAFEIN    39
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                       CAFEIN              37
     O*** END   ADD *** 10/09/96 **************************************
     O**********               CAMOD Z   56
     O*** BEGIN ADD *** 10/09/96 **************************************
     O**********               CAMOD Z   44
     O*** END   ADD *** 10/09/96 **************************************
     O*** BEGIN DEL *** 10/09/96 **************************************
     O                       CAMOD         Z     41
     O*** END   DEL *** 10/09/96 **************************************
     O**********               HC6N1 Z   62
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********               HC6N1 Z   50
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                       HC6N1         Z     48
     O*** END   ADD *** 10/09/96 **************************************
     O**********               CAZIPA    68
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********               CAZIPA    55
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                       CAZIPA              52
     O*** END   ADD *** 10/09/96 **************************************
     O**********               X110A1    72
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********               X110A1    60
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                       X110A1              56
     O*** END   ADD *** 10/09/96 **************************************
     O**********               X210A1    75
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********               X210A1    62
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                       X210A1              58
     O*** END   ADD *** 10/09/96 **************************************
     O**********               CAAST     80
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********               CAAST     66
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                       CAAST               61
     O*** END   ADD *** 10/09/96 **************************************
     O**********               CACSCD    85
     O*** BEGIN DEL *** 10/09/96 **************************************
     O**********               CACSCD    72
     O*** END   DEL *** 10/09/96 **************************************
     O*** BEGIN ADD *** 10/09/96 **************************************
     O                       CACSCD              66
     O                       XICD9               72
     O*** END   ADD *** 10/09/96 **************************************
     O**********               XXFDAT    95 '  /  /  '
     O                       XXFDAT              82 '  /  /  '
     O**********               CAIPOP   100
     O                       CAIPOP              86
     O                       XCTYPE              96
     O               10N25   XXCMSV             114
     O              N10N25   CACPT4             108
     O              N10 25   XXNDC              114
     O                       CAPMOD             119
     O                       CAANMN        4    131
     O                       XXUNTS        4    142
     O                       CAPART             146
     O*** BEGIN CHG *** 10/16/96 *******************************************
     O*************** N99      CATOTCK  159
     O                       CATOTC        K    159
     O*** END   CHG *** 10/16/96 *******************************************
     O                       CATOTD        K    176
     O                       CATOTP        K    192
     O                       CAEOMB             197
     O          E            TOT         1
     O                                           17 'TOTAL # OF ITEMS:'
     O                       TOTCNT        Z     25
     O*** BEGIN ADD *** 09/11/92 **************************************
     O                                           90 'TOTAL AMOUNT:'
     O*************************TOTCHRK  135
     O                       TOTCHR        K    159
     O                       TOTDIS        K    176
     O                       TOTPAD        K    192
     O*** BEGIN ADD *** 03/24/93 **************************************
     O          E            TOT         3
     O               98                          08 '*'
     O               98                          33 'TOTAL PAID REFLECTS ADDI'
     O               98                          55 'TIONAL REDUCTIONS FROM'
     O               98                          79 '"OTHER" AND "NETWORK DI'
     O               98                         101 'SCOUNTS".  USE THE NET'
     O               98                         125 'WORK CHARGES LISTING FOR'
     O               98                         150 'A FULL DISPLAY OF AMOUNT'
     O               98                         152 'S.'
     C*** END ADD *** 03/24/93 ****************************************
     O*** END ADD *** 09/11/92 ****************************************
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
