     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F7011R - TABLE FILE REPORT
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 12/27/00 C:62 PROJECT: MCMNT
     H*               CHANGE THE LOGICAL FOR PLACE OF SERVICE.
     H*
     H* 10/27/00 C:62 PROJECT: MC205
     H*               ADD STATE TO FACILITY REPORT AND PLACE OF SERVICE
     H*
     H* 09/11/00 C:62 PROJECT: MC205
     H*               ROUTING SHEET SHOULD ONLY REFERENCE THE PAR-
     H*               TICULAR DATA FILE SUBMITTED.
     H*
     H* 08/15/00 C:62 PROJECT: MC215
     H*               REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H* 07/28/00  C:62  Project: MC205
     H*                 CONDITION THE STATE IN THE REPORT.
     H*
     H* 07/24/95  C:05  Project: FL006
     H*                 Chgd flag parameter from 14 to 15
     H*                 Added print of Place of Service Code (by State)
     H*
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FFTCVL     IF   E           K DISK
     FFTCLL     IF   E           K DISK
     FFTMCL     IF   E           K DISK
     FFTSSL     IF   E           K DISK
     F*** BEGIN ADD *** 12/27/00 **************************************
     FFTPSL     IF   E           K DISK
     F                                     RENAME(PPS01:PPS02)
     F*** END   ADD *** 12/27/00 **************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D*** BEGIN DEL *** 08/15/00 **************************************
     D**********SRC01
     D**********    RCKEY                           RCKE1
     D**********SCS01
     D**********    CSKEY                           CSKE1
     D*** END   DEL *** 08/15/00 **************************************
     D*
     D                 DS
     D  FL                     1     15
     D                                     DIM(15)                              CHG 07/24/95
     D  SCEBCD                 1      1
     D  SCPMCD                 2      2
     D  SCCSCD                 3      3
     D  SCPACD                 4      4
     D  SCSPCD                 5      5
     D  SCRCCD                 6      6
     D  SCFTCD                 7      7
     D  SCCCCD                 8      8
     D  SCCVCD                 9      9
     D  SCCLCD                10     10
     D  SCMCCD                11     11
     D  SCSSCD                12     12
     D  SCPSCD                13     13
     D  SCARCD                14     14
     D  SCACCD                15     15
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  CCKEY                  1     10
     D  XXCCST                 1      2
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  FS BASE CONTROL FILE
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 ***********************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*** BEGIN CHG *** 07/28/00 **************************************
     C**********           PARM           QQFLG  14
     C                   PARM                    QQFLG            15
     C*** END   CHG *** 07/28/00 **************************************
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQST              2
     C                   PARM                    QQSTNM           40
     C*** BEGIN ADD *** 08/15/00 ***********************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*** END   ADD *** 08/15/00 ***********************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVEL     QQST          XXST              2
     C                   MOVEL     QQSTNM        XXSTNM           40
     C                   MOVEA     QQFLG         FL
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZEROS        X                 2 0
     C*** BEGIN ADD *** 07/28/00 **************************************
     C     QQST          IFEQ      *BLANKS
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*** END   ADD *** 07/28/00 **************************************
     C*
     C*   SET UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   CHAIN TO THE COMPANY FILE.
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   CHECK TO SEE IF NETWORK/PPO INSTALLED SFBPF/FBFL06 = 'Y'
     C*   IF NOT INSTALLED DO NOT DISPLAY A/R TRANACTION CODE LISTING
     C*   OR ACTIVITY CODE LISTING OPTION ON SCREEN.
     C*
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** BEG ADD *** 06/29/98 **************************************
     C**********           SUBSTFBAL04:5  XXAR    1
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** BEGIN CHG *** 08/15/00 ***********************************************
     C********** XXAR      IFNE 'Y'
     C     ##AR          IFNE      'Y'
     C*** END   CHG *** 08/15/00 ***********************************************
     C********** FBFL06    IFNE 'Y'
     C                   MOVE      '1'           *IN99
     C                   ELSE
     C                   MOVE      '0'           *IN99
     C                   END                                                    END FBAL04
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C**********           SUBSTFBAL01:9  XXACT   1
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** BEGIN CHG *** 08/15/00 ***********************************************
     C********** XXACT     IFNE 'Y'
     C     ##MED         IFNE      'Y'
     C*** END   CHG *** 08/15/00 ***********************************************
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END                                                    END FBAL01
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C**********           END                             END *IN60
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** END ADD *** 06/29/98 **************************************
     C*** BEGIN ADD *** 09/11/00 **************************************
     C*
     C* SETON REPORT CONDITIONS FOR ROUTING SHEET
     C*
     C     FL(1)         IFNE      *BLANKS
     C                   SETON                                        21
     C                   ENDIF
     C*
     C     FL(2)         IFNE      *BLANKS
     C                   SETON                                        22
     C                   ENDIF
     C*
     C     FL(3)         IFNE      *BLANKS
     C                   SETON                                        23
     C                   ENDIF
     C*
     C     FL(4)         IFNE      *BLANKS
     C                   SETON                                        24
     C                   ENDIF
     C*
     C     FL(5)         IFNE      *BLANKS
     C                   SETON                                        25
     C                   ENDIF
     C*
     C     FL(6)         IFNE      *BLANKS
     C                   SETON                                        26
     C                   ENDIF
     C*
     C     FL(7)         IFNE      *BLANKS
     C                   SETON                                        27
     C                   ENDIF
     C*
     C     FL(8)         IFNE      *BLANKS
     C                   SETON                                        28
     C                   ENDIF
     C*
     C     FL(9)         IFNE      *BLANKS
     C                   SETON                                        29
     C                   ENDIF
     C*
     C     FL(10)        IFNE      *BLANKS
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     FL(11)        IFNE      *BLANKS
     C                   SETON                                        31
     C                   ENDIF
     C*
     C     FL(12)        IFNE      *BLANKS
     C                   SETON                                        32
     C                   ENDIF
     C*
     C     FL(13)        IFNE      *BLANKS
     C                   SETON                                        33
     C                   ENDIF
     C*
     C     FL(14)        IFNE      *BLANKS
     C                   SETON                                        34
     C                   ENDIF
     C*
     C     FL(15)        IFNE      *BLANKS
     C                   SETON                                        35
     C                   ENDIF
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDEQ     'A'
     C     Q1USER        CHAIN     PUP01                              60
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C*
     C*   PRINT THE REPORT FOR EOMB CODE  - Position 1
     C     FL(1)         IFNE      *BLANK
     C                   EXSR      PRTEB
     C                   END
     C*
     C*   PRINT THE REPORT FOR PROCEDURE CODE MODIFIER  - Position 2
     C     FL(2)         IFNE      *BLANK
     C                   EXSR      PRTPM
     C                   END
     C*
     C*   PRINT THE REPORT FOR CLAIMS STATUS CODE - Position 3
     C     FL(3)         IFNE      *BLANK
     C                   EXSR      PRTCS
     C                   END
     C*
     C*   PRINT THE REPORT FOR PART CODES - Position 4
     C     FL(4)         IFNE      *BLANK
     C                   EXSR      PRTPA
     C                   END
     C*
     C*   PRINT THE REPORT FOR SPECIALTY CODE - Position 5
     C     FL(5)         IFNE      *BLANK
     C                   EXSR      PRTSP
     C                   END
     C*
     C*   PRINT THE REPORT FOR REMARK CODE - Position 6
     C     FL(6)         IFNE      *BLANK
     C                   EXSR      PRTRC
     C                   END
     C*
     C*   Print the report for the Type of Facility Code - Position 7
     C     FL(7)         IFNE      *BLANK
     C                   EXSR      PRTFT
     C                   END
     C*
     C*   Print the report for the County Code - Position 8
     C     FL(8)         IFNE      *BLANK
     C                   EXSR      PRTCC
     C                   END
     C*
     C*   PRINT THE REPORT FOR COVERAGE CODE - Position 9
     C     FL(9)         IFNE      *BLANK
     C                   EXSR      PRTCV
     C                   END
     C*
     C*   PRINT THE REPORT FOR CLOSURE CODE - Position 10
     C     FL(10)        IFNE      *BLANK
     C                   EXSR      PRTCL
     C                   END
     C*
     C*   PRINT THE REPORT FOR MEDICAL SERVICES CODE - Position 11
     C     FL(11)        IFNE      *BLANK
     C                   EXSR      PRTMC
     C                   END
     C*
     C*   PRINT THE REPORT FOR MEDICAL SERVICES STATUS CD - Pos 12
     C     FL(12)        IFNE      *BLANK
     C                   EXSR      PRTSS
     C                   END
     C*
     C*** BEGIN ADD *** 07/24/95 ****************************************
     C*   PRINT THE REPORT FOR PLACE OF SERVICE CODE - POS. 13
     C     FL(13)        IFNE      *BLANK
     C                   EXSR      PRTPS
     C                   END
     C*** END ADD ***** 07/24/95 ****************************************
     C*
     C*   PRINT THE REPORT FOR A/R CODE - Position 13
     C     FL(14)        IFNE      *BLANK                                       CHG 07/24/95
     C                   EXSR      PRTAR
     C                   END
     C*
     C*   PRINT THE REPORT FOR ACTIVITY CODE - Position 14
     C     FL(15)        IFNE      *BLANK                                       CHG 07/24/95
     C                   EXSR      PRTAC
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey Por01;
       If %found and Orstat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*   END OF JOB.
     C*
     C     END           TAG
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     C*   PRINT THE ACTIVITY CODE TABLE                               *
     C*****************************************************************
     C     PRTAC         BEGSR
     C     XXCO#         CHAIN     PAT01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD15
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD15
     C                   END
     C                   EXCEPT    DETAC
     C     XXCO#         READE     PAT01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE A/R CODE TABLE                                    *
     C*****************************************************************
     C     PRTAR         BEGSR
     C     XXCO#         CHAIN     PAC01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD14
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD14
     C                   END
     C                   EXCEPT    DETAR
     C     XXCO#         READE     PAC01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   Print the County Code Table                                 *
     C*****************************************************************
     C     PRTCC         BEGSR
     C     XXCO#         CHAIN     PCC01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD8
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD8
     C                   END
     C     XXCCST        IFEQ      XXST
     C                   ADD       1             XXRCD
     C                   EXCEPT    DETCC
     C                   END
     C     XXCO#         READE     PCC01                                  58
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C     ENDCC         ENDSR
     C*****************************************************************
     C*   PRINT THE CLOSURE CODE TABLE                                *
     C*****************************************************************
     C     PRTCL         BEGSR
     C     XXCO#         CHAIN     PCL01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD10
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE CLOSURE CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD10
     C                   END
     C                   EXCEPT    DETCL
     C     XXCO#         READE     PCL01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE CLAIMS STATUS CODE TABLE                         *
     C*****************************************************************
     C     PRTCS         BEGSR
     C     XXCO#         CHAIN     PCS01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD3
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE CLAIMS STATUS CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD3
     C                   END
     C                   EXCEPT    DETCS
     C     XXCO#         READE     PCS01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE COVERAGE CODE TABLE                               *
     C*****************************************************************
     C     PRTCV         BEGSR
     C     XXCO#         CHAIN     PCV01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD9
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE COVERAGE CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD9
     C                   END
     C                   EXCEPT    DETCV
     C     XXCO#         READE     PCV01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE EOMB CODE TABLE                                   *
     C*****************************************************************
     C     PRTEB         BEGSR
     C     XXCO#         CHAIN     PEB01                              58
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*                    Z-ADD*ZERO     PAGE
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   END
     C                   EXCEPT    DETEB
     C     XXCO#         READE     PEB01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   Print the Facility Code Table                               *
     C*****************************************************************
     C     PRTFT         BEGSR
     C     XXCO#         CHAIN     PFT01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD7
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD7
     C                   END
     C                   EXCEPT    DETFT
     C     XXCO#         READE     PFT01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C     ENDFT         ENDSR
     C*****************************************************************
     C*   PRINT THE MEDICAL SERVICES CODE TABLE FILE                  *
     C*****************************************************************
     C     PRTMC         BEGSR
     C     XXCO#         CHAIN     PMC01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD11
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE MED SERVICES CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD11
     C                   END
     C                   EXCEPT    DETMC
     C     XXCO#         READE     PMC01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE PART CODE TABLE                                   *
     C*****************************************************************
     C     PRTPA         BEGSR
     C     XXCO#         CHAIN     PPA01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD4
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE CLAIMS STATUS CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD4
     C                   END
     C                   EXCEPT    DETPA
     C     XXCO#         READE     PPA01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE PROCEURE CODE MODIFIER TABLE                      *
     C*****************************************************************
     C     PRTPM         BEGSR
     C     XXCO#         CHAIN     PPM01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE PROCEDURE CODE MOD FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   END
     C                   EXCEPT    DETPM
     C     XXCO#         READE     PPM01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*** BEGIN ADD *** 07/24/95 **************************************
     C*****************************************************************
     C*   PRINT THE PLACE OF SERVICE CODE TABLE                      *
     C*****************************************************************
     C     PRTPS         BEGSR
     C*** BEGIN CHG *** 12/27/00 **************************************
     C********** XXCO#     CHAINPPS01                58
     C     XXCO#         CHAIN     PPS02                              58
     C*** END   CHG *** 12/27/00 **************************************
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD13
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE PLACE OF SERVICE CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD13
     C                   END
     C                   EXCEPT    DETPS
     C*** BEGIN CHG *** 12/27/00 **************************************
     C********** XXCO#     READEPPS01                    58
     C     XXCO#         READE     PPS02                                  58
     C*** END   CHG *** 12/27/00 **************************************
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*** END ADD ***** 07/24/95 **************************************
     C*****************************************************************
     C*   PRINT THE REMARK CODE TABLE                                 *
     C*****************************************************************
     C     PRTRC         BEGSR
     C     XXCO#         CHAIN     PRC01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD6
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD6
     C                   END
     C                   EXCEPT    DETRC
     C     XXCO#         READE     PRC01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE SPECIALTY CODE TABLE                              *
     C*****************************************************************
     C     PRTSP         BEGSR
     C     XXCO#         CHAIN     PSP01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD5
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD5
     C                   END
     C                   EXCEPT    DETSP
     C     XXCO#         READE     PSP01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
     C*   PRINT THE MEDICAL SERVICES STATUS CODE TABLE FILE           *
     C*****************************************************************
     C     PRTSS         BEGSR
     C     XXCO#         CHAIN     PSS01                              58
     C                   Z-ADD     *ZERO         PAGE
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD12
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN58         IFEQ      '0'
     C*
     C*** READ THE MED SERVICES STAT CODE FILE UNTIL CO# CHGS.
     C*
     C     *IN58         DOWEQ     '0'
     C   OF              DO
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD12
     C                   END
     C                   EXCEPT    DETSS
     C     XXCO#         READE     PSS01                                  58
     C                   ADD       1             XXRCD
     C                   END
     C                   END
     C*
     C*** PRINT THE TOTAL RECORDS PRINTED.
     C*
     C                   EXCEPT    TOTL
     C                   ENDSR
     C*****************************************************************
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
     O**********E            ROUTE          2
     O**********                                 23 'REPORT SELECTION:'
     O**********E            ROUTE          2
      ***** End Del ***** 08/26/14 *********************************************
     O*** BEGIN CHG *** 07/28/00 **************************************
     O**********                         23 'STATE:'
     O**********               XXST      26
     O**********               XXSTNM    68
     O              N16                          23 'STATE:'
     O              N16      XXST                26
     O              N16      XXSTNM              68
     O*** END   CHG *** 07/28/00 **************************************
     O*** BEGIN CHG *** 09/11/00 **************************************
     O********E* 1             ROUTE
     O**********                         23 'EOMB CODE:'
     O**********               FL,1      25
     O********E* 1             ROUTE
     O**********                         23 'PROCEDURE CD MODIFIER:'
     O**********               FL,2      25
     O********E* 1             ROUTE
     O**********                         23 'CLAIMS STATUS CODE:'
     O**********               FL,3      25
     O********E* 1             ROUTE
     O**********                         23 'PART CODE:'
     O**********               FL,4      25
     O********E* 1             ROUTE
     O**********                         23 'SPECIALTY CODE:'
     O**********               FL,5      25
     O********E* 1             ROUTE
     O**********                         23 'REMARK CODE:'
     O**********               FL,6      25
     O********E* 1             ROUTE
     O**********                         23 'FACILITY CODE:'
     O**********               FL,7      25
     O********E* 1             ROUTE
     O**********                         23 'COUNTY CODE:'
     O**********               FL,8      25
     O********E* 1             ROUTE
     O**********                         23 'COVERAGE CODE:'
     O**********               FL,9      25
     O********E* 1             ROUTE
     O**********                         23 'CLOSURE CODE:'
     O**********               FL,10     25
     O********E* 1             ROUTE
     O**********                         23 'MEDICAL SERVICES:'
     O**********               FL,11     25
     O********E* 1             ROUTE
     O**********                         23 'MED SERVICES STATUS:'
     O**********               FL,12     25
     O*** BEGIN ADD *** 07/24/95 ********************************************
     O********E* 1             ROUTE
     O**********                         23 'PLACE OF SERVICE CD:'
     O**********               FL,13     25
     O*** END ADD ***** 07/24/95 ********************************************
     O********E* 1    N99      ROUTE
     O**********      N99                23 'A/R CODE:'
     O**********      N99      FL,14     25
     O********E* 2    N98      ROUTE
     O**********      N98                23 'ACTIVITY CODE:'
     O**********      N98      FL,15     25
     O**********
     O          E    21      ROUTE          1
     O                                           41 'EOMB Codes              '
     O          E    22      ROUTE          1
     O                                           41 'Procedure Code Modifiers'
     O          E    23      ROUTE          1
     O                                           41 'Adjudication codes      '
     O          E    24      ROUTE          1
     O                                           41 'Body Part codes         '
     O          E    25      ROUTE          1
     O                                           41 'Specialty codes         '
     O          E    26      ROUTE          1
     O                                           41 'Remark codes            '
     O          E    27      ROUTE          1
     O                                           41 'Type of Facility Codes  '
     O          E    28      ROUTE          1
     O                                           41 'County codes            '
     O          E    29      ROUTE          1
     O                                           41 'Coverage codes          '
     O          E    30      ROUTE          1
     O                                           41 'Closure codes           '
     O          E    31      ROUTE          1
     O                                           41 'Case Management Services'
     O                                           47 ' Codes'
     O          E    32      ROUTE          1
     O                                           41 'Med Services Status     '
     O          E    33      ROUTE          1
     O                                           41 'Place of Service Codes  '
     O          E   N99 34   ROUTE          1
     O              N99                          41 'A/R Transaction codes   '
     O          E   N98 35   ROUTE          2
     O              N98                          41 'T/M Activity codes      '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                58 'INC. 2000       '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    122
     O          E            HEAD           3
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          116 'PAGE'
     O                       PAGE          Z    122
     O          E            HEAD1          0
     O                                           69 'EOMB CODE'
     O          E            HEAD1          1
     O                                           69 'EOMB CODE'
     O          E            HEAD1          2
     O                       XXST                62
     O                       XXSTNM             104
     O          E            HEAD1          2
     O                                           11 'CODE'
     O                                           46 'D E S C R I P T I O N'
     O                                          114 'STATUS'
     O          E            DETEB          1
     O                       EBKEY               18
     O                       EBDESC              96
     O                       EBSTAT             111
     O*****************************************************************
     O          E            HEAD2          0
     O                                           76 'PROCEDURE CODE MODIFIER'
     O          E            HEAD2          1
     O                                           76 'PROCEDURE CODE MODIFIER'
     O          E            HEAD2          2
     O                       XXST                62
     O                       XXSTNM             104
     O          E            HEAD2          1
     O                                           82 'PROCEDURE'
     O                                           97 'ASSISTING'
     O                                          111 'PROFESSIONAL'
     O          E            HEAD2          2
     O                                           13 'CODE'
     O                                           44 'D E S C R I P T I O N'
     O                                           83 'MODIFIER %'
     O                                           96 'SURGEON'
     O                                          109 'COMPONENT'
     O                                          120 'STATUS'
     O          E            DETPM          1
     O                       PMKEY               20
     O                       PMDESC              63
     O                       PMPERC        J     81
     O                       PMSURG              93
     O                       PMPROF             105
     O                       PMSTAT             117
     O*****************************************************************
     O          E            HEAD3          0
     O*** BEGIN CHG *** 10/27/00 **************************************
     O**********                         75 'CLAIMS STATUS CODE'
     O                                           70 'ADJUDICATION/CLAIMS'
     O                                           82 'STATUS CODE'
     O*** END   CHG *** 10/27/00 **************************************
     O          E            HEAD3          2
     O*** BEGIN CHG *** 10/27/00 **************************************
     O**********                         75 'CLAIMS STATUS CODE'
     O                                           70 'ADJUDICATION/CLAIMS'
     O                                           82 'STATUS CODE'
     O*** END   CHG *** 10/27/00 **************************************
     O          E            HEAD3          1
     O                                           79 'DISPOSITION'
     O                                           95 '# OF DAYS'
     O                                          109 'OVERRIDE'
     O          E            HEAD3          2
     O                                           13 'CODE'
     O                                           45 'D E S C R I P T I O N'
     O                                           76 'CODE'
     O                                           95 'FOR DIARY'
     O                                          107 'FLAG'
     O                                          123 'STATUS'
     O          E            DETCS          1
     O                       CSKEY               20
     O                       CSDESC              64
     O                       CSDISC              75
     O                       CSDIDY        Z     92
     O                       CSOVRD             106
     O                       CSSTAT             121
     O*****************************************************************
     O          E            HEAD4          0
     O                                           75 '    PART CODE     '
     O          E            HEAD4          1
     O                                           75 '    PART CODE     '
     O          E            HEAD4          2
     O                       XXST                62
     O                       XXSTNM             104
     O          E            HEAD4          1
     O                                           16 'PART'
     O          E            HEAD4          2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                           71 'STATUS'
     O          E            DETPA          1
     O                       PAKEY               23
     O                       PADESC              60
     O                       PASTAT              68
     O*****************************************************************
     O          E            HEAD5          0
     O                                           72 'SPECIALTY CODE'
     O          E            HEAD5          2
     O                                           72 'SPECIALTY CODE'
     O          E            HEAD5          2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                           70 'STATUS'
     O          E            DETSP          1
     O                       SPKEY               23
     O                       SPDESC              60
     O                       SPSTAT              67
     O*****************************************************************
     O          E            HEAD6          0
     O                                           72 ' REMARK CODES '
     O          E            HEAD6          2
     O                                           72 ' REMARK CODES '
     O          E            HEAD6          2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                           70 'STATUS'
     O          E            DETRC          1
     O                       RCKEY               23
     O                       RCDESC              60
     O                       RCSTAT              67
     O*****************************************************************
     O          E            HEAD7          0
     O                                           72 ' FACILITY CODE '
     O          E            HEAD7          2
     O                                           72 ' FACILITY CODE '
     O*** BEGIN ADD *** 10/27/00 **************************************
     O          E            HEAD7          2
     O                                           18 'STATE:'
     O                       XXST              +001
     O                       XXSTNM            +001
     O*** END   ADD *** 10/27/00 **************************************
     O          E            HEAD7          2
     O                                           16 'CODE'
     O                                           28 'SPEC'
     O                                           59 'D E S C R I P T I O N'
     O                                           78 'STATUS'
     O          E            DETFT          1
     O                       FTKEY               23
     O                       FTSPCD              28
     O                       FTDESC              75
     O                       FTSTAT              82
     O*****************************************************************
     O          E            HEAD8          0
     O                                           72 '   COUNTY CODE '
     O          E            HEAD8          1
     O                                           72 '   COUNTY CODE '
     O          E            HEAD8          2
     O                       XXST                62
     O                       XXSTNM             104
     O          E            HEAD8          2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                           70 'STATUS'
     O          E            DETCC          1
     O                       CCKEY               23
     O                       CCDESC              70
     O                       CCSTAT              77
     O*****************************************************************
     O          E            HEAD9          0
     O                                           72 ' COVERAGE CODES'
     O          E            HEAD9          2
     O                                           72 ' COVERAGE CODES'
     O          E            HEAD9          2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                          100 'STATUS'
     O          E            DETCV          1
     O                       CVKEY               23
     O                       CVDESC              90
     O                       CVSTAT              97
     O*****************************************************************
     O          E            HEAD10         0
     O                                           72 ' CLOSURE CODES'
     O          E            HEAD10         2
     O                                           72 ' CLOSURE CODES'
     O          E            HEAD10         2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                          100 'STATUS'
     O          E            DETCL          1
     O                       CLKEY               23
     O                       CLDESC              90
     O                       CLSTAT              97
     O*****************************************************************
     O          E            HEAD11         0
     O                                           74 ' MEDICAL SERVICES CODES'
     O          E            HEAD11         2
     O                                           74 ' MEDICAL SERVICES CODES'
     O          E            HEAD11         2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                          100 'STATUS'
     O                                          117 'EXT CASE MGT'
     O          E            DETMC          1
     O                       MCKEY               23
     O                       MCDESC              90
     O                       MCSTAT              97
     O                       MCEXCM             112
     O*****************************************************************
     O          E            HEAD12         0
     O                                           72 ' MEDICAL SERVICES STATUS'
     O                                           78 ' CODES'
     O          E            HEAD12         2
     O                                           72 ' MEDICAL SERVICES STATUS'
     O                                           78 ' CODES'
     O          E            HEAD12         2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                          100 'STATUS'
     O                                          108 'TYPE'
     O          E            DETSS          1
     O                       SSKEY               23
     O                       SSDESC              90
     O                       SSSTAT              97
     O                       SSTYPE             107
     O*
     O*** BEGIN ADD *** 07/24/95 **************************************
     O*****************************************************************
     O          E            HEAD13         0
     O                                           72 ' PLACE OF SERVICE CODES'
     O          E            HEAD13         2
     O                                           72 ' PLACE OF SERVICE CODES'
     O*** BEGIN ADD *** 10/27/00 **************************************
     O          E            HEAD13         2
     O                                           18 'STATE:'
     O                       XXST              +001
     O                       XXSTNM            +001
     O*** END   ADD *** 10/27/00 **************************************
     O          E            HEAD13         1
     O                                          108 'OLD '
     O          E            HEAD13         2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                          100 'STATUS'
     O                                          108 'CODE'
     O          E            DETPS          1
     O                       PSPOS               15
     O                       PSDESC              70
     O                       PSSTAT              97
     O                       PSKEY              114
     O*** END ADD ***** 07/24/95 **************************************
     O*****************************************************************
     O          E            HEAD14         0
     O                                           72 '   A/R CODES  '
     O          E            HEAD14         2
     O                                           72 '   A/R CODES  '
     O          E            HEAD14         2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                           70 'STATUS'
     O          E            DETAR          1
     O                       ACKEY               23
     O                       ACDESC              60
     O                       ACSTAT              67
     O*****************************************************************
     O          E            HEAD15         0
     O                                           72 ' ACTIVITY CODE '
     O          E            HEAD15         2
     O                                           72 ' ACTIVITY CODE '
     O          E            HEAD15         2
     O                                           16 'CODE'
     O                                           51 'D E S C R I P T I O N'
     O                                           70 'STATUS'
     O          E            DETAC          1
     O                       ATKEY               23
     O                       ATDESC              60
     O                       ATSTAT              67
     O*****************************************************************
     O          E            TOTL        2
     O                       XXRCD         Z     54
     O                                           69 'T O T A L    '
     O                                           82 'R E C O R D S'
     O          E            TOTL
     O                       XXRCD         Z     54
     O                                           69 'T O T A L    '
     O                                           82 'R E C O R D S'
