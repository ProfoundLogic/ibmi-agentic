     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W7371R - PAYROLL BY JOB CLASS CODES WITH PREMIUM
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 10/05/10  C:189 PROJECT WC1776                                *
     H*                 Modify the code to show decimal places for    *
     H*                 payroll amounts                               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 05/28/08  C:65   PROJECT: SUPP#63395                          *
     H*                  EXPAND FINAL TOTAL FIELDS TO BILLIONS TO     *
     H*                  HANDLE LARGE PAYROLL AMOUNTS.                *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use the new logical(s) for editing class       *
     H*                code information.                              *
     H*                                                               *
     H* 01/19/00  C:118  SUPPORT: WC102/21332                         *
     H*                  REVERSE CHGS MADE ON 7/2, EXCEPT DO NOT      *
     H*                  REPORT WRITTEN PREMIUM IN FINAL TOTALS.      *
     H*                                                               *
     H* 07/02/99  C:35   PROJECT: SUPPORT WA102 #18653                *
     H*                  REMOVED FINAL TOTALS, SINCE THEY WERE        *
     H*                  INCLUDING A GIVEN EMPLOYER'S PREMIUM         *
     H*                  MULTIPLE TIMES (ONCE FOR EACH CLASS CODE).   *
     H*                                                               *
     H*  11/04/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT PAYROLL AMOUNTS WITH DOLLARS ONLY.*
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS, AND CENTER NAME.   *
     H*                                                               *
     H*****************************************************************
     FWMPRL     IF   E           K DISK
     F* * * Payroll Classification File
     F*
     FWMPRL1    IF   E           K DISK
     F                                     RENAME(PPR01:PPR02)
     F* * * Payroll Classification File
     F*
     F*** BEGIN CHG *** 04/20/09 *******************************
     F************WMEPL   IF  E           K        DISK
     F* * * Employer Payroll File (CO#,FND,EMP#,DIV,FYR,EFDT,CLAS)
     F*
     F************WMEPL4  IF  E           K        DISK
     F************  PEP01                             KRENAMEPEP02
     FWMEPLE    IF   E           K DISK
     F* * * Employer Payroll File (CO#,FND,EMP#,DIV,FYR,EFDT,CLAS)
     FWMEPL4E   IF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     F*** END   CHG *** 04/20/09 *******************************
     F*** BEGIN ADD *** 04/20/09 *******************************
     FWMPXL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F***** END   ADD **** 04/20/09 ***********************************
     F* * * Employer Payroll File (CO#,FND,FYR,CLAS,SPEC,EMP#,DIV,EFDT)
     F*
     FWMEML     IF   E           K DISK
     F* * * Employer Master File
     F*
     FWDELL     IF   E           K DISK
     F* * * Employer Detail File
     F*
     FWMFNL     IF   E           K DISK
     F* * * Fund File
     F*
     FWMAHL     IF   E           K DISK
     F* * * Agent History File
     F*
     FWMAAL1    IF   E           K DISK
     F* * * Agent Assignment File
     F*
     FWMAGL     IF   E           K DISK
     F* * * Agent Master File
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     F* * * User Parameter File
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F* * * Output File
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D*** BEGIN CHG *** 04/20/09 *******************************
     D*************                           1  24 DSDES1
     D  DSDES1                 1     19
     D*** END   CHG *** 04/20/09 *******************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C     PRKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C     PRKEY3        KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEFDT
     C     PRKEY4        KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    EPCLAS
     C*** BEGIN ADD *** 04/20/09 *******************************
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST              2
     C                   KFLD                    EPEFDT
     C*
     C     PXKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C*
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C***** END   ADD **** 04/20/09 ***********************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR             3 0
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     EPKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC            1
     C*** BEGIN ADD *** 04/20/09 *******************************
     C                   KFLD                    XXEXT             5
     C*** END   ADD *** 04/20/09 *******************************
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     EPKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCLAS            5 0
     C     EPKEY4        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C*** BEGIN ADD *** 04/20/09 *******************************
     C                   KFLD                    XXEXT             5
     C*** END   ADD *** 04/20/09 *******************************
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     *LIKE         DEFINE    AHDT          XXEFDT
     C     *LIKE         DEFINE    AHDT          XXYMD
     C     *LIKE         DEFINE    PRDESC        XXDESC
     C     *LIKE         DEFINE    AHEMP#        XXEMP#
     C     *LIKE         DEFINE    AHDIV         XXDIV
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDATE            8
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQDATE        XXDATE            8 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C                   EXSR      SR1ST
     C*
     C*  GET FUND MASTER FOR FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*** BEGIN ADD *** 10/27/97 *******************************
     C     *IN60         IFEQ      '0'
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C*** END ADD ***** 10/27/97 ********************************
     C*
     C*  CHAIN TO PAYROLL CLASS FILE TO GET FIRST CLASS WITH CO#
     C*
     C*          PRKEY  --------- CO#
     C     PRKEY         CHAIN     PPR01                              50
     C                   Z-ADD     PRCLAS        SVCLAS
     C     *IN50         DOWEQ     '0'
     C*
     C*  READ EMPLOYER PAYROLL RECORDS FOR CLASS FOUND IN PREVIOUS STEP
     C*
     C                   Z-ADD     PRCLAS        XXCLAS
     C*          EPKEY3 ----------- CO#, FND, FYR, CLAS
     C     EPKEY3        CHAIN     PEP02                              51
     C     *IN51         DOWEQ     '0'
     C                   Z-ADD     EPEMP#        XXEMP#
     C                   Z-ADD     EPDIV         XXDIV
     C                   Z-ADD     XXYMD         XXEFDT
     C                   MOVE      EPSPEC        XXSPEC            1
     C*** BEGIN ADD *** 04/20/09 *******************************
     C                   MOVEL     EPEXT         XXEXT             5
     C*** END   ADD *** 04/20/09 *******************************
     C*
     C*  GET ALL CURRENTLY EFFECTIVE PAYROLL CLASS RECORDS FOR EMPLOYER
     C*
     C*          EPKEY2 -----CO#, FND, FYR, CLAS, SPEC, EMP#, DIV
     C     EPKEY2        CHAIN     PEP02                              57
     C     *IN57         IFEQ      '0'
     C*
     C                   Z-ADD     0             XXEAPR           11 2
     C                   Z-ADD     0             XXEMPR           11 2
     C                   Z-ADD     0             XXAAPR           11 2
     C                   Z-ADD     0             XXAMPR           11 2
     C     *IN57         DOWEQ     '0'
     C                   ADD       EPEAPR        XXEAPR
     C                   ADD       EPEMPR        XXEMPR
     C                   ADD       EPAAPR        XXAAPR
     C                   ADD       EPAMPR        XXAMPR
     C     EPKEY2        READE     PEP02                                  57
     C                   END
     C*
     C*  GET EMPLOYER RECORD
     C*
     C                   MOVE      *BLANKS       ENAME
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  GET EMPLOYER DETAIL RECORD
     C*
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C     ELFAF         COMP      'A'                                    78     JJJ 7-3-89
     C**
     C**  EXECUTE SUBROUTINE TO CALCULATE MINIMUMS AND MAXIMUMS
     C                   EXSR      SRMNMX
     C*
     C                   END
     C                   END
     C**
     C**  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C                   MOVE      *BLANKS       ANAME            23
     C                   Z-ADD     XXYMD         XXEFDT
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  80
     C     *IN80         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  80
     C                   END
     C*
     C     *IN80         IFEQ      '0'
     C     AHCO#         ANDEQ     XXCO#
     C     AHFND         ANDEQ     XXFND
     C     AHEMP#        ANDEQ     XXEMP#
     C     AHDIV         ANDEQ     XXDIV
     C     AHFYR         ANDEQ     XXFYR
     C*
     C                   Z-ADD     AHAGT#        XXAGT#
     C**
     C     AAKEY         CHAIN     PAA01                              80
     C     *IN80         IFEQ      '0'
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
     C**
     C     AGKEY         CHAIN     PAG01                              80
     C     *IN80         IFEQ      '0'
     C                   MOVEL     AGNAME        ANAME            23
     C                   END
     C*
     C                   END
     C                   END
     C     PRKEY3        SETGT     PPR02
     C     PRKEY4        READPE    PPR02                                  60
     C*** BEGIN ADD *** 04/20/09 *******************************
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       PRDESC
     C                   MOVE      *BLANKS       XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL     PXDESC        PRDESC
     C     RSKY          CHAIN     PRS01                              61
     C  N61              MOVE      RSJRST        XXST
     C  N61PXKY          SETGT     PPX01
     C  N61PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL     PXDESC        XXDESC
     C                   ENDIF                                                  EPEXT IFNE
     C     EPEXT         IFEQ      *BLANKS
     C                   MOVEL     PRDESC        XXDESC
     C                   ENDIF                                                  EPEXT IFEQ
     C***  END   ADD  ***  04/20/09  *********************************
     C*** BEGIN DLT *** 04/20/09 *******************************
     C*********  *IN60     IFEQ '0'
     C*********            MOVELPRDESC    XXDESC
     C*********            ELSE
     C*********            MOVE *BLANKS   XXDESC
     C*********            ENDIF                           *IN60 EQ '0'
     C*
     C***  END   DLT  ***  04/20/09  *********************************
     C                   MOVEL     XXDESC        DSDES1
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C*** begin chg ***  10/05/10  ****************************************
     C****************  N78                Z-ADDXXEAPR    XXEAP2  90H
     C****************   78                Z-ADDXXAAPR    XXAAP2  90H
     C  N78              Z-ADD     XXEAPR        XXEAP2           11 2
     C   78              Z-ADD     XXAAPR        XXAAP2           11 2
     C*** end   chg ***  10/05/10  ****************************************
     C                   SETOFF                                       151617
     C     ELEXM1        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN15
     C                   ENDIF
     C     ELEXM2        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN16
     C                   ENDIF
     C     ELEXM3        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN17
     C                   ENDIF
     C                   EXCEPT    DETAIL
     C**
     C*  DETERMINE IF ANY MORE CLASSES FOR THIS EMPLOYER
     C*
     C                   MOVEA     '00'          *IN(52)                        SETOF 52-53
     C*          EPKEY  ----------- CO#, FND, EMP#, DIV, FYR
     C     EPKEY         SETLL     PEP01
     C     EPKEY         READE     PEP01                                  53
     C     *IN53         DOWEQ     '0'
     C     EPCLAS        IFNE      XXCLAS
     C                   MOVE      '0'           *IN52
     C                   MOVE      '1'           *IN53
     C                   ELSE
     C                   MOVE      '1'           *IN52
     C                   END
     C     *IN53         IFEQ      '0'
     C     EPKEY         READE     PEP01                                  53
     C                   END
     C                   END
     C*
     C*  OUTPUT SPECIAL LINE FOR EMPLOYER IF ADD'L CLASSES NOT FOUND
     C*
     C     *IN52         IFEQ      '1'
     C                   EXCEPT    SPEC
     C                   END
     C                   END
     C*
     C*  GET NEXT EMPLOYER IN EMPLOYER PAYROLL FILE
     C*
     C*          EPKEY3 ---------- CO#, FND, FYR, CLAS
     C                   MOVEA     '00'          *IN(54)                        SETOF 54-55
     C     EPKEY4        SETGT     PEP02
     C     EPKEY3        READE     PEP02                                  54
     C     *IN54         DOWEQ     '0'
     C     EPSPEC        IFNE      XXSPEC
     C*** BEGIN ADD *** 04/20/09 *******************************
     C     EPEXT         ORNE      XXEXT
     C*** END   ADD *** 04/20/09 *******************************
     C                   EXSR      SUBL1
     C                   MOVEA     '11'          *IN(54)
     C                   ELSE
     C     EPEMP#        IFNE      XXEMP#
     C     EPDIV         ORNE      XXDIV
     C                   MOVEA     '11'          *IN(54)                        SETON 54-55
     C                   ELSE
     C                   MOVE      '0'           *IN55                          NO OTHER EMP
     C                   END
     C                   END
     C     *IN54         IFEQ      '0'
     C     EPKEY3        READE     PEP02                                  54
     C                   END
     C                   END
     C     *IN55         IFEQ      '0'
     C                   MOVE      '1'           *IN51
     C                   END
     C                   END
     C*  SETGT LAST PRCLAS PROCESSED AND READ
     C*  NEXT RECORD FOR COMPANY BEING PROCESSED
     C*          PRKEY2 --------- CO#, CLAS
     C     PRKEY2        SETGT     PPR01
     C     PRKEY         READE     PPR01                                  50
     C     PRCO#         IFNE      XXCO#
     C                   MOVE      '1'           *IN50
     C                   END
     C**
     C*  DETERMINE IF LEVEL BREAK PROCESSING SHOULD BE EXECUTED
     C*  * *  JOB CLASS TOTALS ARE PRINTED HERE IF CRITERIA MET  * *
     C     SVCLAS        IFNE      *ZERO
     C     PRCLAS        ANDNE     SVCLAS
     C     COUNT1        ANDNE     *ZERO
     C                   EXSR      SUBL1
     C                   END
     C                   Z-ADD     PRCLAS        SVCLAS            5 0
     C*
     C                   END
     C**
     C                   MOVE      '1'           *INLR
     C**
     CLR                 EXSR      SUBL1
     C***** BEGIN DEL **** 01/19/00 ***********************************
      **** BEGIN CHG **** 07/02/99 *********************************************
     C**********LR         EXSR SUBLR
     C*LR******* LRCT      IFEQ 0
     C*LR*******           EXCPTDETL2
     C*LR*******           ENDIF                           LRCT IFEQ 0
      **** END   CHG **** 07/02/99 *********************************************
     C***** END   DEL **** 01/19/00 ***********************************
     C*
     C***** BEGIN ADD **** 01/19/00 ***********************************
     CLR                 EXSR      SUBLR
     C***** END   ADD **** 01/19/00 ***********************************
      ***** Beg Add ***** 08/26/14 ***********************************
      /Free
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
      ***** End Add ***** 08/26/14 ***********************************
     C*
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C     UPKEY         CHAIN     WMUPL                              60
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *************************************
     C********           MOVE      '0'           XXPRT
     C********      ORKEY         CHAIN     POR01                              60
     C********      *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C********           EXCEPT    ROUTE
     C********           END
      ***** End Del ***** 08/26/14 *************************************
     C*
     C                   Z-ADD     *HIVAL        MNEAPR           11 2
     C                   Z-ADD     *HIVAL        MNANPR           11 2
     C                   Z-ADD     *HIVAL        MNEMPR           11 2
     C                   Z-ADD     *HIVAL        MNEXM
     C*
     C                   Z-ADD     *HIVAL        MNEALR           11 2
     C                   Z-ADD     *HIVAL        MNANLR           11 2
     C                   Z-ADD     *HIVAL        MNEMLR           11 2
     C                   Z-ADD     *HIVAL        MNEXLR            5 4
     C*
     C                   Z-ADD     *ZERO         MXEALR           11 2
     C                   Z-ADD     *ZERO         MXANLR           11 2
     C                   Z-ADD     *ZERO         MXEMLR           11 2
     C                   Z-ADD     *ZERO         MXEXLR            5 4
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   Minimum/Maximum Calculations   ** Subroutine **   S R M N M X  *
     C*==================================================================*
     C*
     CSR   SRMNMX        BEGSR
     C*
     C                   ADD       1             COUNT1            5 0
     C*
     C     ELFAF         IFEQ      'A'                                          JJJ 7-3-89
     C*                                                    |
     C     XXAAPR        IFLT      MNEAPR                                       |
     C                   Z-ADD     XXAAPR        MNEAPR                         |
     C                   END                                                    |
     C*                                                    |
     C     XXAAPR        IFGT      MXEAPR                                       |
     C                   Z-ADD     XXAAPR        MXEAPR                         |
     C                   END                                                    |
     C*                                                    |
     C     XXAMPR        IFLT      MNEMPR                                       |
     C                   Z-ADD     XXAMPR        MNEMPR                         |
     C                   END                                                    |
     C*                                                    |
     C     XXAMPR        IFGT      MXEMPR                                       |
     C                   Z-ADD     XXAMPR        MXEMPR                         |
     C                   END                                                    |
     C*                                                    |
     C                   ELSE                                                   |
     C*
     C     XXEAPR        IFLT      MNEAPR
     C                   Z-ADD     XXEAPR        MNEAPR
     C                   END
     C*
     C     XXEAPR        IFGT      MXEAPR
     C                   Z-ADD     XXEAPR        MXEAPR
     C                   END
     C*
     C     XXEMPR        IFLT      MNEMPR
     C                   Z-ADD     XXEMPR        MNEMPR
     C                   END
     C*
     C     XXEMPR        IFGT      MXEMPR
     C                   Z-ADD     XXEMPR        MXEMPR
     C                   END
     C*
     C                   END                                                    JJJ 7-3-89
     C*                                                    |
     C   78              ADD       XXAAPR        L1EAPR           13 2
     C   78              ADD       XXAMPR        L1EMPR           11 2          |
     C  N78              ADD       XXEAPR        L1EAPR                         |
     C  N78              ADD       XXEMPR        L1EMPR                         |
     C*
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD1            6 0
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD2            6 0
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEMD3            6 0
     C*
     C                   MOVEL     EMNAME        ENAME            19
     C*
     C     ELFAF         IFEQ      'A'                                          JJJ 7-3-89
     C*                                                    |
     C     ELAAPR        IFLT      MNANPR                                       |
     C                   Z-ADD     ELAAPR        MNANPR                         |
     C                   END                                                    |
     C*                                                    |
     C     ELAAPR        IFGT      MXANPR                                       |
     C                   Z-ADD     ELAAPR        MXANPR                         |
     C                   END                                                    |
     C*                                                    |
     C                   ELSE                                                   |
     C*
     C     ELANPR        IFLT      MNANPR
     C                   Z-ADD     ELANPR        MNANPR
     C                   END
     C*
     C     ELANPR        IFGT      MXANPR
     C                   Z-ADD     ELANPR        MXANPR
     C                   END
     C*
     C                   END                                                    JJJ 7-3-89
     C*                                                    |
     C   78              ADD       ELAAPR        L1ANPR           11 2          |
     C  N78              ADD       ELANPR        L1ANPR           11 2          |
     C                   ADD       ELEXM1        L1EXM             7 2
     C                   ADD       ELEXM2        L1EXM
     C                   ADD       ELEXM3        L1EXM
     C*
     C     ELEXM1        IFNE      *ZERO
     C                   ADD       1             COUNTE            5 0
     C                   END
     C*
     C     ELEXM2        IFNE      *ZERO
     C                   ADD       1             COUNTE            5 0
     C                   END
     C*
     C     ELEXM3        IFNE      *ZERO
     C                   ADD       1             COUNTE            5 0
     C                   END
     C*
     C     ELEXM1        IFNE      *ZERO
     C     ELEXM1        ANDLT     MNEXM
     C                   Z-ADD     ELEXM1        MNEXM             5 4
     C                   END
     C     ELEXM2        IFNE      *ZERO
     C     ELEXM2        ANDLT     MNEXM
     C                   Z-ADD     ELEXM2        MNEXM
     C                   END
     C     ELEXM3        IFNE      *ZERO
     C     ELEXM3        ANDLT     MNEXM
     C                   Z-ADD     ELEXM3        MNEXM
     C                   END
     C*
     C     ELEXM1        IFGT      MXEXM
     C                   Z-ADD     ELEXM1        MXEXM             5 4
     C                   END
     C     ELEXM2        IFGT      MXEXM
     C                   Z-ADD     ELEXM2        MXEXM
     C                   END
     C     ELEXM3        IFGT      MXEXM
     C                   Z-ADD     ELEXM3        MXEXM
     C                   END
     C                   ENDSR
     C*
     C*==================================================================*
     C*   L1 Calculations (PR Class)       ** Subroutine **   S U B L 1  *
     C*==================================================================*
     C*
     CSR   SUBL1         BEGSR
     C*
     C                   Z-ADD     *ZERO         AVEAPR           11 2
     C                   Z-ADD     *ZERO         AVANPR           11 2
     C                   Z-ADD     *ZERO         AVEMPR           11 2
     C*
     C*  CALCULATE AVERAGES, MINIMUMS, MAXIMUMS
     C*
     C     COUNT1        IFNE      *ZERO
     C     L1ANPR        DIV(H)    COUNT1        AVANPR           11 2
     C     L1EAPR        DIV(H)    COUNT1        AVEAPR           11 2
     C     L1EMPR        DIV(H)    COUNT1        AVEMPR           11 2
     C                   ELSE
     C                   Z-ADD     *ZERO         AVANPR
     C                   Z-ADD     *ZERO         AVEAPR
     C                   Z-ADD     *ZERO         AVEMPR
     C                   END
     C**
     C     COUNTE        IFNE      *ZERO
     C     L1EXM         DIV(H)    COUNTE        AVEXM             5 4
     C                   ELSE
     C                   Z-ADD     *ZERO         AVEXM
     C                   END
     C**
     C     MNEAPR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEAPR
     C                   END
     C     MNANPR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNANPR
     C                   END
     C     MNEMPR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEMPR
     C                   END
     C     MNEXM         IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEXM
     C                   END
     C*
     C                   ADD       L1ANPR        LRANPR           11 2
     C                   ADD       L1EXM         LREXM             7 2
     C*
     C                   ADD       L1EAPR        LREAPR           13 2
     O*** BEGIN CHG ***  05/28/08  *********************************************
     C*************        ADD  L1EMPR    LREMPR 112
     C                   ADD       L1EMPR        LREMPR           13 2
     O*** END CHG *****  05/28/08  *********************************************
     C*** BEGIN ADD *** 11/04/97 *******************************
     C*** begin chg ***  10/05/10  ****************************************
     C**************                     Z-ADDL1EAPR    L1EAP1  90H
     C**************                     Z-ADDAVEAPR    AVEAP1  90H
     C                   Z-ADD     L1EAPR        L1EAP1           11 2
     C                   Z-ADD     AVEAPR        AVEAP1           11 2
     C*** end   chg ***  10/05/10  ****************************************
     C*** END ADD ***** 11/04/97 *******************************
     C                   ADD       COUNT1        LRCT              5 0
     C                   ADD       COUNTE        LRCTE             5 0
     C*
     C     MNANPR        IFNE      *ZERO
     C     MNANPR        ANDLT     MNANLR
     C                   Z-ADD     MNANPR        MNANLR
     C                   END
     C*
     C     MXANPR        IFGT      MXANLR
     C                   Z-ADD     MXANPR        MXANLR
     C                   END
     C*
     C     MNEAPR        IFNE      *ZERO
     C     MNEAPR        ANDLT     MNEALR
     C                   Z-ADD     MNEAPR        MNEALR
     C                   END
     C*
     C     MXEAPR        IFGT      MXEALR
     C                   Z-ADD     MXEAPR        MXEALR
     C                   END
     C*
     C     MNEMPR        IFNE      *ZERO
     C     MNEMPR        ANDLT     MNEMLR
     C                   Z-ADD     MNEMPR        MNEMLR
     C                   END
     C*
     C     MXEMPR        IFGT      MXEMLR
     C                   Z-ADD     MXEMPR        MXEMLR
     C                   END
     C*
     C     MNEXM         IFNE      *ZERO
     C     MNEXM         ANDLT     MNEXLR
     C                   Z-ADD     MNEXM         MNEXLR
     C                   END
     C*
     C     MXEXM         IFGT      MXEXLR
     C                   Z-ADD     MXEXM         MXEXLR
     C                   END
     C*
     C*  OUTPUT TOTALS
     C*
     C     COUNT1        IFNE      *ZERO
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C*** BEGIN ADD *** 11/04/97 *******************************
     C                   Z-ADD(H)  MNEAPR        MNEAP1            9 0
     C                   Z-ADD(H)  MXEAPR        MXEAP1            9 0
     C*** END ADD ***** 11/04/97 *******************************
     C                   EXCEPT    TOT
     C                   END
     C*
     C*** BEGIN ADD *** 11/04/97 *******************************
     C                   Z-ADD     *ZERO         L1EAPR
     C*** END ADD ***** 11/04/97 *******************************
     C                   Z-ADD     *ZERO         L1EXM
     C                   Z-ADD     *ZERO         COUNTE
     C                   Z-ADD     *HIVAL        MNEAPR
     C                   Z-ADD     *HIVAL        MNANPR
     C                   Z-ADD     *HIVAL        MNEMPR
     C                   Z-ADD     *HIVAL        MNEXM
     C**
     C                   Z-ADD     *ZERO         MXEAPR           11 2
     C                   Z-ADD     *ZERO         MXANPR           11 2
     C                   Z-ADD     *ZERO         MXEMPR           11 2
     C                   Z-ADD     *ZERO         MXEXM
     C**
     C                   ENDSR
     C*
      **** BEGIN DEL **** 07/02/99 *********************************************
     C*==================================================================*
     C*   LR Calculations                  ** Subroutine **   S U B L R  *
     C*==================================================================*
     C**********
     C********** SUBLR     BEGSR
     C**********
     C**********           Z-ADD*ZERO     AVEAPR 112
     C**********           Z-ADD*ZERO     AVANPR 112
     C**********           Z-ADD*ZERO     AVEMPR 112
     C**********
     C** CALCULATE AVERAGES, MINIMUMS, MAXIMUMS
     C**********
     C********** LRCT      IFNE *ZERO
     C********** LRANPR    DIV  LRCT      AVANPR 112H
     C********** LREAPR    DIV  LRCT      AVEAPR 112H
     C********** LREMPR    DIV  LRCT      AVEMPR 112H
     C**********           ELSE
     C**********           Z-ADD*ZERO     AVANPR
     C**********           Z-ADD*ZERO     AVEAPR
     C**********           Z-ADD*ZERO     AVEMPR
     C**********           END
     C**********
     C********** LRCTE     IFNE *ZERO
     C********** LREXM     DIV  LRCTE     AVEXM   54H
     C**********           ELSE
     C**********           Z-ADD*ZERO     AVEXM
     C**********           END
     C**********
     C********** MNEALR    IFEQ *HIVAL
     C**********           Z-ADD*ZERO     MNEALR
     C**********           END
     C********** MNANLR    IFEQ *HIVAL
     C**********           Z-ADD*ZERO     MNANLR
     C**********           END
     C********** MNEMLR    IFEQ *HIVAL
     C**********           Z-ADD*ZERO     MNEMLR
     C**********           END
     C********** MNEXLR    IFEQ *HIVAL
     C**********           Z-ADD*ZERO     MNEXLR
     C**********           END
     C**********
     C** OUTPUT TOTALS
     C**********
     C** OF                EXCPTHEAD
     C**********           MOVE '0'       *INOF
     C********** LRCT      IFNE *ZERO
     C*** BEGIN ADD *** 11/04/97 *******************************
     C**********           Z-ADDLREAPR    LREAP2  90H
     C**********           Z-ADDAVEAPR    AVEAP2  90H
     C**********           Z-ADDMNEALR    MNEAL2  90H
     C**********           Z-ADDMXEALR    MXEAL2  90H
     C*** END ADD ***** 11/04/97 *******************************
     C******************** EXCPTTOTLR
     C**********           ELSE
     C**********           EXCPTDETL2
     C**********           ENDIF
     C**********
     C**********           ENDSR
      **** END   DEL **** 07/02/99 *********************************************
     C**** BEGIN ADD **** 01/19/00 *********************************************
     C*==================================================================*
     C*   LR Calculations                  ** Subroutine **   S U B L R  *
     C*==================================================================*
     C*
     C     SUBLR         BEGSR
     C*
     C                   Z-ADD     *ZERO         AVEAPR           11 2
     C                   Z-ADD     *ZERO         AVANPR           11 2
     C                   Z-ADD     *ZERO         AVEMPR           11 2
     C*
     C** CALCULATE AVERAGES, MINIMUMS, MAXIMUMS
     C*
     C     LRCT          IFNE      *ZERO
     C     LREAPR        DIV(H)    LRCT          AVEAPR           11 2
     C     LREMPR        DIV(H)    LRCT          AVEMPR           11 2
     C                   ELSE
     C                   Z-ADD     *ZERO         AVEAPR
     C                   Z-ADD     *ZERO         AVEMPR
     C                   END
     C*
     C     LRCTE         IFNE      *ZERO
     C     LREXM         DIV(H)    LRCTE         AVEXM             5 4
     C                   ELSE
     C                   Z-ADD     *ZERO         AVEXM
     C                   END
     C*
     C     MNEALR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEALR
     C                   END
     C     MNEMLR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEMLR
     C                   END
     C     MNEXLR        IFEQ      *HIVAL
     C                   Z-ADD     *ZERO         MNEXLR
     C                   END
     C*
     C*  OUTPUT TOTALS
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C     LRCT          IFNE      *ZERO
     C*** BEGIN CHG ***  05/28/08  ********************************************
     C*************        Z-ADDLREAPR    LREAP2  90H
     C                   Z-ADD(H)  LREAPR        LREAP2           12 0
     C*** END CHG *****  05/28/08  ********************************************
     C*** begin chg ***  10/05/10  ****************************************
     C**************       Z-ADDAVEAPR    AVEAP2  90H
     C**************       Z-ADDMNEALR    MNEAL2  90H
     C**************       Z-ADDMXEALR    MXEAL2  90H
     C                   Z-ADD     AVEAPR        AVEAP2           11 2
     C                   Z-ADD     MNEALR        MNEAL2           11 2
     C                   Z-ADD     MXEALR        MXEAL2           11 2
     C*** end   chg ***  10/05/10  ****************************************
     C                   EXCEPT    TOTLR
     C                   ELSE
     C                   EXCEPT    DETL2
     C                   ENDIF
     C*
     C                   ENDSR
      **** END   ADD **** 01/19/00 *********************************************
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 **************************************
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
      ***** End Del ***** 08/26/14 **************************************
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
     O                       ORTITL              81
      ***** Beg Del ***** 08/26/14 **************************************
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
     O*  MLS 1/18/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 **************************************
     O          E            ROUTE          2
     O                                         +  6 'GROUP:'
     O                       XXFND             +  8
     O          E            ROUTE          2
     O                                         +  6 'POLICY YEAR:'
     O                       XXFYR         Z   +  1
     O          E            ROUTE          2
     O                                         +  6 'DATE RUN:'
     O                       XXDATE            +  5 '  /  /    '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS. INC,.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             139
     O                       DSDATE        Y    198
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             139
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           99 'POLICY YEAR '
     O                       XXFYR         Z   +  2
     O                                          121 'AS OF'
     O                       X1DATE        Y    130
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME            +  2
     O          E            HEAD           1
     O                                            6 'CLASS'
     O                                           84 'WRITTEN'
     O                                          110 'EXP'
     O                                          126 'EXP'
     O                                          142 'EXP'
     O                                          173 'PAYRL MANUAL'
     O          E            HEAD           2
     O                                            6 'CODE'
     O*** BEGIN ADD *** 04/20/09 *******************************
     O                                           10 'EXT'
     O*** END   ADD *** 04/20/09 *******************************
     O*** BEGIN ADD *** 04/20/09 *******************************
     O*************                      18 'DESCRIPTION'
     O                                           24 'DESCRIPTION'
     O*** END   ADD *** 04/20/09 *******************************
     O                                           41 'EMPLR'
     O                                           61 'DIV EMPLOYER NAME'
     O                                           84 'PREMIUM'
     O                                          101 'PAYROLL AMT'
     O                                          119 'MOD EFF DATE'
     O                                          135 'MOD EFF DATE'
     O                                          151 'MOD EFF DATE'
     O                                          159 'RATE'
     O                                          173 'PREMIUM AMT'
     O                                          185 'AGENT NAME'
     O          E            DETL2          1
     O                                           94 '***   NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA   ***'
     O          E            DETAIL         1
     O                       EPCLAS        Z      5
     O                       EPSPEC               6
     O*** BEGIN ADD *** 04/20/09 *******************************
     O                       EPEXT               12
     O*** END   ADD *** 04/20/09 *******************************
     O*** BEGIN CHG *** 04/20/09 *******************************
     O*************            DSDES1    31
     O                       DSDES1              32
     O*** END   ADD *** 04/20/09 *******************************
     O                       EPEMP#        Z     41
     O                       EPDIV         Z     47
     O                       ENAME               67
     O              N78      ELANPR        J     85
     O*** begin chg ***  10/05/10  ****************************************
     O***********     N78      XXEAP2   101 '   ,   , 0 '
     O              N78      XXEAP2        J    101
     O*** end   chg ***  10/05/10  ****************************************
     O               78      ELAAPR        J     85
     O*** begin chg ***  10/05/10  ****************************************
     O***********      78      XXAAP2   101 '   ,   , 0 '
     O               78      XXAAP2        J    101
     O*** end   chg ***  10/05/10  ****************************************
     O                       ELEXM1        4    110
     O              N15      XXEMD1        Y    119
     O                       ELEXM2        4    126
     O              N16      XXEMD2        Y    135
     O                       ELEXM3        4    142
     O              N17      XXEMD3        Y    151
     O                       EPARAT        J    160
     O                 N78   XXEMPR        J    174
     O                  78   XXAMPR        J    174
     O                       ANAME              198
     O          E    52      SPEC           1
     O                                         +  7 '***   NO OTHER CLASSES'
     O                                         +  1 'EXIST FOR EMPLOYER   ***'
     O          E            TOT         1  1
     O                                           69 'JOB CLASS TOTAL'
     O                       L1ANPR        JB    85
     O*** begin chg ***  10/05/10  ****************************************
     O************             L1EAP1   101 '   ,   , 0 '
     O                       L1EAP1        J    101
     O*** end   chg ***  10/05/10  ****************************************
     O                       L1EMPR        JB   174
     O          E            TOT            1
     O                                           69 'AVG  '
     O                       AVANPR        JB    85
     O*** begin chg ***  10/05/10  ****************************************
     O**************           AVEAP1   101 '   ,   , 0 '
     O                       AVEAP1        J    101
     O***end   chg  ***  10/05/10  ****************************************
     O                       AVEXM         3B   110
     O                       AVEMPR        JB   174
     O          E            TOT            1
     O                                           69 'MIN  '
     O                       MNANPR        JB    85
     O*** begin chg ***  10/05/10  ****************************************
     O****************         MNEAP1   101 '   ,   , 0 '
     O                       MNEAP1        J    101
     O*** end   chg ***  10/05/10  ****************************************
     O                       MNEXM         3B   110
     O                       MNEMPR        JB   174
     O          E            TOT            1
     O                                           69 'MAX  '
     O                       MXANPR        JB    85
     O*** begin chg ***  10/05/10  ****************************************
     O***************          MXEAP1   101 '   ,   , 0 '
     O                       MXEAP1        J    101
     O*** end   chg ***  10/05/10  ****************************************
     O                       MXEXM         3B   110
     O                       MXEMPR        JB   174
     O          E            TOT            2
     O                                           69 'COUNT'
     O                       COUNT1        ZB    84
      **** BEGIN DEL **** 07/02/99 *********************************************
     O******* E  1             TOTLR
     O*******                            69 '    FINAL TOTAL'
     O*******                  LRANPRJB  85
     O*******                  LREAP2   101 '   ,   , 0 '
     O*******                  LREMPRJB 174
     O******* E  1             TOTLR
     O*******                            69 'AVG  '
     O*******                  AVANPRJB  85
     O*******                  AVEAP2   101 '   ,   , 0 '
     O*******                  AVEXM 3B 110
     O*******                  AVEMPRJB 174
     O******* E  1             TOTLR
     O*******                            69 'MIN  '
     O*******                  MNANLRJB  85
     O*******                  MNEAL2 B 101 '   ,   , 0 '
     O*******                  MNEXLR3B 110
     O*******                  MNEMLRJB 174
     O******* E  1             TOTLR
     O*******                            69 'MAX  '
     O*******                  MXANLRJB  85
     O*******                  MXEAL2 B 101 '   ,   , 0 '
     O*******                  MXEXLR3B 110
     O*******                  MXEMLRJB 174
     O******* E  2             TOTLR
     O*******                            69 'COUNT'
     O*******                  LRCT  ZB  84
      **** END   DEL **** 07/02/99 *********************************************
     O***** BEGIN ADD **** 11/19/00 ********************************************
     O          E            TOTLR          1
     O                                           69 '    FINAL TOTAL'
     O*** BEGIN CHG ***  05/28/08  *********************************************
     O*************            LREAP2   101 '   ,   , 0 '
     O                       LREAP2             101 '   ,   ,   , 0 '
     O*** END CHG *****  05/28/08  *********************************************
     O                       LREMPR        JB   174
     O          E            TOTLR          1
     O                                           69 'AVG  '
     O*** begin chg ***  10/05/10  ****************************************
     O************             AVEAP2   101 '   ,   , 0 '
     O                       AVEAP2        J    101
     O*** end   chg ***  10/05/10  ****************************************
     O                       AVEXM         3B   110
     O                       AVEMPR        JB   174
     O          E            TOTLR          1
     O                                           69 'MIN  '
     O*** begin chg ***  10/05/10  ****************************************
     O************             MNEAL2 B 101 '   ,   , 0 '
     O                       MNEAL2        JB   101
     O*** end   chg ***  10/05/10  ****************************************
     O                       MNEXLR        3B   110
     O                       MNEMLR        JB   174
     O          E            TOTLR          1
     O                                           69 'MAX  '
     O*** begin chg ***  10/05/10  ****************************************
     O************             MXEAL2 B 101 '   ,   , 0 '
     O                       MXEAL2        JB   101
     O*** end   chg ***  10/05/10  ****************************************
     O                       MXEXLR        3B   110
     O                       MXEMLR        JB   174
     O***** END   ADD **** 11/19/00 ********************************************
