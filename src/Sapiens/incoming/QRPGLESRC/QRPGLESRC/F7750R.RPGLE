     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F7750R - REQUEST LETTER SYSTEM WEEKLY BATCH - REPORT PROGRAM
     H*****************************************************************
     H** REVISIONS:                                                  **
     H**                                                             **
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H** 08/15/00 C:62 PROJECT:WA095 23830                           **
     H**               FIX REPORT TO SHOW CASE INFO IF CHANGE.       **
     H**                                                             **
     H** 12/22/93 C:20     REQ/PROJ: 8/SY008                         **
     H**                   1. DO NOT USE "UP" VALUES BECAUSE PROGRAM **
     H**                      IS SUBMITTED                           **
     H**                                                             **
     H** 10/15/92 KRR   PROJ: 99AAD                                  **
     H**                1. ADDED SUB# TO ENTRY PARMS                 **
     H**                                                             **
     H*****************************************************************
     H** INDICATORS:                                                 **
     H*   08      BATCH                                              **
     H*   09      DEMAND                                             **
     H*   12      NEW CASE - PRINT CASE "HEADER" INFO                **
     H*   25      USED IN LOKUP ON FUND                              **
     H*   50      GENERAL CHAIN                                      **
     H*   51      GENERAL CHAIN                                      **
     H*   52      GENERAL CHAIN                                      **
     H*   60      READ/DOWEQ PLT01                                   **
     H*   80      ALL COMPANIES                                      **
     H*  N80      ONE COMPANIES                                      **
      *****************************************************************
      ** OPEN QUERY FILE: FALTL                                      **
      *     IF CO# EQ 0:                                             **
      *        LTTRDT OR LTDT WITHIN DATE RANGE                      **
      *     IF CO# NE 0:                                             **
      *        LTTRDT OR LTDT WITHIN DATE RANGE, CO#=PARM            **
      *****************************************************************
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFALTL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)
     F*

     D CO#             S              3  0 DIM(9999)                            COMPANY #
     D XBY             S              5  0 DIM(9999)                            ALL BYPASSED
     D XPE             S              5  0 DIM(9999)                            ALL PENDED
     D XUN             S              5  0 DIM(9999)                            ALL UNPENDED
     D XDL             S              5  0 DIM(9999)                            ALL DELETED
     D XCC             S              5  0 DIM(9999)
     D XPC             S              5  0 DIM(9999)
     D FND             S              9  0 DIM(9999)                            FND PROV ARRY
     D SUB             S              9  0 DIM(9999)                            SUB PROV ARRY
     D COM             S              9  0 DIM(9999)                            CO PROV ARRY
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
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
     C*   KEY LIST FOR THE PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE REQUEST LETTER TRANSACTION FILE
     C*
     C     LTKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB             5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB         XXSUB#            5 0
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C                   Z-ADD     0             X                 4 0          COM  ARRAY INDEX
     C                   Z-ADD     0             I                 4 0          FND PROV ARRY NDX
     C                   Z-ADD     0             J                 4 0          COM PROV ARRY NDX
     C                   Z-ADD     0             XXBY              5 0
     C                   Z-ADD     0             XXPE              5 0
     C                   Z-ADD     0             XXUN              5 0
     C                   Z-ADD     0             XXDL              5 0
     C*
     C                   Z-ADD     0             XXBY1             6 0
     C                   Z-ADD     0             XXPE1             6 0
     C                   Z-ADD     0             XXUN1             6 0
     C                   Z-ADD     0             XXDL1             6 0
     C*
     C                   Z-ADD     0             XXBY2             6 0
     C                   Z-ADD     0             XXPE2             6 0
     C                   Z-ADD     0             XXUN2             6 0
     C                   Z-ADD     0             XXDL2             6 0
     C*
     C                   Z-ADD     0             XXBY3             6 0
     C                   Z-ADD     0             XXPE3             6 0
     C                   Z-ADD     0             XXUN3             6 0
     C                   Z-ADD     0             XXDL3             6 0
     C*
     C                   Z-ADD     0             XXBY4             6 0
     C                   Z-ADD     0             XXPE4             6 0
     C                   Z-ADD     0             XXUN4             6 0
     C                   Z-ADD     0             XXDL4             6 0
     C*
     C*  DETERMINE IF ONE COMPANY OR ALL COMPANIES
     C*
     C     XXCO#         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN80                          ALL COMPANIES
     C                   ELSE
     C                   MOVE      '0'           *IN80                          ONE COMPANY
     C                   END
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              50
     C*
     C*** GET ROUTING INFORMATION FOR REPORT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              50
     C     *IN50         IFEQ      '0'
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*  READ LETTER FILE
     C*
     C   80              READ      PLT01                                  60     ALL COMPANIES
     C  N80LTKEY         CHAIN     PLT01                              60         ONE COMPANY
     C*
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END                                                    END IF *IN60
     C*
     C                   Z-ADD     LTCO#         XXCO#             3 0
     C                   Z-ADD     LTSUB#        XXSUB#            5 0
     C                   Z-ADD     LTFND         XXFND             3 0
     C                   Z-ADD     LTFYR         XXFYR             3 0
     C                   Z-ADD     LTFYR         XXYR              2 0
     C                   Z-ADD     LTCASE        XXCASE            7 0
     C                   Z-ADD     LTFEIN        XXFEIN            9 0
     C                   Z-ADD     LTMOD         XXMOD             3 0
     C                   Z-ADD     0             XXBY              5 0
     C                   Z-ADD     0             XXPE              5 0
     C                   Z-ADD     0             XXUN              5 0
     C                   Z-ADD     0             XXDL              5 0
     C                   Z-ADD     0             XXPCT1            5 0
     C                   Z-ADD     0             XXPCT2            5 0
     C                   Z-ADD     0             XXPCT3            5 0
     C                   Z-ADD     0             XXPCT4            5 0
     C                   Z-ADD     0             XXCCT2            5 0
     C                   Z-ADD     0             XXCCT3            5 0
     C                   Z-ADD     0             XXCCT4            5 0
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*  GET FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDHD
     C*
     C*  TRANSPOSE CLAIMANT NAME
     C*
     C                   MOVEL     *BLANKS       XXCLNM           45
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     *BLANKS       SRFNAM           15
     C                   MOVEL     LTLNM         SRLNAM
     C                   MOVEL     LTFNM         SRFNAM
     C                   MOVE      ' '           SSTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SSTERM
     C                   MOVEL     SRLNAM        XXCLNM
     C                   MOVE      '1'           *IN12
     C*
     C* TRANSPOSE PROVIDER NAME
     C     PDKEY         CHAIN     PPD01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVEL     *BLANKS       XXPDNM           45
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     *BLANKS       SRFNAM           15
     C                   MOVEL     PDNAME        SRLNAM
     C                   MOVEL     PDFSN         SRFNAM
     C                   MOVE      ' '           SSTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SSTERM
     C                   MOVEL     SRLNAM        XXPDNM
     C                   END                                                    END IF *IN52
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     LTCO#         IFNE      XXCO#
     C*** BEGIN ADD *** 08/15/00 **************************************
     C                   SETON                                        12
     C*** END   ADD *** 08/15/00 **************************************
     C                   EXSR      SRPROV
     C                   EXSR      SRCASE
     C                   EXSR      SRFND
     C                   EXSR      SRCO
     C                   EXSR      SRCONM
     C                   EXSR      SRFNNM
     C                   EXSR      SRCSNM
     C                   EXSR      SRPRNM
     C                   ELSE
     C     LTSUB#        IFNE      XXSUB#
     C*** BEGIN ADD *** 08/15/00 **************************************
     C                   SETON                                        12
     C*** END   ADD *** 08/15/00 **************************************
     C                   EXSR      SRPROV
     C                   EXSR      SRCASE
     C                   EXSR      SRFND
     C                   EXSR      SRSUB
     C                   EXSR      SRSUBN
     C                   EXSR      SRFNNM
     C                   EXSR      SRCSNM
     C                   EXSR      SRPRNM
     C                   ELSE
     C     LTFND         IFNE      XXFND
     C*** BEGIN ADD *** 08/15/00 **************************************
     C                   SETON                                        12
     C*** END   ADD *** 08/15/00 **************************************
     C                   EXSR      SRPROV
     C                   EXSR      SRCASE
     C                   EXSR      SRFND
     C                   EXSR      SRFNNM
     C                   EXSR      SRCSNM
     C                   EXSR      SRPRNM
     C                   ELSE
     C     LTCASE        IFNE      XXCASE
     C*** BEGIN ADD *** 08/15/00 **************************************
     C                   SETON                                        12
     C*** END   ADD *** 08/15/00 **************************************
     C                   EXSR      SRPROV
     C                   EXSR      SRCASE
     C                   EXSR      SRCSNM
     C                   EXSR      SRPRNM
     C                   ELSE
     C     LTFEIN        IFNE      XXFEIN
     C                   EXSR      SRPROV
     C                   EXSR      SRPRNM
     C                   END                                                    ENDNE XXFEIN
     C                   END                                                    ENDNE XXCASE
     C                   END                                                    ENDNE XXFND
     C                   END                                                    ENDNE XXSUB
     C                   END                                                    ENDNE XXCO#
     C*
     C                   EXSR      SRADD
     C                   END                                                    END DOWEQ 60
     C**********************************
     C*     END THE PROGRAM            *
     C**********************************
     C                   EXSR      SRPROV
     C                   EXSR      SRCASE
     C                   EXSR      SRFND
     C                   EXSR      SRCO
     C     *IN80         IFEQ      '1'
     C                   EXSR      SRALL
     C                   END
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C*  SRADD - CALCULATE STATUS COUNTS PER PROVIDER                 *
     C*****************************************************************
     C     SRADD         BEGSR
     C*
     C     LTSTAT        IFEQ      'B'                                          ## K 5/16/89
     C                   ADD       1             XXBY
     C                   END
     C     LTSTAT        IFEQ      ' '                                          ## K 5/16/89
     C     LTFLG1        ANDEQ     '1'                                          5/15/89 ADD
     C                   ADD       1             XXPE
     C                   END
     C     LTSTAT        IFEQ      'U'                                          ## K 5/16/89
     C                   ADD       1             XXUN
     C                   END
     C     LTSTAT        IFEQ      'D'                                          ## K 5/16/89
     C                   ADD       1             XXDL
     C                   END
     C*
     C*  ACCUMULATE TOTALS FOR FUND, COMPANY AND TOTAL PROV COUNT
     C*
     C                   MOVE      '1'           *IN25
     C     LTFEIN        LOOKUP    FND                                    25
     C     *IN25         IFEQ      '0'
     C                   ADD       1             XXPCT2
     C                   ADD       1             I
     C                   MOVEA     LTFEIN        FND(I)
     C                   END
     C                   MOVE      '1'           *IN25
     C     LTFEIN        LOOKUP    COM                                    25
     C     *IN25         IFEQ      '0'
     C                   ADD       1             XXPCT3
     C                   ADD       1             J
     C                   MOVEA     LTFEIN        COM(J)
     C                   END
     C*
     C**********************************
     C*  READ NEXT RECORD              *
     C**********************************
     C   80              READ      PLT01                                  60     ALL COMPANIES
     C  N80LTKEY         READE     PLT01                                  60     ONE COMPANY
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRPROV - PROVIDER TOTALS                                     *
     C*****************************************************************
     C     SRPROV        BEGSR
     C*
     C                   ADD       1             XXPCT1
     C                   ADD       XXBY          XXBY1                          ACCUMULATE
     C                   ADD       XXPE          XXPE1                          CASE
     C                   ADD       XXUN          XXUN1                          TOTALS
     C                   ADD       XXDL          XXDL1
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDHD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DETAIL
     C                   ENDSR
     C*****************************************************************
     C*  SRPRNM - GET PROVIDER INFORMATION                            *
     C*****************************************************************
     C     SRPRNM        BEGSR
     C                   Z-ADD     LTFEIN        XXFEIN            9 0
     C                   Z-ADD     LTMOD         XXMOD             3 0
     C                   MOVE      '0'           *IN12
     C*
     C*  GET NEW PROVIDER INFORMATION
     C*  TRANSPOSE PROVIDER NAME
     C     PDKEY         CHAIN     PPD01                              52
     C     *IN52         IFEQ      '0'
     C                   MOVEL     *BLANKS       XXPDNM           45
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     *BLANKS       SRFNAM           15
     C                   MOVEL     PDNAME        SRLNAM
     C                   MOVEL     PDFSN         SRFNAM
     C                   MOVE      ' '           SSTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SSTERM
     C                   MOVEL     SRLNAM        XXPDNM
     C                   END                                                    END IF *IN52
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRCASE - CASE TOTALS                                         *
     C*****************************************************************
     C     SRCASE        BEGSR
     C*
     C                   ADD       1             XXCCT2
     C                   ADD       1             XXCCT3
     C                   MOVE      '1'           *IN12                          NEW CASE
     C*
     C                   ADD       XXBY1         XXBY2                          ACCUMULATE
     C                   ADD       XXPE1         XXPE2                          FUND
     C                   ADD       XXUN1         XXUN2                          TOTALS
     C                   ADD       XXDL1         XXDL2
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDHD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    CASTOT
     C                   Z-ADD     0             XXPCT1            5 0
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRCSNM - GET CASE INFORMATION                                *
     C*****************************************************************
     C     SRCSNM        BEGSR
     C*
     C                   Z-ADD     LTFYR         XXFYR
     C                   Z-ADD     LTFYR         XXYR
     C                   Z-ADD     LTCASE        XXCASE
     C*
     C                   MOVEL     *BLANKS       XXCLNM           45
     C                   MOVEL     *BLANKS       SRLNAM           56
     C                   MOVEL     *BLANKS       SRFNAM           15
     C                   MOVEL     LTLNM         SRLNAM
     C                   MOVEL     LTFNM         SRFNAM
     C                   MOVE      ' '           SSTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    SRLNAM
     C                   PARM                    SRFNAM
     C                   PARM                    SSTERM
     C                   MOVEL     SRLNAM        XXCLNM
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRFND - FUND TOTALS                                          *
     C*****************************************************************
     C     SRFND         BEGSR
     C*
     C                   ADD       XXBY2         XXBY3                          ACCUMULATE
     C                   ADD       XXPE2         XXPE3                          COMPANY  R
     C                   ADD       XXUN2         XXUN3                          TOTALS
     C                   ADD       XXDL2         XXDL3
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDHD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    FNDHD2
     C                   EXCEPT    FNDTOT
     C                   Z-ADD     0             XXPCT1            5 0
     C                   Z-ADD     0             I
     C                   Z-ADD     0             FND
     C                   Z-ADD     0             XXPCT2            5 0
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRFNNM - GET FUND NAME                                       *
     C*****************************************************************
     C     SRFNNM        BEGSR
     C*
     C                   Z-ADD     LTFND         XXFND
     C*
     C     FNKEY         CHAIN     PFN01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C     LTCO#         IFEQ      XXCO#
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDHD
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRSUB - SUB# TOTALS                                          *
     C*****************************************************************
     C     SRSUB         BEGSR
     C*
     C                   ADD       XXBY2         XXBY3                          ACCUMULATE
     C                   ADD       XXPE2         XXPE3                          COMPANY  R
     C                   ADD       XXUN2         XXUN3                          TOTALS
     C                   ADD       XXDL2         XXDL3
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDHD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    SUBHD2
     C                   EXCEPT    SUBTOT
     C                   Z-ADD     0             XXPCT1            5 0
     C                   Z-ADD     0             I
     C                   Z-ADD     0             SUB
     C                   Z-ADD     0             XXPCT2            5 0
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRSUBM - GET SUB# NAME                                       *
     C*****************************************************************
     C     SRSUBN        BEGSR
     C*
     C                   Z-ADD     LTSUB#        XXSUB#
     C*
     C     SMKEY         CHAIN     PSM01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBN           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBN
     C                   END
     C*
     C     LTCO#         IFEQ      XXCO#
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDHD
     C                   END
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRCO - COMPANY TOTALS                                        *
     C*****************************************************************
     C     SRCO          BEGSR
     C*
     C                   ADD       1             X                              ARRAY INDEX
     C                   Z-ADD     XXCO#         CO#(X)                         ALL COMP #
     C                   Z-ADD     XXBY3         XBY(X)                         ALL BYPASSED
     C                   Z-ADD     XXPE3         XPE(X)                         ALL PENDED
     C                   Z-ADD     XXUN3         XUN(X)                         ALL UNPENDED
     C                   Z-ADD     XXDL3         XDL(X)                         ALL DELETED
     C                   Z-ADD     XXCCT3        XCC(X)                         ALL CASE COUNT
     C                   Z-ADD     XXPCT3        XPC(X)                         ALL PROVIDER COUNT
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    COMHD
     C                   EXCEPT    COMTOT
     C                   Z-ADD     0             XXPCT1            5 0
     C                   Z-ADD     0             XXPCT2            5 0
     C                   Z-ADD     0             XXPCT3            5 0
     C                   Z-ADD     0             FND
     C                   Z-ADD     0             I
     C                   Z-ADD     0             J
     C                   Z-ADD     0             COM
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRCONM - GET COMPANY NAME                                    *
     C*****************************************************************
     C     SRCONM        BEGSR
     C*
     C                   Z-ADD     LTCO#         XXCO#
     C*
     C     COKEY         CHAIN     PCO01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    FNDHD
     C*
     C                   ENDSR
     C*****************************************************************
     C*  SRALL - PERFORM ALL COMPANIES TOTAL CALCULATIONS & OUTPUT    *
     C*****************************************************************
     C     SRALL         BEGSR
     C*
     C                   MOVE      *BLANKS       XXCONM
     C                   EXCEPT    HEAD
     C                   EXCEPT    ALLHD
     C*
     C                   Z-ADD     1             X
     C     CO#(X)        DOWNE     *ZERO
     C     X             ANDLE     99
     C                   ADD       XBY(X)        XXBY4                          TOT BYPASSED
     C                   ADD       XPE(X)        XXPE4                          TOT PENDED
     C                   ADD       XUN(X)        XXUN4                          TOT UNPENDED
     C                   ADD       XDL(X)        XXDL4                          TOT DELETED
     C                   ADD       XCC(X)        XXCCT4
     C                   ADD       XPC(X)        XXPCT4
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    ALLHD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    ALLDET
     C                   ADD       1             X
     C                   END                                                    END DOWNE 0
     C*
     C                   EXCEPT    ALLTOT
     C*
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
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 1997      '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE               8 '  /  /  '
     O                       XXCONM             119
     O                       DSTIME             198 '  :  :  '
     O          E            HEAD           1
     O                       ORPNAM              10
     O                       ORTITL             129
     O                       PAGE          Z    198
     O                                          192 'PAGE:'
     O          E            HEAD           2
     O                                           89 'FROM:'
     O                       XXFRDT              98 '  /  /  '
     O                                          103 'TO:'
     O                       XXTODT             112 '  /  /  '
     O          E            FNDHD          2
     O                                            8 'COMPANY:'
     O                       XXCO#               13
     O                                           15 '-'
     O                       XXCONM              56
     O          E            FNDHD          2
     O                                            7 'SUB #:'
     O                       XXSUB#              13
     O                                           15 '-'
     O                       XXSUBN              56
     O          E            FNDHD          2
     O                                            7 'FUND #:'
     O                       XXFND               13
     O                                           15 '-'
     O                       XXFNNM              56
     O          E            FNDHD          2
     O                                            4 'YEAR'
     O                                           15 'CASE'
     O                                           38 'CLAIMANT NAME'
     O                                           85 'PROVIDER'
     O                                          106 'PROVIDER NAME'
     O                                          154 '#BYPASSED'
     O                                          168 '#PENDED'
     O                                          183 '#UNPENDED'
     O                                          198 '#DELETED'
     O          E            DETAIL         1
     O               12      XXYR                02
     O               12      XXCASE        Z     18
     O               12      XXCLNM         B    70
     O                       XXFEIN        Z     86
     O                       XXPDNM         B   138
     O                       XXBY          JB   149
     O                       XXPE          JB   165
     O                       XXUN          JB   178
     O                       XXDL          JB   194
     O          E            CASTOT      1  3
     O                                           97 'CASE'
     O                       XXCASE        Z    105
     O                                          112 'TOTALS'
     O                                          132 '#PROVIDERS'
     O                       XXPCT1        JB   139
     O                       XXBY1         JB   149
     O                       XXPE1         JB   165
     O                       XXUN1         JB   178
     O                       XXDL1         JB   194
     O          E            FNDHD2      1  1
     O                                           96 'FUND '
     O                       XXFND         Z    100
     O                                          107 'TOTALS'
     O          E            FNDHD2      1  1
     O                                           50 '#CASES'
     O                                           71 '#PROVIDERS'
     O                                           91 '#BYPASSED'
     O                                          111 '#PENDED'
     O                                          131 '#UNPENDED'
     O                                          149 '#DELETED'
     O          E            FNDTOT      1  2
     O                       XXCCT2        JB    49
     O                       XXPCT2        JB    66
     O                       XXBY2         JB    87
     O                       XXPE2         JB   109
     O                       XXUN2         JB   127
     O                       XXDL2         JB   146
     O          E            SUBHD2      1  1
     O                                           50 '#CASES'
     O                                           71 '#PROVIDERS'
     O                                           91 '#BYPASSED'
     O                                          111 '#PENDED'
     O                                          131 '#UNPENDED'
     O                                          149 '#DELETED'
     O          E            SUBTOT      1  2
     O                       XXCCT2        JB    49
     O                       XXPCT2        JB    66
     O                       XXBY2         JB    87
     O                       XXPE2         JB   109
     O                       XXUN2         JB   127
     O                       XXDL2         JB   146
     O          E            COMHD       1  1
     O                                           97 'COMPANY   '
     O                       XXCO#              104
     O                                          111 'TOTALS'
     O          E            COMHD       1  1
     O                                           50 '#CASES'
     O                                           71 '#PROVIDERS'
     O                                           91 '#BYPASSED'
     O                                          111 '#PENDED'
     O                                          131 '#UNPENDED'
     O                                          149 '#DELETED'
     O          E            COMTOT      1  2
     O                       XXCCT3        JB    49
     O                       XXPCT3        JB    66
     O                       XXBY3         JB    87
     O                       XXPE3         JB   109
     O                       XXUN3         JB   127
     O                       XXDL3         JB   146
     O          E            ALLHD       1  1
     O                                          110 'ALL COMPANIES TOTALS  '
     O          E            ALLHD       1  1
     O                                           48 'COMPANY'
     O                                           64 '#CASES'
     O                                           85 '#PROVIDERS'
     O                                          103 '#BYPASSED'
     O                                          118 '#PENDED'
     O                                          139 '#UNPENDED'
     O                                          156 '#DELETED'
     O          E            ALLDET      1  2
     O                       CO#(X)        Z     44
     O                       XCC(X)        JB    63
     O                       XPC(X)        JB    80
     O                       XBY(X)        JB    99
     O                       XPE(X)        JB   116
     O                       XUN(X)        JB   135
     O                       XDL(X)        JB   153
     O          E            ALLTOT      1  2
     O                                           50 'ALL COMPANIES:   '
     O                       XXCCT4        JB    63
     O                       XXPCT4        JB    80
     O                       XXBY4         JB    99
     O                       XXPE4         JB   116
     O                       XXUN4         JB   135
     O                       XXDL4         JB   153
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
