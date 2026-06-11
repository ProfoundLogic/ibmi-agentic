     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  Program description:                                         *
     H*  This program will allow the user to setup various security   *
     H*  levels and other variable WCMC information for the Provider  *
     H*  bill screens, printing & information.                        *
     H*                                                               *
     H*  PROGRAMMER: C:94                                             *
     H*  CREATED:    01/10/98          PROJECT: MC157                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/28/18 C:180 Project: 114MHV/Support 69834                 *
     H*                 - Do not display CVT claim number             *
     H*                   when MC fund linked to WCMS fund during     *
     H*                   Add mode.                                   *
     H*                 - Unprotect CVT claim number when stand alone *
     H*                   MC group.                                   *
     H*                                                               *
     H*  09/05/17 C:180 Project: 114MHV                               *
     H*                Add CVT claim number flag.                     *
     H*                                                               *
     H*  07/26/17 C:79 Project: SY395                                 *
     H*                Add Remark code for EOB's                      *
     H*                                                               *
     H*  08/01/14 C:212 Project: 114MNV                               *
     H*                 add field CAPAUF in display file and related  *
     H*                 processing                                    *
     H*                                                               *
     H*  04/03/13 C:277 Redmine# 2359                                 *
     H*                 Change the checkbox default to 'N'            *
     H*                                                               *
     H*  12/01/12 C:079 PROJECT: 114MER                               *
     H*                -TAKE OUT CALL TO F0122R                       *
     H*                                                               *
     H*  06/25/12 C:280 PROJECT: 114MER                               *
     H*                -CONVERT TO ILE                                *
     H*                                                               *
     H*  05/28/02 C:62 PROJECT: 114MDC                                *
     H*                -ADD PROCESSING FOR CODEREVIEW FLAG ON SCR02.  *
     H*                -ADD CALL TO F0122R.                           *
     H*                                                               *
     H*  11/30/00 C:62 PROJECT: MCMNT                                 *
     H*                EDIT AGAINST INVALID PEND CODES.               *
     H*                                                               *
     H*  07/13/99 C:62 PROJECT: MC206                                 *
     H*                UPDATE IN ADD MODE                             *
     H*                                                               *
     H*  06/30/99 C:62 PROJECT: MC206                                 *
     H*                CLEAR FIELDS ON ADD                            *
     H*                                                               *
     H*  03/05/99 C:53 PROJECT: MC152                                 *
     H*                REMOVE RECON DARTS RMK CODE FROM SCREEN        *
     H*                                                               *
     H*  01/06/99 C:53 PROJECT: MC152                                 *
     H*                REVISED PROGRAM TO HANDLE PROVIDER BILLS       *
     H*                                                               *
     H*****************************************************************
     H*  /COPY INDICATOR USAGE:                                       *
     H*   90      General error indicator                             *
     H*                                                               *
     H*  GENERAL INDICATOR USAGE:                                     *
     H*   99      General chain                                       *
     H*                                                               *
     H*  SCR01 INDICATOR USAGE:                                       *
     H*   10-19   Error msg and cursor positioning                    *
     H*   41      Position cursor at EOMB w/Check security type       *
     H*   42      Position cursor at EOMB recon allowed               *
     H*   43      Position cursor at EOMB recon security type         *
     H*   44      Position cursor at EOMB recon DARTS remark code     *
     H*   71      Medical examiner heading required                   *
     H*   72      Medical examiner file usage required                *
     H*   73      Medical examiner field usage required               *
     H*   74      MRA reductions heading required                     *
     H*   75      Other reduct 1 heading required                     *
     H*   76      Other reduct 2 heading required                     *
     H*   77      Ntwk reductions heading required                    *
     H*   78      EOMB w/check security level must > zero             *
     H*   79      EOMB w/check security type must be valid            *
     H*   80      EOMB Recon allowed must be Y or N                   *
     H*   81      EOMB Recon security level must > zero               *
     H*   82      EOMB Recon security type must be valid              *
     H*   83      EOMB Recon DARTS remark is valid                    *
     H*   84      Data has been updated since entry into this record  *
     H*****************************************************************
     H*
     FF0121F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for maintaining record)
     FFMCXP     UF   E             DISK
     F                                     RENAME(PCX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** BEGIN ADD *** 05/28/02 **************************************
     F                                     INFDS(DBFDSA)
     F*** END   ADD *** 05/28/02 **************************************
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FFTBSP     IF   E             DISK
     FFTBSL     IF   E           K DISK
     F                                     RENAME(PBS01:PBS02)
     F*** Beg   Add *** 07/26/17 **************************************
     FSTCTL1    IF   E           K DISK
     F*** End   Add *** 07/26/17 **************************************
     D*** BEGIN ADD *** 09/05/17 **************************************
     d  outRtCD        S              1
     d  outCo          S              3
     d  outSub#        S              5
     d  outFnd         S              3
     d  outSts         S              1

     d GroupType       pr                  Extpgm('FA701R')
     d                                1
     d                                3
     d                                5
     d                                3
     d                                1
     D*** END   ADD *** 09/05/17 **************************************
      /COPY SCOPYMT01D
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** BEGIN ADD *** 05/28/02 **************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*** END   ADD *** 05/28/02 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Bill Contro-
     D                                     l Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(2)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*  based on User Parameter values, not screen/*ENTRY values
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C* Generic Table File Search Parm List
     C     GTFPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C* DARTS API Parm List to edit DARTS Remark Type & retrieve desc
     C     DRTPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##RMCD            5
     C                   PARM                    ##DESC           40
     C                   PARM                    ##RTCD            1
     C*
     C     DRTPL1        PLIST
     C                   PARM                    ##RMCD            5
     C                   PARM                    ##DESC           40
     C                   PARM                    ##RTCD            1
     C*
     C     PNDPL         PLIST
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'P'           ##TYPE            1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*** Key list to WCMC Control File
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*** Key list to Subscriber Master File
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C*
     C*** Key list to Group Master File
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*** Key list to Generic Table File
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*** Key list to Bill status code File
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXSUB#        KKSUB#
     C     *LIKE         DEFINE    CXFND         KKFND
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C     Q2RECF        CASEQ     'SCR02'       NXTPGM                         ADD 01/06/99
     C                   ENDCS                                                  ADD 01/06/99
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Edit Medical examiner heading - Required
     C     SCHEX         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Edit Medical examiner file - Required
     C     SCFLEX        IFEQ      *BLANKS
     C                   SETON                                        72  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*** BEG ADD *** 01/28/99 **************************
     C                   MOVEL(P)  'DIUS'        KKTBID
     C                   MOVEL(P)  SCFLEX        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N90
     CAN 99              SETON                                        73  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   MOVEL     GTDESC        SCFLED
     C*** END ADD *** 01/28/99 **************************
     C*** BEG DEL *** 01/28/99 **************************
     C***Edit*Medical examiner field - Required
     C********   SCFDEX    IFEQ *BLANKS
     C********             SETON                     73  90
     C********   *IN90     CABEQ'1'       ERRSTP
     C********             END
     C*** END DEL *** 01/28/99 **************************
     C*
     C*  Edit MRA reductions heading - Required
     C     SCHMRA        IFEQ      *BLANKS
     C                   SETON                                        74  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Edit Other Reduction 1 heading - Required
     C     SCHOT1        IFEQ      *BLANKS
     C                   SETON                                        75  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Edit Other Reduction 2 heading - Required
     C     SCHOT2        IFEQ      *BLANKS
     C                   SETON                                        76  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Edit Network Reductions heading - Required if NTWK installed
     C   21SCHNET        IFEQ      *BLANKS
     C                   SETON                                        77  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Delete EOMB w/check security level CANNOT BE < OR = 0
     C     SCCKDL        IFLE      *ZERO
     C                   SETON                                        78  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Delete EOMB w/check security type must be valid
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  SCCKDT        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N90
     CAN 99              SETON                                        79  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C*
     C*  EOMB recon allowed must be Y or N
     C     SCRCAL        IFNE      'Y'
     C     SCRCAL        ANDNE     'N'
     C  N90              SETON                                        80  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  EOMB recon security level CANNOT BE < OR = 0
     C     SCRCAL        IFEQ      'Y'
     C     SCRCSL        IFLE      *ZERO
     C  N90              SETON                                        81  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C*  EOMB recon security type must be valid
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  SCRCST        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N90
     CAN 99              SETON                                        82  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C*
     C*** BEG DEL *** 03/05/99 ***************************
     C*  EOMB recon DARTS remark code must be valid
     C*********
     C*********            MOVE SCRCRM    ##RMCD
     C*********            MOVE *BLANKS   ##DESC
     C*********            MOVE *BLANK    ##RTCD
     C*********            CALL 'DA200R'  DRTPL1
     C*********  ##RTCD    IFEQ 'Y'
     C*********            MOVEL##DESC    SCRCRD    P
     C*********            ELSE
     C**N90****            SETON                     83  90
     C*********  *IN90     CABEQ'1'       ERRSTP
     C*********            END
     C*** END DEL *** 03/05/99 ***************************
     C                   ENDIF
     C*
     C* Check to be sure screen has not been updated since entry
     C  N90CXDT          IFGT      SAVEDT
     C     CXDT          OREQ      SAVEDT
     C     CXTIME        ANDGT     SAVETM
     C                   SETON                                        84  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*** Beg   Add *** 07/26/17 **************************************
     C     SCRMEB        CHAIN     PCT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8590
     C                   ENDIF                                                  *IN85 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'I'
     C                   SETON                                        8590
     C                   ENDIF                                                  *IN90 IFEQ
     C*** End   Add *** 07/26/17 **************************************
     C*
     C     ERRSTP        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C***** Beg Add *** 08/01/14 **************************************
     C/free
       // initialize error fields and flags
        erAudFlag = *off;
        msAudFlag = *blanks;
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C***
     C***        SCAPSC    IFNE 'Y'
     C***        SCAPSC    ANDNE'N'
     C***                  MOVE *ON       *IN90
     C***        *IN90     CABEQ*ON       ENDCK2         71
     C***                  ENDIF
     C***
     C***        SCAPDF    IFNE 'A'
     C***        SCAPDF    ANDNE'P'
     C***                  MOVE *ON       *IN90
     C***        *IN90     CABEQ*ON       ENDCK2         72
     C***                  ENDIF
     C***
     C***        SCAPDF    IFEQ 'P'
     C***                  MOVE 'P'       KKTYPE
     C***                  MOVE SCAPPN    KKCODE
     C***        BSKY      CHAINPBS02                99
     C***
     C***        *IN99     IFEQ *ON
     C***                  MOVE *BLANKS   SCAPPD
     C***                  MOVE *ON       *IN90
     C***        *IN90     CABEQ*ON       ENDCK2         73
     C***
     C***                  ELSE
     C***                  MOVELBSDESC    SCAPPD
     C***                  ENDIF
     C***                  ENDIF
     C***
     C***        SCCPSC    IFNE 'Y'
     C***        SCCPSC    ANDNE'N'
     C***                  MOVE *ON       *IN90
     C***        *IN90     CABEQ*ON       ENDCK2         74
     C***                  ENDIF
     C*
     C     SCCPDF        IFNE      'C'
     C     SCCPDF        ANDNE     'P'
     C                   MOVE      *ON           *IN90
     C     *IN90         CABEQ     *ON           ENDCK2                   75
     C                   ENDIF
     C*
     C     SCCPDF        IFEQ      'P'
     C*
     C                   MOVE      'P'           KKTYPE
     C                   MOVE      SCCPPN        KKCODE
     C     BSKY          CHAIN     PBS02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*** BEGIN ADD *** 11/30/00 **************************************
     C     BSSTAT        OREQ      'I'
     C*** END   ADD *** 11/30/00 **************************************
     C                   MOVE      *BLANKS       SCCPPD
     C                   MOVE      *ON           *IN90
     C     *IN90         CABEQ     *ON           ENDCK2                   76
     C*
     C                   ELSE
     C                   MOVEL     BSDESC        SCCPPD
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 05/28/02 **************************************
     C*
     C*  Process thru CodeReview flag must be Y or N
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN30         ANDEQ     *ON
     C     SCCRUS        ANDNE     'Y'
     C     SCCRUS        ANDNE     'N'
     C                   SETON                                        7790
     C                   ENDIF
     C*** END   ADD *** 05/28/02 **************************************
     C***** Beg Add *** 08/01/14 **************************************
     C/free
       // Procura Audit Flag must be Y or N
        if *IN90 = *OFF
           and *IN30 = *ON
           and dspAudFlag = *on
           and SCPAUF <> 'Y'
           and SCPAUF <> 'N';
          erAudFlag = *on;
          msAudFlag = 'MCR0501';
          *in90 = *on;
        endif;
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C*
     C     ENDCK2        ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** SETOFF INDICATORS FOR ERROR MSG AND CURSOR POSITIONING
     C*
     C                   SETOFF                                       101112
     C                   SETOFF                                       131415
     C                   SETOFF                                       161718
     C                   SETOFF                                       19
     C***** Beg Add *** 08/01/14 **************************************
     C/free
        erAudFlag = *off;
        msAudFlag = *blanks;
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C*
     C                   MOVEA     '0000'        *IN(41)
     C*
     C                   SELECT
     C*------------
     C* SCR01: MEDICAL EXAMINER USAGE
     C     HFLD          WHENEQ    'SCFLEX'
     C                   MOVEL     'DIUS'        ##TBID
     C                   CALL      'F2380R'      GTFPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                            11
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL     GTKEY         SCFLEX
     C                   MOVEL     GTDESC        SCFLED
     C                   SETON                                        12
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C*------------
     C* SCR01: Delete EOMB w/check Security Type
     C     HFLD          WHENEQ    'SCCKDT'
     C                   MOVEL     'SECT'        ##TBID
     C                   CALL      'F2380R'      GTFPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        41
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL     GTKEY         SCCKDT
     C                   MOVEL     GTDESC        SCCKDD
     C                   SETON                                        42        CHG 01/28/99
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C*------------
     C* SCR01: EOMB recon Security Type
     C     HFLD          WHENEQ    'SCRCST'
     C                   MOVEL     'SECT'        ##TBID
     C                   CALL      'F2380R'      GTFPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*                    SETON                     80  43
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL     GTKEY         SCRCST
     C                   MOVEL     GTDESC        SCRCSD
     C**********           SETON                     44    DEL 03/05/99
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*------------
     C*** BEG DEL *** 03/05/99 ***************************
     C**SCR01:*DARTS Remark Type Code
     C********** HFLD      WHEQ 'SCRCRM'
     C**********           MOVE SCRCRM    ##RMCD
     C**********           CALL 'SC305R'  DRTPL
     C**********
     C********** *INKL     IFEQ *OFF
     C**********           MOVEL##RMCD    SCRCRM
     C**********           MOVEL##DESC    SCRCRD
     C**********           ENDIF
     C*** END DEL *** 03/05/99 ***************************
     C*
     C*------------
     C* SCR01: ERROR MSG AND CURSOR POSITIONING
     C*
     C     HFLD          WHENEQ    'SCHEX'
     C                   SETON                                        1090
     C*** BEG DEL *** 01/28/99 **************************
     C********** HFLD      WHEQ 'SCFLEX'
     C**********           SETON                     1190
     C********** HFLD      WHEQ 'SCFDEX'
     C**********           SETON                     1290
     C*** END DEL *** 01/28/99 **************************
     C     HFLD          WHENEQ    'SCHMRA'
     C                   SETON                                        8490      CHG 01/28/99
     C     HFLD          WHENEQ    'SCHOT1'
     C                   SETON                                        1390      CHG 01/28/99
     C     HFLD          WHENEQ    'SCHOT2'
     C                   SETON                                        1590
     C     HFLD          WHENEQ    'SCHNET'
     C                   SETON                                        1690
     C     HFLD          WHENEQ    'SCCKDL'
     C                   SETON                                        1790
     C     HFLD          WHENEQ    'SCRCAL'
     C                   SETON                                        1890
     C     HFLD          WHENEQ    'SCRCSL'
     C                   SETON                                        1990
     C                   ENDSL
     C**
     C                   SELECT
     C*------------
     C* SCR02: DEFAULT PEND CODES
     C***        HFLD      WHEQ 'SCAPPN'
     C***                  CALL 'F2435R'  PNDPL
     C***
     C***        *INKL     IFEQ *ON
     C***        ##RRN     OREQ *ZEROS
     C***                  SETON                     80  41
     C***                  ELSE
     C***
     C***        ##RRN     CHAINPBS01                99
     C***
     C***        *IN99     IFEQ *ON
     C***                  MOVE *BLANKS   SCAPPD
     C***                  ELSE
     C***                  MOVELBSCODE    SCAPPN
     C***                  MOVELBSDESC    SCAPPD
     C***                  ENDIF
     C***                  ENDIF
     C*
     C     HFLD          WHENEQ    'SCCPPN'
     C                   CALL      'F2435R'      PNDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*** BEGIN CHG *** 05/28/02 **************************************
     C**********           SETON                     80  41
     C                   SETON                                            43
     C*** END   CHG *** 05/28/02 **************************************
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PBS01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVE      *BLANKS       SCCPPD
     C                   ELSE
     C                   MOVEL     BSCODE        SCCPPN
     C                   MOVEL     BSDESC        SCCPPD
     C                   ENDIF
     C*** BEGIN ADD *** 05/28/02 **************************************
     C                   SETON                                        4490
     C*** END   ADD *** 05/28/02 **************************************
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCCPDF'
     C                   SETON                                        7490

     C***** Beg Add *** 08/01/14 **************************************
     C     HFLD          WHENEQ    'SCPAUF'
     C     dspAudFlag    ANDEQ     *on
     C                   eval      erAudFlag = *on
     C                   eval      msAudFlag = 'MC40004'
     C                   eval      *in90 = *on
     C***** End Add *** 08/01/14 **************************************
     C*** BEGIN ADD *** 05/28/02 **************************************
     C     HFLD          WHENEQ    'SCCRUS'
     C                   SETON                                        7890
     C*** END   ADD *** 05/28/02 **************************************
     C*** Beg   Add *** 07/26/17 **************************************
     C*
     C     HFLD          WHENEQ    'SCRMEB'
     C*
     C                   CALL      'SC580R'
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##RTC             1
      *
     C     ##RMCD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  ##RMCD        SCRMEB
     C*
     C     SCRMEB        CHAIN     PCT01                              99
     C*
     C  N99              MOVEL(P)  CTDESC        SCRMED
     C   99              MOVEL(P)  *BLANKS       SCRMED
     C*
     C                   ENDIF
     C*
     C                   SETON                                        45
     C*** End   Add *** 07/26/17 **************************************
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C*------------
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   SETOFF                                       20
     C*
     C                   MOVE      KKSUB#        SCSUB#
     C     SMKY          CHAIN     PSM01                              99
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C                   MOVE      KKFND         SCFND
     C     FNKY          CHAIN     PFN01                              99
     C                   MOVEL     FNNAME        SCGNAM
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     CXHEX         SCHEX
     C                   MOVEL     CXFLEX        SCFLEX
     C***************      MOVELCXFDEX    SCFDEX           DEL 01/28/99
     C                   MOVEL     CXHMRA        SCHMRA
     C                   MOVEL     CXHOT1        SCHOT1
     C                   MOVEL     CXHOT2        SCHOT2
     C                   MOVEL     CXHNET        SCHNET
     C                   Z-ADD     CXCKDL        SCCKDL
     C                   MOVEL     CXCKDT        SCCKDT
     C*** BEG ADD *** 01/28/99 **************************
     C*
     C                   MOVEL(P)  'DIUS'        KKTBID
     C                   MOVEL(P)  CXFLEX        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN CHG *** 06/30/99 **************************************
     C**********           MOVELGTDESC    SCFLED
     C                   MOVEL(P)  GTDESC        SCFLED
     C*** END   CHG *** 06/30/99 **************************************
     C*** BEGIN ADD *** 06/30/99 **************************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFLED
     C*** END   ADD *** 06/30/99 **************************************
     C                   END
     C*** END ADD *** 01/28/99 **************************
     C*
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  CXCKDT        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN CHG *** 06/30/99 **************************************
     C**********           MOVELGTDESC    SCCKDD
     C                   MOVEL(P)  GTDESC        SCCKDD
     C*** END   CHG *** 06/30/99 **************************************
     C*** BEGIN ADD *** 06/30/99 **************************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCKDD
     C*** END   ADD *** 06/30/99 **************************************
     C                   END
     C                   MOVEL     CXRCAL        SCRCAL
     C                   Z-ADD     CXRCSL        SCRCSL
     C                   MOVEL     CXRCST        SCRCST
     C*
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  CXRCST        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN CHG *** 06/30/99 **************************************
     C**********           MOVELGTDESC    SCRCSD
     C                   MOVEL(P)  GTDESC        SCRCSD
     C*** END   CHG *** 06/30/99 **************************************
     C*** BEGIN ADD *** 06/30/99 **************************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCRCSD
     C*** END   ADD *** 06/30/99 **************************************
     C                   END
     C*** BEGIN ADD *** 06/30/99 **************************************
     C                   MOVE      CXCPDF        SCCPDF
     C                   MOVEL     CXCPPN        SCCPPN
     C*
     C     CXCPPN        IFGT      *BLANKS
     C                   MOVE      'P'           KKTYPE
     C                   MOVE      CXCPPN        KKCODE
     C     BSKY          CHAIN     PBS02                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVE      *BLANKS       SCCPPD
     C                   ELSE
     C*** BEGIN CHG *** 06/30/99 **************************************
     C**********           MOVELBSDESC    SCCPPD
     C                   MOVEL(P)  BSDESC        SCCPPD
     C*** END   CHG *** 06/30/99 **************************************
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/30/99 **************************************
     C*
     C*** BEG DEL *** 03/05/99 ***************************
     C**********           MOVELCXRCRM    SCRCRM
     C**********           MOVE SCRCRM    ##RMCD
     C**********           MOVE *BLANKS   ##DESC
     C**********           MOVE *BLANK    ##RTCD
     C**********           CALL 'DA200R'  DRTPL1
     C********** ##RTCD    IFEQ 'Y'
     C**********           MOVEL##DESC    SCRCRD    P
     C**********           ELSE
     C**********           MOVEL*BLANKS   SCRCRD    P
     C**********           END
     C*** END DEL *** 03/05/99 ***************************
     C*** BEGIN ADD *** 05/28/02 **************************************
     C                   MOVEL(P)  CXCRUS        SCCRUS
     C*** END   ADD *** 05/28/02 **************************************
     C***** Beg Add *** 08/01/14 **************************************
     C/free
        If dspAudFlag = *on;
          if CXPAUF = 'Y';
            scPAUF = 'Y';
          else;
            scPAUF = 'N';
          endif;
        Endif;
       //**** Begin Add **** 09/05/17 *********************************
       scCVT# = CXcvt#;
       //**** End   Add **** 09/05/17 *********************************

      /end-free
     C***** End Add *** 08/01/14 **************************************
     C*** Beg Add *** 07/26/17 **************************************
     C                   MOVEL(P)  CXRMEB        SCRMEB
     C*
     C     SCRMEB        CHAIN     PCT01                              99
     C*
     C  N99              MOVEL(P)  CTDESC        SCRMED
     C   99              MOVEL(P)  *BLANKS       SCRMED
     C*** End Add *** 07/26/17 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCHEX
     C                   MOVE      *BLANKS       SCFLEX
     C***************      MOVE *BLANKS   SCFDEX           DEL 01/28/99
     C                   MOVE      *BLANKS       SCHMRA
     C                   MOVE      *BLANKS       SCHOT1
     C                   MOVE      *BLANKS       SCHOT2
     C                   MOVE      *BLANKS       SCHNET
     C                   Z-ADD     *ZEROS        SCCKDL
     C                   MOVE      *BLANK        SCCKDT
     C                   MOVE      *BLANKS       SCCKDD
     C*** Beg Chg *** 04/03/13 ****************************************
     C********           MOVE      *BLANK        SCRCAL
     C                   MOVE      'N'           SCRCAL
     C*** End Chg *** 04/03/13 ****************************************
     C                   Z-ADD     *ZEROS        SCRCSL
     C                   MOVE      *BLANK        SCRCST
     C                   MOVE      *BLANKS       SCRCSD
     C*** BEG DEL *** 03/05/99 ***************************
     C*********            MOVE *BLANK    SCRCRM
     C*********            MOVE *BLANKS   SCRCRD
     C*** END DEL *** 03/05/99 ***************************
     C*** BEGIN ADD *** 06/30/99 **************************************
     C                   MOVE      *BLANKS       SCFLED
     C                   MOVE      *BLANKS       SCCPDF
     C                   MOVE      *BLANKS       SCCPPN
     C                   MOVE      *BLANKS       SCCPPD
     C*** END   ADD *** 06/30/99 **************************************
     C*** BEGIN ADD *** 05/28/02 **************************************
     C*** Beg Chg *** 04/03/13 ****************************************
     C********           MOVEL(P)  *BLANKS       SCCRUS
     C                   MOVEL(P)  'N'           SCCRUS
     C*** End Chg *** 04/03/13 ****************************************
     C*** END   ADD *** 05/28/02 **************************************
     C***** Beg Add *** 08/01/14 **************************************
     C                   eval      scPAUF = 'N'
     C***** End Add *** 08/01/14 **************************************
     C*** Beg   Add *** 07/26/17 **************************************
     C                   MOVEL(P)  *BLANKS       SCRMEB
     C                   MOVEL(P)  *BLANKS       SCRMED
     C*** End   Add *** 07/26/17 **************************************
      /Free
       //**** Begin Add **** 03/28/18 *********************************
       scCVT# = *blanks;
       //**** End   Add **** 03/28/18 *********************************
      /End-Free
     C*
     C                   ENDIF
     C*** SCR02
     C***                  MOVE CXAPSC    SCAPSC
     C***                  MOVE CXAPDF    SCAPDF
     C***                  MOVELCXAPPN    SCAPPN
     C***
     C***        CXAPPN    IFGT *BLANKS
     C***                  MOVE 'P'       KKTYPE
     C***                  MOVE CXAPPN    KKCODE
     C***        BSKY      CHAINPBS02                99
     C***
     C***        *IN99     IFEQ *ON
     C***                  MOVE *BLANKS   SCAPPD
     C***                  ELSE
     C***                  MOVELBSDESC    SCAPPD
     C***                  ENDIF
     C***                  ENDIF
     C***
     C***                  MOVE CXCPSC    SCCPSC
     C*** BEGIN DEL *** 07/01/99 **************************************
     C**********           MOVE CXCPDF    SCCPDF
     C**********           MOVELCXCPPN    SCCPPN
     C*
     C********** CXCPPN    IFGT *BLANKS
     C**********           MOVE 'P'       KKTYPE
     C**********           MOVE CXCPPN    KKCODE
     C********** BSKY      CHAINPBS02                99
     C*
     C********** *IN99     IFEQ *ON
     C**********           MOVE *BLANKS   SCCPPD
     C**********           ELSE
     C**********           MOVELBSDESC    SCCPPD
     C**********           ENDIF
     C**********           ENDIF
     C*** END   DEL *** 07/01/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   Z-ADD     ##CO#         CXCO#
     C                   Z-ADD     SCSUB#        CXSUB#
     C                   Z-ADD     SCFND         CXFND
     C                   MOVE      SCHEX         CXHEX
     C                   MOVEL     SCFLEX        CXFLEX
     C***************      MOVE SCFDEX    CXFDEX           DEL 01/28/99
     C                   MOVE      SCHMRA        CXHMRA
     C                   MOVE      SCHOT1        CXHOT1
     C                   MOVE      SCHOT2        CXHOT2
     C                   MOVE      SCHNET        CXHNET
     C                   Z-ADD     SCCKDL        CXCKDL
     C                   MOVE      SCCKDT        CXCKDT
     C                   MOVE      SCRCAL        CXRCAL
     C                   Z-ADD     SCRCSL        CXRCSL
     C                   MOVEL     SCRCST        CXRCST
     C                   MOVE      SCRCAL        CXRCAL
     C*** BEG DEL *** 03/05/99 ***************************
     C*******              MOVE SCRCRM    CXRCRM
     C*** END DEL *** 03/05/99 ***************************
     C*** SCR02
     C***                  MOVE SCAPSC    CXAPSC
     C***                  MOVE SCAPDF    CXAPDF
     C***                  MOVELSCAPPN    CXAPPN
     C***
     C***                  MOVE SCCPSC    CXCPSC
     C                   MOVE      SCCPDF        CXCPDF
     C                   MOVEL     SCCPPN        CXCPPN
     C*** BEGIN ADD *** 05/28/02 **************************************
     C                   MOVEL(P)  SCCRUS        CXCRUS
     C*** END   ADD *** 05/28/02 **************************************
     C***** Beg Add *** 08/01/14 **************************************
     C/free
        If dspAudFlag = *on;
          if scPAUF = 'Y';
            CXPAUF = 'Y';
          else;
            CXPAUF = *blanks;
          endif;
        Endif;
        //*** Begin Add *** 09/05/17 **********************************
         CXcvt# = SCcvt#;
        //*** End   Add *** 09/05/17 **********************************
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C*** Beg Add *** 07/26/17 **************************************
     C                   MOVEL(P)  SCRMEB        CXRMEB
     C*** End Add *** 07/26/17 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* NXTPGM - EXECUTE NEXT PROGRAM
     C*================================================================
     C     NXTPGM        BEGSR
     C*
     C*** BEG ADD *** 01/06/99 *************
     C                   MOVE      @@MODE        ##MODE            1
     C     *INKL         IFEQ      *OFF
     C*** BEGIN ADD *** 05/28/02 **************************************
     C*
     C* CALL WORK WITH CODEREVIEW GROUP INFORMATION IF FLAG IS 'Y'
     C*
     C*** BEG DEL *** 12/01/12 *********************************************
     C*****
     C*****SCCRUS        IFEQ      'Y'
     C******IN30         ANDEQ     *ON
     C*****
     C*****@@MODE        IFEQ      'D'
     C*****              MOVEL     '1'           ##CALL
     C*****              ELSE
     C*****              MOVEL     ' '           ##CALL
     C*****              ENDIF
     C*****
     C*****              CALL      'F0122R'
     C*****              PARM                    ##CALL
     C******INKC         PARM      '0'           ##KL
     C******INKL         PARM      '0'           ##KL
     C*****              PARM      *ZEROS        ##RRN
     C*****              PARM      DBRRNA        ##RRN2            9 0
     C*****              PARM      @@RRN         ##RRN3            9 0
     C*****              PARM      @@MODE        ##MODE            1
     C*****
     C******INKL         IFEQ      *ON
     C******INKC         OREQ      *ON
     C*****              MOVE      *ON           *IN90
     C*****              MOVE      *OFF          *INKL
     C*****              EXSR      GETDTM
     C*****              ENDIF
     C*****
     C*****              ELSE
     C*** END DEL *** 12/01/12 ****************************************
     C*** END   ADD *** 05/28/02 **************************************
     C*
     C                   CALL      'F0120R99'                           60
     C                   PARM      ' '           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN         ##RRN
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *INKL
     C                   EXSR      GETDTM
     C                   ENDIF
     C*** BEGIN ADD *** 05/28/02 **************************************
     C*** BEG DEL *** 12/01/12 ****************************************
     C*******************ENDIF
     C*** END DEL *** 12/01/12 ****************************************
     C*** END   ADD *** 05/28/02 **************************************
     C                   ENDIF
     C*
     C*** END ADD *** 01/06/99 *************
     C                   ENDSR                                                  NXTPGM
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C*** BEGIN ADD *** 05/28/02 **************************************
     C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
     C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
     C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
     C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
     C*
     C                   SETOFF                                       30
     C     ##CRVW        IFEQ      'Y'
     C                   SETON                                        30
     C                   ENDIF
     C*** END   ADD *** 05/28/02 **************************************
     C*
     C     ##NTWK        COMP      'Y'                                    21     NW INST
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C***** Beg Add *** 08/01/14 **************************************
     C*  initialize error fields and flags
     C*
     C/free
       // display and process procura audit flag only for client 114
        dspAudFlag = *off;
        if cltid = '114';
          dspAudFlag = *on;
        endif;
      /end-free
     C***** End Add *** 08/01/14 **************************************
      /Free
       //**** Beg Add *** 09/05/17 *******************************************
        outRTCD = *blanks;
        outSTS  = *blanks;
        outCO   = %editc(##CO#:'X');
        outSUB# = %editc(KKsub#:'X');
        outFND  = %editc(KKfnd:'X');
        GroupType(outRTCD:outCO:outSUB#:outFND:outSTS);
       //*** Beg Change 03/28/18 **********************************************
          // If outSTS = 'N';
          //    VisCVT# = *on;
          //    DisCVT# = *on;
          // Else;
          //    VisCVT# = *off;
          //    DisCVT# = *on;
          // Endif;
         Select;
         When @@mode = 'A' and outSTS ='N';
            VisCVT# = *off;
            DisCVT# = *on;
         When outSTS = 'N';
            VisCVT# = *on;
            DisCVT# = *on;
         Other;
            VisCVT# = *on;
            DisCVT# = *off;
         Endsl;
       //*** End Change 03/28/18 **********************************************
       //**** End Add *** 09/05/17 *******************************************
      /End-Free
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C                   MOVEL     DSTIME        CXTIME
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C     Q2RECF        CASEQ     'SCR02'       NXTPGM                         ADD 01/06/99
     C                   ENDCS                                                  ADD 01/06/99
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*** BEGIN ADD *** 07/13/99 **************************************
     C                   EXSR      UPDATR
     C                   MOVEL     *ON           OUTFLG
     C*** END   ADD *** 07/13/99 **************************************
     C                   ENDSR
