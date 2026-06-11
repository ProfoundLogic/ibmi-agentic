     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* * *  QUEUE THE INCOMING FAX ************************* * * *
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
     F*
     F*

     D DN              S             75    DIM(5)                               DOC NOTES
     D CMA             C                   CONST('Fax Received from:')
     D CMB             C                   CONST('FaxERRORPartial:')
     D*
     D MDOC            C                   CONST('Document:')
     D MHAS            C                   CONST('has arrived on')
     D MQUE            C                   CONST('Queue:')
     D MSEN            C                   CONST('Sent by:')
     D MID             C                   CONST('&QUEUE  ID')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('** USER ROUTE PGM **')
     D DHBR            C                   CONST('Pgm:')
     D DTOQ            C                   CONST('for TSI:')
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D FIDS02          DS
  O00D  Q3FILE                 1      8
  O00D  Q3OPEN                 9      9
  O00D  Q3EOF                 10     10
  O00D  Q3STCD                11     15  0
  O00D  Q3OPCD                16     21
  O00D  Q3RTN                 22     29
O0000D  Q3SSEQ                30     37
O0000D  Q3RCPR                38     45
O0000D  Q3EXN1                46     52
O0000D  Q3MIO#                53     56
  O00D  Q3FL02                57     76
  O00D  Q3ODPT                77     78
  O00D  Q3FL03                79    122
  O00D  Q3SPL#               123    124B 0
  O00D  Q3FL07               125    151
  O00D  Q3LNRW               152    153B 0
  O00D  Q3CLCH               154    155B 0
  O00D  Q3NREC               156    159B 0
  O00D  Q3FL04               160    187
  O00D  Q3OVLN               188    189B 0
  O00D  Q3FL05               190    260
  O00D  Q3RECF               261    270
  O00D  Q3FL06               271    368
  O00D  Q3IAID               369    369
  O00D  Q3CPOS               370    371B 0
  O00D  Q3FL08               372    377
  O00D  Q3LSFR               378    379B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D FC6             C                   CONST('                    -
     D                                        ')
     D CM              C                   CONST('XXXXXXXXXXXXXXXXX:')

     C*
     C*
     C*
     C*  TRANSACTION FILE
     C*
     C*
     C*   KEY LIST FOR DO FILE
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSID           20            TRSCRBR ID (20)
     C                   PARM                    QQSTS             1            COMPLETION STS
     C                   PARM                    QQFXCN            8            FAX CONTROLLER
     C                   PARM                    QQFXPT            4            FAX PORT
     C                   PARM                    QQFDTE            6            RCV DT (6)
     C                   PARM                    QQFTIM            6            RCV TIM
     C                   PARM                    QQROUT           10            FAX ROUT INFO
     C                   PARM                    QQDT8             8            RCV DATE (8)
     C                   PARM                    QQDCID           12            DOC ID FOR .FAX
     C                   PARM                    QQFOLD           63            FOLDER FOR .FAX
     C                   PARM                    QQPART            1            PERFORM PART LKUP
     C                   PARM                    QQPPGM           10            PARTIAL PGM
     C                   PARM                    QQPLIB           10            PARTIAL PGMLIB
     C                   PARM                    QQDFTQ           10            DFT QUEUE
     C                   PARM                    QQURPG           10            **UR ROUTE PGM
     C                   PARM                    QQURLB           10            USR ROUTE PGM LIB
     C                   PARM                    QQRTCD            1            RTNCODE ********
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8            MDYY
     C                   PARM      '*YYMD'       SRTFMT            8            YYYYMMDD
     C                   PARM                    SRDTO            10
     C                   PARM      *BLANK        SRERR             1
     C*
     C*
     C                   MOVE      QQ#PGS        ###PGS            5 0
     C                   MOVE      QQFDTE        ##FDTE            6 0
     C                   MOVE      QQFTIM        ##FTM             6 0
     C                   MOVE      QQDT8         ##DT8             8 0
     C                   MOVEL     QQRTCD        ##RTN             1
     C                   MOVEL     *BLANKS       QQRTCD
     C                   MOVEL     QQTSID        ##TSID           20
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQURLB        CAT       '/':0         XX11A            11
     C     XX11A         CAT(P)    QQURPG:0      XXPGM            21
     C*
     C     QQURPG        IFNE      *BLANKS
     C                   CALL      XXPGM
     C                   PARM                    QQFAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSID           20            TRSCRBR ID (20)
     C                   PARM                    QQSTS             1            COMPLETION STS
     C                   PARM                    QQFXCN            8            FAX CONTROLLER
     C                   PARM                    QQFXPT            4            FAX PORT
     C                   PARM                    QQFDTE            6            RCV DT (6)
     C                   PARM                    QQFTIM            6            RCV TIM
     C                   PARM                    QQROUT           10            FAX ROUT INFO
     C                   PARM                    QQDT8             8            RCV DATE (8)
     C                   PARM                    QQDCID           12            DOC ID FOR .FAX
     C                   PARM                    QQFOLD           63            FOLDER FOR .FAX
     C                   PARM                    QQPART            1            PERFORM PART LKUP
     C                   PARM                    QQPPGM           10            PARTIAL PGM
     C                   PARM                    QQPLIB           10            PARTIAL PGMLIB
     C                   PARM                    QQDFTQ           10            DFT QUEUE
     C                   PARM                    QQRTCD            1            RTNCODE ********
     C                   ENDIF
     C*
     C***                  CREATE DOC NOTES
     C                   EXSR      CRTNOT
     C                   MOVEL     *BLANKS       QQRTCD
     C                   GOTO      TERM
     C*
     C*
     C     TERM          TAG
     C     ##RTN         CABNE     *BLANKS       ENDPGM                           SETON LR
     C                   RETURN
     C     ENDPGM        TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*
      ********************************************************************
      * * * CREATE DOC NOTES *********************************************
      ********************************************************************
     C     CRTNOT        BEGSR
      *
     C*** FILL ALL DOC NOTE FIELDS
      *
     C                   MOVE      *BLANKS       XX75             75
     C                   MOVEL     QQDT8         XXY               4
     C                   MOVE      QQDT8         XXD               2
     C     2             SUBST     QQDT8:5       XXM               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVEL     QQFTIM        XXH               2
     C                   MOVE      QQFTIM        XXS               2
     C     2             SUBST     QQFTIM:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXHMS             8
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    DREA:5        XX75
     C*
     C                   MOVEL     XX75          DN(1)
     C*
     C     MDOC          CAT(P)    QQDCID:1      XX75
     C                   CAT(P)    DHBR:3        XX75
     C                   CAT(P)    QQURPG:1      XX75
     C                   CAT(P)    'Lib:':3      XX75
     C                   CAT(P)    QQURLB:1      XX75
     C                   CAT(P)    DTOQ:2        XX75
     C                   CAT(P)    QQTSID:1      XX75
     C*
     C                   MOVEL     XX75          DN(2)
     C*
     C                   MOVEL     *BLANKS       XX75
     C                   MOVEL     XX75          DN(3)
     C*
     C*** CALL PGM TO WRITE DOC NOTES
     C*
      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS
     C*
     C                   MOVE      *BLANKS       Q$RTC             1
     C*
     C                   DO        3             G                 3 0
     C*
     C                   MOVEL     QQDCID        $#DCID           12
     C                   MOVEL     DN(G)         QQCM75
     C*
     C*
     C     G             IFEQ      3
     C                   MOVE      '1'           Q$RTC             1
     C                   ENDIF
     C*
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM      QQDCID        QQDCID           12              DOCID
     C                   PARM      QQDT8         QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C     BPDOC         TAG
     C*
     C                   ENDDO
     C*
     C*
     C                   ENDSR
