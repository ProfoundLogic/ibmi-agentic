     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H* * *  PERFORM FAX ROUTING FOR TSI ON INCOMING FAXES     * * *
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
     FIMRTL2    IF   E           K DISK
     F***IMRTL3  IF  E           K        DISK
     F***            PRT01                             KRENAMEPRT02
     FIMRTAL1   IF   E           K DISK
     F                                     INFDS(FIDS01)
     FIMRTAL    IF   E           K DISK
     F                                     INFDS(FIDS02)
     F                                     RENAME(PRT011:PRT021)
     F                                     RENAME(PRT012:PRT022)
     F                                     RENAME(PRT013:PRT023)
     F                                     RENAME(PRT014:PRT024)
     F                                     RENAME(PRT015:PRT025)
     F                                     RENAME(PRT016:PRT026)
     F***
     F*
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D                 DS
     D  Q#LFM                  1     30
     D  Q#LIBN                 1     10
     D  Q#DBFN                11     20
     D  Q#MBRN                21     30
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
     C*
     C*** PARAMERTER LISTS FOR EXTERNAL CALLS
     C*
     C*   PARM LIST FOR "RSNDFXC" CL PGM
     C*
     C     PLSEND        PLIST
     C                   PARM                    QQPHN#           32
     C*
     C     PLSFXE        PLIST
     C                   PARM                    QQPHN#
     C                   PARM                    QQRTCD            1
     C*
     C*
     C*   KEY LIST FOR RT FILE
     C*
     C     RTKY1         KLIST
     C                   KFLD                    ##TSID
     C*
     C*   KEY LIST FOR RTx FILES
     C*
     C     RTKY11        KLIST
     C                   KFLD                    $$FXPT           12
     C                   KFLD                    ##TSID
     C*
     C*   KEY LIST FOR RTx FILES
     C*
     C     RTKY12        KLIST
     C                   KFLD                    $$TSID
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQLIBN           10            LIB NAM-FAXMBR
     C                   PARM                    QQDBFN           10            FIL NAM-FAXMBR
     C                   PARM                    QQMBRN           10            MBR NAM-FAXMBR
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    QQTSI#           20            TRSCRBR ID (20)
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
      *
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
     C                   MOVEL     QQLIBN        Q#LIBN
     C                   MOVEL     QQDBFN        Q#DBFN
     C                   MOVEL     QQMBRN        Q#MBRN
     C*
     C                   MOVE      QQ#PGS        ###PGS            5 0
     C                   MOVE      QQFDTE        ##FDTE            6 0
     C                   MOVE      QQFTIM        ##FTM             6 0
     C                   MOVE      QQDT8         ##DT8             8 0
     C                   MOVEL     QQRTCD        ##RTN             1
     C                   MOVEL     *BLANKS       QQRTCD
     C                   MOVEL     QQTSI#        ##TSID           20
     C                   MOVEL     *BLANKS       XXCOMP            1
     C*
     C                   TIME                    DSDTTM
     C     QQPLIB        CAT       '/':0         XX11A            11
     C     XX11A         CAT(P)    QQPPGM:0      XXPART           21
     C*
      *================================================================
      * * * IF PARTIAL IS USED, CALL THE PARTIAL PGM TO EXTRACT THE COMP
      * * * ARISON VALUE.
      *
      *** INPUT THE TSID (QQTSI#) OUTPUT TSID(##TSID)
     C     QQPART        IFEQ      'Y'
     C     QQPPGM        ANDNE     *BLANKS
     C                   CALL      XXPART
     C                   PARM      QQTSI#        ##TSID           20
     C                   PARM                    $#RTC             1
     C                   ENDIF
      *
      * * * SEARCH FOR THE TSID IN THE ROUTING FILES BY FAX PORT
      * * *
      *        DOES THE TSID EXIST IN THE RT FILE, (ONLY NON-FAXPORT)
     C     RTKY1         CHAIN     PRT01                              60
     C     *IN60         IFEQ      *OFF
      * * * * *
      * FOUND *
      * * * * *
      * * *   TRY TO GET ALL ITEMS AVAILABLE IN THE IMRTP1
      * * *   BY THE TSID(OR PARTIAL) FOR THE SPECIFIC FAXPORT
     C                   MOVEL     ##FAXP        $$FXPT           12
     C                   Z-ADD     0             #PRCSD            5 0
     C                   MOVE      *ON           *IN91                            USE FXPT/TSID
      *
     C                   EXSR      TSI2
     C     #PRCSD        IFNE      *ZERO
     C                   MOVEL     'S'           QQRTCD
     C                   MOVEL     'S'           XXCOMP            1
     C******************** GOTO TERM
     C                   ELSE
     C*** = 0 MEANS NOTHING PROCESSED
     C                   ENDIF
      * * *
      * * *   TRY TO GET ALL ITEMS AVAILABLE IN THE IMRTP1
      * * *   BY THE TSID(OR PARTIAL) FOR THE *ALL FAX PORTS
     C                   MOVEL     *BLANKS       $$FXPT
     C                   MOVEL     '*ALL    '    $$FXPT
     C                   Z-ADD     0             #PRCSD            5 0
     C                   MOVE      *ON           *IN91                            USE FXPT/TSID
      *
     C                   EXSR      TSI2
     C     #PRCSD        IFNE      *ZERO
     C                   MOVEL     'S'           QQRTCD
     C                   GOTO      TERM
     C                   ELSE
     C*** = 0 MEANS NOTHING PROCESSED
     C                   GOTO      TSI2T
     C                   ENDIF
      * * *
     C                   ELSE
      * * *
      * * *   IF NOT FOUND TRY AGAIN THIS TIME BY FAXPORT ID (NO TSID)
     C**
     C     TSI2T         TAG
     C                   MOVE      *OFF          *IN91                            USE TSID
     C                   MOVEL     *BLANKS       $$TSID
     C                   MOVEL     ##FAXP        $$TSID           20
     C                   Z-ADD     0             #PRCSD            5 0
      *
     C                   EXSR      TSI1
     C     #PRCSD        IFNE      *ZERO
     C                   MOVEL     'S'           QQRTCD
     C                   MOVEL     'S'           XXCOMP            1
     C***************      GOTO TERM
     C                   ELSE
     C*** = 0 MEANS NOTHING PROCESSED
     C                   GOTO      TSI3
     C                   ENDIF
      * * * * * * *
      * NOT FOUND *
      * * * * * * *
     C                   ENDIF
      * * *
     C**
      * * *
      * * *   IF NOT FOUND TRY AGAIN THIS TIME FOR FAXPORT *ALL
      *
     C     TSI3          TAG
     C**
     C                   MOVE      *OFF          *IN91                            USE TSID
     C                   MOVEL     *BLANKS       $$TSID
     C                   MOVEL     '*ALL    '    $$TSID           20
     C                   Z-ADD     0             #PRCSD            5 0
      *
     C                   EXSR      TSI1
     C     #PRCSD        IFNE      *ZERO
     C                   MOVEL     'S'           QQRTCD
     C                   GOTO      TERM
     C                   ELSE
     C*** = 0 MEANS NOTHING PROCESSED
     C                   MOVE      'N'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
      * * *       IF NOT FOUND, NO TSI ROUTING IS FOUND.
      *
      *
     C*
      *================================================================
     C* END OF THE PGM
     C*
     C     TERM          TAG
     C     XXCOMP        IFEQ      'S'
     C                   MOVEL     XXCOMP        QQRTCD
     C                   ENDIF
     C     ##RTN         CABNE     *BLANKS       ENDPGM                           SETON LR
     C                   RETURN
     C     ENDPGM        TAG
     C                   MOVE      '1'           *INLR
     C*
     C*
      *================================================================
     C     TSI1          BEGSR
      *================================================================
     C*
     C*
     C***        RTKY12    SETLLIMRTAL
     C***        RTKY12    READEIMRTAL                   61
     C     RTKY12        CHAIN     IMRTAL                             61
     C     *IN61         DOWEQ     *OFF
      * * * * *
      * FOUND * * *  FOR EACH ROUTING ACTION RCD, PERFORM FUNCTION
      * * * * *
     C*
     C                   SELECT
     C*
     C     Q3RECF        WHENEQ    'PRT011'                                     QUEUE THE FAX
     C*** CALL THE QUEUE ASSIGNMENT FILE TO SEE IF WE NEED
     C*** TO RE-ROUTE THE FAX.
     C***
     C                   CALL      'IS305R2'
     C                   PARM      RTIMGQ        $@IMGQ           10
     C                   PARM      *BLANKS       $@RC              1
     C     $@RC          IFEQ      *BLANKS
     C                   MOVEL     RTIMGQ        $@IMGQ
     C                   ENDIF
     C***
     C                   CALL      'IS411R'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    $@IMGQ                         **QUEUE!!!!***
     C                   PARM                    RTDOCT                         **DOCT !!!!***
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE ********
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q3RECF        WHENEQ    'PRT012'                                     PRINT FAX
     C                   CALL      'IS412R'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTOUTQ                         **OUTQ !!!!***
     C                   PARM                    RT#CPY                         **#CPYS!!!!***
     C                   PARM                    RTSPAG                         **STRT PG !***
     C                   PARM                    RTEPAG                         **END PG !!***
     C                   PARM                    RTPDLT                         **PRT THEN DLT*
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE ********
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q3RECF        WHENEQ    'PRT013'                                     NOTIFY
     C                   CALL      'IS413R'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTMSGQ                         **OUTQ !!!!***
     C                   PARM                    RTBMSG                         **#CPYS!!!!***
     C                   PARM                    RTMSG1                         **MSG FLD 1 **
     C                   PARM                    RTMSG2                         **MSG FLD 2 **
     C                   PARM                    RTMSG3                         **MSG FLD 3 ***
     C                   PARM                    RTMSG4                         **MSG FLD 4 ***
     C                   PARM                    RTMSG5                         **MSG FLD 5 ***
     C                   PARM                    RTMSG6                         **MSG FLD 6 ***
     C                   PARM                    RTMSG7                         **MSG FLD 7 ***
     C                   PARM                    RTMSG8                         **MSG FLD 8 ***
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE ********
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q3RECF        WHENEQ    'PRT014'
     C                   CALL      'IS414R'                                     INDEX
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTKEY                          **KEY ********
     C                   PARM                    RTKYTP                         **KEY TYPE ***
     C                   PARM                    RTCOMM                         **COMMENT ****
     C                   PARM                    RTDUSR                         **DIARY USER**
     C                   PARM                    RTRMCD                         **RMK CODE ***
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE ******
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q3RECF        WHENEQ    'PRT015'
     C                   CALL      'IS415R'                                     EMAIL
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTMALU                         **MAIL USER ***
     C                   PARM                    RTMADR                         **MAIL ADDRES**
     C                   PARM                    RTIADR                         **INTERNET ADD*
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE *******
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q3RECF        WHENEQ    'PRT016'
     C                   CALL      'IS416R'                                     USR RCV PGM
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTRCVP                         **USR RCV PGM*
     C                   PARM                    RTRCVL                         **USR RCV LIB**
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE *******
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C                   OTHER
     C                   ENDSL
     C*
     C*
     C     RTKY12        READE     IMRTAL                                 61
     C*
     C                   ENDDO
     C*
     C*
     C     EDTSI1        ENDSR
     C*
     C*
      *================================================================
     C     TSI2          BEGSR
      *================================================================
     C*
     C*
     C*****      RTKY11    SETLLIMRTAL1
     C*****      RTKY11    READEIMRTAL1                  61
     C     RTKY11        CHAIN     IMRTAL1                            61
     C     *IN61         DOWEQ     *OFF
      * * * * *
      * FOUND * * *  FOR EACH ROUTING ACTION RCD, PERFORM FUNCTION
      * * * * *
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'PRT011'                                     QUEUE THE FAX
     C*** CALL THE QUEUE ASSIGNMENT FILE TO SEE IF WE NEED
     C*** TO RE-ROUTE THE FAX.
     C***
     C                   CALL      'IS305R2'
     C                   PARM      RTIMGQ        $@IMGQ           10
     C                   PARM      *BLANKS       $@RC              1
     C     $@RC          IFEQ      *BLANKS
     C                   MOVEL     RTIMGQ        $@IMGQ
     C                   ENDIF
     C***
     C                   CALL      'IS411R'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    $@IMGQ                         **QUEUE!!!!***
     C                   PARM                    RTDOCT                         **DOCT !!!!***
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE ********
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q2RECF        WHENEQ    'PRT012'                                     PRINT FAX
     C                   CALL      'IS412R'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTOUTQ                         **OUTQ !!!!***
     C                   PARM                    RT#CPY                         **#CPYS!!!!***
     C                   PARM                    RTSPAG                         **STRT PG !***
     C                   PARM                    RTEPAG                         **END PG !!***
     C                   PARM                    RTPDLT                         **PRT THEN DLT*
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE ********
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q2RECF        WHENEQ    'PRT013'                                     NOTIFY
     C                   CALL      'IS413R'
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTMSGQ                         **OUTQ !!!!***
     C                   PARM                    RTBMSG                         **#CPYS!!!!***
     C                   PARM                    RTMSG1                         **MSG FLD 1 **
     C                   PARM                    RTMSG2                         **MSG FLD 2 **
     C                   PARM                    RTMSG3                         **MSG FLD 3 ***
     C                   PARM                    RTMSG4                         **MSG FLD 4 ***
     C                   PARM                    RTMSG5                         **MSG FLD 5 ***
     C                   PARM                    RTMSG6                         **MSG FLD 6 ***
     C                   PARM                    RTMSG7                         **MSG FLD 7 ***
     C                   PARM                    RTMSG8                         **MSG FLD 8 ***
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE ********
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q2RECF        WHENEQ    'PRT014'
     C                   CALL      'IS414R'                                     INDEX
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTKEY                          **KEY ********
     C                   PARM                    RTKYTP                         **KEY TYPE ***
     C                   PARM                    RTCOMM                         **COMMENT ****
     C                   PARM                    RTDUSR                         **DIARY USER**
     C                   PARM                    RTRMCD                         **RMK CODE ***
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE ******
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q2RECF        WHENEQ    'PRT015'
     C                   CALL      'IS415R'                                     EMAIL
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTMALU                         **MAIL USER ***
     C                   PARM                    RTMADR                         **MAIL ADDRES**
     C                   PARM                    RTIADR                         **INTERNET ADD*
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE *******
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C     Q2RECF        WHENEQ    'PRT016'
     C                   CALL      'IS416R'                                     USR RCV PGM
     C                   PARM                    ##FAXP           12            FAXCTLR/PORT ID
     C                   PARM                    QQ#PGS            5            #PAGES FOR FAX
     C                   PARM                    ##TSID           20            TRSCRBR ID (20)
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
     C                   PARM                    RTRCVP                         **USR RCV PGM*
     C                   PARM                    RTRCVL                         **USR RCV LIB**
     C                   PARM      *BLANKS       $1RTCD            1            RTNCODE *******
     C*
     C     $1RTCD        IFEQ      'N'
     C*** ERROR???
     C*************** EACH OF THE PGMS 411, 412, ETC. MUST ALSO WRITE
     C**************  DOCUMENT REMARKS ***************
     *** WHAT ABOUT A FAX TRANSACTION LOG??????? THIS IS FOR ERRORS IN THE
     *** ROUTING ROUTINES****!!!!!
     ***
     ***
     C                   ELSE
     *** WHAT ABOUT A FAX TRANSACTION LOG???????
     *** ALSO DOC REMARKS... FROM 411? 412??,.....???
     C*
     C                   ADD       1             #PRCSD
     C                   ENDIF
     C*
     C                   OTHER
     C                   ENDSL
     C*
     C*
     C     RTKY11        READE     IMRTAL1                                61
     C*
     C                   ENDDO
     C*
     C*
     C     EDTSI2        ENDSR
