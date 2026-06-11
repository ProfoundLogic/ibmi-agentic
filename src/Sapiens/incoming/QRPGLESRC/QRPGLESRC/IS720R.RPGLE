     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS720R   UPDATE THE BATCH MASTER RECORD
     H*********************************************************************
     H*                                                                   *
     H*                                                                   *
     H*    CALLS TO:                                                      *
     H* * *          IS721R - UPDATE DOCUMENT STATUS'                     *
     H*                                                                   *
     H*********************************************************************
     H*  REVISIONS:                                                       *
     H**                                                                  *
     H*  09/30/02  - 02 - FIXED CHAINS TO LOOK FOR HIT (*IN62 = *OFF)     *
     H*********************************************************************
     H* INDICATOR USAGE                                                   *
     H*                                                                   *
     H* 60  GENERAL CHAIN INDICATOR                                       *
     H* 61  GENERAL CHAIN INDICATOR                                       *
     H*********************************************************************
     H** FILES:                                                          **
     H*********************************************************************
     F*
      * BATCH ADMINISTRATION MASTER FILE
     FIMBML     UF A E           K DISK
     F*
      * BATCH STATUS TABLE FILE
     FITBSL     IF   E           K DISK
     F*
      * BATCH NOTES FILE
     FIMBNL     IF A E           K DISK
     F*
      * DOCUMENT RECORD ***
     FIMDOL5    IF   E           K DISK

     D FC              S             75    DIM(5) CTDATA PERRCD(1)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XCURDT                 1      8  0
     D  XCURYR                 1      4  0
     D  XCURMD                 5      8  0
     D*
     D                 DS
     D  D2DTTM                 1     14  0
     D  D2TIME                 1      6  0
     D  D2DATE                 7     14  0
     D*
     D                 DS
     D  XXA75                  1     75
     D  XXBSTS                24     25
     D  XXBM                  24     25
     D  XXSLH1                26     26
     D  XXBD                  27     28
     D  XXHYPH                28     28
     D  XXSLH2                29     29
     D  XXBYR                 30     33
     D  XXBTMH                39     40
     D  XXCOL1                41     41
     D  XXBTMM                42     43
     D  XXCOL2                44     44
     D  XXBTMS                45     46
     D  XXLDSC                31     70
     D  XXBUSR                24     33
     D  XXBERR                24     75
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*** BATCH ADMINISTRATION FILE
     C*
     C     BMKY1         KLIST
     C                   KFLD                    QQBCHI
     C*
     C*** BATCH STATUS TABLE FILE
     C*
     C     BSKY1         KLIST
     C                   KFLD                    QQBSTS
     C*
     C*
     C*** BATCH NOTES (HISTORY) FILE
     C*
     C     BNKY1         KLIST
     C                   KFLD                    QQBCHI
     C                   KFLD                    XXASQ#            5 0
     C*
     C*
     C     BNKY2         KLIST
     C                   KFLD                    QQBCHI
     C*
     C     DOKY1         KLIST
     C                   KFLD                    QQBCHI
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQBCHI           12              BTCH ID
     C                   PARM                    QQBSTS            2              BTCH STS
     C                   PARM                    QQDOCT            8              DOC TYPE
     C                   PARM                    QQSREP           10              SCAN REP
     C                   PARM                    QQSDT             8              SCAN DT (A)
     C                   PARM                    QQSTM             6              SCAN TM (A)
     C                   PARM                    QQE#DC            5              EXP # DOCS (A)
     C                   PARM                    QQA#DC            5              ACT # DOCS(A)
     C                   PARM                    QQE#PG            5              EXP # PGS (A)
     C                   PARM                    QQA#PG            5              ACT # DOCS(A)
     C                   PARM                    QQ#PGP            3              # PGS PER (A)
     C                   PARM                    QQDSEP            1              USE DOC SEP?
     C                   PARM                    QQCPGM           10              CALLING PGM
     C                   PARM                    QQBERR           52              Batch Error
     C                   PARM                    QQBDSC           40              Batch DESC
     C                   PARM                    QQRTCD            1              RTN CODE
     C*
     C*
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
      *================================================================
      *
      *** CREATE THE BATCH ADMIN RCD IF IT DOESNT EXIST.
      ***  CHANGE THE STS CODE TO VALUE PASSED IN.
      ***  DEPENDING ON QQBSTS, WE WILL PERFORM OTHER FUNCTIONS
      ***     (IE - CALLING ANOTHER PGM TO PROCESS ALL DOCS OF A BATCH)
      *
     C*
     C     BMKY1         CHAIN     PBM01                              69
     C     *IN69         IFEQ      *OFF
     C                   MOVEL     BMBSTS        SVBSTS            2
     C                   ELSE
     C                   MOVEL     *BLANKS       SVBSTS            2
     C                   ENDIF
     C*
     C*
     C     QQBSTS        CASEQ     'SC'          SRSC                                SCAN
     C     QQBSTS        CASEQ     'RL'          SRRL                                RELEASE
     C     QQBSTS        CASEQ     'HD'          SRHD                                HOLD
     C     QQBSTS        CASEQ     'QC'          SRQC                                QUAL CTL
     C     QQBSTS        CASEQ     'CM'          SRCM                                COMPLETE
     C     QQBSTS        CASEQ     'IX'          SRIX                                INDEX
     C     QQBSTS        CASEQ     'RJ'          SRRJ                                REJECT
     C     QQBSTS        CASEQ     'RS'          SRRS                                RESCAN
     C     QQBSTS        CASEQ     'ER'          SRER                                ERROR
     C     QQBSTS        CASEQ     'DL'          SRDL                                DELETED
     C                   CAS                     SRERR                               PGM ERROR
     C                   ENDCS
     C*
     C     XXNOGO        IFEQ      '1'
     C                   MOVEL     '1'           QQRTCD
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVEL     ' '           QQRTCD
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       BMSTAT                         RCD STS
     C                   MOVEL     QQCPGM        BMLPGM                         CALLING PGM
     C                   MOVEL     Q1USER        BMLUSR                         LST MNT USER
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      BMLDT
     C                   Z-ADD     DSTIME        BMLTIM
     C*
      * * * CREATE BATCH NOTES RECORD AS A HISTORY
     C                   EXSR      BNOTES
     C*
      * * * IF RCD EXISTS, UPDATE, ELSE WRITE THE NEW BATCH RECORD
     C     *IN69         IFEQ      *OFF
     C*
     C                   UPDATE    PBM01
     C*
     C                   ELSE
     C                   MOVEL     QQCPGM        BMIPGM                         CALLING PGM
     C                   MOVEL     Q1USER        BMIUSR                         LST MNT USER
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      BMIDT
     C                   Z-ADD     DSTIME        BMITIM
     C*
     C                   WRITE     PBM01
     C                   ENDIF
     C*
     C* * * CALL THE PROGRAM (721) TO UPDATE THE IDIVIDUAL DOCUMENT RCDS
      * * *   DOCS ARE UPDATED ONLY WHEN THEY ARE NOT ALREADY INDEXED.
      * * *    ETC.
     C*
     C                   CALL      'IS721R'
     C                   PARM                    QQBCHI
     C                   PARM                    QQBSTS
     C                   PARM                    SVBSTS
     C                   PARM                    QQCPGM
     C                   PARM      *BLANK        QQRTCN            1
     C*
     C*** CHECK TO SEE IF STS WAS CHANGED PROPERLY?!!!!!!
     C*
      *
      *================================================================
     C*
     C*
     C     TERM          TAG
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
      *===================================================================
      ***   SRSC     -     UPDATE RECORD TO SCAN STATUS              *****
      *===================================================================
     C*
     C     SRSC          BEGSR
     C*
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C                   MOVEL     QQDOCT        BMDOCT
     C                   MOVEL     QQSREP        BMSREP
     C                   MOVE      QQSDT         BMSDT
     C                   MOVE      QQSTM         BMSTM
     C                   MOVE      QQE#DC        BME#DC
     C** ACTUAL NOT AVAIL  MOVE QQA#DC    BMA#DC **** YET ******
     C                   MOVE      QQE#PG        BME#PG
     C** ACTUAL NOT AVAIL  MOVE QQA#PG    BMA#PG **** YET ******
     C                   MOVE      QQ#PGP        BM#PGP
     C                   MOVEL     QQDSEP        BMDSEP
     C                   MOVEL     QQBDSC        BMBDSC
     C*
     C*
     C     ENDSC         ENDSR
     C*
     C*
      *===================================================================
      ***   SRRL     -     UPDATE RECORD TO RELEASE STATUS           *****
      *===================================================================
     C*
     C     SRRL          BEGSR
     C*
     C*
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
      *
     C*** UPDATE THE ACTUAL VALUES IF NOT ALREADY THERE!!!
     C     BMA#DC        IFEQ      *ZEROS
     C                   MOVE      QQA#DC        BMA#DC
     C                   ENDIF
      *
     C     BMA#PG        IFEQ      *ZEROS
     C                   MOVE      QQA#PG        BMA#PG
     C                   ENDIF
     C     ENDRL         ENDSR
     C*
     C*
      *===================================================================
      ***   SRCM     -     UPDATE RECORD TO COMPLETE STATUS          *****
      *===================================================================
     C*
     C     SRCM          BEGSR
     C*
     C*
     C*
     C     DOKY1         CHAIN     PDO01                              62
     C***** CHG 09/30/02 ***************************************
     C*** CHG ** *IN62     IFEQ *ON
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C                   MOVEL     ' '           XXNOGO
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           XXNOGO            1
     C*
     C                   ENDIF
     C*
     C*
     C*
     C*
     C*
     C     ENDCM         ENDSR
     C*
     C*
      *===================================================================
      ***   SRIX     -     UPDATE RECORD TO INDEXING STATUS          *****
      *===================================================================
     C*
     C     SRIX          BEGSR
     C*
     C*
     C*
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C*
     C*
     C     ENDIX         ENDSR
     C*
     C*
      *===================================================================
      ***   SRHD     -     UPDATE RECORD TO HOLD STATUS              *****
      *===================================================================
     C*
     C     SRHD          BEGSR
     C*
     C*
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C*
     C*
      *
     C*** UPDATE THE ACTUAL VALUES IF NOT ALREADY THERE!!!
     C     BMA#DC        IFEQ      *ZEROS
     C                   MOVE      QQA#DC        BMA#DC
     C                   ENDIF
      *
     C     BMA#PG        IFEQ      *ZEROS
     C                   MOVE      QQA#PG        BMA#PG
     C                   ENDIF
     C*
     C*
     C     ENDHD         ENDSR
     C*
     C*
      *===================================================================
      ***   SRRJ     -     UPDATE RECORD TO REJECT STATUS            *****
      *===================================================================
     C*
     C     SRRJ          BEGSR
     C*
     C*
     C*
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C*
     C*
     C     ENDRJ         ENDSR
     C*
     C*
      *===================================================================
      ***   SRRS     -     UPDATE RECORD TO RESCAN STATUS            *****
      *===================================================================
     C*
     C     SRRS          BEGSR
     C*
     C*
     C*
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C*
     C*
     C     ENDRS         ENDSR
     C*
     C*
      *===================================================================
      ***   SRER     -     UPDATE RECORD TO ERROR  STATUS            *****
      *===================================================================
     C*
     C     SRER          BEGSR
     C*
     C*
     C*
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C*
     C*
     C     ENDER         ENDSR
     C*
     C*
      *===================================================================
      ***   SRQC     -     UPDATE RECORD TO QUALITY CONTROL STATUS   *****
      *===================================================================
     C*
     C     SRQC          BEGSR
     C*
     C*
     C*
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C*
     C*
      *
     C*** UPDATE THE ACTUAL VALUES IF NOT ALREADY THERE!!!
     C     BMA#DC        IFEQ      *ZEROS
     C                   MOVE      QQA#DC        BMA#DC
     C                   ENDIF
      *
     C     BMA#PG        IFEQ      *ZEROS
     C                   MOVE      QQA#PG        BMA#PG
     C                   ENDIF
     C*
     C*
     C     ENDQC         ENDSR
     C*
     C*
      *===================================================================
      ***   SRDL     -     UPDATE RECORD TO DELETED STATUS           *****
      *===================================================================
     C*
     C     SRDL          BEGSR
     C*
     C     DOKY1         CHAIN     PDO01                              62
     C***** CHG 09/30/02 ***************************************
     C*** CHG *  *IN62     IFEQ *ON
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     QQBSTS        BMBSTS
     C                   MOVEL     QQBCHI        BMBCHI
     C                   MOVEL     ' '           XXNOGO
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           XXNOGO            1
     C*
     C                   ENDIF
     C*
     C*
     C     ENDDL         ENDSR
     C*
     C*
      *===================================================================
      ***   SRERR    -     ERRROR - STATUS NOT VALID                 *****
      *===================================================================
     C*
     C     SRERR         BEGSR
     C*
     C*
     C     ENDERR        ENDSR
     C*
     C*
      *===================================================================
      ***   BNOTES   -     CREATE BATCH NOTE                         *****
      *===================================================================
     C*
     C     BNOTES        BEGSR
     C*
     C                   TIME                    D2DTTM
     C                   MOVE      D2DATE        XCURYR
     C                   MOVEL     D2DATE        XCURMD
     C*
     C                   Z-ADD     *HIVAL        XXASQ#
     C     BNKY1         SETGT     PBN01
     C     BNKY2         READPE    PBN01                                  60
     C     *IN60         IFEQ      *OFF
     C     1             ADD       BNASQ#        XXASQ#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXASQ#
     C                   ENDIF
     C*
     C                   MOVEL     QQBCHI        BNBCHI
     C                   MOVEL     *BLANKS       BNCM75
     C                   MOVE      XXASQ#        BNASQ#
     C                   MOVE      XCURDT        BNCDAT
     C*
     C                   MOVEL     QQCPGM        BNPNAM                         12/31/91 CHG
     C                   MOVEL     XCURDT        BNTRDT
     C                   MOVE      D2TIME        BNTRTM
     C*** SECURITY VALUES NOT CURRENTLY USED ******
     C                   MOVE      *ZEROS        BNSECL
     C                   MOVEL     *BLANKS       BNSBYL
     C*** PROTECT THIS RCD, AS IT IS A HISTORY RECORD - "H" - HISTORY
     C                   MOVEL     'H'           BNPRFL
     C*
     C                   MOVEL     *BLANKS       BNSTAT
     C                   MOVEL     Q1USER        BNIUSR
     C                   MOVEL     Q1PGM         BNIPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        BNITIM
     C     DSDATE        MULT      10000.01      BNIDT
     C*
     C                   MOVEL     *BLANKS       BNLUSR
     C                   MOVEL     *BLANKS       BNLPGM
     C                   MOVE      *ZEROS        BNLTIM
     C                   MOVE      *ZEROS        BNLDT
     C*
     C**** FORMAT THE COMMENT ****
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(1)         XXA75            75
     C                   MOVEL     XXA75         BNCM75
     C*
     C                   WRITE     PBN01
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       BNCM75
     C                   MOVE      XXASQ#        BNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(2)         XXA75            75
     C*
     C*** GET THE BATCH STATUS TABLE RCD. FOR THE DESCRIPTION....
     C*
     C     BSKY1         CHAIN     PBS01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     BSLDSC        XXLDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXLDSC
     C                   ENDIF
     C*
     C                   MOVEL     '-'           XXHYPH            1
     C                   MOVEL     QQBSTS        XXBSTS            2
     C*
     C                   MOVEL     XXA75         BNCM75
     C*
     C                   WRITE     PBN01
     C*
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       BNCM75
     C                   MOVE      XXASQ#        BNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(3)         XXA75            75
     C*
     C*
     C                   MOVEL     D2DATE        XXBM              2
     C                   MOVE      D2DATE        XXBYR             4
     C                   MOVE      D2DATE        XXCHR8            8
     C     2             SUBST     XXCHR8:3      XXBD              2
     C*
     C                   MOVEL     D2TIME        XXBTMH            2
     C                   MOVE      D2TIME        XXBTMS            2
     C                   MOVE      D2TIME        XXCHR6            6
     C     2             SUBST     XXCHR6:3      XXBTMM            2
     C*
     C                   MOVEL     '/'           XXSLH1            1
     C                   MOVEL     '/'           XXSLH2            1
     C                   MOVEL     ':'           XXCOL1            1
     C                   MOVEL     ':'           XXCOL2            1
     C*
     C                   MOVEL     XXA75         BNCM75
     C*
     C                   WRITE     PBN01
     C*
     C*
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       BNCM75
     C                   MOVE      XXASQ#        BNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(4)         XXA75            75
     C*
     C*
     C*
     C                   MOVEL     Q1USER        XXBUSR           10
     C*
     C*
     C                   MOVEL     XXA75         BNCM75
     C*
     C                   WRITE     PBN01
     C*
     C*
     C*
     C     QQBERR        IFNE      *BLANKS
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       BNCM75
     C                   MOVE      XXASQ#        BNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(5)         XXA75            75
     C*
     C*
     C*
     C                   MOVEL     QQBERR        XXBERR
     C*
     C*
     C                   MOVEL     XXA75         BNCM75
     C*
     C                   WRITE     PBN01
     C*
     C                   ENDIF
     C*
     ***********************************************************
     C     ENDBN         ENDSR
     C*
      *
      *                22 2  33       34 44 44
**   RMRK ENTRIES      45 7  01       90 23 56
Batch History:
            Status   : xx  -  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
            Date/Time: xx/xx/xxxx     xx:xx:xx
            User     : xxxxxxxxxx
            Error    : xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xx
