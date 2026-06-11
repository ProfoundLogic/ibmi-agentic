     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS723R   UPDATE THE BATCH MASTER RECORD
     H*********************************************************************
     H*                                                                   *
     H*                                                                   *
     H*    CALLS TO:                                                      *
     H*                                                                   *
     H*********************************************************************
     H*  REVISIONS:                                                       *
     H**                                                                  *
     H*                                                                   *
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
     FIMBML     UF   E           K DISK
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
     D DS              S              2    DIM(10) ASCEND
     D #D              S              5  0 DIM(10)
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
     C                   KFLD                    XXNEWS
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
     C                   PARM                    QQCPGM           10
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
     C     DOKY1         CHAIN     PDO01                              60
     C     *IN60         IFEQ      *ON
     C*** STATUS FOR BATCH BECOMES COMPLETED
     C                   MOVEL     'CM'          XXNEWS            2
     C                   GOTO      UPDBCH
     C                   ENDIF
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-ADD     1             X                 3 0
     C     DODSTS        LOOKUP    DS(X)                                  61
      *
     C     *IN61         IFNE      *ON
     C                   Z-ADD     1             X                 3 0
     C     *BLANKS       LOOKUP    DS(X)                                  61
     C     *IN61         IFEQ      *ON
     C                   MOVEL     DODSTS        DS(X)
     C                   ENDIF
     C                   ENDIF
      *
     C                   ADD       1             #D(X)
      *
     C     DOKY1         READE     PDO01                                  60
     C                   ENDDO
      *
      *** IF ALL THE DOCS ARE OF ONE STATUS WE USE THAT STATUS FOR THE BATCH
      *
     C                   Z-ADD     1             X
     C     'DL'          LOOKUP    DS(X)                                  62
     C*
     C   62              ADD       #D(X)         XXDL              5 0
     C*
     C                   Z-ADD     1             X
     C     'RL'          LOOKUP    DS(X)                                  62
     C*
     C   62              ADD       #D(X)         XXRL              5 0
     C*
     C                   Z-ADD     1             X
     C     'HD'          LOOKUP    DS(X)                                  62
     C*
     C   62              ADD       #D(X)         XXHD              5 0
     C*
     C                   Z-ADD     1             X
     C     'QC'          LOOKUP    DS(X)                                  62
     C*
     C   62              ADD       #D(X)         XXQC              5 0
     C*
     C*
     C                   Z-ADD     1             X
     C     'RJ'          LOOKUP    DS(X)                                  62
     C*
     C   62              ADD       #D(X)         XXRJ              5 0
     C*
     C***
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    DS(X)                                  63
     C     X             IFEQ      2
     C                   MOVEL     DS(1)         XXNEWS
     C                   GOTO      UPDBCH
     C                   ENDIF
     C*
     C     X             IFEQ      3
     C     XXDL          ANDGT     0
     C                   Z-ADD     1             Y                 5 0
     C     'DL'          LOOKUP    DS(Y)                                  64
     C     *IN64         IFEQ      *ON
     C     Y             IFEQ      1
     C                   MOVEL     DS(2)         XXNEWS
     C                   ELSE
     C                   MOVEL     DS(1)         XXNEWS
     C                   ENDIF
     C                   GOTO      UPDBCH
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRL          IFGT      0
     C     XXQC          ORGT      0
     C*
     C     XXRL          IFGT      XXQC
     C                   MOVEL     'RL'          XXNEWS
     C                   GOTO      UPDBCH
     C                   ELSE
     C                   MOVEL     'QC'          XXNEWS
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     UPDBCH        TAG
     C*
     C     BMKY1         CHAIN     PBM01                              69
     C     *IN69         IFEQ      *OFF
     C                   MOVEL     XXNEWS        BMBSTS
     C                   MOVEL     QQCPGM        BMLPGM                         CALLING PGM
     C                   MOVEL     Q1USER        BMLUSR                         LST MNT USER
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      BMLDT
     C                   Z-ADD     DSTIME        BMLTIM
     C*
     C                   UPDATE    PBM01
      * * * CREATE BATCH NOTES RECORD AS A HISTORY
     C                   EXSR      BNOTES
     C*
     C                   ENDIF
     C*
      *
      *================================================================
     C*
     C*
     C     TERM          TAG
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
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
     C                   MOVEL     XXNEWS        XXBSTS            2
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
