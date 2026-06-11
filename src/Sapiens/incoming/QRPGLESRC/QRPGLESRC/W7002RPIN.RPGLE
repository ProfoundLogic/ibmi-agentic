     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7002R - CLAIMS CHECKS
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*   JJJ REVISED 9-19-89 TO PRINT CHECKS IN ORDER BY CO# FND FEIN*
     H*   MOD AMT (OMIT RPSTAT NE BLANK)                              *
     H*                                                               *
     H* 10/13/93 C:15 PROJECT WC258                                   *
     H*               ONLY PROCESS REQUESTS WHICH HAVE A TRANSACTION  *
     H*               DATE LESS THAN OR EQUAL TO THE BATCH DATE.      *
     H*                                                               *
     H* 09/08/94 C:15 PROJECT: WC361                                  *
     H*               ADDED A PARM FOR GROUP CHECK PROCESSING. ADDED  *
     H*               AN ADDITIONAL SPOOL FILE FOR GROUP CHECKS.      *
     H*****************************************************************
     FWARPL6    IF   E           K DISK
     FWACPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
     F                                     USROPN
     F*

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RPFYR                  1      3  0
     D  XXCTRY                 1      1  0
     D  XXYR                   2      3  0
     D                 DS
     D  RPFRDT                 1      7  0
     D  XXFRYR                 2      3  0
     D  XXFRMO                 4      5  0
     D  XXFRDA                 6      7  0
     D                 DS
     D  RPTODT                 1      7  0
     D  XXTOYR                 2      3  0
     D  XXTOMO                 4      5  0
     D  XXTODA                 6      7  0
     D                 DS
     D  CMACDT                 1      7  0
     D  XXACYR                 2      3  0
     D  XXACMO                 4      5  0
     D  XXACDA                 6      7  0
     D                 DS
     D  RPZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  CLAIM FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CHECK PROCESSING FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
     C*
     C*  REMARK CODE FILE
     C*
     C     RCKY1         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *LIKE         DEFINE    RPTRDT        XXDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            6
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQTYPE            1
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
     C                   MOVE      QQDATE        XXDATE            6 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C     QQTYPE        COMP      'G'                                    07
     C*
     C*  OPEN THE APPROPRIATE PRINTER FILES
     C*
     C  N07              OPEN      QSYSPRT
     C   07              OPEN      QSYSPRT1
     C*
     C*  GET THE ENTRY FROM THE CHECK PROCESSING FILE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     CPKEY         CHAIN     PCP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ' '           XXSTRT
     C*
     C     RPKEY         CHAIN     PRP01                              50        |
     C*
     C*  IF THE REPLACEMENT CHECK IS NOT ZERO, WE NEED TO START WITH
     C*  A CERTAIN CHECK
     C*
     C     *IN50         DOWEQ     '0'
     C     RPTRDT        IFLE      XXDT
     C*
     C     CPRSTR        IFNE      *ZERO
     C     CPRSTR        IFNE      RPCHK#
     C     XXSTRT        IFEQ      ' '
     C                   GOTO      NCHK#
     C                   END
     C                   ELSE
     C                   MOVE      'Y'           XXSTRT            1
     C                   END
     C                   END
     C*
     C*  RETRIEVE CLAIM RECORD TO GET SS#
     C*
     C     CMKEY         CHAIN     WMCML1                             51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       CMSS#
     C                   Z-ADD     0             CMACDT
     C                   Z-ADD     0             CMEMP#
     C                   END
     C*
     C*  LOAD DOLLAR AMOUNT SPELLED OUT
     C*
     C                   MOVE      RPAMT         QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       XXAMT1           60
     C                   MOVE      *BLANKS       XXAMT2           60
     C                   MOVE      QQAMT1        XXAMT1
     C                   MOVE      QQAMT2        XXAMT2
     C*
     C*  RETRIEVE CLAIMANT DEMOGRAPHICS RECORD
     C*
     C     CDKEY         CHAIN     WMCDL                              51
     C     *IN51         IFEQ      '1'
     C                   MOVE      *BLANKS       XXNAME           47
     C                   ELSE
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST
     C                   MOVEL     CDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME
     C                   END
     C*
     C*  RETRIEVE REMARKS
     C*
     C                   MOVEL     RPRMK1        XXKEY
     C     RCKY1         CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM1           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM1
     C                   END
     C*
     C                   MOVEL     RPRMK2        XXKEY
     C     RCKY1         CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM2           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM2
     C                   END
     C*
     C                   MOVEL     RPRMK3        XXKEY
     C     RCKY1         CHAIN     PRC01                              51
     C     *IN51         IFEQ      '0'
     C                   MOVE      RCDESC        XXREM3           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXREM3
     C                   END
     C*
     C                   MOVEL     RPCKCN        XXKEY                          JJJ 1-6-89
     C     RCKY1         CHAIN     PRC01                              51        |  ADDED
     C     *IN51         IFEQ      '0'                                          |
     C                   MOVE      RCDESC        XXREM4           40            |
     C                   ELSE                                                   |
     C                   MOVE      *BLANKS       XXREM4                         |
     C                   END                                                    |
     C*
     C*  SET UP FIELD TO PRINT FUND YEAR
     C*
     C                   MOVE      RPFYR         XXFYR             4
     C     XXCTRY        IFEQ      0
     C                   MOVEL     '19'          XXFYR
     C                   ELSE
     C                   MOVEL     '20'          XXFYR
     C                   END
     C*
     C*  IF GROUP CHECK THEN USE THE FROM AND TO DATES FROM THE CHECK
     C*   PROCESSING FILE
     C*
     C     QQTYPE        IFEQ      'G'
     C                   MOVE      *BLANKS       QQFRDT            6 0
     C                   MOVE      *BLANKS       QQTODT            6 0
     C                   CALL      'W7002R1'
     C                   PARM                    QQCO#
     C                   PARM                    QQDATE
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQFND
     C*
     C                   MOVE      QQFRDT        XXFRDT            6 0
     C                   MOVE      QQTODT        XXTODT            6 0
     C*
     C                   END
     C*
     C*  LOAD CITY, STATE AND ZIP
     C*
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     RPST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     RPCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C*  CHECK FOR A BLANK SECOND ADDRESS LINE
     C*
     C                   MOVE      '0'           *IN61
     C     RPADR2        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN61
     C                   END
     C*
     C*  PRINT CHECK
     C*
     C  N07              EXCEPT    CHECK
     C   07              EXCEPT    CHECK1
     C     NCHK#         TAG
     C*
     C                   END
     C     RPKEY         READE     PRP01                                  50    |
     C*
     C                   END
     C                   END
     C*
     C*  CLOSE THE PRINTER FILES
     C*
     C  N07              CLOSE     QSYSPRT
     C   07              CLOSE     QSYSPRT1
     C*
     C                   SETON                                        LR
     C*
     OQSYSPRT   E            CHECK            06
     O                       RPCHK#        Z     78
     O          E            CHECK            07
     O                       XXDATE        Y     67
     O          E            CHECK          1 10
     O                       XXAMT1              67
     O                       RPAMT               80 '  ,   , $0.  '
     O          E            CHECK          1
     O                       XXAMT2              67
     O          E            CHECK            15
     O                       XXFYR               63
     O                                           73 'FUND YEAR'
     O          E            CHECK          1 17
     O                       RPPNAM              52
     O          E            CHECK          1
     O                       RPADR1              52
     O          E            CHECK          1
     O              N61      RPADR2              52
     O               61      XXCSZ               52
     O          E   N61      CHECK          1
     O                       XXCSZ               52
     O          E            CHECK            27
     O                       RPFND         Z      3
     O                       XXYR                 7
     O                       RPCASE        Z     15
     O                       CMEMP#        Z     21
     O                       XXACMO              25
     O                       XXACDA              29
     O                       XXACYR              32
     O                       XXFRMO              37
     O                       XXFRDA              41
     O                       XXFRYR              45
     O                       XXTOMO              48
     O                       XXTODA              52
     O                       XXTOYR              56
     O                       RPAMT               80 '  ,   , $0.  '
     O          E            CHECK            29
     O                       XXNAME              57
     O          E            CHECK          1 31
     O                       RPRMDS              40
     O          E            CHECK          1
     O                       XXREM1              50
     O          E            CHECK          1
     O                       XXREM2              50
     O          E            CHECK          1
     O                       XXREM3              50
     O          E            CHECK          1
     O                       XXREM4              50
     O          E            CHECK            36
     O                                            9 'FED ID #'
     O                       RPFEIN        Z     19
     O                       RPMOD         Z     23
     O          E            CHECK            38
     O                       XXDATE        Y     67
     OQSYSPRT1  E            CHECK1           06
     O                       RPCHK#        Z     78
     O          E            CHECK1           07
     O                       XXDATE        Y     67
     O          E            CHECK1         1 10
     O                       XXAMT1              67
     O                       RPAMT               80 '  ,   , $0.  '
     O          E            CHECK1         1
     O                       XXAMT2              67
     O          E            CHECK1         1 17
     O                       RPPNAM              52
     O          E            CHECK1         1
     O                       RPADR1              52
     O          E            CHECK1         1
     O              N61      RPADR2              52
     O               61      XXCSZ               52
     O          E   N61      CHECK1         1
     O                       XXCSZ               52
     O          E            CHECK1           33
     O                                           24 'GROUP CHECK DATE RANGE:'
     O                       XXFRDT        Y     33
     O                                           34 '-'
     O                       XXTODT        Y     42
     O          E            CHECK1           36
     O                                            9 'FED ID #'
     O                       RPFEIN        Z     19
     O                       RPMOD         Z     23
     O          E            CHECK1           38
     O                       XXDATE        Y     67
