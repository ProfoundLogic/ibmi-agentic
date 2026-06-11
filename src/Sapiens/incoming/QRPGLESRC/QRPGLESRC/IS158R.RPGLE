     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE IS158R - REJECT DOCUMENT                                 *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     FIS158F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*
      * DOCUMENT NOTES FILE
     FIMDNL     IF A E           K DISK

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
     D  XXBUSR                57     66
     D  XXBERR                24     75
     D*
     D*
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*
     C*** BATCH STATUS TABLE FILE
     C*
     C     DSKY1         KLIST
     C                   KFLD                    QQBSTS
     C*
     C*
     C*** DOC   NOTES (HISTORY) FILE
     C*
     C     DNKY1         KLIST
     C                   KFLD                    QQDCID
     C                   KFLD                    XXASQ#            5 0
     C*
     C*
     C     DNKY2         KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12              BTCH ID
     C                   PARM                    QQBSTS            2              BTCH STS
     C                   PARM                    QQCPGM           10              CALLING PGM
     C                   PARM                    QQRTCD            1              RTN CODE
     C*
     C     QQRTCD        IFEQ      'L'
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     TOP           TAG
     C                   EXSR      INZSR
      *
      *===============================================================
      *
     C     MAINLN        TAG
      *
      *===============================================================
      * SCREEN EXECUTION AREA
      *
     C                   EXSR      SCREXC
      *
      *===============================================================
      * HANDLE F KEYS IF PRESSED
      *
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
      *
      *===============================================================
     C*
     C*
      *================================================================
      *
     C     SCRLFL        IFEQ      *BLANKS
     C     SCRJFL        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN22
     C                   GOTO      MAINLN
     C                   ENDIF
      *
     C     SCRLFL        IFNE      '1'
     C     SCRJFL        ANDNE     '1'
     C                   MOVE      *ON           *IN23
     C                   GOTO      MAINLN
     C                   ENDIF
      *
     C*
     C     SCRJFL        IFNE      *BLANKS
     C     SCRM01        CABEQ     *BLANKS       MAINLN                   20
     C                   ENDIF
      *
     C     SCRJFL        IFNE      *BLANKS
     C                   MOVEL     'RJ'          QQBSTS
     C                   ELSE
     C                   MOVEL     'RL'          QQBSTS
     C                   ENDIF
      *
      *** Process all Docs for the Batch, Chg status of any NON-Indexed
      *** documents.
      *
     C**** CALL THE 722 PGM HERE???
     C*
     C*
     C                   CALL      'IS722R'
     C                   PARM                    QQDCID
     C                   PARM      QQBSTS        S#DSTS            2
     C                   PARM      'QC'          @@BSTS            2
     C                   PARM      'IS230RA '    QQCPGM           10
     C                   PARM      *BLANK        QQRTNC            1
     C*
     C*
     C     SCRJFL        IFNE      *BLANKS
      * * * CREATE BATCH NOTES RECORD AS A HISTORY
     C                   EXSR      BNOTES
     C*
     C                   ENDIF
     C*
      *
     C*
      *===============================================================
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
      *====================================================================*
      * * *   INZSR SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INZSR         BEGSR
     C*
     C*
     C                   ENDSR
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C                   EXFMT     WDW01
     C*
     C*
     C                   ENDSR
      *
     C/EJECT
      *===============================================================
     C     FKEYS         BEGSR
      *===============================================================
      *
     C     *INKA         IFEQ      '1'
     C                   END
      *
     C     *INKB         IFEQ      '1'
     C                   END
      *
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
     C     *INKD         IFEQ      '1'
     C                   END
      *
      *
     C     *INKE         IFEQ      '1'
     C                   END
      *
      *
     C     *INKF         IFEQ      '1'
     C                   END
      *
      *
     C     *INKG         IFEQ      '1'
     C                   END
      *
      *
     C     *INKH         IFEQ      '1'
     C                   END
      *
      *
     C     *INKI         IFEQ      '1'
     C                   END
      *
      *
     C     *INKJ         IFEQ      '1'
     C                   END
      *
     C     *INKK         IFEQ      '1'
     C                   END
      *
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C*
     C                   END                                                    END *INKL
     C****************************************
     C     *INKM         IFEQ      '1'
     C                   END
      *
     C     *INKN         IFEQ      '1'
     C                   END
      *
     C     *INKP         IFEQ      '1'
     C                   END
      *
     C     *INKQ         IFEQ      '1'
     C                   END
      *
     C     *INKR         IFEQ      '1'
     C                   END
      *
     C     *INKS         IFEQ      '1'
     C                   END
      *
     C     *INKT         IFEQ      '1'
     C                   END
      *
     C     *INKU         IFEQ      '1'
     C                   END
      *
     C     *INKV         IFEQ      '1'
     C                   END
      *
     C     *INKW         IFEQ      '1'
     C                   END
      *
     C     *INKX         IFEQ      '1'
     C                   END
      *
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
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
     C     DNKY1         SETGT     PDN01
     C     DNKY2         READPE    PDN01                                  60
     C     *IN60         IFEQ      *OFF
     C     1             ADD       DNASQ#        XXASQ#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXASQ#
     C                   ENDIF
     C*
     C                   MOVEL     QQDCID        DNDCID
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C                   MOVE      XCURDT        DNCDAT
     C*
     C                   MOVEL     QQCPGM        DNPNAM                         12/31/91 CHG
     C                   MOVEL     XCURDT        DNTRDT
     C                   MOVE      D2TIME        DNTRTM
     C*** SECURITY VALUES NOT CURRENTLY USED ******
     C                   MOVE      *ZEROS        DNSECL
     C                   MOVEL     *BLANKS       DNSBYL
     C*** PROTECT THIS RCD, AS IT IS A HISTORY RECORD - "H" - HISTORY
     C                   MOVEL     'H'           DNPRFL
     C*
     C                   MOVEL     *BLANKS       DNSTAT
     C                   MOVEL     Q1USER        DNIUSR
     C                   MOVEL     Q1PGM         DNIPGM
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        DNITIM
     C     DSDATE        MULT      10000.01      DNIDT
     C*
     C                   MOVEL     *BLANKS       DNLUSR
     C                   MOVEL     *BLANKS       DNLPGM
     C                   MOVE      *ZEROS        DNLTIM
     C                   MOVE      *ZEROS        DNLDT
     C*
     C**** FORMAT THE COMMENT ****
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(1)         XXA75            75
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(2)         XXA75            75
     C*
     C*
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
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
     C                   MOVEL     Q1USER        XXBUSR           10
     C*
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C*
     C     SCRM01        IFNE      *BLANKS
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
      *
     C*
     C                   MOVEL     SCRM01        DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C                   ENDIF
     C*
     C*
     C     SCRM02        IFNE      *BLANKS
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
      *
     C*
     C                   MOVEL     SCRM02        DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C                   ENDIF
     C*
     C*
     C     SCRM03        IFNE      *BLANKS
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
      *
     C*
     C                   MOVEL     SCRM03        DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C                   ENDIF
     C*
     C*
     C     SCRM04        IFNE      *BLANKS
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
      *
     C*
     C                   MOVEL     SCRM04        DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C                   ENDIF
     C*
     C*
     C*
     ***********************************************************
     C     ENDBN         ENDSR
     C*
      *
      *                22 2  33       34 44 44    5   5 5        6
**   RMRK ENTRIES      45 7  01       90 23 56    1   5 7        6
Doc.  History:
    Reject Reasons   :
            Date/Time: xx/xx/xxxx     xx:xx:xx    User: xxxxxxxxxx

            Error    : xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xx
