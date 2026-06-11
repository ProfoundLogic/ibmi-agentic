     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS722R   UPDATE THE DOCUMENT INDEX AND DO RECORD  WITH STATUS
     H*********************************************************************
     H*                                                                   *
     H**** PASS IN THE STATUS THAT DOC  NEED TO BE CHANGED TO., DO NOT!!!
     H**** CHANGE  DOC  THAT ARE ALREADY INDEXED!!!
     H****  CHANGE BOTH THE DO AND IE RECORDS!!!
     F*
     H*                                                                   *
     H*    CALLS TO:                                                      *
     H*                                                                   *
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
      * DOCUMENT INDEX RECORD
     FIMIEL5    UF   E           K DISK
     F*
      * DOCUMENT INDEX RECORD
     FIMDOL6    UF   E           K DISK
     F*
      * BATCH STATUS TABLE FILE
     FITDSL     IF   E           K DISK
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
     D  XXTXT                  4     20
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
     D QCTXT           C                   CONST('Verified, New Sts')

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
     C                   KFLD                    DODCID
     C                   KFLD                    XXASQ#            5 0
     C*
     C*
     C     DNKY2         KLIST
     C                   KFLD                    DODCID
     C*
     C*
     C     DOKY1         KLIST
     C                   KFLD                    QQDCID
     C*
     C*
     C*
     C     IEKY1         KLIST
     C                   KFLD                    DODCID
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12              BTCH ID
     C                   PARM                    QQBSTS            2              BTCH STS
     C                   PARM                    QQFBST            2              FROM BTCH STS
     C                   PARM                    QQCPGM           10              CALLING PGM
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
      *** Process all Docs for the Batch, Chg status of any NON-Indexed
      *** documents.
      *
     C     DOKY1         CHAIN     PDO01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*
     C     QQBSTS        CASEQ     'RL'          SRRL                                RELEASE
     C     QQBSTS        CASEQ     'QC'          SRQC                                QUAL CTL
     C     QQBSTS        CASEQ     'CM'          SRCM                                COMPLETE
     C     QQBSTS        CASEQ     'RJ'          SRRJ                                REJECT
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
     C     DODSTS        IFEQ      QQFBST
     C                   MOVEL     QQBSTS        DODSTS
     C                   UPDATE    PDO01
     C*
      * * * CREATE BATCH NOTES RECORD AS A HISTORY
     C                   EXSR      BNOTES
     C*
     C*
     C*
     C     IEKY1         CHAIN     PIE01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     QQBSTS        IEDSTS
     C                   UPDATE    PIE01
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
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
     C                   MOVEL     DODCID        DNDCID
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
     C*** GET THE BATCH STATUS TABLE RCD. FOR THE DESCRIPTION....
     C*
     C     DSKY1         CHAIN     PDS01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     DSLDSC        XXLDSC           40
     C                   ELSE
     C                   MOVEL     *BLANKS       XXLDSC
     C                   ENDIF
     C*
     C                   MOVEL     '-'           XXHYPH            1
     C                   MOVEL     QQBSTS        XXBSTS            2
     C*** if this is from the QC program, Use Special Text in output
     C     QQCPGM        IFEQ      'IS230R '
     C                   MOVEL     QCTXT         XXTXT
     C                   ENDIF
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
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C*
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     *BLANKS       DNCM75
     C                   MOVE      XXASQ#        DNASQ#
     C*
     C                   MOVEL     *BLANKS       XXA75
     C                   MOVEL     FC(4)         XXA75            75
     C*
     C*
     C*
     C                   MOVEL     Q1USER        XXBUSR           10
     C*
     C*
     C                   MOVEL     XXA75         DNCM75
     C*
     C                   WRITE     PDN01
     C*
     C*
     C*
     C*
     ***********************************************************
     C     ENDBN         ENDSR
     C*
     C*
      *===================================================================
      ***   SRRL     -     UPDATE RECORD TO RELEASE STATUS           *****
      *===================================================================
     C*
     C     SRRL          BEGSR
      *
     C     DOIDFX        IFEQ      'Y'
     C                   MOVEL     '1'           XXNOGO            1
     C                   ENDIF
      *
      *
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
      *
     C     DOIDFX        IFNE      'Y'
     C                   MOVEL     '1'           XXNOGO
     C                   ENDIF
      *
     C*
     C*
     C*
     C     ENDCM         ENDSR
     C*
     C*
      *===================================================================
      ***   SRRJ     -     UPDATE RECORD TO REJECT STATUS            *****
      *===================================================================
     C*
     C     SRRJ          BEGSR
     C*
     C*
      *
     C     DOIDFX        IFEQ      'Y'
     C                   MOVEL     '1'           XXNOGO
     C                   ENDIF
      *
     C*
     C*
     C*
     C     ENDRJ         ENDSR
     C*
     C*
      *===================================================================
      ***   SRQC     -     UPDATE RECORD TO QUALITY CONTROL STATUS   *****
      *===================================================================
     C*
     C     SRQC          BEGSR
     C*
     C*
      *
     C     DOIDFX        IFEQ      'Y'
     C                   MOVEL     '1'           XXNOGO
     C                   ENDIF
      *
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
      *
     C     DOIDFX        IFEQ      'Y'
     C                   MOVEL     '1'           XXNOGO
     C                   ENDIF
      *
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
     C*
      *
      *                22 2  33       34 44 44
**   RMRK ENTRIES      45 7  01       90 23 56
Doc.  History:
   Single Change Sts : xx  -  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
            Date/Time: xx/xx/xxxx     xx:xx:xx
            User     : xxxxxxxxxx
            Error    : xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xx
