     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE APB530 - REMOVE CLEARED CHECKS                          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      READ/DOWEQ OF OPEN QUERY REMARKS DETAIL FILE    *
     H*       60      GENERAL USAGE (CHAIN)                           *
      *****************************************************************
     FAPB530F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FBKCRL2    UF   E           K DISK
     FAPVML1    UF   E           K DISK
     FAPCHL1    UF   E           K DISK
     FAPBKL1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      4  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFAC#
     C*
     C*   KEY LIST FOR THE CLEARED CHECK FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXAPP
     C                   KFLD                    XXBNK#
     C*
     C*   KEY LIST FOR THE CLEARED CHECK FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    CRCO#
     C                   KFLD                    CRFAC#
     C                   KFLD                    CRVDR
     C*
     C*   DEFINE THE PARM LIST.
     C*
     C     *ENTRY        PLIST
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
     C*  INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CRCO#         XXCO#
     C     *LIKE         DEFINE    CRFAC#        XXFAC#
     C     *LIKE         DEFINE    CRVDR         XXVDR
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C                   MOVEL     Q1USER        SCUSER
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C                   MOVE      '1'           QQRTCD
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKD020R'
     C                   PARM                    SCBNK#
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN34
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C                   END                                                    END *INKF
     C**
     C                   END                                                    END CMDKEYS
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  GET THE REQUESTED BANK RECORD.
     C*
     C     SCBNK#        IFGT      *ZERO
     C     SCBNK#        CHAIN     PBK01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   MOVE      SCBNK#        XXBNK#            3 0
     C                   EXCEPT    HEAD
     C*
     C*** BEGIN READING FILE
     C*
     C     QQMODE        IFEQ      'A'
     C                   MOVE      'AP'          XXAPP             2
     C                   ELSE
     C                   MOVE      'PR'          XXAPP
     C                   END
     C*
     C     CRKEY         SETLL     PCR01
     C     CRKEY         READE     PCR01                                  50
     C*
     C*** IF NO RECORDS SELECTED, PRINT ERROR MESSAGE AND END PROGRAM
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C     CRCLR         IFEQ      *BLANKS
     C                   GOTO      READ
     C                   END
     C*
     C     CRVDR         CHAIN     PVM01                              98
     C   98              MOVE      *BLANKS       VMNAME
     C  N98              EXCEPT    PVM01P
     C                   MOVEL     VMNAME        XMNAME           25
0056 C*
     C                   Z-ADD     CRCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCHKD            8 0
     C*
0059 C                   ADD       CRAMT         TOTAMC           11 2
0052 C                   ADD       1             TOTCLR            5 0
     C*
     C*** PRINT LAST RECORD READ
     C*
     C     PRTHD         IFEQ      'X'
     C                   EXCEPT    HEAD
     C                   MOVE      *BLANKS       PRTHD
     C                   END
     C                   EXCEPT    DET1
     C     *INOF         IFEQ      '1'
     C                   MOVE      'X'           PRTHD             1
     C                   END
     C*
     C*** DELETE CHECK RECORD
     C*
     C                   DELETE    PCR01
     C                   MOVEL     CRVDR         TEST              3 0
     C     TEST          IFEQ      999
     C     CRVDR         CHAIN     PVM01                              60
     C  N60              DELETE    PVM01
     C     CHKEY         SETLL     PCH01
     C     CHGCH         TAG
     C     CHKEY         READE     PCH01                                  60
     C  N60              Z-ADD     9999999       CHVDR
     C  N60              UPDATE    PCH01
     C  N60              GOTO      CHGCH
     C  N60              Z-ADD     9999999       CHVDR
     C                   END
     C*
     C*** READ NEXT RECORD.
     C*
     C     READ          TAG
     C     CRKEY         READE     PCR01                                  50
     C                   END                                                    END DOWEQ 50
     C*
     C*** PRINT FINAL TOTALS
     C*
     C                   EXCEPT    TOTC
     C*
     C*** END PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OQSYSPRT   E            HEAD           1 06
     O                       *DATE         Y     10
00640O                                           76 'DROPPED CHECKS REPORT'
     O                       DSTIME             130 '  :  '
     O          E            HEAD           2
     O                                            7 'APB530 '
0073 O                       XXBNK#        Z     13
0074 O                                           15 '-'
0076 O                       BKNAME              56
     O                                          125 'PAGE:'
     O                       PAGE               130
00490O          E            HEAD           2
00530O                                            7 'CHECK#'
00540O                                           27 'CHECK-AMT'
00550O                                           35 'DATE'
0056 O                                           51 'VENDOR#/NAME'
00570O                                           80 'STATUS'
00510O                                           85 'CO#'
00520O                                           89 'FAC'
00630O          E            DET1           1
0066 O                       CRCHK#        Z      7
0067 O                       CRAMT         J     28
0068 O                       XXCHKD        Y     39
0069 O                       CRVDR         Z     48
0069 O                       XMNAME              74
0070 O                       CRCLR               79
0065 O                       CRCO#               87
0065 O                       CRFAC#              91
0071 O          E            TOTC        1  1
0074 O                       TOTCLR        ZB    10
0075 O                       TOTAMC        JB    28
0076 O                                           41 'DROPPED    '
     O*
     O**** ERROR MESSAGE, IF NO RECORDS TO BE PROCESSED *************
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
     OPVM01     E            PVM01P
