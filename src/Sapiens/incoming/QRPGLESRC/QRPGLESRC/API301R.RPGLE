     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE API301R - VOUCHER SELECTION MAINTENANCE
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/06/00 C:33 SET OF *IN21 ON SCR01                          *
     H*                                                               *
     H*  04/02/97 C:46 PROJECT AP016                                  *
     H*                DO NOT SELECT THE VOUCHER IF THE TOTAL AMOUNT  *
     H*                FOR ALL VOUCHERS FOR THE VENDOR IS LESS THAN   *
     H*                OR EQUAL TO ZERO.                              *
     H*                                                               *
     H*  07/17/97 C:107 -  USE DEFAULT COMPANY & FAC NUMBERS INSTEAD  *
     H*                    INSTEAD OF DEFAULTING TO 001-001           *
     H*                                                               *
     H******************************************************************
     FAPI301F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPCSL1    UF A E           K DISK
     FAPADL1    IF   E           K DISK
     FAPADL4    IF   E           K DISK
     F                                     RENAME(PAD01:PAD04)
     FAPADL5    IF   E           K DISK
     F                                     RENAME(PAD01:PAD05)
     F*** BEGIN ADD *** 04/02/97 **************************************
     FAPADL6    IF   E           K DISK
     F                                     RENAME(PAD01:PAD06)
     F*** END ADD ***** 04/02/97 **************************************
      *** BEG ADD * 7/17/97
     FAPAPL1    UF   E           K DISK
      *** END ADD * 7/17/97
     FBKCFL1    IF   E           K DISK
     FAPBKL1    IF   E           K DISK
     FAPVML1    IF   E           K DISK
     FBKSYL1    IF   E           K DISK

0076 D*
     D UA              S             10    DIM(3) CTDATA PERRCD(1)
     D MSG             S             65    DIM(3) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D CMDACT          DS
     D  ACT                    1    200
     D QLFDAP          DS
     D  SSPGM                  1     10
     D  SSLIB                 11     20
     D HELPCT          DS
     D  HLP                    1     60

0070 I*                   AUW        40  1                 WORDS
0071 I*                   TABM#   6  12  2 0 TABMON  3     MON
0072 I*                   TABD#   7   7  1 0 TABDAY  3     DAYS
     IPAD06
     I              ADINV#                      A6INV#
     I              ADPODS                      A6PODS
     I              ADAMT                       A6AMT
     I              ADDISC                      A6DISC
     I              ADBNK#                      A6BNK#
     I              ADSEP                       A6SEP
     I              ADSEQ1                      A6SEQ1
     I              ADVDR                       A6VDR
     I              ADCO#                       A6CO#
     I              ADFAC#                      A6FAC#
     I              ADINVD                      A6INVD
     I              ADDUED                      A6DUED
     I              ADPONO                      A6PONO

     C*
     C*
     C     *LIKE         DEFINE    CSSEQ1        SCSEQ1
     C*** BEGIN ADD *** 04/02/97 **************************************
     C     *LIKE         DEFINE    ADAMT         TOAMT
     C*** END ADD ***** 04/02/97 **************************************
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCALL            1
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
     C*   PARM LIST FOR WINDOWS RETRIEVE PARMS.
     C*
     C     RTVPRM        PLIST
     C                   PARM      QLFDAP        PGMLIB           20
     C     CMDACT        PARM                    CMDACT
     C     BARDTA        PARM                    BARDTA           79
     C     INFOA1        PARM                    INFOA1           78
     C     INFOA2        PARM                    INFOA2           78
     C     HELPCT        PARM                    HELPCT
     C                   PARM      'A'           SSTYPE            1
     C     SSFLG1        PARM                    SSFLG1            1
     C     SSFLG2        PARM                    SSFLG2            1
     C     SSFLG3        PARM                    SSFLG3            1
     C     SSFLG4        PARM                    SSFLG4            1
     C     SSFLG5        PARM                    SSFLG5            1
     C                   PARM                    BARDSC           50
     C*
     C*   PARM LIST FOR WINDOWS CALL.
     C*
     C     EXCPRM        PLIST
     C                   PARM      QLFDAP        PGMLIB
     C                   PARM      'A'           SSTYPE            1
     C     ACTION        PARM      ACTION        ACTION            8
     C                   PARM      HELPCT        HELPCT
     C                   PARM      CMDACT        CMDACT
     C                   PARM      SSFLG1        SSFLG1            1
     C                   PARM      SSFLG2        SSFLG2            1
     C                   PARM      SSFLG3        SSFLG3            1
     C                   PARM      SSFLG4        SSFLG4            1
     C                   PARM      SSFLG5        SSFLG5            1
     C     SSEXT         PARM      ' '           SSEXT             1
     C     SSCNL         PARM      ' '           SSCNL             1
     C     SSRTN         PARM      ' '           SSRTN             1
     C*
     C*
     C*   KEY LIST FOR THE VOUCHER SELECTION FILE.
     C*
     C     CSKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCINV#
     C                   KFLD                    SCSEQ1
     C*
     C*   KEY LIST FOR THE VOUCHER ENTRY FILE.
     C*
     C     ADKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCSEQ1
     C*
     C*   KEY LIST FOR THE VOUCHER ENTRY FILE.
     C*
     C     ADKEY1        KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C*** BEGIN ADD *** 04/02/97 **************************************
     C*
     C*   KEY LIST FOR THE VOUCHER ENTRY FILE BY VENDOR
     C*
     C     ADKEY2        KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C*** END ADD ***** 04/02/97 **************************************
     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C************         MOVE '1'       *IN34
     C                   MOVEL     '1'           SCCODE
      *** BEG CHG * 7/17/97 * GET DEFAULT COMPANY & FAC NUMBERS
     C*****                MOVE '001'     SCCO#
     C*****                MOVE '001'     SCFAC#
     C*
     C     'AP'          CHAIN     PAP01                              60
     C     *IN60         IFEQ      '0'
     C     APCO#         IFGT      0
     C                   Z-ADD     APCO#         DFTCO             3 0
     C                   ELSE
     C                   MOVE      '001'         DFTCO
     C                   END
     C     APFAC#        IFGT      0
     C                   Z-ADD     APFAC#        DFTFAC            3 0
     C                   ELSE
     C                   MOVE      '001'         DFTFAC
     C                   END
     C                   EXCEPT    PAP01E
     C                   MOVE      DFTCO         SCCO#
     C                   MOVE      DFTFAC        SCFAC#
     C                   ELSE
     C                   MOVE      '001'         SCCO#
     C                   MOVE      '001'         SCFAC#
     C                   END                                                    END *IN60
      *** END CHG * 7/17/97 * GET DEFAULT COMPANY & FAC NUMBERS
     C     'BK'          CHAIN     PSY01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVE      'KLBEFCB '    SSPGM
     C                   MOVEL     '*LIBL'       SSLIB
     C                   MOVEL     'ACTION'      ACTBAR           10
     C                   MOVE      'BAR '        ACTBAR
     C                   CALL      'QAB155'      RTVPRM
     C                   MOVE      *BLANKS       ACTION
     C                   MOVEL     SYPOP4        ACTION
     C                   END
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
      * CLEAR MSG LINE
     C                   CLEAR                   SCCNT
     C                   CLEAR                   SCMSG
      *
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
     C*   F4 = SEARCH SELECTION LIST             *
     C********************************************
     C     *INKD         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'APD301R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    SCVDR
     C                   PARM                    SCINV#
     C                   PARM                    SCSEQ1
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      SCR02L
     C*
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKD
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'APD010R'
     C                   PARM                    SCVDR
     C                   PARM                    QQTYPE            1
     C                   PARM      '1'           QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN35
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR01
     C*
     C                   END                                                    END *INKE
     C********************************************
     C*   F6 = OPERATIONS INQUIRY                *
     C********************************************
     C     *INKF         IFEQ      '1'
     C     SYPOP4        ANDNE     *BLANK
     C     SYINS4        ANDEQ     'Y'
     C                   MOVE      *BLANK        ACTION
     C                   MOVEL     SYPOP4        ACTION
     C                   CALL      'QAB152'      EXCPRM
     C                   GOTO      STRT
     C                   END                                                    END *INKF
     C********************************************
     C*   F9= COMPANY FACILITY INQUIRY          *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'BKD001R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN34
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C                   END                                                    END *INKI
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C**
     C                   END                                                    END CMDKEYS
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  TEST THE ACTION CODE FOR 1,2,3, OR 4.
     C*
     C     SCCODE        IFNE      '1'
     C     SCCODE        ANDNE     '2'
     C     SCCODE        ANDNE     '3'
     C     SCCODE        ANDNE     '4'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE SELECTION DATE MUST BE VALID IF OPTION 1 OR 2.
     C*
     C     SCCODE        IFNE      '3'
     C     SCCODE        ANDNE     '4'
     C     SCDATE        IFEQ      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
      *
     C                   END
     C                   END
     C*
     C*  GET THE REQUESTED COMPANY/FACILITY RECORD.
     C*
     C     CFKEY         CHAIN     PCF01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   MOVEL     CFNAME        SCCFNM
      *
      * RESET VOUCHER SELECT COUNTER FIELD AND MSG LINE
      *
     C                   CLEAR                   VSCNT
      *
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C*   PROCESS SELECTED DATA
     C*
     C*  GET THE REQUESTED VOUCHERS BY INVOICE DATE.
     C*
     C     SCCODE        IFEQ      '1'
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SCIDT             7 0
     C     ADKEY1        CHAIN     PAD04                              60
     C     AD1           TAG
     C     ADINVD        IFLE      SCIDT
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ADCO#         SCCO#
     C                   MOVE      ADFAC#        SCFAC#
     C                   MOVE      ADVDR         SCVDR
     C                   MOVE      ADINV#        SCINV#
     C                   MOVE      ADSEQ1        SCSEQ1
     C     CSKEY         CHAIN     PCS01                              40
     C*
     C********   *IN40     IFEQ '1'
     C     *IN40         IFEQ      '0'
     C     CSCHK#        ANDGT     *ZERO
     C                   GOTO      READ
     C                   END
     C*** BEGIN ADD *** 04/02/97 **************************************
     C*
     C*  ADD UP ALL THE AMOUNTS FOR THIS VENDOR.
     C*
     C                   Z-ADD     *ZERO         TOAMT
     C*
     C     ADKEY2        CHAIN     PAD06                              55
     C     *IN55         DOWEQ     '0'
     C     A6INVD        ANDLE     SCIDT
     C                   ADD       A6AMT         TOAMT
     C     ADKEY2        READE     PAD06                                  55
     C                   END
     C*
     C*  IF THE TOTAL FOR VENDOR IS LESS THAN OR EQUAL TO ZERO, DON'T
     C*  SELECT THIS VOUCHER.
     C*
     C     TOAMT         IFLE      *ZERO
     C                   GOTO      READ
     C                   END
     C*
     C*** END ADD ***** 04/02/97 **************************************
     C                   MOVE      ADINV#        CSINV#
     C                   MOVEL     ADPODS        CSPODS
     C                   Z-ADD     ADAMT         CSAMT
     C                   Z-ADD     ADDISC        CSDISC
     C                   Z-ADD     ADBNK#        CSBNK#
     C                   MOVE      ADSEP         CSSEP
     C                   MOVE      Q1USER        CSCUSR
     C                   MOVE      Q1PGM         CSIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSCDT
     C                   Z-ADD     DSTIME        CSCTIM
     C                   MOVE      ADSEQ1        CSSEQ1
     C                   MOVE      ADVDR         CSVDR
     C                   MOVE      ADCO#         CSCO#
     C                   MOVE      ADFAC#        CSFAC#
     C                   MOVE      Q1USER        CSCUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSIDT
     C                   Z-ADD     DSTIME        CSITIM
     C                   Z-ADD     *ZERO         CSCHK#
     C                   Z-ADD     *ZERO         CSCHKD
     C                   MOVE      *BLANKS       CSSEP
     C     *IN40         IFEQ      '1'
     C                   ADD       1             VSCNT             5 0
     C                   WRITE     PCS01
     C                   ELSE
     C                   UPDATE    PCS01
     C                   END                                                    WRITE *IN40
     C                   END                                                          *IN60
     C*
     C     READ          TAG
     C     ADKEY1        READE     PAD04                                  60
     C  N60              GOTO      AD1
      *
     C*
     C                   END                                                    DATE COMP
      * LOAD MSG FIELD APPROPRIATLY
     C                   EXSR      SCMSR
     C                   END                                                    CODE = 1
     C*
     C*   PROCESS SELECTED DATA
     C*
     C*  GET THE REQUESTED VOUCHERS BY INVOICE DATE.
     C*
     C     SCCODE        IFEQ      '2'
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SCIDT             7 0
     C     ADKEY1        CHAIN     PAD05                              60
     C     AD2           TAG
     C     ADDUED        IFLE      SCIDT
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ADCO#         SCCO#
     C                   MOVE      ADFAC#        SCFAC#
     C                   MOVE      ADVDR         SCVDR
     C                   MOVE      ADINV#        SCINV#
     C                   MOVE      ADSEQ1        SCSEQ1
     C     CSKEY         CHAIN     PCS01                              40
     C*
     C*********  *IN40     IFEQ '1'
     C     *IN40         IFEQ      '0'
     C     CSCHK#        ANDGT     *ZERO
     C                   GOTO      READ2
     C                   END
     C*** BEGIN ADD *** 04/02/97 **************************************
     C*
     C*  ADD UP ALL THE AMOUNTS FOR THIS VENDOR.
     C*
     C                   Z-ADD     *ZERO         TOAMT
     C*
     C     ADKEY2        CHAIN     PAD06                              55
     C     *IN55         DOWEQ     '0'
     C     A6DUED        ANDLE     SCIDT
     C                   ADD       A6AMT         TOAMT
     C     ADKEY2        READE     PAD06                                  55
     C                   END
     C*
     C*  IF THE TOTAL FOR VENDOR IS LESS THAN OR EQUAL TO ZERO, DON'T
     C*  SELECT THIS VOUCHER.
     C*
     C     TOAMT         IFLE      *ZERO
     C                   GOTO      READ2
     C                   END
     C*
     C*** END ADD ***** 04/02/97 **************************************
     C                   MOVE      ADINV#        CSINV#
     C                   MOVE      ADINV#        CSINV#
     C                   MOVEL     ADPODS        CSPODS
     C                   Z-ADD     ADAMT         CSAMT
     C                   Z-ADD     ADDISC        CSDISC
     C                   MOVE      ADSEP         CSSEP
     C                   Z-ADD     ADBNK#        CSBNK#
     C                   MOVE      Q1USER        CSCUSR
     C                   MOVE      Q1PGM         CSIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSCDT
     C                   Z-ADD     DSTIME        CSCTIM
     C                   MOVE      ADSEQ1        CSSEQ1
     C                   MOVE      ADVDR         CSVDR
     C                   MOVE      ADCO#         CSCO#
     C                   MOVE      ADFAC#        CSFAC#
     C                   MOVE      *ZEROS        CSCHKD
     C                   MOVE      *ZEROS        CSCHK#
     C                   MOVE      Q1USER        CSCUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSIDT
     C                   Z-ADD     DSTIME        CSITIM
     C     *IN40         IFEQ      '1'
     C                   ADD       1             VSCNT             5 0
     C                   WRITE     PCS01
     C                   ELSE
     C                   UPDATE    PCS01
     C                   END                                                    WRITE *IN40
     C                   END                                                          *IN60
     C*
     C     READ2         TAG
     C     ADKEY1        READE     PAD05                                  60
     C  N60              GOTO      AD2
      *
     C                   END                                                    DATE COMP
      * LOAD MSG FIELD APPROPRIATLY
     C                   EXSR      SCMSR
     C                   END                                                    CODE = 2
     C*
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C     SCCODE        IFEQ      '3'
     C     SCVDR         CHAIN     PVM01                              23
     C*
     C     *IN23         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     VMNAME        SCNAME
     C*
     C                   CALL      'APD211R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    SCVDR
     C                   PARM                    SCSEQ1
     C                   PARM                    QQRTCD            1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QQRTCD        IFEQ      'B'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C********************************************
     C*   ACTION CODE IS 4.                      *
     C********************************************
     C     SCCODE        IFEQ      '4'
     C     Q2RECF        ANDEQ     'SCR01'
     C     SYPOP4        IFNE      *BLANK
     C     SYINS4        ANDEQ     'Y'
     C                   MOVE      *BLANKS       ACTION
     C                   MOVEL     'T1DELCON'    ACTION
     C                   CALL      'QAB152'      EXCPRM
     C                   Z-ADD     *ZERO         SCCO#
     C                   Z-ADD     *ZERO         SCFAC#
     C                   MOVE      *BLANK        SCINV#
     C                   Z-ADD     *ZERO         SCVDR
     C                   Z-ADD     *ZERO         SCSEQ1
     C     ACTION        IFEQ      'YDEL'
     C     CSKEY         SETLL     PCS01
     C     TAG1          TAG
     C                   READ      PCS01                                  60
     C  N60CSCHK#        IFEQ      *ZERO
     C                   ADD       1             VSCNT
     C                   DELETE    PCS01
     C                   END
     C  N60              GOTO      TAG1
     C                   END
     C                   ELSE
     C     CSKEY         SETLL     PCS01
     C     TAG2          TAG
     C                   READ      PCS01                                  60
     C  N60CSCHK#        IFEQ      *ZERO
     C                   ADD       1             VSCNT
     C                   DELETE    PCS01
     C                   END
     C  N60              GOTO      TAG2
     C                   END
      * LOAD MSG FIELD APPROPRIATLY
     C                   EXSR      SCMSR
      *
     C                   END
     C                   MOVE      DFTCO         SCCO#
     C                   MOVE      DFTFAC        SCFAC#
     C****                 MOVE *BLANKS   SCCO#
     C****                 MOVE *BLANKS   SCFAC#
     C                   MOVE      *BLANKS       SCVDR
     C                   MOVE      *BLANKS       SCINV#
     C                   MOVE      *BLANKS       SCBNK#
     C                   MOVE      *BLANKS       SCSEQ1
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C********************************************
     C     SCR02L        TAG
     C*
     C     CSKEY         CHAIN     PCS01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PCS01E
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      UA(2)         SCMODE
     C                   ELSE
     C                   MOVE      UA(3)         SCMODE
     C                   END
     C*
     C                   MOVE      '1'           SCCODE
     C     ADKEY         CHAIN     PAD01                              40
     C*
     C                   Z-ADD     ADINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINVD
     C*
     C                   Z-ADD     ADDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUED
     C*
     C                   MOVE      ADPONO        SCPONO
     C                   MOVE      ADPODS        SCPODS
     C                   Z-ADD     ADAMT         SCAMT
     C                   Z-ADD     ADDISC        SCDISC
     C                   Z-ADD     *ZERO         SCPCT
     C                   Z-ADD     *ZERO         BFPCT             5 4
     C                   Z-ADD     ADBNK#        SCBNK#
     C                   MOVE      ADSEP         SCSEP
     C*
     C                   MOVE      CSINV#        SCINV#
     C                   MOVE      CSPART        SCPART
     C                   Z-ADD     CSAMT         SCPAMT
     C                   Z-ADD     CSDISC        SCPDIS
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      UA(3)         SCMODE
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCINV#
     C                   Z-ADD     *ZEROS        SCAMT
     C                   Z-ADD     *ZEROS        SCDISC
     C                   Z-ADD     *ZEROS        SCPCT
     C                   Z-ADD     *ZEROS        BFPCT
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C     CSCHK#        IFGT      *ZERO
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       REDISP            1
     C*
     C*  THE PARTIAL PAYMENT FLAG MUST BE A Y/N/' '.
     C*
     C     SCPART        IFNE      'Y'
     C     SCPART        ANDNE     'N'
     C     SCPART        ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE PARTIAL PAYMENT MUST BE LESS THAN OR EQUAL TO AMOUNT.
     C*
     C     SCPAMT        IFGT      ADAMT
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  CALCULATE DISCOUNT
     C*
     C     SCPCT         IFNE      BFPCT
     C     SCPCT         MULT      SCPAMT        SCPDIS
     C                   MOVE      SCPCT         BFPCT
     C                   MOVE      'Y'           REDISP            1
     C                   END
     C*
     C*  THE DISCOUNT CAN NOT BE GREATER THAN THE PAYMENT AMOUNT.
     C*
     C     SCPAMT        IFGT      *ZERO
     C     SCPAMT        ANDLT     SCPDIS
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C     SCPAMT        IFLT      *ZERO
     C     SCPDIS        ANDNE     *ZERO
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE DISCOUNT CAN NOT BE ZERO IF FLAG IS YES.
     C*
     C     ADDISC        IFEQ      *ZERO
     C     SCPDIS        ANDEQ     *ZERO
     C     SCPART        ANDEQ     'Y'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED BANK RECORD.
     C*
     C     SCBNK#        CHAIN     PBK01                              25
     C     *IN25         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C     BKCO3         IFNE      ADCO#
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE SEPARATE CHECK FLAG MUST BE A Y/N/' '.
     C*
     C     SCSEP         IFNE      'Y'
     C     SCSEP         ANDNE     'N'
     C     SCSEP         ANDNE     ' '
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK FOR REDISPLAY
     C*
     C     REDISP        IFEQ      'Y'
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     CSKEY         CHAIN     PCS01                              40
     C*
     C                   MOVE      SCSEQ1        CSSEQ1
     C                   MOVE      SCVDR         CSVDR
     C                   MOVE      SCCO#         CSCO#
     C                   MOVE      SCFAC#        CSFAC#
     C                   MOVE      SCINV#        CSINV#
     C                   Z-ADD     SCPAMT        CSAMT
     C                   Z-ADD     SCPDIS        CSDISC
     C                   MOVE      SCPART        CSPART
     C                   MOVE      SCBNK#        CSBNK#
     C                   MOVE      SCSEP         CSSEP
     C*
     C*  THE DISCOUNT CAN NOT BE ZERO IF FLAG IS YES.
     C*
     C     CSDISC        IFEQ      *ZERO
     C     SCPART        ANDEQ     'Y'
     C                   Z-ADD     ADDISC        CSDISC
     C                   END
     C*
     C                   MOVE      Q1USER        CSCUSR
     C                   MOVE      Q1PGM         CSIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSCDT
     C                   Z-ADD     DSTIME        CSCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCS01
     C                   ELSE
     C                   MOVE      Q1USER        CSCUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSIDT
     C                   Z-ADD     DSTIME        CSITIM
     C                   Z-ADD     *ZERO         CSCHK#
     C                   Z-ADD     *ZERO         CSCHKD
     C                   ADD       1             VSCNT             5 0
     C                   WRITE     PCS01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCSEQ1
     C                   MOVE      *BLANK        SCVDR
      * LOAD MSG FIELD APPROPRIATLY
     C                   EXSR      SCMSR
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C/EJECT
     C*** THIS ROUTINE WILL VALIDATE THE VALUE PASSED IN THE
     C*   FIELD 'AUDATE' FOR A VALID DATE.
     C*
     C*** IF AN INVALID DATE IS FOUND, THE PROGRAM INDICATOR
     C*   XXRTCD WILL BE TURNED ON. THE SETTINGS OF THE USED
     C*   INDICATORS WILL BE SAVED AND LATER RESTORED.
     C*
      *****************************************************************
      * LOAD MSG FIELD ON SCREEN APPROPRIATLY
      *****************************************************************
     C     SCMSR         BEGSR
     C     VSCNT         IFGT      0
     C                   Z-ADD     VSCNT         SCCNT
     C     SCCODE        IFEQ      '4'
     C                   MOVEL     MSG(3)        SCMSG
     C                   ELSE
     C                   MOVEL     MSG(1)        SCMSG
     C                   END
     C                   ELSE
     C                   CLEAR                   SCCNT
     C                   MOVEL     MSG(2)        SCMSG
     C                   END
     C                   ENDSR
      *****************************************************************
     OPCS01     E            PCS01E
      *** BEG ADD * 7/17/97
     OPAP01     E            PAP01E
      *** END ADD * 7/17/97
**  TABLE UA
-- ADD --
- UPDATE -
- INQUIRY-
**  MSGS
VOUCHER(S) SELECTED FOR PAYMENT
NO VOUCHERS MEET SELECTION CRITERIA
VOUCHER(S) REMOVED FROM PAYMENT SELECTION
