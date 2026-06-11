     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H******************************************************************
     H/TITLE API110R - MANUAL CHECK ENTRY
     H******************************************************************
     H* REVISIONS:
     H*
     H* 12/16/99  C:33  PROJECT: WC961
     H*                 IF THE REFUND CHECK IS CUT TO THE AGENT, UPDATE
     H*                 THE ACCOUNT CURRENT FILE WITH THE CHECK #.
     H*
     H**** C:22 5/17/93  PROJECT AUZ ***********************************
     H* MANUAL CHECKS PRODUCED FOR AR REFUNDS SHOULD UPDATE AR         *
     H* TRANSACTION WITH CHECK NUMBER                                  *
     H**** C:22 11/24/93 CHANGE AS PER CODE CHANGES FROM CRIMS         *
     H** 4/24/97 *** C:107 * AP010 ************************************
     H*  CHANGED PGM TO OUTPUT ZEROS INSTEAD OF BLANKS TO THE ROLLED  *
     H*   TO GL DATE. (NEW FIELD)                                     *
     H*
     H** 5/20/97 *** C:107 * AP017 ************************************
     H*  CHANGED PGM TO OUTPUT THE INVOICE ENTRY DATE WHEN WRITING    *
     H*   TO A/P HISTORY FILE. (NEW FIELD)                            *
     H*
     H** 10/21/97 *** C:107 * V-3 *************************************
     H*  CHANGED PGM TO USE THE FUND,FUNDYEAR,EMPLR & DIV FROM THE    *
     H*   A/P FILE-APADP WHEN CHAINING TO EMPLOYER AR FILE.           *
     H*
     H******************************************************************
     FAPI110F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR03S:RRN2)
     FAPADL1    UF   E           K DISK
     FAPCHL1    O  A E           K DISK
     FAPCSL1    UF   E           K DISK
     FAPGDL1    O  A E           K DISK
     FAPGFL1    O  A E           K DISK
     FBKCRL1    UF A E           K DISK
     FBKCRL2    IF   E           K DISK
     F                                     RENAME(PCR01:PCR02)
     FBKCFL1    IF   E           K DISK
     FAPVML1    UF   E           K DISK
     FAPBKL1    IF   E           K DISK
     FBKSYL1    IF   E           K DISK
     FBKTBL     IF   E           K DISK
     F*** BEGIN ADD 5/17/93 ******************************************
     FWMEAL     UF   E           K DISK
     F*** END ADD   5/17/93 ******************************************
     F***  BEGIN ADD  ***  12/16/99  ********************************
     FWDABL5    UF   E           K DISK
     F***  END   ADD  ***  12/16/99  ********************************

     D*
0073 D*                         *************************************
0074 D*                         * NORMAL/LEAP YEAR DAY COUNTS       *
0075 D*                         *************************************
0076 D*
0077 D NY              S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
0078 D LY              S              3  0 DIM(12) CTDATA PERRCD(12) ASCEND
0070 D AUW             S              1    DIM(40)                                WORDS
0071 D TABM#           S              2  0 DIM(12) CTDATA PERRCD(6)               MON
0071 D TABMON          S              3    DIM(12) ALT(TABM#)
0072 D TABD#           S              1  0 DIM(7) CTDATA PERRCD(7)                DAYS
0072 D TABDAY          S              3    DIM(7) ALT(TABD#)
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
     D*** BEGIN ADD 5/17/93 ***********************************
     D                 DS
     D  XXINV                  1     13  0
     D  XXFND                  1      3  0
     D  XXFYR1                 4      5  0
     D  XXEMP#                 6     10  0
     D  XXDIV                 11     13  0

     I*** END ADD   5/17/93 ***********************************

     C*
     C*
     C     *LIKE         DEFINE    SCSEQ1        SCSEQ2
     C     *LIKE         DEFINE    ADAMT         XXAMT
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
     C*   KEY LIST FOR THE VOUCHER LIST
     C*
     C     ADKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCSEQ1
     C*
     C     ADKEY1        KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C*
     C     CSKEY         KLIST
     C                   KFLD                    ADCO#
     C                   KFLD                    ADFAC#
     C                   KFLD                    ADVDR
     C                   KFLD                    ADINV#
     C                   KFLD                    ADSEQ1
     C*
     C*   KEY LIST FOR THE CHECK RECON FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    APPKEY
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCCHK#
     C*
     C*   KEY LIST FOR THE CHECK REC BY BNK/CHECK
     C*
     C     CRKEY2        KLIST
     C                   KFLD                    APPKEY
     C                   KFLD                    SCBNK#
     C                   KFLD                    SCCHK#
     C*
     C*   KEY LIST FOR THE VOUCHER G/L DETAIL.
     C*
     C     GDKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCSEQ1
     C                   KFLD                    SCSEQ2
     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C     TBKEY         KLIST
     C                   KFLD                    TCTBL
     C                   KFLD                    TCVAL
     C*
     C*** BEGIN ADD 5/17/93 **************************************
     C*
     C***  KEY LIST FOR ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#             3 0
      *** BEG CHG * V3 * C:107
     C*****                KFLD           XXFND
     C*****                KFLD           XXEMP#
     C*****                KFLD           XXDIV
     C*****                KFLD           XXFYR2  30
     C                   KFLD                    ADFND
     C                   KFLD                    ADEMP#
     C                   KFLD                    ADDIV#
     C                   KFLD                    ADFYR
      *** END CHG * V3 * C:107
     C                   KFLD                    XXTRDT            7 0
     C                   KFLD                    XXTIME            6 0
     C*** END ADD  5/17/93 ***************************************
     C***  BEGIN ADD  ***  12/16/99  ********************************
     C*
     C*  ACCOUNT CURRENT DETAIL FILE
     C*
     C     ABKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ADAGT#
     C                   KFLD                    ADINVD
     C                   KFLD                    XXTIME
     C***  END   ADD  ***  12/16/99  ********************************
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN34
     C                   MOVEL     '1'           SCCODE
     C                   MOVE      'AP'          APPKEY            2
     C                   MOVE      '001'         SCCO#
     C                   MOVE      '001'         SCFAC#
     C     'BK'          CHAIN     PSY01                              54
     C     *IN54         IFEQ      '0'
     C                   MOVE      'KLBEFCB '    SSPGM
     C                   MOVEL     '*LIBL'       SSLIB
     C                   MOVEL     'ACTION'      ACTBAR           10
     C                   MOVE      'BAR '        ACTBAR
      *
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
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C     Q2RECF        OREQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR03
     C                   EXFMT     SCR03C
     C                   MOVE      '0'           *IN01
     C                   END
     C***                                                              *****
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
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           X@CODE
     C                   CALL      'APD010R'
     C                   PARM                    SCVDR
     C                   PARM                    X@CODE            1
     C                   PARM      '1'           QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
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
     C*   F9 = COMPANY FACILITY INQUIRY          *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR01 *
     C*********
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD001R'
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      '1'           *IN34
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END                                                    END SCR02
     C                   END                                                    END *INKF
     C********************************************
     C*   F9 = DISTRIBUTION ENTRY                *
     C********************************************
     C     *INKI         IFEQ      '1'
     C*********
     C* SCR02 *
     C*********
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR29'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR02
     C                   END                                                    END *INKI
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR03 '      Q2RECF
     C                   END                                                    END SCR02
     C     Q2RECF        IFEQ      'SCR03 '
     C     Q2RECF        OREQ      'SCR03S'
     C     Q2RECF        OREQ      'SCR03C'
     C                   MOVEL     'SCR01 '      Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C**
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C**                                                               ********
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03 '      SCR03T
     C     Q2RECF        CABEQ     'SCR03S'      SCR03T
     C     Q2RECF        CABEQ     'SCR03C'      SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  GET THE REQUESTED COMPANY/FACILITY RECORD.
     C*
     C     CFKEY         CHAIN     PCF01                              40
     C     *IN40         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     CFNAME        SCCFNM
     C*
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C     SCVDR         CHAIN     PVM01                              41
     C     *IN41         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   EXCEPT    PVM01E
     C     VMHOLD        IFEQ      'Y'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVEL     VMNAME        SCNAME
     C                   MOVEL     VMTERM        STERMS
     C                   MOVE      *BLANK        STDESC
     C                   MOVE      'TC'          TCTBL
     C                   MOVE      *BLANKS       TCVAL
     C                   MOVEL     VMTERM        TCVAL
     C     TBKEY         CHAIN     PTC01                              60
     C  N60              MOVEL     TCDESC        STDESC
     C                   MOVE      *BLANK        SCWDDT
     C*
     C*  GET THE REQUESTED BANK NAME-INFORMATION
     C*
     C                   MOVEL     CFBNK#        SCBNK#
     C     SCBNK#        CHAIN     PBK01                              60
     C   60              MOVE      *BLANK        BKNAME
     C                   MOVEL     BKNAME        SCBNAM
     C*
     C*  CLEAR INPUT FIELDS
     C*
     C                   MOVE      *ZEROS        SCCHK#
     C                   MOVE      *ZEROS        SCCHKD
     C                   MOVE      *ZEROS        SCCHKA
     C                   MOVE      *ZEROS        SCTOTL            9 2
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   GOTO      REPO
     C****                 MOVEL'SCR02 '  Q2RECF
     C****                 GOTO STRT
     C*
     C* ENTER CHECK INFORMATION
     C*
     C     SCR02T        TAG
     C*
     C*  GET THE REQUESTED BANK MASTER.
     C*
     C     SCBNK#        IFEQ      *ZERO
     C                   MOVE      '1'           *IN58
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     SCBNK#        CHAIN     PBK01                              59
     C*
     C     *IN59         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  MAKE SURE CASH ACCT MATCHES COMPANY OF VOUCHERS
     C*
     C                   Z-ADD     *ZERO         XRRN
     C     OVEDT         TAG
     C                   ADD       1             XRRN
     C     XRRN          CHAIN     SCR03S                             60
     C     *IN60         IFEQ      '0'
     C     SCSEL         IFEQ      '1'
     C     SCCO#         IFNE      BKCO3
     C                   MOVEL     'SCR02 '      Q2RECF
     C                   MOVE      '1'           *IN55
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   GOTO      OVEDT
     C                   END
     C                   MOVEL     'SCR02 '      Q2RECF
     C*
     C*  CHECK NUMBER CAN NOT BE ALREADY USED
     C*
     C                   MOVE      'AP'          APPKEY
     C     CRKEY2        CHAIN     PCR02                              60
     C  N60              MOVE      '1'           *IN54
     C  N60              GOTO      STRT
     C*
     C*  CHECK DATE MUST BE A VALID DATE.
     C*
     C     SCCHKD        IFEQ      *ZERO
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN52
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  GET THE DATE WORD SUBR IN ACTION.
     C*
     C     SCCHKD        IFGT      *ZERO
     C                   Z-ADD     SCCHKD        W6DATE            6 0
     C                   EXSR      AUDWRD
     C                   MOVEA     AUW           SCWDDT
     C                   END
     C*
     C*  INVOICE AMOUNT CAN NOT BE ZERO.
     C*
     C     SCCHKA        IFEQ      *ZERO
     C                   MOVE      '1'           *IN51
     C                   GOTO      STRT
     C                   END
     C*
     C     SCTOTL        IFNE      SCCHKA
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   MOVE      '1'           *IN53
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      UPDATE
     C                   END
     C*
     C*  DISPLAY VOUCHERS FOR SELECTED VENDOR
     C*
     C     REPO          TAG
     C                   Z-ADD     0             RRN2              5 0
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*
     C     ADKEY1        SETLL     PAD01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '0'           *IN59
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR03C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C                   GOTO      STRT
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     UPDATE        TAG
     C*
     C                   Z-ADD     *ZERO         XXGRS             9 2
     C                   Z-ADD     *ZERO         XXNET             9 2
     C                   Z-ADD     *ZERO         XXDISC            9 2
     C                   Z-ADD     *ZERO         XRRN
     C     OVUPD         TAG
     C                   ADD       1             XRRN
     C     XRRN          CHAIN     SCR03S                             60
     C     *IN60         IFEQ      '0'
     C     SCSEL         IFEQ      '1'
     C*
     C     ADKEY         CHAIN     PAD01                              61
     C*
     C     *IN61         IFEQ      '0'
     C*
     C*** BEGIN ADD 5/17/93 ***************************************
     C*
     C*** UPDATE AR IF AR REFUND
     C*
     C*** BEGIN CHANGE 11/24/93 **********************************
     C                   MOVE      ADSEQ2        XXCO#
     C*                    MOVE ADCO#     XXCO#
     C*** END CHANGE 11/24/93 ************************************
     C                   MOVE      ADINV#        XXINV
      *** BEG DEL * V3 * C:107
     C*****                MOVEL*ZERO     XXFYR2
     C*****                MOVE XXFYR1    XXFYR2
      *** END DEL * V3 * C:107
     C                   MOVE      ADCDT         XXTRDT
     C                   Z-ADD     ADBTCH        XXTIME
     C*
     C***  BEGIN ADD  ***  12/16/99  *********************************
     C                   SELECT
     C     ADEORA        WHENEQ    'A'
     C                   EXSR      UPDTAB
     C                   OTHER
     C***  END   ADD  ***  12/16/99  *********************************
     C     EAKEY         CHAIN     PEA01                              69
     C     *IN69         IFEQ      '0'
     C     EAAMT         ANDEQ     ADAMT
     C                   MOVE      SCCHK#        EACHK#
     C                   UPDATE    PEA01
     C                   END
     C*** END ADD   5/17/93 ***************************************
     C***  BEGIN ADD  ***  12/16/99  *********************************
     C                   ENDSL
     C***  END   ADD  ***  12/16/99  *********************************
     C*
     C*  UPDATE INVOICE HISTORY
     C*
     C                   MOVE      ADSEQ1        CHSEQ1
     C                   MOVE      ADCODE        CHCODE
     C                   MOVE      ADVDR         CHVDR
     C                   MOVE      ADCO#         CHCO#
     C                   MOVE      ADFAC#        CHFAC#
     C                   MOVE      ADINVD        CHINVD
     C                   MOVE      ADINV#        CHINV#
     C                   MOVE      ADPONO        CHPONO
     C                   MOVE      ADPODS        CHPODS
     C                   Z-ADD     ADDISC        CHDISC
     C                   Z-ADD     ADAMT         CHAMT
     C                   Z-ADD     SCCHK#        CHCHK#
     C                   Z-ADD     SCBNK#        CHBNK#
     C                   MOVEL     SCCHKD        CHACMO
     C                   MOVE      SCCHKD        CHACYR
     C*
     C                   MOVE      Q1USER        CHCUSR
     C                   MOVE      Q1PGM         CHIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHCDT
     C                   Z-ADD     DSTIME        CHCTIM
     C                   MOVE      Q1USER        CHIUSR
     C                   TIME                    DSDTTM
     C******     DSDATE    MULT 10000.01  CHIDT
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHIDT
     C                   Z-ADD     DSTIME        CHITIM
      * BEG ADD * 05/20/97 * AP017
     C                   Z-ADD     ADRDAT        GLENDT
      * END ADD * 05/20/97 * AP017
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             CHRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PCH01
     C                   DELETE    PAD01
     C     CSKEY         CHAIN     PCS01                              60
     C  N60              DELETE    PCS01
     C*
     C*  UPDATE VENDOR PAID AMOUNT
     C*
     C     SCVDR         CHAIN     PVM01                              41
     C*
     C                   MOVE      Q1USER        VMCUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VMCDT
     C                   Z-ADD     DSTIME        VMCTIM
     C*
     C                   ADD       1             VMTINV
     C     ADAMT         SUB       ADDISC        XPAMT            11 2
     C                   ADD       XPAMT         VMPAMT
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VMLPDT
     C*
     C  N41              UPDATE    PVM01
     C*
     C*  WRITE CHECK REGISTER
     C*
     C                   Z-ADD     ADCO#         GFCO#
     C                   Z-ADD     ADFAC#        GFFAC#
     C                   Z-ADD     ADVDR         GFVDR
     C                   Z-ADD     SCBNK#        GFBNK#
     C                   Z-ADD     SCCHK#        GFCHK#
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GFCHKD
     C******               Z-ADDADCO#     GFCO1
     C******               Z-ADDADFAC#    GFFAC1
     C******               Z-ADDCFDPT1    GFGLD1
     C******               Z-ADDCFACC1    GFACC1
     C******               Z-ADDCFSUB1    GFSUB1
     C     CHAMT         SUB       CHDISC        GFAMT
     C                   MOVE      ADPODS        GFPODS
     C                   Z-ADD     CHDISC        GFDISC
     C                   MOVE      ADINV#        GFINV#
     C                   MOVEL     SCCHKD        GFACMO
     C                   MOVE      SCCHKD        GFACYR
     C                   MOVE      Q1PGM         GFIPGM
     C                   MOVE      Q1USER        GFIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GFIDT
     C                   Z-ADD     DSTIME        GFITIM
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             GFRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PGF01
     C*
     C                   ADD       CHDISC        XXDISC
     C                   ADD       CHAMT         XXGRS
     C     CHAMT         SUB       CHDISC        CHWORK            9 2
     C                   ADD       CHWORK        XXNET
     C*
     C                   END                                                    *IN61 = 0
     C                   END                                                    SCSEL = 1
     C                   GOTO      OVUPD
     C                   END                                                    *IN60 = 0
     C*
     C*** WRITE THE CASH A/P TRANSACTION.
     C*
     C                   Z-ADD     BKCO3         GDCO1
     C                   Z-ADD     BKFAC3        GDFAC1
     C                   Z-ADD     BKDPT3        GDGLD1
     C                   Z-ADD     BKACC3        GDACC1
     C                   Z-ADD     BKSUB3        GDSUB1
     C                   Z-SUB     XXNET         GDAMT
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     'H/CASH  '    GDDESC
     C                   MOVE      'BANK-000'    GDDESC
     C                   MOVE      SCBNK#        GDDESC
     C                   MOVEL     'AP'          GDSRC
     C*
     C                   MOVEL     SCCHKD        GDACMO
     C                   MOVE      SCCHKD        GDACYR
     C                   MOVE      *BLANKS       GDREFR
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDAT
     C                   MOVEL     'MANUAL'      GDBTCH
     C                   Z-ADD     ADSEQ1        GDSEQ1
     C                   Z-ADD     1             GDSEQ2
     C                   MOVE      '2'           GDTYPE
     C                   MOVE      SCVDR         GDVDR
     C                   MOVE      SCCO#         GDCO#
     C                   MOVE      SCFAC#        GDFAC#
     C                   MOVE      SCINV#        GDINV#
     C*
     C                   MOVE      Q1USER        GDCUSR
     C                   MOVE      Q1PGM         GDIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDCDT
     C                   Z-ADD     DSTIME        GDCTIM
     C*
     C                   MOVE      Q1USER        GDIUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDT
     C                   Z-ADD     DSTIME        GDITIM
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             GDRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PGD01
     C*
     C     XXDISC        IFNE      *ZERO
     C*
     C*** WRITE THE CASH DISCOUNT A/P TRANSACTION.
     C*
     C                   Z-ADD     CFCO2         GDCO1
     C                   Z-ADD     CFFAC2        GDFAC1
     C                   Z-ADD     CFDPT2        GDGLD1
     C                   Z-ADD     CFACC2        GDACC1
     C                   Z-ADD     CFSUB2        GDSUB1
     C                   Z-SUB     XXDISC        GDAMT
     C                   Z-ADD     2             GDSEQ2
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     'H/DISC. '    GDDESC
     C                   MOVE      '3'           GDTYPE
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             GDRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PGD01
     C                   END
     C*
     C*** OFFSET THE A/P ACCOUNT
     C*
     C                   Z-ADD     CFCO1         GDCO1
     C                   Z-ADD     CFFAC1        GDFAC1
     C                   Z-ADD     CFDPT1        GDGLD1
     C                   Z-ADD     CFACC1        GDACC1
     C                   Z-ADD     CFSUB1        GDSUB1
     C                   Z-ADD     XXGRS         GDAMT
     C                   Z-ADD     2             GDSEQ2
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     'H/AP    '    GDDESC
     C                   MOVE      '4'           GDTYPE
      * BEG ADD * 04/24/97 * AP010
     C                   Z-ADD     0             GDRDAT
      * END ADD * 04/24/97 * AP010
     C                   WRITE     PGD01
     C*
     C*  WRITE CHECK RECONCILIATION
     C*
     C     CRKEY         CHAIN     PCR01                              40
     C*
     C                   MOVE      'AP'          CRAPP
     C                   MOVE      SCVDR         CRVDR
     C                   MOVE      SCBNK#        CRBNK#
     C                   MOVE      SCCO#         CRCO#
     C                   MOVE      SCFAC#        CRFAC#
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCHKD
     C                   Z-ADD     SCCHKA        CRAMT
     C                   Z-ADD     *ZERO         CRSS#
     C                   MOVE      *BLANK        CRCLR
     C                   Z-ADD     SCCHK#        CRCHK#
     C*
     C                   MOVE      Q1USER        CRCUSR
     C                   MOVE      Q1PGM         CRIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCDT
     C                   Z-ADD     DSTIME        CRCTIM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCR01
     C                   ELSE
     C                   MOVE      Q1USER        CRCUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRIDT
     C                   Z-ADD     DSTIME        CRITIM
     C                   WRITE     PCR01
     C                   END
     C*
     C                   MOVEL     'SCR01 '      Q2RECF
     C                   MOVE      *BLANK        SCSEQ1
     C                   MOVE      *BLANK        SCVDR
     C                   GOTO      STRT
     C*
     C*  PROCESS VOUCHER SELECT
     C*
     C     SCR03T        TAG
     C*
     C*  PROCESS SELECTION
     C*
     C                   MOVE      '0'           *IN59
     C     TAGRC         TAG
     C*
     C                   READC     SCR03S                                 50
     C     *IN50         IFEQ      '0'
     C*
     C*** PROCESS DISPLAY REQUEST
     C*
     C     SCSEL         IFEQ      '5'
     C                   MOVE      ' '           SCSEL
     C                   UPDATE    SCR03S
     C*
     C     ADKEY         CHAIN     PAD01                              40
     C                   MOVEL     ' INQUIRY'    SCMODE
     C                   MOVE      ADCO#         SCCO#
     C                   MOVE      ADFAC#        SCFAC#
     C                   MOVE      ADVDR         SCVDR
     C*
     C*  GET THE REQUESTED COMPANY/FACILITY RECORD.
     C*
     C     CFKEY         CHAIN     PCF01                              40
     C                   MOVEL     CFNAME        SCCFNM
     C*
     C*  GET THE REQUESTED BANK NAME RECORD.
     C*
     C     ADBNK#        CHAIN     PBK01                              40
     C                   MOVEL     BKNAME        SCBNAM
     C                   MOVE      ADINV#        SCINV#
     C                   Z-ADD     ADINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINVD
     C                   Z-ADD     ADDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUED
     C                   MOVE      ADPONO        SCPONO
     C                   MOVE      ADPODS        SCPODS
     C                   MOVE      ADAMT         SCAMT
     C                   MOVE      ADDISC        SCDISC
     C*
     C                   MOVE      '1'           *IN59
     C                   EXFMT     SCR04
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C                   GOTO      TAGRC
     C                   END                                                    END EQ '5'
     C*
     C*** CAN'T SELECT IF CHECK ALREADY WRITTEN
     C*
     C     SCSEL         IFEQ      '1'
     C     ADKEY         CHAIN     PAD01                              60
     C  N60              EXCEPT    PAD01E
     C  N60CSKEY         CHAIN     PCS01                              61
     C  N60
     CANN61              EXCEPT    PCS01E
     C  N60
     CANN61CSCHK#        IFNE      *ZERO
     C                   MOVE      '1'           *IN34
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C**
     C                   END                                                    END 0 *IN50
     C**
     C*** IF DONE WITH THE SUBFILE, CALCULATE TOTAL SELECTED           INDICATE
     C*** VENDOR LIST SUB FILE
     C*
     C*  SELECTED VOUCHERS MUST EQUAL TOTAL
     C*
     C   59              MOVE      *BLANKS       Q2RECF
     C   59              MOVEL     'SCR03'       Q2RECF
     C   59              GOTO      STRT
     C*
     C                   Z-ADD     *ZERO         SCTOTL
     C                   Z-ADD     *ZERO         XRRN              5 0
     C     OVADD         TAG
     C                   ADD       1             XRRN
     C     XRRN          CHAIN     SCR03S                             60
     C     *IN60         IFEQ      '0'
     C     SCSEL         IFEQ      '1'
     C     SCAMT         SUB       SCDISC        SCWORK            9 2
     C                   ADD       SCWORK        SCTOTL
     C                   END
     C                   GOTO      OVADD
     C                   END
     C*
     C                   MOVE      SCTOTL        SCCHKA
     C                   MOVEL     'SCR02 '      Q2RECF
     C                   GOTO      STRT
     C*
     C****       SCTOTL    IFNE SCCHKA
     C****                 MOVE '1'       *IN53
     C****                 GOTO STRT
     C****                 ELSE
     C****                 GOTO UPDATE
     C****                 END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
0002 C*
0003 C*  COMPUTE A NEW CALENDAR DATE A GIVEN NUMBER OF DAYS IN THE FUTURE
0004 C*  OF A GIVEN CALENDAR DATE.
0005 C*
0006 C*  AUDATE IS THE BASE USED FROM WHICH THE DATE IS COMPUTED.
0007 C*  AUDAYS IS THE NUMBER OF DAYS IN THE FUTURE TO ADVANCE THE DATE.
0008 C*
0009 C*  THE RESULTS OF THE CALCULATION ARE REPLACED IN THE FIELD AUDATE.
0010 C*
0011 C     AUFUTR        BEGSR
0012 C*
0013 C                   MOVE      '0'           XX60              1
0014 C                   MOVE      '0'           XX61              1
0015 C*
0016 C   60              MOVE      '1'           XX60
0017 C   61              MOVE      '1'           XX61
0018 C*
0019 C                   Z-ADD     AUDAYS        AUDAYS            5 0  6060
0020 C   60              GOTO      AUFUT2
0021 C*
0022 C                   MOVEL     AUDATE        AUMD              4 0
0023 C                   MOVE      AUDATE        AUY1              2 0
0024 C                   MOVEL     AUMD          AUM1              2 0
0025 C                   MOVE      AUMD          AUD1              2 0
0026 C     AUY1          DIV       4             AUX               2 0
0027 C                   MVR                     AUX                      60
0028 C*
0029 C   60LY(AUM1)      ADD       AUD1          AUDYS1            5 0
0030 C  N60NY(AUM1)      ADD       AUD1          AUDYS1
0031 C     AUDYS1        ADD       AUDAYS        AUDYS1
0032 C     AUFUT1        TAG
0033 C*
0034 C   60AUDYS1        SUB       366           AUDYS2            5 061
0035 C  N60AUDYS1        SUB       365           AUDYS2               61
0036 C   61              ADD       1             AUY1
0037 C   61              Z-ADD     AUDYS2        AUDYS1
0038 C   61AUY1          DIV       4             AUX
0039 C   61              MVR                     AUX                      60
0040 C   61              GOTO      AUFUT1
0041 C*
0042 C                   Z-ADD     1             AUM1
0043 C                   Z-ADD     AUDYS1        AUDYS3            3 0
0044 C   60AUDYS3        LOOKUP    LY(AUM1)                           61  61
0045 C  N60AUDYS3        LOOKUP    NY(AUM1)                           61  61
0046 C   61AUM1          SUB       1             AUM1
0047 C  N61              Z-ADD     12            AUM1
0048 C   60AUDYS1        SUB       LY(AUM1)      AUMD
0049 C  N60AUDYS1        SUB       NY(AUM1)      AUMD
0050 C*
0051 C                   MOVEL     AUM1          AUMD
0052 C                   MOVEL     AUMD          AUDATE
0053 C                   MOVE      AUY1          AUDATE
0054 C*
0055 C     XX60          COMP      '1'                                    60
0056 C     XX61          COMP      '1'                                    61
0057 C*
0058 C*
0059 C     AUFUT2        ENDSR
     C/EJECT
     C*** THIS ROUTINE WILL VALIDATE THE VALUE PASSED IN THE
     C*   FIELD 'AUDATE' FOR A VALID DATE.
     C*
     C*** IF AN INVALID DATE IS FOUND, THE PROGRAM INDICATOR
     C*   XXRTCD WILL BE TURNED ON. THE SETTINGS OF THE USED
     C*   INDICATORS WILL BE SAVED AND LATER RESTORED.
     C*
     C     AUDCK         BEGSR
     C*
     C                   MOVE      '0'           XX60              1
     C                   MOVE      '0'           XX61              1
     C                   MOVE      '0'           XX62              1
     C*
     C   60              MOVE      '1'           XX60
     C   61              MOVE      '1'           XX61
     C   62              MOVE      '1'           XX62
     C*
     C*** IF AN INVALID DATE IS FOUND A 1 WILL BE PLACED IN
     C*** IN THE FIELD XXRTCD.
     C*
     C                   Z-ADD     *ZERO         XXRTCD            1 0
     C                   SETOFF                                       606162
     C*
     C*** BREAK DOWN THE DATE PASSED IN ORDER TO VERIFY IT.
     C*
     C                   MOVEL     AUDATE        SRMD              4 0
     C                   MOVE      AUDATE        SRYC              2 0
     C                   MOVEL     SRMD          SRM1              2 0
     C                   MOVE      SRMD          SRD1              2 0
     C*
     C*** VERIFY THAT THE YEAR SECTION HAS A VALID VALUE.
     C***
     C*** IF THE YEAR IS NEGATIVE FORCE A ZERO AND SETON
     C*** AN ERROR INDICATOR (*IN62).
     C*
     C     SRYC          COMP      *ZERO                                60
     C   60              MOVE      '00'          SRYC
     C   60              SETON                                        62
     C   60              GOTO      SRDCKA
     C*
     C     SRYC          COMP      99                                 60
     C   60              MOVE      '99'          SRYC
     C   60              SETON                                        62
     C*
     C*** VERIFY THAT THE MONTH SECTION HAS A VALID VALUE.
     C*** A VALID MONTH WILL BE ONE OR GREATER, IF NOT SETON
     C*** AN ERROR INDICATOR (*IN62) AND FORCE JANUARY(1).
     C*
     C     SRDCKA        TAG
     C     SRM1          COMP      1                                    60
     C   60              Z-ADD     1             SRM1
     C   60              SETON                                        62
     C   60              GOTO      SRDCKA
     C*
     C*** A VALID MONTH WILL BE 12 OR LESS, IF NOT SETON
     C*** AN ERROR INDICATOR (*IN62) AND FORCE DECEMBER (12).
     C*
     C     SRM1          COMP      12                                 60
     C   60              Z-ADD     12            SRM1
     C   60              SETON                                        62
     C*
     C/EJECT
     C*
     C*** VERIFY THE DAY SECTION OF THE DATE PASSED BUT FIRST
     C*** ESTABLISH IF THE YEAR IS A LEAP YEAR OR A NORMAL
     C*** YEAR.
     C*
     C     SRDCKB        TAG
     C*
     C                   MOVE      SRYC          SRY1              2 0
     C     SRY1          DIV       4             SRX               2 0
     C                   MVR                     SRX                      60
     C   60              Z-ADD     LY(SRM1)      SRDYS1            5 0
     C  N60              Z-ADD     NY(SRM1)      SRDYS1
     C*
     C     SRM1          ADD       1             SRX
     C     SRX           COMP      12                                 61
     C   60
     CANN61              Z-ADD     LY(SRX)       SRDYS2            5 0
     C  N60
     CANN61              Z-ADD     NY(SRX)       SRDYS2
     C   61SRDYS1        ADD       31            SRDYS2
     C     SRDYS2        SUB       SRDYS1        SRDYS1
     C     SRD1          COMP      1                                    60
     C   60              Z-ADD     1             SRD1
     C   60              SETON                                        62
     C   60              GOTO      SRDCKC
     C**
     C     SRD1          COMP      SRDYS1                             60
     C   60              Z-ADD     SRDYS1        SRD1
     C   60              SETON                                        62
     C*
     C*** PUT THE PIECES OF THE DATE BACK TOGETHER IN ORDER
     C*** TO SEND THEM BACK TO THE CALLER.
     C*
     C     SRDCKC        TAG
     C*
     C                   MOVEL     SRM1          SRMD
     C                   MOVE      SRD1          SRMD
     C                   MOVEL     SRMD          AUDATE            6 0
     C                   MOVE      SRYC          AUDATE
     C*
     C*** IF AN ERROR HAS OCCURRED *IN62 WILL BE ON, RETURN TO THE
     C*** CALLER A CODE OF '1' TO REFLECT THIS FACT.
     C*
     C   62              Z-ADD     1             XXRTCD
     C*
     C*** RETURN THE INDICATORS BACK TO THEIR STATE BEFORE THIS SUB-
     C*   ROUTINE WAS EXECUTED.
     C*
     C     XX60          COMP      '1'                                    60
     C     XX61          COMP      '1'                                    61
     C     XX62          COMP      '1'                                    62
     C*
     C                   ENDSR
0616 C************************  *************************************
0617 C* AUDWRD SUBROUTINE    *  * DATE/WORD SUBROUTINE              *
0618 C************************  *************************************
0619 C     AUDWRD        BEGSR
0620 C*
0621 C                   MOVE      '0'           XX40              1
0622 C                   MOVE      '0'           XX41              1
0623 C                   MOVE      '0'           XX42              1
0624 C*
0625 C   40              MOVE      '1'           XX40
0626 C   41              MOVE      '1'           XX41
0627 C   42              MOVE      '1'           XX42
0628 C*
0629 C                   Z-ADD     *ZERO         AUJYD
0630 C                   Z-ADD     *ZERO         AUJD
0631 C                   MOVE      *BLANK        AUW
0632 C                   MOVEL     W6DATE        AUMD              4 0
0633 C                   MOVE      W6DATE        AUY1              2 0
0634 C                   MOVEL     AUMD          AUM1              2 0
0635 C                   MOVE      AUMD          AUD1              2 0
0636 C*
0637 C                   Z-ADD     1900          AUYYYY            4 0
0638 C                   ADD       AUY1          AUYYYY
0639 C*
0640 C     AUM1          SUB       3             AUWKMM            2 0  4041
0641 C   40              ADD       12            AUWKMM
0642 C  N41AUWKMM        MULT(H)   30.6          AUJD              7 0
0643 C                   ADD       AUD1          AUJD
0644 C     AUYYYY        SUB       1900          AUWKYY            3 0    42
     C   40
0645 CANN42              SUB       1             AUWKYY
0646 C     AUWKYY        MULT      365.25        AUJYD             7 0
0647 C                   ADD       AUJYD         AUJD
0648 C     AUJD          DIV       7             AUWK7             7 0
0649 C                   MVR                     AUPTR             1 0
0650 C*          AUPTR     ADD  1         AUPTR
0651 C*
0652 C     AUM1          LOOKUP    TABM#         TABMON                   40
0653 C     AUPTR         LOOKUP    TABD#         TABDAY                   40
0654 C*
0655 C                   Z-ADD     1             I                 3 0
0656 C                   MOVEA     TABDAY        AUW(1)
0657 C     ' '           LOOKUP    AUW(I)                                 40
0658 C                   ADD       1             I
0659 C                   MOVEA     TABMON        AUW(I)
0660 C     ' '           LOOKUP    AUW(I)                                 40
0661 C                   ADD       1             I
0662 C                   MOVE      AUD1          AXD1              2
0662 C                   MOVEA     AXD1          AUW(I)
0663 C     ' '           LOOKUP    AUW(I)                                 40
0664 C                   MOVEA     ','           AUW(I)
0665 C                   ADD       1             I
0666 C                   MOVE      AUYYYY        AXYYYY            4
0666 C                   MOVEA     AXYYYY        AUW(I)
0667 C*
0668 C     XX40          COMP      '1'                                    40
0669 C     XX41          COMP      '1'                                    41
0670 C     XX42          COMP      '1'                                    42
0671 C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C     ADKEY1        READE     PAD01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C     X             IFEQ      1
     C                   MOVE      RRN2          #@$2
     C                   END
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ADCO#         SCCO#
     C                   MOVE      ADFAC#        SCFAC#
     C                   Z-ADD     ADAMT         SCAMT
     C                   Z-ADD     ADDISC        SCDISC
     C                   MOVE      ADSEQ1        SCSEQ1
     C                   MOVEL     ADPODS        SCPO15
     C                   Z-ADD     ADINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCINVD
     C                   Z-ADD     ADDUED        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUED
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR03S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      ' '           SCSEL
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCSEQ1
     C                   MOVE      *ZEROS        SCINVD
     C                   MOVE      *ZEROS        SCDUED
     C                   MOVE      *BLANKS       SCPO15
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR03S
     C                   END
     C*
     C                   ENDSR
     C*
     C***  BEGIN ADD  ***  12/16/99  *********************************
     C*===============================================================
     C* UPDTAS - UPDATE ACCOUNT CURRENT DETAIL FILE WITH CHECK #
     C*===============================================================
     C*
     C     UPDTAB        BEGSR
     C*
     C     ABKY          CHAIN     PAB01                              69
     C*
     C     *IN69         IFEQ      *OFF
     C     ABAMT         ANDEQ     ADAMT
     C*
     C                   Z-ADD     SCCHK#        ABCHK#
     C*
     C                   MOVEL     Q1USER        ABUSER
     C                   MOVEL     Q1PGM         ABPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABDT
     C*
     C                   MOVEL     DSTIME        ABTIME
     C*
     C                   UPDATE    PAB01
     C*
     C                   ENDIF                                                  *IN69=*OFF
     C*
     C                   ENDSR                                                  UPDTAB
     C***  END   ADD  ***  12/16/99  *********************************
     C*
     OPCS01     E            PCS01E
     OPAD01     E            PAD01E
     OPVM01     E            PVM01E
**   NORMAL YEAR NUMBER OF DAYS ARRAY.
000031059090120151181212243273304334
** LEAP YEAR NUMBER OF DAYS ARRAY.
000031060091121152182213244274305335
**
01JAN02FEB03MAR04APR05MAY06JUN
07JUL08AUG09SEP10OCT11NOV12DEC
**
0WED1THU2FRI3SAT4SUN5MON6TUE
