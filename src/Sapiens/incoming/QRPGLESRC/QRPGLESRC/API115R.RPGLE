     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE API115R - VOID CHECK ENTRY
     H******************************************************************
     FAPI115F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FAPCHL2    UF   E           K DISK
     FAPGDL1    UF A E           K DISK
     FBKCRL2    UF   E           K DISK
     FBKCFL1    IF   E           K DISK
     FAPVML1    UF   E           K DISK
     FAPBKL1    IF   E           K DISK
     FAPCSL2    IF   E           K DISK
     FBKSYL1    IF   E           K DISK
     FBKDTL1    IF   E           K DISK
     FAPADL1    O  A E           K DISK

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

     C*
     C*
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
     C                   KFLD                    SCSEQ1            5 0
     C*
     C     ADKEY1        KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C*
     C     CHKEY         KLIST
     C                   KFLD                    SCVDR
     C                   KFLD                    SCCHK#
     C*
     C     CSKEY         KLIST
     C                   KFLD                    SCBNK
     C                   KFLD                    SCCHK#
     C*
     C*   KEY LIST FOR THE CHECK RECON FILE
     C*
     C     CRKEY         KLIST
     C                   KFLD                    APPKEY
     C                   KFLD                    SCBNK
     C                   KFLD                    SCCHK#
     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C     GDKEY         KLIST
     C                   KFLD                    CHCO#
     C                   KFLD                    CHFAC#
     C                   KFLD                    CHVDR
     C                   KFLD                    CHSEQ1
     C*
     C                   TIME                    DSDTTM
     C*
     C*** GET A/P DATE
     C*
     C     'AP'          CHAIN     PDT01                              60
     C                   MOVEL     DTAPDT        YRMO1             5 0
     C                   MOVE      YRMO1         YRMO              4 0
     C                   MOVEL     YRMO          XXYR              2 0
     C                   MOVE      YRMO          XXMO              2 0
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN34
     C                   MOVE      'AP'          APPKEY            2
     C                   MOVE      '001'         SCCO#
     C                   MOVE      '001'         SCFAC#
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
     C                   MOVE      'AP'          APPKEY            2
     C                   MOVE      '1'           QQRTC
     C                   CALL      'BKD202R'
     C                   PARM                    APPKEY
     C                   PARM                    SCBNK
     C                   PARM                    SCCHK#
     C                   PARM                    QQRTC             1
     C*
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
     C                   MOVEL     'SCR01 '      Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C**
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C**                                                               ********
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
     C*  GET CHECK RECONCILIATION RECORD
     C*
     C                   MOVE      'AP'          XXAPP             2
     C     CRKEY         CHAIN     PCR01                              41
     C  N41              EXCEPT    PCR01E
     C     *IN41         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C     CRCLR         IFEQ      'V'
     C                   MOVE      '1'           *IN43
     C                   GOTO      STRT
     C                   END
     C*
     C*  MAKE SURE CHECK HAS BEEN FINALIZED
     C*
     C     CSKEY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN42
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      CRVDR         SCVDR
     C                   MOVE      CRCO#         SCCO#
     C                   MOVE      CRFAC#        SCFAC#
     C*
     C*  GET THE REQUESTED COMPANY/FACILITY RECORD.
     C*
     C     CFKEY         CHAIN     PCF01                              60
     C   60              MOVE      *BLANKS       SCCFNM
     C  N60              MOVEL     CFNAME        SCCFNM
     C*
     C                   Z-ADD     CRCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCHKD
     C*
     C                   Z-ADD     CRAMT         SCCHKA
     C*
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C     SCVDR         CHAIN     PVM01                              60
     C   60              MOVE      *BLANKS       SCNAME
     C  N60              MOVEL     VMNAME        SCNAME
     C  N60              EXCEPT    PVM01E
     C                   MOVE      *BLANK        SCWDDT
     C*
     C*  GET THE REQUESTED BANK NAME-INFORMATION
     C*
     C     SCBNK         CHAIN     PBK01                              60
     C   60              MOVE      *BLANK        BKNAME
     C                   MOVEL     BKNAME        SCBNAM
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02 '      Q2RECF
     C                   GOTO      STRT
     C*
     C* VOID CHECK
     C*
     C     SCR02T        TAG
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C                   Z-ADD     *ZERO         TOTGRS           11 2
     C     CHKEY         SETLL     PCH01
     C*
     C     OVUPD         TAG
     C     CHKEY         READE     PCH01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*  GET DISTRIBUTION ALREADY FED TO GL
     C*
     C                   Z-ADD     *ZERO         XXFEED
     C     GDKEY         SETLL     PGD01
     C     TOTFED        TAG
     C     GDKEY         READE     PGD01                                  40
     C     *IN40         IFEQ      '0'
     C     GDSTAT        IFEQ      'D'
     C     GDTYPE        ANDEQ     '1'
     C                   ADD       GDAMT         XXFEED           11 2
     C                   EXCEPT    PGD01E
     C                   END
     C                   GOTO      TOTFED
     C                   END
     C*
     C*  WRITE NEW RECORD
     C*
     C                   MOVE      CHVDR         ADVDR
     C                   MOVE      CHCO#         ADCO#
     C                   MOVE      CHFAC#        ADFAC#
     C                   MOVE      CHFAC#        ADFAC2
     C                   MOVE      CHSEQ1        ADSEQ1
     C                   MOVE      CHBTCH        ADBTCH
     C                   MOVE      CHINVD        ADINVD
     C                   MOVE      CHDUED        ADDUED
     C                   MOVE      CHACMO        ADACMO
     C                   MOVE      CHACYR        ADACYR
     C                   MOVE      *ZEROS        ADCHK#
     C                   MOVE      CHINV#        ADINV#
     C                   MOVE      CHPONO        ADPONO
     C                   MOVE      CHPODS        ADPODS
     C                   Z-ADD     CHDISC        ADDISC
     C                   Z-ADD     CHAMT         ADAMT
     C                   ADD       CHAMT         TOTGRS
     C                   MOVE      CHCODE        ADCODE
     C                   MOVE      *BLANKS       ADSEP
     C                   MOVE      CHSEQ2        ADSEQ2
     C                   MOVE      CHBUL1        ADBUL1
     C                   MOVE      CHBNK#        ADBNK#
     C                   Z-ADD     XXFEED        ADFEED
     C*
     C                   MOVE      Q1USER        ADCUSR
     C                   MOVE      Q1PGM         ADIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADCDT
     C*
     C                   Z-ADD     DSTIME        ADCTIM
     C                   MOVE      Q1USER        ADIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ADIDT
     C*
     C                   Z-ADD     DSTIME        ADITIM
     C                   WRITE     PAD01
     C                   DELETE    PCH01
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
     C                   Z-ADD     SCCHKA        GDAMT
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     'V/CASH  '    GDDESC
     C                   MOVE      'BANK-000'    GDDESC
     C                   MOVE      SCBNK         GDDESC
     C                   MOVEL     'AP'          GDSRC
     C*
     C                   MOVE      XXMO          GDACMO
     C                   MOVE      XXYR          GDACYR
     C                   MOVE      *BLANKS       GDREFR
     C*
     C                   Z-ADD     SCCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDAT
     C*
     C                   MOVEL     'VOID  '      GDBTCH
     C                   Z-ADD     ADSEQ1        GDSEQ1
     C                   Z-ADD     1             GDSEQ2
     C                   MOVE      '2'           GDTYPE
     C                   MOVE      SCVDR         GDVDR
     C                   MOVE      SCCO#         GDCO#
     C                   MOVE      SCFAC#        GDFAC#
     C                   MOVE      *BLANKS       GDINV#
     C*
     C                   MOVE      Q1USER        GDCUSR
     C                   MOVE      Q1PGM         GDIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDCDT
     C*
     C                   Z-ADD     DSTIME        GDCTIM
     C*
     C                   MOVE      Q1USER        GDIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         GDIDT
     C*
     C                   Z-ADD     DSTIME        GDITIM
     C                   WRITE     PGD01
     C*
     C*** WRITE DISCOUNT ACCOUNT
     C*
     C                   Z-ADD     CFCO2         GDCO1
     C                   Z-ADD     CFFAC2        GDFAC1
     C                   Z-ADD     CFDPT2        GDGLD1
     C                   Z-ADD     CFACC2        GDACC1
     C                   Z-ADD     CFSUB2        GDSUB1
     C     TOTGRS        SUB       SCCHKA        GDAMT
     C                   Z-ADD     2             GDSEQ2
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     'V/DISC  '    GDDESC
     C                   MOVE      '3'           GDTYPE
     C                   WRITE     PGD01
     C*
     C*** OFFSET THE A/P ACCOUNT
     C*
     C                   Z-ADD     CFCO1         GDCO1
     C                   Z-ADD     CFFAC1        GDFAC1
     C                   Z-ADD     CFDPT1        GDGLD1
     C                   Z-ADD     CFACC1        GDACC1
     C                   Z-ADD     CFSUB1        GDSUB1
     C                   Z-SUB     TOTGRS        GDAMT
     C                   Z-ADD     3             GDSEQ2
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     'V/AP    '    GDDESC
     C                   MOVE      '4'           GDTYPE
     C                   WRITE     PGD01
     C*
     C*  UPDATE CHECK RECONCILIATION
     C*
     C     CRKEY         CHAIN     PCR01                              40
     C*
     C                   MOVE      'V'           CRCLR
     C                   MOVE      Q1USER        CRCUSR
     C                   MOVE      Q1PGM         CRIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCDT
     C*
     C                   Z-ADD     DSTIME        CRCTIM
     C                   UPDATE    PCR01
     C*
     C*  UPDATE VENDOR PAID INFO
     C*
     C*
     C*  UPDATE VENDOR PAID AMOUNT
     C*
     C     SCVDR         CHAIN     PVM01                              41
     C*
     C                   MOVE      Q1USER        VMCUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         VMCDT
     C*
     C                   Z-ADD     DSTIME        VMCTIM
     C*
     C                   SUB       1             VMTINV
     C                   SUB       CRAMT         VMPAMT
     C*
     C  N41              UPDATE    PVM01
     C*
     C                   MOVEL     'SCR01 '      Q2RECF
     C                   MOVE      *BLANK        SCSEQ1
     C                   MOVE      *BLANK        SCVDR
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPCR01     E            PCR01E
     OPVM01     E            PVM01E
     OPGD01     E            PGD01E
