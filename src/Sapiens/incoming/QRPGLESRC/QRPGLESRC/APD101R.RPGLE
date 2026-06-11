     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE APD101R- VOUCHER MASTER SEARCH                           *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/09/00  C:33  SEQ#: WA107/23739                            *
     H*                  CLEAR VENDOR NAME PRIOR TO CHAIN.            *
     H*                                                               *
     H* 03/05/96 C:22 FIX ACCESS TO VENDOR MASTER FILE                *
     H*****************************************************************
     FAPD101F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F                                     SFILE(SCR04S:SX)
     F*
     FAPADL1    UF   E           K DISK
     FAPCSL1    UF   E           K DISK
     FBKCFL1    IF   E           K DISK
     FAPVML5    UF   E           K DISK
     FAPGDL1    UF A E           K DISK
     FAPBKL1    IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXCODE            1
     C                   PARM                    XXCO#             3 0
     C                   PARM                    XXFAC#            3 0
     C                   PARM                    XXVDR             7 0
     C                   PARM                    XXSEQ1            5 0
     C                   PARM                    QQRTCD            1
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
     C*
     C*   KEY LIST FOR THE VOUCHER LIST
     C*
     C     ADKEY1        KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C*
     C*   KEY LIST FOR THE VOUCHER LIST
     C*
     C     ADKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCSEQ1
     C*
     C*   KEY LIST FOR THE VOUCHER LIST
     C*
     C     CSKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCINV#
     C                   KFLD                    SCSEQ1
     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CFKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C*
     C*   KEY LIST FOR THE G/L JOURNAL ENTRIES
     C*
     C     GDKEY         KLIST
     C                   KFLD                    ADCO#
     C                   KFLD                    ADFAC#
     C                   KFLD                    ADVDR
     C                   KFLD                    ADSEQ1
     C*
     C*   KEY LIST FOR THE VOUCHER LIST
     C*
     C     VMKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCVDR
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     QQRTCD        COMP      '1'                                    09
     C*
     C     ADKEY1        SETLL     PAD01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '0'           *IN59
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q1USER        SCUSER                                     ILE
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
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
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCCO#         IFNE      *ZERO
     C     SCFAC#        ORNE      *ZERO
     C     SCVDR         ORNE      *ZERO
     C                   GOTO      TOP
     C                   END                                                    END SCUCID NE BLKS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   MOVE      '0'           *IN59
     C                   Z-ADD     *ZERO         SX
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR04C
     C                   MOVE      '0'           *IN04
     C     TAGRC         TAG
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C*
     C     CSKEY         CHAIN     PCS01                              60
     C  N60              EXCEPT    PCS01R
     C     *IN60         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      SCCO#         XXCO#
     C                   MOVE      SCFAC#        XXFAC#
     C                   MOVE      SCVDR         XXVDR
     C                   MOVE      SCSEQ1        XXSEQ1
     C                   MOVE      SCCODE        XXCODE
     C                   ELSE
     C**
     C                   MOVE      '1'           *IN52
     C                   UPDATE    SCR01S
     C                   MOVE      'SCR01 '      Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN60 1
     C*
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C     SCSEL         IFEQ      '4'
     C     *IN09         ANDEQ     '0'
     C     SX            IFLT      999
     C     CSKEY         CHAIN     PCS01                              60
     C   60              MOVE      *ZEROS        CSCHK#
     C     CSCHK#        IFEQ      *ZERO
     C                   ADD       1             DELETS            1 0
     C                   ADD       1             SX
     C                   WRITE     SCR04S
     C                   ELSE
     C                   MOVE      '1'           *IN55
     C                   UPDATE    SCR01S
     C                   END
     C                   END
     C                   GOTO      TAGRC
     C                   ELSE                                                   ELSE EQ '4'
     C     SCSEL         IFEQ      '5'
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     ADKEY         CHAIN     PAD01                              40
     C  N40              EXCEPT    PAD01R
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
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C***  BEGIN ADD  ***  08/09/00  *********************************
     C                   CLEAR                   VMNAME
     C***  END   ADD  ***  08/09/00  *********************************
     C     VMKY          CHAIN     PVM01                              41
     C  N41              EXCEPT    PVM01R
     C                   MOVEL     VMNAME        SCNAME
     C*
     C*  GET THE REQUESTED BANK NAME RECORD.
     C*
     C     ADBNK#        CHAIN     PBK01                              40
     C                   MOVEL     BKNAME        SCBNAM
     C                   MOVE      ADINV#        SCINV#
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
     C                   MOVEL     ADPODS        SCPODD
     C                   MOVE      ADAMT         SCAMT
     C                   MOVE      ADDISC        SCDISC
     C*
     C*
     C                   MOVE      '1'           *IN59
     C                   EXFMT     SCR02
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C                   GOTO      TAGRC
     C                   END
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '4'
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C**
     C                   ELSE
     C**
     C*** IF THE SUBFILE HAS CHANGED DISPLAYED VENDOR DATA REDISPLAY   INDICATE
     C*** VENDOR LIST SUB FILE
     C     *IN59         IFEQ      '1'
     C                   MOVE      ' '           SCSEL
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCVDR
     C                   GOTO      TOP
     C                   END
     C**
     C*** REDISPLAY IF ERROR WAS ENCOUNTERED                           INDICATE
     C**
     C     *IN55         IFEQ      '1'
     C                   MOVE      'SCR01 '      Q2RECF
     C                   GOTO      STRT
     C                   END
     C**
     C*** CHECK FOR DELETE CONFIRMATION SCREEN DISPLAY                 INDICATE
     C     SX            IFGT      *ZERO
     C     *IN09         ANDEQ     '0'
     C                   MOVE      '1'           *IN01
     C                   Z-ADD     SX            #@$4
     C                   WRITE     SCR04
     C                   EXFMT     SCR04C
     C                   MOVE      '0'           *IN01
     C     *INKL         IFEQ      '1'
     C                   MOVEL     *BLANKS       SCCODE
     C                   Z-ADD     0             SCCO#
     C                   Z-ADD     0             SCFAC#
     C                   MOVEL     *BLANKS       SCVDR
     C                   MOVEL     *BLANKS       SCVNM
     C                   MOVEL     *BLANKS       SCPODS
     C                   MOVEL     *BLANKS       SCPODD
     C                   MOVEL     *ZEROS        SCSEQ1
     C                   MOVEL     *ZEROS        SCAMT
     C                   MOVE      ' '           SCSEL
     C                   GOTO      TOP
     C                   END
     C*
     C                   SETOFF                                       60
     C                   Z-ADD     *ZERO         SX                5 0
     C     *IN60         DOUEQ     '1'
     C                   ADD       1             SX
     C     SX            IFLE      999
     C     SX            CHAIN     SCR04S                             60
     C     *IN60         IFEQ      '0'
     C*
     C* DELETE OPEN VOUCHER
     C     ADKEY         CHAIN     APADL1                             61
     C  N61              DELETE    PAD01
     C*
     C* IF FED, REVERSE GENERAL LEDGER EXPENSE AND A/P
     C     ADFEED        IFNE      *ZERO
     C                   MOVE      *ZERO         GDCO1
     C                   MOVE      *ZERO         GDFAC1
     C                   MOVE      *ZERO         GDGLD1
     C                   MOVE      *ZERO         GDACC1
     C                   MOVE      *ZERO         GDSUB1
     C     VMKY          CHAIN     PVM01                              61
     C  N61              EXCEPT    PVM01R
     C  N61              MOVE      ADCO#         GDCO1
     C  N61              MOVE      VMFAC#        GDFAC1
     C  N61VMGLD#        IFNE      *ZERO
     C                   MOVE      VMGLD#        GDGLD1
     C                   MOVE      VMACC#        GDACC1
     C                   MOVE      VMSUB#        GDSUB1
     C                   END
     C*
     C                   Z-SUB     ADFEED        GDAMT
     C                   MOVE      *BLANKS       GDDESC
     C                   MOVEL     'VOID EXP'    GDDES1           16
     C                   MOVE      'ENSE    '    GDDES1
     C                   MOVEL     GDDES1        GDDESC
     C                   MOVEL     'AP'          GDSRC
     C*
     C                   MOVE      ADINVD        GDIDAT
     C                   MOVEL     'VOID    '    GDREFR
     C                   MOVEL     ADINVD        XXYM              4 0
     C                   MOVE      XXYM          GDACMO
     C                   MOVEL     XXYM          GDACYR
     C                   MOVE      ADBTCH        GDBTCH
     C                   Z-ADD     ADSEQ1        GDSEQ1
     C                   Z-ADD     1             GDSEQ2
     C                   MOVE      '1'           GDTYPE
     C                   MOVE      ADVDR         GDVDR
     C                   MOVE      ADCO#         GDCO#
     C                   MOVE      ADFAC#        GDFAC#
     C                   MOVE      ADINV#        GDINV#
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
     C                   MOVE      *BLANKS       GDSTAT
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
     C*** WRITE THE A/P TRANSACTION.
     C*
     C                   MOVE      *ZERO         GDCO1
     C                   MOVE      *ZERO         GDFAC1
     C                   MOVE      *ZERO         GDGLD1
     C                   MOVE      *ZERO         GDACC1
     C                   MOVE      *ZERO         GDSUB1
     C                   MOVE      ADCO#         SCCO#
     C                   MOVE      ADFAC#        SCFAC#
     C     CFKEY         CHAIN     PCF01                              40
     C                   Z-ADD     CFCO1         GDCO1
     C                   Z-ADD     CFFAC1        GDFAC1
     C                   Z-ADD     CFDPT1        GDGLD1
     C                   Z-ADD     CFACC1        GDACC1
     C                   Z-ADD     CFSUB1        GDSUB1
     C                   Z-ADD     ADFEED        GDAMT
     C                   MOVE      *BLANK        GDDESC
     C                   MOVEL     'ACCOUNTS'    GDDES1           16
     C                   MOVE      ' PAYABLE'    GDDES1
     C                   MOVEL     GDDES1        GDDESC
     C*
     C                   MOVEL     'VOID    '    GDREFR
     C                   MOVE      '4'           GDTYPE
     C                   WRITE     PGD01
     C*
     C* IF NOT FED, DELETE GENERAL LEDGER EXPENSE AND A/P
     C                   ELSE
     C     ADKEY         SETLL     PGD01
     C     DGD           TAG
     C     ADKEY         READE     PGD01                                  62
     C  N62              DELETE    PGD01
     C  N62              GOTO      DGD
     C                   END
     C*
     C* DELETE CHECK SELECTION
     C  N61              MOVE      ADINV#        SCINV#
     C     CSKEY         CHAIN     APCSL1                             61
     C  N61              DELETE    PCS01
     C*
     C* DELETE TEMPORARY VENDOR
     C                   MOVEL     ADVDR         TEMP              3 0
     C     TEMP          IFEQ      999
     C     VMKY          CHAIN     PVM01                              61
     C  N61              DELETE    PVM01
     C                   END
     C                   END
     C                   ELSE
     C                   SETON                                        60
     C                   END
     C*
     C                   END
     C                   MOVEL     *BLANKS       SCCODE
     C                   Z-ADD     0             SCCO#
     C                   Z-ADD     0             SCFAC#
     C                   MOVEL     *BLANKS       SCVDR
     C                   MOVEL     *BLANKS       SCVNM
     C                   MOVEL     *BLANKS       SCPODS
     C                   MOVEL     *BLANKS       SCPODD
     C                   MOVEL     *ZEROS        SCSEQ1
     C                   MOVEL     *ZEROS        SCAMT
     C                   MOVE      ' '           SCSEL
     C                   GOTO      TOP
     C                   END                                                    END DELETS
     C                   GOTO      TERM
     C                   END                                                    END 0 *IN50
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '0'           *IN54
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
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
     C                   READ      PAD01                                  60
     C  N60              EXCEPT    PAD01R
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
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      ADCODE        SCCODE
     C                   MOVE      ADCO#         SCCO#
     C                   MOVE      ADFAC#        SCFAC#
     C                   MOVE      ADVDR         SCVDR
     C                   Z-ADD     ADAMT         SCAMT
     C                   MOVE      ADSEQ1        SCSEQ1
     C                   MOVEL     ADPODS        SCPODS
     C                   MOVEL     ADPODS        SCPODD
     C                   MOVEL     ADINV#        SCINV#
     C*
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C***  BEGIN ADD  ***  08/09/00  *********************************
     C                   CLEAR                   VMNAME
     C***  END   ADD  ***  08/09/00  *********************************
     C     VMKY          CHAIN     PVM01                              61
     C  N61              EXCEPT    PVM01R
     C                   MOVEL     VMNAME        SCVNM
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCVDR
     C                   MOVE      *BLANKS       SCSEQ1
     C                   MOVE      *BLANKS       SCCODE
     C                   MOVE      *BLANKS       SCPODS
     C                   MOVE      *BLANKS       SCPODD
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCVDR
     C*
     C                   ENDSR
     OPAD01     E            PAD01R
     OPCS01     E            PCS01R
     OPVM01     E            PVM01R
