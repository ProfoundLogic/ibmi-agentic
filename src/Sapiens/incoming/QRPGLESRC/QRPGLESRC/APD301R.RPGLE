     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE APD301R- VOUCHER SELECTION SEARCH                        *
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
     H* 03/06/96 C:22 FIX TO ACCESS OF VENDOR MASTER                  *
     H*****************************************************************
     FAPD301F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F                                     SFILE(SCR04S:SX)
     F*
     FAPCSL1    UF   E           K DISK
     FAPADL1    IF   E           K DISK
     FBKCFL1    IF   E           K DISK
     FAPVML5    IF   E           K DISK
     FBKSYL1    IF   E           K DISK

     D*
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
     D CMDACT          DS
     D  ACT                    1    200
     D QLFDAP          DS
     D  SSPGM                  1     10
     D  SSLIB                 11     20
     D HELPCT          DS
     D  HLP                    1     60

     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXCO#             3 0
     C                   PARM                    XXFAC#            3 0
     C                   PARM                    XXVDR             7 0
     C                   PARM                    XXINV#           13
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
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*   KEY LIST FOR THE VOUCHER LIST
     C*
     C     ADKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCVDR
     C                   KFLD                    SCSEQ1
     C*
     C*   KEY LIST FOR THE VOUCHER SELECTION
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
     C*   KEY LIST FOR THE VENDOR MASTER
     C*
     C     VMKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCVDR
     C*
     C*   KEY LIST FOR THE VENDOR MASTER
     C*
     C     VMKY1         KLIST
     C                   KFLD                    CSCO#
     C                   KFLD                    CSVDR
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
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C                   MOVE      '0'           *IN59
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     CSKEY         SETLL     PCS01                              60
     C*
     C*** CLEAR SUBFILE
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
     C********************************************
     C*   F6 = OPERATIONS INQUIRY                *
     C********************************************
     C     *INKF         IFEQ      '1'
     C     SYPOP4        ANDNE     *BLANK
     C     SYINS4        ANDEQ     'Y'
     C                   CALL      'QAB152'      EXCPRM
     C                   GOTO      STRT
     C                   END                                                    END *INKF
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
     C     SCVDR         IFNE      *ZEROS
     C                   GOTO      TOP
     C                   END                                                    END         E BLKS
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
     C                   MOVE      'B'           QQRTCD
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      SCCO#         XXCO#             3 0
     C                   MOVE      SCFAC#        XXFAC#            3 0
     C                   MOVE      SCVDR         XXVDR             7 0
     C                   MOVE      SCINV#        XXINV#           13
     C                   MOVE      SCSEQ1        XXSEQ1            5 0
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C     SCSEL         IFEQ      '4'
     C     SX            IFLT      999
     C     SCCHK#        IFEQ      *ZERO
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
     C                   MOVE      '1'           SCCODE            1
     C     ADKEY         CHAIN     PAD01                              40
     C                   MOVEL     ' INQUIRY'    SCMODE
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
     C                   MOVEL     VMNAME        SCNAME
     C                   MOVEL     VMNAME        SCNAME
     C*
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
     C                   MOVE      ADAMT         SCAMT
     C                   MOVE      ADDISC        SCDISC
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     CSKEY         CHAIN     PCS01                              40
     C                   MOVE      CSAMT         SCPAMT
     C                   MOVE      CSDISC        SCPDIS
     C                   MOVE      CSPART        SCPART
     C*
     C                   MOVE      '1'           *IN59
     C                   EXFMT     SCR02
     C**
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
     C                   END
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   MOVE      ' '           SCSEL
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCVDR
     C                   MOVE      *BLANKS       SCINV#
     C                   MOVE      *BLANKS       SCSEQ1
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C**
     C***                                                              INDICATE
     C**
     C     *IN59         IFEQ      '1'
     C                   MOVE      ' '           SCSEL
     C                   MOVE      *BLANKS       SCCO#
     C                   MOVE      *BLANKS       SCFAC#
     C                   MOVE      *BLANKS       SCVDR
     C                   MOVE      *BLANKS       SCINV#
     C                   MOVE      *BLANKS       SCSEQ1
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
     C                   MOVE      '1'           *IN01
     C                   Z-ADD     SX            #@$4
     C                   WRITE     SCR04
     C                   EXFMT     SCR04C
     C                   MOVE      '0'           *IN01
     C     *INKL         IFEQ      '1'
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
     C     CSKEY         CHAIN     PCS01                              61
     C  N61              DELETE    PCS01
     C                   END
     C                   ELSE
     C                   SETON                                        60
     C                   END
     C*
     C                   END
     C                   MOVE      ' '           SCSEL
     C                   GOTO      TOP
     C                   END                                                    END DELETS
     C                   GOTO      TERM
     C                   END                                                    END 0 *IN50
     C*
     C     TERM          TAG
     C*
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
     C                   READ      PCS01                                  60
     C  N60              EXCEPT    CSREL
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C     X             IFEQ      1
     C                   Z-ADD     RRN2          #@$2
     C                   END
     C*
     C                   MOVE      CSCO#         SCCO#
     C                   MOVE      CSFAC#        SCFAC#
     C                   MOVE      CSVDR         SCVDR
     C*
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C***  BEGIN ADD  ***  08/09/00  *********************************
     C                   CLEAR                   VMNAME
     C***  END   ADD  ***  08/09/00  *********************************
     C     VMKY1         CHAIN     PVM01                              41
     C                   MOVEL     VMNAME        SCVNAM
     C*
     C                   MOVE      *BLANKS       SCSEL
     C                   MOVE      CSINV#        SCINV#
     C                   MOVE      CSSEQ1        SCSEQ1
     C                   MOVEL     CSPODS        SCPODS
     C                   Z-ADD     CSAMT         SCAMT
     C                   Z-ADD     CSBNK#        SCBNK#
     C                   Z-ADD     CSCHK#        SCCHK#
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C*
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C*
     C                   Z-ADD     *ZEROS        SCCO#
     C                   Z-ADD     *ZEROS        SCFAC#
     C                   Z-ADD     *ZEROS        SCVDR
     C                   MOVE      *BLANKS       SCINV#
     C                   MOVE      *BLANKS       SCVNAM
     C                   Z-ADD     *ZEROS        SCSEQ1
     C                   MOVE      *BLANKS       SCPODS
     C                   Z-ADD     *ZEROS        SCAMT
     C                   Z-ADD     *ZEROS        SCBNK#
     C                   Z-ADD     *ZEROS        SCCHK#
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     RRN2          #@$2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*** CLEAR KEY/POSITION TO FIELDS
     C*
     C                   Z-ADD     *ZEROS        SCCO#
     C                   Z-ADD     *ZEROS        SCFAC#
     C                   Z-ADD     *ZEROS        SCVDR
     C                   MOVE      *BLANKS       SCINV#
     C                   MOVE      *BLANKS       SCVNAM
     C                   Z-ADD     *ZEROS        SCSEQ1
     C                   MOVE      *BLANKS       SCPODS
     C                   Z-ADD     *ZEROS        SCAMT
     C                   Z-ADD     *ZEROS        SCBNK#
     C                   Z-ADD     *ZEROS        SCCHK#
     C*
     C                   ENDSR
     OPCS01     E            CSREL
