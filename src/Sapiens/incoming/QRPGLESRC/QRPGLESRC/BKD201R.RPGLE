     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE BKD201R- CHECK RECONCILATION INQUIRY                     *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     FBKD201F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F                                     SFILE(SCR04S:SX)
     F*
     FAPVML1    IF   E           K DISK
     FPREMP     IF   E           K DISK
     FBKCRL1    UF   E           K DISK
     FBKSYL1    IF   E           K DISK

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
     C                   PARM                    APPKEY
     C                   PARM                    SCCO#
     C                   PARM                    SCFAC#
     C                   PARM                    SCCHK#
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
     C     EMKEY         KLIST
     C                   KFLD                    CRCO#
     C                   KFLD                    CRFAC#
     C                   KFLD                    CRSS#
     C*
     C*   KEY LIST FOR THE COMPANY/FACILITY MASTER FILE.
     C*
     C     CHKKEY        KLIST
     C                   KFLD                    APPKEY
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFAC#
     C                   KFLD                    SCCHK#
     C*
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C*
     C     QQRTCD        COMP      '1'                                    09
     C*
     C     TOP           TAG
     C*
     C*  GET THE REQUESTED VENDOR RECORD.
     C*
     C     APPKEY        IFEQ      '  '
     C                   MOVE      'AP'          APPKEY            2
     C                   END
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     CHKKEY        SETLL     PCR01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '0'           *IN59
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C                   Z-ADD     *ZERO         DELETS
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
     C     'BK'          CHAIN     PSY01                              54
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
     C     SCCHK#        ORNE      *ZERO
     C                   GOTO      TOP
     C                   END                                                    END SCUCID NE BLKS
     C*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   Z-ADD     *ZERO         DELETS
     C                   MOVE      '0'           *IN59
     C                   Z-ADD     *ZERO         SX
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR04C
     C                   MOVE      '0'           *IN04
     C     TAGRC         TAG
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C*
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   ELSE                                                   ELSE EQ '1'
     C*
     C     SCSEL         IFEQ      '4'
     C     *IN09         ANDEQ     '0'
     C                   ADD       1             DELETS            1 0
     C     SX            IFLT      999
     C                   ADD       1             SX
     C                   WRITE     SCR04S
     C                   END
     C     SX            IFEQ      1
     C                   MOVE      SCCO#         XXCO#             3 0
     C                   MOVE      SCFAC#        XXFAC#            3 0
     C                   MOVE      SCCHK#        XXCHK#            7 0
     C                   END
     C                   GOTO      TAGRC
     C                   ELSE                                                   ELSE EQ '4'
     C*
     C     SCSEL         IFEQ      '5'
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     CHKKEY        CHAIN     PCR01                              40
     C  N40              EXCEPT    CRREL
     C                   MOVE      'INQUIRY '    SCMODE
     C*
     C                   MOVE      CRVDR         SCVDR
     C                   MOVE      CRCLR         SCCLR
     C                   Z-ADD     CRAMT         SCAMT
     C*
     C                   Z-ADD     CRCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCHKD
     C*
     C                   Z-ADD     CRCDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCDT
     C*
     C                   Z-ADD     CRCTIM        SCCTIM
     C                   MOVEL     CRCUSR        SCCSER
     C                   MOVEL     CRIPGM        SCIPGM
     C*
     C                   MOVE      '1'           *IN59
     C                   EXFMT     SCR03
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
     C                   ELSE
     C**
     C*** IF THE SUBFILE HAS CHANGED DISPLAYED VENDOR DATA REDISPLAY   INDICATE
     C*** VENDOR LIST SUB FILE
     C     *IN59         IFEQ      '1'
     C                   MOVE      ' '           SCSEL
     C                   GOTO      TOP
     C                   END
     C**
     C*** CHECK FOR DELETE CONFIRMATION SCREEN DISPLAY                 INDICATE
     C     DELETS        IFGT      *ZERO
     C                   MOVE      '1'           *IN01
     C                   Z-ADD     SX            #@$4
     C                   WRITE     SCR04
     C                   EXFMT     SCR04C
     C                   MOVE      '0'           *IN01
     C     *INKL         IFEQ      '1'
     C                   MOVE      *ZEROS        SCCO#
     C                   MOVE      *ZEROS        SCFAC#
     C                   MOVE      *ZEROS        SCCHK#
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
     C     CHKKEY        CHAIN     BKCRL1                             61
     C  N61              DELETE    PCR01
     C                   END
     C                   ELSE
     C                   SETON                                        60
     C                   END
     C*
     C                   END
     C                   MOVE      *ZEROS        SCCO#
     C                   MOVE      *ZEROS        SCFAC#
     C                   MOVE      *ZEROS        SCCHK#
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
     C                   Z-ADD     *ZERO         #@$2
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C                   READ      PCR01                                  60
     C  N60              EXCEPT    CRREL
     C*
     C  N60CRAPP         COMP      APPKEY                             6060
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C*
     C** POSITION ON RETURN FROM DELETE
     C     DELETS        IFGT      *ZERO
     C     CRCO#         ANDLE     XXCO#
     C     CRFAC#        ANDLE     XXFAC#
     C     CRCHK#        ANDLE     XXCHK#
     C                   Z-ADD     RRN2          #@$2
     C                   END
     C*
     C                   Z-ADD     CRCHKD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCHKD
     C*
     C                   Z-ADD     CRAMT         SCAMT
     C                   Z-ADD     CRCO#         SCCO#
     C                   Z-ADD     CRFAC#        SCFAC#
     C                   Z-ADD     CRVDR         SCVDR
     C     APPKEY        IFEQ      'AP'
     C     SCVDR         CHAIN     PVM01                              60
     C   60              MOVE      *BLANK        VMNAME
     C                   MOVEL     VMNAME        SCNAME
     C                   ELSE
     C     EMKEY         CHAIN     PEM01                              60
     C   60              MOVE      *BLANK        EMNAME
     C                   MOVEL     EMNAME        SCNAME
     C                   END
     C                   MOVE      CRCLR         SCCLR
     C                   Z-ADD     CRCHK#        SCCHK#
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
     C                   MOVE      *BLANK        SCNAME
     C                   Z-ADD     *ZERO         SCCHKD
     C                   Z-ADD     *ZERO         SCAMT
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C     #@$2          IFLE      *ZERO
     C                   Z-ADD     RRN2          #@$2
     C                   END
     C                   Z-ADD     *ZERO         SCCO#
     C                   Z-ADD     *ZERO         SCFAC#
     C                   Z-ADD     *ZERO         SCCHK#
     C*
     C                   ENDSR
     OPCR01     E            CRREL
