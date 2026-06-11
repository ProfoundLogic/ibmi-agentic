     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      /TITLE SC014R - BATCH NOTES     ENTRY SCREEN
      *****************************************************************
      * REVISIONS
      *
      * 01/04/00 C:113 PROJECT: SUPPORT
      *                Corrected date conversion.
      *
      *****************************************************************
      * INDICATOR USAGE
     H*  03       Office NOT installed: Nondisplay of F1=OV/400
     H* N03       Office IS installed:  Display/Use of F1 = OV/400
     H*  18    Roll up
     H*  20       Major Comment is not editable
     H*  21       Additional Remark is not editable
     H*  22       SCM0503 - Value cannot be equal to blanks.
     H*  23       Position Cursor on first available Additional Remark
     H*  30    Used to allow End of file on roll message to display
     H*        at the correct time.
     H*  32    End of file on roll up or roll down.
     H*  40       Used to chain to SDCMP
     H*  55       TOGGLE INFO TO DISPLAY USER, DATE, TIME
     H*  60       General CHAIN
     H*  61       CHAIN to additional remarks file
     H*  99       Image Installed
      *****************************************************************
      * FILE SPECIFICATIONS  * * *
      *****************************************************************
     FIS014F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN2)
     F*
     FIMBNL     UF A E           K DISK
     FSSYLF     IF   E           K DISK
     FIMBML1    IF   E           K DISK
     F*
      *****************************************************************
      * EXCEPTION SPECIFICATIONS * * *
      *****************************************************************

     D*
     D*
      *****************************************************************
      * INPUT SPECIFICATIONS  * * *
      *****************************************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXCM75                 1     75
     D  XXCM46                 1     46
     D  XXBLK1                47     47
     D  XXUSR                 48     57
     D  XXBLK2                58     58
     D  XXMO                  59     60  0
     D  XXSL1                 61     61
     D  XXDA                  62     63  0
     D  XXSL2                 64     64
     D  XXYR                  65     66  0
     D  XXBLK3                67     67
     D  XXHR                  68     69  0
     D  XXCL1                 70     70
     D  XXMN                  71     72  0
     D  XXCL2                 73     73
     D  XXSC                  74     75  0
     D                 DS
     D  XXIDT                  1      6  0
     D  XXYRS                  1      2  0
     D  XXMOS                  3      4  0
     D  XXDAS                  5      6  0
     D                 DS
     D  XXTRTM                 1      6  0
     D  XXHRS                  1      2  0
     D  XXMNS                  3      4  0
     D  XXSCS                  5      6  0
     D*
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
      *****************************************************************
      * CALCULATION SPECIFICATIONS  * * *
      *****************************************************************

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    BNPNAM        QQPNAM
     C     *LIKE         DEFINE    DRAL02        OFCPGM
     C     *LIKE         DEFINE    Q2RECF        HLRECF
     C     *LIKE         DEFINE    RRN2          SRRN2
     C     *LIKE         DEFINE    BNASQ#        ZZASQ#
     C*
     C*
     C*   KEY LIST FOR THE BATCH NOTES FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    QQBCHI
     C                   KFLD                    XXASQ#
     C*
     C     ARKY2         KLIST
     C                   KFLD                    QQBCHI
     C*
     C*
     C*   KEY LIST FOR THE BATCH ADMIN FILE
     C*
     C     BMKY          KLIST
     C                   KFLD                    QQBCHI
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQBCHI           12
     C                   PARM                    QQPNAM
      *** BEGIN ADD *** 01/04/00 ***********************************************
      *
      ** Date check/conversion parameters
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *** END   ADD *** 01/04/00 ***********************************************
     C*
      /Free
       scPrDs = 'Image Batch Notes Maintenance';
      /End-Free
     C                   MOVEL     'SC'          XXSYK
     C                   MOVE      *OFF          *IN55
     C                   MOVE      *BLANKS       XXBLK1
     C                   MOVE      *BLANKS       XXBLK2
     C                   MOVE      *BLANKS       XXBLK3
     C                   MOVE      '/'           XXSL1
     C                   MOVE      '/'           XXSL2
     C                   MOVE      ':'           XXCL1
     C                   MOVE      ':'           XXCL2
     C*
     C*** Check System Control File Install Flags for this Client:
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   Check for AS/400 Office Installed
     C     SYOFIF        IFEQ      'Y'
     C                   MOVE      '0'           *IN03                          Installed
     C                   ELSE                                                   ELS 'Y' SYOFIF
     C                   MOVE      '1'           *IN03                          Not Installed
     C                   END                                                    END 'Y' SYOFIF
     C*
     C                   END                                                    END 0 *IN60
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C*
     C                   EXSR      INIT
     C*
     C*   DISPLAY ALL FORMATS
     C*
      *------------------------*
      *** DISPLAY POINT
      *------------------------*
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
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
     C/EJECT
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C*
     C****************************************
     C*   F1 = OV/400                        *
     C****************************************
     C     *INKA         IFEQ      '1'
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   MOVE      *BLANKS       OFCPGM
     C                   MOVEL     Q2RECF        HLRECF
     C*
     C* Get Office CLP Program Name from SDRPF/DRAL02
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     DRAL02        OFCPGM
     C                   END
     C*
     C                   CALL      OFCPGM
     C                   PARM                    QQRTCD
     C*
     C                   WRITE     SCR99
     C                   MOVEL     HLRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKA
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   END                                                    END *INKL
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN30         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '0'           *IN30
     C                   END
     C*
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C*
     C     ARKY          SETGT     PBN01
     C*
     C                   EXSR      LOAD
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C*
     C****************************************
     C*   F22 = TOGGLE VIEW                  *
     C****************************************
     C     *INKW         IFEQ      *ON
     C*
     C     *IN55         IFEQ      *OFF
     C                   MOVE      *ON           *IN55
     C                   ELSE
     C                   MOVE      *OFF          *IN55
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XXASQ#
     C     ARKY          SETGT     PBN01
     C*
     C                   EXSR      LOAD
     C*
     C                   ENDIF
     C*
     C*
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
     C  N60
     CAN 60              MOVE      '0'           *IN10                            TEMP * STUFF
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C**** HELP PROCESSOR HERE!!!
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C*
      *****************************************************************
      *   GOTO THE APPROPRIATE SCREEN
      *****************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR01S'      SCR01T
     C     Q2RECF        CABEQ     'SCR01C'      SCR01T
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
     C*
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*
     C                   MOVE      ' '           ERROR             1
     C                   MOVEA     '000'         *IN(17)
     C*
     C                   READC     SCR01S                                 60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     RRN2          XXASQ#            5 0
     C     ARKY          CHAIN     PBN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SCCM75        BNCM75
     C*** THESE MAY BE FILLED WITH VALUES??? USED TO BE FROM CM FILE
     C***                  MOVELSCSECL    BNSECL
     C***                  MOVELSCSBYL    BNSBYL
     C***                  MOVELSCSTAT    BNSTAT
     C****************************************************************
     C                   MOVEL     Q1USER        BNLUSR
     C                   MOVEL     Q1PGM         BNLPGM
     C                   MOVE      DSTIME        BNLTIM
     C     DSDATE        MULT      10000.01      BNLDT
     C                   UPDATE    PBN01
     C                   ELSE
     C                   MOVEL     QQBCHI        BNBCHI
     C                   MOVEL     SCCM75        BNCM75
     C                   MOVE      RRN2          BNASQ#
     C                   TIME                    DSDTTM
      *** BEGIN CHG *** 01/04/00 ***********************************************
     C********** DSDATE    MULT 10000.01  BNCDAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'ISCVTDT'     DTYYMD
     C                   MOVEL     SRDTO         BNCDAT
     C                   MOVEL     SRDTO         BNTRDT
      *
     C                   MOVEL     QQPNAM        BNPNAM
     C                   TIME                    DSDTTM
     C********** DSDATE    MULT 10000.01  BNTRDT
      *** END   CHG *** 01/04/00 ***********************************************
     C                   MOVE      DSTIME        BNTRTM
     C*** WAS USING CM VALUES FOR THESE NEXT THREE!!
     C***                  MOVELSCSECL    BNSECL
     C***                  MOVELSCSBYL    BNSBYL
     C***                  MOVELSCSTAT    BNSTAT
     C*
     C                   MOVEL     *BLANKS       BNPRFL                           PROTECT FLG
     C*
     C                   MOVEL     Q1USER        BNLUSR
     C                   MOVEL     Q1PGM         BNLPGM
     C                   MOVE      DSTIME        BNLTIM
     C     DSDATE        MULT      10000.01      BNLDT
     C                   MOVE      BNLDT         BNIDT
     C                   MOVEL     Q1USER        BNIUSR
     C*
     C                   WRITE     PBN01
     C*
     C                   END
     C*
     C                   UPDATE    SCR01S
     C*
     C*
     C     NXTR          TAG
     C*
     C                   READC     SCR01S                                 60
     C                   END
     C*
     C*
     C*  Restore RRN2 so that rolling can properly function
     C*
     C                   MOVE      SRRN2         RRN2
     C*
     C*
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     INIT          BEGSR
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      *HIVAL        @@TIME            6 0
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             HLDRRN            5 0
     C                   Z-ADD     0             HLDRR2            5 0
     C                   Z-ADD     0             XXTIME            6 0
     C                   Z-ADD     0             XXDATE            6 0
     C                   Z-ADD     0             XXSEQ#            5 0
     C                   MOVE      *HIVAL        X#SEQ#            5 0
     C                   Z-ADD     0             XXRRN1            5 0          ADD 11/20/92
     C                   MOVE      *ON           ADDFLG            1
     C*
     C     SYKY          CHAIN     SDR01                              60
     C     *IN60         IFEQ      '0'
     C     DRFL01        ANDEQ     'Y'
     C                   MOVE      '1'           *IN99                            IMAGE INSTALLED
     C                   END
     C*
     C*** INITIALIZE SUBFILE
     C                   MOVE      '1'           *IN02
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN02
     C*
     C                   Z-ADD     0             XXASQ#
     C*
     C     ARKY          SETLL     PBN01
     C                   EXSR      LOAD
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   ENDSR
     C*
     C*
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    I N I T ***
      *====================================================================*
     C*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN69
     C*
     C                   MOVEL     QQBCHI        SCBCHI
     C*
     C     BMKY          CHAIN     PBM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BMBDSC        SCBDSC
     C                   ELSE
     C                   MOVEL     *BLANKS       SCBDSC
     C                   ENDIF
     C*
     C*
     C     ARKY2         READE     PBN01                                  60
     C  N60              EXCEPT    RBN01
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          ADDFLG
     C                   Z-ADD     1             #@$1
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     '0'
     C     BNASQ#        CHAIN     SCR01S                             61
     C     *IN61         IFEQ      '0'
     C*
     C*
     C     BNPRFL        IFNE      *BLANK
     C                   MOVE      '1'           *IN21
     C                   ELSE
     C                   MOVE      '0'           *IN21
     C     ADDFLG        IFEQ      *ON
     C                   MOVE      *ON           *IN23
     C                   MOVE      *OFF          ADDFLG
     C                   Z-ADD     BNASQ#        #@$1
     C                   ENDIF
     C                   END
     C*
     C*
     C     *IN55         IFEQ      *OFF
     C                   MOVEL     BNCM75        SCCM75
     C                   ELSE
     C                   Z-ADD     BNIDT         XXIDT
     C                   Z-ADD     BNTRTM        XXTRTM
     C                   Z-ADD     XXYRS         XXYR
     C                   Z-ADD     XXMOS         XXMO
     C                   Z-ADD     XXDAS         XXDA
     C                   Z-ADD     XXHRS         XXHR
     C                   Z-ADD     XXMNS         XXMN
     C                   Z-ADD     XXSCS         XXSC
     C                   MOVEL     BNIUSR        XXUSR
     C                   MOVEL     BNCM75        XXCM46
     C                   MOVEL     XXCM75        SCCM75
     C                   ENDIF
     C                   UPDATE    SCR01S
     C                   END
     C     ARKY2         READE     PBN01                                  60
     C  N60              EXCEPT    RBN01
     C*
     C                   END
     C*
     C     ADDFLG        IFEQ      *ON
     C                   MOVE      *OFF          ADDFLG
     C     BNASQ#        ADD       1             X                 3 0
     C                   Z-ADD     X             #@$1
     C     X             CHAIN     SCR01S                             61
     C     *IN61         IFEQ      '0'
     C                   MOVE      *OFF          *IN21
     C                   MOVE      *ON           *IN23
     C                   UPDATE    SCR01S
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     OPBN01     E            RBN01
