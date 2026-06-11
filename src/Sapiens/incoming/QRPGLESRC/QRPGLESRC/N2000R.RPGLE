     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE N2000R - EOB#/SERVICE# SEARCH                            *
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
     H*  12/31/98  C:94  PROJECT: MC152                               *
     H*                  CHANGE PROGRAM TO GET INFORMATION FROM       *
     H*                  FAEAP IF ##PRVB = Y                          *
     H*                                                               *
     H* 07/05/95   C:35     REQ/PROJ: NW008                           *
     H*                     1. GET REVIEW FEE FROM NAMBP MEDICAL MGMT *
     H*                        FILE, FOR SYSTEM TYPE 'M'.             *
     H*                                                               *
     H*****************************************************************
     FN2000F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FSMUPL     IF   E           K DISK
     FFAEOL6    IF   E           K DISK
     FFAHBL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FNAMBL1    IF   E           K DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F*** BEG ADD *** 12/31/98 ****************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 12/31/98 ****************************************

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
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE EOB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     EOKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARM FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE HOSPITAL BILL FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*   KEY LIST FOR THE PRE-CERT BILLING FILE
     C*
     C     PBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*   KEY LIST FOR THE MEDICAL MANAGEMENT BILLING FILE
     C*
     C     MBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*** BEG ADD *** 12/31/98 ***************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END ADD *** 12/31/98 ***************************************
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRTCD            1
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    EOCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    EOSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    EOFND         XXFND                          3.0N
     C     *LIKE         DEFINE    EOFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    EOCASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    EOEOB#        XXEOB#                         9.0N
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     UPCO#         XXCO#
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXCO#
     C                   END
     C*
     C                   MOVEL     QQPRDS        SCPRDS
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQEOB#        XXEOB#
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
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C     SCSUB         IFNE      *ZEROS
     C                   Z-ADD     SCSUB         XXSUB#
     C                   Z-ADD     SCFND#        XXFND
     C                   Z-ADD     SCFYR#        XXFYR
     C                   Z-ADD     SCCAS#        XXCASE
     C                   Z-ADD     SCEOB         XXEOB#
     C                   Z-ADD     *ZEROS        SCSUB
     C                   Z-ADD     *ZEROS        SCFND#
     C                   Z-ADD     *ZEROS        SCFYR#
     C                   Z-ADD     *ZEROS        SCCAS#
     C                   Z-ADD     *ZEROS        SCEOB
     C                   GOTO      TOP
     C                   END                                                    END SCSUB
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     WDW01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      SCSUB#        QQSUB#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCEOB#        QQEOB#
     C                   GOTO      TERM
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL         IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   Z-ADD     RRN2          #@$2
     C                   UPDATE    WDW01S
     C                   MOVE      *BLANK        SCSEL
     C                   GOTO      MAINLN
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C                   GOTO      MAINLN
     C                   END                                                    END 0 *IN50
     C*
     C                   GOTO      TOP                                          RE-SET SUBFILE
     C*
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
     C     EOKEY         SETLL     PEO01                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOAD SUBROUTINE                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        7             X                 5 0
     C*
     C     NXTRCD        TAG
     C     EOKY1         READE     PEO01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN DEL ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C********** QQCALL    IFEQ '1'
     C********** EOFLG5    IFEQ 'Y'
     C**********           GOTO NXTRCD
     C**********           END
     C**********           ELSE
     C********** QQCALL    IFEQ '2'
     C********** EOFLG5    IFNE 'Y'
     C**********           GOTO NXTRCD
     C**********           END
     C**********           END
     C**********           END
     C*** END   DEL ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C                   ADD       1             RRN2              5 0
     C                   Z-ADD     EOSUB#        SCSUB#
     C                   Z-ADD     EOFND         SCFND
     C                   Z-ADD     EOFYR         SCFYR
     C                   Z-ADD     EOCASE        SCCASE
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   MOVE      EOSYS         SCSYS
     C*** BEG ADD *** 12/31/98 ***************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C     ##PRVB        IFEQ      'Y'
     C     EOSYS         IFEQ      'P'
     C                   Z-ADD     EOBAMT        SCRFEE
     C                   ENDIF                                                  EOSYS = 'P'
     C*
     C     EOSYS         IFEQ      'H'
     C     EOSYS         OREQ      'U'
     C     EOSYS         OREQ      'M'
     C     EAKEY         CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EABAMT        SCRFEE
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCRFEE
     C                   ENDIF                                                  *IN99 = OFF
     C                   ENDIF                                                  EOSYS = H,U,M
     C                   ELSE                                                   ##PRVB = Y
     C*** END ADD *** 12/31/98 ***************************************
     C     EOSYS         IFEQ      'P'
     C                   Z-ADD     EOBAMT        SCRFEE
     C                   ELSE                                                   ELSE EQ 'P'
     C     EOSYS         IFEQ      'H'
     C     HBKEY         CHAIN     PHB01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     HB1AAM        SCRFEE
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCRFEE
     C                   END                                                    END *IN60
     C                   ELSE                                                   ELSE EQ 'H'
     C     EOSYS         IFEQ      'U'
     C     PBKEY         CHAIN     PPB01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     PBBAMT        SCRFEE
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCRFEE
     C                   END                                                    END *IN60
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   ELSE                                                   ELSE EQ 'U'
     C     EOSYS         IFEQ      'M'
     C     MBKEY         CHAIN     PMB01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     MBBAMT        SCRFEE
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCRFEE
     C                   END                                                    END *IN60
     C                   END                                                    END EOSYS = M
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END                                                    END EOSYS = U
     C                   END                                                    END EOSYS = H
     C                   END                                                    END EOSYS = P
     C*** BEG ADD *** 12/31/98 ****************************************
     C                   ENDIF                                                  ##PRVB = Y
     C*** END ADD *** 12/31/98 ****************************************
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   Z-ADD     *ZEROS        SCFND
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCEOB#
     C                   MOVE      *BLANKS       SCSYS
     C                   Z-ADD     *ZEROS        SCRFEE
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   MOVEL     *BLANK        SCSUB
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
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01C
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     0             SVRRN2
     C                   END
     C                   END
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
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
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
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
     C/EJECT
