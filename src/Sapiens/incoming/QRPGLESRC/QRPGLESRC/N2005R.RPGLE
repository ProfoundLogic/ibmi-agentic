     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE N2005R - PRE-CERT/SERVICE# SEARCH                        *
     H*****************************************************************
     H*                                                               *
     H*     ***** N O T E *****                                       *
     H*                                                               *
     H*     TRIED TO CONVERT PROGRAM TO BE ABLE TO USE NEW PROVIDER   *
     H*     BILLS, BUT THERE MAY BE SOME ISSUES STILL COME UP.  I     *
     H*     COULD NOT DETERMINE THE PURPOSE OF SOME OF THE CODE, SO   *
     H*     IT HAS BEEN COMMENTED OUT.                                *
     H*                                                               *
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
     H* 12/31/98  C:94  PROJECT: MC152                                *
     H*                 CHANGE PROGRAM TO GET INFO FROM FAEAP         *
     H*                 IF ##PRVB = Y                                 *
     H*                                                               *
     H* 12/05/96  C:56  Project: AR018                                *
     H*                 Obtain PCSTAT for output                      *
     H*****************************************************************
     FN2005F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FSMUPL     IF   E           K DISK
     FNAPBL2    IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FUMPCL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
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
     C*   KEY LIST FOR THE PRE-CERT BILLING FILE
     C*
     C     PBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C*
     C     PBKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE EOMB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    PBCO#
     C                   KFLD                    PBSUB#
     C                   KFLD                    PBFND
     C                   KFLD                    PBFYR
     C                   KFLD                    PBCASE
     C                   KFLD                    PBEOB#
     C*
     C*   KEY LIST FOR THE USER PARM FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE PRE-CERT FILE
     C*
     C     PCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCCERT
     C                   KFLD                    SCEXT
     C*** BEG ADD *** 12/31/98 ***************************************
     C*
     C     EOKY1         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C*
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
     C     *LIKE         DEFINE    PBCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PBSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PBFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15A
     C     *LIKE         DEFINE    PCEXT         XXEXT                          3.0N
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
     C                   MOVEL     SCCERT        XXCERT
     C                   Z-ADD     *ZEROS        SCSUB
     C                   Z-ADD     *ZEROS        SCFND#
     C                   MOVE      *BLANKS       SCCER#
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
     C*** BEG ADD *** 12/31/98 ***************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C*
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         SETLL     PEA01                              60
     C                   ELSE
     C*
     C     PBKEY         SETLL     PPB01                              60
     C                   ENDIF
     C*** END ADD *** 12/31/98 ***************************************
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
     C*** BEG ADD *** 12/31/98 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         READE     PEA01                                  60
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   ELSE
     C*
     C     EOKY1         CHAIN     PEO01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   ADD       1             RRN2              5 0
     C                   Z-ADD     EOSUB#        SCSUB#
     C                   Z-ADD     EOFND         SCFND
     C                   Z-ADD     EOFYR         SCFYR
     C                   Z-ADD     EOCASE        SCCASE
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   MOVEL     EOCERT        SCCERT
     C                   Z-ADD     EOEXT         SCEXT
     C                   Z-ADD     EOFEIN        SCFEIN
     C*
     C* CHAIN TO PRE-CERT FILE TO GET THE ADMIT DATE
     C*
     C     PCKEY         CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*
     C                   MOVE      PCSTAT        SCSTAT
     C*
     C                   END
     C*
     C* CHAIN TO CLAIMANT DEMOGRAPHICS FILE TO GET THE CLAIMANT NAME
     C*
     C     PBSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCNAME
     C                   ELSE
     C                   MOVEL     XXLAST        SCNAME
     C                   END                                                    END CDFSN
     C                   END
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C*
     C                   ENDIF
     C                   ELSE
     C*
     C*** END ADD *** 12/31/98 ***************************************
     C*
     C     PBKY1         READE     PPB01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*** BEG DEL *** 12/31/98 ****************************************
     C**********
     C********** QQCALL    IFEQ '1'
     C********** EOKEY     CHAINPEO01                60
     C********** *IN60     IFEQ '0'
     C********** EOFLG5    IFEQ 'Y'
     C**********           GOTO NXTRCD
     C**********           END
     C**********           END
     C**********           ELSE
     C********** QQCALL    IFEQ '2'
     C********** EOKEY     CHAINPEO01                60
     C********** *IN60     IFEQ '0'
     C********** EOFLG5    IFNE 'Y'
     C**********           GOTO NXTRCD
     C**********           END
     C**********           END
     C**********           END
     C**********           END
     C*** END DEL *** 12/31/98 ****************************************
     C*
     C                   ADD       1             RRN2              5 0
     C                   Z-ADD     PBSUB#        SCSUB#
     C                   Z-ADD     PBFND         SCFND
     C                   Z-ADD     PBFYR         SCFYR
     C                   Z-ADD     PBCASE        SCCASE
     C                   Z-ADD     PBEOB#        SCEOB#
     C                   MOVEL     PBCERT        SCCERT
     C                   Z-ADD     PBEXT         SCEXT
     C                   Z-ADD     PBFEIN        SCFEIN
     C*
     C* CHAIN TO PRE-CERT FILE TO GET THE ADMIT DATE
     C*
     C     PCKEY         CHAIN     PPC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*
     C                   MOVE      PCSTAT        SCSTAT
     C*
     C                   END
     C*
     C* CHAIN TO CLAIMANT DEMOGRAPHICS FILE TO GET THE CLAIMANT NAME
     C*
     C     PBSS#         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C     CDFSN         IFNE      *BLANK
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCNAME
     C                   ELSE
     C                   MOVEL     XXLAST        SCNAME
     C                   END                                                    END CDFSN
     C                   END
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   ENDIF
     C                   ENDDO
     C*
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   Z-ADD     *ZEROS        SCFND
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCEOB#
     C                   MOVE      *BLANKS       SCCERT
     C                   MOVE      *BLANKS       SCNAME
     C                   Z-ADD     *ZEROS        SCFEIN
     C                   Z-ADD     *ZEROS        SCEXT
     C*
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
