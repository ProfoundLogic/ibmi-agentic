     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F0534R1 - PROVIDER/NETWORK SEARCH                        *
     H*****************************************************************
     H*  PROGRAMMER: C:20               PROJECT: NW001                *
     H*  CREATED: 03/22/94                                            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*                                                              **
     H*  02/18/98 C:62  Project: MC159                               **
     H*                 ** CHANGED NMPHL & NMHSL WITH FMS4L2/L3.     **
     H*                                                              **
     H*****************************************************************
     FF0534F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     F*
     FFMPDL     IF   E           K DISK
     F*** BEGIN CHG *** 02/18/98 **************************************
     F********** NMHSL   IF  E           K        DISK
     F********** NMPHL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 **************************************
     FSMUPL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FNMNWL     IF   E           K DISK

     D*
     D PW              S              5    DIM(10)                                NETWORK CODE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  GW                     1     50
     D                                     DIM(10)                                NETWORK CODE
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C* KEY LIST FOR THE PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*
     C* KEY LIST FOR THE PROVIDER PPO MASTER FILE
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           SCFEIN
     C**********           KFLD           SCMOD
     C*
     C* KEY LIST FOR THE HOSPITAL PPO MASTER FILE
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           SCFEIN
     C**********           KFLD           SCMOD
     C*** END   DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/18/98 **************************************
     C*
     C* KEY LIST PROVIDER NETWORK CONTRACT FILE
     C*
     C     S4KEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*** END   ADD *** 02/18/98 **************************************
     C*
     C* KEY LIST FOR THE USER PARM FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* KEY LIST FOR THE GROUP AUXILIARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C*
     C* KEY LIST FOR THE PPO NETWORK MASTER FILE
     C*
     C     NWKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCNWCD
     C*
     C*------------------------*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQNWCD            5
     C                   PARM                    QQRTCD            1
     C*
     C                   TIME                    DSDTTM
     C*
     C* GET THE USER PARM FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZEROS        UPCO#
     C                   Z-ADD     *ZEROS        UPSUB#
     C                   Z-ADD     *ZEROS        UPFND
     C                   END
     C*
     C* GET THE GROUP AUXILIARY RECORD
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       GW
     C                   END
     C*
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C     SCMOD         IFGT      *ZEROS
     C                   MOVE      '1'           *IN31
     C                   ELSE
     C                   MOVE      '0'           *IN31
     C                   END
     C*
     C*===============================================================
     C* INITIALIZATION    AREA
     C*
     C     TOP           TAG
     C                   EXSR      INZSR
     C*
     C*===============================================================
     C*
     C     MAINLN        TAG
     C*
     C*===============================================================
     C* SCREEN EXECUTION AREA
     C*
     C                   EXSR      SCREXC
     C*
     C*===============================================================
     C* HANDLE F KEYS IF PRESSED
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
     C*
     C*===============================================================
     C* CHECK FOR USER SELECTION
     C*
     C                   READC     WDW01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL         IFEQ      '1'
     C                   MOVEL     SCNWCD        QQNWCD
     C                   MOVE      ' '           QQRTCD
     C                   GOTO      TERM
     C*
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '5' TO INDICATE
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
     C*******************  GOTO MAINLN                     DEL 06/16/98
     C                   GOTO      TERM                                         ADD 06/16/98
     C                   END                                                    END 0 *IN50
     C*
     C                   GOTO      TOP                                          RE-SET SUBFILE
     C*
     C*===============================================================
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C                   RETURN
     C*
     C/EJECT
     C*====================================================================*
     C* * *   INZSR SUBROUTINE                                    I N I T ***
     C*====================================================================*
     C     INZSR         BEGSR
     C*
     C* CHAIN TO PROVIDER FILE TO VERIFY WHICH FILE FOR LOAD
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     PDNAME        SCNAME
     C                   MOVE      '0'           *IN30
     C     PDTYPE        IFEQ      '1'
     C                   MOVE      '0'           *IN30
     C                   ELSE
     C     PDTYPE        IFEQ      '4'                                          ELSE PDTYPE EQ '1'
     C                   MOVE      '1'           *IN30
     C                   END                                                    END PDTYPE EQ '4'
     C                   END                                                    END PDTYPE EQ '1'
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C                   MOVE      *BLANKS       SCNAME
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   MOVE      *BLANKS       PW
     C     *IN30         IFEQ      '0'
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** PHKEY     CHAINPPH01                60
     C     S4KEY         CHAIN     PS402                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     1             Z                 5 0
     C********** PHNWCD    LOKUPGW,Z                     70
     C     S4NWCD        LOOKUP    GW(Z)                                  70
     C     *IN70         IFEQ      '1'
     C**********           MOVELPHNWCD    PW,Z
     C                   MOVEL     S4NWCD        PW(Z)
     C                   END                                                    END *IN70
     C********** PHKEY     READEPPH01                    60
     C     S4KEY         READE     PS402                                  60
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       PW
     C********** HSKEY     CHAINPHS01                60
     C     S4KEY         CHAIN     PS401                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     1             Z
     C********** HSNWCD    LOKUPGW,Z                     70
     C     S4NWCD        LOOKUP    GW(Z)                                  70
     C     *IN70         IFEQ      '1'
     C**********           MOVELHSNWCD    PW,Z
     C                   MOVEL     S4NWCD        PW(Z)
     C                   END                                                    END *IN70
     C********** HSKEY     READEPHS01                    60
     C     S4KEY         READE     PS401                                  60
     C*** END   CHG *** 02/18/98 **************************************
     C                   END
     C*
     C                   END                                                    END *IN30
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
     C                   ENDSR
     C*====================================================================*
     C* * *   LOAD SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      *BLANKS       HLNWCD            5
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        10            X                 5 0
     C*
     C     PW(X)         IFNE      *BLANKS
     C                   MOVEL     PW(X)         SCNWCD
     C*
     C*
     C                   ADD       1             RRN2              5 0
     C*
     C* GET THE NETWORK CODE DESCRIPTION
     C*
     C     NWKEY         CHAIN     PNW01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     NWNAME        SCDESC
     C                   ELSE
     C                   MOVE      *BLANKS       SCDESC
     C                   END
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C                   END
     C*
     C* * * INZSRIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C                   MOVE      *BLANKS       SCNWCD
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             XXRRN2            5 0
     C                   MOVE      '1'           *IN06
     C                   WRITE     WDW01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     1             #@$2
     C*
     C                   ENDSR
     C/EJECT
     C*===============================================================
     C     SCREXC        BEGSR
     C*===============================================================
     C*
     C*
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
     C                   END
     C*
     C                   ENDSR
     C*
     C/EJECT
     C*===============================================================
     C     FKEYS         BEGSR
     C*===============================================================
     C*
     C     *INKA         IFEQ      '1'
     C                   END
     C*
     C     *INKB         IFEQ      '1'
     C                   END
     C*
     C*** BEG CHG *** 06/16/98 ************************************
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C***        *INKC     IFEQ '1'
     C***                  MOVE 'A'       QQRTCD
     C***                  MOVE '1'       *INLR
     C***                  CALL 'F0534R'
     C***                  GOTO TERM
     C***                  END                             END *INKC
     C*** END CHG *** 06/16/98 ************************************
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C*** BEG CHG *** 06/16/98 *************************************
     C***        Q2RECF    IFEQ 'WDW01'
     C***        Q2RECF    OREQ 'WDW01S'
     C***        Q2RECF    OREQ 'WDW01C'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      *BLANKS       QQNWCD
     C                   GOTO      TERM
     C***                  END
     C*** END CHG *** 06/16/98 *************************************
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
     C*
     C                   ENDSR
     C/EJECT
