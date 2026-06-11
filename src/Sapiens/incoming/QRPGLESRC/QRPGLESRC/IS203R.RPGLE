     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H****** IS203R - FAX IMAGE        SCREEN PGM                     *
     H***************************************************************
     H*  REVISIONS:                                                **
     H*                                                            **
     H*  07/13/00 C:128 PROJECT MC087                              **
     H*                 ON SELECTION OF FAX DETAIL WRONG RECORD    **
     H*                 COULD BE DISPLAYED. WITH THE ADVENT OF THE **
     H*                 RESEND FEATURE THE KEY TO THE LOGICAL FILE **
     H*                 BECAME NON UNIQUE. ADDED LOGICAL FILE WITH **
     H*                 A DEEPER KEY.                              **
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  21    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  22    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  23    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  24    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  25    INVALID RECIPIENT ID ENTERED                           *
     H*****************************************************************
     FIS203F    CF   E             WORKSTN USROPN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMFRL     IF   E           K DISK
     FIMOCL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
      ********BGN CHG 07/13/00 ***************************************
     F******IAFTL   IF  E           K        DISK
     FIAFTL0    IF   E           K DISK
      ********END CHG 07/13/00 ***************************************
     FIMCVL     IF   E           K DISK
     FIMSYP     IF   E             DISK
     F*
     F*

     D*
     D PCFAX           C                   CONST('ISNDPCFXC')
     D SNDFX           DS
     D  FTRCID                 1     10
     D  FTDESC                11     50
     D  FTOUTC                51     52
     D  FTFPH#                53     84
     D  FTCPTI                85    124
     D  FTCPT1               125    164
     D  FTCPT2               165    204
     D  FTCPT3               205    244
     D  FTCPF1               245    284                                           T
     D  FTCPF2               285    324
     D  FTCPF3               325    364
     D  FTCPCM               365    404                                            1
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  XXTIME                 1      6  0
     D  XXMIN                  3      4  0
     D  XXSEC                  5      6  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     FRKY          KLIST
     C                   KFLD                    QQRECP
     C*
     C*   KEY LIST FOR OUTPUT CLASS FILE
     C     OCKY          KLIST
     C                   KFLD                    FROUTC
     C*
     C*   KEY LIST FOR COVER PAGE MASTER FILE
     C     CVKY          KLIST
     C                   KFLD                    SCCVRI
     C*
     C     FTKY          KLIST
     C                   KFLD                    QXJOBN           10
     C                   KFLD                    QXUSRN           10
     C                   KFLD                    QXJOB#            6
     C                   KFLD                    QXFILN           10
     C                   KFLD                    QXFIL#            3
      ********BGN CHG 07/13/00 ***************************************
     C                   KFLD                    QXFCDT
     C                   KFLD                    QXFCTM
      ********END CHG 07/13/00 ***************************************
     C*
     C*
     C     PLCVR         PLIST
     C                   PARM                    QQRECP           10
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM      $$RTN         QQRTCD            1
     C                   PARM                    QQQID            10
     C                   PARM      $$CVRI        QQCVRI            2
     C                   PARM                    SCRCID                         RECIP ID
     C                   PARM                    SCDESC                         DESCRIPTION2
     C                   PARM                    SCOUTC                         OUTPUT CLASS
     C                   PARM                    SCFPH#                         TO FAX PHONE#
     C                   PARM                    SCCPTI
     C                   PARM                    SCCPT1                         TO--LINES 1-3
     C                   PARM                    SCCPT2
     C                   PARM                    SCCPT3
     C                   PARM                    SCCPF1                         CHANGED TO COMMENT
     C                   PARM                    SCCPF2                          LINES INSTEAD OF
     C                   PARM                    SCCPF3                           FROM LINES
     C                   PARM                    SCCPCM                         CHGD TO FROM LINE 1
     C                   PARM                    SCCVRI                         COVER ID          1
     C                   PARM      *BLANKS       ZZRTC             2            RTN CODES...
     C*
     C*
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C     *LIKE         DEFINE    OCDT          XXDATE
     C     *LIKE         DEFINE    OCDT          XXSDDT
     C*
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRECP           10
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQQID            10
     C                   PARM                    QQCVRI            2
     C                   PARM                    QXJOB#            6
     C                   PARM                    QXUSRN           10
     C                   PARM                    QXJOBN           10
     C                   PARM                    QXFILN           10
     C                   PARM                    QXFIL#            3
      ********BGN CHG 07/13/00 ***************************************
     C                   PARM                    QXFCDT            7 0          FAX DATE
     C                   PARM                    QXFCTM            6 0          FAX TIME
      ********END CHG 07/13/00 ***************************************
     C*
     C                   MOVE      QQSEQ         QQSEQ#            5 0
     C                   MOVE      *ZEROS        SCSDDT
     C                   MOVE      *ZEROS        SCSDTM
     C                   MOVE      QQRTCD        $$RTN             1
     C                   MOVE      QQCVRI        $$CVRI            2
     C     QQCVRI        IFEQ      '*R'
     C                   MOVEL     *BLANKS       QQCVRI
     C                   ENDIF
     C*
     C                   MOVEL     QQCVRI        SCCVRI
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
     C                   MOVEL     QQRECP        SCRCID
     C                   MOVEL     QQKEY         XXKEY            40
     C                   MOVE      QQSEQ         XXSEQ#            5 0
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C*
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *ON
     C     SYFXIF        ORNE      'Y'
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*                    MOVELQQPRDS    SCPRDS
     C*
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     TOP           TAG
     C                   MOVEL     'WDW02'       Q2RECF
      *
      *=============================================2=================
      ***===***===*** GET RECIPIENT INFORMATION TO LOAD WDW02 ***===***===
     C     FRKY          CHAIN     PFR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FRDESC        SCDESC                         DESCRIPTION
     C                   MOVEL     FROUTC        SCOUTC                         OUTPUT CLASS
     C                   MOVEL     FRFPH#        SCFPH#                         TO FAX PHONE#
     C                   MOVE      FRCPT1        SCCPT1                         TO--LINES 1-3
     C                   MOVE      FRCPT2        SCCPT2
     C                   MOVE      FRCPT3        SCCPT3
     C*
     C                   MOVE      *BLANKS       SCCPF1                         CHANGED TO COMMENT
     C                   MOVE      *BLANKS       SCCPF2                          LINES INSTEAD OF
     C                   MOVE      *BLANKS       SCCPF3                           FROM LINES
     C                   MOVE      *BLANKS       SCCPCM                         CHGD TO FROM LINE 1
     C**
     C                   MOVE      FRDSTM        SCSDTM
     C                   TIME                    DSDTTM
     C     FRDSTM        IFNE      *ZEROS
     C                   MOVE      DSDATE        SCSDDT
     C                   ENDIF
     C*  GET OUTPUT CLASS INFORMATION
     C*
     C     OCKY          CHAIN     POC01                              61
     C*
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCRCID
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCOUTC
     C                   MOVE      *BLANKS       SCFPH#
     C                   MOVE      *BLANKS       SCCPT1
     C                   MOVE      *BLANKS       SCCPT2
     C                   MOVE      *BLANKS       SCCPT3
     C                   MOVE      *BLANKS       SCCPF1
     C                   MOVE      *BLANKS       SCCPF2
     C                   MOVE      *BLANKS       SCCPF3
     C                   MOVE      *BLANKS       SCCPCM
     C                   END                                                    IF *IN60 = *OFF
     C*
     C     $$CVRI        CASEQ     '*R'          RSND
     C                   ENDCS
     C*
      *
     C     MAINLN        TAG
      *
      *===============================================================
      * SCREEN EXECUTION AREA
      *
     C  N96
     CAN 96              OPEN      IS203F
     C                   MOVE      *ON           *IN96
     C*
     C******* THIS SUBROUTINE IS NO LONGER EXECUTED DUE TO THE USE OF THE
     C******* COVER PAGE PROCESSING
     C  N60
     CAN 60              EXSR      SCREXC
      *
      *===============================================================
      * HANDLE F KEYS IF PRESSED
      *
     C  N60
     CAN 60*IN11         CASEQ     '1'           FKEYS
     C                   ENDCS                                                    END *IN11
      *
      *===============================================================
      *
     C  N60
     CAN 60Q2RECF        CASEQ     'WDW02 '      WDW02T
     C                   ENDCS
      *===============================================================
      *+++++++ NEW EXECUTION FOR COVER PAGE PROGRAMS!!!
      *===============================================================
      *
     C*** GET THE SYSTEM DEFAULTS FOR COVER PAGE ***
      *
     C     1             CHAIN     IMSYP                              60
     C*
     C     GETNEW        TAG
     C     SCCVRI        IFEQ      *BLANKS
     C                   MOVEL     SY0201        SCCVRI
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       ##SPGM
     C     CVKY          CHAIN     PCV01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CVSPGM        ##SPGM           10
     C                   ELSE
     C                   MOVEL     'ISCV@@R '    ##SPGM
     C                   ENDIF
     C*
     C** * CALL COVER PAGE PROGRAM ****
     C                   CALL      ##SPGM        PLCVR
     C* * * ERROR PROCESSING HERE ***
      *
      * * IF A REQUEST TO USE A DIFFERENT COVER IS SPECIFIED, THEN WE MUST
      * * GET IT AND CALL THE PROPER PROGRAM
     C     ZZRTC         CABEQ     'CV'          GETNEW
      *
      * * IF THE SCREEN PROGRAM ACCEPTED VALIDATED, AND COMBINED THE DATA
      * *   THEN WE CAN DO OUR STANDART PROCESSING AND SEND THE FAX.
     C     ZZRTC         CASEQ     '**'          SENDIT
     C                   ENDCS
      *
      * * GET IT AND CALL THE PROPER PROGRAM
     C     ZZRTC         IFEQ      'A '
      *
      *
     C                   MOVEL     'A'           QQRTCD
      *
      *
     C                   ENDIF
      *
      * * GET IT AND CALL THE PROPER PROGRAM
     C     ZZRTC         IFEQ      'B '
      *
      *
     C                   MOVEL     'B'           QQRTCD
      *
      *
     C                   ENDIF
      *===============================================================
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
      *===============================================================
     C     SCREXC        BEGSR
      *===============================================================
      *
      *
     C                   MOVEL     Q2FILE        SCFILE            8            SET FORMAT/FILE
     C*
     C*
     C     Q2RECF        IFEQ      'WDW02   '
     C                   EXFMT     WDW02
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
     C                   MOVE      'A'           QQRTCD            1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      *
     C     *INKD         IFEQ      '1'
     C                   END
      *
      *****************************************
     C*   F5 = FAX RECIPIENT SEARCH           *
     C*****************************************
      *
     C     *INKE         IFEQ      '1'
     C*
     C*   PARM LIST FAX RECIPIENT ID SEARCH
     C*
     C     PL0510        PLIST
     C                   PARM                    SCRCID
     C                   PARM                    QQRTC             1
     C                   CALL      'I0510R'      PL0510
     C*
     C                   MOVEL     'WDW02'       Q2RECF
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCRCID        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           TOP
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
      *** F10 - SEARCH (COVER PAGE FILE )
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
     C     Q2RECF        OREQ      'WDW02'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*
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
     C*
     C                   GOTO      MAINLN
      *
     C                   ENDSR
      *===================WDW02T======================================*
     C     WDW02T        BEGSR
     C**** EDIT FIELDS ENTERED HERE **************
     C*
     C*
     C*  FAX PHONE # CANNOT BE EQUAL TO BLANKS
     C*
     C     SCFPH#        CABEQ     *BLANKS       MAINLN                   20
     C*
     C*  SEND DATE MUST BE VALID AND >= TO CURRENT DATE
     C*
     C     SCSDDT        IFEQ      *ZERO
     C     SCSDTM        ANDEQ     *ZERO
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        XXSDD             6 0
     C                   MOVE      DSTIME        XXSDTM            6 0
     C                   MOVE      '1'           XXCURR            1
     C                   GOTO      CURTAG
     C                   END
     C*
     C     SCSDDT        CABEQ     *ZEROS        MAINLN                   21
     C*
     C                   Z-ADD     SCSDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSDDT
     C*
     C     SRERR         CABEQ     '1'           MAINLN                   21
     C*
     C                   Z-ADD     SCSDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     XXSDDT        CABLT     XXDATE        MAINLN                 22
     C*
     C*  SEND TIME MUST BE GREATER THAN 0 AND LESS THAN 24:00:01
     C*
     C     SCSDTM        CABLE     *ZEROS        MAINLN                 2323
     C     SCSDTM        CABGT     240000        MAINLN               23
     C                   Z-ADD     SCSDTM        XXTIME
     C     XXMIN         CABGT     59            MAINLN               23
     C     XXSEC         CABGT     59            MAINLN               23
     C*
     C*
     C                   MOVE      SCSDDT        XXSDD             6 0
     C                   MOVE      SCSDTM        XXSDTM            6 0
     C*
     C*
     C     CURTAG        TAG
     C*
     C*  SEND TIME MUST BE WITHIN ALLOWED TRANSMI8SION TIME PERIOD.
     C*  I.E., WITHIN TRANSMISSION PERIOD AND NOT WITHIN HOLD PERIOD
     C*    NOTE: DON'T EDIT IF NO RECIPIENT RECORD IS FOUND (*IN61=1).
     C*
     C  N60OCACTN        IFEQ      'H'                                          H = HOLD
     C     XXSDTM        IFGE      OCFTIM                                       HOLD PERIOD
     C     XXSDTM        ANDLE     OCTTIM                                       |
     C                   MOVE      '1'           *IN24
     C                   END
     C                   ELSE                                                   T = TRANSMIT
     C     XXSDTM        IFLT      OCFTIM                                       TRANSMIT PERIOD
     C     XXSDTM        ORGT      OCTTIM                                       |
     C                   MOVE      '1'           *IN24
     C                   END
     C                   END
     C*
     C**** COVER PAGE ID MUST BE VALID *****
     C****  SCCVRI / SCCVRD
     C*
     C     CVKY          CHAIN     PCV01                              60
     C     *IN60         CABEQ     *ON           MAINLN                              ???ERROR IND
     C*
     C     CVSTAT        CABEQ     'I'           MAINLN                              ??? ERR INDC
     C***
     C*
     C*
     C*  COVER PAGE TITLE CANNOT BE EQUAL TO BLANKS
     C*
     C     SCCPTI        CABEQ     *BLANKS       MAINLN                   25
     C*
     C*
     C*  THE TO: INFORMATION CANNOT BE BLANK
     C*
     C     SCCPT1        IFEQ      *BLANKS
     C     SCCPT2        ANDEQ     *BLANKS
     C     SCCPT2        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN26
     C                   GOTO      MAINLN
     C                   END
     C*
     C*  THE NEW FROM: INFORMATION CANNOT BE BLANK
     C*
     C     SCCPCM        CABEQ     *BLANKS       MAINLN                   27
     C*
     C*
     C*  ALL EDITS PASSED.
     C*  AND CALL CL TO SEND FAX.
     C*
     C                   MOVE      SCSDTM        QQSDTM            6            PARMS FOR "SNDFAX"
     C                   MOVE      SCSDDT        QQSDDT            8
     C                   TIME                    DSDTTM                         GET TIME FOR UPDATE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      'C'           QQRTCD                         INC. COVER PG
     C*
      ******========================================================
      ******====== LOAD  TRANSACTION FIELDS ========================
      ******========================================================
      *
     C                   EXSR      LTRN
      *
     C*** SBMJOB  HERE **** ???
     C*** PASS PARMS , DONT FORGET THE SEND TIME/DATE... ETC.
     C***
     C***
     C**** DO THE RESEND FOR TYPE "S", BY CALLING DIRECTLY ***
     C*
     C     $$RTN         IFEQ      'S'
     C                   CALL      PCFAX
     C                   PARM                    FTFPH#
     C                   PARM                    FTCPT1
     C                   PARM                    FTCPT2
     C                   PARM                    FTCPT3
     C                   PARM                    FTFILN
     C                   PARM                    FTJOB#
     C                   PARM                    FTUSRN
     C                   PARM                    FTJOBN
     C                   PARM                    FTFIL#
     C                   PARM                    FTCPTI
     C                   PARM                    FTCPF1
     C                   PARM                    FTCPF2
     C                   PARM                    FTCPF3
     C                   PARM                    FTCPCM
     C                   PARM                    QQSDDT
     C                   PARM                    QQSDTM
     C                   PARM                    $$RTN
     C                   PARM      SCCVRI        QQCVRI
     C*
     C                   ELSE
     C                   CALL      'IS203C1'
     C                   PARM                    SNDFX
     C                   PARM                    QQSDDT
     C                   PARM                    QQSDTM
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOB
     C                   PARM                    QQDCID
     C                   PARM                    QQFOLD
     C                   PARM                    QQKEY
     C                   PARM                    QQSEQ
     C                   PARM                    $$RTN
     C                   PARM                    QQQID
     C                   PARM      SCCVRI        QQCVRI
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C     LTRN          BEGSR
     C*
     C*
     C                   MOVEL     SCRCID        FTRCID                         RECIP ID
     C                   MOVEL     SCDESC        FTDESC                         DESCRIPTION2
     C                   MOVEL     SCOUTC        FTOUTC                         OUTPUT CLASS
     C                   MOVEL     SCFPH#        FTFPH#                         TO FAX PHONE#
     C                   MOVEL     SCCPTI        FTCPTI
     C                   MOVEL     SCCPT1        FTCPT1                         TO--LINES 1-3
     C                   MOVEL     SCCPT2        FTCPT2
     C                   MOVEL     SCCPT3        FTCPT3
     C                   MOVEL     SCCPF1        FTCPF1                         CHANGED TO COMMENT
     C                   MOVEL     SCCPF2        FTCPF2                          LINES INSTEAD OF
     C                   MOVEL     SCCPF3        FTCPF3                           FROM LINES
     C                   MOVEL     SCCPCM        FTCPCM                         CHGD TO FROM LINE 1
     C*
     C                   ENDSR
     C     RSND          BEGSR
     C*
     C*
     C     FTKY          CHAIN     PFT01                              67
     C     *IN67         IFEQ      *OFF
     C                   MOVEL     FTRCID        SCRCID
     C                   MOVEL     FTDESC        SCDESC
     C                   MOVEL     FTOUTC        SCOUTC
     C                   MOVEL     FTFPH#        SCFPH#
     C                   MOVEL     FTCPTI        SCCPTI
     C                   MOVEL     FTCPT1        SCCPT1
     C                   MOVEL     FTCPT2        SCCPT2
     C                   MOVEL     FTCPT3        SCCPT3
     C                   MOVEL     FTCPF1        SCCPF1
     C                   MOVEL     FTCPF2        SCCPF2
     C                   MOVEL     FTCPF3        SCCPF3
     C                   MOVEL     FTCPCM        SCCPCM
     C                   MOVEL     FTCVRI        SCCVRI
     C****
     C                   ENDIF
     C     ERSND         ENDSR
     C****
     C****
     C*====================================================================
     C     SENDIT        BEGSR
     C*====================================================================
     C*
     C*      CALL CL TO SEND FAX.
     C*
     C                   MOVE      SCSDTM        QQSDTM            6            PARMS FOR "SNDFAX"
     C                   MOVE      SCSDDT        QQSDDT            8
     C                   TIME                    DSDTTM                         GET TIME FOR UPDATE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      'C'           QQRTCD                         INC. COVER PG
     C*
      ******========================================================
      ******====== LOAD  TRANSACTION FIELDS ========================
      ******========================================================
      *
     C                   EXSR      LTRN
      *
     C*** SBMJOB  HERE **** ???
     C*** PASS PARMS , DONT FORGET THE SEND TIME/DATE... ETC.
     C*
     C     $$RTN         IFEQ      'S'
     C                   CALL      PCFAX
     C                   PARM                    FTFPH#
     C                   PARM                    FTCPT1
     C                   PARM                    FTCPT2
     C                   PARM                    FTCPT3
     C                   PARM                    FTFILN
     C                   PARM                    FTJOB#
     C                   PARM                    FTUSRN
     C                   PARM                    FTJOBN
     C                   PARM                    FTFIL#
     C                   PARM                    FTCPTI
     C                   PARM                    FTCPF1
     C                   PARM                    FTCPF2
     C                   PARM                    FTCPF3
     C                   PARM                    FTCPCM
     C                   PARM                    QQSDDT
     C                   PARM                    QQSDTM
     C                   PARM                    $$RTN
     C                   PARM      SCCVRI        QQCVRI
     C*
     C                   ELSE
     C                   CALL      'IS203C1'
     C                   PARM                    SNDFX
     C                   PARM                    QQSDDT
     C                   PARM                    QQSDTM
     C                   PARM                    Q1USER
     C                   PARM                    Q1JOB
     C                   PARM                    QQDCID
     C                   PARM                    QQFOLD
     C                   PARM                    QQKEY
     C                   PARM                    QQSEQ
     C                   PARM                    $$RTN
     C                   PARM                    QQQID
     C                   PARM      SCCVRI        QQCVRI
     C                   ENDIF
     C*
     C     ESNDIT        ENDSR
