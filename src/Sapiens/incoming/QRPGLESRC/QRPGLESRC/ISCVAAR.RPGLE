     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H****** ISCVAAR - FAX COVER PAGE / SEND FAX SCREEN "AA"          *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  21    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  22    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  23    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  24    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  25    INVALID RECIPIENT ID ENTERED                           *
     H*****************************************************************
     FISCVAAF   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMFRL     IF   E           K DISK
     FIMOCL     IF   E           K DISK
     FIMCVL     IF   E           K DISK
     F*
     F*

     D*
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
     C                   KFLD                    SCRCID
     C*
     C*   KEY LIST FOR OUTPUT CLASS FILE
     C     OCKY          KLIST
     C                   KFLD                    FROUTC
     C*
     C*   KEY LIST FOR COVER PAGE FILE
     C     CVKY          KLIST
     C                   KFLD                    SCCVRI
     C*
     C     FTKY          KLIST
     C                   KFLD                    QQDCID
     C                   KFLD                    QQKEY
     C                   KFLD                    QQSEQ#
     C                   KFLD                    QQQID
     C                   KFLD                    QQRECP
     C                   KFLD                    Q1USER
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
     C                   PARM                    SCRCID           10            RECIP ID
     C                   PARM                    SCDESC           40            DESCRIPTION2
     C                   PARM                    SCOUTC            2            OUTPUT CLASS
     C                   PARM                    SCFPH#           32            TO FAX PHONE#
     C                   PARM                    SCCPTI           40
     C                   PARM                    SCCPT1           40            TO--LINES 1-3
     C                   PARM                    SCCPT2           40
     C                   PARM                    SCCPT3           40
     C                   PARM                    SCCPF1           40            CHANGED TO COMMENT
     C                   PARM                    SCCPF2           40             LINES INSTEAD OF
     C                   PARM                    SCCPF3           40              FROM LINES
     C                   PARM                    SCCPCM           40            CHGD TO FROM LINE 1
     C                   PARM                    SCCVRI            2            COVER ID          1
     C                   PARM                    ZZRTC             2            RTN CODES...
     C*
     C                   MOVEL     SCCVRI        $HLDCV            2
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
     C     CVKY          CHAIN     PCV01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CVDESC        SCCVRD
     C                   ENDIF
     C*
     C** D E L ** ** ***** MOVELQQCVRI    SCCVRI
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
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
      *
     C                   EXSR      BSDTA                                           BLD SCRN DTA
     C*
     C     FRKY          CHAIN     PFR01                              43
     C     OCKY          CHAIN     POC01                              44
     C     *IN44         IFEQ      *ON
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C*
     C*
     C*
      *===============================================================
      * INITIALIZATION    AREA
      *
     C     TOP           TAG
     C                   MOVEL     'WDW02'       Q2RECF
      *
      *
     C     MAINLN        TAG
      *
      *===============================================================
      * SCREEN EXECUTION AREA
      *
     C                   EXSR      SCREXC
      *
     C                   EXSR      BSDTA2
      *
      *===============================================================
      * HANDLE F KEYS IF PRESSED
      *
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
      *
      *===============================================================
      *
     C     Q2RECF        CASEQ     'WDW02 '      WDW02T
     C                   ENDCS
      *
     C                   EXSR      BSDTA2
      * * * THIS IS A VALID SELECTION INDICATION * * *
     C                   MOVEL     '**'          ZZRTC
      *
     C*
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
     C** ** DEL *          MOVE 'A'       QQRTCD  1
     C                   MOVEL     'A '          ZZRTC
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
     C                   MOVEL     'WDW02'       Q2RECF
     C                   GOTO      MAINLN
     C                   END
     C*
     C* * * FILL THE DATE IN FROM THE FR FILE
     C*
     C     QQRTC         CASEQ     '*'           GETRCP
     C                   ENDCS
     C     QQRTC         CABEQ     '*'           TOP
     C*
     C                   ENDIF                                                   END *INKE
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
     C*
     C     FLD           IFEQ      'SCCVRI  '
     C*
     C                   CALL      'IS142R'
     C                   PARM                    SCCVRI
     C                   PARM                    @#DSC            40
     C                   PARM      'S'           @#RTCD            1
     C*
     C     @#RTCD        IFEQ      'A'
     C                   MOVEL     'A'           ZZRTC
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     @#RTCD        IFEQ      '*'
     C                   MOVEL     @#DSC         SCCVRD
     C                   MOVEL     'CV'          ZZRTC
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C*
     C                   ENDIF
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
     C** D E L ** **       MOVE 'B'       QQRTCD
     C                   MOVEL     'B '          ZZRTC
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
      *
      *===================WDW02T======================================*
      *
      *
     C     WDW02T        BEGSR
      *
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
     C     *IN43         IFEQ      *OFF
     C     *IN44         ANDEQ     *OFF
     C*
     C     OCACTN        IFEQ      'H'                                          H = HOLD
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
     C                   ENDIF
     C*
     C*
     C**** COVER PAGE ID MUST BE VALID *****
     C****  SCCVRI / SCCVRD
     C*
     C     CVKY          CHAIN     PCV01                              60
     C     *IN60         CABEQ     *ON           MAINLN                   28
     C* * * IF COVER HAS CHANGED, THEN WE NEED TO RETURN TO THE CALLER
     C* * * HERE.
     C     $HLDCV        IFNE      SCCVRI
     C                   MOVE      'CV'          ZZRTC
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C***
     C*
     C*
     C*  COVER PAGE TO:   CANNOT BE EQUAL TO BLANKS
     C*
     C     SCCPTI        CABEQ     *BLANKS       MAINLN                   25
     C*
     C*
     C*  THE CLM NAME  26    CANNOT BE BLANK
     C*
     C     SCCPT1        CABEQ     *BLANKS       MAINLN                   26
     C*  THE CLAIM#/DOI CANT BE = BLANKS
     C     SCCLM#        CABEQ     *BLANKS       MAINLN                   29
     C*
     C                   Z-ADD     SCDOI         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFNE      '1'
     C                   MOVEL     SRDTO         SCDOI
     C                   WRITE     WDW02
     C                   ELSE
     C                   MOVE      *ON           *IN30
     C                   GOTO      MAINLN
     C                   ENDIF
     C*
     C     SRERR         CABEQ     '1'           MAINLN                   30
     C*
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
     C*
     C                   ENDSR
     C*
      *=============================================2=================
     C     GETRCP        BEGSR
      *=============================================2=================
      ***===***===*** GET RECIPIENT INFORMATION
     C     FRKY          CHAIN     PFR01                              43
     C     *IN43         IFEQ      *OFF
     C                   MOVEL     FRRCID        QQRECP
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
     C     OCKY          CHAIN     POC01                              44
     C*
     C*
     C                   ELSE
     C                   END                                                    IF *IN60 = *OFF
     C*
     C     EGRCP         ENDSR
     C*
     C*
      *=============================================2=================
     C     BSDTA         BEGSR
      *=============================================2=================
     C*
     C     24            SUBST     SCCPT2:1      SCCLM#
     C     10            SUBST     SCCPT2:31     #$DOI            10
     C*
     C     2             SUBST     #$DOI:1       #$MM              2
     C     2             SUBST     #$DOI:4       #$DD              2
     C     4             SUBST     #$DOI:7       #$YY              4
     C*
     C     #$MM          CAT(P)    #$DD:0        #$MDY             8
     C                   CAT(P)    #$YY:0        #$MDY
     C*
     C     #$MDY         IFEQ      *BLANKS
     C                   MOVE      *ZEROS        SCDOI
     C                   ELSE
     C                   TESTN                   #$MDY                68
     C  N68              MOVE      *ZEROS        SCDOI
     C   68              MOVEL     #$MDY         SCDOI
     C                   ENDIF
     C*
     C     EBSCN         ENDSR
     C*
      *=============================================2=================
     C     BSDTA2        BEGSR
      *=============================================2=================
     C*
     C                   MOVE      SCDOI         #$MDY8            8
     C     2             SUBST     #$MDY8:1      #$MM8             2
     C     2             SUBST     #$MDY8:3      #$DD8             2
     C     4             SUBST     #$MDY8:5      #$YY8             4
     C*
     C     SCCLM#        CAT(P)    ' '           ##25B            25
     C     ##25B         CAT(P)    'DOI:'        SCCPT2
     C                   CAT(P)    #$MM8:1       SCCPT2
     C                   CAT(P)    '/':0         SCCPT2
     C                   CAT(P)    #$DD8:0       SCCPT2
     C                   CAT(P)    '/':0         SCCPT2
     C                   CAT       #$YY8:0       SCCPT2
     C*
     C     EBSCN2        ENDSR
