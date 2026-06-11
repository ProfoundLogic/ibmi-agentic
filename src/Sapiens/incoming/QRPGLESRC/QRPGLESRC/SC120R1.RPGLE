     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SC120R1 - SEND FAX GENERIC SPOOLED FILE SEND
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program collects all fascimile transmission requests  *
     H*    and initiates the sending of fax documents.                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    SNDFXC - Pgm that actually performs the fax transmission   *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:                                                     *
     H******************************************************************
     H*  REVISION:                                                    *
     H*  6/10/92  TAA  PROJ:AOK  COMMENTED OUT UPDATE OF QHUSER       *
     H******************************************************************
     H* INDICATOR USAGE                                               **
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    FAX PHONE # REQUIRED                                   **
     H*  21    INVALID SEND DATE ENTERED                              **
     H*  22    SEND DATE MUST BE >= CURRENT DATE                      **
     H*  23    INVALID SEND TIME ENTERED                              **
     H*  24    SEND TIME NOT WITHIN ALLOWED TRANSMISSION PERIOD       **
     H*  25    TITLE MUST BE ENTERED                                  **
     H*  62    CHAIN TO OUTPUT CLASS FILE                             **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMOCL   (POC01): OUTC                                      *
     H*****************************************************************
     FSC120F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMOCL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FSAFTP     O    E             DISK
     F*

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
     D                 DS
     D  XXTIME                 1      6  0
     D  XXMIN                  3      4  0
     D  XXSEC                  5      6  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** LDA ***
     D LDA            UDS
     D  DUMMY                  1     51
     D  CPCM                 121    160
      ****
     D FC6             C                   CONST('                    -
     D                                        ')

     C*
     C     *DTAARA       DEFINE    *LDA          LDA
     C*
     C     *LIKE         DEFINE    OCDT          XXDATE
     C     *LIKE         DEFINE    OCDT          XXSDDT
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQJOB#            6            IF TYPE = D
     C                   PARM                    QQJOBN           10            |
     C                   PARM                    QQUSRN           10            |
     C                   PARM                    QQFILN           10            |
     C                   PARM                    QQFIL#            3            |
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQRTCD            1
     C*
     C*   PARM LIST FOR "SENDFAX" CL PGM
     C*
     C     PLSEND        PLIST
     C                   PARM                    SCFPH#
     C                   PARM                    SCCPT1
     C                   PARM                    SCCPT2
     C                   PARM                    SCCPT3
     C                   PARM                    QQFILN
     C                   PARM                    QQJOB#
     C                   PARM                    QQUSRN
     C                   PARM                    QQJOBN
     C                   PARM                    QQFIL#
     C                   PARM                    SCCPTI
     C                   PARM                    SCCPF1
     C                   PARM                    SCCPF2
     C                   PARM                    SCCPF3
     C                   PARM                    SCCPCM
     C                   PARM                    QQSDTM            6
     C                   PARM                    QQSDDT            6
     C                   PARM      ' '           QQRTCD            1
     C*
     C*   KEY LIST FOR OUTPUT CLASS FILE
     C*
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
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XXTIME
     C*
     C     'SY'          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *ON
     C     SYFXIF        ORNE      'Y'
     C                   GOTO      TERM
     C                   ENDIF
     **
     **
     C                   MOVE      '0'           *IN40
     C*
     C*  GET THE RECIPIENT INFORMATION AND LOAD SCREEN FIELDS
     C*
     C                   MOVE      *BLANKS       SCRCID
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCOUTC
     C                   MOVE      *BLANKS       SCFPH#
     C                   MOVE      *BLANKS       SCCPTI
     C                   MOVE      *BLANKS       SCCPT1
     C                   MOVE      *BLANKS       SCCPT2
     C                   MOVE      *BLANKS       SCCPT3
     C                   MOVE      *BLANKS       SCCPF1
     C                   MOVE      *BLANKS       SCCPF2
     C                   MOVE      *BLANKS       SCCPF3
     C                   MOVE      *BLANKS       SCCPCM
     **
     C                   MOVE      *ZEROS        SCSDDT
     C                   MOVE      *ZEROS        SCSDTM
     C*
     C*  GET OUTPUT CLASS INFORMATION
     C*
     C                   READ      POC01                                  62
     C*
     C*   LOAD THE SCREEN  RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
     C     *IN10         IFEQ      '1'
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C                   END                                                    END *IN11
     C*************************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  FAX PHONE # CANNOT BE EQUAL TO BLANKS
     C*
     C     SCFPH#        CABEQ     *BLANKS       STRT                     20
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
     C     SCSDDT        CABEQ     *ZEROS        STRT                     21
     C*
     C                   Z-ADD     SCSDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSDDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     21
     C*
     C*
     C                   Z-ADD     SCSDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     XXSDDT        CABLT     XXDATE        STRT                   22
     C*
     C*  SEND TIME MUST BE GREATER THAN 0 AND LESS THAN 24:00:01
     C*
     C     SCSDTM        CABLE     *ZEROS        STRT                   2323
     C     SCSDTM        CABGT     240000        STRT                 23
     C                   Z-ADD     SCSDTM        XXTIME
     C     XXMIN         CABGT     59            STRT                 23
     C     XXSEC         CABGT     59            STRT                 23
     C*
     C*
     C                   MOVE      SCSDDT        XXSDD             6 0
     C                   MOVE      SCSDTM        XXSDTM            6 0
     C*
     C*
     C     CURTAG        TAG
     C*
     C*  SEND TIME MUST BE WITHIN ALLOWED TRANSMISSION TIME PERIOD.
     C*  I.E., WITHIN TRANSMISSION PERIOD AND NOT WITHIN HOLD PERIOD
     C*    NOTE: DON'T EDIT IF NO RECIPIENT RECORD IS FOUND (*IN61=1).
     C*
     C  N62OCACTN        IFEQ      'H'                                          H = HOLD
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
     C*  COVER PAGE TITLE CANNOT BE EQUAL TO BLANKS
     C*
     C     SCCPTI        CABEQ     *BLANKS       STRT                     25
     C*
     C*  AND CALL CL TO SEND FAX.
     C*
     C                   MOVE      XXSDD         SCSDDT
     C                   MOVE      XXSDTM        SCSDTM
     C*
     C*
     C                   MOVE      SCSDTM        QQSDTM                         PARMS FOR "SNDFAX"
     C                   MOVE      SCSDDT        QQSDDT
     C                   TIME                    DSDTTM                         GET TIME FOR UPDATE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C****????????*****    MOVE 'C'       QQRTCD           INC. COVER PG
     C*
     C                   EXSR      LTRN
      *
     C*** SBMJOB  HERE **** ???
     C*
     C*** SEND FAX HERE ====
     C*
     C                   MOVEL     SCCPCM        CPCM
     C*
     C                   OUT       LDA
     C*
     C                   CALL      'SNDFXC'      PLSEND
     C     QQRTCD        IFNE      *BLANKS
      * ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ******
     C                   MOVE      'E'           XXRTCD            1
     C                   GOTO      TERM
     C                   ENDIF
      *
      *** WRITE TRANSACTION RECORD ==========================
      *** BEGIN ADD *** 11/18/99 ***********************************************
      ** Clear the LDA to avoid data decimal errors in the future.
     C                   MOVE      *BLANKS       LDA
     C                   OUT       LDA
      *** END   ADD *** 11/18/99 ***********************************************
      *
     C                   EXSR      WTRN
      *
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C     XXRTCD        IFEQ      'E'
     C                   MOVE      'E'           QQRTCD
     C                   ELSE
     C                   MOVE      ' '           QQRTCD
     C                   END
     C*
     C                   MOVE      '1'           *INLR
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
      *===============================================================
     C*
     C     WTRN          BEGSR
     C*
     C                   CLEAR                   PFT01
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
     C                   MOVE      *ZEROS        FTFCDT
     C                   MOVE      *ZEROS        FTFCTM
     C                   MOVEL     *BLANKS       FTFAXC
     C*
     C*** JOB AND SPOOLED FILE INFO HERE ****
     C                   MOVE      QQJOB#        FTJOB#
     C                   MOVEL     QQUSRN        FTUSRN
     C                   MOVEL     QQJOBN        FTJOBN
     C                   MOVEL     QQFILN        FTFILN
     C                   MOVE      QQFIL#        FTFIL#
     C                   MOVE      *ZEROS        FTRCNT
     C*
     C                   MOVEL     Q1USER        FTSUSR
     C                   MOVEL     Q1JOB         FTSJOB
     C*
     C                   MOVE      'A'           FTSTAT
     C                   MOVE      Q1USER        FTUSER
     C                   MOVE      Q1PGM         FTPGM
     C                   Z-ADD     DSTIME        FTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FTDT
     C*
     C*
     C                   WRITE     PFT01
     C*
     C                   ENDSR
