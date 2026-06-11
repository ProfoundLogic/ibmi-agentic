     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS142R1 - FAX ROUTING - ADD/UPDATE COVER PAGE ENTRIES
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    This program allows the user to add, UPDATE A COVER PAGE   *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    ID  NOT VALID
     H*  21    CVID            ALREADY EXISTS                        **
     H*  22    DESCRIPTION CANT BE BLANK                              **
     H*  23
     H*  24    PRINTER FILE ID NOT VALID                            **
     H*  25    SCREEN PROGRAM ID NOT VALID                           **
     H*
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMCVL   (PCV01): CVCVID                                    *
     H*
     H*****************************************************************
     FIS142F1   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMCVL     UF A E           K DISK

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     DstandardKeys     s                   like(av_keys)

     C*
     C*   KEY LIST FOR DOC INDEX FILE
     C     CVKY1         KLIST
     C                   KFLD                    XXCVID            2
     C*
     C*   KEY LIST FOR DOC INDEX FILE
     C     CVKY          KLIST
     C                   KFLD                    XXCVID            2
     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCVID            2
     C                   PARM                    QQDESC           40
     C                   PARM                    QQCALL            1
     C*
      /Free
       scPrDs = 'Add/Change Cover Page Entry';
       standardKeys = 'F3=Exit'  + '   ' +   'F5=Display Cover Page' + '   '
        + 'F12=Cancel' + '   ' + 'F20=Change Sts';
      /End-Free
     C*
     C                   TIME                    DSDTTM
     C*
     C     TOP           TAG
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVEL     QQCVID        XXCVID
     C                   MOVEL     QQDESC        XXDESC           40
     C     CVKY1         CHAIN     PCV01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PCV01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVEL     CVCVID        SCCVID
     C                   MOVEL     CVDESC        SCDESC
     C                   MOVEL     CVPRTF        SCPRTF
     C                   MOVEL     CVSPGM        SCSPGM
     C*
     C                   MOVEL     CVDCID        SCDCID
     C                   MOVEL     CVFLR         SCFLR
     C                   MOVEL     CVSFLR        SCSFLR
     C*
     C                   MOVE      CVSTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETING
     C                   MOVEL     CD(1)         SCCMDD                         |
      /Free
       av_keys = %trim(standardKeys) + '   ' + cd(1);
      /End-free
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                         |
      /Free
       av_keys = %trim(standardKeys);
      /End-free
     C                   END                                                    |
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCCVID
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVEL     *BLANKS       SCPRTF
     C                   MOVEL     *BLANKS       SCSPGM
     C*
     C                   MOVEL     *BLANKS       SCDCID
     C                   MOVEL     *BLANKS       SCFLR
     C                   MOVEL     *BLANKS       SCSFLR
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
      /Free
       av_keys = %trim(standardKeys);
      /End-free
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*
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
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVEL     'A'           QQCALL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F5 = DISPLAY COVER PAGE ***            *
     C********************************************
     C     *INKE         IFEQ      '1'
     C*** IMAGE DISPLAY
     C                   MOVEL     *BLANKS       Q5RTN
     C                   MOVEL     'GENERIC '    X5DTYP            8
     C                   MOVEL     *ALL'@'       X5DESC           40
     C*
     C                   CALL      'IS211C'
     C                   PARM                    SCDCID           12
     C                   PARM                    SCFLR             8
     C                   PARM                    SCSFLR           12
     C                   PARM      X5DTYP        X5DOCT            8
     C                   PARM      X5DESC        X5DESC           40
     C                   PARM      *BLANKS       Q5RTN             6
      *
      *
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKC
     C********************************************
     C*   F10 = SEARCH                           *
     C********************************************
     C     *INKJ         IFEQ      '1'
     C*
     C                   GOTO      STRT
     C***
     C                   END                                                    END *INKL
     C********************************************
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'B'           QQCALL
     C                   GOTO      TERM
     C                   END                                                    END *INKL
     C********************************************
     C*   *IN10 = PROCESS HELP REQUESTS          *
     C********************************************
     C     *IN10         IFEQ      '1'
     C*
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
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C********************************************
     C*   F20 = CHANGE STATUS                    *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64                           ALLOW DELETE
     C                   MOVEL     CD(1)         SCCMDD                          |
      /Free
       av_keys = %trim(standardKeys) + '   ' + cd(1);
      /End-free
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C                   MOVEL     *BLANKS       SCCMDD                          |
      /Free
       av_keys = %trim(standardKeys);
      /End-free
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C********************************************
     C*   F23 = DELETE ROUTING RCD        *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     CVKY1         DELETE    PCV01                              60
     C                   MOVE      *BLANK        XXCVID
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     ' '           QQCALL
     C                   GOTO      TERM
     C                   END                                                    END *INKX
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*
     C*** ONLY FOR ADD DO THIS EDIT ***************************
     C     SCCVID        CHAIN     PCV01                              60
     C     *IN60         CABEQ     *OFF          STRT                     21
     C*
     C                   ENDIF
     C*
     C     SCCVID        CABEQ     *BLANKS       STRT                     20
     C*
     C*** DESC NOT EQUAL BLANKS    ***************************
     C     SCDESC        CABEQ     *BLANKS       STRT                     22
     C*
     C*** PRINT FILE NOT = BLANKS ************************
     C     SCPRTF        CABEQ     *BLANKS       STRT                     24
     C*
     C**** VALIDATE THE PRINTER FILE OBJECT EXISTS ***
     C*
     C                   CALL      'SVALOBJ'
     C                   PARM      SCPRTF        ##OBJN           10
     C                   PARM      '*FILE  '     ##OBTP            7
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     ##RTCD        CABEQ     '1'           STRT                     24
     C****
     C*
     C*
     C*** SCREEN PROGRAM ¬= BLANKS ************************
     C*
     C     SCSPGM        CABEQ     *BLANKS       STRT                     25
     C*
     C**** VALIDATE THE SCREEN PGM OBJECT EXISTS ***
     C*
     C*
     C                   CALL      'SVALOBJ'
     C                   PARM      SCSPGM        ##OBJN           10
     C                   PARM      '*PGM   '     ##OBTP            7
     C                   PARM      *BLANK        ##RTCD            1
     C     ##RTCD        CABEQ     '1'           STRT                     25
     C*
      * * *  EDIT THE DOCID/FLR/SFLR FIELDS HERE !!!!
      *
     C     SCDCID        CABEQ     *BLANKS       STRT                     26
     C     SCFLR         CABEQ     *BLANKS       STRT                     26
     C     SCSFLR        CABEQ     *BLANKS       STRT                     26
     C*
     C     SCFLR         CAT(P)    '/':0         #@FOLD           60
     C                   CAT(P)    SCSFLR:0      #@FOLD
     C*
      *
     C*
     C                   CALL      'SVALDLO'
     C                   PARM      SCDCID        #@DLO            12
     C                   PARM                    #@FOLD
     C                   PARM      *BLANK        ##RTCD            1
     C     ##RTCD        CABEQ     '1'           STRT                     26
     C*
      *
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     CVKY          CHAIN     PCV01                              40
     C*
     C                   MOVEL     SCCVID        CVCVID
     C                   MOVEL     SCDESC        CVDESC
     C                   MOVE      SCSTAT        CVSTAT
     C                   MOVEL     SCPRTF        CVPRTF
     C                   MOVEL     SCSPGM        CVSPGM
     C                   MOVEL     SCDCID        CVDCID
     C                   MOVEL     SCFLR         CVFLR
     C                   MOVEL     SCSFLR        CVSFLR
     C                   MOVEL     Q1USER        CVLUSR
     C                   MOVEL     Q1PGM         CVLPGM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCV01
     C                   ELSE
     C*
     C                   MOVEL     SCCVID        CVCVID
     C                   MOVEL     SCDESC        CVDESC
     C                   MOVE      SCSTAT        CVSTAT
     C                   MOVEL     Q1USER        CVIUSR
     C                   MOVEL     SCPRTF        CVPRTF
     C                   MOVEL     SCSPGM        CVSPGM
     C                   MOVEL     SCDCID        CVDCID
     C                   MOVEL     SCFLR         CVFLR
     C                   MOVEL     SCSFLR        CVSFLR
     C                   MOVEL     Q1PGM         CVIPGM
     C*
     C                   WRITE     PCV01
     C                   END
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       XXCVID
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPCV01     E            PCV01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
