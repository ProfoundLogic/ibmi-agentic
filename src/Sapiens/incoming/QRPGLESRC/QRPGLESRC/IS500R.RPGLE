     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS500R - Document Type Maintenance
     H*****************************************************************
     H*  PROGRAM DESCRIPTION::                                        *
     H*    This program allows the user to add, delete, change, or    *
     H*    view Document Type Records.                                *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    Menu                                                       *
     H*  CALLS TO:                                                    *
     H*    IS100R - Document Type Search                              *
     H*                                                               *
     H*  PROGRAMMER:                                                  *
     H*  CREATED:    8/8/97               PROJECT:                    *
     H*****************************************************************
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 11/02/10  C:183 Project: SY195                               *
     H*                 Remove hard coding for QDLS. Call RTVDOCROOT *
     H*                 to get the root folder name.                 *
     H*                                                              *
     H* 01/06/09 C:062 PROJECT: 114IRZ                               *
     H*          ADD RESTRICT AUTO INDEX FLAG.                       *
     H*                                                              *
     H* 04/08/04 C:128 PROJECT: IM099 Support: 42073                 *
     H*          Do not allow user to enter a single quote in the    *
     H*          description field.                                  *
     H*                                                               *
     H* 10/20/03 C:128 Support
     H*                Call IS100R with a 'M' in QQRTC so that both
     H*                active and inactive records will display.
     H*                Included validation that remark code status    *
     H*                must not be inactive ('I').                    *
     H*                                                               *
     H* 07/20/02 C:002 PR
     H*                ADDED FIELDS TO UPDATE THE DT10AX FLDS
     H*
     H*
     H* 01/16/00 C:128 PROJECT: DR081                                 *
     H*                Default index queue not being written to       *
     H*                IMDTP.                                         *
     H*                                                               *
     H* 07/27/00 C:113 PROJECT: DR113                                 *
     H*                Include new fields for import server.          *
     H*                                                               *
     H*                                                               *
     H* 01/27/00 C:113 PROJECT: DR095                                 *
     H*                Changed to use the Work With Key Types program *
     H*                (SC092R) for key type searches.                *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  09    INQUIRY ONLY                                           **
     H* SCR01:                                                        **
     H*  20    INVALID Document Type                                  **
     H* SCR02:                                                        **
     H*  20    DESCRIPTION REQUIRED                                   **
     H*  21    DEFAULT INDEX QUEUE                                    **
     H*  22    DARTS REMARK CODE                                      **
     H*  25    QUALITY CONTROL FLAG VALID?                            **
     H*  26    INDEX VALIDATION FLAG VALID?                           **
     H*  64    DELETE NOT ALLOWED                                     **
     H*  40    CHAIN & READ Document Type FILE                        **
     H******************************************************************
     H*  FILE USAGE:                                                  *
     H*    IMDTL   (PDT01): DOCT                                      *
     H*    IMIQL   (PIQ01): DQUE                                      *
     H*    STCTL1  (PCT01): RMCD                                      *
     H*
     H*****************************************************************
     FIS500F    CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     FIMDTL     UF A E           K DISK
     F*** BEGIN ADD *** 07/27/00 ***********************************************
     FIMDTL4    IF   E           K DISK
     F                                     RENAME(PDT01:PDT02)
     F*** END   ADD *** 07/27/00 ***********************************************
     FIMIQL     O    E             DISK
     FSTCTL1    IF   E           K DISK
     FITBSL     IF   E           K DISK
     FIMWSL2    IF   E           K DISK
     F*** BEGIN ADD *** 01/27/00 ***********************************************
     F*
     F** Key Type File
     FSMKTP     IF   E             DISK
     FSMKTL3    IF   E           K DISK
     F                                     RENAME(PKT01:PKT03)
     F*** END   ADD *** 01/27/00 ***********************************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D CD              S             20    DIM(1) CTDATA PERRCD(1)
      *** 04/08/04 Begin Add ******************************************
     D DA              S              1    DIM(40)
      *** 04/08/04 End   Add ******************************************
      *** Beg add ***** 11/02/10 ***********************************************
     D RPGM            C                   CONST('RTVDOCROOT')
      *** End add ***** 11/02/10 ***********************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   PARM LIST FOR INPUT DATA.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPDSC           40
     C                   PARM                    QQCALL            1
     C*
     C*   PARM LIST INDEX PROGRAM Search
     C*
     C     PL0213        PLIST
     C                   PARM                    SCIDXP
     C                   PARM                    QQRTC             1
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C     ACDPL1        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      '2'           ##VIEW            1
     C*** END   ADD *** 01/27/00 ***********************************************
     C*
     C*
     C*   PARM LIST Document Type Search
     C*
     C     PL0120        PLIST
     C                   PARM                    SCDQUE
     C                   PARM                    XXDESC           40
     C                   PARM                    QQRTC             1
     C*
     C*
     C     PL0100        PLIST
     C                   PARM                    SCDOCT
     C                   PARM                    SCDESC
     C                   PARM                    QQRTC             1
     C*
     C*   KEY LIST FOR FAX RECIPIENT FILE
     C     DTKY          KLIST
     C                   KFLD                    SCDOCT
     C*
     C*
     C*   KEY LIST FOR BATCH STATUS  FILE
     C     BSKY1         KLIST
     C                   KFLD                    SCBSTS
     C*
     C*** WORKSTATION FILE
     C*
     C     WSKY1         KLIST
     C                   KFLD                    Q1JOB
     C*
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C     KTKY3         KLIST
     C                   KFLD                    XXINPG           10
     C*** END   ADD *** 01/27/00 ***********************************************
      /Free
       scPrDs = 'Document Type Maintenance';
      /End-Free
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C*
     C* RETRIEVE CLIENT ID
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
      *** Beg add ***** 11/02/10 ***********************************************
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      *** End add ***** 11/02/10 ***********************************************
     C*
     C                   SETOFF                                       16
     C     ##CLT#        IFEQ      '114'
     C                   SETON                                        16
     C                   ENDIF
     C*** END   ADD *** 01/27/00 ***********************************************
     C*
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C                   MOVE      Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C**********************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C********************************************
     C*   F3 = EXIT                              *
     C********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C********************************************
     C*   F12 = PREVIOUS SCREEN (CANCEL)         *
     C********************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    END SCR02
     C                   GOTO      STRT
     C                   END                                                    END *INKL
     C********************************************
     C*   F5 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKE         IFEQ      '1'
     C********************************************
     C* SCR01 = Document Type SEARCh             *
     C********************************************
     C     Q2RECF        IFEQ      'SCR01'
      *** Begin Change 10/20/03 ***************************************
     C                   MOVE      'M'           QQRTC
     C                   CALL      'IS100R'      PL0100
      *** End   Change 10/20/03 ***************************************
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCDOCT        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END                                                    END SCR01
     C                   END                                                    END *INKE
     C********************************************
     C*   F6 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKF         IFEQ      '1'
     C********************************************
     C* SCR02 = DEFAULT INDEX QUEUE              *
     C********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C******               CALL 'IS120R'  PL0120
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCDQUE        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           STRT
     C                   END                                                    END SCR01
     C                   END                                                    END *INKE
     C********************************************
     C*   F7 = SEARCH DARTS REMARKS              *
     C********************************************
     C     *INKG         IFEQ      '1'
     C********************************************
     C* SCR01 = DARTS REMARK SEARCH              *
     C********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C                   CALL      'SC580R'
     C                   PARM                    SCRMCD
     C                   PARM                    QQRTC
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCRMCD        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C     QQRTC         CABEQ     '*'           STRT
     C                   END                                                    END SCR0S
     C                   END                                                    END *INKE
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
     C*   F8 = SEARCH LIST CALL                  *
     C********************************************
     C     *INKH         IFEQ      '1'
     C********************************************
     C* SCR02 = INDEX PROGRAM                    *
     C********************************************
     C     Q2RECF        IFEQ      'SCR02'
     C*** BEGIN DEL *** 01/27/00 ***********************************************
     C**********           CALL 'IS213R'  PL0213
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCIDXP    OREQ *BLANK
     C**********           WRITESCR99
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           GOTO STRT
     C**********           ENDIF
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR02'   Q2RECF
     C********** QQRTC     CABEQ'*'       STRT
     C*** END   DEL *** 01/27/00 ***********************************************
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C                   CALL      'SC092R'      ACDPL1
     C*
     C     *INKC         CABEQ     '1'           TERM
     C*
     C     *INKL         IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     *BLANKS       SCIDXP
     C                   MOVEL(P)  *BLANKS       SCIDES
     C                   WRITE     SCR99
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PKT01                              60
     C                   MOVEL     KTINPG        SCIDXP
     C                   MOVEL(P)  KTPDES        SCIDES
     C                   ENDIF
     C*
     C*** END   ADD *** 01/27/00 ***********************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END SCR01
     C                   END                                                    END *INKE
     C*** ADD 7/20/02 *** MDG *****************************
     C********************************************
     C*   F13 = IMAGE EXIT PROGRAMS BY DOCTYPE (HIDDEN FKEY) *****
     C********************************************
     C     *INKM         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C*
     C                   EXFMT     SCR07
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02   '    Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  END *INKM
     C*** END ADD 7/20/02 *** MDG *************************
     C********************************************
     C*   F20 = CHANGE STATUS                    *
     C********************************************
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   MOVE      '1'           *IN64                           ALLOW DELETE
     C*****              MOVEL     CD(1)         SCCMDD                          |
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
     C                   END
     C                   GOTO      STRT
     C                   END                                                     END *INKU
     C********************************************
     C*   F23 = DELETE RECIPIENT                 *
     C********************************************
     C     *INKX         IFEQ      '1'
     C     *IN64         ANDEQ     '1'
     C     Q2RECF        ANDEQ     'SCR02'
     C*
     C*** 1ST DETERMINE IF THERE ARE DOCS WITH THIS TYPE ACTIVE...
     C*** IF SO **** ERROR MESSAGE **** CAN'T DELETE
     C*** ELSE GO AHEAD AND DELETE ***
     C*
     C     DTKY          DELETE    PDT01                              60
     C                   MOVE      *BLANK        SCDOCT
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKX
      ********************************************************************
      ***  F??????         BATCH STATUS SEARCH ***************
      ********************************************************************
     C*
     C                   GOTO      STRT
     C*
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  Document Type CANNOT BE EQUAL TO BLANKS
     C*
     C     SCDOCT        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     SCDOCT        CHAIN     PDT01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PDT01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      DTDESC        SCDESC                         DESCRIPTION
     C                   MOVE      DTDOCT        SCDOCT
     C                   MOVE      DTDLIM        SCDLIM
     C                   MOVE      DTDQUE        SCDQUE
     C                   MOVE      DT#DIB        SC#DIB
     C                   MOVE      DTRMCD        SCRMCD
     C                   MOVE      DTSECV        SCSECV
     C                   MOVE      DTIDXP        SCIDXP                         INDEX PGM
     C*** ADD 07/20/02 ****************************************
     C                   MOVEL     DT10A0        SC10A0
     C                   MOVEL     DT10A1        SC10A1
     C                   MOVEL     DT10A2        SC10A2
     C                   MOVEL     DT10A3        SC10A3
     C                   MOVEL     DT10A4        SC10A4
     C                   MOVEL     DT10A5        SC10A5
     C                   MOVEL     DT10A6        SC10A6
     C                   MOVEL     DT10A7        SC10A7
     C                   MOVEL     DT10A8        SC10A8
     C                   MOVEL     DT10A9        SC10A9
     C*** END ADD 07/20/02 ************************************
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C                   MOVEL(P)  DTIDXP        XXINPG
     C     KTKY3         CHAIN     PKT03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  KTPDES        SCIDES
     C                   ELSE
     C                   MOVE      *BLANKS       SCIDES
     C                   ENDIF
     C*** END   ADD *** 01/27/00 ***********************************************
     C                   MOVE      DTBSTS        SCBSTS                         QUAL CTL FLG
     C                   MOVE      DTIVFL        SCIVFL                         IDX VERIFY FLG
     C*
     C                   MOVE      DTSTAT        SCSTAT
     C     SCSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN64                          ALLOW DELETING
     C*****              MOVEL     CD(1)         SCCMDD                         |
     C                   ELSE                                                   |
     C                   MOVE      '0'           *IN64                          NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                         |
     C                   END                                                    |
     C*** BEGIN ADD *** 07/27/00 ***********************************************
     C                   MOVE      DTIMPF        SCINCL
     C                   MOVEL     DTDID         SCIMTF
     C                   MOVEL     DTIMPG        SCIMPG
     C*** END   ADD *** 07/27/00 ***********************************************
     C*** BEGIN ADD *** 01/06/09 ***********************************************
     C                   MOVE      DTFL01        SCFL01                         AUTO IDX RESTR
     C*** END   ADD *** 01/06/09 ***********************************************
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
     C                   MOVE      *ZEROS        SCDLIM
     C                   MOVE      *BLANKS       SCDQUE
     C                   MOVE      *ZEROS        SC#DIB
     C                   MOVE      *BLANKS       SCRMCD
     C                   MOVE      *BLANKS       SCSECV
     C                   MOVE      *BLANKS       SCIDXP                         Other fields
     C                   MOVE      *BLANKS       SCBSTS
     C                   MOVE      'N'           SCIVFL                         IDX VERIFY FLG
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      '0'           *IN64                           NO DELETING
     C*****              MOVEL     *BLANKS       SCCMDD                          |
     C                   MOVE      'A'           SCSTAT
     C*** ADD 07/20/02 ****************************************
     C                   MOVEL     *BLANKS       SC10A0
     C                   MOVEL     *BLANKS       SC10A1
     C                   MOVEL     *BLANKS       SC10A2
     C                   MOVEL     *BLANKS       SC10A3
     C                   MOVEL     *BLANKS       SC10A4
     C                   MOVEL     *BLANKS       SC10A5
     C                   MOVEL     *BLANKS       SC10A6
     C                   MOVEL     *BLANKS       SC10A7
     C                   MOVEL     *BLANKS       SC10A8
     C                   MOVEL     *BLANKS       SC10A9
     C*** END ADD 07/20/02 ************************************
     C*** BEGIN ADD *** 01/06/09 ***********************************************
     C                   MOVE      *BLANKS       SCFL01                         AUTO IDX RESTR
     C*** END   ADD *** 01/06/09 ***********************************************
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*****************
     C     SCR02T        TAG
     C*
     C*  DESCRIPTION CANNOT BE EQUAL TO BLANKS
     C*
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
      *** 04/08/04 Begin Add ******************************************
     C                   MOVEA     SCDESC        DA
     C                   MOVE      X'7D'         SINQTE            1
     C     SINQTE        LOOKUP    DA                                     98
     C     *IN98         IFEQ      *ON
     C                   SETON                                        88
     C                   GOTO      STRT
     C                   ENDIF
      *** 04/08/04 End   Add ******************************************
     C*
     C*  DEFAULT INDEX QUEUE MUST BE VALID
     C*
     C***        SCDQUE    IFEQ *BLANKS
     C***                  MOVE '1'       *IN21
     C***                  GOTO STRT
     C***                  END
     C*
     C***        SCDQUE    CHAINPIQ01                41
     C*****      *IN41     CABEQ*ON       STRT           21
     C*
     C*  DARTS REMARK CODE MUST BE VALID
     C     SCRMCD        CABEQ     *BLANKS       STRT                     22
     C*
     C     SCRMCD        CHAIN     PCT01                              41
      *** Begin Change 10/20/03 ***************************************
     C     *IN41         IFEQ      *OFF
     C     CTSTAT        IFEQ      'I'                                          Inactive Rcd
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *** End   Change 10/20/03 ***************************************
     C     *IN41         CABEQ     *ON           STRT                     22
     C*
     C*
     C     BSKY1         CHAIN     PBS01                              41
     C     *IN41         CABEQ     *ON           STRT                     25
     C*
     C*
     C     SCIVFL        IFNE      'N'
     C     SCIVFL        ANDNE     'Y'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
      *** BEGIN ADD *** 07/27/00 ***********************************************
     C*** BEGIN ADD *** 01/27/00 ***********************************************
     C*
     C* THE RESTRICT AUT IDX FLAG MUST BE Y OR BLANK
     C*
     C     *IN16         IFEQ      *ON
     C     SCFL01        ANDNE     'Y'
     C     SCFL01        ANDNE     ' '
     C                   SETON                                        28
     C                   GOTO      STRT
     C                   ENDIF
     C*** END   ADD *** 01/27/00 ***********************************************
     C*
     C* INCLUDE IN IMPORT MUST BE 'Y' OR BLANK
     C*
     C     SCINCL        IFNE      'Y'
     C     SCINCL        ANDNE     *BLANK
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* IF INCLUDE IN IMPORT IS 'Y', OTHER FIELDS CAN'T BE BLANK
     C*
     C     SCINCL        IFEQ      'Y'
     C     SCIMPG        ANDEQ     *BLANKS
     C     SCIMTF        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C* INCLUDE IN IMPORT MUST BY 'Y' TO ENTER THIS FIELD
     C*
     C     SCINCL        IFNE      'Y'
     C     SCIMTF        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCINCL        IFNE      'Y'
     C     SCIMPG        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN35
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* IMPORT PROGRAM AND LIBRARY MUST BE VALID
     C*
     C     SCINCL        IFEQ      'Y'
     C     SCIMPG        ANDNE     *BLANKS
     C                   CALL      'DA500C'
     C                   PARM                    SCIMPG
     C                   PARM      *BLANKS       PIMPL            10
     C                   PARM      '*PGM   '     XOBJTP            7
     C                   PARM                    XERR              1
     C*
     C     XERR          IFNE      '0'
     C                   MOVE      *ON           *IN37
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IMPORT Library must not be used elsewhere
     C*
     C     SCINCL        IFEQ      'Y'
     C     SCIMTF        ANDNE     *BLANKS
     C                   MOVEL     SCIMTF        KDID             20
     C     KDID          CHAIN     PDT02                              30
     C     *IN30         IFEQ      *OFF
     C*** ADD 070401 *****
     C     SCDOCT        IFEQ      DTDOCT
     C     KDID          READE     PDT02                                  30
     C                   ENDIF
     C     *IN30         IFEQ      *OFF
     C*** END ADD 070401 ***
     C                   MOVE      *ON           *IN38
     C                   GOTO      STRT
     C                   ENDIF
     C*** ADD 070401 *****
     C                   ENDIF
     C*** END ADD 070401 ***
     C*
     C                   ENDIF
      *** END   ADD *** 07/27/00 ***********************************************
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C     *IN09         IFEQ      '0'
     C     DTKY          CHAIN     PDT01                              40
     C                   MOVEL     SCDESC        DTDESC
     C                   MOVE      SCDOCT        DTDOCT
     C                   MOVE      SCDLIM        DTDLIM
     C                   MOVE      SC#DIB        DT#DIB
     C                   MOVE      SCRMCD        DTRMCD
     C                   MOVE      SCSECV        DTSECV
     C                   MOVE      SCIDXP        DTIDXP                         Other fields
     C                   MOVE      SCBSTS        DTBSTS                         QUAL CTL FLG
     C                   MOVE      SCIVFL        DTIVFL                         IDX VERIFY FLG
     C                   MOVE      SCSTAT        DTSTAT
     C                   MOVEL     Q1USER        DTLUSR
     C                   MOVEL     Q1PGM         DTLPGM
     C*** BEGIN ADD *** 07/27/00 ***********************************************
     C                   MOVE      SCINCL        DTIMPF
     C                   MOVEL     SCIMPG        DTIMPG
     C                   MOVEL     SCIMTF        DTDID
      *** Beg chg ***** 11/02/10 ***********************************************
     C*******              MOVEL'QDLS'    DTFSID
     C                   MOVEL     ROOT          DTFSID
      *** End chg ***** 11/02/10 ***********************************************
     C*** END   ADD *** 07/27/00 ***********************************************
     C*** BEGIN ADD *** 01/06/09 ***********************************************
     C                   MOVE      SCFL01        DTFL01                         AUTO IDX RESTR
     C*** END   ADD *** 01/06/09 ***********************************************
     C     SCDQUE        IFEQ      *BLANK
     C                   MOVEL     SCDOCT        DTDQUE
     C                   ENDIF
     C*** BEGIN ADD *** 01/16/01 ***********************************************
     C*** END   ADD *** 01/16/01 ***********************************************
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      DTLDT
     C                   Z-ADD     DSTIME        DTLTIM
     C*** ADD 07/20/02 ****************************************
     C                   MOVEL     SC10A0        DT10A0
     C                   MOVEL     SC10A1        DT10A1
     C                   MOVEL     SC10A2        DT10A2
     C                   MOVEL     SC10A3        DT10A3
     C                   MOVEL     SC10A4        DT10A4
     C                   MOVEL     SC10A5        DT10A5
     C                   MOVEL     SC10A6        DT10A6
     C                   MOVEL     SC10A7        DT10A7
     C                   MOVEL     SC10A8        DT10A8
     C                   MOVEL     SC10A9        DT10A9
     C*** END ADD 07/20/02 ************************************
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDT01
     C                   ELSE
     C                   MOVEL     Q1USER        DTIUSR
     C                   MOVEL     Q1PGM         DTIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      DTIDT
     C                   Z-ADD     DSTIME        DTITIM
     C*
     C                   WRITE     PDT01
     C*
     C*** IMAGE QUEUE ENTRY ***
     C                   MOVEL     SCDESC        IQDESC
     C                   MOVEL     SCDOCT        IQID
     C                   MOVE      SCSTAT        IQSTAT
     C*
     C                   MOVEL     Q1USER        IQIUSR
     C                   MOVEL     Q1PGM         IQIPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      IQIDT
     C                   Z-ADD     DSTIME        IQITIM
     C*
     C                   WRITE     PIQ01
     C*
     C*
     C                   END                                                        END 40 = 0
     C*
     C*
     C*** VERIFY IF THE WRKSTN IS SCAN CAPABLE, IF SO, ALLOW SCAN OF TIF
     C*** IF NOT, CONTINUE...
     C*
     C     WSKY1         CHAIN     PWS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     WSSCAP        IFEQ      'Y'
     C     DSP5          TAG
     C                   MOVEL     'SCR05'       Q2RECF
     C                   MOVEL     'SCR05'       SCRECF
     C                   EXFMT     SCR05
     C     *INKH         IFEQ      *ON
     C***   CALL THE SCAN PGM
     C                   CALL      'IS501C'
     C                   PARM      DTDOCT        QQDOCT            8
     C                   PARM      *BLANKS       SCERRM           60
     C                   PARM      *BLANKS       QSRTCD            6
     C*
     C*
     C     QSRTCD        IFNE      '     0'                                     VALID SCAN
     C     QSRTCD        ANDNE     '000000'                                     VALID SCAN
     C     QSRTCD        ANDNE     '      '                                     VALID SCAN
     C*
     C                   MOVE      *ON           *IN20
     C                   GOTO      DSP5
      *
      *
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       SCERRM
     C*
     C*** CALL PROGRAM TO CREATE DEFAULT INI/ZON/TIF FILES
     C                   CALL      'IS500C'
     C                   PARM      DTDOCT        QQDOCT            8
     C*
     C*
     C     ENDIT         TAG
     C                   MOVEL     'SCR04'       Q2RECF
     C                   MOVEL     'SCR04'       SCRECF
     C                   EXFMT     SCR04
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SCDOCT
     C                   GOTO      STRT
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPDT01     E            PDT01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE CD
F23=Delete
