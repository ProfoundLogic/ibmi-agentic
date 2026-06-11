     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *****************************************************************
      *TITLE SC822R - OPTICAL PURGE/DELETE SEARCH PROGRAM             *
      *****************************************************************
      *    PROGRAM DESCRIPTION:                                       *
      *        BACKUP AND BATCH - Optical Purge/Delete Search         *
      *                           Prompt Program                      *
      *                                                               *
      *  CALLED FROM: Menu                                            *
      *                                                               *
      *  CALLS TO: W0060R                                             *
      *                                                               *
      *  CREATED: 03/31/03    PROGROGRAMMER: C:145    PROJECT: 124IAA *
      *****************************************************************
      ** REVISIONS:                                                   *
      *  CREATED:             PROGRAMMER:             PROJECT:        *
      *                                                               *
      *  03/31/02             C:145                   124IAA          *
      *                Original program for optical purge/delete      *
      *                search prompt program was SC852R.  Program was *
      *                renamed and modified to reflect changes for the*
      *                new batch/backup process.                      *
      ******************************************************************
      * INDICATOR USAGE                                               **
      *                                                               **
      *  01    SFLDSPCTL                                              *
      *  02    SFLINZ                                                 *
      *  03    SFLDLT                                                 *
      *  04    SFLCLR                                                 *
      *  05    SFLEND                                                 *
      *  06    If no records to select, don't display column          *
      *        separator on SEL flag fields                           *
      *  18    ROLLUP INDICATOR                                       *
      *  20    WCR0898-INVALID VALUE KEYED                            *
      *  21    SFLMSG -STATUS=C, INVALID OPTION SELECTED              *
      *  30    PURGE OR DELETE INDICATOR                              *
      *  32    ROLL UP INDICATOR - SUBFILE EOF                        *
      *  60    GENERAL CHAIN                                          *
      *  N71   SCR02 - View 1 (A)                                     *
      *  71    SCR02 - View 2 (B)                                     *
      * NOTE:                                                         *
      *        SFLCSRRRN - subfile cursor relative record # returns   *
      *        to the pgm the RRN of the cursor. Using 'CURLOC' field *
      *        to fill #@$2 corrects the page up problem SFL searches *
      *        have.                                                  *
      ******************************************************************
      *
     FSC822F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2)
     F                                     SFILE(SCR01SB:RRN2)
      *
     FSMPWL2    UF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMKDL     IF   E           K DISK
     FSDCMLA    IF   E           K DISK
      *

     D KY              S              1    DIM(40)                              KEY DATA
     D H1              S              1    DIM(8)                               HEAD TEMP
      *
     D                 DS
     D  MP                     1     79
     D                                     DIM(79)                              KEY DEF TO DSP ARRY
     D  KEYOUT                 1     79
     D                 DS
     D  HD                     1     64
     D                                     DIM(8)                               HEAD DESCS
     D  KDHD01                 1      8
     D  KDHD02                 9     16
     D  KDHD03                17     24
     D  KDHD04                25     32
     D  KDHD05                33     40
     D  KDHD06                41     48
     D  KDHD07                49     56
     D  KDHD08                57     64
      *
     D                 DS
     D  HL                     1     40  0
     D                                     DIM(8)                               HEAD LENGTHS
     D  KDHL01                 1      5  0
     D  KDHL02                 6     10  0
     D  KDHL03                11     15  0
     D  KDHL04                16     20  0
     D  KDHL05                21     25  0
     D  KDHL06                26     30  0
     D  KDHL07                31     35  0
     D  KDHL08                36     40  0
      *
     D                 DS
     D  DS                     1     40  0
     D                                     DIM(8)                               DATA FLD START POS
     D  KDDS01                 1      5  0
     D  KDDS02                 6     10  0
     D  KDDS03                11     15  0
     D  KDDS04                16     20  0
     D  KDDS05                21     25  0
     D  KDDS06                26     30  0
     D  KDDS07                31     35  0
     D  KDDS08                36     40  0
      *
     D                 DS
     D  DL                     1     40  0
     D                                     DIM(8)                               DATA FLD LENGTH
     D  KDDL01                 1      5  0
     D  KDDL02                 6     10  0
     D  KDDL03                11     15  0
     D  KDDL04                16     20  0
     D  KDDL05                21     25  0
     D  KDDL06                26     30  0
     D  KDDL07                31     35  0
     D  KDDL08                36     40  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  LIKE
      *

     C     *LIKE         DEFINE    RRN2          SRRN2
     C     *LIKE         DEFINE    RRN2          XXRRN2
     C     *LIKE         DEFINE    SCIKEY        SVIKEY
     C     *LIKE         DEFINE    SCISEQ        SVISEQ
     C     *LIKE         DEFINE    PWKEY         XXKEY
      *
      *  Key Lists
      *
      *   Key list for the remarks detail record
      *
     C     CMKY          KLIST
     C                   KFLD                    PWKEY
     C                   KFLD                    PWSEQ#
      *
     C     PWKY          KLIST
     C                   KFLD                    SCIKEY
     C                   KFLD                    SCISEQ
      *
     C     PWKY1         KLIST
     C                   KFLD                    XXKEY
      *
     C     PWKY2         KLIST
     C                   KFLD                    SCKEY
     C                   KFLD                    SCSEQ#
      *
      * User parameter file
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  Parm Lists
      *
      *    Parameter list for called program
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPDFL            1            PURGE/DELETE FLAG
     C                   PARM                    QQRTCD            1
      *
      *  Initializaition
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      /Free
       scPrDs = 'Optical Purge/Delete Search';
      /End-free
      *
      *  Reset point in program
      *
     C     TOP           TAG
      *
      *  Get the user parameter file
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANKS       UPOUTQ
     C                   END
      *
     C     STRT01        TAG
      *
      *  Initialize routine
      *
     C                   EXSR      INIT
      *
     C                   MOVE      *LOVAL        XXKEY
     C     PWKY1         SETLL     PPW01
      *
      *  Clear subfile
      *
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   WRITE     SCR01CB
     C                   MOVE      '0'           *IN04
      *
     C                   Z-ADD     0             RRN2
      *
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
      *
     C     STRT          TAG
      *
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
      *
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01A
     C                   EXFMT     SCR01CA
     C                   MOVE      '0'           *IN01
     C                   END
      *
     C     Q2RECF        IFEQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01B
     C                   EXFMT     SCR01CB
     C                   MOVE      '0'           *IN01
     C                   END
      *
      *******************************************************************
      *   PROCESS ALL COMMAND KEYS
      *******************************************************************
      *
     C     *IN11         IFEQ      '1'
      ****************************************
      *   F3 = EXIT                          *
      ****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
      ****************************************
      *   F5 = REFRESH                       *
      ****************************************
     C     *INKE         IFEQ      '1'
      *
     C                   EXSR      SRREFS
      *
     C                   ENDIF                                                  END *INKE
      ****************************************
      *   F11 = CHANGE VIEW                  *
      ****************************************
     C     *INKK         IFEQ      '1'
      *
     C     *IN71         IFEQ      '0'
     C                   MOVE      '1'           *IN71
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01B'      Q2RECF
     C                   ELSE                                                   ELSE EQ 'A'
     C                   MOVE      '0'           *IN71
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01A'      Q2RECF
     C                   END                                                    END EQ 'A'
     C                   GOTO      STRT
      *
     C                   END                                                    END *INKK
      ****************************************
      *   F12= PREVIOUS                      *
      ****************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C     Q2RECF        OREQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C                   GOTO      TERM
     C                   END
     C                   END                                                    END *INKL
      ****************************************
      *   F13 = CHANGE USER PARAMETERS       *
      ****************************************
     C     *INKM         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C     Q2RECF        OREQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
      *
     C     *IN71         IFEQ      '0'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01A'      Q2RECF
     C                   ELSE
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01B'      Q2RECF
     C                   GOTO      TOP
     C                   END
      *
     C                   ENDIF
      *
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01SA'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01SB'
     C     Q2RECF        OREQ      'SCR01CB'
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
      *
      *  Get the user parameter file
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   MOVE      *ZERO         UPFYR
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C                   END                                                    END *INKM
      *********************************
      *   F20 - SELECT ALL ENTRIES    *
      *********************************
     C     *INKU         IFEQ      '1'
     C                   MOVE      *LOVAL        XXKEY
     C     PWKY1         SETLL     PPW01
     C                   READ      PPW01                                  60
     C     *IN60         DOWEQ     '0'
     C     QQPDFL        IFEQ      'P'                                          PURGE MODE
     C     PWSTAT        ANDNE     'C'                                          COMPLETE
     C                   MOVE      'P'           PWSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'                                          DELETE MODE
     C     PWDELF        ANDNE     'C'                                          COMPLETE
     C                   MOVE      'D'           PWDELF
     C                   ENDIF
     C                   UPDATE    PPW01
     C                   READ      PPW01                                  60
     C                   ENDDO
      *
      * DO REFRESH FUNCTION
      *
     C                   EXSR      SRREFS
      *
     C                   ENDIF                                                  END *INKU
      *********************************
      *   F21 - DE-SELECT ALL ENTRIES *
      *********************************
     C     *INKV         IFEQ      '1'
     C                   MOVE      *LOVAL        XXKEY
     C     PWKY1         SETLL     PPW01
     C                   READ      PPW01                                  60
     C     *IN60         DOWEQ     '0'
     C     QQPDFL        IFEQ      'P'                                          PURGE MODE
     C     PWSTAT        ANDNE     'C'                                          COMPLETE
     C                   MOVE      ' '           PWSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'                                          PURGE MODE
     C     PWDELF        ANDNE     'C'                                          COMPLETE
     C                   MOVE      ' '           PWDELF
     C                   ENDIF
     C                   UPDATE    PPW01
     C                   READ      PPW01                                  60
     C                   ENDDO
      *
      * DO REFRESH FUNCTION
      *
     C                   EXSR      SRREFS
      *
     C                   ENDIF                                                  END *INKV
      ****************************************
      *   *IN18 -  ROLLUP REQUEST            *
      ****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C     Q2RECF        OREQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END 1 *IN18 ROLLUP
      *
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
      *                                                                *********
      *  GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01A'      SCR01T
     C     Q2RECF        CABEQ     'SCR01CA'     SCR01T
     C     Q2RECF        CABEQ     'SCR01SA'     SCR01T
     C     Q2RECF        CABEQ     'SCR01B'      SCR01T
     C     Q2RECF        CABEQ     'SCR01CB'     SCR01T
     C     Q2RECF        CABEQ     'SCR01SB'     SCR01T
      *
     C     SCR01T        TAG
      *
      *  Edit fields
      *
     C     SCIKEY        IFNE      *BLANKS
     C     SCISEQ        ORNE      0
     C     PWKY          SETLL     PPW01
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   WRITE     SCR01CB
     C                   MOVE      '0'           *IN04
      *
     C                   Z-ADD     0             RRN2
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
      *
      *  Set cursor at top of SFL here
      *
     C                   MOVE      *BLANKS       SCIKEY
     C                   Z-ADD     0             SCISEQ
     C                   GOTO      STRT
     C                   END
     C                   MOVE      '0'           ERR
      *
      *** Save RRN so that rolling can properly function if completion is marked
      *
     C                   MOVE      RRN2          SRRN2
      *
     C  N71              READC     SCR01SA                                50
     C   71              READC     SCR01SB                                50
      *
     C     *IN50         DOWEQ     '0'
     C                   MOVEA     '0000'        *IN(20)
      *
      *  Process for invalid option selected
      *
     C     SCSEL         IFNE      ' '
     C     SCSEL2        ANDNE     ' '
     C     SCSEL         ANDNE     '1'
     C     SCSEL         ANDNE     '2'
     C     SCSEL2        ANDNE     '1'
     C     SCSEL2        ANDNE     '2'
     C                   MOVE      '1'           *IN20
     C                   MOVE      RRN2          XRRN2
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR01SA
     C     RRN2          CHAIN     SCR01SB                            40
     C                   UPDATE    SCR01SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR01SB
     C     RRN2          CHAIN     SCR01SA                            40
     C                   UPDATE    SCR01SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT1
     C                   END
      *
      *    Process for 1=SELECT FOR PURGE ;  status must not equal 'C'.
      *-OR-Process for 1=SELECT FOR DEL  ;  status must not equal 'C'.
      *
     C     SCSEL         IFEQ      '1'
     C     SCSEL2        OREQ      '1'
      *
      *  Error if purge completed don't allow select to purge
      *
     C     QQPDFL        IFEQ      'P'
     C     SCSTAT        IFEQ      'C'
     C     SCSTA2        OREQ      'C'
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
      *
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
      *
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR01SA
     C     RRN2          CHAIN     SCR01SB                            40
     C                   UPDATE    SCR01SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR01SB
     C     RRN2          CHAIN     SCR01SA                            40
     C                   UPDATE    SCR01SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT1
      *
     C                   ENDIF                                                   ENDIF SCSTAT <> 'C'
      *
     C                   ELSE                                                   ELSE 0 *IN71
      *
      *  Error if delete completed, don't allow select to delete
      *
     C     SCDELF        IFEQ      'C'
     C     SCDEL2        OREQ      'C'
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
      *
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR01SA
     C     RRN2          CHAIN     SCR01SB                            40
     C                   UPDATE    SCR01SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR01SB
     C     RRN2          CHAIN     SCR01SA                            40
     C                   UPDATE    SCR01SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT1
      *
     C                   ENDIF                                                   ENDIF SCSTAT <> 'C'
     C                   ENDIF                                                   ENDIF "P"       'C'
      *
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
      *
      *  View One
      *
     C     *IN71         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      'P'           SCSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      'D'           SCDELF
     C                   ENDIF
     C                   UPDATE    SCR01SA
      *
      * Now make sure view two is updated
      *
     C     RRN2          CHAIN     SCR01SB                            40
     C     *IN40         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      'P'           SCSTA2
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      'D'           SCDEL2
     C                   ENDIF
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C                   ELSE
      *
      * Else view two
      *
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      'P'           SCSTA2
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      'D'           SCDEL2
     C                   ENDIF
     C                   UPDATE    SCR01SB
     C                   ENDIF                                                   ENDIF *IN70
      *
      * Now make sure view one is updated
      *
     C     RRN2          CHAIN     SCR01SA                            40
     C     *IN40         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      'P'           SCSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      'D'           SCDELF
     C                   ENDIF
     C                   UPDATE    SCR01SA
     C                   ENDIF                                                  END 0 *IN71
      *
      * Now update file
      *
     C     PWKY2         CHAIN     PPW01                              60
     C     *IN60         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      'P'           PWSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      'D'           PWDELF
     C                   ENDIF
     C                   UPDATE    PPW01
     C                   ENDIF
      *
     C                   ENDIF                                                   ENDIF SCSEL IFEQ '1
      *
      *    Process for 2=SELECT FOR NO PURGE ;  status must not equal 'C'.
      *-OR-Process for 2=SELECT FOR NO DEL   ;  status must not equal 'C'.
      *
     C     SCSEL         IFEQ      '2'
     C     SCSEL2        OREQ      '2'
     C     SCSEL         IFNE      '$'                                            DUMMY CODE HERE
     C     SCSEL         ANDNE     '$'
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
      *
      *  View One
      *
     C     *IN71         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      ' '           SCSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      ' '           SCDELF
     C                   ENDIF
     C                   UPDATE    SCR01SA
      *
      * Now update view two
      *
     C     RRN2          CHAIN     SCR01SB                            40
     C     *IN40         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      ' '           SCSTA2
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      ' '           SCDEL2
     C                   ENDIF
     C                   UPDATE    SCR01SB
     C                   ENDIF
     C                   ELSE                                                   ELSE 0 *IN71
      *
      * Else view two
      *
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      ' '           SCSTA2
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      ' '           SCDEL2
     C                   ENDIF
     C                   UPDATE    SCR01SB
      *
      * Now update view one
      *
     C     RRN2          CHAIN     SCR01SA                            40
     C     *IN40         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      ' '           SCSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      ' '           SCDELF
     C                   ENDIF
     C                   UPDATE    SCR01SA
     C                   ENDIF
     C                   END                                                    END 0 *IN71
      *
      *  Update file
      *
     C     PWKY2         CHAIN     PPW01                              60
     C     *IN60         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      ' '           PWSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      ' '           PWDELF
     C                   ENDIF
     C                   UPDATE    PPW01
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           ERR               1
     C                   MOVE      RRN2          XRRN2
     C     *IN71         IFEQ      '0'
     C                   UPDATE    SCR01SA
     C     RRN2          CHAIN     SCR01SB                            40
     C                   UPDATE    SCR01SB
     C                   ELSE                                                   ELSE 0 *IN71
     C                   UPDATE    SCR01SB
     C     RRN2          CHAIN     SCR01SA                            40
     C                   UPDATE    SCR01SA
     C                   END                                                    END 0 *IN71
     C                   GOTO      NXT1
      *
     C                   ENDIF                                                   ENDIF SCSTAT <> 'C'
     C                   ENDIF                                                   ENDIF SCSEL IFEQ '2
      *
      * Read next subfile record
      *
     C     NXT1          TAG
     C  N71              READC     SCR01SA                                50
     C   71              READC     SCR01SB                                50
     C                   END
      *
      * Restore RRN so that rolling can properly function
      *
     C                   MOVE      SRRN2         RRN2
      *
     C     ERR           IFEQ      '1'
     C                   MOVE      XRRN2         #@$2
     C     *IN71         IFEQ      '0'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVE      'SCR01A'      Q2RECF
     C                   ELSE
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVE      'SCR01B'      Q2RECF
     C                   ENDIF
     C                   GOTO      STRT
     C                   END
      *
      *  Set at last record selected
      *
     C     CURLOC        IFEQ      0
     C                   Z-ADD     XRRN2         #@$2
     C                   ELSE
     C                   Z-ADD     CURLOC        #@$2
     C                   END
      *
     C                   MOVEL     *BLANKS       Q2RECF
     C     *IN71         IFEQ      '0'
     C                   MOVEL     'SCR01A  '    Q2RECF
     C                   ELSE                                                   ELSE 0 *IN71
     C                   MOVEL     'SCR01B  '    Q2RECF
     C                   END                                                    END  0 *IN71
      *
     C                   GOTO      STRT
      *
      *  End of Program
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *====================================================================*
      * INIT SUBROUTIME                                    I N I T         *
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN2              5 0
     C                   Z-ADD     0             XRRN2             5 0
     C                   Z-ADD     0             XXRRN1            5 0
     C                   MOVE      *BLANKS       SCIKEY
     C                   Z-ADD     0             SCISEQ
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      '0'           *IN30
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      '1'           *IN30
     C                   ENDIF
     C                   MOVE      '0'           *IN71
     C                   ENDSR
      *====================================================================*
      * LOAD SUBROUTINE                                    L O A D         *
      *====================================================================*
     C     LOAD          BEGSR
      *
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      'Y'           XXPCFL            1
     C                   MOVE      SCIKEY        SVIKEY
     C                   MOVE      SCISEQ        SVISEQ
      *
      *  Load the subfile screen with up to 13 lines
      *
     C                   DO        13            XY                5 0
      *
     C     JMP           TAG
     C                   READ      PPW01                                  60
      *
      * Do not display PW records that are marked as copy complete (CD)
      * while in purge mode.
      *
     C     *IN60         IFEQ      '0'
     C     QQPDFL        ANDEQ     'P'                                          PURGE MODE
     C     PWDELF        ANDEQ     'D'
     C                   GOTO      JMP
     C                   END
      *
      * Do not display PW records that are marked as having errors (E)
      * while in purge mode.
      *
     C     *IN60         IFEQ      '0'
     C     QQPDFL        ANDEQ     'P'                                          PURGE MODE
     C     PWSTAT        ANDEQ     'E'
     C                   GOTO      JMP
     C                   END
      *
      * Do not display PW records that are completed (STAT='C') while
      * in delete mode.
      *
     C     *IN60         IFEQ      '0'
     C     QQPDFL        ANDEQ     'D'                                          DELETE MODE
     C     PWDELF        ANDEQ     'C'
     C                   GOTO      JMP
     C                   END
      *
      * Do not display PW records that are not prgd  (STAT><C') while
      * in delete mode.
      *
     C     *IN60         IFEQ      '0'
     C     QQPDFL        ANDEQ     'D'                                          DELETE MODE
     C     PWSTAT        ANDEQ     'P'
     C     *IN60         OREQ      '0'
     C     QQPDFL        ANDEQ     'D'                                          DELETE MODE
     C     PWSTAT        ANDEQ     ' '
     C                   GOTO      JMP
     C                   END
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   END
      *
     C     *IN60         IFEQ      '0'
      *
     C                   ADD       1             RRN2
      *
      *  Save 1st subfile RRN of the current page
      *
     C     XXPCFL        IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN1
     C                   MOVE      'N'           XXPCFL
     C                   END
      *
      * Convert key if possible
      *
     C                   MOVE      *BLANKS       SCKEY
     C                   MOVEL     PWKEY         SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     PWDCID        SCDCID
      *
     C     PWPNAM        CHAIN     PKD01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     PWKEY         QQKEY
     C                   EXSR      SRKCVT
     C     QQRTCD        IFNE      'N'
     C                   MOVEL     QQKEY         SCDESC
     C                   ELSE
     C                   MOVEL     PWKEY         SCDESC
     C                   END
     C                   ELSE
     C                   MOVEL     PWKEY         SCDESC
     C                   END
      *
     C                   Z-ADD     PWSEQ#        SCSEQ#
      *
     C                   MOVE      *BLANKS       SCCOMM
     C     CMKY          CHAIN     PCM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     CMCOMM        SCCOMM
     C                   ENDIF
      *
     C                   MOVEL     PWSTAT        SCSTAT
     C                   MOVEL     PWSTAT        SCSTA2
     C                   MOVEL     PWDELF        SCDELF
     C                   MOVEL     PWDELF        SCDEL2
      *
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
      *
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SA
     C                   WRITE     SCR01SB
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END                                                    *IN60 END
      *
     C                   END                                                    DO 13 END
      *
      * Initialize subfile if no records to display
      *
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCSEQ#
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      *BLANKS       SCDELF
     C                   MOVE      *BLANKS       SCSTA2
     C                   MOVE      *BLANKS       SCDEL2
     C                   MOVE      *BLANKS       SCCOMM
     C                   MOVE      *BLANKS       SCDCID
      * Hidden
     C                   MOVE      *BLANKS       SCKEY
      *
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             XXRRN1
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   WRITE     SCR01SB
     C                   END
      *
      *  To position screen and cursor
      *
     C                   Z-ADD     XXRRN1        #@$2
     C                   Z-ADD     XXRRN1        XRRN2
      *
     C                   MOVE      *BLANK        Q2RECF
     C     *IN71         IFEQ      '0'
     C                   MOVEL     'SCR01A'      Q2RECF
     C                   ELSE
     C                   MOVEL     'SCR01B'      Q2RECF
     C                   END
     C                   ENDSR
      *====================================================================*
      * SUBROUTINE FOR KEY FIELD CONVERSION                                *
      *====================================================================*
     C     SRKCVT        BEGSR
      *
      * QQKEY is the key data value passed in from the calling         M,
      * program  M79A - 40 bytes are input to this program.  The QQPGM
      * field contains the name of the program used to get the key
      * definition record.  This field is the QQKEY value and will be
      * passed back to the calling program as a 79 byte field,
      * formatted with the heading for the key data.
      *
     C                   MOVEA     QQKEY         KY(1)
      *
     C                   Z-ADD     1             Z                 5 0
     C                   Z-ADD     1             A                 5 0
     C                   Z-ADD     1             X                 5 0
     C                   Z-ADD     1             Y                 5 0
     C                   MOVE      *BLANKS       MP
      *
     C     X             DOWLE     79
      *
     C     HD(A)         IFNE      *BLANKS
     C                   Z-ADD     HL(A)         Y
      *
     C                   MOVEA     HD(A)         H1
      *
     C                   Z-ADD     1             G                 5 0
      *
     C                   DO        Y
     C                   MOVEL     H1(G)         MP(X)
     C                   ADD       1             X
     C                   ADD       1             G
     C                   END
      *
     C                   ADD       1             X
      *
     C                   Z-ADD     DS(A)         C                 5 0
     C                   Z-ADD     DL(A)         B                 5 0
     C                   DO        B
     C                   MOVEL     KY(C)         MP(X)
     C                   ADD       1             X
     C                   ADD       1             C
     C                   END
      *
     C                   ADD       1             X
      *
     C                   END
      *
     C                   ADD       1             A                 5 0
     C     A             CABGT     8             DONE
      *
     C                   END
      *
     C     DONE          TAG
      *
     C     KEYOUT        IFNE      *BLANK
     C                   MOVEL     KEYOUT        QQKEY
     C                   ELSE
     C                   MOVEL     *BLANK        QQKEY            79
     C                   MOVEL     'N'           QQRTCD            1
     C                   END
      *
     C                   ENDSR
      *
      *====================================================================*
      * REFRESH S/R
      *====================================================================*
     C     SRREFS        BEGSR
      *
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C     Q2RECF        OREQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
      *
      *  Reload all subfile fields do load through the current RRN
      *  or through the RRN as determined by the cursor location.
      *
     C     CURLOC        IFEQ      0
     C                   Z-ADD     RRN2          SVRRN             5 0
     C                   ELSE
     C                   Z-ADD     CURLOC        SVRRN
     C                   END
      *  Clear subfile
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   WRITE     SCR01CB
     C                   MOVE      '0'           *IN04
      *
     C                   Z-ADD     0             RRN2
      *
     C     SVIKEY        IFNE      *BLANKS
     C                   MOVE      SVIKEY        SCIKEY
     C                   MOVE      SVISEQ        SCISEQ
     C                   END
      *
     C     PWKY          SETLL     PPW01                              60
      *
     C                   MOVE      *BLANKS       SCIKEY
     C                   Z-ADD     0             SCISEQ
     C                   END
      *
     C                   EXSR      LOAD
      *
     C                   Z-ADD     0             OVRF
     C     NXTONE        TAG
     C     RRN2          IFLT      SVRRN
     C                   EXSR      LOAD
     C                   ADD       1             OVRF              5 0
      *
      * Sfl overflows at 9999 records, so jump out after executing
      * load S/R (which loads 13 records) 769 times.
      *
     C     OVRF          CABGT     769           OUT
     C                   GOTO      NXTONE
     C                   END
      *
     C     OUT           TAG
      *
     C     *IN71         IFEQ      '0'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01A'      Q2RECF
     C                   ELSE                                                   ELSE EQ 'A'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01B'      Q2RECF
     C                   ENDIF                                                  END EQ 'A'
     C                   ENDSR
