     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC852R - OPTICAL PURGE/DEL SEARCH                        *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  PROGRAMMER: C:12                                             *
     H*  CREATED:    05/04/93             PROJECT: OP2                *
     F**
     F** REVISIONS:
     F**
     F** 02 2/7/07 - CHANGE SO ONLY COPY COMPLETED RECORDS ARE UPDATED
     F**             WHEN USING THE F20 AND F21 KEYS
     F**
     H******************************************************************
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*  01    SFLDSPCTL                                              *
     H*  02    SFLINZ                                                 *
     H*  03    SFLDLT                                                 *
     H*  04    SFLCLR                                                 *
     H*  05    SFLEND                                                 *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  18    ROLLUP INDICATOR                                       *
     H*  20    WCR0898-INVALID VALUE KEYED                            *
     H*  21    SFLMSG -STATUS=C, INVALID OPTION SELECTED              *
     H*  30    PURGE OR DELETE INDICATOR                              *
     H*  32    ROLL UP INDICATOR - SUBFILE EOF                        *
     H*  60    GENERAL CHAIN                                          *
     H*   N71    SCR02 - View 1 (A)
     H*    71    SCR02 - View 2 (B)
     H* NOTE:
     H*        SFLCSRRRN - SUBFILE CURSOR RELATIVE RECORD # RETURNS   *
     H*        TO THE PGM THE RRN OF THE CURSOR. USING 'CURLOC' FIELD *
     H*        TO FILL #@$2 CORRECTS THE PAGE UP PROBLEM SFL SEARCHES *
     H*        HAVE.                                                  *
     H*****************************************************************
     H* FILES:                                                        *
     H*****************************************************************
     FSC852F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01SA:RRN2)
     F                                     SFILE(SCR01SB:RRN2)
     F*
     FSMPRL1    UF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSMKDL     IF   E           K DISK
     FSDCMLA    IF   E           K DISK

     D KY              S              1    DIM(40)                              KEY DATA
     D H1              S              1    DIM(8)                               HEAD TEMP
     D*
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
     D*
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
     D*
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
     D*
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
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
      *------------------------*
      *   *LIKE
      *------------------------*
     C     *LIKE         DEFINE    RRN2          SRRN2
     C     *LIKE         DEFINE    RRN2          XXRRN2
     C     *LIKE         DEFINE    SCIKEY        SVIKEY
     C     *LIKE         DEFINE    SCISEQ        SVISEQ
     C     *LIKE         DEFINE    PRKEY         XXKEY
      *------------------------*
      *   KEY LISTS
      *------------------------*
     C*
     C*   KEY LIST FOR THE REMARKS DETAIL RECORD
     C     CMKY          KLIST
     C                   KFLD                    PRKEY
     C                   KFLD                    PRSEQ#
     C*
     C     PRKY          KLIST
     C                   KFLD                    SCIKEY
     C                   KFLD                    SCISEQ
      *
     C     PRKY1         KLIST
     C                   KFLD                    XXKEY
     C*
     C     PRKY2         KLIST
     C                   KFLD                    SCKEY
     C                   KFLD                    SCSEQ#
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
      *------------------------*
      *   PARM LISTS
      *------------------------*
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C     *ENTRY        PLIST
     C                   PARM                    QQPDFL            1            PURGE/DELETE FLAG
     C                   PARM                    QQRTCD            1
     C*
      *------------------------*
      *   INITIALIZATION
      *------------------------*
     C                   TIME                    DSDTTM
     C*
      /Free
       scPrds = 'Optical Purge/Delete Search';
      /End-Free
      *
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANKS       UPOUTQ
     C                   END
     C*
     C*
     C*
     C     STRT01        TAG
     C*
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C                   MOVE      *LOVAL        XXKEY
     C     PRKY1         SETLL     PPR01
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   WRITE     SCR01CB
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
      *
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C*
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01A
     C                   EXFMT     SCR01CA
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01B
     C                   EXFMT     SCR01CB
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C*******************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*******************************************************************
     C*
     C     *IN11         IFEQ      '1'
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F5 = REFRESH                       *
     C****************************************
     C     *INKE         IFEQ      '1'
      *
     C                   EXSR      SRREFS
      *
     C                   ENDIF                                                  END *INKE
     C****************************************
     C*   F11 = CHANGE VIEW                  *
     C****************************************
     C     *INKK         IFEQ      '1'
     C*
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
     C*
     C                   END                                                    END *INKK
     C****************************************
     C*   F12= PREVIOUS                      *
     C****************************************
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
     C****************************************
     C*   F13 = CHANGE USER PARAMETERS       *
     C****************************************
     C     *INKM         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C     Q2RECF        OREQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C                   CALL      'W0006R'
     C                   WRITE     SCR99
     C*
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
     C*  GET THE USER PARAMETER FILE
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
     C*********************************
     C*   F20 - SELECT ALL ENTRIES    *
     C*********************************
     C     *INKU         IFEQ      '1'
     C                   MOVE      *LOVAL        XXKEY
     C     PRKY1         SETLL     PPR01
     C                   READ      PPR01                                  60
     C     *IN60         DOWEQ     '0'
     C     QQPDFL        IFEQ      'P'                                          PURGE MODE
     C     PRSTAT        ANDNE     'C'                                          COMPLETE
     C                   MOVE      'P'           PRSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'                                          DELETE MODE
     C     PRDELF        ANDNE     'C'                                          COMPLETE
     C*** 2/7/07 ADD ****Z***************************************
     C     PRSTAT        ANDEQ     'C'                                          COPY COMPLETED
     C*** 2/7/07 END ADD ****************************************
     C                   MOVE      'D'           PRDELF
     C                   ENDIF
     C                   UPDATE    PPR01
     C                   READ      PPR01                                  60
     C                   ENDDO
      * DO REFRESH FUNCTION
     C                   EXSR      SRREFS
      *
     C                   ENDIF                                                  END *INKU
     C*********************************
     C*   F21 - DE-SELECT ALL ENTRIES *
     C*********************************
     C     *INKV         IFEQ      '1'
     C                   MOVE      *LOVAL        XXKEY
     C     PRKY1         SETLL     PPR01
     C                   READ      PPR01                                  60
     C     *IN60         DOWEQ     '0'
     C     QQPDFL        IFEQ      'P'                                          PURGE MODE
     C     PRSTAT        ANDNE     'C'                                          COMPLETE
     C                   MOVE      ' '           PRSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'                                          PURGE MODE
     C     PRDELF        ANDNE     'C'                                          COMPLETE
     C*** 2/7/07 ADD ********************************************
     C     PRSTAT        ANDEQ     'C'                                          COPY COMPLETED
     C*** 2/7/07 END ADD ****************************************
     C                   MOVE      ' '           PRDELF
     C                   ENDIF
     C                   UPDATE    PPR01
     C                   READ      PPR01                                  60
     C                   ENDDO
      * DO REFRESH FUNCTION
     C                   EXSR      SRREFS
      *
     C                   ENDIF                                                  END *INKV
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
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
     C*
     C                   GOTO      STRT
     C                   END                                                    END CMDKEYS
     C**************************************************************************
     C*  GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01A'      SCR01T
     C     Q2RECF        CABEQ     'SCR01CA'     SCR01T
     C     Q2RECF        CABEQ     'SCR01SA'     SCR01T
     C     Q2RECF        CABEQ     'SCR01B'      SCR01T
     C     Q2RECF        CABEQ     'SCR01CB'     SCR01T
     C     Q2RECF        CABEQ     'SCR01SB'     SCR01T
     C*
     C*
     C*
     C***********************************
     C     SCR01T        TAG
     C***********************************
     C*
     C* * * Edit fields
     C     SCIKEY        IFNE      *BLANKS
     C     SCISEQ        ORNE      0
     C     PRKY          SETLL     PPR01
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   WRITE     SCR01CB
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*** SET CURSOR AT TOP OF SFL HERE ****
     C*
     C                   MOVE      *BLANKS       SCIKEY
     C                   Z-ADD     0             SCISEQ
     C                   GOTO      STRT
     C                   END
     C                   MOVE      '0'           ERR
     C*
     C*** Save RRN so that rolling can properly function if completion is marked
     C                   MOVE      RRN2          SRRN2
     C*
     C  N71              READC     SCR01SA                                50
     C   71              READC     SCR01SB                                50
     C*
     C     *IN50         DOWEQ     '0'
     C                   MOVEA     '0000'        *IN(20)
     C*
     C*** Process for invalid option selected
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
     C*
     C*** Process for 1=SELECT FOR PURGE ;  status must not equal 'C'.
     C*-OR-Process for 1=SELECT FOR DEL  ;  status must not equal 'C'.
     C     SCSEL         IFEQ      '1'
     C     SCSEL2        OREQ      '1'
     C*
     C*** ERROR IF PURGE COMPLETED DON'T ALLOW SELECT TO PURGE
     C***
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
     C*
     C                   ENDIF                                                   ENDIF SCSTAT <> 'C'
     C*
     C                   ELSE                                                   ELSE 0 *IN71
     C*
     C*** ERROR IF DELET COMPLETED DON'T ALLOW SELECT TO DELET
     C***
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
     C*
     C                   ENDIF                                                   ENDIF SCSTAT <> 'C'
     C                   ENDIF                                                   ENDIF "P"       'C'
     C*****************************
      *
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
      *  VIEW ONE
     C     *IN71         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      'P'           SCSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      'D'           SCDELF
     C                   ENDIF
     C                   UPDATE    SCR01SA
      *
      * NOW MAKE SURE VIEW TWO IS UPDATED
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
      * ELSE VIEW TWO
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      'P'           SCSTA2
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      'D'           SCDEL2
     C                   ENDIF
     C                   UPDATE    SCR01SB
     C                   ENDIF                                                   ENDIF *IN70
      *
      * NOW MAKE SURE VIEW ONE IS UPDATED
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
      * NOW UPDATE FILE
     C     PRKY2         CHAIN     PPR01                              60
     C     *IN60         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      'P'           PRSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      'D'           PRDELF
     C                   ENDIF
     C                   UPDATE    PPR01
     C                   ENDIF
      *
     C                   ENDIF                                                   ENDIF SCSEL IFEQ '1
     C*
     C**** Process for 2=SELECT FOR NO PURGE ;  status must not equal 'C'.
     C*-OR-Process for 2=SELECT FOR NO DEL   ;  status must not equal 'C'.
     C     SCSEL         IFEQ      '2'
     C     SCSEL2        OREQ      '2'
     C     SCSEL         IFNE      '$'                                            DUMMY CODE HERE
     C     SCSEL         ANDNE     '$'
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
      *  VIEW ONE
     C     *IN71         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      ' '           SCSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      ' '           SCDELF
     C                   ENDIF
     C                   UPDATE    SCR01SA
      * NOW UPDATE VIEW TWO
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
      * ELSE VIEW TWO
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      ' '           SCSTA2
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      ' '           SCDEL2
     C                   ENDIF
     C                   UPDATE    SCR01SB
      * NOW UPDATE VIEW ONE
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
      *  UPDATE FILE
     C     PRKY2         CHAIN     PPR01                              60
     C     *IN60         IFEQ      '0'
     C     QQPDFL        IFEQ      'P'
     C                   MOVE      ' '           PRSTAT
     C                   ENDIF
     C     QQPDFL        IFEQ      'D'
     C                   MOVE      ' '           PRDELF
     C                   ENDIF
     C                   UPDATE    PPR01
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
     C*
     C                   ENDIF                                                   ENDIF SCSTAT <> 'C'
     C                   ENDIF                                                   ENDIF SCSEL IFEQ '2
     C*
     C*** Read next subfile record
     C     NXT1          TAG
     C  N71              READC     SCR01SA                                50
     C   71              READC     SCR01SB                                50
     C                   END
     C*
     C*** Restore RRN so that rolling can properly function
     C                   MOVE      SRRN2         RRN2
     C*
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
     C*
     C*
     C*** SET AT LAST RECORD SELECTED  ****
     C     CURLOC        IFEQ      0
     C                   Z-ADD     XRRN2         #@$2
     C                   ELSE
     C                   Z-ADD     CURLOC        #@$2
     C                   END
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C     *IN71         IFEQ      '0'
     C                   MOVEL     'SCR01A  '    Q2RECF
     C                   ELSE                                                   ELSE 0 *IN71
     C                   MOVEL     'SCR01B  '    Q2RECF
     C                   END                                                    END  0 *IN71
     C*
     C                   GOTO      STRT
     C*
     C*** End of Program
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT subroutine                                    I N I T ***
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
      * * *   LOAD Subroutine                                    L O A D ***
      *====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      'Y'           XXPCFL            1
     C                   MOVE      SCIKEY        SVIKEY
     C                   MOVE      SCISEQ        SVISEQ
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UP TO 13 LINES
     C                   DO        13            XY                5 0
     C*
     C     JMP           TAG
     C                   READ      PPR01                                  60
     C* DO NOT DISPLAY PR RECORDS THAT ARE COMPLETED (STAT='C') WHILE IN
     C* DELETE MODE.
     C     *IN60         IFEQ      '0'
     C     QQPDFL        ANDEQ     'D'                                          DELETE MODE
     C     PRDELF        ANDEQ     'C'
     C                   GOTO      JMP
     C                   END
     C*
     C* DO NOT DISPLAY PR RECORDS THAT ARE NOT PRGD  (STAT><C') WHILE IN
     C* DELETE MODE.
     C     *IN60         IFEQ      '0'
     C     QQPDFL        ANDEQ     'D'                                          DELETE MODE
     C     PRSTAT        ANDEQ     'P'
     C     *IN60         OREQ      '0'
     C     QQPDFL        ANDEQ     'D'                                          DELETE MODE
     C     PRSTAT        ANDEQ     ' '
     C                   GOTO      JMP
     C                   END
     C*
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   ADD       1             RRN2
     C*
     C*** SAVE 1ST SUBFILE RRN OF THE CURRENT PAGE
     C*
     C     XXPCFL        IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN1
     C                   MOVE      'N'           XXPCFL
     C                   END
     C*
     C*
     C* CONVERT KEY IF POSSIBLE
     C                   MOVE      *BLANKS       SCKEY
     C                   MOVEL     PRKEY         SCKEY
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCDCID
     C                   MOVEL     PRDCID        SCDCID
      *
     C     PRPNAM        CHAIN     PKD01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     PRKEY         QQKEY
     C                   EXSR      SRKCVT
     C     QQRTCD        IFNE      'N'
     C                   MOVEL     QQKEY         SCDESC
     C                   ELSE
     C                   MOVEL     PRKEY         SCDESC
     C                   END
     C                   ELSE
     C                   MOVEL     PRKEY         SCDESC
     C                   END
      *
     C                   Z-ADD     PRSEQ#        SCSEQ#
      *
     C                   MOVE      *BLANKS       SCCOMM
     C     CMKY          CHAIN     PCM01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVEL     CMCOMM        SCCOMM
     C                   ENDIF
     C*
     C                   MOVEL     PRSTAT        SCSTAT
     C                   MOVEL     PRSTAT        SCSTA2
     C                   MOVEL     PRDELF        SCDELF
     C                   MOVEL     PRDELF        SCDEL2
     C*
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01SA
     C                   WRITE     SCR01SB
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END                                                    *IN60 END
     C*
     C                   END                                                    DO 13 END
     C*
     C* * * Initialize subfile if no records to display
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCSEQ#
     C                   MOVE      *BLANKS       SCSTAT
     C                   MOVE      *BLANKS       SCDELF
     C                   MOVE      *BLANKS       SCSTA2
     C                   MOVE      *BLANKS       SCDEL2
     C                   MOVE      *BLANKS       SCCOMM
     C                   MOVE      *BLANKS       SCDCID
      * HIDDEN
     C                   MOVE      *BLANKS       SCKEY
     C*
     C                   MOVE      ' '           SCSEL
     C                   MOVE      ' '           SCSEL2
     C                   Z-ADD     1             RRN2
     C                   Z-ADD     1             XXRRN1
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01SA
     C                   WRITE     SCR01SB
     C                   END
     C*
     C*** To position screen and cursor
     C                   Z-ADD     XXRRN1        #@$2
     C                   Z-ADD     XXRRN1        XRRN2
     C*
     C                   MOVE      *BLANK        Q2RECF
     C     *IN71         IFEQ      '0'
     C                   MOVEL     'SCR01A'      Q2RECF
     C                   ELSE
     C                   MOVEL     'SCR01B'      Q2RECF
     C                   END
     C                   ENDSR
     C********************************************************************
     ******** SUBROUTINE FOR KEY FIELD CONVERSION ************************
     C********************************************************************
     C     SRKCVT        BEGSR
     C*
     C*
      **** QQKEY IS THE KEY DATA VALUE PASED IN FROM THE CALLING PROGRAM,
      **** 79A - 40 BYTES AR INPUT TO THIS PROGRAM.  THE QQPGM FIELD CONTAINS
      **** THE NAME OF THE PROGRAM USED TO GET THE KEY DEF RCD.  THIS FIELD IS
      **** THE KEY TO THE KEY DEFINITION FILE.
      ****    THE QQKEY VALUE WILL BE PASSED BACK TO THE CALLING PROGRAM
      ****    AS A 79 BYTE FIELD, FORMATTED WITH THE HEADING FOR THE KEY DATA.
     C*
     C*
     C                   MOVEA     QQKEY         KY(1)
     C***
     C*
     C*
     C                   Z-ADD     1             Z                 5 0
     C                   Z-ADD     1             A                 5 0
     C                   Z-ADD     1             X                 5 0
     C                   Z-ADD     1             Y                 5 0
     C                   MOVE      *BLANKS       MP
     C*
     C     X             DOWLE     79
     C*
     C     HD(A)         IFNE      *BLANKS
     C                   Z-ADD     HL(A)         Y
     C*
     C                   MOVEA     HD(A)         H1
     C*
     C                   Z-ADD     1             G                 5 0
     C*
     C                   DO        Y
     C                   MOVEL     H1(G)         MP(X)
     C                   ADD       1             X
     C                   ADD       1             G
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   Z-ADD     DS(A)         C                 5 0
     C                   Z-ADD     DL(A)         B                 5 0
     C                   DO        B
     C                   MOVEL     KY(C)         MP(X)
     C                   ADD       1             X
     C                   ADD       1             C
     C                   END
     C*
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   ADD       1             A                 5 0
     C     A             CABGT     8             DONE
     C*
     C                   END
     C*
     C     DONE          TAG
     C*
     C*
     C     KEYOUT        IFNE      *BLANK
     C                   MOVEL     KEYOUT        QQKEY
     C                   ELSE
     C                   MOVEL     *BLANK        QQKEY            79
     C                   MOVEL     'N'           QQRTCD            1
     C                   END
     C*
     C*
     C                   ENDSR
     C********************************************************************
     C     SRREFS        BEGSR                                                  REFRESH S/R
     C********************************************************************
     C     Q2RECF        IFEQ      'SCR01A'
     C     Q2RECF        OREQ      'SCR01CA'
     C     Q2RECF        OREQ      'SCR01SA'
     C     Q2RECF        OREQ      'SCR01B'
     C     Q2RECF        OREQ      'SCR01CB'
     C     Q2RECF        OREQ      'SCR01SB'
     C*** RELOAD ALL SUBFILE FIELDS DO LOAD THROUGH THE CURRENT RRN
     C*** OR THROUGH THE RRN AS DETERMINED BY THE CURSOR LOCATION.
     C     CURLOC        IFEQ      0
     C                   Z-ADD     RRN2          SVRRN             5 0
     C                   ELSE
     C                   Z-ADD     CURLOC        SVRRN
     C                   END
     C* * * CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01CA
     C                   WRITE     SCR01CB
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C     SVIKEY        IFNE      *BLANKS
     C                   MOVE      SVIKEY        SCIKEY
     C                   MOVE      SVISEQ        SCISEQ
     C                   END
     C*
     C     PRKY          SETLL     PPR01                              60
     C*
     C                   MOVE      *BLANKS       SCIKEY
     C                   Z-ADD     0             SCISEQ
     C                   END
     C*
     C                   EXSR      LOAD
     C*
     C*
     C                   Z-ADD     0             OVRF
     C     NXTONE        TAG
     C     RRN2          IFLT      SVRRN
     C                   EXSR      LOAD
     C                   ADD       1             OVRF              5 0
      * SFL OVERFLOWS AT 9999 RECORDS, SO JUMP OUT AFTER EXECUTING
      * LOAD S/R (WHICH LOADS 13 RECORDS) 769 TIMES.
     C     OVRF          CABGT     769           OUT
     C                   GOTO      NXTONE
     C                   END
     C*
     C     OUT           TAG
     C*
     C     *IN71         IFEQ      '0'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01A'      Q2RECF
     C                   ELSE                                                   ELSE EQ 'A'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01B'      Q2RECF
     C                   ENDIF                                                  END EQ 'A'
     C                   ENDSR
