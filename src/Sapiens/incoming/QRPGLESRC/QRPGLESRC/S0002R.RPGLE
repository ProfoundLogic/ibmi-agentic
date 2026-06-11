     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE S0002R - SYSTEM COMPANY FILE OUTPUT ROUTING MAINTENANCE
     H BNDDIR('STBNDDIR')
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/03/15 C:300 Support: PHW35223                              *
     H*                Unable to allocate a record in SMORL file      *
     H*                Avoid the record lock                          *
     H*                                                               *
     H* 04/08/15 C:65  Project: Redmine #4659 (Client 89 Request)     *
     H*                Check if Dicer is available prior to allowing  *
     H*                Bill Combination output routing to be setup.   *
     H*                                                               *
     H*****************************************************************
     FS0002F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMORL     UF A E           K DISK
     F*** BEGIN DEL *** 08/18/98 **************************************
     F**********WMFNL   IF  E           K        DISK
     F*** END   DEL *** 08/18/98 **************************************
     FSMCOL     IF   E           K DISK
     F*** BEGIN CHG *** 08/18/98 **************************************
     F**********WMUPL   IF  E           K        DISK
     FSMUPL     IF   E           K DISK
     F*** END   CHG *** 08/18/98 **************************************
      *** begin add ***  04/08/15  **************************************
     d/copy sPrGenPop
      *** end add *****  04/08/15  **************************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D AV              S              1    DIM(66) CTDATA PERRCD(66)
     D FID             S              1    DIM(10)
     D CM              S             70    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D XXOFLD          DS           560
     D  X#FLD1                 1    256
     D  X#FLD2               257    512
     D  X#FLD3               513    560
     D  X#FIL#                17     17
     D  X#OUTQ                24     33
     D  X#HOLD                41     44
     D  X#SAVE                52     55
     D  X#COPY                65     66
     D  X#FORM                80     89
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    SCPGMN
     C                   KFLD                    SCFIL#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C*** BEGIN DEL *** 08/18/98 **************************************
     C********** FNKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPFND
     C*** END   DEL *** 08/18/98 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   GET THE USER PARAMETER FILE
     C*
     C*   GET THE BASE USER DEFAULT VALUES
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*** BEGIN DEL *** 08/18/98 **************************************
     C********** FNKEY     CHAINPFN01                60
     C*** END   DEL *** 08/18/98 **************************************
     C************         MOVELUPFND     SCFND            LJQ 7-8-88
     C************         MOVELFNNAME    SCNAME                "
     C*
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
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   COMMAND 05 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C                   CALL      'S0502R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCPGMN
     C                   PARM                    SCFIL#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCPGMN        OREQ      *BLANK
     C     SCFIL#        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C*   CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
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
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  PROGRAM NAME MUST BE GREATER THAN BLANK
     C*
     C     SCPGMN        IFLE      *BLANK
     C*
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*
     C*  FILE NUMBER MUST BE 0 - 7
     C*
     C     SCFIL#        IFLT      '0'
     C     SCFIL#        ORGT      '7'
     C*
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C                   MOVEL     'QSYSPRT'     SCPRTF
     C     SCFIL#        IFEQ      '0'
     C                   MOVE      ' '           SCPRTF
     C                   ELSE
     C                   MOVE      SCFIL#        SCPRTF
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
      ***** beg chg ***** 08/03/15 ****************************************
     C******* ORKEY         CHAIN     POR01                              40
     C     ORKEY         CHAIN(N)  POR01                              40
      ***** end chg ***** 08/03/15 ****************************************
     C     *IN40         IFEQ      '0'
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      OROUTQ        SCOUTQ
     C                   Z-ADD     ORCOPY        SCCOPY
     C                   MOVEL     ORTITL        SCTITL
     C                   MOVEL     ORFORM        SCFORM
     C                   MOVEL     ORDIS1        SCDIS1
     C                   MOVEL     ORDIS2        SCDIS2
     C                   MOVEL     ORDIS3        SCDIS3
     C                   MOVEL     ORDIS4        SCDIS4
     C                   MOVEL     ORHOLD        SCHOLD
     C                   MOVEL     ORSAVE        SCSAVE
     C                   MOVE      ORSTAT        SCSTAT
     C                   MOVE      OROFLD        SCOFLD
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*** begin add ***  04/08/15  **************************************
     C                   If        (scPgmn = 'W7615R' or scPgmn = 'W7700R')
     C                             and scFil# = '6'
     c                   movel(p)  'DICER'       xxpgm            10
     c                   movel(p)  '*LIB'        xxtyp             7
     c                   call      'SRCHKOBJ'
     c                   parm                    xxpgm
     c                   parm                    xxtyp
     c                   parm                    xxDicer           1
     C
     C                   If        xxDicer = '1'
      /free
            PopOneBtn('The Dicer product is required to set up +
                       the Combination bill processing and is +
                       part of the Gumbo suite of products.':
                       'Error - Dicer product not available':'Ok');
      /end-free
     C                   GOTO      STRT
     C                   Endif
     C*
     C                   Endif
     C*** end add *****  04/08/15  **************************************
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCFORM
     C                   MOVE      *BLANK        SCTITL
     C                   MOVE      *BLANK        SCCOPY
     C                   MOVE      *BLANK        SCDIS1
     C                   MOVE      *BLANK        SCDIS2
     C                   MOVE      *BLANK        SCDIS3
     C                   MOVE      *BLANK        SCDIS4
     C                   MOVE      *BLANK        SCOUTQ
     C                   MOVE      *BLANK        SCHOLD
     C                   MOVE      *BLANK        SCSAVE
     C                   MOVE      *BLANK        SCOFLD
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  PROGRAM NAME CANNOT BE BLANK
     C*
     C     SCPGMN        IFEQ      *BLANK
     C*
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  TITLE CANNOT BE BLANK
     C*
     C     SCTITL        IFEQ      *BLANK
     C*
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*  FORM TYPE MUST BE VALID
     C*
     C     SCFORM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  TEST THE FIST VALUE FOR CONDITIONAL VALUE
     C*
     C                   MOVEL     SCFORM        XXTEST            1
     C* * * 1ST CHAR CAN'T BE A NUMBER
     C     XXTEST        IFGE      '0'
     C     XXTEST        ANDLE     '9'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C* * * 1ST CHAR MUST BE AN '*' OR OTHER VALID VALUE
     C     XXTEST        IFNE      '*'
     C     XXTEST        LOOKUP    AV(1)                                  58
     C     *IN58         CABNE     '1'           STRT                 2222
     C                   END
     C*
     C     XXTEST        IFEQ      '_'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  TEST THE EACH BYTE FOR VALID VALUE
     C*
     C                   MOVEA     SCFORM        FID
     C     2             DO        10            I                 5 0
     C*
     C*  IF THIS BYTE IS A BLANK THE REST MUST BE BLANK
     C*
     C     FID(I)        IFEQ      *BLANK
     C     I             ADD       1             X                 5 0
     C     X             IFLT      10
     C                   MOVE      *BLANK        XXTEN            10
     C                   MOVEA     FID(X)        XXTEN
     C     XXTEN         IFNE      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   ELSE
     C*
     C*  IF NOT A BLANK TEST THE BYTE FOR GOODNESS SAKE
     C*
     C     FID(I)        LOOKUP    AV(1)                                  58
     C     *IN58         CABNE     '1'           STRT                 2222
     C                   END
     C                   END
     C*
     C*  COPIES MUST BE 1-99.
     C*
     C     SCCOPY        IFLT      1
     C     SCCOPY        ORGT      99
     C*
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C*
     C                   END
     C*
     C*
     C*  OUTPUT QUEUE
     C*
     C     SCOUTQ        IFEQ      *BLANK
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK THE OUTQ FOR A VALID VALUE
     C*
     C                   CALL      'S0003C'
     C                   PARM                    SCOUTQ
     C                   PARM      ' '           XXFOND            1
     C     XXFOND        IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*  SAVE VALUE
     C*
     C     SCSAVE        IFNE      '*NO '
     C     SCSAVE        ANDNE     '*YES'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  HOLD VALUE
     C*
     C     SCHOLD        IFNE      '*NO '
     C     SCHOLD        ANDNE     '*YES'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C****
     C**** OVRPRTF COMMAND PROMPT/EDIT
     C*
     C     *IN40         IFEQ      '1'                                          ADD MODE
     C     OROFLD        OREQ      *BLANKS
     C                   MOVEL     CM(1)         XX140           140
     C                   MOVE      CM(2)         XX140
     C                   MOVEL     *BLANKS       XXOFLD
     C                   MOVEL     XX140         XXOFLD
     C                   MOVEL     SCOUTQ        X#OUTQ
     C                   MOVEL     SCSAVE        X#SAVE
     C                   MOVEL     SCHOLD        X#HOLD
     C                   MOVE      SCCOPY        X#COPY
     C                   MOVEL     SCFORM        X#FORM
     C     SCFIL#        IFEQ      '0'
     C                   MOVEL     ' '           X#FIL#
     C                   ELSE
     C                   MOVE      SCFIL#        X#FIL#
     C                   END
     C                   Z-ADD     560           X#LEN
     C*
     C                   CALL      'QCMDCHK'                            99
     C                   PARM                    XXOFLD
     C                   PARM                    X#LEN            15 5
     C                   WRITE     SCR99
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVEL     XXOFLD        SCOFLD
     C                   ELSE
     C                   MOVE      *BLANKS       SCOFLD
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR03   '    Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR03T        TAG
     C*
     C*
     C                   Z-ADD     560           X#LEN
     C                   MOVEL     SCOFLD        XXOFLD
     C*
     C     SCOFLD        IFNE      *BLANKS
     C                   CALL      'QCMDCHK'                            99
     C                   PARM                    XXOFLD
     C                   PARM                    X#LEN            15 5
     C                   WRITE     SCR99
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'SCR03   '    Q2RECF
     C*
     C     *IN99         CABEQ     '1'           STRT                     20
     C*
     C                   MOVEL     XXOFLD        SCOFLD
     C                   END
     C*
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
      ***** beg add ***** 08/03/15 ****************************************
     C     *IN40         IFEQ      '0'
     C     ORKEY         CHAIN     POR01                              40
     C                   ENDIF
      ***** end add ***** 08/03/15 ****************************************
     C                   MOVEL     SCPGMN        ORPNAM
     C                   MOVEL     SCFIL#        ORFIL#
     C                   MOVEL     SCTITL        ORTITL
     C                   MOVEL     SCFORM        ORFORM
     C                   MOVEL     SCOUTQ        OROUTQ
     C                   Z-ADD     SCCOPY        ORCOPY
     C                   MOVEL     SCDIS1        ORDIS1
     C                   MOVEL     SCDIS2        ORDIS2
     C                   MOVEL     SCDIS3        ORDIS3
     C                   MOVEL     SCDIS4        ORDIS4
     C                   MOVEL     SCHOLD        ORHOLD
     C                   MOVEL     SCSAVE        ORSAVE
     C                   MOVEL     SCSTAT        ORSTAT
     C                   MOVEL     SCOFLD        OROFLD
     C                   MOVE      Q1USER        ORUSER
     C                   MOVE      Q1PGM         ORPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ORDT
     C                   Z-ADD     DSTIME        ORTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    POR01
     C                   ELSE
     C                   WRITE     POR01
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCPGMN
     C                   MOVE      *ZERO         SCFIL#
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ALLOWABLE VALUES (AV)
abcdefghijklmnopqrstrvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_#$@0123456789
**    OVERRIDE COMMAND
?OVRPRTF QSYSPRT  OUTQ(          ) HOLD(    ) SAVE(    ) COPIES(  )
FORMTYPE(          )
