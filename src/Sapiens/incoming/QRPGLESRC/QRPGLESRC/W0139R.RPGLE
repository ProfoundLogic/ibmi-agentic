     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H* Program: W0139R - Prompt for BULK POLICY ATTACH - PDF CREATION
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  20    SCM0508-Invalid User entered                           *
     H*  21    SCM0500-From Date Invalid                              *
     H*  22    SCM0500-To Date Invalid                                *
     H*  23    SCM0551-Dates in ascending order                       *
     H*  24    SCM0512-Invalid Remark Type                            *
     H*  31    Position Cursor at USER                                *
     H*  31    Position Cursor at USR2                                *
     H*  31    Position Cursor at USR3                                *
     H*  31    Position Cursor at USR4                                *
     H*  31    Position Cursor at USR5                                *
     H*  32    Position Cursor at (From) DATE                         *
     H*  33    Position Cursor at REMARK TYPE                         *
     H*  60    General Chain                                          *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 12/28/07   C:180         CI055 - Cloned from C133PNSRC.
     H*                          ALLOW CLIENT ON-DEMAND POLICY ATTACH
     H*****************************************************************
     FW0139F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMUPL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSMSLL     IF   E           K DISK

     D*
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
     C*
     C*  Remark Type Table File
     C     CTKEY         KLIST
     C                   KFLD                    SCRMTY
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    CTDT          @@TODT
     C     *LIKE         DEFINE    CTDT          @@FRDT
     C*
     C     TEST1         PLIST
     C                   PARM                    QQUSER           10
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQSMCD            4
     C                   PARM                    QQRMTY            5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQDITY            1
     C                   PARM                    QQSORT            2
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      *** BEG ADD ***** 11/08/04 *************************************
     C     UPKEY         CHAIN     PSL01                              99
     C*** END ADD ***** 11/08/04 **************************************
     C*
     C                   MOVEL     *BLANKS       SCUSER
     C                   MOVEL     *BLANKS       SCUSR2
     C                   MOVEL     *BLANKS       SCUSR3
     C                   MOVEL     *BLANKS       SCUSR4
     C                   MOVEL     *BLANKS       SCUSR5
     C                   MOVE      *BLANKS       SCFRDT
     C                   MOVE      *BLANKS       SCTODT
     C                   MOVE      *BLANKS       SCRMTY
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*   Load the Record Format Info for First Time
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display All Formats
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*************************************************************************
     C*   Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C***********************************
     C*   F3 = Exit                     *
     C***********************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C***********************************
     C*   F7 = User Search              *
     C***********************************
     C     *INKG         IFEQ      '1'
     C*
     C                   MOVEL     *BLANKS       XXUSR            10
     C                   SELECT
     C     FLD           WHENEQ    'SCUSER  '
     C                   MOVE      'Y'           QQREGT            1            RE-GET PROFILES
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC650C'
     C                   PARM                    SCUSER                             |
     C                   PARM                    QQREGT            1                |
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     **
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     **
     C     FLD           WHENEQ    'SCUSR2  '
     C                   MOVE      'Y'           QQREGT            1            RE-GET PROFILES
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC650C'
     C                   PARM                    SCUSR2                             |
     C                   PARM                    QQREGT            1                |
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     **
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     **
     C     FLD           WHENEQ    'SCUSR3  '
     C                   MOVE      'Y'           QQREGT            1            RE-GET PROFILES
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC650C'
     C                   PARM                    SCUSR3                             |
     C                   PARM                    QQREGT            1                |
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     **
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     **
     C     FLD           WHENEQ    'SCUSR4  '
     C                   MOVE      'Y'           QQREGT            1            RE-GET PROFILES
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC650C'
     C                   PARM                    SCUSR4                             |
     C                   PARM                    QQREGT            1                |
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     **
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN37
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     **
     C     FLD           WHENEQ    'SCUSR5  '
     C                   MOVE      'Y'           QQREGT            1            RE-GET PROFILES
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC650C'
     C                   PARM                    SCUSR5                             |
     C                   PARM                    QQREGT            1                |
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     **
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     **
     C                   ENDSL
     **
     C                   GOTO      STRT
     C*
     C                   END                                                    END *INKG
     C***********************************
     C*   F8 = Remark Type Search       *
     C***********************************
     C     *INKH         IFEQ      '1'
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SC580R'
     C                   PARM                    SCRMTY
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C     QQRTCD        IFEQ      'B'
     C     SCRMTY        OREQ      *BLANK
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END *INKH
     C***********************************
     C*   *IN10 = Process Help Requests *
     C***********************************
     C  N10
     CAN 10*IN10         IFEQ      '1'
     C  N10
     CAN 10              MOVE      '1'           *IN10
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
     C*
     C                   END                                                    END CMDKEYS
     C*************************************************************************
     C*
     C*  Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C*   Validate User Name - if entered
     C     SCUSER        IFNE      *BLANKS
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SVALUSR'
     C                   PARM                    SCUSER
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     '1'           STRT                     20
     C                   END                                                    END SCUSER NE *BLANK
     C*
     C     SCUSR2        IFNE      *BLANKS
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SVALUSR'
     C                   PARM                    SCUSR2
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     '1'           STRT                     25
     C                   END                                                    END SCUSER NE *BLANK
     C*
     C     SCUSR3        IFNE      *BLANKS
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SVALUSR'
     C                   PARM                    SCUSR3
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     '1'           STRT                     26
     C                   END                                                    END SCUSER NE *BLANK
     C*
     C     SCUSR4        IFNE      *BLANKS
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SVALUSR'
     C                   PARM                    SCUSR4
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     '1'           STRT                     27
     C                   END                                                    END SCUSER NE *BLANK
     C*
     C     SCUSR5        IFNE      *BLANKS
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'SVALUSR'
     C                   PARM                    SCUSR5
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     '1'           STRT                     28
     C                   END                                                    END SCUSER NE *BLANK
     C*
     C*  Validate From Date - if entered
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END                                                    END SRERR EQ '1'
     C*
     C*  Validate To Date - if entered
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END                                                    END SRERR EQ '1'
     C*
     C*   Dates must be in Ascending Order - if entered
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@TODT
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@FRDT
     C*
     C     @@TODT        CABLT     @@FRDT        STRT                   23
     C*
     C*   Validate Remark Type - if entered
     C     SCRMTY        CHAIN     PCT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END                                                     END *IN60 1 - PCT01
     C*
     C*
     C*   All Edits Passed - Call the CLP to submit the report
     C*
     C                   MOVE      *ZEROS        XXFRDT
     C                   MOVE      *ZEROS        XXTODT
     C                   MOVE      @@FRDT        XXFRDT            7
     C                   MOVE      @@TODT        XXTODT            7
     C                   CALL      'W0139C'
     C                   PARM      XXFRDT        QQFRD2            7            CYYMMDD
     C                   PARM      XXTODT        QQTOD2            7            CYYMMDD
     C                   PARM      SCRMTY        QQRMTY            5
     C                   PARM      SCUSER        QQUSER           10
     C                   PARM      SCUSR2        QQUSR2           10
     C                   PARM      SCUSR3        QQUSR3           10
     C                   PARM      SCUSR4        QQUSR4           10
     C                   PARM      SCUSR5        QQUSR5           10
     C                   PARM      *BLANKS       QQRTCD            1
     C*
     C*
     C*  End of the Program
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
