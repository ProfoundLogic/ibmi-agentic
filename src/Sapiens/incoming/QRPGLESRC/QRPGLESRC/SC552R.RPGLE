     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC552R - TABLE BACKUP FILE SEARCH                        *
     H*                                                               *
     H*  PROGRAM DESCRIPTION: THIS PROGRAM PERFORMS TABLE BACKUP      *
     H*                                                               *
     H*     THE PROGRAM ONLY DISPLAYS SELECTIONS FOR A SPECIFIED KEY  *
     H*     WHICH CONSISTS OF THE CUSTOM ID AND FIELD NAME.           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H*== 08/09/91 === LJD === PROJ: 90007/D0007 =====================*
     H*   Added processing to allow display of new field 35A          *
     H*
     H*  07/18/96 C:53    REQ/PROJ: DR021                             *
     H*                   1. WRITE FULL SCREEN ON TABLE DISPLAY       *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  06    IF NO RECORDS TO SELECT, DON'T DISPLAY COLUMN SEPARATOR*
     H*        ON SEL FLAG FIELDS                                     *
     H*  20    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  50    CHECK FOR CHANGE IN SUBFILE SCREEN (I.E. SELECTION)    *
     H*  98    DISPLAY RECORD FORMAT B (W/ 35A)                       *
     H* N98    DISPLAY RECORD FORMAT A (W/ 10A, 6.0N, 9.2N)           *
     H*  99    LOKUP FN1                                              *
     H*****************************************************************
     FSC552F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN1)
     F                                     SFILE(SCR02S:RRN2)
     F*
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL   IF  E           K        DISK
     F**********SMCGL   IF  E           K        DISK
     FSTTBL4    IF   E           K DISK
     FSMCGL1    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  02/01/01  *********************************

     D*
     D FN1             S              4    DIM(20) CTDATA PERRCD(20)            FIELD NAMES RFMT A
     D FNB             S              4    DIM(6) CTDATA PERRCD(6)              FIELD NAMES RFMT B
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  HD1                    1    200
     D                                     DIM(20)                              USR ASGN FLD DSC RFM
     D  CGDA01                 1     10
     D  CGDA02                11     20
     D  CGDA03                21     30
     D  CGDA04                31     40
     D  CGDA05                41     50
     D  CGDA06                51     60
     D  CGDA07                61     70
     D  CGDA08                71     80
     D  CGDA09                81     90
     D  CGDA10                91    100
     D  CGDD01               101    110
     D  CGDD02               111    120
     D  CGDD03               121    130
     D  CGDD04               131    140
     D  CGDD05               141    150
     D  CGDN01               151    160
     D  CGDN02               161    170
     D  CGDN03               171    180
     D  CGDN04               181    190
     D  CGDN05               191    200
     D*
     D                 DS
     D  HDB                    1     60
     D                                     DIM(6)                               USR ASGN FLD DSC RFM
     D  CGDB01                 1     10
     D  CGDB02                11     20
     D  CGDB03                21     30
     D  CGDB04                31     40
     D  CGDB05                41     50
     D  CGDB06                51     60
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    DRFL11        USEGRP
     C     *LIKE         DEFINE    SCGRP         KKGRP
     C     *LIKE         DEFINE    SCGRP         @@GRP
     C*
     C     CGKY          KLIST
     C                   KFLD                    SCCSID
     C*
     C     CGKY1         KLIST
     C                   KFLD                    SCCSID
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*   KEY LIST FOR THE TABLE BACKUP FILE
     C*
     C     TBKEY         KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    SCCSID
     C                   KFLD                    SCFLD
     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY             6
     C                   PARM                    QQVUA            10
     C                   PARM                    QQVUD             6
     C                   PARM                    QQVUN             9
     C                   PARM                    QQVUB            35            08/09/91 ADD
     C                   PARM                    QQRTCD            1
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM                    @@GRP
     C*
     C                   CLEAR                   DRFL11
     C     'SC'          CHAIN     SDR01                              60
     C     DRFL11        COMP      'Y'                                    41
     C                   MOVEL     DRFL11        USEGRP
     C*
     C                   CLEAR                   CGGRP
     C                   CLEAR                   KKGRP
     C*
     C     Q1PRMS        IFGE      7
     C                   MOVE      @@GRP         KKGRP
     C                   ENDIF                                                  Q1PRMS>=7
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   MOVEL     QQKEY         SCCSID
     C                   MOVE      QQKEY         SCFLD
     C*
     C                   MOVEL     SCFLD         XXFMT             2
     C*
     C*** If we are working with Record Format B
     C     XXFMT         IFEQ      'UB'
     C                   MOVE      '1'           *IN98
     C*** GET FIELD HEADING NAME FROM CUSTOM GROUP ID FILE
     C                   Z-ADD     1             Y                 3 0
     C     SCFLD         LOOKUP    FNB(Y)                                 99
     C     *IN99         IFEQ      '1'
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCCSID    CHAINSMCGL                99
     C     CGKY1         CHAIN     PCG01                              99
     C   99CGKY          CHAIN     PCG01                              99
     C***  END   CHG  ***  02/01/01  *********************************
     C     *IN99         IFEQ      '0'
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE      CGGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     HDB(Y)        SCHEAD
     C                   MOVE      CGRFMT        SCRFMT
     C                   END                                                    END 1 *IN99
     C                   END                                                    END 0 *IN99
     C*
     C                   ELSE                                                   ELS EQ 'UB'
     C*
     C*** Else, we are working with Record Format A
     C                   MOVE      '0'           *IN98
     C*** GET FIELD HEADING NAME FROM CUSTOM GROUP ID FILE
     C                   Z-ADD     1             Y                 3 0
     C     SCFLD         LOOKUP    FN1(Y)                                 99
     C     *IN99         IFEQ      '1'
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCCSID    CHAINSMCGL                99
     C     CGKY1         CHAIN     PCG01                              99
     C   99CGKY          CHAIN     PCG01                              99
     C***  END   CHG  ***  02/01/01  *********************************
     C     *IN99         IFEQ      '0'
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE      CGGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     HD1(Y)        SCHEAD
     C                   MOVE      CGRFMT        SCRFMT
     C                   END                                                    END 1 *IN99
     C                   END                                                    END 0 *IN99
     C                   END                                                    END EQ 'UB'
     C                   MOVEL     CGDESC        SCCDSC
     C*
      *------------------------*
      *** RESET POINT IN PROGRAM
      *------------------------*
     C     TOP           TAG
     C*
     C*   INITIALIZE ROUTINE * * *
     C                   EXSR      INIT
     C*
     C     TBKEY         SETLL     PTB01                              60
     C*
     C*** WORKING WITH RECORD FORMAT B
     C     *IN98         IFEQ      '1'
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR02C
     C                   MOVE      '0'           *IN04
     C                   EXSR      LOADB                                        LOAD RCDS FROM FILE
     C*
     C                   ELSE                                                   ELSE 1 *IN98
     C*
     C*** ELSE WORKING WITH RECORD FORMAT A
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     SCR01C
     C                   MOVE      '0'           *IN04
     C                   EXSR      LOADA                                        LOAD RCDS FROM FILE
     C                   END                                                    END 1 *IN98
     C*
     C     STRT          TAG
     C*
     C                   MOVEL     Q2FILE        SCFILE                         SET FORMAT/FILE
     C                   MOVEL     Q2RECF        SCRECF                          FOR DISPLAY
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      '0'           *IN01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02C'
     C     Q2RECF        OREQ      'SCR02S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     SCR02
     C                   EXFMT     SCR02C
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
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C     Q2RECF        OREQ      'SCR02'
     C     Q2RECF        OREQ      'SCR02S'
     C     Q2RECF        OREQ      'SCR02C'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C****************************************
     C*   *IN10 -  PROCESS HELP REQUESTS     *
     C****************************************
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
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           STRT                           NO ROLL IF EOF
     C*
     C     *IN98         IFEQ      '1'                                          ROLLUP
C    C                   EXSR      LOADB
     C                   ELSE                                                   ELS 1 *IN98
C    C                   EXSR      LOADA
     C                   END                                                    END 1 *IN98
     C*
     C                   GOTO      STRT
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      STRT
     C                   END                                                    *IN11
     C**************************************************************************
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR01S'      SCR01T
     C     Q2RECF        CABEQ     'SCR01C'      SCR01T
     C*
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR02S'      SCR02T
     C     Q2RECF        CABEQ     'SCR02C'      SCR02T
     C**************************************************************************
     C*
     C*   PROCESS REQUESTED SCREEN
      *------------------------*
     C     SCR01T        TAG
      *------------------------*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   READC     SCR01S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL1        IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     SCCSID        QQKEY
     C                   MOVE      SCFLD         QQKEY
     C                   MOVEL     SCVUA         QQVUA
     C                   MOVE      SCVUD         QQVUD
     C                   MOVE      SCVUN         QQVUN
     C                   MOVE      SCVUB         QQVUB                          08/09/91 ADD
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL1        IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C                   GOTO      TERM
     C                   END                                                    END 0 *IN50
     C                   GOTO      TERM
     C**************************************************************************
     C*
     C*   PROCESS REQUESTED SCREEN
     C*
      *------------------------*
     C     SCR02T        TAG
      *------------------------*
     C*  CHECK FOR SEARCH SELECTION
     C*
     C                   READC     SCR02S                                 50
     C     *IN50         IFEQ      '0'
     C*** IF THE SUBFILE HAS CHANGED AND THE USER HAS SELECTED A RECORD, THEN
     C*** SAVE THAT DATA TO BE PASSED BACK TO THE CALLING PROGRAM
     C     SCSEL2        IFEQ      '1'
     C                   MOVE      '*'           QQRTCD
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     SCCSID        QQKEY
     C                   MOVE      SCFLD         QQKEY
     C                   MOVEL     SCVUA         QQVUA
     C                   MOVE      SCVUD         QQVUD
     C                   MOVE      SCVUN         QQVUN
     C                   MOVE      SCVUB         QQVUB                          08/09/91 ADD
     C                   ELSE                                                   ELSE EQ '1'
     C*** IF THE SUBFILE HAS CHANGED BUT THE USER DID NOT USE A '1' TO INDICATE
     C*** A SELECTED RECORD, SEND AN ERROR MESSAGE BACK TO THE SCREEN
     C     SCSEL2        IFNE      ' '
     C                   MOVE      '1'           *IN20
     C                   UPDATE    SCR02S
     C                   GOTO      STRT
     C                   END                                                    END NE ' '
     C                   END                                                    END EQ '1'
     C*** IF THE SUBFILE HAS NOT BEEN CHANGED, RETURN TO THE CALLING PROGRAM
     C*** WITHOUT A SELECTED RECORD
     C                   ELSE
     C                   GOTO      TERM
     C                   END                                                    END 0 *IN50
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================*
      * * *   INIT SUBROUTINE                                    I N I T ***
      *====================================================================*
     C     INIT          BEGSR
     C                   Z-ADD     0             RRN1              5 0
     C                   Z-ADD     0             RRN2              5 0
     C*
     C     *IN98         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE                                                   ELSE 1 *IN98
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END                                                    ELSE 2 *IN98
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOADA  SUBROUTINE                                L O A D A ***
      *====================================================================*
     C     LOADA         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C*
     C                   DO        11            X                 5 0          CHG 07/18/96
     C*
     C     TBKEY         READE     PTB01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN1
     C                   MOVEL     TBVUA         SCVUA
     C                   Z-ADD     TBVUD         SCVUD
     C                   Z-ADD     TBVUN         SCVUN
     C                   MOVEL     TBDESC        SCDESC
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR01S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C     X             IFEQ      1                                            ADD 07/18/96
     C                   Z-ADD     RRN1          #@$1                           ADD 07/18/96
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     #@$1          IFEQ      *ZERO
     C                   Z-ADD     1             #@$1
     C                   ENDIF
     C***  END   ADD  ***  02/01/01  *********************************
     C                   END                                                    ADD 07/18/96
     C*
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN1          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCVUA
     C                   MOVE      *BLANKS       SCVUD
     C                   MOVE      *BLANKS       SCVUN
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     1             RRN1
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR01S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C*
     C                   ENDSR
      *====================================================================*
      * * *   LOADB  SUBROUTINE                                L O A D B ***
      *====================================================================*
     C     LOADB         BEGSR
     C*
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        13            X                 5 0
     C*
     C     TBKEY         READE     PTB01                                  60
     C*
     C     *IN60         IFEQ      '1'
     C     X             ANDEQ     1
     C                   MOVE      '1'           *IN32
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C                   ADD       1             RRN2
     C                   MOVEL     TBVUB         SCVUB
     C                   MOVEL     TBDESC        SCDESC
     C*
     C                   MOVE      '0'           *IN06
     C                   WRITE     SCR02S
     C                   ELSE
     C                   MOVE      '1'           *IN05
     C                   END
     C                   END
     C*
     C* * * INITIALIZE SUBFILE IF NO RECORDS TO DISPLAY
     C     RRN2          IFEQ      0
     C     *IN60         ANDEQ     '1'
     C                   MOVE      *BLANKS       SCVUB
     C                   MOVE      *BLANKS       SCDESC
     C                   Z-ADD     1             RRN2
     C                   MOVE      '1'           *IN06
     C                   WRITE     SCR02S
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C                   Z-ADD     RRN2          #@$2
     C*
     C                   ENDSR
** FN1 = FIELD NAMES FOR 10A/6.0N/9.2N
UA01UA02UA03UA04UA05UA06UA07UA08UA09UA10UD01UD02UD03UD04UD05UN01UN02UN03UN04UN05
** FNB = FIELD NAMES FOR 35A
UB01UB02UB03UB04UB05UB06
